(ns com.blockether.vis.internal.activity.presenter-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [charred.api :as json]
            [lazytest.core :refer [defdescribe expect it]]))

(defn result-fixture
  "Production-generated result views shared by Companion stories and TUI grid tests."
  [& [repl-cases]]
  (let
    [path
     "src/com/blockether/vis/internal/activity/presenter.clj"

     cases
     [[:cat path "12:abc│ (defn greeting [name]\n13:def│   (str \"Hello \" name))" nil]
      [:patch path "patched presenter.clj"
       {:metadata
        {:target {:resolved path}
         :diff
         "@@ -12,2 +12,2 @@\n (defn greeting [name]\n-  (str \"Hi \" name))\n+  (str \"Hello \" name))"
         :lines {"added" 1 "removed" 1 "modified" 1}}}]
      [:grep "greeting"
       "grep 'greeting'  2 hits · 2 files\nsrc/greeting.clj  (1)\n  12:abc│ (defn greeting [name]\ntest/greeting_test.clj  (1)\n  8:def│ (is (= \"Hello Ada\" (greeting \"Ada\")))"
       nil]
      [:doc "activity"
       "# Activity\n\nOpen an operation to read its **retained result**.\n\n- Read shows the captured lines.\n- Patch shows the applied changes."
       nil]
      [:council.publish "Activity review"
       {:title "Activity review"
        :entry_id 42
        :kind "informational"
        :thread_id 42
        :ping ["reviewer"]
        :content "Read and Patch now show their **results** after one disclosure."} nil]
      [:run_tests "Activity tests"
       {:is_pass true :total 12 :pass 12 :fail 0 :output "12 tests passed."} nil]]

     ctx
     (event/context)

     events
     (mapcat (fn [[operation label result envelope]]
               (let [invocation
                     (event/invocation ctx nil)

                     details
                     {:operation operation
                      :label label
                      :args [label]
                      :activity (presenter/for-tool operation)
                      :presenter (if (= operation :patch) :patch :generic)
                      :classification (if (= operation :patch) :mutation :observation)}]

                 [(event/start-event ctx invocation details)
                  (event/terminal-event ctx
                                        invocation
                                        (assoc details
                                          :outcome :succeeded
                                          :result result
                                          :result-envelope envelope
                                          :started-at-ms (System/currentTimeMillis)))]))
             (or repl-cases cases))]

    (-> (activity/replay events)
        (assoc :state :succeeded)
        activity/presentation
        (update :rows
                #(mapv (fn [i row]
                         (-> row
                             (assoc :id (str "result-" i)
                                    :duration-ms 0)
                             (dissoc :argument-key)))
                       (range)
                       %))
        wire/->wire)))

(defdescribe result-fixture-test
             (it "matches the shared result-view fixture and satisfies the portable contract"
                 (let [actual
                       (result-fixture)

                       expected
                       (json/read-json (slurp (io/resource
                                                "vis-contract/fixtures/activity-results.json")))]

                   (expect (contract/valid-projection? actual))
                   (expect (= expected actual)))))

(defdescribe
  language-result-summaries-test
  (it "states format outcomes for files, snippets, batches and empty results"
      (doseq [[result expected]
              [[{"changed" false "path" "src/example.clj" "formatter" "zprint"}
                "No formatting changes · src/example.clj"]
               [{:changed true :path "src/example.py" :formatter "ruff"}
                "Formatting changed · src/example.py"]
               [{"changed" true "chars" 2} "Formatting changed"]
               [{"changed" false "unbalanced" "Unmatched delimiter"} "Formatting incomplete"]
               [{"files" [{"path" "src/a.clj" "changed" true} {"path" "src/b.clj" "changed" false}]
                 "changed" 1} "1 of 2 files changed"]
               [{"files" [] "changed" 0} "No files to format"] [{} "No formatting result"]]]
        (expect (= expected
                   (get (presenter/result-presentation {:operation :format_code} result)
                        "summary")))))
  (it
    "states lint severity counts without mistaking absent results for clean checks"
    (doseq [[result expected]
            [[{"error" 0 "warning" 0 "info" 0 "files" 1} "No lint findings · 1 file checked"]
             [{:error 2 :warning 1 :info 3 :files 2}
              "2 errors · 1 warning · 3 info · 2 files checked"]
             [{"error" 0 "warning" 1 "info" 0 "files" 1}
              "0 errors · 1 warning · 0 info · 1 file checked"]
             [{"error" 0 "warning" 0 "info" 0 "files" 0} "No files to lint"] [{} "No lint result"]]]
      (expect (= expected
                 (get (presenter/result-presentation {:operation :lint_code} result) "summary")))))
  (it "keeps useful outcomes when later output exhausts the receipt body budget"
      ;; Regression: both rows had empty summaries, leaving only Details truncated.
      (let [checks
            [[:format_code "src/example.clj"
              {"changed" false "path" "src/example.clj" "formatter" "zprint"} nil]
             [:lint_code "src/example.clj" {"error" 0 "warning" 0 "info" 0 "files" 1 "findings" []}
              nil]]

            bulky
            (repeat 6 [:doc "Guide" (apply str (repeat 16000 "x")) nil])

            projection
            (result-fixture (concat checks bulky))

            rows
            (take 2 (get projection "rows"))]

        (expect (contract/valid-projection? projection))
        (expect (= ["No formatting changes · src/example.clj" "No lint findings · 1 file checked"]
                   (mapv #(get-in % ["presentation" "summary"]) rows)))
        (expect (every? #(empty? (get-in % ["presentation" "content"])) rows))
        (expect (every? #(get % "is_truncated") rows))))
  (it "counts full results before the event bounds per-file and per-finding evidence"
      (let [projection
            (result-fixture
              [[:format_code "src"
                (array-map
                  "files"
                  (vec (repeat 100 {"path" "src/example.clj" "changed" false "formatter" "zprint"}))
                  "changed" 0) nil]
               [:lint_code "src"
                (array-map "findings" (vec (repeat 100
                                                   {"file" "src/example.clj"
                                                    "row" 12
                                                    "col" 3
                                                    "level" "warning"
                                                    "message" "Unused binding"}))
                           "error" 0
                           "warning" 100
                           "info" 0
                           "files" 20) nil]])

            rows
            (get projection "rows")]

        (expect (contract/valid-projection? projection))
        (expect (= ["0 of 100 files changed" "0 errors · 100 warnings · 0 info · 20 files checked"]
                   (mapv #(get-in % ["presentation" "summary"]) rows)))
        (expect (every? #(get % "is_truncated") rows))
        (expect (re-find #"Unused binding" (pr-str (get-in rows [1 "presentation" "content"]))))))
  (it "uses complete results for counts but only public values for displayed text"
      (doseq [[operation complete public expected]
              [[:format_code {"changed" false "path" "original-target"}
                {"changed" false "path" "[REDACTED]"} "No formatting changes · [REDACTED]"]
               [:lint_code
                {"error" 0
                 "warning" 1
                 "info" 0
                 "files" 2
                 "findings" [{"message" "original-diagnostic"}]}
                {"findings" [{"message" "[REDACTED]"}]}
                "0 errors · 1 warning · 0 info · 2 files checked"]]]
        (let [view (presenter/result-presentation {:operation operation :result complete} public)]
          (expect (= expected (get view "summary")))
          (expect (not (re-find #"original-target|original-diagnostic" (pr-str view)))))))
  (it "retains providers, repair notes and finding locations in expanded content"
      (let [rows (get (result-fixture
                        [[:format_code "src/example.clj"
                          {"changed" true
                           "repaired" true
                           "formatter" "zprint"
                           "repairs" ["Completed delimiter on line 12"]} nil]
                         [:lint_code "src/example.clj"
                          {"error" 1
                           "warning" 0
                           "info" 0
                           "files" 1
                           "providers" ["clj-kondo" "general"]
                           "findings" [{"file" "src/example.clj"
                                        "row" 12
                                        "col" 3
                                        "level" "error"
                                        "type" "unresolved-symbol"
                                        "message" "Unresolved symbol: missing"}]} nil]])
                      "rows")]
        (expect (re-find #"Completed delimiter on line 12" (pr-str (first rows))))
        (expect (re-find #"clj-kondo" (pr-str (second rows))))
        (expect (re-find #"general" (pr-str (second rows))))
        (expect (re-find #"Unresolved symbol: missing" (pr-str (second rows))))
        (expect (re-find #"src/example.clj" (pr-str (second rows))))
        (expect (not-any? #(get % "is_truncated") rows))))
  (it
    "keeps running, failed and cancelled lifecycle evidence on the declared bindings"
    (doseq [[operation headline]
            [[:format_code "Format code"] [:lint_code "Lint code"]]

            outcome
            [:failed :cancelled]]

      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            declared
            (presenter/for-tool operation)

            details
            {:operation operation
             :presenter :generic
             :activity declared
             :started-at-ms (System/currentTimeMillis)}

            start
            (event/start-event ctx invocation details)

            terminal
            (event/terminal-event ctx
                                  invocation
                                  (assoc details
                                    :outcome outcome
                                    :error (ex-info "Cannot read target" {})))

            projection
            (activity/presentation (activity/replay [start terminal]))

            row
            (first (:rows projection))]

        (expect (:show-start declared))
        (expect (= headline (get-in start [:presentation "headline"])))
        (expect (= (name outcome) (:state row)))
        (expect (= "Cannot read target" (:error-summary row)))
        (expect (contract/valid-projection? projection))))))

(defdescribe
  result-table-headings-test
  (it "names scalar-table columns for the operation, with a readable fallback"
      (doseq [[operation heading] [[:run_tests "Metric"] [:lint_code "Metric"]
                                   [:council.publish "Message"] [:council.get "Message"]
                                   [:council.read "Thread"] [:council.threads "Thread"]
                                   [:council.members "Member"]]]
        (let [content (get (presenter/result-presentation {:operation operation}
                                                          {:total 12 :title "Activity review"})
                           "content")]
          (expect (= [heading "Result"] (get-in content [0 "columns"]))))))
  (it "keeps nested detail tables distinct from top-level test metrics"
      (let [content (get (presenter/result-presentation {:operation :run_tests}
                                                        {:total 12
                                                         :environment {:language "clojure"}})
                         "content")]
        (expect (= ["Metric" "Result"] (get-in content [0 "columns"])))
        (expect (= ["Detail" "Result"] (get-in content [2 "columns"])))))
  (it "uses the operation's columns for each item in a result list"
      (let [content (get (presenter/result-presentation {:operation "council.members"}
                                                        [{:name "Reviewer"} {:name "Author"}])
                         "content")]
        (expect (= [["Member" "Result"] ["Member" "Result"]] (mapv #(get % "columns") content))))))

(defdescribe
  semantic-results-test
  (it "hides transport identifiers and redundant flags recursively"
      (let [view
            (presenter/result-presentation {:operation :council.publish}
                                           {:title "Review"
                                            :thread_id 258
                                            :entry_id "secret-handle"
                                            :content "Useful result"
                                            :nested {:session_id "uuid" :name "Reviewer"}})

            body
            (pr-str (get view "content"))]

        (expect (not (re-find #"258|secret-handle|uuid|Review\"" body)))
        (expect (re-find #"Useful result" body))))
  (it "shows command, preserved output and actual exit status for shell handles"
      (let [view
            (presenter/result-presentation {:operation :_shell-wait :label "internal"}
                                           {:command "printf 'a\nb' && false"
                                            :out "a\nb"
                                            :exit 1
                                            :id "handle-123"
                                            :log_path "/private/log"
                                            :status "exited"})

            blocks
            (get view "content")]

        (expect (= "Command finished" (get view "headline")))
        (expect (some #(= {"type" "code" "language" "bash" "text" "printf 'a\nb' && false"} %)
                      blocks))
        (expect (some #(= "a\nb" (get % "text")) blocks))
        (expect (re-find #"Exit code: 1" (pr-str blocks)))
        (expect (not (re-find #"handle-123|/private/log|internal" (pr-str view))))))
  (it "never invents a successful exit for a running command"
      (let [view (presenter/result-presentation {:operation :shell}
                                                {:command "sleep 10" :exit nil :status "running"})]
        (expect (re-find #"Running" (pr-str view)))
        (expect (not (re-find #"Exit code: 0" (pr-str view))))))
  (it "does not turn a publish receipt identifier into expandable content"
      (expect
        (= [] (get (presenter/result-presentation {:operation :council.publish} 279) "content"))))
  (it "retains useful titles within member lists"
      (expect (re-find #"Reviewer"
                       (pr-str (presenter/result-presentation {:operation :council.members}
                                                              [{:session_id "opaque"
                                                                :title "Reviewer"}]))))))

(defdescribe
  repl-results-test
  (it "renders both languages as program, optional streams and one result"
      (doseq [language ["clojure" "python"]]
        (let [value {"language" language
                     "code" "source"
                     "value" "42"
                     "values" ["42"]
                     "data" 42
                     "type" "int"
                     "ok" true
                     "out" "hello\n"
                     "err" "warning\n"
                     "status" ["done"]}
              blocks (get (presenter/result-presentation {:operation :repl_eval} value) "content")]

          (expect (= ["Program" "Stdout" "Stderr" "Result"]
                     (mapv #(get % "text") (filter #(= "heading" (get % "type")) blocks))))
          (expect (= [language nil nil language]
                     (mapv #(get % "language") (filter #(= "code" (get % "type")) blocks))))
          (expect (= ["source" "hello\n" "warning\n" "42"]
                     (mapv #(get % "text") (filter #(= "code" (get % "type")) blocks)))))))
  (it "omits empty streams without losing false or zero results"
      (doseq [value [false 0 "false" "0" "\"nil\"" "\"None\""]]
        (let [view (presenter/result-presentation
                     {:operation :repl_eval}
                     {"language" "python" "code" "source" "value" value "out" "" "err" ""})]
          (expect (= ["Program" "Result"]
                     (mapv #(get % "text")
                           (filter #(= "heading" (get % "type")) (get view "content"))))))))
  (it "omits nil results and absent or blank streams independently"
      (doseq [[language result]
              [["clojure" {}] ["clojure" {"value" nil}] ["clojure" {"value" "nil"}]
               ["clojure" {"values" ["nil"]}] ["python" {}] ["python" {"value" nil}]
               ["python" {"value" "None"}]]

              out
              [{} {"out" nil} {"out" ""} {"out" " \n\t"} {"out" "hello\n"}]

              err
              [{} {"err" nil} {"err" ""} {"err" " \n\t"} {"err" "warning\n"}]]

        (let [view
              (presenter/result-presentation
                {:operation :repl_eval}
                (merge {"language" language "code" "source"} result out err))

              expected
              (cond-> [{"type" "heading" "text" "Program"}
                       {"type" "code" "text" "source" "language" language}]
                (= "hello\n" (get out "out"))
                (into [{"type" "heading" "text" "Stdout"} {"type" "code" "text" "hello\n"}])

                (= "warning\n" (get err "err"))
                (into [{"type" "heading" "text" "Stderr"} {"type" "code" "text" "warning\n"}]))]

          (expect (= expected (get view "content"))))))
  (it "keeps partial streams with errors and timeouts, not a successful result"
      (doseq [[failure title] [[{"ok" false "exc" "ValueError: invalid"} "Error"]
                               [{"ex" "ArithmeticException" "status" ["eval-error"]} "Error"]
                               [{"timed_out" true "ms" 100} "Timeout"]]]
        (let [view (presenter/result-presentation
                     {:operation :repl_eval}
                     (merge {"language" "clojure" "code" "source" "out" "partial\n" "value" "old"}
                            failure))
              headings (mapv #(get % "text")
                             (filter #(= "heading" (get % "type")) (get view "content")))]

          (expect (= ["Program" "Stdout" title] headings)))))
  (it "does not duplicate nREPL values or Python structured data"
      (let [view (presenter/result-presentation
                   {:operation :repl_eval}
                   {"language" "clojure" "code" "1 2" "value" "2" "values" ["1" "2"]})]
        (expect (= "1\n2" (get (last (get view "content")) "text"))))))

(defn repl-result-fixture
  "Retained REPL results shared by both production clients."
  []
  (result-fixture
    (mapv
      (fn [result]
        [:repl_eval "REPL" result nil])
      [{"language" "clojure"
        "code"
        "(do\n  (println \"Hello Ada\")\n  (binding [*out* *err*] (println \"Check input\"))\n  {:answer 42 :ready true})"
        "out" "Hello Ada\n"
        "err" "Check input\n"
        "value" "{:answer 42, :ready true}"
        "status" ["done"]}
       {"language" "python"
        "code"
        "import sys\nprint('Hello Ada')\nprint('Check input', file=sys.stderr)\n{'answer': 42, 'ready': True}"
        "out" "Hello Ada\n"
        "err" "Check input\n"
        "value" "{'answer': 42, 'ready': True}"
        "ok" true}
       {"language" "clojure"
        "code" "(/ 1 0)"
        "ex" "ArithmeticException"
        "error_message" "Divide by zero"
        "status" ["eval-error"]}
       {"language" "python"
        "code" "1 / 0"
        "ok" false
        "exc"
        "Traceback (most recent call last):\n  File \"<repl>\", line 1, in <module>\nZeroDivisionError: division by zero"}
       {"language" "clojure" "code" "(Thread/sleep 10000)" "timed_out" true "ms" 100}])))

(defdescribe repl-fixture-test
             (it "pins the portable REPL projection used by both clients"
                 (let [actual (repl-result-fixture)]
                   (expect (contract/valid-projection? actual))
                   (expect (= actual
                              (json/read-json (slurp
                                                (io/resource
                                                  "vis-contract/fixtures/activity-repl.json"))))))))
