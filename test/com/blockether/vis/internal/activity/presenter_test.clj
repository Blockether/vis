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
  (it "keeps useful outcomes and full details when later output grows the history"
      ;; Regression #212: subsequent output must not discard earlier presentation content.
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
        (expect (every? #(seq (get-in % ["presentation" "content"])) rows))
        (expect (not-any? #(get % "is_truncated") rows))))
  ;; #218: all findings remain visible, not just complete aggregate counts.
  (it "retains complete per-file and per-finding evidence alongside counts"
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
        (expect (not (get (first rows) "is_truncated")))
        (expect (not (get (second rows) "is_truncated")))
        (expect (= 100
                   (count (re-seq #"Unused binding"
                                  (pr-str (get-in rows [1 "presentation" "content"]))))))))
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
      (doseq [[operation heading] [[:run_tests "Metric"] [:lint_code "Metric"]]]
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
      (let [content (get (presenter/result-presentation {:operation :run_tests}
                                                        [{:total 1} {:total 2}])
                         "content")]
        (expect (= [["Metric" "Result"] ["Metric" "Result"]] (mapv #(get % "columns") content))))))

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

(defdescribe
  council-activity-test
  (it "shows the message and reply counts without the publication envelope"
      (doseq [operation [:council.publish :council.get]]
        (let [view (presenter/result-presentation
                     {:operation operation}
                     {:title "Review"
                      :content "Tests **passed**."
                      :entry_id 42
                      :thread_id 42
                      :group_id "internal-group"
                      :created_at 1789470180407
                      :kind "coordination"
                      :source "host"
                      :source_ref {:session_id "internal-author"}
                      :ping ["internal-recipient"]
                      :reply_required true
                      :replies
                      [{:session_id "internal-recipient" :state "pending"}
                       {:session_id "internal-other" :state "replied" :reply_entry_id 43}]})]
          (expect (= "Review · Replies: 1 pending · 1 received" (get view "summary")))
          (expect (= [{"type" "markdown" "text" "Tests **passed**."}] (get view "content")))
          (expect (not (re-find #"internal-|Created at|Reply required|Source|coordination"
                                (pr-str view)))))))
  (it "summarizes pages and keeps each read message behind its own disclosure"
      (let [value
            {:entries [{:title "Review"
                        :content "Full **message**."
                        :created_at 1
                        :entry_id 42
                        :kind "coordination"}]
             :after 42
             :has_more true}

            view
            (presenter/result-presentation {:operation :council.read} value)]

        (expect (= "1 message · more available" (get view "summary")))
        (expect (empty? (get view "content")))
        (expect (= [{"headline" "Review"
                     "summary" ""
                     "content" [{"type" "markdown" "text" "Full **message**."}]}]
                   (get view "sections")))))
  (it "uses one compact table for member and thread lists"
      (doseq [[operation value summary columns rows]
              [[:council.members
                [{:session_id "internal-member" :title "Reviewer" :state "running"}] "1 member"
                ["Member" "State"] [["Reviewer" "Running"]]]
               [:council.threads
                {:entries [{:thread_id 42 :title "Review" :created_at 1 :kind "coordination"}]
                 :after 42
                 :has_more false} "1 thread" ["Thread"] [["Review"]]]]]
        (let [view (presenter/result-presentation {:operation operation} value)]
          (expect (= summary (get view "summary")))
          (expect (= [{"type" "table" "columns" columns "rows" rows}] (get view "content"))))))
  (it "names empty Council results instead of exposing empty envelopes"
      (doseq [[operation value summary]
              [[:council.members [] "No active members"]
               [:council.threads {:entries [] :after 0 :has_more false} "No threads"]
               [:council.read {:entries [] :after 0 :has_more false} "No messages"]]]
        (let [view (presenter/result-presentation {:operation operation} value)]
          (expect (= summary (get view "summary")))
          (expect (empty? (get view "content")))
          (expect (empty? (get view "sections"))))))
  (it "retains full message bodies and distinct reply outcomes with wire keys"
      (let [body
            (apply str (repeat 200 "Full **message**.\n"))

            value
            {"content" body
             "reply_to" 42
             "replies" [{"state" "delivered"} {"state" "interrupted"} {"state" "unavailable"}]}

            view
            (presenter/result-presentation {:operation :council.get} value)]

        (expect (= "Reply · Replies: 1 delivered · 1 interrupted · 1 unavailable"
                   (get view "summary")))
        (expect (= [{"type" "markdown" "text" body}] (get view "content")))
        (expect (contract/valid-projection?
                  (result-fixture [[:council.read "" {"entries" [value] "has_more" true} nil]])))))
  (it "keeps local messages and lookups end-only"
      (doseq [operation [:council.publish :council.get :council.read :council.threads
                         :council.members]]
        (expect (false? (:show-start (presenter/for-tool operation)))))))

(defdescribe
  managed-agent-presentation-test
  (it "keeps cancellation counts without exposing recipient identifiers"
      (let [view (presenter/result-presentation {:operation :council.cancel}
                                                {:session_id "internal-child"
                                                 :status "cancelled"
                                                 :cancelled ["internal-child"
                                                             "internal-grandchild"]})]
        (expect (= "Cancelled · 2 subagents" (get view "summary")))
        (expect (empty? (get view "content")))
        (expect (not (re-find #"internal-" (pr-str view))))))
  (it "bounds Unicode summaries without dropping the delegated task"
      (let [task
            (apply str (repeat 160 "😀"))

            agent
            {:task task
             :status "running"
             :model "fixture-model-with-a-long-name"
             :iteration_budget 12
             :iterations_used 2
             :pending_input true}

            projection
            (result-fixture [[:council.publish_spawn "" agent nil]
                             [:council.subagents "" [agent] nil]])

            views
            (mapv #(get % "presentation") (get projection "rows"))]

        (expect (contract/valid-projection? projection))
        (expect (= task (get-in views [0 "content" 0 "text"])))
        (expect (= task (get-in views [1 "sections" 0 "content" 0 "text"])))
        (expect (re-find #"Input pending" (get-in views [1 "sections" 0 "summary"])))))
  (it "shows meaningful empty and populated team counts"
      (doseq [[value expected] [[[] "No subagents"]
                                [[{:session_id "child" :status "queued"}] "1 subagent"]]]
        (expect (= expected
                   (get (presenter/result-presentation {:operation :council.subagents} value)
                        "summary")))))
  (it "keeps all agent result views valid and retains lifecycle evidence"
      (let [projection
            (result-fixture
              [[:council.publish_spawn ""
                {:session_id "child" :task "Verify" :status "queued" :iteration_budget 2} nil]
               [:council.subagents ""
                [{:session_id "child"
                  :task "Verify"
                  :status "running"
                  :iterations_used 1
                  :iteration_budget 2}] nil]
               [:council.cancel "" {:session_id "child" :status "cancelled"} nil]
               [:council.route ""
                {:session_id "child" :provider "fixture" :model "small" :effective "next_request"}
                nil]])]
        (expect (contract/valid-projection? projection))
        (let [views (mapv #(get % "presentation") (get projection "rows"))]
          (expect (= ["Verify · Queued · Up to 2 iterations" "1 subagent" "Cancelled"
                      "fixture/small · next request"]
                     (mapv #(get % "summary") views)))
          (expect (= "Running · Iterations: 1/2" (get-in views [1 "sections" 0 "summary"])))
          (expect (= [{"type" "markdown" "text" "Verify"}]
                     (get-in views [1 "sections" 0 "content"])))
          (expect (every? empty? (map #(get-in views [% "content"]) [2 3])))
          (expect (not (re-find #"child|next_request|Session|Parent" (pr-str views)))))))
  (it "shows only spawn as running work"
      (doseq [[operation visible?] [[:council.publish_spawn true] [:council.subagents false]
                                    [:council.cancel false] [:council.route false]]]
        (expect (= visible? (:show-start (presenter/for-tool operation))))))
  (it
    "preserves failures and cancellation across every Council binding"
    (doseq [operation
            [:council.publish :council.get :council.read :council.threads :council.members
             :council.publish_spawn :council.subagents :council.cancel :council.route]

            outcome
            [:failed :cancelled]]

      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            details
            {:operation operation
             :presenter :generic
             :activity (presenter/for-tool operation)
             :started-at-ms (System/currentTimeMillis)}

            start
            (event/start-event ctx invocation details)

            terminal
            (event/terminal-event ctx
                                  invocation
                                  (assoc details
                                    :outcome outcome
                                    :error (ex-info "Agent request refused" {})))

            projection
            (activity/presentation (activity/replay [start terminal]))

            row
            (first (:rows projection))]

        (expect (= (name outcome) (:state row)))
        (expect (= "Agent request refused" (:error-summary row)))
        (expect (contract/valid-projection? projection))))))

(defn read-session-result
  "Representative multi-turn read with complete, repeated diagnostic evidence."
  []
  (let [request
        (str "Review the long request.\n"
             (apply str (repeat 30 "Keep every requirement.\n"))
             "Final request requirement.")

        failure
        {"turn_id" "turn-2"
         "iteration_id" "iteration-2"
         "iteration" 1
         "source" "code"
         "tool" "repl_eval"
         "classification" "python-error"
         "user_request" request
         "code" "raise ValueError(\"Fixture failure\")"
         "message" (str "Fixture failure\n"
                        (apply str (repeat 20 "Traceback detail\n"))
                        "Final failure detail.")
         "advice" "Check the failing input before retrying."
         "auth_token" "fixture-credential"}]

    {"session"
     {"title" "Session review"
      "model" "example-model"
      "turn_count" 3
      "goal" "Verify a complete session read."
      "turns"
      [{"id" "turn-1"
        "outcome" "completed"
        "iteration_count" 2
        "user_request" "Inspect the session."
        "answer" "Inspection complete."}
       {"id" "turn-2" "outcome" "failed" "iteration_count" 1 "user_request" request}
       {"id" "turn-3" "outcome" "running" "iteration_count" 1 "user_request" "Check the fix."}]}
     "current_turn" {"id" "turn-3"
                     "user_request" "Check the fix."
                     "status" "running"
                     "elapsed_ms" 2300
                     "iteration" {"current" 2}
                     "failures" []
                     "cost" {"input_tokens" 1200
                             "input_cache_read_tokens" 700
                             "input_cache_write_tokens" 100
                             "input_regular_tokens" 400
                             "output_tokens" 90
                             "output_reasoning_tokens" 12
                             "total_cost" 0.025}}
     "usage" {"totals" {"tokens" {"input" 12345
                                  "cached" 6700
                                  "cache_created" 100
                                  "uncached" 5545
                                  "output" 890
                                  "reasoning" 120}
                        "cost_usd" 0.125
                        "turns" 3
                        "iterations" 4}}
     "failures" [failure]
     "diagnosis" {"failure_count" 1
                  "by_classification" {"python-error" 1}
                  "repetition_loop" false
                  "failures" [failure]
                  "next_actions" ["Check the failing input before retrying."]}
     "transcript" {"turns" [{"complete" "Raw transcript remains available."}]}}))

(defn read-session-fixture
  "Production-generated Read session presentation for both clients."
  []
  (result-fixture [[:read_session "" (read-session-result) nil]]))

(defdescribe
  read-session-presentation-test
  ;; Regression #230: keep the overview compact without discarding full evidence.
  (it
    "groups turns and discloses complete requests and each failure only once"
    (let [value
          (read-session-result)

          view
          (presenter/result-presentation {:operation :read_session} value)

          content
          (get view "content")

          sections
          (get view "sections")

          turn-table
          (first (filter #(= ["Turn" "Outcome" "Request"] (get % "columns")) content))

          all-text
          (pr-str view)]

      (expect (= "Read session" (get view "headline")))
      (expect (= ["Current turn" "Usage" "Diagnosis" "Turns"]
                 (mapv #(get % "text") (filter #(= "heading" (get % "type")) content))))
      (expect (= ["Turn 1" "Turn 2" "Turn 3"] (mapv first (get turn-table "rows"))))
      (expect (every? #(<= (count (last %)) 49) (get turn-table "rows")))
      (expect (= "Review the long request.…" (get-in turn-table ["rows" 1 2])))
      (expect (= ["Session details" "Turn details" "Failure details"]
                 (mapv #(get % "headline") sections)))
      (expect (not (.contains (pr-str content) "Final request requirement.")))
      (expect (not (.contains (pr-str content) "Final failure detail.")))
      (expect (= 1 (count (re-seq #"Final request requirement\." all-text))))
      (expect (= 1 (count (re-seq #"Final failure detail\." all-text))))
      (expect (.contains all-text "12345"))
      (expect (.contains all-text "6700"))
      (expect (.contains all-text "1200"))
      (expect (contract/valid-presentation? view))))
  (it "bounds the recent-turn list while retaining every complete request"
      (let [turns
            (mapv (fn [n]
                    {"id" (str n)
                     "outcome" "completed"
                     "user_request" (str "Request " n " ends here.")})
                  (range 1 21))

            value
            (assoc (read-session-result)
              "session" {"title" "Many turns" "turns" turns}
              "current_turn" nil
              "failures" []
              "diagnosis" {})

            view
            (presenter/result-presentation {:operation :read_session} value)

            rows
            (mapcat #(get % "rows") (get view "content"))]

        (expect (= ["Turn 15" "Turn 16" "Turn 17" "Turn 18" "Turn 19" "Turn 20"] (mapv first rows)))
        (expect (.contains (pr-str (get view "content")) "6 of 20"))
        (expect (.contains (pr-str (get view "sections")) "Request 1 ends here."))
        (expect (not-any? #(= "Failure details" (get % "headline")) (get view "sections")))))
  (it "states empty and unavailable data without inventing zero usage"
      (let [view (presenter/result-presentation {:operation :read_session} {})]
        (expect (= "No session data" (get view "summary")))
        (expect (empty? (get view "sections")))
        (expect (contract/valid-presentation? view))))
  (it "matches the shared wire fixture with numeric usage and redacted credentials"
      (let [actual
            (read-session-fixture)

            expected
            (json/read-json (slurp (io/resource
                                     "vis-contract/fixtures/activity-read-session.json")))

            view
            (get-in actual ["rows" 0 "presentation"])]

        (expect (= expected actual))
        (expect (contract/valid-projection? actual))
        (expect (not (.contains (pr-str actual) "fixture-credential")))
        (expect (.contains (pr-str actual) "[REDACTED]"))
        (expect (.contains (pr-str (get view "content")) "12345"))
        (expect (.contains (pr-str (get view "sections")) "1200"))))
  (it "bounds Unicode previews without splitting characters or discarding their complete request"
      (let [request
            (apply str (repeat 100 "😀"))

            view
            (presenter/result-presentation
              {:operation :read_session}
              {"session" {"title" request "turns" [{"id" "unicode" "user_request" request}]}})

            rows
            (mapcat #(get % "rows") (get view "content"))]

        (expect (= (str (apply str (repeat 48 "😀")) "…") (last (first rows))))
        (expect (.contains (pr-str (get view "sections")) request))
        (expect (contract/valid-presentation? view))))
  (it "uses canonical failure records rather than copies nested in diagnosis or current turn"
      (let [value
            (read-session-result)

            failure
            (first (get value "failures"))

            view
            (presenter/result-presentation {:operation :read_session}
                                           (-> value
                                               (assoc "failures" [failure failure])
                                               (assoc-in ["current_turn" "failures"] [failure])))]

        (expect (= 1 (count (re-seq #"Final failure detail\." (pr-str view)))))
        (expect (.contains (get view "summary") "1 failure"))))
  (it "retains the failure request when the corresponding turn snapshot is unavailable"
      (let [failure
            (first (get (read-session-result) "failures"))

            view
            (presenter/result-presentation {:operation :read_session} {"failures" [failure]})]

        (expect (= 1 (count (re-seq #"Final request requirement\." (pr-str view)))))
        (expect (= 1 (count (re-seq #"Final failure detail\." (pr-str view)))))))
  (it "keeps the declared running state and genuine failure or cancellation evidence"
      (doseq [outcome [:failed :cancelled]]
        (let [ctx (event/context)
              invocation (event/invocation ctx nil)
              details {:operation :read_session
                       :presenter :generic
                       :activity (presenter/for-tool :read_session)
                       :started-at-ms (System/currentTimeMillis)}
              start (event/start-event ctx invocation details)
              running (activity/presentation (activity/replay [start]))
              terminal (event/terminal-event ctx
                                             invocation
                                             (assoc details
                                               :outcome outcome
                                               :error (ex-info "Unable to read session" {})))
              settled (activity/presentation (activity/replay [start terminal]))]

          (expect (= "Read session" (get-in running [:rows 0 :presentation "headline"])))
          (expect (= "running" (get-in running [:rows 0 :state])))
          (expect (= (name outcome) (get-in settled [:rows 0 :state])))
          (expect (= "Unable to read session" (get-in settled [:rows 0 :error-summary])))
          (expect (contract/valid-projection? settled)))))
  (it "distinguishes uncached input from regular input excluding cache writes"
      (let [view
            (presenter/result-presentation {:operation :read_session} (read-session-result))

            rows
            (mapcat #(get % "rows") (get (first (get view "sections")) "content"))

            regular
            (first (filter #(= "Regular input tokens" (first %)) rows))]

        (expect (.contains (pr-str (get view "content")) "uncached input 5545"))
        (expect (= ["Regular input tokens" "5445" "400"] regular)))))
