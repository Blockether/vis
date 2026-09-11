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
      (doseq [value ["false" "0" "None" "nil"]]
        (let [view (presenter/result-presentation
                     {:operation :repl_eval}
                     {"language" "python" "code" "source" "value" value "out" "" "err" ""})]
          (expect (= ["Program" "Result"]
                     (mapv #(get % "text")
                           (filter #(= "heading" (get % "type")) (get view "content"))))))))
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
