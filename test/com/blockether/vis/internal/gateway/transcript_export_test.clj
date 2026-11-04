(ns com.blockether.vis.internal.gateway.transcript-export-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.transcript-export :as transcript-export]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private template
  (str "<html><head>" transcript-export/data-element "<script>app()</script></head></html>"))

(defn- data-json
  "The text inside the data element of the filled page `page`."
  [page]
  (second (re-find #"<script id=\"vis-transcript-data\" type=\"application/json\">(.*?)</script>"
                   page)))

(defdescribe
  fill-test
  (describe "a transcript that holds HTML end tags"
            (it "keeps the data element whole, so the page still reads the transcript"
                (let [data
                      {"turns" [{"text" "</script><script>alert(1)</script><!-- x"}]}

                      page
                      (transcript-export/fill template data)]

                  (expect (= 2 (count (re-seq #"</script>" page))))
                  (expect (= data (wire/parse-json (data-json page))))
                  (expect (str/ends-with? page "<script>app()</script></head></html>")))))
  (describe "a page that is not an export page"
            (it "refuses a page without a data element"
                (expect (= :transcript-export/bad-template
                           (try (transcript-export/fill "<html></html>" {})
                                nil
                                (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
            (it "refuses a page with two data elements"
                (expect
                  (= :transcript-export/bad-template
                     (try (transcript-export/fill (str template transcript-export/data-element) {})
                          nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

(defn- form
  "A form of a finished step with Activity rows of `signals` in `state`."
  [signals & {:keys [state error duration-ms] :or {state "succeeded"}}]
  (cond-> {"src" "print(1)"
           "activity" {"rows" (mapv (fn [signal]
                                      {"signal" signal "state" state "summary" signal})
                                    signals)}}
    error
    (assoc "error" error)

    duration-ms
    (assoc "duration_ms" duration-ms)))

;; User report: the Markdown export did not look like the session screen. Its digest rows must
;; say what the Compact mode rows of the web app say, word for word.
(defdescribe
  digest-summary-test
  (it "counts mutations before observations, verifications and external actions, in plurals"
      (expect (= "2 mutations · 1 observation · 1 verification · 1 external action"
                 (transcript-export/digest-summary [(form ["observation" "mutation" "external"])
                                                    (form ["verification" "mutation"])]))))
  (it "counts a row still running in a finished turn as failed after an error, else cancelled"
      (expect (= "1 observation · 1 failed"
                 (transcript-export/digest-summary
                   [(form ["observation"] :state "running" :error "boom")])))
      (expect (= "1 observation · 1 cancelled"
                 (transcript-export/digest-summary [(form ["observation"] :state "running")]))))
  (it "counts an error that no Activity row reports once"
      (expect (= "1 observation · 1 failed"
                 (transcript-export/digest-summary [(form ["observation"] :error "boom")])))
      (expect (= "1 observation · 1 failed"
                 (transcript-export/digest-summary
                   [(form ["observation"] :state "failed" :error "boom")]))))
  (it "names steps without Activity by what they did"
      (expect (= "RAW EXECUTION" (transcript-export/digest-summary [{"src" "x = 1"}])))
      (expect (= "THINKING" (transcript-export/digest-summary []))))
  (it "adds the measured time, rounded as the page rounds it"
      (expect (= "1 mutation · 20.1s"
                 (transcript-export/digest-summary [(form ["mutation"] :duration-ms 20000)
                                                    (form [] :duration-ms 150)])))
      (expect (= "RAW EXECUTION · 1m 15s"
                 (transcript-export/digest-summary [{"duration_ms" 75500}])))))

(defn- summaries [markdown] (mapv second (re-seq #"<summary>▸ (.*?)</summary>" markdown)))

(defdescribe
  markdown-test
  (describe "the steps of a turn"
            (let [doc
                  {"session" {"id" "s1" "title" "Fix"}
                   "turns" [{"request" "Fix it"
                             "content" [{"type" "prose" "markdown" "Fixed."}]
                             "iterations"
                             [{"assistant_prose" "Reading." "forms" [(form ["observation"])]}
                              {"thinking" "Hmm." "forms" [(form ["observation"])]}
                              {"assistant_prose" "Editing." "forms" [(form ["mutation"])]}
                              {"assistant_prose" "Fixed." "forms" [(form ["verification"])]}]}]}

                  out
                  (transcript-export/markdown doc)]

              (it "fold under the note before them into one digest each, as Compact mode shows them"
                  (expect (= ["2 observations" "1 mutation · 1 verification"] (summaries out)))
                  (expect (str/includes? out "Reading.\n\n<details>")))
              (it "show a note that repeats the answer only once, as the answer"
                  (expect (= 1 (count (re-seq #"Fixed\." out)))))))
  (describe "a wake from a subagent of the session"
            (it "hides its request, as the session screen does"
                (let [doc {"session" {"id" "s1"}
                           "child_session_ids" ["child"]
                           "turns" [{"request" "agent done"
                                     "request_kind" "council"
                                     "council" {"author_session_id" "child"}}]}]
                  (expect (not (str/includes? (transcript-export/markdown doc) "agent done")))))))

(defn- metric
  "The value of the row `label` in the Metrics table of `markdown`."
  [markdown label]
  (second (re-find (re-pattern (str "(?m)^\\| " label " \\| (.*?) \\|$")) markdown)))

;; User report: the export must show the session metrics of the web panel on top: tokens, cost,
;; turns and tool failures, in the words and number formats of that panel.
(defdescribe
  metrics-markdown-test
  (it "writes the totals with the formats of the metrics panel"
      (let [out (transcript-export/markdown {"session" {"id" "s1" "model_pref" {"model" "pinned"}}
                                             "usage" {"input_tokens" 2089615
                                                      "output_tokens" 85000
                                                      "cost_usd" 4.2
                                                      "turn_count" 3
                                                      ;; JavaScript `toFixed(1)` of 8.45 is "8.4".
                                                      "iteration_count" 8450
                                                      "tool_call_count" 41
                                                      "tool_failure_count" 2
                                                      "cache_read_share_percent" 61
                                                      "duration_ms" 5400000}
                                             "turns" []})]
        (expect (str/includes? out "<details>\n<summary>Metrics</summary>\n\n| Metric | Value |"))
        (expect (= ["2.1M" "85k" "$4.20" "3" "8.4k" "41" "2" "61%" "—" "pinned" "1.5h"]
                   (mapv #(metric out %)
                         ["Total input" "Total output" "Cost" "Turns" "Calls" "Tools"
                          "Tool failures" "Cached input" "Reuse coverage" "Model" "Active"])))))
  (it "adds the subagent rows and the total cost when the session has subagents"
      (let [out (transcript-export/markdown
                  {"session" {"id" "s1"}
                   "usage" {"cost_usd" 0.004
                            "total_cost_usd" 1250.4
                            "model" "ran"
                            "subagents" {"agent_count" 2 "active_count" 1 "cost_usd" 1250.396}}
                   "turns" []})]
        (expect (= ["<$0.01" "2 · 1 active" "$1,250" "$1,250" "ran"]
                   (mapv #(metric out %)
                         ["Session cost" "Subagents" "Subagent cost" "Total cost" "Model"])))))
  (it "says that no call is measured when the session has no usage"
      (expect
        (str/includes?
          (transcript-export/markdown {"session" {"id" "s1"} "turns" []})
          "<details>\n<summary>Metrics</summary>\n\n_No measured calls yet._\n\n</details>"))))

;; User report: the export counts tool failures, so it must also list them, closed like Metrics.
;; The list names the same calls that `tool_failure_count` counts: a tool call with an error.
(defdescribe
  failed-tools-markdown-test
  (it "lists each failed tool call under a closed Failed tools section"
      (let [out (transcript-export/markdown
                  {"session" {"id" "s1"}
                   "turns" [{"iterations" [{"forms" [{"tool_name" "python_execution"
                                                      "scope" "t1/i2"
                                                      "error" {"message"
                                                               "\npatch | refused\nedit 3/3"}}
                                                     {"tool_name" "python_execution" "stdout" "ok"}
                                                     {"scope" "t1/i3" "error" "no tool"}]}]}
                            {"iterations" [{"forms" [{"tool_name" "python_execution"
                                                      "error" "timed out"}]}]}]})]
        (expect (str/includes? out
                               (str "<details>\n<summary>Failed tools (2)</summary>\n\n"
                                    "| Step | Tool | Error |\n| --- | --- | --- |\n"
                                    "| t1/i2 | python_execution | patch \\| refused |\n"
                                    "| t2 | python_execution | timed out |\n\n</details>"))))))
