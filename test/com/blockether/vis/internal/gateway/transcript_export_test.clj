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
