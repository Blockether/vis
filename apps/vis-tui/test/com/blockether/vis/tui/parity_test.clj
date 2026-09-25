(ns com.blockether.vis.tui.parity-test
  "PARITY INVARIANT — the regression gate for the TUI trace revamp.

   The live-vs-resume split was the root cause of every TUI regression: the
   live progress tracker and the resume projection used to build subtly
   different shapes, and the renderer papered over the gap with heuristics.

   This test feeds the SAME form fixture through BOTH paths and asserts they
   produce an equal canonical iteration-entry (`iteration/parity-entry`). If
   the two paths ever diverge in scope / merged code / status / duration /
   error, this fails and BLOCKS any UX change."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.activity :as activity]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.iteration :as iteration]
            [com.blockether.vis.tui.progress :as progress]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private it->iteration-entry
  (var-get (resolve 'com.blockether.vis.tui.chat/it->iteration-entry)))

;; Shared fixture: ONE fence that ran `(git/status)` then `(git/add ".")` and
;; printed some output. `:stdout` is the SINGLE display surface both paths
;; carry — that is the whole point; everything downstream must agree. The
;; commentary the model wrote beside the code and the Activity the form
;; recorded ride along: both paths must keep them too, or the receipt a reader
;; watched live loses its prose and its steps the moment the turn settles.

(def ^:private fence-stdout "STATUS  clean\nADD  .")

(def ^:private fence-code "(git/status)\n(git/add \".\")")

(def ^:private fence-prose "Staging the tree after a status check.")

(def ^:private activity-wire
  "The canonical wire Activity projection, exactly as a persisted envelope and a
   `block.activity` frame carry it."
  (json/read-json (slurp (io/resource "vis-contract/fixtures/activity.json"))))

(defn- live-entry
  "Drive the live progress tracker with the chunks one executed fence emits —
   the prose beside the code, the form start, its settled Activity frame and the
   `:form-result` carrying the fence code + printed output — then read back the
   canonical entry the tracker emits. The gateway chunk parser rehydrates the
   Activity frame before it reaches the tracker, so the chunk carries the
   engine-spelled snapshot."
  []
  (let [tracker
        (progress/make-progress-tracker)

        on-chunk
        (:on-chunk tracker)]

    (on-chunk {:phase :assistant-prose :iteration 1 :text fence-prose})
    (on-chunk {:phase :form-start :iteration 1 :position 0 :scope "t7/i3/f1" :code fence-code})
    (on-chunk {:phase :form-activity
               :iteration 1
               :position 0
               :scope "t7/i3/f1"
               :activity (activity/from-wire activity-wire)
               :settled? true})
    (on-chunk {:phase :form-result
               :iteration 1
               :position 0
               :scope "t7/i3/f1"
               :code fence-code
               :render-segments [{:kind :code :source "(git/status)"}
                                 {:kind :code :source "(git/add \".\")"}]
               :stdout fence-stdout
               :error nil
               :envelope {:started-at-ms 100 :finished-at-ms 912}})
    (first ((:get-timeline tracker)))))

(defn- resume-entry
  "Project a persisted iteration row with one proof envelope carrying the
   shared printed output — the resume counterpart of the live fence. The row
   already carries the canonical gateway shape, so parity is asserted against
   the ONE shape a channel actually receives (in-process or over HTTP alike)."
  []
  (it->iteration-entry {:produced-answer? false :last-iteration-id :other}
                       {"id" "it-1"
                        "position" 1
                        "code" fence-code
                        "duration_ms" 812
                        "assistant_prose" fence-prose
                        "forms" [{"scope" "t7/i3/f1"
                                  "tag" "observation"
                                  "src" fence-code
                                  "stdout" fence-stdout
                                  "duration_ms" 812
                                  "activity" activity-wire}]}))

(defdescribe iteration-entry-parity-test
             (it "live and resume produce an equal canonical iteration-entry for the same fixture"
                 (let [live
                       (iteration/parity-entry (live-entry))

                       resume
                       (iteration/parity-entry (resume-entry))]

                   ;; The whole regression gate: byte-for-byte equal canonical entries.
                   (expect (= live resume))))
             (it "the prose beside the code and each form's Activity survive both paths"
                 ;; Regression: the resume projection never read `assistant_prose`, so the
                 ;; commentary a reader watched live vanished the moment the turn settled
                 ;; (the terminal refresh swaps in the persisted rows) and after a restart.
                 (let [live
                       (live-entry)

                       resume
                       (resume-entry)

                       activity
                       (activity/from-wire activity-wire)]

                   (expect (some? activity))
                   (expect (= fence-prose (:assistant-prose live) (:assistant-prose resume)))
                   (expect (= activity
                              (:activity (first (:forms live)))
                              (:activity (first (:forms resume)))))))
             (it "the shared fixture yields the expected block-level shape"
                 (let [e (iteration/parity-entry (live-entry))]
                   (expect (= "t7/i3" (:scope e)))
                   (expect (= :ok (:status e)))
                   (expect (= fence-code (:code e)))))
             (it "canonicalize derives one block-level scope/status from the forms"
                 (let [e (iteration/canonicalize (live-entry))]
                   (expect (= "t7/i3" (:scope e)))
                   (expect (= :ok (:status e)))
                   (expect (nil? (:error e))))))

(defdescribe
  gateway-duration-parity-test
  ;; Regression #224: live gateway results carry duration_ms, not an engine envelope.
  (it "preserves measured and missing durations through live and resumed form projections"
      (doseq [durations [[500 700] [0 0] [nil nil] [500 nil]]]
        (let [tracker (progress/make-progress-tracker)
              forms (mapv (fn [index duration]
                            (cond-> {"code" (str "operation_" index "()")}
                              (some? duration)
                              (assoc "duration_ms" duration)))
                          (range)
                          durations)]

          (doseq [[index form] (map-indexed vector forms)]
            ((:on-chunk tracker)
              (#'chat/gateway-event->chunk
               (merge form {"type" "block.output" "iteration" 1 "form_index" index}))))
          (let [live (first ((:get-timeline tracker)))
                resumed (it->iteration-entry {}
                                             {"id" "duration-fixture" "position" 1 "forms" forms})]

            (expect (= durations (mapv :duration-ms (:forms live))))
            (expect (= durations (mapv :duration-ms (:forms resumed)))))))))

(defdescribe formatted-source-parity-test
             ;; Regression #269: `block.started` carries the cached formatted rendering as
             ;; `display_code`; `block.output` repeats only canonical facts. Rebuilding the
             ;; form record from the result frame alone dropped that rendering, so a settled
             ;; block fell back to the raw source the model wrote — two `;`-separated
             ;; statements on one line instead of two.
             (it
               "keeps the formatted display source when the result frame carries none"
               (let [raw
                     "print(cat(\"a.py\")); print(grep({\"query\": [\"x\"]}))"

                     formatted
                     "print(cat(\"a.py\"))\nprint(grep({\"query\": [\"x\"]}))"

                     tracker
                     (progress/make-progress-tracker)]

                 ((:on-chunk tracker)
                   (#'chat/gateway-event->chunk
                    {"type" "block.started"
                     "iteration" 1
                     "form_index" 0
                     "code" raw
                     "display_code" formatted
                     "display_language" "python"}))
                 ((:on-chunk tracker)
                   (#'chat/gateway-event->chunk
                    {"type" "block.output" "iteration" 1 "form_index" 0 "code" raw "stdout" "ok"}))
                 (let [form (first (:forms (first ((:get-timeline tracker)))))]
                   (expect (= formatted (:display-code form)))
                   (expect (= "python" (:display-language form)))
                   (expect (= raw (:code form))))))
             (it "a display field the result frame authors itself still wins"
                 (let [invocation
                       "await shell({\"command\": \"ls\"})"

                       tracker
                       (progress/make-progress-tracker)]

                   ((:on-chunk tracker)
                     (#'chat/gateway-event->chunk
                      {"type" "block.started"
                       "iteration" 1
                       "form_index" 0
                       "code" invocation
                       "display_code" "ls"
                       "display_language" "bash"}))
                   ((:on-chunk tracker)
                     (#'chat/gateway-event->chunk
                      {"type" "block.output"
                       "iteration" 1
                       "form_index" 0
                       "code" invocation
                       "display_code" "ls -l"
                       "display_language" "bash"
                       "stdout" "ok"}))
                   (let [form (first (:forms (first ((:get-timeline tracker)))))]
                     (expect (= "ls -l" (:display-code form)))
                     (expect (= "bash" (:display-language form)))))))
