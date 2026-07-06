(ns com.blockether.vis.ext.channel-tui.parity-test
  "PARITY INVARIANT — the regression gate for the TUI trace revamp.

   The live-vs-resume split was the root cause of every TUI regression: the
   live progress tracker and the resume projection used to build subtly
   different shapes, and the renderer papered over the gap with heuristics.

   This test feeds the SAME form fixture through BOTH paths and asserts they
   produce an equal canonical iteration-entry (`iteration/parity-entry`). If
   the two paths ever diverge in scope / merged code / status / duration /
   error, this fails and BLOCKS any UX change."
  (:require [com.blockether.vis.ext.channel-tui.chat]
            [com.blockether.vis.internal.iteration :as iteration]
            [com.blockether.vis.internal.progress :as progress]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private it->iteration-entry
  (var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/it->iteration-entry)))

;; ---------------------------------------------------------------------------
;; Shared fixture: ONE fence that ran `(git/status)` then `(git/add ".")` and
;; printed some output. `:stdout` is the SINGLE display surface both paths
;; carry — that is the whole point; everything downstream must agree.
;; ---------------------------------------------------------------------------

(def ^:private fence-stdout "STATUS  clean\nADD  .")

(def ^:private fence-code "(git/status)\n(git/add \".\")")

(defn- live-entry
  "Drive the live progress tracker with a single `:form-result` chunk that
   carries the fence code + printed output, then read back the canonical
   entry the tracker emits."
  []
  (let [tracker (progress/make-progress-tracker)]
    ((:on-chunk tracker)
      {:phase :form-result
       :iteration 1
       :position 0
       :scope "t7/i3/f1"
       :code fence-code
       :render-segments [{:kind :code :source "(git/status)"}
                         {:kind :code :source "(git/add \".\")"}]
       :stdout fence-stdout
       :result :vis/silent
       :error nil
       :envelope {:started-at-ms 100 :finished-at-ms 912}})
    (first ((:get-timeline tracker)))))

(defn- resume-entry
  "Project a persisted iteration row with one proof envelope carrying the
   shared printed output — the resume counterpart of the live fence."
  []
  (it->iteration-entry {:produced-answer? false :last-iteration-id :other}
                       {:id :it-1
                        :position 1
                        :code fence-code
                        :duration-ms 812
                        :forms [{:scope "t7/i3/f1"
                                 :tag :observation
                                 :src fence-code
                                 :stdout fence-stdout
                                 :duration-ms 812}]}))

(defdescribe iteration-entry-parity-test
             (it "live and resume produce an equal canonical iteration-entry for the same fixture"
                 (let [live
                       (iteration/parity-entry (live-entry))

                       resume
                       (iteration/parity-entry (resume-entry))]

                   ;; The whole regression gate: byte-for-byte equal canonical entries.
                   (expect (= live resume))))
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
