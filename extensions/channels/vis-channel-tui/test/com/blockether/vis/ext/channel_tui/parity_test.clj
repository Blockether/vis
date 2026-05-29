(ns com.blockether.vis.ext.channel-tui.parity-test
  "PARITY INVARIANT — the regression gate for the TUI BLOCK revamp.

   The live-vs-resume split was the root cause of every TUI regression: the
   live progress tracker and the resume projection used to build subtly
   different shapes, and the renderer papered over the gap with heuristics.

   This test feeds the SAME tool-call fixture through BOTH paths and asserts
   they produce an equal canonical iteration-entry (`iteration/parity-entry`).
   If the two paths ever diverge in scope / merged code / ops / status /
   counts / error, this fails and BLOCKS any UX change."
  (:require
   [com.blockether.vis.ext.channel-tui.chat :as chat]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.iteration :as iteration]
   [com.blockether.vis.internal.progress :as progress]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private it->iteration-entry
  (var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/it->iteration-entry)))

;; ---------------------------------------------------------------------------
;; Shared fixture: ONE fence that ran `(git/status)` then `(git/add ".")`.
;; The channel sink slice is identical for both paths — that is the whole
;; point; everything downstream must agree.
;; ---------------------------------------------------------------------------

(defn- ok-sink-entry
  [position op tag summary-label]
  {:position position
   :form     (str "(" (name op) ")")
   :symbol   op
   :op       op
   :tag      tag
   :success? true
   :error    nil
   :result   {:summary (extension/ir-root
                         (extension/ir-p (extension/ir-strong summary-label)))
              :display (extension/ir-root
                         (extension/ir-p (extension/ir-strong summary-label)
                           "  detail"))}})

(def ^:private fence-channel
  [(ok-sink-entry 0 :git/status :observation "STATUS")
   (ok-sink-entry 1 :git/add    :mutation    "ADD")])

(def ^:private fence-code
  "(git/status)\n(git/add \".\")")

(defn- live-entry
  "Drive the live progress tracker with a single `:form-result` chunk that
   carries the fence code + the shared channel slice, then read back the
   canonical entry the tracker emits."
  []
  (let [tracker (progress/make-progress-tracker)]
    ((:on-chunk tracker)
     {:phase           :form-result
      :iteration-count 1
      :position        0
      :scope           "t7/i3/f1"
      :code            fence-code
      :render-segments [{:kind :code :source "(git/status)"}
                        {:kind :code :source "(git/add \".\")"}]
      :channel         fence-channel
      :result          :vis/silent
      :error           nil
      :envelope        {:started-at-ms 100 :finished-at-ms 912}})
    (first ((:get-timeline tracker)))))

(defn- resume-entry
  "Project a persisted iteration row with one proof envelope carrying the
   shared channel slice — the resume counterpart of the live fence."
  []
  (it->iteration-entry
    {:produced-answer? false :last-iteration-id :other}
    {:id          :it-1
     :position    1
     :code        fence-code
     :duration-ms 812
     :forms       [{:scope   "t7/i3/f1"
                    :tag     :observation
                    :src     fence-code
                    :channel fence-channel
                    :duration-ms 812}]}))

(defdescribe iteration-entry-parity-test
  (it "live and resume produce an equal canonical iteration-entry for the same fixture"
    (let [live   (iteration/parity-entry (live-entry))
          resume (iteration/parity-entry (resume-entry))]
      ;; The whole regression gate: byte-for-byte equal canonical entries.
      (expect (= live resume))))

  (it "the shared fixture yields the expected block-level shape"
    (let [e (iteration/parity-entry (live-entry))]
      (expect (= "t7/i3" (:scope e)))
      (expect (= :ok (:status e)))
      (expect (= {:observations 1 :mutations 1}
                (iteration/op-counts (:ops e))))
      (expect (= [:git/status :git/add] (mapv :op (:ops e))))
      (expect (= [:observation :mutation] (mapv :tag (:ops e))))))

  (it "iteration-entry->display-block emits one block (not one per op) with real counts"
    (let [block (iteration/iteration-entry->display-block (live-entry))]
      (expect (= "t7/i3" (:scope block)))
      (expect (= 2 (count (:ops block))))
      (expect (= {:observations 1 :mutations 1} (:counts block)))
      (expect (= :ok (:status block))))))
