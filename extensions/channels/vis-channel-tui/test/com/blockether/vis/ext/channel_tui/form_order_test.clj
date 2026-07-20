(ns com.blockether.vis.ext.channel-tui.form-order-test
  "Guard: the LIVE form order and the RESTORED form order never diverge.

   Forms run CONCURRENTLY (`await gather(...)`), so their results stream back
   OUT OF ORDER; the live progress tracker re-slots them by `:position`. The DB
   stores the forms as ONE ordered vector and restore re-derives order from that
   vector (`map-indexed`). Both must land on the SAME loop-emission order — a
   silent form filtered the same on both sides. If they ever drift (a slotting
   regression, a restore that trusts a stale stored position, an elision that
   only one path does), this fails CI instead of looking subtly wrong in a bubble."
  (:require [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.internal.progress :as progress]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private it->iteration-entry @#'chat/it->iteration-entry)

(defn- codes [entry] (mapv :code (:forms entry)))

;; Three forms the model emitted in loop order f0,f1,f2 — f1 is a silent
;; (engine-chrome) form. Loop/emission order is the canonical order.
(def ^:private loop-codes ["(f0)" "(f1-silent)" "(f2)"])

(defdescribe
  live-vs-restore-order-test
  (it "LIVE re-slots out-of-order arrivals back into loop order (not arrival order)"
      (let
        [{:keys [on-chunk get-timeline]}
         (progress/make-progress-tracker)

         chunk
         (fn [pos code silent?]
           {:phase :form-result
            :iteration 1
            :position pos
            :code code
            :result (str "r" pos)
            :silent? silent?})]

        ;; results land 2, 0, 1 — the WRONG order on the wire
        (on-chunk (chunk 2 "(f2)" false))
        (on-chunk (chunk 0 "(f0)" false))
        (on-chunk (chunk 1 "(f1-silent)" true))
        (let [live (first (get-timeline))]
          ;; slotted by position back into loop order, NOT [f2 f0 f1]
          (expect (= loop-codes (codes live))))))
  (it "RESTORE keeps the DB forms-vector order"
      ;; The DB persists one ordered :forms vector — restore re-derives order from
      ;; it (map-indexed), never from a stored position field.
      (let
        [it
         {"id" "t1"
          "code" ""
          "forms" [{"src" "(f0)" "result" "r0"} {"src" "(f1-silent)" "result" "r1" "silent" true}
                   {"src" "(f2)" "result" "r2"}]}

         restored
         (it->iteration-entry {:produced-answer? false :last-iteration-id nil} it)]

        (expect (= loop-codes (codes restored)))))
  (it "LIVE order == RESTORE order == loop order (no divergence)"
      (let
        [{:keys [on-chunk get-timeline]}
         (progress/make-progress-tracker)

         chunk
         (fn [pos code silent?]
           {:phase :form-result
            :iteration 1
            :position pos
            :code code
            :result (str "r" pos)
            :silent? silent?})]

        (on-chunk (chunk 2 "(f2)" false))
        (on-chunk (chunk 0 "(f0)" false))
        (on-chunk (chunk 1 "(f1-silent)" true))
        (let
          [live
           (codes (first (get-timeline)))

           it
           {"id" "t1"
            "code" ""
            "forms" [{"src" "(f0)" "result" "r0"} {"src" "(f1-silent)" "result" "r1" "silent" true}
                     {"src" "(f2)" "result" "r2"}]}

           restored
           (codes (it->iteration-entry {:produced-answer? false :last-iteration-id nil} it))]

          (expect (= loop-codes live))
          (expect (= loop-codes restored))
          (expect (= live restored))))))
