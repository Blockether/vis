(ns com.blockether.vis.internal.plan-review-test
  (:require
   [com.blockether.vis.internal.plan-review :as pr]
   [lazytest.core :refer [defdescribe expect it]]))

;; =============================================================================
;; plan-review — the channel-neutral structured-review surface. Pure fns:
;; reviewable-plan (ordered rows), review-pending? (gate), review->message
;; (the ONE canonical `Plan review:` chat message both channels compile to).
;; =============================================================================

(def ^:private tasks
  "A `:session/tasks`-shaped map: two candidates + one accepted plan step
   (out of :order on purpose) + a non-plan hook task that must never leak
   into the review surface."
  {:wire_tui  {:title "Wire the TUI" :status :candidate :order 2 :plan? true
               :acceptance "F7 opens the dialog"}
   :core_fns  {:title "Shared core" :status :todo :order 1 :plan? true}
   :web_card  {:title "Web card" :status :candidate :order 3 :plan? true}
   :hook_task {:title "hook" :status :todo}})

(defdescribe reviewable-plan-test
  (it "returns plan steps only, sorted by :order, keys as strings"
    (let [rows (pr/reviewable-plan tasks)]
      (expect (= ["core_fns" "wire_tui" "web_card"] (mapv :key rows)))
      (expect (= [false true true] (mapv :candidate? rows)))
      ;; acceptance rides along when present, nil otherwise
      (expect (= "F7 opens the dialog" (:acceptance (nth rows 1))))
      (expect (nil? (:acceptance (nth rows 0))))))
  (it "is empty for no tasks / no plan steps"
    (expect (= [] (pr/reviewable-plan {})))
    (expect (= [] (pr/reviewable-plan nil)))
    (expect (= [] (pr/reviewable-plan {:h {:title "hook" :status :todo}})))))

(defdescribe review-pending?-test
  (it "true only when a :plan? step is :candidate"
    (expect (true? (pr/review-pending? tasks)))
    (expect (false? (pr/review-pending? (dissoc tasks :wire_tui :web_card))))
    ;; a non-plan task in :candidate (cannot happen, but) must not gate
    (expect (false? (pr/review-pending? {:h {:title "x" :status :candidate}})))
    (expect (false? (pr/review-pending? nil)))))

(defdescribe review->message-test
  (it "renders the canonical line grammar with em-dash notes"
    (expect (= (str "Plan review:\n"
                 "- core_fns: APPROVE\n"
                 "- wire_tui: REJECT — too invasive\n"
                 "- web_card: COMMENT — also collapse the card after submit\n"
                 "Overall: looks close")
              (pr/review->message
                [{:key "core_fns" :verdict :approve}
                 {:key "wire_tui" :verdict :reject :note "too invasive"}
                 {:key "web_card" :verdict :comment
                  :note "also collapse the card after submit"}]
                "looks close"))))
  (it "accepts string verdict names in any case"
    (expect (= "Plan review:\n- a: APPROVE\n- b: REJECT"
              (pr/review->message [{:key "a" :verdict "APPROVE"}
                                   {:key "b" :verdict "reject"}] nil))))
  (it "collapses multiline notes onto one line (line-oriented grammar)"
    (expect (= "Plan review:\n- a: COMMENT — split it into two"
              (pr/review->message
                [{:key "a" :verdict :comment :note "split it\ninto two"}] nil))))
  (it "drops entries that say nothing"
    ;; empty comment, blank key, unknown/missing verdict — all dropped
    (expect (= "Plan review:\n- a: APPROVE"
              (pr/review->message
                [{:key "a" :verdict :approve}
                 {:key "b" :verdict :comment :note "   "}
                 {:key "  " :verdict :approve}
                 {:key "c" :verdict :bogus}
                 {:key "d"}]
                nil))))
  (it "overall-only review still renders; empty review is nil"
    (expect (= "Plan review:\nOverall: just do it"
              (pr/review->message [] "just do it")))
    (expect (nil? (pr/review->message [] nil)))
    (expect (nil? (pr/review->message [] "  ")))
    (expect (nil? (pr/review->message
                    [{:key "a" :verdict :comment :note " "}] nil)))))
