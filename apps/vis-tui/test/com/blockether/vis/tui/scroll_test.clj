(ns com.blockether.vis.tui.scroll-test
  "Contract for the messages-area scroll state, with focus on
   `scrolled-up?` — the predicate that drives input-cursor hiding so the
   terminal blink does not jump around while the transcript scrolls."
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.tui.scroll :as scroll]))

(defdescribe scrolled-up?-true-only-when-parked
             (it ":at intent (user parked above the bottom, reading history)"
                 (expect (true? (scroll/scrolled-up? {:mode :at :offset 0})))
                 (expect (true? (scroll/scrolled-up? {:mode :at :offset 12})))
                 (expect (true? (scroll/scrolled-up? (scroll/parked 7))))
                 ;; mid-ease toward a parked row still counts as scrolled up
                 (expect (true? (scroll/scrolled-up? {:mode :at :offset 12 :pos 30}))))
             (it ":follow intent (tracking the live bottom) is NOT scrolled up"
                 (expect (false? (scroll/scrolled-up? scroll/follow)))
                 (expect (false? (scroll/scrolled-up? {:mode :follow})))
                 ;; follow mid-ease (pos pinned) is still following, not scrolled up
                 (expect (false? (scroll/scrolled-up? {:mode :follow :pos 5}))))
             (it "missing/legacy scroll defaults to FOLLOW ⇒ not scrolled up"
                 (expect (false? (scroll/scrolled-up? nil)))
                 (expect (false? (scroll/scrolled-up? {})))
                 (expect (false? (scroll/scrolled-up? :garbage)))))

(defdescribe scrolled-up?-tracks-scroll-transitions
             (it "scrolled up? tracks scroll transitions"
                 (let [max-s 100]
                   ;; scrolling UP from follow parks the view ⇒ scrolled up
                   (expect (true? (scroll/scrolled-up? (scroll/up scroll/follow 10 max-s))))
                   ;; scrolling DOWN back to the bottom re-arms follow ⇒ not scrolled up
                   (let [parked (scroll/up scroll/follow 10 max-s)]
                     (expect (false? (scroll/scrolled-up? (scroll/down parked 1000 max-s)))))
                   ;; dragging the scrollbar to the very bottom re-enters follow
                   (expect (false? (scroll/scrolled-up? (scroll/to-y max-s max-s))))
                   (expect (true? (scroll/scrolled-up? (scroll/to-y 5 max-s)))))))

(defdescribe bottom-hidden?-only-when-content-is-below
             ;; The `↓ latest` chip gates on this, NOT `scrolled-up?`. Regression: an empty
             ;; session (max-s 0) where a PageUp parked `:at` offset 0 popped the chip even
             ;; though there was nothing to scroll to.
             (it "nothing overflows (empty/short session, max-s 0) ⇒ never hidden-below"
                 (expect (false? (scroll/bottom-hidden? scroll/follow 0)))
                 ;; PageUp in an empty session parks :at offset 0 — STILL nothing below.
                 (expect (false? (scroll/bottom-hidden? (scroll/up scroll/follow 10 0) 0)))
                 (expect (false? (scroll/bottom-hidden? {:mode :at :offset 0} 0))))
             (it "content overflows and the view is parked ABOVE the bottom ⇒ bottom hidden"
                 (expect (true? (scroll/bottom-hidden? (scroll/parked 0) 100)))
                 (expect (true? (scroll/bottom-hidden? (scroll/parked 40) 100))))
             (it "following, or parked AT the bottom ⇒ not hidden (chip stays away)"
                 (expect (false? (scroll/bottom-hidden? scroll/follow 100)))
                 (expect (false? (scroll/bottom-hidden? (scroll/parked 100) 100)))
                 (expect (false? (scroll/bottom-hidden? (scroll/parked 999) 100)))))

;; ── Turn completion ────────────────────────────────────────────────────────

;; Regression (TUI "when a turn ends the content reflows, as if it scrolled"):
;; completion used to re-pin FOLLOW to the OLD tail plus a `:reveal-from`
;; marker, so `layout-offset` returned a concrete row and the next frames EASED
;; down to the newly measured bottom — a multi-frame scroll after every turn —
;; while a reader parked in history was dragged back to the live edge.

(defdescribe settle-locks-follow-to-the-bottom-without-easing
             (it "settle locks follow to the bottom without easing"
                 (let [old-max
                       100

                       new-max
                       140

                       settled
                       (scroll/settle {:mode :follow :pos old-max})]

                   ;; FOLLOW lands on the exact auto-bottom lock, at any new height
                   (expect (= scroll/follow settled))
                   (expect (nil? (scroll/layout-offset settled old-max)))
                   (expect (nil? (scroll/layout-offset settled new-max)))
                   (expect (false? (scroll/animating? settled new-max)))
                   ;; and the next render frame leaves it there — nothing to animate
                   (let [stepped (scroll/ease settled new-max)]
                     (expect (nil? (scroll/layout-offset stepped new-max)))
                     (expect (false? (scroll/animating? stepped new-max))))
                   ;; a reader parked above the bottom keeps their exact row
                   (expect (= (scroll/parked 40) (scroll/settle (scroll/parked 40))))
                   (expect (= {:mode :at :offset 40 :pos 90}
                              (scroll/settle {:mode :at :offset 40 :pos 90})))
                   ;; missing/legacy scroll settles to FOLLOW
                   (expect (= scroll/follow (scroll/settle nil))))))

;; Reported in Vis session 22b3489b-336f-42d0-9bc8-806dff2de86f: the live band scrolled
;; one row per wheel row while the transcript beside it scrolled three.
(defdescribe wheel-step-scales-with-the-surface
             (it "a surface no paint has measured yet moves one row per wheel row"
                 (expect (= 1 (scroll/wheel-step nil)))
                 (expect (= 1 (scroll/wheel-step 0))))
             (it "a compact table keeps terminal-row granularity"
                 (expect (= 1 (scroll/wheel-step 4)))
                 (expect (= 2 (scroll/wheel-step 8))))
             (it "a surface tall enough reaches the shared notch and stops there"
                 (expect (= scroll/wheel-step-rows (scroll/wheel-step 12)))
                 (expect (= scroll/wheel-step-rows (scroll/wheel-step 200)))))

;; ── Jump-to-bottom chip visibility ────────────────────────────────────
(defdescribe
  jump-chip-visible?-requires-a-real-park
  (it "parked above the bottom with content below ⇒ chip shows"
      (expect (true? (scroll/jump-chip-visible? (scroll/parked 40) 100)))
      (expect (true? (scroll/jump-chip-visible? {:mode :at :offset 12 :pos 30} 100))))
  (it
    "FOLLOW easing during streaming (eased :pos trails the growing bottom)
            must NOT flash the chip — the user never left the bottom"
    ;; regression: gating on bottom-hidden? alone painted the chip every frame
    ;; a stream grew content while the follow ease trailed a few rows behind.
    (expect (false? (scroll/jump-chip-visible? {:mode :follow :pos 90} 100)))
    (expect (false? (scroll/jump-chip-visible? scroll/follow 100))))
  (it "empty/short session parked :at offset 0 ⇒ nothing below, no chip"
      (expect (false? (scroll/jump-chip-visible? (scroll/up scroll/follow 10 0) 0)))
      (expect (false? (scroll/jump-chip-visible? {:mode :at :offset 0} 0))))
  (it "parked AT (or past) the live bottom ⇒ no chip"
      (expect (false? (scroll/jump-chip-visible? (scroll/parked 100) 100)))
      (expect (false? (scroll/jump-chip-visible? (scroll/parked 999) 100)))))

;; #248: proximity to a changing live bottom must not replace parked intent.
(defdescribe down-keeps-a-near-bottom-reader-parked
             (it "down keeps a near bottom reader parked"
                 (doseq [max-s [1203 1205]]
                   (let [sc (scroll/down (scroll/parked 1197) 3 max-s)]
                     (expect (= :at (:mode sc)))
                     (expect (= 1200 (scroll/desired sc max-s)))
                     (expect (= 1200 (scroll/desired sc 3099)))
                     (expect (scroll/jump-chip-visible? sc max-s))))))

(defdescribe parked-intent-survives-live-height-changes
             (it "parked intent survives live height changes"
                 (let [sc (scroll/down (scroll/parked 1197) 3 1203)]
                   (doseq [max-s [1203 3099 1200 3099]]
                     (let [eased (nth (iterate #(scroll/ease % max-s) sc) 20)]
                       (expect (= :at (:mode eased)))
                       (expect (= 1200 (scroll/desired eased max-s)))
                       (expect (= 1200 (scroll/displayed eased max-s)))
                       (expect (= eased (scroll/settle eased)))))
                   ;; an anchor correction preserves parked intent even at the new bottom
                   (let [anchored (scroll/reanchor sc 3096 1899)]
                     (expect (= :at (:mode anchored)))
                     (expect (= 3099 (scroll/desired anchored 3099)))
                     (expect (= 3099 (scroll/desired anchored 3200)))))))

(defdescribe down-rearms-follow-only-on-reaching-the-bottom
             (it "down rearms follow only on reaching the bottom"
                 (doseq [amount [6 7 1000]]
                   (let [sc (scroll/down (scroll/parked 1197) amount 1203)]
                     (expect (= :follow (:mode sc)))
                     (expect (= 1203 (scroll/desired sc 1203)))
                     (expect (= 3099 (scroll/desired sc 3099)))))))

(defdescribe lazy-prepend-preserves-parked-scroll-and-animation
             (it "lazy prepend preserves parked scroll and animation"
                 (let [sc
                       (scroll/down (scroll/parked 1197) 3 1203)

                       shifted
                       (scroll/shift-prepended sc 1896)]

                   (expect (= :at (:mode shifted)))
                   (expect (= 3096 (scroll/desired shifted 3099)))
                   (expect (= 3093 (scroll/displayed shifted 3099)))
                   (expect (= scroll/follow (scroll/shift-prepended scroll/follow 1896))))))
