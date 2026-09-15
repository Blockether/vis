(ns com.blockether.vis.tui.scroll-test
  "Contract for the messages-area scroll state, with focus on
   `scrolled-up?` — the predicate that drives input-cursor hiding so the
   terminal blink does not jump around while the transcript scrolls."
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.tui.scroll :as scroll]))

(deftest scrolled-up?-true-only-when-parked
  (testing ":at intent (user parked above the bottom, reading history)"
    (is (true? (scroll/scrolled-up? {:mode :at :offset 0})))
    (is (true? (scroll/scrolled-up? {:mode :at :offset 12})))
    (is (true? (scroll/scrolled-up? (scroll/parked 7))))
    (testing "mid-ease toward a parked row still counts as scrolled up"
      (is (true? (scroll/scrolled-up? {:mode :at :offset 12 :pos 30})))))
  (testing ":follow intent (tracking the live bottom) is NOT scrolled up"
    (is (false? (scroll/scrolled-up? scroll/follow)))
    (is (false? (scroll/scrolled-up? {:mode :follow})))
    (testing "follow mid-ease (pos pinned) is still following, not scrolled up"
      (is (false? (scroll/scrolled-up? {:mode :follow :pos 5})))))
  (testing "missing/legacy scroll defaults to FOLLOW ⇒ not scrolled up"
    (is (false? (scroll/scrolled-up? nil)))
    (is (false? (scroll/scrolled-up? {})))
    (is (false? (scroll/scrolled-up? :garbage)))))

(deftest scrolled-up?-tracks-scroll-transitions
  (let [max-s 100]
    (testing "scrolling UP from follow parks the view ⇒ scrolled up"
      (is (true? (scroll/scrolled-up? (scroll/up scroll/follow 10 max-s)))))
    (testing "scrolling DOWN back to the bottom re-arms follow ⇒ not scrolled up"
      (let [parked (scroll/up scroll/follow 10 max-s)]
        (is (false? (scroll/scrolled-up? (scroll/down parked 1000 max-s))))))
    (testing "dragging the scrollbar to the very bottom re-enters follow"
      (is (false? (scroll/scrolled-up? (scroll/to-y max-s max-s))))
      (is (true? (scroll/scrolled-up? (scroll/to-y 5 max-s)))))))

(deftest bottom-hidden?-only-when-content-is-below
  ;; The `↓ latest` chip gates on this, NOT `scrolled-up?`. Regression: an empty
  ;; session (max-s 0) where a PageUp parked `:at` offset 0 popped the chip even
  ;; though there was nothing to scroll to.
  (testing "nothing overflows (empty/short session, max-s 0) ⇒ never hidden-below"
    (is (false? (scroll/bottom-hidden? scroll/follow 0)))
    ;; PageUp in an empty session parks :at offset 0 — STILL nothing below.
    (is (false? (scroll/bottom-hidden? (scroll/up scroll/follow 10 0) 0)))
    (is (false? (scroll/bottom-hidden? {:mode :at :offset 0} 0))))
  (testing "content overflows and the view is parked ABOVE the bottom ⇒ bottom hidden"
    (is (true? (scroll/bottom-hidden? (scroll/parked 0) 100)))
    (is (true? (scroll/bottom-hidden? (scroll/parked 40) 100))))
  (testing "following, or parked AT the bottom ⇒ not hidden (chip stays away)"
    (is (false? (scroll/bottom-hidden? scroll/follow 100)))
    (is (false? (scroll/bottom-hidden? (scroll/parked 100) 100)))
    (is (false? (scroll/bottom-hidden? (scroll/parked 999) 100)))))

;; ── Turn completion ────────────────────────────────────────────────────────

;; Regression (TUI "when a turn ends the content reflows, as if it scrolled"):
;; completion used to re-pin FOLLOW to the OLD tail plus a `:reveal-from`
;; marker, so `layout-offset` returned a concrete row and the next frames EASED
;; down to the newly measured bottom — a multi-frame scroll after every turn —
;; while a reader parked in history was dragged back to the live edge.

(deftest settle-locks-follow-to-the-bottom-without-easing
  (let [old-max
        100

        new-max
        140

        settled
        (scroll/settle {:mode :follow :pos old-max})]

    (testing "FOLLOW lands on the exact auto-bottom lock, at any new height"
      (is (= scroll/follow settled))
      (is (nil? (scroll/layout-offset settled old-max)))
      (is (nil? (scroll/layout-offset settled new-max)))
      (is (false? (scroll/animating? settled new-max))))
    (testing "and the next render frame leaves it there — nothing to animate"
      (let [stepped (scroll/ease settled new-max)]
        (is (nil? (scroll/layout-offset stepped new-max)))
        (is (false? (scroll/animating? stepped new-max)))))
    (testing "a reader parked above the bottom keeps their exact row"
      (is (= (scroll/parked 40) (scroll/settle (scroll/parked 40))))
      (is (= {:mode :at :offset 40 :pos 90} (scroll/settle {:mode :at :offset 40 :pos 90}))))
    (testing "missing/legacy scroll settles to FOLLOW" (is (= scroll/follow (scroll/settle nil))))))

;; Reported in Vis session 22b3489b-336f-42d0-9bc8-806dff2de86f: the live band scrolled
;; one row per wheel row while the transcript beside it scrolled three.
(deftest wheel-step-scales-with-the-surface
  (testing "a surface no paint has measured yet moves one row per wheel row"
    (is (= 1 (scroll/wheel-step nil)))
    (is (= 1 (scroll/wheel-step 0))))
  (testing "a compact table keeps terminal-row granularity"
    (is (= 1 (scroll/wheel-step 4)))
    (is (= 2 (scroll/wheel-step 8))))
  (testing "a surface tall enough reaches the shared notch and stops there"
    (is (= scroll/wheel-step-rows (scroll/wheel-step 12)))
    (is (= scroll/wheel-step-rows (scroll/wheel-step 200)))))

;; ── Jump-to-bottom chip visibility ────────────────────────────────────
(deftest jump-chip-visible?-requires-a-real-park
  (testing "parked above the bottom with content below ⇒ chip shows"
    (is (true? (scroll/jump-chip-visible? (scroll/parked 40) 100)))
    (is (true? (scroll/jump-chip-visible? {:mode :at :offset 12 :pos 30} 100))))
  (testing
    "FOLLOW easing during streaming (eased :pos trails the growing bottom)
            must NOT flash the chip — the user never left the bottom"
    ;; regression: gating on bottom-hidden? alone painted the chip every frame
    ;; a stream grew content while the follow ease trailed a few rows behind.
    (is (false? (scroll/jump-chip-visible? {:mode :follow :pos 90} 100)))
    (is (false? (scroll/jump-chip-visible? scroll/follow 100))))
  (testing "empty/short session parked :at offset 0 ⇒ nothing below, no chip"
    (is (false? (scroll/jump-chip-visible? (scroll/up scroll/follow 10 0) 0)))
    (is (false? (scroll/jump-chip-visible? {:mode :at :offset 0} 0))))
  (testing "parked AT (or past) the live bottom ⇒ no chip"
    (is (false? (scroll/jump-chip-visible? (scroll/parked 100) 100)))
    (is (false? (scroll/jump-chip-visible? (scroll/parked 999) 100)))))

;; #248: proximity to a changing live bottom must not replace parked intent.
(deftest down-keeps-a-near-bottom-reader-parked
  (doseq [max-s [1203 1205]]
    (let [sc (scroll/down (scroll/parked 1197) 3 max-s)]
      (is (= :at (:mode sc)))
      (is (= 1200 (scroll/desired sc max-s)))
      (is (= 1200 (scroll/desired sc 3099)))
      (is (scroll/jump-chip-visible? sc max-s)))))

(deftest parked-intent-survives-live-height-changes
  (let [sc (scroll/down (scroll/parked 1197) 3 1203)]
    (doseq [max-s [1203 3099 1200 3099]]
      (let [eased (nth (iterate #(scroll/ease % max-s) sc) 20)]
        (is (= :at (:mode eased)))
        (is (= 1200 (scroll/desired eased max-s)))
        (is (= 1200 (scroll/displayed eased max-s)))
        (is (= eased (scroll/settle eased)))))
    (testing "an anchor correction preserves parked intent even at the new bottom"
      (let [anchored (scroll/reanchor sc 3096 1899)]
        (is (= :at (:mode anchored)))
        (is (= 3099 (scroll/desired anchored 3099)))
        (is (= 3099 (scroll/desired anchored 3200)))))))

(deftest down-rearms-follow-only-on-reaching-the-bottom
  (doseq [amount [6 7 1000]]
    (let [sc (scroll/down (scroll/parked 1197) amount 1203)]
      (is (= :follow (:mode sc)))
      (is (= 1203 (scroll/desired sc 1203)))
      (is (= 3099 (scroll/desired sc 3099))))))

(deftest lazy-prepend-preserves-parked-scroll-and-animation
  (let [sc
        (scroll/down (scroll/parked 1197) 3 1203)

        shifted
        (scroll/shift-prepended sc 1896)]

    (is (= :at (:mode shifted)))
    (is (= 3096 (scroll/desired shifted 3099)))
    (is (= 3093 (scroll/displayed shifted 3099)))
    (is (= scroll/follow (scroll/shift-prepended scroll/follow 1896)))))
