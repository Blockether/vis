(ns com.blockether.vis.ext.channel-tui.scrollbar-test
  "Pinned contract for the unified scrollbar primitive. Every TUI
   scrollbar — chat, settings, slash menu, file picker, provider/model
   cards, every other dialog — routes through this ns, so painter and
   hit-test layers cannot drift apart. If a test here changes, every
   call site moves with it."
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input MouseAction MouseActionType]))

(deftest geometry-returns-nil-without-overflow
  (testing "total <= inner means nothing to scroll"
    (is (nil? (scrollbar/geometry 10 20 nil)))
    (is (nil? (scrollbar/geometry 20 20 nil)))
    (is (nil? (scrollbar/geometry 100 0 0)))))

(deftest geometry-positions-thumb-by-fraction
  (let [g #(scrollbar/geometry 100 20 %)]
    (testing "scroll=0 → thumb at top" (is (= 0 (:thumb-top-rel (g 0)))))
    (testing "scroll=80 (max) → thumb at bottom of track" (is (= 19 (:thumb-top-rel (g 80)))))
    (testing "nil scroll auto-pins to bottom" (is (= 19 (:thumb-top-rel (g nil)))))
    (testing "scroll=40 → thumb in middle of free track" (is (= 9 (:thumb-top-rel (g 40)))))
    (testing "thumb stays one cell tall (Terminal.app ladder bug)" (is (= 1 (:thumb-h (g 0)))))
    (testing "max-scroll is total - inner" (is (= 80 (:max-scroll (g 0)))))))

(deftest geometry-clamps-out-of-range-scroll
  (let [g #(scrollbar/geometry 100 20 %)]
    (is (zero? (:thumb-top-rel (g -50))))
    (is (= 19 (:thumb-top-rel (g 99999))))))

(deftest geometry-supports-track-h-larger-than-viewport
  ;; Chat messages area: viewport = inner-h, but the visible track
  ;; spans `inner-h + top + bottom` rows so the scrollbar fills the
  ;; whole right gutter without a stranded blank row.
  (let [g (scrollbar/geometry 100 20 30 80)]
    (is (= 30 (:track-h g)))
    (is (= 1 (:thumb-h g)))
    (is (= 29 (:thumb-top-rel g)))))

(defn- mouse [type x y] (MouseAction. type 1 (TerminalPosition. (int x) (int y))))

(defn- mouse-with-count
  [type x y n]
  (MouseAction. type (int n) (TerminalPosition. (int x) (int y))))

(deftest wheel-delta-classifies-mouse-actions
  (testing "SCROLL_UP returns -1, SCROLL_DOWN +1"
    (is (= -1 (scrollbar/wheel-delta (mouse MouseActionType/SCROLL_UP 0 0))))
    (is (= 1 (scrollbar/wheel-delta (mouse MouseActionType/SCROLL_DOWN 0 0)))))
  (testing "Non-wheel mouse events return nil"
    (is (nil? (scrollbar/wheel-delta (mouse MouseActionType/CLICK_DOWN 5 5))))
    (is (nil? (scrollbar/wheel-delta (mouse MouseActionType/DRAG 5 5))))
    (is (nil? (scrollbar/wheel-delta nil)))))

(deftest wheel-step-scales-by-coalesced-count
  (testing "Coalesced wheel count multiplies the delta"
    (is (= -3 (scrollbar/wheel-step (mouse-with-count MouseActionType/SCROLL_UP 0 0 3))))
    (is (= 5 (scrollbar/wheel-step (mouse-with-count MouseActionType/SCROLL_DOWN 0 0 5)))))
  (testing "Single tick stays a single delta"
    (is (= -1 (scrollbar/wheel-step (mouse MouseActionType/SCROLL_UP 0 0))))))

(deftest on-track-and-on-thumb-honour-bar-column
  (let
    [bar
     {:col 50 :top 10 :track-h 8}

     geom
     (scrollbar/geometry 100 8 nil)]

    (testing "On-track requires same column and Y inside track range"
      (is (scrollbar/on-track? 50 10 bar))
      (is (scrollbar/on-track? 50 17 bar))
      (is (not (scrollbar/on-track? 50 18 bar)))
      (is (not (scrollbar/on-track? 49 10 bar))))
    (testing "x-band widens horizontal tolerance left of the bar"
      (let [wide (assoc bar :x-band 3)]
        (is (scrollbar/on-track? 48 10 wide))
        (is (scrollbar/on-track? 49 10 wide))
        (is (scrollbar/on-track? 50 10 wide))
        (is (not (scrollbar/on-track? 47 10 wide)))))
    (testing "On-thumb hits only the thumb's row"
      ;; nil scroll → thumb at row track-h - 1 = 7 (relative), abs row 17.
      (is (scrollbar/on-thumb? 50 17 bar geom))
      (is (not (scrollbar/on-thumb? 50 16 bar geom))))))

(deftest scroll-from-mouse-y-maps-track-to-scroll
  (testing "Track top maps to scroll 0" (is (= 0 (scrollbar/scroll-from-mouse-y 10 10 8 100 20))))
  (testing "Track bottom (mouse-y = top + track-h - 1) maps to max-scroll"
    (is (= 80 (scrollbar/scroll-from-mouse-y 17 10 8 100 20))))
  (testing "Mid-track maps to ~half max-scroll"
    (is (= 46 (scrollbar/scroll-from-mouse-y 14 10 8 100 20))))
  (testing "Grip offset preserves the original click point on drag"
    ;; Drag starts at mouse-y 17 with thumb at row 17 → grip-offset 0,
    ;; then the cursor moves to row 14 → still hits the same fractional
    ;; position the user grabbed because the offset travels with it.
    (is (= 0 (scrollbar/scroll-from-mouse-y 10 10 8 100 20 0)))
    (is (= 80 (scrollbar/scroll-from-mouse-y 17 10 8 100 20 0))))
  (testing "Returns nil when there's no overflow"
    (is (nil? (scrollbar/scroll-from-mouse-y 0 0 8 10 20)))))
