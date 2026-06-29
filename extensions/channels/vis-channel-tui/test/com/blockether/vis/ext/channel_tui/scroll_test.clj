(ns com.blockether.vis.ext.channel-tui.scroll-test
  "Contract for the messages-area scroll state, with focus on
   `scrolled-up?` — the predicate that drives input-cursor hiding so the
   terminal blink does not jump around while the transcript scrolls."
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]))

(deftest scrolled-up?-true-only-when-parked
  (testing ":at intent (user parked above the bottom, reading history)"
    (is (true? (scroll/scrolled-up? {:mode :at, :offset 0})))
    (is (true? (scroll/scrolled-up? {:mode :at, :offset 12})))
    (is (true? (scroll/scrolled-up? (scroll/parked 7))))
    (testing "mid-ease toward a parked row still counts as scrolled up"
      (is (true? (scroll/scrolled-up? {:mode :at, :offset 12, :pos 30})))))

  (testing ":follow intent (tracking the live bottom) is NOT scrolled up"
    (is (false? (scroll/scrolled-up? scroll/follow)))
    (is (false? (scroll/scrolled-up? {:mode :follow})))
    (testing "follow mid-ease (pos pinned) is still following, not scrolled up"
      (is (false? (scroll/scrolled-up? {:mode :follow, :pos 5})))))

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
