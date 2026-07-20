(ns com.blockether.vis.ext.channel-tui.scroll-test
  "Contract for the messages-area scroll state, with focus on
   `scrolled-up?` — the predicate that drives input-cursor hiding so the
   terminal blink does not jump around while the transcript scrolls."
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]))

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

;; ── Wheel-momentum smoother ────────────────────────────────────────────────
;; The input-side debouncer that stops a slow trackpad's sign-flipping inertia
;; tail from bouncing the viewport. Drives the bounce-repro stream end-to-end
;; so a regression here would surface as a non-monotonic dispatched sequence.
(defn- drive-stream
  "Feed `raws` through `merge-wheel-delta` from rest momentum, returning the
   sequence of non-nil effective deltas (what actually dispatches)."
  [raws]
  (loop
    [rs
     raws

     m
     0

     out
     []]

    (if (empty? rs)
      out
      (let [[nm eff] (scroll/merge-wheel-delta m (first rs))]
        (recur (rest rs) nm (if eff (conj out eff) out))))))

(deftest merge-wheel-delta-passes-clean-streams-through
  (testing "a clean same-direction drag dispatches every tick verbatim"
    (is (= [1 1 1 1 1] (drive-stream [1 1 1 1 1]))))
  (testing "a solo tap from rest dispatches immediately (responsive)"
    (is (= [3] (drive-stream [3]))))
  (testing "a zero delta is a no-op that preserves momentum" (is (= [2] (drive-stream [2 0])))))

(deftest merge-wheel-delta-absorbs-inertia-tail-reversals
  (testing "the bounce repro: an up run with a sign-flipping tail stays monotonic"
    (let [eff (drive-stream [3 2 1 1 1 -1 1 -1 1])]
      ;; every dispatched tick is positive (same direction) — no reversal leaked
      (is (every? pos? eff))
      ;; the three stray -1 ticks are absorbed, so only the genuine ups dispatch
      (is (= [3 2 1 1 1 1 1] eff)))))

(deftest merge-wheel-delta-lets-a-strong-reversal-through
  (testing "an opposing delta larger than remaining momentum re-seeds and dispatches the net"
    ;; +3 +3 builds momentum +6 (down); a -8 (up) then overwhelms: 6 cancels,
    ;; -2 net leftover re-seeds up and dispatches (not the full -8 — that would
    ;; double-count the cancelled portion).
    (let [eff (drive-stream [3 3 -8])]
      (is (= [3 3 -2] eff))))
  (testing "an exact cancellation swallows the tick and zeroes momentum"
    (let [[m eff] (scroll/merge-wheel-delta 3 -3)]
      (is (zero? m))
      (is (nil? eff)))))

(deftest merge-wheel-delta-caps-runaway-momentum
  (testing "a very long continuous drag can't accumulate unbounded stickiness"
    ;; twenty +1 ticks: dispatches all pass through, but momentum is capped
    (let
      [final-mom
       (loop
         [rs (repeat 20 1)
          m 0]

         (if (empty? rs) m (recur (rest rs) (first (scroll/merge-wheel-delta m (first rs))))))]
      (is (<= final-mom scroll/momentum-cap)))))

(deftest decay-wheel-momentum-is-time-based
  (testing "zero momentum stays zero at any idle time"
    (is (zero? (scroll/decay-wheel-momentum 0 0)))
    (is (zero? (scroll/decay-wheel-momentum 0 999))))
  (testing "momentum is held (verbatim) immediately after a wheel event"
    (is (= 10 (scroll/decay-wheel-momentum 10 0)))
    (is (= -10 (scroll/decay-wheel-momentum -10 0))))
  (testing "inside the hold window the lock NEVER fully releases (sign floor)"
    ;; the whole point: a slow trackpad tail leaves only ±1-2 of momentum and
    ;; events arrive 50-100ms apart — the directional lock must survive that
    ;; gap or the tail's sign-flipped tick dispatches as a real reversal.
    (is (= -1 (scroll/decay-wheel-momentum -1 100)))
    (is (= 1 (scroll/decay-wheel-momentum 1 149)))
    (is (neg? (scroll/decay-wheel-momentum -12 100))))
  (testing "momentum shrinks with idle time (deliberate reversals stay responsive)"
    (is (< (Math/abs (long (scroll/decay-wheel-momentum 12 100)))
           (Math/abs (long (scroll/decay-wheel-momentum 12 20))))))
  (testing "the window's expiry releases the lock exactly to zero"
    (is (zero? (scroll/decay-wheel-momentum 12 scroll/momentum-hold-ms)))
    (is (zero? (scroll/decay-wheel-momentum -12 99999)))))

(defn- drive-timed-stream
  "Feed `[raw gap-ms]` wheel events through USE-time decay + merge exactly as
   the input loop does: momentum decays by the wall-clock gap since the
   previous event BEFORE each merge. Returns the dispatched effective deltas."
  [events]
  (loop
    [es
     events

     m
     0

     out
     []]

    (if (empty? es)
      out
      (let
        [[raw gap]
         (first es)

         [nm eff]
         (scroll/merge-wheel-delta (scroll/decay-wheel-momentum m gap) raw)]

        (recur (rest es) nm (if eff (conj out eff) out))))))

(deftest slow-trackpad-inertia-tail-cannot-bounce
  (testing
    "the streaming bounce repro: SPARSE up ticks (50-90ms apart, slower than
            any poll-based decay window) ending in a sign-flipped tick — the flip is
            absorbed, so no :scroll-down can re-arm FOLLOW under the user"
    (let [eff (drive-timed-stream [[-1 0] [-1 80] [-1 80] [-1 90] [1 60]])]
      (is (every? neg? eff))
      (is (= [-1 -1 -1 -1] eff))))
  (testing "a deliberate reversal AFTER the hold window dispatches immediately"
    (is (= [-1 -1 1] (drive-timed-stream [[-1 0] [-1 80] [1 200]]))))
  (testing "a strong reversal INSIDE the window still crosses (only the net leaks)"
    (let [eff (drive-timed-stream [[-1 0] [-1 40] [4 40]])]
      (is (= [-1 -1] (subvec eff 0 2)))
      (is (pos? (long (peek eff)))))))

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
