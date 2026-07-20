(ns com.blockether.vis.ext.channel-tui.scroll
  "Messages-area scroll state — one tagged value, no scattered flags.

   The old model smeared scroll across four fields with two different
   storage scopes (`:messages-scroll` workspace-local; the animation
   target + two intent booleans top-level), plus a `nil`-vs-concrete
   overload on `:messages-scroll`. Every handler had to hand-maintain
   their consistency, and forgetting one `dissoc` produced the
   \"flash to the top, then scroll to the bottom\" jump.

   This namespace replaces all of it with ONE workspace-local value,
   `:scroll`, that is a tagged variant:

     {:mode :follow}                ;; stick to the bottom (auto-track)
     {:mode :follow, :pos p}        ;;   …easing down toward the bottom
     {:mode :at, :offset n}         ;; parked at row n (user scrolled up)
     {:mode :at, :offset n, :pos p} ;;   …easing toward row n

   Two orthogonal concerns, cleanly separated:

   - INTENT  — `:mode` (+ `:offset`). `:follow` means \"desired = bottom\";
     `:at` means \"desired = offset\". The user's scroll-up intent is just
     `:at` — no separate `:scroll-pinned-up?` flag.
   - DISPLAY — `:pos`, the eased on-screen row. Present only while an
     animation is in flight (and held at the bottom in `:follow` so the
     NEXT content growth has somewhere to ease FROM instead of
     teleporting). Absent ⇒ snapped exactly to desired.

   Every transition REPLACES the whole value, so nothing can dangle.
   `ease` is called once per render frame; in `:follow` mode the desired
   row simply IS the growing bottom, so streaming content eases in for
   free — there is no special \"animated follow\" path any more.

   Pure: no re-frame, no atoms. `state.clj` events are thin wrappers and
   `screen.clj` reads `layout-offset` / `animating?` to drive the loop."
  (:import [java.lang Math]))

(def ^:const slack-rows
  "How close (in rows) to the bottom still counts as the bottom for the
   purpose of re-arming `:follow`. Scrolling DOWN to within this band
   sticks to the bottom again, so freshly streamed content keeps the
   latest message in view. Kept small so a deliberate read-up even a
   few rows above the live edge stays parked."
  4)

(def ^:const step-frac
  "Ease-out fraction per animation step: bigger steps when far, smaller
   as we close in. The render loop ticks `ease` a few times a second."
  0.35)

(def follow "The resting auto-bottom intent: track the latest message." {:mode :follow})

(defn parked
  "A concrete parked intent at row `offset` (snapped, no in-flight ease)."
  [offset]
  {:mode :at :offset (max 0 (long offset))})

(defn- norm
  "Tolerate a missing/legacy `:scroll` by defaulting to FOLLOW."
  [sc]
  (if (map? sc) sc follow))

(defn scrolled-up?
  "True when the user has PARKED the viewport above the live bottom
   (`:at` intent) — reading history rather than following new content.
   The input cursor is hidden in this state so its terminal blink does
   not jump around as the transcript repaints during a scroll."
  [sc]
  (= :at (:mode (norm sc))))

(defn- clamp ^long [^long v ^long lo ^long hi] (max lo (min v hi)))

(defn desired
  "The row the view WANTS to show: bottom in `:follow`, the clamped
   offset in `:at`."
  ^long [sc ^long max-s]
  (let [sc (norm sc)]
    (case (:mode sc)
      :at
      (clamp (long (:offset sc)) 0 max-s)

      ;; :follow (and any unknown mode) tracks the bottom.
      max-s)))

(defn displayed
  "The row currently ON SCREEN: the eased `:pos` if mid-animation, else
   `desired`."
  ^long [sc ^long max-s]
  (let [sc (norm sc)]
    (long (or (:pos sc) (desired sc max-s)))))

(defn animating?
  "True when an ease is still in flight (displayed ≠ desired). Drives the
   render loop's fast tick cadence; settles to false so an idle view
   stops repainting."
  [sc ^long max-s]
  (let [sc (norm sc)]
    (boolean (and (:pos sc) (not= (long (:pos sc)) (desired sc max-s))))))

(defn bottom-hidden?
  "True when the live bottom sits BELOW the current viewport — i.e. there is
   content to jump DOWN to. FALSE when following at the bottom, AND false when
   nothing overflows (`max-s` 0: an empty or short session). The jump-to-bottom
   affordance keys off this, NOT `scrolled-up?` — parking `:at` offset 0 in an
   empty session is still 'at the bottom', so the chip must stay hidden."
  [sc ^long max-s]
  (and (pos? max-s) (< (displayed sc max-s) max-s)))

(defn jump-chip-visible?
  "True when the \"↓ latest\" jump-to-bottom chip should paint: the user is
   PARKED above the live bottom (`:at` intent) AND the bottom actually sits
   off-screen below. It takes BOTH predicates:

   - `bottom-hidden?` alone FLASHES the chip during plain FOLLOW easing —
     while a turn streams, `ease` holds the eased `:pos` a few rows above the
     growing bottom every frame, so the bottom is technically 'hidden' even
     though the user never left it.
   - `scrolled-up?` alone pops the chip in an empty/short session where a
     PageUp parked `:at` offset 0 — nothing below to jump to."
  [sc ^long max-s]
  (and (scrolled-up? sc) (bottom-hidden? sc max-s)))

(defn layout-offset
  "The offset to feed `virtual/layout` and the scrollbar geometry.

   `nil` ⇒ auto-bottom (never anchored, always tracks the latest message
   — used when FOLLOW is settled at the bottom). A concrete row is
   returned while parked or mid-ease so the painter anchors / animates
   against it."
  [sc ^long max-s]
  (let
    [sc
     (norm sc)

     pos
     (:pos sc)]

    (case (:mode sc)
      ;; Settled at (or below) the bottom ⇒ nil exact-bottom lock.
      ;; Mid-ease (pos above the bottom) ⇒ the concrete eased row.
      :follow
      (when (and pos (< (long pos) max-s)) (long pos))

      :at
      (displayed sc max-s)

      nil)))

(defn- step-toward
  "One ease-out step from `cur` toward `target`; snaps within one row."
  ^long [^long cur ^long target]
  (let [diff (- target cur)]
    (cond (zero? diff) cur
          (= 1 (Math/abs diff)) target
          :else (let [step (long (Math/max 1 (long (Math/ceil (* step-frac (Math/abs diff))))))]
                  (if (pos? diff) (Math/min target (+ cur step)) (Math/max target (- cur step)))))))

(defn ease
  "Advance the on-screen `:pos` one step toward `desired`. Called once per
   render frame.

   - Mid-ease ⇒ step `:pos` toward the target.
   - Settled while PARKED ⇒ drop `:pos` (clean snap; no further repaint).
   - Settled while FOLLOWING ⇒ KEEP `:pos` pinned at the bottom so the
     next content growth eases FROM here instead of teleporting."
  [sc ^long max-s]
  (let
    [sc
     (norm sc)

     d
     (desired sc max-s)

     cur
     (displayed sc max-s)]

    (cond (not= cur d) (assoc sc :pos (step-toward cur d))
          (= :follow (:mode sc)) (assoc sc :pos d)
          :else (dissoc sc :pos))))

(defn up
  "Wheel/key scroll UP by `amount`: park `amount` rows above the current
   COMMITTED offset and ease there. Scrolling up is always a deliberate
   \"let me read history\" intent.

   The target is anchored to the committed `:offset` (or the live bottom
   while FOLLOWING) — NOT to the eased display row. An alternating momentum
   stream (slow trackpad drags emit sign-flipping inertia tail events) would
   otherwise re-anchor each event off a half-eased position and ratchet the
   viewport backward, visibly fighting the user. `:pos` is seeded from the
   display row only when no ease is already in flight, so a new ease begins
   from where the view currently sits while a continuing ease keeps its
   origin."
  [sc ^long amount ^long max-s]
  (let
    [sc
     (norm sc)

     cur
     (displayed sc max-s)

     base
     (if (= :follow (:mode sc)) max-s (long (:offset sc)))

     t
     (max 0 (- base amount))]

    {:mode :at :offset t :pos (or (:pos sc) cur)}))

(defn down
  "Wheel/key scroll DOWN by `amount`. Landing within `slack-rows` of the
   bottom re-arms FOLLOW (and eases the rest of the way); otherwise parks
   `amount` rows below the COMMITTED offset.

   As with `up`, the target anchors to the committed `:offset` (or the live
   bottom while FOLLOWING) so an alternating momentum stream can't walk the
   viewport backward off a half-eased position."
  [sc ^long amount ^long max-s]
  (let
    [sc
     (norm sc)

     cur
     (displayed sc max-s)

     base
     (if (= :follow (:mode sc)) max-s (long (:offset sc)))

     t
     (+ base amount)]

    (if (>= t (- max-s slack-rows))
      (assoc follow :pos (or (:pos sc) cur))
      {:mode :at :offset t :pos (or (:pos sc) cur)})))

(def ^:const momentum-cap
  "Upper bound on the running wheel-momentum used by `merge-wheel-delta`.
   Caps how much a long, continuous same-direction drag can accumulate so a
   single stray opposing tick can't be absorbed forever (which would make the
   scroll feel sticky / unable to reverse). Chosen well above any realistic
   coalesced batch (`amount` is `(* 3 |wheel-delta|)`), so normal scrolling
   never clamps, but a frantic flail can't build unbounded inertia."
  12)

(def ^:const momentum-hold-ms
  "How long (ms of wall-clock) the wheel-momentum directional lock survives
   after the LAST wheel event. TIME-based, not poll-based: the old per-poll
   halving ran at the input loop's ~16ms cadence and released the lock in
   ~60ms — SHORTER than the 50-100ms gap between two events of a slow macOS
   trackpad inertia tail — so the tail's sign-flipped final ticks always met
   zero momentum and dispatched as real reversals. Parked within the
   slack band that re-armed FOLLOW mid-stream: the 'scrolling in place'
   bounce. Mirrors kitty's 150ms momentum sample window."
  150)

(defn- cap-momentum
  "Clamp `v` into `[-momentum-cap, momentum-cap]`."
  ^long [^long v]
  (max (- momentum-cap) (min momentum-cap v)))

(defn merge-wheel-delta
  "Smooth a single raw wheel `delta` (signed rows, +down / -up) into a running
   `momentum`, returning `[new-momentum effective-delta]`.

   This is the input-side debouncer that stops a slow macOS trackpad's
   sign-flipping inertia tail from bouncing the viewport. It is the discrete,
   phase-less analog of kitty's `add_velocity` (glfw/momentum-scroll.c) and
   WebKit's kinetic-scroll velocity accumulation:

   - SAME sign as the running momentum (or momentum at rest): accumulate, and
     the raw delta dispatches verbatim. A solo tap or a clean drag is unchanged.
   - OPPOSITE sign: the delta can only BRAKE the existing momentum toward zero,
     never cross it. A small opposing tick is absorbed (effective-delta nil) and
     just shaves momentum; only once the accumulated opposition exceeds the
     remaining momentum does the leftover re-seed in the new direction.

   `effective-delta` is nil when the tick was fully absorbed — the caller drops
   it. `new-momentum` is what the caller stores back for the next poll."
  [momentum delta]
  (let
    [m
     (long (or momentum 0))

     r
     (long delta)]

    (cond (zero? r) [m nil]
          (or (zero? m) (pos? (* r m))) [(cap-momentum (+ m r)) r]
          :else (let [after (+ m r)]
                  (cond (zero? (Long/signum after)) [0 nil]
                        (pos? (* after m)) [(cap-momentum after) nil]
                        ;; raw overwhelms: dispatch ONLY the net leftover (raw beyond what
                        ;; cancelled the prior momentum) and re-seed momentum to that net,
                        ;; so the viewport tracks net displacement instead of double-counting.
                        :else [after after])))))

(defn decay-wheel-momentum
  "Momentum remaining `idle-ms` after the LAST wheel event. Linearly ramped
   toward zero across `momentum-hold-ms` — but the DIRECTIONAL LOCK (at least
   ±1) is kept alive for the whole window, and exactly zero once it expires.

   The sign floor is the point: a slow trackpad tail leaves only ~1-2 rows of
   momentum, and rounding that to zero mid-window would let the very
   sign-flipped tick this smoother exists to absorb through. The ramp (rather
   than holding verbatim) keeps a DELIBERATE quick reversal responsive: pegged
   momentum shrinks ~50% per 80ms gap, so an opposing gesture crosses it in a
   couple of ticks instead of grinding down the full `momentum-cap`.

   Callers compute this at USE time (raw stored momentum + elapsed wall-clock
   ms), never store the decayed value back per poll — that would compound."
  [momentum idle-ms]
  (let
    [m
     (long (or momentum 0))

     idle
     (max 0 (long idle-ms))]

    (cond (zero? m) 0
          (>= idle momentum-hold-ms) 0
          :else (let
                  [scaled (long (Math/round
                                  (* m (- 1.0 (/ (double idle) (double momentum-hold-ms))))))]
                  (if (zero? scaled) (Long/signum m) scaled)))))

(defn to-y
  "Scrollbar drag/track click → `offset` (already mapped from cursor row).
   1:1, so SNAP (no ease). The very bottom re-enters FOLLOW."
  [^long offset ^long max-s]
  (if (>= offset max-s) follow (parked offset)))

(defn reanchor
  "The layout corrected off-screen estimate→real heights and shifted the
   anchor by `delta`; `anchored` is the new absolute on-screen row. Shift
   the concrete fields so the anchored message stays visually put. Only
   meaningful while parked or mid-ease (FOLLOW-at-bottom feeds `nil` and
   never reaches here)."
  [sc ^long anchored ^long delta]
  (let [sc (norm sc)]
    (case (:mode sc)
      :at
      (-> sc
          (assoc :pos anchored)
          (update :offset + delta))

      :follow
      (assoc sc :pos anchored)

      sc)))
