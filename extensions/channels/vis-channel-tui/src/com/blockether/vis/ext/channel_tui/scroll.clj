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

(defn layout-offset
  "The offset to feed `virtual/layout` and the scrollbar geometry.

   `nil` ⇒ auto-bottom (never anchored, always tracks the latest message
   — used when FOLLOW is settled at the bottom). A concrete row is
   returned while parked or mid-ease so the painter anchors / animates
   against it."
  [sc ^long max-s]
  (let [sc
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
  (let [sc
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
   on-screen row and ease there. Scrolling up is always a deliberate
   \"let me read history\" intent."
  [sc ^long amount ^long max-s]
  (let [cur
        (displayed sc max-s)

        t
        (max 0 (- cur amount))]

    {:mode :at :offset t :pos cur}))

(defn down
  "Wheel/key scroll DOWN by `amount`. Landing within `slack-rows` of the
   bottom re-arms FOLLOW (and eases the rest of the way); otherwise parks
   `amount` rows below the current row."
  [sc ^long amount ^long max-s]
  (let [cur
        (displayed sc max-s)

        t
        (+ cur amount)]

    (if (>= t (- max-s slack-rows)) (assoc follow :pos cur) {:mode :at :offset t :pos cur})))

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
