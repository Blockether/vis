(ns com.blockether.vis.ext.channel-tui.enter
  "Transcript entrance animation — the terminal counterpart of the companion's
   `@starting-style` fade-in (`transcriptEnterClass`, ChatContent.tsx).

   Opening or focusing a session TELEPORTS its transcript onto the screen: one
   frame is the loading spinner (or the previous tab), the next is a full,
   bottom-anchored wall of text. Nothing moves, so the eye gets no cue about
   what changed or where to look — the app called it \"jumping\".

   The web app solves this with opacity+translate on mount. A terminal cannot
   translate sub-cell, and shifting whole rows for 3 frames reads as a lurch,
   not a rise. What it CAN do is dissolve: every cell of the messages band is
   painted normally and then blended back toward the terminal background, with
   the blend weight ramping to 1.0 over a few hundred ms. Rows nearest the
   input resolve FIRST and the wave runs upward into history, so the newest
   message — the reason you reopened the session — is what materialises under
   the cursor while older turns fade in behind it.

   Everything here is pure: `active?`/`row-alpha` are wall-clock math and
   `blend` is a color mix. `screen.clj` owns the one back-buffer pass that
   applies them (`paint-transcript-enter!`), and `state.clj` owns the single
   `:transcript-enter-at` timestamp that arms it."
  (:import [com.googlecode.lanterna TextColor TextColor$RGB]))

(def ^:const fade-ms
  "How long ONE row takes to go from invisible to fully painted. Matches the
   companion's `duration-200` beat; long enough to read as a dissolve on a
   60fps terminal tick, short enough that it never delays reading."
  170)

(def ^:const stagger-ms
  "Extra delay budget spread across the band, bottom row (0ms) to top row
   (this much). The wave — not the fade — is what encodes direction, so it has
   to be comparable to `fade-ms`; much smaller and the whole band just blinks
   on together."
  130)

(def ^:const duration-ms
  "Total wall time of the entrance: the last (topmost) row starts at
   `stagger-ms` and needs `fade-ms` to finish. The render loop uses this to
   decide when to stop forcing frames."
  (+ fade-ms stagger-ms))

(def disabled?
  "Kill switch (env `VIS_NO_TRANSCRIPT_ANIM`), mirroring `VIS_FORCE_FULL_FRAME`:
   a production escape hatch for terminals where per-cell recoloring is too
   expensive or simply unwanted. Also the reduced-motion answer — set it and
   transcripts appear instantly, exactly as before."
  (some? (System/getenv "VIS_NO_TRANSCRIPT_ANIM")))

(defn active?
  "True while the entrance armed at `start-ms` is still mid-flight. `nil`
   start (nothing armed) and any elapsed past `duration-ms` are false, so a
   settled view stops repainting."
  [start-ms now-ms]
  (boolean (and (not disabled?)
                start-ms
                (let [e (- (long now-ms) (long start-ms))]
                  (and (>= e 0) (< e (long duration-ms)))))))

(defn- ease-out
  "Cubic ease-out on a already-clamped 0..1 fraction — fast start, gentle
   settle, the same curve as the companion's `ease-out`."
  ^double [^double t]
  (let [u (- 1.0 t)]
    (- 1.0 (* u u u))))

(defn row-alpha
  "Paint weight (0.0 invisible … 1.0 final colors) for screen `row` of the
   messages band `[top, bottom)` at `now-ms`.

   The row's own fade STARTS at a delay proportional to its distance from the
   bottom of the band, so the wave travels bottom → top. Rows outside the band
   and a settled/never-armed entrance answer 1.0, which callers treat as
   \"leave this cell alone\"."
  ;; No primitive hints: a 5-arg fn cannot take them, and the per-cell cost is
  ;; one row-alpha per ROW, not per cell.
  [start-ms now-ms row top bottom]
  (if-not (active? start-ms now-ms)
    1.0
    (let
      [row
       (long row)

       top
       (long top)

       bottom
       (long bottom)

       h
       (max 1 (- bottom top))

       ;; 0 for the bottom-most row, h-1 for the top-most.
       from-bottom
       (max 0 (min (dec h) (- bottom 1 row)))

       delay
       (if (> h 1) (* (double stagger-ms) (/ (double from-bottom) (double (dec h)))) 0.0)

       elapsed
       (- (double (- (long now-ms) (long start-ms))) delay)]

      (cond (<= elapsed 0.0) 0.0
            (>= elapsed (double fade-ms)) 1.0
            :else (ease-out (/ elapsed (double fade-ms)))))))

(defn blend
  "Mix `color` toward `bg` by `alpha` (0.0 ⇒ pure `bg`, 1.0 ⇒ pure `color`).

   Works for any `TextColor` — ANSI and indexed palettes answer `getRed`/
   `getGreen`/`getBlue` too — and always returns a concrete RGB so the fork's
   painter emits a truecolor escape per cell instead of snapping to the
   nearest palette entry mid-fade."
  ^TextColor [^TextColor color ^TextColor bg ^double alpha]
  (cond (>= alpha 1.0) color
        (<= alpha 0.0) bg
        :else (let
                [mix (fn ^long [^long c ^long b]
                       (long (Math/round (+ (* alpha (double c)) (* (- 1.0 alpha) (double b))))))]
                (TextColor$RGB. (mix (.getRed color) (.getRed bg))
                                (mix (.getGreen color) (.getGreen bg))
                                (mix (.getBlue color) (.getBlue bg))))))
