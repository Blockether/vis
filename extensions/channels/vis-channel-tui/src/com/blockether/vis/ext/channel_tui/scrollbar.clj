(ns com.blockether.vis.ext.channel-tui.scrollbar
  "Single source of truth for vertical scrollbars in the TUI.

   One namespace owns geometry, drawing, hit-test, wheel handling and
   click/drag → scroll math. EVERY scrollbar — chat messages, slash
   menu, settings, file picker, provider/model cards, sessions, every
   other dialog — goes through here. No more bespoke thumb math per
   call site, no more drift between painter and hit-test layers.

   Why one cell tall thumbs
   ------------------------
   Terminal.app renders stacked U+2588 FULL BLOCK cells as a ladder of
   separate boxes when the window is tall, which reads as multiple
   scroll positions instead of one current-position marker. So the
   visible thumb is intentionally one row high; the thumb's row index
   inside the track encodes the scroll fraction."
  (:require [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t])
  (:import [com.googlecode.lanterna.graphics TextGraphics]
           [com.googlecode.lanterna.input MouseAction MouseActionType]))

(def ^:const THUMB_H 1)
;;; ── Geometry ───────────────────────────────────────────────────────────────
(defn geometry
  "Compute thumb geometry for a vertical scrollbar.

   Arguments
   - `total-h`  total content size (rows OR item count — same math
                applies to row-pixels and item-windows).
   - `inner-h`  visible viewport size in the same unit as `total-h`.
   - `track-h`  optional track length in screen rows; defaults to
                `inner-h`. The track may extend past the viewport
                (e.g. into top/bottom breathing-room rows).
   - `scroll`   current scroll offset; `nil` ⇒ pinned to bottom
                (`max-scroll`). Out-of-range values are clamped.

   Returns `{:thumb-top-rel :thumb-h :max-scroll :track-h}` when the
   content overflows the viewport, otherwise `nil` (no thumb should be
   drawn and every click hit-test below should return off-thumb)."
  ([total-h inner-h scroll] (geometry total-h inner-h inner-h scroll))
  ([total-h inner-h track-h scroll]
   (let
     [total-h
      (long total-h)

      inner-h
      (long inner-h)

      track-h
      (long track-h)]

     (when (and (pos? inner-h) (pos? track-h) (> total-h inner-h))
       (let
         [max-scroll
          (max 1 (- total-h inner-h))

          eff
          (let [s (long (or scroll max-scroll))]
            (max 0 (min s max-scroll)))

          thumb-h
          THUMB_H

          thumb-top
          (long (* (- track-h thumb-h) (/ (double eff) max-scroll)))]

         {:thumb-top-rel thumb-top :thumb-h thumb-h :max-scroll max-scroll :track-h track-h})))))
;;; ── Drawing ────────────────────────────────────────────────────────────────
(defn draw!
  "Paint a vertical scrollbar.\n\n   Required opts: `:col :top :track-h :total-h :inner-h :scroll`.\n   Optional opts: `:track-fg :track-bg :thumb-fg :thumb-bg`\n   (default = dialog palette, used by every modal scrollbar).\n\n   Returns the geometry map (or nil when no overflow, in which case\n   nothing is painted)."
  [^{:tag TextGraphics} g
   {:keys [col top track-h total-h inner-h scroll track-fg track-bg thumb-fg thumb-bg]
    :or {track-fg t/dialog-border
         track-bg t/dialog-bg
         thumb-fg t/dialog-hint-key
         thumb-bg t/dialog-bg}}]
  (when-let [{:keys [thumb-top-rel thumb-h] :as geom} (geometry total-h inner-h track-h scroll)]
    (let
      [col (long col)
       top (long top)
       track-h (long track-h)]

      (doseq [r (range track-h)]
        (p/set-colors! g track-fg track-bg)
        (p/set-char! g col (+ top (long r)) \│))
      (doseq [r (range thumb-h)]
        (p/set-colors! g thumb-fg thumb-bg)
        (p/set-char! g col (+ top (long thumb-top-rel) (long r)) \█)))
    geom))
;;; ── Mouse / wheel helpers ──────────────────────────────────────────────────
(defn wheel-delta
  "Return -1 for SCROLL_UP, +1 for SCROLL_DOWN, else nil. Accepts a
   Lanterna KeyStroke or MouseAction; non-mouse events return nil."
  [event]
  (when (instance? MouseAction event)
    (let [a (.getActionType ^MouseAction event)]
      (cond (= a MouseActionType/SCROLL_UP) -1
            (= a MouseActionType/SCROLL_DOWN) 1
            :else nil))))

(defn wheel-step
  "Wheel delta multiplied by the coalesced event count carried in
   `MouseAction#getButton`. Lanterna stuffs the coalesced count into
   the button field when an upstream input loop drains a wheel flood,
   so one mouse tick can move multiple rows."
  [event]
  (when-let [d (wheel-delta event)]
    (* (long d) (max 1 (long (.getButton ^MouseAction event))))))

(defn on-track?
  "True when (mx,my) falls inside the scrollbar column at any track row.

   `bar` is `{:col :top :track-h [:x-band]}`. `:x-band` widens the
   x-axis tolerance (default 1) — chat messages use 3 so users don't
   need pixel-perfect aim on the right gutter."
  [mx my {:keys [col top track-h x-band] :or {x-band 1}}]
  (let
    [mx
     (long mx)

     my
     (long my)

     col
     (long col)

     top
     (long top)

     track-h
     (long track-h)

     x-band
     (long x-band)]

    (and (>= mx (- col (dec x-band))) (<= mx col) (>= my top) (< my (+ top track-h)))))

(defn on-thumb?
  "True when (mx,my) lands on the thumb. `bar` carries `:col :top
   [:x-band]`; `geom` is the result of `geometry`."
  [mx my {:keys [col top x-band] :or {x-band 1}} {:keys [thumb-top-rel thumb-h]}]
  (let
    [mx
     (long mx)

     my
     (long my)

     col
     (long col)

     top
     (long top)

     x-band
     (long x-band)

     thumb-top
     (+ top (long thumb-top-rel))]

    (and (>= mx (- col (dec x-band)))
         (<= mx col)
         (>= my thumb-top)
         (< my (+ thumb-top (long thumb-h))))))

(defn scroll-from-mouse-y
  "Convert a mouse Y on the track into a clamped scroll value.

   - `mouse-y`     absolute row the cursor is at
   - `top`         track's first-row screen Y
   - `track-h`     track length
   - `total-h`     total content size
   - `inner-h`     viewport size
   - `grip-offset` rows between thumb top and the user's grip-point;
                   pass the offset saved at CLICK_DOWN so dragging
                   preserves the grip (so the row under the finger
                   stays glued to the same point on the thumb). Use
                   `0` for click-to-position behaviour."
  ([mouse-y top track-h total-h inner-h]
   (scroll-from-mouse-y mouse-y top track-h total-h inner-h 0))
  ([mouse-y top track-h total-h inner-h grip-offset]
   (let
     [total-h
      (long total-h)

      inner-h
      (long inner-h)

      track-h
      (long track-h)]

     (when (and (pos? inner-h) (pos? track-h) (> total-h inner-h))
       (let
         [thumb-h
          THUMB_H

          max-scroll
          (max 0 (- total-h inner-h))

          rel
          (- (long mouse-y) (long top) (long (or grip-offset 0)))

          denom
          (max 1 (- track-h thumb-h))

          frac
          (max 0.0 (min 1.0 (double (/ rel denom))))]

         (long (Math/round (* frac max-scroll))))))))
