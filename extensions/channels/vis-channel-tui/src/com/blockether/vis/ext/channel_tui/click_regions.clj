(ns com.blockether.vis.ext.channel-tui.click-regions
  "Live registry of clickable rectangles painted by the chat
   renderer. The render thread `reset!`s the registry at the start
   of every full repaint and `register!`s one entry per painted
   chrome row; the input thread `lookup`s the entry under the mouse
   cursor on `MOVE` (hover) and `CLICK_DOWN`.

   Why a top-level atom instead of threading state through the
   render API:

   - The renderer paints into a Lanterna `TextGraphics` object that
     has no place to attach \"and also \u2014 here are the clickable
     bits I just drew.\" We'd otherwise need a parallel return
     channel through every paint helper.

   - Render and input run on different threads. An immutable vec
     swapped atomically + a single-threaded `reset!`/`register!`
     pair gives us lock-free reads from input.

   - The set is small (tens of entries at most \u2014 the visible
     scrollback's worth of links); a linear scan on lookup is
     fine and removes the need for any spatial-index dance.

   Coordinate convention: every region is stored in ABSOLUTE screen
   coordinates so the input handler can compare directly against
   `MouseAction.getPosition()`."
  (:refer-clojure :exclude [reset!]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Region shape
;; =============================================================================
;;
;; A region is a plain map:
;;
;;   {:bounds   {:row R :col C :width W}   ; W cells starting at (C, R), height 1
;;    :url      \"https://...\"             ; the original target string
;;    :kind     :url | :image | :file
;;    :line     N | nil                      ; only for :file with a line anchor
;;    :scheme   :http | :https | :file | :rel | :rejected
;;    :enabled? true | false}
;;
;; `:enabled? false` regions are still registered so the renderer
;; can paint them differently (e.g. a 🚫 chrome) and the hover
;; pipeline can communicate \"this one is blocked\" to the user.
;; They never produce an open!.

(defonce ^:private regions-atom (atom []))

(defonce ^:private hover-atom
  "Currently-hovered region, or nil. Read by the renderer to paint a
   highlight; written by the mouse handler on `MOVE` events."
  (atom nil))

;; =============================================================================
;; Mutators
;; =============================================================================

(defn reset!
  "Drop every registered region. Called by the renderer at the start
   of each full chat repaint so stale regions from the previous
   viewport never leak into the current one. Also clears the hover
   pointer."
  []
  (clojure.core/reset! regions-atom [])
  (clojure.core/reset! hover-atom nil))

(defn register!
  "Append `region` to the registry. The renderer calls this once per
   painted chrome row; the order matches paint order so a later
   region painted on top of an earlier one wins on lookup (matches
   what the user actually sees on screen).

   Validates the bounds shape \u2014 silently dropping a bad entry
   would mask renderer bugs."
  [{:keys [bounds] :as region}]
  (assert (map? bounds))
  (assert (integer? (:row bounds)))
  (assert (integer? (:col bounds)))
  (assert (integer? (:width bounds)))
  (swap! regions-atom conj region))

;; =============================================================================
;; Readers
;; =============================================================================

(defn current
  "Snapshot of every currently-registered region, in paint order.
   The renderer reads this on every paint to decide which row \u2014
   if any \u2014 to highlight."
  []
  @regions-atom)

(defn- contains-point?
  "True when (col, row) lies inside `bounds`. Inclusive on the left
   edge, exclusive on the right edge."
  [{:keys [row col width]} c r]
  (and (= r row)
    (>= c col)
    (< c (+ col width))))

(defn lookup
  "Return the topmost (last-registered) region containing (col, row),
   or nil. O(n) linear scan from the END of the regions vec so a
   newer overlay wins over an older one underneath."
  [col row]
  (let [v @regions-atom]
    (loop [i (dec (count v))]
      (when (>= i 0)
        (let [region (nth v i)]
          (if (contains-point? (:bounds region) col row)
            region
            (recur (dec i))))))))

;; =============================================================================
;; Hover pointer
;; =============================================================================

(defn hovered
  "Return the currently-hovered region or nil. Read by the renderer
   to paint a highlight on the matching row."
  []
  @hover-atom)

(defn set-hovered!
  "Update the hover pointer. Returns true when the value actually
   changed; the input handler uses that to decide whether to bump
   the render version (avoids repaint storms on every mouse twitch
   inside the same row)."
  [region]
  (let [prev @hover-atom]
    (when (not= prev region)
      (clojure.core/reset! hover-atom region)
      true)))
