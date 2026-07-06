(ns com.blockether.vis.ext.channel-tui.click-regions
  "Live registry of clickable rectangles painted by the chat
   renderer. The render thread calls `begin-frame!` at the start of
   every full repaint, `register!`s one entry per painted chrome
   row, then `commit-frame!`s the staged set as the new live
   registry. The input thread `lookup`s the entry under the mouse
   cursor on `MOVE` (hover) and `CLICK_DOWN`.

   Why double-buffered (staging + published) instead of one atom:

   - `register!` runs many times during a paint. Reads from the
     input thread can land BETWEEN the per-row registrations of
     a single frame. With a single-buffer design the input thread
     would see a partially-filled vec and miss clicks on chrome
     rows that hadn't been painted yet - the very bug that made
     the header copy-id button feel \"sometimes broken\". The
     staged buffer is private to the render thread; `commit-frame!`
     publishes the WHOLE frame's regions in a single atomic swap,
     so `lookup` always sees a complete frame (the previous one
     until commit, the new one after).

   - The renderer paints into a Lanterna `TextGraphics` object that
     has no place to attach \"and also - here are the clickable
     bits I just drew.\" We'd otherwise need a parallel return
     channel through every paint helper.

   - The set is small (tens of entries at most - the visible
     scrollback's worth of links); a linear scan on lookup is
     fine and removes the need for any spatial-index dance.

   Coordinate convention: every region is stored in ABSOLUTE screen
   coordinates so the input handler can compare directly against
   `MouseAction.getPosition()`."
  (:refer-clojure :exclude [reset!]))

;; =============================================================================
;; Region shape
;; =============================================================================
;;
;; A region is a plain map:
;;
;;   {:bounds   {:row R :col C :width W}   ; W cells starting at (C, R), height 1
;;    :url      \"https://...\"             ; direct-open target, when applicable
;;    :kind     :url | :image | :file | :copy-id | ...
;;    :line     N | nil                     ; only for :file with a line anchor
;;    :scheme   :http | :https | :file | :rel | :rejected
;;    :enabled? true | false}
;;
;; `:enabled? false` regions are still registered so the renderer
;; can paint them differently (e.g. a 🚫 chrome) and the hover
;; pipeline can communicate \"this one is blocked\" to the user.
;; They never produce an open!.

;; PUBLISHED registry - what `lookup` reads. Holds the LAST fully
;; committed frame's regions. The vec is small (tens of entries)
;; so a linear scan on lookup is fine and we avoid the locking
;; dance a spatial index would need.
(defonce ^:private regions-atom (atom []))

;; STAGING buffer - only the render thread writes. Filled by
;; `register!` between `begin-frame!` and `commit-frame!`. The
;; input thread never reads this; lookups always go to
;; `regions-atom` so a half-filled staging buffer never leaks.
(defonce ^:private staging-atom (atom []))

;; Currently-hovered region, or nil. Read by the renderer to paint a
;; highlight; written by the mouse handler on `MOVE` events.
;; Hover is intentionally NOT cleared on `begin-frame!`/`commit-frame!`:
;; the user's cursor doesn't move just because a frame ticked, so
;; the highlight should persist until the next MOVE event.
(defonce ^:private hover-atom (atom nil))

;; =============================================================================
;; Mutators
;; =============================================================================

(defn begin-frame!
  "Open a new paint pass: drop every staged region. Call once at the
   top of every full chat repaint, BEFORE any `register!`. Does not
   touch the published registry - `lookup` keeps returning the
   previous frame's regions until `commit-frame!` runs."
  []
  (clojure.core/reset! staging-atom []))

(defn register!
  "Append `region` to the staging buffer. The renderer calls this
   once per painted chrome row; the order matches paint order so a
   later region painted on top of an earlier one wins on lookup
   (matches what the user actually sees on screen). Regions become
   visible to `lookup` only after `commit-frame!`.

   Validates the bounds shape - silently dropping a bad entry
   would mask renderer bugs."
  [{:keys [bounds] :as region}]
  (assert (map? bounds))
  (assert (integer? (:row bounds)))
  (assert (integer? (:col bounds)))
  (assert (integer? (:width bounds)))
  (swap! staging-atom conj region))

(defn commit-frame!
  "Atomically publish the staged regions as the new live set. Call
   once at the END of every full chat repaint, after every painter
   (messages area, header, footer, ...) has registered its regions.
   `lookup` then sees the freshly-painted frame in a single step,
   never a partial buffer."
  []
  (clojure.core/reset! regions-atom @staging-atom))

(defn reset!
  "Drop every published AND staged region; clear hover. Used by
   tests and by the screen teardown path; not part of the
   per-frame paint pipeline."
  []
  (clojure.core/reset! regions-atom [])
  (clojure.core/reset! staging-atom [])
  (clojure.core/reset! hover-atom nil))

;; =============================================================================
;; Readers
;; =============================================================================

(defn current
  "Snapshot of every currently-registered region, in paint order.
   The renderer reads this on every paint to decide which row -
   if any - to highlight."
  []
  @regions-atom)

(def label-alphabet
  "Single-character jump labels for the vim-style disclosure overlay, home row
   first so the common case is a no-reach keypress. Caps how many folds one
   screen can label; any beyond the alphabet stay mouse-clickable."
  (mapv str "asdfghjklqwertyuiopzxcvbnm"))

(defn assign-labels
  "Assign jump labels to the visible `:toggle-details` regions in `regions`
   (pass `(current)`), in paint order. Returns an ordered vec of `[label
   region]` pairs. Dedupes by `[session-id node-id]` (one fold can register
   more than one glyph row) keeping the first occurrence, and caps at
   `label-alphabet` length. Deterministic: the renderer (painting the badges)
   and the input handler (resolving a keypress) derive the SAME assignment from
   the same frame, so neither needs to share mutable state with the other."
  [regions]
  (let [toggles (:out (reduce (fn [{:keys [seen] :as acc} r]
                                (if (= :toggle-details (:kind r))
                                  (let [k [(:session-id r) (:node-id r)]]
                                    (if (contains? seen k)
                                      acc
                                      (-> acc
                                          (update :seen conj k)
                                          (update :out conj r))))
                                  acc))
                              {:seen #{} :out []}
                              regions))]
    (mapv vector label-alphabet toggles)))

(defn- contains-point?
  "True when (col, row) lies inside `bounds`. Inclusive on the left
   edge, exclusive on the right edge."
  [{:keys [row col width]} c r]
  (and (= r row) (>= c col) (< c (+ col width))))

(defn lookup
  "Return the topmost (last-registered) region containing (col, row),
   or nil. O(n) linear scan from the END of the regions vec so a
   newer overlay wins over an older one underneath."
  [col row]
  (let [v @regions-atom]
    (loop [i (dec (count v))]
      (when (>= i 0)
        (let [region (nth v i)]
          (if (contains-point? (:bounds region) col row) region (recur (dec i))))))))

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
    (when (not= prev region) (clojure.core/reset! hover-atom region) true)))
