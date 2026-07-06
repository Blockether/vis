(ns com.blockether.vis.internal.header
  "Channel-agnostic header layout & content spec.

   Every channel — terminal TUI, web, Telegram, future surfaces —
   renders the same conceptual header band:

       [LEFT 30%]   [CENTER 40% workspace switcher]   [RIGHT 30%]

   The decisions a channel cannot make on its own (slot ratios, workspace
   switcher sizing/visibility caps, default labels, copy id length, glyphs)
   live here as plain Clojure data. A channel imports this namespace,
   reads the values, and projects them onto its medium — TextGraphics
   cells for Lanterna, divs/spans for HTML, a keyboard row for
   Telegram, etc.

   No graphics. No I/O. No channel-specific deps. Pure data + tiny
   pure helpers, written as `.cljc` so a ClojureScript web UI can
   require it directly."
  (:require [clojure.string :as str]))

;;; ── Slot ratios ────────────────────────────────────────────────────────
;;
;; The header is conceptually a 3-column flex row with 30 / 40 / 30
;; split. Channels free to clamp / re-derive widths, but the ratios
;; are the same everywhere so a screenshot of the TUI and a screenshot
;; of the web UI feel like the same product.

(def left-slot-ratio "Fraction of the total width given to the LEFT slot (notifications)." 3/10)

(def right-slot-ratio
  "Fraction of the total width given to the RIGHT slot (channel status +
   session-id copy affordance)."
  3/10)

(defn slot-widths
  "Compute integer widths `[left center right]` for a header `cols` wide.
   `center = cols - left - right` so rounding errors never bleed off-screen."
  [cols]
  (let [cols
        (max 0 (long cols))

        left
        (long (* cols (double left-slot-ratio)))

        right
        (long (* cols (double right-slot-ratio)))

        center
        (max 0 (- cols left right))]

    [left center right]))

(def left-slot-cols
  "Static width (in display cells) of the LEFT header slot (notifications).
   A fixed column count instead of a fraction, so the left slot stays the
   same size whether the terminal is 80 or 200 columns wide."
  60)

(def right-slot-cols
  "Static width (in display cells) of the RIGHT header slot (session-id copy
   affordance). Fixed column count, see `left-slot-cols`."
  50)

(def slot-gap-cols
  "Blank columns kept on EACH side between the centre slot and the left /
   right side slots, so the middle never crashes into either edge slot."
  3)

(defn slot-layout
  "Fixed-width header layout for a band `cols` wide.

   LEFT and RIGHT are sized by their slot ratios (`left-slot-ratio` /
   `right-slot-ratio`) but capped at the static `left-slot-cols` /
   `right-slot-cols` so they never eat the whole band on a wide terminal;
   CENTER absorbs whatever is left after also reserving `slot-gap-cols`
   blank columns on each side of the centre. Every width
   clamps at 0 so a narrow band degrades gracefully (centre collapses first,
   then the right slot) instead of going negative.

   Returns `{:left-x :left-w :center-x :center-w :right-x :right-w}` -
   absolute x positions + widths a channel paints directly."
  [cols]
  (let [cols
        (max 0 (long cols))

        gap
        (long slot-gap-cols)

        left-w
        (min (long left-slot-cols) (long (* cols (double left-slot-ratio))))

        right-w
        (min (long right-slot-cols)
             (long (* cols (double right-slot-ratio)))
             (max 0 (- cols left-w)))

        center-w
        (max 0 (- cols left-w right-w (* 2 gap)))

        left-x
        0

        center-x
        (min cols (+ left-w gap))

        right-x
        (- cols right-w)]

    {:left-x left-x
     :left-w left-w
     :center-x center-x
     :center-w center-w
     :right-x right-x
     :right-w right-w}))

;;; ── Workspace switcher sizing policy ───────────────────────────────────

(def tab-entry-padding
  "Inner padding (in display cells / spaces) reserved on each side of a
   workspace label so the label never crashes into the cell border."
  1)

(def tab-entry-target-width
  "Natural width per workspace entry used to drive the visible-entry
   clamp. Wider → fewer workspaces fit before arrows appear; narrower →
   more workspaces fit but labels truncate sooner."
  14)

(def min-visible-tab-entries
  "Lower clamp for visible workspace count: even on narrowish screens we
   keep at least this many visible (the arrows reach the rest)."
  5)

(def max-visible-tab-entries
  "Upper clamp for visible workspace count: huge screens stop spreading
   past this — past it the centre slot would look like a header full of
   workspaces and nothing else."
  8)

(defn max-visible-workspace-count
  "How many workspaces to show inside `width` cells.

   Policy (identical across channels):
   - clamp the natural fit `width / tab-entry-target-width` to
     `[min-visible-tab-entries, max-visible-tab-entries]`,
   - never exceed `workspace-n` (cannot show what does not exist),
   - if even `min-visible-tab-entries` cannot fit the natural
     budget, fall back to the natural fit so tiny surfaces degrade
     gracefully."
  [workspace-n width]
  (let [width
        (max 0 (long width))

        natural
        (max 1 (quot width (long tab-entry-target-width)))

        clamped
        (max (long min-visible-tab-entries) (min (long max-visible-tab-entries) natural))

        cap
        (if (< natural (long min-visible-tab-entries)) natural clamped)]

    (min (long workspace-n) (long cap))))

;;; ── Glyphs ─────────────────────────────────────────────────────────────
;;
;; Unicode characters render fine in every channel we care about
;; (Lanterna terminal, modern HTML, Telegram); channels needing ASCII
;; fallbacks can swap on their side.

(def workspace-arrow-left "Glyph for the `previous workspace` overflow affordance." "«")

(def workspace-arrow-right "Glyph for the `next workspace` overflow affordance." "»")

(def workspace-ellipsis "Glyph appended when a workspace label has to be truncated." "…")

(def copy-icon "Glyph used by the right-slot `copy session id` affordance." "⧉")

;;; ── Defaults ───────────────────────────────────────────────────────────

(def untitled-session-label
  "Default label every channel shows for a workspace whose session has
   no title yet. Renamed by the state layer once a title is generated."
  "Untitled session")

(def id-display-chars
  "How many leading characters of the session UUID every channel
   shows next to the copy affordance. Matches `vis sessions`
   short form."
  8)

;;; ── Pure helpers ───────────────────────────────────────────────────────

(defn title-or-placeholder
  "Visible label for a session: its title if non-blank, otherwise
   `untitled-session-label`. Used by both workspace-label sync in
   state and the synthesised fallback workspace in channels."
  [title]
  (if (and (string? title) (not (str/blank? title))) title untitled-session-label))

(defn short-id
  "Shorten a session UUID to the shared display length. Returns
   nil for blank input so channels can use truthy guards."
  [id]
  (let [id (some-> id
                   str)]
    (when (seq id) (subs id 0 (min (long id-display-chars) (count id))))))
