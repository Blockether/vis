(ns com.blockether.vis.internal.header
  "Channel-agnostic header layout & content spec.

   Every channel — terminal TUI, web, Telegram, future surfaces —
   renders the same conceptual header band:

       [LEFT 20%]   [CENTER 60% workspace tabs]   [RIGHT 20%]

   The decisions a channel cannot make on its own (slot ratios, tab
   sizing/visibility caps, default labels, copy id length, glyphs)
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
;; The header is conceptually a 3-column flex row with 20 / 60 / 20
;; split. Channels free to clamp / re-derive widths, but the ratios
;; are the same everywhere so a screenshot of the TUI and a screenshot
;; of the web UI feel like the same product.

(def left-slot-ratio
  "Fraction of the total width given to the LEFT slot (notifications)."
  1/5)

(def right-slot-ratio
  "Fraction of the total width given to the RIGHT slot (channel status +
   conversation-id copy affordance)."
  1/5)

(defn slot-widths
  "Compute integer widths `[left center right]` for a header `cols` wide.
   `center = cols - left - right` so rounding errors never bleed off-screen."
  [cols]
  (let [cols   (max 0 (long cols))
        left   (long (* cols (double left-slot-ratio)))
        right  (long (* cols (double right-slot-ratio)))
        center (max 0 (- cols left right))]
    [left center right]))

;;; ── Tab sizing policy ──────────────────────────────────────────────────

(def tab-padding
  "Inner padding (in display cells / spaces) reserved on each side of a
   tab label so the label never crashes into the cell border."
  1)

(def tab-target-width
  "Natural width per tab used to drive the visible-tab clamp. Wider →
   fewer tabs fit before arrows appear; narrower → more tabs fit but
   labels truncate sooner."
  14)

(def min-visible-tabs
  "Lower clamp for visible tab count: even on narrowish screens we keep
   at least this many visible (the arrows reach the rest)."
  5)

(def max-visible-tabs
  "Upper clamp for visible tab count: huge screens stop spreading past
   this — past it the centre slot would look like a header full of
   tabs and nothing else."
  8)

(defn max-visible-count
  "How many tabs to show inside `width` cells.

   Policy (identical across channels):
   - clamp the natural fit `width / tab-target-width` to
     `[min-visible-tabs, max-visible-tabs]`,
   - never exceed `tabs-n` (cannot show what does not exist),
   - if even `min-visible-tabs` cannot fit the natural budget, fall
     back to the natural fit so tiny surfaces degrade gracefully."
  [tabs-n width]
  (let [width   (max 0 (long width))
        natural (max 1 (quot width (long tab-target-width)))
        clamped (max (long min-visible-tabs)
                  (min (long max-visible-tabs) natural))
        cap     (if (< natural (long min-visible-tabs)) natural clamped)]
    (min (long tabs-n) (long cap))))

;;; ── Glyphs ─────────────────────────────────────────────────────────────
;;
;; Unicode characters render fine in every channel we care about
;; (Lanterna terminal, modern HTML, Telegram); channels needing ASCII
;; fallbacks can swap on their side.

(def tab-arrow-left
  "Glyph for the `previous tabs` overflow affordance."
  "‹")

(def tab-arrow-right
  "Glyph for the `next tabs` overflow affordance."
  "›")

(def tab-ellipsis
  "Glyph appended when a tab label has to be truncated."
  "…")

(def copy-icon
  "Glyph used by the right-slot `copy conversation id` affordance."
  "⧉")

;;; ── Defaults ───────────────────────────────────────────────────────────

(def untitled-conversation-label
  "Default label every channel shows for a tab whose conversation has
   no title yet. Renamed by the state layer once a title is generated."
  "Untitled conversation")

(def id-display-chars
  "How many leading characters of the conversation UUID every channel
   shows next to the copy affordance. Matches `vis conversations`
   short form."
  8)

;;; ── Pure helpers ───────────────────────────────────────────────────────

(defn title-or-placeholder
  "Visible label for a conversation: its title if non-blank, otherwise
   `untitled-conversation-label`. Used by both the tab-label sync in
   state and the synthesised fallback tab in channels."
  [title]
  (if (and (string? title) (not (str/blank? title)))
    title
    untitled-conversation-label))

(defn short-id
  "Shorten a conversation UUID to the shared display length. Returns
   nil for blank input so channels can use truthy guards."
  [id]
  (let [id (some-> id str)]
    (when (seq id)
      (subs id 0 (min (long id-display-chars) (count id))))))
