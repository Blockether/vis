(ns com.blockether.vis.ext.channel-tui.components
  "Reusable header/tab UI components.

   Each drawable here is a self-contained fn that paints into a Lanterna
   `TextGraphics` and — when asked — registers its OWN click region, so the
   same widget can't drift between call sites (the bug that left the tab
   close `✕` painted in one place and made clickable in another, or not at
   all). Two kinds live here:

     - text layout helpers — `truncate-with-ellipsis`, `center-padded`
     - drawable components — `close-button!`, `tab-cell!`

   Components own their visual contract AND their interaction contract
   (the click region's `:kind`), keeping `header.clj` a thin layout caller."
  (:require
   [com.blockether.vis.ext.channel-tui.click-regions :as cr]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.theme :as t]
   [com.blockether.vis.internal.header :as vh]))

;; ── text layout ─────────────────────────────────────────────────────────────

(defn truncate-with-ellipsis
  "Truncate `s` so its display width fits in `max-cols`. When truncation
   actually happens, append `vh/workspace-ellipsis` so overflow is visible."
  ^String [s ^long max-cols]
  (let [s (or s "")]
    (cond
      (<= max-cols 0) ""
      (<= (p/display-width s) max-cols) s
      (= max-cols 1) (p/truncate-cols vh/workspace-ellipsis 1)
      :else (str (p/truncate-cols s (dec max-cols)) vh/workspace-ellipsis))))

(defn center-padded
  "Centre `s` inside a `cell-w`-wide cell with `vh/tab-entry-padding`
   reserved on each side; ellipsises overflow."
  ^String [s ^long cell-w]
  (let [inner     (max 0 (- cell-w (* 2 (long vh/tab-entry-padding))))
        text      (truncate-with-ellipsis s inner)
        text-w    (p/display-width text)
        pad-total (max 0 (- cell-w text-w))
        left      (quot pad-total 2)
        right     (- pad-total left)]
    (str (apply str (repeat left \space))
      text
      (apply str (repeat right \space)))))

;; ── close button ────────────────────────────────────────────────────────────

(def ^:const close-button-width
  "Cells a `close-button!` occupies: a 1-col `✕` framed by one padding space
   on each side, so it reads as a distinct block, not a stray glyph."
  3)

(def ^:private close-button-glyph " ✕ ")

(defn close-button!
  "Draw an INVERTED ` ✕ ` close button at (col,row): the host cell's
   (`cell-fg`,`cell-bg`) are SWAPPED so the button always reads as a distinct
   padded block regardless of the host's own colors, and register its
   `:close-tab` click region for `workspace-id`. Returns the consumed width
   (`close-button-width`)."
  [g col row cell-fg cell-bg workspace-id register?]
  (p/clear-styles! g)
  (p/set-colors! g cell-bg cell-fg)            ; inverted vs the host cell
  (p/enable! g p/BOLD)
  (p/put-str! g col row close-button-glyph)
  (p/clear-styles! g)
  (when register?
    (cr/register! {:bounds       {:row row :col col :width close-button-width}
                   :kind         :close-tab
                   :workspace-id workspace-id
                   :text         workspace-id
                   :enabled?     true}))
  close-button-width)

;; ── tab cell ────────────────────────────────────────────────────────────────

(defn tab-cell!
  "Draw one workspace tab into the band at [left,row] spanning `width` cells:

     - the bg slab (active = inverted bold; inactive = dim italic);
     - the centered `label` (already carrying any run-dot / spinner prefix),
       given `close-button-width` fewer cells so it never collides with the ✕;
     - an ALWAYS-VISIBLE `close-button!` pinned to the right edge (when the
       cell is wide enough to host both a title and the button).

   Registers the cell's `:workspace-entry` region FIRST, then the ✕
   `:close-tab` region ON TOP, so a click on the glyph wins the topmost
   (last-registered) lookup and closes the tab instead of selecting it.

   `opts` keys: :left :row :width :label :active? :workspace-id :index
   :register?"
  [g {:keys [left row width label active? workspace-id index register?]}]
  (let [width       (long width)
        ;; Reserve room for the close button only when the cell can still
        ;; show a sliver of title beside it; otherwise the title wins.
        show-close? (>= width (+ close-button-width 3))
        inner-w     (if show-close? (max 0 (- width close-button-width)) width)
        fg          (if active? t/header-active-tab-fg t/border-fg)
        bg          (if active? t/header-active-tab-bg t/dialog-bg)
        text        (center-padded label inner-w)]
    (p/clear-styles! g)
    (p/set-colors! g fg bg)
    (p/enable! g (if active? p/BOLD p/ITALIC))
    (p/fill-rect! g left row width 1)
    (p/put-str! g left row text)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds       {:row row :col left :width width}
                     :kind         :workspace-entry
                     :index        index
                     :workspace-id workspace-id
                     :text         workspace-id
                     :enabled?     true}))
    (when show-close?
      (close-button! g (+ (long left) width (- close-button-width)) row
        fg bg workspace-id register?))))

;; ── inert title ─────────────────────────────────────────────────────────────

(defn title!
  "Paint an INERT, centered header title in [left,row] over `width` cells —
   bold on the header surface, ellipsised on overflow. Registers no click
   region: a single-workspace title is not a switcher and must not look or
   behave like one."
  [g row left width text]
  (when (pos? (long width))
    (let [shown (truncate-with-ellipsis text width)
          w     (p/display-width shown)
          col   (+ (long left) (max 0 (quot (- (long width) w) 2)))]
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g col row shown)
      (p/clear-styles! g))))

;; ── nav arrow ───────────────────────────────────────────────────────────────

(defn nav-arrow!
  "Paint a workspace-overflow navigation arrow `glyph` at (col,row) and
   register its `:workspace-entry` click region keyed by `direction`
   (`:prev` / `:next`), so a click cycles the visible tab window. Brightens on
   hover."
  [g row col glyph direction register?]
  (let [hovered  (cr/hovered)
        hovered? (and (= :workspace-entry (:kind hovered))
                   (= direction (:index hovered))
                   (= row (get-in hovered [:bounds :row])))]
    (p/clear-styles! g)
    (p/set-colors! g (if hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col row glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds       {:row row :col col :width (p/display-width glyph)}
                     :kind         :workspace-entry
                     :index        direction
                     :workspace-id direction
                     :text         direction
                     :enabled?     true}))))
