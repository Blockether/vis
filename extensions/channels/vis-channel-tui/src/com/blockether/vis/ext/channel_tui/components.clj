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
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.header :as vh]
            [com.blockether.vis.internal.format :as fmt]))
;; ── text layout ─────────────────────────────────────────────────────────────
(defn truncate-with-ellipsis
  "Truncate `s` so its display width fits in `max-cols`. When truncation
   actually happens, append `vh/workspace-ellipsis` so overflow is visible."
  ^String [s ^long max-cols]
  (let [s (or s "")
        ;; Reserve the ellipsis's ACTUAL display width, not 1: `…` (U+2026) is
        ;; EAW=A and renders TWO columns on ambiguous-wide terminals, so a fixed
        ;; `(dec max-cols)` over-ran the cell by a column.
        ew (long (p/display-width vh/workspace-ellipsis))]
    (cond (<= max-cols 0) ""
      (<= (p/display-width s) max-cols) s
      (<= max-cols ew) (p/truncate-cols vh/workspace-ellipsis max-cols)
      :else (str (p/truncate-cols s (- max-cols ew)) vh/workspace-ellipsis))))
(defn center-padded
  "Centre `s` inside a `cell-w`-wide cell with `vh/tab-entry-padding`
   reserved on each side; ellipsises overflow."
  ^String [s ^long cell-w]
  (let [inner (max 0 (- cell-w (* 2 (long vh/tab-entry-padding))))
        text (truncate-with-ellipsis s inner)
        text-w (p/display-width text)
        pad-total (max 0 (- cell-w text-w))
        left (quot pad-total 2)
        right (- pad-total left)]
    (str (apply str (repeat left \space)) text (apply str (repeat right \space)))))
;; ── close button ────────────────────────────────────────────────────────────
(def ^{:const true} close-button-width
  "Cells a `close-button!` occupies: a leading space + 1-col `✕` + a\n  trailing space. No divider — the bare ✕ reads as a button, not `|x`."
  3)
(def ^{:private true} close-button-glyph " ✕ ")
(defn close-button!
  "Draw a ` ✕ ` close affordance (space-padded ✕, no divider) at (col,row).\n   At rest the cap is INVERTED — the tab's `tab-fg` becomes the chip bg and\n   `tab-bg` paints the ✕ — so even before you reach it the close target reads\n   as a distinct little button standing out from the tab surface. On hover the\n   chip escalates to a red pill (`close-button-hover-fg` bg behind a white\n   `header-active-tab-fg` ✕) to signal the destructive click. Registers its\n   `:close-tab` click region for `workspace-id`. Returns the consumed width\n   (`close-button-width`)."
  [g col row tab-fg tab-bg workspace-id register?]
  (let [hovered (cr/hovered)
        hovered? (and (= :close-tab (:kind hovered)) (= workspace-id (:workspace-id hovered)))]
    (p/clear-styles! g)
    (p/set-colors! g
      (if hovered? t/header-active-tab-fg tab-bg)
      (if hovered? t/close-button-hover-fg tab-fg))
    (p/enable! g p/BOLD)
    (p/put-str! g col row close-button-glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col col, :width close-button-width},
                     :kind :close-tab,
                     :workspace-id workspace-id,
                     :text workspace-id,
                     :enabled? true}))
    close-button-width))
(defn button!
  "Generic clickable button: paint `label` (already space-padded, e.g. \" < \") as
   a filled accent cap at (col,row) — brighter on hover — AND register its click
   region under `:kind`, plus any identity keys in `extra` (merged into both the
   region and the hover match). One widget owns its look, its hover, and its
   click, so they can't drift apart. Returns the consumed width.

   Hover is detected by matching the registered `:kind` and every `extra` key
   against `cr/hovered`, so two buttons of the same kind but different ids stay
   independent."
  ([g col row label kind] (button! g col row label kind nil))
  ([g col row label kind {:keys [extra danger? register?] :or {register? true}}]
   (let [w        (long (p/display-width label))
         hov      (cr/hovered)
         hovered? (and (= kind (:kind hov))
                    (every? (fn [[k v]] (= v (get hov k))) extra))]
     (p/clear-styles! g)
     ;; ONE button language across the whole TUI — the SAME chip every modal's
     ;; ✕ uses (`dialog-close-button!`): an inverted title-strip cap at rest, and
     ;; on hover it lifts to the accent (or the destructive red for `:danger?`
     ;; actions like close), always bold. So find-bar and dialog buttons read as
     ;; the same control instead of two palettes.
     (cond
       (and hovered? danger?) (p/set-colors! g t/header-active-tab-fg t/close-button-hover-fg)
       hovered?               (p/set-colors! g t/header-active-tab-fg t/header-active-tab-accent)
       :else                  (p/set-colors! g t/button-fg t/button-bg))
     (when hovered? (p/enable! g p/BOLD))
     (p/put-str! g col row label)
     (p/clear-styles! g)
     (when register?
       (cr/register! (merge {:bounds {:row row, :col col, :width w}, :kind kind, :enabled? true}
                       extra)))
     w)))

(def ^:private find-bar-buttons
  "Trailing buttons of the find bar: [click-kind glyph-label]. Padded GLYPHS —
   ◀ ▶ (narrow geometric) and ✕ (1-cell, as `close-button!` uses it). Spaced
   apart by `:gap` ops in `find-bar!`."
  [[:search-prev " ◀ "] [:search-next " ▶ "] [:search-close " ✕ "]])

(def ^:private find-input-width
  "Cells reserved for the white query field, so the box doesn't jitter as you
   type."
  22)

(defn find-bar!
  "Browser-style in-session find WIDGET: a single-line BORDERED box, right-aligned
   at the top of the messages area, holding a WHITE input field (the live query),
   the i/N match count, and spaced ◀ ▶ ✕ glyph buttons (each its own click region
   via `button!`, so the mouse drives the same `:search-*` events as
   Ctrl+P / Ctrl+N / F3). `search` is app-db's `:search` map; no-op when inactive."
  [g cols text-top {:keys [active? query hits index case? total]}]
  (when active?
    (let [n     (long (count hits))
          q     (str query)
          qshow (if (str/blank? q) "type to search…" q)
          ;; The white input field carries its OWN inner padding — a space each
          ;; side on input-field-bg — so the query text isn't jammed against the
          ;; field edge. That's padding the box border can't give.
          qfield (let [s (truncate-with-ellipsis qshow find-input-width)]
                   (str " " s
                     (apply str (repeat (max 0 (- find-input-width (long (p/display-width s)))) \space))
                     " "))
          cnt   (format " %-9s" (cond (str/blank? q) ""
                                  (zero? n)      "0/0"
                                  :else          (str (inc (long (or index 0))) "/" n
                                                   (when (> (long (or total n)) n)
                                                     (str " (" total ")")))))
          ;; content ops. :input is the white field; :chrome/:gap ride the box bg;
          ;; :btn delegates to the reusable button! widget.
          ;; Aa chip leads the buttons: [Aa] = case-sensitive ON, " Aa " = off.
          btns  (cons [:search-case (if case? "[Aa]" " Aa ")] find-bar-buttons)
          ops   (concat
                  [[:chrome " "] [:input qfield] [:chrome "  "] [:chrome cnt] [:chrome " "]]
                  (interpose [:gap " "] (map (fn [[k l]] [:btn k l]) btns))
                  [[:chrome " "]])
          content-w (long (reduce + (map (fn [op] (long (p/display-width (last op)))) ops)))
          box-w (+ content-w 2)
          box-l (max 0 (- (long cols) box-w 2))
          box-t (long text-top)
          row   (inc box-t)
          x0    (inc box-l)]
      ;; box: fill bg, then single-line border
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/fill-rect! g box-l box-t box-w 3)
      (p/draw-box! g box-l box-t box-w 3)
      ;; content row (vertically centred between the borders)
      (reduce
        (fn [x op]
          (case (first op)
            :btn   (let [kind (nth op 1)]
                     (+ x (button! g x row (nth op 2) kind
                            (when (= kind :search-close) {:danger? true}))))
            ;; input field — the canonical `box-fg` on `input-field-bg`, the same
            ;; interior `draw-text-input-field!` paints, so every input matches.
            :input (do (p/clear-styles! g)
                     (p/set-colors! g t/box-fg t/input-field-bg)
                     (p/put-str! g x row (last op))
                     (+ x (long (p/display-width (last op)))))
            ;; chrome / gaps ride the dialog body palette.
            (do (p/clear-styles! g)
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/put-str! g x row (last op))
              (+ x (long (p/display-width (last op)))))))
        x0
        ops))))

(def ^{:private true} header-fkeys "Always-on clickable header chips: the two panel toggles (F1 help, F2\n   context) plus F3 search. Every other shortcut lives in the F1 help overlay,\n   so the header stays uncluttered. `[click-kind label]`." [[:header-help " F1 "] [:header-tasks " F2 "] [:header-search " F3 "]])

(def header-fkeys-width "Cells the header F-key chips occupy: three 4-col chips, each followed by a\n   1-col gap. The header reserves this much before the notification slot." 15)

(defn header-fkeys! "Paint the F1/F2/F3 clickable chips at (x,row) - via the shared `button!` so\n   they match every other button - registering their `:header-help` /\n   `:header-tasks` / `:header-search` click regions. Returns the x after the\n   chips (+ trailing gap)." [g x row] (reduce (fn [x [kind label]] (+ 1 x (button! g x row label kind))) x header-fkeys))

(def ^{:private true} tab-divider-glyph
  ;; U+250A LIGHT QUADRUPLE DASH VERTICAL — a soft, dotted separator that
  ;; reads gentler than a solid │ between tabs / cluster chips. Box-drawing
  ;; (EAW=A → one column in target terminals, same class as the borders).
  "┊")
(defn tab-divider!
  "Paint a 1-col dotted `┊` divider (between tabs, or between header-cluster
   chips) at (col,row). Painted in the high-contrast accent (dark on a light
   terminal, light on a dark one) so the sparse dotted glyph is actually
   visible — `footer-fg` washed it out."
  [g col row]
  (p/clear-styles! g)
  (p/set-colors! g t/text-fg t/terminal-bg)
  (p/put-str! g col row tab-divider-glyph)
  (p/clear-styles! g))
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
  [g
   {:keys [left row width label prefix tab-no active? workspace-id index register? closable?],
    :or {closable? true, prefix ""}}]
  (let [width (long width)
        ;; Reserve room for the close button only when the cell can still
        ;; show a sliver of title beside it; otherwise the title wins.
        show-close? (and closable? (>= width (+ close-button-width 3)))
        inner-w (if show-close? (max 0 (- width close-button-width)) width)
        fg (if active? t/header-active-tab-fg t/border-fg)
        bg (if active? t/header-active-tab-bg t/dialog-bg)
        prefix (let [p (or prefix "")
                     w (long (p/display-width p))]
                 (if (< w 2) (str p (apply str (repeat (- 2 w) \space))) p))
        num-str (when tab-no (str tab-no))
        display (if num-str (str prefix num-str " | " label) (str prefix label))
        text (center-padded display inner-w)
        ;; Where the tab number lands once `display` is centre-padded, so we can
        ;; repaint just those digits in a contrasting colour — the index reads
        ;; as a distinct badge instead of blending into the title text.
        lead (+ (count (take-while #(= \space %) text))
               (- (long (p/display-width prefix)) (count (take-while #(= \space %) prefix))))
        num-fg (if active? t/header-tab-number-fg t/header-active-tab-accent)]
    (p/clear-styles! g)
    (p/set-colors! g fg bg)
    (p/enable! g (if active? p/BOLD p/ITALIC))
    (p/fill-rect! g left row width 1)
    (p/put-str! g left row text)
    (when (and num-str
            (<= (+ lead (count num-str)) (count text))
            (= num-str (subs text lead (+ lead (count num-str)))))
      (p/clear-styles! g)
      (p/set-colors! g num-fg bg)
      (p/enable! g p/BOLD)
      (p/put-str! g (+ (long left) lead) row num-str))
    (when (and num-str (<= (+ lead (count num-str) 3) (count text)))
      (p/clear-styles! g)
      (p/set-colors! g fg bg)
      (when active? (p/enable! g p/BOLD))
      (p/put-str! g (+ (long left) lead (count num-str) 1) row "|"))
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col left, :width width},
                     :kind :workspace-entry,
                     :index index,
                     :workspace-id workspace-id,
                     :text workspace-id,
                     :enabled? true}))
    (when show-close?
      (close-button! g
        (+ (long left) width (- close-button-width))
        row
        fg
        bg
        workspace-id
        register?))))
;; ── inert title ─────────────────────────────────────────────────────────────
(defn title!
  "Paint an INERT, centered header title in [left,row] over `width` cells —
   bold on the header surface, ellipsised on overflow. Registers no click
   region: a single-workspace title is not a switcher and must not look or
   behave like one."
  [g row left width text]
  (when (pos? (long width))
    (let [shown (truncate-with-ellipsis text width)
          w (p/display-width shown)
          col (+ (long left) (max 0 (quot (- (long width) w) 2)))]
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g col row shown)
      (p/clear-styles! g))))
;; ── nav arrow ───────────────────────────────────────────────────────────────
(defn nav-arrow!
  "Paint a workspace-overflow navigation arrow `glyph` at (col,row) as a FILLED
   BUTTON CHIP - routed through the shared `button!`, so the < > read as the
   SAME control as the F1/F2 and id-copy buttons (inverted cap at rest, lifts to
   the accent + bold on hover) instead of a bare character. Registers its
   `:workspace-entry` click region keyed by `direction` (`:prev` / `:next`), so a
   click still cycles the visible tab window."
  [g row col glyph direction register?]
  (button! g col row (str " " glyph " ") :workspace-entry
    {:extra {:index direction :workspace-id direction :text direction}
     :register? register?}))
;; ── help overlay ────────────────────────────────────────────────────────────
(def ^:private help-shortcuts
  "[[keys description] …] rows shown in the Ctrl+H help card."
  [["F1" "Toggle this help"] ["F2" "Toggle the context panel"] ["F3" "Search in session"]
   ["F4" "Managed resources"] ["F5 · Ctrl+K" "Command palette"]
   ["F6 \u00b7 Ctrl+G" "Sessions \u00b7 workspaces"]
   ["F7" "Review proposed plan (approve / reject / comment)"]
   ["Search: F3" "Type to filter · Ctrl+N/P next/prev · Alt+C case · Esc close"]
   ["Enter · Ctrl+X" "Send message"]
   ["Alt+Enter" "Insert a newline"] ["Esc" "Clear draft · cancel turn"]
   ["Ctrl+C" "Cancel turn · quit"] ["Tab · Shift+Tab" "Switch tab"] ["Alt+1…9" "Jump to tab N"]
   ["Ctrl+W" "Close tab  (or click the ✕)"] ["Ctrl+T" "Cycle model"]
   ["Ctrl+R" "Cycle reasoning effort"]
   ["Ctrl+L" "Cycle Codex verbosity"] ["Ctrl+A · Ctrl+E" "Jump to line start · end"]
   ["Ctrl+U" "Delete to line start"] ["Alt+B · Alt+F" "Move word left · right"] ["Ctrl+V" "Paste"]
   ["@" "Pick a file"] ["Mouse" "Click a tab to switch · ✕ to close"]])
;; ── header band chrome ──────────────────────────────────────────────────────
(defn band-rule!
  "Paint a full-width single-line horizontal rule across `cols` on `row`."
  ([g row cols] (band-rule! g row cols t/footer-fg-muted))
  ([g row cols fg]
   (p/clear-styles! g)
   (p/set-colors! g fg t/terminal-bg)
   (dotimes [c (long cols)] (p/set-char! g c row p/BOX_H))
   (p/clear-styles! g)))
(defn- level->fg
  "Map a notification level to a foreground color; unknown levels fall back
   to the muted-footer color so something still renders."
  [level]
  (case level
    :success t/footer-fg-strong
    :warn t/footer-warning-fg
    :error t/footer-error-fg
    :info t/footer-spinner-fg
    t/footer-fg-muted))
(defn notification-slot!
  "Paint the header's left-slot notification/status `text` at (col,row),
   colored by `level` (bold). No-op for blank text."
  [g col row text level]
  (when (seq text)
    (p/clear-styles! g)
    (p/set-colors! g (level->fg level) t/terminal-bg)
    (p/enable! g p/BOLD)
    (p/put-str! g col row text)
    (p/clear-styles! g)))
(defn id-badge!
  "Paint the session-id COPY BUTTON `text` at (col,row) as a filled chip — via
   the shared `button!`, so it reads as the SAME control as the F1/F2 buttons
   instead of bare text — and register its `:copy-id` click region carrying the
   FULL uuid (the click handler drops it on the clipboard). The `text` already
   carries the leading ⧉ copy glyph from `header/id-copy-block-text`. Brightens
   to the accent on hover. No-op for blank text."
  [g col row text full-uuid register?]
  (when (pos? (p/display-width text))
    (button! g col row text :copy-id
      {:extra {:text full-uuid},
       :register? (boolean (and register? full-uuid))})))
(defn header-badge!
  "Paint a clickable header chip `glyph` at (col,row) and register a click
   region of `kind` (e.g. :toggle-tasks / :toggle-help). Brightens on hover.
   Returns the consumed width. A terminal-safe, always-visible stand-in for
   the F1/F2 accelerators."
  [g col row glyph kind register?]
  (let [hovered (cr/hovered)
        hovered? (= kind (:kind hovered))
        w (p/display-width glyph)]
    (p/clear-styles! g)
    (p/set-colors! g (if hovered? t/header-hover-fg t/text-fg) t/terminal-bg)
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col row glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col col, :width w}, :kind kind, :enabled? true}))
    w))
;; ── help overlay ────────────────────────────────────────────────────────────
(defn- pad-right
  ^String [^String s ^long w]
  (str s (apply str (repeat (max 0 (- w (p/display-width s))) \space))))
(defn dialog-close-button!
  "Draw a clickable `✕` close chip at a dialog's top-right title corner —
   the SAME button every modal dialog shows — and register its `kind`
   click region (`:toggle-help` / `:toggle-tasks`) so a mouse click
   dismisses the overlay. Inverted title chip at rest; red pill on hover.
   Returns nil."
  [g bounds kind]
  (let [{:keys [top right]} bounds
        hovered (cr/hovered)
        hovered? (= kind (:kind hovered))
        title-row (inc (long top))
        label " ✕ "
        w (p/display-width label)
        col (- (long right) w)]
    (p/clear-styles! g)
    (p/set-colors! g
      (if hovered? t/header-active-tab-fg t/dialog-title-bg)
      (if hovered? t/close-button-hover-fg t/dialog-title-fg))
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col title-row label)
    (p/clear-styles! g)
    (cr/register! {:bounds {:row title-row, :col col, :width w}, :kind kind, :enabled? true})
    nil)) (defn- box-grid-lines "Render `rows` (each `[left-cell right-cell]`) as a 2-column box-drawing grid:\n   a top/bottom rule, `├┼┤` separators between rows, `│`-framed cells padded to\n   `key-w` / `desc-w`. `bd` is the border color, `key-fg` / `desc-fg` the cell\n   text colors. Returns a vec of segment-rows ready for the overlay painter -\n   the self-contained table renderer the F1 help card uses." [rows key-w desc-w bd key-fg desc-fg] (let [key-w (long key-w) desc-w (long desc-w) bar (fn [l m r] (str l (apply str (repeat (+ key-w 2) "─")) m (apply str (repeat (+ desc-w 2) "─")) r)) rule (fn [l m r] [[(bar l m r) bd false]]) row-segs (fn [[k d]] [["│ " bd false] [(pad-right (str k) key-w) key-fg true] [" │ " bd false] [(pad-right (str d) desc-w) desc-fg false] [" │" bd false]])] (vec (concat [(rule "┌" "┬" "┐")] (interpose (rule "├" "┼" "┤") (mapv row-segs rows)) [(rule "└" "┴" "┘")])))) (defn scrollable-dialog-body! "Paint the scroll plumbing both modal overlays (F1 help, F2 context) share:\n   clamp `scroll` to `[0, (- (count lines) content-h)]`, window `lines` by that\n   effective offset and paint each visible row via `paint-line` (a\n   `(fn [screen-row-i line])`), draw the shared `scrollbar/draw!` in `sb-col`\n   when the content overflows, and a right-aligned `N-M / total` position hint\n   on `hint-row` (anchored to `body-right`). `geom` is\n   `{:content-top :content-h :hint-row :sb-col :body-right}`. Returns\n   `{:scroll :max-scroll :sb? :shown-n}` so callers feed the clamp back and\n   derive their own geometry (e.g. F2's selectable ranges)." [g lines {:keys [content-top content-h hint-row sb-col body-right]} scroll paint-line] (let [n (count lines) max-scroll (max 0 (- n content-h)) eff (max 0 (min (long (or scroll 0)) max-scroll)) sb? (> n content-h) shown-n (min content-h (- n eff))] (dotimes [i shown-n] (paint-line i (nth lines (+ eff i)))) (when sb? (scrollbar/draw! g {:col sb-col, :top content-top, :track-h content-h, :total-h n, :inner-h content-h, :scroll eff})) (when (and hint-row sb?) (let [pos (str (inc eff) "–" (+ eff shown-n) " / " n) pw (p/display-width pos)] (p/clear-styles! g) (p/set-colors! g t/dialog-hint t/dialog-bg) (p/put-str! g (- body-right pw) hint-row pos))) {:scroll eff, :max-scroll max-scroll, :sb? sb?, :shown-n shown-n}))

(defn help-overlay! "Draw the keyboard-shortcut help as a dialog, using the shared\n   `dialogs/draw-dialog-chrome!` + `dialog-layout` so it matches the F2\n   context panel (shadow, border, accent title bar, centered hint row).\n   The grid (built by `box-grid-lines`) spans the full dialog inner width;\n   the desc column stretches to fill, narrowed by one gutter column when the\n   body overflows so the shared `scrollable-dialog-body!` scrollbar gets its\n   own lane instead of landing on the right border. Registers only its\n   close-button click region; the caller dismisses it (Ctrl+H / F1 / any key).\n   Returns `{:scroll :max-scroll}` so the caller can feed the clamp back,\n   exactly like `context-overlay!`." [g cols rows scroll] (let [title "Keyboard shortcuts" key-w (reduce max 0 (map (comp p/display-width first) help-shortcuts)) base-desc-w (reduce max 0 (map (comp p/display-width second) help-shortcuts)) bd t/dialog-border line-cnt (inc (* 2 (count help-shortcuts))) bounds (dialogs/draw-dialog-chrome! g cols rows title line-cnt) {:keys [left inner-w]} bounds {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds line-cnt) sb? (> line-cnt content-h) gutter (if sb? 1 0) desc-w (max base-desc-w (- inner-w key-w 8 gutter)) lines (box-grid-lines help-shortcuts key-w desc-w bd t/footer-fg-strong t/footer-fg) paint-line (fn [i segs] (let [r (+ content-top i)] (loop [x (+ left 2) ss segs] (when-let [[text color bold?] (first ss)] (let [avail (max 0 (- (+ left 1 inner-w) x)) shown (dialogs/ellipsize (str text) avail)] (p/clear-styles! g) (p/set-colors! g color t/dialog-bg) (when bold? (p/enable! g p/BOLD)) (p/put-str! g x r shown) (recur (+ x (p/display-width shown)) (next ss))))))) geom (scrollable-dialog-body! g lines {:content-top content-top, :content-h content-h, :hint-row hint-row, :sb-col (+ left inner-w), :body-right (+ left 1 inner-w)} scroll paint-line)] (dialog-close-button! g bounds :toggle-help) (p/clear-styles! g) (select-keys geom [:scroll :max-scroll])))
;; ── tasks overlay (W3: user-visible :session/tasks) ──────────────────────────
(defn- task-status-glyph
  [status]
  (case status
    :candidate "◇"
    :done "✓"
    :doing "◐"
    :cancelled "✗"
    :rejected "⊘"
    "○"))
(defn- task-status-color
  [status]
  (case status
    :candidate t/warning-fg
    :done t/status-ok
    :doing t/warning-fg
    :cancelled t/cancelled-fg
    :rejected t/cancelled-fg
    t/footer-fg-muted))
(def ^:private task-status-rank {:candidate 0, :doing 1, :todo 2, :done 3, :cancelled 4, :rejected 5})
(defn- clip-str
  "Truncate `s` to `w` display columns with a trailing ellipsis.
   MUST clip by display width, not char index: `subs` indexes characters, so
   for any string with wide graphemes (display-width > char-count) `(dec w)`
   could exceed `(.length s)` and throw StringIndexOutOfBoundsException every
   frame — which froze the whole F2 context overlay. `p/truncate-cols` walks
   graphemes and never overruns the string."
  ^String [^String s ^long w]
  (if (<= (p/display-width s) w) s (str (p/truncate-cols s (max 0 (dec w))) "…")))
(def ^:private overlay-blank-row
  "A single empty overlay row — the spacer between context-dialog entries."
  [["" t/dialog-hint false]])
(def ^{:private true} overlay-card-indent
  "Leading columns inset each task/fact card from BOTH dialog rails (left via\n   indent-rows, right via the narrowed body width at the call sites)."
  3)
(defn- indent-rows
  "Prefix every row with `indent` (default `overlay-card-indent`) leading\n   spaces so the whole card insets from the dialog left rail. Archived\n   tasks pass a larger indent so their cards nest under the indented\n   ARCHIVED TASKS header."
  ([rows] (indent-rows rows overlay-card-indent))
  ([rows indent]
   (let [pad [(apply str (repeat indent \space)) t/dialog-hint false]]
     (mapv (fn [row] (with-meta (into [pad] row) (meta row))) rows))))
(defn- wrap-cols
  "Greedy display-width word-wrap to a vec of lines, each fitting `w` columns.
   Delegates to the native, grapheme/EAW-aware `p/word-wrap` (one shared
   implementation in the lanterna fork) — a token wider than `w` is hard-split
   at grapheme boundaries, blank input yields `[\"\"]`."
  [s ^long w]
  (p/word-wrap s w))
(defn- justify-line
  "Full-justify `s` to `w` display columns by spreading extra spaces between\n   words. Returns `s` untouched when it has fewer than two words or already\n   fills `w`."
  [s ^{:tag long} w]
  (let [words (str/split (str/trim s) #"\s+")
        n (count words)]
    (if (< n 2)
      s
      (let [text-w (reduce + (map p/display-width words))
            gaps (dec n)
            slack (- w text-w gaps)]
        (if (<= slack 0)
          s
          (let [base (quot slack gaps)
                extra (rem slack gaps)]
            (apply str
              (interleave
                words
                (concat (map (fn [i] (apply str (repeat (+ 1 base (if (< i extra) 1 0)) \space)))
                          (range gaps))
                  [""])))))))))
(defn- wrapped-rows
  "Rows for `text` wrapped to `w` columns: the FIRST row is prefixed by the\n   `head` segments (e.g. a colored glyph) and every continuation row is\n   indented by `indent` spaces so it aligns under the head. Non-final lines\n   are full-justified to `w`. Each row is a vec of `[text color bold?]`\n   segments. `indent` MUST equal the head's display width for clean alignment."
  [head indent text w body-color bold?]
  (let [pieces (wrap-cols text (long w))
        last-i (dec (count pieces))
        pad (apply str (repeat (long indent) \space))]
    (vec (map-indexed (fn [i piece]
                        (let [piece (if (< i last-i) (justify-line piece (long w)) piece)]
                          (if (zero? i)
                            (conj (vec head) [piece body-color bold?])
                            [[(str pad piece) body-color bold?]])))
           pieces))))
(defn- run->seg
  "Convert one styled IR run into a `[text color bold?]` overlay segment.
   `code` / link spans take the accent color; `bold` (or the caller's
   `base-bold?`) sets the bold flag; everything else inherits `base-color`."
  [{:keys [text style]} base-color base-bold?]
  (let [accent? (or (contains? style :code) (contains? style :link))]
    [text (if accent? t/header-active-tab-accent base-color)
     (boolean (or base-bold? (contains? style :bold)))]))
(defn- justify-segs
  "Full-justify a row of `[text color bold?]` segments to `w` display columns by\n   widening every inter-word whitespace run across the segments. Returns the\n   segments untouched when there are no gaps or no slack (single word / already\n   full). The styled twin of `justify-line` for `md-wrapped-rows`.\n\n   `prefix-n` leading segments are STRUCTURAL — a list marker (`- `, `• `,\n   `1. `) or a continuation hanging-indent (`  `). Their widths still count\n   toward the slack budget, but their whitespace is never stretched, so a\n   bulleted line keeps a single space after the marker (`- foo bar baz`,\n   not `-    foo  bar  baz`) while the content still justifies edge-to-edge."
  ([segs ^{:tag long} w] (justify-segs segs w 0))
  ([segs ^{:tag long} w ^{:tag long} prefix-n]
   (let [texts (map first segs)
         content-texts (drop prefix-n texts)
         gap-count (reduce + (map (fn [t] (count (re-seq #"\s+" t))) content-texts))
         text-w (reduce + (map p/display-width texts))
         slack (- w text-w)]
     (if (or (< gap-count 1) (<= slack 0)
           ;; Stretch cap: only justify lines that are ALREADY near-full.
           ;; When `slack >= gap-count`, every gap would grow by ≥1 space
           ;; (single→double across the whole line — the "A  lighter
           ;; alternative" rivers). Leave those ragged-right; only lines
           ;; within `gap-count` columns of full get the gentle treatment,
           ;; where `slack` gaps gain +1 space (max 2 per gap) and the rest
           ;; stay single.
           (>= slack gap-count))
       segs
       (let [base (quot slack gap-count)
             extra (rem slack gap-count)
             idx (atom -1)]
         (vec
           (map-indexed
             (fn [si [t color bold?]]
               (if (< si prefix-n)
                 [t color bold?]
                 [(str/replace t
                    #"\s+"
                    (fn [m]
                      (let [i (swap! idx inc)
                            add (+ base (if (< i extra) 1 0))]
                        (str m (apply str (repeat add \space))))))
                  color bold?]))
             segs)))))))
(defn- md-wrapped-rows
  "Markdown-aware `wrapped-rows`: `text` is lifted to canonical IR via\n   `vis/markdown->ir`, wrapped to `w` columns by the shared IR walker, and\n   each line's styled runs become `[text color bold?]` segments — **bold**,\n   `code`, and links render inline (markup stripped). The FIRST row is\n   prefixed by `head`; continuation rows indent `indent` spaces. Runs of\n   blank inter-block lines collapse to a single separator; `-`/`1.` list items\n   keep a `• ` marker. Only lines the wrapper broke on overflow (those\n   the IR walker tags `:wrap?`) are full-justified to `w` via\n   `justify-segs`; paragraph/block-terminal lines stay ragged-right so\n   short tails aren't stretched edge-to-edge."
  [head indent text w base-color base-bold?]
  (let [ir (vis/markdown->ir (str text))
        blank-line? (fn [{:keys [runs]}] (every? (fn [r] (str/blank? (:text r))) runs))
        lines (->> (ir-tui/ir->lines ir (long w))
                   ;; Collapse runs of blank inter-block lines to a single
                   ;; separator (and trim leading/trailing) so multi-paragraph /
                   ;; bulleted task bodies keep their block breaks instead of
                   ;; rendering as one crushed run — without opening huge gaps.
                (reduce (fn [acc line]
                          (if (blank-line? line)
                            (if (or (empty? acc) (blank-line? (peek acc))) acc (conj acc line))
                            (conj acc line)))
                  [])
                ((fn [v] (if (and (seq v) (blank-line? (peek v))) (pop v) v))))
        pad (apply str (repeat (long indent) \space))]
    (if (empty? lines)
      [(vec head)]
      (vec (map-indexed
             (fn [i {:keys [runs wrap?]}]
               (let [segs (mapv (fn [r] (run->seg r base-color base-bold?)) runs)
                     segs (if (seq segs) segs [["" base-color base-bold?]])
                     ;; Leading structural runs — a list `:marker` (`- `,
                     ;; `• `, `1. `) or the pure-whitespace hanging-indent on
                     ;; a wrapped continuation — must NOT be stretched, else
                     ;; justification blows a hole right after the bullet
                     ;; (`-      foo`). Count them so `justify-segs` protects
                     ;; that prefix and only justifies the content gaps.
                     prefix-n (count (take-while
                                       (fn [r] (or (contains? (:style r) :marker)
                                                 (str/blank? (:text r))))
                                       runs))
                     ;; Only lines the wrapper broke on overflow (`:wrap?`)
                     ;; get full-justified. Paragraph/block-terminal lines
                     ;; are short and ragged-right by nature — stretching
                     ;; them edge-to-edge is the "4 words, mega holes" bug.
                     segs (if wrap? (justify-segs segs (long w) (long prefix-n)) segs)]
                 (if (zero? i) (into (vec head) segs) (into [[pad base-color base-bold?]] segs))))
             lines)))))
(defn- task-entry-rows
  "Progressive-disclosure rows for ONE task. SETTLED tasks
   (done/cancelled/rejected/deferred) collapse to a single dim line -
   glyph + title - finished work reads as a quiet receipt. OPEN tasks
   (doing/todo/candidate) render the full card: colored glyph + wrapped
   bold title, a colored meta row (human status word +
   verified/unverified badge), then the contract as labelled dim
   sub-rows when present: rationale, acceptance, files, avoid,
   evidence, dependencies and linked facts. Everything wraps to
   `body-w`; `indent` left-insets the card. Rows are
   `[text color bold?]` segment vecs."
  [k t body-w indent]
  (let [status (or (:status t) :todo)
        settled? (contains? #{:done :cancelled :rejected :deferred} status)
        glyph-seg [(str (task-status-glyph status) " ") (task-status-color status) true]
        title-base (or (not-empty (str (:title t))) (name k))]
    (if settled?
      (conj (indent-rows (vec (md-wrapped-rows [glyph-seg] 2 title-base
                                (max 6 (- body-w 2)) t/footer-fg-muted false))
              indent)
        overlay-blank-row)
      (let [status-label (case status
                           :doing "in progress"
                           :todo "pending"
                           :candidate "proposed · awaiting review"
                           (name status))
            verify-seg (cond (:verified? t) ["  ✓ verified" t/status-ok false]
                         (:acceptance t) ["  ⚠ unverified" t/warning-fg false]
                         :else nil)
            meta-row (into [["  " t/footer-fg-muted false]
                            [status-label (task-status-color status) true]]
                       (when verify-seg [verify-seg]))
            sub-w (max 6 (- body-w 4))
            labelled (fn [marker text]
                       (when-let [s (not-empty (str text))]
                         (md-wrapped-rows [[(str "  " marker " ") t/footer-fg-muted false]]
                           4 s sub-w t/footer-fg-muted false)))
            joined (fn [marker xs sep]
                     (when (seq xs)
                       (wrapped-rows [[(str "  " marker " ") t/footer-fg-muted false]]
                         4 (str/join sep (map str xs)) sub-w t/footer-fg-muted false)))
            rationale-rows (labelled "≡" (:rationale t))
            accept-rows (labelled "▸" (:acceptance t))
            files-rows (joined "▢" (:files t) "  ·  ")
            avoid-rows (joined "⊘" (:avoid t) "  ·  ")
            evidence-rows (labelled "⚑" (:evidence t))
            dep-rows (when (seq (:depends_on t))
                       (wrapped-rows [["  ↳ needs " t/footer-fg-muted false]]
                         4 (str/join ", " (map pr-str (:depends_on t))) sub-w
                         t/footer-fg-muted false))
            fact-rows (joined "⛁ facts" (:facts t) ", ")]
        (-> (vec (md-wrapped-rows [glyph-seg] 2 title-base (max 6 (- body-w 2)) t/dialog-fg true))
          (conj meta-row)
          (conj overlay-blank-row)
          (into rationale-rows)
          (into accept-rows)
          (into files-rows)
          (into avoid-rows)
          (into evidence-rows)
          (into dep-rows)
          (into fact-rows)
          (indent-rows indent)
          (conj overlay-blank-row)
          (conj overlay-blank-row))))))
(defn- task-overlay-lines
  "TASKS section body with progressive disclosure: a progress header
   (▰▰▱▱ bar + `N of M done`), then one `task-entry-rows`
   entry per task, status-sorted (candidate → doing → todo →
   done → cancelled). Settled tasks collapse to one dim line; open
   tasks show their full card. Empty state is a single hint row.
   `indent` (default `overlay-card-indent`) lets ARCHIVED TASKS cards
   nest under their indented header."
  ([tasks body-w] (task-overlay-lines tasks body-w overlay-card-indent))
  ([tasks body-w indent]
   (if (empty? tasks)
     (indent-rows [[["No active tasks — tasks will appear here as work progresses." t/footer-fg-muted false]]] indent)
     (let [total (count tasks)
           settled #{:done :cancelled :rejected :deferred}
           done-n (count (filter (fn [[_ t]] (contains? settled (or (:status t) :todo))) tasks))
           bar-w 14
           filled (long (Math/round (* bar-w (/ done-n (double total)))))
           bar (str (apply str (repeat filled "▰"))
                 (apply str (repeat (- bar-w filled) "▱")))
           header [[(str bar "  ") (if (= done-n total) t/status-ok t/header-active-tab-accent) false]
                   [(str done-n " of " total " done") t/footer-fg-strong true]]
           cards (->> tasks
                   (sort-by (fn [[k t]] [(task-status-rank (or (:status t) :todo) 9) (str k)]))
                   (mapcat (fn [[k t]] (task-entry-rows k t (- body-w (* 2 indent)) indent))))]
       (-> (indent-rows [header] indent)
         (conj overlay-blank-row)
         (into cards)
         vec)))))
(defn- plan-history-lines
  "PLAN HISTORY section body — the append-only task ledger's PAST plan
   generations (`:timeline` on the F2 ctx cache, from `vis/plan-timeline`).
   One dim block per dropped generation, newest first: a `Plan #N` header
   then one glyph+title row per step, frozen at the status it had when the
   whole-replace dropped it. nil when there are no past generations."
  [timeline]
  (let [past (vec (remove :current? (or timeline [])))]
    (when (seq past)
      (-> (->> (rseq past)
            (mapcat (fn [{:keys [gen steps]}]
                      (concat
                        [[[(str "Plan #" gen "  ·  " (count steps)
                             " step" (when (not= 1 (count steps)) "s"))
                           t/footer-fg-strong true]]]
                        (map (fn [{:keys [key title status]}]
                               (let [st (keyword (str (or status "todo")))]
                                 [[(str (task-status-glyph st) " ") t/footer-fg-muted false]
                                  [(str (or (not-empty (str title)) key)) t/footer-fg-muted false]]))
                          steps)
                        [overlay-blank-row])))
            vec)
        (indent-rows 5)))))

(defn- fact-entry-rows
  "Modern multi-row card for ONE fact: a status glyph (active . / superseded
   x) + bold key, the WRAPPED content indented under it, then a blank spacer
   and a dim meta row joining `> N files`, `depends ...`, and `contradicts ...`
   when present, then a blank spacer. When the fact's key is in `expanded` the
   file-bearing card also lists each file path under the meta row, and the meta
   row carries `^{:fact-key <str>}` metadata so the overlay painter can wire a
   per-fact click region over it. Nothing truncated - content wraps to `body-w`."
  [k f body-w expanded]
  (let [super? (= :superseded (:status f))
        kstr (str k)
        files (:files f)
        file-count (count files)
        expandable? (pos? file-count)
        expanded? (and expandable? (contains? expanded kstr))
        glyph-seg [(if super? "⊘ " "• ") (if super? t/footer-fg-muted t/status-ok) true]
        ;; `turn_<N>` → `Turn <N>` for display (canonical via fmt); stored key
        ;; (kstr) stays snake so recall/fold + the click region still match.
        key-row [glyph-seg [(fmt/humanize-fact-key k) (if super? t/footer-fg-muted t/header-active-tab-accent) true]]
        content (not-empty (str (:content f)))
        content-rows (when content
                       (md-wrapped-rows [["  " t/dialog-fg false]]
                         2
                         content
                         (max 6 (- body-w 2))
                         (if super? t/footer-fg-muted t/dialog-fg)
                         false))
        files-label (when expandable?
                      (str (if expanded? "▾ " "▸ ") "⛁ " file-count " files"))
        meta-parts (cond-> []
                     files-label (conj files-label)
                     (seq (:depends_on f)) (conj (str "↳ depends "
                                                   (str/join ", " (map pr-str (:depends_on f)))))
                     (seq (:contradicts f))
                     (conj (str "⚡ contradicts "
                             (str/join ", " (map pr-str (sort (:contradicts f)))))))
        meta-rows (when (seq meta-parts)
                    (wrapped-rows [["  " t/footer-fg-muted false]]
                      2
                      (str/join "  \u00b7  " meta-parts)
                      (max 6 (- body-w 2))
                      t/footer-fg-muted
                      false))
        ;; Tag the FIRST meta row with the fact key so the overlay painter
        ;; can register a click region over it (toggle the file list). Only
        ;; file-bearing cards are clickable.
        meta-rows (if (and (seq meta-rows) expandable?)
                    (into [(vary-meta (first meta-rows) assoc :fact-key kstr)]
                      (rest meta-rows))
                    meta-rows)
        ;; When expanded, list each file path under the meta row.
        ;; When expanded, list each file path under the meta row, and under
        ;; each path its recorded regions (note + anchor) so the panel shows
        ;; WHERE in the file the fact points, not just the filename.
        file-rows (when expanded?
                    (vec (mapcat
                           (fn [file]
                             (let [path-row [["    · " t/footer-fg-muted false]
                                             [(str (or (:path file) file)) t/dialog-fg false]]
                                   region-rows (vec (mapcat
                                                      (fn [r]
                                                        (let [note (not-empty (str (or (:note r) (:src r))))
                                                              anchor (not-empty (str (or (:from_hash r) (:from-hash r))))
                                                              text (str note (when (and note anchor) "  ")
                                                                     (when anchor (str "(" anchor ")")))]
                                                          (when (or note anchor)
                                                            (wrapped-rows [["        \u21b3 " t/footer-fg-muted false]]
                                                              10
                                                              text
                                                              (max 6 (- body-w 10))
                                                              t/footer-fg-muted false))))
                                                      (:regions file)))]
                               (into [path-row] region-rows)))
                           files)))]
    (-> [key-row]
      (conj overlay-blank-row)
      (into content-rows)
      (conj overlay-blank-row)
      (into meta-rows)
      (into file-rows)
      indent-rows
      (conj overlay-blank-row)
      (conj overlay-blank-row))))
(defn- fact-overlay-lines
  "FACTS section body - one `fact-entry-rows` card per fact, active facts
   first then superseded. `expanded` is the set of fact keys (as strings)
   whose file list is currently unfolded. Empty state is a single hint row."
  [facts body-w expanded]
  (if (empty? facts)
    (indent-rows [[["No recorded facts - key findings will appear here as they're discovered." t/footer-fg-muted false]]])
    (->> facts
      (sort-by (fn [[k f]] [(if (= :superseded (:status f)) 1 0) (str k)]))
      (mapcat (fn [[k f]] (fact-entry-rows k f (- body-w (* 2 overlay-card-indent)) expanded)))
      vec)))
(defn- section-line
  "A bold section header line (single segment). Uses a DARK accent\n   (`header-active-tab-accent`) — NOT `dialog-title-fg`, which is white and\n   only legible on the dark title bar, not on the light dialog body.\n   Optional `indent` left-pads the label with that many columns so a\n   subsection can nest visually under its parent (e.g. ARCHIVED TASKS\n   under TASKS)."
  ([label] (section-line label 0))
  ([label indent]
   [[(str (apply str (repeat indent \space)) label) t/header-active-tab-accent true]]))
(defn- short-revision-id
  [revision-id]
  (let [s (str revision-id)]
    (if (> (count s) 8) (subs s 0 8) s)))
(defn- dag-text-rows
  [marker value body-w color]
  (when (some? value)
    (let [text (clip-str (str value) (max 120 (* 6 body-w)))]
      (wrapped-rows [[(str marker " ") t/footer-fg-muted false]]
        2 text (max 8 (- body-w 6)) color false))))
(defn- dag-node-lines
  [{:keys [id kind status label parent depends-on acceptance evidence born
           done-born verified? contradicts files]} body-w]
  (let [status (if (keyword? status) status (keyword (str status)))
        task? (= :task kind)
        color (if task?
                (task-status-color status)
                (if (= :superseded status) t/footer-fg-muted t/status-ok))
        glyph (if task? (task-status-glyph status) (if (= :superseded status) "◇" "◆"))
        header [[[glyph color true]
                 ["  " t/footer-fg-muted false]
                 [id t/header-active-tab-accent true]
                 [(str "  ·  " (name status)) color true]]]
        details (concat
                  (dag-text-rows "│" label body-w t/dialog-fg)
                  (dag-text-rows "├ parent" parent body-w t/footer-fg)
                  (when (seq depends-on)
                    (dag-text-rows "├ needs" (str/join ", " depends-on) body-w t/footer-fg))
                  (when (seq contradicts)
                    (dag-text-rows "├ contradicts" (str/join ", " contradicts) body-w t/cancelled-fg))
                  (dag-text-rows "├ acceptance" acceptance body-w t/footer-fg)
                  (dag-text-rows "├ evidence" evidence body-w t/status-ok)
                  (when (seq files)
                    (dag-text-rows "├ files" (str/join ", " files) body-w t/footer-fg))
                  (when born
                    (dag-text-rows "├ born" born body-w t/footer-fg-muted))
                  (when done-born
                    (dag-text-rows "├ done" done-born body-w t/footer-fg-muted))
                  (when (and task? verified?)
                    (dag-text-rows "└" "verified" body-w t/status-ok)))]
    (-> (vec (concat header details [overlay-blank-row]))
      (indent-rows 3))))
(defn- dag-change-lines
  [{:keys [status path before-sha256 after-sha256]} body-w]
  (let [status (if (keyword? status) status (keyword (str status)))
        color (case status
                :add t/status-ok
                :delete t/cancelled-fg
                :modify t/warning-fg
                t/footer-fg-muted)]
    (indent-rows
      (vec (concat
             [[[(str (name status) "  ") color true]
               [(str path) t/dialog-fg true]]]
             (dag-text-rows "before" before-sha256 body-w t/footer-fg-muted)
             (dag-text-rows "after " after-sha256 body-w t/footer-fg-muted)))
      3)))
(defn- dag-overlay-lines
  "Verbose workspace-backed DAG diagnostic for the F2 context panel. The
   persistence projection bounds nodes, edges, and changes before rendering."
  [dag body-w]
  (if-not dag
    (indent-rows [[["Graph details unavailable for this session." t/footer-fg-muted false]]])
    (let [{:keys [tracked? revision-id checkpoint? undo? redo-count task-count
                  fact-count node-count edge-count root-count
                  workspace-change-count task-update-count fact-update-count
                  parent-revision-id redo-revision-ids updated-at-ms answered?
                  advance-tasks advance-facts nodes edges workspace-changes
                  truncated-node-count truncated-edge-count truncated-change-count]} dag
          state-label (if tracked? "tracked revision" "live context")
          revision-row [[(if tracked? "●  " "○  ")
                         (if tracked? t/status-ok t/footer-fg-muted) true]
                        [(str state-label " " (short-revision-id revision-id))
                         t/footer-fg-strong true]
                        [(str (when checkpoint? "  ·  checkpoint")
                           (when undo? "  ·  undo ready")
                           (when (pos? (long (or redo-count 0)))
                             (str "  ·  redo " redo-count)))
                         t/footer-fg-muted false]]
          counts-row [[(str node-count " nodes") t/header-active-tab-accent true]
                      [(str "  ·  " task-count " tasks  ·  " fact-count " facts"
                         "  ·  " edge-count " edges  ·  " root-count " roots")
                       t/footer-fg-muted false]]
          change-row [[(str "Δ " workspace-change-count " files")
                       (if (pos? (long (or workspace-change-count 0)))
                         t/status-ok
                         t/footer-fg-muted)
                       true]
                      [(str "  ·  advance " task-update-count " task"
                         (when (not= 1 task-update-count) "s")
                         ", " fact-update-count " fact"
                         (when (not= 1 fact-update-count) "s"))
                       t/footer-fg-muted false]]
          lineage-rows (vec (concat
                              [(section-line "LINEAGE" 3)]
                              (dag-text-rows "current" revision-id body-w t/dialog-fg)
                              (dag-text-rows "parent " (or parent-revision-id "none") body-w t/footer-fg)
                              (when (seq redo-revision-ids)
                                (dag-text-rows "redo   " (str/join ", " redo-revision-ids)
                                  body-w t/footer-fg))
                              (dag-text-rows "updated" (or updated-at-ms "not persisted")
                                body-w t/footer-fg-muted)))
          advance-rows (vec (concat
                              [(section-line "ADVANCE" 3)]
                              (dag-text-rows "tasks" (if (seq advance-tasks)
                                                       (str/join ", " advance-tasks)
                                                       "none")
                                body-w t/dialog-fg)
                              (dag-text-rows "facts" (if (seq advance-facts)
                                                       (str/join ", " advance-facts)
                                                       "none")
                                body-w t/dialog-fg)
                              (dag-text-rows "answer" (if answered? "included" "none")
                                body-w (if answered? t/status-ok t/footer-fg-muted))))
          node-rows (if (seq nodes)
                      (mapcat #(dag-node-lines % body-w) nodes)
                      (indent-rows [[["No graph nodes yet." t/footer-fg-muted false]]]))
          node-more-row (when (pos? (long (or truncated-node-count 0)))
                          (indent-rows [[[(str "+ " truncated-node-count " more nodes")
                                          t/footer-fg-muted false]]] 3))
          edge-rows (if (seq edges)
                      (mapcat (fn [{:keys [from relation to]}]
                                (wrapped-rows [["↳ " t/footer-fg-muted false]]
                                  2
                                  (str from
                                    (if (= :parent relation) " parent → " " needs → ")
                                    to)
                                  (max 8 (- body-w 6))
                                  t/dialog-fg
                                  false))
                        edges)
                      [[["No DAG edges yet." t/footer-fg-muted false]]])
          edge-more-row (when (pos? (long (or truncated-edge-count 0)))
                          [[[(str "+ " truncated-edge-count " more edges")
                             t/footer-fg-muted false]]])
          change-rows (if (seq workspace-changes)
                        (mapcat #(dag-change-lines % body-w) workspace-changes)
                        (indent-rows [[["No filesystem changes in this revision."
                                        t/footer-fg-muted false]]] 3))
          change-more-row (when (pos? (long (or truncated-change-count 0)))
                            (indent-rows [[[(str "+ " truncated-change-count " more changes")
                                            t/footer-fg-muted false]]] 3))]
      (vec (concat
             (indent-rows [revision-row counts-row change-row] 3)
             [overlay-blank-row]
             lineage-rows
             [overlay-blank-row]
             advance-rows
             [overlay-blank-row (section-line "NODES" 3) overlay-blank-row]
             node-rows
             node-more-row
             [(section-line "EDGES" 3) overlay-blank-row]
             (indent-rows edge-rows 3)
             (some-> edge-more-row (indent-rows 3))
             [overlay-blank-row (section-line "WORKSPACE DIFF" 3) overlay-blank-row]
             change-rows
             change-more-row)))))
(defn context-overlay!
  "Dialog showing the session's workspace-backed DAG plus working memory -
   `:session/tasks` AND `:session/facts` - the W3 user-visible panel (F2). Uses the shared
   `dialogs/draw-dialog-chrome!` + `dialog-layout` so it looks like every other
   modal (shadow, border, accent title bar, hint row). Title carries a
   tasks-done + facts count summary; a TASKS section (status-sorted, colored
   glyphs, acceptance sub-lines, verify badges) then a FACTS section (active
   first, `> N` for file-bearing facts). `expanded` is the set of fact keys (as
   strings) whose file list is unfolded; each file-bearing fact's meta row gets
   a `:toggle-fact-files` click region so a click folds/unfolds its paths. The
   dialog SIZES to its content (grows to fit, clamped to the terminal); overflow
   scrolls through the shared `scrollable-dialog-body!` (same plumbing as F1
   help). `ctx` is `{:dag ... :tasks ... :facts ...}`. Returns
   `{:scroll :max-scroll :selectable-ranges}`."
  [g cols rows {:keys [dag tasks facts archived timeline]} scroll expanded]
  (let [total (count tasks)
        done (count (filter (fn [[_ t]] (= :done (:status t))) tasks))
        title (str "Context"
                (when dag (format "  ·  dag %dn/%de" (:node-count dag) (:edge-count dag)))
                (when (pos? total) (format "  ·  tasks %d/%d done" done total))
                (when (pos? (count facts)) (format "  ·  facts %d" (count facts))))
        body-w (dialogs/default-content-width cols)
        blank [["" t/dialog-hint false]]
        arch-tasks (into {} (filter (fn [[_ v]] (= :task (:vis/kind v))) archived))
        ;; Archived FACTS render under their original `:vis/key` (the
        ;; archive map is keyed by stable `:id`) so the cards read like
        ;; their live twins did before compaction moved them out.
        arch-facts (into {} (keep (fn [[id v]]
                                    (when (= :fact (:vis/kind v))
                                      [(or (:vis/key v) id) v]))
                              archived))
        lines (vec (concat [blank (section-line "DAG REVISION" 2) blank]
                     (dag-overlay-lines dag body-w)
                     [blank (section-line "TASKS" 2) blank]
                     (task-overlay-lines tasks body-w)
                     (when (seq arch-tasks)
                       (concat [blank (section-line "ARCHIVED TASKS" 4) blank]
                         (task-overlay-lines arch-tasks body-w 5)))
                     ;; The ledger's PAST plan generations — every plan a
                     ;; whole-replace dropped, so the full task timeline is
                     ;; visible, not just the current plan.
                     (when-let [ph (plan-history-lines timeline)]
                       (concat [blank (section-line "PLAN HISTORY" 4) blank] ph))
                     [blank (section-line "FACTS" 2) blank]
                     (fact-overlay-lines facts body-w (set expanded))
                     (when (seq arch-facts)
                       (concat [blank (section-line "ARCHIVED FACTS" 4) blank]
                         (fact-overlay-lines arch-facts body-w (set expanded))))))
        n (count lines)
        cap-h (dialogs/default-content-height rows)
        req-h (min n cap-h)
        bounds (dialogs/draw-dialog-chrome! g cols rows title req-h)
        {:keys [left inner-w]} bounds
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds req-h)
        sb? (> n content-h)
        body-right (+ left 1 inner-w)
        text-right (if sb? (dec body-right) body-right)
        paint-line (fn [i segs]
                     (let [r (+ content-top i)]
                       ;; A meta row tagged with :fact-key gets a click region so
                       ;; clicking the `> N files` glyph toggles its path list.
                       (when-let [fk (:fact-key (meta segs))]
                         (cr/register! {:bounds {:row r, :col (+ left 1),
                                                 :width (max 0 (- text-right (+ left 1)))}
                                        :kind :toggle-fact-files, :fact-key fk, :enabled? true}))
                       (loop [x (+ left 1) ss segs]
                         (when-let [[text color bold?] (first ss)]
                           (let [avail (max 0 (- text-right x))
                                 shown (clip-str (str text) avail)]
                             (p/clear-styles! g)
                             (p/set-colors! g color t/dialog-bg)
                             (when bold? (p/enable! g p/BOLD))
                             (p/put-str! g x r shown)
                             (recur (+ x (p/display-width shown)) (next ss)))))))
        geom (scrollable-dialog-body! g lines
               {:content-top content-top, :content-h content-h, :hint-row hint-row,
                :sb-col (dec body-right), :body-right body-right}
               scroll paint-line)
        shown-n (:shown-n geom)]
    (dialog-close-button! g bounds :toggle-tasks)
    (p/clear-styles! g)
    {:scroll (:scroll geom), :max-scroll (:max-scroll geom),
     :selectable-ranges (vec (for [i (range shown-n)]
                               {:row (+ content-top i), :col (+ left 1),
                                :width (max 0 (- text-right (+ left 1)))}))}))
