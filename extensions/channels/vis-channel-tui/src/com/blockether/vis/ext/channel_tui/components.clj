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
            [com.blockether.vis.internal.header :as vh]))
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
       :else                  (p/set-colors! g t/dialog-title-bg t/dialog-title-fg))
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
  [g cols text-top {:keys [active? query hits index]}]
  (when active?
    (let [n     (long (count hits))
          q     (str query)
          qshow (if (str/blank? q) "type to search…" q)
          qpad  (let [s (truncate-with-ellipsis qshow find-input-width)]
                  (str s (apply str (repeat (max 0 (- find-input-width (long (p/display-width s)))) \space))))
          cnt   (format " %-5s" (cond (str/blank? q) ""
                                      (zero? n)      "0/0"
                                      :else          (str (inc (long (or index 0))) "/" n)))
          ;; content ops between the borders. :input is the white field; :chrome /
          ;; :gap ride the box bg; :btn delegates to the reusable button! widget.
          ops   (concat
                  [[:chrome " "] [:input qpad] [:chrome "  "] [:chrome cnt] [:chrome " "]]
                  (interpose [:gap " "] (map (fn [[k l]] [:btn k l]) find-bar-buttons))
                  [[:chrome " "]])
          content-w (long (reduce + (map (fn [op] (long (p/display-width (last op)))) ops)))
          box-w (+ content-w 2)
          box-l (max 0 (- (long cols) box-w 1))
          box-t (long text-top)
          row   (inc box-t)]
      ;; box: fill bg, then single-line border (3 rows tall)
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/fill-rect! g box-l box-t box-w 3)
      (p/draw-box! g box-l box-t box-w 3)
      ;; content row (the middle one, between the borders)
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
        (inc box-l)
        ops))))

(def ^:private header-fkeys
  "Always-on clickable header chips: ONLY the two panel toggles (F1 help, F2
   context). Every other shortcut lives in the F1 help overlay, so the header
   stays uncluttered. `[click-kind label]`."
  [[:header-help " F1 "] [:header-tasks " F2 "]])

(def header-fkeys-width
  "Cells the header F-key chips occupy: two 4-col chips, each followed by a
   1-col gap. The header reserves this much before the notification slot."
  10)

(defn header-fkeys!
  "Paint the F1/F2 clickable chips at (x,row) — via the shared `button!` so they
   match every other button — registering their `:header-help` / `:header-tasks`
   click regions. Returns the x after the chips (+ trailing gap)."
  [g x row]
  (reduce (fn [x [kind label]] (+ 1 x (button! g x row label kind)))
    x
    header-fkeys))

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
  "Paint a workspace-overflow navigation arrow `glyph` at (col,row) and
   register its `:workspace-entry` click region keyed by `direction`
   (`:prev` / `:next`), so a click cycles the visible tab window. Brightens on
   hover."
  [g row col glyph direction register?]
  (let [hovered (cr/hovered)
        hovered? (and (= :workspace-entry (:kind hovered))
                      (= direction (:index hovered))
                      (= row (get-in hovered [:bounds :row])))]
    (p/clear-styles! g)
    (p/set-colors! g (if hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col row glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col col, :width (p/display-width glyph)},
                     :kind :workspace-entry,
                     :index direction,
                     :workspace-id direction,
                     :text direction,
                     :enabled? true}))))
;; ── help overlay ────────────────────────────────────────────────────────────
(def ^:private help-shortcuts
  "[[keys description] …] rows shown in the Ctrl+H help card."
  [["F1" "Toggle this help"] ["F2" "Toggle the context panel"] ["F3" "Find in session"]
   ["F4" "Managed resources"] ["F5 · Ctrl+K" "Command palette"]
   ["F6 · Ctrl+G" "Sessions · workspaces"]
   ["Find: F3" "Type to filter · Ctrl+N/P next/prev · Esc close"]
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
  "Paint the session-id copy affordance `text` at (col,row) and register its
   `:copy-id` click region carrying the FULL uuid (the click handler drops it
   on the clipboard). Brightens on hover. No-op for blank text."
  [g col row text full-uuid register?]
  (when (pos? (p/display-width text))
    (let [hovered (cr/hovered)
          hovered? (and (= row (get-in hovered [:bounds :row])) (= :copy-id (:kind hovered)))]
      (p/clear-styles! g)
      (p/set-colors! g (if hovered? t/header-hover-fg t/text-fg) t/terminal-bg)
      (when hovered? (p/enable! g p/BOLD))
      (p/put-str! g col row text)
      (p/clear-styles! g)
      (when (and register? full-uuid)
        (cr/register! {:bounds {:row row, :col col, :width (p/display-width text)},
                       :kind :copy-id,
                       :text full-uuid,
                       :enabled? true})))))
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
    nil))
(defn help-overlay!
  "Draw the keyboard-shortcut help as a dialog, using the shared\n   `dialogs/draw-dialog-chrome!` + `dialog-layout` so it matches the F2\n   context panel (shadow, border, accent title bar, centered hint row).\n   The grid spans the full dialog inner width (desc column stretches to fill).\n   Pure chrome — registers no click regions; the caller dismisses it\n   (Ctrl+H / F1 / any key)."
  [g cols rows]
  (let [title "Keyboard shortcuts"
        key-w (reduce max 0 (map (comp p/display-width first) help-shortcuts))
        base-desc-w (reduce max 0 (map (comp p/display-width second) help-shortcuts))
        bd t/dialog-border
        line-cnt (inc (* 2 (count help-shortcuts)))
        prov-w (max (p/display-width title) (+ key-w base-desc-w 7))
        bounds (dialogs/draw-dialog-chrome! g cols rows title prov-w line-cnt)
        {:keys [left inner-w]} bounds
        desc-w (max base-desc-w (- inner-w key-w 8))
        bar
          (fn [l m r]
            (str l (apply str (repeat (+ key-w 2) "─")) m (apply str (repeat (+ desc-w 2) "─")) r))
        row-segs (fn [[k d]] [["│ " bd false] [(pad-right (str k) key-w) t/footer-fg-strong true]
                              [" │ " bd false] [(pad-right (str d) desc-w) t/footer-fg false]
                              [" │" bd false]])
        rule (fn [l m r] [[(bar l m r) bd false]])
        lines (vec (concat [(rule "┌" "┬" "┐")]
                           (interpose (rule "├" "┼" "┤") (mapv row-segs help-shortcuts))
                           [(rule "└" "┴" "┘")]))
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds (count lines))
        n (count lines)
        shown-n (min n content-h)
        paint-line (fn [i segs]
                     (let [r (+ content-top i)]
                       (loop [x (+ left 2)
                              ss segs]
                         (when-let [[text color bold?] (first ss)]
                           (let [avail (max 0 (- (+ left 1 inner-w) x))
                                 shown (dialogs/ellipsize (str text) avail)]
                             (p/clear-styles! g)
                             (p/set-colors! g color t/dialog-bg)
                             (when bold? (p/enable! g p/BOLD))
                             (p/put-str! g x r shown)
                             (recur (+ x (p/display-width shown)) (next ss)))))))]
    (dotimes [i shown-n] (paint-line i (nth lines i)))
    (dialog-close-button! g bounds :toggle-help)
    (p/clear-styles! g)))
;; ── tasks overlay (W3: user-visible :session/tasks) ──────────────────────────
(defn- task-status-glyph
  [status]
  (case status
    :done "✓"
    :doing "◐"
    :cancelled "✗"
    "○"))
(defn- task-status-color
  [status]
  (case status
    :done t/status-ok
    :doing t/warning-fg
    :cancelled t/cancelled-fg
    t/footer-fg-muted))
(def ^:private task-status-rank {:doing 0, :todo 1, :done 2, :cancelled 3})
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
  2)
(defn- indent-rows
  "Prefix every row with `indent` (default `overlay-card-indent`) leading\n   spaces so the whole card insets from the dialog left rail. Archived\n   tasks pass a larger indent so their cards nest under the indented\n   ARCHIVED TASKS header."
  ([rows] (indent-rows rows overlay-card-indent))
  ([rows indent]
   (let [pad [(apply str (repeat indent \space)) t/dialog-hint false]]
     (mapv (fn [row] (into [pad] row)) rows))))
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
  "Full-justify a row of `[text color bold?]` segments to `w` display columns by\n   widening every inter-word whitespace run across the segments. Returns the\n   segments untouched when there are no gaps or no slack (single word / already\n   full). The styled twin of `justify-line` for `md-wrapped-rows`."
  [segs ^{:tag long} w]
  (let [texts (map first segs)
        gap-count (reduce + (map (fn [t] (count (re-seq #"\s+" t))) texts))
        text-w (reduce + (map p/display-width texts))
        slack (- w text-w)]
    (if (or (< gap-count 1) (<= slack 0))
      segs
      (let [base (quot slack gap-count)
            extra (rem slack gap-count)
            idx (atom -1)]
        (mapv (fn [[t color bold?]] [(str/replace t
                                                  #"\s+"
                                                  (fn [m]
                                                    (let [i (swap! idx inc)
                                                          add (+ base (if (< i extra) 1 0))]
                                                      (str m (apply str (repeat add \space))))))
                                     color bold?])
          segs)))))
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
               (let [segs (mapv (fn* [p1__53155#] (run->seg p1__53155# base-color base-bold?)) runs)
                     segs (if (seq segs) segs [["" base-color base-bold?]])
                     ;; Only lines the wrapper broke on overflow (`:wrap?`)
                     ;; get full-justified. Paragraph/block-terminal lines
                     ;; are short and ragged-right by nature — stretching
                     ;; them edge-to-edge is the "4 words, mega holes" bug.
                     segs (if wrap? (justify-segs segs (long w)) segs)]
                 (if (zero? i) (into (vec head) segs) (into [[pad base-color base-bold?]] segs))))
             lines)))))
(defn- task-entry-rows
  "Modern multi-row card for ONE task: a colored status glyph + WRAPPED\n   title, a dim meta row (status label + verify badge), an optional\n   wrapped `:acceptance` sub-line, an optional `↳ needs …` dependency\n   line, then a blank spacer. Sub-rows inset only 2 cols (the glyph\n   width) so they line up UNDER the title text instead of drifting\n   further right. Everything wraps to `body-w`. `indent` left-insets the\n   whole card. Rows are `[text color bold?]` segment vecs."
  [k t body-w indent]
  (let [status (or (:status t) :todo)
        glyph-seg [(str (task-status-glyph status) " ") (task-status-color status) true]
        title (or (not-empty (str (:title t))) (name k))
        title-rows (md-wrapped-rows [glyph-seg] 2 title (max 6 (- body-w 2)) t/dialog-fg true)
        verify (cond (:verified? t) ["✓ verified" t/status-ok]
                     (:acceptance t) ["⌛ unverified" t/warning-fg]
                     :else nil)
        meta-segs (cond-> [[(str "  " (name status)) (task-status-color status) false]]
                    verify (conj [(str "   " (first verify)) (second verify) false]))
        accept-rows (when-let [a (not-empty (str (:acceptance t)))]
                      (md-wrapped-rows [["  ▸ " t/footer-fg-muted false]]
                                       4
                                       a
                                       (max 6 (- body-w 4))
                                       t/footer-fg-muted
                                       false))
        dep-rows (when (seq (:depends_on t))
                   (wrapped-rows [["  ↳ needs " t/footer-fg-muted false]]
                                 4
                                 (str/join ", " (map pr-str (:depends_on t)))
                                 (max 6 (- body-w 4))
                                 t/footer-fg-muted
                                 false))]
    (-> (vec title-rows)
        (conj meta-segs)
        (into accept-rows)
        (into dep-rows)
        (indent-rows indent)
        (conj overlay-blank-row)
        (conj overlay-blank-row))))
(defn- task-overlay-lines
  "TASKS section body — one `task-entry-rows` card per task, status-sorted\n   (doing → todo → done → cancelled). Empty state is a single hint row.\n   `indent` (default `overlay-card-indent`) lets ARCHIVED TASKS cards nest\n   under their indented header."
  ([tasks body-w] (task-overlay-lines tasks body-w overlay-card-indent))
  ([tasks body-w indent]
   (if (empty? tasks)
     (indent-rows [[["No tasks yet — the model opens one with (task-set! …)." t/footer-fg-muted false]]] indent)
     (->> tasks
          (sort-by (fn [[k t]] [(task-status-rank (or (:status t) :todo) 9) (str k)]))
          (mapcat (fn [[k t]] (task-entry-rows k t (- body-w (* 2 indent)) indent)))
          vec))))
(defn- fact-entry-rows
  "Modern multi-row card for ONE fact: a status glyph (active • / superseded
   ⊘) + bold key, the WRAPPED content indented under it, then a dim meta row
   joining `⛁N files`, `↳ depends …`, and `⚡ contradicts …` when present,
   then a blank spacer. Nothing truncated — content wraps to `body-w`."
  [k f body-w]
  (let [super? (= :superseded (:status f))
        glyph-seg [(if super? "⊘ " "• ") (if super? t/footer-fg-muted t/status-ok) true]
        key-row [glyph-seg [(name k) (if super? t/footer-fg-muted t/header-active-tab-accent) true]]
        content (not-empty (str (:content f)))
        content-rows (when content
                       (md-wrapped-rows [["    " t/dialog-fg false]]
                                        4
                                        content
                                        (max 6 (- body-w 4))
                                        (if super? t/footer-fg-muted t/dialog-fg)
                                        false))
        meta-parts (cond-> []
                     (pos? (count (:files f))) (conj (str "⛁" (count (:files f)) " files"))
                     (seq (:depends_on f)) (conj (str "↳ depends "
                                                      (str/join ", " (map pr-str (:depends_on f)))))
                     (seq (:contradicts f))
                       (conj (str "⚡ contradicts "
                                  (str/join ", " (map pr-str (sort (:contradicts f)))))))
        meta-rows (when (seq meta-parts)
                    (wrapped-rows [["    " t/footer-fg-muted false]]
                                  4
                                  (str/join "  ·  " meta-parts)
                                  (max 6 (- body-w 4))
                                  t/footer-fg-muted
                                  false))]
    (-> [key-row]
        (into content-rows)
        (into meta-rows)
        indent-rows
        (conj overlay-blank-row)
        (conj overlay-blank-row))))
(defn- fact-overlay-lines
  "FACTS section body — one `fact-entry-rows` card per fact, active facts
   first then superseded. Empty state is a single hint row."
  [facts body-w]
  (if (empty? facts)
    (indent-rows [[["No facts yet — the model records one with (fact-set! …)." t/footer-fg-muted false]]])
    (->> facts
         (sort-by (fn [[k f]] [(if (= :superseded (:status f)) 1 0) (str k)]))
         (mapcat (fn [[k f]] (fact-entry-rows k f (- body-w (* 2 overlay-card-indent)))))
         vec)))
(defn- section-line
  "A bold section header line (single segment). Uses a DARK accent\n   (`header-active-tab-accent`) — NOT `dialog-title-fg`, which is white and\n   only legible on the dark title bar, not on the light dialog body.\n   Optional `indent` left-pads the label with that many columns so a\n   subsection can nest visually under its parent (e.g. ARCHIVED TASKS\n   under TASKS)."
  ([label] (section-line label 0))
  ([label indent]
   [[(str (apply str (repeat indent \space)) label) t/header-active-tab-accent true]]))
(defn context-overlay!
  "Dialog showing the session's working memory — `:session/tasks` AND
   `:session/facts` — the W3 user-visible panel (F2). Uses the shared
   `dialogs/draw-dialog-chrome!` + `dialog-layout` so it looks like every other
   modal (shadow, border, accent title bar, hint row). Title carries a
   tasks-done + facts count summary; a TASKS section (status-sorted, colored
   glyphs, acceptance sub-lines, verify badges) then a FACTS section (active
   first, `⛁N` for file-bearing facts). The dialog SIZES to its content (grows
   to fit, clamped to the terminal); if it still overflows, the last row shows
   a `… N more` footer so nothing is silently dropped. Registers no click
   regions; the caller dismisses on F2. `ctx` is `{:tasks … :facts …}`."
  [g cols rows {:keys [tasks facts archived]} scroll]
  (let [total (count tasks)
        done (count (filter (fn [[_ t]] (= :done (:status t))) tasks))
        title (str "Context"
                   (when (pos? total) (format "  ·  tasks %d/%d done" done total))
                   (when (pos? (count facts)) (format "  ·  facts %d" (count facts))))
        ;; Build lines at a generous width, then size the dialog to the actual
        ;; content (golden-dialog-size clamps width+height to the terminal).
        body-w (dialogs/default-content-width cols)
        blank [["" t/dialog-hint false]]
        arch-tasks (into {} (filter (fn [[_ v]] (= :task (:vis/kind v))) archived))
        lines (vec (concat [(section-line "TASKS" 2) blank]
                           (task-overlay-lines tasks body-w)
                           (when (seq arch-tasks)
                             (concat [blank (section-line "ARCHIVED TASKS" 4) blank]
                                     (task-overlay-lines arch-tasks body-w 4)))
                           [blank (section-line "FACTS" 2) blank]
                           (fact-overlay-lines facts body-w)))
        line-w (fn [segs] (reduce + 0 (map (comp p/display-width first) segs)))
        content-w (reduce max (p/display-width title) (map line-w lines))
        n (count lines)
        ;; Cap the dialog to the shared modal footprint so the panel takes a
        ;; consistent, smaller slice of the screen instead of growing to fill
        ;; the terminal; the rest of the content is reachable by scrolling.
        cap-h (dialogs/default-content-height rows)
        req-h (min n cap-h)
        bounds (dialogs/draw-dialog-chrome! g cols rows title content-w req-h)
        {:keys [left inner-w]} bounds
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds req-h)
        visible content-h
        max-scroll (max 0 (- n visible))
        eff (max 0 (min (long (or scroll 0)) max-scroll))
        sb? (> n visible)
        body-right (+ left 1 inner-w)
        ;; Reserve the rightmost inner column for the scrollbar when overflowing.
        text-right (if sb? (dec body-right) body-right)
        shown-n (min visible (- n eff))
        paint-line (fn [i segs]
                     (let [r (+ content-top i)]
                       (loop [x (+ left 1)
                              ss segs]
                         (when-let [[text color bold?] (first ss)]
                           (let [avail (max 0 (- text-right x))
                                 shown (clip-str (str text) avail)]
                             (p/clear-styles! g)
                             (p/set-colors! g color t/dialog-bg)
                             (when bold? (p/enable! g p/BOLD))
                             (p/put-str! g x r shown)
                             (recur (+ x (p/display-width shown)) (next ss)))))))]
    (dotimes [i shown-n] (paint-line i (nth lines (+ eff i))))
    (when sb?
      (scrollbar/draw! g
                       {:col (dec body-right),
                        :top content-top,
                        :track-h visible,
                        :total-h n,
                        :inner-h visible,
                        :scroll eff}))
    ;; Top-right ✕ close button + scroll-position indicator on the hint row.
    (dialog-close-button! g bounds :toggle-tasks)
    (when (and hint-row sb?)
      (let [pos (str (inc eff) "–" (+ eff shown-n) " / " n)
            pw (p/display-width pos)]
        (p/clear-styles! g)
        (p/set-colors! g t/dialog-hint t/dialog-bg)
        (p/put-str! g (- body-right pw) hint-row pos)))
    (p/clear-styles! g)
    {:scroll eff,
     :max-scroll max-scroll,
     :selectable-ranges (vec (for [i (range shown-n)]
                               {:row (+ content-top i),
                                :col (+ left 1),
                                :width (max 0 (- text-right (+ left 1)))}))}))
