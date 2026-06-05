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
  (:require [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.dialogs :as dialogs]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.header :as vh]))
;; ── text layout ─────────────────────────────────────────────────────────────
(defn truncate-with-ellipsis
  "Truncate `s` so its display width fits in `max-cols`. When truncation
   actually happens, append `vh/workspace-ellipsis` so overflow is visible."
  ^String [s ^long max-cols]
  (let [s (or s "")]
    (cond (<= max-cols 0) ""
          (<= (p/display-width s) max-cols) s
          (= max-cols 1) (p/truncate-cols vh/workspace-ellipsis 1)
          :else (str (p/truncate-cols s (dec max-cols)) vh/workspace-ellipsis))))
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
  (p/set-colors! g t/header-active-tab-accent t/terminal-bg)
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
   {:keys [left row width label active? workspace-id index register? closable?],
    :or {closable? true}}]
  (let [width (long width)
        ;; Reserve room for the close button only when the cell can still
        ;; show a sliver of title beside it; otherwise the title wins.
        show-close? (and closable? (>= width (+ close-button-width 3)))
        inner-w (if show-close? (max 0 (- width close-button-width)) width)
        fg (if active? t/header-active-tab-fg t/border-fg)
        bg (if active? t/header-active-tab-bg t/dialog-bg)
        text (center-padded label inner-w)]
    (p/clear-styles! g)
    (p/set-colors! g fg bg)
    (p/enable! g (if active? p/BOLD p/ITALIC))
    (p/fill-rect! g left row width 1)
    (p/put-str! g left row text)
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
  [["Ctrl+H · F1" "Toggle this help"] ["F2" "Toggle the task panel"]
   ["Enter · Ctrl+X" "Send message"] ["Alt+Enter" "Insert a newline"]
   ["Esc" "Clear draft · cancel turn"] ["Ctrl+C" "Cancel turn · quit"]
   ["Tab · Shift+Tab" "Switch tab"] ["Ctrl+W" "Close tab  (or click the ✕)"]
   ["Ctrl+K" "Command palette"] ["Ctrl+G" "Sessions · workspaces"] ["Ctrl+T" "Cycle model"]
   ["Ctrl+R" "Cycle reasoning effort"] ["Ctrl+L" "Cycle Codex verbosity"]
   ["Ctrl+A · Ctrl+E" "Jump to line start · end"] ["Ctrl+U" "Delete to line start"]
   ["Alt+B · Alt+F" "Move word left · right"] ["Ctrl+V" "Paste"] ["@" "Pick a file"]
   ["Mouse" "Click a tab to switch · ✕ to close"]])
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
      (p/set-colors! g (if hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
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
    (p/set-colors! g (if hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col row glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds {:row row, :col col, :width w},
                     :kind kind,
                     :enabled? true}))
    w))
;; ── help overlay ────────────────────────────────────────────────────────────
(def ^:private help-title "Keyboard shortcuts  —  Ctrl+H / F1 to close")
(defn- pad-right
  ^String [^String s ^long w]
  (str s (apply str (repeat (max 0 (- w (p/display-width s))) \space))))
(defn help-overlay!
  "Draw a centered modal help card listing every keyboard shortcut, painting
   its own background over whatever is underneath. Pure chrome — registers no
   click regions; the caller dismisses it (Ctrl+H / F1 / any key). No-op when
   the terminal is too small to host the card."
  [g cols rows]
  (let [cols (long cols)
        rows (long rows)
        key-w (reduce max 0 (map (comp p/display-width first) help-shortcuts))
        desc-w (reduce max 0 (map (comp p/display-width second) help-shortcuts))
        inner-w (max (p/display-width help-title) (+ key-w 2 desc-w))
        box-w (+ inner-w 4)                ; 1 border + 1 pad per side
        box-h (+ (count help-shortcuts) 4) ; borders + title + blank row
        left (max 0 (quot (- cols box-w) 2))
        top (max 0 (quot (- rows box-h) 2))
        right (+ left box-w -1)
        bottom (+ top box-h -1)]
    (when (and (>= cols box-w) (>= rows box-h))
      ;; Solid background slab.
      (p/clear-styles! g)
      (p/set-colors! g t/border-fg t/dialog-bg)
      (doseq [r (range top (inc bottom))] (p/fill-rect! g left r box-w 1))
      ;; Border box.
      (p/put-str! g left top (str "┌" (apply str (repeat (- box-w 2) "─")) "┐"))
      (p/put-str! g left bottom (str "└" (apply str (repeat (- box-w 2) "─")) "┘"))
      (doseq [r (range (inc top) bottom)]
        (p/put-str! g left r "│")
        (p/put-str! g right r "│"))
      ;; Title (bold).
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/dialog-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g (+ left 2) (inc top) (pad-right help-title inner-w))
      (p/clear-styles! g)
      ;; Shortcut rows: bold key, dim description.
      (doseq [[i [k d]] (map-indexed vector help-shortcuts)]
        (let [r (+ top 3 i)]
          (p/clear-styles! g)
          (p/set-colors! g t/footer-fg-strong t/dialog-bg)
          (p/enable! g p/BOLD)
          (p/put-str! g (+ left 2) r (pad-right k key-w))
          (p/clear-styles! g)
          (p/set-colors! g t/footer-fg t/dialog-bg)
          (p/put-str! g (+ left 2 key-w 2) r d)))
      (p/clear-styles! g))))
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
(defn- task-overlay-lines
  "Build the overlay body as a vec of LINES, each a vec of `[text color bold?]`
   segments. A task is a primary line (colored status glyph + title + [status]
   + verify badge) plus, when it has `:acceptance`, a dim indented sub-line.
   `body-w` is the inner content width segments are clipped to fit."
  [tasks body-w]
  (if (empty? tasks)
    [[["No tasks yet — the model opens one with (task-set! …)." t/footer-fg-muted false]]]
    (->> tasks
         (sort-by (fn [[k t]] [(task-status-rank (or (:status t) :todo) 9) (str k)]))
         (mapcat
           (fn [[k t]]
             (let [status (or (:status t) :todo)
                   badge (cond (:verified? t) ["  ✓ verified" t/status-ok false]
                               (:acceptance t) ["  ⌛ unverified" t/warning-fg false]
                               :else nil)
                   glyph [(str (task-status-glyph status) " ") (task-status-color status) true]
                   ;; title gets whatever width remains after glyph(2)+badge.
                   badge-w (if badge (p/display-width (first badge)) 0)
                   title-w (max 6 (- body-w 2 badge-w))
                   title [(clip-str (or (not-empty (str (:title t))) (name k)) title-w) t/dialog-fg
                          true]
                   primary (cond-> [glyph title] badge (conj badge))
                   ;; accept is a LINE (a vec of one seg). conj it as a single
                   ;; element — `into` would splice the bare seg in as a line,
                   ;; so line-w's (display-width (first seg)) hit a Character and
                   ;; threw every frame, freezing the TUI.
                   accept (when-let [a (not-empty (str (:acceptance t)))]
                            [(str "    ▸ " (clip-str a (max 6 (- body-w 6)))) t/footer-fg-muted
                             false])]
               (cond-> [primary] accept (conj [accept]))))))))
(defn- fact-overlay-lines
  "Lines for the FACTS section: a status glyph (active • / superseded ⊘) +
   `key: content` clipped to width, plus a `⛁N` badge when the fact carries
   `:files` regions. Active facts first; capped, with a `+N more` tail."
  [facts body-w]
  (if (empty? facts)
    [[["No facts yet — the model records one with (fact-set! …)." t/footer-fg-muted false]]]
    (let [shown (->> facts
                     (sort-by (fn [[k f]] [(if (= :superseded (:status f)) 1 0) (str k)]))
                     (take 14))
          more (max 0 (- (count facts) (count shown)))
          lines
            (mapv (fn [[k f]]
                    (let [super? (= :superseded (:status f))
                          fcount (count (:files f))
                          badge (when (pos? fcount) [(str "  ⛁" fcount) t/footer-fg-muted false])
                          badge-w (if badge (p/display-width (first badge)) 0)
                          glyph [(if super? "⊘ " "• ") (if super? t/footer-fg-muted t/status-ok)
                                 true]
                          txt (clip-str (str (name k) ": " (or (not-empty (str (:content f))) ""))
                                        (max 6 (- body-w 2 badge-w)))]
                      (cond-> [glyph [txt (if super? t/footer-fg-muted t/dialog-fg) false]]
                        badge (conj badge))))
              shown)]
      (cond-> lines (pos? more) (conj [[(str "    … +" more " more") t/footer-fg-muted false]])))))
(defn- section-line
  "A bold section header line (single segment). Uses a DARK accent
   (`header-active-tab-accent`) — NOT `dialog-title-fg`, which is white and
   only legible on the dark title bar, not on the light dialog body."
  [label]
  [[label t/header-active-tab-accent true]])
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
  [g cols rows {:keys [tasks facts]}]
  (let [total   (count tasks)
        done    (count (filter (fn [[_ t]] (= :done (:status t))) tasks))
        title   (str "Context"
                  (when (pos? total)         (format "  ·  tasks %d/%d done" done total))
                  (when (pos? (count facts)) (format "  ·  facts %d" (count facts))))
        hint    "F2 to close"
        ;; Build lines at a generous width, then size the dialog to the actual
        ;; content (golden-dialog-size clamps width+height to the terminal).
        body-w  (dialogs/default-content-width cols)
        blank   [["" t/dialog-hint false]]
        lines   (vec (concat [(section-line "TASKS")] (task-overlay-lines tasks body-w)
                       [blank]
                       [(section-line "FACTS")] (fact-overlay-lines facts body-w)))
        line-w  (fn [segs] (reduce + 0 (map (comp p/display-width first) segs)))
        content-w (reduce max (p/display-width title) (map line-w lines))
        bounds  (dialogs/draw-dialog-chrome! g cols rows title content-w (count lines))
        {:keys [left inner-w]} bounds
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds (count lines))
        n       (count lines)
        overflow? (> n content-h)
        shown-n (if overflow? (max 0 (dec content-h)) n)
        paint-line
        (fn [i segs]
          (let [r (+ content-top i)]
            (loop [x (+ left 1), ss segs]
              (when-let [[text color bold?] (first ss)]
                (let [avail (max 0 (- (+ left 1 inner-w) x))
                      shown (clip-str (str text) avail)]
                  (p/clear-styles! g)
                  (p/set-colors! g color t/dialog-bg)
                  (when bold? (p/enable! g p/BOLD))
                  (p/put-str! g x r shown)
                  (recur (+ x (p/display-width shown)) (next ss)))))))]
    (dotimes [i shown-n] (paint-line i (nth lines i)))
    (when overflow?
      (paint-line shown-n [[(str "… " (- n shown-n) " more — full working memory lives in ctx")
                            t/dialog-hint false]]))
    ;; Centered dim close hint on the dialog's hint row.
    (when hint-row
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-hint t/dialog-bg)
      (p/put-str! g (+ left 1 (max 0 (quot (- inner-w (p/display-width hint)) 2)))
        hint-row hint))
    (p/clear-styles! g)))
