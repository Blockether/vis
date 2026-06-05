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
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
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
      (cr/register! {:bounds {:row row, :col col, :width w}, :kind kind, :enabled? true}))
    w))
;; ── help overlay ────────────────────────────────────────────────────────────

(defn- pad-right
  ^String [^String s ^long w]
  (str s (apply str (repeat (max 0 (- w (p/display-width s))) \space))))
(defn help-overlay!
  "Draw the keyboard-shortcut help as a dialog, using the shared\n   `dialogs/draw-dialog-chrome!` + `dialog-layout` so it matches the F2\n   context panel (shadow, border, accent title bar, centered hint row).\n   Pure chrome — registers no click regions; the caller dismisses it\n   (Ctrl+H / F1 / any key)."
  [g cols rows]
  (let [title "Keyboard shortcuts"
        hint "Ctrl+H / F1 to close"
        key-w (reduce max 0 (map (comp p/display-width first) help-shortcuts))
        lines (mapv (fn [[k d]] [[(pad-right (str k) key-w) t/footer-fg-strong true]
                                 ["  " t/dialog-fg false] [(str d) t/footer-fg false]])
                help-shortcuts)
        line-w (fn [segs] (reduce + 0 (map (comp p/display-width first) segs)))
        content-w (reduce max (p/display-width title) (map line-w lines))
        bounds (dialogs/draw-dialog-chrome! g cols rows title content-w (count lines))
        {:keys [left inner-w]} bounds
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds (count lines))
        n (count lines)
        shown-n (min n content-h)
        paint-line (fn [i segs]
                     (let [r (+ content-top i)]
                       (loop [x (+ left 1)
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
    (when hint-row
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-hint t/dialog-bg)
      (p/put-str! g (+ left 1 (max 0 (quot (- inner-w (p/display-width hint)) 2))) hint-row hint))
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
(defn- hard-split-cols
  "Split `s` into chunks each ≤ `w` display columns. Used by `wrap-cols`
   when a SINGLE token is wider than the wrap width (a long path / URL /
   unbroken CJK run). Guaranteed to terminate: every step consumes ≥1
   char, measured by display width via `p/truncate-cols`."
  [s ^long w]
  (loop [s (str s) out []]
    (if (<= (p/display-width s) w)
      (conj out s)
      (let [prefix (str/trimr (p/truncate-cols s w))
            n (max 1 (min (count prefix) (count s)))]
        (recur (subs s n) (conj out (subs s 0 n)))))))
(defn- wrap-cols
  "Greedy word-wrap `s` into a vec of strings, each fitting `w` DISPLAY
   columns (grapheme/wide-char aware via `p/display-width`). Breaks on
   whitespace; a token longer than `w` is hard-split so nothing overflows
   the dialog. Blank input yields `[\"\"]` so callers always get ≥1 row."
  [s ^long w]
  (let [s (str/trim (str s))]
    (if (or (str/blank? s) (<= w 0))
      [""]
      (loop [words (str/split s #"\s+"), cur "", out []]
        (if-let [word (first words)]
          (if (<= (p/display-width word) w)
            (let [candidate (if (empty? cur) word (str cur " " word))]
              (if (<= (p/display-width candidate) w)
                (recur (rest words) candidate out)
                (recur words "" (conj out cur))))
            ;; token wider than the line — flush current, hard-split it
            (let [out*   (cond-> out (seq cur) (conj cur))
                  pieces (hard-split-cols word w)]
              (recur (rest words) (str (last pieces)) (into out* (butlast pieces)))))
          (cond-> out (seq cur) (conj cur)))))))
(defn- wrapped-rows
  "Rows for `text` wrapped to `w` columns: the FIRST row is prefixed by the
   `head` segments (e.g. a colored glyph) and every continuation row is
   indented by `indent` spaces so it aligns under the head. Each row is a
   vec of `[text color bold?]` segments. `indent` MUST equal the head's
   display width for clean alignment."
  [head indent text w body-color bold?]
  (let [pieces (wrap-cols text (long w))
        pad    (apply str (repeat (long indent) \space))]
    (vec
      (map-indexed
        (fn [i piece]
          (if (zero? i)
            (conj (vec head) [piece body-color bold?])
            [[(str pad piece) body-color bold?]]))
        pieces))))
(defn- task-entry-rows
  "Modern multi-row card for ONE task: a colored status glyph + WRAPPED
   title, a dim meta row (status label + verify badge), an optional
   wrapped `:acceptance` sub-line, an optional `↳ needs …` dependency
   line, then a blank spacer. Everything wraps to `body-w` — nothing is
   truncated. Rows are `[text color bold?]` segment vecs."
  [k t body-w]
  (let [status     (or (:status t) :todo)
        glyph-seg  [(str (task-status-glyph status) " ") (task-status-color status) true]
        title      (or (not-empty (str (:title t))) (name k))
        title-rows (wrapped-rows [glyph-seg] 2 title (max 6 (- body-w 2)) t/dialog-fg true)
        verify     (cond (:verified? t)  ["✓ verified"   t/status-ok]
                         (:acceptance t) ["⌛ unverified" t/warning-fg]
                         :else nil)
        meta-segs  (cond-> [[(str "    " (name status)) (task-status-color status) false]]
                     verify (conj [(str "   " (first verify)) (second verify) false]))
        accept-rows (when-let [a (not-empty (str (:acceptance t)))]
                      (wrapped-rows [["    ▸ " t/footer-fg-muted false]] 6 a
                        (max 6 (- body-w 6)) t/footer-fg-muted false))
        dep-rows   (when (seq (:depends-on t))
                     (wrapped-rows [["    ↳ needs " t/footer-fg-muted false]] 6
                       (str/join ", " (map pr-str (:depends-on t)))
                       (max 6 (- body-w 6)) t/footer-fg-muted false))]
    (-> (vec title-rows)
        (conj meta-segs)
        (into accept-rows)
        (into dep-rows)
        (conj overlay-blank-row))))
(defn- task-overlay-lines
  "TASKS section body — one `task-entry-rows` card per task, status-sorted
   (doing → todo → done → cancelled). Empty state is a single hint row."
  [tasks body-w]
  (if (empty? tasks)
    [[["No tasks yet — the model opens one with (task-set! …)." t/footer-fg-muted false]]]
    (->> tasks
         (sort-by (fn [[k t]] [(task-status-rank (or (:status t) :todo) 9) (str k)]))
         (mapcat (fn [[k t]] (task-entry-rows k t body-w)))
         vec)))
(defn- fact-entry-rows
  "Modern multi-row card for ONE fact: a status glyph (active • / superseded
   ⊘) + bold key, the WRAPPED content indented under it, then a dim meta row
   joining `⛁N files`, `↳ depends …`, and `⚡ contradicts …` when present,
   then a blank spacer. Nothing truncated — content wraps to `body-w`."
  [k f body-w]
  (let [super?    (= :superseded (:status f))
        glyph-seg [(if super? "⊘ " "• ") (if super? t/footer-fg-muted t/status-ok) true]
        key-row   [glyph-seg [(name k) (if super? t/footer-fg-muted t/header-active-tab-accent) true]]
        content   (not-empty (str (:content f)))
        content-rows (when content
                       (wrapped-rows [["    " t/dialog-fg false]] 4 content
                         (max 6 (- body-w 4)) (if super? t/footer-fg-muted t/dialog-fg) false))
        meta-parts (cond-> []
                     (pos? (count (:files f)))
                     (conj (str "⛁" (count (:files f)) " files"))
                     (seq (:depends-on f))
                     (conj (str "↳ depends " (str/join ", " (map pr-str (:depends-on f)))))
                     (seq (:contradicts f))
                     (conj (str "⚡ contradicts "
                             (str/join ", " (map pr-str (sort (:contradicts f)))))))
        meta-rows  (when (seq meta-parts)
                     (wrapped-rows [["    " t/footer-fg-muted false]] 4
                       (str/join "  ·  " meta-parts) (max 6 (- body-w 4))
                       t/footer-fg-muted false))]
    (-> [key-row]
        (into content-rows)
        (into meta-rows)
        (conj overlay-blank-row))))
(defn- fact-overlay-lines
  "FACTS section body — one `fact-entry-rows` card per fact, active facts
   first then superseded. Empty state is a single hint row."
  [facts body-w]
  (if (empty? facts)
    [[["No facts yet — the model records one with (fact-set! …)." t/footer-fg-muted false]]]
    (->> facts
         (sort-by (fn [[k f]] [(if (= :superseded (:status f)) 1 0) (str k)]))
         (mapcat (fn [[k f]] (fact-entry-rows k f body-w)))
         vec)))
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
  [g cols rows {:keys [tasks facts archived]}]
  (let [total (count tasks)
        done (count (filter (fn [[_ t]] (= :done (:status t))) tasks))
        title (str "Context"
                   (when (pos? total) (format "  ·  tasks %d/%d done" done total))
                   (when (pos? (count facts)) (format "  ·  facts %d" (count facts))))
        hint "F2 to close"
        ;; Build lines at a generous width, then size the dialog to the actual
        ;; content (golden-dialog-size clamps width+height to the terminal).
        body-w (dialogs/default-content-width cols)
        blank [["" t/dialog-hint false]]
        arch-tasks (into {} (filter (fn [[_ v]] (= :task (:vis/kind v))) archived))
        lines (vec (concat [(section-line "TASKS") blank]
                           (task-overlay-lines tasks body-w)
                           (when (seq arch-tasks)
                             (concat [blank (section-line "ARCHIVED") blank]
                                     (task-overlay-lines arch-tasks body-w)))
                           [blank (section-line "FACTS") blank]
                           (fact-overlay-lines facts body-w)))
        line-w (fn [segs] (reduce + 0 (map (comp p/display-width first) segs)))
        content-w (reduce max (p/display-width title) (map line-w lines))
        bounds (dialogs/draw-dialog-chrome! g cols rows title content-w (count lines))
        {:keys [left inner-w]} bounds
        {:keys [content-top content-h hint-row]} (dialogs/dialog-layout bounds (count lines))
        n (count lines)
        overflow? (> n content-h)
        shown-n (if overflow? (max 0 (dec content-h)) n)
        paint-line (fn [i segs]
                     (let [r (+ content-top i)]
                       (loop [x (+ left 1)
                              ss segs]
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
      (paint-line shown-n
                  [[(str "… " (- n shown-n) " more — full working memory lives in ctx")
                    t/dialog-hint false]]))
    ;; Centered dim close hint on the dialog's hint row.
    (when hint-row
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-hint t/dialog-bg)
      (p/put-str! g (+ left 1 (max 0 (quot (- inner-w (p/display-width hint)) 2))) hint-row hint))
    (p/clear-styles! g)))
