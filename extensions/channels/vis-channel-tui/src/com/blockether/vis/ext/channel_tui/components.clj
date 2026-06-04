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
  "Cells a `close-button!` occupies: a `│` divider + a space + 1-col `✕` +
  a trailing space."
  4)

(def ^:private close-button-glyph "│ ✕ ")

(defn close-button!
  "Draw a `│ ✕ ` close affordance (divider + space + ✕ + space) at (col,row) in the tab's OWN foreground
   `tab-fg` on `tab-bg` — so it's high-contrast with the tab (white ✕ on a
   dark tab, dark ✕ on a light tab) without being a solid inverted block.
   Turns red + bold on hover. Registers its `:close-tab` click region for
   `workspace-id`. Returns the consumed width (`close-button-width`)."
  [g col row tab-fg tab-bg workspace-id register?]
  (let [hovered  (cr/hovered)
        hovered? (and (= :close-tab (:kind hovered))
                   (= workspace-id (:workspace-id hovered)))]
    (p/clear-styles! g)
    (p/set-colors! g (if hovered? t/close-button-hover-fg tab-fg) tab-bg)
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col row close-button-glyph)
    (p/clear-styles! g)
    (when register?
      (cr/register! {:bounds       {:row row :col col :width close-button-width}
                     :kind         :close-tab
                     :workspace-id workspace-id
                     :text         workspace-id
                     :enabled?     true}))
    close-button-width))

(defn tab-divider!
  "Paint a 1-col vertical `│` divider between two tabs at (col,row), in the
   header's foreground so it stays high-contrast against the surface (white
   on a dark terminal, dark on a light one)."
  [g col row]
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg t/terminal-bg)
  (p/put-str! g col row "│")
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
  [g {:keys [left row width label active? workspace-id index register? closable?]
      :or   {closable? true}}]
  (let [width       (long width)
        ;; Reserve room for the close button only when the cell can still
        ;; show a sliver of title beside it; otherwise the title wins.
        show-close? (and closable? (>= width (+ close-button-width 3)))
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

;; ── help overlay ────────────────────────────────────────────────────────────

(def ^:private help-shortcuts
  "[[keys description] …] rows shown in the Ctrl+H help card."
  [["Ctrl+H · F1"      "Toggle this help"]
   ["F2"               "Toggle the task panel"]
   ["Enter · Ctrl+X"   "Send message"]
   ["Alt+Enter"        "Insert a newline"]
   ["Esc"              "Clear draft · cancel turn"]
   ["Ctrl+C"           "Cancel turn · quit"]
   ["Tab · Shift+Tab"  "Switch tab"]
   ["Ctrl+W"           "Close tab  (or click the ✕)"]
   ["Ctrl+K"           "Command palette"]
   ["Ctrl+G"           "Sessions · workspaces"]
   ["Ctrl+T"           "Cycle model"]
   ["Ctrl+R"           "Cycle reasoning effort"]
   ["Ctrl+L"           "Cycle Codex verbosity"]
   ["Ctrl+A · Ctrl+E"  "Jump to line start · end"]
   ["Ctrl+U"           "Delete to line start"]
   ["Alt+B · Alt+F"    "Move word left · right"]
   ["Ctrl+V"           "Paste"]
   ["@"                "Pick a file"]
   ["Mouse"            "Click a tab to switch · ✕ to close"]])

;; ── header band chrome ──────────────────────────────────────────────────────

(defn band-rule!
  "Paint a full-width single-line horizontal rule across `cols` on `row`."
  ([g row cols] (band-rule! g row cols t/footer-fg-muted))
  ([g row cols fg]
   (p/clear-styles! g)
   (p/set-colors! g fg t/terminal-bg)
   (dotimes [c (long cols)]
     (p/set-char! g c row p/BOX_H))
   (p/clear-styles! g)))

(defn- level->fg
  "Map a notification level to a foreground color; unknown levels fall back
   to the muted-footer color so something still renders."
  [level]
  (case level
    :success t/footer-fg-strong
    :warn    t/footer-warning-fg
    :error   t/footer-error-fg
    :info    t/footer-spinner-fg
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
    (let [hovered  (cr/hovered)
          hovered? (and (= row (get-in hovered [:bounds :row]))
                     (= :copy-id (:kind hovered)))]
      (p/clear-styles! g)
      (p/set-colors! g (if hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
      (when hovered? (p/enable! g p/BOLD))
      (p/put-str! g col row text)
      (p/clear-styles! g)
      (when (and register? full-uuid)
        (cr/register! {:bounds   {:row row :col col :width (p/display-width text)}
                       :kind     :copy-id
                       :text     full-uuid
                       :enabled? true})))))

;; ── help overlay ────────────────────────────────────────────────────────────

(def ^:private help-title "Keyboard shortcuts  —  Ctrl+H / F1 to close")

(defn- pad-right ^String [^String s ^long w]
  (str s (apply str (repeat (max 0 (- w (p/display-width s))) \space))))

(defn help-overlay!
  "Draw a centered modal help card listing every keyboard shortcut, painting
   its own background over whatever is underneath. Pure chrome — registers no
   click regions; the caller dismisses it (Ctrl+H / F1 / any key). No-op when
   the terminal is too small to host the card."
  [g cols rows]
  (let [cols    (long cols)
        rows    (long rows)
        key-w   (reduce max 0 (map (comp p/display-width first) help-shortcuts))
        desc-w  (reduce max 0 (map (comp p/display-width second) help-shortcuts))
        inner-w (max (p/display-width help-title) (+ key-w 2 desc-w))
        box-w   (+ inner-w 4)                   ; 1 border + 1 pad per side
        box-h   (+ (count help-shortcuts) 4)    ; borders + title + blank row
        left    (max 0 (quot (- cols box-w) 2))
        top     (max 0 (quot (- rows box-h) 2))
        right   (+ left box-w -1)
        bottom  (+ top box-h -1)]
    (when (and (>= cols box-w) (>= rows box-h))
      ;; Solid background slab.
      (p/clear-styles! g)
      (p/set-colors! g t/border-fg t/dialog-bg)
      (doseq [r (range top (inc bottom))]
        (p/fill-rect! g left r box-w 1))
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

(defn- task-status-glyph [status]
  (case status :done "✓" :doing "◐" :cancelled "✗" "○"))

(defn- task-status-color [status]
  (case status
    :done      t/status-ok
    :doing     t/warning-fg
    :cancelled t/cancelled-fg
    t/footer-fg-muted))

(def ^:private task-status-rank {:doing 0 :todo 1 :done 2 :cancelled 3})

(defn- clip-str
  "Truncate `s` to `w` display columns with a trailing ellipsis."
  ^String [^String s ^long w]
  (if (<= (p/display-width s) w)
    s
    (str (subs s 0 (max 0 (dec w))) "…")))

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
                badge  (cond (:verified? t)  ["  ✓ verified"   t/status-ok   false]
                         (:acceptance t) ["  ⌛ unverified" t/warning-fg  false]
                         :else           nil)
                glyph  [(str (task-status-glyph status) " ") (task-status-color status) true]
                ;; title gets whatever width remains after glyph(2)+badge.
                badge-w (if badge (p/display-width (first badge)) 0)
                title-w (max 6 (- body-w 2 badge-w))
                title  [(clip-str (or (not-empty (str (:title t))) (name k)) title-w)
                        t/dialog-fg true]
                primary (cond-> [glyph title] badge (conj badge))
                accept (when-let [a (not-empty (str (:acceptance t)))]
                         [[(str "    ▸ " (clip-str a (max 6 (- body-w 6))))
                           t/footer-fg-muted false]])]
            (cond-> [primary] accept (into accept))))))))

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
          more  (max 0 (- (count facts) (count shown)))
          lines (mapv (fn [[k f]]
                        (let [super?  (= :superseded (:status f))
                              fcount  (count (:files f))
                              badge   (when (pos? fcount)
                                        [(str "  ⛁" fcount) t/footer-fg-muted false])
                              badge-w (if badge (p/display-width (first badge)) 0)
                              glyph   [(if super? "⊘ " "• ")
                                       (if super? t/footer-fg-muted t/status-ok) true]
                              txt     (clip-str
                                        (str (name k) ": "
                                          (or (not-empty (str (:content f))) ""))
                                        (max 6 (- body-w 2 badge-w)))]
                          (cond-> [glyph [txt (if super? t/footer-fg-muted t/dialog-fg) false]]
                            badge (conj badge))))
                  shown)]
      (cond-> lines
        (pos? more) (conj [[(str "    … +" more " more") t/footer-fg-muted false]])))))

(defn- section-line
  "A bold section header line (single segment)."
  [label]
  [[label t/dialog-title-fg true]])

(defn context-overlay!
  "Centered modal showing the session's working memory — `:session/tasks` AND
   `:session/facts` — the W3 user-visible panel. Mirrors `help-overlay!` chrome
   (own background, no click regions; caller dismisses on F2 / any key). Title
   carries a tasks-done + facts count summary; a TASKS section (status-sorted,
   colored glyphs, acceptance sub-lines, verify badges) then a FACTS section
   (active first, `⛁N` for file-bearing facts). `ctx` is `{:tasks … :facts …}`.
   No-op when the terminal is too small."
  [g cols rows {:keys [tasks facts]}]
  (let [cols    (long cols)
        rows    (long rows)
        total   (count tasks)
        done    (count (filter (fn [[_ t]] (= :done (:status t))) tasks))
        title   (str "Context"
                  (when (pos? total) (format "   tasks %d/%d done" done total))
                  (when (pos? (count facts)) (format "   facts %d" (count facts))))
        hint    "F2 to close"
        ;; Width: cap the card to most of the screen; size body to content.
        max-box (max 24 (- cols 6))
        body-w  (min 72 (max 28 (- max-box 4)))
        blank   [["" t/footer-fg-muted false]]
        lines   (vec (concat [(section-line "TASKS")] (task-overlay-lines tasks body-w)
                       [blank]
                       [(section-line "FACTS")] (fact-overlay-lines facts body-w)))
        line-w  (fn [segs] (reduce + 0 (map (comp p/display-width first) segs)))
        content-w (reduce max
                    (max (p/display-width title) (+ 2 (p/display-width hint)))
                    (map line-w lines))
        inner-w (min body-w content-w)
        box-w   (+ inner-w 4)
        box-h   (+ (count lines) 4)
        left    (max 0 (quot (- cols box-w) 2))
        top     (max 0 (quot (- rows box-h) 2))
        right   (+ left box-w -1)
        bottom  (+ top box-h -1)]
    (when (and (>= cols box-w) (>= rows box-h))
      ;; Solid slab + border.
      (p/clear-styles! g)
      (p/set-colors! g t/border-fg t/dialog-bg)
      (doseq [r (range top (inc bottom))]
        (p/fill-rect! g left r box-w 1))
      (p/put-str! g left top (str "┌" (apply str (repeat (- box-w 2) "─")) "┐"))
      (p/put-str! g left bottom (str "└" (apply str (repeat (- box-w 2) "─")) "┘"))
      (doseq [r (range (inc top) bottom)]
        (p/put-str! g left r "│")
        (p/put-str! g right r "│"))
      ;; Title (bold) + right-aligned dim close hint on the same row.
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-title-fg t/dialog-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g (+ left 2) (inc top) title)
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-hint t/dialog-bg)
      (let [hx (- right 1 (p/display-width hint))]
        (when (> hx (+ left 2 (p/display-width title)))
          (p/put-str! g hx (inc top) hint)))
      ;; Body lines: paint each colored segment left-to-right.
      (doseq [[i segs] (map-indexed vector lines)]
        (let [r (+ top 3 i)]
          (loop [x (+ left 2), ss segs]
            (when-let [[text color bold?] (first ss)]
              (p/clear-styles! g)
              (p/set-colors! g color t/dialog-bg)
              (when bold? (p/enable! g p/BOLD))
              (p/put-str! g x r text)
              (recur (+ x (p/display-width text)) (next ss))))))
      (p/clear-styles! g))))
