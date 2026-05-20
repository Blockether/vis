(ns com.blockether.vis.ext.channel-tui.header
  "Dedicated header band painted above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       ✓ Copied!                 Session title          ● Recording 00:01  d8d6a0a1
       (notification banner)     (or fallback placeholder)   (channel status + id + click target)

   - LEFT: latest active host notification (`com.blockether.vis.core/notify!`).
     Color-coded by `:level` (success / info / warn / error). Empty
     when no notification is active. The host notifications module
     is the single source of truth for cross-channel ephemeral
     signals - any extension or channel can `(v/notify! ...)` and the
     banner surfaces here. Voice/recording status is NOT rendered here;
     it lives in the RIGHT slot so it cannot collide with notifications.
   - CENTER: session title from app-db (`:title`). When the
     session has no title yet, falls back to a placeholder so
     the row never looks broken on a fresh run.
   - RIGHT: short session id (first 8 chars of the UUID, the
     same convention `vis sessions` uses) as the clickable
     affordance that drops the FULL UUID onto the system
     clipboard. Visual feedback is the LEFT-slot `✓ Copied!` notification
     - same mechanism every other cross-channel signal flows through.

   Pure draw: reads `:title` and `:session` from app-db, the
   active notifications list from `vis.core/notifications`, writes
   cells, registers ONE click region for the copy affordance.

   Repaint: the banner updates as notifications come and go.
   `screen.clj` registers a watcher on screen mount that bumps the
   render version for any change, so a `(notify! ...)` from anywhere
   nudges this band to repaint immediately."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            ;; Channel-agnostic header policy (slot ratios, workspace
            ;; switcher padding/cap, glyphs, default labels). Lives in
            ;; `internal/header.cljc` so a future web/Telegram channel
            ;; reuses the same values without touching TUI code.
            [com.blockether.vis.internal.header :as vh]))

;; -- Channel-contribution conventions consumed by this namespace --------------
;;
;; Extensions contribute to the header band by adding entries to their
;; `:ext/channel-contributions` map. The TUI consumes the following slot:
;;
;;   {:tui.slot/header-row
;;    [{:id :my.extension/header-row
;;      :fn (fn [db cols] -> ir | nil)}]}
;;
;; The fn returns CANONICAL IR (`[:ir {?:align ?:fg-role ...} &
;; blocks]`) — channel-agnostic data, NOT a paint thunk. The TUI
;; walks the IR via `ir-tui/ir->lines` and paints the resulting
;; styled lines into rows; other channels (Telegram, web) translate
;; the same IR to their own surface (markdown, HTML, etc.).
;;
;; Optional render hints carried on the IR root attrs map:
;;   :align    one of #{:left :center :right} — default :center
;;   :fg-role  one of #{:default :muted :warn :error :success}
;;             — maps to the channel's palette; default :default
;;
;; Channels are free to ignore unknown hint keys.
;;
;; Same declarative pattern as `:goal/slash` (slash commands) and
;; `:voice/foo` (voice indicator). No `requiring-resolve` into
;; channel-tui from extension code; channel-tui never imports specific
;; extension namespaces. Symmetric with how other channels would
;; expose their own hook conventions (e.g. `:telegram/preamble`).
;;
;; See `com.blockether.vis.internal.extension/channel-contributions-for`.

(def ^:const header-rows-base
  "Minimum rows reserved by the header band: top rule + content row +
   bottom rule. `header-rows` adds the height contributed by registered
   `:tui.slot/header-row` extensions on top of this base."
  3)

(def ^:private status-action-separator
  "Gap between a live channel status (voice recording/transcribing) and
   persistent right-side copy actions. TUI-specific because it sizes to
   a fixed pair of terminal cells."
  "  ")

(defn- title-or-placeholder
  "Visible title for the active session. Delegates to the shared
   helper so every channel reuses the same placeholder text."
  [db]
  (vh/title-or-placeholder (:title db)))

(def ^:private active-workspace-states
  "PLAN.md decision 12 — header strip shows live workspaces only.
   Merged + discarded rows stay in DB for transcript references but
   never appear in any list, panel, or overlay."
  #{:active :merging})

(defn- workspace-strip-visible?
  "True for entries that should appear in the header strip. Entries
   without an attached :workspace record (synthetic fallback) are
   always visible. Entries with a workspace record are visible only
   when its state is :active or :merging."
  [entry]
  (let [state (some-> entry :workspace :state)]
    (or (nil? state)
      (contains? active-workspace-states state))))

(defn- workspace-entries
  "Return entries to render in the centre strip, ALWAYS non-empty.

   Each entry represents a workspace (1:1 with its session under
   PLAN.md decision 1); the active entry's label tracks the session
   title (the state layer updates it on `:set-title`). Entries are
   filtered to PLAN.md decision 12 — finished (merged + discarded)
   workspaces never reach the strip. When the app-db has not yet
   initialised its workspace list — fresh boot, first paint, or
   stand-alone draw in tests — we synthesise a single active entry
   labelled with the session title (or the `Untitled session`
   placeholder) so the centre slot is never empty."
  [db]
  (let [entries (filterv workspace-strip-visible? (:workspaces db))]
    (if (seq entries)
      entries
      [{:id (or (:active-workspace-id db) :main)
        :label (title-or-placeholder db)
        :active? true}])))

(defn- active-workspace-entry-id
  [db entries]
  (or (:active-workspace-id db)
    (:id (some #(when (:active? %) %) entries))
    (:id (first entries))))

(defn- contributor-disabled?
  [db contribution-id]
  (let [disabled (get-in db [:settings :contributors-disabled])]
    (and (set? disabled) (contains? disabled contribution-id))))

(defn- ir-root?
  "Lightweight check: shape returned by contribution fn is canonical IR.
   Does NOT validate inner blocks; just confirms the envelope so
   we know to walk it. Bad inner shape will be caught by ir-tui."
  [x]
  (and (vector? x) (= :ir (first x)) (>= (count x) 1)))

(defn- fg-role->color
  "Map an extension-declared `:fg-role` keyword to a TUI theme color.
   Channels are free to interpret the same role differently; the role
   is the channel-agnostic intent (`:warn` = warning, `:muted` = de-
   emphasised). Unknown roles fall back to `:default` (`footer-fg`)."
  [role]
  (case role
    :muted   t/footer-fg-muted
    :warn    t/footer-warning-fg
    :error   t/footer-error-fg
    :success t/footer-fg-strong
    t/footer-fg))

(defn- align-x
  "Compute the starting column for a line of `line-w` cells inside a
   `cols`-wide band, given the alignment hint. `:center` is the
   default; `:left` pins to `edge-pad`; `:right` pins to the right."
  ^long [align ^long line-w ^long cols]
  (let [edge-pad 1]
    (case align
      :left  (long edge-pad)
      :right (long (max edge-pad (- cols edge-pad line-w)))
      ;; default = :center
      (long (max edge-pad (quot (- cols line-w) 2))))))

(defn- ir->header-row-spec
  "Convert canonical IR (returned by a header-row hook) into the
   internal row spec `{:height :draw!}` used by `draw-header!`.

   - Walks the IR via `ir-tui/ir->lines` to get styled lines.
   - Reads optional `:align` and `:fg-role` from the IR root attrs.
   - Returns nil when the IR walks to zero lines.

   The :draw! closure paints each line via `paint-styled-line!`,
   honouring inline sentinels (bold/italic/code/strike) emitted by
   the IR walker."
  [ir ^long cols]
  (let [attrs   (when (and (>= (count ir) 2) (map? (nth ir 1))) (nth ir 1))
        align   (or (:align   attrs) :center)
        fg-role (or (:fg-role attrs) :default)
        ;; Subtract a 1-col edge pad on each side; tighter wrap than
        ;; the cols arg the painter receives.
        wrap-w  (max 1 (- cols 2))
        ;; ir-tui returns vec of `{:runs [{:text :style}...]}`. We
        ;; convert to sentinel-prefixed plain strings via the
        ;; existing adapter so paint-styled-line! can render bold /
        ;; italic / code spans the same way every other styled line
        ;; in the TUI does.
        lines    (ir-tui/ir->lines ir wrap-w)
        line-strs (when (seq lines) (ir-tui/lines->sentinel-strings lines))]
    (when (seq line-strs)
      (let [fg (fg-role->color fg-role)]
        {:height (count line-strs)
         :draw!
         (fn [^com.googlecode.lanterna.graphics.TextGraphics g ^long row-top]
           (loop [i 0 lines (seq line-strs)]
             (when lines
               (let [^String line (first lines)
                     visible-w    (p/display-width line)
                     x            (align-x align visible-w cols)
                     y            (+ row-top (long i))]
                 (p/clear-styles! g)
                 (p/set-colors! g fg t/terminal-bg)
                 ;; italic by default for header subtitle rows -
                 ;; that's the established visual style for this
                 ;; band; extensions can override by emitting their
                 ;; own ITALIC sentinels via [:em ...] inside the IR.
                 (p/enable! g p/ITALIC)
                 (p/paint-styled-line! g x y line
                   fg t/terminal-bg t/code-block-fg t/code-block-bg)
                 (p/clear-styles! g)
                 (recur (inc i) (next lines))))))}))))

(defn- header-row-specs
  "Query every extension's `:tui.slot/header-row` contribution, run its
   `:fn`, collect the resulting row specs.

   Slot contract: `:fn` returns canonical IR (or nil to skip this frame).
   The IR is walked + converted to a row spec here.

   Settings can disable a contribution by id via `:contributors-disabled`.
   Contribution crashes never propagate — misbehaving extensions just lose
   their row that frame."
  [db cols]
  (vec
    (for [{:keys [id] f :fn} (vis/channel-contributions-for :tui :tui.slot/header-row)
          :when (and (ifn? f)
                  (not (contributor-disabled? db id)))
          :let [ir   (try (f db cols) (catch Throwable _ nil))
                spec (when (ir-root? ir)
                       (try (ir->header-row-spec ir cols)
                         (catch Throwable _ nil)))]
          :when (and spec (pos? (long (or (:height spec) 0))))]
      {:id id :spec spec})))

(defn header-rows
  "Rows needed by the header for this app-db. Workspace switcher lives inside
   the base content row; each enabled extension that declares a
   `:tui.slot/header-row` channel contribution adds its fn's reported height.

   Note: contribution fns are invoked here AND in `draw-header!` (the
   render path needs both the total height and the draw-fn).
   Extensions should keep their fn cheap or memoise
   per-frame internally — the goal extension uses a 100ms TTL atom
   for its SQLite lookup."
  ([db]
   ;; Arity without cols: pass a sentinel. Most hooks key their
   ;; height on db state only.
   (header-rows db 0))
  ([db cols]
   (+ header-rows-base
     (reduce + 0 (map #(long (:height (:spec %)))
                   (header-row-specs db cols))))))

(defn- short-id
  "Project a session's UUID onto the shared short-form length."
  [session]
  (vh/short-id (:id session)))

(defn- full-id [session]
  (some-> session :id str))

(defn- ellipsize
  [text max-cols]
  (p/truncate-cols (str text) (max 0 (long max-cols))))

(defn- latest-notification
  "Most-recently-pushed active notification, or nil. We display ONE
   at a time in the header - the LEFT slot is a single row. If
   multiple are active simultaneously, the freshest wins; older ones
   stay in the queue and surface as the freshest one expires."
  []
  (last (vis/notifications)))

(defn- status-expired?
  [status now-ms]
  (when-let [until (:until status)]
    (<= (long until) (long now-ms))))

(defn- latest-channel-status
  [{:keys [channel-status]}]
  (let [now-ms (System/currentTimeMillis)]
    (->> (vals channel-status)
      (filter #(seq (:text %)))
      (remove #(status-expired? % now-ms))
      (sort-by #(long (or (:updated-at-ms %) 0)))
      last)))

(defn- level->fg
  "Map a notification level to a foreground color. Falls back to the
   muted-footer color so an unknown level still renders something."
  [level]
  (case level
    :success t/footer-fg-strong
    :warn    t/footer-warning-fg
    :error   t/footer-error-fg
    :info    t/footer-spinner-fg
    t/footer-fg-muted))

(defn- draw-rule!
  "Paint a full-width single-line horizontal rule on `row`."
  ([g row cols]
   (draw-rule! g row cols t/footer-fg-muted))
  ([g row cols fg]
   (p/clear-styles! g)
   (p/set-colors! g fg t/terminal-bg)
   (dotimes [c cols]
     (p/set-char! g c row p/BOX_H))
   (p/clear-styles! g)))

(defn- id-copy-block-text [id-short]
  (if id-short
    id-short
    ""))

(def ^:dynamic *register-click-regions?*
  "Bind false for header-only hover repaints. Geometry did not change,
   so the previous full frame's published click regions remain valid and
   the repaint must not mutate the staged click-region buffer."
  true)

(defn- right-block-text
  "Compose the right-side text: \"4b1ed602\" when a session id
   exists, otherwise empty. Single place that knows the layout so
   `draw-header!` can stay focused on placement math."
  [id-short]
  (id-copy-block-text id-short))

(defn- clamp-long
  [n lo hi]
  (max (long lo) (min (long hi) (long n))))

(defn- active-strip-index
  [entries active-id]
  (or (first (keep-indexed #(when (= (:id %2) active-id) %1) entries)) 0))

(defn- visible-workspace-window
  [entries active-id width]
  (let [entries (vec entries)
        n (count entries)
        width (max 0 (long width))
        max-visible (vh/max-visible-workspace-count n width)
        overflow? (> n max-visible)
        active-idx (active-strip-index entries active-id)
        half (quot max-visible 2)
        start (if overflow?
                (clamp-long (- active-idx half) 0 (max 0 (- n max-visible)))
                0)
        end (min n (+ start max-visible))]
    {:overflow? overflow?
     :start start
     :entries (mapv (fn [idx entry] (assoc entry :header/original-index idx))
                (range start end)
                (subvec entries start end))}))

(defn- truncate-with-ellipsis
  "Truncate `s` so its display width fits in `max-cols`. When truncation
   actually happens, append `vh/workspace-ellipsis` so overflow is visible."
  ^String [s ^long max-cols]
  (let [s (or s "")]
    (cond
      (<= max-cols 0) ""
      (<= (p/display-width s) max-cols) s
      (= max-cols 1) (p/truncate-cols vh/workspace-ellipsis 1)
      :else (str (p/truncate-cols s (dec max-cols)) vh/workspace-ellipsis))))

(defn- center-padded
  "Place `s` centred inside a `cell-w`-wide workspace cell with
   `vh/workspace-entry-padding` reserved on each side; ellipsises overflow."
  ^String [s ^long cell-w]
  (let [inner (max 0 (- cell-w (* 2 (long vh/workspace-entry-padding))))
        text  (truncate-with-ellipsis s inner)
        text-w (p/display-width text)
        pad-total (max 0 (- cell-w text-w))
        left  (quot pad-total 2)
        right (- pad-total left)]
    (str (apply str (repeat left \space))
      text
      (apply str (repeat right \space)))))

(defn- draw-workspace-arrow!
  [g row col text direction]
  (let [hovered (cr/hovered)
        hovered? (and (= :workspace-entry (:kind hovered))
                   (= direction (:index hovered))
                   (= row (get-in hovered [:bounds :row])))]
    (p/clear-styles! g)
    (p/set-colors! g (if hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
    (when hovered? (p/enable! g p/BOLD))
    (p/put-str! g col row text)
    (p/clear-styles! g)
    (when *register-click-regions?*
      (cr/register!
        {:bounds {:row row :col col :width (p/display-width text)}
         :kind :workspace-entry
         :index direction
         :workspace-id direction
         :text direction
         :enabled? true}))))

(defn- draw-center-title!
  "Paint one workspace/session title as inert header text. With only one
   workspace there is nothing to switch, so this must not look or behave like
   a switcher entry."
  [g row left width text]
  (when (pos? (long width))
    (let [shown (truncate-with-ellipsis text width)
          w     (p/display-width shown)
          col   (+ left (max 0 (quot (- width w) 2)))]
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g col row shown)
      (p/clear-styles! g))))

(defn- draw-center-workspaces!
  "Paint the visible workspace switcher window inside the center 60% slot.

   Workspaces are painted directly here because the header needs a fixed
   `vh/workspace-entry-padding`-cell inner margin and an ellipsis on overflow.
   Each cell still occupies its full width on screen — fill-rect paints the
   active/inactive background — but the label itself is centred within the
   inner area `(cell-w - 2*padding)`."
  [g entries active-id row left width]
  (let [{:keys [overflow? entries]} (visible-workspace-window entries active-id width)
        arrow-w 1
        arrow-gap 1
        entries-left (if overflow? (+ left arrow-w arrow-gap) left)
        entries-width (max 0 (- width (if overflow? (* 2 (+ arrow-w arrow-gap)) 0)))
        n (count entries)]
    (when overflow?
      (draw-workspace-arrow! g row left vh/workspace-arrow-left :prev)
      (draw-workspace-arrow! g row (+ left width (- arrow-w)) vh/workspace-arrow-right :next))
    (when (and (pos? n) (pos? entries-width))
      (let [base (quot entries-width n)
            extra (rem entries-width n)
            cells (loop [idx 0 x entries-left out []]
                    (if (= idx n)
                      out
                      (let [cell-w (+ base (if (< idx extra) 1 0))
                            entry (nth entries idx)
                            label (p/tab-display-label entry)
                            text (center-padded label cell-w)
                            active? (= (:id entry) active-id)]
                        (recur (inc idx)
                          (+ x cell-w)
                          (conj out (assoc entry
                                      :left x
                                      :width cell-w
                                      :text text
                                      :active? active?))))))]
        (doseq [{:keys [left width active? text id]
                 idx :header/original-index}
                cells
                :when (pos? (long width))]
          (p/clear-styles! g)
          (if active?
            ;; Inverted slab: black bg + white fg (BOLD) makes the
            ;; active workspace pop; no BORDERED outline so the slab reads
            ;; clean. Inactive workspaces are dim italic on the header surface.
            (do (p/set-colors! g t/header-active-tab-fg t/header-active-tab-bg)
              (p/enable! g p/BOLD))
            (do (p/set-colors! g t/border-fg t/dialog-bg)
              (p/enable! g p/ITALIC)))
          (p/fill-rect! g left row width 1)
          (p/put-str! g left row text)
          (p/clear-styles! g)
          (when *register-click-regions?*
            (cr/register!
              {:bounds {:row row :col left :width width}
               :kind :workspace-entry
               :index idx
               :workspace-id id
               :text id
               :enabled? true})))
        cells))))

(defn draw-header!
  "Paint the header band starting at `header-top`, full width `cols`.
   Main content row is split 20% / 60% / 20%:

   - LEFT 20%: ephemeral host notifications ONLY. The session
     title does NOT live here; it lives in the centre slot. When no
     notification is active the LEFT slot stays blank.
   - CENTER 60%: workspace title or workspace switcher. With one workspace,
     paint inert title text. With multiple workspaces, paint switchable
     workspace entries. When app-db has not yet materialised a workspace list,
     `workspace-entries` synthesises one placeholder workspace so a fresh
     session reads as `Untitled session` in the centre.
   - RIGHT 20%: live channel status + session-id copy affordance.

   Workspaces are part of the header row (no separate band). Overflow shows
   clickable left/right arrows that cycle through workspaces."
  [g db header-top cols]
  (let [workspaces (workspace-entries db)
        top-rule-row header-top
        content-row (inc header-top)
        contrib-specs (header-row-specs db cols)
        bottom-row (dec (+ header-top (header-rows db cols)))
        edge-pad 1
        left-w (max 0 (quot cols 5))
        right-slot-w (max 0 (quot cols 5))
        center-w (max 0 (- cols left-w right-slot-w))
        left-x 0
        center-x left-w
        right-x (+ left-w center-w)
        id-short (short-id (:session db))
        full-uuid (full-id (:session db))
        id-copy-text (id-copy-block-text id-short)
        action-text (right-block-text id-short)
        status (latest-channel-status db)
        status-raw (some-> status :text)
        action-w (p/display-width action-text)
        status-gap (if (and (seq status-raw) (pos? action-w)) status-action-separator "")
        status-gap-w (p/display-width status-gap)
        status-cap (max 0 (- right-slot-w edge-pad action-w status-gap-w))
        status-text (when (seq status-raw) (ellipsize status-raw status-cap))
        status-w (p/display-width (or status-text ""))
        right-text (str (or status-text "") status-gap action-text)
        right-w (p/display-width right-text)
        id-copy-w (p/display-width id-copy-text)
        right-col (max right-x (- cols edge-pad right-w))
        action-col (+ right-col status-w status-gap-w)
        banner (latest-notification)
        notif-text (some-> banner :text)
        notif-level (some-> banner :level)
        status-level (some-> status :level)
        left-cap (max 0 (- left-w edge-pad 1))
        notif-trim (when notif-text (ellipsize notif-text left-cap))
        active-id (active-workspace-entry-id db workspaces)
        single-title (some-> workspaces first p/tab-display-label)]
    (draw-rule! g top-rule-row cols)

    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)
    (p/fill-rect! g 0 content-row cols 1)

    ;; LEFT 20%: notifications only. No title here — centre slot owns
    ;; the active workspace/session label.
    (when (seq notif-trim)
      (p/clear-styles! g)
      (p/set-colors! g (level->fg notif-level) t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g (+ left-x edge-pad) content-row notif-trim)
      (p/clear-styles! g))

    ;; CENTER 60%: one inert title, or a switcher when multiple workspaces exist.
    (if (> (count workspaces) 1)
      (draw-center-workspaces! g workspaces active-id content-row center-x center-w)
      (draw-center-title! g content-row center-x center-w single-title))

    ;; RIGHT 20%: live status + session-id copy affordance.
    (when (pos? right-w)
      (let [hovered-region (cr/hovered)
            id-hovered? (and (= content-row (get-in hovered-region [:bounds :row]))
                          (= :copy-id (:kind hovered-region)))]
        (when (pos? status-w)
          (p/clear-styles! g)
          (p/set-colors! g (level->fg status-level) t/terminal-bg)
          (p/enable! g p/BOLD)
          (p/put-str! g right-col content-row status-text)
          (p/clear-styles! g)
          (when (pos? status-gap-w)
            (p/set-colors! g t/header-fg t/terminal-bg)
            (p/put-str! g (+ right-col status-w) content-row status-gap)
            (p/clear-styles! g)))
        (when (pos? action-w)
          (p/clear-styles! g)
          (p/set-colors! g (if id-hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
          (when id-hovered? (p/enable! g p/BOLD))
          (p/put-str! g action-col content-row id-copy-text)
          (p/clear-styles! g)
          (when (and *register-click-regions?* full-uuid)
            (cr/register!
              {:bounds {:row content-row :col action-col :width id-copy-w}
               :kind :copy-id
               :text full-uuid
               :enabled? true})))))

    ;; Extension-contributed rows.
    (loop [row (inc content-row)
           specs (seq contrib-specs)]
      (when specs
        (let [{:keys [spec]} (first specs)
              h (long (:height spec))
              draw! (:draw! spec)]
          (when (pos? h)
            (p/clear-styles! g)
            (p/set-colors! g t/footer-fg t/terminal-bg)
            (p/fill-rect! g 0 row cols h)
            (when (ifn? draw!)
              (try (draw! g (long row))
                (catch Throwable _ nil))))
          (recur (+ row h) (next specs)))))

    (draw-rule! g bottom-row cols)
    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
