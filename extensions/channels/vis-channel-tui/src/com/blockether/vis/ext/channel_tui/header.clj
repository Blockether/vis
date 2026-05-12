(ns com.blockether.vis.ext.channel-tui.header
  "Dedicated header band painted above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       ✓ Copied!                 Conversation title          ● Recording 00:01  ⧉ d8d6a0a1 | ⧉ Transcript
       (notification banner)     (or fallback placeholder)   (channel status + id + click target)

   - LEFT: latest active host notification (`com.blockether.vis.core/notify!`).
     Color-coded by `:level` (success / info / warn / error). Empty
     when no notification is active. The host notifications module
     is the single source of truth for cross-channel ephemeral
     signals - any extension or channel can `(v/notify! ...)` and the
     banner surfaces here. Voice/recording status is NOT rendered here;
     it lives in the RIGHT slot so it cannot collide with notifications.
   - CENTER: conversation title from app-db (`:title`). When the
     conversation has no title yet, falls back to a placeholder so
     the row never looks broken on a fresh run.
   - RIGHT: short conversation id (first 8 chars of the UUID, the
     same convention `vis conversations` uses) + a clickable
     `⧉` affordance that drops the FULL UUID onto the system
     clipboard, followed by `| ⧉ Transcript` for whole-conversation
     Markdown copy. Visual feedback is the LEFT-slot `✓ Copied!` notification
     - same mechanism every other cross-channel signal flows through.

   Pure draw: reads `:title` and `:conversation` from app-db, the
   active notifications list from `vis.core/notifications`, writes
   cells, registers ONE click region for the copy affordance.

   Repaint: the banner updates as notifications come and go.
   `screen.clj` registers a watcher on screen mount that bumps the
   render version for any change, so a `(notify! ...)` from anywhere
   nudges this band to repaint immediately."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.theme :as t]))

;; -- Channel-hook conventions consumed by this namespace --------------
;;
;; Extensions contribute to the header band by adding entries to their
;; `:ext/channel-hooks` vec. The TUI consumes the following hook ids:
;;
;;   {:channel-id :tui
;;    :hook-id    :tui/header-row             ;; or :*/header-row
;;    :render-fn  (fn [db cols] -> ir | nil)}
;;
;; The render-fn returns CANONICAL IR (`[:ir {?:align ?:fg-role ...} &
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
;; See `com.blockether.vis.internal.extension/channel-hooks-for`.

(def ^:private id-display-chars
  "How many leading characters of the conversation UUID to show in
   the right region. 8 matches the prefix length already exposed by
   `vis conversations`, the `--conversation-id` short form, and the
   `format-conversation-not-found` listing in screen.clj."
  8)

(def ^:const HEADER_ROWS
  "Minimum rows reserved by the header band: top rule + content + bottom
   rule. Use `header-rows` for a concrete app-db, because workspace tabs add
   a tab row plus one bottom spacer when more than one tab exists, and
   registered header-row contributors (see `contributors.clj`) add their own
   rows below the title."
  3)

(def ^:private workspace-tabs-extra-rows
  "Rows added when workspace tabs are visible: tab top border + tab strip."
  2)

(def ^:private placeholder-title
  "Shown center when the conversation has no title yet (fresh run,
   first turn not finished yet). Italicised so it reads as a hint."
  "Untitled conversation")

(def ^:private copy-icon
  "Compact copy glyph used by the conversation-id affordance in the header."
  "⧉")

(def ^:private copy-affordance
  "Compact header affordance painted left of the short conversation id."
  copy-icon)

(def ^:private right-block-separator
  "Visual separator between the UUID-copy affordance and Markdown export action."
  " | ")

(def ^:private markdown-copy-label
  "Compact Markdown transcript export affordance painted after the conversation-id copy block."
  (str copy-icon " Transcript"))

(def ^:private status-action-separator
  "Gap between a live channel status (voice recording/transcribing) and
   persistent right-side copy actions."
  "  ")

(defn- workspace-tabs
  [db]
  (let [tabs (:workspace-tabs db)]
    (when (> (count tabs) 1)
      (vec tabs))))

(defn- active-workspace-tab-id
  [db tabs]
  (or (:active-workspace-id db)
    (:id (some #(when (:active? %) %) tabs))
    (:id (first tabs))))

(defn- contributor-disabled?
  [db hook-id]
  (let [disabled (get-in db [:settings :contributors-disabled])]
    (and (set? disabled) (contains? disabled hook-id))))

(defn- ir-root?
  "Lightweight check: shape returned by render-fn is canonical IR.
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
  "Query every extension's `*/header-row` hook, run its `:render-fn`,
   collect the resulting row specs.

   Hook contract: `:render-fn` returns canonical IR (or nil to skip
   this frame). The IR is walked + converted to a row spec here.

   Settings can disable a hook by id via `:contributors-disabled`.
   Hook crashes never propagate — misbehaving extensions just lose
   their row that frame."
  [db cols]
  (vec
    (for [{:keys [hook-id render-fn]} (vis/channel-hooks-for :tui)
          :when (and (ifn? render-fn)
                  (not (contributor-disabled? db hook-id))
                  ;; Consume hooks whose id is `:tui/header-row` or
                  ;; whose name ends in `header-row` (e.g.
                  ;; `:goal/header-row`). Lets each extension scope
                  ;; its hook id under its own namespace without
                  ;; conflicting with other tui channel-hook surfaces.
                  (or (= :tui/header-row hook-id)
                    (= "header-row" (name hook-id))))
          :let [ir   (try (render-fn db cols) (catch Throwable _ nil))
                spec (when (ir-root? ir)
                       (try (ir->header-row-spec ir cols)
                         (catch Throwable _ nil)))]
          :when (and spec (pos? (long (or (:height spec) 0))))]
      {:id hook-id :spec spec})))

(defn header-rows
  "Rows needed by the header for this app-db. Workspace tabs add a tab top
   border plus tab row when more than one tab exists; each enabled extension
   that declares a `:tui/header-row` channel-hook adds its render-fn's
   reported height.

   Note: render-fns are invoked here AND in `draw-header!` (the
   render path needs both the total height and the draw-fn).
   Extensions should keep their render-fn cheap or memoise
   per-frame internally — the goal extension uses a 100ms TTL atom
   for its SQLite lookup."
  ([db]
   ;; Arity without cols: pass a sentinel. Most hooks key their
   ;; height on db state only.
   (header-rows db 0))
  ([db cols]
   (+ HEADER_ROWS
     (if (seq (workspace-tabs db)) workspace-tabs-extra-rows 0)
     (reduce + 0 (map #(long (:height (:spec %)))
                   (header-row-specs db cols))))))

(defn- short-id [conversation]
  (when-let [id (some-> conversation :id str)]
    (when (seq id)
      (subs id 0 (min id-display-chars (count id))))))

(defn- full-id [conversation]
  (some-> conversation :id str))

(defn- title-text [db]
  (let [t (:title db)]
    (if (and (string? t) (not (str/blank? t)))
      [t false]
      [placeholder-title true])))

(defn- ellipsize
  [text max-chars]
  (let [text (str text)]
    (cond
      (<= max-chars 0) ""
      (<= (count text) max-chars) text
      :else (str (subs text 0 (max 0 (dec max-chars))) "..."))))

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
    (str copy-affordance " " id-short)
    ""))

(def ^:dynamic *register-click-regions?*
  "Bind false for header-only hover repaints. Geometry did not change,
   so the previous full frame's published click regions remain valid and
   the repaint must not mutate the staged click-region buffer."
  true)

(defn- markdown-copy-block-text [id-short]
  (if id-short markdown-copy-label ""))

(defn- right-block-text
  "Compose the right-side text: \"⧉ 4b1ed602 | ⧉ Transcript\" when a
   conversation id exists, otherwise empty. Single place that knows the layout
   so `draw-header!` can stay focused on placement math."
  [id-short]
  (let [id-text (id-copy-block-text id-short)
        md-text (markdown-copy-block-text id-short)]
    (if (seq id-text)
      (str id-text right-block-separator md-text)
      "")))

(defn draw-header!
  "Paint the header band starting at `header-top`, full width `cols`.
   The band is `HEADER_ROWS` rows tall: top rule, content row, bottom
   rule. Pure draw + ONE click-region registration; safe to call
   every frame.

   Layout per the namespace doc: notification banner LEFT, centered
   conversation title CENTER, short conversation id + `⧉`, separator, plus
   `⧉ Transcript` RIGHT. When the title would overlap either edge block, the title
   is truncated with an ellipsis so the diagnostically-important id
   stays readable.

   Side effect: registers separate click regions for the id-copy block
   and the Markdown-copy icon. The screen mouse handler (in `screen.clj`)
   recognises `:kind :copy-id` and `:kind :copy-as-markdown`, copies the
   corresponding payload, then pushes a host notification that this band
   surfaces in the LEFT slot."
  [g db header-top cols]
  (let [tabs         (workspace-tabs db)
        tabs?        (seq tabs)
        tab-top-rule-row (when tabs? header-top)
        tabs-row     (when tabs? (inc header-top))
        top-rule-row (+ header-top (if tabs? 2 0))
        content-row  (+ header-top (if tabs? 3 1))
        ;; Extension-contributed rows sit BETWEEN the title content
        ;; row and the bottom rule. Each `:tui/header-row` hook
        ;; returns a row spec or nil; we allocate rows for the
        ;; non-nil ones and call their `:draw!` to paint. Hooks
        ;; that return nil cost zero vertical space.
        contrib-specs (header-row-specs db cols)
        bottom-row   (dec (+ header-top (header-rows db cols)))
        edge-pad     1
        id-short     (short-id (:conversation db))
        full-uuid    (full-id  (:conversation db))
        id-copy-text (id-copy-block-text id-short)
        md-copy-text (markdown-copy-block-text id-short)
        action-text  (right-block-text id-short)
        status       (latest-channel-status db)
        status-raw   (some-> status :text)
        status-cap   (max 0 (quot cols 3))
        status-text  (when (seq status-raw)
                       (ellipsize status-raw status-cap))
        status-w     (p/display-width (or status-text ""))
        action-w     (p/display-width action-text)
        status-gap   (if (and (pos? status-w) (pos? action-w))
                       status-action-separator
                       "")
        status-gap-w (p/display-width status-gap)
        right-text   (str (or status-text "") status-gap action-text)
        right-w      (p/display-width right-text)
        id-copy-w    (p/display-width id-copy-text)
        md-copy-w    (p/display-width md-copy-text)
        right-col    (when (pos? right-w) (max 0 (- cols edge-pad right-w)))
        action-col   (when right-col (+ right-col status-w status-gap-w))
        separator-w  (p/display-width right-block-separator)
        md-copy-col  (when (and action-col (pos? md-copy-w))
                       (+ action-col id-copy-w separator-w))
        banner       (latest-notification)
        notif-text   (some-> banner :text)
        notif-level  (some-> banner :level)
        status-level (some-> status :level)
        ;; Reserve up to 1/3 of the row for the LEFT banner so the
        ;; centered title still has room. Banner truncates with an
        ;; ellipsis if the notification text is longer.
        left-cap     (max 0 (quot cols 3))
        left-trim    (when notif-text
                       (cond
                         (zero? left-cap)              ""
                         (<= (count notif-text) left-cap) notif-text
                         :else (str (subs notif-text 0 (max 0 (dec left-cap))) "...")))
        left-w       (or (some-> left-trim count) 0)
        gap          2
        title-max    (max 0 (- cols (* 2 edge-pad) right-w left-w
                              (if (pos? right-w) gap 0)
                              (if (pos? left-w) gap 0)))
        [title placeholder?] (title-text db)
        title-trim   (ellipsize title title-max)
        title-w      (p/display-width title-trim)
        title-col-raw (max edge-pad (quot (- cols title-w) 2))
        title-col    (cond
                       (and right-col
                         (> (+ title-col-raw title-w) (- right-col gap)))
                       (max edge-pad (- right-col gap title-w))
                       (and (pos? left-w)
                         (< title-col-raw (+ edge-pad left-w gap)))
                       (+ edge-pad left-w gap)
                       :else
                       title-col-raw)]

    ;; Workspace tabs live above the header chrome. They occupy the full width
    ;; with no left/right padding. The tab strip gets its own yellow top rule;
    ;; the normal header top rule stays between tabs and title.
    (when tabs?
      (draw-rule! g tab-top-rule-row cols t/footer-warning-fg)
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-fg t/dialog-bg)
      (p/fill-rect! g 0 tabs-row cols 1)
      (let [layout (p/draw-tabs! g tabs
                     {:left        0
                      :row         tabs-row
                      :width       cols
                      :gap         0
                      :bordered?   true
                      :active-id   (active-workspace-tab-id db tabs)
                      :fg          t/footer-warning-fg
                      :bg          t/dialog-bg
                      :active-fg   t/footer-warning-fg
                      :active-bg   t/dialog-title-bg
                      :inactive-fg t/footer-warning-fg
                      :inactive-bg t/dialog-bg})]
        (doseq [[idx {:keys [id left width]}] (map-indexed vector layout)
                :when (pos? (long width))]
          (when *register-click-regions?*
            (cr/register!
              {:bounds       {:row tabs-row :col left :width width}
               :kind         :workspace-tab
               :index        idx
               :workspace-id id
               :text         id
               :enabled?     true})))))
    (draw-rule! g top-rule-row cols)

    ;; Wipe content row to terminal-bg first so the previous frame's
    ;; characters can't bleed through the gaps between LEFT/CENTER/RIGHT.
    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)
    (p/fill-rect! g 0 content-row cols 1)

    ;; LEFT - latest notification banner.
    (when (pos? left-w)
      (p/clear-styles! g)
      (p/set-colors! g (level->fg notif-level) t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g edge-pad content-row left-trim)
      (p/clear-styles! g))

    ;; CENTER - current conversation title only. Conversation switching lives
    ;; in the Alt+Shift+↑/↓ picker, not as persistent header tabs.
    (when (pos? title-w)
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (if placeholder?
        (p/enable! g p/ITALIC)
        (p/enable! g p/BOLD))
      (p/put-str! g title-col content-row title-trim)
      (p/clear-styles! g))

    ;; RIGHT - live channel status (voice recording/transcribing) + copy
    ;; conversation ID block + Markdown transcript export label. Keeping the
    ;; live voice status on the right leaves the left notification lane free.
    ;; Visual hover feedback: when either clickable region is hovered,
    ;; that exact affordance shifts to header-hover-fg and gains BOLD. Terminal emulators
    ;; don't allow applications to control the mouse cursor shape, so this
    ;; is the strongest affordance we can offer.
    (when (pos? right-w)
      (let [hovered-region    (cr/hovered)
            hovered-kind      (:kind hovered-region)
            hovered-row?      (= content-row (get-in hovered-region [:bounds :row]))
            id-hovered?       (and hovered-row? (= :copy-id hovered-kind))
            markdown-hovered? (and hovered-row? (= :copy-as-markdown hovered-kind))]
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
          (when id-hovered?
            (p/enable! g p/BOLD))
          (p/put-str! g action-col content-row id-copy-text)
          (p/clear-styles! g)
          (p/set-colors! g t/header-fg t/terminal-bg)
          (p/put-str! g (+ action-col id-copy-w) content-row right-block-separator)
          (p/clear-styles! g)
          (p/set-colors! g (if markdown-hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
          (when markdown-hovered?
            (p/enable! g p/BOLD))
          (p/put-str! g md-copy-col content-row md-copy-text)
          (p/clear-styles! g)
          (when (and *register-click-regions?* full-uuid)
            (cr/register!
              {:bounds   {:row content-row :col action-col :width id-copy-w}
               :kind     :copy-id
               :text     full-uuid
               :enabled? true})
            (cr/register!
              {:bounds   {:row content-row :col md-copy-col :width md-copy-w}
               :kind     :copy-as-markdown
               :text     full-uuid
               :enabled? true})))))

    ;; Extension-contributed rows. Each `:tui/header-row` hook gets
    ;; its allocated row range below the content row. We wipe each
    ;; row first so previous-frame characters can't bleed through,
    ;; then hand the painter to the hook's `:draw!`.
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
                (catch Throwable _
                  ;; Hook crashed — leave its row blank. Painter
                  ;; never crashes the render thread.
                  nil))))
          (recur (+ row h) (next specs)))))

    (draw-rule! g bottom-row cols)

    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
