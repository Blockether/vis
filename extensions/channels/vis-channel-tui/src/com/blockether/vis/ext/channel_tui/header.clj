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
            [com.blockether.vis.ext.channel-tui.theme :as t]
            ;; Pure goal helpers for paused-aware elapsed math + the
            ;; one-line summary formatter. The READ side of the goal
            ;; (db lookup) is resolved lazily via `requiring-resolve`
            ;; below so vis still boots when vis-goal isn't on the
            ;; classpath.
            [com.blockether.vis.internal.goal :as goal]))

(def ^:private id-display-chars
  "How many leading characters of the conversation UUID to show in
   the right region. 8 matches the prefix length already exposed by
   `vis conversations`, the `--conversation-id` short form, and the
   `format-conversation-not-found` listing in screen.clj."
  8)

(def ^:const HEADER_ROWS
  "Minimum rows reserved by the header band: top rule + content + bottom
   rule. Use `header-rows` for a concrete app-db, because workspace tabs add
   one row only when there is more than one tab and an active /goal adds
   one subtitle row."
  3)

(def ^:private goal-subtitle-objective-max
  "How wide the goal objective gets in the subtitle row before we
   ellipsis-truncate. The full objective is up to 4000 chars but we
   never paint more than this on a single header row — the model can
   still see it in full via the system-prompt block."
  80)

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

(defn- current-goal
  "Read the goal for the active conversation, or nil. Resolves the
   goal extension lazily so vis still boots when vis-goal isn't on
   the classpath. Tolerates missing db / conversation-id and any
   downstream throw — the header must never crash on a goal lookup.

   Called every render frame; cost is one DB query against an
   indexed extension_aggregate row. SQLite WAL + connection pool
   makes this measurably cheap (microseconds)."
  [db]
  (when-let [conv-id (some-> db :conversation :id)]
    (when-let [getter (try (requiring-resolve
                             'com.blockether.vis.ext.goal.core/get-goal)
                        (catch Throwable _ nil))]
      (try (getter (vis/db-info) conv-id)
        (catch Throwable _ nil)))))

(defn- goal-subtitle-visible?
  "True when the goal should occupy a subtitle row. Active and paused
   goals always show; done(:cleared|:achieved|:unmet|:budget-limited)
   keep their last subtitle so the user can read the outcome — they
   only disappear when the user / model fires a fresh `(goal/set ...)`
   or `/goal X` (which replaces the row in place)."
  [goal]
  (and goal (some? (:status goal))))

(defn header-rows
  "Rows needed by the header for this app-db. Workspace tabs add one
   row when more than one tab exists; an active or terminated /goal
   adds another row for the subtitle."
  [db]
  (+ HEADER_ROWS
    (if (seq (workspace-tabs db)) 1 0)
    (if (goal-subtitle-visible? (current-goal db)) 1 0)))

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
  "Paint a full-width single-line horizontal rule on `row` in the
   muted footer color."
  [g row cols]
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg-muted t/terminal-bg)
  (dotimes [c cols]
    (p/set-char! g c row p/BOX_H))
  (p/clear-styles! g))

(defn- id-copy-block-text [id-short]
  (if id-short
    (str copy-affordance " " id-short)
    ""))

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
        tabs-row     (when (seq tabs) header-top)
        separator-row (if (seq tabs) (inc header-top) header-top)
        content-row  (+ header-top (if (seq tabs) 2 1))
        ;; Goal subtitle row sits BETWEEN the title content row and
        ;; the bottom rule. Only allocated when a goal exists; nil
        ;; means "no subtitle row, draw the bottom rule one row
        ;; higher". Centered like the title; never bleeds into LEFT
        ;; banner / RIGHT id columns.
        goal*        (current-goal db)
        goal-row     (when (goal-subtitle-visible? goal*)
                       (inc content-row))
        bottom-row   (dec (+ header-top (header-rows db)))
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

    ;; Optional Workspace tabs. They live in the header, but only when there is
    ;; more than one Workspace tab to choose from. Tabs occupy the top row;
    ;; the rule below them separates workspace navigation from conversation
    ;; title/id actions.
    (when (seq tabs)
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (p/fill-rect! g 0 tabs-row cols 1)
      (let [layout (p/draw-tabs! g tabs
                     {:left        edge-pad
                      :row         tabs-row
                      :width       (max 0 (- cols (* 2 edge-pad)))
                      :active-id   (active-workspace-tab-id db tabs)
                      :fg          t/header-fg
                      :bg          t/terminal-bg
                      :active-fg   t/header-hover-fg
                      :active-bg   t/terminal-bg
                      :inactive-fg t/footer-fg-muted
                      :inactive-bg t/terminal-bg})]
        (doseq [[idx {:keys [id left width]}] (map-indexed vector layout)
                :when (pos? (long width))]
          (cr/register!
            {:bounds       {:row tabs-row :col left :width width}
             :kind         :workspace-tab
             :index        idx
             :workspace-id id
             :text         id
             :enabled?     true}))))

    (draw-rule! g separator-row cols)

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
          (when full-uuid
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

    ;; Goal subtitle row (only when a goal exists). Painted between
    ;; the title row and the bottom rule. Wipes the row first so
    ;; previous-frame characters can't bleed through, then center-
    ;; renders the one-line summary in the muted footer color so it
    ;; reads as a hint, not a competing primary.
    (when goal-row
      (let [now-ms  (System/currentTimeMillis)
            summary (goal/format-goal-summary goal* now-ms)]
        (p/clear-styles! g)
        (p/set-colors! g t/footer-fg t/terminal-bg)
        (p/fill-rect! g 0 goal-row cols 1)
        (when (and (string? summary) (not (str/blank? summary)))
          (let [;; Goal-status badge color: paused = warn-yellow,
                ;; done = muted, active = same as title-fg-muted.
                fg (case (:status goal*)
                     :paused t/footer-warning-fg
                     :done   t/footer-fg-muted
                     t/footer-fg)
                trimmed (let [max-w (max 0 (- cols (* 2 edge-pad)))]
                          (ellipsize summary max-w))
                w  (p/display-width trimmed)
                col (max edge-pad (quot (- cols w) 2))]
            (p/clear-styles! g)
            (p/set-colors! g fg t/terminal-bg)
            (p/enable! g p/ITALIC)
            (p/put-str! g col goal-row trimmed)
            (p/clear-styles! g)))))

    (draw-rule! g bottom-row cols)

    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
