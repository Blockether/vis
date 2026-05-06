(ns com.blockether.vis.ext.channel-tui.header
  "Dedicated header band painted above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       ✓ Copied!                 Conversation title          ⧉ d8d6a0a1 | ⧉ Transcript
       (notification banner)     (or fallback placeholder)   (id + click target)

   - LEFT: latest active host notification (`com.blockether.vis.core/notify!`).
     Color-coded by `:level` (success / info / warn / error). Empty
     when no notification is active. The host notifications module
     is the single source of truth for cross-channel ephemeral
     signals — any extension or channel can `(v/notify! …)` and the
     banner surfaces here.
   - CENTER: conversation title from app-db (`:title`). When the
     conversation has no title yet, falls back to a placeholder so
     the row never looks broken on a fresh run.
   - RIGHT: short conversation id (first 8 chars of the UUID, the
     same convention `vis conversations` uses) + a clickable
     `⧉` affordance that drops the FULL UUID onto the system
     clipboard, followed by `| ⧉ Transcript` for whole-conversation
     Markdown copy. Visual feedback is the LEFT-slot `✓ Copied!` notification
     — same mechanism every other cross-channel signal flows through.

   Pure draw: reads `:title` and `:conversation` from app-db, the
   active notifications list from `vis.core/notifications`, writes
   cells, registers ONE click region for the copy affordance.

   Repaint: the banner updates as notifications come and go.
   `screen.clj` registers a watcher on screen mount that bumps the
   render version for any change, so a `(notify! …)` from anywhere
   nudges this band to repaint immediately."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]))

(def ^:private id-display-chars
  "How many leading characters of the conversation UUID to show in
   the right region. 8 matches the prefix length already exposed by
   `vis conversations`, the `--conversation-id` short form, and the
   `format-conversation-not-found` listing in screen.clj."
  8)

(def ^:const HEADER_ROWS
  "Total rows reserved by the header band: top rule + content + bottom
   rule. Single source of truth; `screen.clj` derives `messages-top`
   from this so adding a fourth row later (e.g. breadcrumb) is a
   one-line change here."
  3)

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
      :else (str (subs text 0 (max 0 (dec max-chars))) "…"))))

(defn- latest-notification
  "Most-recently-pushed active notification, or nil. We display ONE
   at a time in the header — the LEFT slot is a single row. If
   multiple are active simultaneously, the freshest wins; older ones
   stay in the queue and surface as the freshest one expires."
  []
  (last (vis/notifications)))

(defn- latest-channel-status
  [{:keys [channel-status]}]
  (->> (vals channel-status)
    (filter #(seq (:text %)))
    (sort-by #(long (or (:updated-at-ms %) 0)))
    last))

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
  (let [content-row  (+ header-top 1)
        bottom-row   (+ header-top 2)
        edge-pad     1
        id-short     (short-id (:conversation db))
        full-uuid    (full-id  (:conversation db))
        id-copy-text (id-copy-block-text id-short)
        md-copy-text (markdown-copy-block-text id-short)
        right-text   (right-block-text id-short)
        right-w      (p/display-width right-text)
        id-copy-w    (p/display-width id-copy-text)
        md-copy-w    (p/display-width md-copy-text)
        right-col    (when (pos? right-w) (max 0 (- cols edge-pad right-w)))
        separator-w  (p/display-width right-block-separator)
        md-copy-col  (when (and right-col (pos? md-copy-w))
                       (+ right-col id-copy-w separator-w))
        banner       (or (latest-channel-status db) (latest-notification))
        notif-text   (some-> banner :text)
        notif-level  (some-> banner :level)
        ;; Reserve up to 1/3 of the row for the LEFT banner so the
        ;; centered title still has room. Banner truncates with an
        ;; ellipsis if the notification text is longer.
        left-cap     (max 0 (quot cols 3))
        left-trim    (when notif-text
                       (cond
                         (zero? left-cap)              ""
                         (<= (count notif-text) left-cap) notif-text
                         :else (str (subs notif-text 0 (max 0 (dec left-cap))) "…")))
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

    (draw-rule! g header-top cols)

    ;; Wipe content row to terminal-bg first so the previous frame's
    ;; characters can't bleed through the gaps between LEFT/CENTER/RIGHT.
    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)
    (p/fill-rect! g 0 content-row cols 1)

    ;; LEFT — latest notification banner.
    (when (pos? left-w)
      (p/clear-styles! g)
      (p/set-colors! g (level->fg notif-level) t/terminal-bg)
      (p/enable! g p/BOLD)
      (p/put-str! g edge-pad content-row left-trim)
      (p/clear-styles! g))

    ;; CENTER — current conversation title only. Conversation switching lives
    ;; in the Alt+Shift+↑/↓ picker, not as persistent header tabs.
    (when (pos? title-w)
      (p/clear-styles! g)
      (p/set-colors! g t/header-fg t/terminal-bg)
      (if placeholder?
        (p/enable! g p/ITALIC)
        (p/enable! g p/BOLD))
      (p/put-str! g title-col content-row title-trim)
      (p/clear-styles! g))

    ;; RIGHT — copy conversation ID block + Markdown transcript export label.
    ;; Visual hover feedback: when either clickable region is hovered,
    ;; that exact affordance shifts to header-hover-fg and gains BOLD. Terminal emulators
    ;; don't allow applications to control the mouse cursor shape, so this
    ;; is the strongest affordance we can offer.
    (when (pos? right-w)
      (let [hovered-region   (cr/hovered)
            hovered-kind     (:kind hovered-region)
            hovered-row?     (= content-row (get-in hovered-region [:bounds :row]))
            id-hovered?      (and hovered-row? (= :copy-id hovered-kind))
            markdown-hovered? (and hovered-row? (= :copy-as-markdown hovered-kind))]
        (p/clear-styles! g)
        (p/set-colors! g (if id-hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
        (when id-hovered?
          (p/enable! g p/BOLD))
        (p/put-str! g right-col content-row id-copy-text)
        (p/clear-styles! g)
        (p/set-colors! g t/header-fg t/terminal-bg)
        (p/put-str! g (+ right-col id-copy-w) content-row right-block-separator)
        (p/clear-styles! g)
        (p/set-colors! g (if markdown-hovered? t/header-hover-fg t/header-fg) t/terminal-bg)
        (when markdown-hovered?
          (p/enable! g p/BOLD))
        (p/put-str! g md-copy-col content-row md-copy-text)
        (p/clear-styles! g)
        (when full-uuid
          (cr/register!
            {:bounds   {:row content-row :col right-col :width id-copy-w}
             :kind     :copy-id
             :text     full-uuid
             :enabled? true})
          (cr/register!
            {:bounds   {:row content-row :col md-copy-col :width md-copy-w}
             :kind     :copy-as-markdown
             :text     full-uuid
             :enabled? true}))))

    (draw-rule! g bottom-row cols)

    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
