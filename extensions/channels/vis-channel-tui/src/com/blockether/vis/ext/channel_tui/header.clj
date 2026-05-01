(ns com.blockether.vis.ext.channel-tui.header
  "Dedicated header band painted above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       ✓ Copied!                 Conversation title          d8d6a0a1 ⧉ Copy
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
     `⧉ Copy` affordance that drops the FULL UUID onto the system
     clipboard. The click target covers the id label and the
     whole `⧉ Copy` label so the user has a forgiving target. Visual
     feedback is the LEFT-slot `✓ Copied!` notification — same
     mechanism every other cross-channel signal flows through.

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
  "Shared copy glyph used by both the per-message bubble control and
   the conversation-id affordance in the header. ASCII-adjacent,
   compact, and already liked by the user in the bubble UI."
  "⧉")

(def ^:private copy-label
  "Visible text after the copy glyph so the affordance reads as an
   actual button, not an unlabeled ornament."
  "Copy")

(def ^:private copy-affordance
  "Full header affordance painted right of the short conversation id."
  (str copy-icon " " copy-label))

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

(defn- latest-notification
  "Most-recently-pushed active notification, or nil. We display ONE
   at a time in the header — the LEFT slot is a single row. If
   multiple are active simultaneously, the freshest wins; older ones
   stay in the queue and surface as the freshest one expires."
  []
  (last (vis/notifications)))

(defn- level->fg
  "Map a notification level to a foreground color. Falls back to the
   muted-footer color so an unknown level still renders something."
  [level]
  (case level
    :success t/footer-fg-strong
    :warn    t/footer-fg-strong
    :error   t/footer-fg-strong
    :info    t/footer-fg
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

(defn- right-block-text
  "Compose the right-side text: \"4b1ed602 ⧉ Copy\" when a conversation
   id exists, otherwise empty. Single place that knows the layout
   so `draw-header!` can stay focused on placement math."
  [id-short]
  (if id-short
    (str id-short " " copy-affordance)
    ""))

(defn draw-header!
  "Paint the header band starting at `header-top`, full width `cols`.
   The band is `HEADER_ROWS` rows tall: top rule, content row, bottom
   rule. Pure draw + ONE click-region registration; safe to call
   every frame.

   Layout per the namespace doc: notification banner LEFT, centered
   conversation title CENTER, short conversation id + `⧉ Copy`
   RIGHT. When the title would overlap either edge block, the title
   is truncated with an ellipsis so the diagnostically-important id
   stays readable.

   Side effect: registers ONE click region for the right-block
   covering both the id label AND the `⧉ Copy` affordance, so the
   click target is forgiving. The screen mouse handler (in
   `screen.clj`) recognises `:kind :copy-id` and drops the FULL
   UUID onto the system clipboard, then pushes a host notification
   that this band surfaces in the LEFT slot."
  [g db header-top cols]
  (let [content-row  (+ header-top 1)
        bottom-row   (+ header-top 2)
        edge-pad     1
        id-short     (short-id (:conversation db))
        full-uuid    (full-id  (:conversation db))
        right-text   (right-block-text id-short)
        right-w      (p/display-width right-text)
        right-col    (when (pos? right-w) (max 0 (- cols edge-pad right-w)))
        notif        (latest-notification)
        notif-text   (some-> notif :text)
        notif-level  (some-> notif :level)
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
        [title placeholder?] (title-text db)
        title-max    (max 0 (- cols (* 2 edge-pad) right-w left-w
                              (if (pos? right-w) gap 0)
                              (if (pos? left-w) gap 0)))
        title-trim   (cond
                       (zero? title-max)            ""
                       (<= (count title) title-max) title
                       :else (str (subs title 0 (max 0 (dec title-max))) "…"))
        title-w      (count title-trim)
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

    ;; CENTER — title or placeholder.
    (when (pos? title-w)
      (p/clear-styles! g)
      (if placeholder?
        (do (p/set-colors! g t/footer-fg-muted t/terminal-bg)
          (p/enable! g p/ITALIC))
        (do (p/set-colors! g t/footer-fg-strong t/terminal-bg)
          (p/enable! g p/BOLD)))
      (p/put-str! g title-col content-row title-trim)
      (p/clear-styles! g))

    ;; RIGHT — id + copy affordance + click region.
    ;; Visual hover feedback: when the copy region is hovered the
    ;; affordance brightens and gains BOLD so it's obvious it's
    ;; clickable. Terminal emulators don't allow applications to
    ;; control the mouse cursor shape, so this is the strongest
    ;; affordance we can offer.
    (when (pos? right-w)
      (let [hovered-region (cr/hovered)
            copy-hovered?  (and hovered-region
                             (= :copy-id (:kind hovered-region))
                             (= content-row (get-in hovered-region [:bounds :row])))]
        (p/clear-styles! g)
        (if copy-hovered?
          (do (p/set-colors! g t/footer-fg-strong t/terminal-bg)
            (p/enable! g p/BOLD))
          (p/set-colors! g t/footer-fg-muted t/terminal-bg))
        (p/put-str! g right-col content-row right-text)
        (p/clear-styles! g)
        (when full-uuid
          (cr/register!
            {:bounds   {:row content-row :col right-col :width right-w}
             :kind     :copy-id
             :text     full-uuid
             :enabled? true}))))

    (draw-rule! g bottom-row cols)

    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
