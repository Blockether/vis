(ns com.blockether.vis.ext.channel-tui.header
  "Dedicated one-row header rendered above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       (reserved, empty today)   Conversation title          d8d6a0a1

   - CENTER: conversation title from app-db (`:title`). When the
     conversation has no title yet, falls back to a placeholder so
     the row never looks broken on a fresh run.
   - RIGHT: short conversation id (first 8 chars of the UUID, the
     same convention `vis conversations` uses, paint-friendly).
   - LEFT: intentionally empty — left as a slot for future surfaces
     (provider lineage / channel badge / branch name) without having
     to redo the layout math.

   The header consumes one row from the top of the screen; the
   messages area starts at row 1 instead of row 0. Footer ownership
   stays exactly as it was.

   Pure draw: reads `:title` and `:conversation` from app-db once,
   writes cells. No allocation on the hot path beyond a couple of
   short strings."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]))

(def ^:private id-display-chars
  "How many leading characters of the conversation UUID to show in
   the right region. 8 matches the prefix length already exposed by
   `vis conversations`, the `--conversation-id` short form, and the
   `format-conversation-not-found` listing in screen.clj."
  8)

(def ^:private placeholder-title
  "Shown center when the conversation has no title yet (fresh run,
   first turn not finished yet). Italicised so it reads as a hint."
  "Untitled conversation")

(defn- short-id [conversation]
  (when-let [id (some-> conversation :id str)]
    (when (seq id)
      (subs id 0 (min id-display-chars (count id))))))

(defn- title-text [db]
  (let [t (:title db)]
    (if (and (string? t) (not (str/blank? t)))
      [t false]
      [placeholder-title true])))

(defn draw-header!
  "Paint the header row at `header-row`, full width `cols`. Pure draw.
   Safe to call every frame.

   Layout per the namespace doc: empty LEFT, centered TITLE, short
   conversation-id on the RIGHT. When the title would overlap the
   id, the title is truncated with an ellipsis so the id stays
   readable — the id is the more diagnostic of the two when the
   user is debugging which conversation they actually opened."
  [g db header-row cols]
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg t/terminal-bg)
  (p/fill-rect! g 0 header-row cols 1)

  (let [edge-pad   1
        id         (short-id (:conversation db))
        id-w       (if id (count id) 0)
        right-col  (when id (max 0 (- cols edge-pad id-w)))
        [title placeholder?] (title-text db)
        gap        2
        title-max  (max 0 (- cols (* 2 edge-pad) id-w (if id gap 0)))
        title-trim (cond
                     (zero? title-max) ""
                     (<= (count title) title-max) title
                     :else (str (subs title 0 (max 0 (dec title-max))) "…"))
        title-w    (count title-trim)
        title-col  (max edge-pad (quot (- cols title-w) 2))
        title-col  (if (and id (> (+ title-col title-w) (- right-col gap)))
                     (max edge-pad (- right-col gap title-w))
                     title-col)]

    (when (pos? title-w)
      (p/clear-styles! g)
      (if placeholder?
        (do (p/set-colors! g t/footer-fg-muted t/terminal-bg)
          (p/enable! g p/ITALIC))
        (do (p/set-colors! g t/footer-fg-strong t/terminal-bg)
          (p/enable! g p/BOLD)))
      (p/put-str! g title-col header-row title-trim)
      (p/clear-styles! g))

    (when (and id (pos? id-w))
      (p/clear-styles! g)
      (p/set-colors! g t/footer-fg-muted t/terminal-bg)
      (p/put-str! g right-col header-row id)
      (p/clear-styles! g)))

  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg t/terminal-bg))
