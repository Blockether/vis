(ns com.blockether.vis.ext.channel-tui.header
  "Dedicated header band painted above the messages area.

   Three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       (reserved, empty today)   Conversation title          d8d6a0a1 ⎘

   - CENTER: conversation title from app-db (`:title`). When the
     conversation has no title yet, falls back to a placeholder so
     the row never looks broken on a fresh run.
   - RIGHT: short conversation id (first 8 chars of the UUID, the
     same convention `vis conversations` uses, paint-friendly) +
     a clickable copy glyph that drops the FULL UUID onto the
     system clipboard. The click target covers both the id label
     and the glyph so the user has a forgiving target. After a
     successful copy the band briefly flashes `✓ Copied!` in place
     of the id (driven by `:header-flash` in app-db; written by
     the click handler in screen.clj, cleared by a timer future).
   - LEFT: intentionally empty — left as a slot for future surfaces
     (provider lineage / channel badge / branch name) without
     having to redo the layout math.

   Pure draw: reads `:title`, `:conversation`, `:header-flash` from
   app-db once, writes cells, registers ONE click region for the
   copy affordance."
  (:require [clojure.string :as str]
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

(def ^:private copy-glyph
  "Single-cell ASCII-friendly copy affordance painted right of the
   short-id. ⎘ (U+2398, NEXT PAGE / 'copy') reads as a copy/paste
   icon in most monospace fonts that ship with terminal emulators
   used in 2026; falls back gracefully to a box on terminals that
   don't render it."
  "⎘")

(def ^:private copied-flash-text
  "✓ Copied!")

(defn- short-id [conversation]
  (when-let [id (some-> conversation :id str)]
    (when (seq id)
      (subs id 0 (min id-display-chars (count id))))))

(defn- full-id [conversation]
  (some-> conversation :id str))

(defn- flash-active?
  "True when `(:header-flash db)` carries an `:until` deadline that
   hasn't passed yet. Read by the header to decide whether to paint
   the id+glyph or the flash text."
  [db]
  (when-let [{:keys [until]} (:header-flash db)]
    (and (number? until)
      (< (System/currentTimeMillis) (long until)))))

(defn- title-text [db]
  (let [t (:title db)]
    (if (and (string? t) (not (str/blank? t)))
      [t false]
      [placeholder-title true])))

(defn- draw-rule!
  "Paint a full-width single-line horizontal rule on `row` in the
   muted footer color."
  [g row cols]
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg-muted t/terminal-bg)
  (dotimes [c cols]
    (p/set-char! g c row p/BOX_H))
  (p/clear-styles! g))

(defn- right-block
  "Compose the RIGHT-side text block (id + glyph, OR the flash
   message). Returns `{:text str :width int :flash? bool}`. Single
   place that knows how the right region looks under each state
   so `draw-header!` can stay focused on layout math + paint."
  [db conversation-id-short]
  (cond
    ;; Flash overrides everything else.
    (flash-active? db)
    {:text copied-flash-text :width (count copied-flash-text) :flash? true}

    ;; Normal: "4b1ed602 ⎘" — id, single space, glyph.
    conversation-id-short
    (let [text (str conversation-id-short " " copy-glyph)]
      {:text text :width (count text) :flash? false})

    ;; No conversation id yet — nothing to paint.
    :else
    {:text "" :width 0 :flash? false}))

(defn draw-header!
  "Paint the header band starting at `header-top`, full width `cols`.
   The band is `HEADER_ROWS` rows tall: top rule, content row, bottom
   rule. Pure draw, safe to call every frame.

   Layout per the namespace doc: empty LEFT, centered CONVERSATION_TITLE,
   short conversation-id + copy glyph on the RIGHT. When the title
   would overlap the right block, the title is truncated with an
   ellipsis so the id stays readable — the id is the more diagnostic
   of the two when the user is debugging which conversation they
   actually opened.

   Side effect: registers ONE click region for the right block when
   a conversation id exists, so the screen mouse handler can copy
   the FULL UUID to clipboard on click."
  [g db header-top cols]
  (let [content-row (+ header-top 1)
        bottom-row  (+ header-top 2)
        edge-pad    1
        id-short    (short-id (:conversation db))
        full-uuid   (full-id  (:conversation db))
        {:keys [text width flash?]} (right-block db id-short)
        right-col   (when (pos? width) (max 0 (- cols edge-pad width)))
        [title placeholder?] (title-text db)
        gap         2
        title-max   (max 0 (- cols (* 2 edge-pad) width (if (pos? width) gap 0)))
        title-trim  (cond
                      (zero? title-max) ""
                      (<= (count title) title-max) title
                      :else (str (subs title 0 (max 0 (dec title-max))) "…"))
        title-w     (count title-trim)
        title-col-raw (max edge-pad (quot (- cols title-w) 2))
        title-col   (if (and right-col (> (+ title-col-raw title-w) (- right-col gap)))
                      (max edge-pad (- right-col gap title-w))
                      title-col-raw)]

    (draw-rule! g header-top cols)

    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)
    (p/fill-rect! g 0 content-row cols 1)

    (when (pos? title-w)
      (p/clear-styles! g)
      (if placeholder?
        (do (p/set-colors! g t/footer-fg-muted t/terminal-bg)
          (p/enable! g p/ITALIC))
        (do (p/set-colors! g t/footer-fg-strong t/terminal-bg)
          (p/enable! g p/BOLD)))
      (p/put-str! g title-col content-row title-trim)
      (p/clear-styles! g))

    (when (pos? width)
      (p/clear-styles! g)
      (cond
        flash?     (p/set-colors! g t/footer-fg-strong t/terminal-bg)
        :else      (p/set-colors! g t/footer-fg-muted  t/terminal-bg))
      (p/put-str! g right-col content-row text)
      (p/clear-styles! g)

      ;; Register the click region for the id+glyph block. We skip
      ;; registration during the flash because (a) the user just
      ;; clicked successfully and a double-click 1.5 s later would
      ;; be uninteresting, (b) the bounds geometry is identical so
      ;; the next paint after the flash expires re-registers it on
      ;; its own.
      (when (and (not flash?) full-uuid)
        (cr/register!
          {:bounds   {:row content-row :col right-col :width width}
           :kind     :copy-id
           :text     full-uuid
           :enabled? true})))

    (draw-rule! g bottom-row cols)

    (p/clear-styles! g)
    (p/set-colors! g t/footer-fg t/terminal-bg)))
