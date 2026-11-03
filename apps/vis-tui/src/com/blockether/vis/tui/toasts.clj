;; Toast card: the one way the session view shows a notification.
(ns com.blockether.vis.tui.toasts
  "The toast card in the top-left corner of the session view.

   Every notification shows here: the TUI's own notices and the `vis.notify`
   toasts of extensions. One card shows at a time, like the Companion card. The
   border stands on the screen paper, and the tinted body fills the card up to
   it. The border is drawn with one-eighth blocks on the cell edges, so no strip
   of paper shows between the body and the border. The border has the level
   color.

   The first row holds the bold title with its source, and the close mark `✕`
   in the top-right corner. A gray rule runs under the heading and stops before
   the `✕`. A card without a heading keeps the `✕` on its first text line. The
   body holds up to `max-text-lines` lines of wrapped text. When more notices
   wait, the page counter `← 1/3 →` is in the bottom-right corner.
   The countdown is a thick bar under the body: the level color is the time
   left over a faint track. A sticky notice has a plain bottom border.

   `layout` is pure: it gives the geometry of the card, and the painter only
   draws that geometry."
  (:require [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:const max-card-cols "The widest card, in columns." 56)

(def ^:const min-card-cols "The narrowest card. A smaller area shows no card." 28)

(def ^:const max-text-lines "The most text lines of one card; more text ends with `…`." 4)

(def ^:const ^:private side-cols
  "Columns of a body row that hold no text: two borders and two spaces."
  4)

(def ^:const ^:private close-cols
  "Columns that the close mark takes on a text row: a space and `✕`."
  2)

(defn- text-lines
  "`text` wrapped to `width` columns, at most `limit` lines. A cut ends with `…`."
  [text width limit]
  (let [limit
        (long limit)

        lines
        (into [] (remove empty?) (p/word-wrap text width))]

    (if (> (count lines) limit)
      (conj (subvec lines 0 (dec limit)) (p/ellipsize (str (nth lines (dec limit)) " …") width))
      lines)))

(defn- heading
  "The first-row text of a card: its title and its source, or nil."
  [{:keys [title source]}]
  (cond (and title source) (str title " · " source)
        :else (or title source)))

(defn- pager
  "The bottom-right page counter of a card, or nil when no other page waits."
  [index total]
  (when (> (long total) 1) (str "← " (inc (long index)) "/" total " →")))

(defn layout
  "The card of `page`, a `notifications/pager` value, at `left`,`top` of an area of
   `cols` x `rows`, or nil when no card fits. The card is
   `{:id :level :heading :title :lines :pager :fraction :col :row :width :height}`.
   `:fraction` is the part of the time that is left, nil for a sticky notice."
  [{:keys [entry index total remaining-ms]} {:keys [left top cols rows]}]
  (let [max-cols
        (min (long max-card-cols) (long cols))

        pages
        (when entry (pager index total))

        head
        (when entry
          (some-> (heading entry)
                  (p/ellipsize (- max-cols (long side-cols) (long close-cols)))
                  not-empty))

        fraction
        (let [ttl (:ttl-ms entry)]
          (when (and remaining-ms ttl (pos? (long ttl)))
            (min 1.0 (/ (double remaining-ms) (double ttl)))))

        ;; Two border rows, the heading row with its rule and the page counter row.
        line-room
        (- (long rows) 2 (if head 2 0) (if pages 1 0))]

    (when (and entry (>= max-cols (long min-card-cols)) (pos? line-room))
      (let [;; Without a heading, the close mark stands at the end of the text rows.
            text-side
            (+ (long side-cols) (if head 0 (long close-cols)))

            lines
            (text-lines (:text entry) (- max-cols text-side) (min (long max-text-lines) line-room))

            head-cols
            (if head (+ (long side-cols) (long close-cols) (long (p/display-width head))) 0)

            pager-cols
            (if pages (+ (long side-cols) (long (p/display-width pages))) 0)

            text-cols
            (+ text-side (long (reduce max 0 (map p/display-width lines))))]

        {:id (:id entry)
         :level (:level entry)
         :title (:title entry)
         :heading head
         :lines lines
         :pager pages
         :fraction fraction
         :col (long left)
         :row (long top)
         :width (min max-cols (max (long min-card-cols) head-cols pager-cols text-cols))
         :height (+ 2 (if head 2 0) (count lines) (if pages 1 0))}))))

(defn- level-fg
  "The border color of a card."
  [level]
  (case level
    :success
    t/status-ok

    :warn
    t/footer-warning-fg

    :error
    t/status-bad

    t/link-chrome-fg))

(defn- card-bg
  "The paper of a card: the screen paper moved toward the ink, so a card stands
   out from the transcript under it on a light and on a dark theme."
  []
  (t/mix-color t/dialog-bg t/text-fg 0.09))

(defn- rule [^long n ^String ch] (apply str (repeat (max 0 n) ch)))

(defn- region!
  "Register one click region of the card on `row`."
  [row col width kind id]
  (when (pos? (long width))
    (.register interactions/hit-map
               {:bounds {:row row :col col :width width} :kind kind :text id :enabled? true})))

(defn paint!
  "Draw `card` from `layout`; nil draws nothing. With `register?`, `←` is a
   `:toast-prev` click, the counter and `→` are `:toast-next`, and the rest of
   the card is `:toast-dismiss`. Each region carries the notification id as `:text`."
  [g card register?]
  (when-let [{:keys [id level heading title lines pager fraction col row width height]} card]
    (let [col (long col)
          row (long row)
          width (long width)
          inner (- width 2)
          ;; The bottom row stands on the paper and carries the bottom line.
          bottom (+ row (long height) -1)
          right (+ col width -1)
          first-row (inc row)
          text-row (if heading (+ first-row 2) first-row)
          pager-cols (long (if pager (p/display-width pager) 0))
          pager-row (dec bottom)
          ;; The counter ends one space before the right border.
          pager-col (- right 1 pager-cols)
          ;; The border stands on the screen paper; the body is tinted up to it.
          paper t/dialog-bg
          bg (card-bg)
          edge (t/legible-ink (level-fg level) paper)
          muted (t/legible-ink t/footer-fg-muted bg)]

      (p/clear-styles! g)
      (p/set-colors! g t/dialog-fg paper)
      (p/fill-rect! g col row width height)
      (p/set-colors! g t/dialog-fg bg)
      (p/fill-rect! g (inc col) first-row inner (- bottom first-row))
      ;; Border: one-eighth blocks on the cell edges around the body. A side line
      ;; is two blocks that meet on one cell edge, so it is as thick as the top line.
      (p/set-colors! g edge paper)
      (p/put-str! g (inc col) row (rule inner "▁"))
      (doseq [r (range first-row bottom)]
        (p/put-str! g col (long r) "▕")
        (p/put-str! g right (long r) "▏"))
      (p/set-colors! g edge bg)
      (doseq [r (range first-row bottom)]
        (p/put-str! g (inc col) (long r) "▏")
        (p/put-str! g (dec right) (long r) "▕"))
      ;; The countdown is a bar under the body, four times as thick as the border:
      ;; the level color is the time left over a faint track. A sticky notice keeps
      ;; a plain bottom line.
      (if fraction
        (let [left-cols (long (Math/ceil (* (double fraction) inner)))]
          (p/set-colors! g edge paper)
          (p/put-str! g (inc col) bottom (rule left-cols "▀"))
          (p/set-colors! g (t/mix-color paper edge 0.3) paper)
          (p/put-str! g (+ col 1 left-cols) bottom (rule (- inner left-cols) "▀")))
        (do (p/set-colors! g edge paper) (p/put-str! g (inc col) bottom (rule inner "▔"))))
      ;; Heading.
      (when heading
        (p/set-colors! g muted bg)
        (p/put-str! g (+ col 2) first-row heading)
        (when title
          (p/set-colors! g t/dialog-fg bg)
          (p/enable! g p/BOLD)
          (p/put-str! g (+ col 2) first-row (subs heading 0 (min (count heading) (count title))))
          (p/clear-styles! g))
        ;; A gray rule under the heading; it stops three cells before the close mark.
        (p/set-colors! g muted bg)
        (p/put-str! g (+ col 2) (inc first-row) (rule (- width 8) "▔")))
      ;; Close mark in the top-right corner, in the text color.
      (p/set-colors! g t/dialog-fg bg)
      (p/enable! g p/BOLD)
      (p/put-str! g (- right 2) first-row "✕")
      (p/clear-styles! g)
      ;; Text.
      (p/set-colors! g t/dialog-fg bg)
      (doseq [[i line] (map-indexed vector lines)]
        (p/put-str! g (+ col 2) (+ text-row (long i)) line))
      ;; Page counter in the bottom-right corner.
      (when pager (p/set-colors! g muted bg) (p/put-str! g pager-col pager-row pager))
      (p/clear-styles! g)
      (when register?
        (doseq [r (range row (+ row (long height)))]
          (if (and pager (= pager-row (long r)))
            (let [next-col (+ pager-col 2)
                  end (+ pager-col pager-cols)]

              (region! pager-row col (- pager-col col) :toast-dismiss id)
              (region! pager-row pager-col 2 :toast-prev id)
              (region! pager-row next-col (- end next-col) :toast-next id)
              (region! pager-row end (- (inc right) end) :toast-dismiss id))
            (region! (long r) col width :toast-dismiss id)))))))
