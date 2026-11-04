;; Toast card: the one way the session view shows a notification.
(ns com.blockether.vis.tui.toasts
  "The toast card in the top-left corner of the session view.

   Every notification shows here: the TUI's own notices and the `vis.notify`
   toasts of extensions. Each lane shows one card at a time, like the Companion card.
   The card of the global lane stands in the top-left corner of the window. The card of
   the session lane stands in the top-left corner of the transcript (`cards`). The
   tinted body fills the whole card. The border is a rounded box line in the
   level color. A terminal joins box lines across rows, so the sides stay whole.
   One empty column stands between each side line and the text.

   One empty row stands under the top border, and one over the bottom border.
   The close mark `✕` is in the top-right corner of the empty top row. Under it,
   the heading holds the bold title with its source. A gray rule runs under the
   heading, exactly as wide as the heading text. The body holds up to
   `max-text-lines` lines of wrapped text. When more notices wait, the page
   counter `1/3` is in the bottom-right corner, on the row over the bottom border.
   A click on the counter shows the next notice.
   The countdown is a thick bar on the bottom border, as long as the time left.
   A sticky notice has a plain bottom border.

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
  "Columns of a body row that hold no text: two border cells and two empty columns."
  4)

(def ^:const ^:private close-cols
  "Columns that the close mark keeps free on the heading row: a space and `✕`."
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
  (when (> (long total) 1) (str (inc (long index)) "/" total)))

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

        ;; Two border rows, the empty top and bottom rows, the heading row with its
        ;; rule and the page counter row.
        line-room
        (- (long rows) 4 (if head 2 0) (if pages 1 0))]

    (when (and entry (>= max-cols (long min-card-cols)) (pos? line-room))
      (let [;; The close mark stands in the empty top row, over the text.
            text-side
            (long side-cols)

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
         :height (+ 4 (if head 2 0) (count lines) (if pages 1 0))}))))

(def ^:const ^:private card-margin "Empty columns between the left edge and a card." 2)

(def ^:const ^:private card-gap "Empty rows between the global card and the session card." 1)

(defn cards
  "The cards of `pages`, a `notifications/show!` value, as `{:global :session}`. The global
   card has window columns and stands in the top-left corner of the window. The session
   card has pane columns and stands in the top-left corner of the transcript. Both keep
   `card-margin` columns from the left edge. When the global card covers the place of the
   session card, the session card moves under it, with `card-gap` empty rows between
   them. `chat-left` is the window column of the pane."
  [{:keys [global session]} {:keys [screen-cols rows chat-left cols text-top messages-bottom]}]
  (let [global-card
        (layout global
                {:left card-margin
                 :top 0
                 :cols (- (long screen-cols) (* 2 (long card-margin)))
                 :rows rows})

        covered?
        (and global-card
             (< (+ (long chat-left) (long card-margin))
                (+ (long (:col global-card)) (long (:width global-card)))))

        top
        (if covered?
          (max (long text-top)
               (+ (long (:row global-card)) (long (:height global-card)) (long card-gap)))
          (long text-top))]

    {:global global-card
     :session (layout session
                      {:left card-margin
                       :top top
                       :cols (- (long cols) (* 2 (long card-margin)))
                       :rows (- (long messages-bottom) top 1)})}))

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

(defn click-regions
  "The click regions of `card` from `layout`, each `{:row :col :width :kind}`.
   The close mark is `:toast-dismiss`, and the page counter is `:toast-next`. A
   click on the rest of the card goes to the transcript under it, so a card never takes a click or a drag from the text that it covers."
  [{:keys [pager col row width height]}]
  (let [right
        (+ (long col) (long width) -1)

        pager-cols
        (long (if pager (p/display-width pager) 0))

        pager-row
        (+ (long row) (long height) -2)

        pager-col
        (- right 1 pager-cols)]

    (cond-> [{:row (inc (long row)) :col (- right 3) :width 3 :kind :toast-dismiss}]
      pager
      (conj {:row pager-row :col pager-col :width pager-cols :kind :toast-next}))))

(defn paint!
  "Draw `card` from `layout`; nil draws nothing. With `register?`, register the
   `click-regions` of the card, moved `shift` columns. Each region carries the
   notification id as `:text`."
  ([g card register?] (paint! g card register? 0))
  ([g card register? shift]
   (when-let [{:keys [id level heading title lines pager fraction col row width height]} card]
     (let [col (long col)
           row (long row)
           width (long width)
           inner (- width 2)
           ;; The bottom border row also carries the countdown bar.
           bottom (+ row (long height) -1)
           right (+ col width -1)
           first-row (inc row)
           ;; One empty row under the top border.
           head-row (inc first-row)
           text-row (if heading (+ head-row 2) head-row)
           pager-cols (long (if pager (p/display-width pager) 0))
           ;; The counter stands on the row over the bottom border.
           pager-row (dec bottom)
           ;; The counter ends one space before the right border.
           pager-col (- right 1 pager-cols)
           bg (card-bg)
           edge (t/legible-ink (level-fg level) bg)
           muted (t/legible-ink t/footer-fg-muted bg)]

       (p/clear-styles! g)
       (p/set-colors! g t/dialog-fg bg)
       (p/fill-rect! g col row width height)
       ;; Border: box lines in the middle of the edge cells. One-eighth blocks on the
       ;; cell edges left gaps between rows in macOS Terminal, so the sides looked broken.
       (p/set-colors! g edge bg)
       (p/put-str! g col row (str "╭" (rule inner "─") "╮"))
       (doseq [r (range first-row bottom)]
         (p/put-str! g col (long r) "│")
         (p/put-str! g right (long r) "│"))
       (p/put-str! g col bottom (str "╰" (rule inner "─") "╯"))
       ;; The countdown is a thick bar on the bottom line, as long as the time left.
       (when fraction
         (p/put-str! g (inc col) bottom (rule (long (Math/ceil (* (double fraction) inner))) "▀")))
       ;; Heading.
       (when heading
         (p/set-colors! g muted bg)
         (p/put-str! g (+ col 2) head-row heading)
         (when title
           (p/set-colors! g t/dialog-fg bg)
           (p/enable! g p/BOLD)
           (p/put-str! g (+ col 2) head-row (subs heading 0 (min (count heading) (count title))))
           (p/clear-styles! g))
         ;; A gray rule under the heading, exactly as wide as the heading. `layout` cuts
         ;; the heading before the close mark, so the rule never reaches it.
         (p/set-colors! g muted bg)
         (p/put-str! g (+ col 2) (inc head-row) (rule (p/display-width heading) "▔")))
       ;; Close mark in the top-right corner, in the empty row over the heading.
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
         (doseq [{:keys [row col width kind]} (click-regions card)]
           (region! row (+ (long col) (long shift)) width kind id)))))))
