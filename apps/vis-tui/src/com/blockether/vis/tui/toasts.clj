;; Toast card: the one way the session view shows a notification.
(ns com.blockether.vis.tui.toasts
  "The toast card in the top-left corner of the session view.

   Every notification shows here: the TUI's own notices and the `vis.notify`
   toasts of extensions. One card shows at a time. Its border has the level
   color and stands on the screen paper, around the tinted card. The top border
   holds the bold title with its source, and the close mark `✕` in the top-right
   corner. The bottom border is the countdown: the bright part is the time left,
   and the faded part the time used. When more notices wait, the page counter
   `← 1/3 →` is in the bottom-right corner. The body holds up to
   `max-text-lines` lines of wrapped text.

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
  "The top-border text of a card: its title and its source, or nil."
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

        line-room
        (- (long rows) 2)]

    (when (and entry (>= max-cols (long min-card-cols)) (pos? line-room))
      (let [pages
            (pager index total)

            ;; "╰" countdown " " pager " ─╯", with at least one countdown column.
            bottom-cols
            (if pages (+ 6 (long (p/display-width pages))) 0)

            ;; "╭─ " heading " " fill " ✕ ─╮"
            head
            (some-> (heading entry)
                    (p/ellipsize (- max-cols 10))
                    not-empty)

            lines
            (text-lines (:text entry)
                        (- max-cols (long side-cols))
                        (min (long max-text-lines) line-room))

            top-cols
            (if head (+ 10 (long (p/display-width head))) 8)

            text-cols
            (+ (long side-cols) (long (reduce max 0 (map p/display-width lines))))

            ttl
            (:ttl-ms entry)]

        {:id (:id entry)
         :level (:level entry)
         :title (:title entry)
         :heading head
         :lines lines
         :pager pages
         :fraction (when (and remaining-ms ttl (pos? (long ttl)))
                     (min 1.0 (/ (double remaining-ms) (double ttl))))
         :col (long left)
         :row (long top)
         :width (min max-cols (max (long min-card-cols) top-cols bottom-cols text-cols))
         :height (+ 2 (count lines))}))))

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
          height (long height)
          inner (- width 2)
          bottom (+ row height -1)
          right (+ col width -1)
          close-col (- right 3)
          pager-cols (long (if pager (p/display-width pager) 0))
          ;; "╰" countdown " " pager " ─╯"
          pager-col (- right 2 pager-cols)
          bar-cols (if pager (- pager-col col 2) inner)
          ;; The border stands on the screen paper, around the tinted card.
          paper t/dialog-bg
          bg (card-bg)
          ink (t/legible-ink (level-fg level) paper)
          muted (t/legible-ink t/footer-fg-muted paper)]

      (p/clear-styles! g)
      (p/set-colors! g t/dialog-fg paper)
      (p/fill-rect! g col row width height)
      (p/set-colors! g t/dialog-fg bg)
      (p/fill-rect! g (inc col) (inc row) inner (- height 2))
      ;; Border.
      (p/set-colors! g ink paper)
      (p/put-str! g col row (str "╭" (rule inner "─") "╮"))
      (doseq [r (range (inc row) bottom)]
        (p/put-str! g col (long r) "│")
        (p/put-str! g right (long r) "│"))
      ;; Countdown: the bright part is the time left.
      (p/put-str! g col bottom "╰")
      (p/put-str! g right bottom "╯")
      (p/put-str! g (inc col) bottom (rule inner "─"))
      (when fraction
        (let [left-cols (long (Math/ceil (* (double fraction) bar-cols)))]
          (p/set-colors! g (t/mix-color paper ink 0.3) paper)
          (p/put-str! g (+ col 1 left-cols) bottom (rule (- bar-cols left-cols) "─"))))
      ;; Page counter in the bottom-right corner.
      (when pager
        (p/set-colors! g muted paper)
        (p/put-str! g (dec pager-col) bottom (str " " pager " ")))
      ;; Heading on the top border.
      (when heading
        (p/set-colors! g muted paper)
        (p/put-str! g (+ col 2) row (str " " heading " "))
        (when title
          (p/set-colors! g t/dialog-fg paper)
          (p/enable! g p/BOLD)
          (p/put-str! g (+ col 3) row (subs heading 0 (min (count heading) (count title))))
          (p/clear-styles! g)))
      ;; Close mark in the top-right corner.
      (p/set-colors! g ink paper)
      (p/put-str! g (dec close-col) row " ")
      (p/enable! g p/BOLD)
      (p/put-str! g close-col row "✕")
      (p/clear-styles! g)
      (p/set-colors! g ink paper)
      (p/put-str! g (inc close-col) row " ")
      ;; Text.
      (p/set-colors! g t/dialog-fg bg)
      (doseq [[i line] (map-indexed vector lines)]
        (p/put-str! g (+ col 2) (+ row 1 (long i)) line))
      (p/clear-styles! g)
      (when register?
        (doseq [r (range row bottom)]
          (region! (long r) col width :toast-dismiss id))
        (if pager
          (let [next-col (+ pager-col 2)]
            (region! bottom col (- pager-col col) :toast-dismiss id)
            (region! bottom pager-col 2 :toast-prev id)
            (region! bottom next-col (- (+ pager-col pager-cols) next-col) :toast-next id)
            (region! bottom
                     (+ pager-col pager-cols)
                     (- (inc right) pager-col pager-cols)
                     :toast-dismiss
                     id))
          (region! bottom col width :toast-dismiss id))))))
