;; Toast cards: the one way the session view shows a notification.
(ns com.blockether.vis.tui.toasts
  "Toast cards in the top-left corner of the session view.

   Every notification shows here: the TUI's own notices and the `vis.notify`
   toasts of extensions. The newest card is on top. A card has a bar in the
   level color, an optional bold title with its source, and up to
   `max-text-lines` lines of wrapped text. A click on a card dismisses it.

   `layout` is pure: it gives the geometry of the cards that fit, and the
   painter only draws that geometry."
  (:require [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:const max-card-cols "The widest card, in columns." 52)

(def ^:const min-card-cols "The narrowest card. A smaller area shows no cards." 24)

(def ^:const max-text-lines "The most text lines of one card; more text ends with `…`." 4)

(def ^:const max-cards "The most cards that show at the same time." 3)

(def ^:const ^:private chrome-cols
  "Columns of a card that hold no text: the bar, one space, and the close mark with its padding."
  5)

(defn- text-lines
  "`text` wrapped to `width` columns, at most `max-text-lines`. A cut ends with `…`."
  [text width]
  (let [lines (into [] (remove empty?) (p/word-wrap text width))]
    (if (> (count lines) max-text-lines)
      (conj (subvec lines 0 (dec max-text-lines))
            (p/ellipsize (str (nth lines (dec max-text-lines)) " …") width))
      lines)))

(defn- heading
  "The bold first row of a card: its title and its source, or nil."
  [{:keys [title source]}]
  (cond (and title source) (str title " · " source)
        :else (or title source)))

(defn- card
  "The size and text of one card at most `max-cols` wide."
  [{:keys [id level text title] :as entry} ^long max-cols]
  (let [text-cols
        (- max-cols (long chrome-cols))

        head
        (some-> (heading entry)
                (p/ellipsize text-cols))

        lines
        (text-lines text text-cols)

        widest
        (reduce max (p/display-width (or head "")) (map p/display-width lines))

        width
        (min max-cols (max (long min-card-cols) (+ (long widest) (long chrome-cols))))]

    {:id id
     :level level
     :title title
     :heading head
     :lines lines
     :width width
     ;; One padding row above and below the text.
     :height (+ 2 (if head 1 0) (count lines))}))

(defn- same-width
  "`cards` with the width of the widest card, so a stack has one straight right edge."
  [cards]
  (let [widest (reduce max 0 (map :width cards))]
    (mapv #(assoc % :width widest) cards)))

(defn layout
  "The cards of `entries` (oldest first) that fit an area at `left`,`top` of
   `cols` x `rows`. The newest card is on top, one empty row between cards.
   Each card is `{:id :level :heading :title :lines :col :row :width :height}`."
  [entries {:keys [left top cols rows]}]
  (let [left
        (long left)

        top
        (long top)

        max-cols
        (min (long max-card-cols) (long cols))

        bottom
        (+ top (long rows))]

    (if (< max-cols (long min-card-cols))
      []
      (loop [pending
             (take max-cards (rseq (vec entries)))

             row
             top

             cards
             []]

        (if-let [entry (first pending)]
          (let [{:keys [height] :as one} (card entry max-cols)]
            (if (> (+ row (long height)) bottom)
              (same-width cards)
              (recur (rest pending)
                     (+ row (long height) 1)
                     (conj cards
                           (assoc one
                             :col left
                             :row row)))))
          (same-width cards))))))

(defn- level-fg
  "The bar and close-mark color of a card."
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

(defn paint!
  "Draw `cards` from `layout`. With `register?`, each card row becomes a
   `:toast-dismiss` click region that carries the notification id as `:text`."
  [g cards register?]
  (doseq [{:keys [id level heading title lines col row width height]} cards]
    (let [col (long col)
          row (long row)
          width (long width)
          height (long height)
          text-col (+ col 2)
          first-row (inc row)
          bg (card-bg)
          bar (t/legible-ink (level-fg level) bg)]

      (p/clear-styles! g)
      (p/set-colors! g t/dialog-fg bg)
      (p/fill-rect! g col row width height)
      (p/set-colors! g bar bg)
      (doseq [r (range row (+ row height))]
        (p/put-str! g col (long r) "┃"))
      (p/put-str! g (- (+ col width) 2) first-row "✕")
      ;; The heading is muted; its title part is bold. A heading cut inside the
      ;; title keeps the cut, so the bold part never runs past the card.
      (when heading
        (p/set-colors! g (t/legible-ink t/footer-fg-muted bg) bg)
        (p/put-str! g text-col first-row heading)
        (when title
          (p/set-colors! g t/dialog-fg bg)
          (p/enable! g p/BOLD)
          (p/put-str! g text-col first-row (subs heading 0 (min (count heading) (count title)))))
        (p/clear-styles! g))
      (p/clear-styles! g)
      (p/set-colors! g t/dialog-fg bg)
      (doseq [[i line] (map-indexed vector lines)]
        (p/put-str! g text-col (+ first-row (if heading 1 0) (long i)) line))
      (p/clear-styles! g)
      (when register?
        (doseq [r (range row (+ row height))]
          (.register interactions/hit-map
                     {:bounds {:row r :col col :width width}
                      :kind :toast-dismiss
                      :text id
                      :enabled? true}))))))
