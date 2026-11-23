(ns com.blockether.vis.tui.queue-tray
  "The Queued tray above the prompt: the messages that wait for the running turn, in a frame
   under the agent dock. A band names the queue and its controls; each row shows its number, its
   words, `next iter` when it is marked and the button that sends it now or takes the mark back.
   `lines` is plain data, so `height` reads no time; `draw!` paints it and registers the buttons."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna.graphics TextGraphics]))

(def ^:private ^:const wide-width
  "Above these content cells a row shows `next iter` in its own column."
  70)

(def ^:private send-label "The button of a row that sends it at the next step." "Send it now")

(def ^:private unsend-label "The button of a marked row: it goes back to the turn end." "Unsend")

(def ^:private marked-label "The state of a row that is marked for the next step." "next iter")

(defn- button-text
  "The text of a row button with `label` and `chord`."
  [label chord]
  (str " " label (when chord (str " (" chord ")")) " "))

(def ^:private button-width
  "Cells of the button column: the widest button, so a press never moves a column."
  (long (p/display-width (button-text send-label "C-x q 9"))))

(defn- content-width ^long [cols] (max 1 (- (long cols) 6)))

(defn lines
  "The tray lines for `queue` at `cols` columns: `{:kind}` maps from a blank line to the bottom
   border. `queue` is `{:pending :paused :focus :running? :session-id}`: the queued rows, the
   pause of the gateway, the C-x q e focus and whether a turn runs, so a row can go at its next
   step. Each row that the running turn can take has a button. The band offers `send all now`
   from two markable rows while one is still unmarked. In focus the buttons go, the selected row
   is reversed and a marked row shows a check. Under the agent team frame (`under-team?`) the tray
   touches it; else a blank line stands above it. Empty without a queued row."
  [{:keys [pending paused focus running? session-id under-team?]} cols]
  (let [queued
        (vec pending)

        running?
        (and running? (not focus))

        markable
        (if running? (filterv render/markable-queued-row? queued) [])

        buttons?
        (boolean (seq markable))

        rows
        (vec (map-indexed
               (fn [i entry]
                 (let [markable? (and running? (render/markable-queued-row? entry))
                       marked? (and markable? (render/marked-queued-row? entry))]
                   {:kind :row
                    :n (inc (long i))
                    :text (cond->> (render/queued-preview (or (:preview-text entry) (:text entry)))
                            (:unsent? entry) (str "unsent: "))
                    :buttons? buttons?
                    :wide? (> (long (content-width cols)) (long wide-width))
                    :selected? (and focus (= i (:index focus)))
                    :checked? (contains? (:marked focus) (render/queued-row-key entry))
                    :send (when markable?
                            {:session-id session-id
                             :turn-id (:turn-id entry)
                             :marked? marked?
                             :chord (keymap/queue-row-chord (inc (long i)))
                             ;; The mode a press SETS: a marked row goes back to the turn end.
                             :deliver (if marked?
                                        gateway-contract/default-queued-turn-deliver
                                        gateway-contract/queued-turn-deliver-next-iteration)})}))
               queued))]

    (if (empty? queued)
      []
      (-> (if under-team? [] [{:kind :gap}])
          (conj {:kind :top}
                {:kind :band
                 :title (str "QUEUED · "
                             (count queued)
                             (when paused
                               (if (:is-breaker-open paused) " · provider unhealthy" " · paused")))
                 :send-all (when (and (>= (count markable) 2) (some #(not (render/marked-queued-row? %)) markable))
                             {:session-id session-id})
                 :edit? (not focus)
                 :buttons? buttons?})
          (into rows)
          (cond->
            paused
            ;; A breaker-open hold resumes when the provider recovers. A turn-failure hold does not:
            ;; the failed prompt is back in the composer, and a new send moves the queue on.
            (conj {:kind :note
                   :text (str "held — "
                              (str/replace (str (or (:reason paused) "provider unhealthy")) #"_" " ")
                              (if (:is-breaker-open paused)
                                " · resumes automatically"
                                " · send a message to continue"))})
            focus
            (conj {:kind :note :text (render/queue-hint-text focus)}))
          (conj {:kind :bottom})))))

(defn height
  "Rows that the tray of `queue` takes at `cols` columns, 0 without a queued row."
  ^long [queue cols]
  (count (lines queue cols)))

(defn- rule-col
  "The frame column of the rule before the button column of a frame `iw` content cells wide."
  ^long [iw]
  (- (+ (long iw) 1) (long button-width)))

(defn band-segments
  "The painted parts of band `line` at `iw` content cells: `[col text bold? control]` vectors from
   the left border. `control` is `:send-all` or `:edit` for a control part. The title stands at
   the left. `send all now` ends before the rule over the button column, and `edit` ends at the
   right edge. A short band keeps fewer words of `send all now`, then only its chord."
  [{:keys [title send-all edit? buttons?]} iw]
  (let [iw
        (long iw)

        title-end
        (+ 2 (long (p/display-width title)))

        rule
        (rule-col iw)

        chord
        (str " (" (keymap/label-for :queue-send-all-now) ")")

        room
        (- rule 1 title-end 2)

        ;; `[[text bold?] ...]` parts; the last word that fits wins, then only the chord.
        send
        (when send-all
          (some (fn [parts]
                  (when (<= (reduce + 0 (map #(long (p/display-width (first %))) parts)) room) parts))
                (concat (map (fn [label] [[label true] [(str chord " ") false]])
                             ["send all now" "all now" "all"])
                        [[[(str (subs chord 1) " ") false]]])))

        edit
        (str " (" (keymap/label-for :queue-edit) ")")]

    (cond-> [[2 title true]]
      send
      (into (first (reduce (fn [[segs col] [text bold?]]
                             [(conj segs [col text bold? :send-all])
                              (+ (long col) (long (p/display-width text)))])
                           [[] (- rule 1 (reduce + 0 (map #(long (p/display-width (first %))) send)))]
                           send)))
      (and buttons? send-all)
      (conj [rule "│" false nil])
      edit?
      (into (let [col (- (+ iw 2) (long (p/display-width (str "edit" edit))))]
              [[col "edit" true :edit] [(+ col 4) edit false :edit]])))))

(defn row-cells
  "The cells of tray row `row` at `iw` content cells: `[width parts align]` vectors. A part is
   `[text ink bold? bg]`. The number, the words, `next iter` on a wide tray and the button,
   right-aligned in its column so it touches the right border."
  [{:keys [n text buttons? wide? send checked?]} iw]
  (let [state?
        (and buttons? wide?)

        text-w
        (max 1
             (- (+ (long iw) (if buttons? 1 0))
                2
                3
                (if buttons? (+ 3 (long button-width)) 0)
                (if state? (+ 3 (count marked-label)) 0)))

        words
        (str (when checked? "✓ ") text)]

    (cond-> [[2 [[(str "#" n) :accent true]]] [text-w [[(p/ellipsize words text-w) :fg false]]]]
      state?
      (conj [(count marked-label) [[(if (:marked? send) marked-label "") :accent false]]])
      buttons?
      (conj [button-width
             (if send
               [[(button-text (if (:marked? send) unsend-label send-label) (:chord send))
                 (if (:marked? send) :primary :secondary)
                 true]]
               [])
             :right]))))

(defn- cell-segments
  "Segments `[col text ink bold?]` of `cells` from frame column 2, with a rule between two cells."
  [cells]
  (first (reduce (fn [[segs col] [i [width parts align]]]
                   (let [used (reduce + 0 (map #(long (p/display-width (first %))) parts))
                         start (if (= :right align) (+ (long col) (- (long width) used)) col)
                         [segs] (reduce (fn [[segs col] [text ink bold?]]
                                          [(conj segs [col text ink bold?])
                                           (+ (long col) (long (p/display-width text)))])
                                        [segs start]
                                        parts)
                         end (+ (long col) (long width))]
                     [(cond-> segs (< (long i) (dec (count cells))) (conj [(inc end) "│" :edge false]))
                      (+ end 3)]))
                 [[] 2]
                 (map-indexed vector cells))))

(defn line-text
  "Plain text of tray line `line` at `cols` columns, from the left border of the frame."
  [line cols]
  (let [iw (content-width cols)
        place (fn [segs]
                (str/trimr (reduce (fn [s [col text]]
                                     (let [col (long col)]
                                       (str s (apply str (repeat (max 0 (- col (count s))) \space)) text)))
                                   ""
                                   (sort-by first segs))))
        edge (fn [l r] (str l (apply str (repeat (+ iw 2) "─")) r))
        framed (fn [segs] (place (concat [[0 "│"]] segs [[(+ iw 3) "│"]])))]
    (case (:kind line)
      :gap ""
      :top (edge "┌" "┐")
      :bottom (edge "└" "┘")
      :band (framed (band-segments line iw))
      :row (framed (cell-segments (row-cells line iw)))
      :note (framed [[2 (p/ellipsize (:text line) iw)]]))))

(defn text
  "Plain tray lines of `queue` at `cols` columns, as the painter shows them."
  [queue cols]
  (mapv #(line-text % cols) (lines queue cols)))

(defn- ink
  "The ink of tray ink key `k`."
  [k]
  (case k
    :edge t/dialog-border
    :accent t/header-active-tab-accent
    :fg t/code-block-fg
    :primary t/terminal-bg
    :secondary t/code-block-fg
    t/dialog-hint))

(defn- button-bg
  "The paper of a button: the accent for a marked row, else a raised paper."
  [k]
  (if (= :primary k) t/header-active-tab-accent (t/zebra-bg (t/zebra-bg t/terminal-bg))))

(defn- hovered?
  "Whether the control of `kind` on screen row `y` is under the pointer."
  [kind y turn-id]
  (let [hovered (.hovered interactions/hit-map)]
    (and (= kind (:kind hovered))
         (= y (:row (:bounds hovered)))
         (or (nil? turn-id) (= (str turn-id) (str (:turn-id hovered)))))))

(defn- put!
  [^TextGraphics g x y text fg bg bold?]
  (p/set-colors! g fg bg)
  (if bold? (p/styled g [p/BOLD] (p/put-str! g x y text)) (p/put-str! g x y text)))

(defn- underline!
  [^TextGraphics g x y width]
  (dotimes [i width] (p/underline-cell! g (+ (long x) (long i)) y t/link-chrome-hover-fg)))

(defn- region!
  [kind y col width extra]
  (.register interactions/hit-map (merge {:bounds {:row y :col col :width width} :kind kind} extra)))

(defn- draw-line!
  "Paint tray line `line` on screen row `y`, with the frame from screen column `left`."
  [^TextGraphics g line left y iw]
  (let [left (long left)
        iw (long iw)
        bg t/terminal-bg
        border! (fn [] (put! g left y "│" t/dialog-border bg false)
                  (put! g (+ left iw 3) y "│" t/dialog-border bg false))
        edge! (fn [l r] (put! g left y (str l (apply str (repeat (+ iw 2) "─")) r) t/dialog-border bg false))]
    (case (:kind line)
      :gap nil
      :top (edge! "┌" "┐")
      :bottom (edge! "└" "┘")
      :band
      (let [band t/dialog-title-bg
            send-all (:send-all line)]
        (p/set-colors! g t/dialog-title-fg band)
        (p/fill-rect! g (inc left) y (+ iw 2) 1)
        (border!)
        (doseq [[control parts] (group-by #(get % 3) (band-segments line iw))
                :let [from (+ left (long (ffirst parts)))
                      width (reduce + 0 (map #(long (p/display-width (second %))) parts))]]
          (doseq [[col text bold?] parts]
            (put! g (+ left (long col)) y text t/dialog-title-fg band bold?))
          (case control
            :send-all
            (do (when (hovered? :queue-send-all-now y nil) (underline! g from y width))
                (region! :queue-send-all-now y from width {:session-id (:session-id send-all)}))
            :edit
            (do (when (hovered? :queue-edit y nil) (underline! g from y width))
                (region! :queue-edit y from width {}))
            nil)))
      :row
      (let [selected? (:selected? line)
            paper (if selected? t/dialog-fg bg)]
        (p/set-colors! g t/code-block-fg paper)
        (p/fill-rect! g (inc left) y (+ iw 2) 1)
        (border!)
        (doseq [[col text k bold?] (cell-segments (row-cells line iw))
                :let [x (+ left (long col))
                      button? (contains? #{:primary :secondary} k)]]
          (cond button?
                (let [send (:send line)
                      width (long (p/display-width text))]
                  (put! g x y text (ink k) (button-bg k) bold?)
                  (when (hovered? :queue-send-now y (:turn-id send))
                    (underline! g (inc x) y (- width 2)))
                  (region! :queue-send-now y x width (select-keys send [:session-id :turn-id :deliver :marked?])))
                selected? (put! g x y text (if (= :edge k) (ink k) bg) paper (or bold? (= :fg k)))
                :else (put! g x y text (ink k) paper bold?))))
      :note
      (do (border!) (put! g (+ left 2) y (p/ellipsize (:text line) iw) t/dialog-hint bg false)))
    (p/clear-styles! g)))

(defn draw!
  "Paint the tray of `queue` from screen row `top` across `cols` columns. The frame spans columns
   1 to `cols - 2`, as the agent dock does. Each button and band control is a hit region."
  [^TextGraphics g queue top cols]
  (let [iw (content-width cols)]
    (doseq [[i line] (map-indexed vector (lines queue cols))
            :let [y (+ (long top) (long i))]]
      (p/set-colors! g t/code-block-fg t/terminal-bg)
      (p/fill-rect! g 0 y (max 1 (long cols)) 1)
      (draw-line! g line 1 y iw))))
