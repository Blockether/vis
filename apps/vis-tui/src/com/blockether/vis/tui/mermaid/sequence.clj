(ns com.blockether.vis.tui.mermaid.sequence
  "Mermaid sequence diagrams and ZenUML diagrams painted as lifelines.

   Both parsers build one event list: `{:kind :message :from :to :text :dotted?
   :end}`, `{:kind :note :over [ids] :side :text}` and `{:kind :block :word :text}` /
   `{:kind :else ...}` / `{:kind :end}`. The painter gives each gap between two
   lifelines the room that its labels need. When the lifelines cannot fit the
   bubble, the events become a numbered message list."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c]))

;; Sequence diagram parsing

(def ^:private arrow-re
  #"^(.+?)\s*(<<-->>|<<->>|-->>|->>|-->|->|--x|-x|--\)|-\))\s*([+-]?)\s*(.+?)\s*:\s*(.*)$")

(def ^:private block-words #{"loop" "alt" "opt" "par" "critical" "break" "rect" "box"})

(def ^:private else-words #{"else" "and" "option"})

(defn- add-participant
  [state id label kind]
  (let [id (str/trim id)]
    (if (some #(= id (:id %)) (:participants state))
      (cond-> state
        label
        (update :participants
                (fn [ps]
                  (mapv #(if (= id (:id %))
                           (assoc %
                             :label label
                             :kind (or kind (:kind %)))
                           %)
                        ps))))
      (update state
              :participants
              conj
              {:id id :label (or label id) :kind (or kind :participant)}))))

(defn- sequence-events
  [lines]
  (reduce
    (fn [state raw]
      (let [line
            (str/trim (str/replace raw #"%%.*$" ""))

            word
            (str/lower-case (first (str/split line #"\s+" 2)))

            rest-text
            (str/trim (subs line (min (count line) (count word))))]

        (cond
          (str/blank? line) state
          (re-find #"^(?:create\s+)?(?:participant|actor)\s" line)
          (let [[_ kind body]
                (re-find #"^(?:create\s+)?(participant|actor)\s+(.*)$" line)

                [_ id meta]
                (re-matches #"(\S+?)@\{(.*)\}.*" body)

                [_ id2 label]
                (re-matches #"(\S+)\s+as\s+(.*)" body)

                type
                (some->> meta
                         (re-find #"\"?type\"?\s*:\s*\"?(\w+)")
                         second)]

            (add-participant state
                             (or id id2 body)
                             (some-> label
                                     c/clean-label)
                             (cond (= "actor" kind) :actor
                                   type (keyword type)
                                   :else :participant)))
          (= "autonumber" word) (assoc state :autonumber? true)
          (re-find
            #"^(?:activate|deactivate|destroy|link|links|properties|details|title|acctitle|accdescr)$"
            word)
          state
          (= "note" word)
          (let [[_ side who text]
                (re-find #"(?i)^note\s+(left of|right of|over)\s+([^:]+?)\s*:\s*(.*)$" line)]
            (if side
              (let [ids (mapv str/trim (str/split who #","))]
                (-> (reduce #(add-participant %1 %2 nil nil) state ids)
                    (update :events
                            conj
                            {:kind :note
                             :over ids
                             :side (str/lower-case side)
                             :text (c/clean-label text)})))
              state))
          ;; A `box` only groups participants; its `end` closes no time block.
          (= "box" word) (update state :open conj :box)
          (contains? block-words word) (-> state
                                           (update :open conj :block)
                                           (update :events
                                                   conj
                                                   {:kind :block
                                                    :word (when-not (= "rect" word) word)
                                                    :text (when-not (= "rect" word)
                                                            (c/clean-label rest-text))}))
          (contains? else-words word)
          (update state :events conj {:kind :else :word word :text (c/clean-label rest-text)})
          (= "end" word) (cond-> (update state :open #(if (seq %) (pop %) %))
                           (not= :box (peek (:open state)))
                           (update :events conj {:kind :end}))
          :else (if-let [[_ from arrow _ to text] (re-matches arrow-re line)]
                  (let [to (str/replace to #"^[+-]" "")]
                    (-> state
                        (add-participant from nil nil)
                        (add-participant to nil nil)
                        (update :events
                                conj
                                {:kind :message
                                 :from (str/trim from)
                                 :to (str/trim to)
                                 :text (c/clean-label text)
                                 :dotted? (str/starts-with? arrow "--")
                                 :both? (str/starts-with? arrow "<<")
                                 :end (cond (str/ends-with? arrow "x") :cross
                                            (str/ends-with? arrow ")") :async
                                            (re-find #"->>$" arrow) :arrow
                                            :else :open)})))
                  state))))
    {:participants [] :events [] :autonumber? false :open []}
    lines))

;; ZenUML parsing

(defn- zen-events
  [lines]
  (loop [lines
         (map #(str/trim (str/replace % #"//.*$" "")) lines)

         state
         {:participants [] :events [] :stack []}

         closers
         []]

    (if-let [line (first lines)]
      (let [caller (or (peek (:stack state)) "●")
            opens? (str/ends-with? line "{")
            body (str/trim (str/replace line #"\{$" ""))
            body (str/trim (str/replace body #"^\}\s*" ""))
            closes? (str/starts-with? line "}")
            ;; `} else {` closes and opens in one line
            [state closers] (if closes?
                              (let [closer (peek closers)]
                                [(case closer
                                   :block
                                   (if opens? state (update state :events conj {:kind :end}))

                                   :call
                                   (update state :stack pop)

                                   state)
                                 (if (and opens? (= :block closer)) closers (pop closers))])
                              [state closers])]

        (cond
          (str/blank? body) (recur (rest lines) state closers)
          (re-find #"^(?:title|zenuml)\b" body) (recur (rest lines) state closers)
          (re-find #"^@(\w+)\s+(\S+)" body)
          (let [[_ kind id] (re-find #"^@(\w+)\s+(\S+)" body)]
            (if (re-find #"(?i)^(?:return|reply)$" kind)
              (recur (rest lines) state closers)
              (recur (rest lines)
                     (add-participant state
                                      id
                                      nil
                                      (if (= "Actor" kind) :actor (keyword (str/lower-case kind))))
                     closers)))
          (re-find #"^@(?:return|reply)$" body) (recur (rest lines) state closers)
          (re-find
            #"^(?:if|else if|while|for|forEach|foreach|loop|try|catch|finally|par|opt|critical|section|group)\b"
            body)
          (let
            [[_ word cond-text]
             (re-find
               #"^(else if|if|while|forEach|foreach|for|loop|try|catch|finally|par|opt|critical|section|group)\s*(?:\((.*)\))?"
               body)
             else? (and closes? (#{"else if" "catch" "finally"} word))
             event (if else?
                     {:kind :else :word word :text (str cond-text)}
                     {:kind :block
                      :word (case word
                              ("while" "for" "forEach" "foreach")
                              "loop"

                              ("if")
                              "alt"

                              word)
                      :text (str cond-text)})]

            (recur (rest lines)
                   (update state :events conj event)
                   (if (and opens? (not else?)) (conj closers :block) closers)))
          (re-find #"^else$" body) (recur
                                     (rest lines)
                                     (update state :events conj {:kind :else :word "else" :text ""})
                                     closers)
          (re-find #"^return\b" body)
          (let [callee (peek (:stack state))
                back (or (peek (pop (or (not-empty (:stack state)) [nil]))) "●")]

            (recur (rest lines)
                   (cond-> state
                     callee
                     (update :events
                             conj
                             {:kind :message
                              :from callee
                              :to back
                              :text (str/trim (subs body 6))
                              :dotted? true
                              :end :open}))
                   closers))
          (re-find #"^([\w.]+)\s*->\s*([\w.]+)\s*:\s*(.*)$" body)
          (let [[_ from to text] (re-find #"^([\w.]+)\s*->\s*([\w.]+)\s*:\s*(.*)$" body)]
            (recur
              (rest lines)
              (-> state
                  (add-participant from nil nil)
                  (add-participant to nil nil)
                  (update :events conj {:kind :message :from from :to to :text text :end :async}))
              (if opens? (conj closers :none) closers)))
          (re-find #"([A-Za-z_][\w]*)\.([\w]+)\s*\(" body)
          (let [[_ assign] (re-find #"^(?:[\w<>]+\s+)?(\w+)\s*=\s*" body)
                [_ callee method args] (re-find #"([A-Za-z_][\w]*)\.([\w]+)\s*\((.*?)\)" body)
                state (cond-> state
                        (= "●" caller)
                        (add-participant "●" "●" :starter))
                state (-> state
                          (add-participant callee nil nil)
                          (update :events
                                  conj
                                  {:kind :message
                                   :from caller
                                   :to callee
                                   :text (str (when assign (str assign " = ")) method "(" args ")")
                                   :end :arrow}))]

            (recur (rest lines)
                   (cond-> state
                     opens?
                     (update :stack conj callee))
                   (if opens? (conj closers :call) closers)))
          (re-find #"^new\s+(\w+)" body)
          (let [[_ callee] (re-find #"^new\s+(\w+)" body)]
            (recur (rest lines)
                   (-> state
                       (add-participant "●" "●" :starter)
                       (add-participant callee nil nil)
                       (update
                         :events
                         conj
                         {:kind :message :from caller :to callee :text "<<create>>" :end :arrow}))
                   (if opens? (conj closers :none) closers)))
          (re-matches #"[\w\"]+(?:\s+as\s+.*)?" body)
          (let [[_ id label] (re-matches #"(\S+)(?:\s+as\s+(.*))?" body)]
            (recur (rest lines)
                   (add-participant state
                                    (str/replace id "\"" "")
                                    (some-> label
                                            c/clean-label)
                                    nil)
                   closers))
          :else (recur (rest lines) state (if opens? (conj closers :none) closers))))
      state)))

;; Painting

(def ^:private kind-tone
  {:actor :blue
   :starter :chrome
   :database :green
   :boundary :purple
   :control :orange
   :entity :yellow
   :queue :orange})

(defn- box-rows
  "Three header rows of a participant box `w` wide, with a joint at `joint`."
  [label kind ^long w]
  (let [round?
        (= :actor kind)

        [tl tr bl br]
        (if round? ["╭" "╮" "╰" "╯"] ["┌" "┐" "└" "┘"])

        inner
        (- w 2)

        mid
        (quot (dec w) 2)]

    [(str tl (apply str (repeat inner \─)) tr)
     (str "│"
          (c/pad-right (str (apply str (repeat (quot (- inner (c/width label)) 2) \space)) label)
                       inner)
          "│")
     (str bl
          (apply str
            (for [i (range inner)]
              (if (= (inc i) mid) \┬ \─)))
          br)]))

(defn- layout
  "Lifeline columns of `participants` that give each message label its room,
   with labels cut at `limit` columns."
  [participants events ^long limit]
  (let [ids
        (mapv :id participants)

        index
        (into {}
              (map-indexed (fn [i id]
                             [id i])
                           ids))

        n
        (count ids)

        box-w
        (mapv #(+ 4 (min limit (c/width (:label %)))) participants)

        gaps
        (long-array (max 0 (dec n)))]

    (doseq [i (range (dec n))]
      (aset gaps i (long (max 4 (+ 1 (quot (long (box-w i)) 2) (quot (long (box-w (inc i))) 2))))))
    (doseq [{:keys [kind from to text over side]} events]
      (case kind
        :message
        (let [a (long (index from 0))
              b (long (index to 0))
              [lo hi] [(min a b) (max a b)]
              need (+ 4 (min limit (c/width text)))]

          (if (= lo hi)
            (when (< hi (dec n)) (aset gaps hi (max (aget gaps hi) (+ 4 need))))
            (let [have (reduce + (map #(aget gaps %) (range lo hi)))]
              (when (< have need) (aset gaps (dec hi) (+ (aget gaps (dec hi)) (- need have)))))))

        :note
        (let [i (long (index (first over) 0))
              need (+ 6 (min limit (c/width text)))]

          (when (and (= "right of" side) (< i (dec n))) (aset gaps i (max (aget gaps i) need)))
          (when (and (= "left of" side) (pos? i))
            (aset gaps (dec i) (max (aget gaps (dec i)) need))))

        nil))
    (let [first-x
          (quot (long (first box-w)) 2)

          xs
          (vec (reductions + first-x (vec gaps)))

          left-need
          (reduce max
                  0
                  (for [{:keys [kind over side text]}
                        events

                        :when (and (= :note kind)
                                   (= "left of" side)
                                   (zero? (long (index (first over) 0))))]

                    (+ 6 (min limit (c/width text)))))

          shift
          (max 0 (- left-need first-x))

          xs
          (mapv #(+ (long %) shift) xs)]

      {:xs xs
       :index index
       :box-w box-w
       :cols (+ (long (peek xs)) 1 (quot (long (peek box-w)) 2) 2)
       :right-room (reduce max
                           0
                           (for [{:keys [kind over side text]}
                                 events

                                 :when (and (= :note kind)
                                            (= "right of" side)
                                            (= (dec n) (long (index (first over) 0))))]

                             (+ 4 (min limit (c/width text)))))})))

(def ^:private end-glyph
  {:arrow {:right \▶ :left \←}
   :open {:right \> :left \<}
   :cross {:right \✕ :left \✕}
   :async {:right \→ :left \←}})

(defn- paint
  [{:keys [participants events autonumber?]} ^long width ^long limit]
  (let [{:keys [xs index box-w cols right-room]}
        (layout participants events limit)

        cols
        (+ (long cols) (long right-room))]

    (when (<= cols width)
      (let [row-count
            (+ 4
               (reduce +
                       (for [{:keys [kind text from to]} events]
                         (case kind
                           :message
                           (+ (count (c/wrap-words text limit)) (if (= from to) 2 1))

                           :note
                           (+ 2 (count (c/wrap-words text limit)))

                           1))))

            canvas
            (c/make-canvas row-count cols)

            x-of
            #(long (xs (index % 0)))]

        (doseq [[i {:keys [label kind]}]
                (map-indexed vector participants)

                :let [w
                      (long (box-w i))

                      left
                      (- (long (xs i)) (quot (dec w) 2))

                      tone
                      (kind-tone kind :cyan)]]

          (doseq [[r text] (map-indexed vector (box-rows (c/clip label (- w 4)) kind w))]
            (c/put-text! canvas r left text tone))
          (c/put-text! canvas
                       1
                       (+ left (quot (- w (c/width (c/clip label (- w 4)))) 2))
                       (c/clip label (- w 4))
                       :text))
        (doseq [r
                (range 3 row-count)

                x
                xs]

          (c/put-char! canvas r x \│ :chrome))
        (loop [events
               events

               row
               3

               number
               1

               depth
               0]

          (when-let [{:keys [kind from to text dotted? end both? over side word]} (first events)]
            (case kind
              :message
              (let [a (x-of from)
                    b (x-of to)
                    text (if autonumber? (str number ". " text) text)
                    lines (c/wrap-words text limit)
                    line-ch (if dotted? \· \─)
                    tone (if dotted? :chrome :text)]

                (if (= a b)
                  (do (doseq [[i l] (map-indexed vector lines)]
                        (c/put-text! canvas (+ row (long i)) (+ a 2) l :text))
                      (let [r (+ row (count lines))]
                        (c/put-text! canvas r a (str "├" line-ch line-ch "┐") :chrome)
                        (c/put-text! canvas
                                     (inc r)
                                     a
                                     (str "│" (get-in end-glyph [end :left] \←) line-ch "┘")
                                     :chrome)
                        (recur (rest events) (+ r 2) (inc number) depth)))
                  (let [[lo hi] [(min a b) (max a b)]
                        r (+ row (count lines))]

                    (doseq [[i l] (map-indexed vector lines)]
                      (c/put-text! canvas
                                   (+ row (long i))
                                   (+ lo 1 (quot (- hi lo 1 (c/width l)) 2))
                                   l
                                   tone))
                    (doseq [x (range (inc lo) hi)]
                      (c/put-char! canvas r x line-ch :chrome))
                    (c/put-char! canvas
                                 r
                                 (if (< a b) (dec b) (inc b))
                                 (get-in end-glyph [end (if (< a b) :right :left)] \▶)
                                 :text)
                    (when both?
                      (c/put-char! canvas
                                   r
                                   (if (< a b) (inc a) (dec a))
                                   (get-in end-glyph [end (if (< a b) :left :right)] \←)
                                   :text))
                    (recur (rest events) (inc r) (inc number) depth))))

              :note
              (let [lines (c/wrap-words text limit)
                    w (+ 4 (long (apply max 1 (map c/width lines))))
                    a (x-of (first over))
                    b (x-of (last over))
                    left (case side
                           "right of"
                           (+ a 2)

                           "left of"
                           (- a 1 w)

                           (- (quot (+ a b) 2) (quot w 2)))
                    bar (apply str (repeat (- w 2) \─))]

                (c/put-text! canvas row left (str "┌" bar "┐") :yellow)
                (doseq [[i l] (map-indexed vector lines)]
                  (c/put-text! canvas
                               (+ row 1 (long i))
                               left
                               (str "│ " (c/pad-right l (- w 4)) " │")
                               :yellow))
                (c/put-text! canvas (+ row 1 (count lines)) left (str "└" bar "┘") :yellow)
                (recur (rest events) (+ row 2 (count lines)) number depth))

              (:block :else)
              (let [label (str/trim (str (or word "") " " (or text "")))
                    label (if (str/blank? label) "" (str " " label " "))]

                (doseq [x (range depth (- cols depth))]
                  (when (= \space (c/char-at canvas row x)) (c/put-char! canvas row x \· :purple)))
                (c/put-text! canvas row (+ depth 1) (c/clip label (- cols depth 2)) :purple)
                (recur (rest events) (inc row) number (if (= :block kind) (inc depth) depth)))

              :end
              (do (doseq [x (range (max 0 (dec depth)) (- cols (max 0 (dec depth))))]
                    (when (= \space (c/char-at canvas row x))
                      (c/put-char! canvas row x \· :purple)))
                  (recur (rest events) (inc row) number (max 0 (dec depth))))

              (recur (rest events) row number depth))))
        (c/canvas->rows canvas)))))

(defn- message-list
  "Numbered message list for lifelines that do not fit `width`."
  [{:keys [participants events]} ^long width]
  (let [label-of (into {} (map (juxt :id :label) participants))]
    (vec
      (keep (fn [{:keys [kind from to text dotted? word over]}]
              (case kind
                :message
                (c/seg-row (c/seg-clip [[(label-of from from) :cyan]
                                        [(if dotted? " ··▶ " " ──▶ ") :chrome]
                                        [(label-of to to) :cyan] [(str ": " text) :text]]
                                       width))

                :note
                (c/seg-row (c/seg-clip [["  note " :chrome] [(str/join ", " over) :chrome]
                                        [(str ": " text) :yellow]]
                                       width))

                (:block :else)
                (c/seg-row (c/seg-clip [[(str word " " text) :purple]] width))

                :end
                (c/seg-row [["end" :purple]])

                nil))
            events))))

(defn- draw-events
  [model width]
  (let [width (long width)]
    (if (empty? (:participants model))
      {:reason "no participants"}
      {:rows (or (some #(paint model width %) [32 24 16 12 8]) (message-list model width))})))

(defn sequence-diagram
  [{:keys [lines]} width]
  (let [model (sequence-events lines)]
    (assoc (draw-events model width)
      :title (some #(some-> (re-find #"^\s*title\s*:?\s+(.*)$" %)
                            second
                            c/clean-label)
                   lines))))

(defn zenuml
  [{:keys [lines]} width]
  (let [model (zen-events lines)]
    (assoc (draw-events model width)
      :title (some #(some-> (re-find #"^\s*title\s+(.*)$" %)
                            second
                            c/clean-label)
                   lines))))
