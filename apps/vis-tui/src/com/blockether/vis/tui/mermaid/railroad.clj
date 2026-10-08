(ns com.blockether.vis.tui.mermaid.railroad
  "Mermaid railroad diagrams (EBNF, ABNF, PEG and the function syntax) as text
   rails. A choice stacks its branches between two junction columns, an option
   adds a bypass rail, a repetition adds a loop back under its body. Terminals
   are green, rule references are cyan and the rails use the chrome tone. A rule
   that is wider than the bubble falls back to its grammar text."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c]))

;; Reading

(defn- token-pattern
  [dialect]
  (re-pattern
    (str
      "\\s+|\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*'|%[xdbXDB][0-9A-Fa-f]+(?:[-.][0-9A-Fa-f]+)*"
      "|<[^>\\s]+>|::=|<-|=/|:=|"
      (case dialect
        :abnf
        "\\d*\\*\\d*|\\d+|"

        :peg
        "\\[(?:[^\\]\\\\]|\\\\.)*\\]|"

        "")
      "\\d+|[A-Za-z_][\\w-]*|.")))

(defn- tokens
  [text dialect]
  (->> (re-seq (token-pattern dialect) text)
       (remove str/blank?)
       (mapv (fn [^String token]
               (let [head (.charAt token 0)]
                 (cond (#{"::=" "<-" "=/" ":=" "="} token) [:def token]
                       (or (= \" head) (= \' head)) [:str (subs token 1 (dec (count token)))]
                       (= \% head) [:str token]
                       (and (= :peg dialect) (= \[ head) (> (count token) 1)) [:str token]
                       (and (= \< head) (> (count token) 2)) [:ident
                                                              (subs token 1 (dec (count token)))]
                       (and (= :abnf dialect) (re-matches #"\d*\*\d*|\d+" token)) [:rep token]
                       (re-matches #"\d+" token) [:num token]
                       (re-matches #"[A-Za-z_][\w-]*" token) [:ident token]
                       :else [:sym token]))))))

(def ^:private calls
  #{"sequence" "seq" "stack" "choice" "alt" "optional" "opt" "zeroormore" "oneormore" "terminal"
    "nonterminal" "skip" "comment" "group" "diagram"})

(defn- parse-rules
  "[[name tree] ...] of the grammar `toks`."
  [toks]
  (let [at
        (volatile! 0)

        peek-at
        (fn [n]
          (get toks (+ (long @at) (long n))))

        kind
        (fn [n]
          (first (peek-at n)))

        value
        (fn [n]
          (when (= :sym (kind n)) (second (peek-at n))))

        take!
        (fn []
          (let [token (peek-at 0)]
            (vswap! at inc)
            token))

        expect!
        (fn [text]
          (if (= text (value 0))
            (take!)
            (throw (ex-info (str "expected " text " at " (or (second (peek-at 0)) "end")) {}))))

        rule-start?
        (fn []
          (and (= :ident (kind 0)) (= :def (kind 1))))

        ends
        #{"|" "/" ")" "]" "}" ";"}]

    (letfn
      [(alt [in-call?]
         (loop [branches [(sequence* in-call?)]]
           (if (#{"|" "/"} (value 0))
             (do (take!) (recur (conj branches (sequence* in-call?))))
             (if (= 1 (count branches)) (first branches) [:alt branches]))))
       (sequence* [in-call?]
         (loop [items []]
           (cond
             (or (nil? (peek-at 0)) (ends (value 0)) (rule-start?) (and in-call? (= "," (value 0))))
             (case (count items)
               0
               [:skip]

               1
               (first items)

               [:seq items])
             (= "," (value 0)) (do (take!) (recur items))
             :else (recur (conj items (postfix in-call?))))))
       (postfix [in-call?]
         (let [token (peek-at 0)]
           (cond (= :rep (first token)) (do (take!) (repeat-of (second token) (postfix in-call?)))
                 (#{"!" "&"} (value 0)) (do (take!)
                                            [:seq [[:mark (second token)] (postfix in-call?)]])
                 :else (loop [tree (primary)]
                         (case (value 0)
                           "?"
                           (do (take!) (recur [:opt tree]))

                           "*"
                           (do (take!) (recur [:opt [:some tree]]))

                           "+"
                           (do (take!) (recur [:some tree]))

                           tree)))))
       (repeat-of [rep tree]
         (let [[_ low high]
               (re-matches #"(\d*)(\*?\d*)" rep)

               low
               (if (str/blank? low) 0 (parse-long low))

               star?
               (str/starts-with? high "*")]

           (cond (not star?) (if (= 1 low) tree [:seq [[:mark (str low "x")] tree]])
                 (zero? (long low)) [:opt [:some tree]]
                 (= 1 low) [:some tree]
                 :else [:seq [[:mark (str low "x")] [:some tree]]])))
       (call [name]
         (take!)
         (expect! "(")
         (let [args
               (loop [args []]
                 (cond (= ")" (value 0)) (do (take!) args)
                       (= "," (value 0)) (do (take!) (recur args))
                       (nil? (peek-at 0)) (throw (ex-info "expected )" {}))
                       :else (recur (conj args (alt true)))))

               args
               (vec (remove #(= :num (first %)) args))

               text
               (fn []
                 (let [arg (first args)]
                   (if (#{:t :n} (first arg)) (second arg) "")))]

           (case (str/lower-case name)
             ("sequence" "seq" "stack" "diagram")
             [:seq args]

             ("choice" "alt")
             [:alt args]

             ("optional" "opt")
             [:opt [:seq args]]

             "zeroormore"
             [:opt [:some (first args)]]

             "oneormore"
             [:some (first args)]

             "terminal"
             [:t (text)]

             "nonterminal"
             [:n (text)]

             "comment"
             [:mark (text)]

             "skip"
             [:skip]

             "group"
             (first args))))
       (primary []
         (let [[token-kind token]
               (peek-at 0)

               text
               (value 0)]

           (cond (and (= :ident token-kind) (= "(" (value 1)) (calls (str/lower-case token)))
                 (call token)
                 (= :ident token-kind) (do (take!) [:n token])
                 (#{:str :num} token-kind) (do (take!) [:t token])
                 (= "(" text) (do (take!)
                                  (let [tree (alt false)]
                                    (expect! ")")
                                    tree))
                 (= "[" text) (do (take!)
                                  (let [tree (alt false)]
                                    (expect! "]")
                                    [:opt tree]))
                 (= "{" text) (do (take!)
                                  (let [tree (alt false)]
                                    (expect! "}")
                                    [:opt [:some tree]]))
                 (= "." text) (do (take!) [:t "any"])
                 :else (throw (ex-info (str "unexpected " (or token "end")) {})))))]
      (loop [rules []]
        (cond (nil? (peek-at 0)) rules
              (= ";" (value 0)) (do (take!) (recur rules))
              (rule-start?) (let [name (second (take!))]
                              (take!)
                              (recur (conj rules [name (alt false)])))
              :else (throw (ex-info (str "expected a rule at " (second (peek-at 0))) {})))))))

;; Drawing. A block is {:rows [[[char tone] ...] ...] :y rail-row :w width}.

(defn- cells
  [text tone]
  (mapv (fn [ch]
          [ch tone])
        (str text)))

(defn- rail [n] (cells (apply str (repeat n \─)) :chrome))

(defn- blank [n] (vec (repeat n [\space nil])))

(defn- text-block [text tone] {:rows [(cells text tone)] :y 0 :w (c/width text)})

(defn- pad-rows
  "Rows of `block` padded to `top` rows above the rail and `total` rows."
  [{:keys [rows y w]} top total]
  (let [above (- (long top) (long y))]
    (vec (concat (repeat above (blank w))
                 rows
                 (repeat (- (long total) above (count rows)) (blank w))))))

(defn- hcat
  [blocks]
  (let [top
        (long (apply max (map :y blocks)))

        total
        (apply max (map #(+ (- top (long (:y %))) (count (:rows %))) blocks))

        padded
        (map #(pad-rows % top total) blocks)

        joint
        (fn [row]
          (if (= row top) (rail 1) (blank 1)))]

    {:y top
     :w (+ (reduce + (map :w blocks)) (dec (count blocks)))
     :rows (vec (for [row (range total)]
                  (vec (apply concat (interpose (joint row) (map #(nth % row) padded))))))}))

(defn- fit
  [row w on-rail?]
  (into row (if on-rail? (rail (- (long w) (count row))) (blank (- (long w) (count row))))))

(defn- choice
  [blocks]
  (let [w
        (apply max (map :w blocks))

        last-at
        (dec (count blocks))

        rows
        (vec
          (apply concat
            (map-indexed
              (fn [at {:keys [rows y]}]
                (map-indexed
                  (fn [row cells]
                    (let [entry?
                          (= row y)

                          [left right]
                          (cond entry? (cond (zero? (long at)) [\┬ \┬]
                                             (= at last-at) [\╰ \╯]
                                             :else [\├ \┤])
                                (and (zero? (long at)) (< (long row) (long y))) [\space \space]
                                (and (= at last-at) (> (long row) (long y))) [\space \space]
                                :else [\│ \│])

                          main?
                          (and entry? (zero? (long at)))]

                      (vec (concat (if main? (rail 1) (blank 1))
                                   [[left (when (not= \space left) :chrome)]]
                                   (if entry? (rail 1) (blank 1))
                                   (fit cells w entry?)
                                   (if entry? (rail 1) (blank 1))
                                   [[right (when (not= \space right) :chrome)]]
                                   (if main? (rail 1) (blank 1))))))
                  rows))
              blocks)))]

    {:rows rows :y (:y (first blocks)) :w (+ 6 (long w))}))

(defn- loop-back
  [{:keys [rows y w]}]
  (let [loop-row (concat [[\space nil] [\╰ :chrome]]
                         (rail (quot (+ 2 (long w)) 2))
                         [[\← :chrome]]
                         (rail (- (+ 2 (long w)) 1 (quot (+ 2 (long w)) 2)))
                         [[\╯ :chrome] [\space nil]])]
    {:y y
     :w (+ 6 (long w))
     :rows (conj (vec (map-indexed (fn [row cells]
                                     (let [side (cond (= row y) \┬
                                                      (> (long row) (long y)) \│
                                                      :else \space)
                                           on? (= row y)]

                                       (vec (concat (if on? (rail 1) (blank 1))
                                                    [[side (when (not= \space side) :chrome)]]
                                                    (if on? (rail 1) (blank 1))
                                                    cells
                                                    (if on? (rail 1) (blank 1))
                                                    [[side (when (not= \space side) :chrome)]]
                                                    (if on? (rail 1) (blank 1))))))
                                   rows))
                 (vec loop-row))}))

(defn- block
  [[kind arg]]
  (case kind
    :t
    (text-block (str "\"" arg "\"") :green)

    :n
    (text-block arg :cyan)

    :mark
    (text-block arg :yellow)

    :skip
    {:rows [[]] :y 0 :w 0}

    :seq
    (hcat (mapv block arg))

    :alt
    (choice (mapv block arg))

    :opt
    (choice [{:rows [[]] :y 0 :w 0} (block arg)])

    :some
    (loop-back (block arg))))

(defn- grammar-text
  "Segments of `tree` in EBNF notation."
  [[kind arg :as tree] nested?]
  (case kind
    :t
    [[(str "\"" arg "\"") :green]]

    :n
    [[arg :cyan]]

    :mark
    [[arg :yellow]]

    :skip
    [["·" :chrome]]

    :seq
    (let [inner (vec (apply concat (interpose [[" " nil]] (map #(grammar-text % true) arg))))]
      (if nested? (vec (concat [["(" :chrome]] inner [[")" :chrome]])) inner))

    :alt
    (let [inner (vec (apply concat (interpose [[" | " :chrome]] (map #(grammar-text % true) arg))))]
      (if nested? (vec (concat [["(" :chrome]] inner [[")" :chrome]])) inner))

    :opt
    (if (= :some (first arg))
      (conj (grammar-text (second arg) true) ["*" :chrome])
      (conj (grammar-text arg true) ["?" :chrome]))

    :some
    (conj (grammar-text arg true) ["+" :chrome])

    (throw (ex-info (str "unknown tree " tree) {}))))

(defn- wrap-segments
  "Segment rows of `segments` broken at spaces to fit `width` columns."
  [segments width indent]
  (reduce (fn [rows [text tone]]
            (let [row
                  (peek rows)

                  used
                  (c/seg-width row)]

              (if (and (str/starts-with? text " ")
                       (> (+ used (c/width text)) (long width))
                       (> used (long indent)))
                (conj rows [[(apply str (repeat indent \space)) nil] [(str/triml text) tone]])
                (conj (pop rows) (conj row [text tone])))))
          [[]]
          segments))

(defn- rule-rows
  [[name tree] width]
  (let [{:keys [rows y w]}
        (block tree)

        heading
        (c/seg-row [[(c/clip name width) :cyan]])]

    (if (<= (+ 4 (long w)) (long width))
      (into [heading]
            (map-indexed (fn [row cells]
                           (let [on?
                                 (= row y)

                                 cells
                                 (concat (if on? [[\├ :chrome] [\─ :chrome]] (blank 2))
                                         cells
                                         (if on? [[\─ :chrome] [\┤ :chrome]] (blank 2)))]

                             (str/trimr (c/seg-row (map (fn [run]
                                                          [(apply str (map first run))
                                                           (second (first run))])
                                                        (partition-by second cells))))))
                         rows))
      (into [heading]
            (map #(c/seg-row (c/seg-clip % width))
                 (wrap-segments (into [["  ::= " :chrome]]
                                      (map (fn [[text tone]]
                                             [text tone]))
                                      (mapcat (fn [[text tone]]
                                                (if (= " " text) [[" " nil]] [[text tone]]))
                                              (grammar-text tree false)))
                                width
                                6))))))

(defn railroad
  "Rows of a Mermaid railroad diagram inside `width` columns."
  [{:keys [kind lines]} width]
  (let [dialect
        (cond (str/includes? kind "abnf") :abnf
              (str/includes? kind "peg") :peg
              :else :ebnf)

        [title-lines body]
        ((juxt filter remove) #(re-find #"^\s*title\b" %) lines)

        title
        (some->> (first title-lines)
                 (re-find #"^\s*title\s+(.*)$")
                 second
                 str/trim
                 (#(str/replace % #"^\"(.*)\"$" "$1")))

        body
        (str/join "\n" (if (= :abnf dialect) body (map #(str/replace % #"^\s*(//|#).*$" "") body)))

        rules
        (parse-rules (tokens body dialect))]

    (when (empty? rules) (throw (ex-info "no rules" {})))
    {:title title :rows (vec (butlast (mapcat #(conj (rule-rows % width) "") rules)))}))
