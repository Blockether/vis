(ns com.blockether.vis.tui.mermaid.canvas
  "Shared paint tools for Mermaid diagrams: text fitting, colour tones and a
   character canvas whose rows come out as ANSI-coloured strings.

   A TONE is an ANSI SGR foreground code. The TUI code painter maps each code
   to a colour of the active theme, so a diagram follows the theme: 90 is the
   muted chrome colour, 91 red, 31 orange, 33 yellow, 32 green, 36 cyan, 34 blue
   and 35 purple. Tone 0 keeps the code foreground."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.primitives :as p]))

;; Tones

(def tones
  "Tone keyword -> ANSI SGR foreground code."
  {:none 0
   :chrome 90
   :red 91
   :orange 31
   :yellow 33
   :green 32
   :cyan 36
   :blue 34
   :purple 35
   :text 37})

(def palette
  "Series colours in the order that a chart gives them to its series."
  [:cyan :orange :green :purple :blue :yellow :red])

(defn series-tone
  "Palette tone of the series at index `at`."
  [at]
  (nth palette (mod (long at) (count palette))))

(defn tone-code ^long [tone] (long (if (number? tone) tone (get tones tone 0))))

(def ^:private named-colours
  {"red" :red
   "darkred" :red
   "crimson" :red
   "maroon" :red
   "firebrick" :red
   "tomato" :orange
   "orange" :orange
   "darkorange" :orange
   "coral" :orange
   "brown" :orange
   "chocolate" :orange
   "gold" :yellow
   "yellow" :yellow
   "khaki" :yellow
   "olive" :yellow
   "green" :green
   "lime" :green
   "darkgreen" :green
   "lightgreen" :green
   "seagreen" :green
   "forestgreen" :green
   "limegreen" :green
   "teal" :cyan
   "cyan" :cyan
   "aqua" :cyan
   "turquoise" :cyan
   "darkcyan" :cyan
   "blue" :blue
   "navy" :blue
   "royalblue" :blue
   "steelblue" :blue
   "lightblue" :blue
   "skyblue" :blue
   "dodgerblue" :blue
   "darkblue" :blue
   "purple" :purple
   "violet" :purple
   "magenta" :purple
   "fuchsia" :purple
   "pink" :purple
   "hotpink" :purple
   "orchid" :purple
   "indigo" :purple
   "plum" :purple
   "lavender" :purple})

(defn- hex-rgb
  [^String hex]
  (let [digits
        (subs hex 1)

        digits
        (case (count digits)
          (3 4)
          (apply str (mapcat #(repeat 2 %) (take 3 digits)))

          (6 8)
          (subs digits 0 6)

          nil)]

    (when digits (mapv #(Long/parseLong (subs digits % (+ (long %) 2)) 16) [0 2 4]))))

(defn- rgb-tone
  "Theme hue bucket of an RGB colour, or nil for a grey, a near-black or a near-white."
  [[r g b]]
  (let [r
        (/ (double r) 255.0)

        g
        (/ (double g) 255.0)

        b
        (/ (double b) 255.0)

        hi
        (max r g b)

        lo
        (min r g b)

        light
        (/ (+ hi lo) 2.0)

        delta
        (- hi lo)

        sat
        (if (zero? delta) 0.0 (/ delta (- 1.0 (Math/abs (- (* 2.0 light) 1.0)))))]

    (when (and (> delta 0.08) (> sat 0.2) (> light 0.12) (< light 0.95))
      (let [hue (* 60.0
                   (double (cond (= hi r) (mod (/ (- g b) delta) 6.0)
                                 (= hi g) (+ (/ (- b r) delta) 2.0)
                                 :else (+ (/ (- r g) delta) 4.0))))]
        (cond (or (< hue 15.0) (>= hue 345.0)) :red
              (< hue 42.0) :orange
              (< hue 70.0) :yellow
              (< hue 160.0) :green
              (< hue 200.0) :cyan
              (< hue 255.0) :blue
              :else :purple)))))

(defn colour-tone
  "Tone of a CSS colour (`#f9f`, `#ff9900`, `rgb(1,2,3)` or a common name), or nil."
  [colour]
  (let [colour (some-> colour
                       str/trim
                       str/lower-case
                       (str/replace #"[;'\"]" ""))]
    (cond (str/blank? colour) nil
          (re-matches #"#[0-9a-f]{3,8}" colour) (some-> (hex-rgb colour)
                                                        rgb-tone)
          (str/starts-with? colour "rgb")
          (let [parts (re-seq #"[0-9.]+" colour)]
            (when (>= (count parts) 3) (rgb-tone (mapv #(Double/parseDouble %) (take 3 parts)))))
          :else (named-colours colour))))

(defn style-tones
  "`{:border tone :text tone}` of a Mermaid style list such as
   `fill:#f9f,stroke:#333,color:#fff`. A coloured stroke or fill tints the
   border; a coloured `color` tints the text."
  [style]
  (let [props
        (into {}
              (for [part
                    (str/split (or style "") #"[,;]")

                    :let [[k v]
                          (str/split part #":" 2)]
                    :when v]

                [(str/lower-case (str/trim k)) (str/trim v)]))

        border
        (or (colour-tone (props "stroke")) (colour-tone (props "fill")))

        text
        (colour-tone (props "color"))]

    (cond-> {}
      border
      (assoc :border border)

      text
      (assoc :text text))))

;; Text

(defn width ^long [s] (long (p/display-width (str s))))

(defn cut-to-width
  "[head tail] of `text` where `head` fits `limit` columns."
  [^String text ^long limit]
  (loop [at
         0

         used
         0]

    (if (or (>= at (.length text)) (> (+ used (width (str (.charAt text at)))) limit))
      [(subs text 0 at) (subs text at)]
      (recur (inc at) (+ used (width (str (.charAt text at))))))))

(defn clip
  "`text` cut to `limit` columns, ending in `…` when something was cut."
  [text ^long limit]
  (let [text (str text)]
    (cond (<= (width text) limit) text
          (< limit 1) ""
          :else (str (first (cut-to-width text (dec limit))) "…"))))

(defn pad-right
  [text ^long cols]
  (str text (apply str (repeat (max 0 (- cols (width text))) \space))))

(defn pad-left
  [text ^long cols]
  (str (apply str (repeat (max 0 (- cols (width text))) \space)) text))

(defn wrap-words
  "Lines of `text` wrapped at word breaks inside `limit` columns. A word wider
   than `limit` is cut."
  [text ^long limit]
  (let [limit (max 1 limit)]
    (loop [words (remove str/blank? (str/split (str/trim (str text)) #"\s+"))
           line ""
           out []]

      (if-let [word (first words)]
        (cond (str/blank? line) (if (> (width word) limit)
                                  (let [[head tail] (cut-to-width word limit)]
                                    (recur (cons tail (rest words)) "" (conj out head)))
                                  (recur (rest words) word out))
              (<= (+ (width line) 1 (width word)) limit)
              (recur (rest words) (str line " " word) out)
              :else (recur words "" (conj out line)))
        (cond (seq line) (conj out line)
              (seq out) out
              :else [""])))))

(defn label-lines
  "Lines of a label: its own line breaks, each wrapped inside `limit` columns."
  [text ^long limit]
  (vec (mapcat #(wrap-words % limit) (str/split-lines (if (str/blank? text) " " text)))))

(defn clean-label
  "Label text without quotes, HTML tags, entities or Markdown emphasis marks."
  [raw]
  (-> (or raw "")
      str/trim
      (str/replace #"^\"(.*)\"$" "$1")
      (str/replace #"^`(.*)`$" "$1")
      (str/replace #"(?i)<br\s*/?>" "\n")
      (str/replace #"(?i)&quot;|#quot;" "\"")
      (str/replace #"(?i)&amp;|#amp;" "&")
      (str/replace #"(?i)&lt;|#lt;" "<")
      (str/replace #"(?i)&gt;|#gt;" ">")
      (str/replace #"(?i)&nbsp;" " ")
      (str/replace #"<[^>]*>" "")
      (str/replace #"\*\*([^*]+)\*\*" "$1")
      (str/replace #"(?<![\w*])\*([^*\s][^*]*?)\*(?![\w*])" "$1")
      (str/replace #"\\n" "\n")
      str/trim))

;; Segment rows

(defn plain "`s` without ANSI SGR codes." [s] (str/replace (str s) #"\u001b\[[0-9;]*m" ""))

(defn row-width "Display columns of an ANSI row." ^long [row] (width (plain row)))

(defn paint
  "`text` wrapped in the ANSI code of `tone`."
  [tone text]
  (let [code
        (tone-code tone)

        text
        (str text)]

    (if (or (zero? code) (= "" text)) text (str "\u001b[" code "m" text "\u001b[0m"))))

(defn seg-width
  "Display columns of a segment row: a vector of `[text tone]` pairs."
  ^long [segments]
  (reduce + 0 (map #(width (first %)) segments)))

(defn seg-row
  "ANSI string of a segment row, with trailing blanks dropped."
  [segments]
  (str/trimr (apply str
               (map (fn [[text tone]]
                      (paint tone text))
                    segments))))

(defn seg-clip
  "Segment row cut to `limit` columns; the last kept segment ends in `…`."
  [segments ^long limit]
  (if (<= (seg-width segments) limit)
    segments
    (loop [left
           segments

           used
           0

           out
           []]

      (let [[text tone]
            (first left)

            w
            (width text)]

        (cond (nil? text) out
              (<= (+ used w) (dec limit)) (recur (rest left) (+ used w) (conj out [text tone]))
              :else (conj out [(clip (str text "…") (- limit used)) tone]))))))

;; Canvas

(def up-bit 1)

(def down-bit 2)

(def left-bit 4)

(def right-bit 8)

(def ^:private light-glyphs
  {1 \│ 2 \│ 3 \│ 4 \─ 8 \─ 12 \─ 5 \┘ 9 \└ 6 \┐ 10 \┌ 7 \┤ 11 \├ 13 \┴ 14 \┬ 15 \┼})

(def ^:private heavy-glyphs
  {1 \┃ 2 \┃ 3 \┃ 4 \─ 8 \─ 12 \─ 5 \┘ 9 \└ 6 \┐ 10 \┌ 7 \┤ 11 \├ 13 \┴ 14 \┬ 15 \┼})

(def ^:private dotted-glyphs (merge light-glyphs {1 \· 2 \· 3 \· 4 \· 8 \· 12 \·}))

(def ^:private style-glyphs {:solid light-glyphs :thick heavy-glyphs :dotted dotted-glyphs})

(def ^:private style-code {:solid 0 :dotted 1 :thick 2})

(def ^:private code-style {0 :solid 1 :dotted 2 :thick})

(def chrome-glyphs
  "Glyphs that draw the SHAPE of a diagram. They take the muted chrome tone
   unless a node or an edge gives them its own colour."
  (set "─│┃┌┐└┘├┤┬┴┼╭╮╯╰/\\·▶▼←↑"))

(defn make-canvas
  [^long rows ^long cols]
  {:rows rows
   :cols cols
   :chars (let [grid (make-array Character/TYPE rows cols)]
            (dotimes [row rows]
              (java.util.Arrays/fill ^chars (aget ^"[[C" grid row) \space))
            grid)
   :bits (make-array Integer/TYPE rows cols)
   :styles (make-array Integer/TYPE rows cols)
   :tones (make-array Integer/TYPE rows cols)})

(defn inside?
  [canvas row col]
  (and (>= (long row) 0)
       (>= (long col) 0)
       (< (long row) (long (:rows canvas)))
       (< (long col) (long (:cols canvas)))))

(defn set-tone!
  [canvas row col tone]
  (when (inside? canvas row col)
    (aset-int ^ints (aget ^"[[I" (:tones canvas) (long row)) (long col) (int (tone-code tone)))))

(defn put-char!
  ([canvas row col ch] (put-char! canvas row col ch nil))
  ([canvas row col ch tone]
   (when (inside? canvas row col)
     (aset-char ^chars (aget ^"[[C" (:chars canvas) (long row)) (long col) ch)
     (when tone (set-tone! canvas row col tone)))))

(defn char-at
  [canvas row col]
  (when (inside? canvas row col) (aget ^chars (aget ^"[[C" (:chars canvas) (long row)) (long col))))

(defn bits-at
  ^long [canvas row col]
  (if (inside? canvas row col)
    (long (aget ^ints (aget ^"[[I" (:bits canvas) (long row)) (long col)))
    0))

(defn put-text!
  ([canvas row col text] (put-text! canvas row col text nil))
  ([canvas row col ^String text tone]
   (let [text (str text)]
     (dotimes [at (.length text)]
       (put-char! canvas row (+ (long col) at) (.charAt text at) tone)))))

(defn put-line!
  "Merge direction bits into one cell; a thicker style wins the joint."
  ([canvas row col bits style] (put-line! canvas row col bits style nil))
  ([canvas row col bits style tone]
   (when (inside? canvas row col)
     (let [^ints bit-row
           (aget ^"[[I" (:bits canvas) (long row))

           ^ints style-row
           (aget ^"[[I" (:styles canvas) (long row))

           col
           (long col)]

       (aset-int bit-row col (bit-or (aget bit-row col) (long bits)))
       (aset-int style-row col (max (aget style-row col) (long (style-code style 0))))
       (when tone (set-tone! canvas row col tone))))))

(defn canvas->rows
  "ANSI rows of `canvas`, without blank rows at the top or the bottom."
  [canvas]
  (let [{:keys [rows cols chars bits styles tones]} canvas]
    (->> (range rows)
         (mapv
           (fn [row]
             (let [^chars char-row (aget ^"[[C" chars row)
                   ^ints bit-row (aget ^"[[I" bits row)
                   ^ints style-row (aget ^"[[I" styles row)
                   ^ints tone-row (aget ^"[[I" tones row)
                   cells (for [col (range cols)]
                           (let [ch (aget char-row col)
                                 drawn (aget bit-row col)
                                 line? (and (= \space ch) (pos? drawn))
                                 ch (if line?
                                      (get (style-glyphs (code-style (aget style-row col) :solid))
                                           drawn
                                           \space)
                                      ch)
                                 tone (long (aget tone-row col))
                                 tone (cond (pos? tone) tone
                                            (or line? (contains? chrome-glyphs ch)) 90
                                            :else 0)]

                             [ch (if (= \space ch) 0 tone)]))]

               (seg-row (map (fn [run]
                               [(apply str (map first run)) (second (first run))])
                             (partition-by second cells))))))
         (drop-while str/blank?)
         (reverse)
         (drop-while str/blank?)
         (reverse)
         vec)))
