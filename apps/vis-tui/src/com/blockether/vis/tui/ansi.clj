(ns com.blockether.vis.tui.ansi
  "Paint text that carries ANSI SGR colour escapes. The code formatter and git
   give such text. An SGR foreground code selects a colour of the Vis theme;
   the caller owns the background.

   The column maths of SGR text (`ansi-truncate-cols`, `ansi-fold-cols`,
   `ansi-slice-cols`, `strip-sgr`) is in the primitives namespace."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna InlineSpans]
           [com.googlecode.lanterna.graphics TextGraphics]))

(defn- code->fg
  [code current-fg base-fg]
  ;; `(long code)` is what keeps this a constant-time tableswitch: the tests are
  ;; int literals, so a boxed expression makes the compiler fall back to a linear
  ;; scan ("case has int tests, but tested expression is not primitive"). Codes
  ;; come from `parse-codes`, which only ever yields non-nil Longs.
  (case (long code)
    7
    t/result-highlight-fg

    0
    base-fg

    30
    t/code-block-fg

    31
    t/code-syntax-string-fg

    32
    t/code-success-fg

    33
    t/warning-fg

    34
    t/code-syntax-number-fg

    35
    t/code-syntax-special-fg

    36
    t/code-syntax-keyword-fg

    37
    t/text-fg

    90
    t/code-syntax-comment-fg

    91
    t/code-error-fg

    92
    t/code-success-fg

    current-fg))

(defn- parse-codes
  [s]
  (into []
        (keep (fn [part]
                (try (Long/parseLong part) (catch NumberFormatException _ nil))))
        (str/split (or s "") #";")))

(defn- codes->fg
  [codes current-fg base-fg]
  ;; An empty SGR (`ESC[m`, canonically ESC[0m) is a full reset — git's own
  ;; `%C(reset)` emits exactly that, so treat no codes as "back to base".
  (if (empty? codes)
    base-fg
    (reduce (fn [fg code]
              (if (= 0 code) base-fg (code->fg code fg base-fg)))
            current-fg
            codes)))

(defn paint-line!
  "Paint a possibly ANSI-colored zprint line onto a Lanterna surface.
   ANSI foreground codes are translated to Lanterna foreground colors;
   `bg` is always controlled by Vis so success/running/error code
   zones keep their own background.

   Tool render-fns return Markdown for result bodies. That Markdown can
   hold inline-span sentinels. They are paint directives, not glyphs: a
   chunk that has one goes through `p/paint-styled-line!`."
  ([^TextGraphics g x y ^String line base-fg bg]
   (paint-line! g x y line base-fg bg t/code-block-fg t/code-block-bg))
  ([^TextGraphics g x y ^String line base-fg bg code-fg code-bg]
   (letfn [(paint-chunk! [col ^String chunk fg]
             (p/set-colors! g fg bg)
             (if (InlineSpans/contains chunk)
               (p/paint-styled-line! g (+ (long x) (long col)) y chunk fg bg code-fg code-bg)
               (p/put-str! g (+ (long x) (long col)) y chunk)))]
     (loop [i
            0

            col
            0

            fg
            base-fg]

       (if (>= (long i) (.length line))
         g
         (let [esc-idx (str/index-of line "\u001b[" i)]
           (if (or (nil? esc-idx) (< (long i) (long esc-idx)))
             (let [end (or esc-idx (.length line))
                   chunk (subs line i end)]

               (paint-chunk! col chunk fg)
               (recur (long end) (+ (long col) (p/display-width chunk)) fg))
             (let [m-idx (str/index-of line "m" (+ (long esc-idx) 2))]
               (if (nil? m-idx)
                 (let [chunk (subs line esc-idx)]
                   (paint-chunk! col chunk fg)
                   g)
                 (let [codes (parse-codes (subs line (+ (long esc-idx) 2) m-idx))
                       fg* (codes->fg codes fg base-fg)]

                   (recur (inc (long m-idx)) col fg*)))))))))))
