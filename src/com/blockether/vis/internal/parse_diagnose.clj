(ns com.blockether.vis.internal.parse-diagnose
  "Cheap heuristics that turn opaque parse / eval errors into precise
   diagnostic hints. Engine wires the catalogue into the per-form trailer so
   the model sees actionable repair instructions instead of a stack trace.

   Catalogue today:

     diagnose-quote-balance
       Odd number of unescaped double quotes in the source. Pinpoints the
       1-based line where the running count first becomes odd. The Clojure
       surface's delimiter repair does indent-mode paren balancing only;
       string-quote imbalance needs its own walker.

     diagnose-bracket-balance
       Unbalanced (), [], {}. Walks a bracket stack skipping string/char
       literals (incl. triple-quoted, with escapes) and `#` comments; reports
       the FIRST wrong-type / extra / unclosed bracket + 1-based line/col.
       Python's mixed (), [], {} are NOT indentation-determined, so parinfer
       (the Clojure paren repairer) cannot be reused here."
  (:require [clojure.string :as str]))

;; Quote balance

(defn count-unescaped-quotes
  "Count double-quote characters in `s` that are NOT preceded by a backslash.
   Also skips the character following a backslash so `\\\\` (escaped
   backslash) doesn't swallow the quote that follows it."
  ^long [^String s]
  (if (or (nil? s) (zero? (.length s)))
    0
    (loop [i
           0

           n
           0]

      (cond (>= i (.length s)) n
            (= \\ (.charAt s i)) (recur (+ i 2) n)
            (= \" (.charAt s i)) (recur (inc i) (inc n))
            :else (recur (inc i) n)))))

(defn first-odd-quote-line
  "Walk `code` line-by-line tracking the running unescaped-quote count.
   Returns the 1-based line where the count first becomes odd at end-of-line,
   or nil if the count is even at every newline. This is the line where the
   user most likely introduced an unbalanced quote — even when the reader's
   error reports a row far below."
  [^String code]
  (let [lines (str/split-lines (or code ""))]
    (loop [i 0
           running 0]

      (when (< i (count lines))
        (let [line (nth lines i)
              cnt (count-unescaped-quotes line)
              total (+ running cnt)]

          (if (odd? total) (inc i) (recur (inc i) total)))))))

(defn diagnose-quote-balance
  "If `code` has an odd number of unescaped quote chars, return a diagnostic
   map shaped `{:reason :unbalanced-quote :total N :line R :hint \"…\"}`.
   Returns nil for balanced sources."
  [^String code]
  (let [total (count-unescaped-quotes code)]
    (when (odd? total)
      (let [line (first-odd-quote-line code)
            line-str (or (str line) "?")]

        {:reason :unbalanced-quote
         :total total
         :line line
         :hint (str "Unbalanced double-quote: "
                    total
                    " unescaped quote chars in this form (odd count). "
                    "A string opened around line "
                    line-str
                    " never closes, so the reader treats the surrounding "
                    "code as bare tokens. Look for an extra OR missing "
                    "escape near line "
                    line-str
                    ".")}))))

(defn diagnose-bracket-balance
  "Walk `code` with a bracket stack over (), [], {} - skipping characters
   inside string/char literals (single, double, and triple-quoted, honoring
   backslash escapes) and `#` comments. Returns a diagnostic map shaped
   `{:reason :unbalanced-bracket :open C :close C :line R :col K :hint \"...\"}`
   for the FIRST imbalance found, or nil when every bracket pairs up.

   Three failure shapes:
     - a closer of the WRONG type        (`:open` is what was open, `:close` the found closer)
     - an EXTRA closer with nothing open (`:open` nil)
     - an UNCLOSED opener at end-of-input (`:close` nil)

   Heuristic only: Python's mixed (), [], {} are NOT indentation-determined, so
   parinfer (Clojure indent-mode) cannot be reused; this just pinpoints the
   first place the nesting goes wrong so the model can repair it."
  [^String code]
  (let [^String s
        (or code "")

        n
        (.length s)

        closer->opener
        {\) \( \] \[ \} \{}

        opener->closer
        {\( \) \[ \] \{ \}}

        openers
        #{\( \[ \{}

        closers
        #{\) \] \}}

        triple-at?
        (fn [^long i ch]
          (let [ch (char ch)]
            (and (< (+ i 2) n)
                 (= ch (.charAt s i))
                 (= ch (.charAt s (inc i)))
                 (= ch (.charAt s (+ i 2))))))]

    (loop [i
           0

           line
           1

           col
           1

           stack
           []

           mode
           :code

           sdelim
           nil

           striple?
           false

           escaped?
           false]

      (if (>= i n)
        (when-let [top (peek stack)]
          {:reason :unbalanced-bracket
           :open (:ch top)
           :close nil
           :line (:line top)
           :col (:col top)
           :hint (str "Unclosed `"
                      (:ch top)
                      "` opened at line "
                      (:line top)
                      ", col "
                      (:col top)
                      " - it never closes. Add the matching `"
                      (opener->closer (:ch top))
                      "`.")})
        (let [c
              (.charAt s i)

              nl?
              (= c \newline)

              nline
              (if nl? (inc line) line)

              ncol
              (if nl? 1 (inc col))]

          (case mode
            :comment
            (recur (inc i) nline ncol stack (if nl? :code :comment) sdelim striple? false)

            :string
            (cond escaped? (recur (inc i) nline ncol stack :string sdelim striple? false)
                  (= c \\) (recur (inc i) nline ncol stack :string sdelim striple? true)
                  (and striple? (triple-at? i sdelim))
                  (recur (+ i 3) line (+ col 3) stack :code nil false false)
                  (and (not striple?) (= c (char sdelim)))
                  (recur (inc i) nline ncol stack :code nil false false)
                  ;; a non-triple string cannot span a raw newline; bail back to
                  ;; :code so a stray quote can't swallow the rest of the source.
                  (and (not striple?) nl?) (recur (inc i) nline ncol stack :code nil false false)
                  :else (recur (inc i) nline ncol stack :string sdelim striple? false))

            :code
            (cond (= c \#) (recur (inc i) nline ncol stack :comment sdelim striple? false)
                  (or (= c \") (= c \')) (if (triple-at? i c)
                                           (recur (+ i 3) line (+ col 3) stack :string c true false)
                                           (recur (inc i) nline ncol stack :string c false false))
                  (openers c) (recur (inc i)
                                     nline
                                     ncol
                                     (conj stack {:ch c :line line :col col})
                                     :code
                                     sdelim
                                     striple?
                                     false)
                  (closers c)
                  (let [top (peek stack)]
                    (cond (nil? top) {:reason :unbalanced-bracket
                                      :open nil
                                      :close c
                                      :line line
                                      :col col
                                      :hint (str "Unexpected `" c
                                                 "` at line " line
                                                 ", col " col
                                                 " - there is no open bracket for it to close. "
                                                 "Remove it, or add the matching opener earlier.")}
                          (not= (closer->opener c) (:ch top)) {:reason :unbalanced-bracket
                                                               :open (:ch top)
                                                               :close c
                                                               :line line
                                                               :col col
                                                               :hint (str
                                                                       "Mismatched bracket: `"
                                                                       c
                                                                       "` at line "
                                                                       line
                                                                       ", col "
                                                                       col
                                                                       " tries to close `"
                                                                       (:ch top)
                                                                       "` opened at line "
                                                                       (:line top)
                                                                       ", col "
                                                                       (:col top)
                                                                       " - expected `"
                                                                       (opener->closer (:ch top))
                                                                       "` instead.")}
                          :else (recur (inc i) nline ncol (pop stack) :code sdelim striple? false)))
                  :else (recur (inc i) nline ncol stack :code sdelim striple? false))))))))
