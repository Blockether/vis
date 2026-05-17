(ns com.blockether.vis.internal.parse-diagnose
  "Cheap heuristics that turn opaque parse / eval errors into precise
   diagnostic hints. Engine wires the catalogue into the per-form trailer so
   the model sees actionable repair instructions instead of a stack trace.

   Catalogue today:

     diagnose-quote-balance
       Odd number of unescaped double quotes in the source. Pinpoints the
       1-based line where the running count first becomes odd. parinferish
       does indent-mode paren balancing only; string-quote imbalance needs
       its own walker.

     unresolved-symbol-hint
       SCI eval threw 'Unable to resolve symbol: X'. Suggests the closest
       symbol(s) in the user's sandbox bindings (Levenshtein-style score),
       so the model sees 'did you mean ...?' instead of 'X is undefined'."
  (:require
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Quote balance
;; ---------------------------------------------------------------------------

(defn count-unescaped-quotes
  "Count double-quote characters in `s` that are NOT preceded by a backslash.
   Also skips the character following a backslash so `\\\\` (escaped
   backslash) doesn't swallow the quote that follows it."
  ^long [^String s]
  (if (or (nil? s) (zero? (.length s)))
    0
    (loop [i 0
           n 0]
      (cond
        (>= i (.length s)) n

        (= \\ (.charAt s i))
        (recur (+ i 2) n)

        (= \" (.charAt s i))
        (recur (inc i) (inc n))

        :else
        (recur (inc i) n)))))

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
        (let [line  (nth lines i)
              cnt   (count-unescaped-quotes line)
              total (+ running cnt)]
          (if (odd? total)
            (inc i)
            (recur (inc i) total)))))))

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
         :total  total
         :line   line
         :hint   (str
                   "Unbalanced double-quote: " total
                   " unescaped quote chars in this form (odd count). "
                   "A string opened around line " line-str
                   " never closes, so the reader treats the surrounding "
                   "code as bare tokens. Look for an extra OR missing "
                   "escape near line " line-str ".")}))))

;; ---------------------------------------------------------------------------
;; Unresolved symbol
;; ---------------------------------------------------------------------------

(defn- levenshtein
  "Simple Levenshtein distance. Capped at 100-char input pair."
  ^long [^String a ^String b]
  (let [la (count a)
        lb (count b)]
    (cond
      (or (> la 100) (> lb 100)) Long/MAX_VALUE
      (zero? la) lb
      (zero? lb) la
      :else
      (let [row0 (long-array (inc lb))]
        (dotimes [j (inc lb)] (aset row0 j j))
        (loop [i 1
               prev row0]
          (if (> i la)
            (aget prev lb)
            (let [curr (long-array (inc lb))
                  ai   (.charAt a (dec i))]
              (aset curr 0 i)
              (loop [j 1]
                (when (<= j lb)
                  (let [bj   (.charAt b (dec j))
                        cost (if (= ai bj) 0 1)
                        a-val (+ (aget prev j) 1)
                        b-val (+ (aget curr (dec j)) 1)
                        c-val (+ (aget prev (dec j)) cost)
                        m     (min a-val b-val c-val)]
                    (aset curr j m)
                    (recur (inc j)))))
              (recur (inc i) curr))))))))

(defn unresolved-symbol-hint
  "When an eval error message is shaped 'Unable to resolve symbol: foo',
   propose the closest sandbox symbol(s) as a hint. `candidates` is a coll of
   symbols / strings (e.g. keys of the live sandbox map). Returns nil when
   the error does not look like an unresolved-symbol failure, or when no
   close match exists."
  [error-message candidates]
  (when (string? error-message)
    (when-let [[_ missing] (re-find #"Unable to resolve symbol[:\s]+([^\s,]+)"
                             error-message)]
      (let [missing-str (str missing)
            scored      (->> (or candidates [])
                          (map str)
                          (remove str/blank?)
                          (map (fn [c] [c (levenshtein missing-str c)]))
                          (filter (fn [[_ d]] (<= d 3)))
                          (sort-by second)
                          (take 3)
                          (map first))]
        (when (seq scored)
          (str "Unresolved `" missing-str "`. Closest defined: "
            (str/join ", " (map #(str "`" % "`") scored))
            "."))))))
