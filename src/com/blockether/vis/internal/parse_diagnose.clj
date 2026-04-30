(ns com.blockether.vis.internal.parse-diagnose
  "Cheap, pure heuristics that turn an opaque edamame parse error
   into a more actionable diagnostic.

   Why a separate ns: the iteration loop already runs every code
   block through edamame, parinfer, and the extension rescue chain,
   so adding more diagnostic logic inline in `internal/loop.clj`
   buries the intent. Keeping these helpers next to a test file
   lets us pin specific real-world LLM mistakes (see the
   conversation-derived regression in the test ns) and grow the
   diagnostic catalogue independently of the iteration runtime.

   Today the catalogue covers ONE class of error — the most common
   one we've seen in real agent traces: UNBALANCED DOUBLE-QUOTE
   STRING. The model writes a multi-line `(answer (md/join …))` and
   accidentally drops or doubles a `\\` + `\"` escape somewhere in
   the middle. edamame's response is `Invalid symbol: <text>` at a
   row far BELOW the actual broken line — because the unbalanced
   quote hijacks chunks of source as string content, and the reader
   only notices when the hijacked content stops resembling a valid
   token. See conversation `cf9e29b5-…`'s last answer (pinned in
   `parse-diagnose-test`) for the canonical reproduction.

   Diagnostic API:

     (diagnose-quote-balance code)
       returns nil when the unescaped quote count is even.
       returns a map otherwise:
         {:reason :unbalanced-quote
          :total  N        total unescaped quote count
          :line   R        first 1-based line where the running
                           count was odd at end-of-line
          :hint   \"...\"  ready-to-show explanation the loop
                           appends to the original parse error}

   Pure: no side effects, no I/O. Safe to call on every parse-error
   path because the cost is one byte-level walk over the source."
  (:require
   [clojure.string :as str]))

(defn count-unescaped-quotes
  "Count double-quote characters in `s` that are NOT preceded by a
   backslash (which would escape them). Each scan also skips the
   character following a backslash so a `\\\\` pair doesn't
   accidentally swallow the quote that follows.

   Returns 0 for nil/empty input."
  ^long [^String s]
  (if (or (nil? s) (zero? (.length s)))
    0
    (loop [i 0
           n 0]
      (cond
        (>= i (.length s)) n

        (= \\ (.charAt s i))
        ;; Skip the escaped char so an escaped quote doesn't count,
        ;; and a literal-backslash pair doesn't swallow the quote
        ;; that follows it.
        (recur (+ i 2) n)

        (= \" (.charAt s i))
        (recur (inc i) (inc n))

        :else
        (recur (inc i) n)))))

(defn first-odd-quote-line
  "Walk `code` line-by-line tracking the running unescaped-quote
   count. Return the 1-based line where the count first becomes odd
   at end-of-line, or nil if the count is even at every newline.

   This pinpoints the line where the user MOST LIKELY introduced an
   unbalanced quote: even if the parse error reports a row far
   below, the rebalanced state breaks here.

   Edge case: a multi-line string spans multiple lines BY DESIGN
   (e.g. a triple-quoted literal in some macro). For those the
   running count goes odd on the line that opens the literal and
   even on the line that closes it. We flag the first odd-at-EOL
   line, which is the open. The CALLER decides whether that's a
   bug — we just surface the location."
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
  "If `code` has an odd number of unescaped quote chars, return a
   diagnostic map; otherwise nil.

   See ns docstring for the shape."
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
                   " never closes, so the reader walks past your"
                   " intended close-quote and treats the surrounding"
                   " code as bare tokens. Look for an extra OR"
                   " missing escape near line " line-str ".")}))))
