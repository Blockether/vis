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
   STRING. The model writes a multi-line `(answer (v/join …))` and
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

;; -----------------------------------------------------------------------------
;; Auto-repair — the parinfer-equivalent for unbalanced strings.
;; -----------------------------------------------------------------------------
;;
;; The same intuition that powers parinfer for parens applies to
;; quotes: if the user's source has an extra OR a missing `"`,
;; ONE local edit on the suspect line is overwhelmingly likely to
;; restore balance. We don't need a fancy parser — a small search
;; over plausible edits is plenty.
;;
;; Strategy:
;;
;;   1. If quote count is even, do nothing.
;;   2. Find the line where running count first goes odd.
;;   3. Generate candidate repairs:
;;        a. Remove each unescaped `"` on that line, ONE AT A TIME.
;;           Handles the "extra `\"`" case — exactly what conversation
;;           cf9e29b5 hit (LLM dropped a `\"` escape and got a stray
;;           bare `"` instead).
;;        b. Append `"` at end of that line.
;;           Handles the "missing close-quote" case — less common
;;           in practice but cheap to try.
;;   4. Each candidate goes through `parse-ok?` (caller-supplied
;;      — typically `(comp boolean #(try (edamame/parse-string-all
;;      % opts) true (catch _ false)))`).
;;   5. First candidate that parses wins.
;;   6. No winner -> nil. The loop falls back to surfacing the
;;      original error + the diagnostic hint.
;;
;; Constraint: we ALWAYS prefer a repair that keeps quote-count
;; even. A repair that just makes parsing succeed by another path
;; (e.g. by deleting a non-quote token) is out of scope.

(defn- positions-of-unescaped-quotes
  "Return the byte positions (0-based, inclusive) of every unescaped
   `\"` in `s`, in source order. Used by the repair search to
   enumerate candidate single-`\"` deletions."
  [^String s]
  (if (or (nil? s) (zero? (.length s)))
    []
    (loop [i 0
           acc (transient [])]
      (cond
        (>= i (.length s)) (persistent! acc)

        (= \\ (.charAt s i))
        (recur (+ i 2) acc)

        (= \" (.charAt s i))
        (recur (inc i) (conj! acc i))

        :else
        (recur (inc i) acc)))))

(defn- line-bounds
  "Return `[line-start line-end-exclusive]` byte indices for the
   1-based `line-no` in `code`. Used to scope the repair search to
   the suspect line only."
  [^String code line-no]
  (let [code-len (.length code)]
    (loop [i 0
           current-line 1
           line-start 0]
      (cond
        (>= i code-len)
        (when (= current-line line-no) [line-start code-len])

        (and (= \newline (.charAt code i)) (= current-line line-no))
        [line-start i]

        (= \newline (.charAt code i))
        (recur (inc i) (inc current-line) (inc i))

        :else
        (recur (inc i) current-line line-start)))))

(defn- candidate-removals
  "Generate every \"remove a single unescaped `\"` somewhere on the
   suspect line\" candidate. Returns a vec of String variants.
   Each variant has exactly one fewer `\"` than `code`."
  [^String code line-no]
  (when-let [[lo hi] (line-bounds code line-no)]
    (let [line-text (subs code lo hi)
          rel-positions (positions-of-unescaped-quotes line-text)]
      (mapv (fn [rel-pos]
              (let [abs-pos (+ lo (long rel-pos))]
                (str (subs code 0 abs-pos)
                  (subs code (inc abs-pos)))))
        rel-positions))))

(defn- candidate-append
  "One repair candidate: append a closing `\"` at end of the suspect
   line. Returns String or nil when the line can't be located."
  [^String code line-no]
  (when-let [[_ hi] (line-bounds code line-no)]
    (str (subs code 0 hi) "\"" (subs code hi))))

(defn try-quote-rebalance
  "Parinfer-equivalent for unbalanced double-quotes.

   Walks `code`, finds the first odd-quote line, and tries small
   local edits (remove a stray `\"` from that line; append a missing
   `\"` at end of line). Each candidate is fed to the caller's
   `parse-ok?` predicate; the first candidate that returns truthy
   wins.

   `parse-ok?`: `(fn [src] -> bool)` — typically a wrapper around
   `edamame/parse-string-all` that returns true on success, false
   on any throw. Pure: callers stay in control of the parser opts.

   Returns the rebalanced source string, or nil when:
     - the source is already balanced, OR
     - none of the local-edit candidates produce a parsable variant.

   Pure. No side effects, no I/O. Stable ordering of candidates so
   tests can pin behaviour."
  [^String code parse-ok?]
  (when (and code (odd? (count-unescaped-quotes code)))
    (when-let [line (first-odd-quote-line code)]
      (let [removals (candidate-removals code line)
            append-c (candidate-append code line)
            ;; Removals first — the "extra `\"`" case is
            ;; overwhelmingly more common in real LLM mistakes than
            ;; the "missing close-quote" case (which would normally
            ;; have crashed at the same line, not later).
            candidates (cond-> (vec removals)
                         append-c (conj append-c))]
        (some #(when (parse-ok? %) %) candidates)))))
