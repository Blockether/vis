(ns com.blockether.vis.internal.parse-diagnose
  "Cheap, pure heuristics that turn an opaque edamame parse error
   into a more actionable diagnostic.

   Why a separate ns: the iteration loop already runs every code
   block through edamame, parinfer, and the extension rescue chain,
   so adding more diagnostic logic inline in `internal/loop.clj`
   buries the meaning. Keeping these helpers next to a test file
   lets us pin specific real-world LLM mistakes (see the
   conversation-derived regression in the test ns) and grow the
   diagnostic catalogue independently of the iteration runtime.

   Today the catalogue covers ONE class of error - the most common
   one we've seen in real agent traces: UNBALANCED DOUBLE-QUOTE
   STRING. The model writes a multi-line `(turn-answer! (v/join ...))` and
   accidentally drops or doubles a `\\` + `\"` escape somewhere in
   the middle. edamame's response is `Invalid symbol: <text>` at a
   row far BELOW the actual broken line - because the unbalanced
   quote hijacks chunks of source as string content, and the reader
   only notices when the hijacked content stops resembling a valid
   token. See conversation `cf9e29b5-...`'s last answer (pinned in
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
   bug - we just surface the location."
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
;; Auto-repair - the parinfer-equivalent for unbalanced strings.
;; -----------------------------------------------------------------------------
;;
;; The same intuition that powers parinfer for parens applies to
;; quotes: if the user's source has an extra OR a missing `"`,
;; ONE local edit on the suspect line is overwhelmingly likely to
;; restore balance. We don't need a fancy parser - a small search
;; over plausible edits is plenty.
;;
;; Strategy:
;;
;;   1. If quote count is even, do nothing.
;;   2. Find the line where running count first goes odd.
;;   3. Generate candidate repairs:
;;        a. Remove each unescaped `"` on that line, ONE AT A TIME.
;;           Handles the "extra `\"`" case - exactly what conversation
;;           cf9e29b5 hit (LLM dropped a `\"` escape and got a stray
;;           bare `"` instead).
;;        b. Append `"` at end of that line.
;;           Handles the "missing close-quote" case - less common
;;           in practice but cheap to try.
;;   4. Each candidate goes through `parse-ok?` (caller-supplied
;;      - typically `(comp boolean #(try (edamame/parse-string-all
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

;; -----------------------------------------------------------------------------
;; Eval-time diagnostic: `Unable to resolve symbol: X`
;;
;; Class of LLM mistake: model writes a vector of strings inside
;; `(turn-answer! (v/join (v/ul [...])))` and forgets the opening `"`
;; on one element. The bare prose still parses (Unicode words are
;; legal Clojure symbols), but SCI throws `Unable to resolve
;; symbol: <FirstWord>` at eval-time. Edamame is happy. The repair
;; pipeline never fires because there's no parse error.
;;
;; This diagnostic doesn't auto-repair - the safe move at this
;; level is to enrich the error message so the model sees WHY the
;; symbol was unresolved (it's prose-shaped, not a real binding).
;; Auto-repair scoped to `(turn-answer! ...)` is a separate concern and
;; lives outside this ns.
;;
;; Source: conversation ec64266c-...'s turn 2 (iter 0+1) where the
;; model failed twice on the same pattern and recovered on iter 2
;; only after switching to ASCII-only prose.
;; -----------------------------------------------------------------------------

(defn- looks-like-prose?
  "Heuristic: does `s` look like a word from natural-language prose
   rather than a Clojure identifier? Yes when:
     - first char is an upper-case letter, AND
     - at least one char is a non-ASCII letter (Polish, Czech,
       German, etc.).
   Catches `Szerokość`, `Niektóre`, `Różne`, `Wszystkie`. Skips
   ASCII Java classes (`String`, `IllegalArgumentException`) and
   plain kebab-case Clojure idents."
  [^String s]
  (and (string? s)
    (>= (count s) 3)
    (let [c0 (.charAt s 0)]
      (and (Character/isLetter c0)
        (Character/isUpperCase c0)
        (loop [i 0]
          (cond
            (>= i (.length s)) false
            (and (Character/isLetter (.charAt s i))
              (> (int (.charAt s i)) 127)) true
            :else (recur (inc i))))))))

(defn- likely-missing-quote-context?
  "Walk backwards from `idx` (where `sym` starts in `source`) past
   whitespace and check for one of two signatures of an unquoted
   prose element inside a vector of strings:

     a. `\"prev-string\"\n   X`  - newline + indent right after a
        closing quote (the model started a new line and forgot the
        leading `\"`).
     b. `\"prev-string\" X`     - same line, single space between
        previous element and X.

   Returns true on either pattern."
  [^String source ^long idx]
  (let [start (max 0 (- idx 200))
        before (subs source start idx)]
    (boolean
      (or
        (re-find #"\"\s*\R\s*$" before)
        (re-find #"\"[ \t]+$" before)))))

(defn unresolved-symbol-hint
  "When SCI surfaces `Unable to resolve symbol: X` and `X` looks
   like prose sitting inside a vector of strings, return a string
   to APPEND to the original error so the next iteration sees a
   concrete suggestion (\"missing opening quote\") instead of the
   bare 'symbol unresolved' message.

   Returns nil when:
     - the error message isn't an `Unable to resolve symbol: X`,
     - X doesn't look like prose, OR
     - X doesn't sit in a missing-quote-shaped context.

   Pure. Does NOT mutate source. Caller is responsible for
   appending the hint to the error message it surfaces to the
   model."
  [^String error-msg ^String source]
  (when (and (string? error-msg) (string? source))
    (when-let [sym (some-> (re-find #"Unable to resolve symbol: (\S+)" error-msg)
                     second)]
      (when (and (looks-like-prose? sym)
              (when-let [idx (str/index-of source sym)]
                (likely-missing-quote-context? source (long idx))))
        (str
          " -- HINT: '" sym "' looks like an unquoted string fragment."
          " Did you forget an opening `\"` before it? Common shape:"
          " `(v/ul [\"a\" \"b\" " sym " ...])` - the third element is"
          " missing its opening quote, so SCI sees `" sym "` as a"
          " bare symbol and fails to resolve it. Wrap that element"
          " as a string literal.")))))

;; -----------------------------------------------------------------------------
;; Eval-time auto-repair scoped to `(turn-answer! ...)` forms.
;;
;; Why scoped to `answer`: it's the terminal sink of the RLM loop.
;; A failed `(turn-answer! ...)` burns an iteration, the cost-USD on the
;; LLM call, and the user's wall-clock - and the user is staring
;; at a spinner while the model regenerates the same long markdown
;; payload. Inside `(turn-answer! ...)` the body is by contract
;; markdown-shaped prose, NOT computation - so a bare-prose symbol
;; in a `v/ul`/`v/p`/etc vector of strings has no legitimate use
;; case. We can rewrite safely.
;;
;; Outside `(turn-answer! ...)` we do NOT rewrite: a bare Unicode symbol
;; in regular code might be the intended binding (rare but legal),
;; and silently coercing it to a string would mask real bugs.
;;
;; Strategies (in order):
;;   A. Prepend `\"` before X. Works when there's no inner unescaped
;;      `\"` between X and the enclosing `]`/`)`.
;;   B. Prepend `\"` AND escape every unescaped `\"` inside the span
;;      EXCEPT the last one (which becomes the natural closer).
;;      Handles iter-0/iter-1 from conversation ec64266c-... where
;;      the prose contained an inner `\"` (`„zjadać\"`).
;;
;; Pure: this fn returns candidates only. The caller (`loop/execute-code`)
;; re-evaluates each through SCI and picks the first that succeeds,
;; so a bad candidate just means we move on. There's no way for
;; this fn alone to corrupt anything.
;; -----------------------------------------------------------------------------

(defn- find-enclosing-close-pos
  "Walk forward from `idx` in `source` and return the byte position
   of the FIRST `]` or `)` we hit at one shallower nesting level
   than where we started. Treats string content (between unescaped
   `\"` ... `\"`) as opaque so quotes inside strings don't confuse
   nesting. Returns nil if no such close exists before EOF."
  ^Long [^String source ^long idx]
  (let [n (.length source)]
    (loop [i idx
           depth 0
           in-string? false]
      (cond
        (>= i n) nil

        (and in-string? (= \\ (.charAt source i)))
        (recur (+ i 2) depth in-string?)

        (= \" (.charAt source i))
        (recur (inc i) depth (not in-string?))

        in-string?
        (recur (inc i) depth in-string?)

        (or (= \[ (.charAt source i))
          (= \( (.charAt source i)))
        (recur (inc i) (inc depth) in-string?)

        (or (= \] (.charAt source i))
          (= \) (.charAt source i)))
        (if (zero? depth)
          i
          (recur (inc i) (dec depth) in-string?))

        :else
        (recur (inc i) depth in-string?)))))

(defn- restitch-candidates
  "Generate ordered candidate source strings that wrap a bare prose
   symbol starting at `idx` as a string literal. Each candidate has
   ONE local edit; nothing outside `[idx, span-end]` changes.

   See ns-level comment block above for strategy rationale."
  [^String source ^long idx]
  (when-let [span-end (find-enclosing-close-pos source idx)]
    (let [span-text   (subs source idx span-end)
          q-positions (positions-of-unescaped-quotes span-text)
          prefix      (subs source 0 idx)
          suffix      (subs source span-end)
          strategy-a  (str prefix "\"" span-text suffix)
          strategy-b  (when (seq q-positions)
                        (let [last-q (long (last q-positions))
                              n      (.length ^String span-text)
                              sb     (StringBuilder.)]
                          (.append sb prefix)
                          (.append sb \")
                          (loop [i 0]
                            (when (< i n)
                              (let [c (.charAt ^String span-text i)]
                                (cond
                                  (= \\ c)
                                  (do (.append sb c)
                                    (when (< (inc i) n)
                                      (.append sb (.charAt ^String span-text (inc i))))
                                    (recur (+ i 2)))

                                  (and (= \" c) (not= i last-q))
                                  (do (.append sb \\)
                                    (.append sb \")
                                    (recur (inc i)))

                                  :else
                                  (do (.append sb c) (recur (inc i)))))))
                          (.append sb suffix)
                          (.toString sb)))]
      ;; Strategy C: prepend `"` AND insert a closing `"` right
      ;; before the enclosing `)` / `]`. Used when there is no
      ;; existing unescaped `"` between the bare ident and the
      ;; close — strategies A and B both rely on an existing inner
      ;; quote to land the close, so without one they generate
      ;; unparseable candidates. Conv a1ccbb8c shape
      ;; `(turn-answer! Hi there)` is the canonical repro.
      (cond-> [strategy-a]
        strategy-b (conj strategy-b)
        ;; Strategy C only when there's no inner unescaped `"` to
        ;; reuse as the closer (otherwise A or B already covers it).
        (empty? q-positions)
        (conj (str prefix "\"" span-text "\"" suffix))))))

(defn- bare-symbol-leads-answer?
  "True when `source` opens with `(turn-answer! SYM` where the first
   non-whitespace token after `(turn-answer! ` is a bare Clojure
   identifier (not a string literal, not an opening paren, not a
   keyword, not a number).

   This is the strongest possible signal that the model forgot the
   opening `\"` on the answer body — the answer-form arity contract
   is `(turn-answer! <markdown-string-or-renderable>)` and a bare symbol
   in the first slot has no legitimate use case (no plain symbol is
   `:vis/answer`-shaped). When this fires we can restitch even when
   the symbol fails the broader `looks-like-prose?` heuristic
   (e.g. short Polish/English words like `Co`, `Hi`, `Ok` — those
   would otherwise slip past the prose detector and burn a whole
   iteration on `Unable to resolve symbol: Co`,
   conversation a1ccbb8c-a1a3-434c-86ad-b0f79cd2dee8).

   Pure regex check. Returns the byte offset of the bare symbol's
   first char (so callers can hand it straight to
   `restitch-candidates`) or nil when the pattern doesn't match."
  ^Long [^String source ^String sym]
  (when (and source sym)
    (let [;; `(turn-answer!` followed by required whitespace, then a
          ;; capture group around the bare-ident shape we want to
          ;; promote to a string. We anchor on `\b` so partial
          ;; matches inside identifiers don't fire. The ident must
          ;; START with a letter — numbers / keywords / strings are
          ;; out of scope and either parse fine or have their own
          ;; restitch path.
          quoted-sym (java.util.regex.Pattern/quote sym)
          pat        (re-pattern (str "\\(turn-answer!\\s+(" quoted-sym ")\\b"))
          m          (re-matcher pat source)]
      (when (.find m)
        (long (.start m 1))))))

(defn try-answer-string-restitch
  "Repair-candidate generator for the eval-time error class
   `Unable to resolve symbol: X` when X is a bare word that was
   supposed to be a string literal inside an `(turn-answer! ...)` form.

   Two trigger paths:

   1. **Prose-shape inside a string-vector** (the classic
      conv ec64266c-... case) — X looks like prose
      (`looks-like-prose?`) and sits in a missing-quote-shaped
      context (`likely-missing-quote-context?`). Catches multi-word
      Polish/German/etc. fragments inside `(v/ul [\"a\" \"b\" Niektore
      ...])`-shaped vectors.

   2. **Bare leading symbol in `(turn-answer! SYM ...)`** (conv
      a1ccbb8c-a1a3-434c-86ad-b0f79cd2dee8) — X is the very first
      token after `(turn-answer! `. The arity contract guarantees there's
      no legitimate symbol there, so we restitch unconditionally,
      bypassing the prose-shape filter. Catches short fragments
      (`Co`, `Hi`, `Ok`) that the prose detector — intentionally
      conservative for the vector-of-strings case — would skip.

   Returns a vector of source candidates in priority order, or nil
   when neither trigger fires or the enclosing `]`/`)` cannot be
   located.

   Pure. The caller is responsible for re-evaluating each candidate
   through SCI and picking the first that succeeds. A candidate
   that re-throws is harmless — it's just discarded."
  [^String source ^String sym]
  (when (and (string? source)
          (string? sym)
          (str/includes? source "(turn-answer!"))
    (let [;; Path 2 first: it's strictly more specific (the symbol
          ;; must literally lead an answer form). When it matches,
          ;; we know the offset already, no fuzzy index-of needed.
          leading-idx (bare-symbol-leads-answer? source sym)]
      (cond
        leading-idx
        (let [cands (restitch-candidates source leading-idx)]
          (when (seq cands) cands))

        (looks-like-prose? sym)
        (when-let [idx (str/index-of source sym)]
          (when (likely-missing-quote-context? source (long idx))
            (let [cands (restitch-candidates source (long idx))]
              (when (seq cands) cands))))))))

(defn try-quote-rebalance
  "Parinfer-equivalent for unbalanced double-quotes.

   Walks `code`, finds the first odd-quote line, and tries small
   local edits (remove a stray `\"` from that line; append a missing
   `\"` at end of line). Each candidate is fed to the caller's
   `parse-ok?` predicate; the first candidate that returns truthy
   wins.

   `parse-ok?`: `(fn [src] -> bool)` - typically a wrapper around
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
            ;; Removals first - the "extra `\"`" case is
            ;; overwhelmingly more common in real LLM mistakes than
            ;; the "missing close-quote" case (which would normally
            ;; have crashed at the same line, not later).
            candidates (cond-> (vec removals)
                         append-c (conj append-c))]
        (some #(when (parse-ok? %) %) candidates)))))
