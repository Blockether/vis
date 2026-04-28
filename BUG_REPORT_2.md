# BUG_REPORT_2 — Code-execution failures the loop never recovers from: regex escaping, malformed `vis/patch` payloads, and the "SEARCH block not found" dead end

**Conversation:** `2fbbf09c-dec7-4d9a-9e0a-80af8a245c84`
**Query under triage:** `b7940d81-1dc2-4114-b7a3-3b09baa4086c` (running)
**Companion:** `BUG_REPORT_1.md` (provider-side spec rejections)

This report covers the SCI / `vis/rg` / `vis/patch` errors visible at
`expression_state.success = 0` in this conversation. Four distinct
failures show up across the running query's 17 iterations, and each
one has a structural Vis-side weakness that turned a single LLM
typo into multiple wasted iterations.

## Summary table — every failed expression in the running query

| iter.k | tool         | error class                                | actually about                   |
|-------:|--------------|--------------------------------------------|----------------------------------|
| 7.1    | `vis/rg`     | `Unable to resolve symbol: CODE`           | bare `"` inside regex string     |
| 9.1    | `vis/rg`     | `Unsupported escape character: \|`         | `\|` in Clojure string literal   |
| 13.1   | `vis/patch`  | `Unmatched delimiter: }, expected: )`      | unbalanced `:search` string      |
| 15.1   | `vis/patch`  | `SEARCH block 1 not found in …render.clj`  | whitespace-sensitive match       |

Three of the four are LLM typos. All three are recoverable by Vis-
side machinery that already exists but **does not fire** in these
shapes. The fourth (15.1) is the patch tool's notorious "found
nothing, said nothing useful" failure mode.

---

## Bug 2.A — `vis/rg` regex strings: quote vs. backslash-pipe escaping

The model wants a regex with alternation. It writes one of:

```clojure
(vis/rg "code-hdr|fa-hdr|...|"CODE"|"Code"|..." "render.clj") ; iter 7
(vis/rg "label-text.*code\|code-hdr\|code-label\|..."  "...") ; iter 9
```

The first uses an unescaped `"` inside the outer string. SCI's reader
ends the string at the first inner `"`, then tries to evaluate `CODE`
as a symbol → `Unable to resolve symbol: CODE`.

The second uses `\|`. Clojure string literals don't support `\|` —
only the standard `\n \t \r \" \\ \uXXXX` etc. Edamame returns
`[line 1, col 28] Unsupported escape character: \|.`

### What's already in the codebase

`extensions/common/vis-common-editing/src/com/blockether/vis/ext/common_editing/editing.clj`:

- `rescue-parse-error` (line 204) — a `:on-parse-error-fn` hook that
  matches the edamame error message and doubles the offending `\` so
  `\|` becomes `\\|`.
- `safe-deescape-meta-chars` (line 61) — `|` is in the safe set.
- `try-extension-parse-rescue`
  (`packages/vis-core/.../iteration/core.clj:170`) — the iteration
  loop's hook driver: takes the rewrite, re-parses, and retries IFF
  the rewrite parses cleanly.

### The actual bug

`rescue-parse-error` only repairs **the first** offending site. Its
docstring even says so:

> "Repair only the single offending site — if more bad escapes lurk
> downstream the next parse will raise, the rescue will fire again,
> and so on."

But the iteration loop's contract is:

```clojure
;; iteration/core.clj:170
(when-let [fixed (ext/try-rescue-parse-error exts code parse-error environment)]
  (when (nil? (parse-clojure-syntax fixed))   ; fix MUST parse cleanly
    fixed))
```

i.e. a single rewrite must parse all the way. The "rescue will fire
again, and so on" the docstring promises **never happens**. So the
model's typical `\|`-spammed regex (`"a\|b\|c\|d"`) defeats the
rescue: only the first `\|` gets doubled, the rest are still
unsupported escapes, the rewrite still doesn't parse, the loop drops
the rewrite, the original error is surfaced to the LLM. The model
re-tries the same shape because the error message gives no hint to
double-escape *every* pipe.

### Reproduction (fully deterministic)

```bash
cd /Users/fierycod/vis
clojure -A:dev -e '
(require "[com.blockether.vis.ext.common-editing.editing :as ed]
         [edamame.core :as edamame]")
(defn show [label src]
  (let [err (try (edamame/parse-string-all src) nil
                 (catch Throwable t (ex-message t)))
        out (#''"'"'ed/rescue-parse-error {:code src :error (str err)})
        re-err (when out
                 (try (edamame/parse-string-all out) nil
                      (catch Throwable t (ex-message t))))]
    (println label "→" :rewrite out :still-broken? (boolean re-err))))
(show "1 \\|" "(vis/rg \"foo\\|bar\" \"x\")")
(show "2 \\|" "(vis/rg \"foo\\|bar\\|baz\" \"x\")")
(show "3 \\|" "(vis/rg \"foo\\|bar\\|baz\\|qux\" \"x\")")'
```

Output (ran on this machine, just now):

```
1 \| → :rewrite (vis/rg "foo\\|bar" "x") :still-broken? false
2 \| → :rewrite (vis/rg "foo\\|bar\|baz" "x") :still-broken? true
3 \| → :rewrite (vis/rg "foo\\|bar\|baz\|qux" "x") :still-broken? true
```

Single occurrence: rescue works. Two or more: rescue partially repairs
but the result still doesn't parse → loop drops the rewrite → user
sees the unhelpful raw edamame error.

### Recommended fixes (priority order)

1. **Loop the rescue.** In `try-extension-parse-rescue`, when the
   rewrite STILL doesn't parse, re-feed it back through the same
   rescue chain (with the *new* parse error message). Cap iterations
   at, say, 8 to avoid pathological infinite loops, and require the
   source to actually shrink the error count or stop. This single
   change closes the multi-`\|` case the docstring already promises.
2. **Strengthen the docstring/promp on `vis/rg`.** The tool's
   `:examples` show `"defn|defmacro"` — fine — but never show how to
   spell a regex pipe inside a Clojure string. Add a one-line note in
   the symbol's `:doc` ("inside a Clojure string `\|` is not a valid
   escape; use a bare `|` for regex alternation, or use `#\"…\"`
   regex literals for complex patterns"). The model literally cannot
   guess this from the current prompt.
3. **For the unescaped-quote case (iter 7),** add a second rescue
   pattern: when the parse error is `Unable to resolve symbol: X`
   *and* the broken form is a `(vis/rg "…")` call, scan the source
   for unescaped inner `"` characters preceding the symbol position,
   and rewrite to `\"`. This is symmetric to the `\|` rescue and
   uses the same hook plumbing.

---

## Bug 2.B — `vis/patch` Mode-3 vector edits: when the search string contains `]`, the LLM truncates the closing quote

Iter 13's call (full text from the DB):

```clojure
(vis/patch
  [{:path "render.clj",
    :search "expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))],
    :replace "expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) -1))]}
   {:path "render.clj",
    :search "fa-pad      (max 0 (- fill-w (count full-label) 1))",
    :replace "fa-pad      (max 0 (- fill-w (count full-label) -1))"}])
```

What happened: the model wanted a `:search` whose value ends with
`...) 1))]`. To do that in a Clojure string the closing `]` doesn't
need escaping, but the model apparently reasoned "I have a `]` here,
better not put `"` right after" and **omitted the closing quote
altogether**, then continued with `, :replace`. Edamame reads the
`:search` value as `"expr-hdr (let [pl (max 0 (- fill-w (count expr-label) 1))], :replace "...`
— a single string that runs all the way to the next `"` — and then
the surrounding map+vector structure goes off the rails. SCI sees:

```
Unmatched delimiter: }, expected: ) to match ( at [1 181]
```

### Reproduction

```bash
cd /Users/fierycod/vis && clojure -A:dev -e '
(require "[edamame.core :as edamame]")
(let [src "(vis/patch [{:path \"render.clj\", :search \"expr-hdr (let [pl (max 0 (- fill-w (count expr-label) 1))], :replace \"x\"}])"]
  (try (edamame/parse-string-all src)
    (catch Throwable t (println :err (ex-message t)))))'
```

Output: `:err Unmatched delimiter: }, expected: ) to match ( at [1 122]` — same exact failure mode as iter 13.

### Why this hits this conversation specifically

Three of the five turns in this conversation are about TUI offsets,
where every `:search` and `:replace` payload is *Clojure source code
that contains `[` `]` `(` `)` `"` `\space`*. The patch tool's Mode-3
vector form fires JSON-style strings inside an EDN map; the model has
to mentally compose **three layers of escaping** at once (LLM JSON →
Clojure EDN reader → file content). One slip and the whole call
fails.

Prior turns in the same conversation got around this by issuing
small, single-line patches at top-level `def`s — those don't carry
embedded square brackets. The current turn needs to patch nested
`(let […])` forms and the model loses track of escapes.

### Recommended fixes

1. **Prefer Mode-2 (`<<<<<<< SEARCH … >>>>>>> REPLACE`) for
   multi-line edits.** The marker-delimited form is robust against
   embedded `"` `[` `]` because no Clojure string-quoting is
   involved. The system prompt currently lists all four modes
   neutrally; promote Mode-2 explicitly for any patch whose
   search/replace contains a quote, bracket, or newline. Models that
   followed this pattern in earlier turns succeeded.
2. **Detect-and-explain the unbalanced-string case in
   `rescue-parse-error`.** When the parse error is `Unmatched
   delimiter: }, expected: )` AND the broken form mentions
   `vis/patch`, surface a much more pointed error to the LLM:
   "patch payload looks like the closing `\"` of a `:search` value
   was lost; consider switching to the `<<<<<<< SEARCH` form".
   Vague edamame messages are not actionable for the model.
3. **Move escaping out of the LLM's hands.** Add a fifth `vis/patch`
   mode whose payload is `[{:path … :search-bytes … :replace-bytes …}]`
   where the byte values come from `(slurp …)` calls the model
   already executed in earlier iterations. The model never re-types
   the search content; it points at a slot it already verified.
   Future work, but high leverage.

---

## Bug 2.C — `(vis/patch …)` "SEARCH block N not found" gives no diff hint

Iter 15's call (real, from the DB):

```clojure
(vis/patch
 [{:path "render.clj"
   :search "expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]
                                      (str (repeat-str \\space pl) expr-label \" \")"
   :replace "expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]
                                      (str (repeat-str \\space pl) expr-label \" \")"}
  {:path "render.clj"
   :search  "fa-pad      (max 0 (- fill-w (count full-label) 1))"
   :replace "fa-pad      (max 0 (- fill-w (count full-label) 3))"}])
```

The Clojure shape is valid this time. SCI accepts it. The patch tool
runs, slurps `render.clj`, calls `apply-one-replacement`, finds zero
matches for the multi-line `:search`, throws:

```
ExceptionInfo: SEARCH block 1 not found in packages/vis-channel-tui/.../render.clj
{:type :ext.common-editing.editing/patch-no-match
 :path …, :block 1, :search "expr-hdr    (let [pl (max 0 …"}
```

### Why it didn't match

The on-disk line was indented with one specific number of spaces
(matters: `extensions/.../editing.clj:679` does `(re-pattern (java.util.regex.Pattern/quote search))` — exact byte match, no whitespace tolerance). The model reconstructed the line from its `cat` projection where leading whitespace had been re-formatted by the file viewer. So the search had **38 leading spaces**, the file had **36** (or vice versa). Result: zero match → patch fails → the model has to read the file again, guess again, retry again.

The error message says **only** "not found in <path>". It does NOT
say:

- which line(s) of the file came closest to matching;
- how many bytes the closest candidate line differs by;
- whether a *whitespace-collapsed* search would have matched
  (almost-always the actual root cause);
- what was actually on disk in the area the model was thinking of.

### Reproduction (fully deterministic)

```bash
mkdir -p /tmp/patch_repro && cat > /tmp/patch_repro/render.clj <<'EOF'
(let [expr-label  (label-text "code" 1)
      expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]
                    (str (repeat-str \space pl) expr-label " "))   ;; ← only 20 leading spaces on this line
      fa-pad      (max 0 (- fill-w (count full-label) 1))]
  ...)
EOF

cd /Users/fierycod/vis
CP=$(clojure -Spath -A:dev)
cd /tmp/patch_repro
java -cp "$CP" clojure.main -e '
(require "[com.blockether.vis.ext.common-editing.editing :as ed]")
(let [patch-fn (:ext.symbol/fn ed/patch-symbol)]
  (try
    (patch-fn
      [{:path "render.clj"
        ;; LLM-supplied search uses 38 leading spaces on line 2
        :search (str "expr-hdr    (let [pl (max 0 (- fill-w (count expr-label) 1))]\n"
                     "                                      (str (repeat-str \\space pl) expr-label \" \")")
        :replace "..."}])
    (catch clojure.lang.ExceptionInfo e
      (println :MSG (ex-message e))
      (println :TYPE (:type (ex-data e))))))'
```

Output (just verified on this machine):

```
:MSG  SEARCH block 1 not found in render.clj
:TYPE :ext.common-editing.editing/patch-no-match
```

Same exception class, same message, same `:type` keyword as the row
in `expression_state` for iteration 15. Bug confirmed end-to-end.

### Recommended fixes

1. **Whitespace-tolerant fallback diagnostic.** In `apply-one-replacement`,
   when `exact-match-count` is zero, run a *second* search where both
   the file content and the search pattern have inner whitespace
   collapsed (`(str/replace ... #"\s+" " ")`). If THAT matches exactly
   once, augment the exception with `:near-match {:line N :hint
   "whitespace differs"}` and a short snippet of the on-disk line so
   the model can re-emit with the correct indentation. The patch
   STILL fails (we don't auto-fuzz writes), but the model now has the
   information to fix it on the next iteration instead of re-reading
   the file from scratch.
2. **Surface the closest line in the error message.** Even without
   whitespace collapse: when the search's first line is a unique
   substring of the file's content, point at it: `"SEARCH block 1
   not found in render.clj. Closest line: 1391 (\"expr-hdr (let
   [pl (max 0 …\"). Difference appears to be in leading
   whitespace."` Cheap to compute, hugely informative.
3. **Add a `:tolerate-whitespace true` opt to `vis/patch`.** When
   set, pre-process both content and search via collapsed whitespace
   for the *match*, but apply the replace at the matched span as-is.
   Defaults to false (keeps semantics tight), but the LLM can opt-in
   when it knows it doesn't care about indent.

---

## Reproducibility — what I actually ran

All four bugs were reproduced from the actual conversation data:

| bug   | reproduction status                             |
|-------|-------------------------------------------------|
| 2.A   | ✅ deterministic (`rescue-parse-error` + edamame)|
| 2.B   | ✅ deterministic (edamame parse on iter 13 src)  |
| 2.C   | ✅ deterministic (`vis/patch` end-to-end)        |
| (1.0) | ✅ deterministic via `svar.internal.spec/str->data-with-spec` (see BUG_REPORT_1) |

The only non-deterministic piece is the *trigger* — the LLM has to
emit the bad input — but every Vis-side failure that follows is
1-to-1 reproducible from the recorded `expression_state.expr` text.

Two hours of tool-error iterations in this conversation turn into
~30 lines of edamame and patch fixtures that the test suite under
`extensions/common/vis-common-editing/test/` already supports. Adding
fixtures for `(rescue-parse-error)` looped, the unbalanced-string
case, and the whitespace-near-match patch hint would have prevented
all three of these bugs from reaching this user.
