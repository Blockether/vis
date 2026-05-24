# Trailer / def / context analysis — c8dc39b1 case study

## Problem observed

Conversation `c8dc39b1-faf0-451e-85ad-8cd054067229` showed trivial later
turns (`siemanko`, `git status`) consuming 50–110k input tokens. The
source was not user text. It was the rendered `;; ctx` block, dominated
by `:session/trailer` pins created in turn 1.

Before turn 5:

- `;; ctx`: ~103k chars
- `:session/trailer`: ~101.8k chars (~99% of ctx)
- biggest pins:
  - `t1/i2`: ~26.7k chars, broad `(v/rg ...)`, `167` files
  - `t1/i3`: ~68.3k chars, `(v/cat ".../render_ir.clj")`, file `42036` bytes

Per-turn input token figures:

| turn | request    | status      | iters | input   | cached |
| ---- | ---------- | ----------- | ----- | ------- | ------ |
| 1    | TAGS.md…   | error       | 9     | 234 816 | 45 366 |
| 2    | git status | done        | 2     | 109 720 | 0      |
| 3    | siema      | interrupted | 0     | 0       | 0      |
| 4    | siema      | interrupted | 0     | 0       | 0      |
| 5    | siemanko   | done        | 1     | 58 958  | 0      |
| 6    | zrób parę  | done        | 2     | 104 146 | 3 143  |

Turns 3 and 4 burned zero provider tokens because the user interrupted
before any provider call landed. Turns 5 and 6 paid the full trailer
weight on every iter.

## PART 1 — What the trailer is, mechanically

The trailer is a vector under `(:session/trailer ctx)`. Each entry is a
"pin" map `{:scope "tN/iM" :forms [...]}` summarising one iteration.

### 1. Pin creation

`advance-iter` (`src/com/blockether/vis/internal/ctx_engine.clj:935-972`)
runs at every iteration end. It takes the form-results vec produced by
`blocks->forms` / `block->envelope`
(`ctx_engine.clj:1580-1622`, `1624-1633`), filters out forms whose
`:src` starts with `(done`, and — if anything is left — conjs one
`{:scope "tN/iM" :forms keepable}` onto `:session/trailer` (line
967-969).

**No size check. No dedup. No cap.**

### 2. Where `:result` comes from — the def-deref

`block->envelope` (`ctx_engine.clj:1580-1622`) starts from
`raw-result (:result block)` — the value SCI returned for the form. For
def-shaped forms (`def`, `defn`, `defmacro`, `defmulti`, `defonce`)
matched by `def-form-src?` (`1527-1544`), it calls `deref-def-result`
(`1546-1554`) on the SCI Var so the pin stores the **bound value**, not
the Var reference. Then the result is walked through
`realize-trailer-value` (`1556-1578`) which forces lazy seqs and
re-derefs nested `IDeref`.

Why this exists (engine comment 1531-1533): without the deref the
trailer would render `#'sandbox/NAME`, the model would re-emit `NAME` in
the next iter to inspect the value, and waste a whole iteration.

Consequence: a `(def x (v/cat …))` over a 40k file plants the whole
file (line-tuples and all) into the trailer pin.

### 3. When something is pruned

Only one prune ever runs: `prune-stale-observation-pins`
(`ctx_engine.clj:906-921`), invoked from `advance-iter` line 958.

Conditions:

- the current iter contains at least one **mutation** form, AND
- the old pin is observation-only — `observation-only-trailer-entry?`
  (`901-904`) requires every form to be `:tag :observation`.

If a pin has even one mutation form, it stays forever. And `(def …)` is
classified as `:mutation` via `mutation-heads` (`1477-1497`) /
`classify-form-tag` (`1499-1520`). So
`(def x (v/rg …))` produces a mixed pin that never gets pruned.

**No size GC, no LRU, no per-iter trailer budget.**

The renderer is explicit about it: "No entry cap, no per-form payload
cap" (`src/com/blockether/vis/internal/ctx_renderer.clj:300-301`).

### 4. When the trailer hits the prompt

At every turn boundary `render-block!`
(`src/com/blockether/vis/internal/ctx_loop.clj:288+`) calls
`render-ctx`, which feeds `:session/trailer` into `render-trailer-value`
(`ctx_renderer.clj:299-311`). Walk path:

`render-trailer-value` → `render-trailer-pin` (`265-273`) →
`render-form-pin` → `presentation-form` (`256-263`).

`presentation-form` drops only `:src` (rendered separately as a `;;`
block). `:result` is "passed through UNTRUNCATED" (comment line
260-261). Every pin's full value is zp-printed into the next system
prompt verbatim.

### 5. When the model owns deletion

Only inside `(done {…})`. `apply-done` (`ctx_engine.clj:1366`) routes
to:

- `apply-trailer-drop` (`1163-1176`) — exact `:scope` match against
  `tN/iM` pins or `tA/iX->tB/iY` summary keys
- `apply-trailer-summarize` (`1178-1226`) — absorbs fully-contained
  pins into one `{:scope-start :scope-end :summary :born}` entry

Outside `(done …)` there is **no API for the model and no engine code**
that shrinks the trailer.

## PART 2 — Why c8dc39b1 blew up, turn by turn

### Turn 1, iter 1 — title hook

Trivial. Satisfied `:vis.foundation/session-title`. Pin `t1/i1` small.

### Turn 1, iter 2 — broad `rg`

```clojure
(task-set! :vis.foundation/session-title {:status :done :proof "t1/i1/f1"})
(def ir-hits (v/rg {:any ["render-ir" "ir-render" ":ir " "defmethod render"]
                    :paths ["."] :files-only? true}))
ir-hits
```

What landed in the pin:

- Form 2 `(def ir-hits …)` is `:mutation` (def). After
  `deref-def-result`, `:result` holds the full 167-file vector.
- Form 3 bare `ir-hits` is `:observation`. Its `:result` is **the same
  167-file vector** again.

Two copies of the same payload in one pin. Pin size ~26.7k chars.

Why prune did not remove it:
`observation-only-trailer-entry?` (`ctx_engine.clj:901-904`) needs
every form tagged `:observation`. Form 2 is `:mutation`, pin is mixed,
the filter at `916-920` never selects it.

### Turn 1, iter 3 — full `v/cat`

```clojure
(def ir-file (v/cat "extensions/.../render_ir.clj"))
```

`v/cat` default reads 2000 lines from line 1 with no caller-supplied
limit. File: 42036 bytes. `block->envelope` derefs the Var,
`realize-trailer-value` forces the `:lines` vec. Pin `t1/i3`: ~68.3k
chars.

### Turn 2 — `git status`

Two iters. Each iter's provider call rebuilds `;; ctx` from scratch and
includes `t1/i1` + `t1/i2` + `t1/i3` verbatim. Result: 109 720 input
tokens for a tiny user question, almost all from the carried trailer.

### Turns 3, 4 — `siema`

`iteration_count = 0`. User interrupted before any provider call landed.
No new pins, but also no `(done …)`, so no `:trailer-drop` ran. Trailer
stays at ~101.8k chars across every subsequent prompt rebuild.

### Turn 5 — `siemanko`

~58k input tokens for one word. The persistent trailer rode in
unchanged.

### Turn 6 — `zrób parę v/ls`

Same baseline + new pins on top. Model emitted
`(done {:answer … :trailer-drop ["t6/i1"]})` to drop its own fresh pin,
but the legacy `t1/i2` / `t1/i3` weight stayed because nobody dropped
them.

## PART 3 — Three roles conflated on one pin

Take `t1/i2/f2` `(def ir-hits (v/rg …))`. One envelope, three roles:

| role               | what it is                                                   | what the model needs in prompt           | where it should live                       |
| ------------------ | ------------------------------------------------------------ | ---------------------------------------- | ------------------------------------------ |
| evidence-for-model | "I ran `v/rg`, got 167 files."                               | `:src` + count + small sample (~200 ch)  | trailer pin (compact)                      |
| result-snapshot    | the literal 167-element vector of paths                      | nothing by default; lazy-fetch on demand | `session_turn_iteration.forms` (DB) only   |
| restore-state      | bound value of `ir-hits` for next iter / next resume         | nothing in prompt; SCI binding only      | `definition_state.value` (DB) → SCI intern |

All three end up in the same `:result` slot today. That is the bug.

## PART 4 — Fix options, smallest to largest

### Option A — size-aware pin at creation (smallest change)

**Touch:** `block->envelope`
(`src/com/blockether/vis/internal/ctx_engine.clj:1580-1622`) or
`advance-iter` (`935-972`).

**Patch shape:** after `realize-trailer-value`, check
`(count (pr-str result))`. When over a threshold (suggest **2 KB**),
replace `:result` with

```clojure
{:vis/result-ref true
 :scope          scope
 :vis.op         (:vis.op raw-result)
 :preview        "first non-empty line of pr-str result"
 :size           N
 :full           "use introspect-form"}
```

The full value stays in DB (`session_turn_iteration.forms[…].result`,
already persisted today) and in an in-memory `:engine/form-results`
cache so hook-task validators see the same data they see today.

**Risk:** low.

- Hook-task validation runs at end-of-iter against the live
  `form-results` vec **before** `advance-iter` writes the pin, so the
  existing validator flow is unaffected.
- `trailer->form-results` (`ctx_loop.clj:275-286`) → `pass-validators`
  (`ctx_engine.clj:744`) need a DB-or-cache fallback only for proofs
  referencing scopes from earlier turns. Same fallback applies to
  `classify-scope` lookups.

**User-visible:** prompts shrink immediately on large `v/cat` / `v/rg`.
Model sees `:src` + `:vis.op` + summary; calls `(introspect-form …)` if
it needs the body. The single biggest leverage point — would have
prevented the entire c8dc39b1 explosion.

### Option B — auto-summarize over budget (medium change)

**Touch:** new compactor next to `apply-trailer-summarize`
(`ctx_engine.clj:1178-1226`), called from `advance-iter` post-step.

**Patch shape:** if
`(reduce + (map rendered-size (:session/trailer ctx))) > BUDGET`
(suggest **20 KB**), auto-synthesise
`{:scope-start :scope-end :summary :born}` over the oldest pins until
under budget. Same shape as model-issued summaries — renderer untouched.

**Risk:** medium.

- `find-overlap-conflict` (`1153`) must run for auto-summaries too.
- Tests for the `apply-done` summarize path must extend to the
  auto-path.
- Model-owned `(done {:trailer-summarize …})` semantics stay; the
  engine simply runs the same pipeline opportunistically.

**User-visible:** trailer asymptotes at a known size. Old pins become
`:summary` strings; model calls `introspect-iter` / `introspect-form`
for replay. Stops the 58k-token short-message regression entirely.

### Option C — full DB-truth / trailer-index split (largest change)

**Touch:** A + B + persistence layer
(`session_turn_iteration.forms` already stores full envelopes but must
become the **canonical** read source), restore path, hook-task validator
pass (must read DB not trailer), `introspect-form` implementation
(must hit DB).

**Risk:** large.

- `trailer->form-results` (`ctx_loop.clj:275-286`) becomes lossy.
  Every consumer (`pass-validators`, `classify-scope`,
  `derive-progression`) needs a DB-aware fallback.
- Restore semantics must be formalised (Part 5).
- Migration: existing sessions store fat trailers; renderer either
  handles both shapes or a migration thins them at load time.

**User-visible:** trailer becomes a compact index
(`:src`, `:tag`, `:vis.op`, `:size`, `:preview`,
`:full "use introspect-form"`). Full data lazy-loadable. Matches the
target architecture.

### Recommended path

A first. It is a one-file, low-risk patch that eliminates 80% of the
bloat without changing any semantics. B second to make the worst case
bounded. C as the long-term clean-up once A + B have run in production
long enough to validate the size thresholds.

### Concrete thresholds (default)

- inline scalar: always
- inline map / vector: only if `pr-str` ≤ **2 KB**
- per-iter result budget across all forms: **8 KB**
- per-ctx trailer hard cap before auto-summary: **20 KB**

(Numbers from the GLM-5.1 critique pass; tunable per session.)

## PART 5 — Restore semantics

Restore means rebuilding the SCI sandbox so that every name the model
`def`-ned in earlier iters resolves to the same value on the next iter
or after a process restart / session resume / fork.

### Case 1 — pure `(defn foo [x] (* x 2))`

Deterministic, side-effect free. Restore replays the form through SCI →
identical `#'sandbox/foo`. DB does not even need the bound value;
source alone is enough. Always safe.

### Case 2 — impure `(def x (v/cat "deps.edn"))`

Replay would re-execute `v/cat` against a possibly-changed file.
Restore must **not** eval the RHS. Instead it interns the stored value
from DB (`session_turn_iteration.forms[…].result`, the same value
`realize-trailer-value` produced at original execution time). Source is
kept for forensics only.

### Case 3 — cross-form `(def y (process x))` after Case 1 + Case 2

Forms restored in `(turn, iter, position)` order via `scope-compare`:

1. Replay `(defn process …)` — pure, SCI binds `#'sandbox/process`.
2. Replay `(def x …)` — skip RHS, intern stored DB value.
3. Replay `(def y …)` — two valid strategies:
   - **(a)** eval RHS: `process` is pure, `x` already bound to the
     bit-identical DB value, so result is deterministic. "Free" if
     serde is bit-faithful — which `realize-trailer-value`
     (`ctx_engine.clj:1556-1578`) was added to guarantee.
   - **(b)** skip RHS, intern stored `y`. Safer when `process` itself
     is non-deterministic.

### What the engine cannot do today

`def-form-src?` only recognises "this is some def". A real restore
needs a `safe-to-replay?` classifier:

- `defn`, `defmacro` → **pure-by-shape**, safe to replay
- `(def NAME (TOOL …))` where TOOL ∈ `mutation-heads` (e.g. `v/write`,
  `v/patch`, `git/commit!`) → **never replay**
- `(def NAME (READ-TOOL …))` where READ-TOOL ∈ {`v/cat`, `v/rg`,
  `git/status`, `v/ls`, network-shaped reads} → **impure-read, intern
  from DB**
- `(def NAME <literal>)` or `(def NAME (pure-expr …))` → safe to replay

`mutation-heads` (`ctx_engine.clj:1484-1497`) already covers most write
tools. The "impure-read" tier is what is missing — `v/cat`, `v/rg`,
`git/status` are reads of mutable state, not writes, but their results
must not be reproduced from source.

## Open questions

1. Where does the `:engine/form-results` cache live? Per-session in
   memory? Bounded? Or do we always round-trip through DB?
2. Does Option A leak abstraction: should the renderer or the engine
   own the size threshold?
3. Restore must run before SCI executes the first form of the resumed
   iter. Where does the restore pass attach? At `make-ctx-atom`
   (`ctx_loop.clj:41-44`) or earlier in the session-open flow?
4. Auto-summary in Option B needs a default summary text generator.
   Reuse the `:vis.op` summary shape from Option A? Or run a small
   prompt-side render per pin?
5. Do we expose `safe-to-replay?` as engine surface so extensions can
   register their own tool tier (read / mutation)?
