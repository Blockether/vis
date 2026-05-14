# PLAN — RLM purity for Vis

## Why this exists

The README claims Vis is "inspired by Recursive Language Models (Zhang,
Kraska & Khattab, 2025)". An honest audit (see "Honest classification"
below) shows we picked up RLM's first idea — external state, code as
the action space — and dropped its second — recursive sub-model
invocation on slices. The journal load-bears that gap: it echoes
content from the external store back into the prompt every iteration
so the model can ground claims. That works, but it is **not** RLM. It
is a tool-use agent with a token-budgeted transcript.

This plan moves us from "RLM-inspired" to "RLM-conformant" without
breaking the existing extension surface.

## What RLM actually requires

Two non-negotiable properties from the paper:

1. **State is external.** Evidence lives in a sandbox (Clojure heap,
   SQLite, filesystem). The model holds *handles* to it. The transcript
   never grows with raw evidence.
2. **Recursive invocation on slices.** When the model needs to know
   something about a large object, it does not read the bytes — it
   invokes another model call scoped to a slice. The slice gets a
   context window. The parent gets back a *structured answer*, not raw
   data. The recursion is literal: parent → child(slice) → grandchild
   (sub-slice) → ... → leaf answer.

The "recursive" in RLM is recursion of **model calls on data slices**,
not recursion of the engine's outer eval loop.

## Honest classification — where Vis is today

| RLM property | Vis today | Gap |
|---|---|---|
| State persists outside the transcript | ✅ SCI vars + SQLite | none |
| Model writes code, not natural-language tool calls | ✅ SCI sandbox | none |
| Journal is token-budgeted, not transcript-shaped | ⚠️ Budgeted but still echoes content | shape leak |
| Reads return *handles*, not content | ❌ `v/cat` returns `:lines [...]` | core gap |
| `<bindings>` shows *handles*, not previews | ⚠️ pr-str of value (truncated) | shape leak |
| Model invokes sub-models on slices | ❌ no primitive | core gap |
| Working memory at each recursion stays small | ❌ one context window per turn | follows from above |

Two of the seven properties are core gaps; two are shape leaks; three
are already correct. The plan addresses the four ⚠️/❌ rows.

## Goals

- **G1.** `v/cat`, `v/rg`, `v/ls` (every observation tool) return a
  *handle* + bounded metadata, not raw content. Content is reachable
  via explicit slice tools.
- **G2.** `<journal>` records *what happened*, not *what was read*. No
  more head/tail line echoes. The model verifies via slice tools, not
  via prompt-echo.
- **G3.** `<bindings>` shows handle shape (type, size, sha, first/last
  hint), not value previews.
- **G4.** A first-class `v/ask` primitive that spawns a scoped
  sub-model call on a handle and returns a typed answer.
- **G5.** No regression in answer quality on the existing TUI / CLI
  benchmark prompts. (If sub-models can be wired in for "summarize this
  file" and "find every callsite of X", we expect *fewer* iterations
  on large-context tasks, not more.)

## Non-goals

- Replacing the existing iteration loop. The outer engine stays the
  same — `run-iteration` still drives the parent. RLM recursion is
  one tool primitive (`v/ask`), not a new loop.
- Removing the `<journal>`. It still records *operations* (with
  metadata + outcome) so the model can prove an action happened. The
  removal is only of *echoed content*.
- A "pure" RLM that hides every byte from the model. We keep an
  escape hatch (`v/peek`) that the model can call when it explicitly
  needs to look at a slice — but every byte of content reaches the
  prompt only via an explicit slice call, not via passive echo.
- Provider-level changes. svar stays as-is.

## Architecture

### Sandbox value model

Today: `(v/cat "x")` returns
```clojure
{:path "x" :lines ["..." ...] :returned N :limit L :next-offset O
 :eof? bool :truncated-by kw}
```
The `:lines` vector is the value — and it is what the journal renderer
serializes back into the next prompt.

New: `(v/cat "x")` returns a **handle map**:
```clojure
{:vis/handle      :v.cat                    ; opaque kind tag
 :vis/handle-id   "h_abc123_5"              ; short stable id
 :path            "x"
 :line-count      5
 :byte-count      327
 :sha256          "ab12..."                 ; short prefix
 :first-line      "# Vis"
 :last-line       "Start with..."
 :truncated-by    :eof
 ;; private — held in a sandbox-side store, NOT printed in renders.
 :vis/store-key   <opaque>}
```
The actual line vector lives in a per-environment **handle store**
(`(:handle-store environment)`), keyed by `:vis/store-key`. The
sandbox can dereference a handle through slice tools (below), but the
handle's `pr-str` *intentionally* renders only the metadata header.

### Slice tools (new)

Every observation tool ships with a paired *slice* tool that the model
calls when it explicitly wants to look at content. Each slice call is
its own journal entry, bounded and attributable:

- `(v/lines handle from to)` — return `{:lines [...] :from :to}`.
  Bounded by `max-cat-window-bytes` (64KB) — same contract as today's
  `v/cat` window. The journal renders only `:from`/`:to`/`:returned`,
  not the lines.
- `(v/at handle line-no)` — single line, used for citations.
- `(v/grep-in handle spec)` — `v/rg` scoped to a single handle.
- `(v/peek handle)` — explicit "show me the first window inline";
  emits the lines into the channel render but not the journal. The
  escape hatch. Use sparingly.

For directories / search results:

- `(v/entries handle)` — page through `v/ls` children.
- `(v/hit handle idx)` — fetch a single `v/rg` hit's full context.

### `v/ask` — the recursion primitive (new)

```clojure
(v/ask handle question)            ; default opts
(v/ask handle question opts)       ; with budgets / model override
```

Behaviour:

1. Spawns a **child iteration** with its own SCI sandbox.
2. The child's `<environment>` carries the handle data as the SOLE
   evidence (a full materialization of the handle's content into the
   child's bindings — bytes flow into the child's context window, not
   the parent's).
3. The child's user message is `question`. The child has no journal
   history.
4. The child runs the normal `turn!` loop, capped by `opts`:
   ```clojure
   {:max-iterations    8         ; child iteration budget
    :max-tokens        16000     ; child total token budget
    :model             :inherit  ; or specific model override
    :reasoning-level   :low      ; cheaper by default
    :answer-shape      :data}    ; :data | :ir | :text
   ```
5. The child's `(turn-answer! ...)` is captured. Its value (an IR or a
   plain Clojure value, depending on `:answer-shape`) becomes the
   return value of `v/ask` in the parent.
6. The parent's journal records: `(v/ask <handle-id> "...") → <typed
   answer summary>` — no child transcript, no child reasoning.
7. Recursion depth is tracked on the environment; default `:max-depth
   3`, hard cap `:max-depth 5`. Exceeding it throws a structured
   error.
8. Cost / token accounting flows back up the stack; the parent's run
   result aggregates all descendant usage.

This is the keystone primitive. Without it, "external state the model
can inspect" remains a slogan — the parent still has to spend its
context window reading data. With it, "inspect" means "ask a
sub-call".

## Phases

Each phase ships independently and is keep-able on its own. No phase
depends on a later phase compiling.

### Phase 0 — handle store + handle pr-str (foundation)

Plumbing only. No behavioural change for existing extensions.

- Add `(:handle-store environment)` — a per-environment atom of
  `{store-key -> value}`. Bounded by `MAX_HANDLE_STORE_BYTES` (start at
  16MB), LRU eviction on insert.
- Add `com.blockether.vis.handle/make`, `deref`, `summary` helpers
  used by extensions to produce handle maps and dereference them
  inside slice tools.
- Print-method for handles: `#vis/handle {:kind :v.cat :id "h_abc123"
  :line-count 5 ...}` — single line, ≤120 chars. Used by `<bindings>`
  and `pr-str` everywhere.

Files:
- `src/com/blockether/vis/handle.clj` (new)
- `src/com/blockether/vis/internal/env.clj` (wire `:handle-store` into
  `create-sci-context`)
- `src/com/blockether/vis/internal/prompt.clj` (update bindings
  renderer to special-case handle maps)

Tests:
- `test/com/blockether/vis/handle_test.clj` — make/deref/summary
  roundtrip; LRU eviction; `pr-str` shape.
- `test/com/blockether/vis/internal/prompt_test.clj` — bindings
  block shows handle summary, not value pr-str.

Acceptance:
- `(def x (vis.handle/make :kind/test {:meta 1} {:big "value"}))`
  prints as a single-line handle summary in `<bindings>`.
- `(vis.handle/deref x)` returns `{:big "value"}`.
- `<journal>` rendering of a handle map prints only the summary, never
  the deref'd payload.

### Phase 1 — handle-shaped bindings render

Smallest visible step toward RLM. No tool changes; only the bindings
renderer treats handle maps specially.

- `prompt/build-bindings-block` (and the channel-side bindings
  renderer) detect `:vis/handle` maps and render the one-line summary
  instead of the truncated pr-str of the underlying value.

Files:
- `src/com/blockether/vis/internal/prompt.clj`
- `extensions/channels/vis-channel-tui/src/.../render_*.clj` (if it
  has its own bindings preview path)

Tests:
- A `(def x (v/cat "..."))` whose result is a handle map renders as
  one summary line in `<bindings>`, not as pr-str of the full result.

Acceptance:
- A bindings block listing 100 large handles fits in <2KB of prompt
  text. (Today: easily 20KB+.)

### Phase 2 — handle-shaped reads (`v/cat`, `v/ls`, `v/rg`)

Behavioural change. Existing tests for these tools will need updating
(noted in PLAN_LIFECYCLE.md acceptance section).

- `cat-tool` produces a handle map. The `:lines` payload goes to the
  handle store; `cat-tool` returns the summary + handle id.
- `ls-tool` produces a handle whose payload is the tree; the handle
  summary shows top-level entry count + depth.
- `rg-tool` produces a handle whose payload is the hit vector; the
  summary shows `:hit-count`, `:truncated-by`, and the first hit's
  `path:line`.
- `journal-render-cat`, `journal-render-ls`, `journal-render-rg`
  reduce to summary-only renders. NO content lines, NO hit excerpts.
  Header line + read-more hint that points the model at the slice
  tools.
- `channel-render-*` keep showing content for human consumption — the
  channel is for the human's terminal, the journal is for the model.
  This is the split the journal/channel renderer pair was designed
  for; we just hadn't been using it.

Files:
- `extensions/common/vis-foundation/src/.../editing/core.clj` (the
  three tools + their journal renderers)
- `extensions/common/vis-foundation/test/.../editing/core_test.clj`
  (update renderer-contract tests)

Tests:
- `journal-render-cat` for a 5000-line file produces a single
  paragraph with no `1: ...`, `2: ...` line content.
- `channel-render-cat` for the same call still produces the
  numbered-line `[:code]` IR block.
- `(v/cat "f")` returned value is a handle map with `:line-count 5000`
  and the actual lines reachable via `(vis.handle/deref x)`.

Acceptance:
- A turn that reads a 5000-line file and then runs four further
  iterations: total `<journal>` token cost grows only by per-call
  metadata, NOT by content.

### Phase 3 — slice tools (`v/lines`, `v/at`, `v/grep-in`, `v/peek`)

Without these, Phase 2 leaves the model unable to see content. Ship
together with Phase 2 if at all possible; minimum is Phase 2 + Phase 3
in the same release.

- `v/lines handle from to` — derefs handle, slices `:lines`, returns
  `{:from :to :returned :lines}`. Bounded by 64KB byte cap. Has its
  own `journal-render-lines` that DOES print the numbered lines —
  this is the model's escape hatch when it needs to see content. The
  difference vs. today: it's an explicit request, journal-attributed,
  and bounded per call.
- `v/at handle line-no` — single line, same renderer family, used for
  citations.
- `v/grep-in handle spec` — run `v/rg`-style search scoped to the
  handle's content. Returns a *new* hits handle.
- `v/peek handle` — render the handle's first 64KB window into the
  channel only. Journal entry is metadata-only. The model uses this
  when it just wants the human to see what it's working with.

Files:
- `extensions/common/vis-foundation/src/.../editing/core.clj`
- new tests in `editing/core_test.clj` for each slice tool

Acceptance:
- A 200-line file read + 3 line-slice peeks costs `<journal>` budget
  roughly equal to: (1 metadata line for `v/cat`) + (3 × 64KB content
  windows, only as long as those windows stay in the token budget).

### Phase 4 — `v/ask` recursion primitive

The big one. This is what makes us RLM.

- `com.blockether.vis.recursion` namespace housing the child-spawn
  helpers (`spawn-child-environment`, `recursion-budget-check!`,
  `aggregate-usage`).
- `v/ask` tool in vis-foundation: dispatches to
  `recursion/run-child!`, which builds a scoped environment with the
  handle materialized into a single `def`, runs `loop/turn!` capped
  by budget, captures the child's `(turn-answer! ...)` value.
- Child environments do NOT see the parent's `<journal>` or
  `<bindings>` — they start clean except for the handle data.
- Recursion depth tracking: each child inherits `:vis/recursion-depth
  (inc parent-depth)`. Hard cap at 5.
- Cost rollup: child usage flows back into the parent's
  `:api-usage` map under `:children`.
- A new system prompt block — `RECURSION` — documents `v/ask` and the
  budgets to the model.

Files:
- `src/com/blockether/vis/internal/recursion.clj` (new)
- `extensions/common/vis-foundation/src/.../core.clj` (register
  `v/ask`)
- `src/com/blockether/vis/internal/prompt.clj` (add `RECURSION`
  section to `CORE_SYSTEM_PROMPT`)
- `src/com/blockether/vis/internal/loop.clj` (thread parent depth
  through `run-iteration`'s environment + enforce cap)

Tests:
- `recursion_test.clj` — child gets a scoped env; child cannot see
  parent's bindings; depth cap throws structured error at 6.
- An integration test that wires a fake provider returning a known
  answer for both parent and child, asserts the child's answer flows
  back up.
- Cost rollup test: parent.tokens + child.tokens == final
  `:api-usage`.

Acceptance:
- A turn that calls `(v/ask (v/cat "BIG.md") "summarize")` and uses
  the result to answer the user shows ONE journal line for the v/cat
  (metadata), ONE journal line for the v/ask (question + answer
  summary), and a non-empty `:children` rollup in the run result.

### Phase 5 — drop journal echo on legacy tools, prompt audit

Cleanup. The journal renderers for every observation tool emit
summary-only output. Models migrating from old behaviour are nudged
through:

- Title-card update in `CORE_SYSTEM_PROMPT`: "Observations return
  handles. To see content, call `(v/lines h from to)` or
  `(v/peek h)`. To analyze content at a distance, call
  `(v/ask h \"...\")`."
- Foundation nudge fires on the first iteration of every turn telling
  the model the recursion primitive exists and how to budget it.
- Decommission the old per-tool head/tail rendering branches and
  delete the `journal-head-tail-lines` constant.

Files:
- `src/com/blockether/vis/internal/prompt.clj`
- `extensions/common/vis-foundation/src/.../nudges.clj`
- `extensions/common/vis-foundation/src/.../editing/core.clj`

Acceptance:
- `grep -r 'journal-head-tail-lines'` returns no hits.
- `core_test.clj` renderer-contract suite passes against the
  summary-only journal renderers.
- The README's "inspired by RLM" line graduates to "implements RLM:
  external state via SCI handles; recursive sub-model invocation via
  v/ask".

## Cross-cutting concerns

### Backward compatibility

- Extensions that today bind `(v/cat ...)` results and read `:lines`
  break in Phase 2. Provide a deprecation period: ship Phase 0–1
  first, then in Phase 2 introduce `(vis.handle/deref h)` as the
  blessed way to get `:lines` back, and keep the handle map's
  `:lines` key accessible via that derefer (NOT directly on the
  handle map) for a release.

### Token-budget contract

Today the contract is `JOURNAL_CONTEXT_FRACTION (0.5)` of the model
window for `<journal>`, capped at `MAX_JOURNAL_TOKENS (24000)`. After
this plan, journal entries are roughly fixed-size per call (metadata
only), so the practical journal cost drops by 10–100× depending on
the workload. We will reduce `MAX_JOURNAL_TOKENS` to a tighter cap
(suggest 6000) and reclaim the freed budget into `<bindings>` and
sub-call quotas.

### Provider compatibility

Nothing in this plan touches svar or the provider extensions. Models
that historically struggled with "tool returns handle, then I have to
call another tool to see content" (mostly older / non-tool-trained
models) get the `v/peek` escape hatch. We do NOT expect every model
to use `v/ask` correctly; in practice it will be most valuable for
Claude Sonnet, GPT-5, GLM-4.6+ — the models with strong tool-use +
reasoning. Cheaper models can still operate via slice tools without
ever touching `v/ask`.

### Cost / safety

`v/ask` is a cost amplifier — a parent call burning $0.10 can spawn
children that, at depth 5, expand to dozens of model calls. Guard
rails:

- Hard depth cap (5) enforced in the engine, NOT in the prompt.
- Per-turn child budget (default: 20 children, configurable).
- Per-call token budget on each child (default: 16k).
- The model sees its remaining recursion / token budget in the
  per-iteration context. If it tries to spawn a child that would
  exceed budget, the call returns a structured error.
- Telemetry: every `v/ask` records depth + child usage; a `vis
  recursion-report <turn-id>` CLI surface for after-the-fact audit.

## Verification

Each phase ships with its own tests; the full suite must stay green
between phases. End-to-end acceptance:

```bash
clojure -M:test                                          # full suite, 0 failures
./verify.sh --quick                                      # smoke
bin/vis run --plain "Summarize README.md and PLAN.md"    # uses v/ask
bin/vis run --trace "Find every callsite of run-iteration"
```

Specifically for the RLM property audit, a new test file
`test/com/blockether/vis/rlm_property_test.clj` will assert, after
Phase 4:

- For a 5000-line file read followed by 5 iterations of analysis, the
  total `<journal>` token cost is `<` 1000 tokens. (Today's number on
  that workload is `>` 30000.)
- A turn that calls `(v/ask handle "...")` does not put any of the
  handle's `:lines` into the parent's prompt at any iteration.
- Recursion depth 6 throws `:vis/recursion-depth-exceeded` with a
  structured payload.

## Risks

- **R1 — Models confused by handle returns.** Mitigation: Phase 1
  ships first (bindings shape only, behaviour unchanged), then
  Phase 2 ships with strong prompt guidance + the `v/peek` escape
  hatch. Validate on the existing benchmark prompts before Phase 5.
- **R2 — `v/ask` runaway costs.** Mitigation: hard depth cap + per-
  turn child budget enforced in engine, NOT prompt. The first n
  releases ship `v/ask` *disabled by default*, opt-in via config.
- **R3 — Slice tools double the iteration count.** Mitigation:
  measure on benchmark workloads after Phase 3. If true, tune
  `default-cat-limit`-equivalent on `v/lines` upward; the journal
  budget freed by Phase 2 absorbs this.
- **R4 — Existing automation depends on `(:lines (v/cat ...))`.**
  Mitigation: deprecation window — keep `:lines` reachable on the
  handle map under the `(vis.handle/deref ...)` path for one release.

## Out of scope (for now)

- Persisting handles across turns. Handles today are
  per-environment / per-turn. Multi-turn handle persistence is a
  separate, larger discussion.
- A typed answer protocol for `v/ask` beyond `:data | :ir | :text`.
  We can grow into shape-aware contracts later (`:as-edn-vector`,
  `:as-int`, etc.).
- Replacing the iteration loop with proper continuations. The outer
  loop stays a `loop/recur`. RLM recursion is a tool-call surface
  feature.

## Done state

Vis is an RLM when, on a turn that reads a 100k-line file and asks
"summarize section 4 and find every TODO":

- The parent's `<journal>` contains: 1 line for the v/cat, 1 line for
  the v/ask, 1 line for the v/rg, 1 line for the turn-answer.
- The parent's `<bindings>` contains: 3 one-line handle summaries.
- The parent never sees a single line of the file in its prompt.
- The child (v/ask) sees section 4 in its own bounded context,
  returns a structured summary.
- The v/rg returns a hits-handle; the parent calls `(v/hit h idx)`
  on the few hits it actually wants to cite.
- Total token spend: small fraction of today's spend on the same
  workload.
- The model's final answer cites concrete line numbers via `v/at`
  calls — every claim is traceable to a handle slice that landed in
  the journal.

When that turn runs end-to-end with the budgets enforced, we ship the
README change.
