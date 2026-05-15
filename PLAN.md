# PLAN — Pivot handoff: one block per iteration; vars are the memory

> **Status:** Infrastructure shipped (20 commits since `1110436a`). Wiring + the
> destructive cuts remain. This document is a handoff — every remaining
> step is concrete (file path + symbol + acceptance criterion). When the
> remaining phases land, the agent surface flips to the new RLM-conformant
> shape; until then the engine still drives the legacy prompt.

## Why this exists

The legacy iteration model has a structural floor: probe + answer,
enforced by the `(done …)` gate. Pushing lower requires changing the
gate and the prompt scaffolding around it — not optimizing inside
them.

The pivot collapses the iteration model:

- **One ```clojure block per iteration**, evaluated atomically.
- **No `<journal>`. No `<bindings>`. No `<current_user_message>`.** The
  user's request is a real `(def USER_REQUEST "doc" "<text>")` injected
  at turn start; the model interpolates it like any other var.
- **Cross-iteration state lives only as named vars** in the SCI sandbox.
- **The model reads its own REPL tape** as commented Clojure source above.
- **Defs are mandatory-docstring**, persistence-instrumented, LRU-pruned.

Hard cut-over: no migration of existing conversations. `~/.vis/vis.mdb`
becomes unreadable on the next install — users delete it and start
fresh.

## Decisions (settled)

| # | Decision |
|---|---|
| 1 | One ```clojure block per iteration; engine rejects multi-form blocks at parse time. |
| 2 | `<journal>` deleted; `<bindings>` deleted; `<current_user_message>` / `<current_turn_context>` deleted. |
| 3 | Tape window: prior iteration only (N-1); on error, render N-1 + N. Cross-turn: last iteration of the prior turn carries forward. |
| 4 | Tape format: rich-comment `;; =>` for results, `;; ! stdout>` / `;; ! stderr>` / `;; ! ERROR` / `;; ! TIMEOUT` for side effects. |
| 5 | `(done [:ir …])` strict IR shape; accepted on no-throw, no other gate. Renamed from `(turn-answer! …)`. |
| 6 | Iterations are the durable unit; turns are projections. Same `conversation_turn_*` tables, but the boundary is an annotation, not a container. |
| 7 | Plain `(def NAME "doc" VAL)` and `(defn NAME "doc" [args] body)`. **Mandatory docstring**. Engine throws `:vis/missing-docstring` on miss. |
| 8 | Per-iteration `*def-sink-atom*`; engine flushes once at iteration-end inside the same transaction as the iteration row. |
| 9 | Capture mechanism: monkey-patch `sci.impl.evaluator/eval-def` from vis bootstrap (`alter-var-root` wrap). Catches every def — including macro-expanded `defn` / `defmacro` / `s/def`. |
| 10 | Second monkey-patch on `sci.impl.resolve/resolve-symbol*` for runtime-resolution LRU bookkeeping. |
| 11 | 30-binding cap on the live-vars discovery surface; 10-turn LRU eviction based on runtime resolution. |
| 12 | Live-vars renderer: names + docstrings, single block above the tape. |
| 13 | Handles: per-kind `defrecord` (CatHandle, RgHandle, LsHandle, …) implementing the `PHandle` protocol in `com.blockether.vis.internal.extension.handle`. Each implements `clojure.lang.IDeref` and ships a per-record `print-method` rendering one-line `#vis/handle` summary. |
| 14 | `view` / `summary` / `kind` / `handle?` are bare engine sandbox primitives in `EXTRA_BINDINGS` — every extension that ships a handle kind gets dispatch on these names automatically. Non-handles return structured `:not-a-handle` data, never throw. |
| 15 | `v/cat`, `v/rg`, `v/ls` return Handle records (CatHandle/RgHandle/LsHandle); content reachable via `@h` or `(view h :op …)`. |
| 16 | `USER_REQUEST` is a regular `(def USER_REQUEST "current turn user request" "<text>")` injected at turn start. In `SYSTEM_VAR_NAMES`. |
| 17 | Schema: collapse `code_blocks` BLOB; promote `code` / `result` / `error` / `stdout` / `stderr` / `duration_ms` columns directly on `conversation_turn_iteration`. |
| 18 | Keep `expression_soul` / `expression_state`. Multiple state versions per iteration allowed (each def writes its own version). |
| 19 | `<iteration_hints>` XML block survives — extension nudges are advisory, not state, and the existing `:turn.iteration/start` hook contract stays intact. |
| 20 | Telemetry per iteration: `;; --- iteration N \| turn=K conv=… state=… \| status=done\|error\|current ---` header before each rendered iteration's code in the tape. Cross-turn boundary implicit in the IDs. |

## Shipped infrastructure (dormant until Phase 4 main + Phase 7 main wire it)

All of the following exist in the repo today with passing tests but DO
NOT yet flow through the live engine path. Test suite: 957 cases green;
`./verify.sh --quick` clean.

### `com.blockether.vis.internal.extension.handle`

- `PHandle` protocol: `(kind h)`, `(summary h)`, `(view h op [a [b]])`.
- `CatHandle` / `RgHandle` / `LsHandle` defrecords. Each implements
  `IDeref` (`@h` or `(deref h)` materializes payload from the
  process-wide LRU store) and ships per-record `print-method` registering
  the one-line `#vis/handle <summary>` form.
- `make-cat` / `make-rg` / `make-ls` constructors stash payload in the
  store and return the handle.
- `handle?` predicate: cheap class check; rejects the Object fallback.
- Object + nil fallback impls of `PHandle` so `(view 42 :peek)` returns
  `{:kind :not-a-handle :value 42 :op :peek :hint "…"}` instead of
  throwing.
- `MAX_STORE_BYTES = 16 MB`; LRU on insert.
- pprint dispatch fix: `prefer-method clojure.pprint/simple-dispatch
  IPersistentMap IDeref` (handles are both; pprint chokes without the
  preference).

### `com.blockether.vis.internal.extension.sci-patches`

- `*def-sink-atom*` (dynamic var, default nil): per-iteration
  collector. When bound, every (def …) the SCI sandbox runs lands as
  `{:ns :name :init :meta :var}`.
- `*lru-atom*` + `*current-turn-position*`: per-iteration LRU stamps;
  `(name sym) -> turn-pos` for every successful sandbox-symbol resolve.
- `validate-single-form-block!` utility — throws `:vis/multi-form-block`
  or `:vis/empty-block`; not yet wired into the engine.
- `count-top-level-forms` parser helper.
- `fresh-sink-atom` / `fresh-lru-atom` ctors for the engine to bind at
  iteration start.
- Two `alter-var-root` monkey-patches with `defonce`-guarded originals
  and startup precondition checks. Patches `sci.impl.evaluator/eval-def`
  + `sci.impl.resolve/resolve-symbol*`.

### `com.blockether.vis.internal.env`

- `EXTRA_BINDINGS` (line ~193): `view` / `summary` / `kind` / `handle?`
  injected as bare sandbox primitives.
- `SYSTEM_VAR_NAMES` (line ~589): includes `USER_REQUEST`.
- `tape-system-vars` (line ~844): walks sandbox, returns
  `[{:name :doc} …]` for UPPERCASE engine-managed vars.
- `tape-live-vars` (line ~860): walks sandbox + LRU map, returns
  `[{:name :doc} …]` filtered by `TAPE_LRU_TURN_WINDOW = 10`, capped at
  `TAPE_LIVE_VARS_CAP = 30`. Vars without LRU stamps are kept (missing ≠
  stale).
- All pre-existing `(def …)` SCI eval calls now ship docstrings to
  satisfy the patch contract (env-injected `*1 *2 *3 *e`,
  `sci-update-binding!`).

### `com.blockether.vis.internal.prompt`

Phase 7 prep primitives (lines ~993-1280):

- `tape-iteration-header`, `tape-result-line`, `tape-side-effect-line`
- `format-tape-iteration` (single iteration → tape entry)
- `format-system-vars-block`, `format-live-vars-block`
- `format-tape` (multi-iteration window — N-1 or N-1 + N)
- `format-user-role-tape-message` (full body: hints + system-vars +
  live-vars + tape)
- `iteration->tape-iter` (engine multi-block iteration shape → tape-iter)
- `build-iteration-context-tape` — drop-in replacement for
  `build-iteration-context`. Same opts contract; different output shape.

### `com.blockether.vis.internal.loop`

- Per-iteration `*def-sink-atom*` + `*lru-atom*` + `*current-turn-position*`
  bound around `(sci/eval-string+ …)` in `run-sci-code` (line 232+).
  Snapshots flow out via `:def-sink` and `:lru` on the
  execution-result map. Engine collects them but doesn't yet read.

### Foundation (`vis-foundation`)

- `v/cat` / `v/rg` / `v/ls` return `Handle` records as their tool
  envelope `:result` (lines 626/666/650 of editing/core.clj).
- Old `v/view` symbol registration removed (view is engine-level now).
- `journal-render-cat` / `journal-render-rg` / `journal-render-ls`
  collapsed to one-line handle pr-str + read-more hint (forward-compat
  with Phase 7).
- `channel-render-*` deref the handle for human display.

### Other shipped pieces

- `(turn-answer! …)` → `(done …)` rename across 96 sites.
- `--plain` CLI flag removed; `--raw` is the only raw-text flag.
- Stale `z/` scrub across model-facing strings, internal comments, and
  AGENTS.md.

---

## Current status (after `60bdcde5`)

Shipped and verified (`./verify.sh` all 7 steps green, 957 tests):

- Phase 5 hard schema cut landed for production code.
  `conversation_turn_iteration` no longer has `code_blocks` or
  `answer_form_idx`; it has flat `code` / `result` / `error` /
  `stdout` / `stderr` / `duration_ms` columns.
- Persistence writer is hard-cut: `db-store-iteration!` requires flat
  `:code` and rejects legacy `:blocks` fixtures.
- Public `db-list-iteration-blocks` facade removed. Remaining callers
  consume flat iteration rows and, where needed, build a one-entry
  display adapter locally.
- LLM block-count policy is enforced before eval:
  - 0 executable Clojure blocks → model-facing error iteration with
    `llm_returned_empty_code = 1`.
  - >1 executable Clojure blocks → model-facing error telling the model
    to emit exactly one block and wrap multiple expressions in `(do ...)`.
  - 1 executable Clojure block → eval path, then single-top-level-form
    validation applies.
- `run-sci-code` / `execute-code` now call
  `sci-patches/validate-single-form-block!` at the model boundary.
- TUI resume, foundation transcript, and foundation introspection read
  flat iteration payloads.
- Persistence and transcript fixtures were moved off legacy `:blocks`
  where they hit storage. Prompt tape tests still intentionally use the
  adapter shape because tape primitives accept legacy/in-memory
  `{:blocks [...]}` until Phase 7 removes that adapter.

Still not done:

- Phase 4 is only partial: actual engine still builds internal
  `:blocks` vectors after eval; def persistence still uses
  `restorable-var-snapshots` instead of flushing `:def-sink` directly;
  `extract-defining-name` still exists.
- Dependency edges are not captured from runtime symbol resolution yet.
- Phase 7 prompt swap not done: live path still calls legacy
  `build-iteration-context`; `CORE_SYSTEM_PROMPT` still contains old
  scaffolding.
- USER_REQUEST injection as sandbox `(def USER_REQUEST "..." ...)` is
  not wired.
- Long-lived per-env LRU merge is not wired into prompt assembly.
- Phase 8 docs / CHANGELOG / `vis db reset` helper remain.

## Remaining work

Order now: finish Phase 4 (def-sink + dependency flush + scalar engine
shape) → Phase 7 prompt swap → Phase 6 remaining channel audit → Phase 8
docs/reset helper.

### Phase 5 — Schema collapse (HARD CUT) — DONE in `60bdcde5`

Done:
- Edited V1 in place, no migration ladder.
- Dropped `code_blocks BLOB` and `answer_form_idx INTEGER`.
- Added flat payload columns: `code TEXT NOT NULL`, `result BLOB`,
  `error BLOB`, `stdout TEXT`, `stderr TEXT`, `duration_ms INTEGER`.
- Added FTS trigger coverage for `conversation_turn_iteration.code`.
- Renamed stale empty-block tracking to `llm_returned_empty_code`.
- `db-store-iteration!` now requires flat `:code` and writes flat
  payload columns.
- `db-list-conversation-turn-iterations` returns decoded flat payload
  fields directly.
- Removed public `db-list-iteration-blocks` facade.
- Updated persistence, TUI resume, transcript, and introspection callers
  that touched persisted iteration blocks.
- `./verify.sh` green after update.

Still intentionally unchanged:
- `expression_soul` / `expression_state` schema is unchanged.
- `expression_dependency` schema is unchanged.
- `db-restore-blocks` remains var-topology based and does not read
  iteration code payloads.

### Phase 4 main — Iteration loop collapse (PARTIAL)

**Files:**
- `src/com/blockether/vis/internal/loop.clj` (the big one)
- Tests across `test/`, `extensions/.../test/` that exercise the
  multi-block iteration shape.

**The cut:**

1. **Reject multi-form blocks at the model boundary.** — DONE. In
   `run-sci-code` (line 232+), call
   `sci-patches/validate-single-form-block!` on `code` BEFORE
   `(sci/eval-string+ …)`. Throws `:vis/multi-form-block` on miss; the
   engine catches this and surfaces the error to the model in the next
   iteration's tape (`;; ! ERROR Block contains N top-level forms …`).

   Engine-internal SCI evals (env.clj `sci-update-binding!`,
   loop.clj `attach-doc-meta!`, `require-extension-alias!`) call
   `sci/eval-string+` directly and bypass `run-sci-code`, so they're
   unaffected.

2. **Collapse `code-entries-preflight` + the multi-block iteration
   `mapv`.** — PARTIAL. Currently each iteration parses N top-level forms and
   loops `execute-code` per form (line ~1730+). With one-form-per-block
   this becomes a single `execute-code` call. The wrapping
   `mapv → vec → map indexed result` collapses to a scalar. Drop the
   per-form `*sink-position*` machinery (`*journal-render-sink*` /
   `*channel-render-sink*` / position counter) — there's no
   between-form coordination to do.

3. **Flush `*def-sink-atom*` to `expression_state` rows.** — TODO. After
   eval returns, walk `(:def-sink execution-result)` and write one
   `expression_soul` row (or upsert the existing soul) + one
   `expression_state` row per def. This replaces the post-eval
   `extract-defining-name` parser path. Same transaction as the
   iteration row write.

4. **Soften the `(done …)` gate.** — TODO. Today's gate (`prior-error-atom`,
   `block-result-error-summary`, `answer-with-extension-preflight-*`)
   exists to catch "tool-call-fails-then-done-fakes-success" patterns
   from the multi-block flow. With one-form-per-block, these can
   collapse to: "did the form throw?" If yes, the turn continues; if
   no, the turn ends. Delete the gate machinery.

5. **Push `*e` on throw; do NOT advance `*1/*2/*3` on failure.** Already
   in `run-sci-code` (lines 290-316). Verify still correct after the
   collapse.

6. **Delete `extract-defining-name`** — TODO. `loop.clj:332+` and its
   callers. The def-sink replaces it.

**Test impact:** every test that constructs an iteration with
multi-form `:code` needs to either (a) wrap in `(do …)` to satisfy the
single-form contract, or (b) split into multiple iterations. Most
fixtures in `extensions/persistance/.../core_test.clj` are
single-form already; the engine-side iteration tests in
`test/com/blockether/vis/internal/loop_test.clj` need scrutiny.

**Acceptance:**
- Model emits `(def x "doc" 42) (def y "doc" 43)` → engine throws
  `:vis/multi-form-block` → next iteration's tape shows the error.
- Model emits `(do (def x "doc" 42) (done [:ir [:p "ok"]]))` → both
  defs run, def-sink captures `x` + writes `expression_state` row,
  `(done …)` ends the turn.
- Model emits `(/ 1 0)` → tape shows `;; ! ERROR Divide by zero`,
  iteration continues.
- `extract-defining-name` no longer exists.
- Full suite green.

---


### Phase 4b — Expression souls + dependencies (NEXT)

**Goal:** vars become memory. Dependency edges become restore/order facts,
not inferred prose.

Done already:
- SCI `eval-def` patch captures every sandbox def into `:def-sink`.
- SCI `resolve-symbol*` patch records per-iteration symbol resolutions
  into `:lru`.
- `db-store-iteration!` can already write `:vars` into
  `expression_soul` / `expression_state` and `db-restore-blocks` reads
  latest var states in topological order.

Still needed:

1. **Persist defs from `:def-sink`, not source parsing.**
   - Delete `restorable-var-snapshots` as primary path or reduce it to
     compatibility-only test helper.
   - For each sink entry, derive:
     - `name` from sink `:name`
     - `value` from derefing sink `:var`
     - `code` from the current single block source
     - `doc` from sink `:meta :doc`
   - Write one expression_state row per def in the same DB transaction as
     the iteration row.

2. **Capture dependency edges during def init.**
   - Add dynamic `*current-def-name*` / `*current-def-soul*` around
     `eval-def` init evaluation or collect symbolic downstream name
     first and resolve soul ids at flush time.
   - Extend resolve patch so when a user var resolves while a def is
     evaluating, it records `upstream-name -> downstream-name`.
   - Filter out core symbols, extension aliases, and SYSTEM vars unless
     explicitly whitelisted.
   - Flush edges to `expression_dependency` after souls exist.

3. **Keep LRU separate from dependency capture.**
   - LRU is prompt-discovery recency.
   - Dependencies are semantic var inputs.
   - Same resolve hook can feed both sinks, but persistence should keep
     separate data structures.

4. **Regression tests.**
   - `(do (def a "doc" 1) (def b "doc" (inc a)))` writes souls for
     `a`, `b` and edge `a -> b`.
   - Re-defining `a` creates new state version, not new soul.
   - Restore order is `a` before `b`.
   - Core symbols like `inc` do not become expression_soul deps.

Acceptance:
- `extract-defining-name` no longer exists.
- Vars persisted from def-sink only.
- `expression_dependency` has user-var edges for defs that read prior
  user vars.
- `db-restore-blocks` restores derived vars after upstream vars.
- Full suite green.

### Phase 7 main — Prompt restructure (MODEL-FACING PAYOFF)

**Files:**
- `src/com/blockether/vis/internal/loop.clj` (call-site swap)
- `src/com/blockether/vis/internal/prompt.clj` (CORE_SYSTEM_PROMPT
  rewrite + delete legacy `build-iteration-context`)
- Tests (anything asserting on `<journal>` / `<bindings>` /
  `<current_user_message>` / `<current_turn_context>` substrings)

**The swap:**

1. **`loop.clj:2791`** — replace
   `(prompt/build-iteration-context env opts)` with
   `(prompt/build-iteration-context-tape env opts)`. Pass:

   ```clojure
   {:active-extensions   active-exts
    :blocks-by-iteration journal-iters
    :iteration           iteration
    :current-status      :current
    :system-vars         (env/tape-system-vars (:sci-ctx env))
    :live-vars           (env/tape-live-vars (:sci-ctx env)
                                             (:initial-ns-keys env)
                                             @per-env-lru
                                             current-turn-pos)}
   ```

2. **Inject USER_REQUEST as a (def …) at turn start.** In whichever
   loop.clj fn handles "turn boundary" (search for
   `current-user-request-atom` at ~3830+). Before the first iteration
   eval, run:

   ```clojure
   (sci/eval-string+ sci-ctx
     (str "(def USER_REQUEST \"current turn user request\" "
          (pr-str user-request) ")")
     {:ns sandbox-ns})
   ```

   The patched eval-def captures it into the def-sink (Phase 4 main
   wires this through to expression_state), and the next iteration's
   `tape-system-vars` surfaces it.

3. **Wire the per-env LRU.** Add a long-lived per-env atom (alongside
   `:bindings-atom`) holding the merged LRU map. After each iteration:

   ```clojure
   (swap! per-env-lru merge (:lru execution-result))
   ```

   `tape-live-vars` reads this map.

4. **Rewrite `CORE_SYSTEM_PROMPT`** (`prompt.clj:762-817`). Drop the
   `<journal>` / `<bindings>` / `EMIT_FINAL` (multi-step) / `LOOP
   DISCIPLINE` sections. Add the new contract:

   ```
   λVis — Clojure SCI harness, one block per iteration.

   ENV  : aliases (str, edn, set, walk, pp, s, json); banned (slurp, spit, eval).
   DEF  : (def NAME "docstring" VAL) — docstring required. defn / defmacro likewise.
          Vars persist across iterations; sandbox is durable per conversation.
   HND  : foundation tools return Handle records (IDeref). @h or (deref h)
          to materialize; (view h :op …) for bounded windows. (handle? v) tests;
          (kind h) / (summary h) inspect any value (non-handles return
          :not-a-handle structured data, not exceptions).
   TAPE : the prior iteration renders as commented Clojure source above.
          ;; --- iteration N | turn=K conv=… state=… | status=done|error|current ---
          <code>
          ;; => result of last form
          ;; ! stdout> / ! stderr> / ! ERROR / ! TIMEOUT
          The CURRENT iteration's status is :current — write your block in
          that section.
   ANS  : (done [:ir <blocks>]) terminates the turn iff the form ran without
          throwing. The IR grammar is unchanged.
   ```

   Plus the existing ANSWER_IR grammar block. Target: ~30 lines (down
   from ~75).

5. **Delete legacy `build-iteration-context`** and its unique helpers
   (`format-journal-block`, `format-journal-iteration-block`,
   `trim-journal-lines`, `read-bindings-str`, `bindings-token-budget`,
   `trim-bindings-str`, `split-bindings-entries`,
   `current-turn-context-block`). All only called by the legacy fn.
   Drop the tunables that go with them: `JOURNAL_CONTEXT_FRACTION`,
   `BINDINGS_CONTEXT_FRACTION`, `MAX_JOURNAL_TOKENS`,
   `MAX_BINDINGS_TOKENS`.

**Test impact:** any test asserting on `<journal>` / `<bindings>`
substrings in assembled prompts breaks. Update to assert on the new
`;; system-vars:` / `;; live-vars (N/30):` / `;; --- iteration N …`
shapes (the prompt-tape-test file already exercises this; no new
test infrastructure needed).

**Acceptance:**
- Model running a 1-iteration probe: prompt contains `<iteration_hints>`
  (if any) + `;; system-vars:` (USER_REQUEST + any TURN_*) + `;;
  live-vars (0/30):` (empty until model defs something) + tape entry
  for the current iteration with `status=current`.
- 2-iteration run: turn 2 iter 1 shows turn 1 iter 5 (last) as the
  prior-iteration tape entry, with `turn=1`. New iteration header
  follows with `turn=2 status=current`.
- Error iteration: tape shows N-1 (status=done) + N (status=error
  with `;; ! ERROR …`).
- `CORE_SYSTEM_PROMPT` text mentions neither `<journal>` nor
  `<bindings>`.

---

### Phase 6 — Channel renderer update

**Files:**
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/...`
- `extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/...`
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/transcript.clj`
  (the `vis transcript` CLI surface)

**The change:**

Channels read iteration data from the new schema columns directly
(Phase 5 produced the new shape). Drop any legacy code that decodes
`code_blocks` BLOB or assumes multi-block-per-iteration shape. The
channel renderer for a single iteration now consumes:

- `code` (single string)
- `result` (Nippy-decoded value; render via existing render pipeline)
- `error` (Nippy-decoded; render via channel error formatter — already
  factored)
- `stdout` / `stderr` (text, render as collapsed blocks)
- per-iteration metadata (duration, status, etc.)

**Acceptance:**
- TUI conversation list view renders correctly against fresh DB.
- TUI conversation detail view renders an iteration's code + result.
- Telegram channel posts iteration summaries with no missing-key
  errors.
- `vis transcript --conversation <id>` produces a well-formed Markdown
  transcript.
- Snapshot tests prevent visual regression: capture sample transcript
  output before Phase 6, assert post-Phase-6 output matches.

---

### Phase 8 — Cleanup & docs

**Files:**
- `README.md` (the "inspired by RLM" line)
- `CHANGELOG.md` (document the hard cut-over + new contract)
- `AGENTS.md` (update if Phase 4/5/7 main change any rule)
- Various unreachable code paths uncovered by 4-7

**The change:**

- README: graduate "inspired by Recursive Language Models" to
  "RLM-conformant: external state in the SCI sandbox; constant N-1
  tape window; recursion-replacement via mandatory-doc vars."
- CHANGELOG: document the hard schema cut, the `(done …)` rename, the
  bare `view`/`summary`/`kind` primitives, the dropped `<journal>` /
  `<bindings>` blocks, and the `vis db reset` migration helper.
- Delete unreachable code paths (the legacy iteration mapper helpers,
  the journal sink machinery referenced from removed renderers, etc.).
- Audit `AGENTS.md` for any stale references to multi-block / journal /
  bindings.

---

## Cross-cutting concerns (read before touching engine code)

### SCI monkey-patch fragility

Two patches live in
`src/com/blockether/vis/internal/extension/sci_patches.clj`. Both are
on `:no-doc` impl namespaces:

1. `sci.impl.evaluator/eval-def` — signature
   `[ctx bindings var-name init m]` stable from sci 0.8.41 → 0.12.51
   (5 cached releases). The patch evaluates `m` via
   `sci-types/eval` BEFORE checking `:doc` (SCI passes `m` as an
   unevaluated Node — same dance the original does on its first line).
2. `sci.impl.resolve/resolve-symbol*` — wraps the lookup; stamps
   `*lru-atom*` on success. The patch over-includes (every successful
   resolve, including core ops); filtering is the renderer's job.

Both patches are guarded by `defonce` for both the captured original
and the install action so namespace reload never wraps the wrap.
Startup precondition checks fail loud (`:vis.sci-patches/precondition-failed`)
if a SCI bump renames or moves either fn.

If SCI ships official `:hook` config in a future release, drop both
patches and adopt the official surface.

### Engine-injected SCI defs need docstrings too

The mandatory-docstring contract applies in production code, not just
model code. Engine-side SCI eval calls that emit `(def …)` must ship
docstrings:
- `env.clj/sci-update-binding!` (line ~41) — vis-managed engine binding
- `env.clj/create-sci-context` `*1`/`*2`/`*3`/`*e` bootstrap (line ~487)
- `loop.clj` USER_REQUEST injection (NEW, Phase 7 main)

### `view` / `summary` / `kind` non-handle fallback

`(view 42 :peek)` returns
`{:kind :not-a-handle :value 42 :op :peek :hint "View / summary / kind operate on Handle records. …"}`
instead of throwing. Same for `(summary nil)` and `(kind {:some :map})`.
The model gets a structured correction signal pointing at `handle?` /
`v/cat` / `v/rg` / `v/ls` instead of a stack trace. Don't change this
without thinking about the model UX implication.

### LRU "missing stamp" semantics

`tape-live-vars` keeps vars whose name has no LRU stamp. The LRU is
*informational* — when the engine just started or the model never
referenced a var by name, the stamp is missing but the var is still
fresh. Only vars with an explicit stale stamp
(`current-turn-pos - last-used > 10`) are hidden.

### pprint dispatch ambiguity

Handle records implement BOTH `IPersistentMap` (defrecord) AND
`IDeref`. Without the `prefer-method` registered in handle.clj,
`clojure.pprint/simple-dispatch` throws "Multiple methods … match
dispatch value". Keep the preference; deleting it will surface
mysteriously in test failure pretty-printing.

### Handle store is process-wide

The `defonce` LRU store in handle.clj is a single atom shared across
every conversation in the JVM. Bounded by `MAX_STORE_BYTES = 16 MB`
with eviction-on-insert. If conversation isolation becomes important
(e.g. multi-user web channel), wire a per-env handle store via a
different key scheme — but DON'T do this until there's a real need.

---

## Risks (and what to watch for)

- **R1 — SCI monkey-patch silently breaks on next SCI bump.** The
  precondition catches signature renames at startup but not subtle
  semantic changes. Run `clojure -M:test --namespace
  com.blockether.vis.internal.extension.sci-patches-test` after every
  SCI dependency bump.
- **R2 — Mandatory docstring tripping engine-internal eval paths.** Any
  new engine code that emits `(def …)` via SCI without a docstring
  fails immediately. Search for `sci/eval-string+` calls that include
  `def` in the source string.
- **R3 — N-1 tape window losing important state.** If a long-lived var
  ages out of the live-vars surface (>10 turns idle) and the model
  forgets it exists, it'll re-shadow with a new def. The pivot intent
  IS this — vars-as-memory means the model curates what it
  references. But on legacy traces it'll look wrong.
- **R4 — Hard schema cut breaks open conversations.** Document loud in
  CHANGELOG. The `vis db reset` CLI helper needs explicit user
  confirmation.
- **R5 — Channel rendering visual regressions.** Snapshot tests before
  Phase 6 ship. Compare side-by-side.
- **R6 — `iteration->tape-iter` adapter divergence.** Today the adapter
  joins multi-block iterations (legacy data); post-Phase 4 main, every
  iteration is single-block. Both shapes pass the same adapter; tests
  cover both.

---

## Out of scope

- **`v/ask` recursion primitive** (the prior PLAN's Phase 4
  keystone). The pivot achieves constant parent context structurally
  via N-1 tape + handle returns; `v/ask` is no longer load-bearing.
  May ship later as a power feature.
- **Persisting handles across turns.** Process-wide store today;
  per-conversation isolation is a future-only concern.
- **Multi-block iterations.** The pivot is one block per iteration. If
  a future need arises (parallel probes, etc.), introduce a new
  primitive — do NOT revert the loop shape.
- **Backward compatibility with legacy conversations.** Hard cut-over.
- **Forking SCI.** Monkey-patch is the chosen mechanism. Reversibility
  beats purity.

---

## Done state

A turn that asks "summarize README.md" runs in **1 iteration**:

```clojure
;; model emits, atomically:
(do
  (def big "README handle" (v/cat "README.md"))
  (done [:ir [:p (str "README has " (:line-count (summary big)) " lines")]]))
```

The next turn's prompt shows: `<iteration_hints>` (if any), iteration
header (`turn=2 conv=… state=… status=current`), system-vars
(`USER_REQUEST` and any TURN_* set by extensions), live-vars (`big
"README handle"`), prior iteration's tape. No XML noise beyond
`<iteration_hints>`. No `<journal>`. No `<bindings>`. No
`<current_user_message>`. No `<current_turn_context>`.

Targets:
- Single-iteration probe-and-answer for typical "summarize / fetch +
  describe" prompts.
- Per-iteration prompt size: **<30 %** of pre-pivot baseline.
- All phases shipped; `./verify.sh` green.
- README's "inspired by RLM" line graduates to "RLM-conformant".

When the canonical 1-iteration trace runs end-to-end with the new
contract enforced and the prompt prints as designed, ship the README
update and close the pivot.
