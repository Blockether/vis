# PLAN — Pivot: one block per iteration; vars are the memory

> Supersedes the prior RLM-phases plan (handles + slice tools + `v/ask`
> recursion primitive). Git history preserves that version. The pivot
> reaches the same RLM-conformance goal by a different route: instead
> of recursive sub-model invocation, it makes the parent context
> structurally constant via an N-1 tape window plus mandatory-doc vars.

## Why this exists

The autoresearch bench bottomed out at `iter_score = 6` across multiple
stability samples (commits `422762ac`, `0f38c067`). The structural
floor was two iterations: probe + answer, enforced by the
`(turn-answer! …)` gate. Pushing lower meant changing the gate and
the prompt scaffolding around it, not optimizing inside them.

The pivot collapses the iteration model to its smallest coherent
shape:

- **One ```clojure block per iteration**, evaluated atomically.
- **No `<journal>` block. No `<bindings>` block.**
- **Cross-iteration state lives only as named vars** in the SCI sandbox.
- **The model reads its own REPL tape** as commented Clojure source.
- **Defs are mandatory-docstring**, persistence-instrumented, LRU-pruned.

The pivot is intentionally a HARD CUT-OVER. No migration of existing
conversations. The schema collapses, the prompt slots delete, the gate
softens.

## Decisions (settled)

| # | Decision |
|---|---|
| 1 | One ```clojure block per iteration; engine rejects multi-form blocks at parse time. |
| 2 | `<journal>` deleted; `<bindings>` deleted. |
| 3 | Tape window: prior iteration only (N-1); on error, render N-1 + N. Cross-turn: last iteration of the prior turn carries forward. |
| 4 | Tape format: rich-comment `;; =>` for results, `;; ! stdout>` / `;; ! stderr>` / `;; ! ERROR` / `;; ! TIMEOUT` for side effects. |
| 5 | `(turn-answer! [:ir …])` strict IR shape; accepted on no-throw, no other gate. |
| 6 | Iterations are the durable unit; turns are projections. Same `conversation_turn_*` tables, but the boundary is an annotation, not a container. |
| 7 | Plain `(def NAME "doc" VAL)` and `(defn NAME "doc" [args] body)`. **Mandatory docstring**. Engine throws on miss. |
| 8 | Per-iteration `*def-sink-atom*`; engine flushes once at iteration-end inside the same transaction as the iteration row. |
| 9 | Capture mechanism: monkey-patch `sci.impl.evaluator/eval-def` from vis bootstrap (`alter-var-root` wrap). Catches every def — including macro-expanded `defn` / `defmacro` / `s/def`. |
| 10 | Second monkey-patch on the SCI symbol-resolver hot path for runtime-resolution LRU bookkeeping. |
| 11 | 30-binding cap on the live-vars discovery surface; 10-turn LRU eviction based on runtime resolution. |
| 12 | Live-vars renderer: names + docstrings, single block above the tape. |
| 13 | Handles: per-kind `defrecord` (CatHandle, RgHandle, LsHandle, …) implementing the `PHandle` protocol in `com.blockether.vis.internal.extension.handle`. Each implements `clojure.lang.IDeref` and ships a `print-method` that renders a one-line `#vis/handle` summary. The protocol lives in the engine extension API so any extension (vis-foundation today; vis-sql / vis-http tomorrow) can ship its own handle kinds. |
| 14 | `v/cat`, `v/rg`, `v/ls` return `Handle` instances. Slice tools `v/lines`, `v/at`, `v/peek`, `v/grep-in` materialize bounded windows. |
| 15 | `USER_REQUEST` is a regular `(def USER_REQUEST "current turn user request" "<text>")` injected at turn start. Lives in the system-vars section. |
| 16 | Schema: collapse `code_blocks` BLOB; promote `code` / `result` / `error` / `stdout` / `stderr` / `duration_ms` columns directly on `conversation_turn_iteration`. |
| 17 | Keep `expression_soul` / `expression_state`. Multiple state versions per iteration allowed (each def writes its own version). |
| 18 | `<iteration_hints>` XML block survives — extension nudges are advisory, not state, and the existing `:turn.iteration/start` hook contract stays intact. |
| 19 | Telemetry per iteration: `;; --- iteration N \| turn=K conv=… state=… \| status=done\|error\|current ---` header before each rendered iteration's code in the tape. Cross-turn boundary implicit in the IDs. |

## What the model sees, end-to-end

**SYSTEM (cached prefix, changes only on extension reload):**

```
<system_prompt>
  λVis — Clojure SCI harness, one block per iteration.
  ENV  : aliases (str, edn, set, walk, pp, s, json); banned (slurp, spit, eval).
  DEF  : (def NAME "docstring" VAL) — docstring required. defn/defmacro idiomatic.
  HND  : foundation tools return Handle records (IDeref). @h or (deref h) to
         materialize; slice tools (v/lines v/peek v/at v/grep-in) for bounded windows.
  TAPE : prior iteration renders as commented Clojure source.
         ;; =>      result of a form
         ;; ! stdout> / ! stderr> / ! ERROR / ! TIMEOUT
  ANS  : (turn-answer! [:ir <blocks>]) terminates the turn iff it ran without
         throwing. ANSWER_IR grammar unchanged.
</system_prompt>

<turn_system_context>
  <extensions>
    <extension id="v">
      v/cat v/rg v/ls v/lines v/at v/peek v/grep-in v/patch v/patch-check
      v/source v/doc v/conversation-state ...
    </extension>
  </extensions>
</turn_system_context>
```

**USER (per iteration):**

```
<iteration_hints>
  <iteration_hint importance="high">context window 88% — converge soon</iteration_hint>
</iteration_hints>

;; --- iteration 5 | turn=1 conv=ab12 state=cd34 | status=done ---
(def big "README handle" (v/cat "README.md"))
;; => #vis/handle {:kind :v.cat :line-count 5 :sha "ab12"}
(turn-answer! [:ir [:p "Done"]])
;; => :vis/answered

;; --- iteration 1 | turn=2 conv=ab12 state=cd34 | status=current ---
;; system-vars:
;;   USER_REQUEST  "current turn user request"
;; live-vars (1/30):
;;   big           "README handle"
```

The user-role message has one XML block (`<iteration_hints>`) plus
pure commented Clojure. No `<current_user_message>`, no
`<current_turn_context>`, no `<journal>`, no `<bindings>`. USER_REQUEST
is a real `def` — the model interpolates it like any other var.

## Architecture

### The new iteration loop

```
turn start
  └─ engine binds USER_REQUEST  (def, persisted via monkey-patch)
       └─ iteration N
            ├─ provider sees: SYSTEM + USER (hints, telemetry, vars, tape)
            ├─ model emits one ```clojure block (one top-level form, or
            │  one top-level (do …)).
            ├─ engine SCI-evals atomically.
            │   ├─ each (def …) invokes the monkey-patched eval-def:
            │   │   - throws if no docstring;
            │   │   - pushes {:ns :name :init :form :var} to *def-sink-atom*.
            │   ├─ each symbol resolution stamps last-used-turn on the
            │   │  per-env LRU map (second monkey-patch).
            │   └─ stdout / stderr captured per-iteration (no per-form sinks).
            ├─ engine flushes *def-sink-atom* to expression_soul / state
            │  rows in the same transaction as the iteration row write.
            ├─ if (turn-answer! …) form ran without throwing → turn ends.
            └─ otherwise → next iteration; tape carries N-1 (and N on error).
```

### Defs persistence

Hook signature (added by `alter-var-root` from vis bootstrap):

```clojure
(let [orig (var-get #'sci.impl.evaluator/eval-def)]
  (alter-var-root #'sci.impl.evaluator/eval-def
    (fn [_]
      (fn [ctx bindings var-name init m]
        (when-not (:doc m)
          (throw (ex-info "def requires a docstring"
                          {:type :vis/missing-docstring :var var-name})))
        (let [v (orig ctx bindings var-name init m)]
          (when-let [sink @vis-def-sink-atom]
            (sink {:ns (str (:ns m)) :name var-name :init init
                   :meta m :var v}))
          v)))))
```

- Catches every `def` — `defn`, `defmacro`, `s/def`, runtime-induced.
- Per-iteration sink atom (default empty vec); engine binds it before
  eval and reads it after.
- Failure mode: model omits docstring → SCI throw → tape shows
  `;; ! ERROR def requires docstring`. Self-corrects in next iteration.

### LRU bookkeeping

Hook on the SCI symbol-resolver hot path stamps every user-var
resolution with the current turn position. Live-vars renderer reads
the LRU map; vars unused for ≥10 turns are hidden from the surface
(the var stays alive in the sandbox; nothing is evicted from the
namespace).

- 30-cap: when more than 30 user vars are within the 10-turn window,
  drop oldest first.
- system-vars (UPPERCASE convention, matches existing
  `SYSTEM_VAR_NAMES` set in `env.clj:578`) don't count toward the
  30-cap; they render in their own section above live-vars.

### Handles

```clojure
(defrecord Handle [kind store-key meta]
  clojure.lang.IDeref
  (deref [_]
    (or (get @*handle-store* store-key)
        (throw (ex-info "Handle expired" {:kind kind})))))

(defmethod print-method Handle [h ^java.io.Writer w]
  (.write w (str "#vis/handle "
                 (pr-str (assoc (:meta h) :kind (:kind h)))))) ; one line
```

- `Handle` lives in `extensions/common/vis-foundation/src/.../handle.clj`.
- Per-environment `*handle-store*` atom keyed by `store-key`. Bounded
  by `MAX_HANDLE_STORE_BYTES` (start at 16 MB, LRU on insert).
- `(v/cat "X.md")` returns a Handle whose `:meta` is `{:path "X.md"
  :line-count 5000 :sha "ab12…" :first-line "…" :last-line "…"}`.
- Slice tools: `(v/lines h from to)` is `(subvec @h from to)` with a
  64 KB byte cap; `(v/at h n)`; `(v/peek h)` renders the first window
  to the channel only; `(v/grep-in h pattern)` returns a new
  hits-Handle.

### turn-answer!

- Strict IR: `[:ir & blocks]`. Same Hiccup grammar.
- Accepted iff the form ran without throwing. No prior-evidence
  check, no clean-iteration check, no "tool calls in same iteration
  drop the answer" rule.
- A single block can do
  `(def big "doc" (v/cat …)) (turn-answer! [:ir [:p (str "Lines: " (:line-count @big))]])`
  and finish in one iteration.

### Schema (V1, hard cut)

Edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`
in place per AGENTS.md. Existing local databases must be deleted by
the user — see Risks §R4.

`conversation_turn_iteration` changes:

- DROP `code_blocks BLOB`.
- DROP `answer_form_idx`.
- ADD `code TEXT NOT NULL` — the single block source verbatim.
- ADD `result BLOB` — Nippy-encoded final return value.
- ADD `error BLOB` — Nippy-encoded structured error map.
- ADD `stdout TEXT`.
- ADD `stderr TEXT`.
- ADD `duration_ms INTEGER CHECK (duration_ms IS NULL OR duration_ms >= 0)`.

`expression_soul` / `expression_state` unchanged. Multiple state
versions per iteration are already allowed by the schema (UNIQUE
on `(soul_id, version)` with monotonic `version`).

FTS5 trigger on `conversation_turn_iteration.code` indexes the new
`code` column directly (today indexes only `expression_state.expression`
and `conversation_turn_soul.user_request`).

## Phases

Each phase ships independently and is keep-able on its own. Full
suite green between phases.

### Phase 0 — stale `z/` scrub

The earliest cleanup, decoupled from the rest. Five sites in
foundation reference a long-removed `z/` extension:

- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj:33, 1194, 1197`
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/introspection.clj:808, 816`

Plus internal docs (`prompt.clj:238`, `extension.clj:922,1393`) — no
behavior change, just stale comment cleanup.

### Phase 1 — Handle defrecord + slice tools

Foundation gets the `Handle` type and slice tools. `v/cat` switches
to returning a `Handle`; `:lines` reachable via `(deref h)` /
`(v/lines h from to)`. Existing tests update to assert handle shape.

Files:
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/handle.clj` (new)
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj`
  (cat-tool, ls-tool, rg-tool return handles; new slice tools)
- `src/com/blockether/vis/internal/env.clj` (wire `:handle-store` into
  `create-sci-context`)

Tests:
- `handle_test.clj` — defrecord, deref, print-method one-liner, store
  LRU eviction.
- `editing/core_test.clj` — `(v/cat …)` returns Handle; `@h` returns
  full lines vec; `(v/lines h 0 10)` slice; `(v/peek h)` renders to
  channel only.

### Phase 2 — SCI eval-def monkey-patch + def-sink + docstring enforcement

Bootstrap installs the patch at JVM start. Per-iteration sink atom
collected, flushed at iteration-end.

Files:
- `src/com/blockether/vis/internal/sci_patches.clj` (new) — single
  source of monkey-patches with startup precondition checks.
- `src/com/blockether/vis/internal/env.clj` — bind `*def-sink-atom*`
  per env; expose flush helper.
- `src/com/blockether/vis/internal/loop.clj` — call flush after eval,
  inside the iteration row write transaction.

Tests:
- `sci_patches_test.clj` — hook fires on raw `def`, `defn`,
  `defmacro`, `s/def`. Docstring miss throws structured error.
  Patch precondition asserts SCI ns + var + arity.

### Phase 3 — SCI symbol-resolver monkey-patch + LRU map

Per-env LRU map of `var-name → last-used-turn-position`. Resolver
hook stamps on every read.

Files:
- `src/com/blockether/vis/internal/sci_patches.clj`
- `src/com/blockether/vis/internal/env.clj` — wire LRU map into env.

Tests:
- LRU stamping correctness. 10-turn eviction policy. 30-cap drop
  policy. system-vars exempt.

### Phase 4 — Iteration loop collapse

`loop.clj`:
- Reject blocks containing more than one top-level form (or accept
  a single top-level `(do …)` as one block).
- Replace per-form journaling with single-form atomic eval.
- Push `*e` on throw; do NOT advance `*1/*2/*3` on failure.
- Drop `extract-defining-name` parser, `journal-render-tool-result`
  per-tool dispatch, and the multi-block sink machinery
  (`*sink-position*`, `*journal-render-sink*`, `*channel-render-sink*`).

### Phase 5 — Schema collapse (V1, hard cut)

Edit V1 in place. Update backend writers/readers in
`extensions/persistance/vis-persistance-sqlite/src/.../core.clj`.
CHANGELOG documents the hard cut-over and the
`vis db reset` CLI surface (Phase 6).

### Phase 6 — Channel renderer update

TUI / Telegram / web consume new schema columns directly. Remove any
journal-derived rendering paths.

### Phase 7 — Prompt restructure + CORE_SYSTEM_PROMPT shrink

`src/com/blockether/vis/internal/prompt.clj`:
- Delete `format-journal-block`, `format-journal-iteration-block`,
  `trim-journal-lines`, `read-bindings-str`, `bindings-token-budget`,
  `trim-bindings-str`, `split-bindings-entries`.
- Add `format-tape`, `live-vars-render`, `system-vars-render`,
  `iteration-header`.
- Rewrite per-iteration trailer assembly: `<iteration_hints>` →
  iteration header → system-vars → live-vars → tape.
- Rewrite `CORE_SYSTEM_PROMPT` to ~35 lines covering the new contract.

### Phase 8 — Cleanup & documentation

- Delete unreachable code paths uncovered by Phases 4-7.
- Update `AGENTS.md` if any rules change (likely: drop "use `z/patch`"
  reference; clarify one-block contract).
- Update `README.md` "inspired by RLM" line to "RLM-conformant via
  constant N-1 tape window and handle-typed reads."

## Cross-cutting

### SCI monkey-patch fragility

Two patches, both on `:no-doc` impl namespaces:

1. **`sci.impl.evaluator/eval-def`** — signature stable from
   `org.babashka/sci 0.8.41` → `0.12.51` (5 cached releases). Bootstrap
   precondition asserts ns + var present + arity == 5; aborts startup
   with a helpful message if SCI internals shifted.
2. **SCI symbol-resolver hot-path fn** — narrower change; needs a
   probe at Phase 3 start to identify the exact entry point. Same
   precondition pattern.

If SCI ever ships a real `:hook` config, drop both patches and adopt
the official surface.

### Open at implementation time

- **Cancellation/timeout**: today's `cancellation/worker-future`
  wraps the block in a 120 s default timeout. With one-block-per-iter,
  timeout = whole iteration aborted. Defs that completed before the
  timeout-point are already in the sink atom; the engine flushes
  whatever was accumulated, then renders `;; ! TIMEOUT after Nms` in
  the tape. Confirm sink semantics in Phase 2 test suite.
- **Tool prune list**: the foundation `v/` surface is large. Audit
  per-symbol in Phase 6 — drop anything that exists only to render
  in the deleted journal block. Likely keepers: `v/cat`, `v/rg`,
  `v/ls`, `v/lines`, `v/at`, `v/peek`, `v/grep-in`, `v/patch`,
  `v/patch-check`, `v/source`, `v/doc`, `v/conversation-state`,
  `v/conversation-report`.
- **Handles for `v/rg` hits**: hit-count, first-hit summary in `:meta`;
  `(v/hit h idx)` returns one expanded hit. Confirm shape in Phase 1.
- **Cross-turn boundary marker**: implicit via iteration header IDs
  today. If channel UIs need an explicit marker, render
  `;; --- TURN BOUNDARY ---` between iterations whose `turn` field
  differs.

## Verification

Each phase ships with its own tests; `./verify.sh` stays green
between phases.

End-to-end acceptance after Phase 7:

```bash
clojure -M:test                                    # full suite, 0 failures
./verify.sh                                        # smoke
bin/vis run --raw "Summarize README.md"            # 1 iteration, 1 block
bin/vis run --trace "Find every TODO in src/"      # tape window N-1
```

A new test file `test/com/blockether/vis/pivot_property_test.clj`
asserts after Phase 7:

- A turn that asks "summarize README.md" finishes in **1 iteration**.
- The model's prompt for that iteration contains **no
  `<journal>` block, no `<bindings>` block, no
  `<current_user_message>`**.
- USER_REQUEST is reachable as a sandbox var inside the model's
  block.
- A `(def x 42)` (no docstring) raises `:vis/missing-docstring`; the
  next iteration's tape contains `;; ! ERROR def requires docstring`.
- Cross-turn: turn 2 iteration 1 sees the rendered tape of turn 1's
  last iteration; nothing earlier.
- A var defined in turn 1 and referenced in turn 11 (same conversation)
  has fallen off the live-vars surface but is still resolvable from
  the sandbox; resolving it brings it back into live-vars.

## Risks

- **R1 — SCI monkey-patch breaks on next SCI bump.** Mitigation:
  startup precondition aborts with a descriptive error; pinned SCI
  version in `deps.edn` until a new release is validated.
- **R2 — Mandatory docstring trips models that omit them.**
  Mitigation: explicit error in tape; first-iteration-of-turn nudge;
  CORE_SYSTEM_PROMPT example shows the form.
- **R3 — N-1 tape window confuses models trained on full transcripts.**
  Mitigation: live-vars + system-vars headers compensate for state
  visibility; runtime-resolution LRU keeps important vars warm
  automatically. autoresearch bench measures real impact before Phase
  7 ships.
- **R4 — Hard cut-over breaks open conversations.** Mitigation:
  CHANGELOG documents the schema break; ship a `vis db reset` CLI
  command in Phase 5 that clears `~/.vis` cleanly with confirmation.
- **R5 — Channel rendering regressions.** Mitigation: snapshot tests
  of rendered iteration output before / after schema collapse in
  Phase 6.
- **R6 — Multi-monkey-patch surface area.** Two patches doubles the
  fragility. Mitigation: both live in `sci_patches.clj`; one place to
  audit, one place to disable.

## Out of scope

- **`v/ask` recursion primitive** (prior PLAN Phase 4). The pivot
  achieves constant parent context structurally (N-1 tape + handle
  returns); `v/ask` is no longer load-bearing as the RLM keystone.
  May ship later as a power feature.
- **Persisting handles across turns.** Handles remain
  per-environment / per-conversation. Multi-turn handle persistence
  is a separate, larger discussion.
- **Multi-block iterations.** The pivot is one block per iteration.
  If a future need arises (parallel probes, etc.), introduce a new
  primitive — do NOT revert the loop shape.
- **Backward compatibility with legacy conversations.** Hard cut-over.
- **Forking SCI.** Monkey-patch is the chosen mechanism; reversibility
  beats purity.

## Done state

A turn that asks "summarize README.md" runs in **1 iteration**:

```clojure
;; model emits, atomically:
(def big "README handle" (v/cat "README.md"))
(turn-answer! [:ir [:p (str "README has " (:line-count @big) " lines")]])
```

The next turn's prompt shows: `<iteration_hints>` (if any), iteration
header, system-vars (`USER_REQUEST`), live-vars (`big`), prior
iteration's tape. No XML noise beyond `<iteration_hints>`. No
`<journal>`. No `<bindings>`. No `<current_user_message>`.

Targets:

- `iter_score` on autoresearch bench: **1** (down from 6).
- Per-iteration prompt size: **<30 %** of pre-pivot baseline.
- All phases shipped. `./verify.sh` green. CHANGELOG documents the
  hard cut-over.
- README's "inspired by RLM" line graduates to "RLM-conformant via
  constant N-1 tape window and handle-typed reads."

When that turn runs end-to-end with the new contract enforced,
ship the README change.
