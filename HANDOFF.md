# Handoff — CTX engine merged + legacy dropped + DB cleared

Status na **`c2646cc1`** (origin/main, pushed):

```
c2646cc1  D10 docs + drop legacy vctx context system
3689b147  PROBES.md (parallel work from the other agent, untouched)
6bc6e240  Phase D9: :session/turn sync + end-to-end persist+restart integration test
87d9a26d  Phase D6/7/8: introspect verbs + form-results + validator-fn integration
ae6e5ac3  Phase D5: render path — CTX block in every iter's user message
308127fe  Phase D4: trailer auto-pin + (done) trailer ops + gc on turn end
f507c2d0  Phase D3: multi-form capture in iteration loop + drop legacy shim
d1cbd0eb  Phase D2: wire ctx engine into loop.clj + persist on (done)
2acbdfe5  Phase D1: ctx-loop adapter — SCI bindings for engine mutators
```

`./verify.sh` → ALL 7 STEPS PASS. 1456 tests green. `cljfmt` clean.

## Co siedzi w main teraz

### New engine surface

```
src/com/blockether/vis/internal/
  ctx_spec.clj       schema (8 invariants, scope cursor, no :journal)
  ctx_engine.clj     pure-fn core: parse-scope, classify-scope, build-indexes,
                     8 mutators, 7 derive-warnings passes, derive-progression,
                     derive-next-actions, apply-done, advance-iter, advance-turn,
                     gc-pass, compile/run-validator-fn, history introspect
  ctx_loop.clj       integration adapter: make-ctx-atom, make-warnings-atom,
                     synthesize-scope, cursor-snapshot, stamp-cursor,
                     build-sci-bindings (8 mutators),
                     build-introspect-bindings (5 verbs),
                     trailer->form-results, drain-warnings!
  ctx_renderer.clj   ONE printer (safe-zprint-str), section-tail annotations,
                     ;; ⚠ warnings inline, ;; progression :K N/M :state inline,
                     trailer + next-actions bounded

test/com/blockether/vis/internal/
  ctx_spec_test.clj                     property + negative
  ctx_engine_test.clj                   72 unit tests
  ctx_engine_scenario_test.clj          rate-limiter 7-turn + cascade + collision + cycle + GC
  ctx_engine_done_test.clj              done handler + validator-fn passes
  ctx_renderer_test.clj                 structural + truncation
  ctx_loop_test.clj                     SCI bindings + introspect + form-results
  ctx_persistence_integration_test.clj  REAL SQLite + Nippy round-trip + restart
```

### Loop wireup

`src/com/blockether/vis/internal/loop.clj`:
- `vctx` require + uses **GONE**
- `(env/bind-and-bump! 'ctx (vctx/build …))` **GONE**
- `(vctx/render-iteration-trailer …)` **GONE**
- New: `ctx-atom` + `ctx-warnings-atom` created at env-build, persist
  through every iter. Loaded from `session_turn_state.ctx` on resume.
- New: env-bindings merge 8 mutators + 5 introspect verbs from ctx-loop.
- New: per-iter render produces `;; ctx\n<EDN>` block with inline
  warnings + progression + next-actions, appended to user message.
- New: per-iter `advance-iter` auto-pins trailer with form envelopes,
  bumps cursor.
- New: `(done {:trailer-drop […] :trailer-summarize […]})` calls
  `apply-done` on ctx-atom before snapshot.
- New: run-turn! end → `gc-pass` → Nippy → `session_turn_state.ctx`.

### Schema

`extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`:
- `session_turn_state.ctx BLOB` — per-turn Nippy CTX snapshot
- `session_turn_iteration.code TEXT NOT NULL` (whole fence body, verbatim)
- `session_turn_iteration.forms BLOB` — Nippy vec of per-form envelopes
  `[{:scope :tag :src :result :error} …]`
- `definition_state.value BLOB` (renamed from `result`)
- **No** legacy `result` / `error` columns anywhere on iteration row
- All V1 inline edits per AGENTS.md

### Deleted

```
src/com/blockether/vis/internal/ctx.clj                   gone (was vctx)
test/com/blockether/vis/internal/ctx_test.clj             gone
```

## Filesystem state

- `~/.vis/vis.mdb` — **cleared**. Legacy data backed up to
  `~/.vis/vis.mdb.legacy.20260522-180513/` in case you want to inspect
  the old sessions.
- Next `bin/vis` invocation creates a fresh `~/.vis/vis.mdb/` with V1
  applied from scratch.
- Dev nREPL JVM (pid 65802) was KILLED to release the DB lock. Restart
  with `bin/dev` or `clojure -M:dev` if you want REPL access again.

## Docs added

`CTX_REDESIGN.md` gains two new sections:

### `## Lifecycle expectations` — what model sees + what engine enforces
- Trailer between iters (within turn)
- Trailer between turns
- Summarization (model-owned, engine-validated, lossy on purpose)
- Validation tiers T0–T7 (with hard vs soft severity)
- Tasks (descriptive not directive)
- Form-results propagation
- What model can NEVER do
- Persistence guarantees table
- Out-of-scope items

### `## Hints` — current shape + proposed validator-fn extension
- Documents the known title-hint bug Karol called out
- Proposes lifting requirement-validator pattern up to hints
- Satisfaction flow with validator
- Hint authoring contract for foundation hooks
- Implementation order (tracked as D11)

## Open work — D11+ (not done in this session)

| # | Item | Notes |
|---|---|---|
| D11 | Hint validator-fn rollout | Add `:session.hint/validator-fn` spec; extend `satisfy-hint!` to eval; migrate foundation `title-hint` + `context-pressure-hint` to ship validator. Documented in CTX_REDESIGN §Hints. |
| D12 | Real LLM end-to-end | Fresh DB exists; pick a small prompt, run `bin/vis --provider <X> "…"`, observe model interacting with new `;; ctx` block, mutators, validators. |
| D13 | Probes for new engine | PROBES.md has a methodology — run a fresh sweep (cavemanize, token budget) now that vctx is gone and `;; ctx` is the only model-facing trailer. Token budget should drop noticeably. |
| - | TODO.md items T10-T15 | Already queued in TODO.md; not engine work. |

## How to verify yourself

```bash
cd ~/vis
./verify.sh                  # all 7 steps PASS
clojure -M:test              # 1456 tests, 0 failures
bin/vis sessions list        # no legacy sessions (fresh DB)
bin/vis help                 # smoke
```

## Cleaning your shell session

You said you'd clean the session and resume — I'll have lost context
on internal atom names / local lets / specific commit refs. To pick
up:

1. `git log --oneline -15` — last 15 commits, you'll see the Phase D
   chain.
2. `cat CTX_REDESIGN.md | tail -300` — the new lifecycle + hints
   sections I added.
3. `cat HANDOFF.md` (this file).
4. `./verify.sh` — sanity.
5. Read the four new srcs: `ctx_spec.clj`, `ctx_engine.clj`,
   `ctx_loop.clj`, `ctx_renderer.clj`. They are self-contained;
   docstrings explain the surface.
6. Loop wireup is in `src/com/blockether/vis/internal/loop.clj` —
   grep for `ctx-loop` / `ctx-engine` / `ctx-renderer` / `ctx-atom`
   / `apply-done` / `advance-iter` to find the integration points.

The architecture invariant: engine is pure-fn, ctx-loop is the only
side-effect layer touching atoms + DB, ctx-renderer is the only string
producer for model-facing text. Anything new should fit one of those
three buckets.

Engine ready. DB clean. Lecimy.
