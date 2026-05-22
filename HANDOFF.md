## Handoff — CTX engine D11 + D12 landed; redesign closed

Status on **`ae9d4212`** (origin/main, pushed):

```
ae9d4212  Phase D12: collapse :session/hints into hook-sourced tasks
bc6008c3  Phase D11: hint validator-fn + single ctx-atom + VCS-agnostic workspace
7525fdba  HANDOFF.md: session handoff summary after vctx removal + DB clear
c2646cc1  D10 docs + drop legacy vctx context system
3689b147  PROBES.md (parallel work from the other agent, untouched)
6bc6e240  Phase D9: :session/turn sync + end-to-end persist+restart integration test
87d9a26d  Phase D6/7/8: introspect verbs + form-results + validator-fn integration
ae6e5ac3  Phase D5: render path — CTX block in every iter's user message
308127fe  Phase D4: trailer auto-pin + (done) trailer ops + gc on turn end
f507c2d0  Phase D3: multi-form capture in iteration loop + drop legacy shim
```

`./verify.sh` → **all 7 steps PASS**, 1454 tests green, cljfmt clean, no flakes.
`~/.vis/vis.mdb` is **not present** — next `bin/vis` invocation creates
a fresh V1 DB. Dev nREPL JVM is **not running** (the listener on :49943
belongs to a different project at `/Users/fierycod/svar`).

---

## What D11 + D12 closed (since the previous handoff)

### D11 — hint validator-fn + single atom + VCS-agnostic workspace (`bc6008c3`)

- Spec: hints carried a REQUIRED `:validator-fn`. Engine ran it via
  `apply-satisfies` at end-of-iter against a `:engine/pending-satisfies`
  queue.
- Atom consolidation: dropped `ctx-warnings-atom` +
  `pending-satisfies-atom`; their state moved to `:engine/warnings` +
  `:engine/pending-satisfies` ephemeral keys on the single ctx-atom.
  Stripped via `eng/strip-ephemeral` before Nippy persistence.
- Workspace: `::workspace` became VCS-agnostic. `:vcs/kind` enum
  (`#{:git :hg :jj :fossil :none}`) + canonical `:vcs/*` keys +
  legacy `:git/*` aliases all opt; `{:vcs/kind :none}` for non-VCS
  sessions; empty `{}` valid.
- Engine: proof scope sanity distinguished `:malformed` / `:future-*` /
  `:errored` / `:unknown` separately.
- 7 new engine tests + drain tests + foundation hook tests.

### D12 — collapse `:session/hints` into hook-sourced tasks (`ae9d4212`)

- **`:session/hints` retired wholesale.** Hook-emitted soft work items
  now live as tasks under `:session/tasks` with `:source :hook`,
  `:hook-id`, `:importance`, `:validator-fn`, `:proof`. Model satisfies
  via the standard `(task-set! :id {:status :done :proof "tN/iM/fK"})`.
- **`apply-satisfies` deleted.** Replaced by
  `reconcile-done-hook-tasks` — pure end-of-iter pass that validates
  every `:source :hook + :status :done + :validator-fn` task against
  `form-results[:proof]`. Failure reverts to `:todo`, drops
  `:done-born` and `:proof`, emits a `:task-done-*` warning.
- **`satisfy-hint!` SCI binding gone.** No legacy alias; the model uses
  `task-set!` exclusively.
- **Hook re-emission dedup:** `apply-task-set!` treats
  `{:source :hook}` writes against an existing hook-task (matching
  `:hook-id`) as a silent no-op regardless of status. Resurrection of
  archived hook-tasks happens via `gc-pass` TTL.
- **`:session.task/proof` keyword renamed → `:session.task/proof-entry`**
  to free the unqualified `:proof` key for the hook-task done scope
  field. `:session.task/specs` map shape unchanged.
- **Renderer:** dropped `:session/hints` section.
- **Prompt:** dropped `(satisfy-hint! …)` CONTROL line; task shape doc
  now documents the optional hook fields and the satisfaction path.
- **Foundation hooks** (`title-hint`, `context-pressure-hint`,
  `bridge-hint`) return hook-task shape `{:title :validator-fn
  :importance}`. Body text directs the model to `task-set! :status
  :done :proof`.
- **introspect-history-loader cached per `(turn, iter)`** — multiple
  `introspect-*` calls inside one iter hit SQLite once.
- 10 new engine reconcile tests + ctx-loop reconcile wrapper tests +
  spec hook-task tests + migrated foundation tests.

---

## Source-of-truth pointers (post-D12)

| Concern | File |
|---|---|
| Spec (data shape) | `src/com/blockether/vis/internal/ctx_spec.clj` |
| Pure engine | `src/com/blockether/vis/internal/ctx_engine.clj` |
| Loop adapter (one atom + SCI bindings) | `src/com/blockether/vis/internal/ctx_loop.clj` |
| Renderer (one EDN string producer) | `src/com/blockether/vis/internal/ctx_renderer.clj` |
| Wire loop | `src/com/blockether/vis/internal/loop.clj` |
| System prompt | `src/com/blockether/vis/internal/prompt.clj` |
| Foundation hook tasks | `extensions/common/vis-foundation-core/src/com/blockether/vis/ext/foundation_core/hints.clj` |
| Bridge hook task | `extensions/common/vis-foundation-bridge/src/com/blockether/vis/ext/foundation_bridge/core.clj` |
| Design doc | `CTX_REDESIGN.md` (banner at the top + the D12 closed-surface section at the end are canonical) |

The architecture invariant remains: **engine is pure-fn, ctx-loop is the
only side-effect layer touching the single ctx-atom + DB, ctx-renderer is
the only string producer for model-facing text.** Anything new should fit
one of those three buckets.

---

## What's NOT done yet (open work — D13+)

| # | Item | Notes |
|---|---|---|
| D13 | Real-model end-to-end smoke | Fresh DB exists; pick a small prompt, run `bin/vis --provider <X> "…"`, observe the model reading `;; ctx`, writing hook-task `(task-set! :status :done :proof "…")`, and reconcile-done-hook-tasks flipping back on bad proof. |
| D14 | Probes sweep for the new engine | PROBES.md has the methodology — re-run cavemanize + token budget probes now that `:session/hints` is gone and hook-tasks are part of `:session/tasks`. Token budget should drop another notch. |
| D15 | Renderer ranking of hook-tasks | Tasks section renders all tasks flat today. Hook-tasks should sort visually by `:importance` desc, then `:source :hook` cue (`;; via hook` annotation), then user tasks. Not blocking; cosmetic. |
| D16 | Workspace detector → `:vcs/*` canonical | `src/com/blockether/vis/internal/workspace.clj` still stamps `:git/*` keys. Permissive spec accepts it, but canonical shape is `:vcs/*`. Migrate detector or add a translation pass when the workspace lands on `:session/workspace`. |
| - | TODO.md items T10-T15 | Already queued in TODO.md; not engine work. |

---

## How to verify yourself

```bash
cd ~/vis
./verify.sh                  # all 7 steps PASS
clojure -M:test              # 1454 tests, 0 failures
bin/vis sessions list        # empty (fresh DB)
bin/vis help                 # smoke
```

---

## Picking up cleanly

If you've lost local context:

1. `git log --oneline -10` — last 10 commits, you'll see D11 + D12.
2. `cat CTX_REDESIGN.md | tail -180` — the new D12 "Hooks → tasks" closed-surface section.
3. `cat HANDOFF.md` (this file).
4. `./verify.sh` — sanity.
5. Read the four engine sources in this order:
   `ctx_spec.clj` → `ctx_engine.clj` → `ctx_loop.clj` → `ctx_renderer.clj`.
   Each is self-contained; docstrings explain the surface.
6. `loop.clj` integration spots — grep for `ctx-loop` / `ctx-engine` /
   `ctx-renderer` / `ctx-atom` / `apply-and-record!` /
   `reconcile-done-hook-tasks!` / `advance-iter`. Five integration
   points total; each carries a comment explaining why.

Engine ready. DB clean. Hints gone. Lecimy.
