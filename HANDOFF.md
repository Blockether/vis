## Handoff — CTX engine closed + PLAN §12 execution started (steps 1–3 partial)

Status on **`ddcacddb`** (origin/main, pushed):

```
ddcacddb  PLAN §12 step 3 (partial): detect-trunk-branch + with-repo-lock
            + label/focus/hydration facade
fccf1d31  PLAN §12 step 2: persistance fns for label + focus + per-repo
            focus pointer
39facb68  PLAN §12 step 1: schema inline V1 — workspace.label +
            last_focused_at_ms + repo_focus + session_state.merge_resolve_parent_id
cb86afd6  HANDOFF.md: refresh — CTX redesign closure + missing inventory
5416daa8  Single :turn-state-atom (6→1 atom collapse)
5863ca0c  Railway-style flatten + D15 hook-task ranking + D16 :vcs/* canonical
```

### PLAN §12 progress

| step | status | notes |
|---|---|---|
| 1 | ✅ DONE (39facb68) | schema inline V1: workspace.label + last_focused_at_ms + repo_focus table + session_state.merge_resolve_parent_id; row->workspace surfaces new cols |
| 2 | ✅ DONE (fccf1d31) | persistance fns: db-workspace-update-label! / db-workspace-touch-focus! / db-repo-focus-get / db-repo-focus-set!; defdelegates |
| 3 | ✅ DONE | (ddcacddb) detect-trunk-branch + with-repo-lock + set-label! / focus! / last-focused / display-label / workspace-with-session / list-active-with-sessions + db-session-state-list-for-workspace; **REST**: mirror-tree! + spawn-branch! rewrite (PLAN §4.4 dirty+ignored copy under with-repo-lock) + commit! + ff-apply! + start-merge-resolve! skeleton (PLAN §7.1 placeholder — throws :workspace/not-yet-implemented for step 10) + KILL apply-to-trunk! body (K4) + KILL workspace-apply-to-trunk! re-export (K5) + db-workspace-update-commit-id! persistence fn; TUI `:apply-workspace-to-trunk` palette entry temporarily routes through `workspace-ff-apply!` (full deletion lands in step 8/K6) |
| 4 | ✅ DONE (2933936e) | KILL `:git/*` aliases (K1, K2); rename :git/file-stats → :vcs/file-stats; workspace/status emits ONLY :vcs/*; tests migrated; one comment-only mention of `:git/branch` left in ctx_spec.clj (historical rationale) |
| 5 | ⏳ PENDING | DECLARATIVE slash surface (PLAN §3 rewrite 2026-05-22): add `::slash` + `:ext/slash-commands` specs to internal/extension.clj alongside `:ext/hooks`; internal/slash.clj `active-slashes` aggregator + parser + dispatch (NO atom, NO register-slash!); core.clj exports `active-slashes` / `slash-by-path` / `slash-children`. Engine refuses load on duplicate `[parent name]` across `(active-extensions)`. |
| 6 | ⏳ PENDING | workspace surface lives in `vis-foundation-core` (NOT a new package — PLAN §6 updated 2026-05-22: workspace is CORE, no architectural payoff splitting into a separate ext). Add `workspace_slashes.clj` + `workspace_ctx.clj` to vis-foundation-core. Voice migration from `:tui.slot/commands` to `register-slash!` (K10). |
| 7 | ⏳ PENDING | engine loop integration: slash dispatch at turn start; synthetic iter persistence; CTX :session/workspace pre-turn stamping |
| 8 | ⏳ PENDING | TUI channel rewrite + KILL slash legacy (K6 / K7 / K8) |
| 9 | ⏳ PENDING | Telegram channel rewrite + KILL parse-command / handle-command! (K9); setMyCommands from registered-slashes |
| 10 | ⏳ PENDING | merge-resolve sub-session real impl |
| 11 | ⏳ PENDING | docs sync (K11, K12) |
| 12 | ⏳ PENDING | cleanup pass + final smoke (PLAN §0b 9-point ripgrep audit) |

### What works today vs. what doesn't (operator-level)

**WORKS** (CTX engine baseline, hook-task surface):
  - `bin/vis --provider <P> --persist "…"` end-to-end with a real
    provider. Model reads `;; ctx`, writes specs/tasks/facts/proofs,
    satisfies foundation hook-tasks via
    `(task-set! :vis.foundation/session-title {:status :done :proof
    "tN/iM/fK"})`. Engine validates the proof envelope; reverts on
    fail. Verified `2026-05-23` against `anthropic-coding-plan`.
  - All engine mutators (`spec-set!` / `task-set!` / `fact-set!` /
    `req-add!` / `req-update!` / `req-remove!` / `proof-add!` /
    `proof-remove!` / `set-session-title!` / `done`) wired through
    `ctx-loop/build-sci-bindings`.
  - `(done {:trailer-drop […] :trailer-summarize […]})` for
    model-owned trailer cleanup. No auto-compaction anywhere.
  - `introspect-*` verbs (spec/task/fact/archived/ctx-at) over the
    persisted history with per-iter DB-roundtrip cache.

**DOESN'T WORK YET** (PLAN §12 steps 5–12):
  - `/workspace …` slash commands in TUI / Telegram channels. No
    `:ext/slash-commands` spec, no `slash.clj` aggregator, no
    channel dispatch hook yet. PLAN §12 steps 5 (slash declarative
    surface) + 6 (workspace slashes on vis-foundation-core) +
    7 (engine loop integration) deliver this.
  - Synthetic iteration persistence for slash results. The model
    sees normal iters today; once steps 5+7 land, slashes will
    persist as synthetic iters in the same `session_turn_iteration`
    table (without `:llm-*` keys) so channels render them
    consistently with model iters.
  - TUI tab strip + Telegram switcher backed by
    `list-active-with-sessions` + per-repo `focus!`. The thin facade
    is in place; the channel rewrite (steps 8 + 9) wires the consumers.
  - Merge-resolve sub-session real impl (step 10).

### CTX engine baseline (D11–D13 + D15 + D16) — unchanged

```
5416daa8  Single :turn-state-atom collapses 6 per-turn atoms into one map
5863ca0c  Railway-style flatten + D15 hook-task ranking + D16 :vcs/* canonical
4546d682  PLAN.md: cross-channel slash discipline (HARD CONTRACT)
            + rename hot-symbol compaction to archival
3b83554f  ctx-engine: reconcile FSM safety — :validated? flag
            (engine side of 4546d682)
2713ecf9  D12 follow-up: fix multi-form scope tracking +
            nested-when cleanup + tel/log
ec2d2987  D12 follow-up: stale satisfy-hint! traces +
            HANDOFF + CTX_REDESIGN refresh
ae9d4212  Phase D12: collapse :session/hints into hook-sourced tasks
bc6008c3  Phase D11: hint validator-fn + single ctx-atom +
            VCS-agnostic workspace
```

`./verify.sh` → **all 7 steps PASS**, 1496 tests green, cljfmt clean.
Live CLI scenario (`bin/vis --provider anthropic-coding-plan
--persist` + multi-iter prompt + `(done {:trailer-drop […]})`) passes
end-to-end with the title hook-task persisted at `:status :done
:validated? true :proof :done-born` and zero reconcile warnings.

---

## What CTX redesign closed (since `c2646cc1` baseline)

### D11 — hint validator-fn + single ctx-atom + VCS-agnostic workspace
- Hints carried REQUIRED `:validator-fn`; engine validated proof scopes
  end-of-iter via `apply-satisfies`.
- Atom consolidation: warnings + pending-satisfies moved from sibling
  atoms onto `:engine/*` ephemeral keys on the single ctx-atom.
- Workspace spec became VCS-agnostic (`:vcs/kind` discriminator +
  optional `:vcs/*` / `:git/*` keys; `{:vcs/kind :none}` for non-VCS).

### D12 — `:session/hints` collapsed into hook-sourced tasks
- `:session/hints`, `satisfy-hint!`, `apply-satisfies`,
  `:engine/pending-satisfies` all retired.
- Foundation hooks emit task shape `{:title :importance :validator-fn}`;
  the loop folds them into `:session/tasks` with `:source :hook`.
- Model satisfies via standard `(task-set! id {:status :done :proof
  "tN/iM/fK"})`.
- `reconcile-done-hook-tasks` runs at end-of-iter (after
  `advance-iter` pins the trailer) and validates every
  `:source :hook + :status :done + :validator-fn` task against the
  form envelope at `:proof`. Failure → revert to `:todo`, drop
  `:done-born` and `:proof`, emit `:task-done-*` warning.
- `apply-mutator :task-set!` gained hook-repeat dedup so foundation
  hooks can re-fire idempotently without collision warnings.

### D13 — real-model CLI smoke + bug-bash

Three CRITICAL bugs surfaced only under real LLM iters:

1. **Multi-form fence scope tracking**
   `run-sci-code`'s `eval-per-form` processed N top-level forms per
   fence but NEVER advanced `:current-form-idx-atom`. The outer
   `executed-mapv` set the atom once per fence (typically 0) and it
   stayed there. Consequences: `(done)` captured `:position 0`,
   cursor `:next-form 1` at iter-end, reconcile saw every past form
   as `:future-form`, every satisfied hook-task got reverted.
   FIX: `eval-per-form` threads form-idx-atom through `:env`,
   `swap!`s `:form-idx` to the current `idx` before each top-level
   form eval.

2. **Multi-form trailer pin collapse**
   The eval pipeline returns ONE outer block per iter; its `:forms`
   sub-vec carries per-top-level-form envelopes. `blocks->forms` saw
   one block, projected one envelope at `f1`, dropped every form
   after the first from both the trailer pin AND the form-results
   map proof scopes resolve against.
   FIX: expand outer blocks into per-form mini-blocks before
   projection.

3. **Off-by-one iter index at trailer pin**
   Cursor at `advance-iter` used raw `iteration` (0-based loop
   counter). Renderer + `cursor-snapshot` used
   `current-iteration-atom` (1-based, set to `(inc iteration)`).
   Model wrote proof scope `t1/i1/f6` against rendered ctx, trailer
   pinned `t1/i0/f6`. Reconcile classified `t1/i1/f6` as `:future`
   → reverted satisfaction.
   FIX: cursor reads turn-state's `:iteration` first (same source
   the renderer uses); raw `iteration` fallback only for tests.

Plus FSM safety:

4. **Hook-task retro-revert when proof envelope vanishes**
   Reconcile re-validated on every iter. A model writing
   `(done {:trailer-drop […]})` nukes the proof envelope from
   form-results; next reconcile pass classifies the proof as
   `:unknown` → revert. Effect: satisfied tasks lost their `:done`
   status because of an unrelated trailer cleanup.
   FIX: `:session.task/validated?` boolean flag, stamped on
   successful reconcile. Subsequent passes skip already-validated
   `:done` hook-tasks. `apply-task-set!` clears the flag on any
   non-`:done` transition so re-entry to `:done` forces a fresh
   validation.

### D15 — renderer ranking of hook-tasks
- `rank-tasks`: `array-map` of `:session/tasks` ordered by
  `[hook? IMPORTANCE STATUS task-id]` so hook-tasks
  (`:critical`/`:warn`/`:info`) render BEFORE user tasks; within a
  group `:todo` < `:doing` < `:blocked` < `:done` < `:cancelled`.
- `hook-task-annotation-lines`: one `;; via hook <importance>
  <id> status=<…>` line per UNRESOLVED hook-task in the
  `:session/tasks` section-tail. Resolved
  (`:status :done :validated? true`) hooks stay silent.

### D16 — workspace detector stamps `:vcs/*` canonical
- `workspace/status` now stamps both
  `:vcs/kind :git + :vcs/branch + :vcs/head + :vcs/dirty?` AND the
  legacy `:git/*` aliases. Spec admits both. Hg / Jujutsu detectors
  fork this fn (or dispatch on `:vcs/kind`); engine reads
  VCS-agnostic.

### Additional cleanups
- **Single `:turn-state-atom`** replaces the 6-atom soup
  (`:current-turn-position-atom`, `:current-iteration-atom`,
  `:current-form-idx-atom`, `:current-iteration-id-atom`,
  `:current-session-turn-id-atom`, `:current-user-request-atom`).
  Reads via `ctx-loop/read-turn-state`; writes via
  `set-turn-state!` / `swap-turn-state!`. Extension scope-coord
  consumers (`extension_aggregate.clj`,
  `foundation_core/introspection.clj`) migrated.
- **Railway-style flatten**: `render-block!`, `apply-done!`,
  `reconcile-done-hook-tasks!` extracted to `ctx-loop` as
  single-call helpers. Loop sites that previously contained
  4-level nested `(when-let [a (:X-atom env)] (let […] (swap! a
  […])))` chains became one call.
- **`HOT_SYMBOL_COMPACTION_TARGET` → `HOT_SYMBOL_ARCHIVE_TARGET`**:
  misleading name retired. CTX has NO compaction — only
  model-explicit `:trailer-drop` and `:trailer-summarize`. The
  hot-symbol code is SCI sandbox eviction, unrelated.
- **`indent-rest`** rewritten from `(split-lines → count → first →
  rest → map → join)` recur soup to a single `str/replace #"\n"`
  with inserted padding.
- **`introspect-history-loader` per-iter cache** keyed on
  `(turn-position, iteration)` so multiple `introspect-*` calls
  inside one iter hit SQLite once.
- **`tel/log!` coverage**: every iter-end phase logs at `:info`
  with structured data:
    `::hook-task-fold` — emitted ids + fold duration
    `::iter-end-pre-reconcile` — cursor, pinned forms, trailer
      entry count, form-result scopes, pre-reconcile hook-task
      snapshot, advance-iter ms
    `::iter-end-post-reconcile` — post snapshot, changed?, warnings,
      reconcile ms
    `::reconcile-done-hook-tasks` — cursor, scopes, warnings, ms
    `::apply-done` — trailer-drop, trailer-summarize, warnings, ms
  Replay a run by tailing `~/.vis/vis.log`. `tel/with-ctx+` injects
  `:session-soul-id` into every signal in turn scope.

---

## What was missing (root-cause inventory)

These are the gaps that surfaced as bugs only when running real
sessions through the engine. Inventoried here so the next CTX
revision can lint against the same classes of mistake.

1. **Form-idx atom stale across multi-form fences** — eval-per-form
   never updated it. Latent since D3 ("multi-form capture"); silent
   until D11/D12 introduced cursor-sensitive reconcile.
2. **0-based vs 1-based iter mixup at trailer pin** — cursor read
   raw loop counter while renderer read the atom. Silent until
   reconcile classified scopes against cursor.
3. **Reconcile re-validates idempotently every iter** — turned a
   model-owned `:trailer-drop` into accidental task revert. Missing
   `:validated?` invariant.
4. **Atom soup on env** — 6 sibling atoms tracking one logical
   concept (per-turn coords + ids). Every consumer wrote 2-3
   `(some-> (:current-X-atom env) deref)` ladders. Collapsed to one
   `:turn-state-atom` map.
5. **Naming drift** — `HOT_SYMBOL_COMPACTION_TARGET` suggested CTX
   summarization had a compaction sibling. It didn't; the symbol
   was unrelated to CTX entirely.
6. **Stringly-typed indent-rest** — split + recur + join when a
   regex replace did the job in one line.
7. **VCS-agnostic spec ≠ VCS-agnostic emit** — D11 admitted
   `:vcs/*` in the workspace spec but `workspace/status` kept
   emitting only `:git/*` aliases. D16 closed the gap.
8. **No telemetry on the iter-end pipeline** — `printf` debugging
   was the only way to diagnose 1-3. D12 follow-up added
   `tel/log! :info` at every phase boundary.
9. **`render-block!` 17-line nested let lived inside the loop**
   instead of behind one fn call. Same for `apply-done!`. Hard
   to test in isolation, hard to read.
10. **`introspect-history-loader` per-iter DB roundtrips** — the
    model can call 4-5 `introspect-*` verbs inside one iter; each
    used to hit SQLite. Now keyed cache.

---

## Source-of-truth pointers (post-cleanup)

| Concern | File |
|---|---|
| Spec | `src/com/blockether/vis/internal/ctx_spec.clj` |
| Pure engine | `src/com/blockether/vis/internal/ctx_engine.clj` |
| Loop adapter (single ctx-atom + single turn-state-atom + SCI bindings) | `src/com/blockether/vis/internal/ctx_loop.clj` |
| Renderer | `src/com/blockether/vis/internal/ctx_renderer.clj` |
| Wire loop | `src/com/blockether/vis/internal/loop.clj` |
| System prompt | `src/com/blockether/vis/internal/prompt.clj` |
| Foundation hook-tasks | `extensions/common/vis-foundation-core/src/com/blockether/vis/ext/foundation_core/hints.clj` |
| Bridge hook-task | `extensions/common/vis-foundation-bridge/src/com/blockether/vis/ext/foundation_bridge/core.clj` |
| Design doc | `CTX_REDESIGN.md` (top banner + closing "Hooks → tasks (D12)" section are canonical) |

Invariant: engine pure-fn, ctx-loop the ONLY side-effect layer
touching the single ctx-atom + single turn-state-atom + DB,
ctx-renderer the ONLY string producer.

---

## What's NOT done yet

| # | Item | Notes |
|---|---|---|
| D14 | Probes sweep for the new engine | PROBES.md methodology. Re-run cavemanize + token budget probes now that `:session/hints` is gone, hook-tasks are part of `:session/tasks`, renderer ranks them. Token budget should drop another notch. |
| PLAN | Workspace + cross-channel slash redesign | §0b in PLAN.md is HARD CONTRACT; the rest of PLAN.md spells the schema, registry, command tree, sub-session machinery, TUI/Telegram surface changes, 12-step implementation sequence. ENGINE side of the work is the CTX redesign that's now done; the SLASH side is the next major. |
| - | TODO.md items T10–T15 | Already queued in TODO.md; not engine work. |

---

## How to run vis (operator cheatsheet)

```bash
# One-shot agent run (CLI, prompt argv, persisted session)
bin/vis --provider anthropic-coding-plan --persist "refactor auth flow"
bin/vis --provider zai-coding-plan --persist "summarise this repo"

# In-memory smoke (no DB pollution; useful for engine probes)
bin/vis --db :memory --full-trace-edn-stream "echo task done"

# Interactive TUI
bin/vis channels tui                              # launches TUI in current shell
bin/vis channels tui --session-id <prefix>        # resume by id (any unambiguous prefix)

# Inspect persisted sessions
bin/vis sessions list                             # latest sessions table
bin/vis sessions show <prefix>                    # session metadata + state versions
bin/vis sessions export <prefix> --md             # markdown transcript on stdout
bin/vis sessions export <prefix> --html out.html  # standalone html

# Providers / auth
bin/vis providers list                            # all configured + auth status
bin/vis providers auth <provider-id>              # interactive auth (OAuth, API key)
bin/vis providers status                          # live model + quota probe
bin/vis models list                               # frontier model cheatsheet

# Cross-extension doctor + ext-contributed commands
bin/vis doctor                                    # cross-ext diagnostics
bin/vis ext list                                  # extensions installed
bin/vis ext run <ext-cmd>                         # run an extension subcommand

# Help
bin/vis --help                                    # top-level
bin/vis <command> --help                          # subcommand help

# DEV: start a controllable JVM with nREPL
bin/dev                                           # equivalent to clojure -M:dev
clj-nrepl-eval --discover-ports                   # find the port the JVM listens on
clj-nrepl-eval -p <port> "(require '[com.blockether.vis.dev :as dev] :reload)"
clj-nrepl-eval -p <port> "(dev/cli! \"providers\" \"list\")"
clj-nrepl-eval -p <port> "(dev/tui!)"             # opens TUI in a fresh Terminal.app

# Provider currently used for live CTX smoke tests in this thread:
#   anthropic-coding-plan (Claude Sonnet/Opus, subscription) — authed via
#   ~/.vis/anthropic-auth.json. zai-coding-plan also authed when you want
#   to swap providers for token-budget probes.
```

DB lives at `~/.vis/vis.mdb/vis.db` (SQLite + Flyway V1, multiprocess
WAL). Schema autoselects + applies migrations on first `bin/vis` per
fresh DB. `rm -rf ~/.vis/vis.mdb` between scenarios for clean state;
do NOT delete it while a Vis JVM holds the lock.

Logs live at `~/.vis/vis.log`. Telemere ingest; the iter-end pipeline
logs `::iter-end-pre-reconcile`, `::iter-end-post-reconcile`,
`::reconcile-done-hook-tasks`, `::hook-task-fold`, `::apply-done` at
`:info`. Replay any run by tailing.

```bash
tail -F ~/.vis/vis.log | rg "::iter-end|::reconcile-done|::hook-task-fold|::apply-done"
```

---

## How to verify yourself

```bash
cd ~/vis
./verify.sh                  # all 7 steps PASS
clojure -M:test              # 1496 tests, 0 failures
bin/vis sessions list        # empty (fresh DB)
bin/vis help                 # smoke

# Live ctx engine smoke (anthropic-coding-plan or whichever provider is authed)
rm -rf ~/.vis/vis.mdb
bin/vis --provider <P> --persist "Set title 'X'. Satisfy hook: (task-set! :vis.foundation/session-title {:status :done :proof \"t1/i1/f1\"}). (done {:answer \"ok\"})."
grep -E "::iter-end-(pre|post)|::reconcile-done|::hook-task-fold|::apply-done" ~/.vis/vis.log
```

---

## Picking up cleanly

**Recommended ritual when restarting a session:**

```bash
cd ~/vis
git pull --ff-only origin main           # sanity
cat HANDOFF.md                            # this file — start here
cat PLAN.md | head -250                   # §0b HARD CONTRACT + §0 vocab + §1 schema
grep -n "^## " PLAN.md                    # table of contents
./verify.sh                               # all 7 steps must pass before touching anything
bin/vis sessions list                     # see persisted state if any
```

Then pick the next PLAN §12 step from the progress table at the top
of this file and follow it. Commit each step separately; re-run
`./verify.sh` between steps.

---

## Picking up cleanly (engine deep-dive)

1. `git log --oneline -10` — last cycle's commits.
2. `cat HANDOFF.md` (this file).
3. `cat PLAN.md | head -140` — §0b cross-channel discipline (HARD
   CONTRACT) + §0a slash live-state verification. Read these BEFORE
   touching any slash code.
4. `cat CTX_REDESIGN.md | tail -140` — D12 closed-surface section.
5. `./verify.sh` — sanity.
6. Read engine sources in order:
   `ctx_spec.clj` → `ctx_engine.clj` → `ctx_loop.clj` →
   `ctx_renderer.clj`. Each is self-contained; docstrings explain
   the surface.
7. Loop integration sites in `loop.clj`: grep for
   `ctx-loop/` / `ctx-engine/` / `ctx-renderer/` / `ctx-atom` /
   `turn-state-atom` / `apply-and-record!` /
   `reconcile-done-hook-tasks!` / `apply-done!` / `render-block!` /
   `advance-iter`. Each integration point carries a comment
   explaining why.

CTX engine: closed. Hints: collapsed. Atoms: consolidated. State:
coherent. Logs: tel/log everywhere. Validators: FSM-guarded.
VCS: agnostic. Multi-form: tracked. Off-by-one: fixed.

Next session picks up PLAN §12 step 3 REST:
  1. spawn-branch! rewrite per PLAN §4.4 (dirty + ignored file copy,
     with-repo-lock wrap, repo-id sanitize, branch auto-mint).
  2. commit! (git add -A + commit, refuses trunk-kind + empty diff,
     bumps commit_id).
  3. ff-apply! (git merge --ff-only, auto-stash dance, transition
     to :merged; returns :ff-failed on conflict for §7 hand-off).
  4. start-merge-resolve! skeleton (spawn sub-session, set
     merge_resolve_parent_id, register temp merge/* op surface).
  5. KILL apply-to-trunk! body + workspace-apply-to-trunk! re-export
     (K4, K5).

Then step 4 (ctx_spec.clj KILL :git/* aliases) which is small. Then
step 5 (slash registry) which is the gateway to everything else.

PROBES sweep (D14) deferred until after the slash redesign so the
cavemanize + token budget runs see the final shape.
