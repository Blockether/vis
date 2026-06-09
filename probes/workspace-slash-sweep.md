# Workspace + slash redesign — D14 probe sweep

Companion to `PROBES.md`. Re-runs the cavemanize / token-budget /
slash-discovery probes against the post-redesign engine. Settle the
following before promoting findings to TODO.md.

Run with:

```bash
./probes/run.sh <provider-id>             # all scenarios
./probes/run.sh <provider-id> 03 07       # just scenarios 03 + 07
```

Outputs land in `target/probe/logs/<scenario-id>.log` (full trace).

## Scenarios

### 01 — Slash discovery: TUI palette
**Workload.** Boot `bin/vis channels tui`. Press `Ctrl+K`. Confirm the
palette shows `/workspace new`, `/workspace list`, `/workspace switch`,
`/voice`, alongside the original `:new-session` etc.

**Pass criteria.**
- No legacy entries: `:workspace`, `:apply-workspace-to-trunk`,
  `:discard-workspace-soft`, `:discard-workspace-hard` GONE.
- Slash entries dispatch via `vis/slash-dispatch` (not `run-command!`).

### 02 — Slash discovery: Telegram bot menu
**Workload.** Boot Telegram bot. Inspect `setMyCommands` payload
(log message `Telegram bot command menu installed`).

**Pass criteria.**
- Menu contains exactly the Telegram-private commands declared on
  `vis-channel-telegram`'s `:ext/slash-commands`, plus any other
  cross-channel slashes available to `:channel/id :telegram`.
- `/start` NOT in menu (hidden alias).
- `/voice` excluded when no voice ext loaded.

### 03 — Engine slash short-circuit
**Workload.** Real LLM session. User sends `/workspace list`.

**Pass criteria.**
- Iteration count = 1, status `:success`, prior-outcome `:complete`.
- Persisted iter row has `forms[0].tag = :user-slash`.
- Provider was NOT called (no `:llm-*` keys on the row).

### 04 — Slash with :slash/specs emit
**Workload.** Register a temporary extension whose slash run-fn
returns `{:slash/status :ok :slash/specs {:probe/spec {:title "probe"}}}`.
User sends `/probe`.

**Pass criteria.**
- `session_turn_state.ctx` (next turn) contains
  `[:session/specs :probe/spec]` with `:born` stamped at the slash
  turn's scope.
- Engine warnings empty.

### 05 — Hook :emit payload
**Workload.** Register an extension hook returning
`{:emit {:facts {:probe/loaded {:value true}}}}` from
`:turn.iteration/start`.

**Pass criteria.**
- After ONE iter, the live ctx (renderer output) contains
  `:probe/loaded` under `:session/facts` with engine-stamped `:born`.
- No `iteration-start-hook-missing-title` warning logged
  (pure-emit hook is legal).

### 06 — Symbol :emit envelope
**Workload.** Tool `v/probe-emit` returns
`(extension/success {:result :ok :emit {:tasks {:t/probe {:title "p" :status :todo}}}})`.
User runs `(v/probe-emit)`.

**Pass criteria.**
- Tool returns `:ok` value to the GraalPy sandbox.
- Next render shows `t/probe` task at `:todo`.
- Engine `:born` scope = the form scope of the GraalPy call (e.g.
  `t1/i1/f1`).

### 07 — Cavemanize prompt budget
**Workload.** New session. Send 1 trivial prompt. Capture total input
tokens of iter 1.

**Pass criteria.**
- Tokens drop ≥10% vs pre-redesign baseline. Pre-redesign baseline
  pulled from the commit before `02b386c4` (step 5). Slash + workspace
  changes should NOT have re-inflated the prompt; `:session/workspace`
  block now carries `:vcs/*` keys only (no `:git/*` duplicates).
- `register-slash!` / `slash-registry` mentions absent from system
  prompt (NO atom, NO imperative call).

### 08 — Merge-resolve happy path
**Workload.** Create a divergent commit on trunk + branch on the
same file. `vis/workspace-ff-apply!` from REPL — should return
`:ff-failed`. Then `vis/workspace-start-merge-resolve!`.

**Pass criteria.**
- `start-merge-resolve!` returns `{:status :ok :sub-session-state-id
  <uuid> :conflicts [{:path "<file>" :state "UU"}]}`.
- `session_state.merge_resolve_parent_id` on the sub-row points at
  the parent state.
- `partial UNIQUE` index admits both parent + sub-session pinned
  to the same workspace.
- `:session/merge-resolve-started` event published.

### 09 — mr/* GraalPy ops drive resolution to completion
**Workload.** Continuing from 08: model issues `(mr/accept-ours
"<file>")` then `(mr/continue! {:message "done"})`.

**Pass criteria.**
- Each op returns canonical envelope (`:slash/status :ok` shape via
  the GraalPy symbol render path).
- JGit `CheckoutCommand` / `AddCommand` / `CommitCommand` paths
  used; no shell-out `git checkout/--ours` in the log.
- HEAD advances to the new commit.
- `:session/merge-resolve-finished :result :continued` event published.

### 10 — mr/abort! restores HEAD via JGit reset
**Workload.** Start merge-resolve, then `(mr/abort!)`.

**Pass criteria.**
- HEAD == ORIG_HEAD (the pre-merge sha).
- MERGE_HEAD file gone (`:in-progress? false` on `(mr/status)`).
- `ResetCommand(mode=HARD ref=ORIG_HEAD)` path taken
  (`ResetType/MERGE` would have thrown UnsupportedOperationException
  in JGit 7.x outside specific preconditions; HARD reset to ORIG_HEAD
  is the documented JGit recipe for `git merge --abort`).

---

## Reporting template

For each scenario:

```
## NN — <title>
Status: PASS | FAIL | SKIPPED
Notes (3-5 lines):
  - What landed in the trace?
  - Token / iter count vs baseline?
  - One concrete improvement / parking item?
```

Promote any actionable improvements to `TODO.md` with a T-tag.
Parking items (long-context limitations, provider quirks) go to
PROBES.md §10.
