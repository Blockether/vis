# Autoresearch ideas backlog

Promising optimizations not yet attempted, kept here so they don't get lost
between context resets.

## Lazy-load heavy persistence backend

- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj`
  takes ~482ms at manifest discovery on every JVM start.
- Heavy classes / libs imported at top: HikariCP, sqlite-jdbc, charred,
  honey.sql, next.jdbc, nippy.
- Plan:
  1. Add a tiny `…/persistance_sqlite/registrar.clj` whose only job is
     `(vis/register-extension! {:ext/persistance [{:persistance/id :sqlite
                                                   :persistance/ns 'core}]})`.
  2. Update `META-INF/vis-extension/vis.edn` to point `:nses` at the
     registrar only.
  3. Move the `(vis/register-extension! ...)` block from `core.clj` into
     `registrar.clj`. `core.clj` keeps everything else.
  4. Switch `resolve-impl` / `resolve-optional-impl` in
     `src/com/blockether/vis/internal/persistance.clj` from `ns-resolve`
     to `requiring-resolve` so the heavy `core` ns is only loaded on the
     first DB op.
- Estimated win: -482ms on commands that don't touch the DB (e.g.
  `VCLI-providers-list`, `VCLI-root-help`, `vis extensions doctor`).
- Risk: invasive (4 files); sqlite tests may need an explicit
  `(require 'core)` since the registrar no longer pulls it.

## Paired-worktree hard-task runner

Needed before judge-required tasks (CLJEXT1, PYEXT1, GAME1, …) can run.

- New `bench/opus/run-paired-task.sh`:
  - Creates `target/vis-bench/<run-id>/{pi,vis}/worktree/` via `git
    worktree add`.
  - Drops the task prompt + constraints into each.
  - Runs Pi (`anthropic/claude-opus-4-7`) and Vis
    (`anthropic-coding-plan/claude-opus-4-7`) on the same prompt, same
    model class, with disposable DB under `target/vis-bench/<run-id>/vis/db`.
  - Captures `git diff`, `verify.sh`, logs, timings, tokens, tool-call
    metrics, Vis trace/proof/intent/audit metrics.
  - Calls Opus judge with `bench/opus/judge-prompt.md` over the diffs.
  - Emits `METRIC name=value` lines that match the existing
    metric schema (pi_*/vis_*/context_*/proof_floor_pass/quality_floor_pass/
    strict_task_win/combined_task_win).
- Tear-down: `git worktree remove --force` after metrics are captured.

## Selective extension discovery for known fast commands

- `vis providers list/status/auth/...` doesn't need sqlite, foundation,
  channels, lang-clojure, mermaid, voice, exa.
- `vis channels tui --conversation-id <id>` (miss) doesn't need
  channel-telegram, exa, lang-clojure, mermaid, voice.
- Plan: tag each extension with `:ext/cli-roots` (vec of top-level
  command names). For a known fast command path, manifest discovery
  loads only the matching extensions. Fall back to full discovery for
  unknown commands or when any agent-loop / TUI runtime path is
  reached.
- Estimated win: ~200–500ms across CLI tasks.
- Risk: misclassification breaks help / unknown-command paths. Need
  thorough tests asserting we never silently miss an extension.

## S0-hi context-reduction (trivial-turn fast path)

- S0-hi sends `vis_input_tokens=18 431` for the prompt "hi". Pi sends 6.
- The bulk is `CORE_SYSTEM_PROMPT` (~2 000 tokens) + foundation
  prompt (~6–8k) + environment block + skills/intent/audit guidance.
- Plan:
  - Detect "trivial / no-tool" turns: prompt is short, no skill loaded,
    no fenced code, no tool-call required by the user.
  - Drop foundation extension prompts when the iteration loop hasn't
    needed any extension symbol yet AND the proof/intents/audit
    backbone has no live work.
  - PRESERVE proof / intents / audit anchors so any non-trivial follow-up
    re-injects the full guidance before the model can answer.
- Risk: breaks proof/intents/audit honesty if a trivial-classified
  turn turns out to be real. Needs a regression test that simulates
  follow-up tool work after a trivial-classified opening turn.

## Parallel manifest discovery

- 17 extensions, total ~1 050 ms sequential. Largest single ns
  (sqlite) is 482 ms.
- `(require ns)` is not documented as thread-safe; cyclic loading
  during simultaneous threads can deadlock.
- Defer until after lazy-sqlite split — that change alone reduces the
  hot-path critical chain.

## Tooling

- `log_experiment` validator currently rejects JSON-encoded `metrics` /
  `asi` parameters even though the schema example uses that exact shape
  (see `runs 3, 4, 5` in `autoresearch.jsonl` — manual jsonl appends).
  Investigate harness regression after this session; possibly upstream
  bug in `pi-autoresearch` parameter coercion.
