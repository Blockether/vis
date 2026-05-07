# Autoresearch ideas backlog

Promising optimizations not yet attempted, kept here so they don't get
lost between context resets.

## Wire the paired-worktree runner (NEXT)

`bench/opus/run-paired-task.sh` is a SCAFFOLD: lifecycle (worktree
create / dispose, disposable Vis DB, tee'd logs, metrics envelope) is
in place and emits all required `METRIC` lines as zeros so the
autoresearch metric-schema gate accepts the run. Three things still
stubbed and must be filled in:

1. **Pi invocation** in `$pi_wt`. Use the same Pi CLI shape as
   `bench/opus/run-task.sh` (look for `PI_MODEL` and the curl/CLI
   block). Capture wall time, per-message tokens, tool events.
2. **Vis invocation** in `$vis_wt` with
   `VIS_DB_PATH=$root/vis/db VIS_CRAC=0`. Use
   `anthropic-coding-plan/claude-opus-4-7`. Write `result.json` with
   `.duration-ms`, `.iteration-count`, full provenance/intent/audit
   trace so judge scoring is honest.
3. **Opus judge call** with `bench/opus/judge-prompt.md`, the prompt,
   the diffs, the `verify.sh` tails, and the trace. Require strict
   JSON output; populate `pi_correctness`, `vis_correctness`,
   `proof_floor_pass`, `quality_floor_pass`, `strict_task_win`,
   `combined_task_win`, `blockers`.

Once wired, the suite-rollup gate kicks in: never claim "Vis beats Pi"
from a single judge_required task.

## Hard task sequence (after runner exists)

Run in order; each builds on the prior:

1. `CTX1-context-contract-compact-proof-safe`
2. `PRES1-presentation-render-contract`
3. `EXTV2-extension-contract`
4. `CLJEXT1-clojure-test-classpath-tools`
5. `PYEXT1-python-structured-edit-extension`
6. `GAME1-python-snake`
7. `GIT1-single-repo-checkpoints`
8. `BGNOTIFY1-background-process-agent-end-notify`
9. `PROVIDER1-provider-visibility-custom-provider`
10. `GUIDANCE1-project-guidance-docs-ux`

CLJEXT1 must treat the completed Tasks 28–34 intent/deferred-intent
backbone as baseline (statuses `:suggested :deferred :active :fulfilled
:abandoned`; source/owner/parent fields; defer triggers; sibling
policies; abandonment scopes; conversation_intent_cursor; the suggest /
accept / defer / resume / abandon DB APIs; lifecycle transitions
provenance_event → evidence_bundle → attestation → audit). Extensions
may suggest/defer owned work but may not silently accept/resume their
own work.

## S0-hi context-reduction (trivial-turn fast path)

S0-hi sends `vis_input_tokens=18 431` for the prompt "hi". Pi sends 6.

Most of the bulk is `CORE_SYSTEM_PROMPT` (~2 000 tokens) plus the
foundation extension prompt (~6–8 k) plus the environment block plus
skills/intent/audit guidance.

Plan once `CTX1` is on the table:

- Detect "trivial / no-tool" turns: short prompt, no skill loaded, no
  fenced code, no tool-call required by the user.
- Drop foundation extension prompts when the iteration loop hasn't
  needed any extension symbol yet AND the proof/intents/audit backbone
  has no live work.
- PRESERVE proof / intents / audit anchors so any non-trivial follow-up
  re-injects the full guidance before the model can answer.
- Add a regression test that simulates follow-up tool work after a
  trivial-classified opening turn and asserts the full guidance is
  re-injected.

## Selective extension discovery for known fast commands

`vis providers list/status/auth/...` doesn't need sqlite, foundation,
channels, lang-clojure, mermaid, voice, exa.

(De-prioritised: CLI/TUI startup speed is no longer an active
optimization target — see `autoresearch.md` "Out of scope".)

## Parallel manifest discovery

17 extensions, total ~1 050 ms sequential. Largest single ns
(sqlite — already moved behind a tiny registrar in this batch) is
~480 ms. `(require ns)` is not documented as thread-safe; cyclic
loading during simultaneous threads can deadlock.

(De-prioritised: CLI/TUI startup speed is no longer an active
optimization target.)

## Tooling: pi-autoresearch upstream PR

Patch already applied locally and saved as
`tools/upstream-patches/pi-autoresearch-prepare-arguments-fix.patch`.

Root cause: `pi-autoresearch` PR #44 changed `metrics`/`asi` from
`Type.Record(...)` to `Type.Object({}, { additionalProperties: ... })`
to fix a Cloud Code Assist 400. That schema is correct, but TypeBox's
`Value.Convert` does NOT auto-parse JSON strings into objects, and
several models (Anthropic Opus 4.6/4.7, GLM-5.1, …) emit object-shaped
tool arguments as JSON strings. The validator then fails with
`metrics: must be object` / `asi: must be object`.

Fix: add a `prepareArguments` hook to `log_experiment` that
JSON-parses string `metrics` and `asi` before schema validation,
mirroring the same workaround already used by the built-in `edit` tool
for its `edits` array (see `pi-coding-agent/dist/core/tools/edit.js`,
`prepareEditArguments`).

To upstream: open a PR against `davebcn87/pi-autoresearch` carrying
the patch + a regression test that asserts string-encoded `metrics`
and `asi` round-trip cleanly through `prepareLogArguments`.
