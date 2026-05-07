# PROOF_AUTORESEARCH.md — Pi boss loop for headless Pi vs Vis proof benchmarks

Status: ready as the single committed autoresearch session artifact.

Purpose: use `pi-autoresearch` as the boss loop that compares Pi and Vis on hard Vis/proof problems, measures quality/speed/tokens on the same `zai-coding/GLM-5.1` model, then improves Vis and reruns the exact same problem. Do not claim Vis "beats Pi" from one cherry-picked task. Vis must keep quality/proof parity on every attempted task and win the combined quality/speed/token score on at least 60% of the completed proof-task suite unless the user explicitly raises the bar to 100%.

Committed artifact rule:

- Commit this file only for the autoresearch plan.
- Runtime artifacts go under `target/proof-autoresearch/…` and are not source docs.
- Do not add separate committed `autoresearch.md`, `autoresearch.sh`, score docs, or prompt docs unless the user explicitly asks. If the pi-autoresearch extension insists on its normal files, keep them as runtime/uncommitted loop files or derive them from this file.
- Final proof architecture means `proof.clj` owns proof-domain semantics. Old provenance/proof namespaces, delegates, shims, facades, proof blobs, and stale API names are migration debt to delete, not target design.

## One-command handoff

Run this from the Vis repo root to hand off the proof campaign to `pi-autoresearch`:

```bash
pi --model zai-coding/GLM-5.1 '/autoresearch Use PROOF_AUTORESEARCH.md as the boss session document and read PROOF.md before choosing work. Do not create extra committed markdown artifacts. Do not use a restrictive --tools allowlist because pi-autoresearch needs its extension tools: init_experiment, run_experiment, and log_experiment. Run headless Pi vs headless Vis benchmarks on zai-coding/GLM-5.1. Compare quality, elapsed time, and tokens. Store runtime artifacts only under target/proof-autoresearch/. Start gates: Pi model smoke, Vis JSON model smoke, ./verify.sh --quick or documented unrelated blocker, selected NEXT/IN PROGRESS proof task in PROOF.md. Known current blocker may be unrelated dirty formatting in extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/footer_test.clj; document it instead of hiding it. Start with P1 unless PROOF.md says otherwise. For each task: run the same prompt in Pi and Vis, write required artifacts, score quality/speed/tokens/proof, include quality_floor_pass strict_task_win combined_task_win, update suite rollup, implement exactly one Vis improvement with tests, fresh runtime check, ./verify.sh --quick, then rerun the exact same task. Optimize the whole proof-task suite, not one cherry-picked task: quality/proof parity must pass on every completed task and suite_combined_win_rate must be >= 0.60 before any Vis-beats-Pi claim. Preserve the trust boundary: extension_aggregate/cache/status/checkpoint is mutable extension state, not proof evidence; iteration.blocks BLOB/projections are not proof-grade block identity; secret storage is separate confidentiality work and secrets must not go in aggregate/proof blobs; :agent/end is a future terminal lifecycle boundary, not running proof; declarative :ext.symbol/aggregate is sidecar convenience only. Runtime writes immutable provenance_event observations; evidence_bundle derives facts from events; attestation decides gate/plan/intent; audit validates the chain. No TUI. No default DB deletion. V1 schema only during this phase. Stop and ask if a user-owned DB reset, real V2 migration, proof-hiding optimization, or model/provider failure is required.'
```

## Entry prompt for pi-autoresearch

Use this prompt in Pi interactive mode after the `pi-autoresearch` package is installed:

```text
/autoresearch Use PROOF_AUTORESEARCH.md as the boss session document and read PROOF.md before choosing work. Do not create extra committed markdown artifacts. Run headless Pi vs headless Vis benchmarks on zai-coding/GLM-5.1. Compare quality, elapsed time, and tokens. Store runtime artifacts under target/proof-autoresearch/. After each comparison, implement one Vis improvement with tests, restart/fresh-check runtime, then rerun the exact same problem. Optimize the whole proof-task suite, not one cherry-picked task: Vis must keep quality/proof parity on every attempted task and win the combined quality/speed/token score on at least 60% of completed tasks before claiming it beats Pi. Preserve proof/provenance/attestation semantics. Preserve the trust boundary: extension aggregate sidecars are mutable state, not proof evidence; iteration.blocks BLOB/projections are not proof-grade block identity; secret storage is separate confidentiality work; :agent/end is a future terminal lifecycle boundary; declarative aggregate sugar is sidecar convenience only. Evidence must derive from immutable provenance_event rows through evidence_bundle and attestation.
```

If using the skill directly:

```text
/skill:autoresearch-create
Goal: Make Vis beat Pi on hard proof/debug tasks using headless benchmarks on zai-coding/GLM-5.1.
Command: derive from PROOF_AUTORESEARCH.md; runtime artifacts only under target/proof-autoresearch/.
Metric: vis_loss lower is better per task; suite_win_rate must be >= 60% before claiming Vis beats Pi.
Files in scope: PROOF.md, PROOF_AUTORESEARCH.md, src/com/blockether/vis/**, extensions/**, test/**.
Constraints: no TUI, no secrets in logs, no extra committed autoresearch docs, tests after every code change, fresh runtime check after every done task, extension aggregate state is not proof evidence.
```

## Confirmed model status

Pi model check passed:

```bash
/usr/bin/time -p pi --model zai-coding/GLM-5.1 --no-tools --no-session -p \
  'Reply exactly: OK_PI_ZAI_CODING_GLM_5_1'
```

Observed result:

```text
OK_PI_ZAI_CODING_GLM_5_1
real 5.30
```

Vis provider status passed:

```bash
bin/vis providers status zai-coding
```

Observed: authenticated yes, dynamic limits visible, quota available.

Vis generation on `zai-coding/glm-5.1` works directly after P0 fix:

```bash
/usr/bin/time -p bin/vis run --json --model zai-coding/glm-5.1 --db :memory \
  'Reply exactly: OK_VIS_ZAI_CODING_GLM_5_1'
```

Observed result:

```text
answer: OK_VIS_ZAI_CODING_GLM_5_1
meta: [zai-coding/glm-5.1]
P0 verification answer: P0_VIS_MODEL_OK
P0 verification real: 13.73
P0 verification Vis JSON tokens: input 11292, output 30, reasoning 15, cached 4992, total 11322
```

P0 fixed:

- `bin/vis run --model zai-coding/glm-5.1 ...` now builds a one-shot router rooted at the requested provider/model.
- The command does not reorder or persist `~/.vis/config.edn`.
- Verification used active global provider `:openai-codex` and still returned JSON cost provider/model `zai-coding/glm-5.1`.

## Extension state versus proof evidence

Autoresearch must preserve this distinction while implementing PROOF.md:

```text
extension symbol runs
  -> writes extension_aggregate cache/status/checkpoint sidecar
  -> runtime writes provenance_event immutable observation
  -> evidence_bundle derives facts from events
  -> attestation decides gate/plan/intent
  -> audit validates whole chain
```

Rules:

- Extension persistence substrate is mutable state: aggregate, cache, status, checkpoint.
- PROOF.md ledger is immutable evidence: `provenance_event` observations.
- Evidence bundles derive values from events only. They must not trust caller-supplied extension aggregate payloads as proof.
- Attestations decide over accepted bundles.
- Audit validates the full chain and flags any shortcut from extension state directly to proof.
- `iteration.blocks` BLOB and extension aggregate projections are not proof-grade block identity. Proof-grade identity needs immutable rows/refs, terminal lifecycle status, op, and digest/summary.
- Secret storage/encryption is a separate confidentiality layer. Do not store secrets in extension aggregate blobs or proof payloads, but do not conflate secret-store work with attestation-ledger work.
- `:agent/end` is a future terminal lifecycle boundary for final-answer permission, audit finalization, notifications, and extension idle hooks. Until first-class, running/partial lifecycle phases still cannot prove completion.
- Declarative `:ext.symbol/aggregate` sugar, if added later, is sidecar-write convenience only. It must not become proof guard/attestation semantics.

Boundary-gap classification:

| Gap | Autoresearch treatment |
|---|---|
| `iteration_block` table missing | Fold into ledger identity design when implementing `provenance_event`; not required for P1 pure harness. |
| Secret storage missing | Security backlog unless a proof task touches secret material; never put secrets in generic aggregate/proof blobs. |
| Proof-ledger semantics missing | Core PROOF.md work. Start with P1, then ledger/bundle/attestation/audit. |
| `:agent/end` missing | Related lifecycle terminal event; add only when task reaches lifecycle/audit/final-answer boundary. |
| Declarative aggregate sugar missing | Lower-priority extension ergonomics; do not mix with proof semantics. |

## Headless-only rule

No TUI commands in this benchmark.

Allowed Pi path:

```bash
pi --model zai-coding/GLM-5.1 --no-session --mode json --tools read,bash,edit,write,grep,find,ls "$PROMPT"
```

Allowed Vis path:

```bash
run_id="$(date +%Y%m%d-%H%M%S)"
mkdir -p "target/proof-autoresearch/$run_id/vis"
bin/vis run --json --model zai-coding/glm-5.1 --db "target/proof-autoresearch/$run_id/vis/db" "$PROMPT"
```

## Runtime artifact layout

Each comparison run creates one directory:

```text
target/proof-autoresearch/<run-id>/
  problem.md
  pi/
    events.jsonl
    output.txt
    time.txt
    metrics.json
  vis/
    result.json
    output.txt
    time.txt
    metrics.json
    db/
      vis.db
  eval/
    score.json
    score.md
  repo/
    before.patch
    after.patch
```

Artifact rules:

- Store full Pi JSONL events.
- Store full Vis JSON result, including `trace` with provenance refs.
- Store wall time from `/usr/bin/time -p`.
- Store extracted token metrics.
- Store quality rubric results with citations to output lines / trace refs.
- Never store provider credentials or config file contents.
- Use a disposable Vis DB directory for every run, e.g. `--db target/proof-autoresearch/<run-id>/vis/db` or `VIS_DB_PATH=target/proof-autoresearch/<run-id>/vis/db`. Vis creates `vis.db` inside that directory.
- Delete/recreate runtime DBs under `target/proof-autoresearch/` freely between iterations.
- If a task edits `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`, keep it as the only migration file. Do not add V2/V3 migrations during this phase.
- Never delete `~/.vis/vis.mdb` while any Vis process may have it open. If a default dev DB reset is needed, first close TUI/CLI/child JVMs and confirm no process owns the DB.

## Metrics

Primary metric for autoresearch: `vis_loss` lower is better per task. Suite-level success, not a single-run win, is the actual benchmark target.

Compute:

```text
vis_loss =
  (100 - vis_quality)
  + max(0, pi_quality - vis_quality)
  + 5 * max(0, (vis_seconds / pi_seconds) - 1)
  + 5 * max(0, (vis_total_tokens / pi_total_tokens) - 1)
```

Task result booleans:

- `quality_floor_pass`: `vis_quality >= pi_quality` and no proof/provenance hard cap fired;
- `strict_task_win`: `quality_floor_pass`, `vis_seconds <= pi_seconds`, and `vis_total_tokens <= pi_total_tokens`;
- `combined_task_win`: `quality_floor_pass` and `vis_loss <= 5` after scoring quality/speed/tokens.

Suite metrics:

- `suite_tasks_completed`;
- `suite_quality_floor_rate = quality_floor_pass_count / suite_tasks_completed`;
- `suite_strict_win_rate = strict_task_win_count / suite_tasks_completed`;
- `suite_combined_win_rate = combined_task_win_count / suite_tasks_completed`;
- `suite_blockers_count`.

Suite success gate:

- quality floor must pass on 100% of completed tasks;
- combined task win rate must be at least 60% before any "Vis beats Pi" claim;
- strict task win rate should improve over time and should be reported, but it is not the default hard gate because proof evidence can legitimately cost extra tokens;
- if user asks for "all tasks", raise `suite_combined_win_rate` gate from 60% to 100%.

Secondary metrics:

- `pi_quality` 0-100;
- `vis_quality` 0-100;
- `pi_seconds`;
- `vis_seconds`;
- `pi_total_tokens`;
- `vis_total_tokens`;
- `vis_prompt_tokens`;
- `vis_reasoning_tokens`;
- `vis_cache_tokens`;
- `proof_refs_count` from Vis trace;
- `attestation_terms_count` in output/trace;
- `tests_passed` boolean for any code-change iteration.

A Vis change is keepable only if:

- target tests pass;
- fresh runtime check passes;
- `./verify.sh --quick` passes for code edits;
- `vis_quality >= pi_quality` or the run documents a deliberate tradeoff;
- `vis_total_tokens <= pi_total_tokens` or the task is explicitly proof-heavy and quality wins;
- no proof/provenance/attestation regression is introduced.

A Vis improvement campaign is successful only if the suite gate passes. Individual task wins are evidence, not victory.

## Token extraction

Pi JSONL token extraction:

```bash
jq -s '
  [ .[]
    | select(.type == "message_end" and .message.role == "assistant")
    | .message.usage ]
  | last
' pi/events.jsonl > pi/metrics.tokens.json
```

Pi output extraction:

```bash
jq -r '
  select(.type == "message_end" and .message.role == "assistant")
  | .message.content[]?
  | select(.type == "text")
  | .text
' pi/events.jsonl > pi/output.txt
```

Vis token extraction:

```bash
jq '.tokens' vis/result.json > vis/metrics.tokens.json
jq -r '.answer' vis/result.json > vis/output.txt
jq '[.trace[].blocks[]?.provenance.ref? | select(. != null)] | length' vis/result.json > vis/proof_refs_count.txt
```

Wall time extraction:

```bash
/usr/bin/time -p -o pi/time.txt <pi command>
/usr/bin/time -p -o vis/time.txt <vis command>
```

## Quality rubric

Score each output 0-100.

| Category | Points | Requirement |
|---|---:|---|
| Correctness | 30 | Solves or diagnoses the actual problem, not adjacent ceremony. |
| Repro discipline | 20 | Builds/uses a concrete pass-fail seam before patching. |
| Test plan | 15 | Adds or specifies targeted tests and original repro rerun. |
| Proof/provenance discipline | 15 | Cites observed refs/artifacts, rejects fake/running/compact proof. |
| Minimality/locality | 10 | Small structural change, no broad unrelated churn. |
| Reporting | 10 | Clear summary of what changed, how measured, and remaining blockers. |

Automatic hard caps:

- No concrete repro on bug task: max 60.
- No tests/checks for code-change task: max 70.
- Uses wrong model/provider: max 50.
- No proof/provenance consideration on proof task: max 75.
- Changes unrelated user files: max 40.

## Prepared proof task suite

These are the concrete tasks on which Pi vs Vis will run the proof work. Each task is a benchmark problem and an implementation slice. Run them in order unless a prerequisite fails. Measure each task independently, then compute suite-level quality floor and win rates across all completed tasks.

Shared task rules:

- Same prompt goes to Pi and Vis.
- Same model: `zai-coding/GLM-5.1` for Pi, `zai-coding/glm-5.1` for Vis.
- Same repo state or paired clean worktrees.
- Output must include:
  - reproduction/verification seam;
  - exact files changed/proposed;
  - tests to run;
  - proof/provenance/attestation implications;
  - whether the change touches extension state, proof evidence, or both;
  - legacy deletion impact.
- Vis implementation side must update `PROOF.md` progress board after each completed task.
- Any SQLite task uses disposable DB directory under `target/proof-autoresearch/<run-id>/vis/db/` and may delete/recreate it.
- Do not report "Vis beats Pi" from a single task. Report per-task result plus suite aggregate.
- Do not collapse extension aggregate state into proof evidence. Runtime must append immutable observations before bundles/attestations can prove anything.

## Gates for running autoresearch

### Start gate

Do not start a new autoresearch comparison until all are true:

- `PROOF_AUTORESEARCH.md` exists and is the only committed autoresearch session artifact.
- `pi --model zai-coding/GLM-5.1 --no-tools --no-session -p 'Reply exactly: OK'` works.
- `bin/vis run --json --model zai-coding/glm-5.1 --db :memory 'Reply exactly: OK'` returns JSON with `.cost.provider == "zai-coding"` and `.cost.model == "glm-5.1"`.
- `./verify.sh --quick` passes or any failure is documented as unrelated existing dirty-state blocker.
- `PROOF.md` progress board has a selected next proof task.

### Per-run gate

Each Pi/Vis comparison run must produce:

- `pi/events.jsonl`;
- `pi/output.txt`;
- `pi/time.txt`;
- `pi/metrics.json`;
- `vis/result.json`;
- `vis/output.txt`;
- `vis/time.txt`;
- `vis/metrics.json`;
- `eval/score.json`;
- `eval/score.md`.

`eval/score.json` must include `quality_floor_pass`, `strict_task_win`, and `combined_task_win`.

### Keep gate

A Vis change may be kept only if:

- target tests pass in a fresh JVM;
- REPL reload/restart gate passes when REPL state matters;
- `./verify.sh --quick` passes for code edits;
- output quality is equal/better or the tradeoff is explicitly accepted;
- speed/tokens improve or the proof-quality gain justifies cost;
- no proof invariant regresses;
- `PROOF.md` progress board is updated.

### Suite gate

After each completed task, update a suite rollup under the run root or latest campaign root:

- completed task IDs;
- `quality_floor_pass_count` / completed count;
- `strict_task_win_count` / completed count;
- `combined_task_win_count` / completed count;
- current `suite_combined_win_rate`.

Do not claim Vis beats Pi until:

- quality floor passes on every completed task;
- `suite_combined_win_rate >= 0.60`;
- all proof/provenance hard gates still pass;
- `PROOF.md` reflects the completed task state.

### Stop/block gate

Stop and ask user if:

- both Pi and Vis fail to use `zai-coding/GLM-5.1`;
- the task requires deleting user-owned default DB while a Vis process may still hold it;
- full implementation would require a real migration file beyond V1;
- a proposed optimization hides proof-critical evidence with no retrieval path.

### Proof task ladder

| ID | Task | Main proof capability | Required artifact |
|---|---|---|---|
| P0 | Provider-qualified Vis model selection | Fair benchmark substrate | `vis run --model zai-coding/glm-5.1` JSON provider proof |
| P1 | Pure guard/gate evidence harness | Fake-proof rejection before DB | pure tests over derived bindings |
| P2 | Canonical proof namespace purge | `proof.clj` only, no legacy owners | old namespace deletion / caller migration plan or patch |
| P3 | First-class event ledger schema | immutable observations | V1 schema + disposable DB schema test |
| P4 | Evidence bundle derivation | runtime-derived values | bundle writer/reader + fake slot rejection |
| P5 | Attestation writer | explicit decisions | gate proof/impediment attestation rows |
| P6 | Plan completion transition | gates aggregate into plan | plan completion tests separate from intent closure |
| P7 | Intent closure attestation | user commitment boundary | fulfillment requires completed plan + closure attestation |
| P8 | Audit surface | whole-system validation | `v/audit` catches missing/running/fake evidence |
| P9 | System intents + extension hooks | extensions hook proof lifecycle | fake extension observes proof events |
| P10 | Final legacy purge | no old proof/provenance bullshit | legacy rg command clean except allowed docs |

## Problem bank: hard tasks

### P0 — DONE — Provider-qualified Vis model selection

Prompt:

```text
In Vis, headless `bin/vis run --model zai-coding/glm-5.1 --db :memory "..."` currently answers using the active provider instead of zai-coding. Diagnose the routing path, implement provider-qualified one-shot model selection without mutating ~/.vis/config.edn, add regression tests, and show a headless command proving the result meta line or JSON cost provider is zai-coding/glm-5.1.
```

Why hard:

- Crosses CLI arg parsing, config/provider coercion, svar router creation, and run result metadata.
- Directly affects fair benchmarking.

Verification:

```bash
clojure -M -e '(require (quote [com.blockether.vis.internal.main :as main])) ... :p0-model-override-ok'
bin/vis run --json --model zai-coding/glm-5.1 --db :memory 'Reply exactly: P0_VIS_MODEL_OK'
./verify.sh --quick
```

Note: `clojure -M:test -n ...` is currently blocked by unrelated dirty test namespace state in the working tree; P0 also has a regression in `test/com/blockether/vis/internal/main_test.clj` for the next full suite run.

### P1 — Guard/gate pure evidence harness

Prompt:

```text
Implement PROOF.md Task 5A. Add pure guard evaluation over runtime-derived bindings only. Add tests proving fake caller slot payloads, fake extension aggregate proof payloads, compact refs, running events, wrong event kind/op, missing extraction paths, and false guards cannot prove a gate. Do not add SQLite storage yet. Preserve the boundary: extension_aggregate/cache/status/checkpoint is mutable state; only runtime-derived immutable event observations may feed evidence bundles.
```

Why hard:

- Needs proof semantics without hiding runtime checks inside specs.
- Must produce a tight pass/fail seam before persistence.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.internal.proof-test
./verify.sh --quick
```

Expected proof artifacts:

- `proof/evaluate-guard` or equivalent pure API;
- `proof/derive-binding` or equivalent extraction API;
- gate harness function that consumes events + requirements and returns accepted/rejected decision data;
- explicit rejection case showing `extension_aggregate` / sidecar payloads cannot stand in for evidence;
- tests named around fake slot payload, fake extension aggregate proof, running event, compact ref, wrong op, missing extract, false guard.

### P2 — Canonical proof namespace purge

Prompt:

```text
Finish PROOF.md Task 3. Migrate every internal caller from provenance_ref.clj and provenance_lifecycle.clj to com.blockether.vis.internal.proof. Delete the old namespaces and their tests or move tests to proof_test. Add a legacy-purge check proving no source namespace requires the deleted namespaces. Do not preserve delegates/facades/shims.
```

Why hard:

- Crosses loop, persistence, foundation, and tests.
- Must remove old names without breaking current provenance rendering.

Expected verification:

```bash
rg -n "com.blockether.vis.internal.provenance-(ref|lifecycle)|provenance_ref|provenance_lifecycle" src extensions test
clojure -M:test -n com.blockether.vis.internal.proof-test
./verify.sh --quick
```

Expected proof artifacts:

- only `proof.clj` owns canonical ref/lifecycle code;
- old files removed;
- old namespace tests deleted or folded into `proof_test.clj`.

### P3 — First-class event ledger schema

Prompt:

```text
Implement the first SQLite ledger slice from PROOF.md Task 6 and Task 13. Add `provenance_event` to V1__schema.sql only. Treat current iteration.blocks BLOB and extension aggregate projections as non-proof-grade projections. Add persistence helpers to insert/query immutable events using HoneySQL. Tests must create a disposable DB from V1, insert done/running/error events, reject duplicate canonical refs, and prove proof-visible refs resolve through the ledger. Do not add V2/V3 migrations.
```

Why hard:

- Crosses inline schema, HoneySQL, Nippy/JSON payload shape, and lifecycle semantics.
- Must be safe for autoresearch by using disposable DB.

Expected verification:

```bash
VIS_DB_PATH=target/proof-autoresearch/manual/vis/db clojure -M:test -n com.blockether.vis.ext.persistance-sqlite.core-test
./verify.sh --quick
```

Expected proof artifacts:

- V1 schema change only;
- disposable DB can be deleted/recreated;
- ledger row has canonical ref, kind, op, status, payload digest;
- clear note whether `iteration_block` identity is still soft/projected or now proof-grade;
- running events query but do not satisfy proof compatibility.

### P4 — Evidence bundle derivation

Prompt:

```text
Implement evidence bundle derivation from ledger events. Add bundle/member storage and pure derivation code that resolves canonical refs, extracts values by path, evaluates guards, records guard_ok and derived_value, and rejects caller-supplied slot values. Use V1__schema.sql only and disposable DB tests.
```

Why hard:

- This fixes the core fake-proof weakness.
- Must join pure proof code with persistence without letting model claims become facts.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.internal.proof-test -n com.blockether.vis.ext.persistance-sqlite.core-test
./verify.sh --quick
```

Expected proof artifacts:

- `evidence_bundle` and `evidence_bundle_member` rows;
- fake slot payload regression test;
- missing extract and false guard regression tests.

### P5 — Attestation writer

Prompt:

```text
Implement attestation storage/writers for gate proof and gate impediment. A gate proof must point to an accepted evidence bundle; old proof blobs must not be authoritative. Add tests proving accepted gate-proof attestation changes gate state and rejected/missing bundles do not.
```

Why hard:

- Decision layer must be explicit and auditable.
- Must start replacing old proof blobs without preserving old semantics.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.ext.persistance-sqlite.core-test
./verify.sh --quick
```

Expected proof artifacts:

- `attestation` rows;
- gate proof/impediment decisions;
- audit-visible link: gate -> attestation -> bundle -> ledger events.

### P6 — Plan completion transition

Prompt:

```text
Implement automatic plan completion after all required gates are proven by attestations. Keep intent fulfillment separate. Add tests proving one gate does not complete multi-gate plan, all required gates complete the plan, impeded required gates block or abandon the plan but do not silently abandon the intent.
```

Why hard:

- Clarifies gate vs plan vs intent lifecycle.
- Prevents accidental user commitment closure.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.ext.persistance-sqlite.core-test
./verify.sh --quick
```

Expected proof artifacts:

- plan completion attestation;
- plan status transition tests;
- intent remains active until explicit closure.

### P7 — Intent closure attestation

Prompt:

```text
Implement intent closure/abandonment attestations. Fulfillment requires a completed active plan and closure rationale; abandonment requires blocker evidence or a blocked/abandoned plan. Direct refs become optional artifact refs, not repeated gate proof bags.
```

Why hard:

- This is the user-facing trust boundary.
- Must preserve final-answer blocking until focused intents resolve.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.ext.persistance-sqlite.core-test -n com.blockether.vis.internal.loop-test
./verify.sh --quick
```

Expected proof artifacts:

- intent closure attestation;
- abandonment attestation;
- final answer blocked with active focused intent.

### P8 — Audit surface

Prompt:

```text
Implement `v/audit` as the primary validation surface over ledger, bundles, attestations, gate states, plan states, and intent states. It must catch missing events, duplicate refs, running refs used as proof, failed guards, missing bundles, invalid attestation subjects, invalid plan completion, invalid intent closure, and stale proof blob dependencies.
```

Why hard:

- Whole-system validation spans all previous layers.
- Must produce bounded, human-readable violations instead of giant transcript dumps.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.ext.foundation.introspection-test -n com.blockether.vis.ext.persistance-sqlite.core-test
./verify.sh --quick
```

Expected proof artifacts:

- structured audit report;
- precise violation codes;
- deprecated proof-check names absent or outer-boundary only.

### P9 — System intents and extension proof hooks

Prompt:

```text
Implement PROOF.md Task 10A. Add source/owner fields for user/system/extension intents and proof lifecycle hook events: event appended, evidence bundle created, attestation accepted, audit violation, final answer blocked. Add fake extension tests proving hooks receive structured payloads and do not scrape prompt prose.
```

Why hard:

- Connects proof lifecycle with Vis extensibility.
- Must not turn extensions into prompt parsers.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.internal.extension-test -n com.blockether.vis.internal.proof-test
./verify.sh --quick
```

Expected proof artifacts:

- system intent spec;
- extension-owned intent spec;
- ordered proof lifecycle hook payload tests.

### P10 — Final legacy purge

Prompt:

```text
Hard-remove all old proof/provenance implementation names after new audit/attestation paths work. Delete old namespaces, old proof blob authority, old proof check/provenance guard internals, compact-ref proof writes, and stale docs/prompts. Keep only explicitly deprecated outer-boundary aliases if still required, with removal notes.
```

Why hard:

- Ensures the migration actually finishes.
- Prevents the old weak model from surviving under new names.

Expected verification:

```bash
rg -n "provenance[-_]guards|proof[-_]checks|proof blob|conversation_intent_gate_ref|provenance_ref|provenance_lifecycle" src extensions test docs
./verify.sh
```

Expected proof artifacts:

- search output clean except approved docs/deprecation notes;
- no old namespaces;
- no authoritative old proof blob read/write paths.

### P11 — Compact proof/provenance prompt surface

Prompt:

```text
Use ANALYSIS.md to reduce Vis token load without weakening proof/provenance. Compact extension metadata and prompt/journal proof surfaces so model-facing context keeps only decision-relevant provenance by default, while full provenance remains available on demand. Add tests proving full provenance is still retrievable and model-facing output drops source hashes/license/owner noise.
```

Why hard:

- Must improve token cost while preserving auditability.
- Crosses prompt, extension rendering, foundation previews, and tests.

Expected verification:

```bash
clojure -M:test -n com.blockether.vis.internal.prompt-test -n com.blockether.vis.internal.extension-test
./verify.sh --quick
```

## Benchmark loop

For each problem:

1. Create clean branch/worktree for Pi run and clean branch/worktree for Vis run.
2. Run Pi headless on `zai-coding/GLM-5.1`.
3. Run Vis headless on `zai-coding/glm-5.1`.
4. Extract output, elapsed seconds, tokens, and proof/provenance artifacts.
5. Score both outputs with the rubric.
6. Pick one Vis weakness from the comparison.
7. Implement one Vis fix.
8. Run targeted tests in a fresh JVM.
9. Restart/reload REPL and check changed namespaces fresh.
10. Run `./verify.sh --quick`.
11. Rerun the exact same problem prompt with Pi and Vis.
12. Log metric lines:

```text
METRIC vis_loss=<number>
METRIC pi_quality=<number>
METRIC vis_quality=<number>
METRIC pi_seconds=<number>
METRIC vis_seconds=<number>
METRIC pi_total_tokens=<number>
METRIC vis_total_tokens=<number>
METRIC proof_refs_count=<number>
METRIC quality_floor_pass=<true|false>
METRIC strict_task_win=<true|false>
METRIC combined_task_win=<true|false>
METRIC suite_combined_win_rate=<number>
```

Keep only if Vis improves without correctness/proof regression. Do not claim suite victory until the suite gate passes.

## Fresh runtime rule after every completed task

For every Vis code change:

```bash
# Targeted fresh JVM test.
clojure -M:test -n <changed-test-namespace>

# Restart/reload REPL if using nREPL state.
clj-nrepl-eval --discover-ports
clj-nrepl-eval -p 7888 "(require '[<changed.namespace> :as x] :reload) :fresh-ok"

# Quick global gate.
./verify.sh --quick
```

When the task explicitly needs a real restart:

```bash
clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload) (dev/stop-nrepl!)"
NREPL_PORT=7888 bin/dev > .nrepl.log 2>&1 &
clj-nrepl-eval -p 7888 "(require '[<changed.namespace> :as x] :reload) :fresh-ok"
```

Then update `PROOF.md` progress board:

- mark done task `DONE`;
- record exact commands;
- mark next task `NEXT`;
- record blockers.

## Proof/provenance requirements during benchmark

Vis must preserve and expose:

- canonical refs only in proof-facing writes;
- no running events as proof;
- no compact display refs as proof;
- derived evidence values, not caller-injected slot claims;
- attestations for gate/plan/intent decisions once those layers exist;
- audit report explaining violations.

Benchmark scoring must penalize Vis if improvements reduce prompt tokens by hiding proof-critical truth with no retrieval path.

## First recommended autoresearch run

P0 is complete enough to start fair comparisons. Start with P1.

Expected first loop:

1. Baseline confirms Pi uses `zai-coding/GLM-5.1`.
2. Baseline confirms Vis uses `zai-coding/glm-5.1` through `bin/vis run --model zai-coding/glm-5.1`.
3. Run P1 in both systems.
4. Compare quality/speed/tokens/proof artifacts.
5. Write per-task booleans: `quality_floor_pass`, `strict_task_win`, `combined_task_win`.
6. Update suite rollup. First run can be 0% or 100%, but it is not a victory claim.
7. Implement one Vis improvement.
8. Rerun the exact same P1 prompt.
9. Continue P2...P11 until quality floor passes on all completed tasks and combined win rate is at least 60%.
