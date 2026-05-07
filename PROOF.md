# PROOF.md — proof.clj consolidation and attestation-ledger implementation plan

Status: implementation started. First slice installed the canonical proof-domain namespace. Task 5A pure guard/gate harness is installed and tested. Legacy provenance namespaces still exist only as blockers to delete; storage/audit migration remains staged work.

Scope: attestation ledger, provenance references, lifecycle events, intent/plan/gate proof semantics, and audit surface.

## Fit in the Vis framework

Vis already uses an OODA-shaped loop: observe runtime, orient through transcript/journal/vars, decide through intents/plans/gates, act through SCI/tools, then verify before final answer. This proof work gives that loop a durable trust spine:

```text
OODA observation -> provenance_event ledger -> evidence bundle -> attestation -> audit -> answer permission
```

Right spot: internal proof semantics belong in `com.blockether.vis.internal.proof`; persistence writes durable rows; foundation `v/` functions expose event/bundle/attestation/audit reads; channels render reports only. Prompt text should stay thin and point the model at functions instead of dumping proof state into context.

Attestation things are not UI decoration. They become the decision layer between observed runtime facts and user-facing lifecycle transitions:

- gate proof/impediment attestations decide one proposition;
- plan completion/blocking attestations aggregate required gates;
- intent closure/abandonment attestations decide the user commitment;
- audit checks that every transition is backed by ledger events, bundles, and attestations.

## Difference from Codex goal/plan model

Codex guidance centers on durable goals, plans, validation commands, and status docs. That is execution hygiene: keep long-horizon work scoped, steerable, and verifiable. Vis proof goes below that into runtime evidence semantics. A Codex-style goal can say "done when tests pass"; Vis proof must record which observed event showed the test exit, which extraction/guard checked it, which attestation accepted it, and which intent closure consumed it.

Short version:

| Model | Primary object | Done means | Weakness this plan fixes |
|---|---|---|---|
| Codex goals/plans | task plan + validation status | agent/user agree acceptance criteria passed | mostly document/process truth |
| Vis proof | ledger + bundle + attestation + audit | runtime evidence justifies lifecycle transition | no caller-injected fake proof |

## Difference from Ralph loop

Ralph is loop + fresh context + verifier + persistence through files/git. It is a control strategy for making progress after failures. Vis already shares the loop instinct, but proof adds typed evidence and lifecycle boundaries inside each loop. Ralph asks "did verifier pass; if not, loop again?" Vis proof asks "which immutable event proves verifier pass, which gate did it prove, did the plan complete, and may the intent close?"

Short version:

| Model | Strength | Stop condition | Vis delta |
|---|---|---|---|
| Ralph loop | persistence and repeated repair with fresh context | external verifier/marker says done | add explicit evidence graph and audit before answer |
| Vis proof | trustable state transitions | focused intents fulfilled/abandoned with attestations | use Ralph/autoresearch as outer loop, not proof source |

## What is missing / wrong in current state

Missing:

- first-class immutable `provenance_event` rows;
- evidence bundles with runtime-derived values;
- attestation rows for gate, plan, and intent decisions;
- audit over ledger, bundles, attestations, gates, plans, and intents;
- automatic plan completion transition;
- intent closure over completed plan instead of repeated raw refs.

Wrong today:

- proof slots can still be caller-supplied claims;
- guards validate supplied slot maps instead of extracted event facts;
- proof-critical checks can depend on reconstructed projections;
- old names (`proof checks`, `provenance guards`) blur layers;
- compact display refs must remain display-only and never proof writes.

## Impact from `ANALYSIS.md`

`ANALYSIS.md` changes this plan from "build proof objects" to "build proof objects that force a tight feedback loop." The failed Vis run had tools, intents, gates, and skills, but no deterministic repro artifact. Therefore proof work must include workflow proof, not only data proof.

Required changes from the analysis:

- Repro before proof ceremony for bug work. Bug mode order is `UNDERSTAND -> REPRO -> INTENT -> GATES -> PATCH -> VERIFY -> ATTEST`.
- A proof ref is not enough. Each gate needs a runnable reproduction/verification seam and a regression test plan.
- Every task must end with fresh-runtime verification: targeted test in a fresh JVM, REPL restart/reload check, then `./verify.sh --quick` for code edits.
- Tool output must be bounded. Audit/proof reports cite counts, refs, and selected rows, not giant transcript dumps.
- Guards/gates must become testable objects: guard expression shape tests, guard evaluation tests over derived values, persistence tests rejecting fake caller slots, audit tests catching stale/missing evidence.
- Legacy proof/provenance names are not part of the target design. During an active migration slice they may exist only as failing blockers to remove; semantic ownership is `proof.clj`, then `ledger`, `bundle`, `attestation`, and `audit` namespaces.
- System intents are first-class: user intents, system intents, and extension-owned intents need explicit source/owner fields so extensions can query durable intent state and hook proof lifecycle events without scraping prose.

## Execution discipline for every implementation task

Each task below has three completion gates:

1. **Task test gate** — run the smallest target test that proves the changed seam.
2. **Fresh runtime gate** — restart or recreate runtime before declaring done.
   - Fresh JVM default: run the targeted `clojure -M:test -n ...` command in a new process.
   - REPL check when REPL state matters:

     ```bash
     clj-nrepl-eval --discover-ports
     clj-nrepl-eval -p 7888 "(require '[changed.namespace :as x] :reload) :fresh-ok"
     ```

   - Full REPL restart when requested for the task:

     ```bash
     clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload) (dev/stop-nrepl!)"
     NREPL_PORT=7888 bin/dev > .nrepl.log 2>&1 &
     clj-nrepl-eval -p 7888 "(require '[changed.namespace :as x] :reload) :fresh-ok"
     ```

3. **Progress marker gate** — edit this file immediately:
   - mark completed task as `DONE` with verification commands;
   - mark exactly one next task as `NEXT`;
   - record blockers as `BLOCKED` with a concrete question or failing command.

During code edits run `./verify.sh --quick`. Before commit-ready handoff run full `./verify.sh`.

## User decision checkpoints from ANALYSIS implementation

Do not turn every analysis proposal into production code automatically. Some ideas are policy/product choices and must stay explicit decision points for the user.

Current decisions to hold for user review:

Completed implementation from `ANALYSIS.md` proposals:

- duplicate executable fenced blocks are rejected before SCI eval;
- `z/locators` rendering is compact and patch-oriented;
- `TURN_ACTIVE_EXTENSIONS` is compact model-facing metadata only;
- active skill bodies render under `<active_skills>`, not `<extensions>`;
- bug/fix/debug requests auto-load the `diagnose` skill;
- `v/nrepl-eval` exists for Clojure runtime checks;
- long shell-quoted `clj-nrepl-eval` through `v/bash` is refused in favor of `v/nrepl-eval`.

Rejected / user-decision items:

1. **TUI render diagnostic helper** — rejected as production `chat.clj` code for now.
   - We already have transcript API/CLI surfaces for conversation reports.
   - A hard-coded helper counting `SHELL bash` / `SEARCH any` in runtime chat projection is too task-specific.
   - If wanted, implement as dev/test diagnostic tooling or a generic report command with caller-supplied matchers, not as TUI chat runtime logic.

2. **Bug-mode repro nudge** — rejected as generic iteration-2 prompt nag for now.
   - Many tool tasks complete in one iteration, so an iteration-2 nudge is late or useless.
   - More prompt nudges can worsen the same noise problem `ANALYSIS.md` criticized.
   - If wanted, enforce reproduction earlier through harness/intent/proof workflow, not another soft model-facing reminder.

3. **UUID/transcript-before-grep prompt nudge** — not implemented by default.
   - We already have transcript API and CLI/report surfaces for conversation evidence.
   - A prompt nag that says "inspect transcript before grep" may be right for some work, but should be a user decision because it changes model behavior globally.
   - Prefer explicit user/workflow instruction or a dev/report command over always-on prompt noise.

4. **Tool/prompt nudges generally** — require user approval when they change model behavior.
   - Prefer concrete artifacts: transcript commands, regression tests, proof slots, and verification logs.
   - Add nudges only when they are tied to a specific observed failure and have tests proving they reduce bad behavior without bloating normal one-shot tasks.

Rule: proposals that alter production runtime, model prompts, or tool refusal behavior need reproduction + regression + user-visible tradeoff note before being marked DONE.

These are current decisions, not permanent bans. Revisit a rejected/user-decision item only with:

1. concrete reproduction;
2. generic design, not task-specific hard-code;
3. regression test;
4. token/UX/proof tradeoff note;
5. explicit user approval.

## Intent tree and deferred-intent decisions

Recorded in `CONTEXT.md` and ADR `docs/adr/0003-intent-tree-and-deferred-intents.md`.

Canonical model:

- An **Intent** lifecycle has `:suggested`, `:deferred`, `:active`, `:fulfilled`, and `:abandoned` states.
- Only `:active` blocks current completion/final answer.
- A conversation has exactly one **Intent Cursor** and therefore at most one **Running Intent** at a time.
- **Subintents** are real intent nodes in an **Intent Tree**, not plan steps and not parallel same-level work.
- The cursor executes depth-first: descend into one subintent, finish/defer/abandon that branch, then move to an adjacent sibling.
- An intent is either decomposed into subintents or resolved by its own plan/gates, not both.
- **Suggested Intents** live in the same intent lifecycle/table as other intents; extension-created work starts suggested, not committed.
- **Intent Acceptance** is user or host-owned system policy only. Extensions may suggest but may not silently accept their own suggestion.
- Extensions discover suggested/deferred work through **Intent Queries**, not an intent transition event bus. DB state is truth; events/nudges are advisory.
- **Deferred Intents** wait on exactly one minimal **Defer Trigger** kind: `:defer/user-input`, `:defer/time`, `:defer/extension-signal`, or `:defer/intent`.
- A deferred subintent records **Defer Sibling Policy**: `:defer/continue-siblings` or `:defer/block-parent`.
- Trigger observation makes a **Resumable Intent**; a separate **Resume Decision** by user or host-owned system policy moves the cursor. Extensions may query/nudge but may not move the cursor themselves.
- Direction changes use an **Abandonment Gate** with explicit **Abandonment Scope**: `:abandon/current-intent`, `:abandon/current-branch`, or `:abandon/all-running`; if unclear, ask.

Proof boundary:

```text
extension suggestion/query/nudge/sidecar != proof authority
user/system/extension observation -> provenance_event -> evidence_bundle -> attestation -> audit
```

Implementation implications:

- Replace plural focused-intent semantics with one running cursor, or keep existing shapes only as compatibility projections with cardinality <= 1.
- Add source/owner/acceptance/defer/resume/tree fields to intent persistence.
- Add intent query filters for status, source, owner extension, resumable state, parent, and conversation.
- Add defer/resume/abandon APIs whose state transitions are attestation/audit visible.
- Keep extension auto-accept/resume out of scope until a future explicit permission model exists.

## Roll protocol for PROOF work

Default next implementation target is the first `NEXT`/`IN PROGRESS` row in the progress board, unless `PROOF_AUTORESEARCH.md` is running the comparison loop. Autoresearch must optimize the whole proof-task suite: no "Vis beats Pi" claim from one task; quality/proof parity must hold on every completed task and combined win rate must be at least 60% unless the user explicitly raises the bar to all tasks.

Before starting any task:

1. Read this file's progress board.
2. Confirm one task is selected.
3. State the task gate:
   - repro/verification seam;
   - target tests;
   - fresh runtime check;
   - proof/provenance invariant being protected.
4. Do the smallest slice.
5. Mark the task `DONE`, `BLOCKED`, or keep `IN PROGRESS` with exact commands.
6. Mark the next task `NEXT`.

No task is DONE if:

- old proof/provenance files survive when that task was deletion-scoped;
- test namespace is missing for a changed Clojure namespace;
- fresh runtime check is skipped without a written reason;
- proof-facing writes accept compact refs, running events, or caller-injected slot facts.

## Progress board

| Task | Status | Verification / note |
|---|---|---|
| Task 1 docs/model fit | DONE | `PROOF.md` updated with framework fit, Codex/Ralph comparison, ANALYSIS impact. Docs-only. |
| Task 2 proof spec first slice | DONE | `clojure -M:test -n com.blockether.vis.internal.proof-test -n com.blockether.vis.internal.provenance-ref-test -n com.blockether.vis.internal.provenance-lifecycle-test -n com.blockether.vis.internal.intent-spec-test`; `./verify.sh --quick`. |
| Task 3 canonical proof namespace migration | DONE | Internal callers now require `com.blockether.vis.internal.proof`; deleted `provenance_ref.clj`, `provenance_lifecycle.clj`, and their tests. Verified: `rg -n "com\\.blockether\\.vis\\.internal\\.provenance-(ref|lifecycle)|provenance_ref|provenance_lifecycle|prov-life|prov-ref" src extensions test`; `clojure -M:test -n com.blockether.vis.internal.proof-test -n com.blockether.vis.internal.loop-test -n com.blockether.vis.ext.foundation.introspection-test -n com.blockether.vis.ext.persistance-sqlite.core-test`; `./verify.sh --quick`. |
| Task 5A pure guard/gate harness | DONE | `clojure -M:test -n com.blockether.vis.internal.proof-test`; fresh REPL reload `(require '[com.blockether.vis.internal.proof :as proof] :reload) :fresh-ok`. Covers fake caller slots, fake extension aggregate proof payloads, compact refs, running events, wrong kind/op, missing extract, and false guards. |
| PROOF_AUTORESEARCH boss plan | DONE | `PROOF_AUTORESEARCH.md` added. Pi and Vis `zai-coding/GLM-5.1` smoke-checked headlessly; P0 provider-qualified `vis run --model zai-coding/glm-5.1` fixed and verified. |
| Task 6 ledger storage | DONE | Added `provenance_event` to V1 only, HoneySQL insert/query helpers, public facade delegates, duplicate canonical-ref rejection, and running-event non-proof test. Verified: `clojure -M:test -n com.blockether.vis.ext.persistance-sqlite.core-test`; `clojure -M:test -n com.blockether.vis.core-test -n com.blockether.vis.internal.persistance-test`; fresh REPL reload of `vis.core`, `internal.persistance`, and SQLite core; `./verify.sh --quick`. |
| Task 7 evidence bundle storage | DONE | Added `evidence_bundle` / `evidence_bundle_member` to V1 only, persistence derivation from `provenance_event`, public facade delegates, and regression proving caller-supplied slot values are ignored. Verified: `clojure -M -e ... :task7-smoke-ok`; fresh REPL reload of `vis.core`, `internal.persistance`, and SQLite core; `./verify.sh --quick`. Note: full `com.blockether.vis.ext.persistance-sqlite.core-test` currently hit pre-existing multiprocess child timeout assertions; non-multiprocess ledger/bundle cases ran before those failures. |
| Task 8 attestation storage | DONE | Added `attestation` V1 table, attestation persistence/read facade, and `db-attest-gate!` writer that requires an accepted evidence bundle and updates gate status as an attestation-derived projection. Rejected bundles cannot prove gates. Verified: attestation in-memory SQLite smoke, fresh REPL reload of `vis.core`, `internal.persistance`, SQLite core, and `./verify.sh --quick`. Note: full SQLite core test still hits pre-existing multiprocess child timeout assertions after passing proof/attestation cases. |
| Task 9 plan completion transition | DONE | Plan status now transitions to `completed` only when all required gates are proven by accepted gate attestations; one proven gate does not complete a multi-gate plan, and intent remains active. Verified: task9 in-memory SQLite smoke, fresh REPL reload of `vis.core`, `internal.persistance`, SQLite core, and `./verify.sh --quick`. |
| Task 10 intent fulfillment/abandonment | DONE | Added intent closure attestation writer. Intent fulfillment now has an attestation-backed path requiring an accepted closure evidence bundle plus a completed plan; early closure over an open plan fails and leaves the intent active. Verified: task10 in-memory SQLite smoke, fresh REPL reload of `vis.core`, `internal.persistance`, SQLite core, and `./verify.sh --quick`. |
| Task 11 audit surface | DONE | Added `db-audit-proof` read-only audit over gates, plans, intents, and accepted attestations. The audit passes for the attestation-ledger closure path and reports legacy closure that bypasses accepted intent closure attestation. Verified: task11 in-memory SQLite smoke, fresh REPL reload of `vis.core`, `internal.persistance`, SQLite core, and `./verify.sh --quick`. |
| Task 12 audit rendering | DONE | `v/audit-report` now renders the persisted proof audit summary and violations alongside the provenance report, so channels and humans can see attestation-ledger failures without scraping proof prose. Verified: foundation audit-report regression, task12 in-memory SQLite smoke, fresh REPL reload of `vis.core`, `internal.persistance`, SQLite core, foundation introspection, and `./verify.sh --quick`. |
| Task 13 extension lifecycle hooks | DONE | Added structured `:ext/on-proof-event-fn` hook dispatch for `:proof/event-appended`, `:proof/evidence-bundle-created`, `:proof/attestation-accepted`, and `:proof/audit-violation`. Persistence emits hook payloads from ledger, bundle, attestation, and audit paths; listener failures are logged and do not break writes. Verified: extension hook regression, task13 in-memory SQLite hook smoke, fresh REPL reload of `vis.core`, `internal.extension`, `internal.persistance`, SQLite core, and `./verify.sh --quick`. |
| Task 14 audit API migration | DONE | Added `v/audit` as the model-facing data surface over `db-audit-proof`, kept `v/audit-report` for human rendering, and updated the prompt to gate normal answers on `(:success? (v/audit))` while marking `v/proof-checks` as legacy gate detail. Verified: foundation audit data/report regression, task14 in-memory SQLite smoke, fresh REPL reload of `vis.core`, SQLite core, foundation introspection, and `./verify.sh --quick`. |
| Task 15 attestation helper migration | DONE | Added `v/attest-gate!` and `v/attest-intent!` helpers that create accepted evidence bundles from runtime `provenance_event` requirements and write gate/intent attestations. Legacy `v/prove-gate!` and `v/fulfill-intent!` remain compatibility-only and are documented as such. Verified: foundation attestation helper regression, task15 in-memory SQLite smoke, fresh REPL reload of `vis.core`, SQLite core, foundation introspection, and `./verify.sh --quick`. |
| Task 16 prompt hard-deprecation | DONE | Updated the core system prompt decision matrix, examples, and finish pattern to use `v/attest-gate!`, `v/attest-intent!`, and `v/audit`; removed model-facing `v/prove-gate!` / `v/fulfill-intent!` mentions from the core prompt while keeping compatibility functions private/public for migrated tests and old calls. Verified: prompt regression, task16 prompt smoke, foundation introspection regression, fresh REPL reload of `vis.core`, `internal.prompt`, foundation introspection, and `./verify.sh --quick`. |
| Task 17 runtime audit appendix | DONE | Runtime final-answer appendix now asks foundation for `foundation-audit` / `foundation-audit-report` and appends proof-audit Markdown only when persisted audit succeeds, instead of legacy `foundation-proof-checks` / `<proofs>`. Duplicate detection covers existing proof audit blocks. Verified: loop runtime appendix regression, task17 runtime smoke, fresh REPL reload of `vis.core`, `internal.loop`, foundation introspection, and `./verify.sh --quick`. |
| Task 18 public proof-checks removal | DONE | Removed public `v/proof-checks` and `v/proofs` symbols from the foundation sandbox export list and prompt fragment. Private diagnostic helpers remain for regression tests only. Proof-management self-reference detection now recognizes `v/audit`, `v/audit-report`, `v/attest-gate!`, and `v/attest-intent!`. Verified: foundation introspection regression, task18 symbol smoke, fresh REPL reload of `vis.core` and foundation introspection, and `./verify.sh --quick`. |
| Task 19 public prove/fulfill removal | DONE | Removed public `v/prove-gate!` and `v/fulfill-intent!` symbols from the foundation sandbox export list and top-level docs. Private compatibility functions remain for existing internal tests and old host callers; model-facing resolution is now `v/attest-gate!` / `v/attest-intent!`. Verified: foundation introspection regression, task19 symbol smoke, fresh REPL reload of `vis.core` and foundation introspection, and `./verify.sh --quick`. |
| Task 20 loop test/caller migration | DONE | Migrated internal loop final-answer-gate tests off legacy `db-prove-gate!` / `db-fulfill-intent!` and onto accepted evidence bundles plus gate/intent attestations. Runtime wrapper acceptance now uses attested state while suppressing appendix in that narrow test. Verified: loop-test regression, task20 smoke, fresh REPL reload of `vis.core`, SQLite core, `internal.loop`, and `./verify.sh --quick`. |
| Task 21 foundation test migration slice | DONE | Migrated normal foundation intent resolution and compact-ref rejection tests from `foundation-prove-gate!` / `foundation-fulfill-intent!` to attestation helpers. Remaining legacy helper uses are isolated to private `foundation-proof-checks` diagnostic regressions. Verified: foundation introspection regression, task21 grep smoke, fresh REPL reload of `vis.core`, SQLite core, foundation introspection, and `./verify.sh --quick`. |
| Task 22 private proof-check helper cleanup | DONE | Removed remaining `foundation-prove-gate!` / `foundation-fulfill-intent!` calls from foundation introspection tests by deleting stale private proof-check/proofs diagnostics whose public symbols were removed in Task 18. Only the explicit legacy audit-bypass fixture still uses `db-prove-gate!` / `db-fulfill-intent!` to prove audit detects old writes. Verified: foundation introspection regression, task22 grep smoke, fresh REPL reload of `vis.core` and foundation introspection, and `./verify.sh --quick`. |
| Task 23 legacy audit fixture migration | DONE | Replaced the last foundation legacy audit-bypass fixture call to `db-prove-gate!` / `db-fulfill-intent!` with attested gate setup plus direct legacy intent-row status update. Foundation tests no longer call legacy prove/fulfill writers. Verified: foundation introspection regression, task23 grep smoke, fresh REPL reload of `vis.core`, SQLite core, foundation introspection, and `./verify.sh --quick`. |
| Task 24 persistence audit fixture migration | DONE | Replaced the persistence core audit-bypass fixture call to `db-prove-gate!` / `db-fulfill-intent!` with attested gate setup plus direct legacy intent-row status update. The audit regression still proves closure bypass is detected, without exercising legacy writer APIs. Verified: persistence SQLite aggregate regression, task24 grep smoke, fresh REPL reload of `vis.core` and SQLite core, and `./verify.sh --quick`. |
| Task 25 persistence happy-path migration | DONE | Migrated the conversation-intent happy path/idempotency regression from `db-prove-gate!` / `db-fulfill-intent!` to provenance events, evidence bundles, `db-attest-gate!`, and `db-attest-intent!`. Raw proof/intent-ref expectations now assert attestation/evidence rows instead of legacy ref rows. Verified: persistence SQLite aggregate regression, foundation introspection regression, task25 grep smoke, fresh REPL reload of `vis.core` and SQLite core, and `./verify.sh --quick`. |
| Task 26 persistence ref-rejection migration | DONE | Migrated the remaining persistence core `db-prove-gate!` ref-rejection regressions to evidence-bundle/attestation behavior: compact refs and unobserved refs reject bundles, running/timeout lifecycle events remain blocker-compatible but proof-bundle rejected. Persistence tests no longer call `db-prove-gate!` or `db-fulfill-intent!`. Verified: persistence SQLite aggregate regression, foundation introspection regression, task26 grep smoke, fresh REPL reload of `vis.core` and SQLite core, and `./verify.sh --quick`. |
| Task 27 legacy write-authority hard-error | NEXT / REQUIRED | Hard-error or delete underlying legacy `db-prove-gate!` / `db-fulfill-intent!` write authority and remove public core/delegate exports after final grep confirms no internal tests/callers depend on them. |
| Task 28 intent lifecycle spec | PLANNED | Add proof/intent specs for `:suggested`, `:deferred`, `:active`, `:fulfilled`, `:abandoned`, intent source/owner, acceptance/resume decision actors, defer trigger kinds, defer sibling policy, abandonment scope, parent/subintent relationships, and single running cursor cardinality. |
| Task 29 intent persistence schema | PLANNED | Extend inline V1 `conversation_intent` storage with source/owner/acceptance/defer/resume/tree metadata and add a single cursor representation. Preserve migration rule: edit V1 directly unless user asks for backward migrations. |
| Task 30 intent query surface | PLANNED | Add DB/foundation query filters for status, source, owner extension, resumable state, parent, and conversation so extensions use durable intent queries instead of prompt scraping or an intent event bus. |
| Task 31 defer/resume APIs | PLANNED | Add APIs for suggesting, accepting, deferring, marking resumable, and resuming intents. User/host policy may accept/resume; extensions may suggest/query/nudge but cannot self-accept or move the cursor. |
| Task 32 intent tree cursor enforcement | PLANNED | Replace plural focused-intent behavior with one intent cursor per conversation, depth-first subintent execution, no same-level parallel running work, and no mixing direct gates with subintents on the same intent. |
| Task 33 abandonment gate/scope | PLANNED | Add abandonment gate/scope state transitions and audit checks so user direction changes abandon current intent/current branch/all running work explicitly with evidence, not silent deletion or cursor jumps. |
| Task 34 deferred-intent audit/reporting | PLANNED | Extend audit/reporting to distinguish active blockers, suggested non-commitments, deferred commitments, resumable intents, sibling-blocking deferred branches, and extension-owned suggested/deferred work. |


Primary objective: make `src/com/blockether/vis/internal/proof.clj` the canonical internal namespace for proof-domain shape, specs, lifecycle transitions, evidence derivation, attestations, and audit. Existing focused files must be folded into it and hard-removed; no legacy proof semantics, delegates, facades, shims, or old proof/provenance internals kept alive in the final state.

- `src/com/blockether/vis/internal/provenance_ref.clj`
- `src/com/blockether/vis/internal/provenance_lifecycle.clj`
- `src/com/blockether/vis/internal/intent_spec.clj`
- persistence/runtime intent-plan-gate proof helpers
- public extension aliases such as proof checks, provenance guards, and proof rendering

Grounding observations:

- User directive captured as a planning invariant: FIRST THINK ABOUT THE CLOJURE SPEC before moving runtime code.
- `provenance_ref.clj` already defines one canonical ref grammar and intentionally rejects compact aliases such as `i4.2`, `E1`, and `G1`.
- `provenance_lifecycle.clj` already separates `:running` from terminal statuses and says running work must not be proof of completion.
- `intent_spec.clj` currently validates boundary shapes but explicitly leaves database existence, same-conversation scope, future-event rejection, and proof adequacy to persistence/runtime layers.
- `TASKS.md` and `docs/adr/0002-attestation-ledger.md` both say current proofing is weak because caller-supplied slot payloads can satisfy guards without runtime-derived evidence extraction.
- `PLAN.md` did not exist before this planning pass; this file is the requested documentation-first plan.

## Extension state versus proof evidence

Recent extension persistence work built an extension persistence substrate. That substrate is state: cache, status, checkpoint, and aggregate sidecars owned by extension execution. It can help runtime operate, but it is not proof evidence by itself.

PROOF.md builds a separate immutable proof/evidence substrate:

```text
extension symbol runs
  -> writes extension_aggregate cache/status/checkpoint sidecar
  -> runtime writes provenance_event immutable observation
  -> evidence_bundle derives facts from events
  -> attestation decides gate/plan/intent
  -> audit validates whole chain
```

Boundary rule:

- `extension_aggregate` / extension sidecars are mutable operational state.
- `provenance_event` rows are immutable observations.
- `evidence_bundle` may derive facts from `provenance_event`, not from caller-supplied extension aggregate claims.
- `attestation` may decide from accepted bundles only.
- `audit` validates the chain from observation to bundle to attestation to entity state.
- Extension aggregate may link to proof refs for navigation, but cannot itself prove a gate, plan, or intent.

Same architectural shape, different trust level: extension aggregate is state; PROOF.md ledger is evidence.

Boundary-contract gaps to keep distinct:

| Missing thing | Similar to PROOF.md how? | Same thing? | PROOF rollout handling |
|---|---|---|---|
| `iteration_block` table | Needed for first-class event/block identity instead of `iteration.blocks` BLOB projection. | Almost; likely part of provenance ledger work. | Treat as part of ledger identity design when adding `provenance_event`; do not claim proof-grade block identity until immutable rows/refs exist. |
| Secret storage/encryption | Same "do not abuse generic blobs" discipline. | No; confidentiality layer, not attestation ledger. | Keep secrets out of extension aggregates and proof payloads; do not block P1/P3 ledger work on a secret store unless touched. |
| Proof-ledger semantics | Direct PROOF.md core. | Yes. | Extension aggregate writes like `{:kind :proof/gate-passed}` must not prove anything. Runtime must append observation -> bundle -> attestation. |
| `:agent/end` lifecycle | Terminal lifecycle boundary for notifications, final-answer permission, audit finalization, and extension idle hooks. | Related. | Model as a future first-class terminal lifecycle/provenance event; running/partial phases still cannot prove completion. |
| Declarative `:ext.symbol/aggregate` sugar | Declarative side-effect contract resembles guard/bundle declarations. | No; convenience only. | Keep separate from trust-critical proof guards/attestations. Do not mix aggregate sugar with proof semantics. |

Concrete examples:

```clojure
;; Sidecar state: allowed, not proof.
{:kind :checkpoint/ref
 :content {:git-ref "..."}}

;; Fake proof sidecar: may be displayed/debugged, but proves nothing.
{:kind :proof/gate-passed
 :content {:gate-id "G1"}}
```

Proof-grade equivalent later:

```text
command/runtime observation completed
  -> provenance_event with canonical ref, terminal status, op, digest/summary
  -> evidence_bundle derives git-ref/result from event
  -> attestation accepts bundle for gate G1
  -> audit validates event -> bundle -> attestation -> gate
```

Vocabulary to use from here onward:

| Layer | Primary noun | Meaning |
|---|---|---|
| Extension state | extension aggregate / sidecar | Mutable operational cache/status/checkpoint; useful runtime state, not proof evidence. |
| Observation | event / ledger | Immutable runtime observation with canonical ref and lifecycle status. |
| Evidence | bundle / derived binding | Runtime-derived facts extracted from ledger events. |
| Decision | attestation / guard / policy | Structured decision over derived evidence. |
| Audit | audit / violation / report | Whole-system validation across ledger, bundles, attestations, gates, plans, and intents. |

Removal vocabulary:

- Hard-remove legacy internal proof/provenance paths. Temporary files are allowed only while a migration slice is actively moving callers; they are not an accepted end state and Task 3 cannot be DONE while they exist.
- Remove old proof-facing names from implementation surfaces as callers move; implementation language is `attestation`, `audit`, `ledger`, and `bundle`.
- Keep `guard` only for local boolean requirements.
- Compact refs may remain display labels only; persisted proof-facing refs must be canonical.

## Task 1 — DONE — Update documentation before source changes

Work:

- Add or refine docs that describe the target `proof.clj` model before changing runtime code.
- Document the exact flow: runtime observation -> event ledger -> evidence bundle -> attestation -> audit.
- Document why gate proof, plan completion, and intent fulfillment remain distinct lifecycle boundaries.
- Document hard-removal boundaries: old internal paths are deleted after callers move; no delegate/shim/facade survives the migration.

Rationale:

- The ADR and TASKS already define the vocabulary and lifecycle model; implementation should follow that language instead of inventing new terms.
- Documentation first prevents moving Clojure specs into `proof.clj` before the model is named.
- Docs include task-level rationale and migration order.

Acceptance criteria:

- `PLAN.md` starts from documentation tasks before runtime code tasks.
- Every task has a rationale.
- The plan explicitly says legacy internals are hard-removed after migration, not preserved as semantic shims.

## Task 2 — DONE FIRST SLICE — Design the Clojure spec model first

Work:

- Create the target spec contract for `com.blockether.vis.internal.proof` before moving behavior.
- Define specs for canonical references, lifecycle events, evidence requirements, evidence bundles, derived bindings, guard expressions, attestations, audit results, gate resolution, plan resolution, and intent resolution.
- Keep specs as boundary validators; do not hide runtime checks inside specs.
- Preserve the existing separation from `intent_spec.clj`: specs validate shape, while persistence/runtime validate existence, same-conversation scope, terminal status, and adequacy.

Initial target shape sketch:

    {:event/ref "turn/6ce90875/iteration/1/block/2"
     :event/status :done
     :event/op :sci/eval
     :event/rendering-kind :vis/sci}

    {:evidence/slot [intent-id :verification]
     :evidence/from-ref "turn/6ce90875/iteration/1/block/2"
     :evidence/extract [:result :exit]
     :evidence/guard [:= 0]}

    {:attestation/kind :gate/proven
     :attestation/subject-id gate-id
     :attestation/evidence-bundle-id bundle-id
     :attestation/decision :proven}

Rationale:

- The user specifically called out the Clojure spec concern.
- Current `intent_spec.clj` admits loose proof shapes around `:refs`, `:slots`, and `:guard`.
- A spec-first pass makes invalid shapes impossible at API boundaries before persistence semantics are tightened.
- Specs should describe data; runtime still owns canonical ref allocation, event lookup, terminal-status checks, and derived evidence extraction.

Acceptance criteria:

- `proof.clj` has public specs or predicates for all proof-domain data shapes.
- Old proof-domain specs move to `proof.clj`; old proof/provenance spec owners are deleted after callers move.
- Tests cover valid and invalid shapes, including fake compact refs and running events.

## Task 3 — DONE — Make `proof.clj` the canonical internal proof-domain namespace

Work:

- Create `src/com/blockether/vis/internal/proof.clj`.
- Move pure proof-domain specs and helpers into it.
- Fold canonical ref parsing/formatting from `provenance_ref.clj` into the proof-domain boundary, update all call sites, then delete the old namespace.
- Fold lifecycle proof semantics from `provenance_lifecycle.clj` into the same domain model, update all call sites, then delete the old namespace.
- Move proof-related specs out of `intent_spec.clj` so intent specs reference proof-domain specs instead of owning them.
- Do not keep legacy internal shims for these namespaces; update call sites and tests instead.

Rationale:

- The user explicitly chose hard removal over legacy compatibility.
- Proof-domain specs, ref grammar, lifecycle statuses, evidence extraction, attestations, and audit are one trust boundary.
- Keeping old internal namespaces as shims would keep stale proof/provenance language alive and make the model harder to audit.

Acceptance criteria:

- New `proof.clj` owns canonical proof-domain specs and helpers.
- Internal callers no longer require `provenance_ref.clj` or `provenance_lifecycle.clj`.
- Old internal proof/provenance namespaces are deleted after caller migration.
- Tests cover the public functions that move into `proof.clj`.

## Task 4 — Preserve canonical ref discipline while consolidating

Work:

- Keep the existing canonical grammar:
  - optional `conversation/<8hex>/` prefix;
  - required `turn/<8hex>/iteration/<positive>/block/<positive>`;
  - optional child `/tool/<id>` or `/error`.
- Reject compact refs in proof-facing writes.
- Keep compact refs display-only if channels still need them.
- Ensure `format-ref` remains the single canonical formatter.

Rationale:

- `provenance_ref.clj` already states canonical refs are durable evidence addresses.
- ADR/TASKS say fallback compact refs must be removed from proof paths.
- Proof trust depends on refs resolving to immutable runtime events, not UI labels.

Acceptance criteria:

- Tests prove canonical refs round-trip through parse/format.
- Tests prove compact refs are rejected.
- All proof write paths accept only canonical refs.

## Task 5 — Preserve lifecycle truth: running is not proof

Work:

- Move lifecycle sets and predicates into `proof.clj`:
  - terminal statuses;
  - successful statuses;
  - blocker statuses;
  - lifecycle statuses.
- Ensure proof and fulfillment writers reject `:running` refs as completion evidence.
- Make deferred/future proof rules explicit in audit failures.

Rationale:

- `provenance_lifecycle.clj` already says a block may prove a future was created, but only terminal child events prove completion.
- This rule belongs in the proof domain because gates and intents consume evidence, not process starts.

Acceptance criteria:

- Running events fail proof adequacy checks.
- Terminal `:done` events may satisfy success guards.
- Terminal blocker events may support impediment/abandonment attestations.

## Task 5A — Add guard/gate test harness before storage writes

Work:

- Implement pure guard evaluation over derived binding maps only.
- Add fixtures for event payloads, extraction paths, derived bindings, and guard outcomes.
- Add a gate proof harness that takes `{event fixtures -> evidence requirement -> derived binding -> attestation decision}` without SQLite.
- Add adversarial cases:
  - caller supplies fake slot value;
  - compact ref is used;
  - running event is used;
  - event kind/op mismatches requirement;
  - extract path is missing;
  - guard returns false.

Rationale:

- `ANALYSIS.md` shows ceremony without a tight pass/fail seam wastes iterations.
- Guard/gate behavior must be testable before persistence makes failures harder to localize.
- Pure tests give fast red/green proof that fake evidence cannot pass.

Acceptance criteria:

- `test/com/blockether/vis/internal/proof_test.clj` covers guard shape and pure guard evaluation.
- New gate harness test proves fake caller slots cannot satisfy a gate.
- Target command is documented in this task when complete.
- Fresh JVM test passes before any SQLite storage work begins.

## Task 6 — DONE — Add first-class event ledger storage

Work:

- Add first-class provenance event persistence to SQLite schema.
- Store immutable events for eval, tool, error, answer, system, diagnostic, and lifecycle events.
- Preserve current transcript rendering, but stop making proof-critical checks depend on reconstructed transcript projections.
- Use HoneySQL for all app queries; raw SQL only where migration DDL rules permit.

Rationale:

- ADR/TASKS say current proof checks reconstruct provenance instead of reading an append-only event ledger.
- A proof ref is only trustworthy if it resolves to a stored immutable event.
- This task is the storage foundation for evidence bundles and attestations.

Acceptance criteria:

- Every proof-visible ref resolves through the ledger.
- Ledger checks can detect missing refs, duplicate refs, non-terminal proof refs, and malformed child refs.
- Existing transcript UI still renders after ledger persistence is introduced.

## Task 7 — DONE — Add evidence bundle and derived binding storage

Work:

- Add tables/entities for evidence bundles and bundle members.
- Represent each member as runtime-derived evidence:
  - slot;
  - source ref;
  - extraction path;
  - extracted value or failure;
  - guard result.
- Make proof writers point to extraction requests, not hand-written slot payloads.

Rationale:

- The core weakness is caller-injected slot payloads.
- Guards must evaluate facts extracted from event payloads, not claims supplied by the assistant.
- Evidence bundle storage makes the derivation auditable later.

Acceptance criteria:

- Fake slot values cannot satisfy a gate.
- Missing extraction paths produce audit violations.
- Guard failures produce structured evidence failures, not vague proof errors.

## Task 8 — DONE — Add attestation storage and writers

Work:

- Add first-class attestation records for:
  - gate proven;
  - gate impeded;
  - plan completed;
  - plan blocked/superseded/abandoned;
  - intent fulfilled;
  - intent abandoned.
- Link each attestation to an evidence bundle and subject entity.
- Hard-remove old proof blobs as write sources; any public read projection must be derived from attestations and deleted with the caller migration.

Rationale:

- ADR/TASKS rename `proof` to `attestation` because this is a structured decision over evidence.
- Gate, plan, and intent decisions are separate lifecycle claims and should each have explicit records.
- Old proof blobs are the legacy shape; keeping them as write sources preserves the weak model the refactor exists to remove.

Acceptance criteria:

- Gate proof writes an attestation.
- Intent fulfillment writes an attestation.
- Audit can list which attestation supports each terminal state.
- No persistence write path treats old proof blobs as authoritative evidence.

## Task 9 — DONE — Make plan completion a real transition

Work:

- Add a runtime function that derives active plan status from required gate states.
- Mark plan `:completed` when all required gates are proven.
- Mark plan blocked/abandoned when a required gate is impeded and no replanning supersedes it.
- Keep user-facing intent fulfillment separate from plan completion.

Rationale:

- Existing docs say `conversation_intent_plan.status` supports `:completed`, but persistence does not actually mark plans completed.
- Plans aggregate many gates; a proven gate alone is not a completed plan.
- A completed plan is still not the same thing as a fulfilled user intent.

Acceptance criteria:

- Tests show one gate proven does not auto-fulfill the intent.
- Tests show all required gates proven completes the active plan.
- Tests show impeded required gates block final answer unless replanned or abandoned.

## Task 10 — DONE — Tighten intent fulfillment and abandonment

Work:

- Require intent fulfillment to cite completed-plan and/or terminal answer evidence.
- Require abandonment to cite blocker evidence or an impeded plan.
- Ensure focused active intents still block final answers.
- Make fulfillment refs evidence-backed through ledger/bundle/attestation, not only ref presence.

Rationale:

- Intent fulfillment is the user-facing commitment boundary.
- Current code already requires refs and gates, but does not require a first-class plan completion attestation.
- Stronger semantics make final answers defensible.

Acceptance criteria:

- Fulfillment without evidence fails.
- Fulfillment with open required gates fails.
- Fulfillment with fake/non-canonical/running refs fails.

## Task 10A / 13 — DONE — Add system intents and extension proof hooks

Work:

- Extend intent shape with source/owner fields:
  - `:intent/source` one of `:user`, `:system`, `:extension`;
  - `:intent/owner-extension-id` optional for extension-owned intents;
  - `:intent/system-kind` optional stable keyword for host/system intents.
- Define hook events extensions can subscribe to:
  - `:proof/event-appended`;
  - `:proof/evidence-bundle-created`;
  - `:proof/attestation-accepted`;
  - `:proof/audit-violation`;
  - `:proof/final-answer-blocked`.
- Ensure system intents can create gates and receive attestations without scraping prompt prose.
- Add tests with a fake extension hook observing proof lifecycle events.

Rationale:

- Extensions need structured proof lifecycle hooks, not markdown/prose parsing.
- System constraints such as final-answer blocking, repro-required, and provider-selection checks are intents too.
- This connects proof semantics to Vis extensibility.

Acceptance criteria:

- Specs cover system and extension intent source/owner fields.
- Fake extension test observes hook events in order.
- Audit can distinguish user-facing unresolved intents from system/extension guardrails.

## Task 11 — DONE — Replace proof checks with audit as the primary validation surface

Work:

- Implement primary audit functions over ledger, bundles, attestations, gate states, plan states, and intent states.
- Remove old proof-check/provenance-guard names from internal implementation and documentation:
  - `provenance guards` -> `ledger checks`;
  - `proof checks` -> `audit`;
  - `proof rendering` -> `audit/attestation rendering`.
- During migration only, keep unavoidable public aliases at the outer boundary. Final state removes stale internal names and routes user-facing reads through audit/attestation names.

Rationale:

- Whole-system validation is broader than proof checks.
- TASKS and ADR call this layer audit.
- Removal-first migration prevents stale proof terminology from surviving in the new model.

Acceptance criteria:

- Primary internal API and docs use audit names.
- Audit reports unresolved refs, running refs, missing bundles, failed guards, state mismatch, and stale proof-blob dependencies.
- Tests assert old internal proof-check/provenance-guard names are not reintroduced.

## Task 12 — DONE — Update rendering after the data model changes

Work:

- Render event ledger, evidence bundles, attestations, gates, plans, and intents from one report surface.
- Render audit failures as precise violations.
- Render compact labels only as display aliases beside canonical refs.
- Update TUI/report paths that currently refer to proofs.

Rationale:

- TASKS says proof rendering should become audit/attestation rendering.
- Users need to see why a gate, plan, or intent is resolved or blocked.
- Rendering should not re-invent proof semantics; it should consume audit data.

Acceptance criteria:

- Reports show event -> bundle -> attestation -> entity resolution.
- Missing or invalid evidence appears as audit violations.
- Regression tests cover collapsed/expanded audit rendering.

## Task 13 — Migrate persistence code carefully

Work:

- Update SQLite schema for new ledger/bundle/attestation tables directly in `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`.
- Do not add `V2__...sql`, `V3__...sql`, or any separate migration file for this work unless the user explicitly asks for real upgrade migrations.
- For local/autoresearch development, prefer a dedicated disposable DB path under `target/proof-autoresearch/` via `VIS_DB_PATH` or `--db`; delete/recreate that DB freely between schema iterations.
- If the default `~/.vis/vis.mdb` or another persistent Vis DB must be removed after editing V1, first close all Vis processes that may have it open, including TUI/CLI child JVMs. Then delete only after confirming no process owns it.
- Identify all persistence functions that read/write proof blobs, refs, gates, plans, intents, and transcript provenance.
- Convert write paths to create ledger events, bundles, and attestations.
- Hard-remove old proof blob read/write paths after attestations are wired; update callers instead of preserving old semantics.
- Use SQLite WAL and existing retry boundaries; do not add process-exclusive locks.

Rationale:

- AGENTS.md explicitly says SQLite schema changes are inline in `V1__schema.sql` until told otherwise.
- User explicitly allows removing/recreating local dev DBs during this inline schema phase; multiprocess safety still requires closing all processes before deleting any DB they may have open.
- Autoresearch should use disposable per-run DBs so schema experiments never risk user data.
- Persistence currently stores iteration blocks and intent/gate proof blobs but lacks first-class event/bundle/attestation tables.
- Multiprocess SQLite rules are explicit in AGENTS.md and must not regress.
- Focused regression tests lower migration risk without preserving legacy proof semantics.

Acceptance criteria:

- Existing tests pass after hard-removal changes.
- New persistence tests prove first-class records are written.
- No forbidden raw SQL or `next.jdbc.sql` helper usage is introduced.
- No new migration file is added for this work.
- Schema tests run against a disposable DB created from the edited V1 schema.
- Autoresearch/runtime benchmark DBs are safe to delete because they live under `target/proof-autoresearch/`, not user-owned default state.

## Task 14 — Add regression tests for every changed namespace

Work:

- Create or update tests for each changed Clojure namespace.
- Required mappings include:
  - `src/com/blockether/vis/internal/proof.clj` -> `test/com/blockether/vis/internal/proof_test.clj`;
  - deleted namespaces have no tests; any remaining changed namespace gets its matching test;
  - persistence namespaces touched by gate/plan/intent writes;
  - extension/reporting namespaces touched by audit aliases.
- Add integration tests for real lifecycle flow.

Rationale:

- AGENTS.md requires every Clojure source namespace to have a matching real test.
- This refactor changes trust boundaries; pure unit tests alone are not enough.
- Regression tests prevent reintroducing fake slot-payload proof.
- Deleting legacy namespaces is valid only when callers and tests prove the replacement path.

Acceptance criteria:

- New namespace has a non-empty public API smoke/regression test.
- Deleted legacy namespaces have no remaining callers.
- Gate proof cannot be faked by injected slot values.
- Plan completion and intent fulfillment are tested separately.

## Task 15 — Stage the migration in small commits

Work:

- Recommended implementation order:
  1. Docs and PLAN.md.
  2. `proof.clj` specs and tests only.
  3. Move canonical ref and lifecycle helpers into `proof.clj`, then delete old namespaces.
  4. Add ledger persistence and ledger checks.
  5. Add evidence bundle derivation.
  6. Add attestation writes.
  7. Add plan completion transition.
  8. Tighten intent closure.
  9. Rename/alias APIs.
  10. Update rendering.
  11. Remove deprecated internal callers.
- Run targeted tests after each code stage.
- Run `./verify.sh --quick` during code edits and full `./verify.sh` before commit-ready handoff.

Rationale:

- This is a deep semantic change crossing docs, specs, persistence, runtime, and UI.
- Small stages keep regressions diagnosable.
- Docs-only planning does not require `verify.sh`; code stages do.

Acceptance criteria:

- Each stage has a clear verification command.
- The repository never sits with source namespace changes lacking tests.
- Deprecated names are removed only after all callers have migrated.

## Task 15A — Hard-remove legacy proof/provenance bullshit

Work:

- Delete old proof/provenance files after all callers migrate:
  - `src/com/blockether/vis/internal/provenance_ref.clj`;
  - `src/com/blockether/vis/internal/provenance_lifecycle.clj`.
- Remove internal uses of legacy names:
  - `provenance guards` as semantic API;
  - `proof checks` as semantic API;
  - proof blobs as authoritative write/read source;
  - compact refs in proof-facing writes.
- Keep public aliases only if explicitly documented as deprecated and implemented at the outermost boundary.
- Add a legacy-purge regression command:

  ```bash
  rg -n "provenance[-_]guards|proof[-_]checks|proof blob|conversation_intent_gate_ref|provenance_ref|provenance_lifecycle" src extensions test docs
  ```

  Every match must be either a migration note, a deprecated public alias test, or removed.

Rationale:

- Temporary files are migration debt. Leaving them alive recreates the old ambiguous model.
- User explicitly requested removal of legacy bullshit.

Acceptance criteria:

- No internal caller requires deleted legacy namespaces.
- No persistence write path treats old proof blobs or gate ref rows as authoritative.
- Legacy search command has only approved compatibility/documentation hits.
- Full `./verify.sh` passes after deletion.

## Task 16 — Final audit before declaring the work done

Work:

- Run full repository verification.
- Inspect `v/audit` behavior on at least one real focused-intent workflow and confirm stale `v/proof-checks` is gone or only an explicitly deprecated outer-boundary alias during migration.
- Confirm final answer blocking still requires intent resolution.
- Confirm audit report can cite canonical refs and explain blockers.

Rationale:

- The point of this work is stronger trust semantics, not just renamed files.
- Final verification must exercise the real lifecycle from observation to intent closure.

Acceptance criteria:

- Full `./verify.sh` passes.
- Audit can trace event -> bundle -> attestation -> gate/plan/intent.
- No proof-facing code accepts compact refs, running futures, or caller-injected slot facts as proof.

## Resolved migration decisions for the first implementation slice

1. `provenance_ref.clj` and `provenance_lifecycle.clj` must be deleted.
   - Why: semantic ownership belongs only in `proof.clj`. Task 3 is not DONE until all callers require `proof.clj` directly and both old files/tests are removed.

2. `intent_spec.clj` stays for intent entity/writer specs and imports proof-domain truth from `proof.clj`.
   - Why: intent shape and proof evidence shape are related but not identical seams. Delete/fold only if remaining intent specs become pass-through.

3. Plan DB status stays `:abandoned` for now; audit language may render blocked semantics.
   - Why: schema-visible status rename is a later persistence migration decision.

4. Old public API names are migration debt, not target API.
   - Why: if an outer-boundary alias is needed briefly to avoid breaking a running session, it must be marked deprecated with a removal task. Internal proof/provenance implementation language must disappear.

5. Evidence bundles should store derivation recipe plus extracted value/result shape and digest.
   - Why: audit must be human-readable and recomputable enough to detect drift without duplicating giant payloads.
