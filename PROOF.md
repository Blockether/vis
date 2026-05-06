# PLAN.md — Documentation-first proof.clj consolidation plan

Status: planned.

Scope: attestation ledger, provenance references, lifecycle events, intent/plan/gate proof semantics, and audit surface.

Primary objective: make `src/com/blockether/vis/internal/proof.clj` the canonical internal namespace for proof-domain shape, specs, lifecycle transitions, evidence derivation, attestations, and audit. Existing focused files must be folded into it and then hard-removed; no legacy shims, no old proof/provenance internals kept alive.

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

Vocabulary to use from here onward:

| Layer | Primary noun | Meaning |
|---|---|---|
| Observation | event / ledger | Immutable runtime observation with canonical ref and lifecycle status. |
| Evidence | bundle / derived binding | Runtime-derived facts extracted from ledger events. |
| Decision | attestation / guard / policy | Structured decision over derived evidence. |
| Audit | audit / violation / report | Whole-system validation across ledger, bundles, attestations, gates, plans, and intents. |

Removal vocabulary:

- Hard-remove legacy internal proof/provenance paths; do not preserve old internals for nostalgia, theoretical migration comfort, or alias-first churn.
- Remove old proof-facing names from implementation surfaces as callers move; implementation language is `attestation`, `audit`, `ledger`, and `bundle`.
- Keep `guard` only for local boolean requirements.
- Compact refs may remain display labels only; persisted proof-facing refs must be canonical.

## Task 1 — Update documentation before source changes

Work:

- Add or refine docs that describe the target `proof.clj` model before changing runtime code.
- Document the exact flow: runtime observation -> event ledger -> evidence bundle -> attestation -> audit.
- Document why gate proof, plan completion, and intent fulfillment remain distinct lifecycle boundaries.
- Document hard-removal boundaries: old internal paths are deleted after callers move; no legacy shim plan.

Rationale:

- The ADR and TASKS already define the vocabulary and lifecycle model; implementation should follow that language instead of inventing new terms.
- Documentation first prevents moving Clojure specs into `proof.clj` before the model is named.
- Docs include task-level rationale and migration order.

Acceptance criteria:

- `PLAN.md` starts from documentation tasks before runtime code tasks.
- Every task has a rationale.
- The plan explicitly says legacy internals are hard-removed, not preserved as shims.

## Task 2 — Design the Clojure spec model first

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
- Old specs either delegate to `proof.clj` or are removed after callers move.
- Tests cover valid and invalid shapes, including fake compact refs and running events.

## Task 3 — Make `proof.clj` the canonical internal proof-domain namespace

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

- Move or delegate lifecycle sets and predicates into `proof.clj`:
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

## Task 6 — Add first-class event ledger storage

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

## Task 7 — Add evidence bundle and derived binding storage

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

## Task 8 — Add attestation storage and writers

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

## Task 9 — Make plan completion a real transition

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

## Task 10 — Tighten intent fulfillment and abandonment

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

## Task 11 — Replace proof checks with audit as the primary validation surface

Work:

- Implement primary audit functions over ledger, bundles, attestations, gate states, plan states, and intent states.
- Remove old proof-check/provenance-guard names from internal implementation and documentation:
  - `provenance guards` -> `ledger checks`;
  - `proof checks` -> `audit`;
  - `proof rendering` -> `audit/attestation rendering`.
- Keep any unavoidable public API aliases only at the boundary and make them delegate to the new audit functions.

Rationale:

- Whole-system validation is broader than proof checks.
- TASKS and ADR call this layer audit.
- Removal-first migration prevents stale proof terminology from surviving in the new model.

Acceptance criteria:

- Primary internal API and docs use audit names.
- Audit reports unresolved refs, running refs, missing bundles, failed guards, state mismatch, and stale proof-blob dependencies.
- Tests assert old internal proof-check/provenance-guard names are not reintroduced.

## Task 12 — Update rendering after the data model changes

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
- Identify all persistence functions that read/write proof blobs, refs, gates, plans, intents, and transcript provenance.
- Convert write paths to create ledger events, bundles, and attestations.
- Hard-remove old proof blob read/write paths after attestations are wired; update callers instead of preserving old semantics.
- Use SQLite WAL and existing retry boundaries; do not add process-exclusive locks.

Rationale:

- AGENTS.md explicitly says SQLite schema changes are inline in `V1__schema.sql` until told otherwise.
- Persistence currently stores iteration blocks and intent/gate proof blobs but lacks first-class event/bundle/attestation tables.
- Multiprocess SQLite rules are explicit in AGENTS.md and must not regress.
- Focused regression tests lower migration risk without preserving legacy proof semantics.

Acceptance criteria:

- Existing tests pass after hard-removal changes.
- New persistence tests prove first-class records are written.
- No forbidden raw SQL or `next.jdbc.sql` helper usage is introduced.
- No new migration file is added for this work.

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
  3. Move/delegate canonical ref and lifecycle helpers.
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

## Task 16 — Final audit before declaring the work done

Work:

- Run full repository verification.
- Inspect `v/audit` / compatibility `v/proof-checks` behavior on at least one real focused-intent workflow.
- Confirm final answer blocking still requires intent resolution.
- Confirm audit report can cite canonical refs and explain blockers.

Rationale:

- The point of this work is stronger trust semantics, not just renamed files.
- Final verification must exercise the real lifecycle from observation to intent closure.

Acceptance criteria:

- Full `./verify.sh` passes.
- Audit can trace event -> bundle -> attestation -> gate/plan/intent.
- No proof-facing code accepts compact refs, running futures, or caller-injected slot facts as proof.

## Open design decisions to resolve before code

1. Should `provenance_ref.clj` and `provenance_lifecycle.clj` be deleted after migration, or kept as tiny compatibility namespaces that delegate to `proof.clj` for one release window?
   - Recommended answer: keep thin delegates first, then delete once internal callers and tests prove no dependency remains.

2. Should `intent_spec.clj` disappear entirely?
   - Recommended answer: not immediately. First move proof-domain specs to `proof.clj`; then leave intent entity specs in `intent_spec.clj` only if they are truly intent-only. If most remaining specs are proof-domain, fold the file fully.

3. Should plan `:abandoned` be renamed to `:blocked`?
   - Recommended answer: defer schema-visible rename. Define `:abandoned` narrowly for now, add audit language `blocked`, and migrate status names only when schema migration policy is ready.

4. Should old public API names be removed now?
   - Recommended answer: no. Add primary names first and keep deprecated aliases until docs, prompts, TUI, and tests are fully migrated.

5. Should evidence bundles store extracted values or only derivation recipe plus hash?
   - Recommended answer: store derivation recipe, result shape/value for audit readability, and enough source event identity to recompute or detect drift. Avoid storing huge payload duplicates.
