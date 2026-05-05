# Provenance, Proof, Intent, Plan, Gate Cleanup

## Why this exists

The current system has useful provenance for local debugging, but weak proof semantics.
It also has lifecycle ambiguity between gates, plans, and intents.

Main problems:

- Proof slots are caller-supplied, not mechanically derived from observed events.
- Guards validate slot payloads, not event facts.
- Provenance timeline is partly reconstructed instead of being a first-class append-only event ledger.
- The system mixes canonical refs and fallback compact refs in different layers.
- Gate resolution and intent resolution are separate, but plan completion is under-modeled.
- `:plan/status` supports `:completed`, but current persistence does not actually mark plans completed.

## Terminology cleanup

Current language is overloaded and harder to remember than it needs to be.

### Current confusing terms

- `provenance guards`
- `guard`
- `proof`
- `proof checks`

These are too close in sound while operating at different layers.

### Recommended vocabulary

| Current term | Target term | Meaning |
|---|---|---|
| `provenance guards` | `ledger checks` | low-level integrity checks on stored provenance events |
| `guard` | `guard` | one local boolean rule inside an evidence requirement |
| `proof` | `attestation` | structured decision over evidence |
| `proof checks` | `audit` | whole-system validation across ledger, bundles, attestations, and resolutions |

### Mental model

- ledger checks = "is the event ledger sane?"
- guard = "does this one condition hold?"
- attestation = "what structured decision was made from evidence?"
- audit = "does the whole story hold together?"

### Naming rule

Use these layers consistently:

1. Observation layer -> `provenance_event`, `ledger`, `ledger checks`
2. Evidence layer -> `evidence_bundle`, `bundle member`, `derived binding`
3. Decision layer -> `attestation`, `guard`, `policy`
4. Audit layer -> `audit`, `violations`, `report`

### API/UI rename direction

Recommended user-facing rename direction:

- `v/provenance-guards` -> `v/ledger-checks`
- keep `:guard` in proof DSL
- `v/proof-checks` -> `v/audit`
- `v/proofs` -> `v/attestations` or `v/audit-report`

Transition rule:

- keep old names temporarily as aliases
- make new names primary in docs, prompts, and UI
- render old names as deprecated terminology

### Internal implementation rule

Do not use `proof` as a generic catch-all anymore.

Use exact nouns:

- `event` for observation
- `bundle` for grouped evidence
- `attestation` for decision object
- `audit` for system-wide validation
- `guard` for a local boolean requirement

## Flow: event -> bundle -> attestation -> audit

These are sequential layers, not competing names.

### One-line flow

```text
runtime observation -> event ledger -> evidence bundle -> attestation -> audit
```

### What each layer does

```text
1. Event / ledger
   "What happened?"

2. Bundle
   "Which observed facts are we using together?"

3. Attestation
   "What decision do we make from that bundle?"

4. Audit
   "Does the whole system still make sense end-to-end?"
```

### Gate proof example

```text
User asks: "Did verification pass?"

Runtime executes bash test command
  -> provenance_event E1
     kind=tool
     op=:v/bash
     status=:done
     payload.result.exit=0

Runtime builds evidence bundle B1
  -> includes E1
  -> extracts [:result :exit] => 0
  -> checks gate guard [:= exit 0]

Runtime writes attestation A1
  -> kind=gate-proof
  -> subject=gate G1
  -> bundle=B1
  -> decision=proven
  -> why="Observed bash exit code 0"

Runtime updates gate G1
  -> status=proven
```

### Plan completion example

```text
Gate G1 proven via attestation A1
Gate G2 proven via attestation A2

Runtime recomputes plan P1
  -> all required gates proven

Runtime builds completion bundle B2
  -> references A1 and A2 or their underlying evidence

Runtime writes attestation A3
  -> kind=plan-completion
  -> subject=plan P1
  -> bundle=B2
  -> decision=completed

Runtime updates plan P1
  -> status=completed
```

### Intent closure example

```text
Plan P1 completed via attestation A3
Assistant produced final user-visible result

Runtime builds closure bundle B3
  -> includes A3
  -> may include final artifact events/files/tests if needed

Assistant explicitly fulfills intent I1

Runtime writes attestation A4
  -> kind=intent-closure
  -> subject=intent I1
  -> bundle=B3
  -> decision=fulfilled
  -> why="Completed plan delivered requested outcome"

Runtime updates intent I1
  -> status=fulfilled
```

### Audit example

```text
Audit reads:
  - event ledger
  - bundles
  - attestations
  - gate states
  - plan states
  - intent states

Audit checks:
  - every referenced event exists
  - every bundle member points to real events
  - every attestation points to a real bundle
  - every proven gate has valid attestation
  - every completed plan is justified by gate outcomes
  - every fulfilled intent is justified by completed plan + closure attestation
```

### Table version

| Layer | Object | Question answered | Example |
|---|---|---|---|
| Observation | `provenance_event` | What happened? | "bash exited 0" |
| Evidence | `evidence_bundle` | Which facts are grouped? | "use this bash event for verification evidence" |
| Decision | `attestation` | What do we conclude? | "gate proven" |
| Validation | `audit` | Does the overall story hold? | "intent fulfillment is justified" |

### Short intuition

```text
event       = fact
bundle      = chosen facts together
attestation = conclusion from those facts
audit       = check that all conclusions are justified
```

## Current behavior

Observed in code:

- Proving a gate does not fulfill the intent.
- Fulfilling an intent is a separate write.
- Fulfillment currently requires:
  - an active plan
  - every required gate on that active plan already proven
  - at least one fulfillment evidence ref
- Active focused intents block final answer until they are resolved.
- Required impeded gates force re-plan or abandon.

Relevant code:

- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj:1485`
- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj:1615`
- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj:1758`
- `src/com/blockether/vis/internal/intent_spec.clj:41`

## Blunt assessment

Current provenance is decent forensic telemetry.
Current proofing is not strong evidence.

Rating:

- Provenance for debugging: 7/10
- Proofing for trust: 3/10
- State-of-the-art attestation quality: 2/10

## Decision: should proving a gate close the intent?

No.

A gate is a proposition about the plan.
An intent is the user-facing commitment/outcome.

Why gate proof must not auto-close the intent:

- A gate says one condition is satisfied, not that the user outcome is delivered.
- A plan can have multiple required gates.
- Even if all gates are proven, the assistant may still need to synthesize the result, write files, or present the answer.
- Intent closure is the semantic boundary where the system says: this user request is now actually resolved.

So:

- `gate proven` means one gate proposition is satisfied.
- `plan completed` means the active plan has reached a terminal successful state.
- `intent fulfilled` means the user-facing goal is resolved.

## Decision: how should plan connect to gates and intent?

Recommended lifecycle:

1. Intent starts `:active`.
2. Exactly one active plan owns current execution strategy.
3. Gates belong to that plan.
4. Each required gate reaches one of: `:proven` or `:impeded`.
5. Plan status is derived from gate states:
   - `:active` if any required gate is still `:open`
   - `:blocked` or existing `:abandoned`/`impeded-plan` equivalent if any required gate is `:impeded`
   - `:completed` if all required gates are `:proven`
6. Intent resolution is separate:
   - `:fulfilled` only after plan is `:completed` and the assistant records fulfillment evidence
   - `:abandoned` when the active plan is blocked and the assistant decides not to re-plan

Important: many gates can satisfy one plan.
Yes, the plan is the aggregate over many gates.

But plan completion should not silently imply intent fulfillment.
It should only permit intent fulfillment.

## Explicit state machine rules

To remove ambiguity, the intended semantics are:

1. Proving one gate changes only that gate directly.
2. Any gate state change may change the aggregate status of its parent plan.
3. A plan is fulfilled by many required gates, not by one gate.
4. A completed plan means "execution strategy succeeded", not "user request resolved".
5. An intent is fulfilled only by an explicit intent-resolution write.
6. A completed plan permits intent fulfillment, but does not perform it automatically.
7. An impeded required gate does not automatically abandon the intent.
8. An impeded required gate means either:
   - re-plan with a new active plan, or
   - explicitly abandon the intent with blocker evidence.
9. Superseding a plan does not resolve the intent; it only changes the active strategy.
10. Final answer is allowed only when focused intents are no longer active.

This gives a clean hierarchy:

- gates resolve propositions
- plans aggregate propositions into execution-state
- intents represent user-facing resolution

## Intent fulfillment evidence: why are refs required again?

Current system behavior requires refs on intent fulfillment.
That is understandable as a safety check, but the current shape is clumsy and partly redundant.

Why it feels wrong:

- gate proof already carries the provenance refs for the plan's required propositions
- plan completion should already summarize that the execution strategy succeeded
- asking the model to manually repeat raw refs at intent fulfillment duplicates evidence already present lower in the stack

So the real problem is not "intent should have no evidence".
The real problem is "intent is currently forced to carry evidence in the wrong form".

Recommended rule:

- gate level stores raw provenance refs
- plan level stores proof/impediment structure and derived completion state
- intent level stores a closure attestation, not a second manual bag of duplicated raw refs

Meaning:

- raw event refs belong primarily to gates
- plan completion is derived from gate outcomes
- intent fulfillment should attach a small explicit closure record saying why the user-visible request is solved

## Recommended intent closure shape

Intent fulfillment should record:

1. rationale/summary
2. closure attestation
3. optional direct fulfillment refs only when needed for extra user-facing evidence

Example direction:

```clojure
{:summary "Feature implemented and verified."
 :closure-attestation
 {:plan-id <active-plan-id>
  :basis :plan-completed
  :gate-refs :derived
  :user-visible-artifacts
  [{:kind :file :path "src/foo.clj"}
   {:kind :test :id "foo-test"}]}}
```

Or more explicitly:

```clojure
{:summary "Done."
 :closure-attestation
 {:plan-id <active-plan-id>
  :completed-gates [gate-a gate-b]
  :evidence-bundle-id <derived-bundle-id>
  :why "All required gates on the active plan were proven, and the resulting artifact was produced."}}
```

Core idea:

- the intent should attest to closure
- the plan/gates should carry the detailed proof graph

## New rule for intent fulfillment

Recommended semantics:

1. `fulfill-intent!` should require a completed active plan.
2. `fulfill-intent!` should require a non-blank rationale/summary.
3. `fulfill-intent!` should record a closure attestation referencing the completed plan.
4. `fulfill-intent!` should not require the caller to manually restate every gate ref.
5. Optional direct refs may still be allowed for user-facing completion evidence not already captured by gate proof.

Examples of optional direct fulfillment refs:

- a final generated file artifact
- a final rendered answer artifact
- a final publish/deploy event

So the intent layer becomes:

- not raw proof duplication
- not evidence-free hand-waving
- but explicit closure over an already-proven plan

## Recommended state model

### Intent

- `:active`
- `:fulfilled`
- `:abandoned`

Intent remains explicit and user-facing.

### Plan

Current statuses are `:active`, `:completed`, `:superseded`, `:abandoned`.

Recommendation:

- keep `:active`
- keep `:completed`
- keep `:superseded`
- rename `:abandoned` to `:blocked` if schema change is acceptable

If renaming is too expensive right now, keep `:abandoned` but define it narrowly:

- `:abandoned` plan = current strategy cannot succeed and is no longer the active strategy

### Gate

- `:open`
- `:proven`
- `:impeded`

This part is okay.

## Product rule

The final answer gate should check intent resolution, not just plan/gate status.

Meaning:

- all focused intents must end as `:fulfilled` or `:abandoned`
- if an intent is still `:active`, final answer is blocked even if one or more gates are proven

This is already roughly how the system behaves, and that is the right direction.

## Stronger proof model

### Phase 1: make provenance canonical and first-class

Goals:

- persist provenance events as append-only records
- stop reconstructing proof-critical events from transcript-only projections
- remove fallback compact refs from proof paths
- use one canonical ref allocator everywhere

Work:

1. Add a first-class provenance event table/store.
2. Persist eval, tool, error, and lifecycle events directly.
3. Require proof checks to read from stored provenance events, not reconstructed timeline fallback.
4. Remove compact fallback refs like `i1.1` from proof-facing code.
5. Keep compact refs display-only if desired.

Success criteria:

- every proof-visible ref is canonical
- every proof-visible ref resolves to an immutable stored event
- provenance guards no longer operate on reconstructed-only events

### Phase 2: bind proof slots to evidence extraction

Current weak shape:

```clojure
{:refs [ref]
 :slots {slot {:ref ref :exit-code 0}}}
```

Problem: `:exit-code 0` is user-supplied claim, not verified fact.

Target shape:

```clojure
{:evidence
 [{:slot verification-exit
   :from-ref ref
   :extract [:result :exit]
   :guard [:= 0]}]}
```

Rules:

- slot values are derived by runtime from event payloads
- proof writer points at evidence source, not hand-written values
- checks fail if extract path is missing, wrong type, or guard fails

Success criteria:

- guards evaluate derived facts, not caller-injected payloads
- fake slot values cannot satisfy proof

### Phase 3: strengthen proof DSL

Keep the DSL small, but make it less squishy.

Recommended direction:

- keep `v/proof-slot` as stable slot identity
- deprecate generic free-form `:guard` over arbitrary slot payload maps
- introduce evidence clauses with explicit source, extractor, guard, and expected event kind/op

Example:

```clojure
{:expected-proof
 {:requires
  [{:slot verification-exit
    :event-kind :tool
    :op :v/bash
    :extract [:result :exit]
    :guard [:= 0]}]}}
```

Optional follow-up:

- add a tiny compiler from declarative `:requires` clauses into internal checks
- keep vector guards internally if convenient, but do not expose weak selectors as the main user model

### Phase 4: separate observation, claim, decision

Introduce three layers:

1. Observation
   - immutable event from runtime
2. Claim
   - normalized statement derived from observation(s)
3. Decision
   - gate/plan/intent state transition justified by claims and policy

Why:

- easier audits
- clearer causality
- less model self-certification

### Phase 5: add stronger invariants

Add checks for:

- event parent exists and precedes child in time
- proof refs belong to same conversation scope unless explicitly cross-linked
- proof refs match expected event kind/op
- required extract paths exist
- derived slot values match guard
- same ref cannot satisfy incompatible roles without explicit support
- fulfilled intent must reference either:
  - explicit completion evidence, or
  - completed active plan plus fulfillment evidence ref

### Phase 6: fix lifecycle semantics in storage

Implement plan status updates.

Rules:

1. `db-prove-gate!` / `db-impede-gate!` must re-evaluate parent plan status.
2. If all required gates are proven, mark plan `:completed`.
3. If any required gate is impeded, keep plan active only if re-planning is still expected; otherwise mark it blocked/abandoned.
4. `db-fulfill-intent!` should require completed active plan, not just "all required gates proven".
5. `db-abandon-intent!` should require blocked/abandoned active plan or explicit blocker evidence.

## Recommended API semantics

### Keep explicit intent closure

Do not auto-fulfill intent inside `prove-gate!`.

Instead:

- `prove-gate!` resolves one gate
- plan status updates automatically
- `fulfill-intent!` resolves the user commitment explicitly

### Optional convenience helper

Add one higher-level helper later:

```clojure
(v/complete-intent! intent-id {:summary "Done." :refs [...]})
```

Behavior:

- verifies active plan is `:completed`
- verifies fulfillment refs
- marks intent `:fulfilled`

This preserves semantic clarity while reducing boilerplate.

## Schema changes: yes, they are needed

If we want real provenance, evidence bundles, and closure attestations, schema changes are required.

Small code-only changes are not enough because the current schema has these hard limits:

- provenance events are not first-class immutable rows
- gate proof stores ad hoc proof blobs plus duplicated `gate_ref` rows
- plan completion is modeled in status enum but not persisted as a real transition surface
- intent fulfillment stores loose refs, not a structured closure attestation
- there is no first-class bundle object for grouping evidence
- there is no first-class attestation object for gate proof, plan completion, or intent closure

For this repo's current migration rule, implementation should edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql` inline when the actual work starts.

## Current vs target model

### Current model

```text
iteration.blocks / vars / transcript reconstruction
  -> provenance-like refs
  -> gate proof blobs + gate_ref rows
  -> intent refs repeated at fulfillment time
```

### Target model

```text
provenance_event (immutable observations)
  -> evidence_bundle (selected observations + derived bindings)
    -> attestation (machine decision over bundle)
      -> gate resolution
      -> plan completion
      -> intent closure
```

## Target entity diagram

```text
conversation_soul
  |
  +-- conversation_intent
       |
       +-- conversation_intent_plan
       |    |
       |    +-- conversation_intent_gate
       |    |    |
       |    |    +-- attestation(kind=gate-proof | gate-impediment)
       |    |         |
       |    |         +-- evidence_bundle
       |    |              |
       |    |              +-- evidence_bundle_member -> provenance_event
       |    |
       |    +-- attestation(kind=plan-completion | plan-blocked)
       |         |
       |         +-- evidence_bundle
       |
       +-- attestation(kind=intent-closure | intent-abandonment)
            |
            +-- evidence_bundle

iteration / blocks / tool results
  -> provenance_event rows
```

## Lifecycle diagram

```text
issue-intent!
  -> intent(active)
  -> issue-plan!
       -> plan(active)
       -> issue-gate! ... issue-gate!

runtime executes code/tools
  -> provenance_event rows appended

offer-proof! (optional)
  -> candidate evidence hints only

prove-gate!
  -> build evidence_bundle from observed provenance_event rows
  -> derive slot bindings
  -> create attestation(kind=gate-proof)
  -> gate(status=proven)
  -> recompute parent plan

if all required gates proven
  -> create attestation(kind=plan-completion)
  -> plan(status=completed)

fulfill-intent!
  -> create attestation(kind=intent-closure)
  -> intent(status=fulfilled)
```

## Proposed schema: new tables

### 1. `provenance_event`

Purpose:

- immutable observation log for proof-critical events
- canonical source of truth for refs
- replaces proof dependence on transcript reconstruction

Suggested columns:

| Column | Type | Notes |
|---|---|---|
| `id` | `TEXT PK` | UUID |
| `conversation_soul_id` | `TEXT NOT NULL` | FK |
| `conversation_state_id` | `TEXT` | FK |
| `conversation_turn_soul_id` | `TEXT` | FK |
| `conversation_turn_state_id` | `TEXT` | FK |
| `iteration_id` | `TEXT` | FK |
| `canonical_ref` | `TEXT NOT NULL UNIQUE` | canonical machine ref |
| `parent_ref` | `TEXT` | canonical parent ref |
| `kind` | `TEXT NOT NULL` | `eval`, `tool`, `error`, `diagnostic`, `system` |
| `op` | `TEXT NOT NULL` | normalized op id |
| `status` | `TEXT NOT NULL` | `running`, `done`, `error`, `interrupted`, `timeout`, `cancelled` |
| `rendering_kind` | `TEXT` | existing rendering kind vocabulary |
| `form_position` | `INTEGER` | for eval/tool rows |
| `started_at` | `INTEGER` | ms epoch |
| `finished_at` | `INTEGER` | ms epoch |
| `duration_ms` | `INTEGER` | derived/persisted |
| `payload_format` | `TEXT NOT NULL` | `nippy` or `json` |
| `payload` | `BLOB NOT NULL` | full event payload |
| `payload_sha256` | `TEXT NOT NULL` | stable digest |
| `metadata` | `TEXT` or `BLOB` | optional side data |
| `created_at` | `INTEGER NOT NULL` | row timestamp |

Important constraints:

- `canonical_ref` unique
- parent must either be null or reference an existing `canonical_ref`
- no updates after insert except perhaps metadata-free backfill during migration tooling
- proof checks must only consume terminal rows when role requires terminal evidence

### 2. `evidence_bundle`

Purpose:

- immutable grouping of observations and derived bindings
- reusable unit for gate proof, plan completion, and intent closure

Suggested columns:

| Column | Type | Notes |
|---|---|---|
| `id` | `TEXT PK` | UUID |
| `conversation_soul_id` | `TEXT NOT NULL` | FK |
| `kind` | `TEXT NOT NULL` | `candidate`, `proof`, `impediment`, `completion`, `closure` |
| `subject_kind` | `TEXT NOT NULL` | `gate`, `plan`, `intent` |
| `subject_id` | `TEXT NOT NULL` | target entity id |
| `source` | `TEXT NOT NULL` | `manual`, `derived`, `automatic` |
| `summary` | `TEXT` | optional human summary |
| `bindings` | `BLOB` | derived slot/value map |
| `bundle_sha256` | `TEXT NOT NULL` | digest over canonicalized bundle |
| `metadata` | `BLOB` | optional |
| `created_at` | `INTEGER NOT NULL` | row timestamp |

### 3. `evidence_bundle_member`

Purpose:

- each bundle member points to one observed event and optionally records extraction details

Suggested columns:

| Column | Type | Notes |
|---|---|---|
| `id` | `TEXT PK` | UUID |
| `bundle_id` | `TEXT NOT NULL` | FK to `evidence_bundle` |
| `provenance_event_id` | `TEXT NOT NULL` | FK |
| `role` | `TEXT NOT NULL` | `observation`, `support`, `blocker`, `artifact`, `context` |
| `slot_intent_id` | `TEXT` | optional slot owner intent |
| `slot_name` | `TEXT` | optional slot keyword name |
| `extract_path` | `TEXT` | canonical serialized path |
| `derived_value` | `BLOB` | extracted fact |
| `derived_value_sha256` | `TEXT` | optional digest |
| `guard_expr` | `BLOB` | normalized guard expression |
| `guard_ok` | `INTEGER` | 0/1 |
| `metadata` | `BLOB` | optional |
| `created_at` | `INTEGER NOT NULL` | row timestamp |

### 4. `attestation`

Purpose:

- explicit machine decision or user-visible closure statement over a bundle

Suggested columns:

| Column | Type | Notes |
|---|---|---|
| `id` | `TEXT PK` | UUID |
| `conversation_soul_id` | `TEXT NOT NULL` | FK |
| `kind` | `TEXT NOT NULL` | `gate-proof`, `gate-impediment`, `plan-completion`, `plan-blocked`, `intent-closure`, `intent-abandonment` |
| `subject_kind` | `TEXT NOT NULL` | `gate`, `plan`, `intent` |
| `subject_id` | `TEXT NOT NULL` | target entity id |
| `bundle_id` | `TEXT NOT NULL` | FK to `evidence_bundle` |
| `status` | `TEXT NOT NULL` | `accepted`, `rejected`, `superseded` |
| `reason` | `TEXT` | summary/rationale |
| `policy_version` | `TEXT NOT NULL` | checker/policy identity |
| `attester_kind` | `TEXT NOT NULL` | `runtime`, `model`, `user`, `migration` |
| `attester_id` | `TEXT` | provider/model/runtime id |
| `schema_version` | `TEXT NOT NULL` | attestation payload schema |
| `payload` | `BLOB NOT NULL` | structured attestation body |
| `payload_sha256` | `TEXT NOT NULL` | digest |
| `created_at` | `INTEGER NOT NULL` | row timestamp |

## Proposed schema: changes to existing tables

### `conversation_intent`

Keep:

- `title`
- `rationale`
- `status`
- `fulfillment_summary`
- `abandonment_reason`

Add:

| Column | Type | Why |
|---|---|---|
| `resolved_plan_id` | `TEXT` | which plan actually closed or abandoned the intent |
| `closure_attestation_id` | `TEXT` | FK to `attestation` |
| `resolution_bundle_id` | `TEXT` | optional direct FK to bundle for easier reads |

Change in semantics:

- `conversation_intent_ref` becomes optional legacy/direct artifact refs only
- primary closure truth moves to `closure_attestation_id`

### `conversation_intent_plan`

Keep:

- `summary`
- `plan_dsl`
- `steps`
- `supersedes_plan_id`

Add:

| Column | Type | Why |
|---|---|---|
| `resolved_at` | `INTEGER` | terminal timestamp |
| `completion_attestation_id` | `TEXT` | FK to `attestation` |
| `status_reason` | `TEXT` | why completed/blocked/superseded |

Consider status change:

- current: `active`, `completed`, `superseded`, `abandoned`
- target preferred: `active`, `completed`, `blocked`, `superseded`

If renaming `abandoned` is too disruptive immediately:

- keep DB status `abandoned`
- present it as blocked/current-strategy-failed in user-facing surfaces

### `conversation_intent_gate`

Keep:

- `proposition`
- `required`

Change shape:

- `expected_proof` stops being loose slot/guard-only blob
- `candidate_proof`, `proof`, and `impediment` stop being the primary truth objects

Add:

| Column | Type | Why |
|---|---|---|
| `candidate_bundle_id` | `TEXT` | optional partial bundle |
| `proof_attestation_id` | `TEXT` | FK to gate-proof attestation |
| `impediment_attestation_id` | `TEXT` | FK to gate-impediment attestation |
| `resolution_summary` | `TEXT` | short human explanation |

Transition strategy:

- keep old blob columns temporarily during rollout
- backfill new attestation/bundle rows from old proof blobs when possible
- remove or deprecate old blob dependence after readers switch

### `conversation_intent_gate_ref`

Recommendation:

- deprecate in favor of `evidence_bundle_member`

Reason:

- current table stores raw refs but not enough structured derivation data
- bundles need extraction paths, derived values, guard results, and stable digests

Possible transition:

- keep table read-only for migration compatibility
- stop writing to it once bundle flow is live

## Proposed spec changes

### New spec concepts

Add to `src/com/blockether/vis/internal/intent_spec.clj`:

| Spec | Purpose |
|---|---|
| `::provenance-event-kind` | event classification |
| `::payload-sha256` | stable digest |
| `::bundle-id` | evidence bundle id |
| `::bundle-kind` | candidate/proof/impediment/completion/closure |
| `::subject-kind` | gate/plan/intent |
| `::evidence-clause` | one derivation requirement |
| `::requires` | vec of evidence clauses |
| `::binding` | derived slot binding |
| `::bindings` | map of slot -> derived binding |
| `::attestation-kind` | proof/completion/closure kinds |
| `::closure-attestation` | intent closure data |

### Replace weak proof shape

Current:

```clojure
::expected-proof {:slots ... :guard ...}
::proof {:summary :refs :slots}
::fulfill-intent-opts {:summary :refs}
```

Target:

```clojure
::expected-proof {:requires [::evidence-clause ...]
                  :policy {...}}

::prove-gate-opts {:summary string?
                   :bundle-id uuid?
                   ;; or raw refs/evidence request that runtime turns into a bundle
                   :evidence [{:from-ref ... :extract ... :guard ...}]}

::fulfill-intent-opts {:summary string?
                       :closure-attestation map?
                       :bundle-id uuid?
                       :refs optional-direct-artifact-refs?}
```

### Example evidence clause

```clojure
{:slot [(str (:id intent)) :verification-exit]
 :event-kind :tool
 :op :v/bash
 :extract [:result :exit]
 :guard [:= 0]
 :required? true}
```

### Example closure attestation spec shape

```clojure
{:plan-id <uuid>
 :basis :plan-completed
 :why "All required gates on the active plan were proven and the result was delivered."
 :artifacts [{:kind :file :path "src/foo.clj"}
             {:kind :test :id "foo-test"}]}
```

## Bundle design

### What a bundle is

An evidence bundle is an immutable, digestable package containing:

- selected provenance events
- extraction paths
- derived values
- guard outcomes
- optional human summary

The bundle is the stable input to an attestation.

### Bundle kinds

- `candidate`: partial evidence not yet accepted
- `proof`: successful gate evidence
- `impediment`: blocker evidence
- `completion`: all required gates proven for a plan
- `closure`: user-visible resolution evidence for an intent

### Bundle creation rules

1. Runtime resolves every ref to `provenance_event` rows.
2. Runtime extracts requested facts.
3. Runtime records derived bindings and guard outcomes.
4. Runtime computes canonical digest.
5. Runtime writes bundle + bundle members atomically.

### Bundle reuse

- gate-proof attestation references proof bundle
- plan-completion attestation may aggregate gate-proof bundles or cite their events directly
- intent-closure attestation references the completed plan and can reuse the plan-completion bundle plus any extra completion artifacts

## Attestation design

### What an attestation is

An attestation is a signed-or-digestable structured statement that says:

- which subject was decided
- using which bundle
- under which policy version
- with which result and rationale

Near-term we can use digests without cryptographic signatures.
If stronger trust is needed later, signatures can layer on top.

### Attestation kinds

- `gate-proof`
- `gate-impediment`
- `plan-completion`
- `plan-blocked`
- `intent-closure`
- `intent-abandonment`

### Attestation body examples

Gate proof:

```clojure
{:gate-id <uuid>
 :decision :proven
 :bundle-id <uuid>
 :derived-bindings {[(str intent-id) :verification-exit] {:value 0}}
 :why "Observed bash tool event exited 0."}
```

Plan completion:

```clojure
{:plan-id <uuid>
 :decision :completed
 :required-gates [gate-a gate-b]
 :why "All required gates were proven."}
```

Intent closure:

```clojure
{:intent-id <uuid>
 :decision :fulfilled
 :plan-id <uuid>
 :basis :plan-completed
 :why "User-visible request resolved after completed plan and delivered artifacts."}
```

## Proofing flow

### Gate proofing

Recommended flow:

1. User/model identifies target gate.
2. Runtime gathers refs or proposed evidence clauses.
3. Runtime resolves refs to immutable events.
4. Runtime derives bindings from event payloads.
5. Runtime checks guards and policy.
6. Runtime writes bundle.
7. Runtime writes gate-proof attestation.
8. Runtime marks gate `:proven`.
9. Runtime recomputes plan status.

## Guard design

Yes, guards need to support richer checks than simple equality.

But they should stay:

- small
- explicit
- deterministic
- data-only
- easy to render for humans

The mistake would be turning guards into a second full programming language.

## Recommended guard surface

Keep guards as a tiny declarative expression language.

### Core operators

| Operator | Meaning | Example |
|---|---|---|
| `:and` | all subguards pass | `[:and [:= x 0] [:contains y "ok"]]` |
| `:or` | any subguard passes | `[:or [:= status :done] [:= status :cached]]` |
| `:not` | negate one subguard | `[:not [:exists path]]` |
| `:=` | equality | `[:= exit 0]` |
| `:!=` | inequality | `[:!= status :error]` |
| `:<` `:<=` `:>` `:>=` | numeric compare | `[:>= duration-ms 0]` |
| `:exists` | value is present | `[:exists [:slot slot :ref]]` |
| `:contains` | collection/string contains value | `[:contains tags :verified]` |
| `:in` | value is one-of set | `[:in status [:done :cached]]` |
| `:matches` | regex/string pattern match | `[:matches stdout "passed"]` |

### Optional second-wave operators

Only add these if real use cases appear:

| Operator | Meaning | Example |
|---|---|---|
| `:every` | all values in a collection satisfy a subguard | `[:every exits [:= % 0]]` |
| `:some` | at least one value satisfies a subguard | `[:some langs [:= % :clojure]]` |
| `:count=` | collection count equals n | `[:count= failures 0]` |
| `:count>=` | collection count at least n | `[:count>= events 2]` |
| `:starts-with` | string prefix | `[:starts-with path "src/"]` |
| `:ends-with` | string suffix | `[:ends-with file ".clj"]` |

If second-wave operators are added, they should compile to the same internal guard evaluator and render cleanly in reports.

## One-of and contains semantics

Two common cases:

### `contains`

Use when the left side is a string or collection.

Examples:

```clojure
[:contains [:extract [:result :stdout]] "passed"]
[:contains [:extract [:result :tags]] :verified]
```

### `in`

Use when the left side is a single value and the right side is the allowed set.

Examples:

```clojure
[:in [:extract [:status]] [:done :cached]]
[:in [:extract [:result :exit]] [0 143]]
```

This keeps intent clear:

- `contains` = haystack contains needle
- `in` = value is one-of allowed values

## Guard examples in evidence clauses

```clojure
{:slot verification-exit
 :event-kind :tool
 :op :v/bash
 :extract [:result :exit]
 :guard [:in [:value] [0 143]]}
```

```clojure
{:slot verification-output
 :event-kind :tool
 :op :v/bash
 :extract [:result :stdout]
 :guard [:contains [:value] "passed"]}
```

```clojure
{:slot verification-status
 :event-kind :tool
 :op :v/bash
 :extract [:status]
 :guard [:and [:exists [:value]] [:in [:value] [:done :cached]] ]}
```

## Guard evaluation rule

Guards should evaluate over derived values, not arbitrary caller-supplied maps.

That means:

1. runtime resolves event
2. runtime extracts value
3. runtime binds extracted value to guard context
4. runtime evaluates guard
5. runtime records guard result in bundle member

Never let the model directly inject a value and say "trust me, the guard passed".

## Guard readability rule

Every supported guard must have a human-readable rendering.

Examples:

- `[:= [:value] 0]` -> "value equals 0"
- `[:contains [:value] \"passed\"]` -> "value contains \"passed\""
- `[:in [:value] [0 143]]` -> "value is one of [0, 143]"
- `[:and [:exists [:value]] [:in [:value] [:done :cached]]]` -> "value exists and is one of [:done, :cached]"

This matters for TUI, reports, and answer-time proof summaries.

## Guard design boundary

Allowed:

- simple boolean algebra
- membership checks
- string/collection containment
- numeric comparisons
- regex matching
- count-based checks if needed

Not allowed:

- arbitrary code execution inside guards
- side effects
- DB lookups from guard expressions
- hidden implicit coercions
- a Turing-complete mini language

The guard language should stay boring on purpose.

### Plan proofing

Recommended flow:

1. Any gate terminal transition triggers plan recomputation.
2. If all required gates are proven, runtime creates plan-completion attestation automatically.
3. If any required gate is impeded and no immediate re-plan happens, runtime creates plan-blocked attestation.

This part should be automatic because it is a pure aggregate over existing gate state.

### Intent proofing / closure

Recommended flow:

1. Intent fulfillment remains explicit.
2. Runtime verifies active plan is completed.
3. Runtime builds closure bundle from:
   - plan completion attestation
   - optional direct completion artifacts/events
4. Runtime writes intent-closure attestation.
5. Runtime marks intent `:fulfilled`.

This should not be fully automatic, because intent closure is the user-facing semantic commitment.

## What should be automatic vs explicit

### Automatic

- canonical provenance event persistence
- plan status recomputation after gate transitions
- derived slot extraction from event payloads
- bundle digest calculation
- plan-completion attestation when aggregate conditions are satisfied
- human-readable labels for refs, bundles, and attestations

### Explicit

- issuing intents
- choosing/issuing plans and gates
- proving or impeding a gate
- fulfilling or abandoning an intent
- writing the human rationale/summary for closure

### Maybe automatic later, but not in first pass

- auto-prove a gate when evidence unambiguously satisfies policy
- auto-suggest fulfillment when completed plan plus closure artifacts exist

If added later, keep them as explicit helpers like:

```clojure
(v/auto-prove-gate! gate-id)
(v/suggest-intent-closure intent-id)
```

Do not make them silent hidden side effects.

## Human-readable surfaces

Human readability matters because machine-valid proof nobody can inspect is bad UX.

### Required read surfaces

1. `v/provenance-timeline`
   - raw immutable event ledger
2. `v/bundles`
   - list bundles with kind, subject, digest, created-at
3. `v/bundle bundle-id`
   - show members, extracted values, guards, results
4. `v/attestations`
   - list attestations with kind, subject, status, policy version
5. `v/attestation attestation-id`
   - full structured decision
6. `v/proof-checks`
   - aggregate audit over intents/plans/gates/bundles/attestations
7. `v/proofs`
   - concise Markdown disclosure for normal answers

### Markdown rendering expectations

Bundle view should show:

| Field | Example |
|---|---|
| kind | `proof` |
| subject | `gate Gabc123` |
| digest | `sha256:...` |
| members | `3` |
| derived bindings | `verification-exit = 0` |

Attestation view should show:

| Field | Example |
|---|---|
| kind | `plan-completion` |
| subject | `plan Pabc123` |
| bundle | linked bundle id |
| policy version | `proof-policy/v1` |
| why | `All required gates were proven.` |

### TUI affordances

- clickable refs open event detail
- clickable bundles open member table
- clickable attestations open structured decision view
- intent screen shows active plan, gate statuses, latest completion/closure attestation

### Answer-friendly compact format

Normal answer should be able to embed a tiny summary like:

```text
Proof summary
- Plan P123 completed via 2 proven gates
- Closure attestation A456 recorded
- Evidence bundle B789 contains 3 observed events
```

## API evolution

### Suggested writer API direction

Current API can be evolved rather than deleted all at once.

Phaseable direction:

```clojure
(v/offer-proof! {:gate-id ...
                 :refs [...]})

(v/prove-gate! gate-id
  {:summary "Verification passed."
   :refs [...]} )
;; runtime converts refs -> bundle -> attestation

(v/fulfill-intent! intent-id
  {:summary "Done."
   :closure-attestation {:why "Delivered requested change."}
   :refs [...]})
;; refs optional; used only for extra completion artifacts
```

Longer-term cleaner API:

```clojure
(v/prove-gate! gate-id {:summary "Verification passed." :bundle-id bid})
(v/fulfill-intent! intent-id {:summary "Done." :plan-id pid :closure-attestation {...}})
```

## Migration strategy

Because the repo currently wants inline schema evolution in `V1__schema.sql`, the implementation plan should be:

1. Add new tables and new columns directly in `V1__schema.sql`.
2. Update persistence writers to populate both old and new structures temporarily if needed.
3. Switch readers/checkers to new bundle/attestation flow.
4. Stop relying on `conversation_intent_gate_ref` and raw repeated intent refs.
5. Remove dead compatibility logic after the rollout settles.

## Concrete schema implementation checklist

1. Add `provenance_event` table.
2. Add `evidence_bundle` table.
3. Add `evidence_bundle_member` table.
4. Add `attestation` table.
5. Add `conversation_intent.resolved_plan_id`.
6. Add `conversation_intent.closure_attestation_id`.
7. Add `conversation_intent.resolution_bundle_id`.
8. Add `conversation_intent_plan.resolved_at`.
9. Add `conversation_intent_plan.completion_attestation_id`.
10. Add `conversation_intent_plan.status_reason`.
11. Add `conversation_intent_gate.candidate_bundle_id`.
12. Add `conversation_intent_gate.proof_attestation_id`.
13. Add `conversation_intent_gate.impediment_attestation_id`.
14. Add indexes on canonical refs, subject ids, bundle ids, attestation subject links.
15. Add triggers/checks for immutability and same-conversation consistency.

## Concrete spec implementation checklist

1. Add specs for bundles, members, and attestations.
2. Replace loose slot/guard-centric proof spec with evidence-clause-centric proof spec.
3. Change `::fulfill-intent-opts` so refs are optional, summary remains required, and closure attestation becomes first-class.
4. Keep old public shapes temporarily only at boundary adapters if needed.
5. Add validation for policy version, subject kind, bundle kind, and attestation kind.

## Concrete report/check implementation checklist

1. `v/proof-checks` must validate bundles and attestations, not just refs.
2. `v/proofs` must render gate proof, plan completion, and intent closure distinctly.
3. `v/intents` must show plan completion and closure attestation directly.
4. `v/provenance-guards` must check event immutability, canonical refs, parent links, and bundle membership integrity.

## Concrete code changes

### Immediate

1. Remove fallback non-canonical refs from proof-critical paths.
2. Add tests proving proof slots cannot lie about extracted facts.
3. Mark plan `:completed` when all required gates are proven.
4. Make `db-fulfill-intent!` check plan status, not gate rows directly.
5. Add report/check output that surfaces plan status transitions.

### Next

1. Introduce stored provenance event records.
2. Add evidence extraction clauses.
3. Deprecate slot-value-as-claim pattern.
4. Add stronger invariants and adversarial tests.

## Test plan

Add regression tests for:

- canonical refs only in proof paths
- no compact fallback refs accepted for proof
- slot payload lies are rejected
- wrong event kind/op for gate evidence is rejected
- all required gates proven => plan becomes `:completed`
- plan `:completed` does not itself fulfill intent
- intent fulfillment requires completed plan plus evidence refs
- impeded required gate blocks final answer until re-plan or abandon

## Final recommendation

Answer to the core question:

- No, proving a gate should not close the intent.
- Yes, one plan is satisfied by many gates.
- Yes, plan completion should be the aggregate state over its required gates.
- No, plan completion should not silently equal intent fulfillment.
- Intent fulfillment should remain explicit, because that is the user-facing resolution boundary.

The system should move from:

- refs + prose + slot payloads

to:

- immutable observations + derived evidence + explicit decisions
