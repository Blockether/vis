# Vis Tasks

This file groups the current backlog into architectural task clusters. English only. Pareto first: fix high-leverage seams before adding more ad-hoc features.

Current focus: [`FOCUS.md`](FOCUS.md) — canonical context contract for XML-tagged prompt surfaces, preview/full value boundaries, var-index/journal behavior, skill activation/full-body blocks, reasoning memory, and audit/attestation loading.

Pinned context rule: `<journal>` is observed block evidence only. It may show block-authored intermediate `;;` / `#_(...)` comments, but it must not show LLM-only iteration `:thinking` or `ITERATION_PREVIOUS_REASONING`. Latest prior reasoning stays available only through the `ITERATION_PREVIOUS_REASONING` system var unless a future explicit reasoning surface is designed.

## Pareto order

| Rank | Task cluster | Covers | Why now |
|---:|---|---|---|
| 1 | Attestation ledger + intent lifecycle cleanup | Ref D, 2, 7, 8, 9, 10, 15, 16, 30, 35, 38 | Current proofing is weak. Trust semantics need immutable events, derived evidence, attestations, plan completion, and intent closure before more proof UX. |
| 2 | Presentation/render contract | 1, 3, 8, 22, 24, 29, 30, 36, Ref D, FOCUS.md | Visible UX must render previews/full retrieval, events, bundles, attestations, audits, proofs, tool calls, system calls, Mermaid, and provider errors from one seam. |
| 3 | Extension contract v2 | 4, 5, 17, 20, 21, 25, 26, 28, 39, Ref A, FOCUS.md | Config, toggles, renderers, preview/full surfaces, background jobs, aggregate state, custom providers, and agent-end hooks need the same extension seam. |
| 4 | [Native workspace manager](workspaes.md) | Ref C, 6, 12, 18, 19 | Git worktrees, submodules, and explicit attached repos give every conversation a real tool-visible workspace. |
| 5 | Git checkpoint/time-travel module | Ref B, Ref C, Ref D, 13, 18, 19, 23 | Protects user data and makes every agent edit a restorable Git tree that can become a branch. Checkpoints should cite provenance events. |
| 6 | Clojure language tooling cleanup | 11, 32, 33, 37 | Remove duplicate SCI test surfaces; add Clojure-native diagnosis tools. |
| 7 | Conversation switching while active | 13 | Useful UX, but stateful and risky. Do after runtime state is clearer. |
| 8 | General UX/docs/protocol cleanup | 14, 31, 34 | Important, but lower leverage until core seams settle. |

## External reference cross-check

### Ref A — Agent-end desktop notification

Reference: <https://github.com/mitsuhiko/agent-stuff/blob/main/extensions/notify.ts>

Observed behavior from the reference:

- listens for `agent_end`;
- extracts the last assistant text;
- simplifies/trims it into a short notification body;
- sends a terminal-native OSC 777 notification escape (`ESC ] 777 ; notify ; title ; body BEL`);
- no heavyweight desktop dependency.

Delta for Vis: existing `vis/notify!` and the planned desktop adapter are not enough. Vis also needs a lifecycle event/hook for “agent response ended and UI is waiting for input”, plus an OSC terminal notification adapter.

### Ref B — Git checkpoint / rewind

Reference: <https://github.com/prateekmedia/pi-hooks/tree/main/checkpoint>

Observed behavior from the reference:

- captures full worktree state at the start of every turn;
- includes tracked, staged, and untracked files;
- stores snapshots as persistent Git refs;
- creates a “before restore” checkpoint before rewinding;
- restore modes: files + conversation, conversation only, files only;
- supports fork/tree navigation restore prompts;
- filters large/generated/ignored paths to avoid snapshot bloat;
- safe restore avoids deleting pre-existing untracked files that were not part of the checkpoint.

Delta for Vis: workspace detection alone is not enough. Vis needs first-class checkpoint refs linked to conversation state/turn/iteration/edit events, with restore/time-travel modes for conversations, files, or both.

### Ref C — Native workspaces ADR

Reference: draft ADR provided 2026-05-01, “Native workspaces via Git worktrees, submodules, and explicit attached repos”.

Decision to align with:

- main repo materializes as a real Git worktree;
- submodules remain Git-native and are reconciled with `git submodule sync --recursive` plus `git submodule update --init --recursive`;
- nested non-submodule repos are never implicit, especially when hidden by `.gitignore`;
- important nested repos must be declared as attached repos;
- attached repos support `:isolated` and `:shared` modes;
- static repo manifest describes workspace intent;
- runtime state lives outside the repo;
- one deep workspace manager owns create/open/refresh/destroy/status/roots;
- external tools must see normal on-disk files and real paths.

Delta for Vis: the workspace cluster must not be just detection/prompt reporting. It must become a materialization manager backed by Git worktrees, submodule reconciliation, explicit attached repo manifests, runtime state outside the source checkout, drift detection, and cleanup.

### Ref D — Provenance, proof, intent, plan, gate cleanup

Reference: user-provided cleanup brief, “Provenance, Proof, Intent, Plan, Gate Cleanup”.

Decision to align with:

- rename overloaded proof language into clear layers;
- observation layer = `provenance_event`, ledger, ledger checks;
- evidence layer = `evidence_bundle`, bundle member, derived binding;
- decision layer = `attestation`, guard, policy;
- audit layer = audit, violations, report;
- stop using `proof` as a generic catch-all;
- persist proof-critical provenance as first-class append-only event rows;
- remove compact/fallback refs from proof-critical paths;
- derive evidence slot values from event payloads at runtime;
- write bundles and attestations for gate proof/impediment, plan completion/blocking, and intent closure/abandonment;
- automatically recompute plan status after gate transitions;
- keep intent fulfillment explicit and user-facing;
- plan completion permits intent fulfillment but does not imply it.

Delta for Vis: current provenance is decent forensic telemetry, but proofing is not strong evidence. The intent/gate system must move from “refs + prose + caller-supplied slot payloads” to immutable observations, derived evidence bundles, explicit attestations, and whole-system audit.

---

## Task cluster 1 — Presentation/render contract

Covers: **1, 3, 8, 22, 24, 29, 30, 36, Ref D**

### Rationale

Current rendering is split across several places:

- `src/com/blockether/vis/internal/markdown.clj`
- `extensions/common/vis-foundation/src/.../foundation/markdown.clj`
- `extensions/channels/vis-channel-tui/src/.../render.clj`
- `extensions/common/vis-foundation/src/.../foundation/introspection.clj`
- per-extension symbol `:render-fn`s
- lifecycle/rendering-kind paths for system calls

This is shallow. Every caller must know too much about Markdown, details, proofs, tool calls, and channel rendering.

### Proposal

Create a deep internal presentation module:

```clojure
com.blockether.vis.internal.presentation.markdown
com.blockether.vis.internal.presentation.blocks
com.blockether.vis.internal.presentation.render
```

Target data shapes:

```clojure
{:kind :presentation/markdown
 :title "Proofs"
 :body markdown-string
 :details? true
 :default-open? false
 :style :proof}

{:kind :presentation/tool-call
 :op :v/cat
 :summary "Read src/foo.clj"
 :body markdown-string
 :result result-map
 :severity :info}

{:kind :presentation/system-call
 :op :v/issue-intent!
 :summary "Intent created"
 :body markdown-string
 :style :system}

{:kind :presentation/provider-error
 :provider :zai
 :status 401
 :category :auth
 :message "Invalid API key"
 :recovery ["Open Settings → Providers" "Update ZAI_API_KEY"]}
```

After Ref D, “proof rendering” means audit/attestation rendering. It should render:

- collapsed summary badge;
- Markdown body inside details;
- optional Mermaid graph;
- event ledger refs;
- evidence bundles and derived bindings;
- attestations for gate proof/impediment, plan completion/blocking, intent closure/abandonment;
- gate, plan, and intent state;
- violations visible, not anemic.

Example proof projection:

```clojure
(md/details
  (md/summary "Proofs · OK · 3/3 gates · 5 refs")
  (md/h2 "Gate graph")
  (md/code-block "mermaid"
    "flowchart TD\n  intent --> plan\n  plan --> gate_verify\n  gate_verify --> ref_tests")
  (md/h2 "Gates")
  (md/table ["gate" "status" "refs"]
            [["verify" "proven" "`turn/.../block/2`"]]))
```

### Acceptance tasks

- [ ] Fix bad `<proofs>` / future `<audit-markdown>` rendering in TUI.
- [ ] Ensure `<details>` body renders full Markdown, not plain text.
- [ ] Extract presentation Markdown seam to internal namespace with clear docstring.
- [ ] Make every tool-call renderer return/use shared presentation blocks.
- [ ] Implement the canonical prompt/context contract in [`FOCUS.md`](FOCUS.md) (including skill `<activation_trigger>` catalog entries and active-skill full bodies).
- [ ] Add pretty render for system calls (`:vis/system`).
- [ ] Add provider-error presentation block.
- [ ] Add Mermaid fenced-code rendering support.
- [ ] Render audit report as Mermaid + event/bundle/attestation/gate/plan/intent report.
- [ ] Add regression tests for audit/proof collapsed/expanded rendering.
- [ ] Add regression tests for Markdown inside details/tool details.

### Considered alternatives

1. Keep TUI-specific parsing in `render.clj`.
   - Fast short-term.
   - Bad locality; every presentation feature edits TUI internals.
2. Put rendering into foundation extension.
   - Bad seam; channels need presentation even without foundation.
3. Use raw Markdown only.
   - Simple.
   - Not enough for click regions, details, tool metadata, system calls.

### Consequences

- One place to fix proof/detail rendering.
- Tool calls become nicer without per-extension hacks.
- Mermaid support becomes normal block rendering.
- Provider errors become visible and actionable.
- Existing `v/` Markdown symbols need compatibility layer.

---

## Task cluster 2 — Extension contract v2

Covers: **4, 5, 17, 20, 21, 25, 26, 28, 39, Ref A**

### Rationale

Extension spec exists and is good, but misses first-class slots for current needs:

- config;
- toggles;
- renderers;
- lifecycle/event hooks;
- background processes;
- aggregate state;
- custom provider descriptors.

Today these leak through env vars, TUI settings, docs, and per-extension ad-hoc code.

### Proposal

Extend extension spec with new slots:

```clojure
{:ext/config
 [{:key :exa/api-key
   :label "Exa API key"
   :type :secret-string
   :source [:config :env]
   :env ["EXA_API_KEY" "EXA_MCP_API_KEY"]
   :required? false}]

 :ext/toggles
 [{:key :exa/enabled?
   :label "Enable Exa tools"
   :default true
   :scope :global}]

 :ext/renderers
 [{:kind :tool
   :op :exa/web-search
   :render-fn #'render-web-search}]

 :ext/backgrounds
 [{:id :indexer
   :label "Workspace indexer"
   :start-fn #'start-indexer!
   :stop-fn #'stop-indexer!
   :status-fn #'indexer-status}]

 :ext/events
 [{:event/id :agent/end
   :event/doc "Agent response finished and Vis is waiting for input."
   :event/listener-fn #'notify-agent-end!}]

 :ext/providers
 [{:provider/id :custom-openai-compatible
   :provider/config-schema [{:key :base-url :type :string}
                            {:key :api-key :type :secret-string}]}]}
```

Settings dialog sections:

```text
Settings
  UI
  Model behavior
  Providers
  Extension config
  Extension toggles
  Background jobs
```

### Extension aggregate persistence

Proposed SQL shape:

```sql
CREATE TABLE extension_aggregate (
  id TEXT PRIMARY KEY NOT NULL,
  extension_id TEXT NOT NULL,
  aggregate_key TEXT NOT NULL,
  kind TEXT NOT NULL,
  metadata TEXT,
  content BLOB,
  conversation_soul_id TEXT NULL,
  conversation_state_id TEXT NULL,
  conversation_turn_state_id TEXT NULL,
  iteration_id TEXT NULL,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
```

Clojure shape:

```clojure
{:extension-id 'exa
 :aggregate-key :exa/cache
 :kind :cache/search-result
 :scope {:conversation-soul-id nil
         :conversation-state-id nil
         :turn-state-id nil
         :iteration-id nil}
 :metadata {:version 1}
 :content value}
```

### Streaming ownership

Do **not** put SSE/streaming/provider transport in `vis.commons`.

Correct split:

- **svar** owns provider transport, streaming, SSE, retry, model protocol.
- **Vis** owns provider registry metadata, config UI, user-visible diagnostics, turn lifecycle, presentation of provider errors.

### Acceptance tasks

- [ ] Add `:ext/config` spec and validation.
- [ ] Add `:ext/toggles` spec and validation.
- [ ] Add `:ext/renderers` spec and validation.
- [ ] Add `:ext/backgrounds` spec and validation.
- [ ] Add `:ext/events` or equivalent lifecycle hook registration for agent-end notifications.
- [ ] Add config/toggle read/write helpers on public facade.
- [ ] Make settings dialog larger.
- [ ] Split Settings into separate UI / Provider / Extension config / Extension toggles sections.
- [ ] Move extension env vars into Extension config box.
- [ ] Add extension aggregate persistence using HoneySQL in app code.
- [ ] Keep SQLite schema changes inline in V1 unless migration policy changes.
- [x] Remove PI mentions, PI aliases, PI config paths, or mark migration-only internally.
- [ ] Add custom provider-by-URL config shape.

### Considered alternatives

1. Add only `:ext/env`.
   - Already partly exists.
   - Too narrow for toggles/renderers/backgrounds.
2. Keep settings TUI hard-coded.
   - Fast.
   - Every extension feature requires TUI edits.
3. Put custom provider in config only.
   - Works for one provider.
   - Bad extension locality.

### Consequences

- Extension authors can declare UI/config instead of editing channel code.
- Settings becomes data-driven.
- Provider/custom URL support has a proper home.
- Legacy Exa PI config paths intentionally dropped; use explicit `EXA_MCP_CONFIG` or env vars.

---

## Task cluster 3 — Native workspace manager

Moved to [`workspaes.md`](workspaes.md).

---

## Task cluster 3A — Git checkpoints and time travel

Covers: **Ref B, 13, 18, 19, 23**

### Rationale

Workspaces tell Vis where work happens. Checkpoints make every meaningful state restorable. The reference checkpoint extension shows the missing product behavior: save code state around turns and restore it independently from conversation navigation.

User requirement is stronger than simple snapshots: every agent edit should become a Git tree that can later become a branch. Time travel should support:

- conversations only;
- files only;
- conversations and files together.

### Proposal

Create a checkpoint module that stores Git commit/tree refs without polluting the user's normal branches:

```text
refs/vis/checkpoints/<workspace-id>/<conversation-id>/<turn-id>/<sequence>
```

Each checkpoint should record:

```clojure
{:checkpoint/id "..."
 :workspace-id "..."
 :conversation-id "..."
 :conversation-state-id "..."
 :conversation-turn-id "..."
 :iteration-id nil-or-id
 :edit-ref nil-or-provenance-ref
 :git {:repo-root "/repo"
       :head "abc123"
       :checkpoint-commit "def456"
       :checkpoint-tree "789abc"
       :dirty-before? true
       :staged? true
       :untracked-count 3}
 :reason :turn-start|:pre-edit|:post-edit|:before-restore|:manual
 :created-at 1234567890}
```

Checkpoint triggers:

- turn start: capture pre-agent state;
- before every mutating Vis edit tool;
- after every successful mutating Vis edit tool;
- before restore/time travel;
- manual checkpoint command.

Restore modes:

```clojure
(v/checkpoint-restore! checkpoint-id {:mode :files})
(v/checkpoint-restore! checkpoint-id {:mode :conversation})
(v/checkpoint-restore! checkpoint-id {:mode :all})
```

Branch creation:

```clojure
(v/checkpoint-branch! checkpoint-id "experiment/from-proof-fix")
```

Implementation notes:

- Checkpoint over roots returned by `(workspace-roots id)`, not ad-hoc repo discovery.
- Capture main repo plus declared submodules and attached repos.
- Never silently checkpoint `.gitignore`d nested repos unless declared as attached repos in the workspace manifest.
- Capture tracked, staged, and bounded untracked files.
- Use a temporary Git index or internal worktree/mirror so checkpointing does not mutate the user's index.
- Store snapshots as Git refs, not loose ad-hoc directories.
- For native workspaces, create one checkpoint aggregate that contains one Git checkpoint per materialized child repo.
- Link checkpoint id into DB/extension aggregate and into provenance events.
- Never delete user-owned pre-existing untracked files on restore.
- Create `:before-restore` checkpoint automatically before applying any restore.
- Restoring files should target the selected materialized workspace root, not the original source checkout unless explicitly requested.

### Acceptance tasks

- [ ] Design checkpoint DB/aggregate shape linked to workspace id, conversation state, turn, iteration, and edit provenance refs.
- [ ] Add Git ref namespace `refs/vis/checkpoints/...`.
- [ ] Add checkpoint creation at turn start.
- [ ] Add checkpoint creation before mutating edit tools.
- [ ] Add checkpoint creation after successful mutating edit tools.
- [ ] Capture tracked, staged, and bounded untracked files without mutating the user's index.
- [ ] Checkpoint main repo plus declared submodules and attached repos from workspace manager roots.
- [ ] Reject or warn on ignored nested repos that are not declared attached repos.
- [ ] Add smart filters for ignored/generated/large files and large directories.
- [ ] Add safe restore that never deletes unrelated pre-existing untracked files.
- [ ] Add automatic `:before-restore` checkpoint.
- [ ] Add restore modes: files only, conversation only, files + conversation.
- [ ] Add branch-from-checkpoint command.
- [ ] Add TUI restore prompt when switching/forking/time-travelling conversations.
- [ ] Add `v/checkpoints`, `v/checkpoint`, `v/checkpoint-restore!`, `v/checkpoint-branch!` symbols.
- [ ] Add multiprocess, multirepo, submodule, and attached-repo regression tests.

### Considered alternatives

1. Store file copies in SQLite/Nippy only.
   - Easier to query, but poor fit for branch creation and large code trees.
2. Use normal Git commits on the user's current branch.
   - Too invasive; pollutes history.
3. Save only turn-start checkpoints.
   - Matches reference, but misses user requirement that every agent edit is a tree.
4. Save only files, not conversation state.
   - Cannot support full time travel.

### Consequences

- Strong safety net for user edits.
- Conversation forks can align code state with chat state.
- Every edit can become a branch.
- More Git complexity; must be bounded and observable.
- Restore UX must be explicit to avoid surprising file rewrites.

---

## Task cluster 4 — Attestation ledger + intent lifecycle cleanup

Covers: **Ref D, 2, 7, 8, 9, 10, 15, 16, 30, 35, 38**

### Runtime cross-check

Current code confirms the cleanup brief:

- `db-prove-gate!` validates caller-supplied `:slots` and `:guard` blobs, then writes `conversation_intent_gate.proof` plus `conversation_intent_gate_ref` rows.
- `db-prove-gate!` does **not** recompute or persist parent plan `:completed`.
- `db-fulfill-intent!` requires manual fulfillment refs and directly scans required gate rows instead of requiring `plan.status = completed`.
- `conversation_intent_plan.status` includes `completed`, but plan completion is under-modeled as a transition surface.
- `intent_spec.clj` defines loose proof shapes around `:slots`, `:refs`, and `:guard`.
- SQLite schema stores `iteration.blocks` and intent/gate proof blobs, but has no first-class `provenance_event`, `evidence_bundle`, or `attestation` tables.

Conclusion: the brief is accurate. Provenance is useful telemetry, but proof semantics are weak because the model can inject slot payloads that guards validate without runtime-derived evidence extraction.

### Vocabulary decision

Use these terms consistently:

| Current term | Target term | Meaning |
|---|---|---|
| `provenance guards` | `ledger checks` | Low-level integrity checks on stored provenance events. |
| `guard` | `guard` | One local boolean rule inside an evidence requirement. |
| `proof` | `attestation` | Structured decision over evidence. |
| `proof checks` | `audit` | Whole-system validation across ledger, bundles, attestations, and resolutions. |

Layer names:

1. Observation layer -> `provenance_event`, `ledger`, `ledger checks`.
2. Evidence layer -> `evidence_bundle`, `bundle member`, `derived binding`.
3. Decision layer -> `attestation`, `guard`, `policy`.
4. Audit layer -> `audit`, `violations`, `report`.

Internal naming rule:

- `event` for observations;
- `bundle` for grouped evidence;
- `attestation` for decision object;
- `audit` for system-wide validation;
- `guard` only for a local boolean requirement.

Transition aliases:

- `v/provenance-guards` -> primary `v/ledger-checks`, old alias deprecated.
- `v/proof-checks` -> primary `v/audit`, old alias deprecated.
- `v/proofs` -> primary `v/audit-markdown` or `v/attestations`, old alias deprecated.
- Keep `:guard` in the DSL.

### Target flow

```text
runtime observation -> event ledger -> evidence bundle -> attestation -> audit
```

Short model:

```text
event       = fact
bundle      = chosen facts together
attestation = conclusion from those facts
audit       = check that all conclusions are justified
```

Gate example:

```text
Runtime executes bash test command
  -> provenance_event E1
     kind=tool
     op=:v/bash
     status=:done
     payload.result.exit=0

Runtime builds evidence bundle B1
  -> includes E1
  -> extracts [:result :exit] => 0
  -> evaluates guard [:= [:value] 0]

Runtime writes attestation A1
  -> kind=gate-proof
  -> subject=gate G1
  -> bundle=B1
  -> decision=proven

Runtime updates gate G1
  -> status=proven
```

Plan example:

```text
Gate G1 proven via attestation A1
Gate G2 proven via attestation A2

Runtime recomputes plan P1
  -> all required gates proven

Runtime writes attestation A3
  -> kind=plan-completion
  -> subject=plan P1
  -> decision=completed

Runtime updates plan P1
  -> status=completed
```

Intent example:

```text
Plan P1 completed via attestation A3
Assistant explicitly fulfills intent I1

Runtime writes attestation A4
  -> kind=intent-closure
  -> subject=intent I1
  -> bundle=B3
  -> decision=fulfilled

Runtime updates intent I1
  -> status=fulfilled
```

### State machine decision

Do **not** auto-close intents when a gate is proven.

Rules:

1. Proving one gate changes only that gate directly.
2. Any gate state change may change aggregate status of the parent plan.
3. A plan is satisfied by many required gates, not one gate.
4. A completed plan means “execution strategy succeeded”, not “user request resolved”.
5. Intent fulfillment is explicit and user-facing.
6. A completed plan permits intent fulfillment, but does not perform it automatically.
7. An impeded required gate does not automatically abandon the intent.
8. An impeded required gate means re-plan or explicitly abandon with blocker evidence.
9. Superseding a plan does not resolve the intent.
10. Final answer is allowed only when focused intents are fulfilled or abandoned.

Status model:

```clojure
;; Intent
:active | :fulfilled | :abandoned

;; Plan target
:active | :completed | :blocked | :superseded

;; Plan transition compatibility
;; If DB rename is too disruptive, keep `abandoned` internally and present it as `blocked`.

;; Gate
:open | :proven | :impeded
```

Plan recomputation:

- `:active` if any required gate is `:open`;
- `:blocked`/legacy `:abandoned` if any required gate is `:impeded` and no immediate re-plan supersedes it;
- `:completed` if all required gates are `:proven`.

### Intent closure semantics

Current fulfillment requires manual refs. That is safety-motivated but duplicates lower-level gate evidence.

Target rule:

1. `fulfill-intent!` requires a completed active plan.
2. `fulfill-intent!` requires non-blank summary/rationale.
3. `fulfill-intent!` records an intent-closure attestation referencing the completed plan.
4. `fulfill-intent!` does not require caller to restate every gate ref.
5. Optional direct refs remain allowed for extra user-facing artifacts not captured by gate/plan evidence.

Closure shape:

```clojure
{:summary "Feature implemented and verified."
 :closure-attestation
 {:plan-id <active-plan-id>
  :basis :plan-completed
  :completed-gates [gate-a gate-b]
  :evidence-bundle-id <derived-bundle-id>
  :why "All required gates on the active plan were proven, and the resulting artifact was produced."
  :artifacts [{:kind :file :path "src/foo.clj"}
              {:kind :test :id "foo-test"}]}}
```

Optional convenience helper later:

```clojure
(v/complete-intent! intent-id {:summary "Done." :refs [...]})
```

It should verify active plan completion and create closure attestation. It must not hide semantics inside `prove-gate!`.

### Stronger evidence clauses

Current weak shape:

```clojure
{:refs [ref]
 :slots {slot {:ref ref :exit-code 0}}}
```

Problem: `:exit-code 0` is a caller-supplied claim.

Target shape:

```clojure
{:expected-proof
 {:requires
  [{:slot verification-exit
    :event-kind :tool
    :op :v/bash
    :extract [:result :exit]
    :guard [:= [:value] 0]
    :required? true}]}}
```

Proof request shape:

```clojure
(v/prove-gate! gate-id
  {:summary "Verification passed."
   :evidence [{:from-ref ref
               :slot verification-exit
               :extract [:result :exit]
               :guard [:= [:value] 0]}]})
```

Rules:

- runtime resolves every ref to immutable `provenance_event` rows;
- runtime extracts values from event payloads;
- runtime binds extracted value as `[:value]` for guard evaluation;
- runtime records derived value and guard result in `evidence_bundle_member`;
- fake slot values cannot satisfy proof.

### Guard language

Keep guards small, explicit, deterministic, data-only, and renderable.

Core operators:

| Operator | Meaning |
|---|---|
| `:and` | all subguards pass |
| `:or` | any subguard passes |
| `:not` | negate one subguard |
| `:=` | equality |
| `:!=` | inequality |
| `:<`, `:<=`, `:>`, `:>=` | numeric compare |
| `:exists` | value is present |
| `:contains` | string/collection contains value |
| `:in` | value is one of allowed values |
| `:matches` | regex/string pattern match |

Optional later operators only when real use cases appear: `:every`, `:some`, `:count=`, `:count>=`, `:starts-with`, `:ends-with`.

Guard boundaries:

- no arbitrary code execution;
- no side effects;
- no DB lookups from guard expressions;
- no hidden coercions;
- no Turing-complete mini-language.

Every guard must render to human text, e.g.:

```clojure
[:= [:value] 0]                  ;; value equals 0
[:contains [:value] "passed"]    ;; value contains "passed"
[:in [:value] [0 143]]           ;; value is one of [0, 143]
```

### Proposed schema

Inline migration rule applies: edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql` directly when implementation starts.

New tables:

```text
provenance_event
  id TEXT PK
  conversation_soul_id TEXT NOT NULL
  conversation_state_id TEXT
  conversation_turn_soul_id TEXT
  conversation_turn_state_id TEXT
  iteration_id TEXT
  canonical_ref TEXT NOT NULL UNIQUE
  parent_ref TEXT
  kind TEXT NOT NULL              -- eval/tool/error/diagnostic/system
  op TEXT NOT NULL
  status TEXT NOT NULL            -- running/done/error/interrupted/timeout/cancelled
  rendering_kind TEXT
  form_position INTEGER
  started_at INTEGER
  finished_at INTEGER
  duration_ms INTEGER
  payload_format TEXT NOT NULL    -- nippy/json
  payload BLOB NOT NULL
  payload_sha256 TEXT NOT NULL
  metadata BLOB
  created_at INTEGER NOT NULL

evidence_bundle
  id TEXT PK
  conversation_soul_id TEXT NOT NULL
  kind TEXT NOT NULL              -- candidate/proof/impediment/completion/closure
  subject_kind TEXT NOT NULL      -- gate/plan/intent
  subject_id TEXT NOT NULL
  source TEXT NOT NULL            -- manual/derived/automatic
  summary TEXT
  bindings BLOB
  bundle_sha256 TEXT NOT NULL
  metadata BLOB
  created_at INTEGER NOT NULL

evidence_bundle_member
  id TEXT PK
  bundle_id TEXT NOT NULL
  provenance_event_id TEXT NOT NULL
  role TEXT NOT NULL              -- observation/support/blocker/artifact/context
  slot_intent_id TEXT
  slot_name TEXT
  extract_path TEXT
  derived_value BLOB
  derived_value_sha256 TEXT
  guard_expr BLOB
  guard_ok INTEGER
  metadata BLOB
  created_at INTEGER NOT NULL

attestation
  id TEXT PK
  conversation_soul_id TEXT NOT NULL
  kind TEXT NOT NULL              -- gate-proof/gate-impediment/plan-completion/plan-blocked/intent-closure/intent-abandonment
  subject_kind TEXT NOT NULL      -- gate/plan/intent
  subject_id TEXT NOT NULL
  bundle_id TEXT NOT NULL
  status TEXT NOT NULL            -- accepted/rejected/superseded
  reason TEXT
  policy_version TEXT NOT NULL
  attester_kind TEXT NOT NULL     -- runtime/model/user/migration
  attester_id TEXT
  schema_version TEXT NOT NULL
  payload BLOB NOT NULL
  payload_sha256 TEXT NOT NULL
  created_at INTEGER NOT NULL
```

New/changed existing columns:

```text
conversation_intent
  resolved_plan_id
  closure_attestation_id
  resolution_bundle_id

conversation_intent_plan
  resolved_at
  completion_attestation_id
  status_reason
  status: prefer active/completed/blocked/superseded; keep abandoned as compatibility if needed

conversation_intent_gate
  candidate_bundle_id
  proof_attestation_id
  impediment_attestation_id
  resolution_summary
```

Deprecate `conversation_intent_gate_ref` in favor of `evidence_bundle_member` after readers switch.

Required constraints/indexes:

- canonical refs unique;
- parent ref exists or is null;
- event rows append-only / immutable;
- bundle/attestation subject indexes;
- same-conversation consistency triggers;
- terminal gate refs/attestations immutable;
- payload and bundle digests present.

### Proposed specs

Add to `src/com/blockether/vis/internal/intent_spec.clj`:

```text
::provenance-event-kind
::payload-sha256
::bundle-id
::bundle-kind
::subject-kind
::evidence-clause
::requires
::binding
::bindings
::attestation-kind
::closure-attestation
```

Target shapes:

```clojure
::expected-proof {:requires [::evidence-clause ...]
                  :policy {...}}

::prove-gate-opts {:summary string?
                   :bundle-id uuid?
                   :evidence [{:from-ref ...
                               :extract ...
                               :guard ...}]}

::fulfill-intent-opts {:summary string?
                       :closure-attestation map?
                       :bundle-id uuid?
                       :refs optional-direct-artifact-refs?}
```

Keep old public shapes temporarily only at boundary adapters.

### Human-readable surfaces

Required read surfaces:

```clojure
(v/provenance-timeline)  ;; raw immutable event ledger
(v/ledger-checks)        ;; new primary name for low-level ledger integrity
(v/bundles)
(v/bundle bundle-id)
(v/attestations)
(v/attestation attestation-id)
(v/audit)                ;; new primary name for whole-system validation
(v/audit-markdown)       ;; concise Markdown disclosure for normal answers
(v/intents)              ;; shows active plan, gates, latest completion/closure attestations
```

TUI affordances:

- clickable refs open event detail;
- clickable bundles open member table;
- clickable attestations open structured decision view;
- intent screen shows active plan, gate statuses, plan completion attestation, and intent closure attestation.

Answer-friendly compact format:

```text
Proof summary
- Plan P123 completed via 2 proven gates
- Closure attestation A456 recorded
- Evidence bundle B789 contains 3 observed events
```

### Immediate implementation checklist

- [ ] Rename docs/prompt/UI terminology: ledger checks, attestation, audit; keep old API aliases deprecated.
- [ ] Remove fallback non-canonical refs from proof-critical paths.
- [ ] Add tests proving compact refs like `i1.1` are display-only and rejected in proof paths.
- [ ] Add tests proving slot payload lies are rejected.
- [ ] Add first-class `provenance_event` table/store.
- [ ] Persist eval, tool, error, diagnostic, system, lifecycle events directly as immutable rows.
- [ ] Add evidence clause spec and evaluator.
- [ ] Build evidence bundles by resolving refs, extracting payload values, evaluating guards, and recording derived bindings.
- [ ] Add attestation writer for gate proof and gate impediment.
- [ ] Recompute parent plan status after `db-prove-gate!` / `db-impede-gate!`.
- [ ] Mark plan `:completed` when all required gates are proven.
- [ ] Create plan-completion attestation automatically when plan completes.
- [ ] Make `db-fulfill-intent!` require completed active plan, not direct gate scan.
- [ ] Make intent fulfillment write intent-closure attestation.
- [ ] Make direct fulfillment refs optional artifact refs, not repeated gate proof refs.
- [ ] Add audit over event ledger, bundles, attestations, gate states, plan states, intent states.
- [ ] Update `v/intents` to show plan completion and closure attestation.
- [ ] Update `v/audit-markdown` / deprecated `v/proofs` rendering to distinguish gate proof, plan completion, and intent closure.

### Schema checklist

- [ ] Add `provenance_event` table.
- [ ] Add `evidence_bundle` table.
- [ ] Add `evidence_bundle_member` table.
- [ ] Add `attestation` table.
- [ ] Add `conversation_intent.resolved_plan_id`.
- [ ] Add `conversation_intent.closure_attestation_id`.
- [ ] Add `conversation_intent.resolution_bundle_id`.
- [ ] Add `conversation_intent_plan.resolved_at`.
- [ ] Add `conversation_intent_plan.completion_attestation_id`.
- [ ] Add `conversation_intent_plan.status_reason`.
- [ ] Add `conversation_intent_gate.candidate_bundle_id`.
- [ ] Add `conversation_intent_gate.proof_attestation_id`.
- [ ] Add `conversation_intent_gate.impediment_attestation_id`.
- [ ] Add indexes on canonical refs, subject ids, bundle ids, and attestation subject links.
- [ ] Add immutability and same-conversation triggers/checks.

### Test plan

- [ ] Canonical refs only in proof paths.
- [ ] No compact fallback refs accepted for proof.
- [ ] Slot payload lies are rejected.
- [ ] Wrong event kind/op for gate evidence is rejected.
- [ ] Missing extract path fails proof.
- [ ] Guard false fails proof.
- [ ] Derived slot values are recorded in bundle member.
- [ ] All required gates proven => plan becomes `:completed`.
- [ ] Plan `:completed` does not itself fulfill intent.
- [ ] Intent fulfillment requires completed plan plus closure attestation.
- [ ] Optional direct artifact refs can attach to intent closure.
- [ ] Impeded required gate blocks final answer until re-plan or abandon.
- [ ] Audit catches missing event, missing bundle member, bad attestation subject, invalid plan completion, invalid intent closure.

### Deferred intents and memory policy integration

The prior deferred-intent and memory tasks remain, but they must use the new audit language:

- deferred intents are inactive user-facing commitments, not hidden active gates;
- deferred plans/gates may exist but do not block final answer until activated/focused;
- deferred activation should create or update intent focus explicitly;
- journal remains recent event display, backed by `provenance_event` rows;
- var index remains hot executable symbol directory;
- durable archive tombstones are still needed if restart-stable symbol archive behavior matters;
- `(def ...)` journal rendering should still show `#'sandbox/x` where appropriate.

System intent kinds remain useful:

```clojure
:explore :validate :proof :implement :push :deploy :refactor :diagnose :document :cleanup
```

But docs/UI should prefer `:attest` or `:audit` where the task is really about evidence validation.

### Considered alternatives

1. Keep current refs + proof blobs.
   - Too weak; guards validate caller-injected slot payloads.
2. Auto-fulfill intent after all gates are proven.
   - Wrong semantic boundary; plan success is not user outcome delivery.
3. Store only bundles without attestations.
   - Misses explicit machine decision and policy version.
4. Store only attestations without bundles.
   - Loses derived evidence and guard auditability.
5. Make guards executable code.
   - Too powerful and unsafe; keep deterministic data DSL.

### Consequences

- Proof-critical events become immutable and queryable.
- Slot lies stop satisfying gates.
- Plan completion becomes real persisted state.
- Intent closure stops duplicating raw gate refs.
- Audit can explain the whole story end-to-end.
- Schema and persistence changes are required.
- Existing public names need alias/deprecation period.
- More concepts exist, but names become clearer and layers become testable.

---

## Task cluster 5 — Tooling, patches, background jobs, notifications

Covers: **23, 25, 26, 27, 28**

### Rationale

Current foundation editing surface should be minimal:

- `v/cat` is the single file-read surface. With no opts it reads the whole file into `[:result :lines]`; `:offset`, `:limit`, and `:char-limit` are explicit pagination knobs only.
- `v/patch` is the single text-edit surface for exact search/replace patches.
- `v/write-lines`, `v/update-file`, and `v/read-all-lines` are removed from the model-facing public surface.
- `v/bash` remains for process boundaries until background lifecycle tools replace it.

Need safer edits and observable long-running work.

### Proposal: preconditioned patch

```clojure
(v/patch
 [{:path "src/foo.clj"
   :precondition {:sha256 "abc..."
                  :mtime-ms 123}
   :search "old"
   :replace "new"}])
```

Result:

```clojure
{:ok? true
 :result {:patched 1
          :files [{:path "src/foo.clj"
                   :before-sha "..."
                   :after-sha "..."}]}
 :provenance {:op :v/patch}}
```

### Proposal: background process lifecycle

```clojure
(v/bg-start!
 {:id :dev-server
  :cmd ["clojure" "-M:dev"]
  :cwd "."
  :stdout :journal
  :stderr :journal
  :restart :never})

(v/bg-status :dev-server)
(v/bg-stop! :dev-server)
(v/bg-tail :dev-server {:lines 100})
```

Persist process metadata in extension aggregate or dedicated process table. Running child events are never proof-compatible; only terminal `:done` can prove success.

### Proposal: OS notifications and agent-end notifications

Internal notifications already exist. Add OS adapter behind same seam:

```clojure
(vis/notify! "Verify passed" :level :success :desktop? true)
```

Also add an agent-end hook matching Ref A: when the agent response finishes and Vis is waiting for input, send a short desktop/terminal notification using the last assistant answer.

```clojure
(vis/notify-agent-end!
 {:title "Vis"
  :body "Done. Verification passed."
  :conversation-id conversation-id
  :turn-id turn-id})
```

Adapters:

- terminal OSC 777: `ESC ] 777 ; notify ; title ; body BEL` for Ghostty/iTerm2/WezTerm-compatible terminals;
- macOS: `osascript` or `terminal-notifier` if installed;
- Linux: `notify-send`;
- Windows: PowerShell toast fallback.

Notification formatting:

- extract last assistant/final answer text;
- strip Markdown to compact plain text;
- normalize whitespace;
- truncate body around 200 chars;
- default to “Ready for input” when no answer text exists.

### Acceptance tasks

- [x] Make `v/cat` no-opts read the whole file; pagination is explicit via opts.
- [x] Remove `v/read-all-lines`, `v/write-lines`, and `v/update-file` from the model-facing public surface.
- [x] Add `v/patch` with exact search/replace.
- [ ] Add `v/patch` sha/mtime preconditions.
- [x] Add patch render fn with Markdown details.
- [ ] Add background process extension/runtime module.
- [ ] Add bg start/status/stop/tail symbols.
- [ ] Persist or aggregate background process metadata.
- [ ] Add lifecycle cleanup on shutdown/conversation disposal.
- [ ] Add desktop notification adapter behind `vis/notify!`.
- [ ] Add OSC 777 terminal notification adapter.
- [ ] Add lifecycle event/hook for “agent response ended and waiting for input”.
- [ ] Add agent-end notification formatter based on last assistant/final answer text.
- [ ] Add TUI/settings toggle for agent-end notifications.
- [ ] Deprecate `v/bash` only after background/process/patch replacements exist.

### Considered alternatives

1. Remove bash immediately.
   - Too breaking; replacements must land first.
2. Background `future` only.
   - Too anemic: no owner, logs, stop, proof semantics.
3. mtime-only patch precondition.
   - Better than nothing, but sha is deterministic.

### Consequences

- Safer edits.
- Long-running tasks become observable.
- Bash can be deprecated after replacement coverage exists.
- Requires careful cleanup on conversation switch/shutdown.

---

## Task cluster 6 — Clojure language support

Covers: **11, 32, 33, 37**

### Rationale

Clojure extension exists, but SCI exposes both lazytest and clojure.test aliases. User wants one test framework. Also wants deeper Clojure introspection:

- classpath;
- dependencies in classpath;
- namespace source;
- who-calls / xref;
- clj-kondo diagnostics.

### Proposal: remove Lazytest from SCI

Current SCI exposes:

```clojure
'lazytest.core {...}
'clojure.test {...}
'lt 'lazytest.core
'test 'clojure.test
```

Target:

```clojure
'clojure.test {'is      clojure.test/is
               'deftest clojure.test/deftest
               'testing clojure.test/testing}
'test 'clojure.test
```

If macro support is awkward in SCI, expose a small single test/check surface. Do not teach two frameworks.

### Proposal: Clojure commands/symbols

Under `clj/` alias:

```clojure
(clj/classpath)
(clj/dependencies)
(clj/ns-source 'com.foo.bar)
(clj/find-symbol 'foo/bar)
(clj/who-calls 'foo/bar)
(clj/kondo {:paths ["src" "test"]})
(clj/xref {:symbol 'foo/bar})
```

Use:

- `clj-kondo` CLI/lib for lint;
- `clj-xref` for call graph;
- current JVM classpath for source lookup.

### Acceptance tasks

- [ ] Remove lazytest namespace and `lt` alias from SCI env.
- [ ] Remove lazytest teaching from prompt.
- [ ] Keep repo test runner unaffected.
- [ ] Add `clj/classpath`.
- [ ] Add `clj/dependencies`.
- [ ] Add `clj/ns-source`.
- [ ] Add `clj/find-symbol`.
- [ ] Add `clj/who-calls` using clj-xref.
- [ ] Add `clj/kondo` using clj-kondo.
- [ ] Add tests for every new namespace.

### Considered alternatives

1. Keep lazytest because repo tests use it.
   - Repo tests can still use lazytest.
   - Model SCI does not need it.
2. Expose raw shell commands only.
   - Weak; model then parses text instead of data.

### Consequences

- Less prompt confusion.
- Better Clojure-specific repair loop.
- New namespaces require matching tests.

---

## Task cluster 7 — Conversation switching while active turn runs

Covers: **13**

### Current truth

TUI already has a conversation switcher. `screen.clj` currently blocks switch while loading with:

```text
Finish or cancel the running turn before switching conversations
```

### Proposal

Allow switching during active turn by separating active runtime turns from current UI view:

```clojure
{:conversation {:id current}
 :running-turns {conversation-id {:turn-id ...
                                  :status :running
                                  :cancel-token ...}}}
```

Behavior:

- old conversation turn continues;
- UI can switch to another conversation;
- notifications show completion/failure of old turn;
- render/prewarm/listeners scoped by conversation id;
- footer/header show running turns elsewhere.

Example UI text:

```text
Running in other conversation: abc123 · switch back to cancel
```

### Acceptance tasks

- [ ] Audit TUI assumptions that only current conversation can have running turn.
- [ ] Add `:running-turns` state shape.
- [ ] Scope lifecycle listeners by conversation id.
- [ ] Allow switch while loading.
- [ ] Add notification on old turn completion/failure.
- [ ] Add UI affordance to return/cancel running turn.
- [ ] Add regression tests for no stale render/prewarm/listener updates.

### Considered alternatives

1. Keep current block.
   - Simple and safe.
   - Poor UX for long turns.

### Consequences

- Better UX.
- More state complexity.
- Must prevent input from accidentally affecting wrong conversation.

---

## Task cluster 8 — Provider behavior, visibility, and custom provider

Covers: **2, 35, 36, 39**

### Rationale

User sees different visibility of intentions/plans/reasoning in Codex vs ZAI. TUI has settings toggles, but provider traces may differ before TUI rendering.

Need normalize provider diagnostics and trace data at runtime seam.

### Proposal: provider trace normalization

Per iteration capture:

```clojure
{:provider :openai-codex
 :model "gpt-5.5"
 :thinking-text ...
 :plan-events [...]
 :raw-provider-metadata {...}
 :visible? true}
```

Settings:

```clojure
:show-thinking true
:show-provider-plans true
:show-system-intents true
```

### Proposal: custom provider

Config shape:

```edn
{:providers
 [{:id :custom/openai-compatible
   :label "My Proxy"
   :base-url "https://llm.example.com/v1"
   :api-style :openai
   :api-key "env:MY_PROXY_KEY"
   :models [{:name "gpt-5"}]}]}
```

### Acceptance tasks

- [ ] Compare raw Codex vs ZAI iteration trace data.
- [ ] Determine whether missing plans/intents are hidden by TUI settings or absent from provider/runtime data.
- [ ] Add `:show-provider-plans` toggle if hidden by us.
- [ ] Add `:show-system-intents` toggle if needed.
- [ ] Normalize provider trace metadata per iteration.
- [ ] Render provider errors as structured presentation diagnostics.
- [ ] Add custom OpenAI-compatible provider config.
- [ ] Add settings UI for custom provider URL/key/model.

### Considered alternatives

1. Add only custom base-url to existing providers.
   - Fast but muddy.
2. Make every provider extension own its UI.
   - Too much duplication.

### Consequences

- Easier debugging.
- User can bring any OpenAI-compatible endpoint.
- Need secret handling in settings.

---

## Task cluster 9 — Project guidance, docs, ACP, general UX cleanup

Covers: **14, 31, 34**

### Rationale

There is accumulated aesthetic/key/doc debt. Also user wants to remove giant `AGENTS.md` and encode guidance sensibly in Clojure extension. ACP should be added, but only after core seams are stable.

### Proposal

Separate cleanup lane:

```text
Aesthetic: spacing, colors, proof badges, system rows
Keybinding: docs vs code parity, no Ctrl+Y
Docs: mdBook only, README stub stays tiny
Project guidance: move generated/model-facing guidance into extension-managed Clojure data
Protocol: ACP spike
```

Caution: the current agent harness reads `AGENTS.md`, so deleting it before replacement may break external agent behavior.

### Acceptance tasks

- [ ] Audit aesthetic rough edges.
- [ ] Audit keybindings; keep Ctrl+Y unbound.
- [ ] Audit docs; keep repo README tiny and move long docs to mdBook.
- [ ] Design extension-managed project guidance replacement for huge AGENTS.md.
- [ ] Keep compatibility for agent harness that reads AGENTS.md.
- [ ] ACP design spike after extension/presentation/workspace seams settle.

### Considered alternatives

1. Do one huge cleanup task.
   - Hard to verify; likely endless.
2. Add ACP now.
   - Adapter would hard-code unstable internals.

### Consequences

- Cleaner product over time.
- Less prompt/manual drift.
- Need careful compatibility path for external agents.

---

## Item-by-item map

| Item | Cluster | Action |
|---:|---|---|
| 1 | Presentation | Fix `<proofs>` rendering/details body. |
| 2 | Provider/intent visibility | Compare raw provider trace + TUI settings; add toggle if hidden by us. |
| 3 | Presentation | Split/clarify internal Markdown module; all renderers use it. |
| 4 | Extension/settings | Bigger settings, separate Extension config box. |
| 5 | Extension cleanup | DONE — removed PI mentions/aliases/paths from Exa and TUI comments; no migration compatibility kept. |
| 6 | Workspace | Add `v/workspace(s)` symbols. |
| 7 | Intent taxonomy | System intent kinds. |
| 8 | Attestation/presentation | Make evidence non-anemic: events, bundles, attestations, gates, plan completion, intent closure, violations. |
| 9 | Ledger/memory | Journal becomes display over provenance events; var index remains hot symbols; durable archive tombstones if needed. |
| 10 | Deferred intents | Design deferred intents/plans/gates; compare to Codex goals. |
| 11 | Clojure SCI | Remove lazytest from SCI prompt/env. |
| 12 | Install/workspace | `install.sh`, `.local/share/vis`, `.local/bin/vis`, record install root. |
| 13 | TUI state | Conversation switching during active turn. |
| 14 | UX cleanup | Aesthetic/key/docs pass. |
| 15 | Memory | Debug symbol restore/restart behavior; def result display. |
| 16 | Intent taxonomy/audit | Duplicate of 7; implement once with audit/attestation vocabulary. |
| 17 | Extension contract | First-class extension toggles. |
| 18 | Workspace | First-class Git workspaces/worktrees. |
| 19 | Workspace | Multirepo prompt summary. |
| 20 | Extension commons | Extract commons, but not streaming; streaming belongs in svar. |
| 21 | Extension persistence | `extension_aggregate` schema. |
| 22 | Presentation/tool calls | Tool details must render Markdown. |
| 23 | Editing | `v/cat` whole-file read by default; `v/patch` as canonical edit surface; add sha/mtime preconditions. |
| 24 | Presentation/system calls | Render system calls prettily. |
| 25 | Background | Background process support. |
| 26 | Notifications | OS notification adapter. |
| 27 | Tooling | Deprecate/remove bash after replacements exist. |
| 28 | Cleanup/background | PI prefixes removed for Exa; background lifecycle remains. |
| 29 | Presentation | Mermaid rendering. |
| 30 | Presentation/audit | Audit report as Mermaid + event/bundle/attestation/gate/plan/intent projection. |
| 31 | Project guidance | Replace huge AGENTS.md with Clojure-coded extension guidance, with compatibility. |
| 32 | Clojure language | clj-xref support. |
| 33 | Clojure language | clj-kondo support. |
| 34 | Protocol | ACP support spike. |
| 35 | Provider/intents | Better model/provider selection during turn; likely intent/provider routing. |
| 36 | Provider/presentation | Visible provider errors. |
| 37 | Clojure language | Classpath/deps/ns-source/who-calls commands. |
| 38 | Deferred behavior | Teach future/deferred syntax/lifecycle to model using ledger/bundle/attestation proof semantics. |
| 39 | Provider config | Custom provider by URL. |
| Ref A | Notifications/extensions | Notify when an agent response ends; implement agent-end hook + OSC 777/desktop adapters. |
| Ref B | Checkpoints/time travel | Save every agent edit/turn state as Git checkpoint refs; restore files, conversations, or both; branch from checkpoint. |
| Ref C | Native workspaces | Materialize real workspaces with Git worktrees, Git-native submodules, explicit attached repos, and runtime state outside source checkout. |
| Ref D | Attestation ledger | Replace proof blobs with immutable provenance events, evidence bundles, derived guards, attestations, plan completion, intent closure, and audit. |

---

## Recommended execution plan

### Sprint 0 — Context contract (`FOCUS.md`)

Canonical detail plan lives in [`FOCUS.md`](FOCUS.md). Do not duplicate the full checklist here.

- [ ] Finish remaining `FOCUS.md` context-contract tasks before deeper ledger/presentation work.
- [ ] Keep prompt/context surfaces XML-tagged and spec-checked.
- [ ] Keep `<skills>` activation catalog and `<active_skills>` full bodies aligned with `FOCUS.md`.
- [ ] Keep nudge return contract aligned with `<system_nudge importance="low|normal|high|critical">`.

### Sprint 1 — Attestation ledger foundation

- [ ] Rename terminology in docs/prompt/UI: ledger checks, attestation, audit.
- [ ] Remove compact/fallback refs from proof-critical paths.
- [ ] Add `provenance_event`, `evidence_bundle`, `evidence_bundle_member`, and `attestation` schema.
- [ ] Persist proof-critical runtime events as immutable provenance rows.
- [ ] Add evidence clause extraction + guard evaluation over derived values.
- [ ] Recompute and persist plan completion after gate transitions.
- [ ] Make intent fulfillment require completed plan and write closure attestation.

### Sprint 2 — Presentation + audit UX

- [ ] Fix proof/details Markdown rendering.
- [ ] Extract presentation Markdown seam cleanly.
- [ ] Add event/bundle/attestation/audit presentation data shape.
- [ ] Add system/tool/provider-error presentation data shape.
- [ ] Add Mermaid block support for audit graphs.

### Sprint 3 — Extension contract

- [ ] Add `:ext/config`, `:ext/toggles`, `:ext/renderers`.
- [ ] Make settings dialog bigger.
- [ ] Add separate Extension config box.
- [x] Remove PI mentions from Exa docs/code.
- [ ] Add custom provider config shape.

### Sprint 4 — Checkpoints + workspaces

- [ ] Add Git checkpoint/time-travel module.
- [ ] Save checkpoint at turn start and around every mutating edit.
- [ ] Add restore modes: files only, conversation only, files + conversation.
- [ ] Add branch-from-checkpoint command.
- [ ] Add first-class workspace module.
- [ ] Add `v/workspace`, `v/workspaces`, `v/git-summary`.
- [ ] Add multirepo prompt block.
- [ ] Add installer script to `.local`.

### Sprint 5 — Intent taxonomy, deferred intents, memory

- [ ] Add intent kind taxonomy with audit/attestation vocabulary.
- [ ] Write deferred intent design doc + DB/API shape.
- [ ] Decide durable archive tombstones / restart-stable symbol behavior.
- [ ] Fix def result rendering regression.

### Quick parallel chores

- [ ] Remove lazytest from SCI.
- [ ] Improve provider error diagnostics.
- [ ] Add adversarial proof tests: slot lies, wrong event op, compact refs rejected.
- [ ] Finish `v/patch` sha/mtime preconditions.
- [ ] Add agent-end notification hook + OSC 777 adapter.
- [ ] Spike clj-kondo/clj-xref integration.
