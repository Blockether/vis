# Scoped settings across the app and TUI

One settings model, four scopes, explicit overrides.

## Context

The implementation uses the existing settings renderers and configuration owners.
`src/com/blockether/vis/internal/config/scoped.clj` resolves sparse values through
Global → canonical project → organizational group → session. Each declaration can
allow any nonempty subset of these scopes. Explicit false differs from inheritance.

`src/com/blockether/vis/internal/sandbox/scoped_policy.clj` applies the same
hierarchy to access settings. Group and session values live in SQLite; project
overrides use the hidden project configuration; global values use the machine
store. Authored YAML is not rewritten. Providers, credentials and gateway
administration remain global; device preferences stay local.

Rejected alternatives: copying effective settings into new sessions or forks,
using Council groups as settings scopes, sending cached client toggles with every
message, cancelling active calls when availability changes, and treating optional
extension activation as decision-model routing.

## 1. Declarations, storage and resolution

Rationale: inheritance must be per key and identical for every client.

Data: the portable toggle schema and contract; config/scoped; configuration writers;
SQLite group/session overrides; gateway settings schemas and routes.

Acceptance criteria: all 15 nonempty scope combinations, false values, inherit,
canonical project identity, ungrouped sessions, moves, forks, concurrent writes,
durable reopening, owner deletion, provenance and rejected unsupported writes.
Explicit global access persists without importing project grants through other writes.

Unknowns: none. Complete, with regression coverage.

## 2. Response and resource lifecycle

Rationale: a submitted response must not change midway, while disabled resources
must not remain callable through cached names or handles.

Data: gateway submission/queue snapshots, loop/turn bindings, live skill discovery,
MCP definitions and availability, optional extension Auto/On/Off gates, access policy.

Acceptance criteria: submission captures response settings, including queued work;
new calls and lookups honor availability; active calls finish; subsequent turns
rebuild access bindings; narrower scopes cannot relax host security ceilings.

Unknowns: none. Complete. Existing drafts are not moved when policy changes.

## 3. Python SDK and host boundary

Rationale: extensions declare typed settings without duplicating host contracts.

Data: canonical `packages/vis-agent/src/blockether/vis/extension.py`, the outside
bridge, engine extension validation, and Clojure Python-extension registration.

Acceptance criteria: boolean/enum declarations, defaults and allowed scopes validate
through the canonical contract; `Host.setting` reads the response snapshot; native
and editable SDK paths support the same declarations. App-hosted extensions reject
engine settings rather than silently accepting unsupported contributions.

Unknowns: none. JVM, SDK and native boundary tests cover the feature.

## 4. Companion, TUI and MCP CLI

Rationale: every scope uses the same controls and clearly exposes inheritance.

Data: Companion ScopedSettingsDialog and MachineSettings; TUI dialogs/client/state;
session, group and project entry points; MCP target-aware HTTP and CLI operations.

Acceptance criteria: effective value, source and own override are visible; inherit
removes only the selected override; session response controls write to that session;
no cached client values are resubmitted. Scoped MCP editing hides global credentials,
authentication and lifecycle actions. TUI settings refresh on opening and local edits;
Companion also refreshes the selected target while the dialog is open.

Unknowns: none. Complete, with client and target-routing tests.

## 5. Documentation and verification

Rationale: behavior must be documented and verified across real storage and interop.

Data: configuration, extension API and skills guides; existing Lazytest, pytest,
Companion and native-image suites. No paid model end-to-end calls are required.

Acceptance criteria: affected tests, formatting, lint/reflection, documentation links,
final diff review and native-image execution pass. Only task-owned files are committed.

Unknowns: none for the scoped implementation. Affected verification is complete;
full-suite CI reports its result on the committed revision.

## Plan state

- Implementation and documentation complete.
- Companion: 3,777 tests passed, 3 skipped; typecheck and lint passed.
- SDK: 1,011 tests passed, 43 skipped; Ruff checks passed.
- Engine/contract/docs: 971 affected tests passed before the final access fix.
- Final configuration/MCP/provider/gateway checks: 396 passed; loop/scoped: 604 passed.
- TUI: 539 tests passed; Clojure lint/reflection checks passed.
- Final GraalVM CE native build passed; all four native SDK boundary tests passed.
- Final Companion settings checks: 21 passed; typecheck and lint passed again.
- Post-push CI exposed six fixture/layering regressions. All 20 reproducing tests
  and 790 affected follow-up tests now pass; lint/reflection checks are clean.
- The follow-up GraalVM CE build and all four native SDK boundary tests passed.
- A full local JVM follow-up did not complete. It also exposed an ambient-provider
  fixture dependency and order-dependent failures outside this scoped follow-up.
- Documentation/link and diff checks passed. Publication and full-suite CI results
  are recorded against the commits delivering this plan and implementation.
- No product release or live gateway restart is part of this task.
