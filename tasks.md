# Leader and subagent sessions

Task-scoped delegation, quiet coordination, and session-local routing.

## Context

The user authorized the complete implementation. Council currently groups sessions by
project/repository and can wake independent leaders. Companion unread marks count all
settled turns. Session forks already copy history and model preferences are persisted,
but forks have no supervised parent/child lifecycle. Existing unrelated changes,
including both PLAN.md files, must be preserved; this requested file owns this plan.

Rejected: project-wide automatic wakes, prompt-only permission checks, copying live
runtime handles, globally changing a provider to reroute one child, and treating all
Council output as a new answer to the human.

## 1. Quiet coordination and explicit leadership

- Rationale: background coordination must not demand human attention.
- Data: Council metadata, persisted turns, unread marks, session model preferences.
- Acceptance criteria: coordination does not create unread/push alerts; ordinary human
  results and explicit input requests remain visible. Leaders cannot wake other leaders.
- Unknowns: resolved by answer-count, terminal-provenance, unread and push regressions.
- [x] Reproduce the current unread and independent-leader wake behavior.
- [x] Persist leadership and enforce wake eligibility in the engine, not only prompts.

## 2. Managed forks and delegation

- Rationale: reuse session history and execution instead of building another agent engine.
- Data: fork snapshot, parent/root identities, delegated task, child execution and result.
- Acceptance criteria: a child inherits the parent's current model context at a safe
  checkpoint, gets an explicit task and fresh runtime, remains inspectable, and returns
  results to its parent. Stop/held states and delegation limits are enforced.
- Unknowns: resolved by real parent-fold inheritance, fresh-runtime and lineage regressions.
- [x] Add spawn/list/inspect/cancel lifecycle and safe snapshot inheritance.
- [x] Scope automatic wakes to explicit parent/child teams, never leader-to-leader.
- [x] Preserve shared-workspace semantics and make edit conflicts visible.

## 3. Session routing and public APIs

- Rationale: the coordinator and child need bounded, independent model control.
- Data: durable per-session preferences; shared provider infrastructure.
- Acceptance criteria: a parent controls its children; a child controls itself within
  inherited constraints; changes apply at a model-call boundary and are observable.
- Unknowns: resolved by gateway/SDK validation and real next-request/retry routing regressions.
- [x] Implement routing controls and permission/budget validation.
- [x] Update canonical contracts, engine SDK, extension API and observed tool bindings.

## 4. Prompt and documentation

- Rationale: leadership, delegated responsibility and new wake rules must be unambiguous.
- Data: actual runtime API and inherited context, not speculative examples.
- Acceptance criteria: leader plans/delegates/verifies/integrates; child follows its
  delegated task rather than resuming inherited unrelated work; examples are executable.
- Unknowns: none beyond the implemented APIs.
- [x] Update system prompts, Council manual, SDK and context guidance.
- [x] Remove obsolete independent-session wake promises and consumers.

## 5. Companion and TUI

- Rationale: one user task should show a team, not many unrelated conversations.
- Data: session lineage, current state/model, unread-human-update counts.
- Acceptance criteria: header summarizes child work; children link back to their parent;
  users can inspect and stop work; no coordination-only new badges.
- Unknowns: physical mobile devices and native-image execution were not verified.
- [x] Add team/header/navigation controls using existing components.
- [x] Cover loading, empty, running, blocked, failure and completion states.
- [x] Verify production rendering and interaction in both clients.

## 6. Verification and completion

- Rationale: this changes host, persistence, SDK and client boundaries together.
- Data: regression tests, affected suites, formatting/lint/reflection and final diff.
- Acceptance criteria: applicable checks pass, unrelated work is preserved, and any
  concrete blocker or unverified platform is reported without claiming completion.
- [x] Run affected Clojure, Python, Companion and TUI tests.
- [x] Run formatting, lint/reflection, client build and Storybook checks.
- [x] Review the complete scoped diff and update this task state.

## Plan state

Implementation and local verification are complete. Independent leaders cannot wake one
another; managed teams inherit safe checkpoints, enforce ownership and durable budgets,
route at request boundaries, and remain inspectable without coordination-only alerts.

Verification: the final engine/host/loop suite passes 595 tests. Earlier gateway/Council/
persistence/model/fork checks pass 553 cases, with 147 persistence/agent cases verifying
atomic admission and resumption. Real sandbox regressions cover parent folding, fresh
child runtime, inherited context and routing reversion across retries. Clojure formatting,
lint/reflection and final scoped diff checks pass without findings.

SDK/guide checks pass 99 cases with 13 opt-in real/native-engine skips. Companion passes
23 affected tests (plus the final 9-case rerun), 277 Storybook cases, the all-story/all-theme
contrast audit, typecheck, compiler lint and build. TUI passes 250 rendering/interaction
cases and 428 final screen/state/team cases, including autonomous header refresh,
one-frame invalidation and recovery of incremental rendering.

Model transport is mocked in boundary tests. No paid-provider integration run, native-image
rebuild, physical mobile-device check or 130% text-scaling check was performed. No commits,
pushes or live service restarts were made by this task. Unrelated shared-checkout work is
preserved.
