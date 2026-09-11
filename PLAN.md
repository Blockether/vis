# Council asynchronous work and documentation

Make wake useful for scoped work, not only immediate answers.

## Context

At main `54a903d9e`, Council validates required replies after every tool iteration as well as final answers. Session `29ac86ff-9058-4409-8b31-c3483f0cdb92` read a request and calculated before replying; both intermediate calls received validation errors. Owners are `internal/council/`, `internal/loop.clj`, gateway wake text and `resources/vis-docs/council.md`. Peers reviewed existing findings in Council thread 720. Reject automatic satisfaction loops, new task-state APIs and unrestricted wake chains. Existing user authorization, cancellation and holds remain authoritative.

## 1. Reproduce and correct the reply boundary

- Rationale: reading and bounded work can span invocations without ending the turn.
- Data: target t4/i1–i3 and the real Python/SQLite required-boundary suite.
- Acceptance criteria: intermediate tools succeed; unanswered final prose is rejected; replies still correlate and notify once.
- Unknowns: none after source review; retain runtime cancellation and wake coverage.

## 2. Align the work protocol and documentation

- Rationale: distinguish questions, work requests, progress, completion and follow-up.
- Data: Council prompt, host tool documentation, wake text and existing reply constraints.
- Acceptance criteria: clear authorized scope, acceptance criteria, final result/blocker and bounded review; no receipt-equals-completion claim. Mermaid message-flow and module diagrams render in static/live docs without a new runtime dependency.
- Unknowns: verify rendered diagram readability and asset serving.

## 3. Verify and publish

- Rationale: the loop and host boundary require behavioral tests, not wording alone.
- Data: affected Council, loop, gateway and docs tests; formatting/lint/reflection; generated site checks.
- Acceptance criteria: scoped checks pass, final diff reviewed, only task files committed and pushed with session trailer; report no live restart.
- Unknowns: concurrent release changes must remain unstaged.

## Plan state

1. Complete: read/research-before-reply regression failed, then passed with final-only gating. Same-thread follow-up regression failed, then passed using stored request/reply pairs; unrelated and transitive wakes remain blocked.
2. Complete: prompt/tool/wake/manual aligned; Mermaid sources and self-contained SVGs render in static/live docs. Browser review confirms loaded bundled fonts and no page overflow at 390 px.
3. Verified: 1007 affected JVM tests and 149 docs-site tests pass; Clojure formatting, lint/reflection, JavaScript formatting/lint and diff checks pass. Ready for scoped commit and push; no gateway restart, deployment or paid-model replay.
