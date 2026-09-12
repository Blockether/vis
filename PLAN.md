# Prompt and tool contract optimization

Reduce repeated instructions and tool output without weakening the contracts.

## Context

At main `9442bb200`, the core prompt has 8,474 characters and the always-on Council prompt has 8,631. Council pagination rejects values above 50, but its callable documentation omits that bound and errors do not identify the failed constraint. The host's `gather` discovery entry omits `return_exceptions`; that mode currently serializes host slots. Council results print full transport dictionaries.

Owners are `internal/context/prompt.clj`, `internal/council/{core,host}.clj`, `internal/python/env.clj`, `resources/vis-guest/vis_results.py` and mirrored tests. Council threads 868–870 divide implementation and independent review between sessions. Preserve concurrent UI/transcript and sharing work, including existing edits to `council/core_test.clj`. Reject runtime concurrency changes, automatic REPL reloads, cache-prefix reordering, upstream skill rewrites and unsupported performance claims in this pass.

## 1. Align callable contracts and diagnostics

- Rationale: avoid preventable failures and make recovery precise.
- Data: canonical Council schema, reproduced pagination failure, live `gather` signature and runtime behavior.
- Acceptance criteria: schema-derived field/constraint errors without echoed user content; documented pagination bounds and ordered exception behavior; regression tests across the host boundary.
- Unknowns: any contract gaps discovered by the focused tests.

## 2. Reduce prompt and result overhead

- Rationale: remove redundancy instead of relying on more instructions.
- Data: core/Council prompt baselines, result presentation hooks and existing contract tests.
- Acceptance criteria: all material safety, authorization, completion, wake and lifecycle rules preserved; prompt assembly unchanged; bounded Council representations retain identifiers, meaningful content, errors and cursors, with full raw data available.
- Unknowns: review may favor retaining wording where brevity would weaken the rule.

## 3. Integrate and verify

- Rationale: independently implemented changes need combined review and affected checks.
- Data: peer results, targeted JVM/guest-runtime tests, formatting/lint/reflection, measured text sizes and final scoped diff.
- Acceptance criteria: affected checks pass, regressions remain in suites, unrelated edits preserved, static reductions distinguished from unmeasured model quality/cost/latency. The user subsequently authorized a scoped commit and push; paid evaluation, deployment and live restarts remain outside this pass.
- Unknowns: decide the smallest later paired-model evaluation from the independent review; no such result is assumed here.

## Plan state

1. Complete locally: Council bounds/diagnostics and discovery docs covered by host-boundary regressions. `gather` signature/modes and the verified async-helper workaround are documented; its direct-await classifier defect remains separately recorded in Council report 896, without a runtime or dependency-pin change.
2. Complete locally: core prompt 8,474 → 8,017 characters; Council 8,631 → 6,727. Council result views retain raw mappings, IDs, cursors, content recovery, ping recipients and reply-state counts. Individual owner checks pass.
3. Verified: independent acceptance/presentation reviews and scoped diff review complete. The combined clean-JVM run passes all 221 affected tests, including embedded Python and jailed-worker coverage; formatting, Clojure lint/reflection, Python lint and diff whitespace checks pass. The fixed 50-entry long-page fixture shrinks from 1,011,201 to 4,472 printed characters with raw data unchanged. Commit and push are explicitly authorized after a fresh scope recheck. No paid model quality/cost/latency evaluation, deployment or live restart was performed.
