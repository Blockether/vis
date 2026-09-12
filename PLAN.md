# Tool correctness and efficiency audit

Fix reproduced tool defects before adding more prompt instructions.

## Context

The first prompt and Council contract pass was verified with 221 tests and pushed as `8aa1ae043`. The next audit reproduced shell auto-ID reuse, direct-host `gather` eager settlement, unbounded `cat` output and expanded continuation, and unnecessary live `doc` lookups. Patch concurrency remains a source-supported risk, not demonstrated data loss.

Owners are `internal/foundation/shell.clj`, `internal/foundation/editing/core.clj`, `internal/python/env.clj`, `resources/vis-guest/vis_introspection.py`, `internal/language/python/ruff.clj` and mirrored tests. The sibling `vis-python-runtime` owns gather classification. Council threads 914, 915 and 942 divide cat/lint, doc and runtime work; the coordinating session owns shell and integration. Preserve unrelated companion UI, release, Python REPL isolation and test-diagnostic work. Do not mirror runtime code, alter gather concurrency, restart services, run paid evaluations or publish the runtime as an incidental dependency fix.

## 1. Repair retained shell identity

- Rationale: a retained handle must not silently read a later automatic run.
- Data: sequential and concurrent reproductions; registry and persisted-log identity.
- Acceptance criteria: regression first, distinct automatic identities across finished and retired runs, preserved logs and live reattachment, affected tests and formatting/lint/reflection.
- Unknowns: resolved baseline coloured-diff failure: a single 16-KiB log page was compared to a complete diff; the test now follows the log cursor.

## 2. Repair gather, cat and doc contracts

- Rationale: enforce exception, output-bound and lookup guarantees in their owners.
- Data: existing audit reproductions and peer-owned regression suites.
- Acceptance criteria: direct gather slots settle through the dispatcher; cat counts UTF-8 and never emits a partial patch-ready line or expands the requested end; authoritative doc pages skip live imports while exact Python fallback works. Verify host/guest boundaries. Runtime changes stay local with no dependency-pin update.
- Unknowns: consumer verification against a local runtime override; precise safe recovery for oversized source lines.

## 3. Audit language tools and integrate

- Rationale: next inspect test verdicts, code freshness, formatting/lint safety and bounded results.
- Data: focused run_tests, repl_eval, format_code and lint_code contracts, runtime observations and existing suites.
- Acceptance criteria: report only evidenced defects or clearly labelled risks; combine affected checks, inspect scoped diffs and preserve concurrent work. Commit/push only safely separable verified Vis fixes under repository authorization.
- Unknowns: new concrete findings may require a separately scoped repair; no broad-build or model-cost claims.

## 4. Reject failed Python lint analyses

- Rationale: a configuration failure must never be reported as a clean file.
- Data: report 956 reproduces disk lint swallowing Ruff failure while snippet lint exposes it.
- Acceptance criteria: suite regression first; invalid configurations and analyzer failures propagate through existing errors, including multi-file selection; successful empty results remain valid. Run affected tests, formatting and lint/reflection.
- Unknowns: check final facade behavior and preserve independently owned Python core changes.

## Plan state

1. Complete locally: two retained-ID regressions failed before the fix, including real Python handles reading the wrong command through logs and wait. Automatic IDs now reserve existing entries and retained logs; explicit named restarts are unchanged. Tests cover finished and retired processes and live reattachment.
2. Cat and doc complete locally with regressions and host/guest coverage. Cat bounds the entire UTF-8 response without partial anchors or expanded continuation. Doc skips live imports for authoritative pages. Runtime gather passes 15 tests / 69 assertions and 66 Vis consumer tests using a command-line local dependency override; direct ordered-exception and default all-settle consumer checks also pass. The two runtime files stay local, with no runtime commit, publication or pin update; installed-pin workaround documentation remains accurate.
3. Combined shell/log/editing/env/presentation checks pass all 490 tests; formatting and lint/reflection are clean and diffs reviewed. Audit identified Python test-diagnostic truncation and conflicting formatter selectors; diagnostic repair and REPL isolation are separately owned work, excluded from this commit. Patch concurrency still needs deterministic reproduction. No managed REPL or temporary test process remains from this session; no paid evaluation or native-image test was performed.
4. Complete locally: Ruff analysis/configuration failures now propagate rather than producing clean results, including a failing later file in a batch. The 71-case Ruff/facade integration passes, giving 561 affected Vis cases across the two runs. Formatting and Clojure lint/reflection are clean. Ten scoped Vis files are verified for commit/push; unrelated work and local-only runtime changes remain excluded. No deployment or live service restart is part of this workflow.
