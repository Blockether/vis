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

# Recoverable turns and complete Activity history (#210–#212)

Retain completed work across provider failures and client reconnects.

## Context

At initial clean main `e3b24d2ca`, Activity drops operations at 128 rows and
sheds details to fit 64 KiB. Provider TTFT classification may have its owned
interrupt re-armed by Svar routing; failure during retry backoff can leave the
durable turn running. Existing liveness-marker repair does not settle that row.
Owners: `internal/activity/`, Python Activity collection in `internal/loop.clj`,
SQLite persistence, gateway routes, Companion and TUI Activity clients, and the
Svar router. Reject higher retention caps, indiscriminate interrupt clearing,
and reconstructing completed history solely from the live event ring.

## 1. Reproduce and repair provider recovery

- Rationale: an owned timeout must retry without swallowing genuine Stop.
- Data: #210 watchdog → router → interrupted retry backoff; isolated HTTP regression.
- Acceptance criteria: actual retry, bounded attempts, no unsafe replay, active goal
  on recovery; exact terminal cause on exhaustion; Svar and Vis integration checks.
- Unknowns: verify the published Svar coordinate before consuming it.

## 2. Settle durable failed turns

- Rationale: a dead worker must not reopen as running or hide saved iterations.
- Data: #211 persisted running row with 65 completed iterations after worker failure.
- Acceptance criteria: exact-once terminal persistence, closed live blocks, retained
  history and concrete error after new turn, reconnect and store reopen.
- Unknowns: reproduce the specific client symptom before attributing it to UI.

## 3. Retain and page complete Activity

- Rationale: response and rendering bounds must not be history-retention caps.
- Data: #212 deterministic row/byte loss in reducer and receipt projection.
- Acceptance criteria: durable per-invocation records, bounded keyset pages and
  client windows, complete search/copy, stable identities, grouped tail outcomes,
  safe redaction and restart coverage beyond 128 operations and 64 KiB.
- Unknowns: client paging integration and compound-form identity preservation.

## 4. Integrate and close out

- Rationale: independently passing pieces do not prove the end-to-end boundary.
- Data: regression failures, affected suites, formatting/lint/reflection and diffs.
- Acceptance criteria: review scoped changes, verify the published Svar pin, commit
  and push, complete the authorized product release, and update all three issues.
  Deployment and live service restarts remain outside this task.
- Unknowns: report concrete blockers rather than marking incomplete fixes resolved.

## Plan state

1. Reproduced routed TTFT/backoff failure on Svar 0.7.167 and a leaked pre-header
   cancel watchdog. Local fixes preserve timeout classification and real Stop.
   Svar 0.7.168 is published from `58b57042e7` with its full verification and
   release checks passing. Vis resolves that Clojars pin without a local override.

2. Worker catch persists terminal state before live completion with an exact-once
   running-state comparison. Goal halts store the valid `:complete` prior outcome.
   Regression tests preserve concrete errors and completed work; integration and
   source review are complete with the published Svar pin.

3. Activity retains every admitted invocation without total row/byte caps. Tests
   cover 300 calls, large grouped details, real Python, disk reopen, independent
   forks, redaction, tail outcomes, write errors and incomplete exports. Companion
   build/lint, targeted tests and 246 stories across 12 themes pass; browser review
   reaches operation 160 through paging and search. TUI passes all 1,974 cases;
   final HtmlTerminal review confirms honest search counts and single captions.
   Protocol 13 prevents older clients from silently dropping paged Activity.

4. All 5,598 JVM cases pass on published Svar 0.7.168, including an old-store
   Activity table/index upgrade and failure to read the final page without losing
   Python output or its original error. Companion passes 2,722 cases (two skips),
   TUI passes 1,974. SDK source passes 589 cases (13 opt-in skips), and the
   installed wheel passes all 602 against the fresh native engine over HTTP and
   stdio. Both native images build with the pinned GraalVM CE; all 37 native
   cases pass, including Activity history and isolated speech round trips. SDK
   artifact parity, metadata, docs, formatting and lint/reflection checks pass.
   The separate README-link regression is fixed in `a239d6eaa`. The original
   fixes were committed and pushed. The v0.2.1 draft release was blocked by Linux
   CI: an ANSI escape split between log pages and a missing project-runner pytest
   dependency. Both are reproduced and repaired. Follow-up checks pass: 146
   shell/raw-log cases, 40 Python-runner cases, all 38 native cases on fresh engine
   and TUI images, 621 SDK source cases (13 opt-in skips) and all 634 installed SDK
   cases, including HTTP and stdio integration. Formatting, lint/reflection,
   actionlint and diff review pass. Main CI 34693438404 is green after one
   unchanged-source retry of a macOS Maven classpath failure. Release v0.2.2
   (`4e492e067`) is published: workflow 34694984081 passes source verification,
   all three native targets, mobile and desktop packaging. SDK publication
   34697843244 is green. All 15 GitHub assets, both Clojars packages and the
   PyPI wheel/source distributions are public; downloaded SDK hashes match.
   Issues #210, #211 and #212 are closed and have final release updates.
   No live service was restarted.

# Privacy-safe screenshot gallery

Show Vis on iOS, desktop and in the terminal without exposing personal work.

## Context

README.md and resources/vis-docs/index.md show all nine iOS, desktop and TUI
screenshots in one gallery, without separate platform sections or descriptions.
Use production clients with newly created Fieldnotes example content. The demo
must have its own home, gateway, SQLite database and client storage; never use
or copy the current gateway, credentials, projects, sessions or tasks. GitHub
cannot run a JavaScript carousel, so provide one linked preview grid there and
one keyboard/touch carousel on the documentation site. Preserve concurrent work.

## 1. Capture fresh example screens

- Rationale: screenshots must be accurate and safe to publish.
- Data: production Companion and TUI renderers; a new isolated demo database.
- Acceptance criteria: three useful screens per platform, readable captures,
  synthetic content only, and no existing client storage or gateway discovery.
- Unknowns: native iOS automation availability; verify before selecting captures.

## 2. Embed one accessible gallery

- Rationale: let readers explore each client without overwhelming installation.
- Data: README, Getting Started, canonical docs assets and existing site tests.
- Acceptance criteria: one gallery with all nine screenshots, concise labels,
  full-size links, keyboard/touch controls, reduced-motion and no-JavaScript fallbacks.
- Unknowns: GitHub sanitization limits; previews must remain useful without scripts.

## 3. Verify and publish

- Rationale: publish only reviewed images and a working documentation site.
- Data: screenshot inspection, gallery interaction/link tests, docs build and lint.
- Acceptance criteria: scoped diff and privacy review, passing affected checks,
  commit/push and successful docs deployment; stop temporary capture machinery.
- Unknowns: concurrent commits and hosted deployment outcome.

## Plan state

1. Complete: nine reviewed production-client captures from a new isolated gateway,
   database, home, sessions and client storage. Native iOS used a fresh simulator.
2. Complete: one linked README preview grid and one accessible Getting Started
   carousel, with a consistent image frame for portrait and landscape screenshots.
3. Complete: 56 docs tests and 160 site tests pass, with formatting, lint,
   asset/link checks and desktop/mobile browser checks. GitHub shows one preview
   grid; the live docs show one nine-slide carousel with working controls.
   Published in `6cdcada18`; Docs run `34695148233` passed verification and deployment.
   The nine reviewed captures are unchanged. Temporary capture and preview
   infrastructure is stopped; no existing gateway or personal work was used.

# Full release end-to-end verification

Publish a complete release from verified sources, then test its public artifacts.

## Context

Published v0.2.2 contains the fixes for #210, #211 and #212. The user requests
all test suites, isolated Linux host validation, green CI and a full release.
Use the existing source, native, installed-SDK and editing end-to-end suites.
Do not restart the live gateway, weaken tests, move tags or publish partial assets.
Keep unreleased client-callback development and local-only runtime work outside this release.

## 1. Verify source suites

- Rationale: every repository-owned component must pass its canonical checks.
- Data: CI, Companion and Storybook, relay, docs, SDK and the editing harness.
- Acceptance criteria: passing full suites and applicable formatting/lint checks;
  distinguish environmental failures from reproduced product failures.
- Unknowns: model availability and any new test failures.

## 2. Verify public native artifacts

- Rationale: source tests do not prove downloaded binaries work.
- Data: macOS and Linux public engine/TUI bundles and installed PyPI SDK.
- Acceptance criteria: full native suites, real HTTP/stdio integration and version
  checks in isolated test environments; no live service changes.
- Unknowns: host availability and artifact-specific failures.

## 3. Repair and publish

- Rationale: the complete release must use exactly the verified source revision.
- Data: scoped regression fixes, version mirrors and canonical release workflow.
- Acceptance criteria: scoped commits, green CI, immutable matching version/tag,
  successful native/mobile/desktop gates, GitHub/Clojars/PyPI publication.
- Unknowns: concurrent main pushes and store processing.

## Plan state

1. Pre-cut complete: source CI 34709606682 passes all 18 jobs. The normal full
   Companion suite passes 2,743 tests with two existing skips across 283 files;
   gateway fixture transforms now load before timed cases without changing deadlines
   or module-reset isolation. Lint and build pass. Storybook passes 251 stories and
   contrast checks across 12 themes; all 1,984 TUI tests, relay and docs pass.
   SDK commit 4eceb78fc passes all 11 gates with 660 source and 679 installed-native
   tests, 61 docs tests and 161 site tests. Isolated Linux checks pass 44 SDK cases
   and root/non-root service smokes, with no live service changes.
   Editing E2E passes all 19 scenarios after one unchanged targeted retry for a
   model lookup error; the strict no-error gate remains intact.
2. Complete: public v0.2.3 engine and TUI bundles match release commit 6b9887854.
   Both macOS and Linux pass all 42 tag-matching native cases; Linux also passes
   17 focused issue regressions. Neither native suite reports reflection or boxed
   math warnings. The public PyPI wheel and sdist pass checksum verification;
   all 13 installed Python modules match the tag. The full macOS installed SDK
   suite passes 661 tests with five explicit skips: four paid-provider opt-ins
   and one Python 3.14-only case on Python 3.13. Linux passes 151 SDK integration
   and guide tests with four paid-provider skips. Public embedded pytest passes
   all 62 client tests. Fresh Maven Java/Clojure recipes both pass; the resolved
   Vis and vis-contract 0.2.3 JARs/POMs match public Clojars checksums. All temporary
   test processes are stopped; no live gateway changes were issued.
3. Complete: runtime 0.5.14 publishes the Path/Counter and embedded pytest signal
   fixes; four-platform CI and all release jobs pass. The final Vis pin passes 146
   consumer tests. Package closure repair 0ea20403a passes both strict candidate
   Maven Java/Clojure recipes; all 100 bundle tests pass after the 0.2.3 version sync.
   Tag v0.2.3 and version mirrors match release commit 6b9887854. Release workflow
   34711372067, source CI 34711370200 and SDK publication 34715267284 are green,
   including every native, mobile and desktop gate. All 15 GitHub assets and the
   Clojars/PyPI packages are public. Released-client docs now use PyPI and a
   dependency-only JVM project rather than the incomplete v0.2.2 workaround.
   Both Java/Clojure recipes also pass from that documented project; a reproduced
   missing-JitPack failure is covered by a passing repository-declaration regression.
   The docs follow-up passes 62 Clojure tests, 161 site tests, 37 SDK guide/README
   tests with four paid-provider skips, formatting and lint/reflection checks.
   Issues #210, #211 and #212 remain closed; #212 has the v0.2.3 verification update.
   New client-callback development and the explicitly local-only runtime gather
   fix remain excluded from this release.
