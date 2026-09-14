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

# JVM gateway and SDK dogfooding

Measure real workloads before optimizing their CPU and memory costs.

## Context

The gateway runs on the JVM and the Python SDK drives its HTTP and stdio
transports. Start from source revision `0c3d53f82`, use isolated engines and
existing SDK integration fixtures, and leave the shared gateway running.
The initial gateway baseline passes 365 tests; the project Python runner passes
666 SDK tests with 24 opt-in skips. Older GraalPython profiling is not evidence
about the current embedded CPython runtime. Avoid speculative caches, weakened
validation and heap-flag tuning without workload measurements. Preserve the
unrelated SDK plan and Companion review files already in the checkout.

## 1. Establish repeatable workload measurements

- Rationale: distinguish gateway overhead from provider latency and JVM warmup.
- Data: existing SDK HTTP/stdio fixtures, real provider tasks, process CPU/RSS,
  heap/GC and allocation profiles; idle, streamed and repeated-session workloads.
- Acceptance criteria: measured baseline, verified task results and isolated
  cleanup; persistent regression or opt-in workload tests rather than demos.
- Unknowns: dominant allocations and retained resources under sustained use.

## 2. Repair measured bottlenecks and correctness failures

- Rationale: optimize the owner of a reproduced cost or error, not a guess.
- Data: gateway event/state/transport paths and SDK client lifecycle.
- Acceptance criteria: failing regression before each fix, passing affected
  suites, and before/after measurements with unchanged workload semantics.
- Unknowns: concurrency, replay and cancellation interactions revealed by tests.

## 3. Verify combined behavior and resource recovery

- Rationale: a faster individual operation must not regress real agent tasks.
- Data: source suites, SDK integration, repeated load, formatting and lint,
  including Clojure reflection checks and a scoped final diff review.
- Acceptance criteria: real SDK tasks complete; CPU and memory results include
  warmup and measurement limits; temporary processes stop and unrelated work
  remains untouched. No release or live-service restart.
- Unknowns: environmental verification limits and residual measured costs.

## Plan state

1. Repeatable baseline established. SDK HTTP/stdio integration and paid-provider
   tasks pass, including invoice repairs whose four fixture unit tests fail before
   the task and pass independently afterward. The HTTP/stdio repair pair passes
   in 339.69 s without changing the protected tests or README.
2. Measured fixes implemented:
   - MCP reconnects no longer leak stdout descriptors (+20 for 20 reconnects
     before, zero growth after). Failed transport setup also reclaims its process
     and available streams. Regression coverage includes normal, timeout and
     setup-failure paths; explicit process types remove 14 reflection sites.
   - Raw Python/MCP JSON avoids a redundant 16 KiB writer buffer: identical small
     messages allocate 1,888 instead of 18,288 bytes. Fidelity, failed conversion,
     failure atomicity and concurrent framing have regression coverage.
   - Build-version lookup is process-local (6,248 to zero allocated bytes per
     repeated read). SQLite metadata omits the unused prompt checkpoint and
     reuses its fixed query. With a 1 MiB checkpoint, get-session allocation falls
     from 1,115,436 to 33,344 bytes; checkpoint retrieval remains unchanged.
   - Worker startup shares one runtime-source snapshot between confinement and
     imports. JVM hosts now prefer the selected runtime's packaged worker, with
     the Java entrypoint retained for source-only runtimes. Controlled real-worker
     samples show about 52 MiB RSS versus 164 MiB for Java, and 440–490 ms versus
     810–820 ms CPU. Host/environment/worker checks pass 143 tests.
3. Controlled combined measurement uses one immutable AOT jar and ABBA ordering,
   with 100 measured tasks after 50 warmups per run. The control restores only the
   old measured hot paths in that same jar. Mean main-process CPU falls from
   31.55 to 27.09 s (about 14%); sampled allocation falls from 5,320.9 to 4,404.9 MiB
   (about 17%). Collected heap remains about 108–109 MiB, and RSS is variable:
   there is no demonstrated retained-memory improvement in the main JVM.
   Worker CPU/RSS is excluded from those main-JVM totals. A second immutable-jar
   ABBA comparison isolates worker selection: 100 tasks after 50 warmups per run
   take 41.27 s with Java workers versus 28.07 s with packaged native workers
   (about 32% less wall time). Shared-worker RSS is about 146 versus 64 MiB;
   maximum sampled session-worker RSS is 199–210 versus 59 MiB. These are point
   samples, not peaks. Observed CPU totals are lower bounds because workers can
   exit before sampling; no exact whole-process-tree CPU reduction is claimed.
4. Boundary verification passes: a fresh GraalVM CE 25.3.4.1 native build completes,
   then eight real SDK HTTP/stdio agent, cancellation and MCP probes pass against
   both the source JVM and the built native binary. Three native Python module
   checks also pass. The shared-primitives invariant passes on rerun after its
   owner fixes the concurrent Improve changes (the earlier combined suite passed
   213 of 214 tests). No shared gateway was restarted.
5. Further SDK dogfooding repairs missing Improve endpoint methods and managed-team
   surface coverage. The project interpreter passes 691 SDK tests with 31 opt-in
   skips; changed Python files pass Ruff format and lint. Python test discovery
   now prunes nested dependency/build environments while preserving explicit roots;
   its two discovery cases and real pytest collection regression pass.
6. Logical request health no longer tokenizes a full-content attribution twice;
   61 prompt tests and 16 affected loop cases pass. A frozen-input, four-run ABBA
   comparison does not establish an end-to-end speedup: main CPU averages 22.68 s
   before versus 23.78 s after, with sampled allocation about 3.92 versus 3.95 GiB.
   Keep this distinct from the measured worker and earlier main-JVM improvements.
7. The retained CLI regression shows that `python -m` cannot run modules owning
   an `asyncio.run` loop. A local runtime-owned synchronous capture helper passes
   five bridge tests (50 assertions). A temporary local dependency/entrypoint
   override passes all 41 Vis CLI cases, including that regression and later
   top-level await. The same temporary JVM CLI override passes 691 SDK tests with
   31 opt-in skips, eliminating the four observed `asyncio.run` conflicts. The
   default Vis pin and entrypoint are unchanged; publication, pin integration and
   native execution remain unverified, not a shipped fix.
8. Mixed Clojure test selection now preserves whole files beside scoped vars.
   The affected suite passes 139 cases; an independent parent rerun passes the two
   regression cases, including exact identities in REPL-form and clean-JVM runs.
   The running host binding is unchanged; these checks load the edited source.
9. Budgeting and request health now share an iteration-local, exact Svar message
   counter. The unchanged reproduction passes; both complete prompt and loop suites
   pass 601 cases. The four changed files pass lint and reflection checks without
   warnings. Cache entries never enter persisted state or outlive the iteration.
   End-to-end performance measurement on a frozen JVM artifact remains pending.
10. Remaining work: combined SDK verification and controlled counter measurement.
    Earlier transient SDK startup failures and the native Python lint binding's
    FFM failure remain unclaimed. No commit, push or release is planned.

# Improve workspace draft

Make collected reports useful, project by project, with human or automatic review.

## Context

The existing Improve ledger retains immutable Council complaints and failed-tool
coordinates. It has no editable issue workflow or client entry point. Extend that
owner, keeping source evidence separate from Markdown analysis. Do not replay source
commands or treat reports as permission to change code. This is a local first draft;
no publishing, live service restart or paid model verification.

## Phases

1. **Persistent records and grouping**
   - Rationale: preserve intake while adding editable project issues.
   - Data: SQLite Improve ledger, new workflow records and portable contract.
   - Acceptance criteria: populated-store backfill, provenance, same-project acyclic
     groups, atomic descendant closure and stale-write protection have tests.
   - Unknowns: settle the record version and paging contract with gateway ownership.
2. **Governed review**
   - Rationale: people choose who writes analysis and which model is used.
   - Data: Off / Governed by human / Automatic; selected provider/model and interval.
   - Acceptance criteria: authenticated routes, bounded periodic project review,
     exact routing, safe cancellation and human-mode model restrictions are tested.
   - Unknowns: safe reproduction execution is unavailable; reports must state that
     reproduction was not attempted and provide an actionable plan.
3. **Companion and TUI**
   - Rationale: make issues accessible where the user works.
   - Data: conditional global Improve icon, C-x e, project issue lists and Markdown.
   - Acceptance criteria: editing, grouping, cascade-close confirmation and settings
     work through the same API; deterministic stories and real TUI renderer tests.
   - Unknowns: verify narrow-screen composition and chosen provider/model discovery.
4. **Integrated verification**
   - Rationale: a first draft must connect intake, persistence, review and both UIs.
   - Data: affected tests, formatting/lint/reflection, rendered previews and diff.
   - Acceptance criteria: required affected checks pass, unrelated changes are
     preserved and limitations are reported without claiming live model execution.
   - Unknowns: concurrent shared-checkout checks may expose unrelated failures.

## Plan state

1. Done — persistent records, immutable provenance, same-project hierarchy, atomic
   closure and optimistic versions; storage and contract tests pass.
2. Done — authenticated API, persisted modes, bounded rotating review worker and
   exact provider/model routing. Reproduction execution remains explicitly deferred.
3. Done — Companion and TUI use the canonical API, conditional entry points and
   persisted modes. Markdown editing, paste, grouping and cascade confirmation are
   covered; narrow layouts and the TUI's cell-aware caret were checked.
4. Done — 335 affected Companion tests, 66 Improve/util backend tests and 166 affected
   TUI/input tests pass. The full TUI suite passes 2,032 cases. Formatting, lint and
   reflection are checked; Companion builds and all 270 stories across 12 themes
   pass the contrast checks. Offline interactive HTML and rendered previews are attached.

Reproduction execution remains outside this draft: automatic reviews write analysis
and safe verification plans, explicitly marked not attempted. They do not run report
commands, apply code fixes or close issues. No paid model call, native-image run,
commit, push, deployment or live service restart was performed.

# Separate Python project and shared environments (#226)

Choose one dependency environment before Python starts.

## Context

The CLI currently activates `.venv` after loading shared Vis packages, which can
hide undeclared dependencies and retain shared editable hooks. The CLI owner is
`internal/main.clj`; interpreter and extension startup are in `internal/python/`.
Keep uv's normal project environment locations. Do not merge project dependencies
into `~/.vis/python/packages` or try to undo executed hooks by filtering `sys.path`.
Preserve concurrent worker-diagnostic and Companion changes.

## 1. Reproduce and select environments at startup

- Rationale: package hooks and cached imports make late path precedence insufficient.
- Data: CLI baseline passes 43 tests; current startup admits shared packages first.
- Acceptance criteria: project code cannot import shared-only wheels, editable
  roots or hooks; shared tools remain available through an explicit selection.
- Unknowns: extension-worker startup must receive the declared package location.

## 2. Verify host, CLI and extension boundaries

- Rationale: separate module globals do not isolate a process's import state.
- Data: clean-process regression tests, existing extension tests and native CLI tests.
- Acceptance criteria: synced/editable project packages work; undeclared shared
  packages do not; missing or incompatible environments fail with useful guidance;
  standard uv behavior and bundled SDK availability remain intact.
- Unknowns: native build coordination with the concurrent diagnostics work.

## 3. Document and publish

- Rationale: users need to know which environment a command selects.
- Data: affected tests, formatting/lint/reflection, documentation and scoped diff.
- Acceptance criteria: required checks pass, commit only these changes, push main
  and update #226 with the verified isolation follow-up.
- Unknowns: safely separating any concurrent edits before staging.

## Plan state

1. Implementation complete — CLI and declared extension workers select packages
   before interpreter startup. Project imports exclude shared packages and hooks;
   `--shared` explicitly selects general tools. No runtime or dependency-pin changes.
2. Verification complete — 134 affected host/CLI/runtime/uv cases and the clean-process
   CLI regression pass. The clean native build passes five isolation cases, four uv
   parity cases and locked-extension registration. Broad extension/worker suites pass
   147/148 and 53/54 cases; their remaining failures reproduce an existing disposed-DB
   assertion and concurrent, unpublished stack-diagnostic work. The changed worker
   entrypoint cases pass 4/4. Formatting and clj-kondo are clean; general reflection
   findings were checked against the baseline and warning-free JVM compilation.
3. Documentation and scoped diff reviewed. Publication: commit only this task, push
   main and report the verified follow-up on #226. No installation or live restart.
