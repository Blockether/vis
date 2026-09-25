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
Owners: `internal/activity/`, Python Activity collection in
`internal/loop/python_exec.clj`, SQLite persistence, gateway routes, Companion
and TUI Activity clients, and the Svar router. Reject higher retention caps,
indiscriminate interrupt clearing, and reconstructing completed history solely
from the live event ring.

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

# Searchable session helpers

Reuse and refine helpers without expanding the session catalogue into the context.

## Context

The runtime's `resources/vis-python/async_runtime.py` renders every helper with
unbounded default representations and catalogue-wide padding. A measured
86-helper listing reached 13.8 million characters, mostly padding (Improve 4353).
Vis owns discovery metadata in `internal/python/env.clj`, the prompt and guides.
Keep exact source lookup and ordinary Python bindings; reject automatic garbage
collection, archival storage and automatic extension promotion. Preserve unrelated
gather, worker diagnostics, Windows runtime and Companion work.

## 1. Bound discovery and expose advisory details

- Rationale: searching a small index must not evaluate or display default values.
- Data: synthetic reproduction, historical listing sizes and peer reuse examples.
- Acceptance criteria: paginated regex search, bounded type-only call hints, exact
  source lookup, bounded errors, source fingerprint and value-free global hints;
  runtime regressions fail first and pass after the implementation.
- Unknowns: source analysis cannot prove dynamic dependencies or handle liveness.

## 2. Integrate the helper lifecycle

- Rationale: bounded output needs matching discovery, reuse and cleanup guidance.
- Data: host worker/non-worker restoration tests and stable-name/alias experiments.
- Acceptance criteria: matching metadata, actionable reader documentation and
  prompt rules; refine stable names, explicitly delete obsolete helpers only after
  checking dependencies, propose proven reusable capabilities to Improve with
  source-version-specific evidence. No new lifecycle manager or extension.
- Unknowns: concurrent local-only changes must remain outside staged hunks.

## 3. Verify and publish the scoped changes

- Rationale: Vis must actually consume the changed runtime.
- Data: affected runtime and pinned-consumer suites, formatting, lint/reflection,
  generated dependency audit and final scoped diffs.
- Acceptance criteria: commit and push runtime changes, update Vis' runtime pin,
  verify the pinned dependency, then commit and push only the Vis task's changes.
- Unknowns: unrelated local-only work may fail broader suites; no service restart,
  native build, release, version bump or tag is authorized by this plan.

## 4. Keep the prompt to policy

- Rationale: three of seven helper prompt lines described source fingerprints and
  Improve proposals, a rare workflow paid for in every request, while no surface said
  how redefinition and deletion change the saved definitions.
- Data: prompt character budget, the four documentation surfaces and the host
  persistence tests.
- Acceptance criteria: the prompt keeps search, stable-name refinement, saved-set
  semantics, explicit deletion and the post-restart dependency check; fingerprints
  and Improve proposals stay on `doc("defs")` and the guide; prompt and host suites pass.
- Unknowns: the runtime docstring repeats the advisory limits until the next runtime
  release.

## Plan state

1. Complete locally: regression reduced a 241-helper catalogue from 57,855,102
   characters to below 6,500. Runtime and source/protection suites pass 31 tests /
   119 assertions; defaults and type-name hooks are not evaluated. Ruff and
   Clojure formatting/lint/reflection pass.
2. Complete: six host cases pass with the exact published runtime dependency,
   including worker/non-worker restoration and deletion. The prompt suite passes
   67 cases and the dependency audit passes six. Documentation and metadata agree;
   existing reflection warnings remain outside the changed code. Native Python
   lint uses the working Ruff CLI fallback because the running tool image lacks a
   downcall registration.
3. Runtime `0213437658bb85441c1f79f90fec7ff720a68dd7` is committed and pushed.
   Vis pins that exact revision and includes the regenerated audit. Pinned-consumer
   verification and scoped diff review are complete; concurrent work stays excluded
   from this task's Vis commit.
4. Complete: the prompt drops the fingerprint and Improve lines and states how saved
   definitions follow redefinition and `del`; `doc("defs")` and the guide carry the
   same contract. Prompt and host suites pass; the Vis commit is pushed.

# Flat transcript

Keep the Vis look, remove everything that is not paper, ink and space.

## Context

The target is one monospace face,
square corners, three paper steps from the shared palette, two ink levels, a
six-step spacing scale, a 60–80 character prose measure, no shadow in flow and one
solid offset in the theme's `dialog-shadow` ink on floating layers only. Measured in Storybook (blockether-light,
1280×800, `components-iteration-trace--*` and `transcript-activity-live-views--settled`):
eight spacing values in use (4, 6, 8, 10, 12, 16, 20, 24px), sibling gaps of
8/10/16/20px, up to four text sizes, two surface tones beyond the page, one in-flow
shadow on the merged-results grid and a 720px text block (about 92 characters at
13px). Offset shadows came in five sizes (2, 3, 4, 6, 8px) across thirteen source
files, two of them grey (`--line2`) on the select and desktop menus, plus a soft
`shadow-sm` on the navigator tab and `shadow-lg` on the PDF page. Rejected: a
sans-serif body, pills, soft shadows, gradients and a smaller thinking size; borrow
space and restraint from ChatGPT/Codex, not chrome.

## 1. One floating-layer shadow, none in flow

- Rationale: five sizes and an in-flow shadow contradict the flat rule and cost more
  than they signal; the `dialog-shadow` offset (amber in Blockether light, near-black
  in dark themes, the TUI's dialog shadow) stays the signature of menus and sheets.
- Data: every `shadow-[` site in `apps/vis-companion/src`, the story audit counts
  before and after, both densities and the dark themes.
- Acceptance criteria: the results grid, queued-turns tray, composer, PDF page and
  navigator tab cast no shadow; menus, popovers, sheets, dialogs, the select menu,
  the voice status and the jump button use `shadow-float`; `index.css` owns the
  token once as `--shadow-float: 4px 4px 0 var(--dialog-shadow)`; `ui.test.tsx`
  rejects every other shadow utility; the audit reports zero in-flow shadows.
- Unknowns: an 8px offset on sheets may have carried legibility in dark themes;
  measure before and after.

## 2. Spacing scale and prose measure

- Rationale: off-scale gaps read as noise, and a 92-character line is hard to read.
- Data: the rhythm audit per story at 1280×800 and iPhone 14, the `message-spacing`,
  `prose-spacing` and `exchange` stories, `ChatContent.layout.test.tsx`.
- Acceptance criteria: transcript layout gaps use only 4/8/12/16/24px, 16px between
  iterations and 24px between turns; desktop prose measures at most 80 characters
  while code, tables and diffs scroll; the audit shows the reduced value set; layout
  tests and stories are updated.
- Unknowns: the column width that serves both prose and 80-column code; whether touch
  keeps the full scale or one step less between turns.

## 3. Collapse finished iterations

- Rationale: the largest single noise reduction; every finished iteration currently
  shows three bands with previews.
- Data: iteration rendering in `ChatContent.tsx` (`REASONING_PREVIEW_LINES`,
  `Disclosure`, `BandLabel`, `BandTally`), `ActivityPanel` receipts, TUI folded lines
  in `apps/vis-tui/src/com/blockether/vis/tui/render.clj`.
- Acceptance criteria: a finished iteration renders one summary row with steps,
  duration and failures and opens on demand; the live iteration stays open; failures
  and cancellations remain visible while collapsed; the TUI applies the same rule;
  collapsed, expanded, failed and cancelled states are tested and have stories.
- Unknowns: whether an opened iteration stays open across reloads; how the collapsed
  receipt interacts with copy actions and search.

## 4. Ink and surface audit

- Rationale: the palette has two ink levels, yet components add `opacity-*` tints
  and verbatim blocks combine a surface with a strong `--code-border` line.
- Data: `opacity-*` (42 sites), `border-code-edge` (9 sites in `ChatContent.tsx`),
  per-story audit counts across the twelve bundled themes.
- Acceptance criteria: text uses the two ink tokens or a state ink, never opacity;
  verbatim blocks are surface-only and tables keep inset grid lines; the audit shows
  at most one border per group; contrast holds at 4.5:1 in every theme.
- Unknowns: gateway themes that map `--dim` and `--dialog-hint` to different values.

## Plan state

1. Complete: `--shadow-float` in `index.css`; eight floating layers use it, five
   in-flow shadows removed. `ui.test.tsx` rejects any other shadow utility.
   Production Storybook checks confirm 4px offsets in light and dark themes and
   no queue shadow, with touch and desktop behavior preserved. The 253 shared
   control tests and 370 Storybook tests pass; lint and the application build pass.
   All 370 stories across 12 themes pass contrast and icon-frame checks, with
   3,384 desktop hover checks passing.
2. Not started; measure both densities before changing widths.
3. Not started; the largest behaviour change, confirm before implementing.
4. Not started; follows 1–3 so counts measure the settled layout.

# Bounded generated tool schemas (#234)

Describe each model once; retain full type metadata for focused inspection.

## Context

`packages/vis-agent/src/blockether/vis/extension.py` owns `_contract_doc`. Its
previous `_contract_field_docs` recursively repeated every field path, including
equivalent union branches and reused nested records. #232 corrected authoring
guidance but did not change this rendering. The portable `.contract` and SDK
`Catalog.spec()` already expose complete metadata; do not add a second schema,
weaken annotations, or truncate callable semantics to shorten generated output.
Existing unrelated worker, companion and native-test changes remain outside scope.

## 1. Reproduce and define the compact representation

- Rationale: short author docstrings cannot remove generated repetition.
- Data: SDK symbol/catalog tests and typed union, shared-record, recursive, deep
  and same-name fixtures; current generated output and token counts.
- Acceptance criteria: failing regressions demonstrate duplication and unbounded
  field expansion, while metadata and omission semantics remain unchanged.
- Unknowns: exact savings for the representative E2E fixture before the fix.

## 2. Render bounded, reusable model definitions

- Rationale: the same schema should cost context once, not once per path.
- Data: registered signature/type metadata; model definitions shared across
  parameter and result schemas, with a 2048-character generated-schema budget.
- Acceptance criteria: deterministic deduplication and abbreviation, useful type
  descriptions, distinct same-named shapes, complete call envelope and an explicit
  path to full nested `.contract`/`Catalog.spec()` details. Document the boundary.
- Unknowns: recursive reference equivalence and per-use annotation descriptions
  must not merge different schemas or discard semantic notes.

## 3. Verify the real host and model workflow

- Rationale: smaller local strings do not prove sandbox discovery or real calls.
- Data: real extension-host registration/doc/invocation tests, existing #232 E2E,
  a new complex-schema E2E and before/after token measurements. Real-model traces
  exposed full-contract dumps after compact discovery, so the consolidated rule in
  `src/com/blockether/vis/internal/context/prompt.clj` requires filtering before
  printing. `e2e/run.py` now checks an optional per-form stdout budget, with tests
  in `e2e/test_run.py`; the schema scenario allows at most 6,000 characters.
- Acceptance criteria: compact and full inspection both work across the worker
  boundary; two available model routes pass without fallback, source reads or
  copied call-shape hints; focused tests, formatting/lint and scoped diff pass.
- Unknowns: availability of a second provider/model route; report failures and
  measurement method rather than infer a universal model-quality gain.

## Plan state

1. Baseline SDK checks passed (49 tests); five new regressions reproduced the bug.
   Fixture baseline: 21,389 characters / 3,473 estimated o200k_base tokens for monitor.
2. Named models, the 2,048-character schema budget and qualified attribute notices
   are implemented. SDK: 820 passed, 38 skipped; complete metadata is unchanged.
   Monitor doc: 2,145 characters / 621 estimated o200k_base tokens, about 82% fewer
   tokens than the fixture baseline. This measures doc text, not total provider cost.
3. Complete: host-boundary, docs and prompt tests pass (137 latest Clojure tests),
   plus 34 E2E-runner/fixture tests and 29 subtests. Python formatting/lint passes;
   Clojure lint has no errors and only four unchanged reflection warnings.
   Final schema E2E passes on OpenAI GPT-6 Astra and GLM-5.3 Flash, both on the
   requested routes, without errors or fallback. Traces confirm the atlas-only
   call precedes the all-cards call, followed by one monitor call and focused leaf
   metadata inspection. Maximum form stdout is 2,408 / 2,635 characters respectively,
   below the 6,000-character gate. The #232 OpenAI regression also passes.
   Earlier runs exposed full-contract dumps, dictionary/attribute confusion and
   sandbox tuple-identity assumptions. Qualified notices, an explicit dictionary
   and sequence boundary, and one consistent filtered-output rule address them;
   the core prompt remains within its unchanged 9,700-character ceiling.
   Scoped diff review is complete. No live-service restart, release, worktree or
   unrelated change is included.

# Signature-first discovery across host calls

**Phrase:** Inspect the call shape before requesting semantic documentation.

## Context

The core prompt still prefers `doc(name)` for unknown arguments. Live inspection
shows useful signatures for `ls` and `cat`, but generic options dictionaries hide
registered keys such as the required `kind` in `council.publish`. The shared owner
is `src/com/blockether/vis/internal/extension/core.clj`: `symbol-signature` already
feeds sandbox wrappers and documentation from registered call metadata. Python
extensions already supply a portable, masked signature; preserve that authority.
Do not duplicate metadata in a new inspection tool or alter dispatch merely to
make an inspection string look complete. One Python signature cannot describe
both required keyword keys and every options-dictionary overload; show the
canonical named call, preserve existing overloads, and document that boundary.

## Phases

1. Expose registered host option keys through ordinary signature inspection.
   - Rationale: avoid full documentation for parameter names and requiredness.
   - Data: existing `:call`, `:arglists`, `:params` and portable Python contracts.
   - Acceptance criteria: required keyword-only parameters are visible, unknown
     defaults stay masked, live registry signatures parse, and existing positional
     and dictionary invocations still work through the real sandbox boundary.
   - Unknowns: which host handles need additional metadata beyond this shared owner.
2. Consolidate a compact signature-first decision matrix in the existing prompt.
   - Rationale: inspect only the unresolved fact, without repeating known contracts.
   - Data: current discovery rules and the unchanged 9,700-character core ceiling.
   - Acceptance criteria: keep semantic preconditions, narrow lookup fallback and
     filtered schema inspection; enforce the matrix and ceiling in prompt tests.
   - Unknowns: real-model compliance with the revised decision rules.
3. Cross-validate inspection and invocation, then review the scoped diff.
   - Rationale: smaller signature strings must lead to correct actual calls.
   - Data: focused Clojure/SDK tests, real sandbox integration, relevant model E2E,
     formatting and lint including reflection; preserve unrelated worker edits.
   - Acceptance criteria: built-ins and Python extension functions/methods inspect
     and execute correctly; report model traces, checks and remaining limits.
   - Unknowns: available provider routes and any unrelated verification failures.

## Plan state

1. Implemented registered host option keys, closed call shapes and masked defaults
   in the existing signature owner. Existing positional and dictionary dispatch is
   unchanged. Real local/worker inspection covers the live registry and shell methods.
2. Reproduced stale inspection after an extension changed its signature on reload.
   The shared host metadata setter now invalidates only the managed callable's
   signature prototype; the existing runtime stamper rebuilds it. Real reload and
   twice-set signature regressions pass, including retained references and doc/keys
   restamps. No SDK/runtime duplication or sibling changes were needed.
3. Consolidated an ordered five-row discovery matrix. Reuse includes the system
   prompt, prior work and recovered context; known facts skip apropos, docs and
   signature inspection across turns, reloads and repeated calls. Only evidence of
   changed contracts justifies refresh. Semantic discovery names a concrete missing
   fact, not a general contract preflight. Searches stay in the known namespace;
   result inspection reuses stored values without whole-value fallbacks, and full
   contracts are traversed in memory rather than printed as schema branches.
   The core prompt is 9,693 characters, below the unchanged 9,700-character ceiling.
4. All 258 affected Clojure tests pass together, including the full extension-core
   suite, prompt, docs, introspection, agent, reload and real sandbox boundary tests.
   Fixed the reproduced doc-metadata failures: draft-diff's short parameter note and
   named result keys for draft-diff and managed agents. All 66 SDK/runner/fixture
   tests pass. Clojure formatting and Python Ruff checks pass. Clojure lint has no
   errors or findings on changed lines; 31 existing reflection warnings remain
   elsewhere. Documentation content/link checks and the scoped diff check pass.
5. Final source-gateway E2E passes both scenarios on both requested routes with no
   tool errors: OpenAI Codex/gpt-6-astra peaks at 193 and 3,158 output characters;
   zai-coding-plan/GLM-5.3-flash peaks at 192 and 3,208, all below 6,000. Both use
   signature inspection and focused deep-contract lookup, preserve the required
   call order and receipts, and reuse discovery across repeated calls. Earlier
   failures drove the explicit preflight, result-reuse and schema-branch rules;
   the final measurements verify these runs, not universal model compliance.
6. Implementation and verification are complete, with explicit commit/push
   authorization. Only task-owned changes and the signature metadata setter hunk
   in python/env.clj belong to this follow-up. Unrelated worker, activity and issue
   reporting work stays excluded. No gates were bypassed. Issue #232 remains closed
   from its earlier fix. Temporary test processes and REPLs have stopped; no live
   service restart or release was performed.

# Discovery evaluation hardening

Measure real workflow correctness and distinguish token use from output size.

## Context

The GLM audit found false passes in `e2e/run.py`: failed Activity snapshots were
ignored, answer/form substrings were insufficient, and token/cache totals were not
reported for discovery. Keep the two complementary fixtures and add one known-
contract reuse case; do not change the production prompt to train to the tests.

## Phases

1. **Rationale:** reproduce evaluator gaps cheaply before paid runs.
   **Data:** existing runner and fixture tests; saved OpenAI/GLM traces.
   **Acceptance criteria:** negative tests reject caught failures, wrong arguments/
   order/answers, redundant discovery and invalid accounting.
   **Unknowns:** absent versus provider-reported optional reasoning tokens.
2. **Rationale:** make hard correctness gates independent of efficiency metrics.
   **Data:** terminal Activities, private fixture journals, exact JSON answers,
   persisted usage and provider totals.
   **Acceptance criteria:** exact workflows, known-contract reuse, token/cache and
   stdout summaries, bounded discovery; no fixed cache-hit-rate requirement.
   **Unknowns:** model adherence under the stronger checks.
3. **Rationale:** verify the evaluator and the real behavior before publishing.
   **Data:** affected Python tests, Ruff, source-gateway runs on OpenAI and GLM.
   **Acceptance criteria:** evaluator regressions and code checks pass; benchmark
   runs retain their real correctness/error verdicts. Review, commit and push only
   task-owned files, preserving concurrent changes. A model failure must not be
   hidden by weakening the evaluator or changing the production prompt.
   **Unknowns:** model error rate under the stronger checks.

## Plan state

1. Baseline: 34 tests and 29 subtests passed. New negative tests reproduced ignored
   failed Activities, misleading JSON/substrings, wrong/missing/extra calls,
   duplicate discovery, invalid cache totals and malformed token payloads.
2. Terminal Activity updates now distinguish successes, failed/cancelled calls,
   unfinished calls, surfaced errors and failures without same-form errors. Missing
   scopes stay unclassified. Exact JSON answers and fixture invocation journals
   independently check facts, arguments, defaults, counts and order.
3. The bounded syntax audit recognizes aliases and literal loops/comprehensions,
   rejects comments/string-only evidence and hidden helper discovery, and detects
   repeated lookups. It is not a general execution tracer or security boundary.
   The added supplied-contract fixture verifies reuse without copying its source;
   it does not claim to test multi-turn memory retention.
4. Provider input/cached/uncached/output totals, optional reasoning, persisted cache
   reconciliation, model calls, forms, wall time and peak/cumulative stdout are
   reported separately. Repetitions retain all outcomes and report min/median/max;
   cache shares use summed counts, not averaged percentages. No fixed cache rate.
5. All 63 affected Python tests and 65 subtests pass. Ruff formatting/lint and scoped
   diff checks pass. The sandbox runner cannot import the repository namespace;
   CLI pytest with `PYTHONPATH=.:packages/vis-agent/src` is the verified path.
   Disk exhaustion briefly refused one patch without changing the file; work
   resumed after space returned, without deleting other sessions' artifacts.
6. Completed source-gateway benchmarks: OpenAI Codex/gpt-6-astra passes 3/3;
   GLM-5.3-flash passes 2/6 across two repetitions of each scenario. Both GLM
   known-contract runs pass without rediscovery. The other runs retain two surfaced
   serialization errors, skipped signatures, one repeated lookup and schema output
   over the peak/cumulative limits. Exact final facts alone no longer imply a pass.
   GLM totals: 233,779 input tokens, including 155,712 cached and 78,067 uncached;
   13,463 output tokens. All six persisted input/cache/output totals and sample
   counts reconcile. Raw traces and every verdict remain in the benchmark output;
   these measurements do not establish universal model compliance or minimal cost.
7. The interrupted implementation was copied exactly into the session-owned
   `discovery-evaluator-hardening` draft; unrelated checkout changes were excluded.
   All 63 tests and 65 subtests were rerun successfully there, with Ruff and diff
   checks clean. The completed paid runs are retained rather than repeated to
   seek a passing score. No production prompt/runtime/SDK changes, native builds
   or live restarts were needed for this evaluation work.
8. Approval created implementation commit `72583ebfe`, then correctly refused to
   overwrite the original checkout's overlapping paths. Copying earlier edits into
   a clean draft did not remove their originals; retrying alone could not resolve
   that collision. The 11 original task files are now preserved byte-for-byte in
   the task-only `vis-recovery-c5511b96-discovery-evaluator-originals` Git stash.
   Ten matched the committed draft exactly; only this plan had newer results in
   the draft. Unrelated files, modes, index state and existing stashes were retained.
   The draft was synchronized with current `main` without rewriting history.
   All five existing approval-safety tests pass, including overlap refusal,
   committed-draft retry, unrelated-work restoration and commit-hook vetoes.
   Recovery leaves the overlap guard intact and uses normal draft approval for
   the fast-forward, restoration and non-force push; the original-file backup stays.

# Multi-repository drafts and recovery (#241–#243)

Keep one task isolated across its selected repositories, with a reviewable and
recoverable path to approval.

## Context

The single-root selector in `74d8c5916` does not create one draft spanning several
catalog repositories. `workspace/core.clj` already persists extra clones, but their
seeding, review and approval lifecycle is incomplete. Issues #242 and #243 describe
the same copied-pending-work incident and a missing supported synchronization
operation; #241 separately requires draft identity and live counts in the footer.
The opt-in default from `8be8e9ac4` is preserved, not treated as a recovery fix.

Owners are `internal/workspace/{core,drafts}.clj`, `internal/foundation/drafts.clj`,
the sandbox/worker policy boundary, `internal/gateway/state.clj`, TUI footer refresh,
and their tests. Council consultations 5916, 5918, 5925 and 5930 distinguish known
lifecycle gaps from an unproven historical resume defect. Existing review snapshots
remain immutable; shared roots never enter a task diff. Do not infer ownership from
matching filenames, bypass hooks, rewrite Git history, or restart the live gateway.

## 1. Select and create a repository group

- Rationale: every selected repository must receive the same isolated lifecycle.
- Data: catalog permissions, persisted extra-root metadata and real worktree/Rift fixtures.
- Acceptance criteria: validate all selections before mutation; seed and capture each
  clone; fail atomically; remap aliases and preserve unselected/restricted roots.
- Unknowns: real worker confinement after a mid-turn workspace change.

## 2. Review and recover every participant

- Rationale: copied source work and task changes have different ownership and review scopes.
- Data: #242/#243 sequence, immutable per-root checkpoints and commit-hook tests.
- Acceptance criteria: multi-repository review/status, supported conflict-safe sync
  with continue/abort, all-target preflight, truthful partial publication and safe retry.
- Unknowns: additional recovery cases exposed by real Git regressions.

## 3. Enforce isolation and show draft state

- Rationale: a workflow prompt alone cannot prevent shared writes or explain current state.
- Data: raw Python/worker, shell and host-edit boundaries; gateway workspace polling.
- Acceptance criteria: enabled-mode prerequisite across writers and resume, off mode
  unchanged; stable footer identity and live aggregate modified/created/deleted counts.
- Unknowns: safe integration with existing uncommitted Python worker changes.

## 4. Verify and close out

- Rationale: a multi-repository task is complete only when every participant is accounted for.
- Data: regression suites, boundary checks, formatting/lint/reflection and scoped diffs.
- Acceptance criteria: preserve foreign work, commit/push only verified task changes,
  then summarize and close resolved issues with evidence. No installation or live restart.
- Unknowns: backend-specific failures, if reproduced, may require a separately verified Rift fix.

## Plan state

1. Research and consultation are complete. The copied-pending incident is covered
   by supported synchronization; historical toggle timing remains unknown.
2. Multi-repository creation, task review, guarded synchronization and recoverable
   approval are implemented. Original checkout changes remain protected.
3. The combined affected engine suite passes 688 tests; TUI identity/count refresh
   passes 52, and draft prompt coverage passes 2. Real jailed-worker cases cover
   remapping, retained handles, copied caches, discard and session restoration.
4. One-shot YAML toggle hydration passes 83 CLI tests, including default OFF and
   override restoration without persistence. The fresh pinned GraalVM CE image
   passes five native cases covering multi-repository confinement, review
   checkpoints and embedded Python.
5. The prospective scoped environment passes 26 API/worker cases with unchanged
   HEAD worker sources, without overwriting unrelated working changes. Formatting,
   scoped lint/reflection and all 17 documentation links are checked.
6. Three broader Python failures reproduce with the captured pre-task environment.
   Broader verification also reports unrelated TUI geometry and foundation/Council
   failures. No installation, release or live gateway restart is included.

# Non-Windows runtime and complete product release

Release the verified Linux/macOS runtime, then publish Vis only after every
supported product gate passes. Keep Windows runtime and desktop publishing disabled.

## Context

The runtime checkout has unreleased reliability changes and separate Windows
experiments. Vis has unrelated pending worker diagnostics that must remain intact.
`vis-python-runtime/.github/workflows/` owns runtime artifacts; `.github/workflows/`
owns Vis CI and product publication. Windows desktop steps are already commented.
Do not restore stashes wholesale, move published tags, bypass tests or install the
new binary into the running gateway.

## 1. Prepare and release the runtime

- Rationale: Vis must consume an immutable, published runtime with verified archives.
- Data: current runtime source, preserved reliability stash, platform CI and release assets.
- Acceptance criteria: scope non-Windows fixes, run JVM/native worker checks, disable
  Windows jobs, publish a new version and verify all Linux/macOS archives plus the JVM jar.
- Unknowns: outstanding reliability failures and whether the current version tag exists.

## 2. Make supported CI green

- Rationale: a release must not hide failing SDK, engine or app behavior.
- Data: GitHub job diagnostics, local regressions and the new runtime pin.
- Acceptance criteria: fix reproduced failures, regenerate the dependency audit, run
  affected formatting/lint/tests, and obtain green supported-platform source CI.
- Unknowns: failures beyond the observed Python 3.11 SDK log-cleanup race.

## 3. Publish and verify the complete product

- Rationale: successful builds alone do not prove deployment and publication.
- Data: version mirrors, changelog, signing metadata and release completeness checks.
- Acceptance criteria: preserve Windows exclusions, commit and push scoped changes,
  tag the release, then verify native, desktop, mobile and package publication gates.
- Unknowns: signing/account availability and store-side processing status.

## Plan state

1. Runtime `8fc943c` is publicly released as `v0.5.20`, following `v0.5.19`.
   Linux/macOS source CI, release CI and all seven assets are verified. The cold,
   read-only startup fix passes 232 JVM and 12 extracted-archive worker tests.
   Windows jobs remain commented out; foreign Windows experiments are untouched.
2. Signing metadata and all relevant stashes were checked. Windows desktop is
   already excluded; the configured signing setup is incomplete.
3. Earlier supported CI regressions are repaired locally. Android CI and release
   setup explicitly select supported SDK packages; the red/green regression and
   lint pass. Changed production code adds no reflection or boxed-math findings.
4. Vis version mirrors and the changelog are prepared for `0.2.5`, including the
   verified composer and draft fixes. The published runtime pin, dependency
   preparation and generated audit are complete; 201 final-pin boundary tests pass.
   Native test launchers now use the resolved-source protocol; TLS is red/green.
5. The FFF timeouts came from an unbound test workspace. Fixture isolation passes
   its red/green regression and all 346 extension/contract cases. Concurrent cold
   runtime provisioning now shares one installation; both new regressions and all
   11 runtime cases pass. The first full post-fix installed SDK run passes 38/38.
6. A fresh Vis native image builds against the published runtime. All 12 affected
   binary cases pass, including real packages in JVM/native workers. This selected
   boundary coverage is not proof that the full native release suite passes.
7. Candidate `18ad4fe2f` and follow-up `0a43fab47` are on `main`. Documentation
   deployment and CodeQL pass. All 62 documentation tests and 3230 Companion tests
   pass locally (two platform skips). Story fixtures still reject mutation timers,
   clocks, random data and live gateway clients.
8. The earlier Linux gather failure asserted a limitation already fixed in the
   published runtime. Updated direct-call/helper contracts and all 74 environment
   cases pass. The next Linux run exposed a conflicting public-manual assertion;
   its red/green regression and all 43 Council host/documentation cases now pass.
9. Follow-up fixes preserve history-pagination coverage without expensive repeated
   role queries and make SDK process cleanup retryable after denied group signals.
   All 824 SDK unit tests pass (38 opt-in skips). A disk-exhausted integration run
   failed; after removing only completed-test caches, the unchanged candidate
   passes all 42 installed-SDK/JVM cases in fresh fixture homes. Failure logs remain.
10. Follow-ups through `2d0ecc146` are on `main`. Both real-SDK platform jobs,
    all ten Python matrix jobs, Android CI, documentation deployment and CodeQL pass.
    Linux core passes 6069 cases, then exposes stale standalone-TUI fixtures.
11. TUI regressions now exercise the inline log-search frame, explicitly open the
    Activity disclosure and recognize both padded section rules. All 2210 TUI cases
    pass in a clean JVM. Formatting and clj-kondo pass; the host reflection checker
    reports the same unrelated `caption-count` warning in the committed baseline.
12. macOS source CI resumed after an explicitly authorized runner restart. Both
    platform suites and the AOT gate pass at `942db083d`. The Linux installed-SDK job
    failed on an upstream GraalVM download returning HTTP 500; its retry was
    superseded by a newer main push. No gateway restart or release-gate bypass occurred.
13. Native goal coverage exposed stale pre-`#216` expectations. All four cases now
    pass with immediate terminal tool summaries and durable empty-response diagnostics;
    the corresponding JVM terminal-goal boundary also passes. Production behavior is unchanged.
14. All three native installer/documented-example regressions now pass. Fixtures
    verify the current versioned install layout and execute the guide's project
    workflow without counting its separate shared-package example. Together with
    the four goal cases, seven affected native checks pass; no production code changed.
15. Deterministic datetime gallery fixtures and disposal guards for terminal transcript
    polling pass their regressions and all 3256 Companion cases (two platform skips).
    Late transcript and metadata results cannot replace a newly opened session.
16. Source CI exposed duplicate canonical-row reads during archive recovery after turn
    header hydration was added. The shared lazy read passes 13 focused cases, including
    missing, running and failed reads, and all 611 gateway cases. Formatting passes;
    lint and reflection findings match the pre-edit baseline.
17. Source CI `35062598255` passed all 18 jobs at `1347ed7c6`: 6098 core and
    2224 TUI cases on each platform, plus installed SDK checks. Companion CI passed
    3273 cases (three platform skips) and built the Android package.
18. Immutable tag `v0.2.5` passed source, mobile and desktop gates in release run
    `35065052347`, but all three native suites failed to compile an obsolete
    `gateway/handshake-keys` fixture. The product remains a draft; Clojars and PyPI
    publication did not run. No tag was moved and no gate was bypassed.
19. The native TUI fixture now uses the canonical wire converter. All six affected
    cases pass against a fresh pinned-GraalVM TUI build, with clean formatting and
    lint. All 102 release-bundle checks pass, including unhealthy and wrong-process
    gateway cases. Both smoke jobs require the launched PID and an explicit available
    port to be healthy; the exact smoke body also passes against a staged local native
    image without touching the live gateway. Clean candidate verification is still required.
20. Store read-back confirms Companion `0.2.5 (6216)` uploaded to both stores,
    available to internal TestFlight testers and assigned to every Android tester
    track. External TestFlight and Play open/closed testing await store review;
    production distribution is unchanged.
21. Version `0.2.6` and its mirrors are prepared; all 11 version/release-note tests
    pass. Complete clean-source and native CI must pass before a replacement tag,
    public artifacts, library publication and final tester-delivery verification.

# Shared FFF lifecycle and retention experiments

Reproduce gateway indexing costs before changing its cache budget.

## Context

`workspace/fff_index.clj` owns the process-wide pool; editing tools and the file
picker lease its indexes. Controlled switching reproduces capacity thrashing; the
historical gateway workload remains unverified. `foundation/housekeeping.clj` owns
runtime retention and draft cleanup. Existing release work in this checkout,
including the preceding plan, remains separate.
Do not merge divergent draft contents, increase the pool limit without evidence,
optimize native builds, or restart the live gateway.

## 1. Reproduce and measure indexing

- Rationale: distinguish repeated construction, unnecessary rescans, and capacity pressure.
- Data: lifecycle events, active leases, scan/queue durations, existing index/editor suites.
- Acceptance criteria: reproduce same-root concurrent users and switching among draft roots;
  verify worker sharing and retain deterministic regressions for defects found.
- Unknowns: whether active eviction or global write invalidation duplicates scanning.

## 2. Correct lifecycle and retention policy

- Rationale: reuse one index per root and ignore policy without retaining dead resources.
- Data: phase-one measurements, canonical terminal draft states, runtime ownership.
- Acceptance criteria: instrument and fix reproduced sharing defects, then implement safe
  terminal-draft/latest-runtime cleanup. Preserve active processes and recoverable work;
  clean up only through validated canonical lifecycle transitions.
- Unknowns: safe live cleanup candidates and required older runtimes.

## 3. Record experiments and verify

- Rationale: separate measured causes from plausible explanations.
- Data: controlled GC/class-loader experiments and isolated retention fixtures.
- Acceptance criteria: validate each remaining item in `TODOS.md`, fix measured defects,
  run affected tests/format/lint/reflection, cross-check the design, and push scoped commits.
  Stop owned test processes; do not restart the live gateway or build a native image.
- Unknowns: whether historical gateway CPU/loader growth reproduces in current code.

## Plan state

1. Reproduced active eviction duplicating an index, unrelated-root write rescans, and
   seven-root/six-slot capacity thrashing. Real confined workers share one host index.
2. Lifecycle instrumentation, active-entry pinning, scoped invalidation and failed-rescan
   handling are implemented. The six-slot budget and separate root/policy identities stay.
3. GC and loader experiments are recorded in `TODOS.md`. Ordinary memory pressure
   reclaimed most accumulated loaders; no tuning change is justified. The current live
   host is native, so the historical JVM report remains unverified.
4. FFF commit `eaa460a7c` is pushed with 309 passing affected cases against its runtime
   pin. Independent review confirmed the shared-host design and regression coverage.
5. Draft safety fixes are pushed in `d9a3b7335`, with 98 passing cases: canonical backend
   retries, extra-root ownership after primary removal, and confinement synchronization
   before release. Formatting, lint and fresh compiler checks found no new warnings.
6. Automatic terminal-draft and newest-runtime deletion remain unimplemented: there is
   no authoritative root-user/spawn exclusion or cross-process runtime lease, and no
   quiescent window. Review and the runtime owner confirmed those gaps. Do not infer
   disposal from cancellation, age or an empty worker snapshot. No user data was deleted.
7. Removed the unsafe runtime/source age sweep. Regressions first reproduced deletion
   of old/newer/unknown installs and deletion through linked stores; all 31 housekeeping
   cases now pass. Advisory inventory and archive cleanup remain. This is a safety
   correction, not completion of the requested newest-only reclamation policy.
8. No live gateway restart or native build is authorized. Those constraints also prevent
   representative deployment verification and testing a new native lifecycle boundary.
   Concurrent release work remains separate.

# JSON Schema as the contract source

Define payloads once; derive runtime vocabulary from their schemas.

## Context

`packages/vis-contract/resources/vis-contract/` currently holds catalog JSON files
and same-named schemas. Several catalogs repeat field names and enums, while some
schemas validate catalogs instead of wire payloads. Clojure, the Python SDK and
Companion consume these files. Remove the catalogs, not the supported behavior.
Do not relocate them wholesale into schema annotations or retain compatibility
readers. Existing unrelated checkout edits remain outside this work.

## 1. Consolidate schemas and consumers

- Rationale: make JSON Schema the only maintained structural contract.
- Data: existing payload definitions, runtime readers, shared fixtures and package resources.
- Acceptance criteria: no top-level catalog JSONs; consumers derive fields, vocabulary,
  defaults and bounds from schemas; operational annotations exist only when needed.
- Unknowns: which declarations affect runtime behavior rather than documentation alone.

## 2. Verify language and packaging boundaries

- Rationale: source validation alone does not prove packaged SDK or host bootstrap behavior.
- Data: contract, engine integration, SDK, Companion and TUI regression suites.
- Acceptance criteria: affected tests, formatting, lint/reflection and package resource
  checks pass; tests prevent catalog reintroduction and exercise real payload validation.
- Unknowns: pre-existing failures and concurrent changes outside the task.

## 3. Review and publish

- Rationale: deliver the complete refactor without absorbing unrelated work.
- Data: final scoped diff, verification results and repository status.
- Acceptance criteria: update contract guidance, commit only this task and push to main.
- Unknowns: remote movement or overlapping edits requiring coordination.

## Plan state

1. Complete: removed 16 catalog JSONs and the obsolete Python-host declaration table.
   The 16 remaining schemas describe real payloads; Clojure, Python, Companion and TUI
   derive their structural contracts from them without compatibility catalog readers.
2. Affected contract and integration tests pass, including 816 Python SDK tests,
   2849 Companion tests and 2211 TUI tests. Formatting and lint introduce no new findings.
   Built sdist/wheel resources contain the same 16 schemas, verified byte for byte.
3. The native image builds and schema/host boundary cases pass. The broad native run
   exposed unrelated stale fixtures, corrected in separate commits, and an uncommitted
   worker hang-evidence test outside this change; it is not claimed fully green.
4. The full core suite passes 6099 of 6102 cases. Two shared-suite skill-discovery
   assertions fail, while all 155 Python-extension cases pass in an isolated JVM.
   A same-JVM diagnostic reproduces a missing skill after its source throws an FFF
   rescan timeout. The third failure flags clock calls in unrelated uncommitted workers.
   These discovery/worker paths were not changed by the schema refactor.
5. Scoped implementation, documentation and verification are complete. Publish this
   change separately on main; preserve concurrent code and plan edits. No release,
   deployment or live-service restart belongs to this work.

# Plan records: spec, tasks and evidence in the session

Make every plan a host-owned session record with one coherent view for the model and the person, and retire attachment-based plans.

## Context

Owners are `internal/session/goals.clj` (one user-owned goal per session; `set-goal!` overwrites; `update_goal` mutates), `internal/context/renderer.clj` (line 63 projects the goal into `session["goal"]` regardless of status; `render-ctx-delta` emits `session[...]` deltas per iteration from `loop.clj`), `internal/context/prompt.clj` (`planning-rules`, toggle `plans`, off in the CLI, keeps plan state in a `PLAN-<feature>.md` attachment), `packages/vis-contract/resources/vis-contract/schema/`, `resources/vis-shims/`, `extension/core.clj`, `gateway/wire.clj`, `apps/vis-tui/` and `apps/vis-companion/` (goal badge). Per-turn ctx snapshots persist in `session_turn_state.ctx` for resume, but `read_session` does not expose them and iteration-level deltas are not stored.

Problem: two continuities follow opposite rules. A terminal goal stays in the cached prefix until a new `/goal` replaces it; live plan state is invisible to the host and the UI and disappears on fold. Vocabularies differ (`complete` and `done`) and nothing links the goal, the plan and the changes.

Decisions from the design conversation (session `d33a97e8-b72c-4199-a2e8-ec9631e953f6`):

- A plan always exists, human-managed (a person accepts and comments) or machine-managed (auto-accepted). The minimal form is one spec item, one task and one diff, so trivial work pays nothing.
- A plan is three layers with different owners: spec (what and why; the person owns it), tasks (how; the model proposes, the person accepts; every task covers spec items), evidence (what changed; observed from patches, test runs and commits, written by nobody).
- The goal is part of the spec: its one-line intent and acceptance criteria. It is the compact projection kept in the prefix to guard execution against drift. One goal per plan; further intents are spec constraints or separate plans.
- The record is host-owned and lives in the session store, projected into `session["plan"]` (compact) and `session["goal"]`. The model changes it only through sandbox functions; acceptance and comments are human actions from the CLI or UI that reach the model as `session[...]` deltas. Because the ctx is part of the transcript, plan history becomes walkable turn by turn.
- Attachments remain a file primitive (a pasted spec can be a source reference) and never carry plan state.
- Status is derived from recorded transitions and evidence, never declared bare: `done(task, evidence)`; the goal completes when every acceptance criterion has evidence and, in human mode, the person confirms.

Rejected alternatives: parsing `PLAN-<feature>.md` attachments into `session["plan"]`; a tracked repository file or an external tracker as the source of truth (export only; `to-tickets` covers tracker export); one flat goal-plus-plan object; a list of goals in the prompt; automatic coupling of `update_goal complete` and `Status: done`.

## 1. Terminal goal leaves the prefix

- Rationale: the smallest safe change and the lifecycle rule every continuity follows: active is compact in the prefix, a terminal transition is one delta in its turn, from the next user turn the key is deleted, history comes from lookup.
- Data: `renderer.clj:63`, `goals.clj`, the goal badge in the TUI and companion.
- Acceptance criteria: the renderer omits a terminal goal from the next user turn; `/goal` without arguments shows the last goal including a terminal state; regression tests in the mirrored `context/renderer_test` and `session/goals_test`; formatting, lint and reflection clean.
- Unknowns: whether the UI badge drops at the same boundary (default: yes, same rule).

## 2. Plan record contract

- Rationale: vocabulary and bounds derive from the schema, never from paired catalogs.
- Data: the goal record shape, `vis-contract` schemas, Skjema validation.
- Acceptance criteria: a JSON Schema for the plan record: `spec` (`goal` with `phrase` and `acceptance`, `requirements`, `constraints`, source references), `tasks` (`id`, `title`, non-empty `covers`, `status`, `evidence`), one status vocabulary shared by goal, plan and task, `mode` (`human` or `machine`), acceptance state and a transition log stamped with turn and iteration; schema tests.
- Unknowns: none once the degenerate plan is one spec item and one task.

## 3. Host record, storage and projection

- Rationale: `session` is a projection, not storage.
- Data: goal storage in `goals.clj`, renderer ambient keys, `render-ctx-delta`, `session_turn_state.ctx` snapshots.
- Acceptance criteria: the record persists in the session store linked to the workspace; `session["plan"]` carries feature, status, current task, next task and done/total; `session["goal"]` derives from `spec.goal`; every transition emits a delta; terminal records leave the prefix from the next user turn; `read_session` exposes the per-turn ctx snapshot or its delta so plan history is walkable by turn; tests.
- Unknowns: size cap of the compact projection and cache cost of frequent task changes (measure).

## 4. Sandbox functions

- Rationale: the model writes through narrow verbs so the host validates every transition and attributes evidence.
- Data: the `update_goal` binding, `resources/vis-shims/`, `extension/core.clj`, Activity presentation rules.
- Acceptance criteria: `plan.propose(spec, tasks)`, `plan.current(task_id)`, `plan.done(task_id, evidence)`, `plan.block(task_id, reason)` and `plan.show()`; the host rejects a task without `covers`; machine mode auto-accepts `propose`; activities between `current` changes attach to that task as evidence; each binding has an Activity presentation with running, success, failure and empty states; docstrings regenerate the apropos resources; tests.
- Unknowns: the evidence shape (patch anchors, test-run identity, commit SHAs), derived from existing activity records.

## 5. Human surface

- Rationale: the same record, a second projection.
- Data: the `/goal` command, gateway routes, TUI badge, companion goal view.
- Acceptance criteria: `/plan` lists, shows, accepts and comments in the CLI; gateway routes use snake_case wire keys; TUI and companion show spec, tasks and evidence with accept and comment; comments reach the model as deltas; tests.
- Unknowns: comment granularity (plan or task).

## 6. Prompt, documentation and retirement

- Rationale: no compatibility layers for the attachment path.
- Data: `planning-rules`, `resources/vis-docs/`, `docs.edn`, `site.edn`, the AGENTS.md plan rule.
- Acceptance criteria: `planning-rules` rewritten around the record and the functions; the `PLAN-<feature>.md` attachment path removed; user and extension docs updated; the AGENTS.md rule points at the record; the `plans` toggle means "require human acceptance before coding"; docs checks.
- Unknowns: none.

## 7. Cross-session continuity

- Rationale: long work outlives one session.
- Data: workspace root in session records, `list_sessions`, Council delegation.
- Acceptance criteria: a new session in the same workspace lists open plans and adopts one; delegation passes task ids, not plan copies; tests.
- Unknowns: concurrent sessions on one plan (proposal: one owning session, others read-only).

## Plan state

Status: draft. Design settled in conversation; no code changed. The umbrella stays `plan`; the goal lives inside the spec.

1. Not started.
2. Not started.
3. Not started.
4. Not started.
5. Not started.
6. Not started.
7. Not started.

# Reliable, faster verification and delivery

Phrase: Remove wasted work without removing useful release guarantees.

Context: The September 16 release repeated source checks after a green candidate,
queued several macOS jobs on one runner, and failed its pickup monitor on HTTP 502.
Core tests took about 24 minutes on Linux; native tests took 14–22 minutes.
Pake's workflow caches the repository Cargo target although packaging runs via npx.
Existing unrelated working-tree changes remain outside this task. Skipping security,
signing, native execution, or installed-SDK checks is not an acceptable optimization.

## 1. Audit tests and isolate wasted work

- Rationale: Delete obsolete or redundant assertions, not coverage of supported behavior.
- Data: Core, native, TUI, SDK and companion suites; completed CI logs and fixtures.
- Acceptance criteria: Each removal has a specific rationale; useful flaky tests are repaired; affected suites, formatting and lint pass.
- Unknowns: Slowest fixtures and remaining obsolete contracts.

## 2. Optimize release orchestration

- Rationale: Avoid unnecessary serialization and repeated work while preserving publication gates.
- Data: `.github/workflows/`, release validation scripts and workflow regression tests.
- Acceptance criteria: Tested workflow changes retain source, native, SDK, signing and complete-artifact gates; transient API errors do not masquerade as missing runners.
- Unknowns: Safe artifact reuse and shared-runner contention.

## 3. Optimize Pake and deployment

- Rationale: Cache the actual compiler output and avoid duplicate packaging work.
- Data: `apps/vis-companion/scripts/desktop-package.mjs`, desktop/mobile workflows and deployment scripts.
- Acceptance criteria: Packaging regressions pass; cache paths match actual build paths; deployment remains immutable and retry-safe; report measured gains separately from estimates.
- Unknowns: Pake cache behavior and external store/notarization time.

## Plan state

Completed locally:

1. Removed the retired-feature name-ban scan, redundant public-symbol existence
   test, and assertions requiring commented-out Windows CI. Retained extension
   behavior, security, signing, native and SDK coverage. Reproduced the denied-domain
   test's DNS-outage failure and replaced public DNS with local-host resolution and
   unconditional, reason-specific bypass assertions; the hosts-only JVM check passes.
2. Native draft builds now overlap source verification; publication explicitly
   requires verification, and store jobs retain their source gates. AOT compilation
   runs independently. Installed-SDK CI reuses the dependency cache. Runner pickup
   retries transient API failures within the existing deadline, with executable
   regression coverage for success, retry, unavailable status and queued timeout.
3. Pake 3.15.7 source confirms that Cargo compilation and artifact lookup honor
   CARGO_TARGET_DIR. Packaging and CI now share an absolute persistent target keyed
   by platform, compiler and Pake version. Kept separate Linux package invocations:
   upstream still builds each format separately. Documented immutable releases,
   existing-build retries and the distinction between upload and store review.

Verification: 190 affected Clojure cases passed; 248 companion script tests passed
with one existing freeze-dependent skip (publishing is not frozen). Clojure
formatting/lint including reflection, companion lint (443 files), JavaScript
formatting, all five changed workflows' actionlint, and documentation target/anchor
checks passed. Final whitespace/diff review follows this update.

No new release, tag, store upload or live deployment was initiated. Changes remain
local and unrelated work is preserved. A warm-cache release timing has not been
measured; shared macOS runner capacity and external review/notarization remain
limits. The audit removes demonstrated waste rather than claiming every individual
repository test was manually reviewed or weakening expensive integration gates.

Second-pass cross-validation:

- Partitioned dependency and native-runtime caches by architecture. Added dependency
  pins to the native-runtime key, so pin changes can save a refreshed cache instead
  of repeatedly restoring an immutable older entry. Removed the unused native
  build-cache path so source and native jobs share the same dependency cache shape.
- Run interpreter-independent SDK lint once, retaining wheel builds and installed
  tests across all ten OS/interpreter combinations and both real-engine jobs.
- Removed three always-true MCP fallback assertions and the silent stream-cleanup
  skip. Python and POSIX prerequisites now fail explicitly rather than hiding lost
  transport/process coverage. Removed duplicate runner assertions and broad word
  bans in desktop workflow tests; supported runner/target checks remain.
- Replaced an SDK checkout assertion tied to action-version spelling and whitespace
  with parsed workflow checks; added parsed cache and lint/matrix regressions.
- Verification: 112 Clojure cases passed, followed by 11 targeted cases after final
  test refactoring; 37 desktop packaging cases passed. Clojure lint/reflection and
  formatting, JavaScript formatting, companion lint (443 files), and actionlint for
  the five affected workflows passed. Changes remain local; no deployment or
  measured end-to-end warm-cache timing is claimed.
