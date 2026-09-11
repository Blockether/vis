# Complete the Python API

Provide one API with two transports, without duplicating the agent.

## Context

`src/blockether/vis/extension.py` is both the distributable extension SDK and the engine-executed
module. `_outside.py` implements extension operations outside Vis; it is not a local agent.
The gateway wire contract lives in `../vis-contract/resources/vis-contract/gateway.json`.
The Python session client and initial managed stdio engine now exist. Root `PLAN.md`
tracks unrelated Companion work and remains untouched. Root `TODO.md` tracks acceptance.

Rejected: embedding a JVM in CPython, copying the agent loop, mirroring the wire vocabulary,
starting a hidden HTTP gateway for a mode advertised as gateway-free, or implicit publishing.

## 1. Contract and remote session client

- Rationale: establish the public lifecycle against the working gateway before adding a transport.
- Data: contract JSON/reader, gateway handlers/client, package tests and pyprojects.
- Acceptance criteria: dedicated typed methods (no public `call`), session/turn/events and views/attachments with explicit
  errors and cleanup; real isolated gateway integration and packaged unit tests.
- Unknowns: exact endpoint body/projection shapes and replay behavior; inspect their implementations.

## 2. Local engine transport

- Rationale: reuse the lifecycle, route handling and wire projections without HTTP or discovery.
- Data: engine session/turn ownership, command dispatch, gateway handler construction, wire codec.
- Acceptance criteria: explicit stdio process mode, shared client objects, bounded startup/exit,
  no live-gateway mutation, deterministic engine/provider integration and native coverage. A one-shot subprocess alone does not complete this phase.
- Unknowns: reusable request handling below Jetty; verify it before exposing the local constructor.

## 3. SDK reference and distribution checks

- Rationale: consumers install artifacts, not a checkout; hosted extensions need no pip installation.
- Data: extension host injection, canonical contracts, wheels/sdists, CI and PyPI metadata.
- Acceptance criteria: installed-artifact tests, rebuilt sdists, typing and mode examples, gated
  publication preparation; no real publication without a separate request.
- Unknowns: PyPI project ownership/trusted-publisher setup and runner availability.

## 4. Collision-free namespace and dataclass boundary

- Rationale: the unrelated PyPI distribution `vis` must not share our import path;
  slotted dataclasses must retain their fields across extension and transport boundaries.
- Data: the canonical SDK, PEP 420 package layouts, runtime extension bootstrap,
  client envelope models, consumer fixtures and installed artifacts.
- Acceptance criteria: a collision-free PEP 420 parent (public domains finalized in phase 7),
  no legacy alias or parent `__init__.py`; named validated records, nested slotted extension
  results, preserved sibling packages, source and installed-wheel verification.
- Unknowns: complete endpoint projection models and the remaining integration checks.

## 5. Canonical View and Activity boundaries

- Rationale: one SDK must expose the same interaction and execution evidence as TUI and Companion.
- Data: existing View JSON Schema, a new Activity document/schema, host-observed symbol metadata,
  Python records, shared cross-language fixtures, and the runtime release/pin.
- Acceptance criteria: Activity is not a View; closed declaration/projection shapes, bounded host-owned
  lifecycle, typed SDK reads and operator actions; no retired imports/envelopes; shared consumer tests.
- Unknowns: fresh native Vis execution and the remaining transport integration checks below.

## 6. Complete transport and consumer acceptance

- Rationale: installed imports and shape fixtures are not proof of a usable SDK lifecycle.
- Data: canonical lease/job-stream semantics, deterministic provider + Python extension, isolated
  HTTP and actual `sdk-stdio`, distribution artifacts and supported-platform CI.
- Acceptance criteria: automatic lease keepalive with bounded cleanup; all canonical SSE routes;
  agent/tool/View/Activity/cancellation across both transports; regression-tested E2E startup;
  installed-package CI and gated publishing configuration, without changing external ownership.
- Unknowns: remote platform-matrix results and externally configured PyPI trusted publishers.

## 7. Separate SDK domains and include the Python contracts in the SDK distribution

- Rationale: engine imports must not initialize extension hosts; shared contracts do not need a second Python product.
- Data: `extension.py`, `engine/`, private contract reader, canonical JSON resources, host bootstrap, consumers and distribution CI.
- Acceptance criteria: lightweight `blockether.vis`; `blockether.vis.extension` and `blockether.vis.engine`; no retired imports; one wheel and rebuildable sdist; preserved host isolation, validation and cross-language fixtures; affected tests, lint and formatting pass.
- Unknowns: none for namespace ownership or archive resources; fresh native and remote release checks remain below.

## 8. Typed extension declarations and Activity content

- Rationale: public SDK values should expose typed fields, not marker dictionaries; construction must not bind a host or register anything.
- Data: `extension.py`, its wire adapter and bootstrap, extension consumers, canonical Activity schemas, `loop.clj` publication and actual HTTP/stdio integration.
- Acceptance criteria: frozen/slotted Extension, Symbol, Provider, SlashCommand, OpHook and NetworkFilter; explicit register; typed bounded Activity blocks with automatic host lifecycle; no old builders; source, installed and real-engine tests, formatting/lint and documented SDK examples.
- Unknowns: native verification needs the pinned GraalVM CE 25.3.4.1; canonical wire shapes, callable/object namespaces and host-owned lifecycle are covered locally.

Implemented and verified on the JVM. Declarations are immutable values until explicit
registration; all consumers use the new classes, and the documented example executes
against the installed SDK. Typed Activity presentations follow the current canonical
contract; unrelated Activity/UI/provider work is preserved.

Real HTTP/stdio tests exposed a lost trailing Activity update while a tool waited for
input. The existing serial dispatcher now schedules one bounded trailing flush, retains
coalescing, and discards delayed work on settlement. The regression failed before the
fix; the loop suite and both real transports pass afterward.

Current verification (separate suites, not additive):
- Source SDK and project GitHub extension: 300 passed, three opt-in engine tests skipped.
- Installed wheel outside the checkout: 262 passed, including actual JVM HTTP/stdio,
  intermediate and terminal Activity, typed results, Views and cancellation.
- Affected Clojure suites: 732 passed. Foundation is registered and deployment environment
  is removed from the test REPL configuration only; earlier baseline caveats remain below.
- Python formatting/lint: 27 files pass. Clojure formatting/lint/reflection: nine files pass.
- Direct wheel and wheel rebuilt from sdist agree; all 24 canonical JSON files and the
  injected extension API match the wheel. Strict Twine checks pass.
- Documentation: 35 Python examples, 52 local file links, three workflow YAML documents
  and three embedded Python scripts checked; the README example also executes as a test.
- Real-model py-repl-compute E2E: 1/1 passed after the Activity fix; required REPL used,
  zero tool errors, temporary source gateway cleaned up.
- Native verification is blocked: bin/require-graalvm --native-image reports the required
  GraalVM CE 25.3.4.1 is absent; the active toolchain is CE 25.1.3. No pin substitution,
  native-image success claim, publication, gateway restart, commit or push was made.

## 9. Typed providers and an end-to-end routing boundary

- Rationale: provider configuration and callback results still expose unchecked dictionaries; refresh exceptions are incorrectly retried as arity mismatches.
- Data: extension SDK, Python provider adapters, canonical provider/config vocabularies, real embedded callbacks and HTTP/stdio routing fixtures.
- Acceptance criteria: immutable typed presets and callback results; correct callback signatures and exactly-once invocation; unchanged canonical wire data; Python-declared provider serves a real local model request; installed SDK, affected JVM tests, formatting/lint/reflection and documentation checks pass.
- Unknowns: local JVM and SDK acceptance is complete; native verification still requires the pinned GraalVM CE 25.3.4.1.

Implemented immutable ProviderPreset, ProviderCredential, ProviderStatus, ProviderModel
and typed limits records, with explicit callback protocols and validation. Constructors
remain pure; each callback is invoked once with its declared signature. Invalid return
records and asynchronous callbacks are rejected without retrying user code.

The regression suite first exposed a refresh callback being retried after its body threw.
Real isolated HTTP/stdio then reproduced cold-start ordering, providerless configuration,
auto-bound preset endpoint precedence, and discarded preset headers/Responses paths.
Extensions now register before routing configuration is resolved; managed providers can
bind without a saved provider entry; runtime credential endpoints/dialects are not
mistaken for explicit configuration; the router receives transport defaults unchanged.
No lifecycle, auth-flow ownership or canonical provider wire contract was replaced.

Final verification (separate, overlapping suites):
- Source SDK plus the project GitHub extension: 348 passed; three opt-in engine cases skipped.
- Fresh installed wheel outside the checkout: 310 passed, including actual JVM HTTP and
  stdio model requests through Python-declared providers, regular and managed binding,
  credential/preset headers, opaque extra-body data, enriched models and Activity/View cancellation.
- Eight affected Clojure suites: 699 passed. The test REPL explicitly registers foundation
  and the built-in LM Studio preset and omits deployment environment only in that REPL.
- Python formatting/lint: four files pass. Clojure formatting/lint/reflection: six files pass.
- Direct and sdist-built wheels agree, match the current injected SDK and all 24 canonical
  JSON resources; strict Twine checks pass. Rebuilt after detecting source/artifact drift.
- Both edited documentation files: 36 Python examples and 45 local file links checked.
- Real-model py-repl-compute E2E: 1/1 passed, required REPL used, zero tool errors and the
  temporary source gateway cleaned up.
- Native-image verification remains blocked by the missing pinned CE toolchain. No live
  gateway restart, publication, commit or push was performed.

Reproduced before production edits: the 94-case extension suite has one failure,
where a throwing refresh callback runs more than once. The 56 existing SDK declaration
and registration tests pass in the dependency-complete isolated environment.

## 10. Reusable SDK verification and a pinned-toolchain release

- Rationale: provide the repeated SDK checks as a repository extension, complete native acceptance with the exact CE pin, and publish only verified scoped changes.
- Data: `.vis/extensions/sdk_checks.py`, its tests, `.graalvm-version`, `bin/require-graalvm`, installed/native suites and the product release workflows.
- Acceptance criteria: `sdk.check` reports ordered typed checks, stops processes it starts and cleans temporary environments, rejects source/artifact mismatches and distinguishes local-only from actual-engine verification; install the checksum-verified locked toolchain, test a release-only snapshot, mirror VIS_VERSION, commit/push/tag only after checks pass.
- Unknowns: PyPI trusted-publisher setup, native publishing readiness and the companion release blockers recorded below.

The repository already pins GraalVM CE 25.3.4.1 across version, vendor, assets, SHA-256
and SDKMAN. The four pin invariants pass. Installed that exact CE release using
`bin/require-graalvm --install --native-image` into a workspace-local toolchain directory;
checksum and reported vendor/version both match. No downgrade, Oracle substitute or live
service restart is involved. Core release 0.1.43 is published on GitHub and Clojars.

Implemented `sdk.check` as an object namespace with immutable results, ordered Activity
steps, bounded subprocess output, timeout cleanup and disposable build/install environments.
The real host-boundary regression invokes its first shell-backed check through the engine.

Verification of the isolated native candidate (separate, overlapping suites):
- All 11 SDK checks pass: source tests 326 passed/three opt-in skips; fresh installed-wheel
  tests 329 passed, including actual native HTTP/stdio engines and the 19 extension tests.
- Direct and sdist-built wheels match each other, the injected source and all 24 canonical
  JSON files; strict Twine validation, 37 Python examples and 18 local links pass.
- Native binary built successfully with the locked CE release; six native assertions,
  27 pin/package/reachability checks, the real host SDK regression and py-repl-compute E2E pass.
- Main CI exposed four existing JVM failures. Reproduced and fixed the long Activity docs
  paragraph, launcher stderr contaminating the lazy-analyzer assertion, duplicated shared
  predicates and retired-name fixtures. The focused JVM selection passes 143 cases; the
  affected companion unit/Chromium stories pass all 23 cases. Clojure lint/reflection passes.
- The engine lifecycle fixture now supplies provider configuration, HOME and JVM user.home;
  installed native tests no longer depend on developer credentials or saved providers.

Final committed-snapshot and publication evidence:
- Annotated `v0.1.43` points to `05ba30e96448063432d10e87b2a6a228c5b20baa`.
  Tag, VIS_VERSION, current main and the passing CI head agreed before the tag was pushed.
- [Main CI](https://github.com/Blockether/vis/actions/runs/34139254583) passes all 18 jobs:
  Linux/macOS JVM suites (4827 cases), all ten Python matrix jobs, both real-engine SDK
  jobs, lint, classpath checks and AOT compilation.
- Rebuilt that exact clean commit with the locked CE toolchain. The staged native wrapper
  reports 0.1.43; all 11 SDK checks and six selected native assertions pass again. Its local
  native artifact is explicitly stamped dry-run and was not uploaded as a stable binary.
- The local full JVM suite does not pass in every environment: shell-profile SDKMAN notices caused
  fixture failures; an isolated HOME removes those, leaving two environment-sensitive
  truststore/home-listing failures. Both full CI platform suites pass the same tests.
- [Release v0.1.43](https://github.com/Blockether/vis/releases/tag/v0.1.43) is public.
  The core release/Clojars job succeeded; the macOS companion DMG is attached. Release
  automation committed its changelog separately. Unrelated working-tree edits were preserved.

Remaining distribution blockers, not successful release steps:
- Native tag publishing is disabled by repository policy; no native engine asset was published.
- The public PyPI project and repository `pypi` environment are not available. SDK publication
  requires trusted-publisher setup; no credentials or environment protection were changed.
- The tagged companion run failed iOS archive signing with `errSecInternalComponent`.
  Desktop checkout ordering, Linux output casing and missing `xdg-utils` were fixed
  afterward; all three targets passed in desktop dry run 34152836189. Windows was
  removed from the requested targets. Android publication remains frozen.
- No live gateway restart, deployment outside the requested release pipeline, release-asset
  overwrite, signing-key change or tag rewrite was performed.

## 11. Desktop runner scope

**Rationale:** Only macOS needs the self-hosted runner. Linux already builds successfully
on GitHub-hosted native runners; routing Linux through a Mac adds unnecessary emulation.

**Data:** Restored the working matrix: self-hosted macOS ARM64 for Universal DMG,
`ubuntu-24.04` for Linux x64 and `ubuntu-24.04-arm` for Linux ARM64. Removed the
uncommitted Linux container implementation and its container-only tests. A regression
asserts the exact runner matrix and rejects container routing in the workflow.

**Acceptance criteria:** Desktop packaging tests and lint pass, with no changes to
unrelated working-tree edits or published release assets.

**Unknowns:** No new installer build is claimed by this correction. The restored
workflow previously passed all three jobs in dry run 34152836189.

## Plan state

Phases 7–9 and the SDK/native/core-release portion of phase 10 are complete. The reusable
SDK extension is committed and core 0.1.43 is published; earlier distribution blockers
are recorded above. Phase 11 restores the previously passing desktop runner split;
17 packaging tests and React compiler lint pass. Gateway and production services are untouched.

### Historical phase 7 state
Phase 7 is implemented and locally verified. Extension authors use `import blockether.vis.extension as vis`; engine clients use `from blockether.vis.engine import GatewayClient, LocalEngine`. The root package is inert, with no legacy aliases. The SDK bundles the private contract reader and all 24 canonical JSON resources in one wheel and rebuildable sdist; canonical Clojure/JSON sources are unchanged. The host-owned injector executes the same extension API shipped in that wheel. Consumers, documentation, version mirroring and CI use the new layout. Unrelated working-tree changes remain untouched. No commit, push, publication, deployment or runtime release was performed.

Earlier namespace-refactor verification (before phase 8; separate, overlapping suites):
- Source SDK plus the project GitHub extension: 243 passed, three opt-in engine cases skipped.
- Installed wheel outside the checkout: 205 passed, including actual JVM HTTP/stdio engines with a deterministic model double.
- Affected Clojure contracts, extension isolation, native resources and shell/language consumers: 287 passed with foundation registered and deployment environment removed from the test REPL configuration only. A plain clean-JVM consumer run has 36 missing-shell-registration failures; the same 157-case selection reproduces all 36 on the clean baseline. The user-environment-dependent extension case also failed before this refactor.
- Python formatting/lint: 26 files pass. Clojure formatting/lint/reflection: eight files pass. Version-sync syntax and execution pass without changing VIS_VERSION.
- Direct wheel and wheel rebuilt from sdist build successfully; wheel/sdist pass strict Twine checks. Bundled resources match all 24 canonical files byte-for-byte, with no retired modules or second Python dependency.
- Documentation: 34 Python examples and 52 local file links checked; three workflow YAML documents and three embedded Python scripts parse.
- Real-model `py-repl-compute` E2E: 1/1 passed on gpt-6-astra, required REPL used, zero tool errors, isolated source gateway cleaned up.
- No fresh native binary was built for this namespace refactor. The earlier native verification below does not establish that the current binary runs.

### Earlier SDK implementation (before phase 7)

The earlier implementation established both transports under a shared PEP 420 parent.
Its import layout is superseded by phase 7. The engine still executes the extension SDK
file shipped in the wheel. The host no longer installs the retired
`find`/`find_files` search aliases or their discovery metadata. Endpoint data without a canonical
schema stays explicitly JSON-typed instead of inventing a parallel model contract.

Activity declarations select presentation only; the engine owns invocation identity, clocks,
lifecycle and bounded evidence. Each `block.activity` replaces the prior projection. View uses
open/patch/close events and validated operator actions. Public input Views omit host routing and
validation metadata; their close receipts expose the reason, never submitted answers or secret
handles. SSE live close receipts may omit the already-streamed contents; journal polling retains
it. Canonical documents, schemas and shared acceptance/rejection fixtures live in vis-contract.

Runtime v0.5.0 is published at tag `v0.5.0`, commit
`5acd3f0e861b8735b56058ebea3bf9c5041b2291`. All five release jobs succeeded and all five assets
are uploaded: four platform archives and the JVM jar. Its suite passed 157 tests / 746 assertions.
Vis pins that immutable commit; integration uses the downloaded release, not a local override.

Historical verification (separate suites, not additive):
- Final wheels built from sdists, with all four artifacts passing strict twine checks.
  Installed outside the checkout with isolated Python: 191 passed, three opt-in cases skipped.
- Installed SDK + actual JVM HTTP/stdio and process/error tests: 19 passed. The full flow loads
  an extension, executes its tool, renders live/input Views, answers, emits Activity, completes,
  cancels a second turn and cleans up. Both transports use the same canonical SDK records.
- Engine contracts, gateway/server/stdio/View and worker: 227 passed.
- Extension integration, Activity and engine-projected View contracts: 108 passed.
- Canonical host binding/metadata regression and environment suite: 43 passed.
- Standalone TUI chat/live View/human input: 201 passed in the TUI project, not the engine runner.
- Companion's six affected suites: 134 passed; typecheck and React compiler lint passed.
- Affected Clojure lint/reflection and Python lint/format checks passed. Documentation checks
  parsed 36 Python examples and resolved 24 repository source links.

Historical native verification passed: GraalVM CE 25.3.4.1 built that image with a 12 GiB
heap (10.20 GiB peak RSS), then fetched and staged runtime v0.5.0. The installed SDK passed
19 tests against the staged native wrapper over both HTTP and stdio, including input-close
privacy and the SSE/polling difference for completed live views. Five selected `test-native`
cases also passed: artifact/version, whole agent turn, Python execution and guarded file IO.
The native and beta release workflows now run the shared installed-SDK native action.
Implementation and local SDK acceptance are complete; external release requirements remain below.

The editing E2E classpath regression is fixed and unit-tested. A real `py-repl-compute` run
completed with no tool errors but failed its required-REPL assertion because the model only
used `cat`; cleanup reported a timeout. A cross-model retry timed out starting its isolated
source gateway. Neither attempt passed. The earlier broader JVM run had 272
cases and 36 unregistered `:shell` / `:_shell-wait` failures without a clean-base comparison;
selected suites do not establish that the whole repository passes.

Reusable distribution CI covers Linux/macOS, CPython 3.11–3.14 and PyPy 3.11, plus actual
HTTP/stdio engine integration; native workflows also test the staged release through the SDK.
A manual protected PyPI publishing workflow consumes distribution CI's
verified artifacts. Workflow lint passed, but the remote matrix has not run. PyPI project
ownership/trusted publishers, SDK publication and a Vis deployment remain external requirements;
none is implicitly authorized by this plan. No second runtime release is needed.

## 11. Typed catalog, generated help and Spel adoption (#203)

- Rationale: expose immutable discovery values derived from Symbol contracts, not another declaration registry.
- Data: canonical extension.py, SDK tests and extension-design/API guides; Spel extensions/vis-spel; the verified Python publishing workflow.
- Acceptance criteria: preserve Python calling conventions and safe default metadata; public catalog/help/doc parity; reusable contract assertions, validation-before-IO and cancellation coverage; registered host calls and installed-wheel verification; Spel uses the same interfaces; publish SDK 0.1.69 without a product release.
- Unknowns: protected PyPI publication approval and remote CI results.

Plan state: implementation and local SDK acceptance complete; publication and installed Spel dependency verification pending.

- Before implementation: 42 existing SDK cases passed (one Python-version skip); all 15 new catalog cases failed because Catalog was absent. Spel's two new cases failed for missing spec/help and entrypoint-only mutation metadata.
- SDK verifier: all 11 gates pass, including 547 source tests and 560 installed-wheel tests with actual isolated JVM HTTP/stdio engines (14 opt-in/source skips; one installed Python-version skip). Direct/sdist wheel contents and canonical contracts agree; strict distribution checks pass.
- Affected Clojure suites: 223 cases pass; formatting, lint and reflection checks pass. The documented catalog crosses registration, the trusted worker and sandbox with immutable nested results.
- Spel: 60 Python cases pass (two opt-in native/browser cases skipped); make lint passes. Local make test is unsafe because existing test-cli.sh globally kills Spel daemons; do not disrupt other sessions. Isolated remote CI remains required.
- Python publishing now verifies the exact main commit and VIS_VERSION for an SDK-only dispatch, preserving protected PyPI approval and artifact checks. No product tag, deployment or service restart is involved.
