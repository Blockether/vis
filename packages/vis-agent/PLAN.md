# Complete the Python API

Ship one API with two transports, without duplicating the agent.

## Context

`src/blockether/vis/__init__.py` is both the distributable extension SDK and the engine-executed
module. `_outside.py` implements extension operations outside Vis; it is not a local agent.
The gateway wire contract lives in `../vis-contract/resources/vis-contract/gateway.json`.
The Python session client and initial managed stdio engine now exist. Root `PLAN.md`
tracks unrelated Companion work and remains untouched. Root `TODO.md` tracks acceptance.

Rejected: embedding a JVM in CPython, copying the agent loop, mirroring the wire vocabulary,
starting a hidden HTTP gateway for a mode advertised as gateway-free, or implicit publishing.

## 1. Contract and remote vertical slice

- Rationale: establish the public lifecycle against the working gateway before adding a transport.
- Data: contract JSON/reader, gateway handlers/client, package tests and pyprojects.
- Acceptance criteria: dedicated typed methods (no public `call`), session/turn/events and views/attachments with explicit
  errors and cleanup; real isolated gateway integration and packaged unit tests.
- Unknowns: exact endpoint body/projection shapes and replay behavior; inspect canonical owners.

## 2. Local engine transport

- Rationale: reuse the lifecycle, route handling and wire projections without HTTP or discovery.
- Data: engine session/turn ownership, command dispatch, gateway handler construction, wire codec.
- Acceptance criteria: explicit stdio process mode, shared client objects, bounded startup/exit,
  no live-gateway mutation, deterministic engine/provider integration and native coverage. A one-shot subprocess alone does not complete this phase.
- Unknowns: smallest reusable boundary below Jetty; prove before exposing the local constructor.

## 3. SDK reference and distribution gates

- Rationale: consumers install artifacts, not a checkout; hosted extensions need no pip bootstrap.
- Data: extension host injection, canonical contracts, wheels/sdists, CI and PyPI metadata.
- Acceptance criteria: installed-artifact tests, rebuilt sdists, typing and mode examples, gated
  publication preparation; no real publication without a separate request.
- Unknowns: PyPI project ownership/trusted-publisher setup and runner availability.

## 4. Collision-free namespace and dataclass boundary

- Rationale: the unrelated PyPI distribution `vis` must not share our import path;
  slotted dataclasses must survive both extension and transport boundaries.
- Data: the canonical SDK, PEP 420 package layouts, runtime extension bootstrap,
  client envelope models, consumer fixtures and installed artifacts.
- Acceptance criteria: `blockether.vis` and `blockether.vis_contract`, no legacy module
  alias or parent `__init__.py`; named validated records, nested slotted extension
  results, preserved sibling packages, source and installed-wheel verification.
- Unknowns: complete endpoint projection models and the remaining integration gates.

## 5. Canonical View and Activity boundaries

- Rationale: one SDK must expose the same interaction and execution evidence as TUI and Companion.
- Data: existing View JSON Schema, a new Activity document/schema, host-observed symbol metadata,
  Python records, shared cross-language fixtures, and the runtime release/pin.
- Acceptance criteria: Activity is not a View; closed declaration/projection shapes, bounded host-owned
  lifecycle, typed SDK reads and operator actions; no retired imports/envelopes; shared consumer tests.
- Unknowns: fresh native Vis execution and the remaining transport integration gates below.

## 6. Close transport and consumer acceptance

- Rationale: installed imports and shape fixtures are not proof of a usable SDK lifecycle.
- Data: canonical lease/job-stream semantics, deterministic provider + Python extension, isolated
  HTTP and actual `sdk-stdio`, distribution artifacts and supported-platform CI.
- Acceptance criteria: automatic lease keepalive with bounded cleanup; all canonical SSE routes;
  agent/tool/View/Activity/cancellation across both transports; regression-tested E2E startup;
  installed-package CI and gated publishing configuration, without changing external ownership.
- Unknowns: remote platform-matrix results and externally configured PyPI trusted publishers.

## Plan state

The SDK and both transports are implemented. Public imports are `blockether.vis` and
`blockether.vis_contract`, sharing a PEP 420 parent without compatibility aliases. The engine
executes the extension SDK file shipped in the wheel. The host no longer installs the retired
`find`/`find_files` search aliases or their discovery metadata. Endpoint data without a canonical
schema stays explicitly JSON-typed instead of inventing a parallel model contract.

Activity declarations select presentation only; the engine owns invocation identity, clocks,
lifecycle and bounded evidence. Each `block.activity` replaces the prior projection. View uses
open/patch/close events and validated operator actions. Public input Views omit host routing and
validation metadata; their close receipts expose the reason, never submitted answers or secret
handles. SSE live close receipts may omit the already-streamed picture; journal polling retains
it. Canonical documents, schemas and shared acceptance/rejection fixtures live in vis-contract.

Runtime v0.5.0 is published at tag `v0.5.0`, commit
`5acd3f0e861b8735b56058ebea3bf9c5041b2291`. All five release jobs succeeded and all five assets
are uploaded: four platform archives and the JVM jar. Its suite passed 157 tests / 746 assertions.
Vis pins that immutable commit; integration uses the downloaded release, not a local override.

Latest verification (separate suites, not additive):
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

Fresh native verification passed: GraalVM CE 25.3.4.1 built the current image with a 12 GiB
heap (10.20 GiB peak RSS), then fetched and staged runtime v0.5.0. The installed SDK passed
19 tests against the staged native wrapper over both HTTP and stdio, including input-close
privacy and the SSE/polling difference for settled live pictures. Five selected `test-native`
cases also passed: artifact/version, whole agent turn, Python execution and guarded file IO.
The native and beta release workflows now run the shared installed-SDK native action.
Implementation and local SDK acceptance are complete; external release gates remain below.

The editing E2E classpath regression is fixed and unit-tested. A real `py-repl-compute` run
converged with no tool errors but failed its required-REPL assertion because the model only
used `cat`; cleanup reported a timeout. A cross-model retry timed out starting its isolated
source gateway. Neither attempt is counted as green. The earlier broader JVM run had 272
cases and 36 unregistered `:shell` / `:_shell-wait` failures without a clean-base comparison;
selected suites do not establish a green whole repository.

Reusable distribution CI covers Linux/macOS, CPython 3.11–3.14 and PyPy 3.11, plus actual
HTTP/stdio engine integration; native workflows also test the staged release through the SDK.
A manual protected PyPI publishing workflow consumes distribution CI's
verified artifacts. Workflow lint passed, but the remote matrix has not run. PyPI project
ownership/trusted publishers, SDK publication and a Vis deployment remain external gates;
none is implicitly authorized by this plan. No second runtime release is needed.
