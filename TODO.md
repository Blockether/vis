# Python API and PyPI readiness

Implement and verify the Python SDK for extensions, remote sessions and local engine processes.

## Scope and boundaries

- `vis-agent` imports as `blockether.vis`; `vis-contract` imports as `blockether.vis_contract` and owns the portable JSON contracts.
- The engine supplies the extension SDK, forms and live views: no pip install inside Vis.
- Remote Python applications install the SDK only. Local applications also need a compatible
  Vis executable. The SDK starts and stops its own subprocess, not the user's
  gateway. Local execution uses stdio, not an in-process JVM.
- No publication, tag, commit, live gateway restart or deployment is authorized by this checklist.
- Detailed implementation phases: [Python SDK plan](packages/vis-agent/PLAN.md).

## 1. Public API and extension SDK

- [x] Document hosted, outside, remote and local execution, with typing and installed-wheel examples.
- [x] Use the same SDK in the wheel and engine; prove registration, state, tools, forms and live Views
  across the released worker boundary, including frozen/slotted dataclasses.
- [x] Define errors, timeouts, cancellation, ownership and noninteractive behavior.
- [x] Remove obsolete import aliases, the public generic `call` and retired host search aliases.

## 2. Gateway client

- [x] Explicit authenticated URL/token, canonical routes/headers/protocol/errors and automatic lease keepalive.
- [x] Session CRUD, turns/history, submission, cancellation, transcripts, artifacts and binary attachments.
- [x] Cursor resume, duplicate suppression and bounded reconnect; never retry mutations automatically.
- [x] Typed input/live Views, form answers and schema-validated operator actions.
- [x] All canonical SDK operations and four speech/voice job streams, with shared HTTP/stdio semantics.
- [x] Error/auth/protocol/timeout/disconnect tests and a complete isolated real HTTP engine flow.

## 3. Local engine without an HTTP gateway

- [x] Versioned stdio reuses engine operations and canonical wire encoding, without a hidden HTTP server.
- [x] Own only the SDK child process: explicit executable, bounded startup/shutdown, malformed-frame
  handling, EOF/crash cleanup and no orphan worker or retained temporary database.
- [x] Share session, turn, event and error types with the remote client.
- [x] Actual provider double + extension tool + live/form interaction + Activity + cancellation flow.
- [x] Verify the freshly built native binary and its staged wrapper, not only the JVM entrypoint.

## 4. Distribution and publication gates

- [x] Build both sdists and build wheels from those sdists; strict twine checks on all four artifacts.
- [x] Install outside the checkout with isolated Python; assert installed imports and run package tests.
- [x] Document explicit local executable selection; no hidden engine download.
- [x] Add reusable distribution and real-engine CI plus a manual, protected PyPI Trusted Publishing workflow.
- [ ] Run the remote Linux/macOS CPython 3.11–3.14 / PyPy 3.11 matrix; workflow lint alone is not proof.
- [ ] Configure PyPI project ownership and both trusted publishers (external authorization required).
- [ ] Publish SDK packages only after explicit authorization and passing required checks.

## 5. Canonical View, Activity and released runtime

- [x] `blockether.vis` and `blockether.vis_contract`, with PEP 561 markers and no namespace parent initializer.
- [x] Typed View and Activity projections on both transports; closed schemas reject retired envelopes.
- [x] Distinguish host form definitions/answers from public input Views/close receipts. Never stream
  submitted values or secret handles. Support live close receipts with or without the final view state.
- [x] Activity has engine-owned identity, clocks and lifecycle, bounded/redacted evidence and explicit
  omissions. Extension declarations choose presentation only; Activity is neither a View nor model context.
- [x] Cross-validate SDK, engine, standalone TUI and Companion against shared contract fixtures.
- [x] Publish runtime v0.5.0, verify platform archives/JVM jar, pin its immutable commit and test the download.

## Verification recorded for this plan

These are separate suites, not an additive total:
- Final installed wheel pair outside the checkout: **191 passed, 3 opt-in cases skipped**.
- Installed SDK actual JVM HTTP/stdio and process/error tests: **19 passed**, including tools,
  View updates/form answers, Activity, completion, cancellation and process cleanup.
- Installed SDK against the fresh staged native release: **19 passed**, including public close
  receipt privacy and the SSE/polling state distinction. Selected `test-native` cases: **5 passed**.
  GraalVM CE 25.3.4.1 build succeeded, 12 GiB heap / 10.20 GiB peak RSS. Native/beta workflows
  now run the shared installed-SDK native action.
- Engine contracts, gateway and worker: **227 passed**. Extension/Activity/View integration: **108 passed**.
- Canonical host bindings/environment: **43 passed**; the no-search-alias regression failed before the fix.
- Standalone TUI chat/live View/human input: **201 passed**. Companion: **134 passed**, plus typecheck/lint.
- Affected Clojure reflection/lint and Python lint/format passed. All four artifacts passed strict twine;
  36 documented Python examples parsed and 24 repository source links resolved. Workflow lint passed.
- Runtime v0.5.0: **157 tests / 746 assertions**, all five release jobs successful, four platform archives
  and JVM jar uploaded. Commit: `5acd3f0e861b8735b56058ebea3bf9c5041b2291`. Integration uses the release.

## Remaining evidence and release gates

- Implementation and local SDK acceptance are complete. The following checks remain unverified:
- Editing E2E `py-repl-compute`: the runner's relative-classpath bug is fixed and unit-tested.
  A model run completed without tool errors but omitted required `repl_eval`; cleanup timed out.
  A multiple-model retry timed out starting its source gateway. Neither attempt passed.
- Earlier broad JVM run: 272 cases / 36 unregistered `:shell` / `:_shell-wait` failures, without a
  clean-base comparison. Passing selected suites does not establish a passing repository-wide suite.
- View/Activity and transport envelopes are named records. Endpoint data without a canonical schema
  remains explicitly JSON-typed; a second hand-maintained endpoint model is deliberately rejected.
- Remote platform CI has not run. PyPI last returned 404 for the SDK distributions; no name reservation,
  trusted-publisher setup or SDK publication was performed. Runtime GitHub publication is complete.
- The SDK changes and this checklist are submitted to `main` together at the user's request.
  Deployment, SDK publication and a live gateway restart are not part of that request.
