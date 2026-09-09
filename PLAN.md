# Extension Center

Serve Extension Center from a Cloudflare Worker with D1, using the exact Vis light documentation stylesheet.

## Context
The first draft stored uploaded packages and displayed a monospace split-pane catalog.
The requested model is link aggregation: public GitHub repositories, with pyproject.toml
and extension.py at the root or an explicitly selected subdirectory. No archive upload,
archive storage, registry installation, publisher token or compatibility path remains.
Pi's package catalog provides the process reference: search/type filters, useful sorting,
a dedicated detail page and a copyable installation command. Vis uses GitHub stars rather
than npm download counts. Keep automatic dependency preparation and last-good reload.
Main publication and GitHub Actions deployment were authorized in turn 12. No live gateway restart.

## 1. Repository contract and installation
- Rationale: inspect public metadata without executing repository code; install reviewed source.
- Data: SDK extension_package, CLI, shared manifest validation.
- Acceptance criteria: root/subdirectory support, explicit trust, pinned Git revision,
  atomic installation, source links, no archive or registry implementation.
- Unknowns: resolved; trusted Git execution is covered on JVM and in the native executable.

## 2. Link catalog
- Rationale: index repositories rather than distribute packages.
- Data: Worker-rendered HTML, D1 public metadata, fixed-host GitHub API and pinned manifests.
- Acceptance criteria: protected preview/submission, pending moderation, idempotency, bounded requests,
  cached public reads and no secrets or direct database access in the browser.
- Unknowns: none in local implementation; production bindings are provisioned during deployment.

## 3. Interface
- Rationale: browsing should not reserve half the screen for an unselected detail pane.
- Data: Pi catalog process; the documentation stylesheet and bundled font in resources/vis-docs/assets.
- Acceptance criteria: shared typography, header, sidebar, content width and breakpoints; category counts,
  search, grid/list, stars/updated/newest/name sorting, separate details, GitHub submission flow,
  keyboard navigation, responsive layout and loading/empty/error states.
- Unknowns: none; supported categories remain tools, providers and workflows, not unsupported Pi resources.

## 4. Verification and documentation
- Rationale: remove the old distribution flow from every consumer and prove the replacement.
- Data: Worker/D1/UI/Lazytest coverage, browser review and Worker-rendered HTML preview.
- Acceptance criteria: affected tests, formatting, lint/reflection, native boundary coverage,
  root and monorepo examples; review desktop, tablet and phone without touching live services.
- Unknowns: none beyond checks recorded below.

## Plan state
1–4 complete locally. The Python web server is removed; the Worker renders catalog/details as HTML
and D1 separates public listings from pending submissions. Installer behavior is unchanged.

Verification for the Worker application:
- 28 tests passed: actual local workerd/D1 plus UI, including Turnstile replay/action/hostname
  rejection, rate limits, pending isolation, cache, SSR, escaping and delayed widget loading.
- 44 affected Clojure documentation/resource tests passed; formatting, lint and reflection passed.
- ESLint, build, npm audit and Wrangler dry-run passed. D1 schema initialized locally only.
- Docs, Worker and inline preview have matching measured shell geometry, fonts and colors.
  Browser review covered 1280, 834 and 393 px, coarse input, 130% text, details and submissions.
- Preview uses the exact Worker renderer/assets and explicit GitHub/Turnstile/API fixtures.
- Public docs link is configured by VIS_EXTENSION_CENTER_URL; it adds no doc() entry.

Production D1, managed Turnstile and the Worker runtime secret are provisioned. GitHub Actions
uses a separate, account-scoped deployment token; application secrets are not stored in the repository.

Earlier installation verification (installer unchanged by the Worker replacement):
- SDK and catalog: 379 passed, 9 optional tests skipped, using the prepared project interpreter.
- Affected JVM suites: 384 passed, including dependency preparation and last-good reload.
- Native image built; local-subdirectory and pinned GitHub installation tests passed.
- Python formatting/lint and all 20 local documentation links/anchors passed.
- Successful GitHub inspection tests use HTTP fixtures; real Git transport tests use local repositories.
  A live public repository without an extension manifest correctly reports the required folder/files.

Turn 12: source-filtered GitHub Actions deployment added; 30 Worker/UI/deployment tests pass.
Turn 15: 365 SDK tests and 233 affected JVM tests passed after integrating current main;
36 Worker/UI/deployment tests, actionlint, npm audit, build, Wrangler dry-run and Gitleaks passed.
Remote main publication, the source-filtered Worker workflow and public docs deployment succeeded.
Live HTML, catalog, CSS and JavaScript return 200; credential scans found no keys in tracked
changes, built assets or those public responses. The docs and Worker use the same stylesheet,
with only the font URL adjusted for each serving path.
Bounded deployment-readiness retries and the Turnstile named-element regression are covered.
The production Turnstile SDK and widget load; completing its interactive challenge and submitting
a repository still requires a human check. No fixture catalog entries were published.
The local default checkout was left untouched; temporary review services and browsers are stopped.

---

# Session health in app metrics

Implement the approved view using measured session data, not demonstration values.

## Context
The Companion health component and stories already render the approved layout. The usage
endpoint currently returns lifetime totals only. Extend the existing request/usage pipeline;
do not reconstruct a prompt from files on disk, add polling to every list row, or deploy.

## 1. Capture and persist request health
- Rationale: health belongs to one actual request, not cumulative billing.
- Data: context renderer, loop request boundary, persisted iteration metadata.
- Acceptance criteria: request-aligned budget, limit and prompt estimates; guidance access
  distinct from loaded instructions; missing measurements stay absent; regression tests.
- Unknowns: existing persistent metadata field and prompt assembly location.

## 2. Connect the usage endpoint and Companion
- Rationale: reuse the on-demand metrics read and the approved UI.
- Data: SQLite usage query, gateway wire projection, SessionUsage, SessionStatsPanel.
- Acceptance criteria: historical and active sessions read measured facts; no fixtures in
  production; totals remain separate; tests cover absent/stale data and the client boundary.
- Unknowns: whether historical requests retained enough metadata for a partial snapshot.

## 3. Verify
- Rationale: prove behavior across the engine, persistence, gateway and UI boundary.
- Data: affected Lazytest/Vitest/Storybook tests, lint/reflection, formatting, app build.
- Acceptance criteria: affected checks pass; approved layout preserved; no deployment,
  gateway restart, commits or unrelated work changed.
- Unknowns: test/runtime availability.

## Plan state
1–3 complete locally. Request health is captured without prompt contents and persisted with
provider input; the existing usage endpoint feeds Companion on demand. Historical budgets
stay absent. Linked-root instruction read receipts are not available and remain unknown.

Verification: 799 affected backend cases passed across loop, SQLite, prompt and gateway
suites; after the final arithmetic-only fix, all 38 prompt cases passed again. Seven Clojure
files pass formatting and lint/reflection with no findings. Companion affected unit tests,
14 Storybook cases, typecheck and React compiler lint pass; production build passes.
Historical and unknown-read states were inspected at 393×852 with annotated screenshots.
No deployment, gateway restart, commit or push. Unrelated local changes were preserved.

# Automatic OAuth return across Vis clients

Sign in in the browser; finish in the client that started the flow.

## Context
Provider auth (`internal/provider/auth.clj`) and MCP auth own credentials on the gateway.
Before this work, TUI pasted provider callbacks and Companion polled device flows only. Model
providers register fixed localhost redirects; MCP can register a redirect dynamically. Desktop
is a Pake wrapper, not a native callback host. Existing tokens do not prove a fresh login.
Do not change provider redirect URIs speculatively, expose PKCE/token material to clients,
restart the live gateway, deploy anything, or replace unrelated local work. OAuth must NEVER pass
through the notification relay/Cloudflare, including authorization codes and browser fallbacks.

## 1. Callback transport and lifecycle
- Rationale: the same gateway-owned exchange must accept a verified callback once, whether
  it arrives locally or from a paired client.
- Data: provider and MCP start/complete/poll/cancel; registered redirect, state, expiry.
- Acceptance criteria: automatic completion, strict state/destination matching, one exchange,
  bounded listener lifetime, cancellation, sanitized errors and fresh-flow regression tests.
- Unknowns: callback transport that native mobile and the current desktop shell can support.

## 2. Client return paths
- Rationale: local listeners on a remote gateway cannot receive a phone/desktop callback.
- Data: TUI browser flow, Companion authentication, desktop/native packaging, deep links.
- Acceptance criteria: TUI, desktop, iOS and Android close the pending UI and refresh auth;
  remote gateway is covered; manual input is an explicit fallback, never claimed automatic.
- Unknowns: native fixed-loopback interception on mobile; Pake native integration API.

## 3. Direct return, no shared intermediary
- Rationale: enterprise OAuth must stay between the provider, the initiating client and its gateway.
- Data: TUI loopback; native private-use scheme; paired HTTPS completion; provider device flow.
- Acceptance criteria: delete relay OAuth code/config/dependencies; app state/destination validation,
  no arbitrary callback target or HTTPS fallback; token exchange stays on the gateway.
- Unknowns: fixed provider callback support on native hosts; no unsupported URI substitutions.

## 4. End-to-end verification
- Rationale: tests of existing tokens cannot prove a new login path.
- Data: deterministic fresh OAuth exchanges, provider/device flow suites, client/native tests.
- Acceptance criteria: affected tests, formatting, lint/reflection and builds; fresh real-provider
  consent and native return tested before claiming the entire feature works.
- Unknowns: interactive consent and real-device availability in this session.

## 5. One authentication mechanism
- Rationale: MCP and model-provider adapters must not maintain duplicate flow registries or client loops.
- Data: shared gateway lifecycle under `internal/provider/flow.clj`; one Companion watcher in
  `src/lib/oauth.ts`; TUI already uses `tui/oauth.clj` for both domains.
- Acceptance criteria: both adapters exercise the same callback validation, single exchange,
  retained verdict, expiry, supersession and cancellation; native callback reception works for
  any adapter declaring the supported app return, without provider-name branches.
- Unknowns: registered provider redirects and native OS constraints remain unchanged.

## 6. Native fixed-loopback transport
- Rationale: a phone browser cannot return to localhost on a remote gateway. Provider
  registrations stay unchanged; a generic receiver must run on the initiating device.
- Data: shared Companion watcher; iOS system Safari view and explicit IPv4/IPv6 sockets;
  Android system browser and bounded loopback listener, installed by native preparation.
- Acceptance criteria: no provider-name branches; readiness before opening, strict callback
  validation, cancellation/expiry, no callback in navigation or persistent storage; native
  socket tests and platform compilation, followed by fresh real-provider/device evidence.
- Unknowns: desktop Pake has no native callback bridge; Android task-return policy and
  provider consent in real system browsers still need actual-device verification.
- State: implemented in the shared watcher and canonical native sources under
  `apps/vis-companion/native/`. Preparation registers both bridges; native socket suites
  and iOS/Android builds pass. Real consent and platform-return acceptance remain open.

## Plan state
1. Callback lifecycle implemented: one-shot state/destination validation, bounded expiry,
   cancellation and retained verdicts for lost completion responses. PKCE and tokens stay
   on the gateway. Anthropic's CSRF nonce is independent of its PKCE verifier.
2. Client paths partially complete: TUI receives loopback returns locally; Codex/Copilot
   use direct device flow. Companion forwards native app returns and device-local fixed
   loopback callbacks to the initiating paired gateway, without changing provider redirects.
   Desktop/Pake and web retain explicit manual fallback for remote fixed-loopback flows.
3. No-relay requirement implemented: removed the callback handler, Durable Objects,
   migrations, origin setting and direct runtime-test dependencies. Notification behavior
   is unchanged. OAuth routes return 404 and do not forward or echo callback query values.
4. Shared mechanism complete: model and MCP adapters delegate to `provider.flow`, and
   both Companion consumers use `lib/oauth.ts`. Separate flow registries and client loops
   were removed. A model adapter declaring the allowed app return is covered without any
   provider-name branch; fixed provider redirects remain fixed.
5. Regressions cover retained manual verdicts, MCP supersession, slow old starts, namespace
   isolation, expiry, cancellation, serialized client submissions and hung-poll deadlines.
   Public errors from the shared engine are sanitized; adapters keep protocol-specific IO.
6. Native transport code and deterministic verification are complete. Fresh provider consent
   and native OS return remain open.

MCP app mode registers only `com.blockether.viscompanion://oauth/callback`, with no
public HTTPS fallback or gateway listener. Both the advertised redirect and authorization
URL must match that destination. Completion requires the full URI and matching state.
iOS/Android preparation registers the scheme; native bridge logging is disabled and OAuth
returns bypass the pairing/share dedupe store. A killed app must restart sign-in rather
than accept an unbound cold-start callback.

Companion and the canonical Clojure gateway client require paired HTTPS off loopback for
model/MCP auth and refuse HTTP redirects. A real local redirect regression reproduces the
old forwarding behavior and verifies that callback bodies now stay on the paired origin.

Current verification: the full Companion suite passes 2,277 tests, with one existing skip
  (248 files, including 153 Storybook interactions). Typecheck, React compiler lint and web
  production build pass. All 153 stories across 10 themes pass contrast checks. The first full
  run caught the old exact native-plugin registration expectation; it was updated to include
  OAuthLoopback while preserving the existing plugins, then the full suite passed.
  Earlier shared-engine verification passed 261 gateway/MCP/provider and 88 TUI tests, plus
  Clojure formatting and lint/reflection; those production namespaces did not change here.
  The no-relay suite passed 38 tests in the preceding implementation.

Swift XCTest and Android JUnit each pass four native cases, including real IPv4/IPv6 socket
  callbacks, invalid state/destination, cancellation and expiry. iOS simulator and Android
  debug app builds pass; iOS preparation passes its check. Android lint now passes with zero
  errors and 21 existing warnings. The splash preparation uses the AndroidX attribute only,
  including on already-generated projects; five regressions cover this without suppressions.

Spel captured the native-loopback UI fixture at 393x852 and 834x1194 with a measured coarse
  pointer, and 1280x800 with a fine pointer, without horizontal overflow. The fixture and
  provider-hook regression use a synthetic adapter/flow, not actual provider consent.

Before the shared-engine refactor, GraalVM CE 25.3.4.1 built `target/vis`; five linked-binary
  smoke cases passed. That refactor has not rebuilt this binary. Those checks are not proof
  of a native mobile OAuth return.

The iOS test app installed and launched on a dedicated iPhone 17 Pro/iOS 26.5 simulator.
  Spel 0.9.31 could inspect the native screen but exposed only NATIVE_APP, not an inspectable
  WKWebView, even after explicitly reinstalling a debug build with CAPACITOR_DEBUG=true.
  This blocked exercising the actual Capacitor callback promise through automation; it is
  not evidence that a fresh provider login succeeded. Ordinary build settings were restored.

Not complete: fresh real-provider consent and automatic native return with a remote gateway,
  including Claude; Android system-browser task-return policy; Pake has no native callback
  bridge. Codex/Copilot device flows finish authentication but cannot guarantee an automatic
  browser-to-app switch. The earlier real Codex endpoint check accepted initiation only;
  no live consent, token exchange or credential write was performed.

Temporary browsers, Appium and test servers were stopped; only the simulator created for
  this check was shut down and removed. No deployment or live gateway restart was performed.
  The user authorized committing and pushing the scoped changes; unrelated shared-worktree
  work is excluded. The preceding shared-engine refactor was included by a concurrent commit.

# Presentation secret containment

Keep credentials out of Activity, input errors, and Live view records without changing tool data.

## Context

Confirmed runtime gaps are in `activity/event.clj`, `view/core.clj`, and `view/sink.clj`:
labelled credentials in strings/errors, private key fields, callable envelopes, secret defaults,
validator echoes, and Live text persisted unchanged. Shared pure text/key redaction belongs in
`internal.util`; Live field selection belongs in `view.materializer`. Raw tool results, vault
semantics, structural IDs, and unrelated loop work must remain unchanged. Reject global secret
registries, blanket rewriting of source literals, and rewriting existing records.

## Phases

1. Reproduce the presentation gaps with synthetic fixtures.
   - Rationale: prior passing suites did not cover these boundaries.
   - Data: Activity, input, and isolated Live lifecycle regression tests.
   - Acceptance criteria: confirmed failures before production edits; no real secrets printed.
   - Unknowns: additional validation and size-limit constraints exposed by the tests.
2. Contain secrets at presentation boundaries.
   - Rationale: preserve usable raw results while protecting rendered and persisted data.
   - Data: shared redactor; Activity walker; input projection/errors; Live lifecycle and sink.
   - Acceptance criteria: callbacks hidden, defaults refused, errors scrubbed, public text redacted;
     IDs/order and normal data preserved; redaction idempotent and traversal bounded.
   - Unknowns: unknown unlabelled/encoded secrets cannot be inferred from arbitrary text.
3. Verify the affected contracts and report the deployment boundary.
   - Rationale: formatting, reflection, and integration regressions matter beyond unit examples.
   - Data: affected Lazytest suites, formatter, lint/reflection, scoped diff review.
   - Acceptance criteria: affected checks pass; no unrelated changes included; no gateway restart,
     credential rotation, release, or historical-record mutation.
   - Unknowns: independent concurrent edits and local environment failures, if encountered.

## Plan state

Phases 1–3 complete locally.

- Regression tests reproduced the Activity, secret-default, validation-error and Live lifecycle
  failures before the fixes. Follow-up tests caught nested text, content-budget and stat-text gaps.
- 300 Activity/View/util/contract tests and 120 Python extension/runtime/worker tests pass.
  The Python boundary test verifies raw typed values remain unchanged while Activity is redacted.
- Formatting is idempotent; clj-kondo and reflection/boxed-math lint report no findings across
  the ten changed Clojure files. Scoped diff checks pass.
- Live tests use isolated records and synthetic secrets. No live gateway restart, credential
  rotation, release, deployment or historical-record rewrite was performed.
- Unrelated loop and Companion OAuth work is excluded. Unknown unlabelled or encoded strings
   remain outside pattern-based redaction; previously exposed credentials still require rotation.

# Joined Activity design

One continuous Thinking / Code / Activity panel with chronological operation groups.

## Context

Companion's `ChatContent.tsx` displays Thinking and Code as adjacent sections. `ActivityPanel.tsx`
still indents individual steps and lacks adjacent-operation grouping. Use the existing projection
and production controls; do not change SDK, transport, raw results or unrelated desktop work.
Reject nested cards, regrouping across chronological boundaries and parsing human summaries.

## Phases

1. Specify grouping and disclosure behavior in tests.
   - Rationale: streaming replacements must preserve order, facts and reader choices.
   - Data: ActivityPanel and ExecutionTrace tests; deterministic story fixtures.
   - Acceptance criteria: failures before implementation, covering adjacency and visible errors.
   - Unknowns: existing projections may lack command correlation or complete resource counts.
2. Implement and render the joined panel.
   - Rationale: alignment alone does not establish visual continuity or useful aggregation.
   - Data: production Activity, transcript composition and existing disclosure controls.
   - Acceptance criteria: shared edge, independent folds, stable groups, no hidden failures.
   - Unknowns: touch layout and existing custom-presentation interactions.
3. Verify and deliver an interactive design artifact.
   - Rationale: tests cannot establish the actual appearance or portable artifact behavior.
   - Data: unit, Storybook, lint and build checks; Spel review; bundled production story.
   - Acceptance criteria: reviewed phone/tablet/desktop states and self-contained attachment.
   - Unknowns: available browser tooling and unrelated baseline failures.

## Plan state

Phases 1–3 complete locally. Commit and push to main authorized; SDK and deployment remain out of scope.

- Adjacent operation groups and independent folds implemented; raw projection data is unchanged.
- Removed the decorative assistant timeline and step marks. Commentary and final prose now share
  the Thinking text edges; a browser regression reproduced the prior mismatch before the fix.
- Latest checks: 381 affected unit tests, 160 Storybook tests, lint and production build pass.
  Storybook build and the all-theme contrast check pass.
- Production story and standalone HTML match at phone, tablet and desktop frames. Phone prose
  measures left 34 / right 367 for thinking, commentary and answer at 393px width.
- Review uses deterministic fixtures, not a gateway. Native iOS/WKWebView remains unverified.
- SDK semantic requirements (including command correlation) are continued below.

# Activity across every client

One grouping contract, joined execution sections in Companion and TUI, typed groups in the SDK.

## Context

The Companion-only design is on main. TUI still renders a separate timeline. Portable receipts
already contain operation, sequence, resources, evidence and shell-handle groups; adding another
transport shape or parsing human summaries would duplicate that information.

## Phases

1. Specify portable grouping behavior in tests.
   - Rationale: all readers must preserve chronology and shell correlation.
   - Data: contract vocabulary and shared fixtures, Clojure/TypeScript/Python tests.
   - Acceptance criteria: adjacent families, unknown operations, state changes, stable shell identity.
   - Unknowns: consumer test environments.
2. Join TUI sections and integrate readers.
   - Rationale: every client must support the same interaction, not just the app.
   - Data: production renderer, expansion store, shared contract and SDK receipt types.
   - Acceptance criteria: aligned continuous sections, independent folds, visible errors and facts.
   - Unknowns: older timeline assertions and terminal geometry.
3. Verify all affected clients.
   - Rationale: wire compatibility, reader choices and terminal rendering need separate checks.
   - Data: affected suites, formatting, lint/reflection and production visual captures.
   - Acceptance criteria: passing checks and reviewed terminal/browser render; no deployment or restart.
   - Unknowns: local toolchain availability.

## Plan state

Phases 1–3 complete locally. No release, gateway restart or remote publication requested.

- Canonical operation families and eight shared fixtures drive Clojure, TypeScript and Python grouping.
  The SDK exposes immutable groups; existing serialized receipts and raw operation counts are unchanged.
- TUI now joins Thinking, Code and Activity in one panel. Adjacent groups preserve sequence,
  shell identity, independent disclosure choices, resource/diff facts and visible failures.
- Verification: 1,766 TUI tests, 43 contract/engine tests, 102 Companion tests and 316 SDK tests pass
  (three SDK skips). Formatting, lint/reflection and the Companion production build pass.
- Live production HtmlTerminal review checked group/code/band folds, streaming replacement, focus,
  aligned cells and resize down to 390px without horizontal overflow. An exact-frame HTML export
  and a native terminal PNG were inspected; parity tests cover 40/80/120 columns and three states.
- Native mobile/WKWebView and an installed release binary were not rebuilt or exercised. Browser
  interaction review used the production renderer with a fixture relay, not the full channel input loop.

# Portable HTML review workflow

Readable static exports and a one-call local live preview, without changing gateway transport.

## Context

`~/lanterna/src/main/resources/com/googlecode/lanterna/terminal/html/terminal.html` shares live
and static presentation. Static frames retain desktop columns and full viewport height.
The review fixture did not configure terminal defaults to match its painted theme.
Do not fix this by weakening Companion's opaque-origin iframe sandbox or copying a renderer.

## Phases

1. Repair static export in Lanterna.
   - Rationale: attachments must remain readable outside a live JVM.
   - Data: exported frame, browser geometry, renderer tests.
   - Acceptance criteria: fit/actual-size controls, explicit row bounds, configured theme, unchanged cells.
   - Unknowns: mobile frame sizing and font measurement.
2. Simplify live review and integrate a reusable TUI fixture workflow.
   - Rationale: review should not require handwritten HTTP/SSE adapters each time.
   - Data: framework-neutral endpoint, production fixture and local server lifecycle.
   - Acceptance criteria: one closeable preview handle, loopback-only server, maintained export path.
   - Unknowns: local dependency verification without publishing a library release.
3. Verify exported bytes in the Companion frame.
   - Rationale: a live desktop screenshot does not verify a static mobile attachment.
   - Data: phone/tablet/desktop geometry, fit/zoom actions and affected suites.
   - Acceptance criteria: no initial horizontal clipping, no incorrect background, safe sandbox retained.
   - Unknowns: native WKWebView availability.

## Plan state

Phases 1–3 complete locally. Lanterna checkout was fast-forwarded to published 3.1.5-vis.49
before edits; its release version and Vis's production dependency pin remain unchanged.

- Static exports have Fit width/Actual size, explicit visible-row bounds, correct configured
  theme defaults, no input cursor and no invisible keyboard-input tab stop.
- HtmlTerminalPreview owns only a loopback development server; HtmlTerminal remains transport-neutral.
  HtmlTerminalView.serve owns GUI2 plus that preview. Closeable lifecycle and request validation are tested.
- The :html-review alias selects sibling compiled classes explicitly. One CLI command runs the
  production Activity fixture, handles disclosures/resize and updates the bounded HTML export.
- Verification: 241 Lanterna tests, 15 affected TUI/development tests against local classes,
  14 HTML-backend tests against the unchanged published dependency, and 23 Companion DocArtifact
  tests pass. Clojure formatting and lint/reflection are clean; both repository diff checks pass.
- Browser review covered live code/group disclosure and resize, static fit/actual-size at
  393/834/1280px, and the exact exported bytes in Companion's opaque-origin DocFrame. Native
  terminal PNG and grid parity were checked. No Companion sandbox relaxation or app-code change.
- Native iOS/WKWebView and a native-image binary were not exercised. No release, remote mutation
or gateway restart; installed binaries and previously generated HTML files are unchanged.

# Token-efficient built-in tool results

Keep complete programmatic data and print the information needed for the next decision.

## Context
The session audit found oversized shell/test representations, a global session index in
`read_session`, duplicate transcript projections, and historical discovery/shape errors.
The audit reproduced the redundant index and null-heavy test result. Relevant owners
are `internal/foundation/introspection.clj`, `internal/python/env.clj`, guest Python and
the canonical SDK. Runtime machinery belongs to the sibling `vis-python-runtime` repository.
Do not remove shell metadata, weaken patch validation, lower the global output limit, change
cache/folding policy, or infer current defects from historical errors. Preserve unrelated work.

## 1. Reproduce and pin contracts
- Rationale: distinguish current failures from obsolete API observations.
- Data: current sandbox results, SDK tests, Lazytest boundary tests, result consumers.
- Acceptance criteria: regression tests for each changed behavior; discovery remains available
  after bootstrap and refresh; no guessed aliases or discarded safety/diagnostic fields.
- Unknowns: result-wrapper ownership, transcript consumers, existing test dependencies.

## 2. Compact result presentation
- Rationale: Python keeps the data; default printing should not expand administrative fields.
- Data: shell states/log pages, test verdicts/failures, apropos records, session projections.
- Acceptance criteria: short deterministic views, explicit failure/timeout/truncation signals,
  full mapping/serialization and pagination preserved; embedded and SDK boundary coverage.
- Unknowns: which presentation hooks already exist without duplicating runtime code.

## 3. Lean session reads and aligned documentation
- Rationale: one conversation should not query all sessions or repeat every content projection.
- Data: canonical transcript builders, model-facing consumers and documentation contracts.
- Acceptance criteria: no global index query; one full block-based model transcript retaining
  folded history and diagnostics; human exports unchanged; documented canonical key names.
- Unknowns: data unique to alternate transcript projections.

## 4. Verify and measure
- Rationale: size reduction must not hide failures or break the next programmatic decision.
- Data: deterministic fixtures, affected SDK/JVM tests, formatting, lint/reflection, diff checks.
- Acceptance criteria: affected checks pass and before/after representation sizes are reported
  separately from task success or provider billing; no gateway restart, deployment or publishing.
- Unknowns: local runtime dependency verification and native-image availability if needed.

## Plan state
- [x] Current redundant index and verbose shell/test results reproduced.
- [x] Regression tests and compact result implementation.
- [x] Lean session read and documentation.
- [x] Pytest collection/early-exit failures cannot become a successful empty run; setup skips
  remain skipped, and faults retain their complete diagnostic.
- [x] Content-addressed guest modules prevent mixed engine versions from overwriting each other.
- [x] Verification and measurements.

Verification: 473 affected JVM cases passed in a clean run using the released runtime, with the
normal network guard. The 55 SDK shell/view tests passed in an isolated project environment with
its declared jsonschema dependency. Formatting is clean; clj-kondo and reflection/boxed-math lint
passed for 10 Clojure files, and Ruff passed for both guest Python files. All five local documentation
links resolve; scoped diff checks pass. The transient runtime archive/dependency setup blockers
were resolved without changing the dependency pin or disabling test guards.

Measured with `o200k_base` on the same input before/after each change:

| Input | Before tokens | After tokens | Reduction |
|---|---:|---:|---:|
| Shell handle with a short successful log | 200 | 18 | 91.0% |
| Test result with 48 passing cases | 419 | 47 | 88.8% |
| Nine common apropos rows | 403 | 265 | 34.2% |
| Captured session, complete JSON projection | 502,292 | 229,001 | 54.4% |

The captured session's default print is 186 tokens; the complete projected data remains accessible.
These are representation-size measurements, not task-success or provider-billing savings. No paid
model benchmark or native-image build was run. A final cross-check reran the eight affected test
namespaces (341 cases) against the current tree before commit. No gateway restart, deployment or
release; unrelated working-tree changes are preserved.

# Council: active-session communication

Shared conversation log with explicit pings; prepare executable tests before production code.

## Context

Council lets active sessions coordinate in both directions, independently of parent/child
relationships. This revision records cross-validation fixes, the public API/session contract and
history/Activity correlations, with a tests-first implementation sequence. This remains a plan and
test specification, not a claim that executable Council tests exist. Preserve all unrelated work.

Current owners and constraints:
- `src/com/blockether/vis/internal/gateway/state.clj` owns live turns, queues and per-session
  idempotency. Its registry can also mirror foreign turns: a visible row alone is not proof that
  this engine owns execution. Council initially supports participants in one execution authority.
- `src/com/blockether/vis/internal/persistance/sqlite/core.clj` owns SQLite access. A session's
  `workspace` is not the shared project identity; the default group uses the owning `project`.
  `db-get-project-by-root` resolves a canonical root, not an arbitrary worktree or current directory.
- DDL belongs to `resources/db/sqlite/migration/V1__schema.sql` and the existing migration runner.
  SQLite uses WAL, IMMEDIATE write transactions and `synchronous=NORMAL`.
- `src/com/blockether/vis/internal/context/prompt.clj` builds the stable system prefix.
  `turn-system-context-block` is NOT the place for peer messages. The iteration loop in
  `src/com/blockether/vis/internal/loop.clj` assembles conversation suffixes; `render-ctx-delta` in
  `src/com/blockether/vis/internal/context/renderer.clj` renders session-state changes, not an inbox.
- `packages/vis-agent/src/blockether/vis/engine/_local.py` gives `LocalEngine` a private process AND
  temporary database. It does not join the daemon's Council; closing it removes that private store.
- `packages/vis-agent/src/blockether/vis/engine/_client.py` creates turns through `Session.send`.
  Council publication is a separate operation and must never submit a turn.

Cross-validation evidence: a sandbox SQLite probe of the old JSON-filtered pending query executed
approximately 9,000 VM instructions for 1,000 unrelated entries and 900,000 for 100,000, repeating
that work when no ping advanced the cursor despite using an index. Executable models also exposed
late publication reaching a new activation, retry failure after a recipient stopped, and global
idempotency-key collisions between authors. These are design counterexamples, not passing Vis tests
or measurements of the bundled JVM driver. Phase 1 must turn them into repository regressions.

Rejected alternatives: a JSON-recipient scan on every iteration; one global idempotency key;
`max(entry_id)` as activation identity; treating index selection as a latency benchmark; putting peer
text in a system message; persistent delivery/approval state; notifications and a second broker;
automatic wakeups, blocking waits or runtime reply gates. A recipient lookup table is not a delivery
state machine. Session termination does not remove the durability requirements of the conversation log.

### Agreed behavior and scope

- Every project has a default Council. Missing `group_id` selects that default; every stored entry
  has a concrete group. Custom groups and parent/subagent membership remain future work, but cursor
  keys and API selectors include `group_id` from the start.
- Members may read the entire group log on demand. There are no private entries. Addressing controls
  pings, not visibility; entries without pings never enter another agent's context automatically.
- A publication may ping selected active participants or all active members. Broadcast excludes its
  author. Resolve and deduplicate recipients from one authoritative registry snapshot. An explicitly
  inactive, foreign-owned or out-of-group recipient rejects a NEW publication without partial writes.
  An empty broadcast is a valid entry with no recipients. Explicit self-pings are rejected in v1.
- Active means running or queued work, not an open UI view. A held queue is visible as held, not as a
  promise of execution. No Council operation resumes it. Presence comes from the runtime, not agents.
- A ping carries author, group, entry and thread identifiers and useful text. Short content arrives
  whole; longer content has a deterministic preview marked `truncated` and a full-entry fetch target.
  Preserve the original preview-and-fetch behavior: a 4,000-character rejection is not its substitute.
  No model generates previews. Larger source material can be a path or session/turn/iteration reference.
- Offer pending pings before the next model invocation if one occurs, within a bounded context budget.
  Do not interrupt a model/tool, wake a session, create a turn or force an extra iteration. Overflow
  stays pending for that activation and is signalled with `has_more`, not silently marked delivered.
- Requests are soft conversation. No tool/completion gates, synchronous waits or required agreement.
  Replies are ordinary entries in the same `thread_id`; they notify nobody without an explicit ping.
  The log records replies and recipient snapshots, not proof that a model read or understood a message.
- Peer text is attributed data, not a user instruction or an authority upgrade. A finished activation
  never receives another ping, and its undelivered pings cannot enter a later activation of the session.
- Provide the same members, publish, threads, read and single-entry fetch operations through the host
  namespace and a session-bound SDK handle. `threads()` lists summaries; `read(thread_id=...)` reads
  their entries through the ordinary paginated read. There is no separate `thread()` content-fetch
  operation. SDK read-only access is not an implicit scope reduction.
  The gateway credential is trusted daemon-level authority, not per-session isolation. Host calls
  derive identity from trusted execution context; the SDK handle internally binds the active generation
  and never silently rebinds after inactivity. New publication by an inactive author fails. Authenticated
  replay of an existing publication does not require the old author or recipients to remain active.

### Groups, threads and public session context

- A group defines membership and one shared log. A thread is a conversation inside exactly one group,
  identified by its root entry. It has no separate membership, permissions, activation or delivery cursor.
  Every group member can read and reply to every thread in that group; a ping does not subscribe a
  participant to later replies. Custom group membership remains future work, not a thread feature.
- Publishing without `thread_id` creates a root entry and thus a new thread; no separate create-thread
  call is needed. Publishing with `thread_id` appends to that existing thread. The selector must identify
  a root in the selected group, not an arbitrary entry. Every returned entry exposes `thread_id`,
  including a root whose thread id is its own entry id. A thread cannot span groups.
- Conversation inside a thread is flat. `thread_id` is the only conversation selector for publication;
  remove message-parent fields from requests, results, storage, history and Activity. `parent_id` is
  unsupported, not a compatibility alias. There are no nested replies or automatic reply-target inference.
- A new thread accepts an optional single-line `title`. Trim and validate an explicit title; reject
  blank, control-containing or over-limit titles. If omitted, derive a bounded title from the first
  non-empty trimmed content line, without a model call or changing the stored content. Reject content
  with no usable line when a fallback is required. Store the title only on the root. A continuation
  with `thread_id` must not supply `title`, even an unchanged one; titles are immutable in v1.
- One group may have zero or many threads; v1 adds no fixed total thread count or lifecycle manager.
  A thread is not a session, subagent or execution thread and never schedules work. Page/content limits
  still apply; reading a thread does not mean fetching an unbounded conversation.
- In `python_execution`, the host supplies `council` directly: no HTTP client, token handling or
  `Session.send()` is needed. Publication commits even when the caller does not `print` its result.
- Keep the existing `session["id"]` as session identity. Add only
  `session["council"]["default_group_id"]` to the host-rebuilt session dictionary. Do not duplicate the
  session id or expose activation ids, cursors, message content or a live member list there. Membership
  is fetched with `council.members(group_id=...)`; omitted group selects the project default.
- A session id survives inactivity; an internal activation id identifies one uninterrupted period of
  running/queued work. Keep it across continuously queued turns, replace it after inactivity, and bind
  stale SDK handles to their original activation. Agents neither choose nor maintain this identity.
  Mutating the Python session dictionary cannot change author, group authorization or activation.

Public forms (planned API, not an existing implementation):
```python
root = await council.publish(
    "Does the new response format affect your work?", title="API contract"
)
await council.publish(
    "The tests pass with that format.", thread_id=root.thread_id, ping=[other_sid]
)
await council.threads(limit=20)
await council.threads(group_id=group_id, after=142, limit=20)
await council.read()
await council.read(thread_id=root.thread_id)
await council.read(thread_id=root.thread_id, after=150)
await council.read(group_id=group_id, thread_id=thread_id)
```
`threads()` discovers conversations; `read(thread_id=...)` reads one; `publish(thread_id=...)`
continues it using exactly the same identifier. Read/list operations share the page envelope: `entries`,
a continuation cursor and `has_more`. Thread-list entries are summaries containing `thread_id`, `title`,
`author_session_id` and `created_at`, not message bodies. Omitting `group_id` always selects the default
in publish, threads and read; a thread in another group requires that group's explicit selector and
authorization. Continuation preserves the group and, for a filtered read, the thread selector.
### Storage and publication

Use the existing database, short transactions and two tables. Content is stored once.

| Owner | Required data and indexes |
|---|---|
| `council_entry` | Server-assigned `INTEGER PRIMARY KEY` id; group; author session and internal activation; source kind (host/SDK) and turn/iteration/block/operation provenance where available; nullable stored `thread_id`; root-only title; content; creation time; non-null idempotency key; normalized request fingerprint. Unique `(author_sid, idempotency_key)` and `(group_id, id)`; indexes `(group_id, thread_id, id)` and `(author_sid, id)` for thread/source-history lookup; partial index `(group_id, id) WHERE thread_id IS NULL` for root-only listing. |
| `council_ping` | Immutable `group_id`, `entry_id`, `recipient_sid`, `activation_id`. Primary lookup key `(recipient_sid, activation_id, group_id, entry_id)`; index `(entry_id, recipient_sid)` for displaying the frozen recipient list. No pending/delivered/replied column. |

A ping row references an entry in the same group. A stored thread reference must also stay inside the
group and identify a root; validate this in the shared publication operation. Roots have a null stored
`thread_id` and a non-null title, and expose their own id as the public thread identity. Continuations
store the validated root id as `thread_id` and have no stored title. There is no separate thread table
or message-parent column. Entries and recipient rows are append-only in v1; no retention/deletion or
caller-supplied entry ids. SQLite serializes writers, so committed generated ids are safe forward
cursors under these restrictions; gaps are valid.

Publication order:
1. Bind and authorize the caller, session and group. Normalize the original request: content, optional
   thread selector and supplied title, group, author activation and ping selector (including sorted/
   deduplicated explicit recipients). A new-thread request keeps an absent thread selector in its
   fingerprint; never replace it with the generated id on replay. Generate one non-empty idempotency
   key per logical call; retries preserve that key and request, including whether a title was supplied.
2. Look up `(author_sid, idempotency_key)` BEFORE checking transient activity. An identical replay
   returns the original entry and original recipient activations. A different normalized request
   returns an explicit idempotency conflict, never another author's result or a fresh broadcast.
3. For a new key, validate payload and capture author/recipient activations from one registry snapshot.
   Do not hold a registry lock while waiting for the database, a tool, a model or an agent response.
4. In one IMMEDIATE transaction, recheck the key to handle concurrent retries, validate the selected
   thread root and title rules, and insert the entry plus all recipient rows. A uniqueness race follows
   the same replay/conflict rules. Any failure rolls back the whole publication, not just its recipients.
5. Return success after commit. A recipient can finish after the snapshot but before commit; the
   immutable old activation target then remains log data, not a ping for its next activation.

Retain `synchronous=NORMAL` initially and state its guarantee precisely: process restart does not
reconstruct active deliveries, while power/OS failure may lose recent acknowledged log commits.
`FULL` remains an explicit durability/performance decision, not a requirement removed by inactivity.
Reopen tests cover the chosen supported behavior; they do not prove power-loss durability.

### Presence, lookup and context insertion

- On inactive-to-active transition, allocate a fresh opaque `activation_id` and initialize empty
  per-group cursor/batch state in the SAME registry transition that exposes the participant. New
  activation cursors start at zero: no old ping can contain their fresh identity. No `MAX(id)` read.
- Keep the activation while running/queued work remains continuously active. Clear it on inactivity,
  cancellation of the remaining work or owner termination. A later turn or engine boot gets a new
  identity. Ignore mirrored foreign turns without authoritative activation ownership.
- `members(group_id=...)` returns a snapshot: session id, title and running/queued/held state, not a
  public activation id. Runtime ownership/generation metadata stays internal. Parent session ids can
  be added with actual subagent support; agents need not maintain task descriptions.
- Pending lookup seeks the `council_ping` primary index by recipient, activation and group, then
  `entry_id > cursor ORDER BY entry_id LIMIT batch_limit + 1`, joining entries by key. Filtering
  unrelated log entries is not part of the algorithm. An empty inbox is cheap even if the group log
  grows; a stationary delivery cursor no longer causes repeated scans of other people's messages.
- Freeze a selected batch as an attributed conversation-data block in the recipient's iteration state.
  Publish that block and advance the corresponding `(session, activation, group)` cursor together;
  advance only through entries actually retained in context, never through a lookahead/deferred entry.
  Preserve the block on provider retry, fallback and prompt reconstruction. Do not perform destructive
  inbox reads from a pure renderer, token estimator or repeated prompt-building call.
- Verify the block on the actual model-input assembly path, including the first invocation of a queued
  turn and iterations without prior tool results. Do not insert it into the stable system/developer
  prefix or mutate previously sent messages. Existing conversation compaction may later fold it normally.
- The guarantee is one context append per entry during an uninterrupted activation, NOT exactly-once
  model consumption. Stateless requests can resend history. Ending the activation drops its live cursor
  and unfinished batch, never the persisted log; it does not schedule further work.
- A pending lookup has a short end-to-end deadline, including SQLite busy waits. On timeout or DB
  failure, leave cursors/batches unchanged, record an observable delivery warning and continue ordinary
  session execution. Retry only at a later naturally occurring invocation; no forced iteration, tight
  retry loop or indefinite wait. This is deferred delivery, not an acknowledgement.

`read(group_id=..., thread_id=..., after=...)` uses group-scoped keyset pages with entry and
serialized-byte limits, a continuation cursor and `has_more`. Without a thread filter it reads the
group log; with one it reads that root and its continuations in entry-id order. Include the root when
it is after the cursor, never repeat it on later pages, and do not require a separate root fetch. Stored
null thread references must not make roots disappear from this filter. Apply the same group/root checks
to publication and filtered read, and group checks to single-entry fetch. No `OFFSET`, separate
`thread()` content-fetch operation or unbounded thread result. Single-entry fetch may return the
bounded full publication content.

`threads(group_id=..., after=..., limit=20)` lists roots in ascending root-entry-id order, using the
partial root index and keyset pagination with count/serialized-byte limits. Return the stored title
and root author/time; do not fetch content, scan/group continuations or compute reply counts/latest
activity. A new continuation neither reorders the list nor changes its title. Empty groups return an
empty page; `has_more` and the continuation cursor follow the same bounded lookahead rules as read.
The cursor is the last returned root id, not a timestamp. New roots after that cursor can appear on
later pages; listing is not a frozen multi-page snapshot. Listing never subscribes, consumes a ping
or schedules work. No additional thread lifecycle, subscription or title-generation service is needed.

Provisional starting limits, to freeze in the canonical contract during phase 1: 64 KiB UTF-8 content
per publication; 256 UTF-8 bytes per title, clipping only a derived fallback title; at most 256 resolved
ping recipients; 1 KiB UTF-8 preview per entry; at most 20 entries and 8 KiB serialized bytes per injected
batch; history/thread-content/thread-list pages at most 50 entries and 256 KiB.
Start with a 100 ms pending-lookup deadline, without changing the shared datasource's default policy.
Also respect remaining model-context capacity using the existing budget machinery. Headers and
metadata count toward a batch budget. Never split a Unicode scalar. Reject invalid/control payloads
consistently; use byte-based validation across Clojure, Python and SQLite, not differing character counts.
A batch that cannot fit remains pending; even its overflow notice must fit. Test exact limits and
continuation without truncating the stored entry or silently dropping an oversized first item.

### Session history and Activity correlations

- The committed entry is authoritative for publication provenance: author, source kind, session and
  available turn/iteration/block/operation identifiers. Source coordinates come from the host, never
  caller-supplied Python metadata. An external SDK publication is marked SDK-originated and cannot
  fabricate a Python block or claim model authorship; unavailable source coordinates remain absent.
- Persist each frozen incoming batch with the recipient iteration's input history: group/entry/thread
  references, attribution, the exact preview included in model input and its `truncated` marker.
  This is a bounded historical input snapshot, not a second log or a delivered/read status table.
  Retry/fallback reuses that snapshot and must not create a second context append.
- The shared history projection exposes outgoing entry references at their real source coordinates
  and incoming snapshots at the recipient iteration, including folded iterations. Do not inject
  later replies or fetch the full Council log into historical sessions. Full content remains an
  explicit Council read. `read_session()` and SDK `Session.transcript()` use this projection;
  `Session.read()` remains the session-record operation, not a transcript API.
- Reading history, a log page, a thread list, a thread or a single entry never acknowledges a ping,
  advances its
  delivery cursor, mutates a retained batch or schedules an iteration. Compaction may remove content
  from the live model context but must preserve these correlations in persisted history.
- Activity describes actual host operations such as publish, members, threads and read. Give its existing
  rows bounded summaries and group/entry/thread references; the enclosing block already supplies
  execution coordinates. Preserve existing redaction and event-replay rules. A presentation failure
  does not roll back a committed publication or create another entry on retry.
- An automatically injected ping belongs to iteration input history, not a fictitious receive tool
  call. It adds no Activity operation count. SDK publication creates no fictitious Python Activity
  row. No new Activity stream, delivery state machine or UI redesign is part of this work.
### Required test inventory

The following cases are specifications, NOT completed tests. Add executable tests in the existing
suites before implementing the corresponding production behavior. Use the real bundled SQLite and
actual query/operation paths; mocks may control time, scheduling and provider IO, not implement Council.

| ID | Case | Required observable result |
|---|---|---|
| C01 | 1,000 then 100,000 unrelated entries; repeated empty pending reads | Recipient-index lookup; no repeated scan proportional to the group log. Measure real query work/latency as well as its plan. |
| C02 | Sparse and dense pings; more than one batch; independent group cursors | Ordered, bounded continuation without misses or duplicate context appends; correct `has_more`; no cross-group skipping. |
| C03 | Pause publication after membership snapshot; recipient stops and reactivates; then commit | Old activation target stays in the log and never enters the new activation. |
| C04 | Race activation initialization with publication and first model invocation | No half-initialized visible participant and no accepted ping skipped by a late cursor initialization. |
| C05 | Authoritative running, queued and held work; UI-only and foreign mirrored sessions | Accurate member states; explicit unsupported/inactive errors; no waking or resuming work. |
| C06 | Publication succeeds, response is lost, then author/recipients finish; retry | Original entry and original recipient snapshot returned with no new write. |
| C07 | Same key concurrently; changed content/title/thread selector; same key from two authors | One row for identical retries, including a new thread with a derived title; explicit conflict for changed input; independent rows across authors. |
| C08 | One invalid explicit recipient; duplicate targets; empty/all broadcast; self-ping | Atomic rejection where required; deduplication; exact frozen recipients; no implicit partial delivery. |
| C09 | Failure between entry and ping insertion; concurrent commit/rollback; reopen store | No orphan/partial publication; forward pagination loses no committed entry; supported persistence survives reopen. |
| C10 | Cross-group single-entry and publish/read thread references; missing/non-root thread ids; paginated thread | Access/group/root validation; bounded ordered pages including the root exactly once; no cross-group inference, silent thread creation or unbounded thread result. |
| C11 | Exact byte limits, multibyte text, long entry preview and full fetch | Consistent validation; explicit truncation only in preview; full stored content remains retrievable. |
| C12 | Many full-size previews; exhausted context capacity; count/byte boundary lookahead | Total injected budget respected; deferred entries keep their cursor position and report overflow when space permits. |
| C13 | Pending lookup timeout/failure; selected batch followed by render failure, provider retry/fallback or repeated rendering | Bounded failure leaves cursor unchanged and warns; retained batches survive retries without a lost ping, duplicate append or renderer-side consumption. |
| C14 | First model invocation, ordinary next iteration and tool-free continuation | Actual provider-input capture contains the ping at the eligible boundary, with author and ids. |
| C15 | Peer content resembling instructions; unpinged entries and unpinged replies | Attribution and data trust preserved; stable system prefix unchanged; unpinged text never auto-injected. |
| C16 | Finish/cancel before delivery; simultaneous mutual pings; ordinary final answer | No extra turn/iteration, wait, completion gate, approval requirement or automatic reply ping. |
| C17 | Restart and two isolated LocalEngines using the same project directory | Fresh activation identities; no stale delivery; independent private stores and no false daemon membership. |
| C18 | Host, authenticated gateway and SDK publish/threads/read roundtrip; missing auth/spoofed identity | One contract and same semantics; no author override through host payload; inactive new publication rejected. |
| C19 | Ordinary session writes under concurrent Council publication and reads | Report latency percentiles, writer contention, busy failures and baseline impact; no unbounded retry layer. |
| C20 | Default group across shared/isolated workspaces and supported launch surfaces | Same owning project resolves to the same group; unresolved/projectless identity has an explicit tested policy. |
| C21 | Two new threads in one group; flat continuations via `publish(thread_id=...)`; filtered/unfiltered reads | Distinct root ids; continuations retain the selected root; same page envelope and exact `after`/`has_more` behavior; visibility remains group-wide and continuations never imply a subscription or ping. No reply tree or separate `thread()` content-fetch operation. |
| C22 | Host-supplied Council in Python; rebuilt session dictionary; tampering; publication without `print` | `session["id"]` remains unchanged; Council metadata exposes only `default_group_id`, not activation, cursors, inbox or duplicate identity. Trusted author/group binding survives dictionary edits; publication and source reference exist without stdout. |
| C23 | Several continuously queued turns, then inactivity/reactivation; old SDK handle publishes/retries | Public session id stays stable; activation stays internal and changes only at the activity boundary. Old handle cannot publish into the new activation; identical replay returns its original entry. Members expose ids/titles/states, not generations. |
| C24 | Outgoing source and incoming preview through `read_session()`/SDK transcript, before/after fold and provider retry | Stable source references and exact attributed input snapshots survive; no invented SDK block/model authorship or later-thread hydration. Repeated reads leave cursors/batches untouched and schedule nothing. |
| C25 | Publish/members/threads/read Activity, no stdout, presentation failure, replay and automatic incoming ping | Real operations have bounded/redacted summaries and stable Council references. Failure cannot undo/duplicate publication; incoming delivery and external SDK publication create no fake Python operation or extra Activity count. |
| C26 | Real Python A publish -> SQLite -> ping in B's captured model input -> both histories and Activity, plus SDK roundtrip | Correlations agree across actual boundaries, including a truncated preview and paginated thread fetch; one entry and one retained append on retry. No mocked Council implementation, implicit acknowledgement, turn submission or wakeup. |
| C27 | Root with explicit/omitted title; blank lines, multibyte fallback boundary and invalid title/content | Deterministic bounded first-nonempty-line fallback without a model call; explicit titles trimmed/validated, never silently truncated; full content unchanged; title persisted only on the root and immutable. |
| C28 | Empty/multiple groups; sparse roots among 1,000 then 100,000 continuations; concurrent roots/replies and paginated `threads()` | Root-index seek with measured bounded query work, not a scan/group of continuations; exact summary fields with no bodies; stable ascending root-id order; count/byte limits, cursor and `has_more`; no duplicate roots or reordering on reply. |
| C29 | Thread-selector-only contract; unsupported `parent_id`; title on a continuation, including an unchanged title | Host/SDK/wire reject unsupported fields and invalid combinations before writing. Entry schemas, storage and history/Activity correlations contain no message-parent field or compatibility path. Thread/root errors use C10. Only omission of `thread_id` creates a new root, after normal validation. |
| C30 | Host and SDK discover via `threads()`, read and publish using the returned `thread_id`, then retry | Same id reused without translating it to another selector; no new root or title change on continuation; no implicit ping. Explicit nondefault group stays scoped; original result survives retry; changed title/thread with the same key conflicts. Real boundary coverage includes thread-list Activity and read-only cursor behavior. |

Test locations to extend, not a second bespoke harness:
- Storage: `test/com/blockether/vis/internal/persistance/sqlite/core_test.clj` and its existing
  migration fixtures/helpers. Exercise the production schema and queries, not copied test DDL.
- Runtime: `test/com/blockether/vis/internal/gateway/state_test.clj` and
  `test/com/blockether/vis/internal/loop_test.clj`. Use controlled barriers/events rather than sleeps
  for C03/C04/C13 and a stub provider that captures the actual sent message vector. Cover retained
  input history/folding, internal activation lifetime and C22-C24/C26 on these real paths.
- Host/wire: `test/com/blockether/vis/internal/extension/core_test.clj`,
  `test/com/blockether/vis/internal/gateway/server_test.clj` and the existing Python-host boundary suite
  `test/com/blockether/vis/internal/python/extensions_test.clj`. Exercise C22/C25/C26 through real
  Python host calls and the existing Activity observation/presentation fixtures, not fabricated events.
- SDK: `packages/vis-agent/tests/test_contracts.py`, `packages/vis-agent/tests/test_client.py`,
  `packages/vis-agent/tests/test_local.py` and `packages/vis-agent/tests/test_engine.py`.
  Enable the real LocalEngine test path explicitly; a skipped integration test is not verification.
  Use the client/transcript fixtures for C23/C24/C26 and the existing Activity/contract fixtures for
  wire replay and correlation fields. Canonical session-context/history/Activity shapes are required.
  Extend the same storage, host/wire and SDK fixtures for C27-C30; cover titles, indexed root listing,
  thread-only selectors and the discover/read/publish workflow without a new test harness.
- Add a mirrored test namespace if the Council domain gains a new production namespace; do not
  centralize every layer's assertions in a new test-only simulation of Council.

## 1. Contract and executable tests FIRST

- Rationale: fix observable behavior and capture the cross-validation defects before implementation.
- Data: inventory C01-C30; canonical documents/schemas under
  `packages/vis-contract/resources/vis-contract/`; existing Lazytest, SDK and host boundary fixtures.
- Acceptance criteria: first run the affected existing suites and record their baseline. Freeze
  remaining field/error/route names, request normalization, limit values, execution-owner selection
  and projectless-session policy. Include `publish(thread_id=...)`, `threads()` summaries/titles and
  filtered `read`, the absence of message-parent fields, minimal public Council session metadata, hidden
  activation and provenance/history/Activity contracts. Add executable cases for ALL inventory rows,
  with boundary helpers and deterministic scheduling ready. Record test paths/names and RED results
  for the missing behavior.
  Tests must load and be discovered; an import, fixture or environment failure is not an acceptable
  RED result. Exercise stable public dispatch/route boundaries to assert a missing feature without
  requiring nonexistent production namespaces. No skipped tests, unconditional expected-failure
  markers or fake Council implementation may satisfy this gate.
- Unknowns: remaining public field/route names; default-project resolution and execution-owner predicate;
  available query-work instrumentation in the bundled driver and reference benchmark hardware.
  Resolve these while preparing tests, before declaring this phase complete.

No Council production implementation starts until phase 1 is complete. Before each later production
slice, rerun its relevant RED tests; after the change make them GREEN. Tests added after a working
implementation do not satisfy this sequence. The planning-only probes above are not a substitute.

## 2. Schema and log operations

- Rationale: make entry/recipient persistence atomic, replay-safe and efficiently addressable.
- Data: the existing SQLite/migration owners and canonical contract; prepared C01/C02/C06-C12/C20/C21,
  C27-C29 and the persistence assertions in C24/C26/C30.
- Acceptance criteria: those tests fail first for the intended missing behavior, then pass against
  the real migrated database. Implement shared publish/threads/read/single-entry functions and the two
  indexed tables, with a root-only title/index and no message-parent column. Use `publish(thread_id=...)`
  and `read(thread_id=...)`; `threads()` returns only paginated summaries, not content. Cover identical
  and conflicting concurrent retries, rollback, immutable recipient activations, source references,
  title/byte limits and group-scoped thread pagination. Record actual pending-query and thread-list
  work at 1,000 and 100,000 rows for empty/sparse/dense inboxes and sparse roots among continuations,
  not just `EXPLAIN QUERY PLAN` output.
- Unknowns: none in the first-version retention policy (no deletion); revisit stronger durability
  only with an explicit requirement and measurements, without claiming NORMAL is power-loss safe.

## 3. Authoritative presence and iteration delivery

- Rationale: prevent stale activation delivery and preserve selected batches across model retries.
- Data: registry lifecycle, session-bound execution context, the loop's actual provider-input path;
  prepared C02-C05/C12-C17, relevant C20 launch cases and runtime assertions in C22-C24/C26.
- Acceptance criteria: fresh activation and per-group cursor state become visible atomically;
  indexed pending batches enter attributed conversation context without changing the system prefix.
  Retry/fallback preserves the frozen batch and its persisted iteration-input snapshot; folding keeps
  the historical correlation. Cancellation cannot leak it to a new activation. Public session metadata
  stays minimal; runtime owns activation identity across continuously queued turns. Tests prove no
  wakeups, reply gates, blocking waits or extra turns. A pending fetch must not consume the ordinary
  session's entire SQLite busy timeout without an explicit bounded policy.
- Unknowns: exact loop-state field/owner for a retained batch; settle placement against the prepared
  failure-injection tests without changing the specified timeout or retry semantics.

## 4. Host, gateway, SDK, history and Activity roundtrip

- Rationale: one operation implementation must serve agents and authenticated session-bound clients.
- Data: `packages/vis-agent/src/blockether/vis/extension.py`, host shims under
  `resources/vis-shims/`, `packages/vis-contract/src/com/blockether/vis/contract/wire.clj` and SDK clients;
  prepared C06/C07/C11/C17/C18/C21-C30. Do not mirror the canonical host API in another file.
- Acceptance criteria: members/publish/threads/read/single-entry fetch work through host and SDK,
  including titled creation, thread-only continuation, discovery, filtered read and authenticated
  gateway publication. Bind author/internal activation
  rather than accepting an arbitrary author field; retain a call's identity/key across transport
  retries. Host callers need no client initialization; session-bound SDK handles cannot silently
  rebind to later activity. HTTP and LocalEngine share semantics within their own engine/store scope.
  Correlations survive the shared history projection, SDK transcript and Activity replay; only real
  host operations contribute Activity rows. Canonical schemas and real boundary tests pass; mocked
  HTTP alone is insufficient. Namespace docs and `resources/vis-docs/` explain groups vs threads,
  titled creation, thread discovery and `threads()` -> `read(thread_id=...)` -> `publish(thread_id=...)`,
  public session metadata, internal activation expiry, previews, history correlations, budgets, replay
  conflicts and the existing daemon-level trust model. Descriptions/examples must not teach a message-
  parent selector or suggest automatic pings.
- Unknowns: remaining handle/route and correlation-field spelling, frozen in phase 1. No new per-session
  ACL system, cross-engine presence service or thread create/rename/subscription API is implied.

## 5. Performance, failure verification and completion

- Rationale: test correctness under load and measure effect on normal Vis work before claiming speed.
- Data: prepared C01/C02/C09/C13/C17/C19/C24-C28/C30, the bundled SQLite/JVM, real query paths and existing
  session write fixtures. No paid model calls, live gateway restart, deployment or production-state mutation.
- Acceptance criteria: run the matrix against the integrated implementation. Report fixture sizes,
  entry/recipient distribution, hardware, engine/SQLite versions, concurrency and warm/cold conditions.
  Measure publish, history/thread-content/thread-list pages and pending lookups separately, with
  p50/p95/p99 and writer contention alongside a no-Council baseline. Initial reference profile:
  10 active sessions and up to
  100,000 entries, including empty/rare/dense recipient distributions and burst broadcasts. Provisional
  targets are p95 publish below 50 ms and bounded page read below 20 ms on the declared reference
  profile; agree/freeze the reference and budgets before the acceptance run. These are not measured
  results or flaky wall-clock assertions for every developer's unit suite. Index evidence alone cannot
  pass performance acceptance. Document unmet targets or blocked measurements rather than claiming fast.
  All affected Lazytest/Python suites, formatting and lint/reflection pass; reload changed production
  namespaces or use clean JVMs. Run relevant native/boundary coverage if interop or native entrypoints
  change. Inspect actual integration counts, including the explicitly enabled LocalEngine path.
- Unknowns: measured latency and contention; they stay unknown until this phase runs.

## Plan state

- [x] Cross-validation findings incorporated; tests-first order and C01-C30 acceptance cases specified.
- [x] Thread-only publication, titled discovery, filtered read and group/thread semantics implemented.
- [x] Minimal session metadata and history/Activity correlations retained, without message-parent fields.
- [ ] Phase 1: the strict all-inventory RED-before-production gate was not fully met.
  Contracts, limits and policies were frozen; several integration assertions were added later.
- [x] Phase 2: canonical storage, atomic publication, replay, pagination and additive schema repair.
- [x] Phase 3: activation lifecycle, bounded delivery, immutable input batches and recovery paths.
- [x] Phase 4: real host, authenticated HTTP/stdio SDK, history and Activity roundtrips.
- [ ] Phase 5: a fully green broad run remains blocked by baseline failures listed below.
  Scoped Council verification, reference measurements, native checks and lint pass.

Implementation commit `98f92d7bd` reached `origin/main` through `92928501b`.
The follow-up simplifications were verified in `.gitworktrees/council` on `feat/council`.
The persisted `council` toggle defaults off. Enabled sessions expose only the default group in
public Council metadata; activation identity remains host-owned. There is no wakeup, wait, reply
completion gate, implicit ping, message-parent selector or cross-engine presence service.

Review corrections:
- Keyword-only `council.publish(content=...)` and `council.get(entry_id=...)` use explicit call shapes.
  The real Python/model regression exercises both forms.
- Sparse thread reads seek the root and indexed continuations separately. The regression explains
  actual production queries with a full-page fixture, checks pagination and rejects group scans.
  Page recipients use one batched query rather than one query per entry.
- The closed publication schema includes the SDK activation field. Group-only requests use a closed
  shared schema. Unknown server exceptions propagate as server failures, not HTTP 400 responses.
- SDK text normalization and byte validation have one authority: the engine. Real HTTP/stdio tests
  cover trimmed titles and rejection of blank, oversized and multiline text. POST group addressing
  is carried only in the body.
- UTF-8 clipping uses the JDK encoder; page accounting adds encoded row sizes instead of repeatedly
  encoding the whole page. Unicode boundaries, JSON escaping and final-envelope byte limits are tested.
- A single-session runtime projection reads only that session; a regression counts the database reads.
  Duplicate enabled checks and unused descriptive contract fields were removed.
- The 100 ms lookup deadline remains. The group-change check is not dead: a session can move projects
  during an outstanding lookup. A regression proves that the old result is discarded without delivery.
- Provider retry and tool-free continuation regressions remain in the real Python/model boundary test.

Verification checkpoint:
- Final clean-JVM broad run: 1,633 cases, four failures. Focused Council domain, host, gateway and
  persistence suites: 276 passed. The real Python/model boundary test passed separately.
- Three additional baseline failures reproduce on unmodified `main` at `f5a3a27ed`: the gateway
  route contract omits the existing provider reset-credits route; the jail documentation exceeds
  the paragraph-length limit; the permission snapshot fixture omits the process toolchain grant.
  The fourth, the `languages` signature expectation, was already recorded before Council edits.
  These failures are outside the Council behavior and were not hidden or repaired here.
- SDK unit suite: 331 passed, eight real-engine cases deselected rather than counted as verified.
  Separately enabled Council/LocalEngine integration: five passed over HTTP/stdio on both JVM and
  the final native build. These use isolated stores and a synthetic provider, not paid calls.
- The wider SDK integration previously reproduced two missing input `view.close` receipts in
  `test_real_agent_tool_view_activity_and_cancellation[stdio,http]` with both the original test and
  engine source/resources from `d015ea65c`; those unrelated failures are not repaired here.
- The real Python/model test checks executed-block errors, not just invocation counts. It covers
  publication without stdout, a truncated incoming preview, transparent provider retry, a recoverable
  model-format error, tool-free continuation, full fetch, actual `read_session()`/fold calls,
  unchanged persisted input and removal of the folded preview from subsequent model input.
  Input-only iterations have fold scopes without fabricated tools or Activity rows.
- SDK/host publication provenance and Activity references agree. Incoming pings and external SDK calls
  do not add fake host-operation counts. Disabled Council has no model guidance, callable surface or
  public Council dictionary.
- Clojure formatting and lint/reflection and Python formatting/lint pass for the scoped files.
  Canonical Council document/schema and served-page links pass; the unrelated broad failures remain
  reported above. `git diff --check` passes.
- Before the later main merges, GraalVM CE 25.3.4.1 built the Council engine in 3m52s with 23 warnings.
  The resulting binary passed 11 native cases and five Council/LocalEngine SDK cases.
- After integrating main at `e66f83912`, the 276 Council/HTTP/store cases, the Python/model boundary
  case, five JVM Council/LocalEngine SDK cases and 260 TUI rendering cases passed again.
- Provider and search commits through `e20c1b4e6` then merged without conflicts. Combined Council,
  gateway, store, provider and editing suites passed 565 cases; configuration passed 149. The model
  boundary case and five real JVM SDK cases passed again. Merged gateway formatting and lint/reflection
  pass. The native results above precede these provider/search changes, not a rebuilt final image.

Simplification follow-up, based on `92928501b`:
- Reused the shared SHA-256 helper, removed a redundant string guard and simplified JSON-body parsing.
  Recipient queries alias authorship directly in SQL. SDK publication builds its activation field
  with the body; tests check the exact optional-field payload, including an empty recipient list.
- Removed the redundant root index. Production-query EXPLAIN assertions cover thread reads, root
  listings and empty recipients; schema repair now expects the three remaining Council indexes.
- Council alone caps input bytes. The caller still reserves 256 tokens of context headroom.
  A new schema-checked regression covers small and oversized caller budgets, including attribution.
- Consolidated duplicate query probes into a 50-continuation fixture and removed their timing samples
  and the 600 ms wall-clock assertion. The single-outstanding-lookup test and the 100,000-row,
  four-worker memory/WAL contention reference remain executable in the suite.
- Kept independent input/publication history columns, typed SDK provenance and portable schemas.
  The local UTF-8 counter avoids an unrelated dependency on Activity solely to replace one line.
- Verification: 278 focused Council/host/gateway/store cases and the real Python/model case pass.
  The broader 764-case run has only the previously reproduced permission-snapshot baseline failure.
  SDK units: 331 passed; real Council/LocalEngine HTTP/stdio: five passed on JVM and five on native.
  GraalVM CE 25.3.4.1 built the simplification source in 4m1s with 23 warnings; 13 native cases pass.
  Scoped Clojure lint/reflection, Python lint and both formatters pass. No gateway was restarted.
- Main advanced to `e71f28c9e` during verification; its recap-only folding fix merged without conflicts.
  The merged Council/context/loop run has 732 cases and only the same permission-snapshot failure.
  The Python/model regression and five real JVM SDK cases pass again; merged Clojure formatting and
  lint/reflection pass. Native results above precede this pure-Clojure main merge.
Reference performance before this follow-up (milliseconds; informational, not timing assertions):
macOS/aarch64, 14 logical processors, JVM 25.0.3, bundled SQLite 3.53.2. The fixture uses 10 active
sessions, one root plus 100,000 short continuations, explicit sparse/dense recipients and broadcasts.
Each measurement has 10 warmup calls and 100 samples; concurrent publication combines 200 samples
from two writers. Four mixed-load workers are two broadcast writers, a log reader and an ordinary
session-title writer. Memory and persistent WAL stores were measured separately. This session ran no
build or other verification shell concurrently with the reference; no cold-cache or power-loss claim
is made.

| Operation | Memory p50/p95/p99 | Persistent WAL p50/p95/p99 |
|---|---|---|
| Publish | 0.593 / 1.148 / 1.634 | 1.000 / 2.116 / 3.605 |
| Log page | 0.940 / 4.321 / 15.095 | 0.589 / 1.049 / 3.133 |
| Thread-content page | 0.730 / 2.381 / 4.389 | 0.690 / 1.634 / 3.671 |
| Thread-list page | 0.361 / 1.321 / 3.537 | 0.322 / 0.926 / 1.098 |
| Empty pending | 0.086 / 0.352 / 0.569 | 0.071 / 0.108 / 0.255 |
| Sparse pending | 0.082 / 0.107 / 0.450 | 0.075 / 0.087 / 0.231 |
| Dense pending | 0.115 / 0.350 / 0.850 | 0.122 / 0.279 / 0.473 |
| Session write, baseline | 0.084 / 0.171 / 0.562 | 0.097 / 0.443 / 0.718 |
| Session write, mixed load | 0.651 / 1.237 / 1.634 | 0.276 / 3.112 / 4.570 |
| Concurrent publish | 1.869 / 2.717 / 5.103 | 2.064 / 4.891 / 8.631 |
| Concurrent log read | 2.115 / 3.617 / 5.729 | 0.771 / 2.201 / 3.259 |

Both stores recorded zero busy failures. Provisional p95 publish/page targets are met for this warm
short-entry profile, not asserted for every payload/distribution. Ordinary writes slow under contention;
the table reports that impact. The sparse-thread fixture separately measured p50/p95/p99
0.228 / 0.371 / 0.635 ms in memory, with 10 warmups and 100 samples, for a root and late reply separated
by 100,000 unrelated continuations.

The user explicitly authorized commit and push to `main` for this feature. No release, deployment
or live gateway restart is included. Existing main-worktree changes are preserved separately.

## Native production release and deployment

Phrase: publish a complete, tested native production release and verify its installed processes.

Context: `deps.edn` pins the verified runtime 0.5.6 release commit. The previous release
published bootstrap assets before native artifacts existed. `bin/install-vis-agent`,
`bin/vis-agent`, native integration tests and companion workflows define the delivery
boundary. Production now selects complete stable native bundles; explicit JVM development
remains available. Published version tags remain immutable, including the bootstrap tag.

1. Verify runtime and reproduce delivery gaps.
   Rationale: an archive or a successful compilation does not prove native execution.
   Data: runtime pin/release assets, CI runs, affected release tests, native test fixtures.
   Acceptance criteria: current runtime checked; failing delivery contracts recorded;
   native worker archive contents and execution verified on supported platforms.
   Unknowns: outstanding platform build/test failures and available signing credentials.
2. Repair production installation and complete-release publication.
   Rationale: installers must not select incomplete releases or silently require a JVM.
   Data: installer, release workflows, bundle and launcher regression tests.
   Acceptance criteria: production is the default; explicit development/beta opt-ins;
   complete artifact gate before stable promotion; affected tests, formatting and lint pass.
   Unknowns: existing workflow boundaries and mobile distribution requirements.
3. Build and test all release artifacts.
   Rationale: engine, TUI, gateway, worker and companion packages must match the release.
   Data: clean JVM/runtime tests, native binary tests, SDK boundary checks, platform CI.
   Acceptance criteria: required checks green and all supported release artifacts present;
   failures fixed rather than bypassed; immutable version/tag/main agree at preparation.
   Unknowns: build resource limits and signing/toolchain availability.
4. Install and verify on the administered server.
   Rationale: native build results alone do not establish end-to-end operation.
   Data: effective private deployment configuration, process identity, canonical gateway
   client requests, native worker and TUI integration checks.
   Acceptance criteria: production installation runs without a JVM; native gateway, TUI
   and worker exercised end to end; healthy requested services left running.
   Unknowns: current server state and safe deployment/rollback boundary.
5. Record evidence and deliver.
   Rationale: release completeness and deployment health must be independently inspectable.
   Data: scoped commits, immutable release, CI outcomes and runtime observations.
   Acceptance criteria: scoped changes committed/pushed, complete published assets,
   explicit verification results and any concrete unresolved blockers reported.
   Unknowns: none beyond the preceding phases.

Plan state: phases 1–2 are complete; phases 3–5 are in progress for v0.1.53. Runtime
v0.5.6 remains current. Release v0.1.52 passed its complete publication gate, but an
extended native regression reproduced a missing directory-listing downcall registration
in the published binary. The FFF 0.12.9 fix and native search coverage are already on main.
Build a new immutable release with that fix and rerun the complete installed native path.
Preserve the healthy production hotfix until the replacement passes every required check.

The expanded native search regression fails against published v0.1.52 and passes against
the current native hotfix. The release also includes the verified package-readiness and
reload corrections: 230 affected JVM cases and five native cases pass, with formatting,
lint and reflection checks clean. Local release/version checks pass all 54 cases.

- Runtime v0.5.6 is published at 51f02270ffc78b5eb49bcab914b27564b1960f82. Run
  34297827905 passed all four platform builds/tests and published the runtime archives and
  JVM jar. The readiness handshake passes 2000 immediate process-group terminations on
  each tested operating system; the full runtime suite passes 171 cases on macOS and Linux.
- Cancellation waits for actual sandbox and trusted-worker replies, not only cancelled
  host futures. Wedged native waits retire their workers; normal cancellation preserves
  state and permits reuse. Affected suites passed 580 cases, with formatting, scoped lint
  and reflection checks passing.
- The installer defaults to complete stable native bundles. Release preparation checks
  tag/version/main alignment before long-running source verification. Every artifact job
  still requires source verification; stable promotion requires all 15 nonempty assets.
  iOS signing pins the imported identity for archive and export and fails closed.
- [Release v0.1.52](https://github.com/Blockether/vis/releases/tag/v0.1.52) passed all 31 jobs
  in [release run 34331911466](https://github.com/Blockether/vis/actions/runs/34331911466).
  Its 15 assets include bootstrap files, native engine/worker and TUI bundles for Linux
  x64, Linux ARM64 and macOS ARM64, signed mobile packages and five desktop packages.
  TestFlight and Android tester distribution passed; Android production was not published.
  The pinned CE toolchain does not support a native macOS x64 engine.
- The immutable v0.1.52 checkout passed 5083 Linux JVM cases. Its installed SDK passed
  25 HTTP/stdio cases and the original native suite passed 15 cases. A real-terminal TUI
  turn exercised native gateway and worker processes and returned to idle. These results
  do not cover the subsequently added native directory-listing regression, which fails
  against that published binary. Release v0.1.53 must pass the expanded suite.
- Fresh default-stable installation, native SDK and TUI end-to-end verification, complete
  release assets and rollback-safe production replacement remain required for v0.1.53.
  Existing immutable tags and concurrent work are preserved.

---

# Extension contracts, packaged skills and authoring documentation

One tool declaration supplies discovery and documentation; one package carries code and skills.

## Context
Issue #176 requests machine-readable tool descriptions without another callable registry.
`extension.py` owns SDK declarations; `python/extensions.clj` bridges them into the engine.
`extension_package.py` validates inert package metadata; harness discovery owns skills.
`resources/vis-docs/` serves both doc() and the site. Keep unrelated plans and edits intact.
Do not generate a CLI, invent a workflow language, evaluate annotations, or change sys.path.

## 1. Tool contract and tested example
- Rationale: derive structure from Python rather than duplicating signatures in ToolSpec.
- Data: Symbol and method declarations, annotations, docstrings, SDK and loader tests.
- Acceptance criteria: portable contracts cover every parameter kind, absent/None defaults,
  typed results and field descriptions; sandbox callables expose the same data used by doc().
  Inspection never calls tools, authentication, factories or annotation expressions.
- Unknowns: resolved with existing symbol entries and callable attributes; no runtime shim changes.

## 2. Package-owned skills
- Rationale: install, reload and remove tools and their procedures as one reviewed package.
- Data: tool.vis manifest, frozen extension sources, harness skill discovery and reload tests.
- Acceptance criteria: declared in-package skills and resources, explicit provenance and
  collision policy, last-good reload, no automatic execution or second skill registry.
- Unknowns: resolved by registered extension skills and the existing discovery cache marker.

## 3. Documentation consolidation and verification
- Rationale: one canonical page per concern and one executable authoring example.
- Data: quickstart, design guide, packaging, API reference, troubleshooting, docs catalog/site.
- Acceptance criteria: tested example, affected SDK/JVM suites, formatting, lint/reflection,
  canonical contract validation, content/link/diff checks; no remote publication in this task.
- Unknowns: none in scope; unrelated full-suite failures are recorded below.

## Plan state
Completed locally. No commit, push, release or service restart was performed.
- Portable Symbol contracts drive tool documentation and sandbox callable inspection. Tests
  cover parameter kinds, private defaults, nested result fields, recursive references and
  inert Python 3.14 deferred annotations. Existing declaration and invocation paths remain.
- Declared package skills retain resources and package/version provenance, qualified names,
  local override precedence and last-good reload behavior. Removal clears discovery. Tests
  exercise the real loader, doc(), slash templates and sandbox without executing procedures.
- The quickstart and four focused guides share one greeter package. Tests execute its domain
  tests and real registered tool, and enforce exact documentation snippets. Navigation,
  cross-page anchors and the SDK README point to the canonical pages.
- Verification: 132 affected SDK tests and 807 JVM tests pass. Scoped Python/Clojure
  formatting, lint including reflection, canonical JSON validation and docs/link checks pass.
  The scoped diff check is clean. Full SDK: 381 passed, 9 skipped, 6 existing failures in
  test_activity.py shared_operation_groups; its implementation/tests were not changed here.
  A concurrent TUI test edit also caused an unrelated whole-worktree whitespace check failure.
  All unrelated shared-worktree changes are preserved.
- Discovery follow-up: apropos previews page prose instead of repeating Markdown titles.
  Real sandbox tests cover tool/doc/skill row types, bounded descriptions, doc(row), complete
  parameter/result documentation and skill provenance/resources. The same session observes
  successful reloads, retains last-good discovery on failure and removes uninstalled skills.
  The quickstart demonstrates apropos → doc(row) → contract → call. All 254 affected JVM
  tests and 4 executable-example SDK tests pass; formatting, lint/reflection and diff checks pass.
