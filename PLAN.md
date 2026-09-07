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
