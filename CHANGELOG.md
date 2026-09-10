# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

## [v0.1.61] - 2026-09-10

### Fixed
- Restore Python guest modules before worker startup when their staged directory was removed or the runtime home changed, fixing the native regression that blocked v0.1.60 (#185).
- Include the `python.tls_strict` configuration, shared sandbox/extension policy and runtime 0.5.9 prepared for v0.1.60.

## [v0.1.60] - 2026-09-10

### Added
- Configure `python.tls_strict` in user or project YAML for both sandbox execution and trusted Python extensions (#185). It defaults to `true`; `false` clears only strict X.509 checks while preserving certificate trust and hostname verification. Includes runtime 0.5.9, documentation and JVM/native worker regression coverage.

## [v0.1.59] - 2026-09-10

### Fixed
- Pin Svar 0.7.166: estimate Responses context from the prepared request, excluding discarded reasoning and including tool declarations and output-format schemas (#186). Provider usage remains authoritative.
- Pin Python runtime 0.5.8 to keep shared asynchronous clients on the same event loop.
- Normalize spacing between Companion trace blocks.
- Reset the execution budget after extension calls return (#187).
- Avoid Companion input reflow during draft corrections.

## [v0.1.58] - 2026-09-10

### Fixed
- Copy TUI text through the controlling terminal or Windows `clip.exe`, including Unicode; keep native bubble-copy and drag-selection regressions separate from double-click line selection.
- Verify native editable packages in their upstream `uv` project environments and run the development guide's tests through that environment.
- Persist TUI themes in the runtime user's home, apply the configured package index to `uv`, and restore the per-launch JVM override.

## [v0.1.57] - 2026-09-10

### Fixed
- Align the extension installation regression with upstream `uv` configuration and project environments, resolving the source verification failure that blocked v0.1.56.
- Include the WSL clipboard and native runtime fixes prepared for v0.1.56.

## [v0.1.56] - 2026-09-10

### Added
- Configure the coding agent name through gateway settings.
- Track session goals and iteration budgets with shared client controls.
- Select stable, beta and development update tracks from the CLI.

### Fixed
- Send TUI clipboard sequences to the controlling terminal instead of the log, and copy Unicode through Windows `clip.exe` on WSL. Native PTY tests cover bubble copying and drag selections.
- Bundle `uv` with Python runtime 0.5.7, restore native Python workers and retain the Ubuntu 22.04 compatibility baseline.
- Restore native TUI resizing, syntax highlighting and attachment picking; keep model shortcuts responsive during gateway requests.
- Stabilize repository ordering in Companion and collapse tool error diagnostics by default.
- Preserve complete test failure diagnostics, refresh provider limits and notify Council requesters when replies omit explicit pings.

## [v0.1.55] - 2026-09-09

### Fixed
- Reactivate idle Council sessions after terminal transitions and preserve publications addressed to inactive peers.
- Retry silent gateway requests, persist timeout errors and stop turns after sandbox retirement.
- Align activity spacing in the TUI and companion and pause hidden draft subscriptions.
- Keep extension type documentation and early GitHub workflow watches reliable.
- Use official Ubuntu package sources for native sandbox test dependencies.

## [v0.1.54] - 2026-09-09

### Fixed
- Supersede older branch CI runs without cancelling tagged release verification.
- Keep mobile and desktop npm caches job-local and prune expired or oversized GitHub npm caches weekly.
- Run the documented editable-package native regression from its current guide.
- Reopen recorded live views from the TUI transcript.

### Changed
- Serve public documentation and the extension catalog from one site.

## [v0.1.53] - 2026-09-09

### Fixed
- Register native directory-listing calls and verify native file search in the release regression suite.
- Isolate Council execution context and preserve completed answers when persistence fails.
- Keep prepared Python environments valid after unrelated package updates and report stale tools after failed reloads.

## [v0.1.52] - 2026-09-09

### Fixed
- Validate release tag, version and main alignment before long-running CI while preserving every artifact and publication gate.
- Isolate the fork-baseline regression clock from background timers.

## [v0.1.51] - 2026-09-09

### Fixed
- Synchronize drafts before fast-forward approval and include the current main fixes in the native production release.

## [v0.1.50] - 2026-09-09

### Fixed
- Report compact, actionable tool errors and validate them independently of checkout path length.
- Correct draft handling, attachment metadata and companion activity spacing.
- Verify that cancellation leaves no empty answer bubble in the companion.
- Keep gateway project ordering stable during live updates.

## [v0.1.49] - 2026-09-09

### Fixed
- Pin the imported iOS distribution certificate for both archive signing and package export.

## [v0.1.48] - 2026-09-09

### Fixed
- Isolate the latest jail policy test from concurrent configuration readers.

## [v0.1.47] - 2026-09-09

### Fixed
- Resolve unpublished GitHub releases by ID before uploading or validating product assets.

## [v0.1.46] - 2026-09-09

### Fixed
- Reclaim trusted Python extensions that stay blocked after cancellation, while preserving healthy interpreter state and subsequent turns.
- Use Python runtime 0.5.6 to prevent lost termination signals during process and PTY startup.
- Keep cold JVM dependency preparation off the SDK stdio protocol.
- Preserve edits made at the draft fork timestamp and isolate cached test permission policies.
- Allow the complete native release workflow to inspect its macOS runner pickup.
- Resolve Council groups from persisted repositories for sessions without an explicit UI project.

## [v0.1.45] - 2026-09-08

### Changed
- Install complete stable native engine and TUI bundles by default; JVM development is explicit.
- Gate production publication on full CI, native integration tests and all 15 required desktop, mobile, engine, TUI and installer assets.
- Use Python runtime 0.5.5 with verified native platform archives and isolated worker execution.
- Preserve published release tags and update the rolling installer only after complete stable publication.

### Fixed
- Provision native Python before network-guarded tests on clean runners.
- Verify worker cancellation, native gateway startup and SDK HTTP/stdio integration.
- Correct companion code and jump-to-latest contrast and keyboard access.
- Exclude hidden worktrees from test discovery and preserve vetoed draft discards.
## [v0.1.44] - 2026-09-08

### Changed
- chore(release): v0.1.44
- fix(ui): restore trace insets and nest results under code
- fix(python): isolate trusted extensions from model workers
- perf(gateway): defer speech engine loading
- fix(grep): confine filename matches to requested paths
- perf(grep): default context to 3 lines and add read budget guidance
- perf(gateway): defer Clojure test runner loading
- fix(python): scope host callers to their worker connection
- fix(ui): order execution bands and remove left rails
- feat(speech): log correlated synthesis and model loading
- perf(gateway): isolate SQLite temp and defer tokenizers
- fix(ci): share the isolated home across Clojure JVMs
- feat(python): compact tool results and lean session reads
- fix(python): fail pytest runs that exit without test reports
- fix(tui): show results before code and activity
- fix(companion): refresh response controls after reconnect
- fix(python): use runtime 0.5.2 for jailed native workers
- fix(python): close reload-stale workers between turns
- perf(gateway): defer Clojure formatter loading
- fix(tui): align execution disclosures and fill band padding
- fix(companion): correct OAuth flow types
- chore: checkpoint UI and live-view changes
- fix(companion): correct send and keyboard activation timing
- fix(companion): open MCP sign-in tab inside the tap
- chore(deps): upgrade svar to 0.7.162
- fix(companion): keep refreshing MCP rows until the reconnect lands
- feat(companion): open MCP sign-in directly from the row
- fix(python): prepare uv extensions only through explicit sync
- fix(gateway): require newline before accepting journal terminal
- fix(ui): align execution rails and collapse activity by default
- chore(release): record companion build 5487
- test(companion): allow the MCP rows' own swipe verbs in settings
- fix(shell): kill TERM-resistant children after the launcher exits
- docs(ui): highlight Python and show interactive terminal captures
- feat(python): support indexed dependencies and locked uv projects
- perf(gateway): settle a finished turn from the journal's tail in hydrate!
- perf(mcp): back off connects to a server that awaits sign-in
- perf(config): re-parse a dotenv file only when its stamp moves
- perf(gateway): hold keychain answers for a minute instead of forking per read
- perf(db): leave Hikari's leak detector off unless a property arms it
- fix(docs): align prose and improve installation layout
- fix(testing): skip generated copies and initialize shell tests
- chore(tui): upgrade Lanterna to 3.1.5-vis.50
- docs(site): simplify guides and improve mobile tables
- feat(activity): unify client groups and align execution text
- feat(companion): match MCP servers panel to provider rows
- perf(shell): sample a waited shell's cost once per wait
- refactor(environment): remove unused inspection tools
- fix(companion): clear completed project removal state
- chore(release): record companion build 5468
- feat(companion): join Activity with execution bands
- docs: restructure docs site and PyPI README for newcomers
- fix(desktop): keep only macOS on self-hosted runners
- fix(ios): register OAuth callbacks in existing projects
- chore(release): record companion build 5463
- fix(companion): align thinking and code bands
- fix(oauth): use paired transport without VPN prompts
- fix(context): compact pending input and recover resumed overflows
- fix(oauth): allow confirmed VPN gateway connections
- fix(presentation): redact secrets at public view boundaries
- chore(release): record companion build 5457
- fix(activity): present python object fields, not transport envelopes
- fix(activity): hide Python transport envelopes
- fix(desktop): install Linux AppImage desktop helpers
- docs(audit): record runtime 0.5.1 pin
- fix(python): recover dead workers with runtime 0.5.1
- fix(desktop): collect normalized Linux package names
- feat(auth): receive OAuth callbacks on native clients
- fix(tui): decode live-view patch operations from gateway
- fix(tui): open attachment links from assistant replies
- fix(desktop): target Linux and universal macOS
- release: update release notes for v0.1.43

### Package changes

#### com.blockether/vis
- chore(release): v0.1.44 (d015ea65c)
- fix(ui): restore trace insets and nest results under code (331a00415)
- fix(python): isolate trusted extensions from model workers (fba0254d8)
- perf(gateway): defer speech engine loading (a44e35506)
- fix(grep): confine filename matches to requested paths (bfb0551d7)
- perf(grep): default context to 3 lines and add read budget guidance (b33b2c5c1)
- perf(gateway): defer Clojure test runner loading (4e67363bf)
- fix(python): scope host callers to their worker connection (1e3dc5837)
- fix(ui): order execution bands and remove left rails (a3c1f1e05)
- feat(speech): log correlated synthesis and model loading (fde0b44e3)
- perf(gateway): isolate SQLite temp and defer tokenizers (d2382c5bc)
- fix(ci): share the isolated home across Clojure JVMs (191dbca98)
- feat(python): compact tool results and lean session reads (76bf5f0de)
- fix(python): fail pytest runs that exit without test reports (7fb046dc8)
- fix(tui): show results before code and activity (9f1a68a4e)
- fix(companion): refresh response controls after reconnect (a5a37bb0a)
- fix(python): use runtime 0.5.2 for jailed native workers (dea035d93)
- fix(python): close reload-stale workers between turns (4c2226140)
- perf(gateway): defer Clojure formatter loading (e802ef20b)
- fix(tui): align execution disclosures and fill band padding (99e12bd4f)
- fix(companion): correct OAuth flow types (d462f126e)
- chore: checkpoint UI and live-view changes (817fcb8da)
- fix(companion): correct send and keyboard activation timing (a5175bab3)
- fix(companion): open MCP sign-in tab inside the tap (fb59002da)
- chore(deps): upgrade svar to 0.7.162 (de13598de)
- fix(companion): keep refreshing MCP rows until the reconnect lands (17d5e3223)
- feat(companion): open MCP sign-in directly from the row (55b6372d9)
- fix(python): prepare uv extensions only through explicit sync (0613e01d1)
- fix(gateway): require newline before accepting journal terminal (1554f1a51)
- fix(ui): align execution rails and collapse activity by default (132d7592c)
- chore(release): record companion build 5487 (5609bda18)
- test(companion): allow the MCP rows' own swipe verbs in settings (599dfd9ac)
- fix(shell): kill TERM-resistant children after the launcher exits (23df1586f)
- docs(ui): highlight Python and show interactive terminal captures (8ae4ffaa8)
- feat(python): support indexed dependencies and locked uv projects (d940a681d)
- perf(gateway): settle a finished turn from the journal's tail in hydrate! (ea021e3a2)
- perf(mcp): back off connects to a server that awaits sign-in (252872cb4)
- perf(config): re-parse a dotenv file only when its stamp moves (2e3293799)
- perf(gateway): hold keychain answers for a minute instead of forking per read (2c433a078)
- perf(db): leave Hikari's leak detector off unless a property arms it (b0cef5769)
- fix(docs): align prose and improve installation layout (9ba9d2c0f)
- fix(testing): skip generated copies and initialize shell tests (5296e2832)
- chore(tui): upgrade Lanterna to 3.1.5-vis.50 (32757fc82)
- docs(site): simplify guides and improve mobile tables (3ac0f91ee)
- feat(activity): unify client groups and align execution text (775aebfca)
- feat(companion): match MCP servers panel to provider rows (07fbf574d)
- perf(shell): sample a waited shell's cost once per wait (8c13eba12)
- refactor(environment): remove unused inspection tools (a80c369e1)
- fix(companion): clear completed project removal state (926946175)
- chore(release): record companion build 5468 (b1326f195)
- feat(companion): join Activity with execution bands (063f89981)
- docs: restructure docs site and PyPI README for newcomers (dc8854054)
- fix(desktop): keep only macOS on self-hosted runners (9c2f39003)
- fix(ios): register OAuth callbacks in existing projects (c9f59212c)
- chore(release): record companion build 5463 (c67d5cbc7)
- fix(companion): align thinking and code bands (548183a79)
- fix(oauth): use paired transport without VPN prompts (97bd801a3)
- fix(context): compact pending input and recover resumed overflows (d855bac2e)
- fix(oauth): allow confirmed VPN gateway connections (de7f8a40b)
- fix(presentation): redact secrets at public view boundaries (da700762e)
- chore(release): record companion build 5457 (922c25278)
- fix(activity): present python object fields, not transport envelopes (b716429ab)
- fix(activity): hide Python transport envelopes (51c40f0fc)
- fix(desktop): install Linux AppImage desktop helpers (ca0f3c91c)
- docs(audit): record runtime 0.5.1 pin (1d4e8043d)
- fix(python): recover dead workers with runtime 0.5.1 (aef9b60f7)
- fix(desktop): collect normalized Linux package names (72c5c20ab)
- feat(auth): receive OAuth callbacks on native clients (5659d0ff6)
- fix(tui): decode live-view patch operations from gateway (a4e2aa774)
- fix(tui): open attachment links from assistant replies (fb233b8fd)
- fix(desktop): target Linux and universal macOS (169b38392)
- release: update release notes for v0.1.43 (51cf309d8)



### Fixed

- Preserve one execution and result per native tool call, including repeated programs;
  use Svar 0.7.164's single-source Responses stream handling (issue #173).
- Calibrate context pressure from measured provider usage and count only the new
  message tail. Report unavailable tool results as errors instead of empty successes.
- Keep successful results nested under code while errors and Activity remain visible
  in Companion and TUI. Place transcript forks on the assistant answer.

### Changed

- Cover cold gateway startup from the packaged AOT artifact without attaching to a
  running gateway.

## [v0.1.43] - 2026-09-07

### Changed
- chore(release): v0.1.43
- fix(ci): repair release validation fixtures
- feat(sdk): add reproducible verification checks
- refactor(python): merge path aliases into filesystem roots
- feat(python): register project path globals and align prompt
- feat: unify activity, SDK and authentication flows
- fix(companion): align session controls and reserve hover actions
- fix(tui): prevent idle typing and scrolling stalls
- fix(queue): resume TUI submissions after provider failures
- fix(code-band): turn CODE red on failure instead of naming the error
- fix(activity): Collapse every step by default and provide a chevron to expand it
- fix(companion): align verbosity with TUI model capabilities
- fix(tui): keep iteration prose on settled and restored turns
- fix(mcp): correct connection status and caller context
- fix(tui): set a step's words one space after its mark
- fix(gateway): preserve model verbosity capabilities
- feat(tui): enable typing stall diagnostics
- fix(tui): align iteration prose with the answer, outside the timeline
- fix(tui): close a failed call's error rows with a blank row
- fix(tui): omit absent chooser group headers
- feat(activity): fold CODE and RESULT like THINKING in both clients
- fix(tui): remove redundant empty MCP message
- fix(tui): enable native access in the JVM launcher
- chore(release): record companion build 5421
- fix(tui): explain gateway connection failures
- fix(tui): repaint settings during theme preview
- fix(companion): Expand Application by clicking anywhere in its header
- fix(tui): show theme choices in a transient grid
- feat(context): estimate linked repository guidance in metrics
- fix(gateway): retract liveness marker on turn terminal synchronously
- feat(activity): add rich symbol content and refine execution UI
- docs(agents): authorize commit and push for simple bug fixes
- fix(tui): infer bearer token for the default local gateway
- chore(deps): bump svar to 0.7.160
- fix(companion): pad queued turn rows
- fix(tui): explain MCP sign-in before saving
- fix(mcp): allow OAuth setup and preserve validation errors
- fix(companion): shrink queued remove mark
- fix(companion): compact queued remove control
- fix(companion): reduce queued row padding
- fix(companion): compact queued turn tray
- refactor(python): remove interpreter configuration override
- fix(context): publish fold counts without printed receipts
- fix(gateway): publish cancelled-turn terminal events from interrupted workers
- fix(context): refresh fold telemetry and record executed folds
- refactor(core): finish engine and TUI cleanup
- feat(sdk): unify Python clients and interaction contracts
- refactor(companion): drop duplicate limits from session health
- chore(release): record companion build 5397
- fix(companion): keep outline icons unfilled
- chore(release): record companion build 5395
- Merge remote-tracking branch 'origin/main'
- ci(macos): route trusted builds to self-hosted runner
- fix(clojure): honor project runners and isolate shadow tests
- feat(metrics): show persisted session health in app metrics
- fix(gh): archive log snapshots with window_lines
- chore(deps): bump svar to 0.7.159 for cancel-safe Responses WebSockets
- fix(companion): Reopen only the transcript displayed when the app terminated
- test(core): upgrade Lazytest and synchronize Python regressions
- test(tui): make terminal reply draining deterministic
- fix(gateway): run local speech without an AI session
- docs(docs): remove completed implementation plan
- fix(gateway): complete native Jetty set registrations
- fix(gateway): register Jetty path sets for native startup
- fix(gateway): register the native startup collection constructor
- ci(release): expose native gateway startup failures
- fix(cli): prepare the package directory before confining workers
- chore(release): v0.1.42
- refactor(cli): consolidate Python workers in the runtime
- fix(companion): preserve touch-opened dialogs
- ci(build): move macOS jobs to GitHub-hosted runners
- fix(clojure): format file-leading and trailing whitespace
- feat(housekeeping): sweep stale Python runtime versions
- refactor(core): group internal namespaces by domain
- feat(clojure): keep exactly one blank line between top-level forms
- build(sandbox): consume Python runtime v0.4.1
- feat(cli): support recursive extension namespaces
- fix(companion): Match the diagnostics header height
- fix(companion): compact the diagnostics panel
- docs(cli): close resolved issues after release
- fix(cli): load introspection in Python workers
- fix(companion): Shrink queued message remove buttons
- fix(cli): bind Python extension file metadata
- fix(companion): fill starred session marks
- fix(tui): route vis-agent tui to terminal client
- docs(docs): point quick start at vis-agent TUI
- chore(release): record companion build 5358
- fix(cli): use an absolute JVM classpath so --jvm runs from any directory
- feat(companion): make drawing rail collapsible
- refactor(gateway): fold the provider limits cache into the host
- feat(jail): isolate Python execution in native worker
- fix(companion): refine mobile drawing rail
- fix(gateway): clear Anthropic status after OAuth
- fix(companion): move drawing tools into side rail
- fix(gateway): fall back after provider quota exhaustion
- fix(companion): use icons and frame zoom controls
- fix(companion): move drawing check into pencil slot
- feat(companion): use JetBrains Mono throughout
- chore(release): record companion build 5347
- fix(companion): let the sessions list scroll under the home indicator
- feat(companion): Present phone pairing methods as alternatives, not sequential steps
- feat(companion): pair through one field on a three-step page
- fix(companion): Pin parked sessions within their project and retain the list header
- fix(tui): support WSL2 PipeWire recording
- fix(gateway): enforce snake_case diagnostic codes
- fix(jail): allow unrestricted shell working directories
- feat(companion): Provide Pake desktop installers and hide Scan QR on desktop
- fix(cli): isolate embedded pytest runtime
- fix(companion): Remove the row's Fork action; retain forking from a turn
- feat(companion): Support forking from a session row or a specific turn
- fix(companion): Reduce the fork menu to two choices
- fix(companion): Limit the fit sheet height to the space below the notch
- fix(cli): serialize embedded pytest runs
- fix(companion): move the list toggle off the app bar, read the Mac host natively
- feat(companion): add the desktop conversation sidebar
- fix(companion): align and collapse the desktop sidebar
- feat(companion): Split the desktop layout into a session sidebar and transcript
- fix(companion): Use circular icon buttons for desktop row actions, outside the row columns
- fix(ci): prepare git dependencies on the pinned JDK
- feat(companion): Use the prose font for session titles
- build(jail): pin the released confinement runtime
- refactor(jail): state the policy in Vis, let the runtime enforce it
- refactor(sandbox): remove unused Python :fs/access checks
- refactor(providers): fold the limits namespaces into their providers
- refactor(providers): fold the provider packs into core and retire extensions/
- refactor(languages): fold the Clojure and Python packs into core
- refactor(persistance): fold the SQLite backend into core
- refactor(cli): remove obsolete search extension
- feat(cli): add GPT-6 Astra Codex model
- test(ci): align retired feature checks
- refactor(companion): remove obsolete terminal artifact bridge
- test(core): trim redundant coverage and cap suite heap
- test(cli): remove redundant schema validation cases
- test(cli): remove duplicated Python runtime coverage
- docs(audit): refresh dependency inventory
- refactor(contract): remove aggregate and trim source commentary
- refactor(contract): validate config only with JSON Schema
- fix(companion): Expand diagnostics by clicking anywhere in its header
- refactor(contract): make JSON Schema the sole contract
- chore(release): record companion build 5306
- fix(companion): stop blurring the composer as the app backgrounds
- feat(tui): Provide a standalone app with built-in speech
- fix(engine): keep image descriptions on the foreground provider
- fix(companion): set diagnostics facts at the dialog detail size
- fix(companion): Collapse the diagnostics panel under its header
- refactor(companion): Present diagnostics as individual values instead of paragraphs
- feat(companion): add project swipe deletion
- fix(cli): require exact patch anchors
- refactor(gh): rebuild the gh extension on typed dataclasses
- chore(deps): bump svar to 0.7.152
- feat(companion): Collapse the Application column with its header chevron
- refactor(extensions): adopt uplink in .vis, drop python examples
- feat(python): object-first tool pattern docs and remote server example
- fix(gateway): preserve long Copilot turns
- fix(companion): remove divider before project plus
- fix(companion): compact project creation and deletion
- fix(companion): target copied session title events
- refactor(cli): remove Nippy and Ruff sandbox shims
- fix(gateway): scope session headers to the start of a provider request
- fix(gateway): reclaim cancelled Python workers
- fix(tui): fold tool source with receipt
- feat(gateway): add session-start provider header hooks
- fix(tui): label visible Python source
- fix(sandbox): pass the parent's resolved interpreter to its worker
- feat(providers): add Claude Fable 5.1 support
- fix(companion): move pull-to-search prompt into app bar
- fix(companion): move favorites to row lead
- fix(runtime): preserve Python extension result objects
- fix(companion): restore queued action gutter
- fix(companion): preserve partial output after cancellation
- chore(companion): record release entry for build 5275
- chore(companion): record release entries for builds 5209-5232
- chore(config): catalog the vis-python-runtime sibling root
- feat(sandbox): run a session's extensions in the same interpreter as its sandbox
- fix(shell): resolve the runtime library in the process that spawns
- revert(repo): take somebody else's working tree back out of my commit
- feat(sandbox): give each gateway session its own Python worker process
- refactor(sandbox): make the python child a worker, one per key
- fix(gateway): stop every turn a session is running, and keep the session
- perf(gateway): build a session when one is asked for, not four at boot
- fix(sandbox): warn on a crossed session only when it was not the caller's
- fix(extensions): bind the host only when there is an interpreter to bind
- perf(cli): remember the prep answer instead of asking on every command
- feat(extensions): give a trusted extension the filesystem through vis.fs
- fix(sandbox): authorize a host call against the caller the interpreter names
- build(lint): lint sandbox leases as their own form rather than let
- feat(sandbox): let the jail switch decide the guest's confinement
- fix(sandbox): preserve runtime bindings while initializing the environment
- refactor(sandbox): stop repairing the subprocess redirect CPython does itself
- refactor(sandbox): use CPython descriptor management in the sandbox
- fix(sandbox): start the interpreter once and let the second session wait
- fix(cli): provide a protected Python println
- fix(companion): keep stop control circular
- fix(companion): balance project header controls
- fix(tui): increase new-session plus contrast
- fix(companion): Make queued and send controls circular
- fix(companion): align thinking with user prose
- chore(cli): merge main into CPython migration
- feat(runtime): adopt native CPython process runtime
- fix(cli): protect Python output callables
- fix(theme): strengthen TokyoNight companion contrast
- feat(theme): add all TokyoNight styles
- refactor(cli): remove extension helper commands
- feat(cli): support object tool namespaces
- chore(repo): merge current main into CPython migration
- fix(tui): preserve prose beside the execution receipt list
- fix(engine): stop retrying exhausted output requests
- docs(providers): explain managed authentication setup
- fix(providers): authenticate managed providers on first use
- fix(tui): keep Python source visible
- fix(companion): Add a bottom border to the session list
- chore(repo): reformat Clojure sources with zprint
- refactor(tui): drop cinema MP4 export and the commit convention test
- refactor(python): drop the anydoc sandbox door
- test(ci): stop policing the merge commit GitHub builds for a pull request
- fix(sandbox): support package installation and import before prior initialization
- feat(python): fetch the embedded interpreter from its release archive

### Package changes

#### com.blockether/vis
- chore(release): v0.1.43 (05ba30e96)
- fix(ci): repair release validation fixtures (22d89ca90)
- feat(sdk): add reproducible verification checks (089b14c2a)
- refactor(python): merge path aliases into filesystem roots (c06fc943f)
- feat(python): register project path globals and align prompt (6720741f9)
- feat: unify activity, SDK and authentication flows (4f9b7a0d6)
- fix(companion): align session controls and reserve hover actions (00ba8d011)
- fix(tui): prevent idle typing and scrolling stalls (5032bb463)
- fix(queue): resume TUI submissions after provider failures (789527556)
- fix(code-band): turn CODE red on failure instead of naming the error (5011ec941)
- fix(activity): Collapse every step by default and provide a chevron to expand it (9615517c5)
- fix(companion): align verbosity with TUI model capabilities (70e90c3ea)
- fix(tui): keep iteration prose on settled and restored turns (023e801a3)
- fix(mcp): correct connection status and caller context (ec3a4ee87)
- fix(tui): set a step's words one space after its mark (9ecd559ac)
- fix(gateway): preserve model verbosity capabilities (a19b18c62)
- feat(tui): enable typing stall diagnostics (4ae9e185b)
- fix(tui): align iteration prose with the answer, outside the timeline (913f44a6c)
- fix(tui): close a failed call's error rows with a blank row (8ddafdd50)
- fix(tui): omit absent chooser group headers (cd6b9913f)
- feat(activity): fold CODE and RESULT like THINKING in both clients (085685aac)
- fix(tui): remove redundant empty MCP message (006106f46)
- fix(tui): enable native access in the JVM launcher (74c4f4049)
- chore(release): record companion build 5421 (cbeef0e69)
- fix(tui): explain gateway connection failures (9b5dd76ed)
- fix(tui): repaint settings during theme preview (b474a223c)
- fix(companion): Expand Application by clicking anywhere in its header (5ee698244)
- fix(tui): show theme choices in a transient grid (2b4c6fd33)
- feat(context): estimate linked repository guidance in metrics (83cfc111a)
- fix(gateway): retract liveness marker on turn terminal synchronously (f5300cc0f)
- feat(activity): add rich symbol content and refine execution UI (81532c9d0)
- docs(agents): authorize commit and push for simple bug fixes (57361e612)
- fix(tui): infer bearer token for the default local gateway (12112e7c6)
- chore(deps): bump svar to 0.7.160 (dab3fdf2b)
- fix(companion): pad queued turn rows (699a5bcb9)
- fix(tui): explain MCP sign-in before saving (ab69c1680)
- fix(mcp): allow OAuth setup and preserve validation errors (46c545ec3)
- fix(companion): shrink queued remove mark (8afb50222)
- fix(companion): compact queued remove control (5aee1261f)
- fix(companion): reduce queued row padding (1f311d759)
- fix(companion): compact queued turn tray (13e2b0109)
- refactor(python): remove interpreter configuration override (f5a823c57)
- fix(context): publish fold counts without printed receipts (cfc6bf22e)
- fix(gateway): publish cancelled-turn terminal events from interrupted workers (9110095df)
- fix(context): refresh fold telemetry and record executed folds (fe2069a5a)
- refactor(core): finish engine and TUI cleanup (5f66784d2)
- feat(sdk): unify Python clients and interaction contracts (186f0735e)
- refactor(companion): drop duplicate limits from session health (6ddca8647)
- chore(release): record companion build 5397 (c47e9fa98)
- fix(companion): keep outline icons unfilled (48d1b443c)
- chore(release): record companion build 5395 (271782cc9)
- Merge remote-tracking branch 'origin/main' (e4baada06)
- ci(macos): route trusted builds to self-hosted runner (a9e596247)
- fix(clojure): honor project runners and isolate shadow tests (30f564552)
- feat(metrics): show persisted session health in app metrics (2bdd91916)
- fix(gh): archive log snapshots with window_lines (b19464c01)
- chore(deps): bump svar to 0.7.159 for cancel-safe Responses WebSockets (4c69fad4f)
- fix(companion): Reopen only the transcript displayed when the app terminated (bfd128712)
- test(core): upgrade Lazytest and synchronize Python regressions (94942232c)
- test(tui): make terminal reply draining deterministic (c837419ee)
- fix(gateway): run local speech without an AI session (aa0baf83d)
- docs(docs): remove completed implementation plan (708c55afb)
- fix(gateway): complete native Jetty set registrations (2ebcef9f3)
- fix(gateway): register Jetty path sets for native startup (a69059ab2)
- fix(gateway): register the native startup collection constructor (c64210ea1)
- ci(release): expose native gateway startup failures (d44c297f3)
- fix(cli): prepare the package directory before confining workers (e3c571330)
- chore(release): v0.1.42 (e759d7040)
- refactor(cli): consolidate Python workers in the runtime (bae4c4862)
- fix(companion): preserve touch-opened dialogs (065108c42)
- ci(build): move macOS jobs to GitHub-hosted runners (1fb47ebec)
- fix(clojure): format file-leading and trailing whitespace (d66dbf0e8)
- feat(housekeeping): sweep stale Python runtime versions (123363d97)
- refactor(core): group internal namespaces by domain (e15c3ff8e)
- feat(clojure): keep exactly one blank line between top-level forms (a906f25af)
- build(sandbox): consume Python runtime v0.4.1 (e4aa77c77)
- feat(cli): support recursive extension namespaces (3c6fd7a58)
- fix(companion): Match the diagnostics header height (f24472a6d)
- fix(companion): compact the diagnostics panel (f9fc94ff2)
- docs(cli): close resolved issues after release (566e761dc)
- fix(cli): load introspection in Python workers (0eee0e98d)
- fix(companion): Shrink queued message remove buttons (4043869ee)
- fix(cli): bind Python extension file metadata (004c336ba)
- fix(companion): fill starred session marks (11c7e1eab)
- fix(tui): route vis-agent tui to terminal client (f5f7681c6)
- docs(docs): point quick start at vis-agent TUI (51cfa79d7)
- chore(release): record companion build 5358 (b3f38edf1)
- fix(cli): use an absolute JVM classpath so --jvm runs from any directory (630a7e52b)
- feat(companion): make drawing rail collapsible (6d94cf315)
- refactor(gateway): fold the provider limits cache into the host (8246f32fc)
- feat(jail): isolate Python execution in native worker (697197a92)
- fix(companion): refine mobile drawing rail (a9396825b)
- fix(gateway): clear Anthropic status after OAuth (7812ab440)
- fix(companion): move drawing tools into side rail (0c6e417e0)
- fix(gateway): fall back after provider quota exhaustion (369888233)
- fix(companion): use icons and frame zoom controls (7b10e6d91)
- fix(companion): move drawing check into pencil slot (b60e4a6f1)
- feat(companion): use JetBrains Mono throughout (e160f1b4b)
- chore(release): record companion build 5347 (6c71f80da)
- fix(companion): let the sessions list scroll under the home indicator (285fe174b)
- feat(companion): Present phone pairing methods as alternatives, not sequential steps (e497f04a2)
- feat(companion): pair through one field on a three-step page (6414bf9b6)
- fix(companion): Pin parked sessions within their project and retain the list header (e9c6a98bb)
- fix(tui): support WSL2 PipeWire recording (accd9987a)
- fix(gateway): enforce snake_case diagnostic codes (50267f18a)
- fix(jail): allow unrestricted shell working directories (8c2318cb5)
- feat(companion): Provide Pake desktop installers and hide Scan QR on desktop (3c0b19da5)
- fix(cli): isolate embedded pytest runtime (f116224f2)
- fix(companion): Remove the row's Fork action; retain forking from a turn (797f3a1a4)
- feat(companion): Support forking from a session row or a specific turn (39f240d8a)
- fix(companion): Reduce the fork menu to two choices (732d13329)
- fix(companion): Limit the fit sheet height to the space below the notch (5879cd361)
- fix(cli): serialize embedded pytest runs (dc7378299)
- fix(companion): move the list toggle off the app bar, read the Mac host natively (4f7e92890)
- feat(companion): add the desktop conversation sidebar (b6b84a7e9)
- fix(companion): align and collapse the desktop sidebar (ebe133f14)
- feat(companion): Split the desktop layout into a session sidebar and transcript (d27a04802)
- fix(companion): Use circular icon buttons for desktop row actions, outside the row columns (f9f48bf25)
- fix(ci): prepare git dependencies on the pinned JDK (3d2a551c3)
- feat(companion): Use the prose font for session titles (69d856be5)
- build(jail): pin the released confinement runtime (7b0831a3d)
- refactor(jail): state the policy in Vis, let the runtime enforce it (00c79662f)
- refactor(sandbox): remove unused Python :fs/access checks (2b4b88d53)
- refactor(providers): fold the limits namespaces into their providers (21b5733d1)
- refactor(providers): fold the provider packs into core and retire extensions/ (ba955012c)
- refactor(languages): fold the Clojure and Python packs into core (91a244578)
- refactor(persistance): fold the SQLite backend into core (3e189d848)
- refactor(cli): remove obsolete search extension (46dc1ce38)
- feat(cli): add GPT-6 Astra Codex model (ad1c989ca)
- test(ci): align retired feature checks (160d88ca7)
- refactor(companion): remove obsolete terminal artifact bridge (5ca55fda5)
- test(core): trim redundant coverage and cap suite heap (2052615d8)
- test(cli): remove redundant schema validation cases (d0d3d2d65)
- test(cli): remove duplicated Python runtime coverage (92023bbcc)
- docs(audit): refresh dependency inventory (0cd7becf5)
- refactor(contract): remove aggregate and trim source commentary (55f185796)
- refactor(contract): validate config only with JSON Schema (6d185c334)
- fix(companion): Expand diagnostics by clicking anywhere in its header (8a3b1c0bc)
- refactor(contract): make JSON Schema the sole contract (927f2ef05)
- chore(release): record companion build 5306 (a569a5fdb)
- fix(companion): stop blurring the composer as the app backgrounds (22cf694c3)
- feat(tui): Provide a standalone app with built-in speech (d6b5d3871)
- fix(engine): keep image descriptions on the foreground provider (db37597a8)
- fix(companion): set diagnostics facts at the dialog detail size (f4f5fa0a1)
- fix(companion): Collapse the diagnostics panel under its header (2b3c3ac8b)
- refactor(companion): Present diagnostics as individual values instead of paragraphs (a3b8752d7)
- feat(companion): add project swipe deletion (582e7c4d6)
- fix(cli): require exact patch anchors (f492ef4de)
- refactor(gh): rebuild the gh extension on typed dataclasses (b3fabd24a)
- chore(deps): bump svar to 0.7.152 (abf347d02)
- feat(companion): Collapse the Application column with its header chevron (dd7eac31d)
- refactor(extensions): adopt uplink in .vis, drop python examples (61837fb82)
- feat(python): object-first tool pattern docs and remote server example (8b374d870)
- fix(gateway): preserve long Copilot turns (bdbce8d53)
- fix(companion): remove divider before project plus (85f0ffd14)
- fix(companion): compact project creation and deletion (0f74acffe)
- fix(companion): target copied session title events (8505aba10)
- refactor(cli): remove Nippy and Ruff sandbox shims (09d04eba6)
- fix(gateway): scope session headers to the start of a provider request (e22a7ec68)
- fix(gateway): reclaim cancelled Python workers (91d453cf6)
- feat(gateway): add session-start provider header hooks (ce942d0d7)
- fix(sandbox): pass the parent's resolved interpreter to its worker (7c4a9990d)
- feat(providers): add Claude Fable 5.1 support (b6d2b99b1)
- fix(companion): move pull-to-search prompt into app bar (08420178f)
- fix(companion): move favorites to row lead (c2c9bff7e)
- fix(runtime): preserve Python extension result objects (faaf0a57d)
- fix(companion): restore queued action gutter (1872145ef)
- fix(companion): preserve partial output after cancellation (e49ac76f3)
- chore(companion): record release entry for build 5275 (f2023ad63)
- chore(companion): record release entries for builds 5209-5232 (b15d79597)
- chore(config): catalog the vis-python-runtime sibling root (933cfebfb)
- feat(sandbox): run a session's extensions in the same interpreter as its sandbox (e9158d8d2)
- fix(shell): resolve the runtime library in the process that spawns (22d51ff00)
- revert(repo): take somebody else's working tree back out of my commit (c37c135de)
- feat(sandbox): give each gateway session its own Python worker process (6bb886915)
- refactor(sandbox): make the python child a worker, one per key (4dce4ceb3)
- fix(gateway): stop every turn a session is running, and keep the session (a951a8d70)
- perf(gateway): build a session when one is asked for, not four at boot (367bb4a13)
- fix(sandbox): warn on a crossed session only when it was not the caller's (8eba77e1d)
- fix(extensions): bind the host only when there is an interpreter to bind (9e0f54b1e)
- perf(cli): remember the prep answer instead of asking on every command (9ce67eead)
- feat(extensions): give a trusted extension the filesystem through vis.fs (df514f6d0)
- fix(sandbox): authorize a host call against the caller the interpreter names (7479ae44b)
- build(lint): lint sandbox leases as their own form rather than let (2fb8d37da)
- feat(sandbox): let the jail switch decide the guest's confinement (128d6d90e)
- fix(sandbox): preserve runtime bindings while initializing the environment (6344faa4a)
- refactor(sandbox): stop repairing the subprocess redirect CPython does itself (416f79df8)
- refactor(sandbox): use CPython descriptor management in the sandbox (4621f1d13)
- fix(sandbox): start the interpreter once and let the second session wait (e486a1de9)
- fix(cli): provide a protected Python println (52231562f)
- fix(companion): keep stop control circular (f030957e1)
- fix(companion): balance project header controls (96a1c122c)
- fix(companion): Make queued and send controls circular (c6cd6d1cc)
- fix(companion): align thinking with user prose (67fe76eef)
- chore(cli): merge main into CPython migration (95c11d80e)
- feat(runtime): adopt native CPython process runtime (6eaff8b07)
- fix(cli): protect Python output callables (4d1f465ff)
- fix(theme): strengthen TokyoNight companion contrast (9a2c1a3d1)
- feat(theme): add all TokyoNight styles (07d5639d8)
- refactor(cli): remove extension helper commands (64b1de671)
- feat(cli): support object tool namespaces (16076dc7f)
- chore(repo): merge current main into CPython migration (af5e0dee1)
- fix(engine): stop retrying exhausted output requests (c6d3317b2)
- docs(providers): explain managed authentication setup (31bb9d090)
- fix(providers): authenticate managed providers on first use (66586f212)
- fix(companion): Add a bottom border to the session list (04dbe734e)
- chore(repo): reformat Clojure sources with zprint (24cb0325f)
- refactor(tui): drop cinema MP4 export and the commit convention test (345d741a3)
- refactor(python): drop the anydoc sandbox door (966e46523)
- test(ci): stop policing the merge commit GitHub builds for a pull request (0381c6b25)
- fix(sandbox): support package installation and import before prior initialization (aca874e9e)
- feat(python): fetch the embedded interpreter from its release archive (55226d959)



## [v0.1.42] - 2026-09-05

### Changed
- Consolidate Python worker execution in vis-python-runtime 0.4.2 and initialize session sandboxes lazily.
- Share host certificate trust with native Python and preserve pip index, certificate and proxy options.
- Build native releases with GraalVM CE 25.3.4.1; allow ordinary JVM launches on stock JDK 25+.
- Package Python guest modules with the native distribution and verify wrapper-based wheel installation.
- refactor(jail): state the confinement policy in Vis and let the runtime enforce it; `jail.mach_services` becomes the platform-neutral `jail.keychain: true`
- feat(zai): make GLM-5.3-Flash the default Coding Plan model through svar 0.7.146
- fix(live): answer a live-view tap without waiting out the provider tick
- feat(gateway): authenticate GitHub CLI through private human input
- fix(companion): reveal live artifacts after an interrupt
- refactor(extensions): expose one GitHub Actions watcher for both runs and pull-request checks
- fix(gateway): supersede explicit GitHub run watches when replacement runs start
- fix(live): rewrite a live log pane without pumping copies into its record
- fix(live): read archived focus snapshots using wire-format keys
- fix(sandbox): start the interpreter once and let the second session wait
- refactor(sandbox): use CPython descriptor management in the sandbox
- refactor(sandbox): stop repairing the subprocess redirect CPython does itself
- fix(sandbox): preserve runtime bindings while initializing the environment
- fix(sandbox): authorize a host call against the caller the interpreter names
- feat(extensions): give a trusted extension the filesystem through vis.fs
- fix(extensions): stop the first Python extension failing where there is no interpreter to bind
- perf(gateway): build a session when one is asked for, not four at boot
- fix(gateway): stop every turn a session is running, and keep the session
- feat(sandbox): give each gateway session its own Python worker process
- feat(sandbox): run a session's extensions in the same interpreter as its sandbox
## [v0.1.41] - 2026-08-21

### Changed
- chore(release): v0.1.41
- feat(companion): tighten composer footer spacing
- feat(tui): clarify response footer controls
- feat(live): return optimized model results
- fix(cli): merge Ubuntu and Windows trust in WSL
- fix(companion): keep keyboard open after send
- fix(companion): retain finished job details
- fix(companion): Disable justification in fenced code
- fix(companion): Disable justification in Markdown code spans
- refactor(extensions): remove duplicate CI activity
- fix(companion): resume Android beta publishing
- chore(cli): bump svar to 0.7.126
- refactor(extensions): share live view test harness
- fix(companion): rename quick reasoning effort to low
- fix(companion): restore the verbosity composer control
- style(companion): preserve live artifact formatting
- fix(companion): settle superseded CI states
- fix(cli): find parent tests for nested sources
- fix(companion): scope tester notes to app changes
- docs(companion): record TestFlight build 4618
- docs(cli): record parinferish 0.1.1 in audit
- fix(companion): sync selected GitHub job
- docs(companion): record TestFlight build 4615
- fix(cli): Use parinferish closing-delimiter relocation
- feat(cli): Use Windows trust stores through WSL
- feat(cli): discover system certificate stores
- fix(tui): Keep finished live runs in stable positions
- chore(cli): add clj-parinferish workspace root
- fix(extensions): isolate live views from test runs
- fix(companion): keep short responses beside composer
- fix(extensions): show GitHub run start time
- fix(companion): load session totals with list
- fix(live): improve GitHub watch recovery and identify NDJSON records
- fix(companion): keep composer pinned during keyboard dismissal
- fix(companion): Collapse repeated live-run snapshots
- fix(tui): stop watching superseded CI runs
- fix(companion): preserve interrupted live records
- fix(tui): show live CI job activity
- ci(release): use GitHub-hosted macOS runners
- docs(gateway): refresh dependency audit
- build(gateway): bump svar to 0.7.125
- feat(tui): minimize active live views
- fix(tui): expand and focus live CI jobs
- chore(release): merge concurrent main update
- fix(release): install macOS native test dependency
- fix(shell): slice log pages as output text
- fix(release): install macOS native test dependency
- fix(shell): slice log pages as output text
- fix(gateway): scale folding budget for small contexts
- feat(shell): page log windows on the handle
- ci(release): run macOS native image on Blacksmith
- feat(companion): focus live CI jobs
- chore(gateway): merge concurrent main update
- fix(gateway): count every successful session fold
- fix(gateway): skip auto-title after cancelled turns
- fix(gateway): preserve the session's selected provider
- fix(engine): unwedge a turn queued behind a leaked Python GIL
- fix(editing): check access to requested grep paths rather than their parent directories
- fix(loop): keep the record of a view a stopped block abandoned
- build(ci): run every macOS job on our own Apple-silicon runner
- fix(ci): close the test sandboxes that were exhausting the heap
- ci(gateway): give the Linux test suite the heap it needs
- fix(ci): resolve the lint and test failures blocking the pipeline
- fix(companion): settle Settings on the machine a session came from
- docs(docs): state the boundary, do not prescribe the call site
- docs(docs): a Path crosses a tool boundary as itself
- fix(docs): let a lower-case ask reach a camelCase name
- fix(companion): persist machine-unreachable state detected by this device
- refactor(cli): drop the duplicate by-cwd view from tool results
- docs(python): a capped grep names next(r), not a retyped call
- refactor(python): walk grep pages with next(), not more()
- fix(tui): copy short thinking blocks independently
- fix(tui): apply settings through gateway
- feat(python): page a capped grep from the result itself
- docs(changelog): record the shell out rename and the daemon live view
- refactor(shell): name the merged pty stream out
- fix(tui): render live views opened by the serve daemon
- fix(python): give the PIL shim Pillow's whole helper-module surface
- fix(loop): stop create-environment abandoning its sandbox when a later step fails
- feat(sandbox): centralize sandbox teardown and its ordering
- fix(python): close the Engine with contexts built outside python-extensions
- docs(cli): document merged pty stderr and stdout
- fix(companion): shorten the composer's queued placeholder
- fix(cli): close the PIL shim's remaining Pillow gaps
- fix(companion): let the human, not the mounted screen, take a share
- fix(sandbox): give PIL the CSS colours, hsl wheels and a real mode filter
- feat(companion): accept shared files and let the share pick its session
- fix(companion): preserve machine scope after one failed read
- fix(python): hold the condition lock across gather children
- feat(config): declare a non-secret literal in environment:
- docs(release): record companion TestFlight build 4546
- feat(shell): scroll the log line window back up with a negative count
- fix(tui): Render recording transcripts as quotations, not code
- fix(companion): Render memo transcripts as quotations instead of decorated blocks
- fix(loop): Dispose of environments replaced by cache inserts
- fix(cli): Normalize near-matching tool keywords instead of rejecting calls
- fix(tui): Wrap recording transcripts at word boundaries
- feat(companion): Open a recording's transcription below its player
- feat(tui): Collapse recording transcripts under their associated turn
- feat(gateway): Include attached recording transcriptions in the model's manifest
- feat(config): Allow issued credentials to specify their API format
- docs(docs): Split long documentation paragraphs into lists, tables and steps
- fix(tui): Include live-view row types in the SDK wheel
- feat(config): Use one validated API-format vocabulary with normalized inputs
- fix(companion): Identify the live view shown in the running row
- fix(tui): Identify the live view shown in the running row
- docs(cli): Standardize tool documentation pages and validate them against handlers
- feat(extensions): Update GitHub watches continuously and retain one final view
- fix(cli): keep project-scoped blocks out of the machine store
- fix(companion): Use the extension to identify Android octet-stream selections
- fix(tui): Isolate dropped recording paths in the same way as image paths
- feat(gateway): Detect all supported recorder formats, not only m4a
- docs(docs): Validate documentation against one page structure
- feat(cli): run_tests takes deps.edn aliases for the clean-JVM run
- feat(companion): reach any file from + and play what it cannot show
- feat(gateway): accept voice recordings as attachments
- docs(cli): document the required parameters enforced by language operations
- chore(deps): bump imaging to 0.1.10 to support empty presentations
- chore(deps): bump svar to 0.7.124
- test(providers): prove MANAGED crosses the Python extension boundary
- docs(cli): run_tests and repl_eval state requiredness and the `project` spelling
- feat(providers): let an extension declare a MANAGED provider
- fix(language-surface): accept project as cwd for all language operations
- fix(tui): show OPTION content in the panel and wrap wide headings
- docs(release): record the TestFlight builds 4488 and 4508
- fix(tui): measure dialog widths in lanterna columns, not chars
- fix(human-input): attach late close records to their iteration after the block ends
- docs(docs): drop sub-agent vocabulary and the drifting host-op count
- fix(tui): show finished runs as transcript rows rather than live-panel content
- fix(python): one spelling, runner, for the test backend on every surface
- perf(companion): Fetch notification state once per machine
- fix(python): Accept pathlib.Path wherever shims accept paths
- fix(companion): Fetch machine notification state before opening its settings row
- test(python): dispose three more single-test sandboxes
- fix(extensions): Watch GitHub runs until completion
- style(repo): format every namespace with the canonical formatter
- fix(build): Handle binding forms inside #() without formatter failures
- fix(gateway): Retire inactive client leases
- refactor(gateway): identify a source build by its commit alone
- fix(human-input): Exclude watched-run time from execution deadlines
- feat(cli): Show the gateway's current build and available replacement
- fix(human-input): Attribute live-view wait time to the watching block
- fix(languages): Report failed REPL starts consistently across languages
- fix(gateway): refuse a status this build cannot read
- feat(gateway): identify a dev build by its commit, not by "dev"
- feat(gateway): Replace an idle daemon when a new build is available
- fix(languages): one REPL lifecycle contract for every language
- fix(gateway): Stop managed daemons when no clients remain
- test(languages): read pack test counts by their contract names
- fix(language-surface): refuse a bare string where repl options belong
- fix(language-surface): Interpret repl_stop's first argument as its ID regardless of later arguments
- docs(config): point whole-store writers at update-machine-config!
- test(posix-shim): dispose the sandboxes this suite builds for itself
- fix(companion): Apply the stored palette on the first frame
- test(network-guard): close the Engine with the sandbox it belongs to
- test(config): cover removed_providers in the exhaustive config fixture
- feat(providers): make deleting a provider actually delete it
- test(python): Restrict each test sandbox to that test's lifetime
- fix(gateway): say why a provider cannot be deleted instead of silently refusing
- docs(language-surface): Document the four REPL lifecycle operations
- docs(cli): describe ls's tree string where the sandbox reads it
- fix(gateway): Count folds from turn context records
- fix(gateway): let /projects/overview and /projects/:pid coexist
- feat(companion): Render project counts from the gateway overview
- feat(cli): render ls as one compact tree string
- fix(companion): Inset panel actions rather than extending them to the panel edges
- chore(python): format _outside.py the way ruff 0.16.3 does
- fix(companion): Match the notification action to the panel action container
- fix(companion): Show newly created sessions without requiring a filter tap
- docs(gateway): correct the remote-target claims to what the code does
- feat(companion): Fork a session or selected turn from its row actions
- fix(gateway): Store settings without merging configuration levels
- fix(python): Verify the first live-view operation after machine startup
- feat(extensions): watch a GitHub Actions run in one live view
- fix(companion): Open the project inventory rather than the browser from the projects icon
- fix(loop): Preserve printed output in failed-block responses
- docs(cli): name the remote gateway flags in help, not only in the docs
- fix(gateway): Avoid boxing the log-page window and remove unused test bindings
- fix(sandbox): repair the PIL envelope arity, and stop repeating the scope
- feat(sandbox): Declare guest exports in shims and manage their lifetime automatically
- fix(sandbox): Restrict guest host objects to the session that opened them
- docs(test): Document duplicate registration in the shared sandbox
- refactor(loop): Remove child-environment code used only by sub_loop
- fix(python): one GraalPy Engine per session, not one per process
- refactor(harness): Remove agent() and all sub_loop callers
- docs(release): record the TestFlight build testers now have
- refactor(human-input): Make sink-only functions private
- fix(human-input): Serve completed view records and persist them on stop
- feat(human-input): Save finished live views as artifacts
- fix(contract): Stabilize the live materializer used by the TUI channel
- refactor(gateway): serve on Jetty 12 core, drop the ee9 servlet layer
- docs(gateway): name the commands a remote target does not redirect
- feat(gateway): Control a remote gateway from the command line
- test(gateway): Test that gateway shutdown publishes pending patches
- refactor(gateway): Remove unused live-view data and flush patches on stop
- feat(human-input): Arrange live views with the same groups as forms
- fix(companion): Show the pull-to-search activation state
- fix(companion): load the session list behind an open transcript
- fix(companion): Keep turns before and after the visible turn laid out
- test(tui): Show the entire aside section and test is_aside outside Vis
- feat(tui): Fill the section, render inline Markdown and support aside nodes
- feat(human-input): Allow users to stop any live view with a note
- feat(companion): watch and stop a live view from the phone
- feat(cli): open live views from a Python extension

### Package changes

#### com.blockether/vis
- chore(release): v0.1.41 (845c396e5)
- feat(companion): tighten composer footer spacing (8e6180024)
- feat(tui): clarify response footer controls (e9a90f4cb)
- feat(live): return optimized model results (ec9864ec8)
- fix(cli): merge Ubuntu and Windows trust in WSL (aeceaa0ce)
- fix(companion): keep keyboard open after send (cc88261f5)
- fix(companion): retain finished job details (4bed954db)
- fix(companion): Disable justification in fenced code (4a67ac707)
- fix(companion): Disable justification in Markdown code spans (eaee188a7)
- refactor(extensions): remove duplicate CI activity (28e9aaa51)
- fix(companion): resume Android beta publishing (c724b13ce)
- chore(cli): bump svar to 0.7.126 (994c7ff07)
- refactor(extensions): share live view test harness (b16203edf)
- fix(companion): rename quick reasoning effort to low (19cc394e2)
- fix(companion): restore the verbosity composer control (6787fe808)
- style(companion): preserve live artifact formatting (0e34d4c7a)
- fix(companion): settle superseded CI states (57d22fccb)
- fix(companion): scope tester notes to app changes (4485e3c8b)
- docs(companion): record TestFlight build 4618 (27a5ffbea)
- docs(cli): record parinferish 0.1.1 in audit (02602989f)
- fix(companion): sync selected GitHub job (73bce25d5)
- docs(companion): record TestFlight build 4615 (7b847c0a4)
- fix(cli): Use parinferish closing-delimiter relocation (e2cd4f701)
- feat(cli): Use Windows trust stores through WSL (485363ecf)
- feat(cli): discover system certificate stores (52677f502)
- fix(tui): Keep finished live runs in stable positions (a614c9029)
- chore(cli): add clj-parinferish workspace root (93d5fdc1d)
- fix(extensions): isolate live views from test runs (3c64395a9)
- fix(companion): keep short responses beside composer (992185d5b)
- fix(extensions): show GitHub run start time (fd24cd394)
- fix(companion): load session totals with list (58c8a3815)
- fix(live): improve GitHub watch recovery and identify NDJSON records (089547073)
- fix(companion): keep composer pinned during keyboard dismissal (8b07ee528)
- fix(companion): Collapse repeated live-run snapshots (f0500d788)
- fix(tui): stop watching superseded CI runs (8094319b3)
- fix(companion): preserve interrupted live records (ef73c0271)
- fix(tui): show live CI job activity (c15bcc121)
- ci(release): use GitHub-hosted macOS runners (4ad2f023c)
- docs(gateway): refresh dependency audit (183f72652)
- build(gateway): bump svar to 0.7.125 (18173bba8)
- feat(tui): minimize active live views (a94cc8278)
- fix(tui): expand and focus live CI jobs (6f5d67865)
- fix(release): install macOS native test dependency (9e0c63d91)
- fix(shell): slice log pages as output text (5ff3caaf6)
- fix(gateway): scale folding budget for small contexts (e33c08ab8)
- feat(shell): page log windows on the handle (448463804)
- ci(release): run macOS native image on Blacksmith (fb934f4f4)
- feat(companion): focus live CI jobs (47cc50ac3)
- fix(gateway): skip auto-title after cancelled turns (851f41948)
- fix(gateway): preserve the session's selected provider (0e5fdf521)
- fix(engine): unwedge a turn queued behind a leaked Python GIL (b3c78e43f)
- fix(editing): check access to requested grep paths rather than their parent directories (b9af259d4)
- fix(loop): keep the record of a view a stopped block abandoned (df1280ba5)
- build(ci): run every macOS job on our own Apple-silicon runner (d19f66361)
- fix(ci): close the test sandboxes that were exhausting the heap (a91da2786)
- ci(gateway): give the Linux test suite the heap it needs (dab6dfb18)
- fix(ci): resolve the lint and test failures blocking the pipeline (a6b091834)
- fix(companion): settle Settings on the machine a session came from (ef54e2323)
- docs(docs): state the boundary, do not prescribe the call site (4e77cf513)
- docs(docs): a Path crosses a tool boundary as itself (73f906ed2)
- fix(docs): let a lower-case ask reach a camelCase name (3ea6c2711)
- fix(companion): persist machine-unreachable state detected by this device (010d8cde3)
- refactor(cli): drop the duplicate by-cwd view from tool results (86a35379d)
- docs(python): a capped grep names next(r), not a retyped call (28cfaeb62)
- refactor(python): walk grep pages with next(), not more() (aadf51db3)
- fix(tui): apply settings through gateway (0d4114f6e)
- feat(python): page a capped grep from the result itself (133fd9c0f)
- docs(changelog): record the shell out rename and the daemon live view (5c352fefc)
- refactor(shell): name the merged pty stream out (879c79cb9)
- fix(tui): render live views opened by the serve daemon (c0ceb78bb)
- fix(python): give the PIL shim Pillow's whole helper-module surface (e87af58fd)
- fix(loop): stop create-environment abandoning its sandbox when a later step fails (0a80cc775)
- feat(sandbox): centralize sandbox teardown and its ordering (ef71d025e)
- fix(python): close the Engine with contexts built outside python-extensions (251f5d710)
- docs(cli): document merged pty stderr and stdout (094256421)
- fix(companion): shorten the composer's queued placeholder (ab87a9ea3)
- fix(cli): close the PIL shim's remaining Pillow gaps (4dd507f6e)
- fix(companion): let the human, not the mounted screen, take a share (3f97a49f3)
- fix(sandbox): give PIL the CSS colours, hsl wheels and a real mode filter (76fbe2316)
- feat(companion): accept shared files and let the share pick its session (86d3fac0f)
- fix(companion): preserve machine scope after one failed read (e7d184d0c)
- fix(python): hold the condition lock across gather children (e68c119c6)
- feat(config): declare a non-secret literal in environment: (a50b09c86)
- docs(release): record companion TestFlight build 4546 (bd908456b)
- feat(shell): scroll the log line window back up with a negative count (a46b91043)
- fix(companion): Render memo transcripts as quotations instead of decorated blocks (1c2dc41e4)
- fix(loop): Dispose of environments replaced by cache inserts (22210bd3c)
- fix(cli): Normalize near-matching tool keywords instead of rejecting calls (2351576e5)
- feat(companion): Open a recording's transcription below its player (1fc0f9fd0)
- feat(gateway): Include attached recording transcriptions in the model's manifest (898ca2ebd)
- feat(config): Allow issued credentials to specify their API format (fcf08a7d9)
- docs(docs): Split long documentation paragraphs into lists, tables and steps (09fe3914f)
- fix(tui): Include live-view row types in the SDK wheel (8ae7c4067)
- feat(config): Use one validated API-format vocabulary with normalized inputs (845d807d8)
- fix(companion): Identify the live view shown in the running row (3830f09a8)
- docs(cli): Standardize tool documentation pages and validate them against handlers (9ebe5f56f)
- feat(extensions): Update GitHub watches continuously and retain one final view (ede8099e3)
- fix(cli): keep project-scoped blocks out of the machine store (4fa5eccef)
- fix(companion): Use the extension to identify Android octet-stream selections (8a972d1b4)
- feat(gateway): Detect all supported recorder formats, not only m4a (f59a1176f)
- docs(docs): Validate documentation against one page structure (fca795da9)
- feat(cli): run_tests takes deps.edn aliases for the clean-JVM run (a92be52fd)
- feat(companion): reach any file from + and play what it cannot show (c4c0914ec)
- feat(gateway): accept voice recordings as attachments (1fc0d2852)
- docs(cli): document the required parameters enforced by language operations (1534fd728)
- chore(deps): bump imaging to 0.1.10 to support empty presentations (2e9aa9c10)
- chore(deps): bump svar to 0.7.124 (6ed5893c5)
- test(providers): prove MANAGED crosses the Python extension boundary (c79a5679e)
- docs(cli): run_tests and repl_eval state requiredness and the `project` spelling (a91d72e90)
- feat(providers): let an extension declare a MANAGED provider (95a29c152)
- fix(language-surface): accept project as cwd for all language operations (8f35760e0)
- docs(release): record the TestFlight builds 4488 and 4508 (20065edda)
- fix(human-input): attach late close records to their iteration after the block ends (682665bff)
- docs(docs): drop sub-agent vocabulary and the drifting host-op count (2d71f8304)
- fix(tui): show finished runs as transcript rows rather than live-panel content (e379b3b6a)
- fix(python): one spelling, runner, for the test backend on every surface (4719ef96d)
- perf(companion): Fetch notification state once per machine (057697b52)
- fix(python): Accept pathlib.Path wherever shims accept paths (020727bff)
- fix(companion): Fetch machine notification state before opening its settings row (281c28d6a)
- test(python): dispose three more single-test sandboxes (95037c627)
- fix(extensions): Watch GitHub runs until completion (192162006)
- style(repo): format every namespace with the canonical formatter (211f49894)
- fix(build): Handle binding forms inside #() without formatter failures (2575e9a6a)
- fix(gateway): Retire inactive client leases (8670f3a3a)
- refactor(gateway): identify a source build by its commit alone (62ff92d46)
- fix(human-input): Exclude watched-run time from execution deadlines (2a24b9ee9)
- feat(cli): Show the gateway's current build and available replacement (659a7e464)
- fix(human-input): Attribute live-view wait time to the watching block (131618013)
- fix(languages): Report failed REPL starts consistently across languages (2b2ed346c)
- fix(gateway): refuse a status this build cannot read (9687d8ff3)
- feat(gateway): identify a dev build by its commit, not by "dev" (c93567d31)
- feat(gateway): Replace an idle daemon when a new build is available (bcc0c8208)
- fix(languages): one REPL lifecycle contract for every language (8d3849c43)
- fix(gateway): Stop managed daemons when no clients remain (7b1c56db8)
- fix(language-surface): refuse a bare string where repl options belong (15104751e)
- fix(language-surface): Interpret repl_stop's first argument as its ID regardless of later arguments (3bf013412)
- docs(config): point whole-store writers at update-machine-config! (5d3859cdb)
- test(posix-shim): dispose the sandboxes this suite builds for itself (526305a13)
- fix(companion): Apply the stored palette on the first frame (9da688df8)
- test(network-guard): close the Engine with the sandbox it belongs to (7d8173d24)
- test(config): cover removed_providers in the exhaustive config fixture (89ac152c2)
- feat(providers): make deleting a provider actually delete it (e058c8981)
- test(python): Restrict each test sandbox to that test's lifetime (b79dee16b)
- fix(gateway): say why a provider cannot be deleted instead of silently refusing (b839b72c9)
- docs(language-surface): Document the four REPL lifecycle operations (413640b3c)
- docs(cli): describe ls's tree string where the sandbox reads it (831eeb49e)
- fix(gateway): let /projects/overview and /projects/:pid coexist (525f69c7b)
- feat(companion): Render project counts from the gateway overview (21375640d)
- feat(cli): render ls as one compact tree string (e192628bd)
- fix(companion): Inset panel actions rather than extending them to the panel edges (6f15c5ec3)
- chore(python): format _outside.py the way ruff 0.16.3 does (d8b5815db)
- fix(companion): Match the notification action to the panel action container (12ba1ba0a)
- fix(companion): Show newly created sessions without requiring a filter tap (a6746f2b5)
- docs(gateway): correct the remote-target claims to what the code does (f5c63b4a9)
- feat(companion): Fork a session or selected turn from its row actions (f6c426551)
- fix(gateway): Store settings without merging configuration levels (8aba1aa52)
- fix(python): Verify the first live-view operation after machine startup (a075a41c8)
- feat(extensions): watch a GitHub Actions run in one live view (c682f1321)
- fix(companion): Open the project inventory rather than the browser from the projects icon (c2e5d4122)
- fix(loop): Preserve printed output in failed-block responses (f1e879210)
- docs(cli): name the remote gateway flags in help, not only in the docs (f10f05930)
- fix(gateway): Avoid boxing the log-page window and remove unused test bindings (4821af913)
- fix(sandbox): repair the PIL envelope arity, and stop repeating the scope (10aeef006)
- feat(sandbox): Declare guest exports in shims and manage their lifetime automatically (fb58b8fe7)
- fix(sandbox): Restrict guest host objects to the session that opened them (1a7ba8f4c)
- docs(test): Document duplicate registration in the shared sandbox (b4dee14d6)
- refactor(loop): Remove child-environment code used only by sub_loop (bb918bd7e)
- fix(python): one GraalPy Engine per session, not one per process (66bff4fd3)
- refactor(harness): Remove agent() and all sub_loop callers (2bafa1de5)
- docs(release): record the TestFlight build testers now have (4f59e0df3)
- refactor(human-input): Make sink-only functions private (f03e0ddb8)
- fix(human-input): Serve completed view records and persist them on stop (3fc614cb1)
- feat(human-input): Save finished live views as artifacts (b943593a5)
- fix(contract): Stabilize the live materializer used by the TUI channel (cb1cd2208)
- refactor(gateway): serve on Jetty 12 core, drop the ee9 servlet layer (d122fd9df)
- docs(gateway): name the commands a remote target does not redirect (556fc2202)
- feat(gateway): Control a remote gateway from the command line (8574bea26)
- test(gateway): Test that gateway shutdown publishes pending patches (fa732df29)
- refactor(gateway): Remove unused live-view data and flush patches on stop (e4010ffa5)
- feat(human-input): Arrange live views with the same groups as forms (4269ec1a5)
- fix(companion): Show the pull-to-search activation state (1d9620dbf)
- fix(companion): load the session list behind an open transcript (687b121e9)
- fix(companion): Keep turns before and after the visible turn laid out (5b21f1ce9)
- test(tui): Show the entire aside section and test is_aside outside Vis (f36b60e4a)
- feat(tui): Fill the section, render inline Markdown and support aside nodes (2dc4e63c0)
- feat(human-input): Allow users to stop any live view with a note (898b43f77)
- feat(companion): watch and stop a live view from the phone (10ca1f34d)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-channel-tui
- feat(tui): clarify response footer controls (e9a90f4cb)
- fix(tui): Keep finished live runs in stable positions (a614c9029)
- feat(tui): minimize active live views (a94cc8278)
- fix(tui): expand and focus live CI jobs (6f5d67865)
- fix(tui): copy short thinking blocks independently (27bf4deae)
- fix(tui): apply settings through gateway (0d4114f6e)
- refactor(shell): name the merged pty stream out (879c79cb9)
- fix(tui): render live views opened by the serve daemon (c0ceb78bb)
- feat(config): declare a non-secret literal in environment: (a50b09c86)
- fix(tui): Render recording transcripts as quotations, not code (ac9e2bde2)
- fix(tui): Wrap recording transcripts at word boundaries (2132830f7)
- feat(tui): Collapse recording transcripts under their associated turn (e8b8c9e73)
- fix(tui): Include live-view row types in the SDK wheel (8ae7c4067)
- fix(tui): Identify the live view shown in the running row (f03c36211)
- fix(tui): Isolate dropped recording paths in the same way as image paths (6587b6a7a)
- feat(providers): let an extension declare a MANAGED provider (95a29c152)
- fix(tui): show OPTION content in the panel and wrap wide headings (c57466fb0)
- fix(tui): measure dialog widths in lanterna columns, not chars (dfa919688)
- fix(tui): show finished runs as transcript rows rather than live-panel content (e379b3b6a)
- style(repo): format every namespace with the canonical formatter (211f49894)
- fix(companion): Show newly created sessions without requiring a filter tap (a6746f2b5)
- fix(gateway): Store settings without merging configuration levels (8aba1aa52)
- docs(cli): name the remote gateway flags in help, not only in the docs (f10f05930)
- feat(human-input): Save finished live views as artifacts (b943593a5)
- refactor(gateway): Remove unused live-view data and flush patches on stop (e4010ffa5)
- feat(human-input): Arrange live views with the same groups as forms (4269ec1a5)
- test(tui): Show the entire aside section and test is_aside outside Vis (f36b60e4a)
- feat(tui): Fill the section, render inline Markdown and support aside nodes (2dc4e63c0)
- feat(human-input): Allow users to stop any live view with a note (898b43f77)
- feat(companion): watch and stop a live view from the phone (10ca1f34d)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-foundation-bridge
- docs(cli): Standardize tool documentation pages and validate them against handlers (9ebe5f56f)
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-foundation-search
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-foundation-voice
- feat(gateway): Include attached recording transcriptions in the model's manifest (898ca2ebd)
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-language-clojure
- fix(cli): find parent tests for nested sources (a2d6ae373)
- fix(cli): Use parinferish closing-delimiter relocation (e2cd4f701)
- refactor(cli): drop the duplicate by-cwd view from tool results (86a35379d)
- feat(cli): run_tests takes deps.edn aliases for the clean-JVM run (a92be52fd)
- style(repo): format every namespace with the canonical formatter (211f49894)
- fix(languages): one REPL lifecycle contract for every language (8d3849c43)
- fix(language-surface): refuse a bare string where repl options belong (15104751e)
- fix(language-surface): Interpret repl_stop's first argument as its ID regardless of later arguments (3bf013412)
- docs(language-surface): Document the four REPL lifecycle operations (413640b3c)
- fix(companion): Show newly created sessions without requiring a filter tap (a6746f2b5)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-language-python
- fix(extensions): isolate live views from test runs (3c64395a9)
- refactor(cli): drop the duplicate by-cwd view from tool results (86a35379d)
- fix(python): one spelling, runner, for the test backend on every surface (4719ef96d)
- style(repo): format every namespace with the canonical formatter (211f49894)
- fix(languages): Report failed REPL starts consistently across languages (2b2ed346c)
- fix(languages): one REPL lifecycle contract for every language (8d3849c43)
- test(languages): read pack test counts by their contract names (aae6e5046)
- fix(language-surface): refuse a bare string where repl options belong (15104751e)
- fix(language-surface): Interpret repl_stop's first argument as its ID regardless of later arguments (3bf013412)
- docs(language-surface): Document the four REPL lifecycle operations (413640b3c)
- fix(companion): Show newly created sessions without requiring a filter tap (a6746f2b5)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-language-typescript-bun
- style(repo): format every namespace with the canonical formatter (211f49894)
- fix(languages): Report failed REPL starts consistently across languages (2b2ed346c)
- fix(languages): one REPL lifecycle contract for every language (8d3849c43)
- test(languages): read pack test counts by their contract names (aae6e5046)
- fix(language-surface): refuse a bare string where repl options belong (15104751e)
- fix(language-surface): Interpret repl_stop's first argument as its ID regardless of later arguments (3bf013412)
- docs(language-surface): Document the four REPL lifecycle operations (413640b3c)
- fix(companion): Show newly created sessions without requiring a filter tap (a6746f2b5)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-persistance-sqlite
- fix(live): improve GitHub watch recovery and identify NDJSON records (089547073)
- fix(gateway): count every successful session fold (71e3b7335)
- feat(gateway): Include attached recording transcriptions in the model's manifest (898ca2ebd)
- fix(human-input): attach late close records to their iteration after the block ends (682665bff)
- style(repo): format every namespace with the canonical formatter (211f49894)
- fix(gateway): Count folds from turn context records (2afd2a92c)
- refactor(harness): Remove agent() and all sub_loop callers (2bafa1de5)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-alibaba
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-anthropic
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-github-copilot
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-openai-codex
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-opencode-go
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-openrouter
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-standard
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)

#### com.blockether/vis-provider-zai
- style(repo): format every namespace with the canonical formatter (211f49894)
- feat(cli): open live views from a Python extension (d59111718)



### Changed

- feat(live,extensions): `LiveView.close(model_result=…)` can return a model-specific string while preserving the complete final picture and focus snapshots exclusively for the human artifact. GitHub watches now return one compact JSON diagnostic with run metadata, every job’s id/outcome/timing and nested step list, plus one bounded log tail for each failed job; field schemas occur once and the artifact tree is never duplicated into that result.

- fix(live,companion,extensions): finished live artifacts now retain a bounded picture for every row of a focusable table. Reopened GitHub runs can switch jobs locally to inspect each job's final steps and log after the watcher process has exited; these archive-only pictures stay out of the model verdict and live close broadcast.

- refactor(extensions): the GitHub live view no longer renders a run-wide Activity feed that duplicated job states and selected-job steps. The job table remains the run overview, while selecting a row gives that job's steps and log; provider retries remain visible in the run status.

- refactor(extensions): use the shared `vis.testing.LiveRecorder` harness for isolated open/patch/state/focus/close materialization and exact view comparisons. GitHub tests now retain only GitHub polling, fixtures and projections.

- fix(live,companion,extensions): close GitHub watches replaced by newer commits with `superseded`, not `finished`. Queued and running jobs receive the same verdict; running-step indicators, focused-log updates and queued counters stop.

- fix(companion,extensions): highlight every cell in the selected GitHub job row and refresh its steps and log from shared state before the next provider request. Distinguish run-wide activity from selected-job details.

- fix(tui): place finished live runs beside the Python form that opened them. The transcript chevron opens or collapses the read-only record in the live panel.

- fix(extensions): isolate Python extension tests from live user sessions. GitHub watch tests use an in-memory host; the test boundary rejects calls that would publish live views or attach NDJSON records to the conversation.

- fix(extensions): show the GitHub run's start date and time in UTC alongside workflow, branch and focus.

- fix(live,extensions): name live-view records `*.live.ndjson` with media type `application/vnd.vis.live+ndjson`, matching their append-only open/patch/close stream. GitHub watches retain the last valid view during transient polling failures and fail after three consecutive errors. Unknown unfinished states remain running, unavailable job logs are retried, and pull requests without checks finish neutral.
- fix(companion): repeated saved cuts of one settled live run now render as one `RUN` row in the transcript, using the newest cut, instead of one identical row per version.
- fix(extensions): an implicit GitHub watch now stops when a newer commit starts the same workflow on the same branch and event. The live card marks the old run as superseded, links to its replacement, and returns that reason to the model instead of polling obsolete work indefinitely; a caller that explicitly names a historical run still watches exactly that run.
- fix(live,companion): publish live-view close events after attaching the record to its iteration, allowing Companion to refresh the transcript immediately. Records attached after block completion also reach the next model request as filename, attachment ID and `read_attachment(…)` reference.
- fix(shell): support text slicing on shell log results: `status_res.logs()[-4000:]` slices `out` instead of raising KeyError. String-key lookup, JSON, mutation and iteration retain dictionary behavior; other shell result maps remain nonsliceable.
- feat(shell): add shell-log pagination with `next(page)`, `next(page, None)` and `page.pages(max_pages=…)`. Pages preserve the original id, lines and limit; `pages` includes the current page and defaults to ten pages. Negative windows stop at byte zero; forward windows stop at the snapshot's EOF. Later output requires a fresh `sh.logs()`. Results remain dictionaries whose ordinary iteration yields keys.
- feat(live): make GitHub watches interactive live views. Activity shows job and step changes plus newly failed job logs. Focusable job tables use shared `is_focusable` / `focused_ids` state across Python, engine, gateway and Companion. Running jobs are focused by default; user selections update steps and logs, persist across row updates and override stale polling defaults. Companion uses full-width 44px row controls with `aria-pressed`; non-focusable tables remain unchanged.
- fix(live): show focused jobs' current step or runner wait while GitHub job logs remain unavailable. Job and step durations update on every poll, with an explicit log-publication note. When a job finishes, its raw log replaces the progress summary; transition history and failed-job log tails remain unchanged.
- fix(engine): recover from Python contexts whose GIL remains locked after cancellation. After acquiring the turn lock, a bounded detached probe evaluates `None`; an unresponsive context is detached and replaced once per acquisition without blocking on disposal. Busy contexts are still awaited. Recovery may reset sandbox variables but preserves the conversation.
- test(loop): make the sandbox-roots test list its own fixture directory instead of the whole home directory. This prevents native directory reads from exceeding the evaluation timeout and blocking context cleanup; the test file completes in 34 seconds.
- fix(loop): preserve live-view records and already-printed output when a block is cancelled or times out. Engine-thread cleanup uses its own artifact collector, adds the record to the block outcome and includes its verdict in model-visible output.
- refactor(language-surface): remove duplicate directory-grouped `by-cwd` results from `format_code`, `lint_code` and `run_tests`. Retain the flat `findings`, `files` and `failures` lists with full paths; update the contract and both language packs.
- feat(python): add pagination to grep text results: `next(g)`, `next(g, None)`, bounded lazy `pages()` and `all()`. The page preserves the entire original options map and exposes `next_offset`; bounded aggregation reports remaining output. String operations, slicing, printing, `.get('op')` and character iteration remain unchanged.
- refactor(shell): rename merged pty output to `out` / `out_omitted_chars` across run, handles, logs, wait, type and stop. Both stdout and stderr appear in order in this stream. Update shell cards, documentation and the standalone SDK; remove the stdout alias (issue #137).
- fix(tui): render daemon-originated live views in the TUI by projecting gateway open/patch/close events. Preserve engine IDs and sequence numbers, log invalid frames, replay open views on mid-run attachment and ignore duplicate or outdated patches and closes.
- feat(config): support explicit non-secret configuration values such as `VIS_MANAGED: {literal: "true"}` through the shared environment resolver. Managed REPLs, tests, shells and extensions use the same value. Bare scalar declarations and literals under credential-like names remain invalid. Per-call env deltas also accept the literal wrapper.
- fix(python,cli): normalize common attachment, grep and shell keyword variants to canonical names while rejecting duplicate or unknown keys. Preserve normalized grep filters during content search and include the accepted vocabulary in errors. Aliases cover attachment filename/label/source, grep paths/include/context/limit and shell lines/seconds.
- feat(shell): support forward and backward line windows in `sh.logs`: positive counts read after an offset, negative counts read before it. Backward windows never exceed their starting offset; truncated windows report `is_eof` false.
- docs(python): generate attachment shim documentation from `inspect.signature`. Pages use `__vis_calls__` / `__vis_keys__`, consistent required markers, per-key notes and Raw result descriptions; tests compare documented keys with live signatures in both directions.
- fix(python): complete Pillow 10 helper-module APIs, including ExifTags, TiffTags.lookup, palette operations, ImageMath, parser context management, feature tables, ImageShow and GUI adapters. Apply UnsharpMask thresholds, preserve font arguments and variants, expose filter kernels and Color3DLUT.transform, and support image value equality, identity hashing, deepcopy, NumPy array access and format MIME types. Document remaining shim limitations in `doc("pil")`.
- fix(tui): give active TUI live views a stable four-fifths-height viewport. Use raw signed wheel deltas without transcript momentum. Focusable rows are full-width targets marked ○ / ●; selection uses the shared engine or gateway action and ordinary patches. Read-only completed records have no controls.
- fix(tui): allow running TUI live views to collapse to a status row without stopping updates. The title control or status row restores the viewport; Escape restores it before showing the interruption-note field.
- docs(cli): standardize tool documentation and validate language parameters against handlers. Document Python formatting/lint options, Clojure REPL connection options and language-specific test filters. Requiredness uses `:required?`; parameter notes have a six-word limit. Tests enforce note length, requiredness and key order across registered tools.
- fix(tui,companion): show `Vis is showing <view> — live (iter N)` while a live view is open in TUI or Companion. Restore the ordinary turn phase after closure; completed runs appear in the transcript.
- fix(extensions): update GitHub watches with changed jobs, focused steps and failed-job log tails as soon as each job completes. Poll every 3 seconds while jobs run and 8 seconds while queued. Return the final counters, failed job and log tail within the engine's 120-line budget rather than the full update history.
- fix(python): support saving and reopening empty presentations by detecting `ppt/presentation.xml` rather than requiring slide files. Pin imaging 0.1.10, which fixes the incorrect workbook-format error.
- fix(language-surface): accept `project` as `cwd` across language tools and reject conflicting directory values. Document actual requiredness and workspace-root defaults. `repl_connect` is Clojure-only and supports shadow-cljs builds; `run_tests` supports build selection; `repl_start` launches project REPLs and uses `id` as a Python/Bun label rather than a Clojure ID.
- feat(clj-test): append requested deps.edn aliases to clean-JVM test runs, retaining :test and inherited JVM options. Accept aliases with or without a leading colon; reject them for lein/bb projects. Report when aliases cannot apply to reused nREPL or shadow-cljs runs. Document repl_start aliases as additions to :dev and :test.
- fix(human-input): attach live-view records to their opening iteration when the block collector has already closed. Resolve the owning turn directly so an iteration's first artifact can be a live record.
show completed runs as persistent transcript rows with title, verdict, line count and duration. TUI opens read-only records in the live panel; Companion opens them full-screen and fetches bytes on demand. Multiple completed runs remain accessible and expose no stop or input controls.
- fix(python): normalize `os.PathLike` inputs in image-save and plotting shims before selecting file-object or filename behavior. PIL detects file objects by `.write`; plotting chooses the same format for string and Path filenames.
- fix(extensions): remove the arbitrary 90-minute limit and minutes parameter from GitHub watches. Watches end on provider completion, an unrecoverable error or user interruption; the extension documentation defines the same lifecycle rule.
- fix(human-input): suspend a block's evaluation deadline while it has an open live view. Restore the base budget from the time the view closes. Timeout or cancellation closes abandoned views with their current contents, while views deliberately retained between blocks remain open.
- fix(companion): give live-status headlines full width and place detail text below them to avoid word-per-line wrapping on phones.
- fix(languages): standardize REPL startup failures as message, exit and log_tail across languages. Drain child stderr from startup to avoid pipe blocking and lost diagnostics. Report running, pid and cmd only for live interpreters; failed Bun starts no longer register live resources.
- fix(languages): standardize REPL lifecycle behavior: reuse live REPLs, reject changed environments by key, report environment names and digests, and use stopped/not-managed with status down. Centralize comparison and errors in process-jail; wait for the Bun handshake before reporting started.
- refactor(language-surface): replace repl's op string with repl_start, repl_status, repl_stop and repl_connect. Keep bare-ID and language-led stop forms; remove restart in favor of explicit stop and start.
- fix(language-surface): reject bare strings where REPL option maps are required. Honor IDs in status and stop calls, report resources alongside per-directory state and update messages that referenced removed repl APIs or session resource keys.
- feat(shell): support per-call shell and REPL environment deltas over workspace dotenv and configured declarations. Null unsets a variable; source maps supply secrets without recording values in call arguments. Validate names and sources and reject unsafe process-initialization variables. REPL identity includes environment names and digests; changed environments require an explicit restart.
- feat(python): return compact tree text from ls instead of structured rows. Include path and directory/file counts, two-character branches, expanded child counts, compact file sizes and blank-line-separated sections for multiple paths. Listings are rendered directly rather than indexed.
- fix(python): flush a live view's first operation immediately after machine startup. Represent never-flushed state as None rather than 0.0 so monotonic-clock origin does not suppress the first update.
- feat(extensions): add GitHub Actions live views with headline, job progress, counters, job table, focused steps, log tail and links. Poll every 5 seconds, slowing to 15 seconds after five minutes; read logs through redirected CLI output and state when GitHub has not published them. Shared pull-request mapping and Python/Clojure envelope tests verify engine acceptance. Replace the extension documentation sketch with this example.
save closed live views as `application/vnd.vis.live+ndjson` attachments addressed by `vis-live://<session-id>/<view-id>`, using read-only storage and inline bytes only below 256 KiB. Preserve interrupted views through the opening block's collector. TUI reopens completed views read-only; Companion displays RUN artifacts, reads only the ends of records above 1 MB and pages logs from the gateway.
- refactor(gateway): replace the servlet-based gateway adapter with Jetty 12 core via info.sunng/ring-jetty9-adapter. Streaming Ring responses use a direct Handler; dependencies decrease from 22 to 10 jars by 1.66 MB. Remove obsolete Jetty CVE pins and ee9 HttpOutput initialization settings. Disable Jetty's separate shutdown hook so stop! remains responsible for cancelling and draining turns before socket closure.
- feat(gateway): support remote gateway targets through --gateway / --gateway-token or VIS_GATEWAY_URL / VIS_GATEWAY_TOKEN. Remote calls do not discover, spawn or manage a local daemon and do not claim a local PID. Support tokenless SSH-tunnel targets; invalid or unreachable targets fail without local fallback. Each TUI process uses one gateway; status and pair use that target, stop refuses remote targets, and --db does not apply.
- fix(gateway): read string-keyed gateway status correctly instead of reporting a running daemon as stopped.
- feat(clj-repl): attach Clojure REPLs to shadow-cljs builds alongside managed JVM REPLs for the same directory. Validate build existence, watch state, nREPL type and connected JavaScript runtime; preserve the selected build for later evaluations.
- feat(clj-test): run selected ClojureScript tests through the project's shadow-cljs build, using :ns-regexp and optional build selection. Determine results from printed counts because shadow-cljs can exit zero after test failures.
- fix(clj-test): answer a `run_tests` path that is NOT on disk with `no such path` plus the deepest part of it that exists, instead of "no tests under it"
- fix(clj-test): select `*_test.cljc` files — a `.cljc` test namespace was invisible to `run_tests`
- feat(clj-test): `run_tests` runs the clojure namespace a call names — `ns` / `nses` / `namespace` / `namespaces` and `var` / `vars` / `only` select instead of being refused
- fix(extensions): make `vis.state` a whole mapping — `pop`, `setdefault`, `update`, `clear`, keys and iteration
- refactor(editing): remove structural editing — `struct_index`, `struct_nodes` and `struct_patch` are gone; `grep` → `cat` → `patch` is the whole editing surface
- feat(python): cover asyncio queues, locks, futures and timeouts in the sandbox shim
- fix(python): resolve tools passed to asyncio.to_thread or run_in_executor within their gather slots
- build(ci): pause scheduled beta native builds after failures on all native platforms; retain manual dispatch
- fix(build): identify dev builds as a separate track and reject dev as an update target
- feat(companion): open search across machines by pulling down the session list
- fix(companion): move the pull-to-search indicator with the touch gesture
- feat(companion): keep an unsaved comment and let it be edited
- build(ci): build the macOS asset on our own runner, and watch the queue
- build(ci): release JVM-only builds until native platform checks pass
- feat(voice): download pocket-tts with every other model
- fix(copilot): authenticate only the account tier associated with the credential
- fix(tui): remove a provider for good, credential included
- refactor(tui): drop provider Log Out for Remove, and ask inside the transient
- refactor(tui): show provider and MCP questions in the panel that initiated them
- fix(providers): Preserve the default-root marker across machine updates

## [v0.1.40] - 2026-08-16

### Changed
- chore(release): v0.1.40
- fix(build): copy every root classpath path into the native image
- release: update release notes for v0.1.39

### Package changes

#### com.blockether/vis
- chore(release): v0.1.40 (382410b19)
- fix(build): copy every root classpath path into the native image (c1b39d0a1)
- release: update release notes for v0.1.39 (ed3eeb944)

## [v0.1.39] - 2026-08-16

### Changed
- fix(shell): keep the output a pty child printed just before it exited
- fix(shell): stop a pty child inheriting the parent's descriptors
- fix(ci): track the clj-kondo configs imported from dependencies
- chore(release): v0.1.39
- fix(companion): read the artifact overlay after its bytes arrive
- build(ci): build the macOS asset on a hosted runner, never a self-hosted one
- feat(build): let native-image size its own builder heap, and measure it
- fix(companion): put the image viewer's Save button in its header beside Close
- fix(companion): shorten settings headers and close the final group border
- fix(ci): make the hosted macOS fallback fit the free runner
- fix(companion): raise the annotator composer above the keyboard
- feat(build): build the beta track on free hosted runners rather than the macOS builder
- fix(companion): reconnect unreachable machines in the background outside the All view
- feat(build): stamp every native runtime with the commit that built it
- fix(cli): resolve PIL colors by image mode and replace drawn pixels
- fix(sandbox): answer two bands for an 'LA' image, as Pillow does
- docs(companion): record TestFlight build 4319 in the app changelog
- fix(companion): brand the launch screen and name the Android channel
- fix(companion): reopen a session where the reader stopped reading
- fix(sandbox): read a paste mask's alpha band, not its blue channel
- fix(companion): Generate Android launcher and notification icons from the iOS source
- fix(companion): Fetch one session-list page when the list is unchanged
- fix(companion): tag Android alerts so the badge can clear
- fix(sandbox): Allow attachment references in the block that created them
- feat(companion): Set the app badge to the pending notification count
- fix(companion): Put an opened note's Save button in its header
- fix(companion): Indicate operation state with a status dot
- fix(companion): Describe the consequences in destructive-action confirmation
- feat(tui): Replace the speech toggle with voice conversation controls
- fix(companion): keep a project folded once you have folded it
- fix(companion): Remove voice-section borders and the Off route
- docs(docs): plan making every capability an extension of one contract
- refactor(contract): make the Python host an object, not a dict
- fix(companion): Remove the MCP header and move its action to the last list row
- feat(contract): give the host contract its own package
- fix(companion): Color the Providers action amber and unpin the blocked banner
- fix(companion): make single settings actions full-width and align status indicators
- fix(companion): preserve session-list position as machine data arrives
- fix(companion): resume live-session following when the reader reaches the end
- fix(companion): use one notification action and reduce nested-header emphasis
- feat(extensions): gate the Clojure host surface with a contract
- fix(companion): restore the Machines action and reduce emphasis on Providers
- fix(companion): limit the Providers header to its title and a short action label
- refactor(loop): drop the native-tool leftovers from the one-tool wire
- fix(companion): add machine-panel borders and shorten header descriptions
- feat(run-tests): give a test run ten minutes in every pack
- feat(sandbox): give a Python block five minutes before the backstop
- docs(python): write the package README example in the formatter's own shape
- feat(gateway): show sessions awaiting user input in the machine list
- ci(python): verify the Python package on the engine's minimum supported Python version
- fix(python): dispatch the outside shell from the contract's own vocabulary
- feat(gateway): fold every window a fold_session key names
- perf(companion): fill pulled-in history sixteen segments a frame, not two
- docs(companion): record TestFlight build 4280 in the app changelog
- fix(companion): drop the trace a command turn never wrote
- fix(companion): disclose each machine's settings under its own row
- fix(companion): keep a reader's place when the keyboard comes and goes
- fix(tui): Update the table-card test for the relocated display
- fix(shell): Drain exited-child output before reporting completion
- feat(gateway): reclaim the SQLite freelist once a fortnight
- perf(companion): Render only visible session turns
- fix(gateway): drop a retired column SQLite was refusing to drop
- feat(shell): raise the wait budget and cap to thirty minutes
- perf(gateway): Stop persisting values that readers can derive
- feat(python): Distribute the extension API as the vis-agent package
- chore(repo): Remove the fully completed TODO list
- docs(cli): Complete item 1 and record the unscheduled remainder
- test(bench): make the redaction test independent of the caller's environment
- docs(cli): record the TODO state after the doc and ranker work
- chore(repo): reformat every Clojure and Python source in place
- test(ci): Remove timing sensitivity from two tests on loaded runners
- test(ci): Provide missing espeak and pty-log test preconditions
- docs(cli): Document test-result keys
- fix(ci): Fix the test suite on both runners
- feat(companion): give providers the machine row and its slide
- feat(cli): give a helper's docstring a gist, a page and a search hit
- docs(repo): delete banner rules and labels that repeat the code
- docs(voice): trim manifest and JNI notes, drop the finished plan
- perf(companion): make a long live session scroll without stalling
- feat(voice): carry every Piper voice at the highest level published
- docs(docs): show how a tool page renders and how apropos previews it
- docs(cli): give every code verb in the core prompt its call shape
- fix(cli): answer the ask a model types, not the one the page assumes
- fix(persistance): bound the SQLite -wal sidecar with journal_size_limit
- fix(cli): saturate the three ranked fields together, not one by one
- fix(voice): pass sherpa a map compatible with the distributed native binary
- chore(docs): drop the session-id marker from the commit trailer
- fix(companion): rank machines only when multiple machines are available
- fix(companion): clear the app's selection lock when it must switch machines
- docs(voice): say why the progress callback is a deftype
- docs(cli): record what the tool pages answer today
- test(cli): follow the auto-title order to the Alibaba plan
- fix(cli): stamp the call proto for a tool that takes no arguments
- chore(tui): reformat the command suggestion namespace
- chore(cli): reformat three namespaces the formatter had drifted from
- feat(gateway): keep the human's star on the gateway, not the device
- feat(voice): recover from a failed engine without restarting Vis
- feat(cli): teach every tool page its call line and required keys
- perf(companion): stop the hidden sessions list polling the fleet
- fix(cli): sweep every unbounded ~/.vis directory, not just the logs
- feat(companion): swipe the image gallery instead of pressing arrows
- perf(companion): stop a keystroke re-rendering the hidden sessions list
- feat(companion): choose where a reply is spoken, and in which voice
- fix(cli): reclaim the sandbox's sockets, the third door onto a descriptor
- chore(lint): clear every ruff and reflection warning in the tree
- refactor(cli): reclaim every shim's host handles in one runtime registry
- feat(voice): render THIRD_PARTY_MODELS.md from the model manifest
- fix(cli): answer a bare apropos() with the listing's own shape
- chore(commits): minimal conventional commits that name their session
- feat(grep): exclude globs drop files from the content sweep
- A dropped PIL image gives its raster back
- The ASR archive names its direction
- Use descriptive names for voice assets
- Store voices per machine and support uploads from the app
- Fix iOS keyboard-safe companion modals
- Namespace skill slash commands
- Preserve the inherited trace during delayed transcript replacement
- Add three generated pocket-tts reference clips and support user-provided clips
- apropos: answer a row per hit, not a bare first line
- Vendor the MIT pocket-tts ONNX export code instead of cloning it
- Export pocket-tts ourselves and publish it in the assets pack
- Keep message progress visible while its POST request is pending
- A parked run's push asks the question, not which session
- Use Action needed for input-wait notifications and include the question in the body
- Rank documents on a stamped corpus instead of rebuilding it per call
- The session heading keeps its row when the notch pushes it down
- Distribute first-party assets only and use system eSpeak
- Use one line per machine row and put its address action in the swipe drawer
- bm25: extract the ranker and make it fast and shareable
- Reevaluate live-to-persisted transcript replacement on every reconciliation
- Probe provider routers concurrently to load Companion Providers settings
- apropos: rank with BM25F instead of ANDing terms
- Release the voice assets package with Ryan as the first optional voice
- Add local speech with licensed asset metadata, Piper voices and optional pocket-tts
- Expose speech over HTTP with /v1/sessions/:sid/speech, its job stream and features.speech
- voice: one registry for both directions, keyed by transcribe or synthesize
- Primitive math in the balancer's hot loops, and no boxed math left in the tree
- Fetch only this platform's sherpa-onnx native, and plan speech into voice.clj
- balance: seat a repaired delimiter where the replaced text had it
- voice: take upstream sherpa-onnx v1.13.5 and delete the ONNX Runtime pin
- providers: add the Alibaba Coding Plan and Token Plan endpoints
- Record pocket-tts acceptance and the distributed voice list in PLAN.md
- Plan speech output as an engine registry on upstream sherpa-onnx
- vis-agent: run the checkout it sits in when nothing is installed
- format: the delimiter repair is add-only there too
- cli: finish the vis -> vis-agent rename in text that tells you what to run
- Delimiter repair adds only: never delete a closer the edit wrote
- Run what is installed, never the checkout the command sits in
- Re-read a swiped row's offset every frame it claims to be open
- Remove the runtime selector: what is installed is what runs
- Refuse a delimiter repair that moves or retypes what the caller wrote
- Align TUI duration and ID badges to one right edge
- Remove the dev runtime: vis-agent installs and runs under ~/.vis
- Show call duration on every TUI result, including results without cards
- Record Companion TestFlight build 4179 in the app changelog
- Repair an edit's delimiters from the file, never from the fragment
- Select a machine address within its settings row
- Update marker tables after removing the duration marker
- Show tool-call duration in the TUI, as in Companion
- Handle failures in unattended warm-up loads
- Keep the direct push in the magit push transient (issue #144)
- Companion: move model-picker actions into its header and size the sheet to its content
- Fix #145: handle answers promoted from rows without content in Companion
- Fix #145: render completed answers once in TUI and Companion
- Every editor write is atomic, and a refused struct_patch batch is rolled back
- Resolve tool calls before helper return so callers receive values rather than thunks
- The gateway keeps "no AI provider" typed, so the TUI opens the dialog
- release: update release notes for v0.1.38

### Package changes

#### com.blockether/vis
- fix(shell): keep the output a pty child printed just before it exited (c162148a4)
- fix(shell): stop a pty child inheriting the parent's descriptors (da26cafe3)
- fix(ci): track the clj-kondo configs imported from dependencies (568592963)
- chore(release): v0.1.39 (fea04e0c1)
- fix(companion): read the artifact overlay after its bytes arrive (3f3288fdf)
- build(ci): build the macOS asset on a hosted runner, never a self-hosted one (12775d58f)
- feat(build): let native-image size its own builder heap, and measure it (4314130a0)
- fix(companion): put the image viewer's Save button in its header beside Close (6c07f93d4)
- fix(companion): shorten settings headers and close the final group border (7c7eebfad)
- fix(ci): make the hosted macOS fallback fit the free runner (fb7927c9c)
- fix(companion): raise the annotator composer above the keyboard (2141a57e1)
- feat(build): build the beta track on free hosted runners rather than the macOS builder (4430c7404)
- fix(companion): reconnect unreachable machines in the background outside the All view (1273b10b2)
- feat(build): stamp every native runtime with the commit that built it (c2abf4c10)
- fix(cli): resolve PIL colors by image mode and replace drawn pixels (52a215b56)
- fix(sandbox): answer two bands for an 'LA' image, as Pillow does (29fb1d4d1)
- docs(companion): record TestFlight build 4319 in the app changelog (380806099)
- fix(companion): brand the launch screen and name the Android channel (8088f819e)
- fix(companion): reopen a session where the reader stopped reading (b5eb54df3)
- fix(sandbox): read a paste mask's alpha band, not its blue channel (11879470e)
- fix(companion): Generate Android launcher and notification icons from the iOS source (a30ceb8f0)
- fix(companion): Fetch one session-list page when the list is unchanged (1a9f51080)
- fix(companion): tag Android alerts so the badge can clear (fe187409f)
- fix(sandbox): Allow attachment references in the block that created them (ed8777994)
- feat(companion): Set the app badge to the pending notification count (666fa4b03)
- fix(companion): Put an opened note's Save button in its header (cd2ba0705)
- fix(companion): Indicate operation state with a status dot (0ff682bd0)
- fix(companion): Describe the consequences in destructive-action confirmation (0aa7d76d9)
- feat(tui): Replace the speech toggle with voice conversation controls (3338a1d0c)
- fix(companion): keep a project folded once you have folded it (581ed57c9)
- fix(companion): Remove voice-section borders and the Off route (55cb6ff67)
- docs(docs): plan making every capability an extension of one contract (6ac932db4)
- refactor(contract): make the Python host an object, not a dict (137d00650)
- fix(companion): Remove the MCP header and move its action to the last list row (b71ab6bd8)
- feat(contract): give the host contract its own package (4cdcae1a4)
- fix(companion): Color the Providers action amber and unpin the blocked banner (41e8c3a3f)
- fix(companion): make single settings actions full-width and align status indicators (db909334e)
- fix(companion): preserve session-list position as machine data arrives (012df177e)
- fix(companion): resume live-session following when the reader reaches the end (1e79fb42c)
- fix(companion): use one notification action and reduce nested-header emphasis (94024bc92)
- feat(extensions): gate the Clojure host surface with a contract (82a3220ff)
- fix(companion): restore the Machines action and reduce emphasis on Providers (705a52029)
- fix(companion): limit the Providers header to its title and a short action label (6f0a5c0d0)
- refactor(loop): drop the native-tool leftovers from the one-tool wire (4647db9d9)
- fix(companion): add machine-panel borders and shorten header descriptions (4cd359424)
- feat(run-tests): give a test run ten minutes in every pack (f01b6e98a)
- feat(sandbox): give a Python block five minutes before the backstop (04be7c8eb)
- docs(python): write the package README example in the formatter's own shape (2d96a8bd4)
- feat(gateway): show sessions awaiting user input in the machine list (ec56d8347)
- ci(python): verify the Python package on the engine's minimum supported Python version (d85c63f4a)
- fix(python): dispatch the outside shell from the contract's own vocabulary (67d8ffcc8)
- feat(gateway): fold every window a fold_session key names (a9cf4dec3)
- perf(companion): fill pulled-in history sixteen segments a frame, not two (19ecbe5db)
- docs(companion): record TestFlight build 4280 in the app changelog (bad2fb19a)
- fix(companion): drop the trace a command turn never wrote (ce5caf195)
- fix(companion): disclose each machine's settings under its own row (936e3be55)
- fix(companion): keep a reader's place when the keyboard comes and goes (6b8787d45)
- fix(tui): Update the table-card test for the relocated display (dc0cc7852)
- fix(shell): Drain exited-child output before reporting completion (20f56cc4d)
- perf(companion): Render only visible session turns (e5ec15679)
- feat(shell): raise the wait budget and cap to thirty minutes (04225aea2)
- perf(gateway): Stop persisting values that readers can derive (77587a196)
- feat(python): Distribute the extension API as the vis-agent package (93e379fa0)
- chore(repo): Remove the fully completed TODO list (81bc8f91d)
- docs(cli): Complete item 1 and record the unscheduled remainder (155bb8094)
- test(bench): make the redaction test independent of the caller's environment (8b83864e2)
- docs(cli): record the TODO state after the doc and ranker work (27af056d8)
- chore(repo): reformat every Clojure and Python source in place (2770d1cb5)
- test(ci): Remove timing sensitivity from two tests on loaded runners (65b83f977)
- test(ci): Provide missing espeak and pty-log test preconditions (886b34bac)
- docs(cli): Document test-result keys (98cda9046)
- fix(ci): Fix the test suite on both runners (0403732e6)
- feat(companion): give providers the machine row and its slide (21ec0d43a)
- feat(cli): give a helper's docstring a gist, a page and a search hit (851ac835f)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- docs(voice): trim manifest and JNI notes, drop the finished plan (6ff7b063e)
- perf(companion): make a long live session scroll without stalling (e43f74c0d)
- feat(voice): carry every Piper voice at the highest level published (6183aff27)
- docs(docs): show how a tool page renders and how apropos previews it (ebf0441cd)
- docs(cli): give every code verb in the core prompt its call shape (b2a63f670)
- fix(cli): answer the ask a model types, not the one the page assumes (84140ba94)
- fix(cli): saturate the three ranked fields together, not one by one (ae1f3f45a)
- fix(voice): pass sherpa a map compatible with the distributed native binary (24ea302b0)
- chore(docs): drop the session-id marker from the commit trailer (898dc0e4a)
- fix(companion): rank machines only when multiple machines are available (f55919ee5)
- fix(companion): clear the app's selection lock when it must switch machines (7670b6908)
- docs(voice): say why the progress callback is a deftype (297b36c4d)
- docs(cli): record what the tool pages answer today (e39420f10)
- test(cli): follow the auto-title order to the Alibaba plan (191551d7a)
- fix(cli): stamp the call proto for a tool that takes no arguments (4192065fe)
- chore(cli): reformat three namespaces the formatter had drifted from (63cac8ce9)
- feat(gateway): keep the human's star on the gateway, not the device (da626a28c)
- feat(voice): recover from a failed engine without restarting Vis (736fcb865)
- feat(cli): teach every tool page its call line and required keys (4eb49268f)
- perf(companion): stop the hidden sessions list polling the fleet (110112ccc)
- fix(cli): sweep every unbounded ~/.vis directory, not just the logs (4f611f7e5)
- feat(companion): swipe the image gallery instead of pressing arrows (d9956d02a)
- perf(companion): stop a keystroke re-rendering the hidden sessions list (7dfd4e915)
- feat(companion): choose where a reply is spoken, and in which voice (13e72b9a5)
- fix(cli): reclaim the sandbox's sockets, the third door onto a descriptor (ca68b2335)
- chore(lint): clear every ruff and reflection warning in the tree (f5ed3ed5d)
- refactor(cli): reclaim every shim's host handles in one runtime registry (d483ce291)
- feat(voice): render THIRD_PARTY_MODELS.md from the model manifest (12608da39)
- fix(cli): answer a bare apropos() with the listing's own shape (0e63625aa)
- chore(commits): minimal conventional commits that name their session (4841cc984)
- feat(grep): exclude globs drop files from the content sweep (900411403)
- A dropped PIL image gives its raster back (49d20b6cf)
- The ASR archive names its direction (b584a7fd7)
- Use descriptive names for voice assets (b1f159997)
- Store voices per machine and support uploads from the app (ee7adde58)
- Fix iOS keyboard-safe companion modals (46ed794b3)
- Namespace skill slash commands (c947be7be)
- Preserve the inherited trace during delayed transcript replacement (ef344cc60)
- Add three generated pocket-tts reference clips and support user-provided clips (faaad441f)
- apropos: answer a row per hit, not a bare first line (5e3ad668a)
- Vendor the MIT pocket-tts ONNX export code instead of cloning it (9be9db401)
- Export pocket-tts ourselves and publish it in the assets pack (dce9af64b)
- Keep message progress visible while its POST request is pending (6d9db565d)
- A parked run's push asks the question, not which session (e5d640de6)
- Use Action needed for input-wait notifications and include the question in the body (d05986a09)
- Rank documents on a stamped corpus instead of rebuilding it per call (d12f581b7)
- The session heading keeps its row when the notch pushes it down (ba1c32522)
- Distribute first-party assets only and use system eSpeak (2f9e2c1a2)
- Use one line per machine row and put its address action in the swipe drawer (957f37054)
- bm25: extract the ranker and make it fast and shareable (1d36ba9bd)
- Reevaluate live-to-persisted transcript replacement on every reconciliation (bcf95fcb2)
- Probe provider routers concurrently to load Companion Providers settings (7193aa3a9)
- apropos: rank with BM25F instead of ANDing terms (84b53c5d8)
- Release the voice assets package with Ryan as the first optional voice (a939800c5)
- Add local speech with licensed asset metadata, Piper voices and optional pocket-tts (19c5655a8)
- Expose speech over HTTP with /v1/sessions/:sid/speech, its job stream and features.speech (a5864247a)
- voice: one registry for both directions, keyed by transcribe or synthesize (40c9d2464)
- Primitive math in the balancer's hot loops, and no boxed math left in the tree (7c40a20c3)
- Fetch only this platform's sherpa-onnx native, and plan speech into voice.clj (4442a2df0)
- balance: seat a repaired delimiter where the replaced text had it (08142bded)
- voice: take upstream sherpa-onnx v1.13.5 and delete the ONNX Runtime pin (f99eaee39)
- providers: add the Alibaba Coding Plan and Token Plan endpoints (25f540fb7)
- Record pocket-tts acceptance and the distributed voice list in PLAN.md (4e98cbd64)
- Plan speech output as an engine registry on upstream sherpa-onnx (54a0353c8)
- vis-agent: run the checkout it sits in when nothing is installed (d7af0c84b)
- format: the delimiter repair is add-only there too (5edf44475)
- cli: finish the vis -> vis-agent rename in text that tells you what to run (ec7ce2942)
- Delimiter repair adds only: never delete a closer the edit wrote (28636b45c)
- Run what is installed, never the checkout the command sits in (b92a16d43)
- Re-read a swiped row's offset every frame it claims to be open (6bea3d606)
- Remove the runtime selector: what is installed is what runs (cf3ffd751)
- Refuse a delimiter repair that moves or retypes what the caller wrote (53ca5ef96)
- Remove the dev runtime: vis-agent installs and runs under ~/.vis (b7c4b29b3)
- Record Companion TestFlight build 4179 in the app changelog (f324ec589)
- Repair an edit's delimiters from the file, never from the fragment (1c55f10b2)
- Select a machine address within its settings row (258f80c47)
- Handle failures in unattended warm-up loads (3deb6f0df)
- Companion: move model-picker actions into its header and size the sheet to its content (d6e9309f1)
- Fix #145: handle answers promoted from rows without content in Companion (fa2a29c36)
- Fix #145: render completed answers once in TUI and Companion (c70b46431)
- Every editor write is atomic, and a refused struct_patch batch is rolled back (12d2ad319)
- Resolve tool calls before helper return so callers receive values rather than thunks (2be4b0701)
- The gateway keeps "no AI provider" typed, so the TUI opens the dialog (eefc71c0a)
- release: update release notes for v0.1.38 (6b2f66de5)

#### com.blockether/vis-channel-tui
- feat(tui): Replace the speech toggle with voice conversation controls (3338a1d0c)
- feat(gateway): show sessions awaiting user input in the machine list (ec56d8347)
- fix(tui): Update the table-card test for the relocated display (dc0cc7852)
- perf(gateway): Stop persisting values that readers can derive (77587a196)
- test(ci): Remove timing sensitivity from two tests on loaded runners (65b83f977)
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- chore(tui): reformat the command suggestion namespace (10296499f)
- Namespace skill slash commands (c947be7be)
- Primitive math in the balancer's hot loops, and no boxed math left in the tree (7c40a20c3)
- cli: finish the vis -> vis-agent rename in text that tells you what to run (ec7ce2942)
- Align TUI duration and ID badges to one right edge (7a8be54e8)
- Show call duration on every TUI result, including results without cards (7ac776bec)
- Update marker tables after removing the duration marker (f4636de51)
- Show tool-call duration in the TUI, as in Companion (4c98c7479)
- Keep the direct push in the magit push transient (issue #144) (ec6eaaa70)
- Fix #145: render completed answers once in TUI and Companion (c70b46431)
- The gateway keeps "no AI provider" typed, so the TUI opens the dialog (eefc71c0a)

#### com.blockether/vis-foundation-bridge
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- feat(cli): teach every tool page its call line and required keys (4eb49268f)

#### com.blockether/vis-foundation-search
- refactor(loop): drop the native-tool leftovers from the one-tool wire (4647db9d9)
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- feat(cli): teach every tool page its call line and required keys (4eb49268f)

#### com.blockether/vis-foundation-voice
- feat(tui): Replace the speech toggle with voice conversation controls (3338a1d0c)
- test(ci): Provide missing espeak and pty-log test preconditions (886b34bac)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- docs(voice): trim manifest and JNI notes, drop the finished plan (6ff7b063e)
- feat(voice): carry every Piper voice at the highest level published (6183aff27)
- fix(voice): pass sherpa a map compatible with the distributed native binary (24ea302b0)
- feat(voice): recover from a failed engine without restarting Vis (736fcb865)
- chore(lint): clear every ruff and reflection warning in the tree (f5ed3ed5d)
- feat(voice): render THIRD_PARTY_MODELS.md from the model manifest (12608da39)
- The ASR archive names its direction (b584a7fd7)
- Use descriptive names for voice assets (b1f159997)
- Add three generated pocket-tts reference clips and support user-provided clips (faaad441f)
- Vendor the MIT pocket-tts ONNX export code instead of cloning it (9be9db401)
- Export pocket-tts ourselves and publish it in the assets pack (dce9af64b)
- Distribute first-party assets only and use system eSpeak (2f9e2c1a2)
- Release the voice assets package with Ryan as the first optional voice (a939800c5)
- Add local speech with licensed asset metadata, Piper voices and optional pocket-tts (19c5655a8)
- voice: one registry for both directions, keyed by transcribe or synthesize (40c9d2464)
- Fetch only this platform's sherpa-onnx native, and plan speech into voice.clj (4442a2df0)
- voice: take upstream sherpa-onnx v1.13.5 and delete the ONNX Runtime pin (f99eaee39)

#### com.blockether/vis-language-clojure
- refactor(loop): drop the native-tool leftovers from the one-tool wire (4647db9d9)
- feat(run-tests): give a test run ten minutes in every pack (f01b6e98a)
- feat(shell): raise the wait budget and cap to thirty minutes (04225aea2)
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- balance: seat a repaired delimiter where the replaced text had it (08142bded)
- format: the delimiter repair is add-only there too (5edf44475)
- Delimiter repair adds only: never delete a closer the edit wrote (28636b45c)
- Refuse a delimiter repair that moves or retypes what the caller wrote (53ca5ef96)
- Repair an edit's delimiters from the file, never from the fragment (1c55f10b2)

#### com.blockether/vis-language-python
- feat(run-tests): give a test run ten minutes in every pack (f01b6e98a)
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)

#### com.blockether/vis-language-typescript-bun
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)

#### com.blockether/vis-persistance-sqlite
- fix(sandbox): Allow attachment references in the block that created them (ed8777994)
- feat(gateway): reclaim the SQLite freelist once a fortnight (9e2d5c230)
- fix(gateway): drop a retired column SQLite was refusing to drop (2c4e59ee1)
- perf(gateway): Stop persisting values that readers can derive (77587a196)
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- fix(persistance): bound the SQLite -wal sidecar with journal_size_limit (e0aa37591)
- feat(gateway): keep the human's star on the gateway, not the device (da626a28c)
- Primitive math in the balancer's hot loops, and no boxed math left in the tree (7c40a20c3)
- cli: finish the vis -> vis-agent rename in text that tells you what to run (ec7ce2942)

#### com.blockether/vis-provider-alibaba
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- providers: add the Alibaba Coding Plan and Token Plan endpoints (25f540fb7)

#### com.blockether/vis-provider-anthropic
- fix(ci): Fix the test suite on both runners (0403732e6)
- Primitive math in the balancer's hot loops, and no boxed math left in the tree (7c40a20c3)

#### com.blockether/vis-provider-github-copilot
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- Primitive math in the balancer's hot loops, and no boxed math left in the tree (7c40a20c3)

#### com.blockether/vis-provider-openai-codex
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)
- Primitive math in the balancer's hot loops, and no boxed math left in the tree (7c40a20c3)
- cli: finish the vis -> vis-agent rename in text that tells you what to run (ec7ce2942)

#### com.blockether/vis-provider-opencode-go
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)

#### com.blockether/vis-provider-openrouter
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)

#### com.blockether/vis-provider-standard
- fix(ci): Fix the test suite on both runners (0403732e6)

#### com.blockether/vis-provider-zai
- fix(ci): Fix the test suite on both runners (0403732e6)
- docs(repo): delete banner rules and labels that repeat the code (39eeb1718)



### Added
- Credit every model the built-in voice service can install in `THIRD_PARTY_MODELS.md`, generated from the manifest
- Select reply speech on this device or the responding machine and choose a voice. Fall back to device speech when remote speech is unavailable.
- Show listening and speech capabilities in machine settings, including model download progress, selected engine, missing-component errors and retry controls.
- Add vis-agent speech say and speech transcribe commands for local synthesis and transcription checks.
- Add LJ (en-US), an automatically downloaded voice trained from scratch on public-domain recordings.
- Mark sessions waiting for input as INPUT NEEDED and sort them first. Highlight their TUI tabs and rows in amber and send a phone notification.
- Add TUI voice conversation mode with C-x b for the current tab. Speak new replies and automatically send dictated input; activating the microphone stops playback.
- Count pending notifications across machines in the app badge. Reading a session removes its alerts from that count.
- Record the source commit in native runtimes so vis-agent runtime can identify build age and source differences without executing the binary.
- Add the beta update track with rolling builds from passing main commits. --track stable returns to releases; updates never switch tracks implicitly.

### Changed
- No release depends on a machine of ours being awake: the macOS ARM64 asset builds on GitHub's free hosted Apple-silicon runner, no workflow names a self-hosted label any more, and the builder shrinks the build to fit that machine instead of asking for a heap it does not have
- Commits are `type(scope): summary` with a body of at most six WHY lines and a mandatory `Vis-Session: <uuid>` trailer
- Move /v1/voice/model and /v1/speech/model from session scope to machine scope.
- Use each Piper speaker's highest published quality model, including Cori high. Quality levels are separately trained models, not runtime settings.
- Make reply speech a per-conversation client mode rather than a machine-wide feature toggle. The machine synthesizes speech when requested.

### Fixed
- Keep document comment composers above the keyboard in all clients by using the keyboard-resized app layout. Indicate selected passages with background color only; remove the duplicate side marker and quotation.
- Replace Capacitor launch screens with the Vis logo on the app background. Use iOS system launch-screen configuration rather than a storyboard to avoid image-size failures on large screens.
- Create an Android Answers notification channel at launch rather than using Firebase's Miscellaneous fallback, allowing users to configure answer alerts separately.
- Use mask alpha, grayscale or bitmap values in sandbox Pillow paste operations rather than the blue channel. Reject unsupported mask modes and support LA image fills.
- Regenerate Android launcher images from the iOS source to match its colors and alpha, removing the double-applied transparency.
- Use a monochrome Vis notification icon with teal tint on Android instead of Firebase's fallback bell.
- Tag Android notifications by session so reading a session removes its delivered alerts and clears the launcher dot after the last alert.
- Reduce Companion typing and scrolling work: hidden session lists do not rerender or poll, reading position persists once per gesture and streaming updates rerender only the active step.
- Render only visible session turns and preserve measured heights for skipped content. A 24-turn scrolling test improves from 104 ms to 17 ms per frame without shifting content.
- Preserve the exact historical reading position when tapping the transcript dismisses the keyboard.
- Start history rendering at sixteen segments and triple the batch while frames remain within budget. A 20,000-node history loads with syntax highlighting in 1.8 seconds rather than 6.7 seconds.
- Resume following when readers reach the end they targeted, even if a live turn grows during scrolling. Preserve earlier rendered steps so readers in history are not displaced.
- Preserve the top visible session row while machine responses and history pages arrive independently.
- Retry failed speech-engine loads on the next use and allow model-download retries. Failed recordings no longer block later recordings.
- Register sherpa JNI callback and generation-option classes in native images so installed binaries can synthesize and transcribe speech.
- Render /reload and shell-command turns as their responses rather than as Python source with a result card.
- A project you fold stays folded: the sessions list opens the project at the top of a machine and leaves every other one as you last left it, through opening a session, coming back, and relaunching the app — a search still shows what it matched, fold or no fold
- Place destructive-action explanations and confirmation choices inside one error-styled container for provider, machine and session removal.
- Move document Save beside Close in the header and report the saved revision there. Remove the lower save strip, adding 53 px of document space.
- Poll only the first session-list page and retain existing rows when ordering is unchanged. For 1,192 sessions, polling decreases from twelve requests to one every ten seconds without rebuilding filters, sorting or scroll position.
- Save reading position on session exit and restore it after rendering enough history. Ignore opening-time scroll events; if the saved position is unavailable, open the newest turn and resume following.
- Represent LA images as two channels consistently in pixels, bytes, split and histograms. Reject invalid merge band counts and preserve grayscale when adding alpha.
- Resolve sandbox Pillow colors according to image mode, initialize transparent images correctly, replace pixels when drawing with alpha and pack monochrome bitmap bytes. Verify behavior against Pillow 12.1.1 in 216 cases.

## [v0.1.38] - 2026-08-14

### Changed
- Release v0.1.38
- Close row action drawers immediately instead of animating them
- Pin svar 0.7.117 to support GLM-5.3 routing and GLM reasoning levels
- Restore a session's helpers through the rewrite that defined them
- List addresses with actions in each row's swipe drawer
- Unregister this device when forgetting a machine
- Record Companion TestFlight build 4159 in the app changelog
- The slide is back on both lists, and it stops closing the row it just opened
- Make notifications machine-specific and ensure Disconnect completes
- Cross-validate the peer plan against the runtime and fix what it cited wrong
- Record Companion TestFlight build 4154 in the app changelog
- Document fold_session(key, gist) in the core prompt
- Restore the plan for communication between peer sessions
- Stop patch growing a blank line the caller never asked for
- fold_session takes a string range grammar
- Show row actions directly and use a single-line Machines header
- TUI: write a plan's name once in the limits summary
- Pin svar 0.7.116 so a declined retry states its reason
- Native release: name the precondition the macOS runner override needs
- OpenCode Go: report live 5h/7d/30d quotas from /zen/go/v1/usage
- Make patch take one file's whole batch of anchored edits
- Show machine actions as text in the associated settings row
- Carry svar's stream-finalization evidence into the fatal provider log
- The composer height test declares its observer field instead of a constructor parameter property
- Recalculate composer height when its width changes, not only on typing
- The write that records a turn's outcome survives the payload it was given
- release: update release notes for v0.1.37

### Package changes

#### com.blockether/vis
- Release v0.1.38 (e686fb2d3)
- Close row action drawers immediately instead of animating them (e56eafbee)
- Pin svar 0.7.117 to support GLM-5.3 routing and GLM reasoning levels (11ebcf482)
- Restore a session's helpers through the rewrite that defined them (92ca7349b)
- List addresses with actions in each row's swipe drawer (0c8997098)
- Unregister this device when forgetting a machine (c4a37c91c)
- Record Companion TestFlight build 4159 in the app changelog (91668ea90)
- The slide is back on both lists, and it stops closing the row it just opened (09ad53d9c)
- Make notifications machine-specific and ensure Disconnect completes (c5487390d)
- Cross-validate the peer plan against the runtime and fix what it cited wrong (b3130f92a)
- Record Companion TestFlight build 4154 in the app changelog (4c32f0ea6)
- Document fold_session(key, gist) in the core prompt (eefd08947)
- Restore the plan for communication between peer sessions (a80a5a898)
- Stop patch growing a blank line the caller never asked for (40c9ec904)
- fold_session takes a string range grammar (57d09f967)
- Show row actions directly and use a single-line Machines header (5a718178b)
- TUI: write a plan's name once in the limits summary (6be874415)
- Pin svar 0.7.116 so a declined retry states its reason (125d0e4d0)
- Native release: name the precondition the macOS runner override needs (28aca9751)
- Make patch take one file's whole batch of anchored edits (22b36784f)
- Show machine actions as text in the associated settings row (02aae91a3)
- Carry svar's stream-finalization evidence into the fatal provider log (b4a7ae7b6)
- The composer height test declares its observer field instead of a constructor parameter property (7c43ff0c7)
- Recalculate composer height when its width changes, not only on typing (ca6a5aa81)
- The write that records a turn's outcome survives the payload it was given (b824b706d)
- release: update release notes for v0.1.37 (92c7b024b)

#### com.blockether/vis-channel-tui
- TUI: write a plan's name once in the limits summary (6be874415)

#### com.blockether/vis-language-clojure
- Make patch take one file's whole batch of anchored edits (22b36784f)

#### com.blockether/vis-persistance-sqlite
- The write that records a turn's outcome survives the payload it was given (b824b706d)

#### com.blockether/vis-provider-opencode-go
- OpenCode Go: report live 5h/7d/30d quotas from /zen/go/v1/usage (93fe630af)



### Changed
- Show list-row actions at the trailing edge without swipe drawers, overflow menus or a separate action strip.
- Limit the Machines header to its title and Add a machine action, removing repeated addresses and instructions.
- `fold_session(key, gist)` takes a key and a gist and nothing else: the key is a string — `"t2/i5"` one step, `"t2"` a whole turn, `"t2/i1-i9"` a range, `"-t2/i9"` everything through it, `"t2/i5-"` everything since it, commas union several

### Fixed
- Require explicit Connect before registering notifications for a newly paired machine. Unanswered switches no longer count as consent.
- Revoke notification registrations identified by relay grants even when no OS token is available. A failed revocation does not prevent revoking the other registration.
- Forgetting a machine takes this device off it — the machine kept the device row and went on notifying, and the forget itself was what made that permanent, since a machine that is no longer paired is never swept again; the revocation it is owed is now kept and retried until that machine accepts it

## [v0.1.37] - 2026-08-14

### Changed
- Release v0.1.37
- Label the notification action rather than repeating the current state
- Complete only the matching turn, preserve its rendered answer and refetch a short transcript

### Package changes

#### com.blockether/vis
- Release v0.1.37 (fbca3dd3a)
- Label the notification action rather than repeating the current state (22f5e2b7a)
- Complete only the matching turn, preserve its rendered answer and refetch a short transcript (b20892310)



### Added
- The C-x g status buffer lists the project, nested Git repositories and read-write repositories declared in workspace.filesystem. Each repository has a branch/count header and independent actions under the cursor. Clean repositories collapse to their header. Display count is unlimited; bounded discovery reports scan truncated when incomplete.
- `jail.environment` selects `declared` or `inherit` for child environments. The default includes project dotenv values, environment declarations and a non-secret basic allowlist. Inherit includes ambient values, including secrets, without changing other confinement. Variables that could interfere with jail initialization remain rejected.
- Add Trim to the Companion image viewer. Crop the visible region at original resolution with annotations flattened into it; drawing, Copy, Share and Use edit then use the crop. Undo trim restores the full image.

### Changed
- Replace the notification token list with a device-to-machine connection row and switch. Show Checking, Connected or Not connected consistently for APNs, FCM and Web Push. OS-disabled permissions are not reported as connected; iOS offers a system Settings link.
- Copy session IDs as `vis_session_id#<uuid>` from TUI and Companion. `read_session` and `get_session` accept this marked form directly; introspection instructions describe it.
- Use one Companion microphone control: tap to activate the current mode and hold for 450 ms to switch between dictation and voice conversation. Right-click and Shift+Enter also switch modes. Remove separate mode and exit controls. Exiting releases audio, recordings, speech, queued utterances and leases. Entering conversation mode enables it; the next tap starts recording. Icons and accessible names identify the mode and gesture.
- Set the JVM maximum heap to `-Xmx5g` instead of 75% of host RAM. This gives concurrent GC and memory-pressure thresholds a consistent limit across machines rather than allowing a roughly 27 GiB maximum on a 48 GB host.
- Allow comma-separated or repeated Android --track values, defaulting to internal,alpha,beta. Assign the build to all selected tracks in one transactional Play edit. Production remains explicit; reject multi-track staged rollouts before building.
- Distribute iOS releases to internal TestFlight groups and the public group after Beta App Review by default. --audience internal restricts distribution to the team. Validate the audience before archiving. Product release tags target tester channels in both stores rather than Android beta alone.
- Discover tester channels from the stores with --track all and --audience all. Play updates all discovered non-production tracks in one edit and validates names before building. TestFlight assigns external groups and internal groups that need explicit assignment. Both GitHub workflows use all.
- Use paths to select tests in every language. Clojure maps test files, source files and directories to test namespaces. Remove ns, namespace and namespaces selectors and explicitly reject the old options rather than running the full suite.
- Select individual tests with `<path>::<test-name>`. Keep each name paired with its file; Clojure maps source names to test namespaces and accepts pathless var names. Remove Clojure only and Bun filter options. Python forwards node IDs to pytest and uses -k for pathless names; the whole-file GraalPy runner rejects individual-test selection and recommends the project environment.
- Make language packs return pass, fail, errored, command and is_pass directly. Remove shared result-name translation so each pack applies its runner's count semantics. Python call options use environment; configuration uses python.runner. Remove runner/interpreter call aliases.
- Rename session_state to read_session, sessions to list_sessions and session_fold to fold_session, without aliases. Add get_session for one descriptor without a transcript. Replace list_sessions' channel filter with search, using the same title/request/reply/thinking ranking as TUI and Companion. Results include rank, match flags and request/reply snippets.
- Android voice capture now opens a connected Bluetooth headset's HFP/SCO microphone before
  WebView starts recording, then restores the normal audio route when recording ends.
- Voice-conversation playback on Android now uses the system text-to-speech engine when WebView
  does not expose the Web Speech API.
- Voice conversation requests the spoken projection for an idle session as well as a queued one;
  previously the usual idle path returned only the full on-screen answer.
- Prepare `vis-foundation-bridge` for Bridge 0.3: migrate the project profile and policy to the
  reduced YAML schema, recognize every 0.3 profile filename, preserve exact validation paths, and
  keep malformed Bridge configuration from crashing ordinary filesystem access.
- Allow branch-dispatched Native Release dry runs to build the image, stage the bundle and run smoke/native tests without publishing. Only v* refs publish artifacts.
- Move macOS arm64 native releases to GitHub-hosted macos-26 and remove the self-hosted runner. Use a 14 GiB maximum / 2 GiB initial heap with ParallelGC and swap for the 3-core, 7 GiB runner; log resource measurements. Keep VIS_MACOS_ARM64_RUNNER as an optional hosted-runner override. Cache dependencies and smoke-test the gateway; manual release builds remain available on Macs with at least 32 GB RAM.
- Remove shell stderr and stderr_omitted_chars fields and the STDERR card section (issue #137). Pty commands and the internal blocking runner merge stdout and stderr into one output stream.
- The agent prompt and the shell docstrings spell the keystroke method `sh.type("y")`, never a
  bare `sh.type()` (issue #137): it SENDS text and its argument is required, so the old spelling
  among the status accessors raised a `TypeError` for anyone who followed it.
- The process-jail doc page is `jail.md` ("Process jail & egress"), not `sandbox.md`: in this
  repo *sandbox* now names only the in-process GraalPy sandbox, and *jail* names OS confinement —
  the same split the config keys and `session["access"]` already use. Every in-tree link moved
  with it.
- Require one options map for grep and struct_nodes, also accepting equivalent keyword arguments. Remove positional forms and report the canonical call shape when rejected.
- Expand /<name> into a skill reference, task, project and resource paths rather than the full skill text. The model reads doc(name) if the instructions are not already present. Repeated skill expansion is stateless.
- Load workspace .env and .env.local by default for shells, managed REPLs, tests and Python extensions. Resolve environment declarations before dotenv and startup environment values. Use declarations for renames, keychain items, helper commands and explicit ambient access.
- Include project dotenv values in confined child environments while keeping ambient operator variables denied by default. Reject unsafe process-initialization variables from dotenv and declarations alike.

- Use jail for confinement terminology. Session access reports is_jailed; workspace copies report isolated instead of sandbox. Reserve sandbox for the Python execution environment.

### Removed
- Remove top-level sandbox and filesystem configuration aliases. Require jail.enabled and jail.filesystem and reject obsolete keys during configuration loading.
- Remove jail.env. Declare variables in environment instead, using source maps such as `CI: {env: CI}`. Confined children still omit undeclared ambient variables and reject unsafe process-initialization names.
- `extensions.env-passthrough`, a third list of the same names: `extensions` was never a valid
  top-level config key, so the block was rejected before anything could read it.
- Remove the skill operation and activation state. Use apropos to discover skills and doc(name) to read their full instructions. Prompt discovery still includes each skill's name, description and project.
- The `git` tool. There is no model-facing Git schema, no `git` binding in the sandbox and no
  `foundation-git` extension: a Git command is an ordinary `shell` command, run by the same
  jail, capture and timeout as everything else. Workspace Git FACTS (footer status, environment
  block, file picker) and the TUI Magit surface are unchanged.
- Disable automatic outbox capture and remove VIS_OUTBOX from sandbox environments. Temporary files remain writable but are no longer collected as artifacts; use attach explicitly. Retain tested capture support behind mpl-capture/incidental-capture-enabled?.

### Fixed
- Accept complete cat, grep and struct_index anchor rows by stripping the display separator and text before parsing the hash. Previously, including the line text caused valid printed anchors to fail. Replacement text containing a separator remains literal.
- Honor supported requests, httpx and urllib3 TLS options instead of silently discarding them (issue #141). Preserve ssl, select and selectors imports; only asyncio still uses import rewriting. Emit the standard warning for unverified requests.
- Handle string, bytes and PathLike certificate paths consistently, respect configured CA-bundle environment variables unless trust_env is disabled, and preserve SSL/configuration error types. Apply TLS version bounds, expose create_urllib3_context and explicitly reject unsupported options rather than ignoring them.
- Ignore default/fallback provider and model selections in committed project vis.yml/vis.yaml, with a warning directing users to personal configuration. Retain those settings in global config, machine state and gitignored project overlays (issue #140).
- Initialize Lanterna TTYDeviceControl at native runtime rather than build time. This avoids unusable foreign-function handles and startup crashes in v0.1.33–v0.1.35; use registered termios support or Lanterna's stty fallback.
- Add :image-nses to extension manifests for namespaces resolved by name. Compile them into native images without eagerly loading them during discovery. Add reachability tests to detect omitted TUI, SQLite and voice namespaces.
- Share Android JDK discovery between release preflight and build through scripts/jdk.mjs. Require stock JDK 21, including SDKMAN installations, and reject GraalVM because its jlink is incompatible with AGP JdkImageTransform.

- Test the pty bridge with a real pseudo-terminal running cat. Cover replay, live output and typed-input echo rather than only a listener/send stub.

- Include missing built-in extension namespaces in native build initialization and test the list against runtime discovery. This fixes startup failures caused by missing introspection and shim classes.

- Restore Linux native releases by omitting Lanterna FFM TTY downcall registration, avoiding the TUI smoke-test SIGSEGV seen in v0.1.33–v0.1.35. Linux uses Lanterna's stty fallback, as through v0.1.32; macOS retains native calls.

- Report the push relay before Web Push in capabilities so automatic VAPID configuration does not hide the native provider. Route browser devices through Web Push rather than reporting an unsupported platform.
- Store a Var rather than a function value in the provider router-rebuild hook so namespace reloads use the current implementation.
- Keep pasta diagnostics out of jailed command output by using --quiet and a process-specific log file under ~/.vis/logs.

- Preserve JVM interruption across best-effort catch blocks in process, Git, gateway, credential, jail and MCP operations. cancellation/preserve-interrupt! restores the flag after InterruptedException but not CancellationException. The MCP listener exits on interruption.
- The prompt's Clojure `run_tests` note said the opposite of what the runner does. It claimed the
  managed REPL does "NOT reload namespaces automatically" and told a session to reload every
  changed *test* namespace, while the runner already `(require … :reload)`s (or `load-file`s)
  every namespace it RUNS — and never their dependencies. Proven at runtime: a poisoned Var in a
  test namespace came back restored from a run, the poisoned Var in the production namespace that
  test requires did not. The prompt line, `run-form`'s docstring and the repo guidance now name
  the real trap: a changed PRODUCTION namespace keeps the Vars the reused REPL already holds.
- Restore the interrupt flag when shell usage sampling is interrupted, returning no measurement. Cancelled sh.wait calls now stop rather than continuing to their deadline.
- Remove the obsolete tool_name lookup, SessionArtifact.tool field and related screen-reader caption branch from the artifact gallery. Retain turn and iteration provenance.
- Make capfd capture actual file descriptors in the sandbox pytest shim (issue #138), including os.write, C-level and child-process output. Drain redirected pipes during the test, include captured bytes in CaptureResult and replay unread output on failure. capsys remains stream-only.
- Correct documented language and structural-tool calls to match runtime contracts: explicit language options, paths versus path, edit batches, Python True and one-options-map calls. Add a corpus test for every doc/apropos document.
- Include failed Python test identities and messages in structured results. The project runner reads pytest JUnit XML; the GraalPy runner converts per-test records. Truncate output in the middle to retain the session header, failure details and summary, reporting the omitted character count (issue #136).
- Accept PIL images directly in attach, alongside paths, bytes and matplotlib figures. Use JPEG for .jpg filenames with compatible mode conversion and lossless PNG otherwise. Reject unsupported source types explicitly rather than interpreting their repr as a missing path.

## [v0.1.35] - 2026-08-09

### Changed
- Release v0.1.35
- A wait is ONE budget for the whole batch, not one per command
- Drop README library packaging note
- Trim README install and runtime prose
- Simplify README install section
- Name the one shell tool `shell`
- Document raw results for apropos groups and sandbox operations
- Make `wait` the only difference between a run and a background shell
- Serve the whole session's artifacts from a metadata index
- PLAN: name Phase 5 by its commit
- Make every run a handle: a timeout is a wait that expired
- Stop forcing deferred work at namespace load, which native-image runs on the builder
- PLAN: name Phase 4 by its commit
- Give background shells log files and byte-offset cursors
- Paginate the displayed project list rather than the gateway's result window
- Delete the project-wide rename
- Apply :fs/access checks to struct_rename
- release: update release notes for v0.1.34
- Record what a host map actually looks like in the sandbox

### Package changes

#### com.blockether/vis
- Release v0.1.35 (165edb397)
- A wait is ONE budget for the whole batch, not one per command (cdcfe21e8)
- Drop README library packaging note (9bdbc1cf7)
- Trim README install and runtime prose (66527db92)
- Simplify README install section (0d29554ca)
- Name the one shell tool `shell` (25089f16f)
- Document raw results for apropos groups and sandbox operations (07cf88d36)
- Make `wait` the only difference between a run and a background shell (634c0476c)
- Serve the whole session's artifacts from a metadata index (cac2e80a3)
- PLAN: name Phase 5 by its commit (4f9983d23)
- Make every run a handle: a timeout is a wait that expired (451a644a2)
- Stop forcing deferred work at namespace load, which native-image runs on the builder (8d0eae493)
- PLAN: name Phase 4 by its commit (1be019874)
- Give background shells log files and byte-offset cursors (e98cc607e)
- Paginate the displayed project list rather than the gateway's result window (a264ef547)
- Delete the project-wide rename (71f00d8c9)
- Apply :fs/access checks to struct_rename (a6557f244)
- release: update release notes for v0.1.34 (6469254b2)
- Record what a host map actually looks like in the sandbox (1afa5d007)

#### com.blockether/vis-channel-tui
- Name the one shell tool `shell` (25089f16f)
- Make `wait` the only difference between a run and a background shell (634c0476c)

#### com.blockether/vis-persistance-sqlite
- Serve the whole session's artifacts from a metadata index (cac2e80a3)

#### com.blockether/vis-provider-opencode-go
- Stop forcing deferred work at namespace load, which native-image runs on the builder (8d0eae493)



## [v0.1.34] - 2026-08-09

### Changed
- Release v0.1.34
- Exclude toolchain output from incidental attachment capture
- Stop capturing the temp file nobody named
- PLAN: record Phase 3 as done
- Replace the `ls` native tool with a sandbox `ls()` helper
- Use a deterministic provider fixture in native tests
- Companion: calculate whether Latest is needed from the current scroll position
- Verify native binaries with native tests, not only Docker builds
- Companion: show artifact notes on their tiles and keep controls legible
- Companion: one Settings dialog, this device beside the machines
- Record Phase 2 in the plan
- Replace protected paths with one :fs/access gate hook
- Companion: search is a page, and the bar keeps two marks
- Prove the native TUI, the agent entrypoint and the zai provider in the image build
- Make the container image a base a deployment extends
- Group documents by step and open them from their rows
- Pin the container agent's home to the vis user and prove it at build time
- Install a provisioning profile only where Xcode reads it
- Serve the container gateway from the native image
- Keep the transcript fixed within the keyboard-resized app layout
- Run attached-page scripts in isolation from the app origin
- Use direct operation names for sandbox attachment APIs
- Label close controls with the element they close
- Move Companion rendering styles into the relevant components
- Use the shared close control in the image viewer
- Use page text colors for close icons
- Scale image zoom by scroll distance and support Safari pinch gestures
- release: update release notes for v0.1.33

### Package changes

#### com.blockether/vis
- Release v0.1.34 (7e2d6cefa)
- Exclude toolchain output from incidental attachment capture (e8c512bae)
- Stop capturing the temp file nobody named (9386b5144)
- PLAN: record Phase 3 as done (e3240898f)
- Replace the `ls` native tool with a sandbox `ls()` helper (d3db4c514)
- Use a deterministic provider fixture in native tests (5701c9d7e)
- Companion: calculate whether Latest is needed from the current scroll position (871420423)
- Verify native binaries with native tests, not only Docker builds (72ef3fec8)
- Companion: show artifact notes on their tiles and keep controls legible (7c6fae0b9)
- Companion: one Settings dialog, this device beside the machines (df428cafe)
- Record Phase 2 in the plan (287bcda62)
- Replace protected paths with one :fs/access gate hook (49d5a182e)
- Companion: search is a page, and the bar keeps two marks (8b6bea53b)
- Prove the native TUI, the agent entrypoint and the zai provider in the image build (72bd2b6e4)
- Make the container image a base a deployment extends (2f36d3e44)
- Group documents by step and open them from their rows (c173d5421)
- Pin the container agent's home to the vis user and prove it at build time (b2656c39a)
- Install a provisioning profile only where Xcode reads it (59c889f7b)
- Serve the container gateway from the native image (b067471d4)
- Keep the transcript fixed within the keyboard-resized app layout (eb3303a52)
- Run attached-page scripts in isolation from the app origin (5c0bcda60)
- Use direct operation names for sandbox attachment APIs (3913d59c9)
- Label close controls with the element they close (b243f9cb7)
- Move Companion rendering styles into the relevant components (a4eaf0e27)
- Use the shared close control in the image viewer (00b058dd8)
- Use page text colors for close icons (80af604cf)
- Scale image zoom by scroll distance and support Safari pinch gestures (b245817ce)
- release: update release notes for v0.1.33 (9d9bf9855)

#### com.blockether/vis-channel-tui
- Use direct operation names for sandbox attachment APIs (3913d59c9)

#### com.blockether/vis-foundation-bridge
- Replace protected paths with one :fs/access gate hook (49d5a182e)

#### com.blockether/vis-persistance-sqlite
- Use direct operation names for sandbox attachment APIs (3913d59c9)



## [v0.1.33] - 2026-08-08

### Changed
- Release v0.1.33
- Put the search field and every transcript card header on one height
- Align machine-strip side borders with the page
- Page grep results with offset and next_offset
- Let a session row fill its swipe track
- Align Companion search with the app bar's trailing edge
- Default grep to 50 elements, filename fallback included
- Use the artifact image frame for sent images and a gallery for multiple images
- Stretch row-end icon buttons at pointer density as well as touch density
- Put search back on the app bar with its own magnifying glass
- Give mobile search a separate row and place Clear at the field edge
- Render MetaButton children so composer labels remain visible
- Name button variants by hierarchy and use one composer control row
- Give the companion one chip, one row, one disclosure and one remove
- Show a saved artifact revision without refetching the transcript
- Use the shared header and button in opened documents
- Remove the copy, move, delete, create_directory and file_exists tools
- Start a stroke from beside the picture, not only on its edge
- Release the companion app locally when this machine can sign
- Enforce :ext/protected-paths in the Python sandbox filesystem
- Put creation labels inside their buttons and shorten row confirmation prompts
- Show document artifacts once as cards that open an overlay viewer
- Color the Star action yellow and keep starred rows visible
- Let the app stop the turn it started again
- Exclude TestFlight builds from internal beta groups
- Give a note ten annotation threads and a comment on the whole document
- Push from workflows through one shared git-push action
- Mark annotations in theme colours and annotate plain text too
- Use a square machine switcher, highlight unread state and hide it for a single machine
- Draw a comment ordinal as a plain coloured number
- Accept any spelling of a path in vis_attach
- Clear the NEW badge on the row you just read
- Use one segmented machine switcher
- Put the session star immediately right of the title
- Number and colour markdown comments, and underline the passage each is about
- Remove the machine card below the machine switcher
- Test the Companion artifact-revision save URL against its route
- Read a note inline as source and comment on it by tapping
- Use page text colors for Add machine and increase machine-strip contrast
- Put the machine tabs and Add machine on one control height
- Give the phone a full-bleed sessions card with a fixed height
- Link every TestFlight build to every beta group
- Label the machine-strip pairing action Add machine
- Pair machines from the tab strip's plus button
- Cancel the active stroke when a pinch gesture begins
- End the machine card where its content ends
- Use one metadata text color in the session list and enclose the machine card
- Let the viewer zoom out to 50%
- Write PDF annotations with the maintained pdf-lib fork
- Support comments on notes and drawing on PDFs and images in artifact viewers
- Place machine tabs outside the machine card and remove All scope
- Give a document preview an Open chip that fills the screen
- Remove the bin/vis-agent launcher tests
- Keep a Python extension loaded between tests that ask for the same one
- Restore the transcript copy chip's own look
- Use the shared button styling for app-bar search
- Use the shared button for transcript copying and one preview container
- Open every artifact at full height
- Render markdown artifacts and let a human comment on them
- Make search the app bar and move pairing into preferences
- Make Return type a new line on phones and dismiss the keyboard on send
- Collapse vis_attach_bytes into vis_attach
- Show document artifacts plainly: no draw, hide or new tab
- Stop shelling out to real git in the tests
- Use visible button backgrounds for app-bar and machine actions
- Label Companion actions and move machine controls into the header
- Pair from the app bar and use one machine strip regardless of machine count
- Make pairing a chip and drop the tab bar
- Key shared test sandboxes to isolate suites with incompatible state
- Remove the ACP extension
- Show the machine as a chip instead of a second header
- Require UI proposals as ASCII sketches, not app-built galleries
- Remove model, reasoning, verbosity, drafts, magit palette verbs and /export-html
- Keep a renamed machine name across dev reloads
- Close the gap between a session row and its disclosure
- Cut the paramiko and sandbox-fd test walls
- Rename a machine on its own header and add a project in one tap
- Route overlay card justification through the shared run justifier
- End the phone status on the timestamp edge, with the flags against it
- Reduce unnecessary waits in the slowest gateway and loop tests
- Pin svar 0.7.109 for the 2-minute retrying TTFT watchdog
- Use plus and gear icons for the two machine-header actions
- Cap the session pager to a centred cluster
- Sit the phone row flags beside the status they qualify
- Align header names on one glyph column and always print the tally noun
- Give the first token two minutes instead of five
- Give session-row flags their own column
- Keep pagination controls in fixed positions
- Page the session list with numbered, jumpable pages
- Add a sharded parallel test runner
- Always offer the draft half and delete the Offer drafts setting
- Wall the test suite off from the public internet
- Make Manage projects the same anchored panel as the draft picker
- Frame transcript media as a plate with a docked filename label
- Move the Draft action into the project header as a split button
- Show a human-input pause only on its own session's tab
- Draw the draft mark as a forked project folder
- Add hard no-profanity rule to AGENTS.md
- Give the Draft action a folder icon
- Remove profanity from source comments and test reports
- Make the drafts setting a named choice on the shared settings cell
- Stop provider tests dialing the network and trim eval-timeout walls
- Stop the Offer drafts toggle sliding sideways
- Pad the trailing inside edge of a list row
- Stretch a row-ending icon button to its row height
- Open every dialog in the one Modal + DialogFrame
- Pin the manage-projects path bar to one fixed height
- Keep the project browser still when the pencil is taken
- Make every dialog a full-height phone sheet and one desktop box
- Refuse re-adding a folder that is already a project
- Open project browsing one level above the current project
- Dock the Manage projects footer and align its trash to the row edge
- Import UI source through Vite raw imports in the sheet test
- Make every dialog a bottom sheet on phones
- Align companion trailing controls and drop the glyph hover frame
- Keep inline images visible when a transient band opens
- Shrink compact header buttons to a 32px face with a 44px touch target
- Use the terminal background for transient panels
- Load the TUI provider dialog from one gateway call
- Enclose the transient panel and hint bar on a slightly darker background
- Render TUI diff fences compact instead of side-by-side
- Add a transient-panel border and remove tinted backgrounds and column dividers
- Pin svar 0.7.107 for uncompressed SSE streaming
- Never title a session after a pasted image's clipboard path
- Order provider limit windows shortest-first (5h before 7d)
- Keep a TUI tab's reading position across a workspace switch
- Remember where a session transcript was being read
- Never animate auto-follow scrolling in the TUI
- Drop tool name from pending-summary docstring example
- Drop unused form display exports and the dead auto-repaired flag
- Clear stale tab layouts on switch to prevent automatic scrolling
- Correct label-overrides docstring after the shell/fs tool split
- Drop same-path coalescing and the running-code-tools exception
- Apply TUI scroll position immediately after terminal resize
- Purge legacy fs/shell tool names from form and loop tests
- Split shell and filesystem tools into named operations
- Print a TUI notice's sentence without its machine code
- Unify the transient band into one embeddable component
- Use one positioning function for main-screen transient panels
- Adopt svar 0.7.106 so a declared retry cooldown is waited out
- Place the transient title on its top divider and move the palette into Tools
- Serve a live turn's text-named images from the gateway
- Position session transients above the prompt with separate backgrounds
- Report total line counts in a collapsed multi-file patch headline
- Give a form one text column by moving the focus ring out of it
- Report added/removed/modified line counts on every edit summary
- Stop blaming the provider for a turn that never reached one
- Replace unresponsive session engines instead of queueing behind them
- Give every companion list header one band
- Associate every attachment row with its turn, including tool artifacts
- Give every attachment descriptor its turn id
- Respond to cancellation even when the tab shows no active turn
- Give every header one trailing cluster and one kebab
- Share one overflow button and one header action cluster in the companion
- Unify companion overflow menus, icon buttons, and dialog closes
- Return attachment descriptors and drop the answer gallery
- Search the zipper tree with one parse instead of re-parsing per node
- Put form inputs on separate rows and remove toggle indentation
- Bump svar to 0.7.105
- Decode Python provider maps from declared field tables
- Give every C-x category its own column
- Open one blank row under every human-input label
- Lay a tall transient band out in which-key columns
- Implement C-x as a transient keyboard-command panel
- Show follow-up questions within the initiating panel
- Give every managed nREPL its own log file
- Scope the tid-less cancel to the turn its caller submitted
- Accept clean Rift drafts without pending changes
- Name the project that owns a nested skill
- Remove native tool color roles
- Revert recent folding changes
- Respect disabled Bridge toggle during commits
- Require canonical gateway client for all API calls
- Resolve nested skills from their owning projects
- Fix live session turn iteration counts
- Expose Impeccable to repository sessions
- Keep skill slash commands root-only
- Make nested slash commands project-relative
- Fix wait card command rendering
- Clear unread badge after live answer settles
- Simplify session list borders
- Remove retry_native replay verb
- Auto-connect companion dev gateways
- Compact project rows around new session
- Center compact new session buttons
- Keep new session button compact
- Canonicalize workspace roots at persistence
- Show companion diffs in one unified column
- Keep project roots canonical
- Put project borders on the clickable header
- Fix HTTP migration namespace compilation
- Give every project header both borders
- Use babashka HTTP client and preserve requested services
- Extract Web Push protocol interop helpers
- Refactor gateway Web Push interop
- Render filesystem copy results as expandable cards
- Fix sticky machine border seams
- Keep Magit responsive during commit verification
- Keep browser push gateway-local
- Keep machine banner borders single
- Finish pending companion and engine work

### Package changes

#### com.blockether/vis
- Release v0.1.33 (2ebb36887)
- Put the search field and every transcript card header on one height (f8df6596e)
- Align machine-strip side borders with the page (f17a0baa9)
- Page grep results with offset and next_offset (be31c6165)
- Let a session row fill its swipe track (94f996d60)
- Align Companion search with the app bar's trailing edge (3102648f0)
- Default grep to 50 elements, filename fallback included (d6b9d2253)
- Use the artifact image frame for sent images and a gallery for multiple images (116c59834)
- Stretch row-end icon buttons at pointer density as well as touch density (c85b77e9a)
- Put search back on the app bar with its own magnifying glass (ba9296498)
- Give mobile search a separate row and place Clear at the field edge (85d9d3b4c)
- Render MetaButton children so composer labels remain visible (3bae6b37d)
- Name button variants by hierarchy and use one composer control row (bb59694f1)
- Give the companion one chip, one row, one disclosure and one remove (0d3274b0b)
- Show a saved artifact revision without refetching the transcript (94b26fdc5)
- Use the shared header and button in opened documents (b430dea50)
- Remove the copy, move, delete, create_directory and file_exists tools (87c1562ea)
- Start a stroke from beside the picture, not only on its edge (686ea2827)
- Release the companion app locally when this machine can sign (7ad4c1123)
- Enforce :ext/protected-paths in the Python sandbox filesystem (b11aa1706)
- Put creation labels inside their buttons and shorten row confirmation prompts (2d4b8e7ec)
- Show document artifacts once as cards that open an overlay viewer (65cf41550)
- Color the Star action yellow and keep starred rows visible (8ead5cd33)
- Let the app stop the turn it started again (421734b0e)
- Exclude TestFlight builds from internal beta groups (6cef66c7c)
- Give a note ten annotation threads and a comment on the whole document (8b832e827)
- Push from workflows through one shared git-push action (5f4e33ac9)
- Mark annotations in theme colours and annotate plain text too (7c3ba0413)
- Use a square machine switcher, highlight unread state and hide it for a single machine (305cd6789)
- Draw a comment ordinal as a plain coloured number (53d371558)
- Accept any spelling of a path in vis_attach (d383fed53)
- Clear the NEW badge on the row you just read (aea33924a)
- Use one segmented machine switcher (050cb9b70)
- Put the session star immediately right of the title (c8b6263e2)
- Number and colour markdown comments, and underline the passage each is about (1167b97b4)
- Remove the machine card below the machine switcher (7d1bfb53f)
- Test the Companion artifact-revision save URL against its route (637600b9b)
- Read a note inline as source and comment on it by tapping (1a706229c)
- Use page text colors for Add machine and increase machine-strip contrast (e0918c09d)
- Put the machine tabs and Add machine on one control height (02700e1ed)
- Give the phone a full-bleed sessions card with a fixed height (8e771c3e9)
- Link every TestFlight build to every beta group (9142379af)
- Label the machine-strip pairing action Add machine (41715e01e)
- Pair machines from the tab strip's plus button (ec71c1c2a)
- Cancel the active stroke when a pinch gesture begins (ebcc90c27)
- End the machine card where its content ends (e41345a66)
- Use one metadata text color in the session list and enclose the machine card (2e312c16f)
- Let the viewer zoom out to 50% (f93e02b32)
- Write PDF annotations with the maintained pdf-lib fork (1a0d386d5)
- Support comments on notes and drawing on PDFs and images in artifact viewers (7fde3d069)
- Place machine tabs outside the machine card and remove All scope (eec70eb58)
- Give a document preview an Open chip that fills the screen (76164492e)
- Remove the bin/vis-agent launcher tests (b3f98438a)
- Keep a Python extension loaded between tests that ask for the same one (efb5d97d2)
- Restore the transcript copy chip's own look (82383147c)
- Use the shared button styling for app-bar search (3ed7d5270)
- Use the shared button for transcript copying and one preview container (6586e491d)
- Open every artifact at full height (a4e500782)
- Render markdown artifacts and let a human comment on them (c4a1ea54f)
- Make search the app bar and move pairing into preferences (7308e9c00)
- Make Return type a new line on phones and dismiss the keyboard on send (da74a5be7)
- Collapse vis_attach_bytes into vis_attach (1afa4df70)
- Show document artifacts plainly: no draw, hide or new tab (774cfb6a8)
- Stop shelling out to real git in the tests (e0d8aa5ca)
- Use visible button backgrounds for app-bar and machine actions (b498ea4e9)
- Label Companion actions and move machine controls into the header (cc7f57260)
- Pair from the app bar and use one machine strip regardless of machine count (50cadeda4)
- Make pairing a chip and drop the tab bar (975a0d4b7)
- Key shared test sandboxes to isolate suites with incompatible state (545cad9e5)
- Remove the ACP extension (8e6e9e413)
- Show the machine as a chip instead of a second header (6e9e6ef28)
- Require UI proposals as ASCII sketches, not app-built galleries (afac2f6ae)
- Remove model, reasoning, verbosity, drafts, magit palette verbs and /export-html (0d255f454)
- Keep a renamed machine name across dev reloads (13aafb0b7)
- Close the gap between a session row and its disclosure (bb611b512)
- Cut the paramiko and sandbox-fd test walls (682e4206e)
- Rename a machine on its own header and add a project in one tap (3295bc072)
- End the phone status on the timestamp edge, with the flags against it (0687d2486)
- Reduce unnecessary waits in the slowest gateway and loop tests (375fce532)
- Pin svar 0.7.109 for the 2-minute retrying TTFT watchdog (4de9b8291)
- Use plus and gear icons for the two machine-header actions (e1b46d338)
- Cap the session pager to a centred cluster (cbeb3e224)
- Sit the phone row flags beside the status they qualify (52012ca27)
- Align header names on one glyph column and always print the tally noun (f3f29d16a)
- Give the first token two minutes instead of five (75fae850f)
- Give session-row flags their own column (620af0f38)
- Keep pagination controls in fixed positions (0b92b77b3)
- Page the session list with numbered, jumpable pages (6db2a1982)
- Add a sharded parallel test runner (293ea7e19)
- Always offer the draft half and delete the Offer drafts setting (0561f5fd6)
- Wall the test suite off from the public internet (1275bf716)
- Make Manage projects the same anchored panel as the draft picker (3f617032b)
- Frame transcript media as a plate with a docked filename label (1bfa6855e)
- Move the Draft action into the project header as a split button (07ed1075f)
- Draw the draft mark as a forked project folder (030af03e3)
- Add hard no-profanity rule to AGENTS.md (d0045b87f)
- Give the Draft action a folder icon (13529ded0)
- Remove profanity from source comments and test reports (d56c65216)
- Make the drafts setting a named choice on the shared settings cell (c0d100981)
- Stop provider tests dialing the network and trim eval-timeout walls (5326ced75)
- Stop the Offer drafts toggle sliding sideways (c844743eb)
- Pad the trailing inside edge of a list row (520e11f29)
- Stretch a row-ending icon button to its row height (98a999030)
- Open every dialog in the one Modal + DialogFrame (ae7c9a2c6)
- Pin the manage-projects path bar to one fixed height (309299e32)
- Keep the project browser still when the pencil is taken (e222d043e)
- Make every dialog a full-height phone sheet and one desktop box (479d482a7)
- Refuse re-adding a folder that is already a project (c0eacae97)
- Open project browsing one level above the current project (353b0ac01)
- Dock the Manage projects footer and align its trash to the row edge (80d33e9ad)
- Import UI source through Vite raw imports in the sheet test (1e2bd6cda)
- Make every dialog a bottom sheet on phones (27606096f)
- Align companion trailing controls and drop the glyph hover frame (e9d60a060)
- Shrink compact header buttons to a 32px face with a 44px touch target (3b0fdbaf1)
- Load the TUI provider dialog from one gateway call (c181c0896)
- Pin svar 0.7.107 for uncompressed SSE streaming (2c114ac36)
- Never title a session after a pasted image's clipboard path (3ccc7ea62)
- Order provider limit windows shortest-first (5h before 7d) (e50e51b0b)
- Remember where a session transcript was being read (949000137)
- Drop tool name from pending-summary docstring example (75e3858ef)
- Drop unused form display exports and the dead auto-repaired flag (75bfd91ba)
- Correct label-overrides docstring after the shell/fs tool split (020ed396f)
- Drop same-path coalescing and the running-code-tools exception (6611bbb1b)
- Purge legacy fs/shell tool names from form and loop tests (3e16113bf)
- Split shell and filesystem tools into named operations (5846c9dc9)
- Adopt svar 0.7.106 so a declared retry cooldown is waited out (492c93ff8)
- Serve a live turn's text-named images from the gateway (31970dec1)
- Report total line counts in a collapsed multi-file patch headline (a357ce607)
- Report added/removed/modified line counts on every edit summary (1fb5fe24f)
- Stop blaming the provider for a turn that never reached one (0b8194c57)
- Replace unresponsive session engines instead of queueing behind them (3e813c15a)
- Give every companion list header one band (bc5516194)
- Give every attachment descriptor its turn id (eec04cf37)
- Give every header one trailing cluster and one kebab (2661521fc)
- Share one overflow button and one header action cluster in the companion (795346602)
- Unify companion overflow menus, icon buttons, and dialog closes (c4963e1d2)
- Return attachment descriptors and drop the answer gallery (2d29bf3e9)
- Search the zipper tree with one parse instead of re-parsing per node (54e508e2c)
- Bump svar to 0.7.105 (f0e35f757)
- Decode Python provider maps from declared field tables (7ae92e716)
- Scope the tid-less cancel to the turn its caller submitted (9daf44093)
- Accept clean Rift drafts without pending changes (8de1c5504)
- Name the project that owns a nested skill (5d399ef38)
- Remove native tool color roles (3f92b2cbd)
- Revert recent folding changes (57f722736)
- Require canonical gateway client for all API calls (645235f83)
- Resolve nested skills from their owning projects (df1592e7d)
- Fix live session turn iteration counts (e99f19e8e)
- Expose Impeccable to repository sessions (6dba3f4ba)
- Keep skill slash commands root-only (d1809bdb6)
- Make nested slash commands project-relative (2385a429f)
- Fix wait card command rendering (26e96a2e4)
- Clear unread badge after live answer settles (886e10997)
- Simplify session list borders (06dd224e1)
- Remove retry_native replay verb (81097fbdc)
- Auto-connect companion dev gateways (4d6e6a1a2)
- Compact project rows around new session (3028852d4)
- Center compact new session buttons (51cbff21b)
- Keep new session button compact (2213321b3)
- Canonicalize workspace roots at persistence (e181774b8)
- Show companion diffs in one unified column (a79d29b58)
- Keep project roots canonical (4317177dc)
- Put project borders on the clickable header (455d4135f)
- Fix HTTP migration namespace compilation (ce754530a)
- Give every project header both borders (faaa463bc)
- Use babashka HTTP client and preserve requested services (a4dc97066)
- Extract Web Push protocol interop helpers (e22d2ae18)
- Refactor gateway Web Push interop (5983d042a)
- Render filesystem copy results as expandable cards (c8367cb76)
- Fix sticky machine border seams (2ec8f9a96)
- Keep browser push gateway-local (4dedb0cc5)
- Keep machine banner borders single (3a0377cdd)
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-channel-tui
- Remove model, reasoning, verbosity, drafts, magit palette verbs and /export-html (0d255f454)
- Route overlay card justification through the shared run justifier (86adb7aaa)
- Wall the test suite off from the public internet (1275bf716)
- Show a human-input pause only on its own session's tab (e1ff3aa14)
- Remove profanity from source comments and test reports (d56c65216)
- Keep inline images visible when a transient band opens (2ab21ff16)
- Use the terminal background for transient panels (f670c026d)
- Load the TUI provider dialog from one gateway call (c181c0896)
- Enclose the transient panel and hint bar on a slightly darker background (d639c94cd)
- Render TUI diff fences compact instead of side-by-side (cf36e81c8)
- Add a transient-panel border and remove tinted backgrounds and column dividers (12f68cc6d)
- Order provider limit windows shortest-first (5h before 7d) (e50e51b0b)
- Keep a TUI tab's reading position across a workspace switch (c896ed12d)
- Never animate auto-follow scrolling in the TUI (6b2a8111e)
- Clear stale tab layouts on switch to prevent automatic scrolling (8d2b238ce)
- Drop same-path coalescing and the running-code-tools exception (6611bbb1b)
- Apply TUI scroll position immediately after terminal resize (c6dd2fe6f)
- Print a TUI notice's sentence without its machine code (e7146ae58)
- Unify the transient band into one embeddable component (3fc733b99)
- Use one positioning function for main-screen transient panels (2ff6b8f0d)
- Place the transient title on its top divider and move the palette into Tools (4be2df9f7)
- Position session transients above the prompt with separate backgrounds (b9a9db2d2)
- Give a form one text column by moving the focus ring out of it (699faae0d)
- Respond to cancellation even when the tab shows no active turn (8e5f97569)
- Return attachment descriptors and drop the answer gallery (2d29bf3e9)
- Put form inputs on separate rows and remove toggle indentation (bfd1c544e)
- Decode Python provider maps from declared field tables (7ae92e716)
- Give every C-x category its own column (151340f56)
- Open one blank row under every human-input label (9bc3de016)
- Lay a tall transient band out in which-key columns (1d67bfabb)
- Implement C-x as a transient keyboard-command panel (f20d4c7aa)
- Show follow-up questions within the initiating panel (5414f777b)
- Scope the tid-less cancel to the turn its caller submitted (9daf44093)
- Remove native tool color roles (3f92b2cbd)
- Keep Magit responsive during commit verification (d0531b830)
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-foundation-bridge
- Respect disabled Bridge toggle during commits (e2907fb8d)
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-foundation-search
- Remove native tool color roles (3f92b2cbd)
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-foundation-voice
- Fix HTTP migration namespace compilation (ce754530a)
- Use babashka HTTP client and preserve requested services (a4dc97066)
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-language-clojure
- Give every managed nREPL its own log file (f0c3c358a)
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-language-python
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-language-typescript-bun
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-persistance-sqlite
- Render markdown artifacts and let a human comment on them (c4a1ea54f)
- Collapse vis_attach_bytes into vis_attach (1afa4df70)
- Reduce unnecessary waits in the slowest gateway and loop tests (375fce532)
- Associate every attachment row with its turn, including tool artifacts (ab7a722da)
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-provider-anthropic
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-provider-github-copilot
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-provider-openai-codex
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-provider-opencode-go
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-provider-openrouter
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-provider-standard
- Finish pending companion and engine work (ab1db24db)

#### com.blockether/vis-provider-zai
- Finish pending companion and engine work (ab1db24db)



## [v0.1.32] - 2026-08-06

### Changed
- Release v0.1.32
- Drop stale workspace backend re-exports from core
- Allow per-model provider API styles
- Match machine-list header typography
- Improve machine-list headers and attachment controls
- Use the shared Button component for doc-artifact toolbar controls
- companion: let a pinch start while a stroke is in progress
- Combine the artifact sheet title and filter rows
- Drop oversized py-2 override from image-viewer/annotation buttons
- Raise ArtifactsSheet z-index above transcript copy buttons
- Bridge extension: throw on missing :workspace/root instead of falling back to user.dir
- companion: resume last session on cold hashless relaunch
- Drop bossy "ATTACH ONE OR TWO PER TURN" framing from attach shim's prompt-facing description
- Print every shim's supported surface in the system prompt
- Write every shim description as an editable multi-line str
- Keep document updates under one name
- Version artifacts under a shared name
- Hardcode workspace backend to Rift, remove pluggable backend registry
- Right-align machine overflow menus and match project-header typography
- Add vis-agent gateway mcp CLI commands
- Replace ⋯ glyph with a proper DotsIcon in the icon set
- Remove the machine-list overflow menu; retain one per machine header
- Rename release-companion skill to release-companion-hotfix
- Remove the /clear slash command
- Fix iOS keyboard glitch when tapping a slash command
- Group machine actions in one overflow menu and allow session project selection
- Suspend the zoom-viewer snap transition during a live pinch/pan
- Fit the composer text to its own line box
- Increase small-text sizing intervals and remove fixed line height
- release: update release notes for v0.1.31
- Draw the design board with the app's own list components

### Package changes

#### com.blockether/vis
- Release v0.1.32 (6d4bf9101)
- Drop stale workspace backend re-exports from core (5609ac9f2)
- Allow per-model provider API styles (efb7b129d)
- Match machine-list header typography (1cf300753)
- Improve machine-list headers and attachment controls (93d4aff0b)
- Use the shared Button component for doc-artifact toolbar controls (c17473390)
- companion: let a pinch start while a stroke is in progress (5ed890df4)
- Combine the artifact sheet title and filter rows (48fde2874)
- Drop oversized py-2 override from image-viewer/annotation buttons (9b344dc58)
- Raise ArtifactsSheet z-index above transcript copy buttons (9abaf9a00)
- companion: resume last session on cold hashless relaunch (ee72aad57)
- Drop bossy "ATTACH ONE OR TWO PER TURN" framing from attach shim's prompt-facing description (344923028)
- Print every shim's supported surface in the system prompt (da99aadd0)
- Write every shim description as an editable multi-line str (64fcd44c1)
- Keep document updates under one name (06424ae2f)
- Version artifacts under a shared name (4e5ba78ab)
- Hardcode workspace backend to Rift, remove pluggable backend registry (a84314623)
- Right-align machine overflow menus and match project-header typography (c269eefa3)
- Add vis-agent gateway mcp CLI commands (74f26e03c)
- Replace ⋯ glyph with a proper DotsIcon in the icon set (52280c55e)
- Remove the machine-list overflow menu; retain one per machine header (32f70e99b)
- Rename release-companion skill to release-companion-hotfix (386459ad6)
- Remove the /clear slash command (398d28da8)
- Fix iOS keyboard glitch when tapping a slash command (23cbf2f1b)
- Group machine actions in one overflow menu and allow session project selection (62daee7ea)
- Suspend the zoom-viewer snap transition during a live pinch/pan (577388d1f)
- Fit the composer text to its own line box (c877137e0)
- Increase small-text sizing intervals and remove fixed line height (5f45e7f0b)
- release: update release notes for v0.1.31 (507ca8dc1)
- Draw the design board with the app's own list components (0f423eb9b)

#### com.blockether/vis-foundation-bridge
- Bridge extension: throw on missing :workspace/root instead of falling back to user.dir (cb4e63b8e)

#### com.blockether/vis-persistance-sqlite
- Version artifacts under a shared name (4e5ba78ab)



## [v0.1.31] - 2026-08-05

### Changed
- Release v0.1.31
- Prove format_code/lint_code's invoke-symbol-wrapper respects draft cwd
- Prove native-handler workspace-root fix covers repl/repl_connect/repl_eval
- Accept fold-record anchor syntax in session_fold
- Use app controls and layout in the design proposal
- Bind workspace context for native handler-tool dispatch
- Add opencode-go provider with per-model wire routing
- Render the path edit pencil without a button background
- Photograph the chosen session flow: machine menu, path pencil, one switch
- Refuse an unparseable session_fold scope id instead of acking a fold of nothing
- Show per-file +/- line counts in the /draft apply report
- Keep an icon's size when a caller only styles it
- Fix non-deterministic model picker title in the add-provider band
- Show draft workspace name in session header status row
- Check home for dotfiles where the filesystem root has none
- Refresh the audit snapshot for imaging 0.1.9
- Refuse a misplaced shell options map by name
- Spec the TUI transient and compute its layout once
- Compose transient panels through embed-transient!
- Revert "Rename bar identifiers to baz"
- feat(tui): answer every /draft slash with the draft band
- fix(gateway,hitl): one sid spelling in the registry, lock-free hydrate, OTP as a secret
- Serve ls of an unindexable directory from fff itself
- Split the draft band into create, switch and abandon commands
- Merge the draft transient's Create and Danger groups into Actions
- Implement draft controls as a Magit transient panel
- Make environment refresh! a host-only /reload hook
- Make environment refresh host-only, off the Python sandbox
- Convey caller thread bindings into the structural scan pool
- Enforce the provider auth cooldown vis already logged
- Fall back to a filesystem walk when ls cannot use fff
- Track agent skills under .agents/
- Rename bar identifiers to baz
- Remove /draft blank
- Replay every parked human-input request on attach
- Surface daemon-side human input in the terminal
- Run bang turns in the session workspace root
- Separate the artifact section from the dark controls above it
- Find search hits the session list has not paged in, and date the rows
- Draw every companion icon at one optical size
- Stream empty-reply resends live and name why each provider call exists
- Pretty-print nested status_fn maps in provider status text
- Draw every control mark in the companion as a real icon
- Size the artifacts chip to the session id beside it and give it a paperclip
- Make the favorite star icon truly yellow
- Keep the caller's session inside a bounded provider probe
- Make failed-turn error cards selectable in the TUI transcript
- Match the artifact-sheet header to dialog-header height
- Keep Python provider limits rows valid
- Open text artifacts and give the artifacts sheet canonical chrome
- Name the provider and model a stalled turn died on
- Bound provider probes off the UI thread
- Pin the strings-only Python boundary across the whole tool surface
- Refuse human input requests without a session
- Cross the Python boundary string-clean from every environment symbol
- Cache companion artifacts on the device and page a gallery by size
- List attachments without their bytes and filter history like the byte endpoint
- Move the desktop header tabs to the right, beside the cog
- Split extension jail policy modes
- Automate iOS crash collection
- Anchor the desktop header tabs to the left edge
- Serve attachment bytes from the list the descriptors number
- Treat a blank rg include glob as no filter
- Release iOS keyboard before backgrounding
- Add one artifact index for all session output
- Keep trusted extension shell outside the jail
- Guard collapsed tool result memory
- Unify filesystem tool input schema
- Allow trusted extensions to spawn subprocesses
- Extract the annotation stack into reusable single-purpose modules
- Give the TUI error card a margin row and bottom padding
- Make the artifacts gallery proposals operable and touch-sized
- Paint a failed turn as a card in the TUI
- Stop advertising strict tools on every wire
- Log the tool-call transport wreckage the door drops
- Advertise :strict only when supported by the request API format
- Refuse a tool-call arguments payload that is not an object
- Decode drifted escapes in one linear allocation-free pass
- Propose three Artifacts gallery designs for the session header
- Drop leaked tool-call close tags from tool arguments
- Describe a failed turn in its own terminal event
- Decode drifted \uXXXX escapes only into visible assigned characters
- Let the app annotate a PDF page or an HTML artifact and attach it
- fix(gateway/bus): mirror sibling events under the sid the process registered
- Decode drifted unicode escapes in patch and struct_patch text
- Name the voice job SSE stream so clients cannot confuse it with the session log
- Cross-validate every anydoc example against the engine

### Package changes

#### com.blockether/vis
- Release v0.1.31 (91f6db754)
- Prove format_code/lint_code's invoke-symbol-wrapper respects draft cwd (80a31e269)
- Prove native-handler workspace-root fix covers repl/repl_connect/repl_eval (3c7620aec)
- Accept fold-record anchor syntax in session_fold (52d4e4c3a)
- Use app controls and layout in the design proposal (d3187b86a)
- Bind workspace context for native handler-tool dispatch (6afd4bf25)
- Add opencode-go provider with per-model wire routing (a6dc50277)
- Render the path edit pencil without a button background (9a13e8a00)
- Photograph the chosen session flow: machine menu, path pencil, one switch (3c94a6df4)
- Refuse an unparseable session_fold scope id instead of acking a fold of nothing (6cc908129)
- Show per-file +/- line counts in the /draft apply report (3695a2a1b)
- Keep an icon's size when a caller only styles it (a16f467c8)
- Show draft workspace name in session header status row (d1eaf593d)
- Check home for dotfiles where the filesystem root has none (0be3bca04)
- Refresh the audit snapshot for imaging 0.1.9 (80df2e0ea)
- Refuse a misplaced shell options map by name (92e01d772)
- Revert "Rename bar identifiers to baz" (8105572f7)
- fix(gateway,hitl): one sid spelling in the registry, lock-free hydrate, OTP as a secret (fd152579d)
- Serve ls of an unindexable directory from fff itself (23d49aaa7)
- Make environment refresh! a host-only /reload hook (c2cc94560)
- Make environment refresh host-only, off the Python sandbox (cea42698a)
- Convey caller thread bindings into the structural scan pool (e0459543e)
- Enforce the provider auth cooldown vis already logged (b1c1c50bc)
- Fall back to a filesystem walk when ls cannot use fff (16b25bccd)
- Track agent skills under .agents/ (6ace873ab)
- Rename bar identifiers to baz (d921da75a)
- Remove /draft blank (45f5ff1fa)
- Surface daemon-side human input in the terminal (537da3648)
- Run bang turns in the session workspace root (4b4989131)
- Separate the artifact section from the dark controls above it (14a251b7f)
- Find search hits the session list has not paged in, and date the rows (f79fba147)
- Draw every companion icon at one optical size (770e51cda)
- Stream empty-reply resends live and name why each provider call exists (8569d0fed)
- Pretty-print nested status_fn maps in provider status text (27beac717)
- Draw every control mark in the companion as a real icon (5345456ec)
- Size the artifacts chip to the session id beside it and give it a paperclip (01d4a9eac)
- Make the favorite star icon truly yellow (674b790cd)
- Keep the caller's session inside a bounded provider probe (1e08f32a1)
- Match the artifact-sheet header to dialog-header height (702eb438d)
- Keep Python provider limits rows valid (92a9400b6)
- Open text artifacts and give the artifacts sheet canonical chrome (51005c0ed)
- Name the provider and model a stalled turn died on (c4482e973)
- Bound provider probes off the UI thread (5e863831a)
- Pin the strings-only Python boundary across the whole tool surface (da43f3352)
- Refuse human input requests without a session (9eb733c90)
- Cross the Python boundary string-clean from every environment symbol (2ab1690ec)
- Cache companion artifacts on the device and page a gallery by size (4ab23fa96)
- List attachments without their bytes and filter history like the byte endpoint (f1d3de024)
- Move the desktop header tabs to the right, beside the cog (c659e46f5)
- Split extension jail policy modes (27a7da8a3)
- Automate iOS crash collection (25fd7f414)
- Anchor the desktop header tabs to the left edge (edb5c0606)
- Serve attachment bytes from the list the descriptors number (e64b35427)
- Treat a blank rg include glob as no filter (bb617c375)
- Release iOS keyboard before backgrounding (376ca3b23)
- Add one artifact index for all session output (d62ac00b4)
- Keep trusted extension shell outside the jail (a6aecf9f9)
- Guard collapsed tool result memory (38f630b98)
- Unify filesystem tool input schema (f62d39a97)
- Allow trusted extensions to spawn subprocesses (cf6498c3a)
- Extract the annotation stack into reusable single-purpose modules (d88225c54)
- Make the artifacts gallery proposals operable and touch-sized (66166db20)
- Stop advertising strict tools on every wire (6d60bf87f)
- Log the tool-call transport wreckage the door drops (b6a5630e0)
- Advertise :strict only when supported by the request API format (0276623e9)
- Refuse a tool-call arguments payload that is not an object (242250462)
- Decode drifted escapes in one linear allocation-free pass (34fdf9f66)
- Propose three Artifacts gallery designs for the session header (44aac9a23)
- Drop leaked tool-call close tags from tool arguments (a7fd20b95)
- Describe a failed turn in its own terminal event (5dc4e2abf)
- Decode drifted \uXXXX escapes only into visible assigned characters (3bfb8633d)
- Let the app annotate a PDF page or an HTML artifact and attach it (66f6d90ba)
- fix(gateway/bus): mirror sibling events under the sid the process registered (64d0e8de7)
- Decode drifted unicode escapes in patch and struct_patch text (90b79ba69)
- Name the voice job SSE stream so clients cannot confuse it with the session log (6237baaa9)
- Cross-validate every anydoc example against the engine (380d8f0ae)

#### com.blockether/vis-channel-tui
- Fix non-deterministic model picker title in the add-provider band (996e8422e)
- Spec the TUI transient and compute its layout once (a1235b3a3)
- Compose transient panels through embed-transient! (aa009dad5)
- Revert "Rename bar identifiers to baz" (8105572f7)
- feat(tui): answer every /draft slash with the draft band (4dcfef4e7)
- fix(gateway,hitl): one sid spelling in the registry, lock-free hydrate, OTP as a secret (fd152579d)
- Split the draft band into create, switch and abandon commands (35adceb1e)
- Merge the draft transient's Create and Danger groups into Actions (2f39dafc8)
- Implement draft controls as a Magit transient panel (d1cef6941)
- Rename bar identifiers to baz (d921da75a)
- Remove /draft blank (45f5ff1fa)
- Replay every parked human-input request on attach (03e86ba9c)
- Surface daemon-side human input in the terminal (537da3648)
- Stream empty-reply resends live and name why each provider call exists (8569d0fed)
- Make failed-turn error cards selectable in the TUI transcript (1f9ab4fcf)
- Bound provider probes off the UI thread (5e863831a)
- Give the TUI error card a margin row and bottom padding (3a8364543)
- Paint a failed turn as a card in the TUI (a18825579)

#### com.blockether/vis-persistance-sqlite
- List attachments without their bytes and filter history like the byte endpoint (f1d3de024)

#### com.blockether/vis-provider-opencode-go
- Add opencode-go provider with per-model wire routing (a6dc50277)



## [v0.1.28] - 2026-08-04

### Changed
- Release v0.1.28
- Serve the installer and the vis-agent command as release assets
- Crown the start menu with the Blockether yellow
- Keep the start menu reading parked drafts when the menu is re-anchored
- Add a dark-theme Vis logo and remove background color from transparent assets
- Fix numpy and PIL sandbox shim gaps for image work
- Take the keyboard down before the attachment sheet
- Delete a session without waiting on its live teardown
- Point the design-shots section at cap/shot!
- Make TUI screenshots one call and paint italic and underline like a terminal
- Build the OAuth file refresher lazily so its lock path is the user's
- Re-anchor the start menu on resize instead of closing it
- Align disclosure copy targets with their rendered content
- Photograph where the Blockether yellow goes on the start menu
- Read the environment at runtime, not at native-image build time
- Give each machine a distinct colored indicator in the session list
- Give every commit on main its own CI run
- Rasterize a bold run as bold, not as the mono face's only weight
- Never cancel a CI run for the commit already on main
- Select the runtime an update names
- Use inverse text colors for vis-table card headers
- Clone the dev checkout on main when there is none
- Separate machines in the sessions list with air and a banner
- Keep the vis-table card's row bands inside the frame
- Dress the vis-table card as a sheet: muted rules, banded head, zebra rows
- Prove the canvas flush beats reflection, not a stopwatch
- Float sessions holding unsent work and keep their attachments
- Offer none authentication in the paramiko shim SSH server
- Make the urllib3 shim a package, not one flat module
- Use a green background rather than green text for the live badge
- Rasterize captured TUI frames in Clojure, in the theme's own colours
- Report the inspected session's turn and form-level failures
- Ignore Wrangler local dev state
- Give the live count the unread badge's filled block, in green
- Refresh the companion incremental build info
- Record svar 0.7.100 in the dependency audit
- Judge the PIL pixel-read budget by canvas ratio, not wall clock
- Report the real cause when a lazy Python shim fails to import
- Render a vis-table preview as one stretched card
- Pin ruff at 0.3.4, whose reported version is the released one
- Cross-validate the vis-table pipeline on the TUI
- Limit turns with no provider output using a first-output timeout
- Stop cancel from settling a live turn's durable row
- Keep attached CSV rows out of model context and rebuild the grid
- Fail a Python lint/format target that holds no Python
- Bracket the live count in a scope chip
- Name the publisher's relay by default on the gateway too
- Show live and unread machine counts in the scope strip
- Let a device name the relay that sealed its grant
- Add ruled-grid and cell-inspector table design proposals
- Reformat the bs4 shim with ruff
- Count the files a Python lint read, not the ones with findings
- Show unread as the filled badge, not a bare number
- Report rows changed from sqlite3 executemany
- Make the urllib3 shim match urllib3 2.x behaviour
- Render tables through a vendored python-tabulate
- Page the CSV table viewer instead of filtering it
- Show a chip's live count as a coloured (N)
- Drop unresolvable fault locations from Clojure test results
- Keep an Android device Google only disliked one message from
- Decide a notification's size before a provider does
- Add a vis-table design gallery variant
- Show the selected relay and explain relay rejection
- Make the requests and httpx shims behave like the real clients
- Retry transient relay failures once and require HTTPS for delivery grants
- Fetch the relay healthcheck URL the workflow was given
- Register through the relay when a machine holds no push key
- Expose httpx.Response.elapsed in the sandbox shim
- Calculate strict-tool cost from provider grammar capacity
- Render attached CSV as an interactive table in TUI and companion
- Grant the relay workflow only what a deploy needs
- Bound the relay's request body and never leak a stack trace
- Stream iterable request bodies instead of form-encoding them
- Correct the APNs environment-retry comment for a stateless relay
- Seal the relay's grants and delete its database
- Meter the relay's public routes before they can cost anything
- Deploy the relay from CI on every commit that touches it
- Relay push through a grant so a gateway needs no signing key
- Size the fleet scope chips to the app control scale
- Keep sessions with unsent messages in the companion list as dirty rows
- Accept a label caption on vis_attach and vis_attach_bytes
- Constrain sampling of the tools whose payload can be mis-serialized
- Document red-then-green regression discipline and link regression tests to their issues
- Document that APNs and FCM credentials bind to the app build
- Identify Companion search as spanning machines
- Make struct_patch `match` a sub-expression selector under every locator
- Organize companion sessions by machine
- Keep a timed-out Python block's output and budget HTTP evals
- Derive per-tool strict sampling from the wire schema
- Add companion design gallery for screenshotted UI proposals
- Keep enforceable schema constraints on the model-facing wire
- Detect awaitables by type so BeautifulSoup survives auto-settle
- Name the sending gateway in every push alert
- Accept a scheme-less gateway address in the connect form
- Paint authored line breaks in the companion thinking card
- Correct the tool-schema docstring: nothing re-validates inbound input
- Run the container gateway on the JVM instead of the native binary
- Keep a shell command's own line structure in the COMMAND card
- Coerce a stringified patch edits batch instead of refusing it
- Keep tool-authored blank lines inside op-card sections
- Enforce queue retraction at the single queued-turn writer
- Run settings MCP and provider verbs as magit transients
- Render a pending git call with the finished card's renderers
- Coerce reversed and non-positive cat windows instead of failing the read
- Make the TUI queue mirror survive repeats, cancels and acks
- Pin the Character/codePointAt registration every YAML read needs
- Render a running native call with the finished card's own renderer
- Name the offending cat range window in the rejection message
- Correct the queue-mirror identity docstring for optimistic rows
- Show a queued TUI submission the instant it is sent
- Extract the magit transient into an embeddable component
- Key the live render cache on a running call's display fields
- Render a running native call as its own op-card
- Guard TUI submissions in flight against double queueing
- Move providers into TUI Settings
- Carry native input on every pending tool-call block
- Pin svar 0.7.99 so provider responses are never interned
- Default TUI frame renders to the bundled JetBrains Mono
- Pass the requested font family through the PIL shim bridge
- Probe GraalPy context health instead of matching exception text
- Cap rendered TUI frame PNGs at 1024px per side
- Heal Python extension symbols whose context was torn down
- Pin svar 0.7.98 so tool arguments are never interned
- Narrow the tool-call door to model drift and extension EDN
- Bind the session environment for Python hook callbacks
- Record the measured cost of dropping builder swap and heap limits
- Measure the wire image cap on the base64 payload
- Render TUI capture frames legibly and under a 2000px cap
- Stamp every Python sandbox shim with __file__ and __version__
- Pin svar 0.7.97 for strings-only tool arguments
- Drop launcher-owned runtime and update commands from the binary
- Measure every outgoing image against the pixel limit
- Normalize svar tool calls to strings at one engine door
- Limit outgoing image dimensions for multi-image requests
- Pin the owned source checkout instead of cloning it
- Cross the PIL draw bridge once per run and convert rasters without reflection
- Align the Magit transient panel with the dialog's bottom controls
- Test that notification taps open the associated session
- Batch PIL draw ops into one cdylib call
- Open the session a tapped notification is about after a cold start
- Stringify keyword tool-argument values at the svar edge
- Pin the status-bar padding of the chrome-less session screen
- Batch PIL shim drawing through one live imaging image
- Keep companion chrome when a notification opens a session
- Anchor magit transient band to its own hint bar
- Fall back to any healthy container engine in release-native
- Carry the vis-agent command through a source update
- Accept bare runtime words in vis-agent update
- Document Python layout-read warnings and venv interpreter resolution
- Skip credential prompts for automatically generated credentials
- release: update release notes for v0.1.27

### Package changes

#### com.blockether/vis
- Release v0.1.28 (95469bf91)
- Serve the installer and the vis-agent command as release assets (e1f106241)
- Crown the start menu with the Blockether yellow (49ccf1b15)
- Keep the start menu reading parked drafts when the menu is re-anchored (18eadb087)
- Add a dark-theme Vis logo and remove background color from transparent assets (ec8343973)
- Fix numpy and PIL sandbox shim gaps for image work (5d1944e16)
- Take the keyboard down before the attachment sheet (8f2a0f339)
- Delete a session without waiting on its live teardown (7d90486fb)
- Point the design-shots section at cap/shot! (d45ddf0e0)
- Make TUI screenshots one call and paint italic and underline like a terminal (367bbe19b)
- Build the OAuth file refresher lazily so its lock path is the user's (0ff2630a3)
- Re-anchor the start menu on resize instead of closing it (a859de278)
- Photograph where the Blockether yellow goes on the start menu (19416e983)
- Read the environment at runtime, not at native-image build time (0d980d92c)
- Give each machine a distinct colored indicator in the session list (e8b2b563e)
- Give every commit on main its own CI run (e819e86c4)
- Never cancel a CI run for the commit already on main (451fa1e36)
- Select the runtime an update names (a09ee8b9c)
- Clone the dev checkout on main when there is none (3f0f26627)
- Separate machines in the sessions list with air and a banner (ed538a7a4)
- Prove the canvas flush beats reflection, not a stopwatch (0bf4e7d20)
- Float sessions holding unsent work and keep their attachments (563738cdd)
- Offer none authentication in the paramiko shim SSH server (ca2d8e1b0)
- Make the urllib3 shim a package, not one flat module (297543cf0)
- Use a green background rather than green text for the live badge (86575fd5e)
- Rasterize captured TUI frames in Clojure, in the theme's own colours (7d62afb1b)
- Report the inspected session's turn and form-level failures (9f700957a)
- Ignore Wrangler local dev state (d05b62c20)
- Give the live count the unread badge's filled block, in green (6480d32d4)
- Refresh the companion incremental build info (0cdb59d82)
- Record svar 0.7.100 in the dependency audit (6b7b4cbaa)
- Judge the PIL pixel-read budget by canvas ratio, not wall clock (9641dffef)
- Report the real cause when a lazy Python shim fails to import (a60ade92e)
- Pin ruff at 0.3.4, whose reported version is the released one (36d8e805b)
- Limit turns with no provider output using a first-output timeout (6060ebdcc)
- Stop cancel from settling a live turn's durable row (0a92426f7)
- Keep attached CSV rows out of model context and rebuild the grid (0e4242f92)
- Bracket the live count in a scope chip (afa1480a1)
- Name the publisher's relay by default on the gateway too (774e9b99e)
- Show live and unread machine counts in the scope strip (31c4bb444)
- Let a device name the relay that sealed its grant (7a87cbf8f)
- Add ruled-grid and cell-inspector table design proposals (8781c07cf)
- Reformat the bs4 shim with ruff (161430165)
- Show unread as the filled badge, not a bare number (3abd1b858)
- Report rows changed from sqlite3 executemany (de25ca3f3)
- Make the urllib3 shim match urllib3 2.x behaviour (6c0f57375)
- Render tables through a vendored python-tabulate (943f7b74b)
- Page the CSV table viewer instead of filtering it (faf5f94ec)
- Show a chip's live count as a coloured (N) (5086b22cd)
- Keep an Android device Google only disliked one message from (9c803eff8)
- Decide a notification's size before a provider does (4a3d96cff)
- Add a vis-table design gallery variant (6500d3d2e)
- Show the selected relay and explain relay rejection (1e938b352)
- Make the requests and httpx shims behave like the real clients (fe5175e4b)
- Retry transient relay failures once and require HTTPS for delivery grants (63b849b87)
- Fetch the relay healthcheck URL the workflow was given (300444b66)
- Register through the relay when a machine holds no push key (8f0d4a259)
- Expose httpx.Response.elapsed in the sandbox shim (ea0f30b75)
- Calculate strict-tool cost from provider grammar capacity (b6e72492c)
- Render attached CSV as an interactive table in TUI and companion (6ea932a46)
- Grant the relay workflow only what a deploy needs (f39021e51)
- Bound the relay's request body and never leak a stack trace (84bd31f12)
- Stream iterable request bodies instead of form-encoding them (94970f570)
- Correct the APNs environment-retry comment for a stateless relay (e19c030d7)
- Seal the relay's grants and delete its database (1bdd8f5bd)
- Meter the relay's public routes before they can cost anything (a821eedb8)
- Deploy the relay from CI on every commit that touches it (56fe51321)
- Relay push through a grant so a gateway needs no signing key (e2b8afa2d)
- Size the fleet scope chips to the app control scale (33479369c)
- Keep sessions with unsent messages in the companion list as dirty rows (052ac305b)
- Accept a label caption on vis_attach and vis_attach_bytes (063e9fac0)
- Constrain sampling of the tools whose payload can be mis-serialized (b0b8699e5)
- Document red-then-green regression discipline and link regression tests to their issues (28c3efb05)
- Document that APNs and FCM credentials bind to the app build (21d1e8ba3)
- Identify Companion search as spanning machines (ab5bc6be4)
- Make struct_patch `match` a sub-expression selector under every locator (6d78ec191)
- Organize companion sessions by machine (ea200ee24)
- Keep a timed-out Python block's output and budget HTTP evals (7bc2c93e1)
- Derive per-tool strict sampling from the wire schema (14d11f638)
- Add companion design gallery for screenshotted UI proposals (8412114d8)
- Keep enforceable schema constraints on the model-facing wire (4b82fb470)
- Detect awaitables by type so BeautifulSoup survives auto-settle (4d21cb772)
- Name the sending gateway in every push alert (db80d9ad4)
- Accept a scheme-less gateway address in the connect form (a29819910)
- Paint authored line breaks in the companion thinking card (25988a469)
- Correct the tool-schema docstring: nothing re-validates inbound input (2537cc54b)
- Run the container gateway on the JVM instead of the native binary (bc473e2d1)
- Keep a shell command's own line structure in the COMMAND card (966352609)
- Coerce a stringified patch edits batch instead of refusing it (81730582e)
- Render a pending git call with the finished card's renderers (436cc17b0)
- Coerce reversed and non-positive cat windows instead of failing the read (d4e1c0c1f)
- Pin the Character/codePointAt registration every YAML read needs (39acd8439)
- Render a running native call with the finished card's own renderer (42dec7b04)
- Name the offending cat range window in the rejection message (bd36a7bfd)
- Render a running native call as its own op-card (d39ddf82b)
- Carry native input on every pending tool-call block (f4aafdc17)
- Pin svar 0.7.99 so provider responses are never interned (cf3e00bca)
- Default TUI frame renders to the bundled JetBrains Mono (8b379d1f1)
- Pass the requested font family through the PIL shim bridge (c591fd622)
- Probe GraalPy context health instead of matching exception text (15df749f7)
- Heal Python extension symbols whose context was torn down (a1654168c)
- Pin svar 0.7.98 so tool arguments are never interned (640085960)
- Narrow the tool-call door to model drift and extension EDN (7a5b76538)
- Bind the session environment for Python hook callbacks (133de48cc)
- Record the measured cost of dropping builder swap and heap limits (c13bb7fb9)
- Measure the wire image cap on the base64 payload (0dfa76363)
- Stamp every Python sandbox shim with __file__ and __version__ (a176541f4)
- Pin svar 0.7.97 for strings-only tool arguments (a8443d0d9)
- Drop launcher-owned runtime and update commands from the binary (555a52d75)
- Measure every outgoing image against the pixel limit (9782a139f)
- Normalize svar tool calls to strings at one engine door (583c359e8)
- Limit outgoing image dimensions for multi-image requests (5b8d5df2f)
- Pin the owned source checkout instead of cloning it (bf42529be)
- Cross the PIL draw bridge once per run and convert rasters without reflection (75f894a5a)
- Align the Magit transient panel with the dialog's bottom controls (b12b18a4c)
- Test that notification taps open the associated session (dab963288)
- Batch PIL draw ops into one cdylib call (ea08babc6)
- Open the session a tapped notification is about after a cold start (71ecc2a93)
- Stringify keyword tool-argument values at the svar edge (8b2363d15)
- Pin the status-bar padding of the chrome-less session screen (dc86aa54a)
- Batch PIL shim drawing through one live imaging image (9715dde2b)
- Keep companion chrome when a notification opens a session (bc36b82a9)
- Fall back to any healthy container engine in release-native (27d4e9be1)
- Carry the vis-agent command through a source update (69f2c71d8)
- Accept bare runtime words in vis-agent update (271f5f91f)
- Document Python layout-read warnings and venv interpreter resolution (031d08cb5)
- Skip credential prompts for automatically generated credentials (543df0972)
- release: update release notes for v0.1.27 (14db65ea6)

#### com.blockether/vis-channel-tui
- Make TUI screenshots one call and paint italic and underline like a terminal (367bbe19b)
- Align disclosure copy targets with their rendered content (c2cd40d3b)
- Read the environment at runtime, not at native-image build time (0d980d92c)
- Rasterize a bold run as bold, not as the mono face's only weight (46590f686)
- Use inverse text colors for vis-table card headers (52c059fab)
- Keep the vis-table card's row bands inside the frame (0e3fafe24)
- Dress the vis-table card as a sheet: muted rules, banded head, zebra rows (e5e38270e)
- Rasterize captured TUI frames in Clojure, in the theme's own colours (7d62afb1b)
- Render a vis-table preview as one stretched card (7d21dbd90)
- Cross-validate the vis-table pipeline on the TUI (a53af2392)
- Page the CSV table viewer instead of filtering it (faf5f94ec)
- Render attached CSV as an interactive table in TUI and companion (6ea932a46)
- Keep tool-authored blank lines inside op-card sections (f0207533b)
- Enforce queue retraction at the single queued-turn writer (62941ef45)
- Run settings MCP and provider verbs as magit transients (da6cf30f7)
- Make the TUI queue mirror survive repeats, cancels and acks (d7f582c78)
- Render a running native call with the finished card's own renderer (42dec7b04)
- Correct the queue-mirror identity docstring for optimistic rows (6fb63767b)
- Show a queued TUI submission the instant it is sent (a9324f3f9)
- Extract the magit transient into an embeddable component (cf9327a9e)
- Key the live render cache on a running call's display fields (15ef8f46d)
- Render a running native call as its own op-card (d39ddf82b)
- Guard TUI submissions in flight against double queueing (67ee89fe7)
- Move providers into TUI Settings (b789941f4)
- Default TUI frame renders to the bundled JetBrains Mono (8b379d1f1)
- Pass the requested font family through the PIL shim bridge (c591fd622)
- Cap rendered TUI frame PNGs at 1024px per side (396c0090d)
- Render TUI capture frames legibly and under a 2000px cap (7a9eb30be)
- Align the Magit transient panel with the dialog's bottom controls (b12b18a4c)
- Anchor magit transient band to its own hint bar (dcc6d81ec)
- Skip credential prompts for automatically generated credentials (543df0972)

#### com.blockether/vis-foundation-voice
- Read the environment at runtime, not at native-image build time (0d980d92c)

#### com.blockether/vis-language-clojure
- Read the environment at runtime, not at native-image build time (0d980d92c)
- Drop unresolvable fault locations from Clojure test results (bfb63ad9f)

#### com.blockether/vis-language-python
- Fail a Python lint/format target that holds no Python (6dc17beb0)
- Count the files a Python lint read, not the ones with findings (09383af1c)

#### com.blockether/vis-provider-anthropic
- Build the OAuth file refresher lazily so its lock path is the user's (0ff2630a3)
- Read the environment at runtime, not at native-image build time (0d980d92c)

#### com.blockether/vis-provider-github-copilot
- Read the environment at runtime, not at native-image build time (0d980d92c)

#### com.blockether/vis-provider-openai-codex
- Build the OAuth file refresher lazily so its lock path is the user's (0ff2630a3)
- Read the environment at runtime, not at native-image build time (0d980d92c)

#### com.blockether/vis-provider-openrouter
- Read the environment at runtime, not at native-image build time (0d980d92c)

#### com.blockether/vis-provider-zai
- Read the environment at runtime, not at native-image build time (0d980d92c)



## [v0.1.27] - 2026-08-03

### Changed
- Pin the Python interpreter from vis.yml and fix interpreter resolution
- Document --native as the released build with a self-built fallback
- Read API keys in a transient instead of a logo dialog
- Align root help columns and document runtime and config options
- Configure providers and models with magit transients
- Let a magit transient paint inside a host dialog's frame
- Expose project test environment consistently
- Retain selected live provider models
- Fix Bridge commit gate project selection
- Park timeout walls while human input is pending
- Release v0.1.27
- Rebuild the LLM router on /reload
- Give provider card limits their own row
- Stop the composer keyboard flickering after attaching media
- Extend the Magit transient panel to the dialog edges with a title margin
- Accept images up to 25MB over the gateway
- Build Linux release assets locally with a real version sha
- Keep a gateway notification choice even when that machine is unreachable
- Give every paired machine its own notification switch
- release: update release notes for v0.1.26

### Package changes

#### com.blockether/vis
- Pin the Python interpreter from vis.yml and fix interpreter resolution (1afda1c45)
- Document --native as the released build with a self-built fallback (338a8d4f7)
- Align root help columns and document runtime and config options (ba0311e57)
- Expose project test environment consistently (98b0e4b82)
- Park timeout walls while human input is pending (df4b3bad5)
- Release v0.1.27 (6374d3564)
- Rebuild the LLM router on /reload (a76e905f0)
- Stop the composer keyboard flickering after attaching media (a3b35fa59)
- Accept images up to 25MB over the gateway (c5a4f39c8)
- Build Linux release assets locally with a real version sha (f8447edbd)
- Keep a gateway notification choice even when that machine is unreachable (fa66a971d)
- Give every paired machine its own notification switch (fb5a62b45)
- release: update release notes for v0.1.26 (1cd946955)

#### com.blockether/vis-channel-tui
- Read API keys in a transient instead of a logo dialog (c79c52f69)
- Configure providers and models with magit transients (4279a17b1)
- Let a magit transient paint inside a host dialog's frame (609d503d6)
- Retain selected live provider models (6d9f91ef6)
- Give provider card limits their own row (985d90ad1)
- Extend the Magit transient panel to the dialog edges with a title margin (30fc5386e)

#### com.blockether/vis-foundation-bridge
- Fix Bridge commit gate project selection (03514779f)

#### com.blockether/vis-language-python
- Pin the Python interpreter from vis.yml and fix interpreter resolution (1afda1c45)
- Expose project test environment consistently (98b0e4b82)



## [v0.1.26] - 2026-08-03

### Changed
- Release v0.1.26
- Register the BigInteger constructors the image invokes reflectively
- Explain a torn-down Python context and break identical retry loops
- Unwrap host tool envelopes before crossing into extension Python
- Emit pytest terminal report and --junitxml from the sandbox shim
- Guarantee a terminal event for every launched gateway turn
- Teach python run_tests the project's declared layout
- Underline Markdown table links and test their click regions
- Make markdown links inside table cells clickable
- Record how a draft clone was actually made
- Let a jailed command reach the macOS Keychain
- Mount a workspace root only where it exists
- Paint a failed call's error line red
- Derive a background shell id from its command
- Settle a cancelled turn in one frame
- Record who cancelled a turn
- Record the typed human-input pause API in the changelog
- Reformat the tree and drop the ambiguous loop name in the issues example
- Cover human-input dialog queueing in the TUI state store
- Wrap dialog descriptions to rendered width and support narrow terminals
- Render optional dialog description above human-input fields
- Require snake_case keys at the Python human-input boundary
- Ask the clone what its fork skipped instead of mirroring the rules
- Document the no-root options for draft copy-on-write
- Document that btrfs drafts need root only once
- Enforce required human-input fields and mark them REQUIRED
- Document that one btrfs mount over a subdirectory enables drafts
- Give every human-input field a name, a label and a description
- Explain the copy-on-write filesystem drafts require
- Enforce human-input rules identically on the TUI and the app
- Stop tracking generated tsc buildinfo files
- Pick the podman machine that can hold the native builder
- Exclude filtered Yarn artifacts from draft deletion reports
- Render magit transient flags as toggles distinct from commands
- Build the linux-x64 release asset locally through Rosetta
- Cross-validate human input on the TUI and the companion app
- Stop counting a draft-generated ignored tree as an agent change
- Dismiss companion overlays when a session opens underneath them
- Refresh command-backed provider credentials on auth rejection
- Deliver human input requests to the companion app
- Never delete the trees a gitignore-aware fork skipped
- Render human-input dialog with canonical TUI painters
- Wire the human-input dialog into the TUI screen
- Add the TUI human-input dialog renderer
- Make the python_execution helper rule imperative
- Build the local linux-arm64 asset with podman as well as docker
- Ship GraalPy language resources beside the native runtime
- Flag off-wire scopes in the session_fold card
- Isolate drafted sessions from configured filesystem roots
- Add typed human-input pause API for extensions
- Drop monorepo release note and extending link from README
- Render a pending shell call as its shell block
- Give the x64 native builder the heap its analysis measurably needs
- Keep the composer keyboard across native picker and camera sheets
- Make pushing verified work a repo contract
- Add a Magit-style --no-verify switch to the TUI commit transient
- fix(ios): ask App Store Connect for the relationship it is matched by
- release: update release notes for v0.1.25

### Package changes

#### com.blockether/vis
- Release v0.1.26 (89df19352)
- Register the BigInteger constructors the image invokes reflectively (ac67464c7)
- Explain a torn-down Python context and break identical retry loops (632247f3a)
- Unwrap host tool envelopes before crossing into extension Python (e95dcf997)
- Emit pytest terminal report and --junitxml from the sandbox shim (b8f5b580f)
- Guarantee a terminal event for every launched gateway turn (f879b7825)
- Teach python run_tests the project's declared layout (c02a64918)
- Record how a draft clone was actually made (bd51f3649)
- Let a jailed command reach the macOS Keychain (ae2f2a76b)
- Mount a workspace root only where it exists (619ff91eb)
- Derive a background shell id from its command (d77cee07c)
- Record who cancelled a turn (4ffc57709)
- Record the typed human-input pause API in the changelog (3ba586e5b)
- Reformat the tree and drop the ambiguous loop name in the issues example (f7046766e)
- Render optional dialog description above human-input fields (d84392570)
- Require snake_case keys at the Python human-input boundary (609374929)
- Ask the clone what its fork skipped instead of mirroring the rules (ad00c209c)
- Document the no-root options for draft copy-on-write (c397c6327)
- Document that btrfs drafts need root only once (f61be0fe3)
- Enforce required human-input fields and mark them REQUIRED (46bb32041)
- Document that one btrfs mount over a subdirectory enables drafts (d3461ecf5)
- Give every human-input field a name, a label and a description (bc7b618e9)
- Explain the copy-on-write filesystem drafts require (782f4d5c4)
- Enforce human-input rules identically on the TUI and the app (3633dabcb)
- Stop tracking generated tsc buildinfo files (4ec7b78dd)
- Pick the podman machine that can hold the native builder (ac1ddcabe)
- Exclude filtered Yarn artifacts from draft deletion reports (024bfc742)
- Build the linux-x64 release asset locally through Rosetta (e756ef8fe)
- Cross-validate human input on the TUI and the companion app (f00138072)
- Stop counting a draft-generated ignored tree as an agent change (9febe38bd)
- Dismiss companion overlays when a session opens underneath them (0b0efcecf)
- Refresh command-backed provider credentials on auth rejection (7037440d5)
- Deliver human input requests to the companion app (a0ce6fa32)
- Never delete the trees a gitignore-aware fork skipped (86b30737a)
- Make the python_execution helper rule imperative (745f7917a)
- Build the local linux-arm64 asset with podman as well as docker (165b65445)
- Ship GraalPy language resources beside the native runtime (d37c55e2e)
- Flag off-wire scopes in the session_fold card (f315a8b80)
- Isolate drafted sessions from configured filesystem roots (22548058d)
- Add typed human-input pause API for extensions (d1eeb3221)
- Drop monorepo release note and extending link from README (96e0f910c)
- Render a pending shell call as its shell block (7abe19cda)
- Give the x64 native builder the heap its analysis measurably needs (9ec55ca33)
- Keep the composer keyboard across native picker and camera sheets (a77c5ff85)
- Make pushing verified work a repo contract (4da60f138)
- fix(ios): ask App Store Connect for the relationship it is matched by (aa92c592e)
- release: update release notes for v0.1.25 (c22ad7f0b)

#### com.blockether/vis-channel-tui
- Underline Markdown table links and test their click regions (0270524f4)
- Make markdown links inside table cells clickable (71f3950a5)
- Paint a failed call's error line red (ed0eccaa8)
- Settle a cancelled turn in one frame (9730a9d02)
- Cover human-input dialog queueing in the TUI state store (24ef40908)
- Wrap dialog descriptions to rendered width and support narrow terminals (483947fd6)
- Render optional dialog description above human-input fields (d84392570)
- Enforce required human-input fields and mark them REQUIRED (46bb32041)
- Give every human-input field a name, a label and a description (bc7b618e9)
- Enforce human-input rules identically on the TUI and the app (3633dabcb)
- Render magit transient flags as toggles distinct from commands (0baca3543)
- Cross-validate human input on the TUI and the companion app (f00138072)
- Render human-input dialog with canonical TUI painters (4cd3c8ae3)
- Wire the human-input dialog into the TUI screen (5ecfb8fbc)
- Add the TUI human-input dialog renderer (1c8ca4172)
- Render a pending shell call as its shell block (7abe19cda)
- Add a Magit-style --no-verify switch to the TUI commit transient (fdcb547b2)

#### com.blockether/vis-language-python
- Teach python run_tests the project's declared layout (c02a64918)

#### com.blockether/vis-persistance-sqlite
- Record how a draft clone was actually made (bd51f3649)
- Isolate drafted sessions from configured filesystem roots (22548058d)

#### com.blockether/vis-workspace-rift
- Record how a draft clone was actually made (bd51f3649)
- Ask the clone what its fork skipped instead of mirroring the rules (ad00c209c)



### Added

- human-input: let extensions ask typed questions with vis.ask or vis/request-human-input!. TUI, gateway and Companion present the same fields, validation, submit and cancel actions. Supported fields include plaintext, password, multiline, select, multiselect and checkbox. Require snake_case keys and enforce is_required across clients and HTTP. Cancellation and timeout return a falsey Answer with a reason. Passwords return opaque vis-secret handles; reveal resolves them in-process and forget removes them. Documented in resources/vis-docs/extending.md.

- config: add conditional filesystem roots using when.os, when.exists and optional. Omit roots unavailable on the current host before building the jail, including their allow-list references. Doctor reports conditional skips as info and missing required paths as warnings.
- config: allow explicitly declared macOS Mach services for confined children. The keychain option grants required security services and keychain-directory access while excluding those directories from default search. Additional services require explicit names; access remains denied by default and errors identify missing permissions.

### Changed

- change(drafts): persist rift 0.0.10-10's actual copy mechanism as workspace_mechanism: btrfs, reflink, apfs, worktree or copy. Store NULL when the backend does not report it; existing drafts remain usable.

- change(drafts): use rift 0.0.10-9's recorded pruned paths rather than duplicating backend filter rules. Preserve tracked artifact directories and matching Git status. Older clones without a marker use the source repository's ignore rules.

### Fixed

- fix(native-image): register BigInteger(String) and BigInteger(String,int) constructors in reachability metadata. This fixes YAML integer parsing and tools.reader failures in native commands outside the repository root; tests enforce both registrations.

- fix(drafts): exclude paths omitted during cloning from the deletion diff used by /draft apply. Ignored and generated directories absent from gitignore-aware forks no longer cause deletion of source-repository files.

- fix(drafts): exclude untracked, ignored files generated within a draft from changed-paths and /draft apply. Force-added tracked files remain included.

- fix(drafts): prevent /draft apply from deleting committed Yarn artifact paths omitted by the fork, including .yarn/cache, unplugged, install-state.gz and build-state.yml. Continue reporting actual deletions in patches and releases.

### Documentation

- docs: document the per-root `draft` policy (`shared`, `copy-only`,
  `copy-and-apply`, `not-allowed`) in `resources/vis-docs/configuration.md`,
  `resources/vis-docs/sandbox.md`, and `resources/vis-docs/drafts.md`, replacing
  the stale claim that filesystem roots are not draft-specific.

## [v0.1.25] - 2026-08-03

### Changed
- release: v0.1.25
- Keep generated audit dates stable during content checks
- perf(drafts): clone gitignore-aware trees by bumping rift to 0.0.10-8
- Cover nested edits and deletions in the draft apply! round-trip
- Continue queued work after provider timeouts
- Pin the wrapped-401 cooldown with a test
- Detect 401 responses in wrapped router failure attempts
- Name cat's own key when a batch entry is malformed
- Trust a corporate CA without patching the JDK
- Prune build directories at any depth, not just the tree root
- Drop the OPEN pill from session list rows
- Regenerate the audit inventory for svar 0.7.96
- Stop op hooks from killing the call they only observe
- Name the draft that carries my uncommitted changes
- Read the credential verdict off the attempts, not the wrapper
- Open the app from the share extension with the modern selector
- Speak of stopping and starting a REPL, never restarting it
- Name the container in the git tool's raw-result contract
- Renumber provider priority whenever the router is reordered
- Read the routing attempts off the live throwable
- Let a provider fetch its key from a command instead of an env var
- Stop calling a dropped connection a rejection
- Revert retry handling for connection timeouts with status codes
- Bind skill as a Python verb beside its native tool
- Retire the restart op from REPL and resource lifecycle
- Stop the environment running code before the jail exists
- Format the pack-owned scan the way zprint wants it
- Retry connection timeouts that include a status code
- Implement parallel scanning in the language pack
- Stop a child's Ctrl-C from killing the gateway
- Report gateway termination causes
- Compile the lint target without ever running it
- Find the needle in a 20 MB file, and stop sweeping forever
- Never let the audit record downgrade a license it already vetted
- Retry every App Store Connect call, not only the two that failed
- One attachment control in the composer, not two
- Do not lose a release to one 401 from Apple
- Sign with the distribution identity the keychain actually has
- Sign archives manually rather than generating certificates in CI
- Name a profile for every bundle, or export automatically
- Export the archive even when only the app has a pinned profile
- audit: read imaging 0.1.7 license and size from the published artifact
- Move the imaging pin to 0.1.7 and lock the pptx shim against it
- Set the evaluation timeout above the maximum allowed shell wait
- Offer "Start the session in" when the TUI opens a new session
- Test the companion share intake, and pin its dependencies exactly
- test(pptx): lock the imaging 0.1.6 chart part, picture crop and read-back
- Track raw io.FileIO and host sqlite3 descriptors
- Document shell wait n and until limits
- Accept system shares into the companion composer
- test(mcp): assert the kill brake by pool state, not by an exact connect log
- test(sandbox): make the fd hardening cases actually discriminate
- test(sandbox): pin fd reclamation against every open door
- test: prove parallel OAuth refresh by overlap, not wall-clock
- Reclaim and cap sandbox Python file descriptors
- chore: com.blockether/imaging 0.1.6 — pptx shim round-trip asserted unconditionally
- fix(shell): recover from descriptor exhaustion instead of blaming the JDK
- test(editing): pin struct_index/cat ranges parity for absent, empty and bad shapes
- refactor(prompt): keep the sleep/poll prohibition only in the tool description
- feat(editing): accept [-1, -1] as cat/struct_index whole-file sentinel
- refactor(python): read pyproject table headers with the tree-sitter TOML grammar
- fix(python): detect uv by TOML table header, not substring
- feat(python): name the undeclared src import root when pytest collection fails
- fix(test): gate the pptx re-open assertion on the resolved imaging reader
- fix(audit): keep the generator's prose in sync and refresh the inventory
- test(python): lock pytest node-id selection and --collect-only
- feat(prompt): make reproduction REPL-first and keep it as a test
- fix(test): lock the fold card on bin/vis-agent
- fix(python): support pytest node-id selection and --collect-only
- fix(shell): capture streams whole and clip only the card
- fix(python): honor explicit run_tests targets and never green an empty run
- fix(python): re-expand folded kwargs for Python-backed tool symbols
- fix(shell): require until for wait and report exited processes accurately
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.38
- fix(companion): one owner for transcript scroll anchoring
- fix(mcp,acp): string-keyed session-server results, like every MCP surface
- feat(shell): wait for a background job's condition, not a guessed clock
- fix(routing): keep both halves of a session model pin canonical
- fix(mcp): refuse an unauthorized OAuth server instead of blocking on a browser
- fix(config,cli): render a vis.yml provider id verbatim, never capitalized
- fix(companion): follow the gateway's session.model_updated broadcast
- fix(mcp,routing): total JSON-RPC encoding, headless MCP OAuth, pin fixes
- docs(site): describe the mechanism directly in site headings
- fix(companion): keep the reader's line by anchoring an element, not a height
- test(loop): pin the auth cooldown and the pinned-model router
- fix(providers): log out a key-only provider by clearing its key, keeping its entry
- chore(cli,structural): formatter reflow and reflection hints
- docs(readme): lead with Clojars and license, drop build and allowlist sections
- test(loop): mcp exposes one mcp__call verb, server-only lists schemas
- feat(companion,install): camera capture, install script as a release asset, documented Clojars
- fix(loop): resolve provider credentials per request to handle OAuth token rotation
- release: update release notes for v0.1.24

### Package changes

#### com.blockether/vis
- release: v0.1.25 (158cc2df0)
- Keep generated audit dates stable during content checks (cccd0fefd)
- perf(drafts): clone gitignore-aware trees by bumping rift to 0.0.10-8 (3015f6ce5)
- Cover nested edits and deletions in the draft apply! round-trip (9990742ff)
- Continue queued work after provider timeouts (346f09bed)
- Pin the wrapped-401 cooldown with a test (3ed40ced7)
- Detect 401 responses in wrapped router failure attempts (086d0c166)
- Name cat's own key when a batch entry is malformed (dd2b1d88a)
- Trust a corporate CA without patching the JDK (ca46cc9c5)
- Prune build directories at any depth, not just the tree root (04f8f9ef3)
- Drop the OPEN pill from session list rows (479635c2a)
- Regenerate the audit inventory for svar 0.7.96 (eee605167)
- Stop op hooks from killing the call they only observe (14a594dee)
- Name the draft that carries my uncommitted changes (a883c1d94)
- Read the credential verdict off the attempts, not the wrapper (be3daff6a)
- Open the app from the share extension with the modern selector (4e2876853)
- Speak of stopping and starting a REPL, never restarting it (866b981e2)
- Name the container in the git tool's raw-result contract (44b2b4c34)
- Renumber provider priority whenever the router is reordered (b778185e3)
- Read the routing attempts off the live throwable (4bcf77594)
- Let a provider fetch its key from a command instead of an env var (2fd49ce1a)
- Stop calling a dropped connection a rejection (4ff5d0f6d)
- Revert retry handling for connection timeouts with status codes (c349c707a)
- Bind skill as a Python verb beside its native tool (dfbb1e3b4)
- Retire the restart op from REPL and resource lifecycle (5921c471c)
- Stop the environment running code before the jail exists (a7c0648b0)
- Format the pack-owned scan the way zprint wants it (3ae016e97)
- Retry connection timeouts that include a status code (712ee9f7a)
- Implement parallel scanning in the language pack (dfc7ed12c)
- Stop a child's Ctrl-C from killing the gateway (56ae91192)
- Report gateway termination causes (0e6df3101)
- Find the needle in a 20 MB file, and stop sweeping forever (49302325b)
- Never let the audit record downgrade a license it already vetted (5995096f0)
- Retry every App Store Connect call, not only the two that failed (b097a6d1a)
- One attachment control in the composer, not two (a5a2c2c61)
- Do not lose a release to one 401 from Apple (64c6f5612)
- Sign with the distribution identity the keychain actually has (87cb8f2c4)
- Sign archives manually rather than generating certificates in CI (b5ad3c947)
- Name a profile for every bundle, or export automatically (87dae362f)
- Export the archive even when only the app has a pinned profile (4fc4a2136)
- audit: read imaging 0.1.7 license and size from the published artifact (34333c540)
- Move the imaging pin to 0.1.7 and lock the pptx shim against it (70435b2f8)
- Set the evaluation timeout above the maximum allowed shell wait (7c4fb70c1)
- Test the companion share intake, and pin its dependencies exactly (0d88054b9)
- test(pptx): lock the imaging 0.1.6 chart part, picture crop and read-back (2922d5d00)
- Track raw io.FileIO and host sqlite3 descriptors (bd08a4061)
- Document shell wait n and until limits (a0680bbb7)
- Accept system shares into the companion composer (1b572e7f0)
- test(mcp): assert the kill brake by pool state, not by an exact connect log (95a072020)
- test(sandbox): make the fd hardening cases actually discriminate (7a5daf39e)
- test(sandbox): pin fd reclamation against every open door (746804d33)
- test: prove parallel OAuth refresh by overlap, not wall-clock (bcaf6d840)
- Reclaim and cap sandbox Python file descriptors (61505abe2)
- chore: com.blockether/imaging 0.1.6 — pptx shim round-trip asserted unconditionally (e71d6dcf2)
- fix(shell): recover from descriptor exhaustion instead of blaming the JDK (43e23a515)
- test(editing): pin struct_index/cat ranges parity for absent, empty and bad shapes (9fd9d014c)
- refactor(prompt): keep the sleep/poll prohibition only in the tool description (1a48581aa)
- feat(editing): accept [-1, -1] as cat/struct_index whole-file sentinel (464dc6c86)
- feat(python): name the undeclared src import root when pytest collection fails (194487fd3)
- fix(test): gate the pptx re-open assertion on the resolved imaging reader (cdfaaf299)
- fix(audit): keep the generator's prose in sync and refresh the inventory (be1c2b9d6)
- test(python): lock pytest node-id selection and --collect-only (2c8aa3d42)
- feat(prompt): make reproduction REPL-first and keep it as a test (e5809ae68)
- fix(test): lock the fold card on bin/vis-agent (e22406e5d)
- fix(python): support pytest node-id selection and --collect-only (1be741e74)
- fix(shell): capture streams whole and clip only the card (e31fc62f9)
- fix(python): honor explicit run_tests targets and never green an empty run (66d90e4e3)
- fix(python): re-expand folded kwargs for Python-backed tool symbols (0fdf52d91)
- fix(shell): require until for wait and report exited processes accurately (4d01e8db2)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.38 (7d7e0d625)
- fix(companion): one owner for transcript scroll anchoring (14c0e51fd)
- fix(mcp,acp): string-keyed session-server results, like every MCP surface (82aba80ba)
- feat(shell): wait for a background job's condition, not a guessed clock (574cdc5a7)
- fix(routing): keep both halves of a session model pin canonical (dcfede359)
- fix(mcp): refuse an unauthorized OAuth server instead of blocking on a browser (0ccba71f2)
- fix(config,cli): render a vis.yml provider id verbatim, never capitalized (a838d9ea1)
- fix(companion): follow the gateway's session.model_updated broadcast (f8177e8db)
- fix(mcp,routing): total JSON-RPC encoding, headless MCP OAuth, pin fixes (a10ba6158)
- docs(site): describe the mechanism directly in site headings (8cfd79373)
- fix(companion): keep the reader's line by anchoring an element, not a height (4c4c4b17e)
- test(loop): pin the auth cooldown and the pinned-model router (aa386b0b4)
- fix(providers): log out a key-only provider by clearing its key, keeping its entry (1a3a9c382)
- chore(cli,structural): formatter reflow and reflection hints (fa5a4f00d)
- docs(readme): lead with Clojars and license, drop build and allowlist sections (4718fc6b5)
- test(loop): mcp exposes one mcp__call verb, server-only lists schemas (54f90c31e)
- feat(companion,install): camera capture, install script as a release asset, documented Clojars (76183368d)
- fix(loop): resolve provider credentials per request to handle OAuth token rotation (9147f7379)
- release: update release notes for v0.1.24 (bed466948)

#### com.blockether/vis-channel-tui
- Retire the restart op from REPL and resource lifecycle (5921c471c)
- Offer "Start the session in" when the TUI opens a new session (e902804f9)
- fix(routing): keep both halves of a session model pin canonical (dcfede359)
- fix(providers): log out a key-only provider by clearing its key, keeping its entry (1a3a9c382)
- feat(companion,install): camera capture, install script as a release asset, documented Clojars (76183368d)

#### com.blockether/vis-language-clojure
- Speak of stopping and starting a REPL, never restarting it (866b981e2)
- Retire the restart op from REPL and resource lifecycle (5921c471c)
- Compile the lint target without ever running it (47cd48924)

#### com.blockether/vis-language-python
- Retire the restart op from REPL and resource lifecycle (5921c471c)
- refactor(python): read pyproject table headers with the tree-sitter TOML grammar (0e78932c8)
- fix(python): detect uv by TOML table header, not substring (a6eb925d3)
- fix(python): honor explicit run_tests targets and never green an empty run (66d90e4e3)

#### com.blockether/vis-language-typescript-bun
- Retire the restart op from REPL and resource lifecycle (5921c471c)

#### com.blockether/vis-persistance-sqlite
- feat(companion,install): camera capture, install script as a release asset, documented Clojars (76183368d)

#### com.blockether/vis-workspace-rift
- perf(drafts): clone gitignore-aware trees by bumping rift to 0.0.10-8 (3015f6ce5)
- Prune build directories at any depth, not just the tree root (04f8f9ef3)



### Changed

- perf(drafts): pin rift 0.0.10-8 for gitignore-aware cloning. Skip generated output while preserving force-added paths and .git. Repository fork time decreases from about 3.8 seconds to 0.7 seconds with matching Git status.
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.38 — reference
  search runs as one native batch walk, no longer matches names written inside
  string literals or comments (rename included), and the shared native library
  cache is now safe for many concurrent vis sessions/processes

## [v0.1.24] - 2026-08-02

### Changed
- release: v0.1.24
- feat(mcp): manage MCP servers and their OAuth from the companion and the TUI
- fix(release): preserve release notes when main advances and add v0.1.23 notes
- test(ls): pin the warm-index fast path and recursive listing order

### Package changes

#### com.blockether/vis
- release: v0.1.24 (a56ad6009)
- feat(mcp): manage MCP servers and their OAuth from the companion and the TUI (139cdc801)
- fix(release): preserve release notes when main advances and add v0.1.23 notes (57693dc17)
- test(ls): pin the warm-index fast path and recursive listing order (ccbd03009)

#### com.blockether/vis-channel-tui
- feat(mcp): manage MCP servers and their OAuth from the companion and the TUI (139cdc801)
- test(ls): pin the warm-index fast path and recursive listing order (ccbd03009)

#### com.blockether/vis-persistance-sqlite
- test(ls): pin the warm-index fast path and recursive listing order (ccbd03009)



## [v0.1.24] - 2026-08-02

### Added

- mcp: expose server start, stop and authentication lifecycle through the gateway, Companion Settings and a matching TUI dialog. Browser sign-in is available during connection setup.

### Fixed

- bs4 shim: match BeautifulSoup 4.12's TypeError for None input. Verify soupsieve and tree-builder behavior against beautifulsoup4 4.12.3 and soupsieve 2.5.
- release: the workflow's "Commit release notes" step rebases onto
  `origin/main` and retries, so a `main` that moved during the run no longer
  fails the release job and skips the mobile release with it.

- persistence: detect database replacement by filesystem device/inode identity rather than size and modification time. Normal WAL checkpoints no longer trigger repeated SQLite pool closure, leaked housekeeping threads or SIGBUS during live queries.

## [v0.1.23] - 2026-08-02

### Changed

- release: v0.1.23
- test: cover external-opener, notifications and serial-batch
- perf(ls): serve directory listings from the warm fff index
- fix(acp): answer `cancelled` when the cancel itself throws, and bind cancels to turn numbers
- fix(companion): resume live turns from existing session rows
- feat(drafts): a draft can start from your last commit, not your dirty tree
- fix(cli): keep runtime/update in the launcher and reject unknown flags
- fix(acp): refuse phantom resumes and walk tool arguments iteratively
- fix(launcher): dev names one checkout, never a silent substitute
- docs(changelog): record the coherent vis-agent runtime surface
- Advertise cat's directory listing (ls) and compress native tool prose
- chore: vis-agent runtime docs, ACP concurrency fixes, bs4 fidelity, companion polish
- feat(rewind): add /rewind with context reporting
- Compress fs tool reference prose
- Ratchet the native tool prose budget to 1250
- Compress shell and grep reference prose
- Compress structural tool reference prose
- docs(runtime): document release-following default and dev mode precisely
- Compress the core system prompt and ratchet its budget
- feat(launcher): follow releases by default, opt in to dev mode
- chore: include pending foundation, gateway, TUI and Companion changes
- fix(loop): align the overflow-rescue tests with graduated folding
- refactor(installer): drop pre-bundle release asset fallback
- fix: bs4 4.12 serialization fidelity and preflight context-overflow recovery
- feat: unify the vis-agent installer and harden reconnect/teardown paths
- fix(shims): restore two-space prettify indent in the bs4 shim
- feat: launcher restructure, provider relogin fix, and shim/docs refresh
- fix(release): close mobile lineage guards
- feat(shell): add background job completion waits
- style: normalize sqlite test formatting
- feat: improve companion session details and runtime support
- feat(ui): show running raw invocation only for shell and python_execution
- release: update release notes for v0.1.22

### Added

- drafts: add /draft clean <label> and the equivalent Companion option to fork committed HEAD without uncommitted changes. Record omitted paths so later apply operations preserve original work. Reject repositories without a commit.

### Changed

- launcher: `vis-agent` is one coherent surface — run Vis, `vis-agent runtime
  show|use native|jvm|dev|auto`, and `vis-agent update [--native|--jvm|--dev]
  [--rebuild] [vX.Y.Z|<ref>]`. Vis follows releases by default and `dev` is the
  only runtime that follows a moving branch.
- installer: `bin/install-vis-agent --runtime native|jvm` installs the wrapper
  and lets it acquire its own runtime, so wrapper and runtime cannot drift.

### Removed

- launcher: the `--source` alias, the `--jar` tombstone, the wrapper-owned
  `native`/`uber` build commands, `VIS_SOURCE_DIR`, and `VIS_LOCAL_BIN_DIR`.
- installer: `bin/install-source` (use `install-vis-agent --runtime jvm`).
- state: `~/.vis/source-dir`, `~/.vis/sourcecode`, `~/.vis/install/mode`, and
  `~/.vis/install/sha`. Runtime state is now `~/.vis/runtime` plus
  `~/.vis/install/{vis-agent-native,src,ref}`.

### Fixed

- sandbox: track writable GraalPy file handles weakly and flush them before tool calls and at block completion. Writes from handles dropped without close now reach disk instead of remaining buffered.
- git tool: insert --verbose before git add's -- separator, preventing it from being treated as a pathspec.
- launcher: `vis-agent update --native|--jvm|--dev` reaches the update path
  again; the launch-flag parser previously discarded those flags. A final
  `[[ … ]] && cmd` no longer makes a successful `runtime use` exit 1.
- launcher: the `dev` runtime names one checkout. When `$VIS_DEV_CHECKOUT` does
  not hold one, `vis-agent` now says so instead of silently running whichever
  checkout the launcher happens to sit in.
- cli: `runtime` and `update` are listed by `vis-agent --help` and are owned by
  the launcher, so the binary no longer advertised a second, different `update`
  that failed on its own documented flag.
- cli: a mistyped one-shot flag is refused instead of being glued into the
  prompt. `vis-agent --modle gpt-5 "task"` used to run with the DEFAULT model
  and a polluted prompt; it now exits 2 naming the flag. A value flag left
  without a value is refused the same way, `--verbose`/`-v` are consumed as
  debug flags, and `--` ends flag parsing for prompts that start with dashes.
- launcher: `runtime use` and JVM/dev launches report a missing runtime,
  missing `clojure`, or missing `java` with the command that fixes it, instead
  of exiting 127 from `exec`.
- launcher: stop wrapper flag parsing at -- so later runtime/profiling flags remain prompt text. Reject runtime use combined with --native, --jvm or --dev rather than ignoring a flag.
- launcher: persisting a runtime verifies the write, so a `VIS_HOME` that is a
  file, a `~/.vis/runtime` that is a directory, or an unwritable home reports
  the path it could not write instead of claiming "runtime is now …".
- cli: a value flag rejects a blank, `--`, or flag-shaped value. `--model ""`
  used to run the default model and `--model --json task` used to request a
  model literally named `--json`.
- cli: two output modes at once (`--json --code`, `--stream-json --code`, …) are
  refused instead of silently honouring one and dropping the other, and an
  unusable `--db` path is named instead of surfacing a raw SQLite pool error.
- cli: `--help` described `--persist` as the opposite of what it does, twice.

## [v0.1.22] - 2026-08-01

### Changed
- release: v0.1.22
- feat(companion): label session usage stats as meta rows
- feat: canonical MCP transports plus video/media attachment support
- Compress Clojure capability prompt guidance
- Deduplicate run_tests description against its schema
- Restore REPL lifecycle ownership wording
- Tighten REPL facade reference docs
- Tighten language facade tool descriptions
- Tighten language facade reference docs
- Further reduce language capability prompt
- Reduce language facade schema prose
- Reduce REPL start reference docs
- Reduce REPL evaluation reference docs
- Reduce external REPL reference docs
- Reduce linter reference docs
- Reduce test runner reference docs
- Reduce formatter reference docs
- Reduce language capability prompt
- Reduce REPL stop tool surface
- Reduce formatter tool surface
- Reduce lint tool surface
- Reduce external REPL connection surface
- Reduce REPL evaluation tool surface
- Reduce test runner tool surface
- Reduce REPL lifecycle tool surface
- Reduce delete helper surface
- Reduce copy helper surface
- Reduce structural rename tool surface
- Reduce write tool surface
- Reduce filesystem tool surface
- Reduce patch tool surface
- Reduce cat tool surface
- Reduce structural node tool surface
- Reduce structural patch tool surface
- Reduce struct index tool surface
- Reduce grep tool surface
- Reduce Vis docs reference text
- Improve terminal media support and editor search
- Reduce introspection reference docs
- Reduce MCP reference docs
- Reduce Git reference docs
- Reduce search reference docs
- Reduce shell reference docs
- Reduce Bridge tool discovery docs
- Reduce search extension discovery docs
- Reduce Clojure language extension discovery docs
- Reduce Python language extension discovery docs
- Reduce Bun language extension discovery docs
- Reduce introspection extension discovery docs
- Reduce harness extension discovery docs
- Reduce Git extension discovery docs
- Reduce shell extension discovery docs
- Reduce MCP extension discovery docs
- Reduce foundation extension discovery docs
- Reduce YAML extension discovery docs
- Reduce Nippy extension discovery docs
- Reduce HTTPX extension discovery docs
- Reduce requests extension discovery docs
- Reduce urllib3 extension discovery docs
- Improve companion, gateway, TUI, and file attributes
- Reduce attachment extension discovery docs
- Reduce Ruff extension discovery docs
- Reduce BeautifulSoup extension discovery docs
- Reduce TOML extension discovery docs
- Reduce timezone extension discovery docs
- Reduce pytest extension discovery docs
- Reduce NumPy extension discovery docs
- Reduce tabulate extension discovery docs
- Reduce pandas extension discovery docs
- Reduce FontTools extension discovery docs
- Reduce SQLite extension discovery docs
- Reduce XlsxWriter extension discovery docs
- Reduce PPTX extension discovery docs
- Reduce Paramiko extension discovery docs
- Reduce Matplotlib extension discovery docs
- Reduce Pillow extension discovery docs
- Reduce attachment shim discovery docs
- Reduce SQLite shim discovery docs
- Reduce tabulate shim discovery docs
- Reduce requests shim discovery docs
- Reduce BeautifulSoup shim discovery docs
- Reduce urllib3 shim discovery docs
- Reduce TOML shim discovery docs
- Reduce PPTX shim discovery docs
- Reduce YAML shim discovery docs
- Reduce timezone shim discovery docs
- Reduce XlsxWriter shim discovery docs
- Reduce pandas shim discovery docs
- Reduce HTTPX shim discovery docs
- Reduce FontTools shim discovery docs
- Reduce NumPy shim discovery docs
- Reduce Ruff shim discovery docs
- Reduce PIL shim discovery docs
- Reduce Nippy shim discovery docs
- Reduce matplotlib shim discovery docs
- Reduce pytest shim discovery docs
- Reduce Paramiko shim discovery docs
- Reduce attachment shim tool docs
- Reduce sandbox discovery tool docs
- Reduce sandbox helper tool docs
- Use FFF for directory listings
- Reduce Bridge tool surfaces
- Reduce introspection tool surfaces
- Reduce skill tool surface
- Reduce Git tool surface
- Reduce shell tool surface
- Reduce language facade tool surfaces
- Reduce node and filesystem tool surfaces
- Reduce mutation tool surfaces
- Reduce read tool surfaces
- Reduce struct index tool surface
- Reduce session fold tool surface
- refactor(tools): compact engine native contracts
- refactor(tools): compact research search contract
- refactor(tools): compact MCP contracts
- refactor(tools): compact repl lifecycle contract
- fix(companion): make iOS release validation authoritative
- feat(companion): modernize empty session state
- Improve attachment picker cancellation copy
- refactor(tools): clarify native tool contracts
- fix(companion): build iOS releases with Xcode 26
- fix(companion): install App Store profile in CI
- fix(companion): install iOS signing identity in CI
- fix(companion): authenticate Xcode archive export
- fix(companion): scaffold native projects in clean CI
- refactor: consolidate session introspection
- feat(companion): separate application settings
- docs(companion): document beta as the default Play track
- fix(companion): recover Play releases without reuploading
- release: update release notes for v0.1.21

### Package changes

#### com.blockether/vis
- release: v0.1.22 (c528fe794)
- feat(companion): label session usage stats as meta rows (63cf04ccb)
- feat: canonical MCP transports plus video/media attachment support (e2700372f)
- Compress Clojure capability prompt guidance (6bdb4c566)
- Deduplicate run_tests description against its schema (d8be51283)
- Restore REPL lifecycle ownership wording (149421651)
- Tighten REPL facade reference docs (8f35957da)
- Tighten language facade tool descriptions (699c68fe7)
- Tighten language facade reference docs (3467c5928)
- Further reduce language capability prompt (ff8001bf0)
- Reduce language facade schema prose (b88d5993b)
- Reduce REPL start reference docs (1fb581a28)
- Reduce REPL evaluation reference docs (04fcf5795)
- Reduce external REPL reference docs (3a7c50a49)
- Reduce linter reference docs (ee1efe770)
- Reduce test runner reference docs (3adefde76)
- Reduce formatter reference docs (5e7d13491)
- Reduce language capability prompt (07a9b7a18)
- Reduce REPL stop tool surface (75e8bc4d6)
- Reduce formatter tool surface (6250424d6)
- Reduce lint tool surface (e1d11eab0)
- Reduce external REPL connection surface (689d2245d)
- Reduce REPL evaluation tool surface (2bb3cce44)
- Reduce test runner tool surface (0034f6a1f)
- Reduce REPL lifecycle tool surface (c98e8fe19)
- Reduce delete helper surface (611a19903)
- Reduce copy helper surface (8f7e6d014)
- Reduce structural rename tool surface (bd4db7e04)
- Reduce write tool surface (d60e8ca32)
- Reduce filesystem tool surface (e730bf178)
- Reduce patch tool surface (1032ffef6)
- Reduce cat tool surface (57a7da25a)
- Reduce structural node tool surface (63007676a)
- Reduce structural patch tool surface (0c85444dc)
- Reduce struct index tool surface (58ec15e2a)
- Reduce grep tool surface (e3b84ff8f)
- Reduce Vis docs reference text (5a08ade6c)
- Improve terminal media support and editor search (732b4477e)
- Reduce introspection reference docs (b4cbd35a3)
- Reduce MCP reference docs (51bb0ab51)
- Reduce Git reference docs (787301526)
- Reduce shell reference docs (bbe33d429)
- Reduce introspection extension discovery docs (850c5df7c)
- Reduce harness extension discovery docs (a0cf048d8)
- Reduce Git extension discovery docs (d144b5902)
- Reduce shell extension discovery docs (d0f36b2da)
- Reduce MCP extension discovery docs (8adca4ac9)
- Reduce foundation extension discovery docs (1801c2c7a)
- Reduce YAML extension discovery docs (625b6d197)
- Reduce Nippy extension discovery docs (d3748d334)
- Reduce HTTPX extension discovery docs (20e0c5999)
- Reduce requests extension discovery docs (3a6ffc174)
- Reduce urllib3 extension discovery docs (296ae56cc)
- Improve companion, gateway, TUI, and file attributes (930077b92)
- Reduce attachment extension discovery docs (c158b81bc)
- Reduce Ruff extension discovery docs (a5db65cf0)
- Reduce BeautifulSoup extension discovery docs (41482efde)
- Reduce TOML extension discovery docs (a14ffad21)
- Reduce timezone extension discovery docs (0d1a96fa8)
- Reduce pytest extension discovery docs (85d98fc4f)
- Reduce NumPy extension discovery docs (99a9a7149)
- Reduce tabulate extension discovery docs (b5fbfeff4)
- Reduce pandas extension discovery docs (1fc984fe9)
- Reduce FontTools extension discovery docs (eb6df52d6)
- Reduce SQLite extension discovery docs (b3c5e48dd)
- Reduce XlsxWriter extension discovery docs (077db3cea)
- Reduce PPTX extension discovery docs (8ee540d6f)
- Reduce Paramiko extension discovery docs (71561ceae)
- Reduce Matplotlib extension discovery docs (85938d837)
- Reduce Pillow extension discovery docs (efb74cf56)
- Reduce attachment shim discovery docs (2f5d49dc7)
- Reduce SQLite shim discovery docs (1147e2d3c)
- Reduce tabulate shim discovery docs (cd08f0be7)
- Reduce requests shim discovery docs (05071d2fe)
- Reduce BeautifulSoup shim discovery docs (978dfcc80)
- Reduce urllib3 shim discovery docs (ad1177fa5)
- Reduce TOML shim discovery docs (b88814452)
- Reduce PPTX shim discovery docs (be8c505e9)
- Reduce YAML shim discovery docs (7506d5740)
- Reduce timezone shim discovery docs (6f7584ffa)
- Reduce XlsxWriter shim discovery docs (e960ec6b6)
- Reduce pandas shim discovery docs (b695ada80)
- Reduce HTTPX shim discovery docs (5d03f9854)
- Reduce FontTools shim discovery docs (6a9c2f5f4)
- Reduce NumPy shim discovery docs (d2542eaa3)
- Reduce Ruff shim discovery docs (67a1a1a34)
- Reduce PIL shim discovery docs (7689f08fc)
- Reduce Nippy shim discovery docs (3ac70543d)
- Reduce matplotlib shim discovery docs (8c339587b)
- Reduce pytest shim discovery docs (6c189f35e)
- Reduce Paramiko shim discovery docs (c7326ae5d)
- Reduce attachment shim tool docs (53fa235d2)
- Reduce sandbox discovery tool docs (8e0f5ae57)
- Reduce sandbox helper tool docs (29463d297)
- Use FFF for directory listings (6e780f83a)
- Reduce introspection tool surfaces (32fb8690d)
- Reduce skill tool surface (9a949de58)
- Reduce Git tool surface (5c643414c)
- Reduce shell tool surface (e5b2e49a0)
- Reduce language facade tool surfaces (555fcd1b5)
- Reduce node and filesystem tool surfaces (7e410a471)
- Reduce mutation tool surfaces (37c94a04c)
- Reduce read tool surfaces (fb346b800)
- Reduce struct index tool surface (a4ce21b57)
- Reduce session fold tool surface (ca4b5778b)
- refactor(tools): compact engine native contracts (d3473bfb0)
- refactor(tools): compact MCP contracts (995d6ae13)
- refactor(tools): compact repl lifecycle contract (dc89bb6e8)
- fix(companion): make iOS release validation authoritative (a33c57271)
- feat(companion): modernize empty session state (b2643e9d6)
- Improve attachment picker cancellation copy (477eba821)
- refactor(tools): clarify native tool contracts (0348fe903)
- fix(companion): build iOS releases with Xcode 26 (2ef9cf6de)
- fix(companion): install App Store profile in CI (5c05b8615)
- fix(companion): install iOS signing identity in CI (aa2f43133)
- fix(companion): authenticate Xcode archive export (62daa768c)
- fix(companion): scaffold native projects in clean CI (bde4c9cd6)
- refactor: consolidate session introspection (afbe7def0)
- feat(companion): separate application settings (2d44ff13f)
- docs(companion): document beta as the default Play track (f29eafcb9)
- fix(companion): recover Play releases without reuploading (8775b94b3)
- release: update release notes for v0.1.21 (7e838249e)

#### com.blockether/vis-channel-tui
- feat: canonical MCP transports plus video/media attachment support (e2700372f)
- Improve terminal media support and editor search (732b4477e)
- Improve companion, gateway, TUI, and file attributes (930077b92)

#### com.blockether/vis-foundation-bridge
- feat: canonical MCP transports plus video/media attachment support (e2700372f)
- Reduce Bridge tool discovery docs (fcb8313f7)
- Reduce Bridge tool surfaces (a27817463)

#### com.blockether/vis-foundation-search
- Reduce search reference docs (6b2d1c6a9)
- Reduce search extension discovery docs (36040ee03)
- refactor(tools): compact research search contract (64d04a84e)

#### com.blockether/vis-language-clojure
- Reduce Clojure language extension discovery docs (f2e1dc2a2)

#### com.blockether/vis-language-python
- Reduce Python language extension discovery docs (46de06528)

#### com.blockether/vis-language-typescript-bun
- Reduce Bun language extension discovery docs (6d25d2ad5)

## [v0.1.21] - 2026-08-01

### Changed
- docs(companion): release notes for 0.1.21 (2861)
- feat: unify releases and harden live companion behavior
- release: update version files for v0.1.20, bump to next dev version

### Package changes

#### com.blockether/vis
- docs(companion): release notes for 0.1.21 (2861) (51c1fce7d)
- feat: unify releases and harden live companion behavior (52953cd36)
- release: update version files for v0.1.20, bump to next dev version (d6930850e)

#### com.blockether/vis-channel-tui
- feat: unify releases and harden live companion behavior (52953cd36)

## [v0.1.20] - 2026-08-01

### Changed
- chore(deps): svar 0.7.95, refresh the audit inventory
- ci: enforce the locked GraalVM pin across build workflows
- feat: Release viewport performance improvements and accumulated runtime changes
- docs(companion): release notes for 0.1.19 (2854)
- feat(companion): expandable session stats and drafts grouped under their project
- feat(companion): add image viewer and smooth native viewport
- docs(companion): release notes for 0.1.18 (2851)
- perf(companion): keep the app shell off the compositor during keyboard/rotation
- docs(companion): release notes for 0.1.17 (2849)
- style(companion): full-bleed paste blocks in user messages
- perf(companion): update app-shell geometry through CSS custom properties
- docs(companion): release notes for 0.1.16 (2846)
- feat(companion): show recently-active sessions in collapsed projects
- docs(companion): release notes for 0.1.15 (2844)
- perf(companion): isolate shell re-renders from keyboard and rotation frames
- feat(companion): collapsible projects with per-project paging and richer settings
- docs(companion): release notes for 0.1.15 (2841)
- fix(companion): Match composer font sizes and reduce divider thickness
- docs(companion): release notes for 0.1.15 (2839)
- feat(companion): glyph-free composer strip and animated reasoning swap
- docs(companion): release notes for 0.1.15 (2837)
- fix(routing): preserve pinned provider selection
- revert(companion): restore the composer strip glyphs
- revert(companion): bring the glyphs back
- fix(tui): handle whitespace split across styled runs
- docs(companion): release notes for 0.1.15 (2832)
- feat(companion): glyph-free thinking band and model manager
- docs(companion): release notes for 0.1.15 (2830)
- feat(companion): footer reasoning chip, landscape safe areas
- fix(editing): Remove redundant --- before / +++ after labels from diffs
- fix(ci): Restore the last successful native-builder arguments
- fix(ci): give the native builder a 22g heap on the swapfile-backed runner
- fix(ci): switch the preselected ParallelGC off before enabling G1
- fix(ci): Use G1 and an overcommitted heap for the native builder
- docs(companion): release notes for 0.1.15 (2823)
- test(loop): measure the guest-interrupt CPU delta, not JVM-wide CPU
- fix(ci): Limit native-image heap to runner RAM and extend the build timeout
- fix(ci): Increase native-image build heap and support tag rebuilds through workflow dispatch
- docs(audit): refresh dependency inventory
- docs(companion): TestFlight notes for 0.1.14 (2817)
- release: update version files for v0.1.14, bump to next dev version

### Package changes

#### com.blockether/vis
- chore(deps): svar 0.7.95, refresh the audit inventory (e9a1a6f84)
- ci: enforce the locked GraalVM pin across build workflows (0deb9e403)
- feat: Release viewport performance improvements and accumulated runtime changes (66b0c31d8)
- docs(companion): release notes for 0.1.19 (2854) (b0f316183)
- feat(companion): expandable session stats and drafts grouped under their project (da7516494)
- feat(companion): add image viewer and smooth native viewport (91170014c)
- docs(companion): release notes for 0.1.18 (2851) (e43ed8bc6)
- perf(companion): keep the app shell off the compositor during keyboard/rotation (d5f4f08cf)
- docs(companion): release notes for 0.1.17 (2849) (821f28861)
- style(companion): full-bleed paste blocks in user messages (e1e9c7743)
- perf(companion): update app-shell geometry through CSS custom properties (c34efdda8)
- docs(companion): release notes for 0.1.16 (2846) (2815efc72)
- feat(companion): show recently-active sessions in collapsed projects (68352e07b)
- docs(companion): release notes for 0.1.15 (2844) (5b8ddeaa4)
- perf(companion): isolate shell re-renders from keyboard and rotation frames (a70d92516)
- feat(companion): collapsible projects with per-project paging and richer settings (2d04f57a6)
- docs(companion): release notes for 0.1.15 (2841) (49276fdf0)
- fix(companion): Match composer font sizes and reduce divider thickness (d15a8b24b)
- docs(companion): release notes for 0.1.15 (2839) (d0e0b328d)
- feat(companion): glyph-free composer strip and animated reasoning swap (25fa99d8e)
- docs(companion): release notes for 0.1.15 (2837) (bcb1f7cfc)
- fix(routing): preserve pinned provider selection (2add31771)
- revert(companion): restore the composer strip glyphs (cf7f968d4)
- revert(companion): bring the glyphs back (0a8d32b2a)
- docs(companion): release notes for 0.1.15 (2832) (2519dcd7a)
- feat(companion): glyph-free thinking band and model manager (012b26d82)
- docs(companion): release notes for 0.1.15 (2830) (7bd15a8df)
- feat(companion): footer reasoning chip, landscape safe areas (dbab3492c)
- fix(editing): Remove redundant --- before / +++ after labels from diffs (f7870f43b)
- fix(ci): Restore the last successful native-builder arguments (8edf48275)
- fix(ci): give the native builder a 22g heap on the swapfile-backed runner (fff80c57a)
- fix(ci): switch the preselected ParallelGC off before enabling G1 (e63b9282a)
- fix(ci): Use G1 and an overcommitted heap for the native builder (8ea6b9d15)
- docs(companion): release notes for 0.1.15 (2823) (c85f7da50)
- test(loop): measure the guest-interrupt CPU delta, not JVM-wide CPU (2626ea8d7)
- fix(ci): Limit native-image heap to runner RAM and extend the build timeout (715cd41e2)
- fix(ci): Increase native-image build heap and support tag rebuilds through workflow dispatch (eadda4851)
- docs(audit): refresh dependency inventory (8e2a282f3)
- docs(companion): TestFlight notes for 0.1.14 (2817) (0738c1822)
- release: update version files for v0.1.14, bump to next dev version (34f89e45b)

#### com.blockether/vis-channel-tui
- feat: Release viewport performance improvements and accumulated runtime changes (66b0c31d8)
- fix(tui): handle whitespace split across styled runs (cdf9bd256)

#### com.blockether/vis-persistance-sqlite
- feat: Release viewport performance improvements and accumulated runtime changes (66b0c31d8)

## [v0.1.14] - 2026-07-30

### Changed
- fix(editing): Preserve newlines in structural edits and support comment documentation across 28 languages
- Record the 0.1.14 (2815) release notes
- Cover turn attachments with tests and note 0.1.14 in the changelog
- Bump tree-sitter-language-pack to 1.12.3-blockether.32
- Serve a turn's inline attachments and hide the footer mid-turn
- Bump tree-sitter-language-pack to 1.12.3-blockether.31
- Name every working directory `cwd` across the tool surface
- Cache live turn content for immediate rendering on session re-entry
- docs(companion): release notes for 0.1.14 (2808)
- Let the companion app change the reasoning mode
- Adopt already-running turns in the companion session screen
- Add PRIVACY.md for the companion app (Play store policy URL)
- Ensure turns emit terminal events and bound Python GC
- Allow block-local shadowing of bound tool names in vis Python
- docs(companion): release notes for 0.1.14 (2802)
- Unify tool input carriers and refresh companion diff view
- Fix gateway, Python, Git, and TUI regressions (#61, #73, #74, #75)
- Fix failed turn error cards after watchdog recovery
- Guard orphan retirement against registered gateways
- Retire orphaned loopback gateways before restart
- Route extension subprocess APIs through jailed shell
- Improve dotenv environment handling
- Activate Git tool for nested repositories
- feat(git): gate commits through verification hooks
- docs(bridge): Prefer JSON in Bridge extension docs
- fix: enforce GraalVM pin consistency
- chore: update GraalVM and extension runtime
- Smooth terminal result scrolling
- Improve extension configuration and tools
- Refresh shell bindings after settings changes
- Respect disabled shell toggle in sub-agents
- Restore transcript layout stabilization
- Limit NTR browsing to latest turn
- Simplify companion reconnect and transcript behavior
- Improve compaction guidance and retry diagnostics
- feat(build): install the pinned GraalVM CE automatically when it is missing
- fix(companion): stabilize transcript rotation
- refactor(prompt,fold): Clarify instructions and advertise only the five newest NTR entries
- fix(provider-error): Attribute injected tool fields to the gateway rather than a Vis schema
- feat(titling): Generate LLM titles after turns through a separate route
- test(loop): the deferred title upgrade is after-turn-auto-title! (#71)
- feat(titling): configurable session titling, deferred past the foreground turn (#71)
- test(loop): widen the observation-batch concurrency margin for loaded runners
- test(tui): de-flake the live-progress layout budget on shared runners
- chore(audit): regenerate audit/README.md for svar 0.7.88
- fix(cli): describe vis as a coding agent, not a "Recursive Language Model"
- feat(config): per-provider `is_stateless` for gateways that reject replayed item ids
- release: update version files for v0.1.13, bump to next dev version
- chore(audit): regenerate the dependency inventory (ruff 0.3.2, svar 0.7.86)

### Package changes

#### com.blockether/vis
- fix(editing): Preserve newlines in structural edits and support comment documentation across 28 languages (edcac200a)
- Record the 0.1.14 (2815) release notes (e50c94af9)
- Cover turn attachments with tests and note 0.1.14 in the changelog (060fe81df)
- Bump tree-sitter-language-pack to 1.12.3-blockether.32 (e3b729f13)
- Serve a turn's inline attachments and hide the footer mid-turn (29a05339a)
- Bump tree-sitter-language-pack to 1.12.3-blockether.31 (f6881c9dd)
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache live turn content for immediate rendering on session re-entry (ece994fa2)
- docs(companion): release notes for 0.1.14 (2808) (96563d153)
- Let the companion app change the reasoning mode (f567648ee)
- Adopt already-running turns in the companion session screen (2dd3ad3ba)
- Add PRIVACY.md for the companion app (Play store policy URL) (4ea71f8a3)
- Ensure turns emit terminal events and bound Python GC (5bb959dd7)
- Allow block-local shadowing of bound tool names in vis Python (88bbea7eb)
- docs(companion): release notes for 0.1.14 (2802) (971591d06)
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)
- Fix gateway, Python, Git, and TUI regressions (#61, #73, #74, #75) (571e761d4)
- Fix failed turn error cards after watchdog recovery (c9b84ab7e)
- Guard orphan retirement against registered gateways (21cb13822)
- Retire orphaned loopback gateways before restart (bc49a4636)
- Route extension subprocess APIs through jailed shell (17808cf25)
- Improve dotenv environment handling (f9202278d)
- Activate Git tool for nested repositories (028fcce96)
- feat(git): gate commits through verification hooks (826c528c5)
- fix: enforce GraalVM pin consistency (dfece5942)
- chore: update GraalVM and extension runtime (fe6d2949a)
- Smooth terminal result scrolling (56568baa0)
- Improve extension configuration and tools (31a974b4b)
- Refresh shell bindings after settings changes (eef369280)
- Respect disabled shell toggle in sub-agents (a3ff72111)
- Restore transcript layout stabilization (66da8bf72)
- Limit NTR browsing to latest turn (ce8f20afa)
- Simplify companion reconnect and transcript behavior (bf8988086)
- Improve compaction guidance and retry diagnostics (c39edc0b8)
- feat(build): install the pinned GraalVM CE automatically when it is missing (956cb66aa)
- fix(companion): stabilize transcript rotation (636ea5af0)
- refactor(prompt,fold): Clarify instructions and advertise only the five newest NTR entries (df93fefef)
- fix(provider-error): Attribute injected tool fields to the gateway rather than a Vis schema (7abfd121a)
- feat(titling): Generate LLM titles after turns through a separate route (a696d8d2d)
- test(loop): the deferred title upgrade is after-turn-auto-title! (#71) (ec1c01f38)
- feat(titling): configurable session titling, deferred past the foreground turn (#71) (eb0b6a793)
- test(loop): widen the observation-batch concurrency margin for loaded runners (3f1fe723c)
- chore(audit): regenerate audit/README.md for svar 0.7.88 (dbe920b6b)
- fix(cli): describe vis as a coding agent, not a "Recursive Language Model" (0e737768d)
- feat(config): per-provider `is_stateless` for gateways that reject replayed item ids (41f87ea34)
- release: update version files for v0.1.13, bump to next dev version (2a35e648b)
- chore(audit): regenerate the dependency inventory (ruff 0.3.2, svar 0.7.86) (af62949a3)

#### com.blockether/vis-channel-tui
- Cache live turn content for immediate rendering on session re-entry (ece994fa2)
- Allow block-local shadowing of bound tool names in vis Python (88bbea7eb)
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)
- Fix gateway, Python, Git, and TUI regressions (#61, #73, #74, #75) (571e761d4)
- Fix failed turn error cards after watchdog recovery (c9b84ab7e)
- feat(git): gate commits through verification hooks (826c528c5)
- chore: update GraalVM and extension runtime (fe6d2949a)
- Smooth terminal result scrolling (56568baa0)
- Improve extension configuration and tools (31a974b4b)
- Improve compaction guidance and retry diagnostics (c39edc0b8)
- test(tui): de-flake the live-progress layout budget on shared runners (ca49b15e0)

#### com.blockether/vis-foundation-bridge
- feat(git): gate commits through verification hooks (826c528c5)
- docs(bridge): Prefer JSON in Bridge extension docs (f8e973098)

#### com.blockether/vis-foundation-search
- Cache live turn content for immediate rendering on session re-entry (ece994fa2)
- Improve extension configuration and tools (31a974b4b)

#### com.blockether/vis-foundation-voice
- Cache live turn content for immediate rendering on session re-entry (ece994fa2)

#### com.blockether/vis-language-clojure
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache live turn content for immediate rendering on session re-entry (ece994fa2)
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)

#### com.blockether/vis-language-python
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache live turn content for immediate rendering on session re-entry (ece994fa2)
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)

#### com.blockether/vis-language-typescript-bun
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache live turn content for immediate rendering on session re-entry (ece994fa2)

#### com.blockether/vis-persistance-sqlite
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)
- chore: update GraalVM and extension runtime (fe6d2949a)
- Limit NTR browsing to latest turn (ce8f20afa)

### Added

- Bridge exact-candidate options in `br/check` and `br/run-evidence`.
- Add a shared fail-closed :git/commit operation for the Git tool and TUI Magit. Resolve repository options, reject index-changing commit forms, recheck staged content and verify the resulting tree. Bridge supplies the lifecycle approval hook.

### Changed

- `vis-foundation-bridge` now targets the Bridge 0.2.2
  candidate-verification API.
- tree-sitter-language-pack 1.12.3-blockether.34 preserves final newlines and CRLF endings across structural edits and replaces only non-whitespace node content. Support add_doc/replace_doc for 26 languages that use comment documentation.

### Fixed

- `struct_patch` moves no longer drop the file's trailing newline (or `\r`):
  the structural editor now splits lines without collapsing the final empty
  line, and only collapses a seam when there is one.

## [v0.1.13] - 2026-07-29

### Changed
- chore(deps): svar 0.7.86 treats exhausted quota, credit and budget as non-retryable errors
- feat(companion): collapse recorded non-image attachments into one disclosure row
- feat(python): read packaging metadata with Python's own parsers, add `python.source_paths`
- fix(companion): a dead event stream can no longer silently freeze an open session
- feat(python): ruff config discovery, `vis python -m ruff`, formatted shims
- feat(python): ruff-powered format_code/lint_code for the Python pack
- feat(python): infer more src-layout import roots for vis python (#62)
- fix(sandbox): stop losing the real Python error on warm contexts
- fix(gateway): avoid holding the state lock across session work
- feat(companion): expire superseded TestFlight builds
- fix(sandbox): always grant the ~/.vis session folder in the engine
- feat(introspection): gate session self-inspection behind a toggle
- docs(companion): TestFlight notes for 0.1.13 (2755)
- feat(provider-error): classify a too-small output-token budget
- fix(reload): re-hydrate feature toggles from config on /reload
- refactor(tools): always respect .gitignore; config-only filesystem + search scope
- docs(companion): TestFlight notes for 0.1.13 (2751)
- feat: add a web-search toggle, typed extension schemas and accurate Python CLI exit codes
- refactor(tools): use ranges-only line windows
- test(shims): cover deferred shim dependency loading
- test(loop): tolerate runner JIT activity after timeout
- docs(audit): refresh generated dependency audit
- fix(ci): size macOS heap for Truffle suite
- test(jail): align contract with supported runtime
- fix(ci): probe Linux jail capability before E2E
- fix(audit): generate inventory date in UTC
- fix(ci): prepare generated Android project before Gradle
- docs(audit): refresh dependency inventory
- docs(companion): TestFlight notes for 0.1.13 (2739)
- fix(python): link extension shims statically
- docs(companion): TestFlight notes for 0.1.13 (2737)
- fix(native): arm AWT headless at runtime, not at image-build time
- build(companion): one version everywhere, from the repo-root VERSION
- feat(queue): cancelled turn returns queued messages to the input
- shims: move every sandbox shim's Python into real .py resources
- companion: release notes for 1.0.1 (2732)
- attachments: drop image optimization, keep container conversion only
- providers: add OpenRouter and persist auth files in snake_case
- release: notes for 1.0.1 (2729)
- companion: native iOS viewport bridge for rotation and resume
- cli: make `vis update` explain and recover from diverged history (#53)
- companion: clamp the shell to the device when iOS resumes an oversized webview
- release: notes for 1.0.1 (2725)
- companion cold-open cache, justified fold cards, config-driven model pick
- release: notes for 1.0.1 (2723)
- companion: kill rotation layout thrash, restore justified prose
- release: notes for 1.0.1 (2721)
- companion: request/transcription deadlines, durable voice outbox, rotation + typography fixes
- fix(shell): allow host-root descendants outside jail
- companion: coalesced tool-card grids, justified prose, correct live ticker
- release: notes for 1.0.1 (2717)
- companion: Companion: accurate live status, coordinated keyboard movement and resume at the end
- companion: one-motion iOS keyboard; TUI limits, shims, editing fixes
- labelled ntr recovery, image optimization at ingest, companion back/paste/perf
- release: update version files for v0.1.12, bump to next dev version

### Package changes

#### com.blockether/vis
- chore(deps): svar 0.7.86 treats exhausted quota, credit and budget as non-retryable errors (02252578e)
- feat(companion): collapse recorded non-image attachments into one disclosure row (9dba6166e)
- feat(python): read packaging metadata with Python's own parsers, add `python.source_paths` (f8ac1d4ec)
- fix(companion): a dead event stream can no longer silently freeze an open session (c229e28d5)
- feat(python): ruff config discovery, `vis python -m ruff`, formatted shims (83082fdb9)
- feat(python): ruff-powered format_code/lint_code for the Python pack (99352e9a6)
- feat(python): infer more src-layout import roots for vis python (#62) (8a0a4751d)
- fix(sandbox): stop losing the real Python error on warm contexts (655a6586d)
- fix(gateway): avoid holding the state lock across session work (3b4363478)
- feat(companion): expire superseded TestFlight builds (922f6ca97)
- fix(sandbox): always grant the ~/.vis session folder in the engine (edbdb1375)
- feat(introspection): gate session self-inspection behind a toggle (38db934ae)
- docs(companion): TestFlight notes for 0.1.13 (2755) (db43ca288)
- feat(provider-error): classify a too-small output-token budget (8f20677e1)
- fix(reload): re-hydrate feature toggles from config on /reload (75893813a)
- refactor(tools): always respect .gitignore; config-only filesystem + search scope (6f89aaa01)
- docs(companion): TestFlight notes for 0.1.13 (2751) (4966f3233)
- feat: add a web-search toggle, typed extension schemas and accurate Python CLI exit codes (a9ee2d552)
- refactor(tools): use ranges-only line windows (136c408dd)
- test(shims): cover deferred shim dependency loading (df5a6b7c1)
- test(loop): tolerate runner JIT activity after timeout (522367ffa)
- docs(audit): refresh generated dependency audit (a68962e34)
- fix(ci): size macOS heap for Truffle suite (e0da867db)
- test(jail): align contract with supported runtime (7218a90c2)
- fix(ci): probe Linux jail capability before E2E (2df6f1102)
- fix(audit): generate inventory date in UTC (3d53adf52)
- fix(ci): prepare generated Android project before Gradle (69bfc6842)
- docs(audit): refresh dependency inventory (42a8c63e4)
- docs(companion): TestFlight notes for 0.1.13 (2739) (98d98e7b3)
- fix(python): link extension shims statically (5d7cf4c0a)
- docs(companion): TestFlight notes for 0.1.13 (2737) (606941f36)
- fix(native): arm AWT headless at runtime, not at image-build time (8bcec2f9d)
- build(companion): one version everywhere, from the repo-root VERSION (6fc89832e)
- feat(queue): cancelled turn returns queued messages to the input (505bb13d8)
- shims: move every sandbox shim's Python into real .py resources (ce39c555b)
- companion: release notes for 1.0.1 (2732) (cbe597b9d)
- attachments: drop image optimization, keep container conversion only (b8e0e9875)
- providers: add OpenRouter and persist auth files in snake_case (3ee3993c6)
- release: notes for 1.0.1 (2729) (a1152ddff)
- companion: native iOS viewport bridge for rotation and resume (ffdb768f5)
- cli: make `vis update` explain and recover from diverged history (#53) (f369fdaed)
- companion: clamp the shell to the device when iOS resumes an oversized webview (1b7da420f)
- release: notes for 1.0.1 (2725) (2bc67ac68)
- companion cold-open cache, justified fold cards, config-driven model pick (80f263b81)
- release: notes for 1.0.1 (2723) (1911c5883)
- companion: kill rotation layout thrash, restore justified prose (d9c943e47)
- release: notes for 1.0.1 (2721) (5340873cd)
- companion: request/transcription deadlines, durable voice outbox, rotation + typography fixes (f11835f3f)
- fix(shell): allow host-root descendants outside jail (e226f48d2)
- companion: coalesced tool-card grids, justified prose, correct live ticker (d8bd17eb2)
- release: notes for 1.0.1 (2717) (406756439)
- companion: Companion: accurate live status, coordinated keyboard movement and resume at the end (445a3b2d4)
- companion: one-motion iOS keyboard; TUI limits, shims, editing fixes (154a0e343)
- labelled ntr recovery, image optimization at ingest, companion back/paste/perf (f6a109f89)
- release: update version files for v0.1.12, bump to next dev version (f07ee491c)

#### com.blockether/vis-channel-tui
- refactor(tools): always respect .gitignore; config-only filesystem + search scope (6f89aaa01)
- feat(queue): cancelled turn returns queued messages to the input (505bb13d8)
- attachments: drop image optimization, keep container conversion only (b8e0e9875)
- companion cold-open cache, justified fold cards, config-driven model pick (80f263b81)
- companion: one-motion iOS keyboard; TUI limits, shims, editing fixes (154a0e343)

#### com.blockether/vis-foundation-search
- feat: add a web-search toggle, typed extension schemas and accurate Python CLI exit codes (a9ee2d552)

#### com.blockether/vis-language-clojure
- feat(queue): cancelled turn returns queued messages to the input (505bb13d8)

#### com.blockether/vis-language-python
- feat(python): ruff config discovery, `vis python -m ruff`, formatted shims (83082fdb9)
- feat(python): ruff-powered format_code/lint_code for the Python pack (99352e9a6)

#### com.blockether/vis-persistance-sqlite
- refactor(tools): always respect .gitignore; config-only filesystem + search scope (6f89aaa01)
- companion: request/transcription deadlines, durable voice outbox, rotation + typography fixes (f11835f3f)
- labelled ntr recovery, image optimization at ingest, companion back/paste/perf (f6a109f89)

#### com.blockether/vis-provider-anthropic
- providers: add OpenRouter and persist auth files in snake_case (3ee3993c6)

#### com.blockether/vis-provider-github-copilot
- providers: add OpenRouter and persist auth files in snake_case (3ee3993c6)
- companion: one-motion iOS keyboard; TUI limits, shims, editing fixes (154a0e343)
- labelled ntr recovery, image optimization at ingest, companion back/paste/perf (f6a109f89)

#### com.blockether/vis-provider-openai-codex
- providers: add OpenRouter and persist auth files in snake_case (3ee3993c6)

#### com.blockether/vis-provider-openrouter
- providers: add OpenRouter and persist auth files in snake_case (3ee3993c6)

#### com.blockether/vis-provider-zai
- providers: add OpenRouter and persist auth files in snake_case (3ee3993c6)

## [v0.1.12] - 2026-07-28

### Changed
- feat(config): provider `compatibility` key, svar model limits, vis.yml model order
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format
- session_fold: accept keyword arguments from the Python sandbox
- config: name the failing fields and stop dumping a stack trace
- gateway state test: verify over-budget turns are retained rather than deferred
- release notes: keep one preamble in CHANGELOG.md, human-readable 2707 entry
- Restore images on transcript reload, smooth rotation, auto TestFlight notes
- shell logs: return the tail once as plain strings
- docs(language-surface): note lint_code also reports reflection + boxed-math
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass
- Tailscale address preference, persisted draft messages, rotation-aware viewport, shell send keystroke labels, nippy shim merge
- fix(companion,gateway): resume/keyboard/scroll fixes, unread marks, swipe actions
- fix(tui): preserve terminal cancellation notice
- fix(bridge): unblock nested workspace searches
- feat(bridge): discover projects across workspace sessions
- refactor(bridge): remove redundant next surface
- Native QR scanning on iOS, wider pairing bind, companion fixes
- feat(gateway,companion): self-configuring pairing, loopback mirror, and answer-bearing push
- release: update version files for v0.1.11, bump to next dev version

### Package changes

#### com.blockether/vis
- feat(config): provider `compatibility` key, svar model limits, vis.yml model order (1c2478678)
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format (18cadf1f9)
- session_fold: accept keyword arguments from the Python sandbox (1fbad6b56)
- config: name the failing fields and stop dumping a stack trace (5d270615b)
- gateway state test: verify over-budget turns are retained rather than deferred (d361085f1)
- release notes: keep one preamble in CHANGELOG.md, human-readable 2707 entry (3df9a77cb)
- Restore images on transcript reload, smooth rotation, auto TestFlight notes (16c287e2e)
- shell logs: return the tail once as plain strings (9c94b083a)
- docs(language-surface): note lint_code also reports reflection + boxed-math (27e66a4ba)
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)
- Tailscale address preference, persisted draft messages, rotation-aware viewport, shell send keystroke labels, nippy shim merge (89ffa1761)
- fix(companion,gateway): resume/keyboard/scroll fixes, unread marks, swipe actions (c8cface3a)
- fix(bridge): unblock nested workspace searches (64637a76c)
- feat(bridge): discover projects across workspace sessions (4e4560352)
- Native QR scanning on iOS, wider pairing bind, companion fixes (e710ee571)
- feat(gateway,companion): self-configuring pairing, loopback mirror, and answer-bearing push (12be0911e)
- release: update version files for v0.1.11, bump to next dev version (53c36bbf4)

#### com.blockether/vis-channel-tui
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format (18cadf1f9)
- session_fold: accept keyword arguments from the Python sandbox (1fbad6b56)
- config: name the failing fields and stop dumping a stack trace (5d270615b)
- Restore images on transcript reload, smooth rotation, auto TestFlight notes (16c287e2e)
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)
- Tailscale address preference, persisted draft messages, rotation-aware viewport, shell send keystroke labels, nippy shim merge (89ffa1761)
- fix(companion,gateway): resume/keyboard/scroll fixes, unread marks, swipe actions (c8cface3a)
- fix(tui): preserve terminal cancellation notice (8a25d5e84)
- feat(bridge): discover projects across workspace sessions (4e4560352)
- Native QR scanning on iOS, wider pairing bind, companion fixes (e710ee571)
- feat(gateway,companion): self-configuring pairing, loopback mirror, and answer-bearing push (12be0911e)

#### com.blockether/vis-foundation-bridge
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format (18cadf1f9)
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)
- fix(bridge): unblock nested workspace searches (64637a76c)
- feat(bridge): discover projects across workspace sessions (4e4560352)
- refactor(bridge): remove redundant next surface (102d26a1f)

#### com.blockether/vis-foundation-search
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)

#### com.blockether/vis-foundation-voice
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format (18cadf1f9)
- config: name the failing fields and stop dumping a stack trace (5d270615b)
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)
- Tailscale address preference, persisted draft messages, rotation-aware viewport, shell send keystroke labels, nippy shim merge (89ffa1761)

#### com.blockether/vis-language-clojure
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)

#### com.blockether/vis-language-python
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format (18cadf1f9)

#### com.blockether/vis-language-typescript-bun
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format (18cadf1f9)

#### com.blockether/vis-persistance-sqlite
- turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format (18cadf1f9)
- config: name the failing fields and stop dumping a stack trace (5d270615b)
- Restore images on transcript reload, smooth rotation, auto TestFlight notes (16c287e2e)
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)
- Tailscale address preference, persisted draft messages, rotation-aware viewport, shell send keystroke labels, nippy shim merge (89ffa1761)

#### com.blockether/vis-provider-anthropic
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)

#### com.blockether/vis-provider-github-copilot
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)

#### com.blockether/vis-provider-openai-codex
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)

#### com.blockether/vis-provider-zai
- Lint clean, dependency refresh, repo-wide format and top-level spacing pass (947e61281)

### Changed
- feat(workspace): move `/draft-blank` under the draft tree as `/draft blank <label>`

## [v0.1.11] - 2026-07-27

### Changed
- fix(native): give the arm64 builder the heap it measurably needs
- feat(gateway): protocol version handshake + compatibility verdict
- feat(config,ui): ${VAR} config references, provider-level env gaps, drop "gateway" from user-facing copy
- eval: Improve senior SWE bench reporting
- feat(tui): dissolve the transcript in when a session opens
- release: update version files for v0.1.10, bump to next dev version

### Package changes

#### com.blockether/vis
- fix(native): give the arm64 builder the heap it measurably needs (5f973b184)
- feat(gateway): protocol version handshake + compatibility verdict (ea2226448)
- feat(config,ui): ${VAR} config references, provider-level env gaps, drop "gateway" from user-facing copy (796c917ad)
- eval: Improve senior SWE bench reporting (4743d9263)
- release: update version files for v0.1.10, bump to next dev version (10c022eef)

#### com.blockether/vis-channel-tui
- feat(gateway): protocol version handshake + compatibility verdict (ea2226448)
- feat(config,ui): ${VAR} config references, provider-level env gaps, drop "gateway" from user-facing copy (796c917ad)
- feat(tui): dissolve the transcript in when a session opens (708e19504)

## [v0.1.10] - 2026-07-27

### Changed
- fix(native): release Linux x64 and arm64 binaries from CI and remove the unsupported macOS job
- release: update version files for v0.1.9, bump to next dev version

### Package changes

#### com.blockether/vis
- fix(native): release Linux x64 and arm64 binaries from CI and remove the unsupported macOS job (005b9b806)
- release: update version files for v0.1.9, bump to next dev version (d00e41718)

## [v0.1.9] - 2026-07-27

### Changed
- fix(native): build macOS on the 14 GB Intel runner, delete stale target/vis
- feat(companion): header-first python card and live transcript entrance
- fix(loop): price fold weights from the visible wire projection
- fix(shell): normalize captured terminal output before fencing
- fix(tui): make the code-band accordion reachable and header-first
- release: update version files for v0.1.8, bump to next dev version

### Package changes

#### com.blockether/vis
- fix(native): build macOS on the 14 GB Intel runner, delete stale target/vis (9883cb0c4)
- feat(companion): header-first python card and live transcript entrance (b9263165f)
- fix(loop): price fold weights from the visible wire projection (c4aa24331)
- fix(shell): normalize captured terminal output before fencing (a386197d2)
- release: update version files for v0.1.8, bump to next dev version (44518611d)

#### com.blockether/vis-channel-tui
- fix(tui): make the code-band accordion reachable and header-first (928ab62ea)

## [v0.1.8] - 2026-07-27

### Changed
- ci(native): restore workflow, community-only, macOS quick-build (-Ob)
- feat: improve companion pairing and context rendering
- feat(copilot): use svar 0.7.84 current models
- Improve transcript previews and runtime reliability
- build(native): drop the corporate profile — community-only distributions
- release: update version files for v0.1.7, bump to next dev version

### Package changes

#### com.blockether/vis
- ci(native): restore workflow, community-only, macOS quick-build (-Ob) (7d19345a3)
- feat: improve companion pairing and context rendering (43c935dba)
- feat(copilot): use svar 0.7.84 current models (3fc447867)
- Improve transcript previews and runtime reliability (5162cee30)
- build(native): drop the corporate profile — community-only distributions (02767a32e)
- release: update version files for v0.1.7, bump to next dev version (135117911)

#### com.blockether/vis-channel-tui
- feat: improve companion pairing and context rendering (43c935dba)
- Improve transcript previews and runtime reliability (5162cee30)

#### com.blockether/vis-language-clojure
- Improve transcript previews and runtime reliability (5162cee30)

#### com.blockether/vis-persistance-sqlite
- Improve transcript previews and runtime reliability (5162cee30)

#### com.blockether/vis-provider-github-copilot
- feat(copilot): use svar 0.7.84 current models (3fc447867)
- Improve transcript previews and runtime reliability (5162cee30)

## [v0.1.7] - 2026-07-27

### Changed
- ci: provision swap for corporate native image
- release: update version files for v0.1.6, bump to next dev version

### Package changes

#### com.blockether/vis
- ci: provision swap for corporate native image (2bdc2096a)
- release: update version files for v0.1.6, bump to next dev version (f977db0da)

## [v0.1.6] - 2026-07-27

### Changed
- fix(native): bound GraalPy analysis memory
- Harden prompts, persistence, and mobile push delivery
- Improve compaction persistence and agent guidance
- fix(persistence): restore canonical assistant blocks
- release: update version files for v0.1.5, bump to next dev version

### Package changes

#### com.blockether/vis
- fix(native): bound GraalPy analysis memory (1a55017b7)
- Harden prompts, persistence, and mobile push delivery (20f149d89)
- Improve compaction persistence and agent guidance (a5da90763)
- fix(persistence): restore canonical assistant blocks (8e2e145b1)
- release: update version files for v0.1.5, bump to next dev version (4316b08de)

#### com.blockether/vis-persistance-sqlite
- Improve compaction persistence and agent guidance (a5da90763)
- fix(persistence): restore canonical assistant blocks (8e2e145b1)

## [v0.1.5] - 2026-07-26

### Changed
- feat: harden context handling and push notifications
- fix(runtime): rescue provider auth failures
- fix(companion): prepare manual iOS archives
- fix(loop): let repeated actions continue
- feat(companion): automate TestFlight releases
- fix: harden gateway compatibility and runtime lifecycle
- fix(gateway): validate transcript window parameters
- feat: page companion transcripts and clarify shell calls
- fix: streamline queued polling and session surfaces
- test: synchronize interrupted gather child
- fix: harden session recovery and provider surfaces
- fix(ci): restore local provider presets
- fix(tui): keep navigator scroll arithmetic primitive
- feat: harden queues, tools, and companion
- fix(gateway): refresh stall watchdog on real progress
- companion: canonical type scale, line-height in the scale, taller session header
- perf(search): delegate all ignore rules to fff and remove the Clojure filesystem walk
- fix(queue): images in queued turns render as chips, not raw paths
- fix(companion): smaller composer text and narrower +/mic buttons on phone
- fix(companion): center the model caption below the composer and add spacing from the screen edge
- perf(companion): cache /v1/router for 5 minutes and prefetch it at connect
- fix(companion): New session back to solid primary; Share chip primary
- fix(companion): model badge spans the composer width
- fix(companion): model badge centered below the composer box, smaller
- fix(companion): smaller Button chrome; sessions header actions share one palette
- fix(companion): shrink session header chrome; move model picker under the composer as a badge
- fix(companion): shrink composer + chrome — 36px icon buttons, smaller input text, 12px base
- style(companion): bold button labels, smaller buttons and input text
- fix(companion): point Capacitor ios.scheme at the real Xcode scheme
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning
- perf(search): canonical pooled-fff search path
- fix(lint): clear every eslint + clj-kondo finding
- feat(tui): flatten dialog, footer, and button chrome onto terminal background
- Show authenticated OAuth providers in Router/Models manager
- deps: bump svar to 0.7.77 (claude-opus-5)
- Upstream vis config
- Move network egress config under jail.network, gated by jail.enabled
- Fix fold-card savings note breaking intent recording; type levels param
- Fold card always reports savings, even a no-op re-fold
- chore: sync working tree
- fix(companion): point session search client at /v1/sessions/actions/search
- fix(sandbox): align config-spec test + docs to jail.enabled rename (off by default)
- Session search: title-first transcript preview across TUI and Companion
- Group GitHub Copilot tiers and filter to active variant (#47, #48)
- chore: working-tree updates (companion screens, copilot provider)
- Merge remote-tracking branch 'origin/main'
- Normalize toggle values to strings; surface config-denial hint in access view
- chore(tui): channel-tui dialog/test updates
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.23
- vis-companion: TS7-compatible capacitor config codegen + build:ios
- chore(sandbox): sandbox off by default; config-spec rooted paths + deny-exec + ports; net-probe; docs + repo-wide reformat
- Fix lint warning in oauth_test and stage pending workspace changes
- Fixes
- feat(providers): surface & route authenticated OAuth providers; bound gather pool
- Suggest Tailscale IP when pairing a loopback-bound gateway
- feat(streaming): distinct native-call preview phase + svar 0.7.75
- fix(streaming): render native tool previews distinctly
- fix(tui): scope Codex verbosity to session provider
- chore(deps): bump tree-sitter-language-pack .20 -> .21 (TSX value bindings + arities)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.20
- feat(editing): struct_index range/ranges windows + resilient edit-kind resolution
- feat(editing): fold rg into find_files (name+content search); assorted WIP
- feat(sandbox): per-session Seatbelt jail + gateway MITM egress proxy
- chore(deps): bump svar to 0.7.73 (health-gated connect-blip retry)
- chore: remove committed transcript scratch artifacts
- Fix ForeignObject leak in session_state transcript projection
- feat(transcript): canonical /export dialog with gateway md+html routes
- feat(transcript): surface python stdout and attachments in transcript (#40)
- fix(env-python): make tool failures catchable in-block (#42)
- Fix Blockether light theme tab colors and dialog margins
- fix(paramiko): drop shared MINA SSHD io-factory so every server negotiates
- Render fixes
- fix(channel-tui): restore result-row copy alignment for pre-indented output
- fix(paramiko-shim): self-reap MINA sshd host-side on session close
- fix(gateway/bus): treat turn.cancelled as terminal in hydrate!
- fix(channel-tui): declare flatland/ordered dep + align queue-close test
- fix(prompt): gate user-pasted images on model vision + steer PIL fallback
- deps: bump com.blockether/bridge 0.1.2 -> 0.2.1
- Fix cat tool to accept all-kwargs spec map with path key
- fix(editing): steer append_child/prepend_child used with a NAME locator
- fix: preserve summary-only printed results
- deps: bump svar 0.7.69 -> 0.7.70 (transient-message classifier: statusless/wrapper/gRPC transients + mid-stream code casing)
- fix(tui): Linux clipboard OSC 52 fallback + scroll-safe transcript copy
- Remove Piper TTS (speaking) from voice extension
- Fix the errors
- wip: in-progress vis changes
- chore(deps): bump com.blockether/svar 0.7.66 -> 0.7.67 (catalog-driven Copilot vision)
- chore(deps): bump com.blockether/svar 0.7.65 -> 0.7.66
- Optimizations
- Render doc/apropos native cards as authored markdown
- refactor(lint): drop redundant coercions; suppress clj-kondo false positives
- style: blank line between all top-level forms
- feat(clojure-format): report which backend formatted each result
- style: full-project cljfmt reformat
- fix(tui-magit): make status keybindings faithful to vanilla magit
- fix(clojure-test): relativize fault file paths in repl-mode test output
- fix(python-cli): wire caller stdin into CLI context so -c/FILE can read sys.stdin
- fix(self-docs): vis_docs both arities return {"pages": [...]} shape
- Fixes to anchored editing
- fix(provider): name rejected tool schemas
- fix(tools): reject nonportable schema roots
- fix(prompt): enforce compact actionable style
- perf(tui): drop lazy-mapcat counter atom in list->lines; StringBuilder CLI lists
- docs(extensions): prefer native agent tools
- perf(render): drop per-list counter atoms for pure map-indexed
- test(tui): recording proxies reconstruct pre-segmented styled runs
- Performance improvements
- perf(tui): single StringBuilder pass in run->sentinel-segment
- Performance fixes for TUI!
- perf(tui): halve styled-line paint allocation via sentinel split
- perf(tui): skip grapheme array on plain lines in paint-styled-line!
- Bump lanterna
- deps(tui): bump com.blockether/lanterna to 3.1.5-vis.32
- Improve compiler error reporting
- perf(tui): identity-memoize layout height-key vector
- fix(clj-test): honor run_tests :dir so sibling-project tests hit their own nREPL
- perf(tui): O(log n) binary-search visible-window for scroll layout
- perf(tui): replace pass-1 est mapv with primitive transient loop
- perf(tui): cache last-bubble tail projection; coalesce tab-switch refresh
- Performance fixes
- fix(prompt): resolve guidance and error rendering regressions
- Fixes to rendering
- feat: refine prompts, errors, and TUI caching
- Stage changes to prompts
- Prompt changes, tui optimizations
- Fixes to highlighting
- Test runner fixes
- Gateway fixes
- Update language surface contract
- Fixes to prompt and python env
- Optimize prompt
- test: align failing tests with current code
- style: reformat with zprint (.zprint.edn) across src, extensions, test
- chore: scrub residual telegram & web-channel mentions from comments and docs
- refactor(gateway): resolve workspace via live-env fallback so root ops work pre-first-turn
- chore(channels): remove telegram bot channel
- fix(gateway): decouple SSE delivery from the turn thread via bounded per-connection queues
- refactor(telegram): require transcript directly, drop requiring-resolve
- fix(transcript): make HTML session export work without channel-web
- fix(locks): drop obsolete call-py monitor, bound close!/close-all! turn-lock waits
- chore(channels): remove web channel and react-native companion
- refactor(env-python): one session context, one shared engine — pure-JVM renderer, in-context parser
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.18
- fix(outline): surface the native cause, not opaque "FFI call failed"
- fix(env-python): freeze-proof every GraalPy context + dict-safe ntr results
- docs: add full security audit (AUDIT.md)
- feat(loop): self-heal empty model replies via svar 0.7.63 same-model resend
- Revert "fix(security): bound gateway JSON request body to 4 MiB (#6)"
- fix(security): bound gateway JSON request body to 4 MiB (#6)
- fix(security): allowlist markdown link schemes in server render (#2)
- fix(editing): bound nearest-existing-dir climb to allowed roots; drop stale ls test
- feat(editing): climb missing search paths to nearest existing dir + report missing_paths
- refactor(rg): search paths as named (file OR dir), skip missing
- fix(security): harden credential storage and token comparison (Batch A)
- fix(python): seed apropos/doc for aliased extension symbols; apropos returns {name: gist} dict; clarify mcp double- vs single-underscore naming
- chore(deps): update dependencies via antq
- chore(deps): bump com.blockether/fff to 0.10.0-2
- fix(editing): rg/find resolve paths to nearest existing directory instead of erroring
- fix(clojure): reuse one nREPL session per connection + eval-based (+ 1 1) health check in ctx
- refactor(repl): remove bin/dev launcher and clj_repl references
- fix(clojure): fail eval fast on nREPL eval-error, interrupt the eval on timeout
- feat(language-surface): aggregate lint findings by path
- fix(channel-tui): keep the cost/token bubble footer on a cancelled turn that spent tokens
- fix(loop): normalize svar 0.7 canonical token usage + append-only live tool-code fence
- test(language-clojure): cover blank snippet defaults
- feat(introspection): include session modified time
- fix(language-clojure): ignore blank snippet defaults
- fix(channel-tui): expand tabs before markdown wrapping
- feat(paramiko): support key generation in shim
- fix(channel-tui): preserve account plan quota windows
- docs(ctx-engine): clarify compact-src docstring
- feat(channel-tui): tint diff fence add/del rows and fix scrollbar flicker
- refactor(foundation-search): drop Exa MCP env toggles from extension registration
- feat(channel-web): colorize language-diff fences in static export
- fix(channel-tui): read gateway project fields by string key
- docs(fold): require full workspace-relative path:line anchors in gists
- big refactor
- feat(python-sandbox): auto-settle deferred tool calls on subscript/len/in
- fix(tui): wire Esc to clear the pending-sends queue
- fix(clojure): discard unresponsive nREPL connections after evaluation timeout
- feat(python-cli): forward script argv and caller env into vis python sandbox
- fix(channel-tui): reserve exact inline-image box from real terminal cell size
- fix(tui): stop slash/file suggestion overlay flicker during live stream
- fix(tui): ellipsize bubble footer meta line on narrow terminals
- fix(shim-matplotlib): resolve per-element hex color lists without float() crash
- fix(loop): eliminate boxed-math + recur-primitive warnings in re-entrant park/retry loop
- fix(channel-tui): gate provider-limits poll resolve to cut idle CPU
- fix(transcript): fence folded-gist body so it renders verbatim
- fix(clojure-test-runner): silence framework reporter, structured failure digest
- feat(self-docs): render vis_docs op-card as a table
- perf(providers): cache fleet enumeration for footer-frequency reads (#29)
- docs(readme): correct java prereq to 25+
- chore(docs): drop Windows/PowerShell support from docs, readme, and installers
- fix(test-runner): pr-str result under pinned print vars to survive truncating nREPL sessions
- fix(self-docs): treat blank/absent vis_docs slug as list request
- fix(gateway): parse /v1/events sids as UUIDs and persist cancel stamp
- feat(editing): name the searched directory scope in rg & find_files op-card headlines
- fix(loop): make the native-tool park re-entrant so nested run_tests keeps its budget
- fix(compaction): show fold card savings as % of window, not a rising projected level
- docs(channel-tui): document queue, cancel & Ctrl+C behavior
- feat(compaction): project next-request % on fold cards and freeze unbounded fold selectors
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight

### Package changes

#### com.blockether/vis
- feat: harden context handling and push notifications (445e449fd)
- fix(runtime): rescue provider auth failures (2b51f2973)
- fix(companion): prepare manual iOS archives (d15887c94)
- fix(loop): let repeated actions continue (fd36551a1)
- feat(companion): automate TestFlight releases (17596e521)
- fix: harden gateway compatibility and runtime lifecycle (690669489)
- fix(gateway): validate transcript window parameters (bc23c3cb9)
- feat: page companion transcripts and clarify shell calls (9091fb3d1)
- fix: streamline queued polling and session surfaces (7c6bc8b19)
- test: synchronize interrupted gather child (24f5604a9)
- fix: harden session recovery and provider surfaces (96d755216)
- fix(ci): restore local provider presets (9ecddf6ac)
- feat: harden queues, tools, and companion (5d773f2cf)
- fix(gateway): refresh stall watchdog on real progress (553626fe1)
- companion: canonical type scale, line-height in the scale, taller session header (f095e603a)
- perf(search): delegate all ignore rules to fff and remove the Clojure filesystem walk (760197012)
- fix(queue): images in queued turns render as chips, not raw paths (5c5e4c6e5)
- fix(companion): smaller composer text and narrower +/mic buttons on phone (dc16e6641)
- fix(companion): center the model caption below the composer and add spacing from the screen edge (75869fb6f)
- perf(companion): cache /v1/router for 5 minutes and prefetch it at connect (90474b8ce)
- fix(companion): New session back to solid primary; Share chip primary (955a1fa32)
- fix(companion): model badge spans the composer width (e6d773d80)
- fix(companion): model badge centered below the composer box, smaller (043ee1209)
- fix(companion): smaller Button chrome; sessions header actions share one palette (2e76aa43c)
- fix(companion): shrink session header chrome; move model picker under the composer as a badge (e8bf1f0b8)
- fix(companion): shrink composer + chrome — 36px icon buttons, smaller input text, 12px base (c46fe10ba)
- style(companion): bold button labels, smaller buttons and input text (78651b508)
- fix(companion): point Capacitor ios.scheme at the real Xcode scheme (26d282c43)
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- perf(search): canonical pooled-fff search path (6e71ab1e5)
- fix(lint): clear every eslint + clj-kondo finding (dc0af88d3)
- feat(tui): flatten dialog, footer, and button chrome onto terminal background (3bbf8f888)
- Show authenticated OAuth providers in Router/Models manager (776afc717)
- deps: bump svar to 0.7.77 (claude-opus-5) (54ea9c8e2)
- Upstream vis config (4d4e34a10)
- Move network egress config under jail.network, gated by jail.enabled (20c3e3521)
- Fix fold-card savings note breaking intent recording; type levels param (b3aedb4d6)
- Fold card always reports savings, even a no-op re-fold (b4914e5cc)
- chore: sync working tree (b1a3ee0e1)
- fix(companion): point session search client at /v1/sessions/actions/search (ecf9b20d2)
- fix(sandbox): align config-spec test + docs to jail.enabled rename (off by default) (1e629070a)
- Session search: title-first transcript preview across TUI and Companion (762455312)
- Group GitHub Copilot tiers and filter to active variant (#47, #48) (00ef8a991)
- chore: working-tree updates (companion screens, copilot provider) (850cfb30f)
- Normalize toggle values to strings; surface config-denial hint in access view (379d5d9e1)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.23 (acffb332c)
- vis-companion: TS7-compatible capacitor config codegen + build:ios (945833600)
- chore(sandbox): sandbox off by default; config-spec rooted paths + deny-exec + ports; net-probe; docs + repo-wide reformat (82db9d860)
- Fix lint warning in oauth_test and stage pending workspace changes (5ca75032c)
- Fixes (0b734bad0)
- feat(providers): surface & route authenticated OAuth providers; bound gather pool (403e7982c)
- Suggest Tailscale IP when pairing a loopback-bound gateway (a60702ba5)
- feat(streaming): distinct native-call preview phase + svar 0.7.75 (b2488cca3)
- fix(streaming): render native tool previews distinctly (b14c1ec69)
- fix(tui): scope Codex verbosity to session provider (e4fa8fa83)
- chore(deps): bump tree-sitter-language-pack .20 -> .21 (TSX value bindings + arities) (30396f84c)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.20 (1d8a6a28d)
- feat(editing): struct_index range/ranges windows + resilient edit-kind resolution (929ff267c)
- feat(editing): fold rg into find_files (name+content search); assorted WIP (1f798cef4)
- feat(sandbox): per-session Seatbelt jail + gateway MITM egress proxy (6dad9340e)
- chore(deps): bump svar to 0.7.73 (health-gated connect-blip retry) (39497c64b)
- chore: remove committed transcript scratch artifacts (546f44e2e)
- Fix ForeignObject leak in session_state transcript projection (846a29df6)
- feat(transcript): canonical /export dialog with gateway md+html routes (1f090ce6f)
- feat(transcript): surface python stdout and attachments in transcript (#40) (5bab7111e)
- fix(env-python): make tool failures catchable in-block (#42) (a0353dd0a)
- Fix Blockether light theme tab colors and dialog margins (d8d06f748)
- fix(paramiko): drop shared MINA SSHD io-factory so every server negotiates (a4f71b963)
- Render fixes (cf9b43414)
- fix(paramiko-shim): self-reap MINA sshd host-side on session close (3b5bbeff2)
- fix(gateway/bus): treat turn.cancelled as terminal in hydrate! (1e82589d2)
- fix(channel-tui): declare flatland/ordered dep + align queue-close test (88668e84d)
- fix(prompt): gate user-pasted images on model vision + steer PIL fallback (be8cf6554)
- deps: bump com.blockether/bridge 0.1.2 -> 0.2.1 (bc7ef6ad4)
- Fix cat tool to accept all-kwargs spec map with path key (9cfef1a60)
- fix(editing): steer append_child/prepend_child used with a NAME locator (781f4bf47)
- fix: preserve summary-only printed results (aff9a70b6)
- deps: bump svar 0.7.69 -> 0.7.70 (transient-message classifier: statusless/wrapper/gRPC transients + mid-stream code casing) (e97085cfb)
- fix(tui): Linux clipboard OSC 52 fallback + scroll-safe transcript copy (6803f3396)
- Remove Piper TTS (speaking) from voice extension (cb60b8e08)
- Fix the errors (afe668357)
- wip: in-progress vis changes (3efbfa51d)
- chore(deps): bump com.blockether/svar 0.7.66 -> 0.7.67 (catalog-driven Copilot vision) (a057751a2)
- chore(deps): bump com.blockether/svar 0.7.65 -> 0.7.66 (6e93506f2)
- Optimizations (c847659af)
- Render doc/apropos native cards as authored markdown (5f28638c0)
- refactor(lint): drop redundant coercions; suppress clj-kondo false positives (62722c45b)
- style: blank line between all top-level forms (d6fd30b17)
- feat(clojure-format): report which backend formatted each result (4a140c50d)
- style: full-project cljfmt reformat (5f81e3684)
- fix(tui-magit): make status keybindings faithful to vanilla magit (bd8c1a958)
- fix(clojure-test): relativize fault file paths in repl-mode test output (075d68353)
- fix(python-cli): wire caller stdin into CLI context so -c/FILE can read sys.stdin (73d2c5e77)
- fix(self-docs): vis_docs both arities return {"pages": [...]} shape (c4bd0865e)
- Fixes to anchored editing (c9746b69d)
- fix(provider): name rejected tool schemas (8b7d86986)
- fix(tools): reject nonportable schema roots (836cd507b)
- fix(prompt): enforce compact actionable style (9ef8c92fc)
- perf(tui): drop lazy-mapcat counter atom in list->lines; StringBuilder CLI lists (1f9eb889c)
- docs(extensions): prefer native agent tools (c7e167e21)
- perf(render): drop per-list counter atoms for pure map-indexed (d1cf76102)
- Performance improvements (5857ca85b)
- Performance fixes for TUI! (cb2460d7a)
- Bump lanterna (cf28b89fb)
- Improve compiler error reporting (4b96c7cd5)
- fix(clj-test): honor run_tests :dir so sibling-project tests hit their own nREPL (8cde86af8)
- perf(tui): cache last-bubble tail projection; coalesce tab-switch refresh (4b2acd233)
- Performance fixes (8be5edce6)
- fix(prompt): resolve guidance and error rendering regressions (9b53851aa)
- Fixes to rendering (8d80c62cf)
- feat: refine prompts, errors, and TUI caching (ae55718d0)
- Stage changes to prompts (72916fea4)
- Prompt changes, tui optimizations (ecf8c0edc)
- Fixes to highlighting (b4d1c8dfe)
- Test runner fixes (2b1e8f754)
- Gateway fixes (ad3ff3325)
- Update language surface contract (bebfe1cdd)
- Fixes to prompt and python env (14a075275)
- Optimize prompt (98d24612f)
- test: align failing tests with current code (6810dff21)
- style: reformat with zprint (.zprint.edn) across src, extensions, test (7eecbe1e6)
- chore: scrub residual telegram & web-channel mentions from comments and docs (1135b69c9)
- refactor(gateway): resolve workspace via live-env fallback so root ops work pre-first-turn (bdb475d40)
- chore(channels): remove telegram bot channel (503f82937)
- fix(gateway): decouple SSE delivery from the turn thread via bounded per-connection queues (0c5fca9f9)
- fix(transcript): make HTML session export work without channel-web (e47208045)
- fix(locks): drop obsolete call-py monitor, bound close!/close-all! turn-lock waits (5ede59ced)
- chore(channels): remove web channel and react-native companion (d73b78a4e)
- refactor(env-python): one session context, one shared engine — pure-JVM renderer, in-context parser (1dea108d3)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.18 (48996c12c)
- fix(outline): surface the native cause, not opaque "FFI call failed" (35711bbb4)
- fix(env-python): freeze-proof every GraalPy context + dict-safe ntr results (5bd507d9b)
- docs: add full security audit (AUDIT.md) (59e4c13bf)
- feat(loop): self-heal empty model replies via svar 0.7.63 same-model resend (703e76484)
- Revert "fix(security): bound gateway JSON request body to 4 MiB (#6)" (dd9899ff8)
- fix(security): bound gateway JSON request body to 4 MiB (#6) (ada976fa7)
- fix(security): allowlist markdown link schemes in server render (#2) (eef312469)
- fix(editing): bound nearest-existing-dir climb to allowed roots; drop stale ls test (164ad4b19)
- feat(editing): climb missing search paths to nearest existing dir + report missing_paths (b3d9bfd1f)
- refactor(rg): search paths as named (file OR dir), skip missing (eb0e2fc7a)
- fix(security): harden credential storage and token comparison (Batch A) (4c63a2534)
- fix(python): seed apropos/doc for aliased extension symbols; apropos returns {name: gist} dict; clarify mcp double- vs single-underscore naming (1d55f071f)
- chore(deps): update dependencies via antq (de1c707ff)
- chore(deps): bump com.blockether/fff to 0.10.0-2 (0a98070dd)
- fix(editing): rg/find resolve paths to nearest existing directory instead of erroring (0cbc9f802)
- fix(clojure): reuse one nREPL session per connection + eval-based (+ 1 1) health check in ctx (74852acea)
- refactor(repl): remove bin/dev launcher and clj_repl references (05f9ebaa2)
- feat(language-surface): aggregate lint findings by path (7b5cbbc63)
- fix(loop): normalize svar 0.7 canonical token usage + append-only live tool-code fence (dc04a9a8f)
- feat(introspection): include session modified time (e85582900)
- feat(paramiko): support key generation in shim (86df6895d)
- fix(channel-tui): preserve account plan quota windows (3e3546dd5)
- docs(ctx-engine): clarify compact-src docstring (cdf6bfa68)
- feat(channel-tui): tint diff fence add/del rows and fix scrollbar flicker (928c43b56)
- docs(fold): require full workspace-relative path:line anchors in gists (c158a5b5b)
- big refactor (be1dbaa62)
- feat(python-sandbox): auto-settle deferred tool calls on subscript/len/in (a7584b3fe)
- feat(python-cli): forward script argv and caller env into vis python sandbox (3c0552d57)
- fix(tui): stop slash/file suggestion overlay flicker during live stream (a3cf4971e)
- fix(tui): ellipsize bubble footer meta line on narrow terminals (cad17abdb)
- fix(shim-matplotlib): resolve per-element hex color lists without float() crash (42127aacc)
- fix(loop): eliminate boxed-math + recur-primitive warnings in re-entrant park/retry loop (5a6aa5721)
- fix(transcript): fence folded-gist body so it renders verbatim (b1f7baf9c)
- feat(self-docs): render vis_docs op-card as a table (9af4039c9)
- perf(providers): cache fleet enumeration for footer-frequency reads (#29) (bd821219f)
- docs(readme): correct java prereq to 25+ (e8e01f2f3)
- chore(docs): drop Windows/PowerShell support from docs, readme, and installers (47056f6ca)
- fix(self-docs): treat blank/absent vis_docs slug as list request (4de0642ef)
- fix(gateway): parse /v1/events sids as UUIDs and persist cancel stamp (fc79477ce)
- feat(editing): name the searched directory scope in rg & find_files op-card headlines (3e30c97ea)
- fix(loop): make the native-tool park re-entrant so nested run_tests keeps its budget (2aceff86c)
- fix(compaction): show fold card savings as % of window, not a rising projected level (e5b277650)
- docs(channel-tui): document queue, cancel & Ctrl+C behavior (2673fbf62)
- feat(compaction): project next-request % on fold cards and freeze unbounded fold selectors (a8f835102)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-channel-tui
- feat: harden context handling and push notifications (445e449fd)
- fix: harden gateway compatibility and runtime lifecycle (690669489)
- fix: harden session recovery and provider surfaces (96d755216)
- fix(tui): keep navigator scroll arithmetic primitive (30891138f)
- feat: harden queues, tools, and companion (5d773f2cf)
- fix(queue): images in queued turns render as chips, not raw paths (5c5e4c6e5)
- perf(companion): cache /v1/router for 5 minutes and prefetch it at connect (90474b8ce)
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- feat(tui): flatten dialog, footer, and button chrome onto terminal background (3bbf8f888)
- Show authenticated OAuth providers in Router/Models manager (776afc717)
- chore: sync working tree (b1a3ee0e1)
- Session search: title-first transcript preview across TUI and Companion (762455312)
- Group GitHub Copilot tiers and filter to active variant (#47, #48) (00ef8a991)
- Merge remote-tracking branch 'origin/main' (ecfb8df36)
- Normalize toggle values to strings; surface config-denial hint in access view (379d5d9e1)
- chore(tui): channel-tui dialog/test updates (69e1f775e)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.23 (acffb332c)
- chore(sandbox): sandbox off by default; config-spec rooted paths + deny-exec + ports; net-probe; docs + repo-wide reformat (82db9d860)
- Fix lint warning in oauth_test and stage pending workspace changes (5ca75032c)
- Fixes (0b734bad0)
- feat(providers): surface & route authenticated OAuth providers; bound gather pool (403e7982c)
- feat(streaming): distinct native-call preview phase + svar 0.7.75 (b2488cca3)
- fix(streaming): render native tool previews distinctly (b14c1ec69)
- fix(tui): scope Codex verbosity to session provider (e4fa8fa83)
- feat(editing): fold rg into find_files (name+content search); assorted WIP (1f798cef4)
- feat(sandbox): per-session Seatbelt jail + gateway MITM egress proxy (6dad9340e)
- Fix ForeignObject leak in session_state transcript projection (846a29df6)
- feat(transcript): canonical /export dialog with gateway md+html routes (1f090ce6f)
- feat(transcript): surface python stdout and attachments in transcript (#40) (5bab7111e)
- Fix Blockether light theme tab colors and dialog margins (d8d06f748)
- Render fixes (cf9b43414)
- fix(channel-tui): restore result-row copy alignment for pre-indented output (88a58bf47)
- fix(channel-tui): declare flatland/ordered dep + align queue-close test (88668e84d)
- fix(prompt): gate user-pasted images on model vision + steer PIL fallback (be8cf6554)
- fix(editing): steer append_child/prepend_child used with a NAME locator (781f4bf47)
- deps: bump svar 0.7.69 -> 0.7.70 (transient-message classifier: statusless/wrapper/gRPC transients + mid-stream code casing) (e97085cfb)
- fix(tui): Linux clipboard OSC 52 fallback + scroll-safe transcript copy (6803f3396)
- Remove Piper TTS (speaking) from voice extension (cb60b8e08)
- Optimizations (c847659af)
- refactor(lint): drop redundant coercions; suppress clj-kondo false positives (62722c45b)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- fix(tui-magit): make status keybindings faithful to vanilla magit (bd8c1a958)
- fix(clojure-test): relativize fault file paths in repl-mode test output (075d68353)
- fix(python-cli): wire caller stdin into CLI context so -c/FILE can read sys.stdin (73d2c5e77)
- fix(self-docs): vis_docs both arities return {"pages": [...]} shape (c4bd0865e)
- perf(tui): drop lazy-mapcat counter atom in list->lines; StringBuilder CLI lists (1f9eb889c)
- test(tui): recording proxies reconstruct pre-segmented styled runs (08c3e66f5)
- Performance improvements (5857ca85b)
- perf(tui): single StringBuilder pass in run->sentinel-segment (5ed478b6b)
- Performance fixes for TUI! (cb2460d7a)
- perf(tui): halve styled-line paint allocation via sentinel split (64e4d6db0)
- perf(tui): skip grapheme array on plain lines in paint-styled-line! (285a07af4)
- deps(tui): bump com.blockether/lanterna to 3.1.5-vis.32 (ba40cc26a)
- Improve compiler error reporting (4b96c7cd5)
- perf(tui): identity-memoize layout height-key vector (5f32d67d6)
- perf(tui): O(log n) binary-search visible-window for scroll layout (0b01e116f)
- perf(tui): replace pass-1 est mapv with primitive transient loop (f64f5dfdb)
- perf(tui): cache last-bubble tail projection; coalesce tab-switch refresh (4b2acd233)
- Performance fixes (8be5edce6)
- Fixes to rendering (8d80c62cf)
- feat: refine prompts, errors, and TUI caching (ae55718d0)
- Prompt changes, tui optimizations (ecf8c0edc)
- Fixes to highlighting (b4d1c8dfe)
- Test runner fixes (2b1e8f754)
- Gateway fixes (ad3ff3325)
- Update language surface contract (bebfe1cdd)
- Fixes to prompt and python env (14a075275)
- Optimize prompt (98d24612f)
- chore: scrub residual telegram & web-channel mentions from comments and docs (1135b69c9)
- fix(python): seed apropos/doc for aliased extension symbols; apropos returns {name: gist} dict; clarify mcp double- vs single-underscore naming (1d55f071f)
- chore(deps): update dependencies via antq (de1c707ff)
- fix(editing): rg/find resolve paths to nearest existing directory instead of erroring (0cbc9f802)
- fix(clojure): reuse one nREPL session per connection + eval-based (+ 1 1) health check in ctx (74852acea)
- refactor(repl): remove bin/dev launcher and clj_repl references (05f9ebaa2)
- fix(channel-tui): keep the cost/token bubble footer on a cancelled turn that spent tokens (ab7560bab)
- fix(channel-tui): expand tabs before markdown wrapping (a1cd5b01e)
- fix(channel-tui): preserve account plan quota windows (3e3546dd5)
- feat(channel-tui): tint diff fence add/del rows and fix scrollbar flicker (928c43b56)
- fix(channel-tui): read gateway project fields by string key (aa61779e0)
- big refactor (be1dbaa62)
- fix(tui): wire Esc to clear the pending-sends queue (f147d7b7c)
- fix(channel-tui): reserve exact inline-image box from real terminal cell size (ca7b9426d)
- fix(tui): stop slash/file suggestion overlay flicker during live stream (a3cf4971e)
- fix(tui): ellipsize bubble footer meta line on narrow terminals (cad17abdb)
- fix(channel-tui): gate provider-limits poll resolve to cut idle CPU (171abd2bf)
- perf(providers): cache fleet enumeration for footer-frequency reads (#29) (bd821219f)
- docs(channel-tui): document queue, cancel & Ctrl+C behavior (2673fbf62)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-foundation-bridge
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- Fixes (0b734bad0)
- deps: bump com.blockether/bridge 0.1.2 -> 0.2.1 (bc7ef6ad4)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to rendering (8d80c62cf)
- Fixes to highlighting (b4d1c8dfe)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-foundation-search
- feat: harden queues, tools, and companion (5d773f2cf)
- feat(tui): flatten dialog, footer, and button chrome onto terminal background (3bbf8f888)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- Bump lanterna (cf28b89fb)
- Improve compiler error reporting (4b96c7cd5)
- Fixes to highlighting (b4d1c8dfe)
- chore: scrub residual telegram & web-channel mentions from comments and docs (1135b69c9)
- refactor(repl): remove bin/dev launcher and clj_repl references (05f9ebaa2)
- refactor(foundation-search): drop Exa MCP env toggles from extension registration (6bce7d3a0)
- big refactor (be1dbaa62)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-foundation-voice
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- feat(streaming): distinct native-call preview phase + svar 0.7.75 (b2488cca3)
- Remove Piper TTS (speaking) from voice extension (cb60b8e08)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- fix(prompt): resolve guidance and error rendering regressions (9b53851aa)
- Fixes to rendering (8d80c62cf)
- Fixes to highlighting (b4d1c8dfe)
- chore: scrub residual telegram & web-channel mentions from comments and docs (1135b69c9)
- chore(deps): update dependencies via antq (de1c707ff)
- big refactor (be1dbaa62)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-language-clojure
- fix: harden session recovery and provider surfaces (96d755216)
- feat: harden queues, tools, and companion (5d773f2cf)
- companion: canonical type scale, line-height in the scale, taller session header (f095e603a)
- feat(tui): flatten dialog, footer, and button chrome onto terminal background (3bbf8f888)
- Show authenticated OAuth providers in Router/Models manager (776afc717)
- chore(sandbox): sandbox off by default; config-spec rooted paths + deny-exec + ports; net-probe; docs + repo-wide reformat (82db9d860)
- Fixes (0b734bad0)
- feat(streaming): distinct native-call preview phase + svar 0.7.75 (b2488cca3)
- feat(editing): fold rg into find_files (name+content search); assorted WIP (1f798cef4)
- Render fixes (cf9b43414)
- fix(editing): steer append_child/prepend_child used with a NAME locator (781f4bf47)
- deps: bump svar 0.7.69 -> 0.7.70 (transient-message classifier: statusless/wrapper/gRPC transients + mid-stream code casing) (e97085cfb)
- Optimizations (c847659af)
- refactor(lint): drop redundant coercions; suppress clj-kondo false positives (62722c45b)
- style: blank line between all top-level forms (d6fd30b17)
- feat(clojure-format): report which backend formatted each result (4a140c50d)
- style: full-project cljfmt reformat (5f81e3684)
- fix(clojure-test): relativize fault file paths in repl-mode test output (075d68353)
- Fixes to anchored editing (c9746b69d)
- Performance fixes for TUI! (cb2460d7a)
- Improve compiler error reporting (4b96c7cd5)
- fix(clj-test): honor run_tests :dir so sibling-project tests hit their own nREPL (8cde86af8)
- Performance fixes (8be5edce6)
- Prompt changes, tui optimizations (ecf8c0edc)
- Fixes to highlighting (b4d1c8dfe)
- Test runner fixes (2b1e8f754)
- Gateway fixes (ad3ff3325)
- Update language surface contract (bebfe1cdd)
- Fixes to prompt and python env (14a075275)
- Optimize prompt (98d24612f)
- chore(deps): update dependencies via antq (de1c707ff)
- fix(editing): rg/find resolve paths to nearest existing directory instead of erroring (0cbc9f802)
- fix(clojure): reuse one nREPL session per connection + eval-based (+ 1 1) health check in ctx (74852acea)
- refactor(repl): remove bin/dev launcher and clj_repl references (05f9ebaa2)
- fix(clojure): fail eval fast on nREPL eval-error, interrupt the eval on timeout (2361ecb6e)
- test(language-clojure): cover blank snippet defaults (9e398bd6b)
- fix(language-clojure): ignore blank snippet defaults (f9c9490c1)
- big refactor (be1dbaa62)
- fix(clojure): discard unresponsive nREPL connections after evaluation timeout (47fc63e2c)
- fix(clojure-test-runner): silence framework reporter, structured failure digest (bc84dd95e)
- perf(providers): cache fleet enumeration for footer-frequency reads (#29) (bd821219f)
- fix(test-runner): pr-str result under pinned print vars to survive truncating nREPL sessions (dfa51f7dd)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-language-python
- fix: harden session recovery and provider surfaces (96d755216)
- companion: canonical type scale, line-height in the scale, taller session header (f095e603a)
- feat(tui): flatten dialog, footer, and button chrome onto terminal background (3bbf8f888)
- Show authenticated OAuth providers in Router/Models manager (776afc717)
- Fixes (0b734bad0)
- feat(editing): fold rg into find_files (name+content search); assorted WIP (1f798cef4)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-language-typescript-bun
- companion: canonical type scale, line-height in the scale, taller session header (f095e603a)
- feat(tui): flatten dialog, footer, and button chrome onto terminal background (3bbf8f888)
- Show authenticated OAuth providers in Router/Models manager (776afc717)
- Fixes (0b734bad0)
- feat(editing): fold rg into find_files (name+content search); assorted WIP (1f798cef4)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-persistance-sqlite
- feat: harden context handling and push notifications (445e449fd)
- fix(runtime): rescue provider auth failures (2b51f2973)
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- chore: sync working tree (b1a3ee0e1)
- Normalize toggle values to strings; surface config-denial hint in access view (379d5d9e1)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.23 (acffb332c)
- chore(sandbox): sandbox off by default; config-spec rooted paths + deny-exec + ports; net-probe; docs + repo-wide reformat (82db9d860)
- Fix lint warning in oauth_test and stage pending workspace changes (5ca75032c)
- feat(editing): fold rg into find_files (name+content search); assorted WIP (1f798cef4)
- feat(sandbox): per-session Seatbelt jail + gateway MITM egress proxy (6dad9340e)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- Gateway fixes (ad3ff3325)
- Fixes to prompt and python env (14a075275)
- chore: scrub residual telegram & web-channel mentions from comments and docs (1135b69c9)
- fix(security): harden credential storage and token comparison (Batch A) (4c63a2534)
- fix(python): seed apropos/doc for aliased extension symbols; apropos returns {name: gist} dict; clarify mcp double- vs single-underscore naming (1d55f071f)
- chore(deps): update dependencies via antq (de1c707ff)
- big refactor (be1dbaa62)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-provider-anthropic
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- Fix lint warning in oauth_test and stage pending workspace changes (5ca75032c)
- Fixes (0b734bad0)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- chore(deps): update dependencies via antq (de1c707ff)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-provider-github-copilot
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- chore: sync working tree (b1a3ee0e1)
- Group GitHub Copilot tiers and filter to active variant (#47, #48) (00ef8a991)
- chore: working-tree updates (companion screens, copilot provider) (850cfb30f)
- Fixes (0b734bad0)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- chore(deps): update dependencies via antq (de1c707ff)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-provider-openai-codex
- feat(providers): per-gateway providers, gateway-side auth, and session model pinning (ae734a66f)
- Normalize toggle values to strings; surface config-denial hint in access view (379d5d9e1)
- Fix lint warning in oauth_test and stage pending workspace changes (5ca75032c)
- Fixes (0b734bad0)
- feat(streaming): distinct native-call preview phase + svar 0.7.75 (b2488cca3)
- style: blank line between all top-level forms (d6fd30b17)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- chore(deps): update dependencies via antq (de1c707ff)
- big refactor (be1dbaa62)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-provider-standard
- fix(ci): restore local provider presets (9ecddf6ac)
- Fixes (0b734bad0)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- Optimize prompt (98d24612f)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-provider-zai
- Fixes (0b734bad0)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- chore(deps): update dependencies via antq (de1c707ff)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

#### com.blockether/vis-workspace-rift
- feat(editing): fold rg into find_files (name+content search); assorted WIP (1f798cef4)
- fix(tui): Linux clipboard OSC 52 fallback + scroll-safe transcript copy (6803f3396)
- style: full-project cljfmt reformat (5f81e3684)
- Fixes to highlighting (b4d1c8dfe)
- big refactor (be1dbaa62)
- fix(channel-tui): let Ctrl+C quit while a cancel is already in flight (f5d09ea93)

### Changed
- feat(workspace): rename `/draft-fresh` slash command to `/draft-blank` (empty drafts start with no HEAD files)

## [v0.1.4] - 2026-07-16

### Changed
- Revert "build(release): publish ONE canonical com.blockether/vis jar"
- build: publish ONE bundled com.blockether/vis jar instead of sibling packages
- build(release): publish ONE canonical com.blockether/vis jar
- feat(attachments): paint vis_attach images inline in the TUI
- perf(channel-tui): defer whole-bubble clipboard formatting to click time (#24)
- fix(shim-matplotlib): accept categorical (string) x/y on bar/barh
- ci(native): drop native-experiment.yml — no Intel/macos-13 build in the matrix
- fix(render): drop the 🖼 emoticon placeholder from :img nodes
- fix(language-surface): park run_tests outside the 30s native tool wall
- refactor(editing)!: remove the ls tool — find_files/outline/rg supersede it
- docs(prompt): enforce full-output access + python/shell narrowing across shell, mcp, bridge fragments
- Merge remote-tracking branch 'origin/main' into ci/native-free-runners
- ci(native): probe free macos-13 intel runner + gateway smoke test
- chore(verify): ratchet boxed-math baseline to 0
- fix(persist,ctx): keep realized non-lazy seqs in error :data instead of the {:vis/ref :expr} sentinel
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide
- ci(native): build macOS arm64 release on free macos-15 runner
- ci(native): raise arm probe timeout to 240m, drop dead intel/linux probes
- feat(config,search): YAML config tiers + :search :include-gitignored-paths overlay (#23)
- fix(provider): retry pre-response TTFT timeouts
- feat(bench): add readable Vis transcripts
- fix(channel-tui): stop streaming FULL-frame spin from :scroll identity churn
- perf(boxed-math): primitive leaves in asr, persistance-sqlite, shim-sqlite3, attachments
- perf(boxed-math): coerce primitive leaves in shim-pil, format, foundation-search, file-picker
- perf(shims,shell,channel-web,channel-telegram): eliminate boxed math in cold render/format paths
- fix(transcript): keep bottom code-bg padding on expanded paste disclosure
- ci(native): add linux-x64 probe to native-experiment matrix
- perf(build): restore -Os + interpreter-only GraalPy (accidentally reverted)
- ci(native): trigger experiment on branch push
- ci(native): probe free macOS runners + env-tunable builder heap
- fix(gateway): drain a message queued after Esc as soon as the cancelled turn unwinds
- feat(compaction): session_fold receipt carries saved-tokens note and op-card headline
- fix(theme): add a ^double hint to rel-luminance to remove Math/abs reflection; baseline 1977->395
- feat(loop): show saved-token counts on fold summary cards
- docs(language-surface): document manual reload after editing source (no auto-reload)
- refactor(loop): clarify fold card wording (saved ~tokens / utilization %)
- fix(language-clojure): :only matches fully-qualified var names and errors on no-match
- refactor(loop,extension): park the native tool wall during slow setup instead of startup budgets
- fix(gateway): clear remaining boxed-math warnings in state.clj and server.clj
- chore(deps): bump svar to 0.7.62 (Responses-API "completed" clean-stop fix)
- fix(loop,gateway,language-surface): boxed-math cleanup + startup-aware native tool timeouts
- fix(loop): fail stuck native tool calls faster
- fix(native): use glibc-compatible FFF
- fix(editing): clean boxed math and tighten hashline error data
- fix(tui): compact provider limit footer resets
- fix(language-surface): name the lint target in the LINT_CODE headline
- ci: cache docs site dependencies
- ci: run docs site on java 25
- ci: upgrade actions to node24
- feat(cli): add `vis python` standalone interpreter exposing GraalPy sandbox + shims
- feat(context): surface reclaimed tokens + live utilization in fold readouts
- refactor(channel-tui): move box rule builders to lanterna
- ci: cache classpath dependencies
- refactor(channel-tui): reuse lanterna clamp and clear table boxed math
- fix(clojure): autostart repl_eval with stale id and dir
- fix(channel-tui): remove duplicate queued prompt echo
- fix(tui): improve project picker selection
- refactor(channel-tui): hoist canonical ^long clamp into primitives, dedup call sites
- fix(channel-tui): eliminate all boxed-math warnings across the TUI
- feat(shim-paramiko): add server-side API surface (ServerInterface/SFTPServer/…)
- chore: ignore prompt regression outputs
- style: format channel sources
- chore(lint): exclude generated Telemere vars
- feat(swe-bench): add GLM-5.2 effort parity evaluation
- fixup! fix(native): require tree-sitter platform artifact
- test: repair stale UI and editing contracts
- feat(eval): add provider-native reasoning effort
- fix(native): require tree-sitter platform artifact
- feat(editing): strip echo diffs from python_execution stdout for patch/write/struct_patch
- perf(channel-tui): eliminate boxed-math in render pipeline
- docs(graalpython): document the two Python surfaces (sandbox vs project interpreter)
- perf(loop): drop echo diff from patch/write results on model wire
- feat(language-python): run_tests handler with graalpy (default) + project pytest backends
- fix(channel-tui): keep a send-during-cancel in the editor, never queued
- perf(editing): primitive-hint diff-preview helpers in editing/core.clj
- perf(tui): primitive-hint components.clj justify-line/justify-segs
- perf(tui): long-hint provider.clj copilot dialogs + card painters
- feat(shim-pytest): pytest.main([paths]) discovers test files on disk (#19)
- chore(graal): re-baseline boxed-math ratchet to 2079 (external loop fix +1)
- refactor(channel-tui): delegate ANSI fold/slice to lanterna 3.1.5-vis.26
- fix(loop): key post-refresh 401 lag detection on recency, not token value
- feat(cat): mark non-contiguous slices with canonical ⋯ divider
- perf(tui): primitive-hint provider.clj + dialogs.clj leaf layout helpers
- fix(provider-github-copilot): subtract refresh margin on the refresh_in mint branch (#21)
- refactor(tui): move column layout kernels into lanterna fork (vis.25)
- chore: update python shims, docs, and tui channel
- perf(tui): primitive-hint dialog geometry producers to cut boxed math
- refactor(tui): collapse duplicate ellipsize into one lanterna-backed p/ellipsize
- perf(tui): move column measurement into lanterna fork (vis.24), delegate primitives
- perf(tui): primitive-hint render.clj input/geometry leaf helpers
- perf(tui): primitive-hint dialogs.clj leaf layout helpers to cut boxed math
- chore(graal): re-baseline boxed-math ratchet to 2270 (pre-existing drift; reflection now 0)
- fix(graal): add type hints to silence reflective calls + advertise sandbox shims
- Remove the leftovers
- feat(compaction): store fold summaries in cards; utilization retains only the current budget
- refactor(sqlite): squash migrations V1..V10 into a single consolidated V1__schema.sql
- chore: sync workspace changes across core, extensions, and docs
- feat(compaction): merge fold ledger into utilization as one-line readout
- feat(python-extensions): author LLM providers from Python
- feat(env-python): guard against GraalVM/Truffle version mismatch on --jvm
- chore: sync workspace changes across core, extensions, and docs
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll
- fix(verify,reflection): make graal gate actually run + zero reflection warnings
- fix(loop): treat post-refresh 401 responses as propagation delays rather than invalid credentials
- fix(loop): mark invalid OAuth credentials across the gateway to prevent repeated refreshes
- fix(clojure): self-heal nrepl eviction and gate test runner on repl liveness
- perf(channel-tui): throttle render-loop scroll-ease to stop streaming CPU spin
- perf(build): shrink native image with -Os + interpreter-only GraalPy
- fix(build,ci): build native image on every commit for all platforms
- perf(channel-tui): input-text fast path + phase-tagged slow-frame logs
- fix(copilot): honor refresh_in to stop the 401 'IDE token expired' storm (#16)
- feat(self-docs): add per-page blurbs to vis_docs listing
- fix(loop): treat live thread interrupt as user cancel (#13)
- fix(gateway): converge auth-token rotation storm, graceful drain, DB-backed turn queue + auto-resume
- chore(deps): bump com.blockether/svar 0.7.59 -> 0.7.60 (models.dev-backed catalog, slim overlays, canonical :resets-at-ms)
- feat(gateway): multiplexed /v1/events — ONE SSE stream for many sessions
- perf(gateway/bus,repl-mgr): kill journal-scan churn + reap idle project REPLs
- feat(progress/web/tui): surface a live 'Vis is running: …' ticker for coarse activity
- fix(tui/render-test): assert the real coalesced flush contract
- fix(tui/shell): green the suite — resume duration bug + stale expectations
- perf(client/gateway/bridge): cut TUI typing lag + idle CPU/alloc
- perf(gateway/bus): adapt tailer polling to reduce idle CPU usage
- fix(gateway): reap orphaned event journals + JFR recordings; isolate draft store
- chore(rn-companion): harden app .gitignore for Expo/native build artifacts
- feat(magit): colorize diff header filenames in TUI status view
- fix(language-clojure): resolve lint config from nearest .clj-kondo in nested projects
- chore: wip changes to env_python, loop, prompt, compaction tests
- fix(gateway): unwedge stalled turns, drain queue, sync fs confinement
- fix(openai-codex): retry quota fetch after oauth rotation
- fix(openai-codex): honor explicit quota window duration
- docs(todo): rename gateway command to `vis gateway start` and refresh status
- fix(channel-tui): single space before footer chord hint
- fix(clojure): treat "default" repl id as sentinel
- style: format sources with zprint
- fix(companion): keep QR pairing stable while settings scroll
- fix(companion): make settings sheet dismissible and compact
- style(companion): capitalize shared UI module
- fix(companion): make settings full-screen and scrollable
- test(gateway): lock Tailscale preference for pairing QR
- fix(companion): keep gateway settings scrollable while scanning
- fix(companion): pin gateway settings until connection recovers
- fix(companion): explain gateway network failures
- style(companion): native iOS polish without yellow chrome
- style(companion): make settings feel native on iOS
- feat(sandbox): add pure-Python/JVM-bridge compat shims for the GraalPy sandbox
- style(companion): format ios app before TestFlight build
- feat(companion): consume canonical gateway feature surface
- feat(companion): rehydrate settled tool cards from turn trace
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.12
- feat(companion): local turn-completion notifications + restore app.json / build bump
- test(companion): jest-expo unit suite for streaming/markdown/gateway logic
- fix(companion): render streaming/unclosed markdown fences + graceful group/error handling
- fix(companion): resolve iOS white-screen crash from mismatched Expo native modules
- fix(companion): disable user script sandboxing on iOS app target
- release: update version files for v0.1.3, bump to next dev version

### Package changes

#### com.blockether/vis
- Revert "build(release): publish ONE canonical com.blockether/vis jar" (a1872194)
- build: publish ONE bundled com.blockether/vis jar instead of sibling packages (c2bde1c5)
- build(release): publish ONE canonical com.blockether/vis jar (8e6385f3)
- feat(attachments): paint vis_attach images inline in the TUI (d3fc1f1c)
- fix(shim-matplotlib): accept categorical (string) x/y on bar/barh (7896231b)
- ci(native): drop native-experiment.yml — no Intel/macos-13 build in the matrix (7b595c5a)
- fix(render): drop the 🖼 emoticon placeholder from :img nodes (fed01943)
- fix(language-surface): park run_tests outside the 30s native tool wall (60d096f3)
- refactor(editing)!: remove the ls tool — find_files/outline/rg supersede it (7aeef72a)
- docs(prompt): enforce full-output access + python/shell narrowing across shell, mcp, bridge fragments (990d9bc2)
- Merge remote-tracking branch 'origin/main' into ci/native-free-runners (b2d60bd9)
- ci(native): probe free macos-13 intel runner + gateway smoke test (c5b3cef8)
- chore(verify): ratchet boxed-math baseline to 0 (43f8b71d)
- fix(persist,ctx): keep realized non-lazy seqs in error :data instead of the {:vis/ref :expr} sentinel (d3c6ed7b)
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- ci(native): build macOS arm64 release on free macos-15 runner (5796db1d)
- ci(native): raise arm probe timeout to 240m, drop dead intel/linux probes (0a0fe93e)
- feat(config,search): YAML config tiers + :search :include-gitignored-paths overlay (#23) (8d47c25e)
- fix(provider): retry pre-response TTFT timeouts (b9d46872)
- feat(bench): add readable Vis transcripts (d8f8c588)
- perf(boxed-math): primitive leaves in asr, persistance-sqlite, shim-sqlite3, attachments (15d48cc1)
- perf(boxed-math): coerce primitive leaves in shim-pil, format, foundation-search, file-picker (44b68ad0)
- perf(shims,shell,channel-web,channel-telegram): eliminate boxed math in cold render/format paths (489c0b67)
- ci(native): add linux-x64 probe to native-experiment matrix (cf6fa27c)
- perf(build): restore -Os + interpreter-only GraalPy (accidentally reverted) (ee3ce58b)
- ci(native): trigger experiment on branch push (fc385eaf)
- ci(native): probe free macOS runners + env-tunable builder heap (22b22dba)
- fix(gateway): drain a message queued after Esc as soon as the cancelled turn unwinds (b4afb32d)
- feat(compaction): session_fold receipt carries saved-tokens note and op-card headline (f56fced4)
- fix(theme): add a ^double hint to rel-luminance to remove Math/abs reflection; baseline 1977->395 (a1433367)
- feat(loop): show saved-token counts on fold summary cards (5b21d983)
- docs(language-surface): document manual reload after editing source (no auto-reload) (321c48f2)
- refactor(loop): clarify fold card wording (saved ~tokens / utilization %) (f1473666)
- refactor(loop,extension): park the native tool wall during slow setup instead of startup budgets (7cd3e3e8)
- fix(gateway): clear remaining boxed-math warnings in state.clj and server.clj (9b655432)
- chore(deps): bump svar to 0.7.62 (Responses-API "completed" clean-stop fix) (91fb359d)
- fix(loop,gateway,language-surface): boxed-math cleanup + startup-aware native tool timeouts (8fee45ab)
- fix(loop): fail stuck native tool calls faster (98988948)
- fix(native): use glibc-compatible FFF (f0d135e6)
- fix(editing): clean boxed math and tighten hashline error data (f019e131)
- fix(tui): compact provider limit footer resets (c80cf071)
- fix(language-surface): name the lint target in the LINT_CODE headline (28f6f8a6)
- ci: cache docs site dependencies (a542387c)
- ci: run docs site on java 25 (b5fa21e9)
- ci: upgrade actions to node24 (e0ed7526)
- feat(cli): add `vis python` standalone interpreter exposing GraalPy sandbox + shims (1afcec09)
- feat(context): surface reclaimed tokens + live utilization in fold readouts (65cffcf7)
- ci: cache classpath dependencies (798dee3a)
- feat(shim-paramiko): add server-side API surface (ServerInterface/SFTPServer/…) (48d41c40)
- chore: ignore prompt regression outputs (958ab100)
- chore(lint): exclude generated Telemere vars (8b8cb098)
- feat(swe-bench): add GLM-5.2 effort parity evaluation (846328a4)
- fixup! fix(native): require tree-sitter platform artifact (12afc4aa)
- test: repair stale UI and editing contracts (8bf435e1)
- feat(eval): add provider-native reasoning effort (c415ef58)
- fix(native): require tree-sitter platform artifact (e039e4f0)
- feat(editing): strip echo diffs from python_execution stdout for patch/write/struct_patch (571b199f)
- perf(channel-tui): eliminate boxed-math in render pipeline (9c093753)
- docs(graalpython): document the two Python surfaces (sandbox vs project interpreter) (a4e00cd9)
- perf(loop): drop echo diff from patch/write results on model wire (8653e0c4)
- perf(editing): primitive-hint diff-preview helpers in editing/core.clj (6f879e1c)
- perf(tui): primitive-hint components.clj justify-line/justify-segs (507d832a)
- perf(tui): long-hint provider.clj copilot dialogs + card painters (0bbd7de6)
- feat(shim-pytest): pytest.main([paths]) discovers test files on disk (#19) (4755d28d)
- chore(graal): re-baseline boxed-math ratchet to 2079 (external loop fix +1) (690d36ed)
- fix(loop): key post-refresh 401 lag detection on recency, not token value (b6de8d56)
- feat(cat): mark non-contiguous slices with canonical ⋯ divider (7cc5740c)
- perf(tui): primitive-hint provider.clj + dialogs.clj leaf layout helpers (c05d7bec)
- refactor(tui): move column layout kernels into lanterna fork (vis.25) (e8bcb2da)
- chore: update python shims, docs, and tui channel (5444a1fc)
- perf(tui): primitive-hint dialog geometry producers to cut boxed math (e2ed742c)
- perf(tui): move column measurement into lanterna fork (vis.24), delegate primitives (ea6d17d7)
- perf(tui): primitive-hint render.clj input/geometry leaf helpers (296c13c5)
- perf(tui): primitive-hint dialogs.clj leaf layout helpers to cut boxed math (45da5d14)
- chore(graal): re-baseline boxed-math ratchet to 2270 (pre-existing drift; reflection now 0) (9a17a06a)
- fix(graal): add type hints to silence reflective calls + advertise sandbox shims (4d17b60a)
- Remove the leftovers (709c1451)
- feat(compaction): store fold summaries in cards; utilization retains only the current budget (eec7c974)
- refactor(sqlite): squash migrations V1..V10 into a single consolidated V1__schema.sql (c0898015)
- chore: sync workspace changes across core, extensions, and docs (9a68ff54)
- feat(compaction): merge fold ledger into utilization as one-line readout (c35868e8)
- feat(python-extensions): author LLM providers from Python (e370650e)
- feat(env-python): guard against GraalVM/Truffle version mismatch on --jvm (9aaf179c)
- chore: sync workspace changes across core, extensions, and docs (e410b355)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)
- fix(loop): treat post-refresh 401 responses as propagation delays rather than invalid credentials (473c0f9f)
- fix(loop): mark invalid OAuth credentials across the gateway to prevent repeated refreshes (f7a251b8)
- fix(clojure): self-heal nrepl eviction and gate test runner on repl liveness (8ee71a22)
- perf(build): shrink native image with -Os + interpreter-only GraalPy (46e006d4)
- fix(build,ci): build native image on every commit for all platforms (793b1ba1)
- feat(self-docs): add per-page blurbs to vis_docs listing (767f0c93)
- fix(loop): treat live thread interrupt as user cancel (#13) (aaae74f4)
- fix(gateway): converge auth-token rotation storm, graceful drain, DB-backed turn queue + auto-resume (c16a1134)
- chore(deps): bump com.blockether/svar 0.7.59 -> 0.7.60 (models.dev-backed catalog, slim overlays, canonical :resets-at-ms) (069da8fa)
- feat(gateway): multiplexed /v1/events — ONE SSE stream for many sessions (9594470c)
- perf(gateway/bus,repl-mgr): kill journal-scan churn + reap idle project REPLs (ee1109b5)
- feat(progress/web/tui): surface a live 'Vis is running: …' ticker for coarse activity (0e0280fc)
- fix(tui/shell): green the suite — resume duration bug + stale expectations (bccd7d22)
- perf(client/gateway/bridge): cut TUI typing lag + idle CPU/alloc (605d6639)
- perf(gateway/bus): adapt tailer polling to reduce idle CPU usage (17357e96)
- fix(gateway): reap orphaned event journals + JFR recordings; isolate draft store (9dddbc53)
- feat(magit): colorize diff header filenames in TUI status view (1dc58309)
- fix(language-clojure): resolve lint config from nearest .clj-kondo in nested projects (61f475f4)
- chore: wip changes to env_python, loop, prompt, compaction tests (c74adc11)
- fix(gateway): unwedge stalled turns, drain queue, sync fs confinement (3ff2d07b)
- docs(todo): rename gateway command to `vis gateway start` and refresh status (2144797e)
- style: format sources with zprint (e15b9a35)
- test(gateway): lock Tailscale preference for pairing QR (6660f83c)
- feat(sandbox): add pure-Python/JVM-bridge compat shims for the GraalPy sandbox (ce5af764)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.12 (96a4f2c4)
- release: update version files for v0.1.3, bump to next dev version (0768f7c8)

#### com.blockether/vis-channel-telegram
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- perf(shims,shell,channel-web,channel-telegram): eliminate boxed math in cold render/format paths (489c0b67)
- style: format channel sources (605ed163)
- chore: sync workspace changes across core, extensions, and docs (9a68ff54)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)

#### com.blockether/vis-channel-tui
- perf(channel-tui): defer whole-bubble clipboard formatting to click time (#24) (7840d0b5)
- refactor(editing)!: remove the ls tool — find_files/outline/rg supersede it (7aeef72a)
- fix(channel-tui): stop streaming FULL-frame spin from :scroll identity churn (bd0c5c52)
- fix(transcript): keep bottom code-bg padding on expanded paste disclosure (ed37efde)
- fix(tui): compact provider limit footer resets (c80cf071)
- feat(context): surface reclaimed tokens + live utilization in fold readouts (65cffcf7)
- refactor(channel-tui): move box rule builders to lanterna (600386a8)
- refactor(channel-tui): reuse lanterna clamp and clear table boxed math (afd5ae56)
- fix(channel-tui): remove duplicate queued prompt echo (e5495522)
- fix(tui): improve project picker selection (31236707)
- refactor(channel-tui): hoist canonical ^long clamp into primitives, dedup call sites (98d4e62f)
- fix(channel-tui): eliminate all boxed-math warnings across the TUI (2697ef5a)
- test: repair stale UI and editing contracts (8bf435e1)
- feat(editing): strip echo diffs from python_execution stdout for patch/write/struct_patch (571b199f)
- perf(channel-tui): eliminate boxed-math in render pipeline (9c093753)
- fix(channel-tui): keep a send-during-cancel in the editor, never queued (c6227d9b)
- perf(tui): primitive-hint components.clj justify-line/justify-segs (507d832a)
- perf(tui): long-hint provider.clj copilot dialogs + card painters (0bbd7de6)
- refactor(channel-tui): delegate ANSI fold/slice to lanterna 3.1.5-vis.26 (48b24ac7)
- perf(tui): primitive-hint provider.clj + dialogs.clj leaf layout helpers (c05d7bec)
- chore: update python shims, docs, and tui channel (5444a1fc)
- perf(tui): primitive-hint dialog geometry producers to cut boxed math (e2ed742c)
- refactor(tui): collapse duplicate ellipsize into one lanterna-backed p/ellipsize (9082bf66)
- perf(tui): move column measurement into lanterna fork (vis.24), delegate primitives (ea6d17d7)
- perf(tui): primitive-hint render.clj input/geometry leaf helpers (296c13c5)
- perf(tui): primitive-hint dialogs.clj leaf layout helpers to cut boxed math (45da5d14)
- fix(graal): add type hints to silence reflective calls + advertise sandbox shims (4d17b60a)
- feat(compaction): merge fold ledger into utilization as one-line readout (c35868e8)
- feat(python-extensions): author LLM providers from Python (e370650e)
- feat(env-python): guard against GraalVM/Truffle version mismatch on --jvm (9aaf179c)
- chore: sync workspace changes across core, extensions, and docs (e410b355)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)
- fix(loop): treat post-refresh 401 responses as propagation delays rather than invalid credentials (473c0f9f)
- fix(clojure): self-heal nrepl eviction and gate test runner on repl liveness (8ee71a22)
- perf(channel-tui): throttle render-loop scroll-ease to stop streaming CPU spin (1bffb7e7)
- perf(channel-tui): input-text fast path + phase-tagged slow-frame logs (fc738374)
- feat(self-docs): add per-page blurbs to vis_docs listing (767f0c93)
- fix(gateway): converge auth-token rotation storm, graceful drain, DB-backed turn queue + auto-resume (c16a1134)
- feat(gateway): multiplexed /v1/events — ONE SSE stream for many sessions (9594470c)
- feat(progress/web/tui): surface a live 'Vis is running: …' ticker for coarse activity (0e0280fc)
- fix(tui/render-test): assert the real coalesced flush contract (0de6e6a3)
- fix(tui/shell): green the suite — resume duration bug + stale expectations (bccd7d22)
- perf(client/gateway/bridge): cut TUI typing lag + idle CPU/alloc (605d6639)
- feat(magit): colorize diff header filenames in TUI status view (1dc58309)
- fix(gateway): unwedge stalled turns, drain queue, sync fs confinement (3ff2d07b)
- fix(channel-tui): single space before footer chord hint (5cd3685f)
- style: format sources with zprint (e15b9a35)
- feat(sandbox): add pure-Python/JVM-bridge compat shims for the GraalPy sandbox (ce5af764)

#### com.blockether/vis-channel-web
- perf(shims,shell,channel-web,channel-telegram): eliminate boxed math in cold render/format paths (489c0b67)
- feat(context): surface reclaimed tokens + live utilization in fold readouts (65cffcf7)
- style: format channel sources (605ed163)
- fix(graal): add type hints to silence reflective calls + advertise sandbox shims (4d17b60a)
- feat(python-extensions): author LLM providers from Python (e370650e)
- chore: sync workspace changes across core, extensions, and docs (e410b355)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)
- fix(loop): treat live thread interrupt as user cancel (#13) (aaae74f4)
- fix(gateway): converge auth-token rotation storm, graceful drain, DB-backed turn queue + auto-resume (c16a1134)
- feat(progress/web/tui): surface a live 'Vis is running: …' ticker for coarse activity (0e0280fc)
- fix(tui/shell): green the suite — resume duration bug + stale expectations (bccd7d22)
- perf(client/gateway/bridge): cut TUI typing lag + idle CPU/alloc (605d6639)
- fix(gateway): unwedge stalled turns, drain queue, sync fs confinement (3ff2d07b)
- style: format sources with zprint (e15b9a35)
- feat(sandbox): add pure-Python/JVM-bridge compat shims for the GraalPy sandbox (ce5af764)

#### com.blockether/vis-foundation-bridge
- docs(prompt): enforce full-output access + python/shell narrowing across shell, mcp, bridge fragments (990d9bc2)
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)
- perf(client/gateway/bridge): cut TUI typing lag + idle CPU/alloc (605d6639)

#### com.blockether/vis-foundation-harness
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)

#### com.blockether/vis-foundation-mcp
- docs(prompt): enforce full-output access + python/shell narrowing across shell, mcp, bridge fragments (990d9bc2)
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)

#### com.blockether/vis-foundation-search
- perf(boxed-math): coerce primitive leaves in shim-pil, format, foundation-search, file-picker (44b68ad0)

#### com.blockether/vis-foundation-voice
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- perf(boxed-math): primitive leaves in asr, persistance-sqlite, shim-sqlite3, attachments (15d48cc1)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)

#### com.blockether/vis-language-clojure
- fix(language-surface): park run_tests outside the 30s native tool wall (60d096f3)
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- fix(language-clojure): :only matches fully-qualified var names and errors on no-match (1b897c6e)
- refactor(loop,extension): park the native tool wall during slow setup instead of startup budgets (7cd3e3e8)
- fix(loop,gateway,language-surface): boxed-math cleanup + startup-aware native tool timeouts (8fee45ab)
- fix(tui): compact provider limit footer resets (c80cf071)
- feat(context): surface reclaimed tokens + live utilization in fold readouts (65cffcf7)
- fix(clojure): autostart repl_eval with stale id and dir (212a660f)
- feat(python-extensions): author LLM providers from Python (e370650e)
- feat(env-python): guard against GraalVM/Truffle version mismatch on --jvm (9aaf179c)
- chore: sync workspace changes across core, extensions, and docs (e410b355)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)
- fix(clojure): self-heal nrepl eviction and gate test runner on repl liveness (8ee71a22)
- feat(self-docs): add per-page blurbs to vis_docs listing (767f0c93)
- fix(gateway): converge auth-token rotation storm, graceful drain, DB-backed turn queue + auto-resume (c16a1134)
- perf(gateway/bus,repl-mgr): kill journal-scan churn + reap idle project REPLs (ee1109b5)
- fix(language-clojure): resolve lint config from nearest .clj-kondo in nested projects (61f475f4)
- fix(gateway): unwedge stalled turns, drain queue, sync fs confinement (3ff2d07b)
- fix(clojure): treat "default" repl id as sentinel (a867ae48)
- style: format sources with zprint (e15b9a35)

#### com.blockether/vis-language-python
- feat(language-python): run_tests handler with graalpy (default) + project pytest backends (2d4b6670)

#### com.blockether/vis-language-typescript-bun
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)

#### com.blockether/vis-persistance-sqlite
- fix(persist,ctx): keep realized non-lazy seqs in error :data instead of the {:vis/ref :expr} sentinel (d3c6ed7b)
- perf(boxed-math): primitive leaves in asr, persistance-sqlite, shim-sqlite3, attachments (15d48cc1)
- style: format channel sources (605ed163)
- refactor(sqlite): squash migrations V1..V10 into a single consolidated V1__schema.sql (c0898015)
- chore: sync workspace changes across core, extensions, and docs (9a68ff54)
- feat(python-extensions): author LLM providers from Python (e370650e)
- feat(env-python): guard against GraalVM/Truffle version mismatch on --jvm (9aaf179c)
- chore: sync workspace changes across core, extensions, and docs (e410b355)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(gateway): converge auth-token rotation storm, graceful drain, DB-backed turn queue + auto-resume (c16a1134)
- fix(gateway): unwedge stalled turns, drain queue, sync fs confinement (3ff2d07b)
- feat(sandbox): add pure-Python/JVM-bridge compat shims for the GraalPy sandbox (ce5af764)

#### com.blockether/vis-provider-anthropic
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- feat(context): surface reclaimed tokens + live utilization in fold readouts (65cffcf7)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)

#### com.blockether/vis-provider-github-copilot
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- fix(provider-github-copilot): subtract refresh margin on the refresh_in mint branch (#21) (8334a21b)
- fix(copilot): honor refresh_in to stop the 401 'IDE token expired' storm (#16) (39635a88)

#### com.blockether/vis-provider-openai-codex
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)
- feat(context): surface reclaimed tokens + live utilization in fold readouts (65cffcf7)
- fix(gateway): converge auth-token rotation storm, graceful drain, DB-backed turn queue + auto-resume (c16a1134)
- fix(openai-codex): retry quota fetch after oauth rotation (3bac7996)
- fix(openai-codex): honor explicit quota window duration (f64a1754)

#### com.blockether/vis-provider-zai
- perf(boxed-math): eliminate the remaining 127 boxed-math warnings project-wide (f918dc90)

### Fixed

- fix(openai-codex): use provider/enrich-models-fn to supply context windows for models absent from svar's catalog, including gpt-5.6-terra, instead of its 8192 default. Preserve known catalog values and explicit context configuration. Add gpt-5.6-terra to default models.

## [v0.1.3] - 2026-07-12

### Changed
- feat(sessions): owner + session-group folders across sqlite/gateway/tui
- feat(companion): live SSE tool-card streaming + native iOS TestFlight scaffolding
- fix(gateway): decode workspace kebab keys at the client boundary so added filesystem roots show
- feat(gateway): client-managed daemon self-reap + settings/models API
- refactor(companion): drop the clojure extension, rewrite the RN app
- style(channel-web): use --primary-fg for text on filled primary buttons
- fix(editing): treat non-positive expected_mtime as no staleness guard
- feat(language): full TypeScript/JavaScript/JSX/TSX support
- test(channel-tui): repair stale state-test setups against current code
- style(tui,web): bold dialog titles and use warmer modal colors
- fix(channel-tui): drop the attaching turn from the queued mirror
- chore(companion): trim web dependencies
- fix(gateway): route provider diagnostics through daemon
- feat(tui): colour the footer git/draft chips like sibling buttons
- feat(tui): async magit network verbs + C-x g chord on the footer git button
- bench: tolerate EDN sets and tags in preflight config parser
- test(python): close matplotlib contexts
- fix(gateway): extend native startup timeout
- feat(tui): magit dialog WIP, hint-bar fitting + F4 log fetch under a timeout
- fix(repl): make nREPL start truly synchronous + health-aware resources
- fix(tui): bound clipboard helpers with a hard deadline; table wrap via shared lanterna word-wrap
- fix(tui): stop the mid-stream scroll bounce on macOS trackpads
- fix(tui): wrap markdown table cells inside their columns
- fix(tui): sync session titles live across processes during streaming
- fix(workspace): fresh drafts can never delete HEAD files
- feat(workspace): /draft-fresh empty drafts + multi-TUI tab sync merge
- fix(gateway): synchronize queued turns across channels
- feat(gateway): canonical wire transcripts + turn traces across channels
- fix(editing): make rg scan phase and parallel sub-loops cancellable
- fix(editing): stop runaway rg CPU on cancelled gather
- fix(gateway): probe entry timeout
- chore(format): reformat foundation editing and language-surface
- fix(gateway): release listen socket before resource reap and exit daemon on stop
- feat(language-typescript-bun): refuse monorepo-root REPL with app-dir hint
- feat(gateway): kill session background resources on TUI close
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.10
- fix(language-surface): advertise dir/timeout_ms on repl_eval, dir/filter on run_tests
- feat(gateway): route interactive clients through daemon
- feat(language-typescript-bun): managed Bun REPL + bun test language pack
- release: update version files for v0.1.2, bump to next dev version

### Package changes

#### com.blockether/vis
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- fix(gateway): decode workspace kebab keys at the client boundary so added filesystem roots show (404f4c91)
- feat(gateway): client-managed daemon self-reap + settings/models API (58cbac17)
- refactor(companion): drop the clojure extension, rewrite the RN app (b483c962)
- fix(editing): treat non-positive expected_mtime as no staleness guard (a97de2d7)
- feat(language): full TypeScript/JavaScript/JSX/TSX support (93272651)
- fix(gateway): route provider diagnostics through daemon (10021653)
- bench: tolerate EDN sets and tags in preflight config parser (4ebe1e57)
- test(python): close matplotlib contexts (7203812e)
- fix(gateway): extend native startup timeout (aacd1e6c)
- fix(repl): make nREPL start truly synchronous + health-aware resources (bb1ce93e)
- fix(workspace): fresh drafts can never delete HEAD files (d9e743a6)
- feat(workspace): /draft-fresh empty drafts + multi-TUI tab sync merge (a881e23e)
- fix(gateway): synchronize queued turns across channels (19d1721a)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)
- fix(editing): make rg scan phase and parallel sub-loops cancellable (16bed7e8)
- fix(editing): stop runaway rg CPU on cancelled gather (5163a878)
- fix(gateway): probe entry timeout (98934a52)
- chore(format): reformat foundation editing and language-surface (12f933ae)
- fix(gateway): release listen socket before resource reap and exit daemon on stop (80106799)
- feat(gateway): kill session background resources on TUI close (614ecf21)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.10 (4758ddc8)
- fix(language-surface): advertise dir/timeout_ms on repl_eval, dir/filter on run_tests (ba850596)
- feat(gateway): route interactive clients through daemon (c23d8035)
- feat(language-typescript-bun): managed Bun REPL + bun test language pack (41b8d217)
- release: update version files for v0.1.2, bump to next dev version (5503af84)

#### com.blockether/vis-channel-tui
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- test(channel-tui): repair stale state-test setups against current code (1b5c2a3f)
- style(tui,web): bold dialog titles and use warmer modal colors (63ea851b)
- fix(channel-tui): drop the attaching turn from the queued mirror (7b02d0db)
- fix(gateway): route provider diagnostics through daemon (10021653)
- feat(tui): colour the footer git/draft chips like sibling buttons (f50e23dc)
- feat(tui): async magit network verbs + C-x g chord on the footer git button (1c3aa843)
- feat(tui): magit dialog WIP, hint-bar fitting + F4 log fetch under a timeout (893fe04f)
- fix(tui): bound clipboard helpers with a hard deadline; table wrap via shared lanterna word-wrap (bdd63e32)
- fix(tui): stop the mid-stream scroll bounce on macOS trackpads (5c004898)
- fix(tui): wrap markdown table cells inside their columns (4fa8daf6)
- fix(tui): sync session titles live across processes during streaming (6ff45bfd)
- feat(workspace): /draft-fresh empty drafts + multi-TUI tab sync merge (a881e23e)
- fix(gateway): synchronize queued turns across channels (19d1721a)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-channel-web
- style(channel-web): use --primary-fg for text on filled primary buttons (0d4262fc)
- style(tui,web): bold dialog titles and use warmer modal colors (63ea851b)
- fix(gateway): route provider diagnostics through daemon (10021653)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-foundation-harness
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-language-clojure
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- fix(repl): make nREPL start truly synchronous + health-aware resources (bb1ce93e)
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-language-typescript-bun
- feat(language): full TypeScript/JavaScript/JSX/TSX support (93272651)
- feat(language-typescript-bun): refuse monorepo-root REPL with app-dir hint (4722c8e9)
- feat(language-typescript-bun): managed Bun REPL + bun test language pack (41b8d217)

#### com.blockether/vis-persistance-sqlite
- feat(sessions): owner + session-group folders across sqlite/gateway/tui (3eda3304)
- feat(gateway): canonical wire transcripts + turn traces across channels (3c56c0df)

#### com.blockether/vis-provider-standard
- feat(gateway): route interactive clients through daemon (c23d8035)

#### com.blockether/vis-provider-zai
- feat(gateway): route interactive clients through daemon (c23d8035)

## [v0.1.2] - 2026-07-10

### Changed
- fix(release): auto-publish extension packages
- fix(editing): treat blank paths entry as "search everything"
- release: update version files for v0.1.1, bump to next dev version

### Package changes

#### com.blockether/vis
- fix(release): auto-publish extension packages (1408366a)
- fix(editing): treat blank paths entry as "search everything" (484fa5d9)
- release: update version files for v0.1.1, bump to next dev version (9d0764d8)

#### com.blockether/vis-provider-github-copilot
- fix(release): auto-publish extension packages (1408366a)

## [v0.1.1] - 2026-07-10

### Changed
- fix(release): publish all vis monorepo packages
- feat(providers): surface svar 0.7.56 models
- fix(editing): coerce stringified array in rg include/query/paths
- feat(provider-github-copilot): allow claude-sonnet-5 in policy models
- fix(prompt): render every prior answer in full in resume block
- feat: improve vis tool rendering and resources
- Fix markdown fence and comment rendering
- docs(agents): note commit message style
- fix(editing): accept rg include shorthand
- fix(transcript): render nested markdown fences
- fix(editing): delete directory trees
- test: fix full suite regressions
- Update senior SWE benchmark tooling
- refactor(tui): reuse active turn cleanup helper
- Fix TUI workspace root sync
- fix(tui): reconcile stale in-flight state
- fix(tui): clear stale cancelling state
- fix(loop): close GraalPy context on environment disposal
- fix(self-docs): string-key vis_docs payloads
- fix(clojure-test-runner): empty selectors run everything, not error
- feat(attachments): session-level introspection lister (P1)
- feat(resources): live-tail + paging in background-log viewers
- feat(attachments): add attachment storage offloading with a registry, pure selection logic and resolver
- feat(resources): view background logs in TUI + web
- fix(channel-web): space + chip styling for result summaries
- feat(attachments): session_fold collapses vision replay too
- fix(channel-web): harden renderProse against UI-spoofing HTML injection
- fix(web): strip vis-image fence in DB-restored history; drop comment profanity
- fix(tui): collapse same-file edit band to full path shown once
- feat(attachments): unify tool and user attachment storage in one session_attachment table
- refactor(attachments): use self-describing handle IDs and remove the read-back fallback
- feat(attachments): unify read-back across tool + user attachments
- test(loop): update synth oracle for symbol_rename as positional native tool
- feat(attachments): introspection read-back API + misc workspace changes
- feat(editing): promote symbol_rename back to a native tool
- test(attachments): lock down gather->virtual-thread sink conveyance
- clj-ext: format only on :write, not patch/struct_patch
- feat(attachments): $VIS_OUTBOX filesystem tap + rename :images -> :attachments
- feat(attachments): support generic artifact producers in vis_attach
- feat(attachments): V3 brings session_turn_attachment to V2 payload parity
- feat(attachments): connect iteration attachments to producers and readers
- refactor(mpl): capture produced images at their source and remove stdout-fence parsing
- feat(loop): replay generated figures to vision models on the wire
- feat(loop): capture matplotlib figure bytes into iteration attachments
- feat(persist): V2 session_iteration_attachment table + store/read
- tui(navigator): drop empty Modified column, rename Directory -> Dir

### Added
- Register GitHub Copilot Enterprise alongside Individual and Business. Use the same curated Claude catalog with dotted models.dev IDs and the native Anthropic /v1/messages API.
- Extension system with global registry, topo-sort, hot-reload
- `:ext/nudge-fn` for per-iteration system nudges from extensions
- `:ext/requires` for extension dependency declaration
- `:ext/version`, `:ext/author`, `:ext/license` metadata
- `register-global!`, `load-extension!`, `reload-extension!`
- `extensions/common/vis-foundation` package (read, list, grep, patch)
- mdBook documentation (current documentation: https://vis.blockether.com/)
- Iteration metadata stores active extensions (namespace + version)
- Apache-2.0 license

### Changed
- Default reasoning level: `:balanced` (was `:quick`)
- `create-env` -> `create-environment`
- `dispose-env!` -> `dispose-environment!`
- `vis!` -> `query!`
- `register-env-def!` removed (use extensions)
- Nudges moved from `loop/nudges.clj` to `loop/runtime/prompt.clj`
- `session/shared.clj` folded into `session/core.clj`

### Removed
- `var-diff` (dead code)
- `restore-var` references (never existed as callable tool)
- Scattered .md files (consolidated into `resources/docs/`)
- Remove the built-in repetition system_nudge and its repetition-warning, REPETITION_THRESHOLD and call-count state. Retain journal and cached-result information for detecting repeated work.

### Fixed
- Fix Copilot Claude 404 responses by appending /v1 to token-exchanged LLM base URLs with idempotent ensure-api-version. Cache the versioned URL while retaining the root host for model-policy calls. Apply to Individual, Business and Enterprise.
- `github-copilot-provider-id?` omitted `:github-copilot-enterprise`, so
  enterprise models were filtered out of the visible catalog mapping.

[Unreleased]: https://github.com/Blockether/vis/compare/v0.1.58...HEAD
[v0.1.58]: https://github.com/Blockether/vis/compare/v0.1.57...v0.1.58
[v0.1.57]: https://github.com/Blockether/vis/compare/v0.1.56...v0.1.57
[v0.1.56]: https://github.com/Blockether/vis/compare/v0.1.55...v0.1.56
[v0.1.1]: https://github.com/Blockether/vis/releases/tag/v0.1.1
[v0.1.2]: https://github.com/Blockether/vis/releases/tag/v0.1.2
[v0.1.3]: https://github.com/Blockether/vis/releases/tag/v0.1.3
[v0.1.4]: https://github.com/Blockether/vis/releases/tag/v0.1.4
[v0.1.5]: https://github.com/Blockether/vis/releases/tag/v0.1.5
[v0.1.6]: https://github.com/Blockether/vis/releases/tag/v0.1.6
[v0.1.7]: https://github.com/Blockether/vis/releases/tag/v0.1.7
[v0.1.8]: https://github.com/Blockether/vis/releases/tag/v0.1.8
[v0.1.9]: https://github.com/Blockether/vis/releases/tag/v0.1.9
[v0.1.10]: https://github.com/Blockether/vis/releases/tag/v0.1.10
[v0.1.11]: https://github.com/Blockether/vis/releases/tag/v0.1.11
[v0.1.12]: https://github.com/Blockether/vis/releases/tag/v0.1.12
[v0.1.13]: https://github.com/Blockether/vis/releases/tag/v0.1.13
[v0.1.14]: https://github.com/Blockether/vis/releases/tag/v0.1.14
[v0.1.20]: https://github.com/Blockether/vis/releases/tag/v0.1.20
[v0.1.21]: https://github.com/Blockether/vis/releases/tag/v0.1.21
[v0.1.22]: https://github.com/Blockether/vis/releases/tag/v0.1.22
[v0.1.23]: https://github.com/Blockether/vis/releases/tag/v0.1.23
[v0.1.24]: https://github.com/Blockether/vis/releases/tag/v0.1.24
[v0.1.25]: https://github.com/Blockether/vis/releases/tag/v0.1.25
[v0.1.26]: https://github.com/Blockether/vis/releases/tag/v0.1.26
[v0.1.27]: https://github.com/Blockether/vis/releases/tag/v0.1.27
[v0.1.28]: https://github.com/Blockether/vis/releases/tag/v0.1.28
[v0.1.31]: https://github.com/Blockether/vis/releases/tag/v0.1.31
[v0.1.32]: https://github.com/Blockether/vis/releases/tag/v0.1.32
[v0.1.33]: https://github.com/Blockether/vis/releases/tag/v0.1.33
[v0.1.34]: https://github.com/Blockether/vis/releases/tag/v0.1.34
[v0.1.35]: https://github.com/Blockether/vis/releases/tag/v0.1.35
[v0.1.37]: https://github.com/Blockether/vis/releases/tag/v0.1.37
[v0.1.38]: https://github.com/Blockether/vis/releases/tag/v0.1.38
[v0.1.39]: https://github.com/Blockether/vis/releases/tag/v0.1.39
[v0.1.40]: https://github.com/Blockether/vis/releases/tag/v0.1.40
[v0.1.41]: https://github.com/Blockether/vis/releases/tag/v0.1.41
[v0.1.43]: https://github.com/Blockether/vis/releases/tag/v0.1.43
[v0.1.44]: https://github.com/Blockether/vis/releases/tag/v0.1.44
[v0.1.45]: https://github.com/Blockether/vis/releases/tag/v0.1.45
[v0.1.46]: https://github.com/Blockether/vis/releases/tag/v0.1.46
[v0.1.47]: https://github.com/Blockether/vis/releases/tag/v0.1.47
