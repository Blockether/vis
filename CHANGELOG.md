# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

### Changed
- Every list row wears its own verbs at its trailing edge — no swipe, no `⋯`, no strip of words under the row
- The machines band is one line: its name and `Add a machine`, with no sentence about tapping and no address it already lists

## [v0.1.37] - 2026-08-14

### Changed
- Release v0.1.37
- The notifications row offers the verb, not the state it is already in
- A turn settles only itself, keeps the answer it just painted, and refetches a short transcript

### Package changes

#### com.blockether/vis
- Release v0.1.37 (fbca3dd3a)
- The notifications row offers the verb, not the state it is already in (22f5e2b7a)
- A turn settles only itself, keeps the answer it just painted, and refetches a short transcript (b20892310)



### Added
- One `C-x g` status buffer for EVERY repository a session works on: the project, every Git
  repository nested below it (a mega-repo's `repositories/` clones) and every read-write
  repository declared in `vis.yml`'s `workspace.filesystem` catalog. Each earns a header carrying
  its branch and dirty counts — a clean one folded to that single line — and every verb (`s`/`u`,
  `S`/`U`, `c`, `P`, `F`, `b`, `z`) acts on the repository under the cursor. Nothing caps how many
  are shown; only DISCOVERY is bounded, and a walk that stops early says `scan truncated` in the
  title instead of quietly listing fewer.
- `jail.environment` — one MODE, not a list, over the operator's ambient environment in a
  confined child: `declared` (the default: only the project's own `.env` + `environment:` plus a
  non-secret basics allowlist) or `inherit` (the whole ambient environment, secrets included,
  with filesystem, network, exec and Mach confinement untouched). The escape hatch for a
  toolchain that needs a pile of host variables instead of naming each one. `LD_*`, `DYLD_*`,
  `PERL*`, `BASH_ENV` and friends are refused under `inherit` too — that scrub protects the
  jail's own installation, not the child.
- Trim to view in the Companion image viewer: zoom or pan to a detail and `Trim` makes that
  region the picture — cut at the ORIGINAL resolution, with any strokes flattened into it — so
  the pen, Copy, Share and Use edit all act on the detail. `Undo trim` brings the whole picture
  back.

### Changed
- The Companion's Notifications panel answers ONE question: is this device connected to this
  machine. It used to list every push token the gateway holds — one iPhone reinstalled three times
  stood in it four times, under four masked tokens and two APNs environments — while the only state
  a reader wants was left to the verb printed on a button. One row now states it (`Connected` /
  `Not connected`, naming the machine in the sentence, `Checking…` before the first answer) and one
  `Switch` is both verbs, in the same place either way; native APNs/FCM and Web Push ask it with the
  same row. A permission turned off in the OS is never reported as connected, and on iOS the blocked
  state offers the door to system Settings.
- Copying a session id — the TUI header chip and the Companion chip beside the title — now puts
  `vis_session_id#<uuid>` on the clipboard instead of a bare UUID. The marker says WHAT the id
  addresses, so whoever it is pasted to recognises a Vis session, and `read_session` /
  `get_session` accept the marked form verbatim (the introspection prompt names the format).
- The Companion composer has ONE microphone. Tap it and it acts in the mode you are in; press and
  hold it for 450ms and the mode flips — dictation writes into the box, voice conversation sends
  what you said and reads the answer aloud. The disclosure beside it, the two-item mode menu, the
  rule that welded them and the separate leave button are gone: the mode was always one piece of
  state, and holding is now both the way in and the way out (the exit runs the full teardown —
  lease, queued utterance, speech, an in-flight recording and the audio route). Voice conversation
  wears its own drawn mark and the accent square instead of a `V` badge, the accessible name spells
  the gesture out in both modes, and a pointer that cannot hold gets the same switch from a
  right-click or Shift+Enter. Entering the conversation now only ARMS it; the next tap is what
  starts talking.
- The Vis JVM heap ceiling is an explicit 5 GiB instead of a share of host RAM. `-XX:MaxRAMPercentage=75.0`
  scaled with the machine — ~27 GiB on a 48 GB host — so a gateway running for hours sat at ~5 GB resident
  with a 3.2 GiB live set and shrank nothing: the tight free ratios beside it only uncommit once the heap
  looks full, and the engine's memory-pressure gates (heap watermark 85%, heap budget 2 GB, RSS budget
  3 GB) were either unreachable or shedding idle sessions that were not what held the memory. `-Xmx5g` is
  the same ceiling on every machine, so the periodic concurrent GC has something to give back and the
  pressure gates sit at a meaningful fraction of the cap.
- One Android release lands on EVERY Play tester track. `release:android:store` wrote a single
  `--track`, so the channels drifted apart — internal served 0.1.21 (2861) while beta already
  served 0.1.35 (4075), and lining them up again was a second, manual promote afterwards.
  `--track` now takes a LIST (comma-separated, or the flag repeated) and defaults to
  `internal,alpha,beta`, all assigned inside ONE transactional Play edit: either every track gets
  the build or none does, so no channel can be left a version behind. `production` is never
  implied, and a staged `--rollout` — which Play defines per track — refuses more than one track
  BEFORE the build rather than after a signed .aab.
- One iOS release reaches EVERY TestFlight audience, the same rule Android just got. An upload
  reaches only the internal groups by itself, and the public-link group had to be asked for with
  `--public` — so the public link served build 4042 while the team group already had 4075 and
  every Play tester track served 4090. `release:ios:store` now fans out by default (internal
  groups PLUS the public group after Beta App Review) and `--audience internal` keeps a build
  inside the team; the audience is planned BEFORE the archive, so an unknown one costs a second
  instead of a signed .ipa. The product release workflow no longer narrows Android to `beta`
  alone either: a `vX.Y.Z` tag ships one build to every tester channel of both stores.
- `all` — every tester channel each STORE has, not the list this repo happens to know. The
  fan-out was still three track names frozen in the release script and repeated in two workflow
  files, so a closed testing track created in the Play Console could never be released to
  (`--track qa` was refused as unknown) and an internal TestFlight group created WITHOUT "access
  to all builds" — the one kind Apple does not hand new builds to — was skipped on every run.
  `--track all` now asks Play which tracks this listing HAS and writes them all in the one edit
  (`production` still never implied, a typo still refused against the real names, before the
  build), `--audience all` links every external group and every internal group that needs the
  build assigned, and both GitHub workflows pass `all` — so a channel added in a console is
  served by the next release with no change to this repository.
- `run_tests` selects by `paths` and nothing else, in every language. The `ns` / `namespace` /
  `namespaces` selector is gone: name a test file, a directory, or the SOURCE file whose `*-test`
  namespace should run, and the clojure pack does that translation itself (a `*_test.clj` file is
  read for the namespace it declares, a source file maps to its `*-test` namespace, a directory is
  walked for both). Two vocabularies for one selection meant the same run could be named two ways
  and only one of them reached a pack; the old spellings are refused by name rather than silently
  running the WHOLE suite.
- A `paths` entry may name ONE TEST: `<path>::<test-name>` — pytest's own node-id grammar, now the
  single way to say it in every pack. The name half translates the way the file half already did
  (`src/a/core.clj::adds` runs `adds-test` in `com.example.core-test`, just as `core.clj` runs
  `core-test`), a namespace-less `::adds-test` finds that var wherever it lives, and each id is
  paired with its OWN file instead of cross-producting names over namespaces. Clojure's `only` and
  bun's `filter` are deleted — a second key that narrowed a run could disagree with the path beside
  it, and only one pack understood each spelling. Python passes node ids straight to pytest and
  turns a pathless `::name` into `-k`; the hermetic GraalPy backend runs whole files, so it REFUSES
  a node id (naming `{"environment": "project"}`) instead of quietly running the rest of the file.
- Every language pack now emits the `run_tests` result vocabulary ITSELF — `pass`, `fail`,
  `errored`, `command`, `is_pass`. The surface's translation table (`passed`/`failed` -> `pass`/
  `fail`, `ok` -> `is_pass`, an argv `cmd` -> `command`) is deleted: it guessed each runner's
  arithmetic from outside the pack that knew it (pytest's `failed` and `errors` are DISJOINT,
  lazytest's are not), and a fact reached a result under two spellings. Python's `runner` /
  `interpreter` call aliases are gone too: a CALL says `environment`, config says `python.runner`.
- The session introspection surface reads as VERBS instead of storage nouns: `session_state` is
  now `read_session`, `sessions` is `list_sessions`, `session_fold` is `fold_session`, and the
  read that was missing between them is `get_session(target)` — ONE descriptor row (identity,
  turn count, last activity, provider/model, the last turn) so asking what a session IS no longer
  costs a whole transcript. `session_state` was the name of a DB TABLE, and a bare plural noun is
  not a verb. `list_sessions` drops the `channel` filter for `search=`, which is the SAME ranked
  answer the TUI and the companion app paint: the server ranks title (0), request (1), reply (2)
  and thinking (3), and each matched row carries `rank`,
  `is_in_title`/`is_in_request`/`is_in_reply`/`is_in_thinking` plus the request/reply snippet
  windows. There are no aliases — the old names are gone.
- Android voice capture now opens a connected Bluetooth headset's HFP/SCO microphone before
  WebView starts recording, then restores the normal audio route when recording ends.
- Voice-conversation playback on Android now uses the system text-to-speech engine when WebView
  does not expose the Web Speech API.
- Voice conversation requests the spoken projection for an idle session as well as a queued one;
  previously the usual idle path returned only the full on-screen answer.
- Prepare `vis-foundation-bridge` for Bridge 0.3: migrate the project profile and policy to the
  reduced YAML schema, recognize every 0.3 profile filename, preserve exact validation paths, and
  keep malformed Bridge configuration from crashing ordinary filesystem access.
- The native release build is verifiable without spending a tag. `Native Release` dispatched
  from a BRANCH is now a dry run — it builds the image, stages the bundle and runs every smoke
  test and the native suite, and attaches nothing; only a `v*` ref publishes. It used to refuse
  a non-tag dispatch outright, so the only way to learn whether a release would build was to cut
  the tag and watch.
- The macOS arm64 release build runs on GITHUB's hosted runner and on nobody's laptop. The
  self-hosted Apple-silicon runner is deregistered, its launchd service uninstalled and its
  working directory deleted; the `macos` job of `.github/workflows/native-release.yml` now
  defaults to the free hosted `macos-26` — free and unlimited on public repositories, because
  only LARGER runners are billed and this workflow uses none. That builder is 3 cores / 7 GiB
  against a ~13.7 GiB points-to live set, so every heap that fits in RAM is a known OOM: the job
  pins `-J-Xmx14g -J-Xms2g -J-XX:+UseParallelGC` and lets macOS dynamic swap back it, and its
  first step prints cores, RAM, free disk and swap so a failure is a NUMBER on the log. The
  repository variable `VIS_MACOS_ARM64_RUNNER` survives only as an OPTIONAL override naming a
  bigger CLOUD Apple-silicon label; by hand it is still `bin/release-native --tag vX.Y.Z
  --upload` on any 32 GB+ Mac. The job also caches dependencies and smoke-tests the gateway,
  because a hosted runner is ephemeral.
- A shell result has NO `stderr` field (issue #137). Every command runs under a real pty, where
  stdout and stderr are physically ONE stream, so `stderr` could only ever answer `nil` — a
  caller reading it to diagnose a failure got nothing while the message sat in `stdout`. The
  internal blocking runner merges the two streams the same way, so one shape and one reading of
  "the bytes" hold everywhere; `stderr_omitted_chars` and the card's STDERR section are gone with
  it.
- The agent prompt and the shell docstrings spell the keystroke method `sh.type("y")`, never a
  bare `sh.type()` (issue #137): it SENDS text and its argument is required, so the old spelling
  among the status accessors raised a `TypeError` for anyone who followed it.
- The process-jail doc page is `jail.md` ("Process jail & egress"), not `sandbox.md`: in this
  repo *sandbox* now names only the in-process GraalPy sandbox, and *jail* names OS confinement —
  the same split the config keys and `session["access"]` already use. Every in-tree link moved
  with it.
- `grep` and `struct_nodes` take ONE options map and nothing else — `grep({"query": q, "paths":
  ["src"]})`, `struct_nodes({"path": p, "line": n})` — the same shape `struct_index` and
  `struct_patch` already had, and the shape Python kwargs (`grep(query=q, paths=["src"])`) fold
  into. The positional forms are gone: `grep("q")`, `grep("q", {opts})` and `struct_nodes("p")`
  made the SECOND argument mean options, so the obvious `grep(["a", "b"], ["src", "tools"])` —
  needles, then scopes — failed on argument shape instead of searching. The refusal now names the
  one canonical call.
- `/<name>` NAMES a skill instead of pasting it. The expansion is one sentence — use this skill,
  read it with `doc("name")` unless its `SKILL.md` is already in the conversation — plus your
  task, the owning project of a nested skill, and the paths of its bundled resources. Whether the
  instructions still have to be fetched is the model's call, since only the model can see whether
  that text is still in front of it. Every skill surface is now stateless: two `/<name>`s expand
  identically and nothing is recorded between them.
- The workspace's `.env` / `.env.local` are now loaded BY DEFAULT, whole, with nothing declared,
  and reach every child Vis spawns — `shell(...)` (jailed or not), managed REPLs, test runners and
  Python extensions. Resolution order everywhere is `environment:` declaration, then `.env`, then
  the environment that started Vis. `environment:` is now only for what a dotenv file cannot say:
  a rename, a keychain item, a helper command, or re-admitting an ambient variable to a confined
  child.
- The jail no longer withholds the project's `.env` from a confined child. The child was granted
  the workspace and can read that file itself, so dropping the values confined nothing; what
  `jail.enabled` draws a line around is the OPERATOR's ambient environment, which is still
  deny-by-default. `LD_*`, `DYLD_*`, `PERL*`, `BASH_ENV` and friends are refused from a project
  `.env` exactly as they are from a declaration.

- `jail` is the ONLY word for confinement, in config and in the model's own session map.
  The read-only `session["access"]` view reports `is_jailed` instead of `sandboxed`, and
  `session["workspace"]` reports `isolated` (a backend workspace copy, which was never about
  confinement) instead of `sandbox`. "Sandbox" now means only the Python sandbox.

### Removed
- The top-level `sandbox:` and `filesystem:` config keys. Both were silently REWRITTEN into
  `jail:` before the schema saw the file, so an operator's key became a different one and a
  `sandbox: false` written next to a `jail:` block was quietly ignored. Write `jail.enabled` and
  `jail.filesystem` — anything else is refused by name, loudly, on load.
- `jail.env`. It was a second list of the same names that could only ever re-admit an AMBIENT
  variable — never a `dotenv:`/`keychain:`/`command:` value — so the two blocks disagreed exactly
  where it mattered. Declare the variable in `environment:` instead (`CI: {env: CI}` is the old
  `jail.env: [CI]`). Everything undeclared is still dropped for a confined child, and `LD_*`,
  `DYLD_*`, `PERL*`, `BASH_ENV` and friends are still refused even when declared.
- `extensions.env-passthrough`, a third list of the same names: `extensions` was never a valid
  top-level config key, so the block was rejected before anything could read it.
- The `skill` verb. A skill is a document like any other: `apropos(text)` finds it, `doc("name")`
  prints the whole `SKILL.md`, and reading it is the whole of using it. There is no activation, no
  `status`/`scope`/`note` receipt, no idempotent re-read and no fold protection for an "active"
  body. Skill DISCOVERY is unchanged — every skill is still listed in the prompt by name,
  description and owning project.
- The `git` tool. There is no model-facing Git schema, no `git` binding in the sandbox and no
  `foundation-git` extension: a Git command is an ordinary `shell` command, run by the same
  jail, capture and timeout as everything else. Workspace Git FACTS (footer status, environment
  block, file picker) and the TUI Magit surface are unchanged.
- The automatic outbox capture. `$VIS_OUTBOX` no longer exists in the sandbox, a file the
  sandbox writes into system temp is no longer harvested, and neither is a `write_file` that
  lands in `/tmp` — the session, the transcript and the Companion stop filling with scratch,
  build chips and half-finished files nobody asked for. `attach` is how an artifact is kept:
  a producer that wants a human to SEE something names it and hands over the bytes. Writing to
  temp still WORKS everywhere it did; it is simply not collected. The machinery stays in the
  tree, dormant and tested behind `mpl-capture/incidental-capture-enabled?`, in case a future
  feature wants an engine-owned capture directory again.

### Fixed
- `patch` and `cat` refused the anchor they had just printed. Every addressable line these tools
  render is `<line>:<hash>│ <text>`, but the anchor parser read EVERYTHING after the colon as the
  hash — the gutter and the line's own text included — so a row pasted back whole hashed to
  `5af│ /**`, matched no line in the file, and came back as `no line within 40 lines carries
  5af│ /**` with a "current anchor" identical to the one just refused (its text lower-cased on the
  way out, since the hash is folded case-insensitively). The gutter and everything behind it are
  now cut before parsing, so a `cat` line, a `grep` hit row (indent and all) or a `struct_index`
  anchor addresses a line exactly as printed. A REPLACEMENT carrying a gutter is unchanged: it is
  still written verbatim, with the note that says so.
- Sandbox HTTPS could not skip certificate verification, and the escape hatch was deleted behind
  the caller's back. `requests.get(url, verify=False)`, `Session.verify`, `cert=`, httpx's
  `verify=`/`cert=` and urllib3's `cert_reqs` / `ca_certs` / `cert_file` / `assert_hostname` /
  `ssl_context` were all accepted and thrown away — every request used urlopen's default verified
  context, so an expired, self-signed or internal-CA host was unreachable from a block — while a
  top-level `import ssl` (and `select` / `selectors`) was silently DELETED from the source by the
  import preprocessor, so even the stdlib workaround died on an unexplainable `NameError`. Those
  options now build the TLS context for the request, warning with urllib3's own
  `InsecureRequestWarning` (which `urllib3.disable_warnings()` really silences); only `asyncio` is
  still rewritten, and every other stdlib import reaches the block verbatim.
- Sandbox TLS options were honoured only in their most common spelling, and every other one
  failed QUIETLY. A `pathlib.Path` CA bundle fell through the shim's `str` check and restored the
  DEFAULT store, so the narrow bundle a caller pinned silently became the wide one; a missing
  bundle or client certificate surfaced as a bare `FileNotFoundError` from inside `ssl` instead of
  requests' own message naming the file; `REQUESTS_CA_BUNDLE` / `CURL_CA_BUNDLE` were never read;
  urllib3 reported a certificate failure as a bare `ProtocolError`, so
  `except urllib3.exceptions.SSLError` never fired, and dressed an unreadable CA path as a
  transport error; `cert_reqs="NONE"` — upstream's own bare spelling — and an unknown name both
  verified anyway; and `assert_fingerprint`, `ciphers` and the TLS version bounds were swallowed
  whole, reporting a guarantee nothing enforced. Paths are now any `str` / `bytes` / `os.PathLike`,
  the environment's bundle is read unless `Session.trust_env` is off, a certificate failure is
  `urllib3.exceptions.SSLError`, a configuration error is raised verbatim,
  `ssl_minimum_version` / `ssl_maximum_version` reach the context,
  `urllib3.util.ssl_.create_urllib3_context` is published, and the two options this transport
  cannot honour are REFUSED with a message instead of ignored.
- A committed `vis.yml` forced one developer's provider and model on every clone. The visible
  project file merges LAST, over `~/.vis`, so `default_provider`, `default_model`,
  `fallback_provider` and `fallback_model` written there silently replaced each teammate's own
  selection — a teammate without that entitlement got a broken session on first run, and
  validation reported no problem at all. Those four keys are now dropped from
  `<project>/vis.yml` (and `vis.yaml`) with one warning naming the file and the right home.
  They still decide routing in every file a person owns: `~/.vis/config.yml`, the
  machine-written `~/.vis/state.yml`, and the gitignored `<project>/.vis/config.yml` overlay.
- The native binary crashed the moment the TUI painted. Lanterna's `TTYDeviceControl` builds its
  termios/ioctl `MethodHandle`s in a class initializer that ran in the image BUILDER, where
  `java.lang.foreign` works — so the binary inherited `SUPPORTED = true` and handles with no
  downcall stubs behind them, and segfaulted inside `DowncallStubsHolder` on the first
  `open("/dev/tty")` (v0.1.33-v0.1.35, x64 and arm64). The class now initializes at RUN time, so
  the binary decides for itself: the termios fast path where the descriptors are registered, and
  lanterna's own fallback to forking `/bin/stty` where they are not.
- The native binary could not open its own terminal UI or touch its database. The TUI's `screen`
  and `chat`, the sqlite backend's `core` and voice's `input` are reached BY NAME
  (`requiring-resolve`) on first use, so nothing required them at discovery and the image never
  build-time initialized them — a native image cannot define classes at run time, so `vis` died
  with `Could not locate …channel_tui/screen__init.class` and any DB command with `Backend
  :sqlite … failed to load`, while every JVM test stayed green. An extension manifest now
  declares such a namespace under `:image-nses`: compiled INTO the image, still not required at
  startup, so discovery keeps paying nothing for Lanterna or JDBC. `native-reachability-test`
  now fails when a by-name namespace is undeclared.
- The Android release preflight asks the question the build asks. It probed
  `/usr/libexec/java_home -v 21`, which only reports JDKs registered under
  `/Library/Java/JavaVirtualMachines` and is blind to SDKMAN — so a machine whose only
  stock Temurin 21 lives in SDKMAN was declared unfit for the Play leg, even though
  `release:android:store` searches SDKMAN itself and builds there happily.
  `apps/vis-companion/scripts/jdk.mjs` now owns the rule (exactly 21, never GraalVM,
  whose `jlink` breaks AGP's JdkImageTransform) and the search; `android-release.mjs`
  imports it instead of carrying its own copy, and `node scripts/jdk.mjs` prints the JDK
  Gradle will really use — the preflight and the build can no longer disagree.

- The pty bridge is tested against a REAL pseudo-terminal. `pty_bridge_test` drove a hand-written
  `{:add-listener :send}` stand-in, which could only prove that `serve!` called two functions —
  never that a byte typed into the socket reaches a terminal and comes back. It now spawns `cat`
  under `pty/spawn!` (the exact handle production hands the bridge) and asserts the whole loop:
  replay, live tee, and typed input echoed back out of the master.

- The native binary starts again. `build.clj` kept its own copy of the built-in extension
  namespaces vis `require`s at runtime, and every built-in added since — `foundation.introspection`
  and the whole shim family — was missing from it, so the image never initialized them and the
  binary died on its first line with "Could not locate …introspection__init.class on classpath".
  The copy is complete, and a reachability test now fails when the two lists drift.

- The Linux native binaries ship again. Every release since v0.1.33 built them and then died in
  the TUI smoke test with a SIGSEGV inside the generated FFM stub for lanterna's
  `open("/dev/tty", …)`, so v0.1.33, v0.1.34 and v0.1.35 attached no Linux distribution at all.
  The build feature no longer registers those downcall descriptors on Linux, which leaves
  lanterna's own `catch Throwable` to mark the native TTY unsupported and drive the terminal by
  forking `/bin/stty` — exactly what shipped through v0.1.32. macOS keeps the fast path.

- Push capability named the wrong provider. Web Push MINTS its own VAPID identity the first time
  it is asked, so its half is "configured" on every gateway — and it sat AHEAD of the relay in
  `/v1/capabilities`, hiding the one provider a machine with no credentials at all actually
  delivers through. The relay is named first now, Web Push last; a browser device also takes the
  real Web Push path instead of being reported as an unsupported platform.
- The providers router-rebuild hook held the FUNCTION, and `defonce` skips its body on a
  `(require … :reload)` — so after any reload of the agent loop the hook still pointed at the
  definition from the first load, and a default-model change reached a stale router. It holds the
  VAR now.
- On Linux, pasta's own diagnostics ("No routable interface for IPv6: IPv6 is disabled") were read
  back as the jailed command's output: pasta is the argv PREFIX, so it writes to the child's stdio.
  It now runs `--quiet` (which drops only the informational half) with `--log-file` pointing at
  this process' own `~/.vis/logs`.

- A cancel that landed inside a best-effort `(catch Throwable _ …)` is no longer
  swallowed. The JVM clears the interrupt flag as it throws, so every catch-all
  around a blocking call — `git`, the workspace's git, `rewind`, the credential
  helper, `stty`, the jail's detacher probe, the RSS sampler, the shell's tree
  teardown, the gateway client's port poll, the MCP listen thread, `gh auth
  token`, the Copilot keychain read — answered its fallback value with the
  cancellation gone, and the turn polled on to its own deadline.
  `cancellation/preserve-interrupt!` re-arms the flag (never for a
  `CancellationException`, which interrupted nothing), and the MCP listen thread
  now ENDS on an interrupt instead of sleeping through it.
- The prompt's Clojure `run_tests` note said the opposite of what the runner does. It claimed the
  managed REPL does "NOT reload namespaces automatically" and told a session to reload every
  changed *test* namespace, while the runner already `(require … :reload)`s (or `load-file`s)
  every namespace it RUNS — and never their dependencies. Proven at runtime: a poisoned Var in a
  test namespace came back restored from a run, the poisoned Var in the production namespace that
  test requires did not. The prompt line, `run-form`'s docstring and the repo guidance now name
  the real trap: a changed PRODUCTION namespace keeps the Vars the reused REPL already holds.
- A cancelled turn no longer keeps polling a shell wait to its own deadline. `sh.wait` samples the
  process tree's usage on every iteration, and that sampler spawns `ps` and calls `.waitFor` inside
  a best-effort `catch Throwable` — which caught the `InterruptedException` the JVM throws with the
  interrupt flag already CLEARED, so a cancellation landing in that window was swallowed and the
  wait ran on for up to ten minutes. The sampler now restores the flag and answers `nil`: the
  measurement is worthless after a cancel, the cancellation is not.
- The artifacts gallery no longer reads a wire key nobody sends. `collectArtifacts` asked each
  iteration for `tool_name` — a field that left the wire when the 21 native tools did — so it
  always answered `""` and the tile's screen-reader caption silently degraded from "produced in
  turn 6 by ..." to "produced in turn 6". The dead read, the `SessionArtifact.tool` field and the
  caption branch are gone rather than re-pointed at the iteration's op: with one door, naming the
  producer on every row is noise, and the turn plus the iteration it hangs off is the provenance
  that is real.
- `capfd` captures the REAL file descriptor in the sandbox pytest shim (issue #138): it used to
  be a second name for `capsys` — a `sys.stdout`/`sys.stderr` swap — so `os.write(1, ...)`, a
  C-level write or a child process's output never came back from `readouterr()`. Fd 1 and fd 2 are
  now redirected onto a drained pipe for the test's lifetime (no filesystem needed, so it works in
  a Context granted none), the descriptor's bytes follow the stream text in the same
  `CaptureResult`, and a tail nobody read is still replayed under the failure. `capsys` stays
  stream-only, exactly as real pytest does.
- Every model-facing document names a call the runtime accepts. `run_tests("python")` /
  `repl_eval("python")` never selected the Python pack: the language surface reads the pack from
  `{"language": "python"}` (or as the FIRST of two arguments), so a lone string was the PAYLOAD —
  `run_tests("python")` asked the Clojure runner for a `python` namespace, and
  `repl_eval("python")` evaluated `python` as Clojure. The token-optimization page also showed
  `struct_index({"path": …})` (the key is `paths`), a project-wide rename through
  `struct_patch({"paths": ["."]})` (there is no `paths` key — a rename batch is `edits`, whose
  entries inherit the shared top-level keys), a JSON `true` where Python needs `True`, and the
  positional `struct_index(paths)` / `struct_nodes(nodes)` spellings the one-options-map contract
  refuses. A corpus test now scans every document `doc`/`apropos` can hand back for those shapes.
- `run_tests({"language": "python"})` NAMES the tests that failed. Both backends now return every fault in
  `failures` / `errors` as `{ns, test, message, file, line}` — the project backend reads pytest's
  own `--junitxml` report, the hermetic GraalPy backend maps its per-test records — where before a
  run could report `fail: 1` and not one node id, because pytest's summary line carries counts
  only. The transcript cap in `output` is also cut in the MIDDLE now, keeping the session header
  AND the `FAILURES` section, the short test summary and the counts line, behind a marker that
  says how many characters it dropped; the old tail slice hid a whole `FAILURES` section behind a
  bare ellipsis.
- `attach(img, 'crop.png')` takes a PIL image, the way it already took a matplotlib figure. A
  picture cropped or composed in the sandbox fell through to the PATH branch and died with
  `attach: no such file: <PIL.Image.Image ...>`, so it had to be written to a temp file first.
  The FILENAME chooses the encoder (`shot.jpg` really stores a JPEG, converting an alpha mode the
  encoder cannot take; anything else is lossless PNG), and a source that is neither a path nor a
  producer is now refused by SHAPE — `attach: source must be a path, bytes, a PIL image or a
  matplotlib figure, got dict` — instead of having its repr reported as a missing file.

## [v0.1.35] - 2026-08-09

### Changed
- Release v0.1.35
- A wait is ONE budget for the whole batch, not one per command
- Drop README library packaging note
- Trim README install and runtime prose
- Simplify README install section
- Name the one shell tool `shell`
- Give apropos groups and every sandbox verb its raw-result doc
- Make `wait` the only difference between a run and a background shell
- Serve the whole session's artifacts from a metadata index
- PLAN: name Phase 5 by its commit
- Make every run a handle: a timeout is a wait that expired
- Stop forcing deferred work at namespace load, which native-image runs on the builder
- PLAN: name Phase 4 by its commit
- Give a background shell a log FILE and a byte OFFSET cursor
- Page a project from the list on screen, not the gateway's own window
- Delete the project-wide rename
- Ask the :fs/access gate from struct_rename too
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
- Give apropos groups and every sandbox verb its raw-result doc (07cf88d36)
- Make `wait` the only difference between a run and a background shell (634c0476c)
- Serve the whole session's artifacts from a metadata index (cac2e80a3)
- PLAN: name Phase 5 by its commit (4f9983d23)
- Make every run a handle: a timeout is a wait that expired (451a644a2)
- Stop forcing deferred work at namespace load, which native-image runs on the builder (8d0eae493)
- PLAN: name Phase 4 by its commit (1be019874)
- Give a background shell a log FILE and a byte OFFSET cursor (e98cc607e)
- Page a project from the list on screen, not the gateway's own window (a264ef547)
- Delete the project-wide rename (71f00d8c9)
- Ask the :fs/access gate from struct_rename too (a6557f244)
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
- Refuse toolchain output at the incidental capture tap
- Stop capturing the temp file nobody named
- PLAN: record Phase 3 as done
- Replace the `ls` native tool with a sandbox `ls()` helper
- Invent the provider the native suite talks to
- Companion: measure the "Latest" offer instead of remembering it
- Prove the native binary from its own suite, not from a docker build
- Companion: let an artifact tile show its own note, and keep its controls legible
- Companion: one Settings dialog, this device beside the machines
- Record Phase 2 in the plan
- Replace protected paths with one :fs/access gate hook
- Companion: search is a page, and the bar keeps two marks
- Prove the native TUI, the agent entrypoint and the zai provider in the image build
- Make the container image a base a deployment extends
- Companion: one document stack per step, and the row is the verb
- Pin the container agent's home to the vis user and prove it at build time
- Install a provisioning profile only where Xcode reads it
- Serve the container gateway from the native image
- Pin the transcript in the frame the keyboard shrinks the shell
- Let an attached page run its own script, never the app's origin
- Rename the sandbox attachment surface to plain verbs
- Make every way out say what it closes
- Give the companion's paint back to the components that own it
- Leave the image viewer through the app's one close
- Give every close mark the page's own ink
- Zoom a picture by the distance scrolled, and take Safari own pinch
- release: update release notes for v0.1.33

### Package changes

#### com.blockether/vis
- Release v0.1.34 (7e2d6cefa)
- Refuse toolchain output at the incidental capture tap (e8c512bae)
- Stop capturing the temp file nobody named (9386b5144)
- PLAN: record Phase 3 as done (e3240898f)
- Replace the `ls` native tool with a sandbox `ls()` helper (d3db4c514)
- Invent the provider the native suite talks to (5701c9d7e)
- Companion: measure the "Latest" offer instead of remembering it (871420423)
- Prove the native binary from its own suite, not from a docker build (72ef3fec8)
- Companion: let an artifact tile show its own note, and keep its controls legible (7c6fae0b9)
- Companion: one Settings dialog, this device beside the machines (df428cafe)
- Record Phase 2 in the plan (287bcda62)
- Replace protected paths with one :fs/access gate hook (49d5a182e)
- Companion: search is a page, and the bar keeps two marks (8b6bea53b)
- Prove the native TUI, the agent entrypoint and the zai provider in the image build (72bd2b6e4)
- Make the container image a base a deployment extends (2f36d3e44)
- Companion: one document stack per step, and the row is the verb (c173d5421)
- Pin the container agent's home to the vis user and prove it at build time (b2656c39a)
- Install a provisioning profile only where Xcode reads it (59c889f7b)
- Serve the container gateway from the native image (b067471d4)
- Pin the transcript in the frame the keyboard shrinks the shell (eb3303a52)
- Let an attached page run its own script, never the app's origin (5c0bcda60)
- Rename the sandbox attachment surface to plain verbs (3913d59c9)
- Make every way out say what it closes (b243f9cb7)
- Give the companion's paint back to the components that own it (a4eaf0e27)
- Leave the image viewer through the app's one close (00b058dd8)
- Give every close mark the page's own ink (80af604cf)
- Zoom a picture by the distance scrolled, and take Safari own pinch (b245817ce)
- release: update release notes for v0.1.33 (9d9bf9855)

#### com.blockether/vis-channel-tui
- Rename the sandbox attachment surface to plain verbs (3913d59c9)

#### com.blockether/vis-foundation-bridge
- Replace protected paths with one :fs/access gate hook (49d5a182e)

#### com.blockether/vis-persistance-sqlite
- Rename the sandbox attachment surface to plain verbs (3913d59c9)



## [v0.1.33] - 2026-08-08

### Changed
- Release v0.1.33
- Put the search field and every transcript card header on one height
- Take the machine strip's side edges from the page
- Page grep results with offset and next_offset
- Let a session row fill its swipe track
- Anchor the companion search field to the bar's trailing edge
- Default grep to 50 elements, filename fallback included
- Frame a sent picture like a produced one and gallery several
- Stretch a row-ending icon button at mouse density too
- Put search back on the app bar with its own magnifying glass
- Give search its own band on a phone and land Clear on the field's edge
- Render MetaButton's children so the composer strip has words again
- Name a button by its rank and give the composer one strip
- Give the companion one chip, one row, one disclosure and one remove
- Show a saved artifact revision without refetching the transcript
- Give the opened document the one header band and the one button
- Remove the copy, move, delete, create_directory and file_exists tools
- Start a stroke from beside the picture, not only on its edge
- Release the companion app locally when this machine can sign
- Enforce :ext/protected-paths in the Python sandbox filesystem
- Say the create inside its button and shrink the row question
- Show a document artifact once, as a card that opens over everything
- Paint the star action yellow and keep the starred row in view
- Let the app stop the turn it started again
- Never link a TestFlight build to an internal beta group
- Give a note ten annotation threads and a comment on the whole document
- Push from workflows through one shared git-push action
- Mark annotations in theme colours and annotate plain text too
- Make the machine switcher square, unread a highlight, and hide it for a solo machine
- Draw a comment ordinal as a plain coloured number
- Accept any spelling of a path in vis_attach
- Clear the NEW badge on the row you just read
- Make the fleet switcher one segmented track
- Put the session star immediately right of the title
- Number and colour markdown comments, and underline the passage each is about
- Clear the machine card below the fleet strip
- Pin the companion's artifact-revision save URL to its route
- Read a note inline as source and comment on it by tapping
- Pour the page ink into Add machine and lift the fleet strip
- Put the machine tabs and Add machine on one control height
- Give the phone a full-bleed sessions card with a fixed height
- Link every TestFlight build to every beta group
- Name the machine strip's pairing verb Add machine
- Pair a machine from the tab strip's own + button
- Abandon the stroke a pinch turned out to be
- End the machine card where its content ends
- Give the session list one meta ink and a closed machine card
- Let the viewer zoom out to 50%
- Write PDF annotations with the maintained pdf-lib fork
- Annotate every opened artifact: comments on notes, ink on PDFs and pictures
- Stand the machine tabs outside the machine card and drop the All scope
- Give a document preview an Open chip that fills the screen
- Remove the bin/vis-agent launcher tests
- Keep a Python extension loaded between tests that ask for the same one
- Restore the transcript copy chip's own look
- Give the app bar search the buttons own face
- Give the transcript's copy chip the app's own button and one preview box
- Open every artifact at full height
- Render markdown artifacts and let a human comment on them
- Make search the app bar and move pairing into preferences
- Make Return type a new line on phones and dismiss the keyboard on send
- Collapse vis_attach_bytes into vis_attach
- Show document artifacts plainly: no draw, hide or new tab
- Stop shelling out to real git in the tests
- Give the app bar and machine verbs a real button face
- Label every companion verb and move the machines into the header
- Pair from the app bar and show one fleet strip for any fleet size
- Make pairing a chip and drop the tab bar
- Key the shared test GraalPy sandbox so an abusive suite keeps its own
- Remove the ACP extension
- Make the machine a chip, not a second header band
- Require UI proposals as ASCII sketches, not app-built galleries
- Remove model, reasoning, verbosity, drafts, magit palette verbs and /export-html
- Keep a renamed machine name across dev reloads
- Close the gap between a session row and its disclosure
- Cut the paramiko and sandbox-fd test walls
- Rename a machine on its own header and add a project in one tap
- Route overlay card justification through the shared run justifier
- End the phone status on the timestamp edge, with the flags against it
- Cut the wall-clock waste out of the slowest gateway and loop tests
- Pin svar 0.7.109 for the 2-minute retrying TTFT watchdog
- Put the machine header two verbs on the band as + and gear
- Cap the session pager to a centred cluster
- Sit the phone row flags beside the status they qualify
- Align header names on one glyph column and always print the tally noun
- Give the first token two minutes instead of five
- Give session-row flags their own column
- Hold the pager steps in fixed slots so > never moves
- Page the session list with numbered, jumpable pages
- Add a sharded parallel test runner
- Always offer the draft half and delete the Offer drafts setting
- Wall the test suite off from the public internet
- Make Manage projects the same anchored panel as the draft picker
- Frame transcript media as a plate with a docked filename label
- Move the draft verb onto the project header as a split button
- Show a human-input pause only on its own session's tab
- Draw the draft mark as a forked project folder
- Add hard no-profanity rule to AGENTS.md
- Mark the draft verb with its own folder icon
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
- Read ui source through Vite raw import in the sheet pin
- Make every dialog a bottom sheet on phones
- Align companion trailing controls and drop the glyph hover frame
- Keep inline images visible when a transient band opens
- Shrink compact header buttons to a 32px face with a 44px touch target
- Paint the transient band flat on the terminal's own paper
- Load the TUI provider dialog from one gateway call
- Close the transient band around its hint bar on slightly darker paper
- Render TUI diff fences compact instead of side-by-side
- Border the transient band, drop its tinted paper and column rules
- Pin svar 0.7.107 for uncompressed SSE streaming
- Never title a session after a pasted image's clipboard path
- Order provider limit windows shortest-first (5h before 7d)
- Keep a TUI tab's reading position across a workspace switch
- Remember where a session transcript was being read
- Never animate auto-follow scrolling in the TUI
- Drop tool name from pending-summary docstring example
- Drop unused form display exports and the dead auto-repaired flag
- Drop a tab's stale layout on switch so the view stops self-scrolling
- Correct label-overrides docstring after the shell/fs tool split
- Drop same-path coalescing and the running-code-tools exception
- Land the TUI scroll on a terminal resize instead of easing to it
- Purge legacy fs/shell tool names from form and loop tests
- Split the `shell` and `fs` mega-tools into named verbs
- Print a TUI notice's sentence without its machine code
- Unify the transient band into one embeddable component
- Anchor every main-screen transient through one band anchor
- Adopt svar 0.7.106 so a declared retry cooldown is waited out
- Ink the transient band's title on its opening rule and move the palette into Tools
- Serve a live turn's text-named images from the gateway
- Anchor in-session transients above the prompt on their own paper
- Report total line counts in a collapsed multi-file patch headline
- Give a form one text column by moving the focus ring out of it
- Report added/removed/modified line counts on every edit summary
- Stop blaming the provider for a turn that never reached one
- Abandon a wedged session engine instead of queueing behind it
- Give every companion list header one band
- Pin the turn soul on every attachment row, tool artifacts included
- Give every attachment descriptor its turn id
- Answer every cancel, even when the tab paints no live turn
- Give every header one trailing cluster and one kebab
- Share one overflow button and one header action cluster in the companion
- Unify companion overflow menus, icon buttons, and dialog closes
- Return attachment descriptors and drop the answer gallery
- Search the zipper tree with one parse instead of re-parsing per node
- Give a form field's input its own row of air and stop indenting toggles
- Bump svar to 0.7.105
- Decode Python provider maps from declared field tables
- Give every C-x category its own column
- Open one blank row under every human-input label
- Lay a tall transient band out in which-key columns
- Make C-x a transient hydra band
- Ask a band's follow-up question in the band's own frame
- Give every managed nREPL its own log file
- Scope the tid-less cancel to the turn its caller submitted
- Let Rift hand a clean draft over without pending changes
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
- Take the machine strip's side edges from the page (f17a0baa9)
- Page grep results with offset and next_offset (be31c6165)
- Let a session row fill its swipe track (94f996d60)
- Anchor the companion search field to the bar's trailing edge (3102648f0)
- Default grep to 50 elements, filename fallback included (d6b9d2253)
- Frame a sent picture like a produced one and gallery several (116c59834)
- Stretch a row-ending icon button at mouse density too (c85b77e9a)
- Put search back on the app bar with its own magnifying glass (ba9296498)
- Give search its own band on a phone and land Clear on the field's edge (85d9d3b4c)
- Render MetaButton's children so the composer strip has words again (3bae6b37d)
- Name a button by its rank and give the composer one strip (bb59694f1)
- Give the companion one chip, one row, one disclosure and one remove (0d3274b0b)
- Show a saved artifact revision without refetching the transcript (94b26fdc5)
- Give the opened document the one header band and the one button (b430dea50)
- Remove the copy, move, delete, create_directory and file_exists tools (87c1562ea)
- Start a stroke from beside the picture, not only on its edge (686ea2827)
- Release the companion app locally when this machine can sign (7ad4c1123)
- Enforce :ext/protected-paths in the Python sandbox filesystem (b11aa1706)
- Say the create inside its button and shrink the row question (2d4b8e7ec)
- Show a document artifact once, as a card that opens over everything (65cf41550)
- Paint the star action yellow and keep the starred row in view (8ead5cd33)
- Let the app stop the turn it started again (421734b0e)
- Never link a TestFlight build to an internal beta group (6cef66c7c)
- Give a note ten annotation threads and a comment on the whole document (8b832e827)
- Push from workflows through one shared git-push action (5f4e33ac9)
- Mark annotations in theme colours and annotate plain text too (7c3ba0413)
- Make the machine switcher square, unread a highlight, and hide it for a solo machine (305cd6789)
- Draw a comment ordinal as a plain coloured number (53d371558)
- Accept any spelling of a path in vis_attach (d383fed53)
- Clear the NEW badge on the row you just read (aea33924a)
- Make the fleet switcher one segmented track (050cb9b70)
- Put the session star immediately right of the title (c8b6263e2)
- Number and colour markdown comments, and underline the passage each is about (1167b97b4)
- Clear the machine card below the fleet strip (7d1bfb53f)
- Pin the companion's artifact-revision save URL to its route (637600b9b)
- Read a note inline as source and comment on it by tapping (1a706229c)
- Pour the page ink into Add machine and lift the fleet strip (e0918c09d)
- Put the machine tabs and Add machine on one control height (02700e1ed)
- Give the phone a full-bleed sessions card with a fixed height (8e771c3e9)
- Link every TestFlight build to every beta group (9142379af)
- Name the machine strip's pairing verb Add machine (41715e01e)
- Pair a machine from the tab strip's own + button (ec71c1c2a)
- Abandon the stroke a pinch turned out to be (ebcc90c27)
- End the machine card where its content ends (e41345a66)
- Give the session list one meta ink and a closed machine card (2e312c16f)
- Let the viewer zoom out to 50% (f93e02b32)
- Write PDF annotations with the maintained pdf-lib fork (1a0d386d5)
- Annotate every opened artifact: comments on notes, ink on PDFs and pictures (7fde3d069)
- Stand the machine tabs outside the machine card and drop the All scope (eec70eb58)
- Give a document preview an Open chip that fills the screen (76164492e)
- Remove the bin/vis-agent launcher tests (b3f98438a)
- Keep a Python extension loaded between tests that ask for the same one (efb5d97d2)
- Restore the transcript copy chip's own look (82383147c)
- Give the app bar search the buttons own face (3ed7d5270)
- Give the transcript's copy chip the app's own button and one preview box (6586e491d)
- Open every artifact at full height (a4e500782)
- Render markdown artifacts and let a human comment on them (c4a1ea54f)
- Make search the app bar and move pairing into preferences (7308e9c00)
- Make Return type a new line on phones and dismiss the keyboard on send (da74a5be7)
- Collapse vis_attach_bytes into vis_attach (1afa4df70)
- Show document artifacts plainly: no draw, hide or new tab (774cfb6a8)
- Stop shelling out to real git in the tests (e0d8aa5ca)
- Give the app bar and machine verbs a real button face (b498ea4e9)
- Label every companion verb and move the machines into the header (cc7f57260)
- Pair from the app bar and show one fleet strip for any fleet size (50cadeda4)
- Make pairing a chip and drop the tab bar (975a0d4b7)
- Key the shared test GraalPy sandbox so an abusive suite keeps its own (545cad9e5)
- Remove the ACP extension (8e6e9e413)
- Make the machine a chip, not a second header band (6e9e6ef28)
- Require UI proposals as ASCII sketches, not app-built galleries (afac2f6ae)
- Remove model, reasoning, verbosity, drafts, magit palette verbs and /export-html (0d255f454)
- Keep a renamed machine name across dev reloads (13aafb0b7)
- Close the gap between a session row and its disclosure (bb611b512)
- Cut the paramiko and sandbox-fd test walls (682e4206e)
- Rename a machine on its own header and add a project in one tap (3295bc072)
- End the phone status on the timestamp edge, with the flags against it (0687d2486)
- Cut the wall-clock waste out of the slowest gateway and loop tests (375fce532)
- Pin svar 0.7.109 for the 2-minute retrying TTFT watchdog (4de9b8291)
- Put the machine header two verbs on the band as + and gear (e1b46d338)
- Cap the session pager to a centred cluster (cbeb3e224)
- Sit the phone row flags beside the status they qualify (52012ca27)
- Align header names on one glyph column and always print the tally noun (f3f29d16a)
- Give the first token two minutes instead of five (75fae850f)
- Give session-row flags their own column (620af0f38)
- Hold the pager steps in fixed slots so > never moves (0b92b77b3)
- Page the session list with numbered, jumpable pages (6db2a1982)
- Add a sharded parallel test runner (293ea7e19)
- Always offer the draft half and delete the Offer drafts setting (0561f5fd6)
- Wall the test suite off from the public internet (1275bf716)
- Make Manage projects the same anchored panel as the draft picker (3f617032b)
- Frame transcript media as a plate with a docked filename label (1bfa6855e)
- Move the draft verb onto the project header as a split button (07ed1075f)
- Draw the draft mark as a forked project folder (030af03e3)
- Add hard no-profanity rule to AGENTS.md (d0045b87f)
- Mark the draft verb with its own folder icon (13529ded0)
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
- Read ui source through Vite raw import in the sheet pin (1e2bd6cda)
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
- Split the `shell` and `fs` mega-tools into named verbs (5846c9dc9)
- Adopt svar 0.7.106 so a declared retry cooldown is waited out (492c93ff8)
- Serve a live turn's text-named images from the gateway (31970dec1)
- Report total line counts in a collapsed multi-file patch headline (a357ce607)
- Report added/removed/modified line counts on every edit summary (1fb5fe24f)
- Stop blaming the provider for a turn that never reached one (0b8194c57)
- Abandon a wedged session engine instead of queueing behind it (3e813c15a)
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
- Let Rift hand a clean draft over without pending changes (8de1c5504)
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
- Paint the transient band flat on the terminal's own paper (f670c026d)
- Load the TUI provider dialog from one gateway call (c181c0896)
- Close the transient band around its hint bar on slightly darker paper (d639c94cd)
- Render TUI diff fences compact instead of side-by-side (cf36e81c8)
- Border the transient band, drop its tinted paper and column rules (12f68cc6d)
- Order provider limit windows shortest-first (5h before 7d) (e50e51b0b)
- Keep a TUI tab's reading position across a workspace switch (c896ed12d)
- Never animate auto-follow scrolling in the TUI (6b2a8111e)
- Drop a tab's stale layout on switch so the view stops self-scrolling (8d2b238ce)
- Drop same-path coalescing and the running-code-tools exception (6611bbb1b)
- Land the TUI scroll on a terminal resize instead of easing to it (c6dd2fe6f)
- Print a TUI notice's sentence without its machine code (e7146ae58)
- Unify the transient band into one embeddable component (3fc733b99)
- Anchor every main-screen transient through one band anchor (2ff6b8f0d)
- Ink the transient band's title on its opening rule and move the palette into Tools (4be2df9f7)
- Anchor in-session transients above the prompt on their own paper (b9a9db2d2)
- Give a form one text column by moving the focus ring out of it (699faae0d)
- Answer every cancel, even when the tab paints no live turn (8e5f97569)
- Return attachment descriptors and drop the answer gallery (2d29bf3e9)
- Give a form field's input its own row of air and stop indenting toggles (bfd1c544e)
- Decode Python provider maps from declared field tables (7ae92e716)
- Give every C-x category its own column (151340f56)
- Open one blank row under every human-input label (9bc3de016)
- Lay a tall transient band out in which-key columns (1d67bfabb)
- Make C-x a transient hydra band (f20d4c7aa)
- Ask a band's follow-up question in the band's own frame (5414f777b)
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
- Cut the wall-clock waste out of the slowest gateway and loop tests (375fce532)
- Pin the turn soul on every attachment row, tool artifacts included (ab7a722da)
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
- Match fleet header typography
- Make fleet headers and attachments easier to use
- Use the shared Button component for doc-artifact toolbar controls
- companion: let a pinch start while a stroke is in progress
- Fold the artifacts sheet's title band into its filter strip
- Drop oversized py-2 override from image-viewer/annotation buttons
- Raise ArtifactsSheet z-index above transcript copy buttons
- Bridge extension: throw on missing :workspace/root instead of falling back to user.dir
- companion: resume last session on cold hashless relaunch
- Drop bossy "ATTACH ONE OR TWO PER TURN" framing from attach shim's prompt-facing description
- Print every shim's supported surface in the system prompt
- Write every shim description as an editable multi-line str
- Tell the write side to keep one document under one name
- Version artifacts so a name is one continuous thread of work
- Hardcode workspace backend to Rift, remove pluggable backend registry
- Align fleet kebab flush-right and match project header typography
- Add vis-agent gateway mcp CLI commands
- Replace ⋯ glyph with a proper DotsIcon in the icon set
- Remove the redundant fleet bar ⋯ — one kebab per machine header only
- Rename release-companion skill to release-companion-hotfix
- Remove the /clear slash command
- Fix iOS keyboard glitch when tapping a slash command
- Put every machine verb behind one ⋯ menu and let a session pick its project
- Suspend the zoom-viewer snap transition during a live pinch/pan
- Fit the composer text to its own line box
- Loosen the small type steps and refuse a hardcoded line-height
- release: update release notes for v0.1.31
- Draw the design board with the app's own list components

### Package changes

#### com.blockether/vis
- Release v0.1.32 (6d4bf9101)
- Drop stale workspace backend re-exports from core (5609ac9f2)
- Allow per-model provider API styles (efb7b129d)
- Match fleet header typography (1cf300753)
- Make fleet headers and attachments easier to use (93d4aff0b)
- Use the shared Button component for doc-artifact toolbar controls (c17473390)
- companion: let a pinch start while a stroke is in progress (5ed890df4)
- Fold the artifacts sheet's title band into its filter strip (48fde2874)
- Drop oversized py-2 override from image-viewer/annotation buttons (9b344dc58)
- Raise ArtifactsSheet z-index above transcript copy buttons (9abaf9a00)
- companion: resume last session on cold hashless relaunch (ee72aad57)
- Drop bossy "ATTACH ONE OR TWO PER TURN" framing from attach shim's prompt-facing description (344923028)
- Print every shim's supported surface in the system prompt (da99aadd0)
- Write every shim description as an editable multi-line str (64fcd44c1)
- Tell the write side to keep one document under one name (06424ae2f)
- Version artifacts so a name is one continuous thread of work (4e5ba78ab)
- Hardcode workspace backend to Rift, remove pluggable backend registry (a84314623)
- Align fleet kebab flush-right and match project header typography (c269eefa3)
- Add vis-agent gateway mcp CLI commands (74f26e03c)
- Replace ⋯ glyph with a proper DotsIcon in the icon set (52280c55e)
- Remove the redundant fleet bar ⋯ — one kebab per machine header only (32f70e99b)
- Rename release-companion skill to release-companion-hotfix (386459ad6)
- Remove the /clear slash command (398d28da8)
- Fix iOS keyboard glitch when tapping a slash command (23cbf2f1b)
- Put every machine verb behind one ⋯ menu and let a session pick its project (62daee7ea)
- Suspend the zoom-viewer snap transition during a live pinch/pan (577388d1f)
- Fit the composer text to its own line box (c877137e0)
- Loosen the small type steps and refuse a hardcoded line-height (5f45e7f0b)
- release: update release notes for v0.1.31 (507ca8dc1)
- Draw the design board with the app's own list components (0f423eb9b)

#### com.blockether/vis-foundation-bridge
- Bridge extension: throw on missing :workspace/root instead of falling back to user.dir (cb4e63b8e)

#### com.blockether/vis-persistance-sqlite
- Version artifacts so a name is one continuous thread of work (4e5ba78ab)



## [v0.1.31] - 2026-08-05

### Changed
- Release v0.1.31
- Prove format_code/lint_code's invoke-symbol-wrapper respects draft cwd
- Prove native-handler workspace-root fix covers repl/repl_connect/repl_eval
- Coerce the fold ledger's own anchor grammar in session_fold
- Draw the proposal in the app's own chrome
- Bind workspace context for native handler-tool dispatch
- Add opencode-go provider with per-model wire routing
- Draw the path pencil as ink, not a box
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
- Compose every band on one embed-transient! seam
- Revert "Rename bar identifiers to baz"
- feat(tui): answer every /draft slash with the draft band
- fix(gateway,hitl): one sid spelling in the registry, lock-free hydrate, OTP as a secret
- Serve ls of an unindexable directory from fff itself
- Split the draft band into create, switch and abandon commands
- Merge the draft transient's Create and Danger groups into Actions
- Make drafts a magit transient band
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
- Separate the artifacts band from the dark chrome above it
- Find search hits the session list has not paged in, and date the rows
- Draw every companion icon at one optical size
- Stream empty-reply resends live and name why each provider call exists
- Pretty-print nested status_fn maps in provider status text
- Draw every control mark in the companion as a real icon
- Size the artifacts chip to the session id beside it and give it a paperclip
- Make the favorite star icon truly yellow
- Keep the caller's session inside a bounded provider probe
- Make failed-turn error cards selectable in the TUI transcript
- Give the artifacts sheet the dialog band's own height
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
- Ship the artifacts sheet: one index of everything a session produced
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
- Gate advertised :strict on the request's own wire
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
- Coerce the fold ledger's own anchor grammar in session_fold (52d4e4c3a)
- Draw the proposal in the app's own chrome (d3187b86a)
- Bind workspace context for native handler-tool dispatch (6afd4bf25)
- Add opencode-go provider with per-model wire routing (a6dc50277)
- Draw the path pencil as ink, not a box (9a13e8a00)
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
- Separate the artifacts band from the dark chrome above it (14a251b7f)
- Find search hits the session list has not paged in, and date the rows (f79fba147)
- Draw every companion icon at one optical size (770e51cda)
- Stream empty-reply resends live and name why each provider call exists (8569d0fed)
- Pretty-print nested status_fn maps in provider status text (27beac717)
- Draw every control mark in the companion as a real icon (5345456ec)
- Size the artifacts chip to the session id beside it and give it a paperclip (01d4a9eac)
- Make the favorite star icon truly yellow (674b790cd)
- Keep the caller's session inside a bounded provider probe (1e08f32a1)
- Give the artifacts sheet the dialog band's own height (702eb438d)
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
- Ship the artifacts sheet: one index of everything a session produced (d62ac00b4)
- Keep trusted extension shell outside the jail (a6aecf9f9)
- Guard collapsed tool result memory (38f630b98)
- Unify filesystem tool input schema (f62d39a97)
- Allow trusted extensions to spawn subprocesses (cf6498c3a)
- Extract the annotation stack into reusable single-purpose modules (d88225c54)
- Make the artifacts gallery proposals operable and touch-sized (66166db20)
- Stop advertising strict tools on every wire (6d60bf87f)
- Log the tool-call transport wreckage the door drops (b6a5630e0)
- Gate advertised :strict on the request's own wire (0276623e9)
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
- Compose every band on one embed-transient! seam (aa009dad5)
- Revert "Rename bar identifiers to baz" (8105572f7)
- feat(tui): answer every /draft slash with the draft band (4dcfef4e7)
- fix(gateway,hitl): one sid spelling in the registry, lock-free hydrate, OTP as a secret (fd152579d)
- Split the draft band into create, switch and abandon commands (35adceb1e)
- Merge the draft transient's Create and Danger groups into Actions (2f39dafc8)
- Make drafts a magit transient band (d1cef6941)
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
- Ship a dark-theme vis logo and un-matte the transparent marks
- Fix numpy and PIL sandbox shim gaps for image work
- Take the keyboard down before the attachment sheet
- Delete a session without waiting on its live teardown
- Point the design-shots section at cap/shot!
- Make TUI screenshots one call and paint italic and underline like a terminal
- Build the OAuth file refresher lazily so its lock path is the user's
- Re-anchor the start menu on resize instead of closing it
- Fix disclosure copy targets landing one row above the painted body
- Photograph where the Blockether yellow goes on the start menu
- Read the environment at runtime, not at native-image build time
- Give every machine its own coloured rail in the sessions list
- Give every commit on main its own CI run
- Rasterize a bold run as bold, not as the mono face's only weight
- Never cancel a CI run for the commit already on main
- Select the runtime an update names
- Invert the vis-table card header onto its own dark band
- Clone the dev checkout on main when there is none
- Separate machines in the sessions list with air and a banner
- Keep the vis-table card's row bands inside the frame
- Dress the vis-table card as a sheet: muted rules, banded head, zebra rows
- Prove the canvas flush beats reflection, not a stopwatch
- Float sessions holding unsent work and keep their attachments
- Offer none authentication in the paramiko shim SSH server
- Make the urllib3 shim a package, not one flat module
- Lighten the live badge to a green FILL, not the green ink
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
- Cap a turn whose provider never answered at a tight first-output ceiling
- Stop cancel from settling a live turn's durable row
- Keep attached CSV rows out of model context and rebuild the grid
- Fail a Python lint/format target that holds no Python
- Bracket the live count in a scope chip
- Name the publisher's relay by default on the gateway too
- Let the scope strip own the fleet's live and unread counts
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
- Name the relay a machine chose, and say so when it refuses one
- Make the requests and httpx shims behave like the real clients
- Ask a stumbling relay twice and never hand a grant to cleartext
- Fetch the relay healthcheck URL the workflow was given
- Register through the relay when a machine holds no push key
- Expose httpx.Response.elapsed in the sandbox shim
- Price strict tools by the provider's real grammar slots
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
- Report companion search as a fleet question
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
- Detect a dead GraalPy context by asking it, not by matching error text
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
- Guarantee every wire image is measured under the pixel ceiling
- Normalize svar tool calls to strings at one engine door
- Cap wire image dimensions so many-image requests survive
- Pin the owned source checkout instead of cloning it
- Cross the PIL draw bridge once per run and convert rasters without reflection
- Glue the magit transient band to the dialog's bottom chrome
- Pin that a tapped notification lands on its session screen
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
- Never prompt for a credential the machine mints itself
- release: update release notes for v0.1.27

### Package changes

#### com.blockether/vis
- Release v0.1.28 (95469bf91)
- Serve the installer and the vis-agent command as release assets (e1f106241)
- Crown the start menu with the Blockether yellow (49ccf1b15)
- Keep the start menu reading parked drafts when the menu is re-anchored (18eadb087)
- Ship a dark-theme vis logo and un-matte the transparent marks (ec8343973)
- Fix numpy and PIL sandbox shim gaps for image work (5d1944e16)
- Take the keyboard down before the attachment sheet (8f2a0f339)
- Delete a session without waiting on its live teardown (7d90486fb)
- Point the design-shots section at cap/shot! (d45ddf0e0)
- Make TUI screenshots one call and paint italic and underline like a terminal (367bbe19b)
- Build the OAuth file refresher lazily so its lock path is the user's (0ff2630a3)
- Re-anchor the start menu on resize instead of closing it (a859de278)
- Photograph where the Blockether yellow goes on the start menu (19416e983)
- Read the environment at runtime, not at native-image build time (0d980d92c)
- Give every machine its own coloured rail in the sessions list (e8b2b563e)
- Give every commit on main its own CI run (e819e86c4)
- Never cancel a CI run for the commit already on main (451fa1e36)
- Select the runtime an update names (a09ee8b9c)
- Clone the dev checkout on main when there is none (3f0f26627)
- Separate machines in the sessions list with air and a banner (ed538a7a4)
- Prove the canvas flush beats reflection, not a stopwatch (0bf4e7d20)
- Float sessions holding unsent work and keep their attachments (563738cdd)
- Offer none authentication in the paramiko shim SSH server (ca2d8e1b0)
- Make the urllib3 shim a package, not one flat module (297543cf0)
- Lighten the live badge to a green FILL, not the green ink (86575fd5e)
- Rasterize captured TUI frames in Clojure, in the theme's own colours (7d62afb1b)
- Report the inspected session's turn and form-level failures (9f700957a)
- Ignore Wrangler local dev state (d05b62c20)
- Give the live count the unread badge's filled block, in green (6480d32d4)
- Refresh the companion incremental build info (0cdb59d82)
- Record svar 0.7.100 in the dependency audit (6b7b4cbaa)
- Judge the PIL pixel-read budget by canvas ratio, not wall clock (9641dffef)
- Report the real cause when a lazy Python shim fails to import (a60ade92e)
- Pin ruff at 0.3.4, whose reported version is the released one (36d8e805b)
- Cap a turn whose provider never answered at a tight first-output ceiling (6060ebdcc)
- Stop cancel from settling a live turn's durable row (0a92426f7)
- Keep attached CSV rows out of model context and rebuild the grid (0e4242f92)
- Bracket the live count in a scope chip (afa1480a1)
- Name the publisher's relay by default on the gateway too (774e9b99e)
- Let the scope strip own the fleet's live and unread counts (31c4bb444)
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
- Name the relay a machine chose, and say so when it refuses one (1e938b352)
- Make the requests and httpx shims behave like the real clients (fe5175e4b)
- Ask a stumbling relay twice and never hand a grant to cleartext (63b849b87)
- Fetch the relay healthcheck URL the workflow was given (300444b66)
- Register through the relay when a machine holds no push key (8f0d4a259)
- Expose httpx.Response.elapsed in the sandbox shim (ea0f30b75)
- Price strict tools by the provider's real grammar slots (b6e72492c)
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
- Report companion search as a fleet question (ab5bc6be4)
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
- Detect a dead GraalPy context by asking it, not by matching error text (15df749f7)
- Heal Python extension symbols whose context was torn down (a1654168c)
- Pin svar 0.7.98 so tool arguments are never interned (640085960)
- Narrow the tool-call door to model drift and extension EDN (7a5b76538)
- Bind the session environment for Python hook callbacks (133de48cc)
- Record the measured cost of dropping builder swap and heap limits (c13bb7fb9)
- Measure the wire image cap on the base64 payload (0dfa76363)
- Stamp every Python sandbox shim with __file__ and __version__ (a176541f4)
- Pin svar 0.7.97 for strings-only tool arguments (a8443d0d9)
- Drop launcher-owned runtime and update commands from the binary (555a52d75)
- Guarantee every wire image is measured under the pixel ceiling (9782a139f)
- Normalize svar tool calls to strings at one engine door (583c359e8)
- Cap wire image dimensions so many-image requests survive (5b8d5df2f)
- Pin the owned source checkout instead of cloning it (bf42529be)
- Cross the PIL draw bridge once per run and convert rasters without reflection (75f894a5a)
- Glue the magit transient band to the dialog's bottom chrome (b12b18a4c)
- Pin that a tapped notification lands on its session screen (dab963288)
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
- Never prompt for a credential the machine mints itself (543df0972)
- release: update release notes for v0.1.27 (14db65ea6)

#### com.blockether/vis-channel-tui
- Make TUI screenshots one call and paint italic and underline like a terminal (367bbe19b)
- Fix disclosure copy targets landing one row above the painted body (c2cd40d3b)
- Read the environment at runtime, not at native-image build time (0d980d92c)
- Rasterize a bold run as bold, not as the mono face's only weight (46590f686)
- Invert the vis-table card header onto its own dark band (52c059fab)
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
- Glue the magit transient band to the dialog's bottom chrome (b12b18a4c)
- Anchor magit transient band to its own hint bar (dcc6d81ec)
- Never prompt for a credential the machine mints itself (543df0972)

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
- Make the magit transient a full-bleed band with a title margin
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
- Make the magit transient a full-bleed band with a title margin (30fc5386e)

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
- Underline markdown table links and pin the painted click region
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
- Wrap dialog descriptions at the painted width and survive tiny terminals
- Render optional dialog description above human-input fields
- Enforce snake_case string keys at the Python human-input seam
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
- Never read a filtered .yarn artifact pair as an agent deletion
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
- Enforce snake_case string keys at the Python human-input seam (609374929)
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
- Never read a filtered .yarn artifact pair as an agent deletion (024bfc742)
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
- Underline markdown table links and pin the painted click region (0270524f4)
- Make markdown links inside table cells clickable (71f3950a5)
- Paint a failed call's error line red (ed0eccaa8)
- Settle a cancelled turn in one frame (9730a9d02)
- Cover human-input dialog queueing in the TUI state store (24ef40908)
- Wrap dialog descriptions at the painted width and survive tiny terminals (483947fd6)
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

- human-input: an extension can pause its run and ask the operator a typed
  question. `vis.ask(title, fields, **options)` (Clojure:
  `vis/request-human-input!`) blocks the calling extension until a human answers,
  and the request rides every channel at once — the TUI paints a form dialog, the
  gateway publishes `human_input.request` / `human_input.close` as session events
  and serves `GET /v1/sessions/:sid/human-input` plus per-request submit and
  cancel actions, and the companion app renders the same fields (and gets a push
  notification for the block). Field types are `plaintext`, `password`,
  `multiline`, `select`, `multiselect`, and `checkbox`, each with `name`,
  `label`, `description`, `default`, `placeholder`, `max_length`, and an
  `is_required` that both dialogs and the HTTP seam enforce. Every key is a
  snake_case string; a camelCase or kebab-case spelling is refused with the right
  name rather than silently ignored, so a required field can never turn optional.
  Cancelling or timing out never raises: it returns a falsey `Answer` whose
  `reason` says which. A `password` answers with an opaque `vis-secret:` handle —
  transcript, logs, and the model see only the handle, `answer.reveal(name)`
  resolves the plaintext in-process, and `vis.forget(handle)` drops it.
  Documented in `resources/vis-docs/extending.md`.

- config: a `workspace.filesystem` entry can say WHERE it mounts. `when.os`
  (`macos`, `linux`, `wsl`, `windows`) and `when.exists: <path>` gate a root on
  the host, and `optional: true` skips a declared root whose own path is absent,
  so one `vis.yml` serves a laptop, a workstation and CI. A gated id may stay in
  `jail.filesystem.allow` on every machine: roots this host does not mount are
  dropped before the jail is built instead of failing the config as an unknown
  id. `doctor` — and the startup hint that reuses it — now reports every root
  that did not mount as written: `info` for a conditional root the host skipped,
  a warning for an admitted root whose path is missing. Documented in
  `resources/vis-docs/sandbox.md`.
- config: `jail.mach_services` opens macOS Mach lookups to a confined child.
  `keychain: true` allows `com.apple.SecurityServer`, `com.apple.ocspd` and
  `com.apple.trustd.agent` and grants read access to `~/Library/Keychains` and
  `/Library/Keychains` (kept out of the default search sweep) — which is what
  makes `security`, `gh auth token` and `git credential-osxkeychain get` work
  inside a Seatbelt jail; `allow` names any further service by global name.
  Deny stays the default, and a command that fails on a lookup the jail did not
  grant now carries a `note` naming the denial and the setting that lifts it,
  instead of only the opaque Security-framework message. Documented in
  `resources/vis-docs/sandbox.md`.

### Changed

- change(drafts): a draft now records HOW it was physically made, not just who
  made it. rift 0.0.10-10 reports the copy mechanism it actually used
  (`btrfs`, `reflink`, `apfs`, `worktree`, `copy`) and vis persists it as the
  workspace's `workspace_mechanism`, so a clone on a filesystem without
  copy-on-write is labelled `worktree` — the linked Git worktree it really is —
  instead of being described by the backend's name. A backend that reports no
  mechanism (or an older native library) stores NULL, and every existing draft
  keeps working unchanged.

- change(drafts): the draft itself now says what its fork skipped. rift 0.0.10-9
  records every pruned path in the workspace marker at the clone root, and
  `deleted-paths` reads that record instead of mirroring the backend's filter
  rules, which vis had to keep in sync by hand and twice failed to (a tracked
  `dist`, a committed `.yarn/cache`). Same release keeps a git-TRACKED artifact
  directory in the clone, so a draft's `git status` matches its source's. The
  mirrored name lists are gone; a clone with no marker falls back to trunk's own
  ignore rules, so older drafts behave exactly as before.

### Fixed

- fix(native-image): the native binary no longer aborts with "Cannot reflectively
  invoke constructor 'public java.math.BigInteger(java.lang.String)'". Loading any
  YAML document that contains a plain integer runs
  `yamlstar.numbers/parse-safe-integer`, i.e. `clojure.core/bigint` on the raw
  scalar STRING, and that ends in an untyped `(BigInteger. x)` — a
  `clojure.lang.Reflector` call the image had no metadata for, so `vis doctor`,
  `vis sessions list`, `vis providers status` and one-shot prompts died on startup
  in every workspace but the vis repository root. vis's own
  `reachability-metadata.json` now registers `BigInteger(String)` plus the
  `BigInteger(String,int)` that `clojure.tools.reader` uses for integer literals,
  and `com.blockether.vis.native-reachability-test` pins both.

- fix(drafts): `/draft apply` no longer deletes the trees the fork never copied.
  Since rift 0.0.10-8 a clone is gitignore-aware, so every ignored path (and every
  regenerable artifact directory such as `dist`, `build`, `coverage`, or a
  virtualenv) is missing from the draft by construction — `deleted-paths` read that
  absence as an agent deletion and `apply!` erased those files from the user's real
  repository. Trunk paths the backend cannot have cloned are now excluded from the
  deletion diff, and `resources/vis-docs/drafts.md` documents what a fork copies.

- fix(drafts): a draft's own generated output is no longer reported as an agent
  change. `changed-paths` now prunes whatever the CLONE's repository ignores and
  does not track: a gitignore-aware fork never copies those trees, so an ignored
  file inside a draft was built there — a regenerated native project alone pushed
  one session to 8,426 changed files, all of which `/draft apply` would have
  dumped into the real repository. Force-added ignored files stay tracked and
  still land.

- fix(drafts): `/draft apply` no longer deletes a committed `.yarn/cache`. The fork
  drops the `.yarn/<artifact>` pairs (`cache`, `unplugged`, `install-state.gz`,
  `build-state.yml`) that a Yarn zero-install repository commits, and the deletion
  guard matched single directory names only, so every file under a tracked
  `.yarn/cache` read as an agent deletion. The guard now mirrors the pair rule;
  `.yarn/patches` and `.yarn/releases` keep reporting real deletions.

### Documentation

- docs: document the per-root `draft` policy (`shared`, `copy-only`,
  `copy-and-apply`, `not-allowed`) in `resources/vis-docs/configuration.md`,
  `resources/vis-docs/sandbox.md`, and `resources/vis-docs/drafts.md`, replacing
  the stale claim that filesystem roots are not draft-specific.

## [v0.1.25] - 2026-08-03

### Changed
- release: v0.1.25
- Stop the audit-doc gate failing on its own date stamp
- perf(drafts): clone gitignore-aware trees by bumping rift to 0.0.10-8
- Cover nested edits and deletions in the draft apply! round-trip
- Let a provider timeout drain the queue instead of wedging it
- Pin the wrapped-401 cooldown with a test
- Spot the 401s a wrapped fleet failure hides on :attempts
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
- Revert "Retry a connect timeout that arrives wearing a status code"
- Bind skill as a Python verb beside its native tool
- Retire the restart op from REPL and resource lifecycle
- Stop the environment running code before the jail exists
- Format the pack-owned scan the way zprint wants it
- Retry a connect timeout that arrives wearing a status code
- Let the language pack own the parallel scan
- Stop a child's Ctrl-C from killing the gateway
- Name what killed the gateway
- Compile the lint target without ever running it
- Find the needle in a 20 MB file, and stop sweeping forever
- Never let the audit record downgrade a license it already vetted
- Retry every App Store Connect call, not only the two that failed
- One attachment control in the composer, not two
- Do not lose a release to one 401 from Apple
- Sign with the distribution identity the keychain actually has
- Sign the archive by hand, so CI stops minting certificates
- Name a profile for every bundle, or export automatically
- Export the archive even when only the app has a pinned profile
- audit: read imaging 0.1.7 license and size from the published artifact
- Move the imaging pin to 0.1.7 and lock the pptx shim against it
- Floor the eval watchdog above the LONGEST legal shell budget
- Offer "Start the session in" when the TUI opens a new session
- Test the companion share intake, and pin its dependencies exactly
- test(pptx): lock the imaging 0.1.6 chart part, picture crop and read-back
- Close the last descriptor doors: raw io.FileIO and host sqlite3 handles
- Say what `n` and `until` actually bound in shell wait
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
- fix(shell): require `until` for wait and never report a dead process as running
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.38
- fix(companion): one owner for transcript scroll anchoring
- fix(mcp,acp): string-keyed session-server results, like every MCP surface
- feat(shell): wait for a background job's condition, not a guessed clock
- fix(routing): keep both halves of a session model pin canonical
- fix(mcp): refuse an unauthorized OAuth server instead of blocking on a browser
- fix(config,cli): render a vis.yml provider id verbatim, never capitalized
- fix(companion): follow the gateway's session.model_updated broadcast
- fix(mcp,routing): total JSON-RPC encoding, headless MCP OAuth, pin fixes
- docs(site): tagline and sub say the mechanism, not the metaphor
- fix(companion): keep the reader's line by anchoring an element, not a height
- test(loop): pin the auth cooldown and the pinned-model router
- fix(providers): log out a key-only provider by clearing its key, keeping its entry
- chore(cli,structural): formatter reflow and reflection hints
- docs(readme): lead with Clojars and license, drop build and allowlist sections
- test(loop): mcp exposes one mcp__call verb, server-only lists schemas
- feat(companion,install): camera capture, install script as a release asset, documented Clojars
- fix(loop): request-bound provider credentials so a rotated OAuth token never strands a turn
- release: update release notes for v0.1.24

### Package changes

#### com.blockether/vis
- release: v0.1.25 (158cc2df0)
- Stop the audit-doc gate failing on its own date stamp (cccd0fefd)
- perf(drafts): clone gitignore-aware trees by bumping rift to 0.0.10-8 (3015f6ce5)
- Cover nested edits and deletions in the draft apply! round-trip (9990742ff)
- Let a provider timeout drain the queue instead of wedging it (346f09bed)
- Pin the wrapped-401 cooldown with a test (3ed40ced7)
- Spot the 401s a wrapped fleet failure hides on :attempts (086d0c166)
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
- Revert "Retry a connect timeout that arrives wearing a status code" (c349c707a)
- Bind skill as a Python verb beside its native tool (dfbb1e3b4)
- Retire the restart op from REPL and resource lifecycle (5921c471c)
- Stop the environment running code before the jail exists (a7c0648b0)
- Format the pack-owned scan the way zprint wants it (3ae016e97)
- Retry a connect timeout that arrives wearing a status code (712ee9f7a)
- Let the language pack own the parallel scan (dfc7ed12c)
- Stop a child's Ctrl-C from killing the gateway (56ae91192)
- Name what killed the gateway (0e6df3101)
- Find the needle in a 20 MB file, and stop sweeping forever (49302325b)
- Never let the audit record downgrade a license it already vetted (5995096f0)
- Retry every App Store Connect call, not only the two that failed (b097a6d1a)
- One attachment control in the composer, not two (a5a2c2c61)
- Do not lose a release to one 401 from Apple (64c6f5612)
- Sign with the distribution identity the keychain actually has (87cb8f2c4)
- Sign the archive by hand, so CI stops minting certificates (b5ad3c947)
- Name a profile for every bundle, or export automatically (87dae362f)
- Export the archive even when only the app has a pinned profile (4fc4a2136)
- audit: read imaging 0.1.7 license and size from the published artifact (34333c540)
- Move the imaging pin to 0.1.7 and lock the pptx shim against it (70435b2f8)
- Floor the eval watchdog above the LONGEST legal shell budget (7c4fb70c1)
- Test the companion share intake, and pin its dependencies exactly (0d88054b9)
- test(pptx): lock the imaging 0.1.6 chart part, picture crop and read-back (2922d5d00)
- Close the last descriptor doors: raw io.FileIO and host sqlite3 handles (bd08a4061)
- Say what `n` and `until` actually bound in shell wait (a0680bbb7)
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
- fix(shell): require `until` for wait and never report a dead process as running (4d01e8db2)
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.38 (7d7e0d625)
- fix(companion): one owner for transcript scroll anchoring (14c0e51fd)
- fix(mcp,acp): string-keyed session-server results, like every MCP surface (82aba80ba)
- feat(shell): wait for a background job's condition, not a guessed clock (574cdc5a7)
- fix(routing): keep both halves of a session model pin canonical (dcfede359)
- fix(mcp): refuse an unauthorized OAuth server instead of blocking on a browser (0ccba71f2)
- fix(config,cli): render a vis.yml provider id verbatim, never capitalized (a838d9ea1)
- fix(companion): follow the gateway's session.model_updated broadcast (f8177e8db)
- fix(mcp,routing): total JSON-RPC encoding, headless MCP OAuth, pin fixes (a10ba6158)
- docs(site): tagline and sub say the mechanism, not the metaphor (8cfd79373)
- fix(companion): keep the reader's line by anchoring an element, not a height (4c4c4b17e)
- test(loop): pin the auth cooldown and the pinned-model router (aa386b0b4)
- fix(providers): log out a key-only provider by clearing its key, keeping its entry (1a3a9c382)
- chore(cli,structural): formatter reflow and reflection hints (fa5a4f00d)
- docs(readme): lead with Clojars and license, drop build and allowlist sections (4718fc6b5)
- test(loop): mcp exposes one mcp__call verb, server-only lists schemas (54f90c31e)
- feat(companion,install): camera capture, install script as a release asset, documented Clojars (76183368d)
- fix(loop): request-bound provider credentials so a rotated OAuth token never strands a turn (9147f7379)
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

- perf(drafts): bump com.blockether/rift to 0.0.10-8 — draft creation clones a
  gitignore-aware tree, so generated output (e.g. a companion app's `ios/`
  build) is pruned instead of copy-on-write cloned file by file. Forking this
  repository drops from ~3.8s to ~0.7s; force-added paths and `.git` are still
  cloned, so a fresh draft's `git status` matches its trunk
- chore(deps): bump tree-sitter-language-pack to 1.12.3-blockether.38 — reference
  search runs as one native batch walk, no longer matches names written inside
  string literals or comments (rename included), and the shared native library
  cache is now safe for many concurrent vis sessions/processes

## [v0.1.24] - 2026-08-02

### Changed
- release: v0.1.24
- feat(mcp): manage MCP servers and their OAuth from the companion and the TUI
- fix(release): release notes survive a main that moved, and land v0.1.23's
- test(ls): pin the warm-index fast path and recursive listing order

### Package changes

#### com.blockether/vis
- release: v0.1.24 (a56ad6009)
- feat(mcp): manage MCP servers and their OAuth from the companion and the TUI (139cdc801)
- fix(release): release notes survive a main that moved, and land v0.1.23's (57693dc17)
- test(ls): pin the warm-index fast path and recursive listing order (ccbd03009)

#### com.blockether/vis-channel-tui
- feat(mcp): manage MCP servers and their OAuth from the companion and the TUI (139cdc801)
- test(ls): pin the warm-index fast path and recursive listing order (ccbd03009)

#### com.blockether/vis-persistance-sqlite
- test(ls): pin the warm-index fast path and recursive listing order (ccbd03009)



## [v0.1.24] - 2026-08-02

### Added

- mcp: the whole MCP server lifecycle is reachable outside a terminal. The
  gateway exposes kill, start and auth start/complete/poll/cancel/logout per
  server; the companion drives them from Settings (and surfaces them at connect
  time), and the TUI gets a matching MCP dialog. A server that needs a browser
  login is no longer a dead end you can only watch fail.

### Fixed

- bs4 shim: `BeautifulSoup(None, ...)` now raises
  `TypeError: object of type 'NoneType' has no len()` exactly like upstream bs4
  4.12 instead of quietly parsing as empty markup, plus soupsieve and
  tree-builder parity work cross-validated probe-by-probe against real
  beautifulsoup4 4.12.3 + soupsieve 2.5.
- release: the workflow's "Commit release notes" step rebases onto
  `origin/main` and retries, so a `main` that moved during the run no longer
  fails the release job and skips the mobile release with it.

- persistence: the shared SQLite pool is no longer torn down underneath live
  queries. The snapshot behind "was `~/.vis/vis.mdb/vis.db` replaced under this
  JVM?" compared the file's size and mtime, and SQLite rewrites `vis.db` in
  place on every WAL checkpoint — so ordinary write traffic made the store look
  replaced forever after. The gateway answered by closing its connection pool
  and opening a new one, over and over: a crashed 3h21m process had reached
  pool generation 351, leaked seven housekeeper threads, and died with SIGBUS
  inside `NativeDB.step`, taking every live session with it. The check now
  compares the filesystem `(dev, ino)` identity only, which moves exactly when
  a reopen is the right answer.

## [v0.1.23] - 2026-08-02

### Changed

- release: v0.1.23
- test: cover external-opener, notifications and serial-batch
- perf(ls): serve directory listings from the warm fff index
- fix(acp): answer `cancelled` when the cancel itself throws, and bind cancels to turn numbers
- fix(companion): resume a live turn from the row already in hand
- feat(drafts): a draft can start from your last commit, not your dirty tree
- fix(cli): no dead ends — launcher owns runtime/update, flag typos are refused
- fix(acp): refuse phantom resumes and walk tool arguments iteratively
- fix(launcher): dev names one checkout, never a silent substitute
- docs(changelog): record the coherent vis-agent runtime surface
- Advertise cat's directory listing (ls) and compress native tool prose
- chore: vis-agent runtime docs, ACP concurrency fixes, bs4 fidelity, companion polish
- feat(rewind): land the /rewind slash surface with context reporting
- Compress fs tool reference prose
- Ratchet the native tool prose budget to 1250
- Compress shell and grep reference prose
- Compress structural tool reference prose
- docs(runtime): document release-following default and dev mode precisely
- Compress the core system prompt and ratchet its budget
- feat(launcher): follow releases by default, opt in to dev mode
- chore: land in-flight foundation, gateway, TUI, and companion work
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

- drafts: `/draft clean <label>` (and the companion's "A new draft, without my
  uncommitted changes") forks the project and rewinds the copy to the committed
  `HEAD`, so a fresh draft starts from your last commit while modified tracked
  files, untracked files, and staged-but-uncommitted ones stay in the real
  project. The skipped paths are recorded, so applying the draft later never
  deletes work it never received; a repo without a commit is refused instead of
  silently forking a dirty tree.

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

- sandbox: a Python block's `open(path, "w").write(text)` reaches the disk. The
  sandbox runs on GraalPy, which does not refcount, so a handle dropped without
  `close()` was never finalized at the end of the statement — the bytes stayed
  in the buffer and the file was EMPTY, so the next tool (`git commit -F`) read
  nothing. Writable handles are now tracked weakly and flushed before every tool
  call and at the end of the block.
- git tool: `git add -- <paths>` stages again. The `--verbose` the tool appends
  so `add` reports what it staged landed AFTER the `--` separator, where git
  reads it as a pathspec (`fatal: pathspec '--verbose' did not match any
  files`); it is now inserted before the separator.
- launcher: `vis-agent update --native|--jvm|--dev` reached the update path
  again (the launch-flag loop used to swallow them), and a `[[ … ]] && cmd`
  tail no longer makes a successful `runtime use` exit 1.
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
- launcher: `--` ends the wrapper's own flag parsing too. `vis-agent -- --dev`
  used to switch runtime (and `-- --measure`/`-- --jfr` used to turn on
  profiling) while handing the app an empty prompt; those tokens are now prompt
  text. `runtime use <name>` combined with `--native|--jvm|--dev` is refused
  instead of silently dropping the flag.
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
- feat: ship viewport speedups and accumulated runtime work
- docs(companion): release notes for 0.1.19 (2854)
- feat(companion): expandable session stats and drafts grouped under their project
- feat(companion): add image viewer and smooth native viewport
- docs(companion): release notes for 0.1.18 (2851)
- perf(companion): keep the app shell off the compositor during keyboard/rotation
- docs(companion): release notes for 0.1.17 (2849)
- style(companion): full-bleed paste blocks in user messages
- perf(companion): drive shell geometry through CSS custom properties
- docs(companion): release notes for 0.1.16 (2846)
- feat(companion): show recently-active sessions in collapsed projects
- docs(companion): release notes for 0.1.15 (2844)
- perf(companion): isolate shell re-renders from keyboard and rotation frames
- feat(companion): collapsible projects with per-project paging and richer settings
- docs(companion): release notes for 0.1.15 (2841)
- fix(companion): match the composer strip type ladder and shrink the rule
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
- fix(editing): stop stamping --- before / +++ after on every diff
- fix(ci): restore the native builder args that last built green
- fix(ci): give the native builder a 22g heap on the swapfile-backed runner
- fix(ci): switch the preselected ParallelGC off before enabling G1
- fix(ci): overcommit the native builder heap with G1 instead of starving it
- docs(companion): release notes for 0.1.15 (2823)
- test(loop): measure the guest-interrupt CPU delta, not JVM-wide CPU
- fix(ci): keep the native-image heap inside runner RAM and give it a longer clock
- fix(ci): give the native-image builder enough heap, and let a dispatch rebuild a tag
- docs(audit): refresh dependency inventory
- docs(companion): TestFlight notes for 0.1.14 (2817)
- release: update version files for v0.1.14, bump to next dev version

### Package changes

#### com.blockether/vis
- chore(deps): svar 0.7.95, refresh the audit inventory (e9a1a6f84)
- ci: enforce the locked GraalVM pin across build workflows (0deb9e403)
- feat: ship viewport speedups and accumulated runtime work (66b0c31d8)
- docs(companion): release notes for 0.1.19 (2854) (b0f316183)
- feat(companion): expandable session stats and drafts grouped under their project (da7516494)
- feat(companion): add image viewer and smooth native viewport (91170014c)
- docs(companion): release notes for 0.1.18 (2851) (e43ed8bc6)
- perf(companion): keep the app shell off the compositor during keyboard/rotation (d5f4f08cf)
- docs(companion): release notes for 0.1.17 (2849) (821f28861)
- style(companion): full-bleed paste blocks in user messages (e1e9c7743)
- perf(companion): drive shell geometry through CSS custom properties (c34efdda8)
- docs(companion): release notes for 0.1.16 (2846) (2815efc72)
- feat(companion): show recently-active sessions in collapsed projects (68352e07b)
- docs(companion): release notes for 0.1.15 (2844) (5b8ddeaa4)
- perf(companion): isolate shell re-renders from keyboard and rotation frames (a70d92516)
- feat(companion): collapsible projects with per-project paging and richer settings (2d04f57a6)
- docs(companion): release notes for 0.1.15 (2841) (49276fdf0)
- fix(companion): match the composer strip type ladder and shrink the rule (d15a8b24b)
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
- fix(editing): stop stamping --- before / +++ after on every diff (f7870f43b)
- fix(ci): restore the native builder args that last built green (8edf48275)
- fix(ci): give the native builder a 22g heap on the swapfile-backed runner (fff80c57a)
- fix(ci): switch the preselected ParallelGC off before enabling G1 (e63b9282a)
- fix(ci): overcommit the native builder heap with G1 instead of starving it (8ea6b9d15)
- docs(companion): release notes for 0.1.15 (2823) (c85f7da50)
- test(loop): measure the guest-interrupt CPU delta, not JVM-wide CPU (2626ea8d7)
- fix(ci): keep the native-image heap inside runner RAM and give it a longer clock (715cd41e2)
- fix(ci): give the native-image builder enough heap, and let a dispatch rebuild a tag (eadda4851)
- docs(audit): refresh dependency inventory (8e2a282f3)
- docs(companion): TestFlight notes for 0.1.14 (2817) (0738c1822)
- release: update version files for v0.1.14, bump to next dev version (34f89e45b)

#### com.blockether/vis-channel-tui
- feat: ship viewport speedups and accumulated runtime work (66b0c31d8)
- fix(tui): handle whitespace split across styled runs (cdf9bd256)

#### com.blockether/vis-persistance-sqlite
- feat: ship viewport speedups and accumulated runtime work (66b0c31d8)

## [v0.1.14] - 2026-07-30

### Changed
- fix(editing): newline-faithful structural edits and comment docs across 28 languages
- Record the 0.1.14 (2815) release notes
- Cover turn attachments with tests and note 0.1.14 in the changelog
- Bump tree-sitter-language-pack to 1.12.3-blockether.32
- Serve a turn's inline attachments and hide the footer mid-turn
- Bump tree-sitter-language-pack to 1.12.3-blockether.31
- Name every working directory `cwd` across the tool surface
- Cache the live turn bubble so re-entry paints it instantly
- docs(companion): release notes for 0.1.14 (2808)
- Let the companion app change the reasoning mode
- Adopt already-running turns in the companion session screen
- Add PRIVACY.md for the companion app (Play store policy URL)
- Guarantee turn terminals and bound Python GC
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
- refactor(prompt,fold): say what to do, and advertise only the 5 newest ntr entries
- fix(provider-error): blame the gateway, not a Vis schema, for injected tool fields
- feat(titling): the LLM title always runs after the turn, on its own route
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
- fix(editing): newline-faithful structural edits and comment docs across 28 languages (edcac200a)
- Record the 0.1.14 (2815) release notes (e50c94af9)
- Cover turn attachments with tests and note 0.1.14 in the changelog (060fe81df)
- Bump tree-sitter-language-pack to 1.12.3-blockether.32 (e3b729f13)
- Serve a turn's inline attachments and hide the footer mid-turn (29a05339a)
- Bump tree-sitter-language-pack to 1.12.3-blockether.31 (f6881c9dd)
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache the live turn bubble so re-entry paints it instantly (ece994fa2)
- docs(companion): release notes for 0.1.14 (2808) (96563d153)
- Let the companion app change the reasoning mode (f567648ee)
- Adopt already-running turns in the companion session screen (2dd3ad3ba)
- Add PRIVACY.md for the companion app (Play store policy URL) (4ea71f8a3)
- Guarantee turn terminals and bound Python GC (5bb959dd7)
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
- refactor(prompt,fold): say what to do, and advertise only the 5 newest ntr entries (df93fefef)
- fix(provider-error): blame the gateway, not a Vis schema, for injected tool fields (7abfd121a)
- feat(titling): the LLM title always runs after the turn, on its own route (a696d8d2d)
- test(loop): the deferred title upgrade is after-turn-auto-title! (#71) (ec1c01f38)
- feat(titling): configurable session titling, deferred past the foreground turn (#71) (eb0b6a793)
- test(loop): widen the observation-batch concurrency margin for loaded runners (3f1fe723c)
- chore(audit): regenerate audit/README.md for svar 0.7.88 (dbe920b6b)
- fix(cli): describe vis as a coding agent, not a "Recursive Language Model" (0e737768d)
- feat(config): per-provider `is_stateless` for gateways that reject replayed item ids (41f87ea34)
- release: update version files for v0.1.13, bump to next dev version (2a35e648b)
- chore(audit): regenerate the dependency inventory (ruff 0.3.2, svar 0.7.86) (af62949a3)

#### com.blockether/vis-channel-tui
- Cache the live turn bubble so re-entry paints it instantly (ece994fa2)
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
- Cache the live turn bubble so re-entry paints it instantly (ece994fa2)
- Improve extension configuration and tools (31a974b4b)

#### com.blockether/vis-foundation-voice
- Cache the live turn bubble so re-entry paints it instantly (ece994fa2)

#### com.blockether/vis-language-clojure
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache the live turn bubble so re-entry paints it instantly (ece994fa2)
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)

#### com.blockether/vis-language-python
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache the live turn bubble so re-entry paints it instantly (ece994fa2)
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)

#### com.blockether/vis-language-typescript-bun
- Name every working directory `cwd` across the tool surface (4df25f19e)
- Cache the live turn bubble so re-entry paints it instantly (ece994fa2)

#### com.blockether/vis-persistance-sqlite
- Unify tool input carriers and refresh companion diff view (7e3b8a2c2)
- chore: update GraalVM and extension runtime (fe6d2949a)
- Limit NTR browsing to latest turn (ce8f20afa)

### Added

- Bridge exact-candidate options in `br/check` and `br/run-evidence`.
- A shared fail-closed `:git/commit` operation for the model-facing Git tool
  and TUI Magit. The generic Git adapter resolves Git-global repository
  options, rejects index-changing commit forms, rechecks the staged tree, and
  verifies the resulting commit tree; Bridge contributes only the
  lifecycle-owned approval hook.

### Changed

- `vis-foundation-bridge` now targets the Bridge 0.2.2
  candidate-verification API.
- tree-sitter-language-pack 1.12.3-blockether.34: structural editing keeps a
  file's final newline and CRLF endings across every splice, replaces a node by
  its non-whitespace core, and understands comment docs — so `add_doc` /
  `replace_doc` work for the 26 languages whose docs are comments, not strings.

### Fixed

- `struct_patch` moves no longer drop the file's trailing newline (or `\r`):
  the structural editor now splits lines without collapsing the final empty
  line, and only collapses a seam when there is one.

## [v0.1.13] - 2026-07-29

### Changed
- chore(deps): svar 0.7.86 -- every quota/credit/budget wall is a hard error
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
- feat: web-search toggle, typed extension schemas, honest python CLI exits
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
- companion: honest live status, one-motion keyboard, resume at the end
- companion: one-motion iOS keyboard; TUI limits, shims, editing fixes
- labelled ntr recovery, image optimization at ingest, companion back/paste/perf
- release: update version files for v0.1.12, bump to next dev version

### Package changes

#### com.blockether/vis
- chore(deps): svar 0.7.86 -- every quota/credit/budget wall is a hard error (02252578e)
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
- feat: web-search toggle, typed extension schemas, honest python CLI exits (a9ee2d552)
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
- companion: honest live status, one-motion keyboard, resume at the end (445a3b2d4)
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
- feat: web-search toggle, typed extension schemas, honest python CLI exits (a9ee2d552)

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
- gateway state test: assert the budget-busting turn is kept, not deferred
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
- gateway state test: assert the budget-busting turn is kept, not deferred (d361085f1)
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
- fix(native): ship linux x64 + arm64 from CI, drop the impossible macOS job
- release: update version files for v0.1.9, bump to next dev version

### Package changes

#### com.blockether/vis
- fix(native): ship linux x64 + arm64 from CI, drop the impossible macOS job (005b9b806)
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
- perf(search): fff owns every ignore decision (overlay), no Clojure walk left
- fix(queue): images in queued turns render as chips, not raw paths
- fix(companion): smaller composer text and narrower +/mic buttons on phone
- fix(companion): model badge is a quiet centered caption; composer no longer hugs the screen edge
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
- fix(channel-tui): restore result-row copy alignment for baked output indent
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
- Better error signal for the compiler exceptions
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
- fix(clojure): evict wedged nrepl connection on eval timeout
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
- perf(search): fff owns every ignore decision (overlay), no Clojure walk left (760197012)
- fix(queue): images in queued turns render as chips, not raw paths (5c5e4c6e5)
- fix(companion): smaller composer text and narrower +/mic buttons on phone (dc16e6641)
- fix(companion): model badge is a quiet centered caption; composer no longer hugs the screen edge (75869fb6f)
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
- Better error signal for the compiler exceptions (4b96c7cd5)
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
- fix(channel-tui): restore result-row copy alignment for baked output indent (88a58bf47)
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
- Better error signal for the compiler exceptions (4b96c7cd5)
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
- Better error signal for the compiler exceptions (4b96c7cd5)
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
- Better error signal for the compiler exceptions (4b96c7cd5)
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
- fix(clojure): evict wedged nrepl connection on eval timeout (47fc63e2c)
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
- fix(theme): hint rel-luminance ^double to kill Math/abs reflection; baseline 1977->395
- feat(loop): surface the saved-tokens note on fold breadcrumb cards
- docs(language-surface): teach manual reload after editing source (no auto-reload)
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
- feat(compaction): breadcrumb-canonical fold gist, utilization keeps only the live budget
- refactor(sqlite): squash migrations V1..V10 into a single consolidated V1__schema.sql
- chore: sync workspace changes across core, extensions, and docs
- feat(compaction): merge fold ledger into utilization as one-line readout
- feat(python-extensions): author LLM providers from Python
- feat(env-python): guard against GraalVM/Truffle version mismatch on --jvm
- chore: sync workspace changes across core, extensions, and docs
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll
- fix(verify,reflection): make graal gate actually run + zero reflection warnings
- fix(loop): treat post-refresh 401 as propagation lag, not dead credential
- fix(loop): latch dead OAuth credentials gateway-wide to stop 401 refresh storms
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
- perf(gateway/bus): adaptive tailer poll to kill idle CPU burn
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
- fix(theme): hint rel-luminance ^double to kill Math/abs reflection; baseline 1977->395 (a1433367)
- feat(loop): surface the saved-tokens note on fold breadcrumb cards (5b21d983)
- docs(language-surface): teach manual reload after editing source (no auto-reload) (321c48f2)
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
- feat(compaction): breadcrumb-canonical fold gist, utilization keeps only the live budget (eec7c974)
- refactor(sqlite): squash migrations V1..V10 into a single consolidated V1__schema.sql (c0898015)
- chore: sync workspace changes across core, extensions, and docs (9a68ff54)
- feat(compaction): merge fold ledger into utilization as one-line readout (c35868e8)
- feat(python-extensions): author LLM providers from Python (e370650e)
- feat(env-python): guard against GraalVM/Truffle version mismatch on --jvm (9aaf179c)
- chore: sync workspace changes across core, extensions, and docs (e410b355)
- perf(channel-tui,git): route footer git through gateway, drop dead client-side walks; sync cache TTL to poll (3e161cb8)
- fix(verify,reflection): make graal gate actually run + zero reflection warnings (001c99fe)
- fix(loop): treat post-refresh 401 as propagation lag, not dead credential (473c0f9f)
- fix(loop): latch dead OAuth credentials gateway-wide to stop 401 refresh storms (f7a251b8)
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
- perf(gateway/bus): adaptive tailer poll to kill idle CPU burn (17357e96)
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
- fix(loop): treat post-refresh 401 as propagation lag, not dead credential (473c0f9f)
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

- fix(openai-codex): give new Codex models svar's pinned catalog doesn't know
  yet (e.g. `gpt-5.6-terra`) their real context window via a
  `:provider/enrich-models-fn` hook, instead of svar's 8192 default that
  rejected normal turns with "Context overflow … has 8192 context". Only fills
  the gap — models svar knows keep their catalog window and explicit config
  `:context` still wins. `gpt-5.6-terra` also added to the provider's
  default-models.

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
- style(tui,web): bolden dialog titles and warm modal chrome
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
- style(tui,web): bolden dialog titles and warm modal chrome (63ea851b)
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
- style(tui,web): bolden dialog titles and warm modal chrome (63ea851b)
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
- feat(attachments): storage-offload rail — registry + pure decision + resolver
- feat(resources): view background logs in TUI + web
- fix(channel-web): space + chip styling for result summaries
- feat(attachments): session_fold collapses vision replay too
- fix(channel-web): harden renderProse against UI-spoofing HTML injection
- fix(web): strip vis-image fence in DB-restored history; drop comment profanity
- fix(tui): collapse same-file edit band to full path shown once
- feat(attachments): V4 unifies both rails into one session_attachment table
- refactor(attachments): self-describing handle ids kill the read-back fallback
- feat(attachments): unify read-back across tool + user attachments
- test(loop): update synth oracle for symbol_rename as positional native tool
- feat(attachments): introspection read-back API + misc workspace changes
- feat(editing): promote symbol_rename back to a native tool
- test(attachments): lock down gather->virtual-thread sink conveyance
- clj-ext: format only on :write, not patch/struct_patch
- feat(attachments): $VIS_OUTBOX filesystem tap + rename :images -> :attachments
- feat(attachments): vis_attach — generic producer rail for any artifact
- feat(attachments): V3 brings session_turn_attachment to V2 payload parity
- feat(attachments): wire iteration-attachment rail to both ends
- refactor(mpl): sink produced images at source, drop stdout-fence parsing
- feat(loop): replay generated figures to vision models on the wire
- feat(loop): capture matplotlib figure bytes into iteration attachments
- feat(persist): V2 session_iteration_attachment table + store/read
- tui(navigator): drop empty Modified column, rename Directory -> Dir

### Added
- GitHub Copilot **Enterprise** provider (`:github-copilot-enterprise`). The
  provider extension already shipped the enterprise base-url, provider id,
  label, and account type, but only registered `:individual` + `:business`,
  so Copilot Enterprise users could not select Claude Opus 4.8 / Sonnet 4.6 /
  Haiku 4.5 at all. Enterprise now registers alongside the other tiers and
  inherits the same curated catalog: dotted models.dev ids
  (`claude-opus-4.8`, `claude-sonnet-4.6`, `claude-haiku-4.5`) over the native
  Anthropic `/v1/messages` wire (never `/chat/completions`).
- Extension system with global registry, topo-sort, hot-reload
- `:ext/nudge-fn` for per-iteration system nudges from extensions
- `:ext/requires` for extension dependency declaration
- `:ext/version`, `:ext/author`, `:ext/license` metadata
- `register-global!`, `load-extension!`, `reload-extension!`
- `extensions/common/vis-foundation` package (read, list, grep, patch)
- mdBook documentation at https://blockether.github.io/vis/
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
- Built-in repetition `[system_nudge]` ("You repeated the same expression ...").
  `<journal>` + the dedup cache (`:cached? true`) already give the model
  enough signal to change strategy; the nudge was noise. Drops
  `repetition-warning`, `REPETITION_THRESHOLD`, the `call-counts-atom`
  plumbing, and the `:call-counts-atom` arg to `prompt/build-iteration-context`.

### Fixed
- GitHub Copilot Claude requests returning `404 page not found`. The token
  exchange's authoritative `endpoints.api` (and the account fallback hosts)
  are bare roots with no `/v1`, so `provider-token-base-url` handed svar a
  versionless base and Claude hit `{host}/messages` instead of
  `{host}/v1/messages`. The token's LLM base is now suffixed with `/v1` at
  exchange time (idempotent `ensure-api-version`) and reused from cache, while
  the model-policy call still targets the root host. Affects all Copilot tiers
  (individual/business/enterprise), since every account's token endpoint
  resolves to the same versionless host.
- `github-copilot-provider-id?` omitted `:github-copilot-enterprise`, so
  enterprise models were filtered out of the visible catalog mapping.

[Unreleased]: https://github.com/Blockether/vis/compare/v0.1.37...HEAD
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
