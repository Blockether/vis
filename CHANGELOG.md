# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

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

[Unreleased]: https://github.com/Blockether/vis/compare/v0.1.4...HEAD
[v0.1.1]: https://github.com/Blockether/vis/releases/tag/v0.1.1
[v0.1.2]: https://github.com/Blockether/vis/releases/tag/v0.1.2
[v0.1.3]: https://github.com/Blockether/vis/releases/tag/v0.1.3
[v0.1.4]: https://github.com/Blockether/vis/releases/tag/v0.1.4
