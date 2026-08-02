# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]

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

[Unreleased]: https://github.com/Blockether/vis/compare/v0.1.22...HEAD
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
