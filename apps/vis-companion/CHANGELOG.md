# Vis Companion — release notes

What each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.

## 0.1.21 (2861) — 2026-08-01
<!-- commit: 52953cd362b54144b18dde9388055588cc2abe7a -->

- Unify releases and harden live companion behavior
- Enforce the locked GraalVM pin across build workflows

## 0.1.20 (2856) — 2026-08-01
<!-- commit: 66b0c31d8faee8c348f30a412c520b826579c506 -->

- Ship viewport speedups and accumulated runtime work
- Release notes for 0.1.19 (2854)

## 0.1.19 (2854) — 2026-08-01
<!-- commit: da7516494469a10de4d4becab81f330722c792a1 -->

- Expandable session stats and drafts grouped under their project

## 0.1.19 (2853) — 2026-08-01
<!-- commit: 91170014c043b6acf3a1821f6a4a91924bc5f030 -->

- Add image viewer and smooth native viewport
- Release notes for 0.1.18 (2851)

## 0.1.18 (2851) — 2026-08-01
<!-- commit: d5f4f08cf0cb184cdc2df14da7642baa8ca4b896 -->

- Faster keyboard show/hide and orientation change: the app shell no longer sits on its own composited layer, so raising the keyboard and rotating re-run layout only instead of re-rasterizing the whole screen every frame (fixes a regression from 0.1.17)

## 0.1.17 (2849) — 2026-08-01
<!-- commit: e1e9c7743a054f472fd4d6e35e7eabe7d4dc0cca -->

- Smoother keyboard show/hide and rotation: the app shell now tracks the visual viewport through CSS custom properties instead of React state, so the screen no longer re-renders on every keyboard/rotation frame
- Pasted blocks in your messages are now full-width with wider spacing

## 0.1.16 (2846) — 2026-08-01
<!-- commit: 68352e07b671b3aad7b3b1e53087c8699423ab03 -->

- Show recently-active sessions in collapsed projects
- Release notes for 0.1.15 (2844)

## 0.1.15 (2844) — 2026-07-31
<!-- commit: a70d925169253f6c0758254f726a880ff0685297 -->

- Isolate shell re-renders from keyboard and rotation frames
- Collapsible projects with per-project paging and richer settings
- Release notes for 0.1.15 (2841)

## 0.1.15 (2841) — 2026-07-31
<!-- commit: d15a8b24b9b1cf6a97b25f0e2a3cc03954df4028 -->

- Match the composer strip type ladder and shrink the rule
- Release notes for 0.1.15 (2839)

## 0.1.15 (2839) — 2026-07-31
<!-- commit: 25fa99d8ee45873af76c2d03b0cdce1a46520331 -->

- Glyph-free composer strip and animated reasoning swap
- Release notes for 0.1.15 (2837)

## 0.1.15 (2837) — 2026-07-31
<!-- commit: 2add31771ef95afb1f471452210627b31cb7f95d -->

- Preserve pinned provider selection
- Revert(companion): restore the composer strip glyphs
- Revert(companion): bring the glyphs back
- Handle whitespace split across styled runs
- Release notes for 0.1.15 (2832)

## 0.1.15 (2832) — 2026-07-31
<!-- commit: 012b26d82f5a9fd6a817d70fa924f01251887f19 -->

- Glyph-free thinking band and model manager
- Release notes for 0.1.15 (2830)

## 0.1.15 (2830) — 2026-07-31
<!-- commit: dbab3492cd03e2c55a1be61d42a37a88eef3c069 -->

- Footer reasoning chip, landscape safe areas
- Stop stamping --- before / +++ after on every diff
- Restore the native builder args that last built green
- Give the native builder a 22g heap on the swapfile-backed runner
- Switch the preselected ParallelGC off before enabling G1
- Overcommit the native builder heap with G1 instead of starving it
- Release notes for 0.1.15 (2823)

## 0.1.15 (2823) — 2026-07-31
<!-- commit: 2626ea8d7835f485bea60d9a79e0babc2e1c0501 -->

- Measure the guest-interrupt CPU delta, not JVM-wide CPU
- Keep the native-image heap inside runner RAM and give it a longer clock
- Give the native-image builder enough heap, and let a dispatch rebuild a tag
- Refresh dependency inventory
- TestFlight notes for 0.1.14 (2817)

## 0.1.14 (2817) — 2026-07-30
<!-- commit: edcac200a917a05e9f724ecc8a3706b122d60a37 -->

- Newline-faithful structural edits and comment docs across 28 languages
- Record the 0.1.14 (2815) release notes

## 0.1.14 (2815) — 2026-07-30
<!-- commit: 060fe81dfe16d410e4ef6efd2b03cd7d8bc70572 -->

- Cover turn attachments with tests and note 0.1.14 in the changelog

## 0.1.14 (2814) — 2026-07-30
<!-- commit: e3b729f13299640d797fef528bf655c7fc1f6f56 -->

- Serve a turn's inline attachments and hide the footer mid-turn
- Name every working directory `cwd` across the tool surface
- Cache the live turn bubble so re-entry paints it instantly
- Release notes for 0.1.14 (2808)

## 0.1.14 (2808) — 2026-07-30
<!-- commit: f567648ee4a861d6110c4475d7b75e95da3482da -->

- Let the companion app change the reasoning mode
- Adopt already-running turns in the companion session screen
- Add PRIVACY.md for the companion app (Play store policy URL)

## 0.1.14 (2805) — 2026-07-30
<!-- commit: 5bb959dd751a3ade42121036f3221ca427647e4a -->

- Guarantee turn terminals and bound Python GC

## 0.1.14 (2804) — 2026-07-30
<!-- commit: 88bbea7eb3eee254690946ef364d5f0a31b67e16 -->

- Allow block-local shadowing of bound tool names in vis Python
- Release notes for 0.1.14 (2802)

## 0.1.14 (2802) — 2026-07-30
<!-- commit: 7e3b8a2c2788faca62f436ad6fda377531a79824 -->

- Unify tool input carriers and refresh companion diff view
- Fix gateway, Python, Git, and TUI regressions (#61, #73, #74, #75)

## 0.1.14 (2800) — 2026-07-30
<!-- commit: c9b84ab7ed0ca15062d65d199aa3d951eb2b6886 -->

- Fix failed turn error cards after watchdog recovery
- Guard orphan retirement against registered gateways
- Retire orphaned loopback gateways before restart
- Route extension subprocess APIs through jailed shell
- Improve dotenv environment handling
- Activate Git tool for nested repositories
- Gate commits through verification hooks
- Prefer JSON in Bridge extension docs

## 0.1.14 (2792) — 2026-07-30
<!-- commit: dfece59420181d2b9112e8a56514468a25a2dbc6 -->

- Enforce GraalVM pin consistency
- Update GraalVM and extension runtime
- Smooth terminal result scrolling
- Improve extension configuration and tools
- Refresh shell bindings after settings changes
- Respect disabled shell toggle in sub-agents

## 0.1.14 (2786) — 2026-07-30
<!-- commit: 66da8bf722c3080cec05a5fa84dcbcbff60e833f -->

- Restore transcript layout stabilization
- Limit NTR browsing to latest turn

## 0.1.14 (2784) — 2026-07-30
<!-- commit: bf89880867cd250b2947ade75598de10288edad6 -->

- Simplify companion reconnect and transcript behavior
- Improve compaction guidance and retry diagnostics
- Install the pinned GraalVM CE automatically when it is missing

## 0.1.14 (2781) — 2026-07-29
<!-- commit: 636ea5af07bb6fcea247870d9771f7d56efee6a7 -->

- Stabilize transcript rotation

## 0.1.14 (2780) — 2026-07-29
<!-- commit: df93fefef6194b1309ca45cfc2e4370fde1fca65 -->

- Say what to do, and advertise only the 5 newest ntr entries
- Blame the gateway, not a Vis schema, for injected tool fields
- The LLM title always runs after the turn, on its own route
- The deferred title upgrade is after-turn-auto-title! (#71)
- Configurable session titling, deferred past the foreground turn (#71)
- Widen the observation-batch concurrency margin for loaded runners
- De-flake the live-progress layout budget on shared runners
- Regenerate audit/README.md for svar 0.7.88
- Describe vis as a coding agent, not a "Recursive Language Model"
- Per-provider `is_stateless` for gateways that reject replayed item ids
- Regenerate the dependency inventory (ruff 0.3.2, svar 0.7.86)
- Collapse recorded non-image attachments into one disclosure row
- Read packaging metadata with Python's own parsers, add `python.source_paths`
