# Vis Companion — release notes

What each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.

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
