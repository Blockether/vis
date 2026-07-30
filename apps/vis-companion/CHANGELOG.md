# Vis Companion — release notes

What each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.

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
