# Vis Companion — release notes

What each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.

## 1.0.1 (2721) — 2026-07-28
<!-- commit: f11835f3f8432df8ee7e51312bedb289ea28e9b1 -->

- Companion: request/transcription deadlines, durable voice outbox, rotation + typography fixes
- Allow host-root descendants outside jail

## 1.0.1 (2719) — 2026-07-28
<!-- commit: d8bd17eb26201de9bb72849b8e04e13621f0f2bc -->

- Companion: coalesced tool-card grids, justified prose, correct live ticker
- Release: notes for 1.0.1 (2717)

## 1.0.1 (2717) — 2026-07-28
<!-- commit: 445a3b2d4fcd557f87d831b3bcd3a61836114159 -->

- The spinner and timer now stop when the work actually stops — a finished turn no longer looks like it is still running
- While the keyboard is up you can see what Vis is doing, and for how long, right above the message box
- Tapping a pasted attachment no longer makes the keyboard drop and jump back
- The transcript no longer jumps around while an answer is streaming in
- Coming back to a session after a while away takes you to the latest message instead of where you left off
- Code blocks that contain code fences render correctly instead of spilling out as plain text
- Dictation keeps recording when you switch apps or lock the screen
- Loading sessions and settings now shows a visible "Loading…" placeholder instead of a blank screen

## 1.0.1 (2715) — 2026-07-28
<!-- commit: f6a109f89d8ef4cd0fa884034d259e0974882d6a -->

- Labelled ntr recovery, image optimization at ingest, companion back/paste/perf

## 1.0.1 (2713) — 2026-07-28
<!-- commit: 1c2478678779fb1647676e015e388b2554d1247a -->

- Provider `compatibility` key, svar model limits, vis.yml model order
- Turn failures: fail once, fail legibly; session_fold kwargs; repo-wide format
- Session_fold: accept keyword arguments from the Python sandbox

## 1.0.1 (2710) — 2026-07-28
<!-- commit: 5d270615be75c17b8651ffbf567d9b1bba2b63be -->

- Config: name the failing fields and stop dumping a stack trace

## 1.0.1 (2709) — 2026-07-28
<!-- commit: d361085f12f1a524b875a1686ddedc8b7c4002b3 -->

- Gateway state test: assert the budget-busting turn is kept, not deferred
- Release notes: keep one preamble in CHANGELOG.md, human-readable 2707 entry

## 1.0.1 (2707) — 2026-07-28
<!-- commit: 16c287e2e477ffef632c2843e499f151e6d45b95 -->

- Images you upload now stay in the conversation after you leave and re-open a session
- Rotating the phone no longer makes the layout float and resize — it fades back in once settled
- Tapping the message box keeps you at the bottom of the conversation
- TestFlight builds now ship these notes automatically

## 1.0.1 (2706) — 2026-07-28
<!-- commit: 9c94b083a23e99c5032d72acae95d96d76b5e0ab -->

- Shell logs: return the tail once as plain strings
- Note lint_code also reports reflection + boxed-math
- Tailscale address preference, persisted draft messages, rotation-aware viewport, shell send keystroke labels, nippy shim merge
- Resume/keyboard/scroll fixes, unread marks, swipe actions
- Preserve terminal cancellation notice
- Unblock nested workspace searches
- Discover projects across workspace sessions
- Remove redundant next surface
- Native QR scanning on iOS, wider pairing bind, companion fixes
- Self-configuring pairing, loopback mirror, and answer-bearing push
- Give the arm64 builder the heap it measurably needs
- Protocol version handshake + compatibility verdict
- ${VAR} config references, provider-level env gaps, drop "gateway" from user-facing copy
