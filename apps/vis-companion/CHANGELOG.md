# Vis Companion — release notes

What each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.

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
