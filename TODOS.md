# Memory retention fixes

Keep durable history on disk. Retain only bounded live working state and transient
responses in memory; do not restart or replace the running gateway during this work.
The shell log fix (`25377756d`) is already complete.

## Ordered work

1. [x] **Live-view log windows.** Remove backing-vector retention for lines and
   tones. Keep complete history in the existing disk journal and verify reads after
   window eviction, clear, and completion.
2. [x] **Session history and folds.** Select relevant history before loading payloads
   or hydrating attachments. Release completed/folded iteration payloads from active
   loop state without losing replay signatures, trace results, or persisted history.
3. [x] **Finished gateway turns.** Release requests, attachments, messages, options,
   cancellation tokens, and other execution-only state on every terminal path;
   preserve required status, queue, retry, and history behavior.
4. [x] **Gateway event replay.** Read historical event payloads from disk rather than
   retaining per-session rings. Preserve ordered replay, live subscriptions, cursor
   gaps, journal rotation, activity snapshots, and restart behavior.
5. [x] **Idle FFF indexes.** Evict unused indexes periodically without a new search;
   never close a borrowed index and clean up the reaper with its owner.
6. [x] **Full-text caches.** Bound formatter cache by bytes as well as entries, and
   stop retaining full provider/model requests after their useful lifetime. Preserve
   exact fresh cross-turn cache checkpoints and cache telemetry.

## Verification and delivery

For each item: reproduce in existing tests, implement, run affected tests and
formatting/lint (including Clojure reflection checks), and review the scoped diff.
Then run integration checks across changed owners, preserve unrelated checkout work,
commit only this task, and push to `main`. Record results below as work completes.

## Progress

- Live views complete: 248 affected tests passed after structural and disk-read
  regressions failed on the old code. Formatting and clj-kondo clean; the one
  existing reflection warning in unchanged `input-raise!` also reproduces on HEAD.
- Session history/folds complete: select metadata before payloads, hydrate only
  chosen images, and keep intermediate trace entries in a turn-local disk journal.
  The SQLite suite passed 170 tests, with one additional selective-turn reader test.
- Finished turns and event replay complete: disk-backed projections and bounded
  descriptors passed 591 gateway tests, including cancellation, failure, forgetting,
  cursor gaps and callback-lock races; 15 retention/replay tests passed after the
  final native interop correction.
- Idle FFF indexes complete: 270 index/editor tests passed; four lifecycle tests
  passed again after making their thread-exit assertions race-safe.
- Full-text caches complete: formatter cache has an 8 MiB text budget and all 19
  tests pass. Live prompt-cache state uses bounded fingerprints; exact accepted
  checkpoints remain on disk. The final focused loop checks passed 53 tests, plus
  five cache-status tests after formatting the last fixture.
- The combined loop suite passed 554 of 558 cases. The same four pre-existing
  Python/autocomplain failures reproduced before the memory changes; no new failures.
- Formatting and scoped lint checks passed with only previously verified baseline
  warnings. Native testing exposed a reflective `Files/write` overload; an explicit
  array type now compiles to a direct invocation. A clean reflection-enabled reload
  of all changed production namespaces passed.
- Rebuilt with the pinned GraalVM CE. Three existing native cases passed on that
  image (Python/format/search and durable Activity history), and the new owned-gateway
  regression passed, verifying real archive files, terminal reads and ordered SSE replay.
- The running gateway was not restarted or replaced. These checks verify the new
  code and image, not a reduction in the old process's live RSS.
