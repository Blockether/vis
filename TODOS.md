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

# Gateway indexing and retention experiments

## FFF: reproduced and corrected

- [x] Verify sharing across workers. Two distinct confined Python worker processes
  called into one host pool and reused the identical native index: one creation.
  Canonical aliases reused it; a different root or ignore policy correctly did not.
- [x] Keep borrowed indexes addressable. The old capacity sweep evicted a borrowed
  root and a second borrower constructed another index for that root. A regression
  failed before the fix. Acquisition and eviction now share a short pool lock;
  native work stays outside it. Active entries can temporarily exceed six slots,
  then release trims idle entries back to the existing budget.
- [x] Scope write invalidation. One write previously caused both of two unrelated
  roots to rescan. Notifications now identify the changed path; overlapping roots
  resync, unrelated drafts do not. A failed rescan no longer advances the epoch or
  runs a potentially incomplete search; writes arriving during a scan remain pending.
- [x] Instrument creation, reuse, acquire/release, active-user count, eviction reason,
  initial/resync duration and scan-permit wait. `workspace.fff-index/pool-stats`
  returns bounded totals and current entries without building indexes. Structured
  `::lifecycle` logs carry details; high-frequency reuse/lease events are debug-level.
- [x] Reproduce capacity thrashing with real native indexes over isolated draft-like
  fixture roots, not user worktrees. Three passes, 32 files per root, 2 ms between
  switches: six roots produced 6 builds, 12 reuses and 0 evictions; seven roots
  produced 21 builds, 0 reuses and 15 capacity evictions. Measured elapsed times were
  127/314 ms and summed initial-scan times 72/245 ms, with no write rescans or permit
  waits. These are controlled small-fixture measurements, not a gateway CPU benchmark.
- [ ] Capture a representative gateway workload using these events. Measure distinct
  root/policy demand inside the ten-minute window, scan frequency, latency, native
  CPU and resident memory before changing capacity. The experiment proves how seven
  cyclic roots thrash six slots, not that the historical gateway followed that pattern.
  Do not merge divergent worktree contents or increase the pool blindly.

## GC and class loaders: no tuning justified by the measured workloads

- [x] Compare current 10/25 heap-free ratios with defaults in clean Java 25.0.3 JVMs,
  retaining the other flags and 5 GiB ceiling. Each ran 10 warmups, 500 reflection
  suite repetitions (10,500 passing cases), 65 seconds idle, then explicit GC.
  Active process CPU was 12,130/12,108 ms; concurrent collection counts were 6/4.
  Idle process CPU was 67/43 ms with no additional collection counts. After GC,
  both used about 47.9 MB heap; committed heap was 109.1/251.7 MB. This single paired
  workload did not reproduce near-secondly marking. Collection-time counters are
  not GC-thread CPU, and committed heap is not total process memory.
- [ ] Capture actual GC causes and per-thread CPU during representative turn activity;
  repeat paired latency/RSS measurements before changing `deps.edn` ratios.
- [x] Separate loader retention from ordinary warm linting. Repeated full reflection
  suites retained about 6,821 live DynamicClassLoaders after explicit GC, versus
  814 initially. The suite includes deliberate `load-string`/dependency compilation.
  Three hundred warm compile-warning repetitions changed live loaders only 706→707.
  A diagnostic JVM with immediate soft-reference aging reduced live loaders to 692
  after GC; Clojure's class cache uses soft references. This is not proof of an
  unreclaimable gateway leak, nor a recommendation to change soft-reference policy.
- [x] Verify unloading under ordinary memory pressure, without a soft-reference-policy
  override. With a 128 MiB ceiling, the same 10,500 cases passed; bounded retained
  and transient allocations reduced live loaders from 6,821 to 1,735 and unloaded
  4,616 classes. Metaspace fell from 75.7 to 60.9 MB; 21 post-pressure cases passed.
  No OOM occurred. This supports collectible retention, not a gateway leak verdict.
- [ ] Correlate actual gateway loader growth and retaining roots with real turn activity.
  The current observed Vis host is a native executable; `jps -l` found no target
  JVM. The historical JVM GC report cannot be validated against that process.
  No GC flags or soft-reference policy were changed.

## Draft cleanup: requested policy, not yet automatic

- [ ] Remove genuinely abandoned/cancelled/approved drafts through the canonical
  workspace backend after preserving recoverable work and releasing every user.
  Current approval is repeatable and is not a terminal workspace state; cancelling
  a turn or subagent does not prove its draft is disposable. The workspace and
  foundation draft suites confirm these lifecycle boundaries.
- [ ] Automatic approval cleanup needs a path-use lifecycle guard, not a snapshot:
  other workspace roots, pending process starts and active handles must veto release.
  Independent review found no such authoritative guard. The workspace-ID uniqueness
  constraint does not cover shared-root references. Copy-only and unpublished work
  must survive; no user services may be stopped merely to force cleanup.
- [x] Synchronize live confinement before backend release. The ordering regression
  failed with persistence already on trunk but the live pointer still in the draft;
  both now move before asynchronous deletion starts. Discard hooks still veto both.
- [x] Retry stale and discarded rows through the canonical backend, including private
  extra roots whose primary clone is already gone. Extra roots no longer become
  orphan/raw-delete candidates while their owning draft is active. Regressions failed
  before the fixes. Timeout, refusal and failure never trigger raw-delete fallback;
  success requires the root to be demonstrably absent.
- No user draft was removed. Explicit orphan/journal purge policy is unchanged.

## Runtime retention: newest version plus live users

- [x] Reproduce release churn: 17 fresh runtime versions in each fixture store all
  survived the former fourteen-day sweep. Add the read-only
  `foundation.housekeeping/runtime-retention-plan`: numeric version ordering,
  newest release plus the required pin retained, unknown names/symlinks retained,
  other versions explicitly marked `:liveness-unverified`. Fifteen candidates per
  fixture store remained untouched.
- [x] Read-only live inventory found 29 runtime directories and 23 source directories,
  with newest installed version 0.5.16 and loaded-host pin 0.5.15. The plan listed
  27 runtime and 21 source candidates, not safe deletion approvals; no size claim.
- [x] Remove unsafe age-only deletion of installed runtimes and sources. A regression
  reproduced deletion of six old, newer and unrecognized version trees; another
  reproduced deletion through linked stores. Both now retain the installed files.
  Stale downloaded archives still age out. This safety fix does not reclaim the
  accumulated runtime trees or prove any candidate unused.
- [ ] Implement newest-version retention after trustworthy cross-process liveness
  checks. Keep any older version still required by a running process, then remove
  it after its last user exits. New lock files alone cannot prove that legacy
  processes are not using a version. No user runtime was deleted.
- [ ] Low priority: remove leftover socket directories only after proving their owner
  is gone. Python worker memory was not identified as a defect in these experiments.

## Verification

- Initial verification passed 306 cases; follow-up selected suites pass 309 with the
  committed runtime pin `be7ccea`, including native FFF and confined worker boundaries.
  Independent review found no correctness blocker; initial scan failure timing and
  pool identity now have a failing-before/passing-after regression. Formatting and
  scoped clj-kondo checks passed. The general analyzer reports 67 reflection warnings;
  the baseline contains matching warnings, and additional FFF findings did not reproduce in a clean
  compiler reload. Fresh-JVM reflection/boxed-math reloads emitted no warnings in
  changed FFF/editor code; housekeeping retains an existing Thread-constructor
  warning outside the diff. No warning suppressions were added.
- The live gateway was not restarted. Native-build optimization and native-image
  rebuilds were excluded. These results verify local source, not deployment or a
  measured reduction in the old gateway's CPU/RSS. Owned experiment workers and REPLs
  are stopped; existing concurrent checkout work remains separate. Follow-up delivery
  now validates and pushes each scoped item rather than ending at the experiment plan.
- Draft cleanup safety checks pass 98 cases: 31 housekeeping and 67 workspace/foundation
  lifecycle cases, including the real Python argument boundary. The latter used the
  committed runtime pin `be7ccea`, separate from concurrent release work. Formatting
  and clj-kondo passed; general reflection findings match the existing baseline.
  Clean compiler reload emitted only existing housekeeping Thread-constructor and
  draft case-performance warnings outside the diff.
- The concurrent runtime `0.5.19` bump failed confined-worker startup because bundled
  sources were extracted inside the jail. Its release owner reports a verified fix
  being prepared for `0.5.20`; that separate runtime change is not part of this work.
  The owner also confirmed there is no verified lifetime lease or quiescent window
  that could authorize deleting older runtime/source caches.
- Runtime-retention safety checks pass all 31 housekeeping cases after formatting.
  Clj-kondo reports no errors or warnings; the general analyzer reports 16 existing
  reflection findings outside the changed lines. Removed the obsolete version-sweep
  helper, target declarations and test option rather than leaving a disabled path.
