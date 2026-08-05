---
name: ios-crash-triage
description: Retrieve and diagnose the latest Vis Companion iOS/TestFlight crash, hang, watchdog, or OOM from App Store Connect and a paired physical iPhone. Use when a user reports an iOS crash or asks to check TestFlight feedback/device logs.
---
# iOS crash triage

Use the repository's collector instead of hand-building App Store Connect requests or
copying the iPhone's complete crash-log domain:

```sh
cd apps/vis-companion
npm run diagnostics:ios -- --days 14
```

The command collects both independent sources into a timestamped directory under
`/tmp` and prints the exact path:

- `app-store-connect/feedback.json`: recent TestFlight crash and screenshot feedback,
  with the TestFlight build number attached;
- `app-store-connect/*.ips`: Apple's crash log when Apple actually supplied one;
- `app-store-connect/*-screenshot-*`: tester screenshots;
- `device/index.json`: every physical-device candidate considered and why it was kept;
- `device/App-*.ips`: Vis app crash/watchdog reports;
- `device/JetsamEvent-*.ips`: system memory-pressure reports;
- `device/stacks-*.ips`: system stack snapshots;
- `manifest.json`: source successes/failures, timestamps, and the cutoff.

## Prerequisites

The App Store Connect half reads the same credentials as release tooling: environment
variables `VIS_ASC_KEY_ID`, `VIS_ASC_ISSUER_ID`, and either `VIS_ASC_KEY` or
`VIS_ASC_KEY_PATH`, then the `vis-ios` entries in the macOS login keychain. Configure
them once with:

```sh
npm run secrets asc /path/to/AuthKey_KEYID.p8 --issuer <uuid> --team <team-id>
```

The device half needs macOS, Xcode's command-line tools, and an unlocked, trusted,
paired physical iPhone visible to CoreDevice. It does not use a Simulator.

Either half can still succeed when the other is unavailable. Narrow explicitly when
needed:

```sh
npm run diagnostics:ios -- --asc-only --days 3
npm run diagnostics:ios -- --device-only --days 3
npm run diagnostics:ios -- --device-only --device <CoreDevice-id-or-UDID>
npm run diagnostics:ios -- --out /tmp/vis-ios-case --days 30
```

Use `npm run diagnostics:ios -- --help` for the complete option list.

## Why the script is the contract

- It queries both `betaFeedbackCrashSubmissions` and
  `betaFeedbackScreenshotSubmissions`; a tester can report a freeze without an Apple
  crash artifact.
- A `404` from a feedback submission's `crashLog` relationship is expected for hangs
  and OOMs. Keep the feedback record and continue; do not call the collection broken.
- It lists the root and `Retired` directories of the iPhone's
  `systemCrashLogs` domain separately. Filtering only the root prevents CoreDevice
  from descending into the nonmatching `Retired` directory.
- It copies only `App-*`, `JetsamEvent-*`, and `stacks-*` candidates one file at a
  time. Never replace this with `--source .`: that also transfers large sysdiagnose
  archives and can pull hundreds of megabytes.
- Generic `App-*` reports are retained only when their contents identify
  `com.blockether.viscompanion` or Vis Companion. Jetsam and stack reports are kept as
  context because iOS may omit the owning app's bundle id after killing its WebKit
  process.

## Triage procedure

1. Run the collector from `apps/vis-companion` with the smallest useful `--days`
   window. Read `manifest.json` first and distinguish “source unavailable” from “zero
   reports.”
2. Sort evidence by the report's own timestamp, not filesystem discovery order. Note
   the app/TestFlight build number before comparing it with current `main`; an old
   TestFlight report does not prove the current implementation still has the bug.
3. Read the newest relevant `App-*.ips` header, `termination`, crashed thread, and
   main-thread stack:
   - `0x8BADF00D` is an iOS watchdog kill; use the watchdog event and the blocked
     main-thread frames, not the generic word “crash.”
   - `EXC_BAD_ACCESS`, exception type, and termination namespace identify a normal
     process crash.
4. Inspect nearby `JetsamEvent-*` reports for memory pressure, the killed/largest
   process, resident pages, lifetime maximum, priority, and coalition. A WebKit
   `WebContent` kill can terminate a Capacitor app without producing an app crash
   log. Do not attribute a generic jetsam to Vis unless timestamp, foreground state,
   coalition, companion app evidence, or surrounding reports support it.
5. Use `stacks-*` only as a timestamped process snapshot. Correlate it with a crash or
   jetsam; do not treat the presence of a stack report as a crash by itself.
6. Read TestFlight comments and screenshots as user evidence. Do not expose tester
   email addresses, signed screenshot URLs, device identifiers, or raw logs in an
   answer. Diagnostic output is local and must never be committed.
7. Reproduce before editing. Add a regression test and observe RED against the
   affected build/commit, then apply the smallest fix and observe GREEN unchanged.
   For Companion work, finish with the relevant test plus `npm run typecheck`,
   `npm run lint`, and `npm run build`; run the iOS prepare/native build checks when
   native lifecycle code changes.
8. Report exactly which source produced the newest evidence, its timestamp/version/
   build, the concrete termination reason, the fix, RED/GREEN proof, and any source
   that was unavailable. Never claim App Store Connect had a crash log when it only
   had feedback.

The collector itself is covered by `scripts/ios-crashes.test.mjs`. If CoreDevice or
App Store Connect changes shape, update that test and the script together, then smoke
both live sources before committing.
