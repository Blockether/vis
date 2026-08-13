---
name: release-companion-hotfix
description: Ship the Vis Companion app to TestFlight and Google Play under the CURRENT VIS_VERSION — an app-only rebuild, no product release. Use when asked to publish the app, get a companion fix to testers, or push an app-only build: verify the app, commit and push it to main, then PREFLIGHT this machine — if the store credentials and toolchains are here, build and upload LOCALLY with `release:ios:store` / `release:android:store`; only when something is missing fall back to the `companion-vX.Y.Z-build.N` tag that CI builds.
version: "2.0.0"
license: Apache-2.0
compatibility: agents
---

# release-companion-hotfix — app-only rebuild under the same version

Ships `apps/vis-companion` to both public beta channels **without touching
`VIS_VERSION`**: iOS → TestFlight internal groups AND the public-link group after
Beta App Review, Android → the Play tester tracks `internal`, `alpha` and `beta`
in one edit (`beta` IS "open testing"). Both defaults are the full fan-out, so
the two stores serve the same build: one build, every channel, nothing to
promote.

Use `release-vis` instead when the CLI/native/gateway changed, or when the
marketing version must move. Marketing version bumps are that skill's job, never
this one's.

## Where the build happens: LOCAL FIRST

Waiting on GitHub Actions is the fallback, not the plan. This machine already has
the same credentials CI does, and a local run gives its failure in a minute
instead of twenty. So the release path is chosen by a **preflight**, never by
habit — and the preflight is run before anything is announced:

```bash
cd apps/vis-companion
npm run --silent secrets -- doctor      # ASC key, Play service account, upload keystore
xcodebuild -version                     # iOS leg
node scripts/jdk.mjs                    # Android leg: the stock JDK 21 Gradle will use
```

- **Every line above green → release LOCALLY** (steps 5L–6L). No tag, no CI, no wait.
- **iOS ready, Android not** (or vice versa) → run the leg that is ready locally and
  say plainly which platform did not ship, then decide with the user.
- **Neither leg ready** (missing key, no Xcode, no JDK 21) → fall back to the tag
  route (steps 5C–7C) and say *why* CI was used, naming the missing thing.

`release:ios:store` / `release:android:store` are the ordinary local release
commands under this skill. They stay off-limits for a *regular* product release,
which `release-vis` owns.

## The contract

- `VIS_VERSION` (repo root) is the marketing version and **stays put**. The store
  build number is `git rev-list --count HEAD` on both platforms, so every commit on
  `main` is a new, monotonically increasing build of the same version — identical
  whether the bytes were built here or in CI.
- The CI trigger is one immutable annotated tag: `companion-v<VIS_VERSION>-build.<N>`.
  `.github/workflows/mobile-release.yml` fires on `companion-v*`. Only
  `scripts/mobile-release.mjs` (via `npm run release:mobile`) may create it —
  **never** `git tag` by hand, never move, reuse or delete it.
- A local release creates **no tag**. Tagging after a local upload would make CI
  build and upload the very same build number a second time.

## Steps

1. **Confirm the intent and the state.** `git status --short`, `git log --oneline -3`,
   `cat VIS_VERSION`. Say which version and which HEAD commit is about to reach
   testers before doing anything.
2. **Verify the app** in `apps/vis-companion`:
   `npm run typecheck && npm run lint && npm run test && npm run build`.
   Anything red stays uncommitted; report it and stop.
3. **Commit and push the app change to `main`** — imperative subject, production code
   and its tests in one commit, hooks enabled (never `--no-verify`). Push even for a
   local release: a build that testers can install must be reproducible from `main`.
4. **Park unrelated work in progress.** Other people's half-finished files must not be
   swept into the release commit — stash exactly those paths and restore them after:
   ```bash
   git stash push -u -m "wip parked for companion release" -- <paths…>
   # …release…
   git stash pop
   ```
   Verify `git status --porcelain -uall` matches its pre-stash content after the pop.
   (The CI route additionally *requires* a clean tree; a local build only needs the
   app sources committed, but park the noise anyway so the upload matches `main`.)

### Local route (preferred)

5L. **Preflight** exactly as above and report the three lines.
6L. **Release**, one leg at a time so a failure names its platform:
   ```bash
   npm run release:ios:store          # web → cap sync → signed archive → .ipa → TestFlight
   npm run release:android:store      # web → cap sync → signed .aab → Play internal+alpha+beta
   ```
   Both legs fan out to **every tester channel** by default and match each other:
   iOS internal groups + the public-link group (Beta App Review), Play
   `internal,alpha,beta`. Narrow only when asked — `-- --audience internal`,
   `-- --track internal` — and never to the store itself. Report the build number
   each leg uploaded and the channels it landed in. Do **not** create a companion
   tag afterwards.

### CI route (fallback only)

5C. `npm run release:mobile -- --dry-run` — read its four lines (version, build
   number, regular tag, companion tag) out loud in the reply.
6C. `npm run release:mobile` — creates and pushes the annotated tag at `HEAD`.
7C. **Watch the run**, do not assume it: `gh run list --workflow=mobile-release.yml -L 3`
   then `gh run watch <id>`. Report the run URL and the resulting build number.

## When the tag script refuses (each message is a real precondition)

| Message | What it means | Do |
| --- | --- | --- |
| `working tree is not clean` | uncommitted files | commit the release, stash the rest (step 4) |
| `mobile releases must be cut from main` | wrong branch | merge/land on `main` first |
| `local main must exactly match origin/main` | unpushed or behind | push, or `git pull --ff-only` |
| `vX.Y.Z does not exist` | no regular release for this `VIS_VERSION` | run `release-vis` first |
| `vX.Y.Z is not an ancestor of HEAD` | `VIS_VERSION` does not describe this branch | rebase onto `main`, or bump the version |
| `companion-… already exists on origin` | that commit already shipped | land another commit, or nothing to release |
| `✓ No companion tag needed` (exit 0) | `vX.Y.Z` already points at `HEAD` | **stop** — the regular release ships both stores by itself |

## Afterwards

- TestFlight builds can only be **expired**, never deleted: `npm run release:expire`
  (`--yes` is destructive — confirm with the user first).
- Release notes derive from committed history; `CHANGELOG.md` is authoritative and
  hand-edited, never regenerated for an app-only rebuild.
- A local upload and a CI upload are interchangeable products; never ship the same
  build number twice by doing both.
