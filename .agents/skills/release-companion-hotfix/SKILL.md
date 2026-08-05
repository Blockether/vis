---
name: release-companion-hotfix
description: Ship the Vis Companion app to TestFlight and Google Play under the CURRENT VIS_VERSION — an app-only rebuild, no product release and no local store build. Use when asked to publish or tag the app, get a companion fix to testers, or push an app-only build: verify the app, commit and push it to main, then let `npm run release:mobile` create the immutable companion-vX.Y.Z-build.N tag that CI builds.
version: "1.0.0"
license: Apache-2.0
compatibility: agents
---

# release-companion-hotfix — app-only rebuild under the same version

Ships `apps/vis-companion` to both public beta channels **without touching
`VIS_VERSION`**: iOS → TestFlight (plus the public-link group after Beta App
Review), Android → Play track `beta` (that IS "open testing"). Nothing is built
on this machine; the tag is the trigger and GitHub Actions does the work.

Use `release-vis` instead when the CLI/native/gateway changed, or when the
marketing version must move. Marketing version bumps are that skill's job, never
this one's.

## The contract

- `VIS_VERSION` (repo root) is the marketing version and **stays put**. The store
  build number is `git rev-list --count HEAD`, so every commit on `main` is a new,
  monotonically increasing build of the same version.
- The trigger is one immutable annotated tag: `companion-v<VIS_VERSION>-build.<N>`.
  `.github/workflows/mobile-release.yml` fires on `companion-v*` and runs both
  store jobs exactly once. A platform whose repo secrets are absent is skipped,
  not failed.
- Only `apps/vis-companion/scripts/mobile-release.mjs` (via `npm run release:mobile`)
  may create that tag. **Never** create, move, reuse, or delete it with `git tag`
  by hand, and never run `release:ios:store` / `release:android:store` — those are
  recovery internals, not the release flow.

## Steps

1. **Confirm the intent and the state.** `git status --short`,
   `git log --oneline -3`, `cat VIS_VERSION`. Say which version and which HEAD
   commit is about to reach testers before doing anything.
2. **Verify the app** in `apps/vis-companion`:
   `npm run typecheck && npm run lint && npm run test && npm run build`.
   Anything red stays uncommitted; report it and stop.
3. **Commit and push the app change to `main`** — imperative subject, production
   code and its tests in one commit, hooks enabled (never `--no-verify`).
   `release:mobile` refuses anything else: the tree must be clean, the branch must
   be `main`, and local `HEAD` must equal `origin/main` exactly.
4. **Park unrelated work in progress.** Other people's half-finished files make the
   tree dirty and abort the release. Do not sweep them into the release commit —
   stash exactly those paths and restore them afterwards:
   ```bash
   git stash push -u -m "wip parked for companion release" -- <paths…>
   # …release…
   git stash pop
   ```
   Verify `git status --porcelain -uall` matches its pre-stash content after the pop.
5. **Preflight**: `npm run release:mobile -- --dry-run`. It prints the version, the
   store build number, the regular tag it hangs off, and the companion tag it would
   create. Read those four lines out loud in the reply.
6. **Release**: `npm run release:mobile`. It fetches tags, creates the annotated tag
   `Release Vis Companion <version> (<build>)` at `HEAD`, and pushes it to origin.
7. **Watch the run**, do not assume it: `gh run list --workflow=mobile-release.yml -L 3`
   then `gh run watch <id>`. Report the run URL and the resulting build number.

## When the script refuses (each message is a real precondition)

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
- Local keychain credentials only matter for store-script recovery
  (`npm run secrets -- doctor`); CI signs from repository secrets.
