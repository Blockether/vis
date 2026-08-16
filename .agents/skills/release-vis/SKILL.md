---
name: release-vis
description: Cut a full Vis release: bump VIS_VERSION, mirror it into the companion, commit the `chore(release): vX.Y.Z` bump, push main, then push the annotated vX.Y.Z tag that ships Clojars packages, the GitHub Release, the native distributions, and both app stores. Use when asked to release Vis, publish a new version, or bump the product version — the CLI/native/gateway path, not an app-only rebuild.
version: "1.0.0"
license: Apache-2.0
compatibility: agents
---

# release-vis — one version, one tag, everything ships

`VIS_VERSION` at the repo root is the single version source for the CLI, the
native image, iOS and Android. Pushing `v<VIS_VERSION>` starts four workflows:

- `release.yml` — Clojars deploy of `com.blockether/vis` + every extension package,
  the GitHub Release (with `bin/install-vis-agent`, `bin/vis-agent`), the version
  stamp in `README.md`/`resources/vis-docs/index.md`, and a
  `release: update release notes for vX.Y.Z` commit that writes `CHANGELOG.md`.
- `native-release.yml` — `vis-agent-<os>-<arch>-community.tar.gz` per platform,
  attached to that same release.
- `mobile-release.yml` (called by `release.yml` **after** it succeeds) — iOS to
  TestFlight with the public link group, Android to Play track `beta`.

So the changelog is generated in CI: do not hand-write the new section locally.
For an app-only fix under the version that is already out, use
`release-companion` instead — this skill is never the way to rebuild just the app.

## Preconditions

- On `main`, clean tree, `HEAD == origin/main`. Unrelated work in progress gets
  stashed (`git stash push -u -- <paths…>` … `git stash pop`), never folded into
  the release commit.
- The repo is green: the relevant `run_tests` namespaces, `lint_code`, and
  `format_code` for what changed; `npm run typecheck && npm run lint && npm run test`
  in `apps/vis-companion` when the app changed.
- The new version is agreed with the user (semver against `cat VIS_VERSION`).
  Ask once if it was not stated; a released version can never be re-cut.
- Toolchain untouched: GraalVM CE 25.1.3 (`.graalvm-version`) is locked, and the
  Android job builds on stock JDK 21. A release is not the place to move a pin.

## Steps

1. **Show the state**: `cat VIS_VERSION`, `git log --oneline -1`,
   `git tag --list 'v*' --sort=-v:refname | head -3`, `git status --short`.
   State the version you are about to cut and what is in it.
2. **Bump** `VIS_VERSION` to the bare `X.Y.Z` (no `v`, single line).
3. **Mirror it**: `cd apps/vis-companion && npm run sync:version`. That is
   `scripts/version.mjs`; it stamps `package.json`, `package-lock.json` and BOTH
   PyPI distributions — `packages/vis-contract/python/pyproject.toml`,
   `packages/vis-agent/pyproject.toml` and the `vis-contract==X.Y.Z` pin inside the
   latter. Never hand-edit a version
   field it owns, and never override a store's marketing version.
4. **Commit exactly those five files** with subject `chore(release): vX.Y.Z`
   (`VIS_VERSION`, `apps/vis-companion/package.json`,
   `apps/vis-companion/package-lock.json`,
   `packages/vis-contract/python/pyproject.toml`,
   `packages/vis-agent/pyproject.toml`). Hooks stay on: never `--no-verify`.
5. **Push main**: `git push origin main`. The tag must point at a commit that is
   already on `origin/main` — CI verifies tag == `VIS_VERSION` == current `main`
   and fails the release otherwise.
6. **Tag and push the tag** (this is the irreversible step; do it only when the
   user asked for a release):
   ```bash
   git tag --annotate "vX.Y.Z" --message "Release vX.Y.Z"
   git push origin "refs/tags/vX.Y.Z"
   ```
7. **Watch all three**, do not assume:
   ```bash
   gh run list -L 6
   gh run watch <id>          # release.yml → then its mobile job
   ```
   Report each conclusion, the release URL, and the store build number.

## Failure modes

- **Tag rejected / version mismatch** — `VIS_VERSION`, the tag name, and `main`
  disagree. Fix `main` first, delete the *unpushed* local tag, re-tag.
- **The tag published but a later step failed** — the tag is spent. Never move or
  re-push it: fix forward with the next patch version, or, when only the app job
  failed, ship the app with `release-companion`.
- **Clojars says the version exists** — the deploy is skipped by design; the rest of
  the release still completes.
- **Release notes push races another commit** — CI rebases and retries five times;
  if it still fails, the artifacts are published and only the notes commit is missing.
- Store submissions, expiring TestFlight builds, force pushes, and history rewrites
  are never automatic follow-ups. Ask.
