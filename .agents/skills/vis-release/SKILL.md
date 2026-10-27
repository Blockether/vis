---
name: vis-release
description: Make a Vis product release or a Companion app release. Use only when the user asks for a release.
disable-model-invocation: true
---
# Releases

Do these steps only when the user asks for a release.

- `VIS_VERSION` is the version source. `npm run sync:version` in `apps/vis-companion` copies it to the package manifests and the SDK pyproject file; do not edit those version fields by hand.
- A product release bumps, mirrors, commits as `chore(release): vX.Y.Z`, pushes `main`, then pushes the annotated `v<VIS_VERSION>` tag. The tag, the version and the current `main` must agree.
- Never move a published tag. Publish a new version instead.
- An app-only rebuild keeps the version and uses the git commit count as the build number.
- Use `npm run release:ios:store` / `release:android:store` or `npm run release:mobile`, not both. Only `release:mobile` creates `companion-v<version>-build.<N>`. Never create that tag by hand or submit the same build both ways.
- Write the root `CHANGELOG.md` by hand: each product release commit adds its dated `## [vX.Y.Z] - YYYY-MM-DD` section. CI does not edit it; the release workflow drafts the GitHub Release with generated notes.
