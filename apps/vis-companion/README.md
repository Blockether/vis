# vis-companion

A web, Android and iOS client for the Vis gateway, built with React 19,
Tailwind CSS v4 and Capacitor 8. It connects to the same gateway as the TUI.
Native push notifications use the [notification relay](../vis-companion-relay/README.md).

## What it does

- **Pair with a gateway** by scanning the QR from `vis-agent gateway pair`, opening the
  `vis://gateway?url=…&token=…` deep link, or pasting the URL + bearer token.
- **Sessions** — list, create, open, send turns, and watch replies stream live
  over SSE (`GET /v1/events?sids=<sid>`).
- **Voice dictation** — uploads a WAV recording and follows its transcription
  job through `GET /v1/sessions/:sid/voice/jobs/:id/events`. Match the
  `voice.job` event name, not the payload shape. This stream is separate from
  session events and distinguishes upload progress from transcription progress.
- **Settings** — reads `GET /v1/settings?channel=all`. Toggle changes persist in
  the gateway and apply to both the TUI and app.
- **Themes** — uses CSS generated from the Clojure themes with
  `clojure -X:companion-themes` in `src/lib/themes.generated.{css,ts}`. Theme
  selection is local to the app.
- **Multiple gateways** — save several (home LAN, Tailscale, cloudflared) and
  switch between them.

The gateway implementation is in `src/com/blockether/vis/internal/gateway/`.

## Connect from anywhere: Tailscale or cloudflared

The gateway itself is unchanged; you only choose how the phone reaches it.

**Tailscale** — put both devices on one tailnet, then on the host:

```sh
vis-agent gateway start --host 0.0.0.0 --require-token --pair
```

The pairing QR prefers the machine's `100.x` Tailscale address for access
outside the LAN. `0.0.0.0` listens on all IPv4 interfaces; use
`--host 100.x.y.z` to listen only on Tailscale.

**cloudflared** — expose the loopback gateway through a tunnel:

```sh
vis-agent gateway start --host 127.0.0.1 --require-token --pair   # note the token it prints
cloudflared tunnel --url http://127.0.0.1:7890              # prints https://<name>.trycloudflare.com
```

In the app's manual pairing form, enter the tunnel URL and gateway token.
Keep `--require-token` enabled when a tunnel exposes a loopback gateway.

## Develop

```sh
cd apps/vis-companion
npm install
npm run dev        # web at http://localhost:5273
npm run build      # type-check + production bundle into dist/ (React Compiler on)
npm run lint       # React Compiler static analysis over every src file (no eslint)
```

## Testing

```sh
npm test                 # the whole suite, the way CI runs it
npx vitest run <file>    # one file while you work
npm run typecheck        # tsc over the app and its tests
npm run test:storybook   # story tests, a static Storybook build, contrast audit
```

Each test answers one kind of question. Pick the layer by what you want to
know, and keep a test in a single layer:

- **Behavior** is the default: render the component with Testing Library and
  ask the accessibility tree — role, accessible name, state.
  `src/components/ui.test.tsx` is the reference suite. A file that needs a
  DOM says so with a `// @vitest-environment jsdom` docblock; everything
  else stays in the fast node environment.
- **Source conventions** are rules about the code itself — the closed control
  vocabulary, corner and shadow rungs, named ways out, gallery coverage.
  They live in `src/components/ui.conventions.test.ts`, which scans source
  files instead of rendering them. Add a rule there when a behavior test
  cannot see the thing you want to enforce.
- **Looks** belong to Storybook: stories are the visual gallery, and the
  `storybook` vitest project exercises the shipped Tailwind layout in
  Chromium.

Do not pin rendered markup with `toContain` string matches, and do not
restate source text imported with `?raw`. Both break on every styling
change and assert nothing a user can perceive. How a control paints is
reviewed in Storybook, not unit-pinned.

The suite runs as three vitest projects. `unit` covers everything under `src`,
`scripts` covers the tests that drive the real toolchain (a production Vite
build, a Playwright launch), and `storybook` runs the gallery in Chromium.
`unit` uses the `vmForks` pool: each file still gets its own module registry
and its own jsdom, but in a VM context inside a pooled process instead of a
worker started for that one file. Building a DOM for each of them cost 190s of
CPU and now costs 20s, which halves the `src` run; on a two-core machine — what
CI gets — it went from 124s to 46s. `scripts` keeps real processes, because a
native bundler refuses the objects a VM realm hands it.

## Native builds

Capacitor's generated `android/` and `ios/` projects are gitignored; create them
on demand:

```sh
npm run build
npm run add:android && npm run android   # needs Android Studio / SDK
npm run add:ios && npm run ios           # needs Xcode (macOS)
```

For QR scanning on device, install the optional ML Kit plugin:

```sh
npm install @capacitor-mlkit/barcode-scanning
```

On the web, scanning falls back to pasting the pairing link.

## Release the companion app

`VIS_VERSION` at the repository root defines the product version. Regular
Vis releases start the mobile workflow after the main release succeeds.
For an app-only release at the same version, use the mobile release command:

```sh
npm run release:mobile -- --dry-run
npm run release:mobile
```

Commit and push first. The mobile release command requires a clean `main`
matching `origin/main`. It creates an immutable `companion-vX.Y.Z-build.N` tag,
where `N` is the commit count, to release both stores. It does not create a
companion tag when a regular `vX.Y.Z` release already identifies `HEAD`.

To build or release one store directly, use:

```sh
npm run release:ios:store -- --no-upload
npm run release:android:store -- --no-upload
```

Do not submit the same build through both release methods or create companion
tags manually. `scripts/version.mjs` copies `VIS_VERSION` into the package
manifests. Store scripts use these version sources:

| Store field | Source |
| --- | --- |
| `CFBundleShortVersionString` / Android `versionName` | repo-root `VIS_VERSION` (npm metadata is only a mirror) |
| `CFBundleVersion` / Android `versionCode` | `git rev-list --count HEAD` — strictly monotonic and shared by both stores |

Before archiving manually in Xcode, run the iOS release script with `--prepare`.
It builds the web bundle, syncs Capacitor and writes version settings into
`App.xcodeproj`. Xcode archives read those project settings:

```sh
open ios/App/App.xcworkspace   # scheme App, destination "Any iOS Device (arm64)"
```

Signing uses Xcode-managed distribution certificates for the configured Apple
team. `-allowProvisioningUpdates` creates missing profiles. For API-key
authentication during upload, set:

```sh
export VIS_ASC_KEY_ID=XXXXXXXXXX
export VIS_ASC_ISSUER_ID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
export VIS_ASC_KEY_PATH=~/.appstoreconnect/private_keys/AuthKey_XXXXXXXXXX.p8
```

Alternatively, set `VIS_ASC_APPLE_ID` and `VIS_ASC_APP_PASSWORD`. Without either
method, `xcodebuild -exportArchive` uses the account signed into
**Xcode → Settings → Accounts**. Build artifacts are written to `build/ios/`.

### TestFlight audiences

```sh
npm run release:ios:store                          # internal groups AND the public link
npm run release:ios:store -- --audience all        # the default, spelled out
npm run release:ios:store -- --audience internal   # team only, no Beta App Review
npm run release:testflight                         # same distribution step for the LAST uploaded build
```

The default `--audience all` distributes the build to external groups and
internal groups that do not receive all builds automatically. Use
`--audience internal` to omit external distribution. Unknown values are
rejected before building.

External distribution waits for App Store Connect processing, associates the
build with a beta group and submits it for Beta App Review. The default group
is **Public**, with a public link. Review timing is controlled by Apple.
Internal testers do not require Beta App Review. The script prints the public
URL after distribution.

Public test link: <https://testflight.apple.com/join/4anYT4Wk>. To use the
existing **External Testers** group, pass its name explicitly:

```sh
node scripts/testflight.mjs --group "External Testers"
```

Beta metadata must use the app's primary locale. The distribution script reads
`primaryLocale` rather than assuming `en-US`. A locale mismatch can cause
`betaAppReviewSubmissions` to report that `betaAppLocalizations` is missing.

## Release to Google Play (Android)

> When publishing is disabled in `scripts/android-publish-freeze.mjs`,
> `release:android:store` refuses uploads, and CI skips Play and Firebase
> distribution. Local builds with `--no-upload`, track inspection with
> `--tracks`, and iOS releases remain available. Only the release owner may
> authorize re-enabling Android publication.
```sh
npm run release:android:store                             # signed .aab → EVERY tester track
npm run release:android:store -- --track all              # the default, spelled out
npm run release:android:store -- --track internal         # one channel only
npm run release:android:store -- --track beta,production  # any subset, comma-separated
npm run release:android:store -- --no-upload              # stop at the signed .aab
npm run release:android:store -- --track beta --rollout 0.1          # staged 10%, one track
npm run release:android:store -- --reuse-existing --build 4090 --track alpha  # no rebuild
npm run release:android:store -- --tracks                 # what each track serves today
```

`--track` accepts a comma-separated list and defaults to `all`. The script
queries Play for available testing tracks and updates the selected tracks in
one transaction. It rejects unknown names before building. Production is
excluded unless explicitly requested. A staged `--rollout` requires exactly
one track.

`beta` is open testing: users join through a public URL. `internal` supports
up to 100 named testers; `alpha` is closed testing.

Opt-in URL once the track is live:
**<https://play.google.com/apps/testing/com.blockether.viscompanion>**

While the app is still a **draft** in Play Console (never published), the API
refuses a normal rollout with *"Only releases with status draft may be created
on draft app"*. Upload with `--draft` in the meantime:

```sh
npm run release:android:store -- --track beta --draft
```

Then complete the store listing and required App content declarations in Play
Console before publishing.

Android uses `VIS_VERSION` through `package.json` for `versionName` and
`git rev-list --count HEAD` for `versionCode`. `scripts/android-prepare.mjs`
writes them to the generated project along with signing configuration,
`sdk.dir` and the barcode plugin's minimum SDK version.

Two credentials, both kept in the macOS login keychain, never in the repo:

```sh
npm run secrets keystore create                # upload key, once, 30-year validity
npm run secrets play <service-account.json>    # Play Developer API access
npm run secrets doctor                         # what is configured
```

Google Play re-signs the bundle with its app-signing key. Your upload keystore
authenticates uploads; back it up with `npm run secrets export-keystore` before
replacing a machine. Configure the service account in Play Console with
permission to release to testing tracks.

Gradle needs a **stock JDK 21**: Capacitor 8 compiles with `source 21`, and
GraalVM's `jlink` cannot run AGP's JdkImageTransform. The script finds one
itself and says so if none is installed (`sdk install java 21.0.11-tem`).

## Automatic releases

The Companion uses the repository's `VIS_VERSION`. Follow the root release
instructions to update versions and publish the annotated `vX.Y.Z` tag.
Tag and version must match; do not move a published tag.

The release workflow runs source verification and native builds in parallel.
Native artifacts remain in an unpublished draft; store delivery and stable
publication still require their verification gates. You do not need to run a
second full native dry run before every tag: the release runs those checks itself.

The tag also runs `.github/workflows/desktop-companion.yml`, which packages the
web bundle with [Pake](https://github.com/tw93/pake): macOS Universal
(Intel + Apple silicon, `.dmg`) and Linux x64/ARM64 (`.deb`, `.AppImage`).
Windows release packaging is currently disabled. macOS uses the self-hosted
runner; Linux uses native GitHub-hosted runners. All supported installers must
be present before the GitHub Release becomes public.

`scripts/desktop-package.mjs` holds the flags and asset names. To build installers
for your host, run `npm run build` followed by `npm run package:desktop`; output
lands in `build/desktop/`. Builds need a Rust toolchain and the platform's native
build dependencies: Xcode on macOS, WebKitGTK on Linux, or Visual Studio Build Tools
with **Desktop development with C++** and WebView2 on Windows.
Cargo compilation is reused from `build/desktop-target/`, independently of npx's
package cache. Set `CARGO_TARGET_DIR` to use another cache location. CI keys this
cache by OS, architecture, compiler and Pake version. A cold build still compiles
dependencies; cache hits do not skip compilation, signing or notarization checks.
A **Run workflow** from a branch keeps installers as workflow artifacts and
publishes nothing.

See [Desktop setup](../../resources/vis-docs/distributions.md#open-the-desktop-app)
for installation and gateway pairing.

For an app-only build, commit and push the changes, then run:

```sh
npm run release:mobile -- --dry-run
npm run release:mobile
```

The command tags the current `origin/main` with `companion-vX.Y.Z-build.N`.
`.github/workflows/mobile-release.yml` distributes that build to TestFlight
audiences and Android testing tracks, subject to the publishing controls
above. CI uses the same store scripts as local releases. Missing credentials may
skip an optional app-only platform; they fail a complete product release.

Before retrying, check whether the exact version/build already reached the store.
Do not upload it again merely because review is pending. Android's
`--reuse-existing --build N` promotes an uploaded build without rebuilding;
TestFlight distribution can be retried with `release:testflight`. API acceptance
is not review approval: check TestFlight's external build state and Play Console's
Publishing overview before telling external testers an update is available.
Apple/Google review and notarization queues are external waits, not build time.

| Secret | Value |
| --- | --- |
| `VIS_ASC_KEY_ID`, `VIS_ASC_ISSUER_ID`, `VIS_ASC_KEY` | App Store Connect API key (`VIS_ASC_KEY` is the `.p8` *contents*) |
| `VIS_IOS_TEAM_ID` | Apple team id |
| `VIS_PLAY_SERVICE_ACCOUNT` | the service-account JSON |
| `VIS_ANDROID_KEYSTORE` | `npm run secrets export-keystore \| base64` |
| `VIS_ANDROID_KEYSTORE_PASSWORD`, `VIS_ANDROID_KEY_ALIAS`, `VIS_ANDROID_KEY_PASSWORD` | as generated |
| `VIS_ANDROID_GOOGLE_SERVICES` | `google-services.json` — only needed for Android push |

## Layout

```
src/
  lib/
    gateway.ts    REST + SSE client (bearer auth, fetch-stream SSE)
    pairing.ts    parse vis:// pairing links
    storage.ts    Capacitor Preferences (localStorage fallback) — saved gateways
    scan.ts       optional QR scanning
    deeplink.ts   vis:// app-open handler
    types.ts      gateway wire shapes
  screens/        Connect · Sessions · Session · Settings
  components/      shared UI primitives
  App.tsx         shell, tab bar, connection state
```
