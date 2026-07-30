# vis-companion

Universal (**web · Android · iOS**) companion app for the **vis gateway**.
Built with **React 19**, **Tailwind CSS v4**, and **Capacitor 8**. It is a pure
gateway *client*: it reuses the exact same long-lived gateway daemon the TUI and
other channels drive — no separate backend.

## What it does

- **Pair with a gateway** by scanning the QR from `vis gateway pair`, opening the
  `vis://gateway?url=…&token=…` deep link, or pasting the URL + bearer token.
- **Sessions** — list, create, open, send turns, and watch replies stream live
  over SSE (`GET /v1/sessions/:sid/events`).
- **Settings** — renders the gateway's cross-channel feature-toggle registry
  (`GET /v1/settings?channel=all`). Flipping a toggle persists in the daemon, so
  the **TUI and the app share one settings state**.
- **Shared theme** — loads the TUI's selected palette from `GET /v1/theme`, applies
  the returned CSS variables immediately, and persists changes through
  `POST /v1/theme`, so the next TUI and every companion use the same theme.
- **Multiple gateways** — save several (home LAN, Tailscale, cloudflared) and
  switch between them.

The wire contracts mirror `src/com/blockether/vis/internal/gateway/{server,client,pairing}.clj`.

## Connect from anywhere: Tailscale or cloudflared

The gateway itself is unchanged; you only choose how the phone reaches it.

**Tailscale** — put both devices on one tailnet, then on the host:

```sh
vis gateway start --host 0.0.0.0 --require-token --pair
```

The pairing QR automatically prefers the durable `100.x` Tailscale address, so
the scanned URL keeps working off-LAN.

**cloudflared** — expose the loopback gateway through a tunnel:

```sh
vis gateway start --host 127.0.0.1 --require-token --pair   # note the token it prints
cloudflared tunnel --url http://127.0.0.1:7890              # prints https://<name>.trycloudflare.com
```

Then in the app: **Gateway → Manual**, paste the `https://<name>.trycloudflare.com`
URL and the bearer token. (A non-loopback / tunnelled gateway always requires the
token — that is the only thing guarding your sessions.)

## Develop

```sh
cd apps/vis-companion
npm install
npm run dev        # web at http://localhost:5273
npm run build      # type-check + production bundle into dist/ (React Compiler on)
npm run lint       # React Compiler static analysis over every src file (no eslint)
```

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

## Release to TestFlight (iOS)

One command builds the bundle, syncs Capacitor, archives, exports a signed
`.ipa`, and uploads it to App Store Connect:

```sh
npm run release:ios                 # full run
npm run release:ios -- --no-upload  # stop at the signed .ipa
npm run release:ios -- --version 1.2.0 --build 4711
npm run release:ios -- --prepare    # web + cap sync + stamp versions, then STOP
```

Versioning has **no hand-edited state** — the script passes both numbers to
`xcodebuild` as build settings, so the gitignored `ios/` project never has to be
touched:

| Store field | Source |
| --- | --- |
| `CFBundleShortVersionString` (version) | `package.json` `"version"` — the same value the app stamps on every gateway request and shows on the version-mismatch screen |
| `CFBundleVersion` (build) | `git rev-list --count HEAD` — strictly monotonic, so two uploads can never collide |

Bump `package.json` `version` for a user-visible release; the build number takes
care of itself.

To archive by hand in Xcode, run `--prepare` first: it builds the bundle, syncs
Capacitor, and writes the same two numbers into `App.xcodeproj` — a GUI archive
reads the project, not our build settings, so without the stamp *Product >
Archive* would ship `1.0 (1)` and App Store Connect would reject it. Then:

```sh
open ios/App/App.xcworkspace   # scheme App, destination "Any iOS Device (arm64)"
```

Signing uses Xcode-managed (cloud) distribution certificates for team
`JSZTFUBUBB`; `-allowProvisioningUpdates` creates what is missing. For the
upload step, set an App Store Connect API key (no 2FA prompt):

```sh
export VIS_ASC_KEY_ID=XXXXXXXXXX
export VIS_ASC_ISSUER_ID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
export VIS_ASC_KEY_PATH=~/.appstoreconnect/private_keys/AuthKey_XXXXXXXXXX.p8
```

or `VIS_ASC_APPLE_ID` + `VIS_ASC_APP_PASSWORD` (app-specific password). With
neither set, the upload falls back to `xcodebuild -exportArchive
-exportOptionsPlist … destination=upload`, which authenticates as the Apple
account signed into **Xcode → Settings → Accounts** — that is what a local
release from this machine uses. Artifacts land in `build/ios/`.

### Public TestFlight link

```sh
npm run release:ios -- --public     # upload, then open the build to the public group
npm run release:testflight          # same distribution step for the LAST uploaded build
```

`--public` waits for App Store Connect to finish processing, creates (once) an
external beta group named **Public** with a public link, attaches the build, and
submits it for **Beta App Review**. Review takes hours on the first build of a
version and is usually instant afterwards; the link itself never changes, so it
can go in a README or a tweet. Internal testers get the build immediately,
without review. The public URL is printed at the end of the run.

**Live link: <https://testflight.apple.com/join/4anYT4Wk>** (group **External
Testers**). The group already existed, so pass it explicitly instead of letting
the default create a third one:

```sh
node scripts/testflight.mjs --group "External Testers"
```

Beta metadata (feedback email, review contact, notes) is filled and permanent.
One gotcha it cost us: Apple validates `betaAppLocalizations` against the app's
**primary locale** — this app is `en-GB`, and an `en-US`-only localization made
`betaAppReviewSubmissions` answer the misleading *"betaAppLocalizations not
found for this app"*. `distribute()` now reads `primaryLocale` from the app and
uses it.

## Release to Google Play (Android)

```sh
npm run release:android                        # signed .aab → internal track
npm run release:android -- --track beta        # OPEN testing — the public one
npm run release:android -- --no-upload         # stop at the signed .aab
npm run release:android -- --rollout 0.1       # staged 10%
npm run release:android -- --tracks            # what each track serves today
```

Play's equivalent of a public TestFlight link is the **`beta` (Open testing)**
track: anyone with the URL joins, no invite and no tester list. `internal` is
the fast lane (100 named testers, no review), `alpha` is closed testing.

Opt-in URL once the track is live:
**<https://play.google.com/apps/testing/com.blockether.viscompanion>**

While the app is still a **draft** in Play Console (never published), the API
refuses a normal rollout with *"Only releases with status draft may be created
on draft app"*. Upload with `--draft` in the meantime:

```sh
npm run release:android -- --track beta --draft
```

and finish it in Play Console ▸ the app's **Dashboard** — store listing, App
content (privacy policy, data safety, content rating, target audience, ads) and
then **Publish** — none of which the Play Developer API exposes. After that
first publish, `--track beta` rolls out unattended.

Versioning matches iOS exactly — `versionName` from `package.json`,
`versionCode` from `git rev-list --count HEAD` — and both are stamped into the
gitignored `android/` project by `scripts/android-prepare.mjs`, together with
the signing config, `sdk.dir`, and the minSdk floor the barcode plugin needs.

Two credentials, both kept in the macOS login keychain, never in the repo:

```sh
npm run secrets keystore create                # upload key, once, 30-year validity
npm run secrets play <service-account.json>    # Play Developer API access
npm run secrets doctor                         # what is configured
```

Google Play App Signing re-signs the bundle with Google's own key, so the
upload keystore only proves the upload came from you — but losing it still means
a support ticket, hence `npm run secrets export-keystore` before you wipe a
machine. The service account comes from Play Console ▸ Users and permissions ▸
Invite via API, and needs *Release to testing tracks*.

Gradle needs a **stock JDK 21**: Capacitor 8 compiles with `source 21`, and
GraalVM's `jlink` cannot run AGP's JdkImageTransform. The script finds one
itself and says so if none is installed (`sdk install java 21.0.11-tem`).

## Automatic releases

`.github/workflows/mobile-release.yml` runs both stores from one tag:

```sh
git tag companion-v1.0.2 && git push origin companion-v1.0.2
```

iOS goes to TestFlight *and* the public group; Android goes to the `beta` track.
A manual **Run workflow** lets you pick platform, track, and whether to submit
for Beta App Review. The workflow contains no build logic — it calls the very
same `release:ios` / `release:android` scripts, reading credentials from
repository secrets instead of the keychain, so a local release and a CI release
cannot drift. A platform whose secrets are missing is skipped, not failed.

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
    pairing.ts    parse vis:// links and the JSON pairing payload
    storage.ts    Capacitor Preferences (localStorage fallback) — saved gateways
    scan.ts       optional QR scanning
    deeplink.ts   vis:// app-open handler
    types.ts      gateway wire shapes
  screens/        Connect · Sessions · Session · Settings
  components/      shared UI primitives
  App.tsx         shell, tab bar, connection state
```
