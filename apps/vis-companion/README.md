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
