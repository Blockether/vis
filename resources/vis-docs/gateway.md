# Gateway, pairing & remote access

Every vis channel talks to one long-lived **gateway daemon**: an HTTP + SSE
runtime that owns
the sessions, turns, and the live event bus. You rarely start it by hand; a
channel spawns it for you. This page explains its lifecycle, why
`vis-agent gateway start` stays in the foreground, the token model (and the
`HTTP 401` you hit on `--host 0.0.0.0`), and how to pair a phone over LAN or
Tailscale.

## The gateway starts itself (in the background)

When you run a client such as:

```sh
vis-agent tui
```

the client looks up the gateway registered for the current database
(`~/.vis/vis.mdb` by default). If none is alive it **spawns one, fully
detached** — on unix under `nohup … &`, reparented to init, with its
stdout/stderr captured to a per-database boot log under
`~/.vis/gateway/`. That daemon is *client-managed*: it self-reaps once the
last client disconnects. So the normal flow is just "start the TUI" — the
background gateway is automatic, and a herd of clients all attach to the
same one.

You do **not** need to run `vis-agent gateway start` yourself for local use.

### Why `vis-agent gateway start` does not go to the background

`vis-agent gateway start` is deliberately a **foreground** daemon: it prints its
connection line and parks until you stop it with `Ctrl-C` / `SIGTERM`. It is
meant for running the gateway as a supervised, user-owned process (a
`systemd`/`launchd` unit, a container entrypoint, a `tmux` pane) — not for a
throwaway shell. A foreground `vis-agent gateway start` is **not** refcounted, so
it will not self-reap when clients come and go.

To run it detached yourself, background it explicitly:

```sh
# quick-and-dirty
nohup vis-agent gateway start --host 0.0.0.0 --require-token > ~/.vis/gateway.out 2>&1 &

# or let a client auto-spawn the managed background daemon for you
vis-agent tui
```

Inspect and control the daemon:

```sh
vis-agent gateway status     # pid, url, db, client count, auth mode
vis-agent gateway stop       # ask the running daemon to exit
```

## The token model — and the `HTTP 401`

Auth is gated on the **bind host**:

| Bind                                   | Bearer token | Why                                             |
| -------------------------------------- | ------------ | ----------------------------------------------- |
| `127.0.0.1` (default)                  | **off**      | a single-user localhost daemon; the token dance is pure friction |
| any non-loopback (`0.0.0.0`, LAN, Tailscale) | **required** | the port is reachable by other hosts            |
| `127.0.0.1 --require-token`            | required     | force the token on loopback too                 |

The token is a secret minted on first run into `~/.vis/gateway.token`
(mode `600`); override with `--token-file PATH`. A client on the **same
machine** reads that secret from the gateway's on-disk registry
automatically, so a local TUI authenticates transparently even against a
`--host 0.0.0.0` daemon. Remote clients (a phone, another machine) must be
handed the token — that is what pairing does.

If you ever see:

```
vis: fatal error - gateway HTTP 401
{:error {:type "unauthorized", :message "missing or invalid bearer token"}}
```

it means the client reached a token-gated gateway without a valid token.
The usual causes: connecting from a different machine without pairing, or a
stale/rotated `gateway.token`. Fixes: run the client on the same host as the
gateway, re-pair the remote client, or restart the gateway on loopback
(`vis-agent gateway start`).

## Getting the companion app (public testing)

The companion ships as one app for web, iOS and Android. Both mobile stores
are in **public testing** — the links are open to anyone, with no tester list
and no invite:

| Platform | Link |
|---|---|
| iOS / iPadOS (TestFlight) | <https://testflight.apple.com/join/4anYT4Wk> |
| Android (Play open testing) | <https://play.google.com/apps/testing/com.blockether.viscompanion> |

On iOS you install Apple's free **TestFlight** app first, then open the link;
on Android the link is an opt-in page that switches your Play account over to
the testing build of `com.blockether.viscompanion`. Both builds carry the same
version number as the CLI (the repo-root `VIS_VERSION`), and the app refuses to
talk to a gateway whose protocol version does not match — see
[Protocol version and compatibility](#protocol-version-and-compatibility).

Beta feedback goes to `karol@blockether.com`, or the **Send Beta Feedback**
button inside TestFlight. There is no account to create: the app is a client
for **your** gateway, so until you pair one it shows only the pairing screen.

## Pairing a phone (mobile companion)

Start the gateway on a reachable host and print a pairing QR:

```sh
vis-agent gateway start --host 0.0.0.0 --require-token --pair
```

`--pair` prints a terminal QR encoding a tiny URL payload:

```
vis://gateway?url=http%3A%2F%2F<host>%3A7890&token=<bearer-token>
```

In the companion app, open **Gateways → Add a gateway → Pairing link** and tap
**Scan QR** (or paste the link into the field and tap **Pair**). The
QR also lists the reachable hosts it picked, in preference order:
**Tailscale addresses first** (they keep working off-LAN), then LAN
(`10.x` / `192.168.x` / `172.16–31.x`), then the concrete bind host.

### Connecting from the companion app

Open the companion (web, iOS, or Android). Its first screen is **Gateways**.
Under **Add a gateway** there are two ways in:

- **Pairing link** — the fastest path, and the only one that also carries the
  token. Either tap **Scan QR** and point the camera at the QR from `vis-agent gateway
  pair` (or `vis-agent gateway start … --pair`), or paste the
  `vis://gateway?url=…&token=…` link into the field and tap **Pair**. Both fill
  in the URL and bearer token together, so there is nothing else to type.
- **URL + token** — for a gateway whose address you already know. Enter the
  gateway URL (LAN, Tailscale, or a Cloudflare tunnel address) and, when it is
  token-gated, the bearer token, then tap **Connect**. The token is optional
  only for a loopback (`127.0.0.1`) gateway.

Each saved gateway then carries a live status dot, re-probed every six seconds:
green `●` online (with the round-trip in milliseconds), red `●` offline, amber
`●` unauthorized — reachable, but the token is missing or wrong. Tap a row to
open its **Settings**; tap the active row to reconnect.

### Pairing a gateway that is already running

The TUI (and other channels) auto-spawn a **loopback** gateway on first launch,
so there is usually one running already — but bound to `127.0.0.1`, which a
phone can never reach. To pair a running daemon **without a start flag**, use:

```sh
vis-agent gateway pair
```

It reads the gateway registered for the current DB and prints the same QR that
`--pair` prints at boot — no restart needed. Two guardrails:

- **No gateway running** → it tells you to start one:
  `vis-agent gateway start --host 0.0.0.0 --require-token --pair`.
- **Running but loopback-bound** (the auto-spawned TUI daemon) → it refuses,
  because `127.0.0.1` is phone-local, and prints the exact restart to run:
  `vis-agent gateway stop` then `vis-agent gateway start --host 0.0.0.0 --require-token --pair`.

So: if you only ever ran `vis-agent tui`, the daemon behind it is loopback
and cannot be paired as-is — stop it and restart the gateway reachable (above).
Once it is bound to `0.0.0.0` (or a Tailscale host), `vis-agent gateway pair` prints
the QR on demand any time.

## Tailscale (access from anywhere)

`0.0.0.0` only exposes the gateway on the local network. To reach it from
your phone off-LAN, put both devices on a [Tailscale](https://tailscale.com)
tailnet and bind/advertise the machine's Tailscale IP (the `100.64.0.0/10`
range):

```sh
# with tailscale up on this machine
vis-agent gateway start --host 0.0.0.0 --require-token --pair
```

The pairing QR **automatically prefers the `100.x` Tailscale address** when a
tailnet interface is present, so the scanned URL keeps working when you leave
the LAN. (Binding `0.0.0.0` still listens on all interfaces including
Tailscale; the QR just advertises the durable `100.x` host.) For a locked-down
setup you can instead bind the Tailscale IP directly with
`--host 100.x.y.z`.

Because a non-loopback bind always requires the token, keep
`--require-token` on for any remote/Tailscale exposure — the bearer token is
the only thing standing between the tailnet and your sessions.

## Protocol version and compatibility

The gateway, the TUI, and the companion app update on different clocks: a phone
keeps a cached build for weeks while `brew upgrade` moves the daemon, or a
long-lived gateway serves a client shipped months later. So both halves publish
two numbers next to their release version — the wire `protocol` they speak, and
the oldest counterpart they still serve.

The gateway advertises its contract on every open endpoint (`GET /healthz`,
`GET /v1/capabilities`, `GET /v1/admin/status`):

```json
{"protocol": {"protocol": 1, "min_client": 1, "min_gateway": 1, "version": "…"}}
```

Every client stamps the mirror image on each request:

| Header | Meaning |
| --- | --- |
| `X-Vis-Protocol` | wire protocol the client speaks |
| `X-Vis-Min-Gateway-Protocol` | oldest gateway it can drive |
| `X-Vis-Client` / `X-Vis-Client-Version` | who is calling, and its release |

A client below `min_client` gets **HTTP 426 Upgrade Required** with a plain
explanation instead of a payload it would misread; `/healthz`, `/readyz`,
`/v1/capabilities`, and `/docs` stay open so the refusal can explain itself. A
gateway older than the client's floor is caught client-side from the same
advertised block. Both directions render the SAME verdict — the TUI prints it as
a panel, the companion replaces its UI with a version-mismatch screen naming
which half is stale and how to update it. A peer that advertises nothing is
grandfathered in, never refused.

Bump `protocol-version` in `gateway/protocol.clj` (and `APP_PROTOCOL` in the
companion's `lib/compat.ts`) only for a breaking wire change, and raise
`min-client-protocol` only when the old shape genuinely cannot be served.

## Push notifications (iOS / Android)

The gateway pushes exactly **one alert per finished turn** — `turn.completed` or
`turn.failed` — to every device registered with it, so you can leave the app and
still learn when the model is done. Nothing else is pushed, and the alert carries
only the session title plus `session_id`, `turn_id`, `status` and the sending
gateway's own `gateway_id` (the opaque id `/healthz` reports, so a phone paired
with several machines opens the tapped session on the machine that raised it);
the transcript never leaves the gateway. iOS devices are delivered through
**APNs**, Android devices through **FCM**; each half is configured independently
and either can be left off.

### Who a gateway can actually push to

Read this before configuring anything: APNs and FCM bind the credential to the
**app build**, not to the gateway.

- An APNs key signs only for topics owned by the Apple team that issued it. A key
  from any other team, signing for someone else's bundle id, is refused with HTTP
  403 `InvalidProviderToken` (or `TopicDisallowed`) — every time, permanently.
- An FCM service account may only send to tokens minted from its **own** Firebase
  project's `google-services.json`. Anything else is 403 `SENDER_ID_MISMATCH`.

So the credentials below configure push for a companion **you build and sign
yourself**, under your own Apple team and Firebase project, with your own bundle
id and package name. They cannot make a companion distributed through the App
Store or Play Store — which carries its publisher's topic and sender id — accept
a push from your gateway. That is an Apple/Google constraint, not a vis setting:
no key, topic, or environment value works around it.

| You run | Push to that app |
| --- | --- |
| your gateway + your own rebuilt companion | ✅ configure it below |
| your gateway + a store-distributed companion | only from a gateway holding **that publisher's** credentials |
| your gateway + a store-distributed companion, using your own key | ❌ permanent 403 — nothing to configure |

Push is optional. With no credentials the gateway still registers devices,
reports `features.push` as unavailable, and simply never sends; sessions,
streaming, and drafts are unaffected.

### Relayed push (a gateway with no Apple or Google key)

The way around the wall above is to hand the gateway a **capability** instead of a
credential. The signing key stays on a relay the app's publisher runs; the device
asks that relay for an opaque **grant** and gives the grant to the gateway.

```
app     -> POST   /v1/grants        {device_token}   => a grant
app     -> hands the grant to this gateway on "notify this device"
gateway -> POST   /v1/push          Bearer <grant>   => the relay signs and sends
(nothing is stored: the grant carries its own sealed expiry)
```

Point a gateway at a relay with either of:

```bash
export VIS_PUSH_RELAY_URL=https://push.example.com
echo '{:url "https://push.example.com"}' > ~/.vis/relay.edn
```

Then register the device with a `grant` instead of a `token`:

```bash
curl -sS -X POST "$GATEWAY/v1/devices" \
  -H "x-vis-protocol: 2" -H "authorization: Bearer $VIS_TOKEN" \
  -H 'content-type: application/json' \
  -d '{"grant":"…","platform":"ios","label":"iPhone"}'
```

What that changes, and why it is worth an extra hop:

- this gateway holds **no `.p8` and no service-account JSON** — nothing whose leak
  can only be repaired by breaking push for every other gateway;
- this gateway **never learns the raw device token**, so a gateway you do not trust
  cannot fingerprint the device it notifies;
- **a grant expires by itself** — its expiry is sealed inside it, so an abandoned
  gateway goes mute on its own and the relay never keeps a list of anybody;
- the relay learns *when* a push happened, never *what* — the alert body can be
  encrypted app-side, so the promise above still holds.

A relay answering `404` (the grant is forged, expired, or was sealed under a key the
relay has since rotated away) or `410` (the provider says the device is gone) makes
the gateway forget the device on the spot. Direct credentials and a
relay can coexist: a device registered with a `token` uses the credentials below,
a device registered with a `grant` uses the relay. `GET /v1/devices` reports which
under `push.relay`.

The relay itself is in this repo — `apps/vis-companion-relay`, a Cloudflare Worker
that stores **nothing**: no database, no queue, no cron. A grant is an AES-256-GCM
sealed capability carrying its own device token and expiry, and the abuse counters
live in Cloudflare's rate limiting bindings, so there is no table to dump, exhaust,
or migrate. Its README covers deploying, the limits and the failure verdicts.

### Gateway side, iOS (APNs credentials)

Push is **off until the gateway holds an APNs key**. On macOS the key can live in
the login keychain, so nothing sensitive touches the filesystem:

```bash
security add-generic-password -U -s vis-apns -a key      -w "$(cat AuthKey_ABCD123456.p8)"
security add-generic-password -U -s vis-apns -a key_id   -w ABCD123456
security add-generic-password -U -s vis-apns -a team_id  -w YOURTEAMID
security add-generic-password -U -s vis-apns -a topic    -w com.example.yourapp
security add-generic-password -U -s vis-apns -a environment -w production
```

The key is read per signature rather than cached, so locking the keychain stops
delivery immediately. Otherwise give the gateway one of:

```bash
export VIS_APNS_KEY_PATH=~/.vis/apns/AuthKey_ABCD123456.p8
export VIS_APNS_KEY_ID=ABCD123456
export VIS_APNS_TEAM_ID=YOURTEAMID                  # your Apple team id
export VIS_APNS_TOPIC=com.example.yourapp           # your own build's bundle id
export VIS_APNS_ENV=production                      # or sandbox for Xcode builds
```

or drop the `.p8` into `~/.vis/apns/` (the key id is read from its filename) and
put the rest in `~/.vis/apns/apns.edn`:

```clojure
{:team-id "YOURTEAMID" :topic "com.example.yourapp" :environment "production"}
```

Create the key once at *Apple Developer -> Certificates, Identifiers & Profiles
-> Keys*, with the **Apple Push Notifications service (APNs)** capability
enabled. A team-scoped key signs for every app of that team (Apple allows two
per environment); a topic-specific key is restricted to the bundle ids you
select. Neither kind expires — the only remedy for a leaked key is revoking it,
which breaks push for every gateway that was using it.

`GET /v1/capabilities` reports readiness as `features.push`, naming what is
missing when it is not ready.

### Gateway side, Android (FCM credentials)

Android uses **FCM HTTP v1**, which authenticates with a Firebase *service
account* JSON (Firebase console -> *Project settings -> Service accounts ->
Generate new private key*). Same trust model as the APNs key:

```bash
security add-generic-password -U -s vis-fcm -a service_account -w "$(cat sa.json)"
security add-generic-password -U -s vis-fcm -a project_id      -w your-firebase-project
```

or, without a keychain:

```bash
export VIS_FCM_SERVICE_ACCOUNT_PATH=~/.vis/fcm/service-account.json
```

A `*.json` service account dropped into `~/.vis/fcm/` is picked up as well. The
gateway signs an RS256 assertion, exchanges it for an OAuth access token (cached
for 50 minutes) and posts to `projects/<id>/messages:send`; tokens FCM reports as
`UNREGISTERED`/`INVALID_ARGUMENT` are evicted like their APNs counterparts.

The app half is `google-services.json` from the same Firebase project, for an
Android app whose package name equals the Capacitor `appId`. Because `android/`
is regenerable and gitignored, it is stamped in at build time by
`npm run prepare:android` (also run by `npm run android` / `build:android`) from
the keychain (`vis-fcm/google_services`), `~/.vis/fcm/google-services.json`, or
`--file`. Without it the app still builds and simply never gets a token.

Registrations from a platform the gateway cannot serve are stored and listed but
never sent — the device reports `unsupported-platform` rather than having its
token thrown at the wrong provider.

### Secret, or shippable?

Running gateways for other people means distributing none of the first group.

| File or value | Verdict |
| --- | --- |
| APNs key `~/.vis/apns/AuthKey_*.p8` | **secret** — a private signing key; whoever holds it can push to every app it is scoped to, until it is revoked |
| FCM service account `~/.vis/fcm/*.json` | **secret** — a Google private key, same rule |
| gateway token (`--token-file`, `~/.vis/gateway.token`) | **secret** — full API access to that gateway |
| device tokens (`~/.vis/devices.edn`) | **private** — never echoed in full; `GET /v1/devices` masks them |
| `google-services.json` | **shippable** — client config that already sits inside every APK; it holds no private key |
| bundle id / package name, APNs topic, Apple team id, Firebase project id | **public** — identifiers, not credentials |
| APNs key id (`ABCD123456`) | **public** — it is just the `.p8` filename |

There is no way to hand out the first group safely, and no per-holder
revocation: one leak revokes the key for everyone. If devices must be woken by
gateways you do not control, the signing key stays on a service you run and
those gateways call it — it is never copied onto them.

### Device registry

| Route | What it does |
| --- | --- |
| `GET /v1/devices` | registered devices (tokens **masked**) + this gateway's push readiness |
| `POST /v1/devices` | idempotently register `{token, platform, environment, client, client_version, label}` |
| `DELETE /v1/devices/:token` | stop pushing to one device |
| `POST /v1/devices/actions/test` | one test alert to every device, with the provider's per-device verdict |

Tokens live in `~/.vis/devices.edn`, are never echoed back in full, and a device
Apple reports as `Unregistered`/`BadDeviceToken` is evicted automatically. A
token registered under the wrong APNs environment is retried once against the
other one and then re-labelled — the single most common misconfiguration fixes
itself.

### App side

Companion -> gateway **Settings -> Notifications**: *Notify this device* asks the
OS for permission, registers the device token with that gateway, and *Send a test*
proves the whole chain. Tapping an alert reopens the session it came from.

The iOS capability itself (`aps-environment` entitlement + AppDelegate token
forwarding) is stamped into the regenerable `ios/` project by
`npm run release:ios:store -- --prepare`, so it survives a `cap add ios`. Android needs
no entitlement — only `google-services.json`, stamped by `npm run prepare:android`.


## Shared slash commands

`GET /v1/slashes` returns the channel-safe command palette used by web clients.
The daemon derives it from the same extension slash registry and prompt templates
as the TUI, then adds client-native navigation commands such as `/new-session`
and `/sessions`. Sending an engine command as a normal turn still uses the
canonical slash dispatcher and does not call an LLM.

## Draft workspaces

The gateway also owns each session's current workspace and the repo-scoped draft
list. That shared ownership is why a draft survives client reconnects and why TUI
and web surfaces see the same state. Draft creation, safety, persistence, slash
commands, and the workspace HTTP routes are documented in [Drafts](drafts.md).

## Resource limits and metrics

The gateway bounds expensive GraalPy work process-wide and evicts idle session
environments under heap or resident-memory pressure. Defaults favor stable
memory use; override them before starting the gateway when a larger host needs
more concurrency:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `VIS_GATEWAY_MAX_CONCURRENT_TURNS` | `50` | Simultaneously executing turns across all sessions |
| `VIS_GATEWAY_EVENT_RING_MAX` | `2000` | In-memory SSE replay events retained per session |
| `VIS_ENV_CACHE_MAX` | `8` | Resident idle session environments |
| `VIS_ENV_MAX_TURNS_PER_CTX` | `25` | Turns before a long-lived GraalPy context is recycled |
| `VIS_ENV_HEAP_BUDGET_MB` | `2048` | JVM heap pressure threshold |
| `VIS_ENV_RSS_BUDGET_MB` | `3072` | Whole-process RSS threshold, including native/GraalPy memory |

Values `<= 0` disable an eviction threshold; non-positive concurrency and event
ring values fall back to their defaults. `GET /metrics` exposes active/waiting
turns, queue depth, replay retention, environment-cache size, JVM heap/GC/thread
gauges, process RSS, and memory-pressure state in Prometheus format (or JSON
when requested with `Accept: application/json`).
