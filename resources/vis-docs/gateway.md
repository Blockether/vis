# Remote access and the Companion app

The terminal UI, the Companion app and the CLI all talk to one **gateway**: a
local HTTP service that owns sessions, turns and the live event stream. Many
clients can attach to the same gateway and see the same sessions.

## Starting the gateway

You normally do not start it. `vis-tui` or `vis-agent` finds the gateway for the
current database (`~/.vis/vis.mdb`) and starts one in the background when none
is running. That gateway stops itself once the last client disconnects and no
turn is still producing output.

```bash
vis-agent gateway status          # pid, url, database, clients, auth mode
vis-agent gateway stop            # stop it; escalates to SIGTERM/SIGKILL if needed
vis-agent gateway stop --if-idle  # stop only when nobody is using it
```

`vis-agent gateway start` runs in the **foreground** and does not stop on its
own. Use it for a supervised process — a systemd or launchd unit, a container,
a tmux pane — or background it yourself:

```bash
nohup vis-agent gateway start --host 0.0.0.0 --require-token > ~/.vis/gateway.out 2>&1 &
```

`vis-agent update` stops an idle managed gateway so the next client starts the
new build; `--keep-gateway` opts out.

## Connecting the Companion app

The Companion is one app for web, iOS and Android. Both stores are in open
testing:

| Platform | Link |
|---|---|
| iOS / iPadOS (TestFlight) | <https://testflight.apple.com/join/4anYT4Wk> |
| Android (Play open testing) | <https://play.google.com/apps/testing/com.blockether.viscompanion> |

The app has no account; it is a client for your gateway. Feedback goes to
`karol@blockether.com` or the TestFlight feedback button.

### Pair a phone

The gateway a client starts for you listens on `127.0.0.1`, which a phone
cannot reach. Start one on all interfaces with a token and print a pairing QR:

```bash
vis-agent gateway start --host 0.0.0.0 --require-token --pair
```

In the app, open **Machines → Add a machine** and scan the QR, or paste the
`vis://gateway?url=…&token=…` line printed under it. Both fill in the address
and token.

For a gateway that is already running, print the QR without restarting:

```bash
vis-agent gateway pair
```

If the running gateway is bound to `127.0.0.1`, this tells you to restart it
with the command above.

You can also type an address directly (LAN, Tailscale or a tunnel) and paste
the token from `~/.vis/gateway.token` on the gateway's machine. Each saved
machine shows a status dot: green online, red offline, amber wrong or missing
token.

### Access from anywhere with Tailscale

`0.0.0.0` exposes the gateway on your local network only. Put both devices on a
[Tailscale](https://tailscale.com) tailnet and start the gateway as above. The
QR prefers the machine's `100.x` Tailscale address, so the pairing keeps working
away from the LAN. To listen on Tailscale only, use `--host 100.x.y.z`.

Keep `--require-token` on for any non-local exposure.

## Using a remote gateway from the CLI

Two root flags send a whole invocation, including the terminal UI, to another
gateway:

```bash
vis-agent --gateway 10.0.0.5 --gateway-token "$TOKEN" tui
vis-agent --gateway https://gateway.example.com/vis --gateway-token "$TOKEN" gateway status
```

`--gateway` takes `HOST`, `HOST:PORT` or a full URL; a bare host means HTTP on
port `7890`. `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` set the same for a shell.
Through an SSH tunnel no token is needed:

```bash
ssh -N -L 7890:127.0.0.1:7890 you@10.0.0.5 &
vis-agent --gateway 127.0.0.1 tui
```

With `--gateway`, Vis never starts, restarts or stops that gateway, and an
unreachable target is an error rather than a fallback to a local one. The
`sessions` commands (`list`, `show`, `fork`, `delete`, `export`) always read the
local database.

## Tokens and HTTP 401

| Bind | Token |
|---|---|
| `127.0.0.1` (default) | off |
| any other host (`0.0.0.0`, LAN, Tailscale) | required |
| `127.0.0.1 --require-token` | required |

The token is created on first run in `~/.vis/gateway.token` (mode `600`);
`--token-file PATH` overrides it. Clients on the same machine pick it up
automatically. Remote clients receive it by pairing.

```text
vis-agent: fatal error - gateway HTTP 401
```

means the client reached a token-protected gateway without a valid token:
pair the remote client again, run on the gateway's machine, or restart the
gateway on loopback.

## Push notifications

The gateway sends one alert per finished turn to every registered device, with
the session title and ids only; the transcript never leaves the gateway. In the
app, open the gateway's **Settings → Notifications**, enable *Notify this
device* and use *Send a test*.

The store-distributed app needs no configuration: pushes go through a relay run
by the app's publisher, which never sees the alert content and stores nothing.
`VIS_PUSH_RELAY_URL` or `~/.vis/relay.edn` points one machine at a different
relay.

Direct APNs or FCM credentials only work for a Companion you build and sign
yourself; Apple and Google bind push credentials to the app build. In that
case give the gateway your key, in the macOS keychain or through environment
variables:

```bash
# iOS
security add-generic-password -U -s vis-apns -a key -w "$(cat AuthKey_ABCD123456.p8)"
security add-generic-password -U -s vis-apns -a key_id -w ABCD123456
security add-generic-password -U -s vis-apns -a team_id -w YOURTEAMID
security add-generic-password -U -s vis-apns -a topic -w com.example.yourapp
security add-generic-password -U -s vis-apns -a environment -w production
# or: VIS_APNS_KEY_PATH, VIS_APNS_KEY_ID, VIS_APNS_TEAM_ID, VIS_APNS_TOPIC, VIS_APNS_ENV

# Android
security add-generic-password -U -s vis-fcm -a service_account -w "$(cat sa.json)"
security add-generic-password -U -s vis-fcm -a project_id -w your-firebase-project
# or: VIS_FCM_SERVICE_ACCOUNT_PATH
```

A `.p8` in `~/.vis/apns/` with `apns.edn` beside it, or a service-account JSON
in `~/.vis/fcm/`, is picked up as well. `GET /v1/capabilities` reports push
readiness under `features.push`.

The web Companion uses browser Web Push with a key pair the gateway generates
under `~/.vis/web-push/`; `VIS_WEB_PUSH_SUBJECT` sets the contact.

Treat the APNs key, the FCM service account, the gateway token and
`~/.vis/devices.edn` as secrets. Bundle ids, team ids, project ids and
`google-services.json` are not.

## HTTP API

Every built-in route is described as OpenAPI 3.1, without a token:

```bash
curl -sS http://127.0.0.1:7890/openapi.json -o vis-gateway.json
```

Routes added by extensions are not included.

Gateway and clients each publish the protocol version they speak and the oldest
counterpart they serve. The gateway advertises it on `GET /healthz`,
`GET /v1/capabilities` and `GET /v1/admin/status`; a client sends
`X-Vis-Protocol`, `X-Vis-Min-Gateway-Protocol`, `X-Vis-Client` and
`X-Vis-Client-Version`. A client that is too old gets `HTTP 426`, and a client
facing a gateway that is too old shows a version-mismatch screen. The health,
capabilities, OpenAPI and docs routes stay open so the message can be read.

Other routes worth knowing:

- `GET /v1/events?sids=<sid>` — the session event stream (SSE), resumable with
  `Last-Event-ID`.
- `POST /v1/sessions/:sid/voice` — upload a recording; answers `202` with a
  job, whose progress streams from `…/voice/jobs/:job-id/events` as
  `voice.job` frames.
- `GET /v1/slashes` — the slash commands available to web clients.
- `GET`/`POST`/`DELETE /v1/devices` — registered push devices, tokens masked.
- `GET /metrics` — Prometheus metrics (or JSON with `Accept: application/json`).

## Python SDK

`blockether.vis.engine.GatewayClient` talks to a gateway you name by URL and
token. `LocalEngine` runs a Vis executable as a subprocess with no gateway at
all. See the [Python SDK](https://pypi.org/project/vis-agent/).

## Resource limits

Set before starting the gateway:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `VIS_GATEWAY_MAX_CONCURRENT_TURNS` | `50` | Turns executing at once across all sessions |
| `VIS_GATEWAY_EVENT_RING_MAX` | `2000` | Events kept per session for SSE replay |
| `VIS_ENV_CACHE_MAX` | `8` | Idle session environments kept resident |
| `VIS_ENV_MAX_TURNS_PER_CTX` | `25` | Turns before a Python session is recycled |
| `VIS_ENV_RSS_BUDGET_MB` | `3072` | Process memory threshold for eviction |

A value `<= 0` disables an eviction threshold.

## See also

- [Process jail and network policy](jail.md) — the egress proxy the gateway runs.
- [Configuration](configuration.md) — MCP servers and other gateway-managed settings.
- [Runtime distributions](distributions.md) — updating the gateway's runtime.
