# Desktop and mobile apps

Use Vis from your desktop or phone without starting a separate conversation.
The apps connect to a **gateway**, the service running Vis on the computer where
your projects live. Your files, commands and sessions stay on that computer;
you choose which device to use to follow the work.

## Connecting the Companion app

Download an app for your device. Desktop releases are stable; both mobile stores
currently offer public testing without an invitation.

| Platform | Download | What to choose |
| --- | --- | --- |
| macOS | [Latest desktop release](https://github.com/Blockether/vis/releases/latest) | Universal `.dmg` for Apple silicon or Intel |
| Linux | [Latest desktop release](https://github.com/Blockether/vis/releases/latest) | `.AppImage` for x64 or ARM64 |
| iPhone / iPad | [TestFlight](https://testflight.apple.com/join/4anYT4Wk) | iOS / iPadOS public beta |
| Android | [Google Play beta](https://play.google.com/apps/testing/com.blockether.viscompanion) | Open testing |

On macOS or Linux, `vis-agent desktop --track release` can download and open the
app for you. See [Desktop setup](distributions.md#open-the-desktop-app) for
updates, requirements and source builds. Windows is not a desktop target.

The app does not install or start the engine. If you have not installed Vis on
the computer that will run your work, start with [Getting started](index.md#install).
You do not need a separate Vis account to connect an app. Model providers may
require their own account or API key.

### Connect the desktop app

For an app and gateway on the same computer, use the local address
`http://127.0.0.1:7890`. If a gateway is already running, check its address with
`vis-agent gateway status` and reuse it. Otherwise, start one:

```bash
vis-agent gateway start --host 127.0.0.1
```

Keep that terminal open: this command runs the gateway in the foreground.
Open the desktop app, or use another terminal to run:

```bash
vis-agent desktop --track release
```

In **Add a machine**, paste the gateway's address (`http://127.0.0.1:7890` for
the command above). Leave the bearer token empty for the default local gateway;
if you enabled token authentication, supply its token. Once connected, open
your project and start or resume a session. The
[first-session guide](index.md#first-session) covers choosing a model and a task.

An Intel Mac can use the desktop app with a remote gateway, or run a local
engine from the [JVM source distribution](distributions.md). There is no native
Intel macOS engine bundle.

For a gateway on another computer, follow the phone pairing steps below and
paste the pairing link instead of scanning its QR code. `127.0.0.1` always means
the device you are using; it cannot reach a different computer.

### Pair a phone

Your phone needs an address it can reach over your local network or a private
VPN such as Tailscale. A local-only gateway on `127.0.0.1` is not reachable from
your phone. Keep token authentication enabled for remote access. The pairing
link and QR code contain that token: treat them like a password and do not share
them publicly. Use a trusted network, VPN or HTTPS for remote connections.

On the computer running Vis, start a gateway on its network address. Replace
`10.0.0.5` with that computer's address:

```bash
vis-agent gateway start --host 10.0.0.5 --require-token --pair
```

In the app, open **Add a machine** and scan the QR, or paste the
`vis://gateway?url=…&token=…` link printed under it. Both fill in the address and
token. Once connected, you can open the same projects and sessions as in the
terminal or desktop app.

For a gateway that is already running, print a new pairing QR without restarting:

```bash
vis-agent gateway pair
```

If that gateway is bound to `127.0.0.1`, the command asks you to restart with a
reachable address. Check that other clients and sessions can be interrupted
before stopping it; see [Starting the gateway](#starting-the-gateway).

You can also type a reachable address and supply the token from
`~/.vis/gateway.token` on the gateway's computer. Each saved machine shows its
connection state: green online, red offline, amber wrong or missing token.

### Access from anywhere with Tailscale

Put both devices on a [Tailscale](https://tailscale.com) tailnet to reach your
gateway away from the local network. Listen on the computer's Tailscale address,
then pair the app. The pairing QR prefers the machine's `100.x` Tailscale address.

You can use `--host 0.0.0.0` to listen on all IPv4 interfaces, but that includes
public interfaces if present. Bind to a specific private address when you only
need private access. A bearer token controls access; it does not encrypt HTTP.

## Starting the gateway

The terminal client finds a local gateway and starts one in the background if
needed. This managed gateway stops after the last client disconnects and no
work remains. Opening the desktop app does not start a gateway.

```bash
vis-agent gateway status          # show the address and connected clients
vis-agent gateway stop --if-idle  # stop only when nobody is using it
vis-agent gateway stop            # stop even if clients are connected
```

Stopping a busy gateway interrupts its clients and work. The unconditional stop
can escalate to SIGTERM/SIGKILL. Use `--if-idle` when you do not want to interrupt
another session.

`vis-agent gateway start` runs in the foreground and does not stop on its own.
For a gateway you want to keep available, run it under a process supervisor
such as systemd or launchd, or in a terminal multiplexer such as tmux.

`vis-agent update` stops an idle managed gateway so the next client starts the
new build. Use `--keep-gateway` if you want to leave it running.

## Using a remote gateway from the CLI

The terminal client can also connect to another computer. Supply its address
and token with these root flags:

```bash
vis-agent --gateway 10.0.0.5 --gateway-token "$TOKEN" tui
vis-agent --gateway https://gateway.example.com/vis --gateway-token "$TOKEN" gateway status
```

`--gateway` accepts `HOST`, `HOST:PORT` or a full URL; a bare host means HTTP on
port `7890`. You can instead set `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` in
your shell. To reach a local-only gateway through an SSH tunnel:

```bash
ssh -N -L 7890:127.0.0.1:7890 you@10.0.0.5 &
vis-agent --gateway 127.0.0.1 tui
```

Supply a token if that gateway requires one. With `--gateway`, Vis never starts,
restarts or stops a replacement gateway. An unreachable target is an error,
not a fallback to a local one. The `sessions` commands (`list`, `show`, `fork`,
`delete`, `export`) still read the local database.

## Tokens and HTTP 401

| Gateway address | Token required? |
| --- | --- |
| `127.0.0.1` (default) | No |
| Any other address (`0.0.0.0`, LAN, Tailscale) | Yes |
| `127.0.0.1 --require-token` | Yes |

Vis creates the token in `~/.vis/gateway.token` with owner-only permissions
(mode `600`). `--token-file PATH` chooses a different file. Local CLI clients
read it automatically; remote clients receive it through pairing. In the
desktop app, supply it when adding a token-protected local gateway.

An `HTTP 401` error means the gateway is reachable but the token is missing or
incorrect. Pair the client again or check that you supplied the token for the
right gateway. Do not disable remote authentication to work around the error.

## HTTP API

The gateway serves its OpenAPI 3.1 schema without a token:

```bash
curl -sS http://127.0.0.1:7890/openapi.json -o vis-gateway.json
```

Use the schema for routes, request formats and responses. Protected routes
require the gateway token. An incompatible client receives `HTTP 426`;
update the client or gateway to a compatible version.

## Python SDK

`blockether.vis.engine.GatewayClient` connects to a gateway by URL and token.
`LocalEngine` runs a Vis executable as a subprocess without a gateway. See
the [Python SDK](https://pypi.org/project/vis-agent/).

## Resource limits

Set these environment variables before starting the gateway:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `VIS_GATEWAY_MAX_CONCURRENT_TURNS` | `50` | Turns executing at once across all sessions |
| `VIS_GATEWAY_EVENT_RING_MAX` | `2000` | Events kept per session for SSE replay |
| `VIS_ENV_CACHE_MAX` | `8` | Idle session environments kept resident |
| `VIS_ENV_MAX_TURNS_PER_CTX` | `5` | Turns before a Python session is recycled |
| `VIS_ENV_RSS_BUDGET_MB` | `3072` native / `5120` JVM | Process memory threshold for eviction |

A value `<= 0` disables an eviction threshold.

## See also

- [Getting started](index.md) — install the engine and try your first task.
- [Runtime distributions](distributions.md) — update the engine or desktop app.
- [Configuration](configuration.md) — choose providers and gateway settings.
- [Process jail and network policy](jail.md) — control access to files and the network.
