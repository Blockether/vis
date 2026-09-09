# Remote access and the Companion app

The terminal UI, Companion app and CLI connect to a **gateway**: a local HTTP
service that manages sessions, turns and the live event stream. Multiple clients
can connect to the same gateway and access the same sessions.

## Starting the gateway

`vis-agent` finds the gateway for the current database (`~/.vis/vis.mdb`) and
starts one in the background if needed. This managed gateway stops after the
last client disconnects and no turn is producing output.

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

The app does not require an account; it connects to your gateway. Send feedback
to `contact@blockether.com` or use the TestFlight feedback button.

### Pair a phone

A managed gateway listens on `127.0.0.1`, which a phone cannot access. To allow
remote connections, listen on all interfaces and print a pairing QR code:

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

`0.0.0.0` listens on all IPv4 interfaces, including public ones if present.
For private remote access, put both devices on a
[Tailscale](https://tailscale.com) tailnet. The pairing QR code prefers the
machine's `100.x` Tailscale address. To listen only on Tailscale, use
`--host 100.x.y.z` rather than `0.0.0.0`.

Keep token authentication enabled for remote connections.

## Using a remote gateway from the CLI

Two root flags send a whole invocation, including the terminal UI, to another
gateway:

```bash
vis-agent --gateway 10.0.0.5 --gateway-token "$TOKEN" tui
vis-agent --gateway https://gateway.example.com/vis --gateway-token "$TOKEN" gateway status
```

`--gateway` takes `HOST`, `HOST:PORT` or a full URL; a bare host means HTTP on
port `7890`. `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` set the same values for
a shell. An SSH tunnel can reach a loopback gateway. Supply a token if that
gateway requires one:

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

Set before starting the gateway:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `VIS_GATEWAY_MAX_CONCURRENT_TURNS` | `50` | Turns executing at once across all sessions |
| `VIS_GATEWAY_EVENT_RING_MAX` | `2000` | Events kept per session for SSE replay |
| `VIS_ENV_CACHE_MAX` | `8` | Idle session environments kept resident |
| `VIS_ENV_MAX_TURNS_PER_CTX` | `5` | Turns before a Python session is recycled |
| `VIS_ENV_RSS_BUDGET_MB` | `3072` native / `5120` JVM | Process memory threshold for eviction |

A value `<= 0` disables an eviction threshold.

## See also

- [Process jail and network policy](jail.md) — the egress proxy the gateway runs.
- [Configuration](configuration.md) — MCP servers and other gateway-managed settings.
- [Runtime distributions](distributions.md) — updating the gateway's runtime.
