# Running a gateway

Keep a gateway available for desktop and phone apps, terminal clients or your own scripts.
Your project files and tools stay on the computer running it.

## When to use

- **You want to follow the same sessions from your phone, desktop app and
  terminal.** [Start a gateway](#start-a-local-gateway) that all of them connect to.
- **The agent should run on a remote server while you work from a laptop or phone.**
  [Connect from another machine](#connect-from-another-machine) through a VPN, an
  SSH tunnel or HTTPS.
- **Scripts and SDK clients need a service that is always running.** [Keep it
  running on Linux](#keep-it-running-on-linux) under a service manager.
- **A client cannot connect, or a task stops making progress.** See [Troubleshoot a
  connection](#troubleshoot-a-connection) and [Collect evidence when work stops
  progressing](#collect-evidence-when-work-stops-progressing).

For terminal use on one computer, you do not need to set this up: `vis-agent tui`
starts a local gateway when none is running. See [Runtime
distributions](distributions.md#terminal-gateway-lifecycle).

## Install the runtime

On the machine that will run the agent, install the native release:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

The installer places `vis-agent` and its companion runtime files under
`~/.local/bin`. Put that directory on `PATH`, then check `vis-agent --version`.
Native releases do not require Java. The Python SDK package alone is not an
engine installation. See
[Runtime distributions](distributions.md) for supported platforms, release
tracks and updates.

Run Vis interactively once as the account that will own the service. Configure
a [provider and model](configuration.md), complete any sign-in and try a task in
the intended project. The service needs that account's configuration,
credentials, writable state directory and access to the project. Connecting
from a laptop does not transfer the laptop's files or credentials to the server.

## Start a local gateway

Choose an unused port and start the gateway in a terminal:

```bash
vis-agent gateway start --host 127.0.0.1 --port 7890 --require-token
```

This runs in the foreground until you stop it. Leave the terminal open. In another terminal, run
`vis-agent gateway status`, then connect your
[Python](python-sdk.md#connect-to-a-gateway-and-run-a-task) or [JVM](jvm-sdk.md#connect-from-java)
program to `http://127.0.0.1:7890`. If a gateway is already running, check its status and port
before you start another. Do not stop a shared gateway only to try an example.

`--require-token` enables authentication, even on loopback. The default token file is
`~/.vis/gateway.token`, with owner-only permissions (mode `600`). `--token-file` selects another location.

This token gives access to an agent that can run tools. Keep it out of source control, logs,
screenshots and command arguments. The CLI can use the local token automatically. An SDK client
needs its documented connection settings.

The terminal client starts a managed gateway if needed. It stops after the last client disconnects and no work remains.
Opening the desktop app does not start a gateway.
Explicit `gateway start` stays running without clients. Use this mode under a service manager.

### Stop or restart a gateway

Check the gateway before stopping it:

```bash
vis-agent gateway status          # show the address and connected clients
vis-agent gateway stop --if-idle  # stop only when nobody is using it
vis-agent gateway stop            # stop even if clients are connected
```

Stopping a busy gateway interrupts its clients and active work.
An unconditional stop can escalate to SIGTERM/SIGKILL. Use `--if-idle` to avoid interrupting another session.
For a supervised service, use its service manager to stop it. Otherwise, its restart policy may start it again.

To change the listening address, stop the gateway before starting it with the new address.
Check for active work before any restart.

## Connect from another machine

For a remote client, use a trusted VPN, an SSH tunnel or HTTPS with a trusted
certificate. Do not send bearer tokens over plain HTTP on an untrusted network.
Keep the gateway bound to loopback when a tunnel or reverse proxy provides the
remote entry point.

For example, forward a local port to a gateway on your server:

```bash
ssh -N -L 7891:127.0.0.1:7890 visgw@gateway.example.com
```

Keep that tunnel open and point the SDK at `http://127.0.0.1:7891`. Authentication
is still required. For HTTPS, use an origin such as `https://gateway.example.com`
with no path prefix if you use the Python SDK. Your proxy must forward
authorization headers and let server-sent events stream without buffering or
short idle timeouts.

Supply the token through your application's secret configuration.
Pairing links and QR codes also contain credentials. View them only in a private terminal.
For the app's first connection, see [Pair a phone](index.md#pair-a-phone).

### Access from anywhere with Tailscale

Put both devices on a [Tailscale](https://tailscale.com) tailnet.
Start the gateway on the computer's Tailscale address, then pair the app.
The pairing link prefers that address over a local network address.

You can use `--host 0.0.0.0` to listen on all IPv4 interfaces, including public ones.
The pairing link then includes other addresses the gateway can answer on.
The app tries them in order, so it can change networks without a new pairing link.

If you need only private access, bind to one private address.
Keep token authentication enabled. A bearer token controls access, but it does not encrypt HTTP.

### Choose a pairing address

If you use a proxy, forwarded port or public hostname, set `--advertise` to its address:

```bash
vis-agent gateway start --host 0.0.0.0 --require-token --pair --advertise 10.0.0.5
vis-agent gateway pair --advertise https://gateway.example.com
```

`--advertise` accepts a host, `host:port` or a full URL.
It changes the pairing link, not the listening address. The address must still reach the gateway.
For access through a router, forward the gateway's port and advertise the public address.
Use a trusted VPN or HTTPS for remote connections.

The address you choose comes first in the link. Detected addresses follow as fallbacks.
To reuse it, set `VIS_GATEWAY_ADVERTISE` or your [gateway pairing address](configuration.md#gateway-pairing-address).
The flag takes priority, then the environment variable, then the configuration file.

### Using a remote gateway from the CLI

The terminal client can also connect to another computer. Supply its address
and token with these root flags:

```bash
vis-agent --gateway 10.0.0.5 --gateway-token "$TOKEN" tui
vis-agent --gateway https://gateway.example.com/vis --gateway-token "$TOKEN" gateway status
```

`--gateway` accepts `HOST`, `HOST:PORT` or a full URL. A bare host means HTTP on
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

## Keep it running on Linux

A service manager should own a **foreground** gateway, not a Python program
that repeatedly calls `LocalEngine`. `LocalEngine` is for a private child
process with temporary session history, not a persistent HTTP service.

The following systemd example uses an account named `visgw`. Before enabling
it, create that account, install Vis for it, prepare `/srv/vis-project` and
complete provider setup as that account. The account must own its state
directory and have only the project permissions it needs. Adapt paths to your
machine. Installing a unit alone does not prepare these prerequisites.

Save the unit as `/etc/systemd/system/vis-gateway.service`:

```ini
[Unit]
Description=Vis agent gateway
Wants=network-online.target
After=network-online.target

[Service]
Type=simple
User=visgw
Group=visgw
WorkingDirectory=/srv/vis-project
Environment=HOME=/home/visgw
Environment=PATH=/home/visgw/.local/bin:/usr/local/bin:/usr/bin:/bin
ExecStart=/home/visgw/.local/bin/vis-agent gateway start --host 127.0.0.1 --port 7890 --require-token
Restart=on-failure
RestartSec=5
TimeoutStopSec=60
UMask=0077

[Install]
WantedBy=multi-user.target
```

Enable it only when you are ready to start a persistent service:

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now vis-gateway.service
sudo systemctl status vis-gateway.service
sudo journalctl -u vis-gateway.service -n 50 --no-pager
```

You should see an active service and a listening gateway, not a repeating restart
loop. Run `vis-agent gateway status` as the service account to inspect the same
local gateway. Avoid exporting `VIS_GATEWAY_URL` or `VIS_GATEWAY_TOKEN` into the
service: those select a remote target for clients, not the listener's address.

Set `gateway: advertise:` in the configuration file that the service account reads. Then every
pairing link from the service uses the same address. A service unit starts without your shell
profile. So a `VIS_GATEWAY_ADVERTISE` that you export in a login shell never reaches it. See
[gateway pairing address](configuration.md#gateway-pairing-address).

Keep the complete native bundle together. Its launcher sets up the Python
sidecar. Copying only `vis-agent-native` can leave a process that starts but
cannot run Python tools. A custom launcher must preserve the bundle layout and
runtime environment. Prefer the supplied wrapper unless you maintain and test
that setup yourself.

## Operate a small server

Use prebuilt native releases on a small VPS. Compile native images on a larger
builder for the target operating system and architecture. A native engine does
not eliminate the memory used by Python workers, extensions or commands the
agent starts. Begin with modest concurrency and monitor memory under your real
workload. See [resource limits](#resource-limits).

The service account's `~/.vis` holds persistent configuration and history.
Protect its backups and leave room for databases, attachments, logs and project
builds. Do not store it in a replaceable release directory. `VIS_HOME` controls
launcher installation state. It does not relocate all engine configuration.
Use a separate OS account for a separate service's home and credentials.

Before an update, review the [update behavior](distributions.md) and check for
active work. `vis-agent update --keep-gateway` leaves the running gateway alone.
The new runtime is used after a planned restart. Restarting a service can
interrupt requests and tools, so do it in a maintenance window, not on every
client connection.

### Resource limits

Set these environment variables before starting the gateway:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `VIS_GATEWAY_MAX_CONCURRENT_TURNS` | `50` | Turns executing at once across all sessions |
| `VIS_GATEWAY_EVENT_RING_MAX` | `2000` | Events kept per session for SSE replay |
| `VIS_ENV_CACHE_MAX` | `8` | Idle session environments kept resident |
| `VIS_ENV_IDLE_TTL_MS` | `300000` | Idle milliseconds before a session's Python sandbox stops |
| `VIS_ENV_RSS_BUDGET_MB` | `3072` native / `5120` JVM | Process memory threshold for eviction |

A value `<= 0` disables an eviction threshold.

## HTTP API

The gateway serves its OpenAPI 3.1 schema without a token:

```bash
curl -sS http://127.0.0.1:7890/openapi.json -o vis-gateway.json
```

Use the schema for routes, request formats and responses. Protected routes
require the gateway token. An incompatible client receives `HTTP 426`.
Update the client or gateway to a compatible version.

For integrations, use the [Python SDK](python-sdk.md) or [Java and Clojure SDK](jvm-sdk.md).

## Troubleshoot a connection

| Symptom | Check |
| --- | --- |
| Connection refused | Service state, listener port, tunnel and firewall |
| Authentication fails | The token must belong to this gateway, and the client must receive it. Never print the token to debug. |
| Client reports an incompatible protocol | Update the SDK and gateway to compatible versions |
| Requests work but progress stalls | Proxy SSE buffering, idle timeouts and the client's transport timeout |
| Python tools fail after a manual install | The launcher, Python sidecar, file permissions and service environment |
| A session cannot find the project | The path exists and is accessible on the gateway machine |

### Tokens and HTTP 401

| Gateway address | Token required? |
| --- | --- |
| `127.0.0.1` (default) | No |
| Any other address (`0.0.0.0`, LAN, Tailscale) | Yes |
| `127.0.0.1 --require-token` | Yes |

In the app's saved machine list, green means online. Red means offline.
Amber means the token is wrong or missing.

Remote clients can receive the token through pairing. In the desktop app,
enter it when adding a token-protected local gateway.

An `HTTP 401` error means the gateway is reachable but the token is missing or incorrect.
Pair the client again or check that you supplied the token for the right gateway.
Do not disable remote authentication to work around the error.

## Collect evidence when work stops progressing

Vis saves a JSON report before the gateway watchdog cancels a stalled turn or
gives up on a cancellation. A running local CLI client can also capture JVM
threads when the gateway stops answering.

Reports live under `~/.vis/logs/YYYY-MM-DD/gateway-hang-<id>/`, using the UTC
capture date. See [Hang reports](logging.md#hang-reports) for contents, collection
limits and client-side paths, and [retention](logging.md#rotation-and-retention)
for cleanup rules. Review the files before sharing them.

## See also

- [Getting started](index.md) — install Vis and connect an app for the first time.
- [Python SDK](python-sdk.md) — connect a script or wrap an owned local agent.
- [Java and Clojure SDK](jvm-sdk.md) — connect a JVM application.
- [Native builds for JVM extensions](jvm-native-image.md) — only when adding Java/Clojure capabilities to the engine.
- [Process jail and network policy](jail.md) — limit what the service's tools can access.
