# Running a gateway

Run a gateway when you want the Vis app, your scripts and other clients to share
one agent service. The gateway owns sessions and runs tools on its machine;
clients send requests and follow progress. You can run it in your terminal or
keep it running under a service manager.

## Install the runtime

On the machine that will run the agent, install the native release:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

The installer places `vis-agent` and its companion runtime files under
`~/.local/bin`. Put that directory on `PATH`, then check `vis-agent --version`.
Native releases do not require Java; the Python SDK package alone is not an
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

This runs in the foreground until you stop it; leave the terminal open. In
another terminal, run `vis-agent gateway status`, then connect your
[Python](python-sdk.md#connect-to-a-gateway-and-run-a-task) or
[JVM](jvm-sdk.md#connect-from-java) program to `http://127.0.0.1:7890`.
If a gateway is already running, check its status and port before starting
another; do not stop a shared gateway just to try an example.

`--require-token` enables authentication even on loopback. The default token
file is `~/.vis/gateway.token` and is restricted to its owner; `--token-file`
selects another location. Treat this token as access to an agent that can run
tools. Keep it out of source control, logs, screenshots and command arguments.
The CLI can use the local token automatically; an SDK client needs its documented
connection settings.

An automatically started gateway can exit when it has no clients or active
work. Explicit `gateway start` is different: it stays running without clients,
which is the mode to use under a supervisor.

## Keep it running on Linux

A service manager should own a **foreground** gateway, not a Python program
that repeatedly calls `LocalEngine`. `LocalEngine` is for a private child
process with temporary session history, not a persistent HTTP service.

The following systemd example uses an account named `visgw`. Before enabling
it, create that account, install Vis for it, prepare `/srv/vis-project` and
complete provider setup as that account. The account must own its state
directory and have only the project permissions it needs. Adapt paths to your
machine; installing a unit alone does not prepare these prerequisites.

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
Environment=VIS_HOME=/home/visgw/.vis
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

Keep the complete native bundle together. Its launcher sets up the Python
sidecar; copying only `vis-agent-native` can leave a process that starts but
cannot run Python tools. A custom launcher must preserve the bundle layout and
runtime environment. Prefer the supplied wrapper unless you maintain and test
that setup yourself.

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

Supply the token through your application's secret configuration. Pairing links
and QR codes also contain connection credentials: generate or view them only in
a private terminal, and do not use them as public examples. See
[remote app connections](index.md#pair-a-phone) for pairing the Vis app.

## Operate a small server

Use prebuilt native releases on a small VPS; compile native images on a larger
builder for the target operating system and architecture. A native engine does
not eliminate the memory used by Python workers, extensions or commands the
agent starts. Begin with modest concurrency and monitor memory under your real
workload; see [resource limits](index.md#resource-limits).

Keep the state directory persistent across service restarts and protect its
backups: it contains conversation history and configuration, including private
data. Leave room for databases, attachments, logs, runtime bundles and project
builds. Do not place persistent state inside a release directory you will replace.

Before an update, review the [update behavior](distributions.md) and check for
active work. `vis-agent update --keep-gateway` leaves the running gateway alone;
the new runtime is used after a planned restart. Restarting a service can
interrupt requests and tools, so do it in a maintenance window, not on every
client connection.

## Troubleshoot a connection

| Symptom | Check |
| --- | --- |
| Connection refused | Service state, listener port, tunnel and firewall |
| Authentication fails | The token belongs to this gateway and is passed to the client; never print it to debug |
| Client reports an incompatible protocol | Update the SDK and gateway to compatible versions |
| Requests work but progress stalls | Proxy SSE buffering, idle timeouts and the client's transport timeout |
| Python tools fail after a manual install | The launcher, Python sidecar, file permissions and service environment |
| A session cannot find the project | The path exists and is accessible on the gateway machine |

Stopping a shared gateway affects every client. `vis-agent gateway stop --if-idle`
requests an idle-only stop; `vis-agent gateway stop` can interrupt active work.
For a supervised service, use the service manager for an intentional stop so its
restart policy does not undo your action.

## See also

- [Python SDK](python-sdk.md) — connect a script or wrap an owned local agent.
- [Java and Clojure SDK](jvm-sdk.md) — connect a JVM application.
- [Building the native binary](jvm-native-image.md) — build and package a complete native runtime.
- [Process jail and network policy](jail.md) — limit what the service's tools can access.
