# Running a gateway

Run a gateway when you want the Vis app, your scripts and other clients to share
one agent service. The gateway owns sessions and runs tools on its machine.
Clients send requests and follow progress. You can run it in your terminal or
keep it running as a daemon under a service manager such as systemd.

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
`~/.vis/gateway.token`, and only its owner can read it. `--token-file` selects another location.

This token gives access to an agent that can run tools. Keep it out of source control, logs,
screenshots and command arguments. The CLI can use the local token automatically. An SDK client
needs its documented connection settings.

An automatically started gateway can exit when it has no clients or active
work. Explicit `gateway start` is different: it stays running without clients,
which is the mode to use under a supervisor.

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

Supply the token through the secret configuration of your application. Pairing links and QR codes
also contain connection credentials. Create or view them only in a private terminal, and do not use
them as public examples. To pair the Vis app, see [remote app connections](index.md#pair-a-phone).

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
workload. See [resource limits](index.md#resource-limits).

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

## Troubleshoot a connection

| Symptom | Check |
| --- | --- |
| Connection refused | Service state, listener port, tunnel and firewall |
| Authentication fails | The token must belong to this gateway, and the client must receive it. Never print the token to debug. |
| Client reports an incompatible protocol | Update the SDK and gateway to compatible versions |
| Requests work but progress stalls | Proxy SSE buffering, idle timeouts and the client's transport timeout |
| Python tools fail after a manual install | The launcher, Python sidecar, file permissions and service environment |
| A session cannot find the project | The path exists and is accessible on the gateway machine |

Stopping a shared gateway affects every client. `vis-agent gateway stop --if-idle` stops the gateway
only when it is idle. `vis-agent gateway stop` can interrupt active work. To stop a supervised
service, use the service manager. Then its restart policy does not undo your action.

## Collect evidence when work stops progressing

Vis saves a JSON report before the gateway watchdog cancels a stalled turn or
gives up on a cancellation. A running local CLI client can also capture JVM
threads when the gateway stops answering.

Reports live under `~/.vis/logs/YYYY-MM-DD/gateway-hang-<id>/`, using the UTC
capture date. See [Hang reports](logging.md#hang-reports) for contents, collection
limits and client-side paths, and [retention](logging.md#rotation-and-retention)
for cleanup rules. Review the files before sharing them.

## See also

- [Python SDK](python-sdk.md) — connect a script or wrap an owned local agent.
- [Java and Clojure SDK](jvm-sdk.md) — connect a JVM application.
- [Native builds for JVM extensions](jvm-native-image.md) — only when adding Java/Clojure capabilities to the engine.
- [Process jail and network policy](jail.md) — limit what the service's tools can access.
