# Running a gateway

Run one gateway that the desktop app, the phone app, terminal clients, and your scripts share.
Your project files and tools stay on the computer that runs the gateway, also for a remote client.

## When to use

- **You want the same sessions on your phone, in the desktop app, and in the terminal.**
  [Start a gateway](#start-a-local-gateway) that all of them connect to.
- **The agent must run on a remote server while you work from a laptop or a phone.**
  [Connect from another machine](#connect-from-another-machine) through a VPN, an SSH tunnel, or
  HTTPS.
- **Scripts and SDK clients need a service that always runs.**
  [Keep it running on Linux](#keep-it-running-on-linux) under a service manager.
- **Your own program must call the gateway.** Read [Python SDK](python-sdk.md) or
  [HTTP API](http-api.md).
- **A client cannot connect, or a task makes no progress.** Read
  [Troubleshoot a connection](#troubleshoot-a-connection) and
  [Collect evidence when work stops progressing](#collect-evidence-when-work-stops-progressing).

For terminal use on one computer, skip this page. `vis-agent tui` starts a local gateway when none
runs, as [Runtime distributions](distributions.md#terminal-gateway-lifecycle) explains.

## Install the runtime

On the machine that runs the agent, install the native release:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

The installer puts `vis-agent` and its runtime files under `~/.local/bin`. Put that directory on
`PATH`, then run `vis-agent --version`. A native release does not need Java. A script that uses only
the Python SDK downloads Vis by itself, but a service needs this installation. [Runtime
distributions](distributions.md) lists the supported platforms, release tracks, and updates.

Run Vis one time as the account that owns the service. Configure a
[provider and model](configuration.md), sign in, and try a task in the project. The service needs
the configuration, the credentials, a writable state directory, and project access of that
account. A connection from a laptop does not copy the files or credentials of the laptop to the
server.

## Start a local gateway

Choose an unused port and start the gateway in a terminal:

```bash
vis-agent gateway start --host 127.0.0.1 --port 7890 --require-token
```

The command runs in the foreground until you stop it, so keep the terminal open. In another
terminal, run `vis-agent gateway status`. Then connect your program to `http://127.0.0.1:7890` with
the [Python SDK](python-sdk.md) or the [HTTP API](http-api.md). If a gateway already runs, check
its status and port before you start another. Do not stop a shared gateway to try an example.

`--require-token` turns on authentication, also on loopback. The default token file is
`~/.vis/gateway.token`, with owner-only permissions (mode `600`). `--token-file` selects another
file.

The token gives access to an agent that can run tools. Keep it out of source control, logs,
screenshots, and command arguments. The CLI uses the local token by itself.

| Started by | Lifetime |
| --- | --- |
| The terminal client | Until the last client disconnects and no work remains |
| `vis-agent desktop` | While the app runs. The app opened another way starts no gateway. |
| `vis-agent gateway start` | Until you stop it, also without clients. Use this mode under a service manager. |

### Stop or restart a gateway

A stop of a busy gateway interrupts its clients and its active work. To keep the work, first
[hand it over](#hand-over-work-before-a-planned-stop). Check the gateway before you stop it:

```bash
vis-agent gateway status          # show the address and connected clients
vis-agent gateway stop --if-idle  # stop only when nobody is using it
vis-agent gateway stop            # stop even if clients are connected
```

Without `--if-idle`, a stop can end the process with SIGTERM or SIGKILL. With `--if-idle`, the
gateway stops only when no session works. Stop a supervised service with its service manager. If you
do not, the restart policy of the service may start it again.

To change the listening address, stop the gateway, then start it with the new address.

### Hand over work before a planned stop

Drain a busy gateway before you stop or replace it on purpose. A drain keeps the running work and
the queued work for the next gateway. You cannot cancel a drain. Set up `vis_api` as [Authenticate
requests](http-api.md#authenticate-requests) shows, then drain the gateway:

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/admin/drain" \
  -H 'content-type: application/json' --data '{"timeout_ms": 60000, "grace_ms": 10000}'
```

During the drain, the gateway does these things:

- It starts no more turns. New messages wait in the queue for the next gateway.
- Each running turn stops at the end of its current step.
- After `timeout_ms`, the gateway stops the turns that still run, inside their step.
- The answer comes when all turns stopped, or at the latest `grace_ms` later.

The answer has three counts:

| Field | Turns that it counts |
| --- | --- |
| `parked` | The stopped turns |
| `forced` | The turns that `timeout_ms` stopped |
| `running` | The turns that did not stop in time |

Then stop the gateway and start the next one with the same Vis home directory. The next gateway
continues each stopped turn with the same turn ID, then runs the queue in its order, with no new
message from you.

A drain has these limits:

- A turn stopped inside a step can have run part of a tool call. The model gets a note to check the
  current state before it repeats a change. Vis does not repeat or undo the tool call.
- A turn in the `running` count also continues in the next gateway, as a turn stopped inside a step.
- A turn can continue in any number of gateways until it is older than the **Turn resume limit**
  setting, `turn_resume_limit`. The default is 8 hours. An older turn ends as interrupted.
- A turn that stops three gateways in a row without a drain also ends as interrupted.

A gateway that ends without a drain, for example after a crash, also keeps its running turns and
its queue. The next gateway continues each running turn as a turn stopped inside a step, then runs
the queue.

## Connect from another machine

For a remote client, use a trusted VPN, an SSH tunnel, or HTTPS with a trusted certificate. Do not
send a bearer token over plain HTTP on an untrusted network. When a tunnel or a reverse proxy is
the remote entry point, keep the gateway bound to loopback.

For example, forward a local port to a gateway on your server:

```bash
ssh -N -L 7891:127.0.0.1:7890 visgw@gateway.example.com
```

Keep the tunnel open and point the SDK at `http://127.0.0.1:7891`. The gateway still requires
authentication. For HTTPS with the Python SDK, use an origin such as `https://gateway.example.com`
with no path prefix. Your proxy must forward authorization headers. It must also stream server-sent
events without buffering and without a short idle timeout.

Give the token to your program through its secret configuration. A pairing link and a QR code also
contain credentials, so show them only in a private terminal. For the first connection of an app,
read [Pair a phone](quickstart.md#pair-a-phone).

### Access from anywhere with Tailscale

Put both devices on a [Tailscale](https://tailscale.com) tailnet. Vis finds the Tailscale address
of the computer by itself, so you do not enter it. Start the gateway with `--pair` and without
`--host`, then pair the app. The pairing link puts the Tailscale address before the local network
addresses.

The app prefers the Tailscale address to a local network address, because the Tailscale address
works on any network. A MagicDNS name that ends in `.ts.net` counts as a Tailscale address.

Without `--host`, or with `--host 0.0.0.0`, the gateway listens on all IPv4 interfaces, public ones
included. The pairing link then
includes the other addresses on which the gateway can answer. The app tries them in order, so it
can change networks without a new pairing link.

For private access only, bind to one private address. Keep token authentication on. A bearer token
controls access, but it does not encrypt HTTP.

### Choose a pairing address

If clients reach the gateway through a proxy, a forwarded port, or a public hostname, set
`--advertise` to that address:

```bash
vis-agent gateway start --host 0.0.0.0 --require-token --pair --advertise 10.0.0.5
vis-agent gateway pair --advertise https://gateway.example.com
```

`--advertise` changes the pairing link, not the listening address, so the address must still reach
the gateway. For access through a router, forward the port of the gateway and advertise the public
address.

To reuse the address, set `VIS_GATEWAY_ADVERTISE` or your [gateway pairing
address](configuration.md#gateway-pairing-address).

### Using a remote gateway from the CLI

To connect the terminal client to another computer, give the address and the token with these root
flags:

```bash
vis-agent --gateway 10.0.0.5 --gateway-token "$TOKEN" tui
vis-agent --gateway https://gateway.example.com/vis --gateway-token "$TOKEN" gateway status
```

`--gateway` accepts `HOST`, `HOST:PORT`, or a full URL. A bare host means HTTP on port `7890`.
Instead of the flags, you can set `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` in your shell. To reach
a local-only gateway through an SSH tunnel:

```bash
ssh -N -L 7890:127.0.0.1:7890 you@10.0.0.5 &
vis-agent --gateway 127.0.0.1 tui
```

Give a token if that gateway requires one. With `--gateway`, Vis never starts, restarts, or stops a
replacement gateway. A target that Vis cannot reach is an error, with no fallback to a local
gateway. The `sessions` commands `list`, `show`, `fork`, `delete`, and `export` still read the local
database.

## Keep it running on Linux

Let a service manager own a **foreground** gateway. Do not use a Python program that calls
`StdioEngineClient` again and again. `StdioEngineClient` is for a private child process with
temporary session history, not for a persistent HTTP service.

The systemd example below uses an account named `visgw`. Before you enable the unit, do these steps:

1. Create the account and install Vis for it.
2. Prepare `/srv/vis-project`.
3. Complete the provider setup as that account.
4. Make the account the owner of its state directory, and give it only the project permissions that
   it needs.
5. Change the paths to match your machine.

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
SuccessExitStatus=143
UMask=0077

[Install]
WantedBy=multi-user.target
```

On SIGTERM, the gateway exits with status 143, and `SuccessExitStatus=143` tells systemd that this
stop is not a failure.

Enable and start the unit:

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now vis-gateway.service
sudo systemctl status vis-gateway.service
sudo journalctl -u vis-gateway.service -n 50 --no-pager
```

The output shows an active service and a listening gateway, not a restart loop. To inspect the same
gateway, run `vis-agent gateway status` as the service account. Do not export `VIS_GATEWAY_URL` or
`VIS_GATEWAY_TOKEN` into the service. They select a remote target for clients, not the address of
the listener.

These settings apply to a service:

- **Pairing address.** Set `gateway: advertise:` in the configuration file that the service account
  reads. A service unit starts without your shell profile, so a `VIS_GATEWAY_ADVERTISE` from a login
  shell never reaches it. See [gateway pairing address](configuration.md#gateway-pairing-address).
- **Machine name.** Apps show the server address as the name of the machine. For a short name, set
  `VIS_MACHINE_NAME` in the service unit, or save a [machine name](configuration.md#machine-name).
- **Agent name.** To give the coding agent one name that no client can change, set `VIS_AGENT_NAME`
  in the service unit. See [One fixed name](configuration.md#one-fixed-name).

Keep the complete native bundle together. A copy of only `vis-agent-native` can start but cannot run
Python tools. A custom launcher must keep the bundle layout and the runtime environment.

## Operate a small server

- **Builds.** Use prebuilt native releases on a small VPS. Compile a native image on a larger
  builder for the target operating system and architecture.
- **Memory.** A native engine still needs memory for Python workers, extensions, and the commands
  that the agent starts. Start with low concurrency and monitor memory under your real workload.
  See [resource limits](#resource-limits).
- **State.** The `~/.vis` directory of the service account holds the configuration and the history.
  Protect its backups. Leave room for databases, attachments, logs, and project builds. Do not
  store it in a release directory that you replace.
- **Homes.** `VIS_HOME` controls the installation state of the launcher. It does not move all engine
  configuration. Use a separate OS account for the home and the credentials of a separate service.
- **Updates.** Before an update, read the [update behavior](distributions.md) and check for active
  work. `vis-agent update --keep-gateway` leaves the running gateway alone, and the new runtime
  starts after a planned restart. Restart in a maintenance window, not on each client connection.

### Resource limits

Set these environment variables before you start the gateway:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `VIS_GATEWAY_MAX_CONCURRENT_TURNS` | `50` | Turns executing at once across all sessions |
| `VIS_GATEWAY_EVENT_RING_MAX` | `2000` | Events kept per session for SSE replay |
| `VIS_ENV_CACHE_MAX` | `8` | Idle session environments kept resident |
| `VIS_ENV_IDLE_TTL_MS` | `1800000` | Idle milliseconds before a session's Python sandbox stops |
| `VIS_ENV_RSS_BUDGET_MB` | A third of RAM, at least `5120` and at most three quarters of RAM | Memory of the gateway and its sandboxes at which idle sandboxes stop, least recently used first |

For the last three variables, a value of `0` or less turns the limit off. For the first two, a
value of `0` or less selects the default.

## Troubleshoot a connection

| Symptom | Check |
| --- | --- |
| Connection refused | Service state, listener port, tunnel and firewall |
| Authentication fails | The token must belong to this gateway, and the client must receive it. Never print the token to debug. |
| Client reports an incompatible protocol, or an HTTP client receives `HTTP 426` | Update the SDK and gateway to compatible versions |
| Requests work but progress stalls | Proxy SSE buffering, idle timeouts and the client's transport timeout |
| Python tools fail after a manual install | The launcher, the embedded Python runtime, file permissions and the service environment |
| A session cannot find the project | The path exists and is accessible on the gateway machine |

### Tokens and HTTP 401

| Gateway address | Token required? |
| --- | --- |
| `127.0.0.1` (default) | No |
| Any other address (`0.0.0.0`, LAN, Tailscale) | Yes |
| `127.0.0.1 --require-token` | Yes |

In the saved machine list of the app, green means online, red means offline, and amber means a
wrong or missing token.

A remote client can receive the token through pairing. In the desktop app, enter the token when
you add a local gateway that requires one.

An `HTTP 401` error means that the client reached the gateway, but the token is missing or wrong.
Pair the client again, or check that you gave the token of the right gateway. Do not turn off
remote authentication to remove the error.

## Collect evidence when work stops progressing

Vis saves a JSON report before the gateway watchdog cancels a stalled turn or gives up on a
cancellation. A running local CLI client can also capture the JVM threads when the gateway stops
answering.

Vis writes each report under `~/.vis/logs/YYYY-MM-DD/gateway-hang-<id>/`, with the UTC date of the
capture. [Hang reports](logging.md#hang-reports) lists the contents, the collection limits, and the
client paths. [Rotation and retention](logging.md#rotation-and-retention) has the cleanup rules.
Review the files before you share them.

## See also

- To install Vis and connect an app for the first time, read [Quickstart](quickstart.md).
- To work with the sessions that every connected client shows, read [Sessions](sessions.md).
- To change the settings that the gateway reads, read [Configuration](configuration.md).
- To connect a script or run a private local agent, read [Python SDK](python-sdk.md).
- To authenticate requests from any language and read gateway errors, read [HTTP API](http-api.md).
- To add Java or Clojure capabilities to the engine, read
  [Native builds for JVM extensions](jvm-native-image.md).
- To limit what the tools of the service can access, read
  [Process jail and network policy](jail.md).
