# Gateway, pairing & remote access

Every vis channel talks to one long-lived **gateway daemon**: an HTTP + SSE
runtime that owns
the sessions, turns, and the live event bus. You rarely start it by hand; a
channel spawns it for you. This page explains its lifecycle, why
`vis gateway start` stays in the foreground, the token model (and the
`HTTP 401` you hit on `--host 0.0.0.0`), and how to pair a phone over LAN or
Tailscale.

## The gateway starts itself (in the background)

When you run a client such as:

```sh
vis tui
```

the client looks up the gateway registered for the current database
(`~/.vis/vis.mdb` by default). If none is alive it **spawns one, fully
detached** — on unix under `nohup … &`, reparented to init, with its
stdout/stderr captured to a per-database boot log under
`~/.vis/gateway/`. That daemon is *client-managed*: it self-reaps once the
last client disconnects. So the normal flow is just "start the TUI" — the
background gateway is automatic, and a herd of clients all attach to the
same one.

You do **not** need to run `vis gateway start` yourself for local use.

### Why `vis gateway start` does not go to the background

`vis gateway start` is deliberately a **foreground** daemon: it prints its
connection line and parks until you stop it with `Ctrl-C` / `SIGTERM`. It is
meant for running the gateway as a supervised, user-owned process (a
`systemd`/`launchd` unit, a container entrypoint, a `tmux` pane) — not for a
throwaway shell. A foreground `vis gateway start` is **not** refcounted, so
it will not self-reap when clients come and go.

To run it detached yourself, background it explicitly:

```sh
# quick-and-dirty
nohup vis gateway start --host 0.0.0.0 --require-token > ~/.vis/gateway.out 2>&1 &

# or let a client auto-spawn the managed background daemon for you
vis tui
```

Inspect and control the daemon:

```sh
vis gateway status     # pid, url, db, client count, auth mode
vis gateway stop       # ask the running daemon to exit
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
(`vis gateway start`).

## Pairing a phone (mobile companion)

Start the gateway on a reachable host and print a pairing QR:

```sh
vis gateway start --host 0.0.0.0 --require-token --pair
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
  token. Either tap **Scan QR** and point the camera at the QR from `vis gateway
  pair` (or `vis gateway start … --pair`), or paste the
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
vis gateway pair
```

It reads the gateway registered for the current DB and prints the same QR that
`--pair` prints at boot — no restart needed. Two guardrails:

- **No gateway running** → it tells you to start one:
  `vis gateway start --host 0.0.0.0 --require-token --pair`.
- **Running but loopback-bound** (the auto-spawned TUI daemon) → it refuses,
  because `127.0.0.1` is phone-local, and prints the exact restart to run:
  `vis gateway stop` then `vis gateway start --host 0.0.0.0 --require-token --pair`.

So: if you only ever ran `vis tui`, the daemon behind it is loopback
and cannot be paired as-is — stop it and restart the gateway reachable (above).
Once it is bound to `0.0.0.0` (or a Tailscale host), `vis gateway pair` prints
the QR on demand any time.

## Tailscale (access from anywhere)

`0.0.0.0` only exposes the gateway on the local network. To reach it from
your phone off-LAN, put both devices on a [Tailscale](https://tailscale.com)
tailnet and bind/advertise the machine's Tailscale IP (the `100.64.0.0/10`
range):

```sh
# with tailscale up on this machine
vis gateway start --host 0.0.0.0 --require-token --pair
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
