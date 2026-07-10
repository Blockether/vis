# TODO — Gateway daemon: one engine, many clients

Turn "the second TUI shows *Cancelled by user* while the first still streams"
into "every TUI/web/whatever is a thin client of ONE long-lived gateway that
owns execution." This is the opencode model, adapted to vis (which already has
most of the pieces).

Status: **IMPLEMENTED — daemon client path landed.** Discovery/registry, HTTP/SSE client facade, TUI/web daemon routing, explicit `vis gateway status|stop`, and refcount shutdown are in code and verified below.

---

## The bug we're killing

Two engines share `~/.vis/vis.mdb`. Each is a full in-process runtime. On boot,
every engine runs a **global** orphan sweep that marks *every* `:running` turn
in the shared DB as interrupted/cancelled — with no check for whether another
live process owns it.

Trace (current code):
- `pre-resolve-session-id!` at TUI boot → `sweep-orphaned-running-turns!`
  **before** history rebuild — `screen.clj:2746`, `2752`.
- → `vis/gateway-reconcile-running-turns!` → `db-sweep-orphaned-running-turns!`
  — `gateway/state.clj:732`, `core.clj:500`.
- The sweep lists **all** `:running` turns DB-wide (NOT session-scoped) and
  unconditionally stamps `:status :interrupted` + `:prior-outcome :cancelled`
  — `loop.clj:8283-8301`.
- Second TUI rebuilds history, sees the now-`:cancelled` row with a blank
  answer, renders **"Cancelled by user."** — `chat.clj:432`, `442`.
- First TUI keeps streaming from its own in-memory engine (never re-reads DB),
  so it still shows live output.
- Gateway/web does the same sweep at `server.clj:709` — a web restart clobbers a
  live TUI turn too.

Root cause: the sweep assumes **single process** ("any `:running` row is my own
crash orphan"). With two live engines on one DB, the newcomer murders the
incumbent's in-flight turn.

---

## Why we're NOT doing the pid-lease (the option we almost built)

We scoped an option-1 "pid + boot-nonce lease": stamp owner pid/nonce on a turn,
sweep only reaps rows whose owner is provably dead. It correctly stops the false
cancel. **But it does not make the second TUI stream** — streaming comes from the
live engine/bus, not the DB. The lease only stops the murder; it doesn't deliver
the thing the user actually expected (both TUIs watching one live stream).

The lease survives, but shrunk: once there's ONE owner (the daemon), "any
`:running` row at daemon startup is my own orphan" becomes *true*, so the sweep
becomes correct single-process recovery. No cross-process lease needed.

---

## What opencode taught us (and where it's honest about limits)

- opencode's TUI is **not an engine** — it's a thin client of one server
  (HTTP + SSE `/event`). A second TUI connects to the **same** server and
  subscribes to the **same** event stream. There is only ever ONE owner of a
  session's execution → the clobber can't happen.
- "running/busy" is **in-memory server state, broadcast** — their `SessionStatus`
  (`idle|busy|retry`) is explicitly *not durable* ("empty after a process
  restart"). Nothing in the DB for a peer to read and clobber. **That is exactly
  our bug's root — we persist `:running` and everyone sweeps it.**
- DB recovery runs ONCE, at the single server's startup, only to finalize
  genuine orphans. Their single-process assumption holds *because there's one
  server*.
- Honest limit: plain `opencode` spawns the worker as the TUI's **child**, so
  TUI exit → `Instance.disposeAll()` aborts running sessions (their open issue
  **#8948**) — the exact "gateway owned by the TUI" trap. Their clean story is
  `opencode serve` (detached, standalone, idle-timeout disabled) + `attach`.
- #8948's proposed registry is the model we're copying:
  1. server writes `state/servers/<sha256(cwd)>.json` = `{pid, port, secret}`.
  2. booting TUI hashes cwd, reads registry: **alive → attach**; **stale →
     delete + spawn fresh**.
  3. **"if attached to a discovered server, do NOT send shutdown on exit."**

---

## Decisions (confirmed with user)

- **D1 — Every TUI always goes through the daemon.** No "sometimes in-process,
  sometimes remote." Uniform path (avoids the dual-path bug class). *(user:
  "The TUI always go over the fucking daemon")*
- **D2 — No idle timeout.** The daemon does not self-exit on a timer. *(user:
  "there is nothing like idle timeout")*
- **D3 — Daemon lifetime is decoupled from whoever started it.** If the TUI that
  spawned the daemon exits while another TUI is alive, the daemon **stays up**.
  *(user: "if the TUI that started the daemon exits and there is other TUI that
  still is alive the daemon is not stopped")*
- **D4 — The daemon is NOBODY's child.** Correction to the user's "owned by the
  TUI" phrasing: if the daemon were a child of TUI-A, killing TUI-A (or closing
  its terminal → SIGHUP) would take the daemon down under everyone else — the
  same failure relocated. So: spawn **detached** (double-fork / reparented to
  init/launchd), then connect to it as client #1 like everyone else. Nobody
  owns it.
- **D5 — Auto-start.** First channel that finds no gateway spawns it
  (detached) and connects. No manual `vis serve` required for the common path.

### Resolved decisions (locked — user said "do all", proceeding with the leans)

- **Q1 — Shutdown policy → (a) refcount.** Daemon exits only when **zero
  connected clients AND zero running turns**. A background/agent turn keeps it
  alive after every human closes their TUI. D2 (no idle-timeout) still holds —
  this is refcount, not a timer.
- **Q2 — Registry key → the DB path.** `sha256(canonical db-path)`. Two dirs
  sharing `--db` share one daemon; `:memory` never registers (see Q5).
- **Q3 — Race resolution → port-bind winner.** The process that wins `bind()` on
  the port IS the daemon; the loser gets `EADDRINUSE` and falls back to
  connecting. No election, no coordinator, no stale lock file. The registry
  `secret` guards pid reuse on attach.
- **Q4 — Command name → keep `vis serve`, add `vis gateway` alias + `vis gateway
  stop`/`status`.** `serve-main!` stays the entry (`server.clj:751`); the daemon
  self-spawns via the `serve` argv.
- **Q5 — Headless one-shot → stays in-process.** `vis "do X"` and `--db :memory`
  do NOT spawn/attach a daemon (a non-interactive single turn owns its own
  runtime). The daemon path is for interactive/persistent sessions only.

Build status: **implemented through refcount/status/stop/web-SSE cleanup.** Remaining validation that needs real multi-terminal/manual exercise: two live TUIs on one long provider stream and OAuth refresh race with real expired Anthropic credentials.

---

## What vis ALREADY has (so this is less work than it sounds)

- **`vis serve` is already the gateway daemon.** `gateway/server.clj` is a full
  HTTP/SSE server over the session/turn runtime: `POST submit-turn`,
  `cancel-turn`, sessions CRUD, and `/event` SSE doing **replay-then-live**
  (`sse-body`, `server.clj:108`). Registered at `main.clj:2839` ("Serve the
  session/turn runtime over HTTP + SSE (the gateway daemon)").
- **`gateway/state.clj` is the single "engine" façade.** Today CLI/TUI/web all
  call it **in-process** (`submit-turn-sync!`, `main.clj:656`). Each process runs
  its own copy — that's the bug.
- **`gateway/bus.clj`** — the *daemonless* alternative (per-session NDJSON
  journal `~/.vis/gateway/events/<sid>.ndjson` + per-process mirror tailer +
  `hydrate!`). Built to avoid a daemon. Our plan takes the OTHER fork (commit to
  the daemon) → this becomes redundant.

### The missing half
- **No vis-side HTTP/SSE CLIENT** that talks to a remote gateway. `vis serve` is
  a server nobody connects to yet. This client is the real work.

---

## Change set (build order)

1. **Launcher / discovery** — `discover-or-start!`:
   detached spawn (no parent) + registry file `~/.vis/gateway/registry/<hash>.edn`
   `{pid, port, host, secret}` + pid-liveness check + HTTP `/healthz` secret probe
   (guards pid reuse) + race-safe startup. **Implemented in `gateway/discovery.clj`; daemon self-registers from `gateway/server.clj`.**
2. **HTTP/SSE client adapter** — a `gateway.state`-shaped facade that speaks to
   the daemon instead of calling in-process: submit → POST, cancel → POST, watch
   → SSE `/event` replay-then-live. **Implemented in `gateway/client.clj`, including remote `events-since`, `current-seq`, and `subscribe!`.**
3. **Wire TUI (and web) to the client adapter** instead of `submit-turn-sync!`
   (`main.clj:656`) / the in-process gateway path. **TUI-facing public `vis/gateway-*` exports now point at `gateway.client`; `vis channels web` now attaches to the daemon instead of owning/stopping an in-process server.**
4. **Refcount-based shutdown** — track live client leases + SSE clients; daemon exits (and deletes its registry file) when client count and running-turn count are both zero. **Implemented in `gateway/server.clj`.**
5. **Exit-behavior discipline** — a client that *attached* to a discovered daemon
   NEVER sends shutdown on its own exit. **Implemented: client release only drops its lease; explicit shutdown is `vis gateway stop`.**

---

## Legacy to kill / demote (the "what the fuck the legacy" list)

- **The global orphan sweep as a cross-process cancel** — `screen.clj:2746/2752`,
  `gateway/state.clj:732`, `loop.clj:8283-8301`, `server.clj:709`. Once one daemon
  owns execution, DEMOTE this to daemon-startup-only recovery (finalize the
  daemon's OWN crash orphans). It must NOT run at TUI boot anymore (TUIs become
  clients, they have no orphans to sweep).
- **`:running` as a swept, peer-visible DB flag** — stop treating persisted
  `:running` as a signal any process may act on. Execution status becomes
  in-memory daemon state broadcast over SSE (opencode's `SessionStatus`). DB
  holds finished turns; a `:running` row is only meaningful to the daemon that
  wrote it, at its own startup.
- **In-process gateway path for interactive channels** — `submit-turn-sync!`
  in-process call from TUI/web (`main.clj:656`). Replaced by the client adapter.
  KEEP it for headless/`:memory` one-shot (pending Q5).
- **`gateway/bus.clj` (journal + mirror tailer + `hydrate!`)** — RETIRE, or demote
  to a documented "no-daemon fallback." The daemon's single-owner + SSE fan-out
  supersedes the whole cross-process mirror mechanism. Decide: delete now, or keep
  dormant behind a flag.
- **Per-process OAuth refresh single-flight** — `oauth.clj` `make-file-refresher`
  serializes token refresh with a **JVM `Object` monitor** (`oauth.clj:41`,
  `new-lock`) + a 15s `fresh-within?` reuse window (`oauth.clj:43`). Its docstring
  claims refresh is "serialized PER CREDENTIAL STORE" — but "store" = the
  in-memory refresher instance, ONE PER JVM. Two threads in one process serialize;
  **two TUI processes do NOT.** The auth file itself has no `FileLock`
  (`load-auth-file` is a plain `slurp`, `save-auth-file!` a plain `spit` —
  `provider_anthropic.clj:185,190`). Result with two live TUIs both hitting
  Anthropic: both 401 → both read the SAME refresh_token → both call
  `refresh-access-token!` → Anthropic **rotates** the refresh_token on the first
  exchange → the second process now holds a DEAD token → `invalid_grant` / 401
  **"Invalid authentication credentials"**. The `fresh-within?` window only masks
  it when A persists before B enters; concurrent entry gets no help (the JVM lock
  can't serialize across processes).
  **Gateway fix (same collapse-N-engines→1 payoff as streaming):** every token
  refresh funnels through the ONE daemon, so the EXISTING in-process single-flight
  lock (`refresher`) becomes globally correct again — not by adding cross-process
  coordination, but because there's no second process to race. **No `FileLock`
  needed once the daemon owns all sessions.** DO NOT build the interim blocking
  `FileChannel.lock` stopgap unless multi-TUI 401s block someone BEFORE the daemon
  ships — it's exactly the cross-process coordination the daemon makes unnecessary.
- **The pid/boot-nonce lease design** — DROP the cross-process version we scoped.
  Keep only trivial single-process startup recovery inside the daemon.
- **`chat.clj` "Cancelled by user." placeholder path from a swept row**
  (`chat.clj:432/442`) — stays as a renderer, but it should stop being *reachable*
  via a peer sweep once the sweep is demoted. Verify no client ever synthesizes it
  from another process's live turn.

---

## Verification performed

- REPL loaded changed gateway/TUI/web namespaces successfully.
- REPL smoke: started a gateway on a temp DB/port, `client/status` reported running, `client/create-session!` registered a client lease (`clients=1`), `client/release-session!` dropped the final lease, and `client/status` then reported stopped.
- REPL smoke: explicit `client/stop-daemon!` stopped a temp gateway and registry status reported stopped.
- CLI smoke: `clojure -M:vis gateway status --db <temp>` reports stopped without auto-starting.
- Focused tests: gateway discovery + pty bridge + voice core tests pass in the project REPL.
- `bb scripts/gen-audit.bb` regenerated `audit/README.md` for the current deps graph.
- `./verify.sh --quick` passes (format + clj-kondo).

Manual validation still worth doing before declaring the product behavior perfect:
- Two real TUIs on the same long provider turn: both stream the same live turn; no "Cancelled by user.".
- Kill the TUI that spawned the daemon while another TUI is alive: daemon and second TUI stream survive.
- OAuth refresh with real expired Anthropic creds from two TUIs: exactly one token exchange through the daemon, no `invalid_grant`/401.

---

## Notes / risks

- **SIGHUP on terminal close** is the classic detach failure — the spawned daemon
  must be fully reparented (double-fork or platform launcher), not just `&`.
  opencode has a pile of SIGHUP/SIGTERM PRs (#12718/#14505/#14548) from getting
  this wrong.
- **pid reuse**: the registry pid-liveness check can false-positive if the OS
  recycles a dead daemon's pid. Mitigate with the registry `secret` (probe the
  port + secret, not just pid-alive) before attaching.
- **Native image**: detached-spawn mechanism must work under the GraalVM binary
  (`bin/vis`), not just JVM dev — no `ProcessBuilder` assumptions that break in
  native.
