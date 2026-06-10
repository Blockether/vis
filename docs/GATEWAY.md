# Vis Gateway — HTTP/WS API

> Status: **design spec** (not yet implemented). This is the canonical
> contract the `:api` channel and every client (Desktop, Web replay,
> mission-control, CI, IDE) build against.

## 1. What this is

The Gateway is a **channel whose surface is HTTP + WebSocket instead of a
terminal**. It is the front door to a fleet of **stateful agent sessions** —
not a proxy for tokens.

A token gateway (Hermes / LiteLLM / OpenRouter) exposes *chat completions*:
stateless, request in, tokens out. svar — the router wired into
`internal/loop.clj` — already does that *internally*. Rebuilding it at the
edge throws away everything Vis is.

The Gateway's unit is a **Turn against a stateful Session bound to a
Workspace**. What it exposes that a completion gateway structurally cannot:

- a **mind** you can read (`context`, facts, tasks) and *write* (pin a fact,
  approve a plan),
- an **execution stream** — the Python the model ran this turn, with
  `rg` → hits, `patch` → diff, `clj_eval` → REPL output inline,
- a session that **persists, resumes, and can block for human approval**
  mid-turn.

Building this is also what turns the Desktop two-pane instrument from a
fixture into a live tool: the Gateway **is** the live-data render contract.
Desktop, the scrubbable permalink, and mission-control all consume the
endpoints below.

### Scope (locked)

**Single node, single user (single tenant), local daemon.** Out of scope —
on the record:

- ❌ OpenAI-compatible `/chat/completions` shim
- ❌ Public *frozen* event-schema versioning (an **internal** schema doc — §8
  — stays; the daemon and the Desktop app are two processes that must agree)
- ❌ Multi-tenant auth / per-tenant keys / per-tenant budgets
- ❌ Multi-node sandbox affinity (leases, consistent hashing, cross-node
  rehydrate)

Because there is only ever **one node**, the hardest problem — sandbox
affinity — does not exist: one process owns every live session, so a turn
always lands on the process holding its GraalPy context.

---

## 2. Process model

```
vis serve [--port 7890] [--host 127.0.0.1] [--token-file ~/.vis/gateway.token]
```

Registers as the `:api` channel (`:channel/cmd "serve"`,
`:channel/owns-tty? false`). Boots an HTTP/1.1 + WebSocket server. No
controlling terminal; logs to `~/.vis/vis.log`.

### SessionManager

One process-global component owns the live fleet:

```
session-id (UUID)
  └─ { :environment   <RLM env: GraalPy sandbox + :ctx-atom + router>
       :lock          <ReentrantLock — one turn per session, already in send!>
       :cancellation  <cancellation token>
       :current-turn  <turn-id | nil>
       :event-log     <append-only, seq-indexed, ring-buffered>
       :last-active   <epoch-ms> }
```

- **One turn per session.** Reuses the existing `send!` `ReentrantLock`. A
  second `POST /turns` while a turn runs → `409 turn_in_progress`.
- **Idle eviction.** After `idle-ttl` (default 15 min) with no turn, persist
  state and dispose the sandbox (`close!` semantics). The session row stays in
  SQLite. *This is single-node memory management — not the cluster problem.*
- **Rehydrate on demand.** Next `POST /turns` for an evicted session
  reconstructs the environment from SQLite (`env-for` / `ensure-env!`) before
  running. Cold-start cost is the GraalPy context spin-up; report it in
  `turn.started.meta.cold_start_ms`.
- **Durability.** On boot, reconcile SQLite: any turn left `running` is marked
  `interrupted`; clients see a clean terminal status, never a zombie.
- **Event log.** Every channel-event for a session is appended with a
  monotonic `seq`. Backs resumable streaming (§6.3). Ring-buffered per session
  (default 10 000 events); older events are durable in the transcript.

### Engine mapping

| Gateway concept      | Engine surface (`vis.core` / `internal.loop`)                |
| -------------------- | ------------------------------------------------------------ |
| create session       | `create! :api {…}` → `{:id :channel :title :workspace-id}`   |
| submit turn          | `send! id [(svar/user text)] opts` (async, on a worker)      |
| turn result          | `turn!` return `{:trace :tokens :cost :confidence :status}`  |
| live stream          | `channel-events/{add-listener!, publish!}` for `:api`        |
| the mind             | env `:ctx-atom` → `:session/{facts,tasks,utilization,…}`     |
| cancel               | `cancellation/cancel!` on the session token                  |
| close / delete       | `close!` / `delete!`                                         |

---

## 3. Authentication

Single static bearer token, minted on first `vis serve` to
`~/.vis/gateway.token` (mode 600). Every request except `/healthz`:

```
Authorization: Bearer <token>
```

Missing/wrong → `401 unauthorized`. Bind to `127.0.0.1` by default; binding
to a non-loopback host requires `--host` explicitly (the daemon logs a
warning). No cookies, no sessions-of-auth, no refresh — it is one local user.

---

## 4. Conventions

- **JSON, snake_case keys** everywhere — matching the GraalPy boundary
  (`py_key->clj` keeps snake_case). Engine kebab/namespaced keys are mapped to
  snake_case on the wire (`:session/utilization` → `utilization`).
- **IDs**: session/turn ids are UUID strings. Event `seq` is a per-session
  monotonic integer starting at 1.
- **Times**: epoch milliseconds (integer) as `*_at` / `*_ms`.
- **Idempotency**: `POST /turns` accepts `idempotency_key`; a repeat returns
  the **same** turn, never double-runs the agent. Keys are unique per session,
  retained 24 h.
- **Pagination**: list endpoints take `?limit=` (default 50, max 200) and
  `?cursor=` (opaque); responses carry `next_cursor` (null at end).
- **Versioning**: all routes under `/v1`. The event schema (§8) carries
  `schema: 1`.
- **Errors**: §9.

---

## 5. Resource model / route table

```
# Sessions ───────────────────────────────────────────────
POST   /v1/sessions                      create a session
GET    /v1/sessions                      list sessions (mission-control)
GET    /v1/sessions/:sid                 session soul
PATCH  /v1/sessions/:sid                 set title
DELETE /v1/sessions/:sid                 close + delete

# Turns ──────────────────────────────────────────────────
POST   /v1/sessions/:sid/turns           submit a turn (async)
GET    /v1/sessions/:sid/turns           list turns
GET    /v1/sessions/:sid/turns/:tid      turn status + final answer
POST   /v1/sessions/:sid/turns/:tid/cancel    cancel a running turn
POST   /v1/sessions/:sid/turns/:tid/approve   resolve a candidate proposal-stop

# Live stream ─────────────────────────────────────────────
GET    /v1/sessions/:sid/events          SSE stream  (?cursor= | Last-Event-ID)
GET    /v1/sessions/:sid/events/ws       WebSocket stream (same payloads)

# The Mind ────────────────────────────────────────────────
GET    /v1/sessions/:sid/mind            context snapshot + facts + tasks + utilization
GET    /v1/sessions/:sid/mind/facts      durable facts
POST   /v1/sessions/:sid/mind/facts      pin a fact (human co-edit)
GET    /v1/sessions/:sid/mind/tasks      task algebra
GET    /v1/sessions/:sid/transcript      structured turns/iterations (replay/permalink)

# Process ─────────────────────────────────────────────────
GET    /v1/models                        resolvable models/providers
GET    /metrics                          tokens/cost/latency (Prometheus + JSON)
GET    /healthz                          liveness (no auth)
GET    /readyz                           readiness
```

---

## 6. Endpoints

### 6.1 Sessions

#### `POST /v1/sessions`

Request:

```json
{
  "title": "wire the done-gate",        // optional
  "workspace_id": "ws_…",               // optional; auto-minted trunk if absent
  "model": "claude-opus-4-8",           // optional; falls back to config default
  "external_id": "desktop:tab-3"        // optional channel-specific id
}
```

Response `201`:

```json
{
  "id": "8d6a0a1c-…",
  "channel": "api",
  "title": "wire the done-gate",
  "workspace_id": "ws_…",
  "model": "claude-opus-4-8",
  "created_at": 1749556800000
}
```

#### `GET /v1/sessions/:sid` — session soul

```json
{
  "id": "8d6a0a1c-…",
  "channel": "api",
  "title": "wire the done-gate",
  "model": "claude-opus-4-8",
  "workspace_id": "ws_…",
  "status": "idle",                     // idle | running | suspended | interrupted
  "current_turn_id": null,
  "utilization": { "pct_of_limit": 34, "tokens": 41200, "limit": 120000 },
  "created_at": 1749556800000,
  "last_active_at": 1749560000000
}
```

#### `GET /v1/sessions` — list (mission-control)

`?status=running,suspended` filter optional. Returns
`{ "sessions": [<soul>…], "next_cursor": null }`. This + `/events` fan-in **is**
mission-control.

#### `PATCH /v1/sessions/:sid`

`{ "title": "new title" }` → `200` updated soul. (Maps to `set-title!`.)

#### `DELETE /v1/sessions/:sid`

`close!` + `delete!`. `204`. Idempotent — deleting an unknown id is `204`.

---

### 6.2 Turns

#### `POST /v1/sessions/:sid/turns` — submit (async)

A turn is long-running; this **never blocks** on completion. It enqueues onto
a worker, returns immediately, and the caller follows `/events`.

Request:

```json
{
  "request": "Add a done-gate that forces :evidence on completed plan steps.",
  "idempotency_key": "desktop:c8df-0007",   // optional, recommended
  "model": "claude-opus-4-8",               // optional per-turn override
  "reasoning_default": "high",              // optional: low|medium|high
  "images": [ { "b64": "…", "mime": "image/png" } ]   // optional, multimodal
}
```

Response `202`:

```json
{ "turn_id": "t_3f…", "session_id": "8d6a0a1c-…", "status": "running" }
```

`409 turn_in_progress` if a turn is already running for this session. A repeat
with a seen `idempotency_key` returns the original turn with `200`.

#### `GET /v1/sessions/:sid/turns/:tid` — status + answer

Mirrors the `turn!` return map (§ `internal/loop.clj`):

```json
{
  "turn_id": "t_3f…",
  "session_id": "8d6a0a1c-…",
  "status": "completed",            // running | completed | failed | cancelled | suspended
  "request": "Add a done-gate…",
  "answer": "Done. The gate now rejects a :done step lacking :evidence …",  // VERBATIM done() markdown (source of truth); null until completed
  "answer_ir": null,                // optional: canonical [:ir …] as JSON when ?include=answer_ir
  "iteration_count": 7,
  "tokens": { "input": 38120, "output": 4210, "total": 42330 },
  "cost":   { "input_cost": 0.19, "output_cost": 0.31, "total_cost": 0.50 },
  "confidence": "high",             // high | medium | low
  "reasoning": "Located the gate in ctx_engine, added the evidence check…",
  "duration_ms": 81200,
  "started_at": 1749560000000,
  "finished_at": 1749560081200
}
```

**Answer serialization (why no renderer).** `done("""…""")` yields
`{:answer "<markdown>"}`; that markdown is the engine's **source of truth**
(`:answer-markdown`, "no fallback paths" — `render.clj`). The canonical
`[:ir …]` is *derived from* it (`markdown->ir`), not the reverse. The TUI and
Telegram channels register a `:channel/messages-renderer-fn` to flatten that IR
into a **display** encoding (ANSI cells / Telegram-HTML) — both lossy. The
gateway is a **transport**, so it does the opposite: it serves the markdown
**verbatim** as `answer`, and exposes the structured `answer_ir` (canonical IR
as JSON) on request for clients that want to paint rich blocks themselves. The
client renders; the gateway never flattens. (Hence the descriptor in §10 omits
`:channel/messages-renderer-fn`.)

`GET /v1/sessions/:sid/turns` lists turn summaries (no `trace`), newest first.

#### `POST /v1/sessions/:sid/turns/:tid/cancel`

Fires the session cancellation token. `202 { "status": "cancelling" }`. The
turn lands on `cancelled` within one iteration and emits `turn.failed`
(`status: cancelled`).

#### `POST /v1/sessions/:sid/turns/:tid/approve` — resolve a candidate

When the agent proposes a `candidate` plan it **suspends** (status
`suspended`) and emits `candidate.proposed`. The human resolves it:

```json
{ "decision": "approve" }                       // approve | reject | edit
// edit form:
{ "decision": "edit", "plan": [ { "title": "…", "status": "pending" }, … ] }
```

`approve` resumes the turn; `reject` ends it (`cancelled` with a reason);
`edit` replaces the plan then resumes. `409 not_suspended` if the turn is not
parked. Suspended turns survive a daemon restart (it is persisted state).

---

### 6.3 Live stream — `GET /v1/sessions/:sid/events`

Server-Sent Events. Lifts the process-local `channel-events` bus (§
`internal/channel_events.clj`) onto the wire. **Ordered and resumable**:
reconnect with `Last-Event-ID: <seq>` (or `?cursor=<seq>`) and miss nothing.

```
GET /v1/sessions/8d6a0a1c-…/events
Accept: text/event-stream
Last-Event-ID: 412
```

```
id: 413
event: iteration.started
data: {"schema":1,"seq":413,"ts":1749560002100,"session_id":"8d6a0a1c-…","turn_id":"t_3f…","n":3}

id: 414
event: block.output
data: {"schema":1,"seq":414,"ts":1749560002400,"session_id":"8d6a0a1c-…","turn_id":"t_3f…","n":3,"block_id":1,"call":"rg","result":{"kind":"matches","hits":[{"path":"src/…/ctx_engine.clj","line":418,"text":"(defn apply-done! …"}]}}

id: 415
event: mind.updated
data: {"schema":1,"seq":415,"ts":1749560002450,"session_id":"8d6a0a1c-…","utilization":{"pct_of_limit":71},"facts":[{"key":"done_gate","op":"set"}]}
```

A heartbeat comment (`: ping`) is sent every 15 s to keep the connection
warm. `/events/ws` carries identical JSON payloads framed as WS text messages
(useful for browsers that want bidirectional control on one socket).

---

### 6.4 The Mind

#### `GET /v1/sessions/:sid/mind`

The same snapshot the model receives as its `context` dict, mapped to the
wire. This is what the Desktop right pane renders.

```json
{
  "utilization": { "pct_of_limit": 34, "tokens": 41200, "limit": 120000 },
  "scope": "src/com/blockether/vis/internal/ctx_engine.clj",
  "tasks": [
    { "id": "k1", "title": "add evidence check to done-gate",
      "status": "done", "evidence": ["done_gate"] },
    { "id": "k2", "title": "block silent abandonment", "status": "done",
      "evidence": ["abandon_guard"] }
  ],
  "facts": [
    { "key": "done_gate",
      "content": "`apply-done!` rejects a :done step lacking :evidence",
      "files": [ { "path": "src/…/ctx_engine.clj",
                   "regions": [ { "from_hash": "a1b2", "src": "(defn apply-done! …)",
                                  "note": "the gate", "stale": false } ] } ] }
  ],
  "trailer": [ { "call": "clj_eval", "result": "true" } ]
}
```

`stale` on a region is computed by re-hashing the file at `from_hash`'s anchor
— `true` means the memorized source drifted from the file. (The trust signal
the Desktop fact-card turns red on.)

#### `POST /v1/sessions/:sid/mind/facts` — pin a fact (human co-edit)

```json
{ "key": "build_cmd",
  "content": "verify with ./verify.sh --quick before any commit",
  "files": [] }
```

`201`. Writes through `fact_set` into the live `:ctx-atom`; it rides the next
turn's `context["session_facts"]` and emits a `mind.updated` event. This is
the human writing into the same memory the model reads.

`GET /v1/sessions/:sid/mind/facts` and `…/mind/tasks` return just those
sub-collections (cheaper polling for the Mind panels).

#### `GET /v1/sessions/:sid/transcript`

Structured, replay-grade history: every turn → iterations → blocks
(`code`, `result`/`error`, `envelope` timing). This is the **scrubbable
permalink** payload. `?from_turn=` / `?to_turn=` window it.

---

### 6.5 Process

- `GET /v1/models` → `{ "models": [ { "id": "claude-opus-4-8", "provider":
  "anthropic", "reasoning": true }, … ], "default": "claude-opus-4-8" }`
  (from `resolve-effective-model` / the provider registry).
- `GET /metrics` → Prometheus text by default, JSON with `Accept:
  application/json`. Series, **per session / per model / global** (no tenant
  dimension):
  - `vis_turn_tokens_total{session,model,kind=input|output}`
  - `vis_turn_cost_usd_total{session,model}`
  - `vis_turn_duration_ms_bucket{model}` (histogram)
  - `vis_sessions_live`, `vis_sessions_suspended`, `vis_turns_running`
  - `vis_sandbox_rehydrate_ms_bucket`
  This is the feed for the Desktop cost gauge (the "stakes" surface).
- `GET /healthz` → `200 {"status":"ok"}` (no auth) while the process is alive.
- `GET /readyz` → `200` when able to accept turns; `503` while saturated
  (all worker slots busy) or shutting down (drain in progress).

---

## 7. Turn lifecycle (state machine)

```
            POST /turns
   idle ───────────────────▶ running ──────────────▶ completed
     ▲                         │  │                      (answer set)
     │                         │  │ candidate.proposed
     │                         │  ▼
     │            approve/edit │ suspended ◀── survives restart
     │                         │  │ reject
     │  cancel / error / reject│  ▼
     └─────────────────────────┴ failed | cancelled
                              (daemon restart mid-run → interrupted)
```

`session.status` is derived: `running`/`suspended` mirror the current turn;
otherwise `idle`; `interrupted` is sticky until the next turn supersedes it.

---

## 8. Event schema (internal contract, `schema: 1`)

Every event: `{ schema, seq, ts, session_id, type, … }`. `seq` is the SSE
`id`. Not frozen for third parties — evolved freely — but the daemon and
in-house clients (Desktop, replay, mission-control) build against this table.

| `type`               | Payload (beyond the envelope)                                   | Meaning |
| -------------------- | --------------------------------------------------------------- | ------- |
| `turn.started`       | `turn_id, request, meta:{cold_start_ms}`                        | worker picked up the turn |
| `iteration.started`  | `turn_id, n`                                                    | iteration N began |
| `reasoning.delta`    | `turn_id, n, text`                                              | streamed reasoning tokens (throttled) |
| `block.started`      | `turn_id, n, block_id, call, code`                              | a Python form began evaluating |
| `block.output`       | `turn_id, n, block_id, call, result \| error, envelope`        | form result — `rg` hits / `patch` diff / `clj_eval` output |
| `answer.delta`       | `turn_id, text`                                                 | streamed `done()` markdown |
| `mind.updated`       | `utilization?, facts?:[{key,op}], tasks?:[{id,op}], scope?`     | engine state changed — drives the Mind pane |
| `candidate.proposed` | `turn_id, plan:[{title,status}]`                                | proposal-stop; turn suspended, awaits `/approve` |
| `turn.completed`     | `turn_id, answer, tokens, cost, confidence, iteration_count, duration_ms` | terminal success |
| `turn.failed`        | `turn_id, status:error\|cancelled\|interrupted, error`         | terminal failure |
| `notify`             | `level:info\|warn\|error, text`                                | host `notify!` surfaced |
| `heartbeat`          | —                                                               | keepalive (SSE comment) |

`block.output.result` shapes follow the tool render contract
(`{:summary :display}`); the wire carries `{ "kind": …, … }` per tool (e.g.
`rg` → `{kind:"matches", hits:[…]}`, `patch` → `{kind:"diff", unified:"…"}`,
`clj_eval` → `{kind:"value", value:"…"}`).

---

## 9. Error model

```json
{ "error": { "type": "turn_in_progress",
             "message": "session already has a running turn",
             "session_id": "8d6a0a1c-…",
             "turn_id": "t_3f…" } }
```

| HTTP | `type`                  | When |
| ---- | ----------------------- | ---- |
| 400  | `invalid_request`       | malformed body / bad enum |
| 401  | `unauthorized`          | missing/wrong bearer token |
| 404  | `session_not_found` / `turn_not_found` | unknown id |
| 409  | `turn_in_progress`      | concurrent `POST /turns` |
| 409  | `not_suspended`         | `/approve` on a non-parked turn |
| 422  | `workspace_unavailable` | workspace gone / cannot rehydrate |
| 429  | `busy`                  | all worker slots full (also flips `/readyz` 503) |
| 500  | `engine_error`          | uncaught turn failure (also `turn.failed`) |

Engine `ex-info` maps (`anomaly` builders) carry through into `error.detail`
when present, never the raw stacktrace.

---

## 10. The `:api` channel descriptor

```clojure
(vis/channel
  {:channel/id        :api
   :channel/cmd       "serve"
   :channel/doc       "HTTP/WS gateway over the session/turn runtime"
   :channel/usage     "vis serve [--port 7890] [--host 127.0.0.1]"
   :channel/owns-tty? false
   :channel/main-fn   #'gateway/serve!          ; boots server + SessionManager
   ;; NO :channel/messages-renderer-fn. That hook flattens the answer IR into
   ;; a DISPLAY surface's native encoding (TUI → ANSI cells, Telegram → its
   ;; HTML subset). JSON is a transport, not a display surface — the gateway
   ;; stays lossless and lets the client render. See §6.2 (answer serialization).
   :channel/subcommands
   [{:cmd/name "token"  :cmd/doc "print/rotate the gateway bearer token"}
    {:cmd/name "status" :cmd/doc "show live sessions + port"}]})
```

`serve!` (1) starts the SessionManager, (2) registers a `channel-events`
listener for `:api` that appends to per-session event logs and fans out to SSE
subscribers, (3) starts the HTTP/WS server, (4) blocks until SIGTERM, then
drains (`/readyz` → 503, finish in-flight turns, persist, dispose sandboxes).
Lives in a new `extensions/channels/vis-channel-api/`, jar-droppable like
every other channel.

---

## 11. Definition of Done

**Bar:** *a session created over HTTP is indistinguishable from a TUI session,
survives a mid-turn restart, streams its mind in order without loss, and
reports its own health and cost — all in one local process you run with
`vis serve`.* Every box checkable and **verified**, not self-asserted.

### L0 — Local daemon, single tenant
- [ ] `:api` channel registered; `vis serve` boots HTTP+WS, no controlling terminal.
- [ ] Full lifecycle over HTTP: create → submit turn → stream events → read answer → close, against a real workspace.
- [ ] `/events` streams the live `channel-events` bus; a TUI and an HTTP client on the same session see the same turn.
- [ ] `/mind` returns the same `context`/facts/tasks the model gets that turn.
- [ ] Single localhost bearer-token auth.
- [ ] **Verify:** drive a real turn end-to-end (curl + Desktop) — watch a fact pin and a patch land. No fixture.

### L1 — Multi-session, resumable, controllable
- [ ] N concurrent live sessions, stated ceiling, idle-eviction → persist + dispose → rehydrate-from-SQLite on next turn.
- [ ] `/events?cursor=` resumes a dropped stream with zero gaps, zero dupes (kill mid-turn, reconnect, diff the log).
- [ ] `idempotency_key`: double-submit runs the agent once.
- [ ] `/cancel` aborts a running turn within one iteration.
- [ ] Restart the daemon mid-turn → session resumes or reports `interrupted` cleanly; no zombie `running`.
- [ ] `candidate` suspend → `/approve` round-trips; a suspended turn survives restart.

### Ops — observability
- [ ] `/metrics` exposes tokens / cost / latency per session, per model, and global.
- [ ] `/healthz` + `/readyz` correct under concurrent load (readiness flips false while saturated / draining).
- [ ] **Cut, on the record:** OpenAI-compat shim; frozen public event-schema; per-tenant auth/keys/budgets; multi-node affinity. The internal event-schema doc (§8) stays.
