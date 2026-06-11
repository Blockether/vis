# Vis Gateway — HTTP/SSE API

> Status: **L0 implemented and verified live** (`internal/gateway/*`,
> `vis serve`): lifecycle, async turns, SSE with cursor resume,
> idempotency, /mind, /metrics, bearer auth — exercised end-to-end with
> a real LLM turn over HTTP. L1 (eviction sweep, restart reconcile) and
> transcript/fact-pin endpoints remain. This document is the canonical
> contract every client (Desktop, Web replay, mission-control, CI, IDE)
> builds against.

## 1. What this is

The Gateway is **the session/turn runtime served over HTTP + SSE**. It is the
front door to a fleet of **stateful agent sessions** — not a proxy for tokens.

It is deliberately **internal core** (`internal/gateway/{wire,state,server}.clj`),
NOT a channel extension: the bus it lifts onto the wire (`channel-events`) is
internal, it owns no renderer (ALWAYS IR, §4.1), and it drops nothing from the
classpath when unused — so the "drop the jar, drop the feature" extension
justification does not apply. Consequence: **any host process can serve it
alongside whatever else it is doing** (`vis.core/gateway-start!`) — the
`vis serve` daemon, a TUI run (Desktop attaches to the same live session you
watch in the terminal), or an embedded caller.

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

`serve` is a top-level binary built-in (`registry/register-cmd!` in
`internal/main.clj`, lazy-resolving the gateway), not a channel. The stack is
Clojure-native: **reitit-ring** routes → Ring middleware (auth, errors, query
params) → the **Ring Jetty adapter on JDK virtual threads**
(`:virtual-threads? true`, Java 21+). SSE is a Ring `StreamableResponseBody`
parked on its virtual thread. No controlling terminal; logs to
`~/.vis/vis.log`. `VIS_DB_PATH` overrides the SQLite path (handy for
throwaway daemons).

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

### 4.1 Canonical IR is the wire's display representation — ALWAYS

Every **renderable** payload — an answer, a tool/block output, a notification,
a fact's content, a task's label — is serialized as **canonical IR**
(`[:ir {attrs} & blocks]`) encoded to JSON. This is not a gateway invention:
IR is already the engine's channel-agnostic display contract (`header.clj`:
*"channel-agnostic data, NOT a paint thunk"*; tool render-fns already return
`{:summary :display}` with `:display` = IR). The TUI walks IR → ANSI, Telegram
walks IR → HTML; the gateway emits IR and the web/Desktop client walks IR →
DOM. **One interpreter, every surface** — the client writes a single IR walker.

The thin **envelope** around the IR stays plain data: `id`, `seq`, `ts`,
`status` enums, `tokens`/`cost`/`utilization` numbers, fact `key`, task
`status`. These are values the client *branches and computes on* — the cost
gauge needs the integer `42330`, not an IR tree.

> **Rule: if a human reads it → IR. If the client branches/computes on it → data.**

Verbatim markdown is not lost — it is the **source** the IR is derived from
(`markdown->ir`). The gateway carries it alongside as a raw `*_md` field for
text-only clients and copy-paste, but **IR is the always-present primary render
path**. (This is why §10 omits `:channel/messages-renderer-fn`: that hook
flattens IR into one display dialect; the gateway ships IR itself and lets the
client be the renderer.)

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
# (/events/ws WebSocket twin: NOT built — SSE covers L0/L1; add only if a client needs bidi)

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
  "answer_ir": ["ir", {}, ["p", {}, "Done. The gate now rejects a :done step lacking :evidence …"]],
                                    // canonical [:ir …] as JSON — THE answer, always present once completed (null while running)
  "answer_md": "Done. The gate now rejects a :done step lacking :evidence …",  // verbatim done() markdown — the SOURCE the IR is derived from
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

**Answer serialization.** `answer_ir` is the always-present primary (canonical
IR, per §4.1); `answer_md` is the verbatim `done()` markdown it derives from
(source of truth — `:answer-markdown`, `render.clj`). The gateway never
flattens; the client walks the IR. See §4.1.

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
warm. (A `/events/ws` WebSocket twin carrying identical payloads is a
possible later addition; SSE is the implemented transport.)

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

## 10. Wiring (internal core, NOT a channel)

As built:

| Piece | Where |
| ----- | ----- |
| Wire encoding (EDN→JSON walker, `bounded-pr`, SSE frames) | `internal/gateway/wire.clj` |
| SessionManager (event ring + `:seq`, subscribers, turn workers, idempotency, cancel, metrics) | `internal/gateway/state.clj` |
| Reitit routes + Ring middleware (auth/errors/query) + Jetty on virtual threads + SSE body | `internal/gateway/server.clj` |
| `vis serve` built-in (`--port` `--host` `--token-file`), lazy-resolved | `internal/main.clj` (`cli-serve!`) |
| Facade exports `gateway-start!` / `gateway-stop!` / `gateway-running?` | `core.clj` |
| Deps: `ring/ring-core`, `ring/ring-jetty-adapter`, `metosin/reitit-ring` | root `deps.edn` |

There is deliberately **no channel descriptor and no
`:channel/messages-renderer-fn`**: that hook flattens canonical IR into a
DISPLAY surface's native encoding (TUI → ANSI cells, Telegram → its HTML
subset). The gateway ships canonical IR itself and lets the client be the
renderer (IR → DOM). See §4.1 — ALWAYS IR.

Live turn observation rides the engine's `send!` `:hooks {:on-chunk …}`
phased chunks (the same `progress.clj` contract the TUI consumes), translated
into §8 events by `state/chunk->event`. Sessions are created on the `:api`
channel id, so `vis sessions list` shows them like any other channel's.

### 10.1 Web companion (`/ui`) — the `vis-channel-web` extension

The two-pane instrument at `/ui` lives in
`extensions/channels/vis-channel-web` (jar-droppable like every channel) and
**auto-mounts**: the gateway core exposes a route-contribution registry
(`vis.core/gateway-register-routes!` → reitit routes + `:open-uris` +
`:request-authed-fn` + `:on-unauthorized` + `:form-params?` per contribution,
hot-swapped into the live handler), and the extension registers its
contribution at namespace load. Namespaces load via the
`META-INF/vis-extension/vis.edn` classpath manifest scan — so **drop the
jar, get `/ui`; remove it, the gateway serves the pure JSON API**. (The same
auto-discovery move as Java's ServiceLoader / Spring auto-configuration, in
vis's own extension idiom.) `vis channels web` starts the gateway and prints
the `/ui` address.

LEFT the conversation (user bubbles + answers), RIGHT **the Mind** (plan,
fact cards with `@hash` regions, utilization bar) with a live activity feed.
No JavaScript is written or built: hiccup renders HTML, HTMX (CDN) does
declarative swaps, and the live feed is the htmx SSE extension consuming
`/ui/session/:sid/stream` — a gateway SSE stream that emits named **HTML
fragments** (`activity`, `thinking`, `mind`) instead of JSON, rendered
server-side from the same events.

`ir->hiccup` is the **third canonical-IR walker** (TUI → ANSI cells,
Telegram → HTML subset, web → DOM), closing the §4.1 loop: the answer IR on
the wire and the answer DOM in the browser come from one representation.

Browser auth: POST `/ui/auth` exchanges the same bearer token for an
HttpOnly `vis_token` cookie (EventSource carries it on the SSE connect);
`/ui`, `/ui/auth`, `/ui/app.css` are the only open routes and leak nothing.
Param parsing is scoped (`wrap-scoped-params`): uris under a contribution
prefix that declared `:form-params?` get form+query params,
the JSON API gets query-only — so the urlencoded form parser can never
consume a JSON request body (curl `-d` defaults to that content-type).

Known limitation: the turn registry (records + event ring) is in-memory —
after a daemon restart the session list and souls persist (SQLite) but
prior turns' wire records and event replay do not (L1 restart-reconcile
territory; the engine's own persisted iterations are untouched).

---

## 11. Definition of Done

**Bar:** *a session created over HTTP is indistinguishable from a TUI session,
survives a mid-turn restart, streams its mind in order without loss, and
reports its own health and cost — all in one local process you run with
`vis serve`.* Every box checkable and **verified**, not self-asserted.

### L0 — Local daemon, single tenant
- [x] `vis serve` built-in registered; boots HTTP+SSE on Jetty/virtual threads, no controlling terminal. *(verified 2026-06-10: real daemon, `/healthz` in ~10s)*
- [x] Full lifecycle over HTTP: create → submit turn → stream events → read answer → close, against a real workspace. *(verified: real session + auto-minted workspace + real `claude-opus-4-8` turn, answer `ROUNDTRIP 42` as canonical IR, DELETE 204)*
- [x] `/events` streams the live turn (9 ordered events: `turn.started` → `reasoning.delta` → `block.started/output` → `iteration.completed` → `turn.completed`). *(verified live over curl -N)*
- [ ] …and a TUI and an HTTP client attached to the same session see the same turn. *(dual-attach not yet exercised)*
- [x] `/mind` returns the same `context`/facts/tasks the model gets — incl. the engine's auto `turn_1` fact. *(verified)*
- [x] Single localhost bearer-token auth (401 without token; token minted mode-600 at `~/.vis/gateway.token`). *(verified)*
- [x] **Verify-plus (fact half):** a turn that pins a `fact_set` fact, submitted and watched through the web companion — `web_smoke` landed in `/mind` and the answer rendered in the page (2026-06-11, on the default fable-5 config that ran away the day before). *(a `patch`-landing turn watched live remains)*

### L1 — Multi-session, resumable, controllable
- [ ] N concurrent live sessions, stated ceiling, idle-eviction → persist + dispose → rehydrate-from-SQLite on next turn. *(rehydrate path exists via `env-for`; eviction sweep not built)*
- [x] `/events?cursor=` / `Last-Event-ID` resumes with zero gaps, zero dupes. *(verified: reconnect at id 5 replayed exactly 6–9)*
- [x] `idempotency_key`: double-submit runs the agent once. *(verified: replay returned the same turn id, 200 not 202)*
- [x] `/cancel` aborts a running turn within one iteration. *(verified live 2026-06-10 — twice, killing 150–400-iteration runaway-overflow turns (TASKS VIS-9) in ~4s each)*
- [ ] Restart the daemon mid-turn → session resumes or reports `interrupted` cleanly; no zombie `running`. *(boot reconcile not built)*
- [ ] `candidate` suspend → `/approve` round-trips. *(implemented as the engine's stop-and-wait: a `needs-input` answer marks the turn `suspended`; `/approve` submits the decision as the next turn — not yet exercised live)*

### Ops — observability
- [x] `/metrics` exposes tokens / cost / latency — global + per session (Prometheus text + JSON). *(verified: counters matched the live turn to the digit, $0.0124305; per-model dimension not yet emitted)*
- [x] `/healthz` (open) + `/readyz` (authed) respond correctly. *(load-shedding readiness flip not built)*
- [x] **Cut, on the record:** OpenAI-compat shim; frozen public event-schema; per-tenant auth/keys/budgets; multi-node affinity; WS twin. The internal event-schema doc (§8) stays.
