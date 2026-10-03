# Council rooms, schedules and webhooks

Let Vis machines share Council threads through one small HTTP server, and start agent work from a
crontab schedule or a webhook.

## Context

Council is local today. Each session has one group: UI group, then project, then workspace
repository, then a directory hash (`session-group` in
`src/com/blockether/vis/internal/council/core.clj`). A foreign `group_id` fails with
`group-not-found`. Pings and wakes stay inside the group: an explicit ping or an answering
`reply_to` resumes an idle peer, `ping="all"` and `members()` see only active sessions, and a
managed team keeps its team rule (`wake-allowed?` in `session/agents.clj`). The gateway routes
`/v1/sessions/:sid/council/...` serve one session on one engine.

No scheduler exists; only internal `ScheduledExecutor`s (MCP, views, session model). The gateway
already has what a task runner needs: `create-session!` (accepts `:group-id`), `submit-turn!`
(queues a busy session, takes an idempotency key), `close-session!`, and the terminal events
`turn.completed`, `turn.failed` and `turn.cancelled` (`gateway.json`). Gateway authentication is
one bearer secret; a route contribution can declare its own `:prefix`, `:open-uris` and
unauthorized response (`gateway/server.clj`).

Goals:

1. Multiplayer Council: sessions on different machines use the same `council.publish`, `read`,
   `threads`, `get` and `members` calls. The model-facing API gets no new names.
2. A rooms server with a written contract: machine IDs, rooms that an admin or moderators create,
   membership, presence leases, entries and threads. A Python reference server with SQL and
   DynamoDB stores, and a Python SDK client.
3. Scheduled tasks with standard crontab expressions. A task targets an existing session, a new
   session or a temporary session.
4. Webhooks: an external system starts a task with a token and gets the outcome by polling or by a
   signed callback.

Prior art (Hermes Agent, `NousResearch/hermes-agent` main: `website/docs/user-guide/features/cron.md`,
`website/docs/user-guide/messaging/webhooks.md`, `website/docs/user-guide/features/hooks.md`,
`agent/outbound_webhooks.py`):

- Cron: the gateway ticks every 60 s and runs each due job in a fresh, isolated agent session.
  Formats: one-shot delays (`in 30m`), intervals (`every 2h`), natural phrases compiled to cron,
  5-field cron with names, ISO timestamps. Output goes to a delivery target; `[SILENT]` suppresses a
  successful delivery; failures always deliver. Cron runs cannot create cron jobs.
- Inbound webhooks: named routes with a required secret (GitHub `X-Hub-Signature-256`, GitLab
  token, Standard Webhooks, timestamped generic HMAC), event filters, dot-path prompt templates,
  delivery IDs cached for 1 h, rate and body limits, and `cron_job` routes that fire an existing job.
- Events: gateway hooks (`gateway:startup`, `session:start|end|reset|compress`,
  `agent:start|step|end`, `command:*`), plugin hooks (`pre/post_tool_call`, `pre/post_llm_call`,
  stream observers, `pre/post_api_request`, ...), and outbound webhooks that POST plugin-hook events
  with `X-Hermes-Signature-256`, a 10 s timeout and at most 2 attempts, fire and forget.

Vis takes: one isolated run per fire as a target mode, delivery-ID idempotency, signed callbacks,
no self-scheduling. Vis does not take now: natural-language schedules (the model writes the cron
line), platform delivery adapters, coalescing, filter scripts and per-tool outbound events
(authenticated clients already stream turn events).

Naming: "relay" belongs to the push Worker `apps/vis-companion-relay`. The new feature is
"Council rooms": server `apps/vis-rooms`, contract `rooms.json`, SDK module `blockether.vis.rooms`.

Rejected alternatives:

- Rooms link as an extension with a timer: a wake writes entries as the local session, so
  `reply_to`, `members()` and recipient checks cannot see remote sessions. It also needs three new
  extension APIs (scheduler, Council publish hook, ingest and roster host calls). The engine owns
  Council semantics, so the link lives there.
- Reuse `apps/vis-companion-relay`: another trust model; it seals APNs/FCM grants and stores nothing.
- WebSockets or a broker (NATS, Redis): stateful infrastructure. Polling against a `head` counter
  is enough for agent traffic and runs on Lambda.
- FastAPI/Pydantic models: a second source of shapes. The server validates with the canonical
  JSON Schemas.
- DynamoDB only: does not run on the test server. SQL first; DynamoDB behind the same store API.
- A new CRUD API for schedules and webhooks: settings already have typed objects, scopes,
  revisions, `PATCH /v1/settings` and the SDK `patch_settings`.
- Model-managed schedules and room links: a model could schedule itself or share a group without
  consent. People configure both through settings or the SDK.

## 0. Self-wake rule (done)

Rationale: #202 lets only a wakeable managed subagent wake itself. 4795379b7 (explicit pings
between independent peers) also let an independent session wake itself, against the SDK
docstrings in `extension.py` and `engine/_council.py`.

Data: `wake-allowed?` in `session/agents.clj` adds `(not= author-id recipient-id)` to the
independent-peer branch.

Acceptance criteria: `independent-session-self-wake-test` and the self cases in `agents_test`
pass; independent peers still wake each other; managed children still wake themselves.

Unknowns: none.

## 1. Rooms contract

Rationale: the SDK, the server and the engine link implement one written contract.

Data: `packages/vis-contract/resources/vis-contract/schema/rooms.json`. Its `$defs` reference the
`council.json` shapes (`publish` fields, `entry`, `entry_page`, `thread_page`, `member`, `kind`).

- IDs: `machine_id` and `room` are slugs `^[a-z0-9][a-z0-9-]{0,62}$`. A remote session ID is
  `<session-uuid>@<machine_id>`; the server adds the suffix from the token, so a machine cannot
  speak for another. `entry_id` is a gapless sequence per room from 1; `thread_id` is the root
  `entry_id`.
- Roles: `admin` (the server secret from deployment, `VIS_ROOMS_ADMIN_SECRET`), `moderator` (a
  machine that the admin promoted), `member` (any enrolled machine).

| Method and path | Role | Request -> response |
|---|---|---|
| `GET /v1/info` | none | -> `{contract, limits}` (TTL bounds, page limit, byte limits) |
| `POST /v1/enrollments` | admin, moderator | `{expires_in_s, max_uses}` -> `{code, expires_at}`; code shown once |
| `POST /v1/machines` | enrollment code, admin | `{machine_id}` -> `{machine_id, role, token}`; token shown once; 409 when taken |
| `GET /v1/machines/me` | member | -> `machine` |
| `DELETE /v1/machines/me` | member | release the ID: presence, memberships and token go, entries stay -> 204 |
| `PUT /v1/machines/{machine_id}/role` | admin | `{role}` -> `machine` |
| `DELETE /v1/machines/{machine_id}` | admin | -> 204 |
| `POST /v1/rooms` | admin, moderator | `{room, title}` -> `room`; 409 when it exists |
| `GET /v1/rooms` | member | -> `{rooms: [room + joined]}` |
| `DELETE /v1/rooms/{room}` | admin, moderator | -> 204 |
| `PUT /v1/rooms/{room}/members/me` | member | join -> `room` |
| `DELETE /v1/rooms/{room}/members/{machine_id or me}` | self, moderator | leave or remove -> 204 |
| `PUT /v1/rooms/{room}/presence` | room member | `{ttl_s, sessions: [member]}` -> `{expires_at, head, members: [remote_member]}` |
| `DELETE /v1/rooms/{room}/presence` | room member | -> 204 |
| `POST /v1/rooms/{room}/entries` | room member | `room_publish` -> `entry` |
| `GET /v1/rooms/{room}/entries?after&limit&thread_id` | room member | -> `entry_page` |
| `GET /v1/rooms/{room}/entries/{entry_id}` | room member | -> `entry` |
| `GET /v1/rooms/{room}/threads?after&limit` | room member | -> `thread_page` |

- New `$defs`: `machine {machine_id, role, created_at}`, `enrollment`,
  `room {room, title, head, created_by, created_at, joined?}`, `presence_request {ttl_s, sessions}`,
  `presence {expires_at, head, members}`, `remote_member` (`member` + `machine_id`, `expires_at`),
  `room_publish` (`publish` without `group_id` and `activation_id`, with required
  `author_session_id` and `idempotency_key`), `error {error: {code, message}}`.
- Guarantees: a reader never sees entry n before n-1. The same machine and `idempotency_key`
  with the same body returns the first entry; another body returns 409. `ping` IDs must be live
  presence members (422). `thread_id` and `reply_to` must exist in the room (422). Byte limits
  come from `council.json`. Presence TTL is 15-300 s and is renewed every TTL/3; members are
  filtered by `expires_at > now`.
- Errors: 400 invalid, 401 missing or bad token, 403 role or membership, 404, 409 conflict,
  413 too large, 422 bad reference, 429 rate limit.

Acceptance criteria: schema examples validate in Python (`validate("rooms", ...)`) and in Clojure
(Skjema); code reads bounds from the schema and never copies them; contract and docs tests pass.

Unknowns: open rooms for every enrolled machine (proposed) or an invite-only flag later; a
long-poll `wait_s` on entries for lower latency (later).

## 2. Reference server and SDK client

Rationale: one Python server runs on a small VPS and on Lambda; one SDK client serves scripts,
extensions and tests.

Data:

- `apps/vis-rooms/`: `pyproject.toml` (uv); `vis_rooms/app.py` (Starlette ASGI; Mangum adapter for
  Lambda); `auth.py` (SHA-256 token hashes, constant-time admin check, per-token rate limit);
  `store/sql.py` (SQLite now, Postgres-compatible DDL); `store/dynamo.py` (boto3); `tests/` (one
  contract suite against both stores; DynamoDB through moto).
- `packages/vis-agent/src/blockether/vis/rooms.py`: `RoomsClient(server, token)` with `info`,
  `reserve_machine`, `release_machine`, `me`, `create_enrollment`, `set_role`, `create_room`,
  `rooms`, `delete_room`, `join`, `leave`, `presence`, `end_presence`, `publish`, `entries`,
  `entry`, `threads`. Typed frozen results; bodies validated with the canonical schemas.

SQL (one transaction per publish; SQLite uses `BEGIN IMMEDIATE`):

```sql
CREATE TABLE machines (machine_id TEXT PRIMARY KEY, role TEXT NOT NULL,
  token_sha256 TEXT NOT NULL UNIQUE, created_at INTEGER NOT NULL);
CREATE TABLE enrollments (code_sha256 TEXT PRIMARY KEY, uses_left INTEGER NOT NULL,
  expires_at INTEGER NOT NULL, created_by TEXT NOT NULL);
CREATE TABLE rooms (room TEXT PRIMARY KEY, title TEXT, head INTEGER NOT NULL DEFAULT 0,
  created_by TEXT NOT NULL, created_at INTEGER NOT NULL);
CREATE TABLE room_members (room TEXT NOT NULL REFERENCES rooms ON DELETE CASCADE,
  machine_id TEXT NOT NULL REFERENCES machines ON DELETE CASCADE, joined_at INTEGER NOT NULL,
  PRIMARY KEY (room, machine_id));
CREATE TABLE presence (room TEXT NOT NULL, machine_id TEXT NOT NULL, sessions TEXT NOT NULL,
  expires_at INTEGER NOT NULL, PRIMARY KEY (room, machine_id));
CREATE TABLE entries (room TEXT NOT NULL, entry_id INTEGER NOT NULL, thread_id INTEGER NOT NULL,
  machine_id TEXT NOT NULL, body TEXT NOT NULL, created_at INTEGER NOT NULL,
  PRIMARY KEY (room, entry_id));
CREATE INDEX entries_by_thread ON entries (room, thread_id, entry_id);
CREATE TABLE publish_keys (room TEXT NOT NULL, machine_id TEXT NOT NULL,
  idempotency_key TEXT NOT NULL, entry_id INTEGER NOT NULL, body_sha256 TEXT NOT NULL,
  PRIMARY KEY (room, machine_id, idempotency_key));
```

Publish: `UPDATE rooms SET head = head + 1 WHERE room = ? RETURNING head`, then insert the entry
and its key in the same transaction.

DynamoDB (one table, keys `PK`/`SK`, TTL attribute `ttl`):

| Item | PK | SK |
|---|---|---|
| machine | `MACHINE#<machine_id>` | `META` |
| token lookup | `TOKEN#<sha256>` | `META` |
| enrollment | `ENROLL#<sha256>` | `META` (+ `ttl`) |
| room | `ROOM#<room>` | `META` (`head`) |
| membership | `ROOM#<room>` | `MEMBER#<machine_id>` |
| presence | `ROOM#<room>` | `PRESENCE#<machine_id>` (+ `ttl`) |
| entry | `ROOM#<room>` | `ENTRY#<20-digit entry_id>` |
| publish key | `ROOM#<room>` | `KEY#<machine_id>#<idempotency_key>` |
| thread | `ROOM#<room>` | `THREAD#<20-digit root entry_id>` |

Publish is one `TransactWriteItems`: `META` with condition `head = n-1` sets `n`; `ENTRY#n` and
`KEY#...` are put with `attribute_not_exists`; `THREAD#root` is updated. A lost race retries with
the new head. Reads use `Query` with `ConsistentRead`. DynamoDB TTL deletes late, so presence
reads also filter `expires_at > now`. A sparse GSI over room `META` items lists rooms.

Acceptance criteria: the contract suite passes on SQLite and on DynamoDB (moto) with the same
cases: order under 8 parallel writers, idempotent retry, 409 for a changed body, 409 for a taken
machine ID, role checks on every route, presence expiry, 422 for a ping to an expired member,
thread listing and byte limits. SDK tests run against the in-process server. Ruff format and lint
are clean.

Unknowns: CI wiring for `apps/vis-rooms`; the license audit for `starlette`, `uvicorn`, `mangum`,
`boto3` and `moto`.

## 3. Test deployment

Rationale: prove the server on real HTTPS before engine work depends on it.

Data: the deployment recipe lives in the `infrastructure` repository: a `vis-rooms` systemd unit
(uvicorn, own user), a SQLite file, TLS through the existing front, the admin secret from the
infrastructure secret store and a hostname under the project domain. No deployment detail goes
into this repository.

Acceptance criteria: `GET /v1/info` answers over HTTPS; the SDK on two machines reserves machine
IDs, creates a room, joins it and exchanges entries; a service restart keeps the data; the admin
secret never appears in logs.

Unknowns: hostname and TLS front on the test server; backup of the SQLite file.

## 4. Engine link

Rationale: Council semantics (members, recipient checks, `reply_to`, wakes) stay in the engine;
the link only moves entries.

Data:

- Settings: global `council.rooms.server` and `council.rooms.machine`. A gateway route
  `POST /v1/council/rooms/enroll {server, machine, code}` reserves the machine ID and writes the
  token to `~/.vis/state.yml`; the SDK, the TUI and the Companion call it. The group or project
  setting `council.room` links that Council group to a room; a project `vis.yml` can link a whole
  team. One local group per room on a machine.
- `src/com/blockether/vis/internal/council/rooms.clj` (`babashka.http-client`, `wire/json-str`):
  one loop per linked group. Presence every TTL/3 (sessions are the active local members); pull
  when `head` is above the cursor; push the outbox. `gateway/wiring.clj` starts and stops it next
  to `install-waker!`.
- Store: a map `(room, entry_id) <-> local entry_id`, a cursor per room and an outbox keyed by the
  local idempotency key. Pushed: entries published in a linked group. Not pushed: pulled entries,
  `autocomplain` entries, `source_ref` and entries from before the link.
- `council/core.clj`: `members` adds the live remote roster; recipient checks accept
  `<uuid>@<machine>` while that member is live; a pulled entry uses the local delivery path (an
  active recipient gets input when `reply_required`; an idle recipient wakes). A remote author has
  no agent record, so `wake-allowed?` treats it as an independent peer.
- Docs: a `council.md` section on other machines: what leaves the machine, who can read it and how
  to leave. Settings reference for the new keys.

Acceptance criteria: Lazytest with two engines, separate stores and one in-process fake server
validated against `rooms.json`: a remote ping wakes an idle session; a `reply_to` across machines
marks the request replied; a retry after a server outage publishes once; an expired lease removes
the member; an unknown remote ping ID fails with `invalid-recipient`; nothing leaves the machine
without `council.room`. The model-facing Council API has no new names.

Unknowns: whether a remote ping may wake an idle session (proposed: yes, as a local peer, with a
limit per remote author); the exact settings keys and scope rules in the settings catalog.

## 5. Two-machine end-to-end

Rationale: the real check is two gateways on two hosts.

Data: the phase 3 server, the Vis gateway on the test server (`visgw`) and a laptop gateway;
machine IDs such as `mikrus` and `karol-mbp`; room `vis-dev`.

Acceptance criteria: enrollment, room creation by a moderator and linking work from settings;
`council.members()` on each side lists the other machine's sessions; a `coordination` ping with
`reply_required` wakes the remote session, and its `reply_to` answer arrives within one heartbeat
plus 2 s; the outage drill (stop the server for 2 min, publish, start it) delivers exactly once;
a stopped gateway leaves the roster after the TTL; a taken machine ID gets 409.

Unknowns: whether the test-server gateway stays up between checks.

## 6. Scheduled tasks (crontab)

Rationale: start agent work at fixed times without a person present.

Data:

- Contract `automation.json`: `cron`, `timezone` (IANA, default system zone), `target`,
  `schedule`, `webhook`, `callback`, `task`, `task_event`.
- `target`: `{"mode": "session", "session_id": ...}`, `{"mode": "new", "project": ...,
  "group_id": ...}` or `{"mode": "temporary", "project": ...}`. `session` queues a turn in that
  session. `new` creates a session per run that stays in the session list. `temporary` creates a
  session per run, saves the answer on the task and deletes the session.
- Settings (global scope only): `automations.schedules.<id> = {cron, timezone, target, prompt,
  enabled, callback}`. The toggle `automations` (default off) pauses all schedules and webhooks.
- `src/com/blockether/vis/internal/automation/cron.clj`: 5 fields (minute, hour, day of month,
  month, day of week 0-7), `*`, lists, ranges, steps, `JAN-DEC`, `SUN-SAT` and the macros
  `@yearly @annually @monthly @weekly @daily @midnight @hourly`. When day of month and day of week
  are both restricted, either one matches (Vixie rule). A local time in a DST gap runs at the
  first instant after the gap; a repeated local time runs once.
- `automation/tasks.clj` and `automation/scheduler.clj`: one scheduler thread computes the next
  fire per schedule. A fire creates a task. When the previous task of that schedule is still
  queued or running, the new task is `skipped`. Fires missed while the gateway is down do not run
  later. Tasks are a gateway DB table: `task_id, trigger {kind, id}, status (queued | running |
  completed | failed | cancelled | skipped), session_id, turn_id, created_at, started_at,
  finished_at, answer, error`. Status follows the terminal turn event. No model-facing tool writes
  `automations` settings, so a task cannot schedule more work.
- HTTP and SDK: `GET /v1/tasks`, `GET /v1/tasks/{task_id}`, `POST /v1/schedules/{id}/run` (run
  now); `sdk_session.tasks()` and `sdk_session.run_schedule(id)`.
- Docs: a new page for scheduled work that starts with When to use; configuration reference.

Acceptance criteria: cron table tests (fields, names, steps, macros, invalid input, the OR rule,
Europe/Warsaw DST gap and overlap); scheduler tests with a fake clock for the three target modes,
overlap skip and toggle off; a temporary session is gone after the run and its answer stays on
the task; run now works through the SDK.

Unknowns: whether the settings catalog can restrict keys to global scope; the session-creation
options for `project`; the model for a task (proposed: the target session, else the global
default).

## 7. Webhooks and task events

Rationale: an external system starts a task with a token and learns the outcome.

Data:

- Settings `automations.webhooks.<id> = {target, prompt, token_sha256, callback, enabled}`.
  `POST /v1/webhooks/{id}/token` (gateway authentication) creates the webhook token and the
  callback secret, returns them once, stores the token hash in settings and the secret in
  `~/.vis/state.yml`.
- `POST /v1/webhooks/{id}` (route contribution with its own prefix and `open-uris`;
  `Authorization: Bearer <webhook token>`): JSON body up to 64 KiB; `Idempotency-Key` (or
  `webhook-id`, `X-GitHub-Delivery`) kept for 24 h; `prompt` renders `{payload}` and `{dot.path}`
  values. Answer: 202 `{task_id, status, session_id}`. Errors: 401, 404, 409 for a reused key with
  another body, 413, 429, 503 when `automations` is off.
- `GET /v1/webhooks/{id}/tasks/{task_id}` (same token) -> `task`.
- Callback on `task.completed`, `task.failed`, `task.cancelled` (and `task.skipped` for schedules):
  POST `{type, timestamp, data: task}` with Standard Webhooks headers (`webhook-id`,
  `webhook-timestamp`, `webhook-signature: v1,<base64 HMAC-SHA256 of id.timestamp.body>`). At
  least once: persisted outbox, 10 s timeout, retries after 1 min, 5 min, 30 min and 2 h;
  receivers deduplicate by `webhook-id`. Schedules use the same `callback` block.

Acceptance criteria: route tests for each status code; a duplicate delivery returns the same task;
a local receiver verifies the signature with the reference algorithm; a retry after a receiver
failure delivers once; the webhook token cannot call any other gateway route; docs show a curl
example and a signature check.

Unknowns: exposing only `/v1/webhooks/*` from the test-server gateway; inbound GitHub-style HMAC
signatures (later).

## 8. AWS (optional)

Rationale: corporate deployments use AWS; no earlier phase needs it.

Data: the same server on Lambda (Mangum) behind API Gateway with the DynamoDB store; IaC in the
`infrastructure` repository; a JWT authorizer (Entra, Okta or Cognito) can replace enrollment codes.

Acceptance criteria: the phase 2 contract suite passes against a real on-demand table; the phase 5
script passes against the API Gateway URL.

Unknowns: AWS account and credentials; how JWT identities map to roles.

## Plan state

- Phase 0 is complete and pushed in 65bba8f7b. The affected namespaces pass (762 cases); clj
  format and lint are clean.
- Phases 1-8 are a proposal and wait for a decision. Track A (phases 1-5 and 8) and track B
  (phases 6-7) do not depend on each other.
- Open decisions: remote wake policy; open or invite-only rooms; a default project for `new` and
  `temporary` targets; the server hostname.
