# Council rooms, schedules and webhooks

Connect selected Vis groups and sessions through the existing relay. Keep sharing and remote waking under human control.

## Context

Council is local today. The accepted Rooms design extends `apps/vis-companion-relay`, the existing Cloudflare Worker.
Push keeps its sealed grants and has no device database. Rooms uses separate credentials, rate limits and D1 tables.
The operator can read room messages. This design does not provide end-to-end encryption.

Canonical protocols live in `packages/vis-contract/resources/vis-contract/schema/`. Rooms reuses the Council payload definitions.
The Worker uses compiled validators from these schemas. The gateway and Python SDK validate the same definitions.
The model keeps the existing `council.publish`, `members`, `read`, `threads` and `get` methods.

A machine has a persistent ID and credential. A room membership survives presence expiry.
An owner creates bounded invitations. Opening a link has no effect. Explicit redemption consumes an invite atomically.
The fragment carries the invite secret. The database stores only credential and invite hashes.

Joining a machine does not share a group, a session, past messages, files or local failure reports.
Settings select the sharing scope. Child scopes must not widen parent restrictions. Remote waking requires separate consent.
Room access must constrain sends, reads, discovery, notification delivery and wake requests.

The user authorized Rooms end to end, including verification, commits and pushes. A Vis product release needs another request.
Cron, webhooks, a Python server and AWS hosting are not part of this implementation turn.

Rejected alternatives:

- A separate Python server: the existing Worker already provides the relay address and deployment boundary.
- Push credentials for Rooms: possession of a notification grant must not authorize room access.
- Implicit join on GET: previews and link scanners must not consume invitations or enable sharing.
- Model-managed sharing: a model must not widen its own room permissions.
- Exporting local history: consent starts new room traffic, not a historical upload.
- Unrestricted session overrides: a session must not bypass its group's restrictions.

## 0. Self-wake rule (done)

Rationale: #202 lets only a wakeable managed subagent wake itself. 4795379b7 (explicit pings
between independent peers) also let an independent session wake itself, against the SDK
docstrings in `extension.py` and `engine/_council.py`.

Data: `wake-allowed?` in `session/agents.clj` adds `(not= author-id recipient-id)` to the
independent-peer branch.

Acceptance criteria: `independent-session-self-wake-test` and the self cases in `agents_test`
pass; independent peers still wake each other; managed children still wake themselves.

Unknowns: none.

## 1. Canonical Rooms protocol

Rationale: clients and servers must share one source for payload shapes, bounds and operations.

Data: `packages/vis-contract/resources/vis-contract/schema/rooms.json` references `council.json`.
`apps/vis-companion-relay/scripts/compile-rooms-contract.mjs` compiles Worker validators without runtime code generation.
The SDK registry includes the Rooms schema. Generated validators and database limits must stay current.

Acceptance criteria:

- Every HTTP operation has request, response, authorization and error definitions.
- Python, Clojure and Worker consumers validate the same payloads.
- Tests reject unknown fields, malformed IDs, excessive bodies and unsafe continuations.

Unknowns: none. The Rooms catalog covers 21 relay operations and nine gateway management operations.

## 2. Rooms module and Python SDK

Rationale: keep the relay deployment while separating room storage and authorization from Push.

Data: `apps/vis-companion-relay/src/rooms/` and `migrations/` own the Worker module and D1 schema.
The SDK client belongs in `packages/vis-agent/src/blockether/vis/rooms.py`.

Acceptance criteria:

- Only administrators and moderators create rooms. Owners manage invitations and membership.
- One-use invites admit one concurrent redeemer. Lost responses can be retried without consuming another use.
- Revocation takes effect on the next request. A replay must not restore revoked membership.
- Session identity belongs to one machine. Presence leases do not remove membership.
- Council replies, receipts, idempotency and bounded pagination preserve their protocol semantics.
- Rooms failures do not disable Push. All existing relay tests pass.

Unknowns: none for the implemented protocol. Real workerd and D1 tests cover every declared relay operation.

## 3. Engine and scoped Settings

Rationale: humans select sharing boundaries; ordinary Council calls enforce those boundaries.

Data: `src/com/blockether/vis/internal/council/`, scoped settings and the existing gateway lifecycle.
A configured room is authoritative for its new Council traffic. Unconfigured sessions remain local.
Credentials must stay outside returned Settings values and model context.

Acceptance criteria:

- Machine, project, group and session restrictions resolve predictably. Child scopes cannot broaden an explicit parent limit.
- A group or session can stay local. Joining alone enables no sharing and no waking.
- Normal Council APIs use the selected room without exposing gateway secrets or local source references.
- Automatic failure reports remain local.
- Held queues stay held. Incoming remote pings require current membership, room permission and wake consent.
- Reconnection, restart, policy changes and duplicate delivery do not replay completed work.
- Affected Lazytest suites, formatting and reflection lint pass.

Unknowns: the final native rebuild remains to be checked. JVM tests cover durable cursors, policy changes and remote receipt failures.

## 4. Settings user interface

Rationale: joining must show the machine and sharing effect before the user confirms.

Data: Companion Settings and shared `src/components/ui.tsx` controls; existing scoped Settings support in the TUI.

Acceptance criteria:

- A person can paste an invite link, confirm joining and see the resulting membership.
- A person can restrict a project, group or session to allowed rooms and select its active room.
- The interface distinguishes membership, sharing and wake consent.
- Expired, used and revoked invitations produce useful, secret-free errors.
- UI tests cover confirmation, refusal, inherited limits and successful join.

Unknowns: none in the implemented Settings flow. Companion tests cover confirmation, invitation refusal and inherited restrictions.

## 5. Deployment and two-machine verification

Rationale: local tests do not prove that two independent gateways can exchange Council traffic.

Data: deploy the tested Worker with separate Rooms bindings. Use isolated gateways on a laptop and a second machine.
Private deployment details belong in `infrastructure`, never in this public repository.

Acceptance criteria:

- Gateway A creates a room and invitation. Gateway B joins through Settings or the same SDK protocol.
- An addressed Council question reaches B. B's reply resolves A's obligation.
- A disallowed group or session cannot send, read, discover or wake across the room boundary.
- Revocation and reconnect tests pass. Existing Push behavior remains healthy.
- Record the tested revisions, checks, commits and deployment outcome without secrets.

Unknowns: production deployment and physical two-machine verification remain pending. Cloudflare access, D1 creation and the second host JVM are verified.

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

## 8. Alternate hosting (deferred)

Rationale: self-hosted Python and AWS can implement the same Rooms protocol later.

Data: the canonical schema and SDK must not require D1-specific behavior from clients.

Acceptance criteria: alternate backends pass the shared protocol and concurrency tests before use.

Unknowns: no alternate backend or AWS deployment is authorized in the current Rooms scope.

## Plan state

- Self-wake fix: complete, verified and pushed in `65bba8f7b`.
- Rooms protocol, Worker, SDK, engine, scoped Settings and Companion interface are implemented.
- Verification: 674 affected JVM tests, 22 SDK tests, 94 documentation tests and 3396 Companion tests pass. One Companion test is skipped.
- Relay verification: 49 Worker tests, three deployment tests and 52 shared HTTPS tests pass. Dependency audit reports no vulnerabilities.
- Native verification: a fresh native build and the two-gateway Rooms suite pass, including the final receipt fix.
- Cloudflare: the Rooms database, administrator secret and private CI variable are provisioned. Production Push remains healthy.
- Deployment and physical two-machine verification remain pending. No live gateway was restarted.
- Cron, webhooks and alternate hosting remain deferred.
- No Rooms commit, push, Worker deployment or Vis product release has occurred.
