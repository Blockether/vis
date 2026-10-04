# Automations: schedules and webhooks

Start agent work on a schedule or from an external event, and report the result. Follow the Hermes Agent feature set where it fits Vis.

## Context

The previous plan, Council Rooms, is complete, deployed and verified on two machines (`580c5051b` to `d8183db51`).
Its open item is the D1 write permission of the relay CI token. Git history keeps that plan.

Hermes Agent (`NousResearch/hermes-agent`) has cron jobs and webhook routes. A job runs a prompt in a fresh session on a schedule.
A route checks a signature, filters the event, renders a prompt from the payload and starts a run.
Hermes records each attempt and delivers the result to a chat platform.

Vis already has the parts that one run needs: `state/create-session!`, `state/submit-turn-sync!`, `state/close-session!`, scoped Settings and Push.
Vis has no scheduler, no run ledger and no public inbound route.

Current paths:

- Contracts: `packages/vis-contract/resources/vis-contract/schema/automations.json` and `gateway.json`.
- Engine: `src/com/blockether/vis/internal/automation/` and `gateway/server/automations.clj`.
- Storage: `resources/db/sqlite/migration/V1__schema.sql`, `persistance/core.clj` and `persistance/sqlite/core.clj`.
- Clients: `packages/vis-agent/src/blockether/vis/engine/_client.py`, `apps/vis-companion/src/` and `apps/vis-tui/`.

Decisions:

- An automation has one or more triggers, one action and one delivery.
- Rooms and Council are not delivery targets. The session, Push and signed callbacks report the result.
- The setting `automations` is off by default. An explicit `false` on a project, group or session blocks runs there.
- A run that an automation started cannot create, change or delete automations.
- Webhooks arrive only at the gateway route `POST /v1/hooks/{automation_id}`. The relay does not carry webhooks.
- The gateway keeps every secret and checks every signature.

Rejected alternatives:

- Separate toggles for webhooks and agent management: one toggle and one fixed rule are easier to understand.
- A webhook inbox on the relay: webhooks belong in the gateway. Commit `5f2a59a4a` added the inbox, and phase 4 removes it.
- Script filters and script-only jobs: they run code for a webhook and need a separate security decision.
- Council or Room delivery: it adds a second result channel without a new capability.

## 1. Contract, schedules and run ledger

Rationale: every client uses one canonical protocol, and schedules run without a person.

Data:

- `automations.json`: `automation`, `automation_input`, `trigger` (`cron`, `every`, `once`, `webhook`), `target` (`session`, `new`, `temporary`), `delivery`, `run`, `run_event`, `webhook_result` and limits.
- Routes: `GET`/`POST /v1/automations`, `GET`/`PATCH`/`DELETE /v1/automations/{id}`, `POST /v1/automations/{id}/run`, `POST /v1/automations/{id}/secrets`, `GET /v1/automations/runs` and `GET /v1/automations/runs/{run_id}`.
- `automation/cron.clj`: five fields, names, lists, ranges, steps, macros, the Vixie OR rule and IANA time zones.
- A local time in a DST gap runs at the first instant after the gap. A repeated local time runs once.
- Tables `automation` and `automation_run`. A due trigger claims one run atomically. Missed fires do not run later.
- An overlapping run is `skipped`. A run that a stopped process left becomes `unknown` and never runs again.
- Targets: `session` queues a turn, `new` keeps a fresh session, and `temporary` keeps only the answer.
- The `automations` setting of the target scope (session, group or global) allows each run. The scheduler fires no schedule while the global setting is off.

Acceptance criteria: cron tests, including Europe/Warsaw DST; scheduler tests with a fake clock; route, contract and SDK tests; a run reaches `completed` with its answer through a stub model.

Unknowns: none.

## 2. Delivery

Rationale: the owner learns the result without watching the session.

Data:

- Push: the existing turn push. A completed run whose answer starts with `[SILENT]` sends no push and no callback. Failures always deliver.
- A temporary session sends an automation push, not a session push.
- Callback: Standard Webhooks headers and `v1,<base64 HMAC-SHA256>` signatures for `run.completed`, `run.failed`, `run.cancelled` and `run.skipped`.
- The persisted outbox uses a 10-second timeout. It retries after 30 seconds, 2 minutes, 10 minutes, 1 hour and 6 hours, and stops after 6 attempts. Each attempt keeps the message ID.

Acceptance criteria: a local receiver checks the signature; a receiver failure retries and then delivers once; `[SILENT]` suppresses delivery.

Unknowns: none.

## 3. Webhook triggers

Rationale: external events start work at once.

Data:

- `POST /v1/hooks/{automation_id}` is public. Its own signature check replaces the API token and protocol gates.
- Schemes: `github` (`X-Hub-Signature-256`), `standard` (Standard Webhooks), `generic` (`X-Webhook-Signature-V2` with a timestamp within 300 seconds) and `token` (bearer).
- An `events` filter and field filters (`equals`, `contains`, `in`) select deliveries. Prompt templates use `{dot.path}` and `{__raw__}`.
- One delivery ID starts at most one run while that run stays in the run ledger. Limits: 30 requests each minute and 1 MiB for each body.
- `deliver_only` sends the rendered text without a model. The answer is `202` with the run ID.

Acceptance criteria: route tests for each status; a repeated delivery returns the first run; the turn request marks payload text as untrusted.

Unknowns: event coalescing (later).

## 4. Remove the relay inbox

Rationale: webhooks belong in the gateway, not in the relay. A sender must reach the gateway route directly.

Data: remove the Worker `hooks` module, its D1 tables, the gateway poller and the `webhook.url` field. The wire shape keeps `webhook.path`.

Acceptance criteria: relay, engine, TUI and Companion tests pass without the inbox. The deployed relay answers `404` on the inbox routes and has no hook tables.

Unknowns: relay CI still lacks D1 write permission. A manual deployment works.

## 5. Interfaces and documentation

Data: a Companion Automations screen (list, create, edit, pause, run now, runs and a one-time secret); TUI `/automations`; a model tool for interactive sessions; a docs page.

Acceptance criteria: Companion unit and Storybook tests, TUI tests, documentation tests and Activity presentation tests.

Unknowns: none.

## 6. End-to-end verification

Data: a native suite with an owned gateway, a stub model and a local callback receiver.

Acceptance criteria: the schedule, webhook and callback paths pass on a fresh native build.

Unknowns: none.

## Plan state

- Phase 1: done. Cron tests with Europe/Warsaw DST, scheduler, route, contract and SDK tests pass. An isolated source gateway starts and answers the automation routes.
- Phase 2: done. A local receiver checks the Standard Webhooks signature. A failed callback retries, then arrives once, or stops after 6 attempts. `[SILENT]` sends nothing.
- Phase 3: done. Route and runner tests cover each webhook status, a repeated delivery and the untrusted-content note.
- Phase 4: done. Commit `2687ca926` removes the relay inbox. Relay tests pass (deploy 4 of 4, suite 50 of 50). The relay is deployed by hand: the four inbox routes answer `404`, and Push and Rooms still answer. The two hook tables are dropped from production D1. They held 3 test inboxes and no requests. The relay CI deploy still stops at the D1 migration with Cloudflare code 7403.
- Phase 5: done. The model tool `automations.*` has Activity presentation and refuses changes during an automation run. The TUI Automations view (command palette) runs, pauses, resumes, lists runs, creates one-time secrets and deletes. The Companion Automations screen does the same and also creates and edits automations in a form. The form sends only the changed fields and keeps webhook filters. Companion unit tests (lib 19, screen 14), Storybook tests (9) and the guide page contract pass.
- Phase 6: done. `native_automations_test` passes 4 of 4 cases on a fresh native build with commit `2687ca926`. The cases cover one-time and cron schedules, a GitHub webhook with forged and repeated deliveries, a Standard Webhooks request with a forged signature and a signed callback. Each webhook goes directly to the gateway route. `native_rooms_test` passes with the restored relay fixture.
