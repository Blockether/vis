# Automations

An automation gives Vis a prompt without a message from you. It starts on a schedule, at a set
time or when another service sends a webhook. Vis reports each result in a session, as a phone
notification or at an address that you choose. **Automations are off by default.**

## When to use

- **You want the same check or report every morning or every hour.** [Create an
  automation](#create-an-automation) with a [schedule](#triggers).
- **You want a reminder or one task at a later time.** Use a [one-time trigger](#triggers).
- **Vis must react to an event in another service, for example a new pull request.** [Start a
  run from a webhook](#start-a-run-from-a-webhook) and keep only the events that you need.
- **Your own system must receive each result.** [Receive callbacks](#receive-callbacks) at an
  address that you choose.
- **You want to test, pause or remove an automation.** [Manage your
  automations](#manage-automations) in the TUI or the app.

To follow a task while it runs, use [Sessions](sessions.md). To ask another session for help or a
second review, use [Council](council.md).

## Turn on automations

Automations are off on each gateway until you turn them on. The gateway is the background service
that the TUI and the app connect to.

1. Open **Settings** in the TUI or the app.
2. Turn on **Allow automations**.

You can also set the toggle in a configuration file, as in [Feature
toggles](configuration.md#feature-toggles). Run `/reload` after editing.

```yaml
toggles:
  automations: true   # default false; lets schedules and webhooks start turns
```

The same setting exists for a project, a group and a session. Set it to `false` there to block
runs in that scope. A blocked run ends as `skipped` with the reason `settings`. A lower scope cannot
turn automations on when the global value or a higher scope is off. [Project, group and session
settings](configuration.md#project-group-and-session-settings) explains the scopes.

Vis runs automations only while the gateway runs. If the gateway stops during a run, the run ends
as `unknown`.

## Create an automation

Ask Vis in a chat. Say when the automation runs, what Vis does and where you want the result. For
example:

```text
Every weekday at 8:00, list the open pull requests in this repository that wait for a review.
Send the list to my phone.
```

```text
At 17:30 today, check whether the release build passed and tell me the result.
```

Vis creates the automation with its `automations` tool and shows you the result. To change it
later, ask again, for example `Move the morning review list to 7:30.` The tool is available only
while automations are on. A run that an automation started cannot create, change or delete
automations.

You can also create an automation in the app. Select the **Open automations** icon, choose the
machine and select **New automation**. Fill in the name, the prompt and the triggers, then select
**Create automation**. To change an automation, open it and select **Edit**. The form keeps the
[filters](#filter-events) of a webhook trigger. To change them, ask Vis in a chat.

Each automation has these parts:

| Part | What it sets |
|---|---|
| `name` | The name in lists and notifications. |
| `triggers` | One to eight [triggers](#triggers) that start a run. |
| `prompt` | The request for each run, up to 16384 bytes. A webhook can [fill it from the payload](#use-the-payload-in-the-prompt). |
| `target` | The [session](#targets) where a run works. |
| `delivery` | How you [get the results](#get-the-results). |
| `model` | Optional. A fixed `provider` and `model` for each run. Without it, Vis chooses the model as for any other turn. |
| `deliver_only` | Optional. Vis sends the prompt as the answer and does not call a model. Use it to forward a webhook to your phone. |
| `enabled` | Whether the triggers start runs. A paused automation does not start runs from its triggers. |

## Triggers

| Trigger | When it starts a run | Example |
|---|---|---|
| `cron` | At the times of a five-field cron expression: minute, hour, day of month, month and day of week | `{"kind": "cron", "expression": "0 8 * * 1-5", "timezone": "Europe/Warsaw"}` |
| `every` | At a fixed interval from the creation time, from 60 seconds to one year | `{"kind": "every", "seconds": 3600}` |
| `once` | One time, at a time in milliseconds since 1970 UTC | `{"kind": "once", "at": 1767254400000}` |
| `webhook` | When a signed request arrives, see [Start a run from a webhook](#start-a-run-from-a-webhook) | `{"kind": "webhook", "signature": "github"}` |

A `cron` trigger uses its `timezone`, or the time zone of the gateway. It also accepts `@hourly`,
`@daily`, `@weekly`, `@monthly` and `@yearly`. When a clock change skips a time, the run starts at
the end of the gap. When a time occurs twice, the run starts only at the first one. An automation
can have only one `webhook` trigger.

Vis does not catch up on missed schedules. If the gateway was stopped at a scheduled time, Vis
waits for the next time. A missed `once` trigger still runs if it is at most one day late.

An automation works on one run at a time. If a scheduled time arrives while a run still works, Vis
skips the new run with the reason `overlap`. Webhook and manual runs wait in a queue of up to 8
runs for each automation. When the queue is full, Vis skips the run with the reason `queue_full`.
One gateway works on at most 4 runs at the same time.

## Targets

| `mode` | What a run does | Use it for |
|---|---|---|
| `session` | Adds a turn to the session in `session_id` | A running log or a task that needs earlier context |
| `new` | Creates a session for each run and keeps it. `root` sets the project folder and `group_id` the sidebar group | Results that you want to read and continue later |
| `temporary` | Creates a session for the run and deletes it after the run. The run keeps the answer | Checks that need no history |

A run cannot ask you questions. If a run asks for input, it fails with the error `The run asked for
input. An automation cannot answer questions.` If the target session no longer exists, the run
fails with `The target session does not exist.`

## Get the results

Vis records the status and the answer of each run. You find the result in these places:

- **The session.** With the `session` and `new` targets, the answer stays in the session as a
  normal turn.
- **Runs.** The [automation views](#manage-automations) show the last 200 runs of each automation.
- **A notification.** Push is on by default. The Vis app on your phone shows a notification for
  each run that Vis did not skip.
- **A callback.** Vis sends each run event to an address that you choose. See [Receive
  callbacks](#receive-callbacks).

To report only changes, tell Vis to start its answer with `[SILENT]` when nothing changed. Vis then
sends no notification and no callback for that completed run. The run keeps its answer. A failed
run always reports.

| Status | Meaning |
|---|---|
| `queued` | The run waits for its turn. |
| `running` | Vis works on the run. |
| `completed` | The run finished with an answer. |
| `failed` | The run stopped with an error. |
| `cancelled` | The turn of the run was cancelled. |
| `skipped` | Vis did not start the run. The reason is `overlap`, `queue_full` or `settings`. |
| `unknown` | The gateway stopped before the run finished. |

### Receive callbacks

Add a callback with an `http` or `https` address to the delivery. Vis sends a `POST` request with a
JSON body for each run event:

```json
{
  "type": "run.completed",
  "timestamp": 1767254460000,
  "data": {
    "id": "6f1c2a90-4b7e-4d2f-9a51-3c8e0b7d4e21",
    "automation_id": "0b8e5d7c-1f2a-4c3b-8d9e-5a6f7b8c9d0e",
    "automation_name": "Morning review list",
    "trigger": "cron",
    "status": "completed",
    "reason": null,
    "scheduled_at": 1767254400000,
    "created_at": 1767254400000,
    "started_at": 1767254400150,
    "finished_at": 1767254460000,
    "session_id": "c3d4e5f6-0718-4a9b-8c7d-6e5f4a3b2c1d",
    "turn_id": "9a8b7c6d-5e4f-4a3b-9c2d-1e0f9a8b7c6d",
    "answer": "Three pull requests wait for a review.",
    "error": null,
    "is_silent": false
  }
}
```

The `type` is `run.completed`, `run.failed`, `run.cancelled`, `run.skipped` or `run.unknown`. Set
`events` in the callback to receive only some types. Without `events`, Vis sends every type.

Create a callback secret in the [automation views](#manage-automations) to sign each request. Vis
then sends the [Standard Webhooks](https://www.standardwebhooks.com/) headers `webhook-id`,
`webhook-timestamp` and `webhook-signature`. Without a callback secret, the requests have no
signature. This Python function checks a signed request:

```python
import base64
import hashlib
import hmac


def is_from_vis(secret: str, headers: dict[str, str], body: bytes) -> bool:
    """Check the Standard Webhooks signature of one Vis callback."""
    key = base64.b64decode(secret.removeprefix("whsec_"))
    signed = f"{headers['webhook-id']}.{headers['webhook-timestamp']}.".encode() + body
    digest = hmac.new(key, signed, hashlib.sha256).digest()
    expected = "v1," + base64.b64encode(digest).decode()
    return any(hmac.compare_digest(expected, item) for item in headers["webhook-signature"].split())
```

Answer with a `2xx` status within 10 seconds. Otherwise Vis tries again after 30 seconds, 2
minutes, 10 minutes, 1 hour and 6 hours. It stops after 6 attempts. A repeated attempt keeps its
`webhook-id`, so your receiver can drop a copy.

## Start a run from a webhook

A webhook trigger starts a run when another service sends a signed request.

1. Ask Vis for an automation with a webhook trigger. Name the service and the events, for example
   `When GitHub reports a new pull request for main, review it in a new session.` In the app form,
   add a **Webhook** trigger instead.
2. Open the automation in the [automation views](#manage-automations). Select **Create webhook
   secret** and copy the secret. Vis shows it only once.
3. Copy the webhook address. The app shows it as **Webhook address**, and the TUI shows it in
   **Details**. With the relay, the address is on the relay. Without it, the address is the
   gateway address followed by `/v1/hooks/<automation-id>`.
4. In the other service, add a webhook with this address, the content type `application/json` and
   the secret.

The other service must reach the webhook address. A gateway on a laptop is usually not reachable
from the internet, so Vis receives the webhook through the
[relay](#receive-webhooks-through-the-relay). A gateway on a server can also receive webhooks
directly. [Running a gateway](gateway-service.md) explains how to run a gateway on a server.

### Receive webhooks through the relay

The relay is the service that also sends Push notifications to the app. It keeps a private inbox
for each gateway. A sender calls the inbox address, and the relay stores the request. The gateway
collects the stored requests, checks each signature and starts the runs.

- The gateway creates its inbox when an enabled automation has a webhook trigger and a webhook
  secret. The `automations` setting must also be on.
- The webhook address on the relay is `<relay>/hooks/<inbox-id>/<automation-id>`.
- The relay answers the sender with `202` and `"status": "stored"`. The sender does not see the
  result of the signature check.
- The gateway asks for new requests every 5 to 60 seconds. A run can start up to one minute after
  the request arrives.
- The timestamp check uses the time when the relay received the request.

By default, Vis uses the publisher's relay at `https://vis.relay.blockether.com`. The relay
operator can read the stored requests, so do not put secrets in a webhook body. The relay deletes a
request when the gateway collects it, or after 7 days. To use your own relay, set
`VIS_PUSH_RELAY_URL` or put `{:url "https://relay.example.com"}` in `~/.vis/relay.edn`. An empty
value turns off the relay for webhooks and for Push.

### Signatures

| `signature` | What Vis checks | Senders |
|---|---|---|
| `github` | `X-Hub-Signature-256`: `sha256=` and a hex HMAC-SHA256 of the body | GitHub |
| `standard` | `webhook-id`, `webhook-timestamp` and `webhook-signature`, as in Standard Webhooks | Services that follow Standard Webhooks |
| `generic` | `X-Webhook-Timestamp` and `X-Webhook-Signature-V2`: a hex HMAC-SHA256 of `<timestamp>.<body>` | Your own scripts |
| `token` | The secret itself in `X-Gitlab-Token`, `X-Webhook-Token` or `Authorization: Bearer` | GitLab and services without signatures |

The `github` and `generic` kinds use the whole secret as the HMAC key. The `standard` kind uses the
base64-decoded part after `whsec_`. A `standard` or `generic` timestamp is in seconds. It must be
within 300 seconds of the gateway clock.

This script sends a `generic` request:

```bash
body='{"type":"deploy","environment":"production"}'
ts=$(date +%s)
sig=$(printf '%s.%s' "$ts" "$body" | openssl dgst -sha256 -hmac "$SECRET" -hex | sed 's/^.* //')
curl -X POST "https://gateway.example.com/v1/hooks/$AUTOMATION_ID" \
  -H 'content-type: application/json' \
  -H "X-Webhook-Timestamp: $ts" \
  -H "X-Webhook-Signature-V2: $sig" \
  --data "$body"
```

### Filter events

An empty `events` list accepts every event. Otherwise, Vis reads the event name from the
`X-GitHub-Event`, `X-Gitlab-Event` or `X-Webhook-Event` header. Without these headers, it uses the
`type`, `event_type` or `object_kind` field of the payload. An entry matches the event name, or the
name and the `action` field, for example `pull_request.opened`.

Each filter reads one `field` of the payload as a dot path, for example `pull_request.base.ref` or
`commits.0.id`. It compares the value with `equals`, `contains` or `in`. A run starts only when
every filter passes.

```json
{
  "kind": "webhook",
  "signature": "github",
  "events": ["pull_request.opened", "pull_request.synchronize"],
  "filters": [
    {"field": "pull_request.base.ref", "equals": "main"},
    {"field": "pull_request.draft", "equals": false}
  ]
}
```

### Use the payload in the prompt

Write `{dot.path}` in the prompt to insert a value from the payload. Write `{__raw__}` to insert the
whole body. A path that does not exist stays as written. Vis clips each value to 4000 bytes.

```text
Review pull request {pull_request.number}: {pull_request.title}. The diff is at {pull_request.diff_url}.
```

Vis marks the text from a webhook as untrusted, so the agent treats it as data and not as
instructions. The payload fills only the prompt. It cannot choose the target, the model or a tool.

### Check a delivery

| Answer | Meaning |
|---|---|
| `202` and `"status": "stored"` | The relay stored the request for the gateway. |
| `202` and `"status": "accepted"` | Vis queued a run. `run_id` names the run. |
| `202` and `"status": "ignored"` | Vis started no run. The `reason` is `disabled`, `event` or `filter`. |
| `200` and `"status": "duplicate"` | Vis already received this delivery. |
| `401` | The automation has no webhook secret, or the signature or the timestamp is not valid. |
| `404` | The automation does not exist or has no webhook trigger. |
| `413` | The body is larger than 1 MiB. |
| `429` | The automation received more than 30 requests in one minute. |

Vis drops a repeated delivery by its delivery ID. It reads the ID from `X-GitHub-Delivery`,
`webhook-id`, `X-Gitlab-Event-UUID`, `Idempotency-Key` or `X-Request-Id`.

The relay answers `404` for an unknown inbox. It answers `429` when the inbox is full or receives
more than 120 requests in one minute. A request with a bad signature starts no run, and only the
gateway log shows the reason.

## Manage automations

In the TUI, open the command palette and choose **Automations**. Select an automation to see
**Details**, **Run now**, **Pause** or **Resume**, **Runs** and **Delete**. An automation with a
webhook trigger also shows **Create webhook secret**. An automation with a callback also shows
**Create callback secret**.

In the app, select the **Open automations** icon in the header. The icon shows when a connected
machine allows automations or has automations. Choose the machine, then select an automation. The
app shows its triggers, next run, target, delivery, webhook address and recent runs. It has buttons
for the same actions. **New automation** and **Edit** open the form from [Create an
automation](#create-an-automation).

**Run now** starts a manual run at once. **Pause** stops the triggers until you select **Resume**.
**Replace webhook secret** and **Replace callback secret** create a new secret. The old secret
stops working at once, so update the other service right after the change.

## Limits

| Limit | Value |
|---|---|
| Automations on one gateway | 256 |
| Triggers in one automation | 8 |
| Prompt size | 16384 bytes |
| Shortest `every` interval | 60 seconds |
| Runs kept for each automation | 200 |
| Runs that work at the same time | 4 |
| Queued runs for each automation | 8 |
| Webhook body | 1 MiB |
| Webhook requests for each automation | 30 in one minute |
| Webhook timestamp window | 300 seconds |
| One payload value in a prompt | 4000 bytes |
| Callback attempts | 6, each with a 10-second timeout |
| Requests that wait in a relay inbox | 100, together at most 8 MiB |
| Time that the relay keeps a request | 7 days |
| Webhook requests for each relay inbox | 120 in one minute |
| Time until the relay deletes an unused inbox | 30 days |

## HTTP reference

These routes use the normal gateway authentication. The webhook route is the exception: the
signature of the request replaces it. The [Python SDK](python-sdk.md) has a method for each route,
for example `get_automations` and `post_automation_run`.

| Method and path | What it does |
|---|---|
| `GET /v1/automations` | Lists the automations and `is_enabled`, the global setting. |
| `POST /v1/automations` | Creates an automation. |
| `GET /v1/automations/{id}` | Reads one automation. |
| `PATCH /v1/automations/{id}` | Replaces the given top-level fields. |
| `DELETE /v1/automations/{id}` | Deletes an automation and its runs. |
| `POST /v1/automations/{id}/run` | Starts a manual run. |
| `POST /v1/automations/{id}/secrets` | Creates a `webhook` or `callback` secret. The answer is the only copy. |
| `GET /v1/automations/runs` | Lists runs. Filter with `automation_id`, `status`, `session_id` and `limit`. |
| `GET /v1/automations/runs/{run_id}` | Reads one run. |
| `POST /v1/hooks/{id}` | Receives a webhook. |

The relay has its own routes. The gateway uses them, so you need them only for your own relay
client.

| Method and path | What it does |
|---|---|
| `POST /hooks/{inbox_id}/{id}` | Stores a webhook request. Senders call this route. |
| `POST /v1/hooks/inboxes` | Creates an inbox. The answer has `inbox_id` and `token`. |
| `GET /v1/hooks/inbox` | Returns up to 20 stored requests. It needs `Authorization: Bearer <token>`. |
| `POST /v1/hooks/inbox/ack` | Deletes the stored requests with the given `ids`. It needs the same token. |

This body creates the automation from the first example:

```json
{
  "name": "Morning review list",
  "triggers": [{"kind": "cron", "expression": "0 8 * * 1-5", "timezone": "Europe/Warsaw"}],
  "prompt": "List the open pull requests that wait for a review. If there are none, answer [SILENT].",
  "target": {"mode": "temporary", "root": "/srv/projects/shop"},
  "delivery": {
    "push": true,
    "callback": {"url": "http://10.0.0.5:9000/vis-results", "events": ["run.completed", "run.failed"]}
  }
}
```

## See also

- [Configuration](configuration.md) — the `automations` toggle and the settings of a project, a group
  or a session.
- [Sessions](sessions.md) — find and continue the sessions that runs create.
- [Running a gateway](gateway-service.md) — keep a gateway online, so that schedules and webhooks
  work.
- [Python SDK](python-sdk.md) — call the automation routes from your own code.
