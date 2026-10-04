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
- **Your own program must create and control automations.** Use the [Python SDK](#python-sdk) or
  the [HTTP API](#http-api). Both have the same operations.

To follow a task while it runs, use [Sessions](sessions.md). To ask another session for help or a
second review, use [Council](council.md).

## How automations work

An automation is a saved prompt with three rules: when it runs, where the agent works and how you
get the result. Each time a trigger fires, Vis creates a run. The run gives the prompt to the agent
in the target session. Vis records the status and the answer, then sends the result to you.

Automations support these features:

- **Schedules.** Cron expressions with time zones, fixed intervals and one-time runs.
- **Webhooks.** Runs from GitHub, GitLab, Standard Webhooks senders and your own scripts. Vis checks
  the signature, filters the events and can put payload values in the prompt.
- **Targets.** An existing session, a new session for each run or a temporary session.
- **Delivery.** Phone notifications and signed callbacks. `[SILENT]` reports only changes.
- **Control.** Manual runs, pause and resume, secrets and the last 200 runs of each automation.
- **Access.** A chat, the TUI, the app, the [Python SDK](#python-sdk) and the [HTTP API](#http-api).

Each automation has these parts:

| Part | What it sets |
|---|---|
| `name` | The name in lists and notifications, up to 128 characters. |
| `triggers` | One to eight [triggers](#triggers) that start a run. |
| `prompt` | The request for each run, up to 16384 bytes. A webhook can [fill it from the payload](#use-the-payload-in-the-prompt). |
| `target` | The [session](#targets) where a run works. |
| `delivery` | How you [get the results](#runs-and-results). |
| `model` | Optional. A fixed `provider` and `model` for each run. Without it, Vis chooses the model as for any other turn. |
| `deliver_only` | Optional. Vis sends the prompt as the answer and does not call a model. Use it to forward a webhook to your phone. |
| `enabled` | Whether the triggers start runs. A paused automation does not start runs from its triggers. |

### Triggers

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

### Targets

| `mode` | What a run does | Use it for |
|---|---|---|
| `session` | Adds a turn to the session in `session_id` | A running log or a task that needs earlier context |
| `new` | Creates a session for each run and keeps it. `root` sets the project folder and `group_id` the sidebar group | Results that you want to read and continue later |
| `temporary` | Creates a session for the run and deletes it after the run. The run keeps the answer | Checks that need no history |

A run cannot ask you questions. If a run asks for input, it fails with the error `The run asked for
input. An automation cannot answer questions.` If the target session no longer exists, the run
fails with `The target session does not exist.`

### Runs and results

Vis records the status and the answer of each run. You find the result in these places:

- **The session.** With the `session` and `new` targets, the answer stays in the session as a
  normal turn.
- **Runs.** The [automation views](#manage-automations) show the last 200 runs of each automation.
  Your program reads them with the [Python SDK](#read-runs-in-python) or the [HTTP
  API](#read-runs-over-http).
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

To create automations from your own program, use the [Python
SDK](#create-an-automation-in-python) or the [HTTP API](#create-an-automation-over-http).
[How automations work](#how-automations-work) lists the parts of an automation.

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

## Receive callbacks

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
3. Copy the webhook address. The app shows it as **Webhook address**. It is the gateway address
   followed by `/v1/hooks/<automation-id>`.
4. In the other service, add a webhook with this address, the content type `application/json` and
   the secret.

The other service must reach your gateway at this address. A gateway on a laptop is usually not
reachable from the internet. [Running a gateway](gateway-service.md) explains how to run a gateway
on a server.

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

To send a signed request from your own code, see [Send a webhook from
Python](#send-a-webhook-from-python) or [Send a webhook over HTTP](#send-a-webhook-over-http).

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
| `202` and `"status": "accepted"` | Vis queued a run. `run_id` names the run. |
| `202` and `"status": "ignored"` | Vis started no run. The `reason` is `disabled`, `event` or `filter`. |
| `200` and `"status": "duplicate"` | Vis already received this delivery. |
| `401` | The automation has no webhook secret, or the signature or the timestamp is not valid. |
| `404` | The automation does not exist or has no webhook trigger. |
| `413` | The body is larger than 1 MiB. |
| `429` | The automation received more than 30 requests with a valid signature in one minute. Requests that fail the check do not count. |

Vis drops a repeated delivery by its delivery ID. It reads the ID from `X-GitHub-Delivery`,
`webhook-id`, `X-Gitlab-Event-UUID`, `Idempotency-Key` or `X-Request-Id`.

## Python SDK

`GatewayClient` has one method for each automation route. A method sends the same request as the
[HTTP API](#http-api) and returns the parsed JSON answer. You give a body or a query as a Python
`dict` with the same fields.

| Task | Method |
|---|---|
| List automations | `get_automations()` |
| Read one automation | `get_automation(automation_id)` |
| Create an automation | `post_automations(body=...)` |
| Change, pause or resume an automation | `patch_automation(automation_id, body=...)` |
| Start a run now | `post_automation_run(automation_id)` |
| Create a secret | `post_automation_secrets(automation_id, body=...)` |
| List runs | `get_automation_runs(query=...)` |
| Read one run | `get_automation_run(run_id)` |
| Delete an automation | `delete_automation(automation_id)` |
| Send a webhook | Any HTTP library, see [Send a webhook from Python](#send-a-webhook-from-python) |

### Connect a Python client

[Install the SDK](python-sdk.md#install-the-sdk). Then set `VIS_GATEWAY_URL` and
`VIS_GATEWAY_TOKEN` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task).

```python
import os

from blockether.vis.engine import GatewayClient

with GatewayClient(os.environ["VIS_GATEWAY_URL"], token=os.environ["VIS_GATEWAY_TOKEN"]) as client:
    listing = client.get_automations()
    print("Automations allowed:", listing["is_enabled"])
```

When the `with` block starts, the client calls `get_capabilities()` and checks the protocol. It then
sends the token with each request. The next examples are calls on this `client` inside the `with`
block.

### List and read automations in Python

```python
listing = client.get_automations()
for automation in listing["automations"]:
    print(automation["id"], automation["name"], automation["enabled"], automation["next_run_at"])

automation = client.get_automation(automation_id)
print(automation["webhook"], automation["secrets"], automation["last_run"])
```

`is_enabled` is the global `automations` setting. Each automation has the [parts](#how-automations-work)
that you set and these fields:

- `next_run_at` is the next scheduled time in milliseconds since 1970 UTC, or `None`.
- `webhook` has the `path` of the webhook address, or is `None`.
- `secrets` tells whether a `webhook` secret and a `callback` secret exist. It never contains a secret.
- `last_run` is the newest run, or `None`.

### Create an automation in Python

```python
automation = client.post_automations(
    body={
        "name": "Morning review list",
        "triggers": [{"kind": "cron", "expression": "0 8 * * 1-5", "timezone": "Europe/Warsaw"}],
        "prompt": "List the open pull requests that wait for a review. If there are none, answer [SILENT].",
        "target": {"mode": "temporary", "root": "/srv/projects/shop"},
        "delivery": {
            "push": True,
            "callback": {
                "url": "http://10.0.0.5:9000/vis-results",
                "events": ["run.completed", "run.failed"],
            },
        },
    }
)
automation_id = automation["id"]
```

The body needs `name`, `triggers`, `prompt` and `target`. `enabled` and `push` are `True` when you
do not send them. The answer is the new automation with its `id`.

### Change or pause an automation in Python

```python
client.patch_automation(automation_id, body={"enabled": False})  # pause
client.patch_automation(automation_id, body={"enabled": True})  # resume
client.patch_automation(
    automation_id,
    body={"triggers": [{"kind": "cron", "expression": "30 7 * * 1-5", "timezone": "Europe/Warsaw"}]},
)
```

The body changes only the fields that it names. Vis replaces `triggers`, `target` and `delivery` as a
whole, so send the full new value. The answer is the changed automation.

### Start a run in Python

```python
run = client.post_automation_run(automation_id)
print(run["id"], run["status"])
```

The answer is the new run with the trigger `manual`. A manual run waits in the same queue as a
webhook run.

### Create a secret in Python

```python
created = client.post_automation_secrets(automation_id, body={"kind": "webhook"})
webhook_secret = created["secret"]
```

The `kind` is `webhook` or `callback`. The answer is the only copy of the secret, so store it at
once. A new secret replaces the old secret immediately.

### Read runs in Python

```python
runs = client.get_automation_runs(query={"automation_id": automation_id, "status": "failed", "limit": 20})
for run in runs["runs"]:
    print(run["id"], run["trigger"], run["status"], run["error"])

run = client.get_automation_run(run_id)
print(run["answer"])
```

The list starts with the newest run. You can filter by `automation_id`, `status` and `session_id`.
`limit` is from 1 to 200, and the default is 50. A run has the fields of the `data` object in
[Receive callbacks](#receive-callbacks).

### Delete an automation in Python

```python
deleted = client.delete_automation(automation_id)
print(deleted["is_deleted"])
```

Vis deletes the automation and its runs.

### Handle errors in Python

```python
from blockether.vis.engine import GatewayError

try:
    client.get_automation("0b8e5d7c-1f2a-4c3b-8d9e-5a6f7b8c9d0e")
except GatewayError as error:
    print(error.status, error.code)  # 404 not-found
```

A failed request raises `GatewayError` with the HTTP `status` and the error `code`. The codes are in
[Handle HTTP errors](#handle-http-errors). The exception does not contain the message of the
gateway. A connection failure raises `TransportError`.

### Send a webhook from Python

The webhook route is public, because the signature replaces the gateway token. `GatewayClient` has
no method for it. This example sends a `generic` request with the standard library:

```python
import hashlib
import hmac
import json
import os
import time
import urllib.request

secret = os.environ["VIS_WEBHOOK_SECRET"].encode()
url = "https://gateway.example.com/v1/hooks/" + os.environ["VIS_AUTOMATION_ID"]
body = json.dumps({"type": "deploy", "environment": "production"}).encode()
timestamp = str(int(time.time()))
signature = hmac.new(secret, timestamp.encode() + b"." + body, hashlib.sha256).hexdigest()
request = urllib.request.Request(
    url,
    data=body,
    method="POST",
    headers={
        "content-type": "application/json",
        "X-Webhook-Timestamp": timestamp,
        "X-Webhook-Signature-V2": signature,
    },
)
with urllib.request.urlopen(request, timeout=10) as response:
    print(response.status, json.load(response))
```

[Check a delivery](#check-a-delivery) explains the answer.

## HTTP API

The HTTP API has the same operations as the [Python SDK](#python-sdk). Requests and answers are
JSON.

| Task | Method and path |
|---|---|
| List automations | `GET /v1/automations` |
| Read one automation | `GET /v1/automations/{id}` |
| Create an automation | `POST /v1/automations` |
| Change, pause or resume an automation | `PATCH /v1/automations/{id}` |
| Start a run now | `POST /v1/automations/{id}/run` |
| Create a secret | `POST /v1/automations/{id}/secrets` |
| List runs | `GET /v1/automations/runs` |
| Read one run | `GET /v1/automations/runs/{run_id}` |
| Delete an automation | `DELETE /v1/automations/{id}` |
| Send a webhook | `POST /v1/hooks/{id}` |

### Authenticate HTTP requests

Each request needs these headers. The webhook route is the exception.

- `x-vis-protocol` with the protocol number of the gateway. `GET /v1/capabilities` returns it as
  `protocol.protocol`.
- `Authorization: Bearer <token>` when the gateway requires a token. [Tokens and HTTP
  401](gateway-service.md#tokens-and-http-401) tells when it does.

This setup reads the token and the protocol number once. The next examples use the `vis_api`
function. Do not print or commit the token.

```bash
export VIS_GATEWAY_URL=http://127.0.0.1:7890
export VIS_GATEWAY_TOKEN="$(cat "$HOME/.vis/gateway.token")"
export VIS_PROTOCOL="$(curl -sS -H "Authorization: Bearer $VIS_GATEWAY_TOKEN" \
  "$VIS_GATEWAY_URL/v1/capabilities" |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["protocol"]["protocol"])')"

vis_api() {
  curl -sS -H "x-vis-protocol: $VIS_PROTOCOL" -H "Authorization: Bearer $VIS_GATEWAY_TOKEN" "$@"
}
```

### List and read automations over HTTP

```bash
vis_api "$VIS_GATEWAY_URL/v1/automations"
vis_api "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID"
```

The list has `automations` and `is_enabled`, the global `automations` setting. Each automation has
the [parts](#how-automations-work) that you set and these fields:

- `next_run_at` is the next scheduled time in milliseconds since 1970 UTC, or `null`.
- `webhook` has the `path` of the webhook address, or is `null`.
- `secrets` tells whether a `webhook` secret and a `callback` secret exist. It never contains a secret.
- `last_run` is the newest run, or `null`.

### Create an automation over HTTP

Save this body as `morning-review.json`:

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

Then send it:

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/automations" \
  -H 'content-type: application/json' --data @morning-review.json
```

The body needs `name`, `triggers`, `prompt` and `target`. `enabled` and `push` are `true` when you
do not send them. The answer is the new automation with its `id`.

### Change or pause an automation over HTTP

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID" \
  -H 'content-type: application/json' --data '{"enabled": false}'
```

Send `{"enabled": true}` to resume. The body changes only the fields that it names. Vis replaces
`triggers`, `target` and `delivery` as a whole, so send the full new value. The answer is the
changed automation.

### Start a run over HTTP

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID/run"
```

The answer is the new run with the trigger `manual`. A manual run waits in the same queue as a
webhook run.

### Create a secret over HTTP

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID/secrets" \
  -H 'content-type: application/json' --data '{"kind": "webhook"}'
```

The `kind` is `webhook` or `callback`. The answer has `kind` and `secret`. It is the only copy of
the secret, so store it at once. A new secret replaces the old secret immediately.

### Read runs over HTTP

```bash
vis_api "$VIS_GATEWAY_URL/v1/automations/runs?automation_id=$AUTOMATION_ID&status=failed&limit=20"
vis_api "$VIS_GATEWAY_URL/v1/automations/runs/$RUN_ID"
```

The list has `runs` and starts with the newest run. You can filter by `automation_id`, `status` and
`session_id`. `limit` is from 1 to 200, and the default is 50. A run has the fields of the `data`
object in [Receive callbacks](#receive-callbacks).

### Delete an automation over HTTP

```bash
vis_api -X DELETE "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID"
```

Vis deletes the automation and its runs. The answer has `id` and `is_deleted`.

### Handle HTTP errors

An error answer has this body:

```json
{"error": {"type": "not-found", "message": "Automation not found"}}
```

| Status | `type` | Meaning |
|---|---|---|
| `400` | `invalid-automation` | The body is not valid. The `message` names the problem. |
| `401` | `unauthorized` | The token is missing or not correct. |
| `404` | `not-found` | The automation or the run does not exist. |
| `409` | `automation-limit` | The gateway already has 256 automations. |
| `426` | `incompatible_protocol` | The `x-vis-protocol` header is missing or not supported. |

### Send a webhook over HTTP

The webhook route needs no gateway token and no `x-vis-protocol` header. The signature replaces
them. This script sends a `generic` request:

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

[Check a delivery](#check-a-delivery) explains the answer.

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

## See also

- [Configuration](configuration.md) — the `automations` toggle and the settings of a project, a group
  or a session.
- [Sessions](sessions.md) — find and continue the sessions that runs create.
- [Running a gateway](gateway-service.md) — keep a gateway online, so that schedules and webhooks
  work.
- [Python SDK](python-sdk.md) — install the SDK and run agents from your own code.
