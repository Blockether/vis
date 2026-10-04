# Automations API

Create, change and run [automations](automations.md) from your own program.

<div data-variant="python">

Each `GatewayClient` method sends one gateway request and returns the parsed JSON answer.

</div>

<div data-variant="http">

The requests and answers are JSON.

</div>

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A script must create the same automations on several gateways.** [Create an
  automation](#create-an-automation).
- **A deployment must pause a schedule and resume it later.** [Change or pause an
  automation](#change-or-pause-an-automation).
- **Your monitoring must find failed runs.** [Read runs](#read-runs) and filter them by status.
- **A test must start a run now or send a webhook.** [Start a run](#start-a-run) or [send a
  webhook](#send-a-webhook).

To learn what an automation does and how to create one in the app, read
[Automations](automations.md). To send the same requests from another language, select **HTTP** at
the top of the page.

## Before you start

<div data-variant="python">

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

The next examples are calls on this `client` inside the `with` block.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. The webhook route is the exception: it uses a webhook secret, not the gateway
token.

</div>

## Operations

<div data-variant="python">

`GatewayClient` has one method for each automation route. A method sends the same request as the
[HTTP API](http-api.md) and returns the parsed JSON answer. You give a body or a query as a Python
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
| Send a webhook | Any HTTP library, see [Send a webhook](#send-a-webhook) |

</div>

<div data-variant="http">

Requests and answers are JSON.

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

</div>

## List and read automations

<div data-variant="python">

```python
listing = client.get_automations()
for automation in listing["automations"]:
    print(automation["id"], automation["name"], automation["enabled"], automation["next_run_at"])

automation = client.get_automation(automation_id)
print(automation["webhook"], automation["secrets"], automation["last_run"])
```

`is_enabled` is the global `automations` setting. Each automation has the [parts](automations.md#how-automations-work)
that you set and these fields:

- `next_run_at` is the next scheduled time in milliseconds since 1970 UTC, or `None`.
- `webhook` has the `path` of the webhook address, or is `None`.
- `secrets` tells whether a `webhook` secret and a `callback` secret exist. It never contains a secret.
- `last_run` is the newest run, or `None`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/automations"
vis_api "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID"
```

The list has `automations` and `is_enabled`, the global `automations` setting. Each automation has
the [parts](automations.md#how-automations-work) that you set and these fields:

- `next_run_at` is the next scheduled time in milliseconds since 1970 UTC, or `null`.
- `webhook` has the `path` of the webhook address, or is `null`.
- `secrets` tells whether a `webhook` secret and a `callback` secret exist. It never contains a secret.
- `last_run` is the newest run, or `null`.

</div>

## Create an automation

<div data-variant="python">

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

</div>

<div data-variant="http">

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

</div>

## Change or pause an automation

<div data-variant="python">

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

</div>

<div data-variant="http">

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID" \
  -H 'content-type: application/json' --data '{"enabled": false}'
```

Send `{"enabled": true}` to resume. The body changes only the fields that it names. Vis replaces
`triggers`, `target` and `delivery` as a whole, so send the full new value. The answer is the
changed automation.

</div>

## Start a run

<div data-variant="python">

```python
run = client.post_automation_run(automation_id)
print(run["id"], run["status"])
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID/run"
```

</div>

The answer is the new run with the trigger `manual`. A manual run waits in the same queue as a
webhook run.

## Create a secret

<div data-variant="python">

```python
created = client.post_automation_secrets(automation_id, body={"kind": "webhook"})
webhook_secret = created["secret"]
```

The `kind` is `webhook` or `callback`. The answer is the only copy of the secret, so store it at
once. A new secret replaces the old secret immediately.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID/secrets" \
  -H 'content-type: application/json' --data '{"kind": "webhook"}'
```

The `kind` is `webhook` or `callback`. The answer has `kind` and `secret`. It is the only copy of
the secret, so store it at once. A new secret replaces the old secret immediately.

</div>

## Read runs

<div data-variant="python">

```python
runs = client.get_automation_runs(query={"automation_id": automation_id, "status": "failed", "limit": 20})
for run in runs["runs"]:
    print(run["id"], run["trigger"], run["status"], run["error"])

run = client.get_automation_run(run_id)
print(run["answer"])
```

The list starts with the newest run. You can filter by `automation_id`, `status` and `session_id`.
`limit` is from 1 to 200, and the default is 50. A run has the fields of the `data` object in
[Receive callbacks](automations.md#receive-callbacks).

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/automations/runs?automation_id=$AUTOMATION_ID&status=failed&limit=20"
vis_api "$VIS_GATEWAY_URL/v1/automations/runs/$RUN_ID"
```

The list has `runs` and starts with the newest run. You can filter by `automation_id`, `status` and
`session_id`. `limit` is from 1 to 200, and the default is 50. A run has the fields of the `data`
object in [Receive callbacks](automations.md#receive-callbacks).

</div>

## Delete an automation

<div data-variant="python">

```python
deleted = client.delete_automation(automation_id)
print(deleted["is_deleted"])
```

Vis deletes the automation and its runs.

</div>

<div data-variant="http">

```bash
vis_api -X DELETE "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID"
```

Vis deletes the automation and its runs. The answer has `id` and `is_deleted`.

</div>

## Handle errors

<div data-variant="python">

```python
from blockether.vis.engine import GatewayError

try:
    client.get_automation("0b8e5d7c-1f2a-4c3b-8d9e-5a6f7b8c9d0e")
except GatewayError as error:
    print(error.status, error.code)  # 404 not-found
```

A failed request raises `GatewayError` with the HTTP `status` and the error `code`. To see the
codes, select **HTTP** at the top of the page. The exception does not contain the message of the
gateway. A connection failure raises `TransportError`.

</div>

<div data-variant="http">

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

</div>

## Send a webhook

<div data-variant="python">

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

</div>

<div data-variant="http">

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

</div>

[Check a delivery](automations.md#check-a-delivery) explains the answer.

## See also

- [Automations](automations.md) — triggers, targets, callbacks and webhook signatures.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [HTTP API](http-api.md) — authentication, the OpenAPI document and gateway errors.
