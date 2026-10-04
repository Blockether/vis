# Automations in Python

Create, change and run [automations](automations.md) from a Python program. Each `GatewayClient`
method sends one gateway request and returns the parsed JSON answer.

## When to use

- **A script must create the same automations on several gateways.** [Create an
  automation](#create-an-automation) from a Python `dict`.
- **A deployment must pause a schedule and resume it later.** [Change or pause an
  automation](#change-or-pause-an-automation).
- **Your monitoring must find failed runs.** [Read runs](#read-runs) and filter them by status.
- **A test must start a run now or send a webhook.** [Start a run](#start-a-run) or [send a
  webhook](#send-a-webhook).

To learn what an automation does and how to create one in the app, read
[Automations](automations.md). To send the same requests from another language, read [Automations
over HTTP](http-automations.md).

## Before you start

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

## Operations

`GatewayClient` has one method for each automation route. A method sends the same request as the
[HTTP API](http-automations.md) and returns the parsed JSON answer. You give a body or a query as a Python
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

## List and read automations

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

## Create an automation

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

## Change or pause an automation

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

## Start a run

```python
run = client.post_automation_run(automation_id)
print(run["id"], run["status"])
```

The answer is the new run with the trigger `manual`. A manual run waits in the same queue as a
webhook run.

## Create a secret

```python
created = client.post_automation_secrets(automation_id, body={"kind": "webhook"})
webhook_secret = created["secret"]
```

The `kind` is `webhook` or `callback`. The answer is the only copy of the secret, so store it at
once. A new secret replaces the old secret immediately.

## Read runs

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

## Delete an automation

```python
deleted = client.delete_automation(automation_id)
print(deleted["is_deleted"])
```

Vis deletes the automation and its runs.

## Handle errors

```python
from blockether.vis.engine import GatewayError

try:
    client.get_automation("0b8e5d7c-1f2a-4c3b-8d9e-5a6f7b8c9d0e")
except GatewayError as error:
    print(error.status, error.code)  # 404 not-found
```

A failed request raises `GatewayError` with the HTTP `status` and the error `code`. The codes are in
[Automations over HTTP](http-automations.md#handle-errors). The exception does not contain the message of the
gateway. A connection failure raises `TransportError`.

## Send a webhook

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

[Check a delivery](automations.md#check-a-delivery) explains the answer.

## See also

- [Automations](automations.md) — triggers, targets, callbacks and webhook signatures.
- [Automations over HTTP](http-automations.md) — the same operations as HTTP requests.
- [Python SDK basics](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
