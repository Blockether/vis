# Automations over HTTP

Create, change and run [automations](automations.md) with HTTP requests from any language. The
requests and answers are JSON.

## When to use

- **A script must create the same automations on several gateways.** [Create an
  automation](#create-an-automation) with one `POST` request.
- **A deployment must pause a schedule and resume it later.** [Change or pause an
  automation](#change-or-pause-an-automation).
- **Your monitoring must find failed runs.** [Read runs](#read-runs) and filter them by status.
- **A test must start a run now or send a webhook.** [Start a run](#start-a-run) or [send a
  webhook](#send-a-webhook).

To learn what an automation does and how to create one in the app, read
[Automations](automations.md). To use typed calls from Python, read [Automations in
Python](python-automations.md).

## Before you start

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. The webhook route is the exception: it uses a webhook secret, not the gateway
token.

## Operations

The HTTP API has the same operations as the [Python SDK](python-automations.md). Requests and answers are
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

## List and read automations

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

## Create an automation

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

## Change or pause an automation

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID" \
  -H 'content-type: application/json' --data '{"enabled": false}'
```

Send `{"enabled": true}` to resume. The body changes only the fields that it names. Vis replaces
`triggers`, `target` and `delivery` as a whole, so send the full new value. The answer is the
changed automation.

## Start a run

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID/run"
```

The answer is the new run with the trigger `manual`. A manual run waits in the same queue as a
webhook run.

## Create a secret

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID/secrets" \
  -H 'content-type: application/json' --data '{"kind": "webhook"}'
```

The `kind` is `webhook` or `callback`. The answer has `kind` and `secret`. It is the only copy of
the secret, so store it at once. A new secret replaces the old secret immediately.

## Read runs

```bash
vis_api "$VIS_GATEWAY_URL/v1/automations/runs?automation_id=$AUTOMATION_ID&status=failed&limit=20"
vis_api "$VIS_GATEWAY_URL/v1/automations/runs/$RUN_ID"
```

The list has `runs` and starts with the newest run. You can filter by `automation_id`, `status` and
`session_id`. `limit` is from 1 to 200, and the default is 50. A run has the fields of the `data`
object in [Receive callbacks](automations.md#receive-callbacks).

## Delete an automation

```bash
vis_api -X DELETE "$VIS_GATEWAY_URL/v1/automations/$AUTOMATION_ID"
```

Vis deletes the automation and its runs. The answer has `id` and `is_deleted`.

## Handle errors

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

## Send a webhook

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

[Check a delivery](automations.md#check-a-delivery) explains the answer.

## See also

- [Automations](automations.md) — triggers, targets, callbacks and webhook signatures.
- [Automations in Python](python-automations.md) — the same operations as typed Python calls.
- [HTTP API basics](http-api.md) — authentication, the OpenAPI document and gateway errors.
