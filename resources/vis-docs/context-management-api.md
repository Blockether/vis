# Context management API

Read the context budget, the usage and the cache health of a session from your own program. The
answers have the same values that the model and the session diagnostics show.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A dashboard must show how full the context of a session is.** [Read the context of a
  session](#read-the-context-of-a-session) and compare the measured input with the budgets.
- **You must know what a long session costs.** [Read usage and cache
  health](#read-usage-and-cache-health) for the tokens, the cache reuse and the cost.
- **A long session must continue with less context.** [Fold settled work](#fold-settled-work) with
  a message to the session.

To learn how Vis keeps the context small, read [Context management](context-management.md). To send
the same requests from another language, select **HTTP** at the top of the page.

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client` and `session`, a
`Session` from `client.session(session_id)`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. They also use `SESSION_ID` for the ID of a session that you created or found.

</div>

## Operations

<div data-variant="python">

Generated methods return the JSON answer of one request.

| Task | Typed call | Generated method |
|---|---|---|
| Read the context of a session | — | `get_session_context(sid)` |
| Read usage and cache health | — | `get_session_usage(sid)` |
| Fold settled work | `session.send(request)` | `post_session_turns(sid, body=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Read the context of a session | `GET /v1/sessions/{sid}/context` |
| Read usage and cache health | `GET /v1/sessions/{sid}/usage` |
| Fold settled work | `POST /v1/sessions/{sid}/turns` |

</div>

## Read the context of a session

<div data-variant="python">

```python
context = client.get_session_context(session.id)
budget = context["utilization"]
print(budget["latest_measured_input_tokens"], budget["auto_compress_above"], budget["model_input_limit"])
```

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/context" |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["utilization"])'
```

</div>

The answer is the read-only `session` object that the model sees in its sandbox. It has the
`utilization`, the `workspace`, the `access` rules, the `routing` and the `council` state of the
session. [Reference: folds and context
budget](context-management.md#reference-folds-and-context-budget) explains the budget values.

## Read usage and cache health

<div data-variant="python">

```python
usage = client.get_session_usage(session.id)["usage"]
if usage:
    print(usage["input_tokens"], usage["output_tokens"], usage["cost_usd"])
    print(usage["cache_read_share_percent"], usage["health"]["budget_state"])
```

`usage` is `None` until the session has a turn. It has these values for the whole session:

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/usage"
```

The answer has `usage`, which is `null` until the session has a turn. It has these values for the
whole session:

</div>

- `turn_count`, `iteration_count`, `tool_call_count` and `fold_count`.
- `input_tokens`, split into `input_regular_tokens`, `input_cache_read_tokens` and
  `input_cache_write_tokens`.
- `output_tokens` and `output_reasoning_tokens`.
- `cost_usd` and `duration_ms`.
- `cache_read_share_percent` and `reusable_prefix_coverage_percent`, the reuse of the prompt cache.

`health` describes the latest request. Its `budget_state` is `within-budget`, `fold-reminder`,
`over-budget`, `input-limit` or `budget-unreported`. The gateway reads every step of the session for
this answer, so do not request it in a fast loop.

## Fold settled work

<div data-variant="python">

```python
turn = session.send("Fold the finished research before you start the implementation.")
turn.wait(timeout=300)
print(client.get_session_usage(session.id)["usage"]["fold_count"])
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "Fold the finished research before you start the implementation.", "idempotency_key": "fold-1"}'
```

</div>

Only the agent folds its context, with `fold_session`. Your program asks for a fold in a message.
The saved history keeps every step. [Folding settled work](context-management.md#folding-settled-work)
explains what a fold keeps.

## See also

- [Context management](context-management.md) — folds, budgets and helpers that keep the context
  small.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [HTTP API](http-api.md) — authentication, the OpenAPI document and gateway errors.
