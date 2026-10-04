# Context management over HTTP

Read the context budget, the usage and the cache health of a session with HTTP requests from any
language. The requests return the same values that the model and the session diagnostics show.

## When to use

- **A dashboard must show how full the context of a session is.** [Read the context of a
  session](#read-the-context-of-a-session) and compare the measured input with the budgets.
- **You must know what a long session costs.** [Read usage and cache
  health](#read-usage-and-cache-health) for the tokens, the cache reuse and the cost.
- **A long session must continue with less context.** [Fold settled work](#fold-settled-work) with
  a message to the session.

To learn how Vis keeps the context small, read [Context management](context-management.md). To use
typed calls from Python, read [Context management in Python](python-context-management.md).

## Before you start

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. They also use `SESSION_ID` for the ID of a session that you created or found.

## Operations

The HTTP API has the same operations as the [Python SDK](python-context-management.md).

| Task | Method and path |
|---|---|
| Read the context of a session | `GET /v1/sessions/{sid}/context` |
| Read usage and cache health | `GET /v1/sessions/{sid}/usage` |
| Fold settled work | `POST /v1/sessions/{sid}/turns` |

## Read the context of a session

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/context" |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["utilization"])'
```

The answer is the read-only `session` object that the model sees in its sandbox. It has the
`utilization`, the `workspace`, the `access` rules, the `routing` and the `council` state of the
session. [Reference: folds and context
budget](context-management.md#reference-folds-and-context-budget) explains the budget values.

## Read usage and cache health

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/usage"
```

The answer has `usage`, which is `null` until the session has a turn. It has these values for the
whole session:

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

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "Fold the finished research before you start the implementation.", "idempotency_key": "fold-1"}'
```

Only the agent folds its context, with `fold_session`. Your program asks for a fold in a message.
The saved history keeps every step. [Folding settled work](context-management.md#folding-settled-work)
explains what a fold keeps.

## See also

- [Context management](context-management.md) — folds, budgets and helpers that keep the context
  small.
- [Context management in Python](python-context-management.md) — the same operations as typed Python
  calls.
- [HTTP API basics](http-api.md) — authentication, the OpenAPI document and gateway errors.
