# HTTP API basics

Call the Vis gateway with HTTP requests from any language. This page shows how to get the OpenAPI
document, authenticate requests, and read gateway errors. Each feature page shows the requests for
its tasks.

## When to use

- **Your program is not written in Python.** [Authenticate requests](#authenticate-requests).
  Then open the API page of your feature, for example [Sessions API](sessions-api.md), and select
  **HTTP** at the top of that page.
- **You need the exact request and answer formats.**
  [Get the OpenAPI document](#get-the-openapi-document).
- **A request fails with an error status.** [Handle gateway errors](#handle-gateway-errors).

For typed calls from Python, read [Python SDK](python-sdk.md). To install and run the gateway, read
[Running a gateway](gateway-service.md).

## Before you start

[Start a gateway](gateway-service.md#start-a-local-gateway). For a gateway on another computer,
follow [Connect from another machine](gateway-service.md#connect-from-another-machine). Get its
token from the operator through a secure channel.

## Get the OpenAPI document

The gateway serves its OpenAPI 3.1 document without a token:

```bash
curl -sS http://127.0.0.1:7890/openapi.json -o vis-gateway.json
```

The document has the routes, the request formats, and the answers. Each JSON answer refers to its
schema in `components/schemas`.

## Authenticate requests

Each request needs these headers:

- `x-vis-protocol` with the protocol number of the gateway. `GET /v1/capabilities` returns the
  number as `protocol.protocol`. An incompatible client receives `HTTP 426`.
- `Authorization: Bearer <token>` when the gateway requires a token.
  [Tokens and HTTP 401](gateway-service.md#tokens-and-http-401) tells when it does.

Do not print or commit the token. This setup reads the token and the protocol number one time, and
the `vis_api` function sends both headers. The examples on the feature pages use this function.

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

## Handle gateway errors

An error answer has a JSON body with `error.type` and `error.message`:

```json
{"error": {"type": "unauthorized", "message": "missing or invalid bearer token"}}
```

| Status | When |
|---|---|
| `401` | The token is missing or not correct. |
| `426` | The type is `incompatible_protocol`. The client and the gateway have no common protocol. Update the client or the gateway to compatible versions. |
| Other `4xx` and `5xx` | The feature page of the route explains the `type`. |

## Repair an event stream gap

An event stream frame has a limit of 1 MiB, including its headers and delimiters. If the gateway
cannot send an event, it sends `event.elided` and gives the sequence number of the omitted event.
The `event_type` field names that event. The `reason` is `frame_size` or `replay_unavailable`.
If the frame has a `cursor`, use it to reset the stream position.
The `turn_id` and `view_id` fields identify the affected record if they fit. If the frame does not
identify a record, read the session snapshot.

Save the sequence number before you read the affected state again. Do not reconnect to request the
omitted body. Read the affected turn, view, or session from its HTTP resource. For current
subscription state, read the following resource. Do not use its cursor to skip later stream events.

```http
GET /v1/sessions/<session-id>/events-since?snapshot=true
```

A poll of stored events returns one page and its `cursor`. Use that cursor for the next poll, even
when the page has no events. If a gateway does not return a cursor, read the state again before
assuming that no events are missing. A fleet marker has `scope: fleet`. Read the list again without
changing any session cursor. For a speech job frame with `is_elided: true`, read the job at the resource URL
for the stream.

## Stop work and acknowledge results

A turn cancellation can return HTTP `200` with `accepted: false` and a reason of `no_running_turn`
or `not_running`. The gateway does not cancel queued work in this case.

A Task cancellation can return `requested: false` with a reason of `already_ended` or
`already_requested`. After you read the result of a Council watch Task, send `POST` to
`/v1/sessions/<session-id>/tasks/<task-id>/acknowledge`. The gateway removes the Task from the list
and keeps its result for a limited number of recent Tasks.

## See also

- For the same gateway with typed Python calls, read [Python SDK](python-sdk.md).
- To send messages, follow progress, and manage sessions, read [Sessions API](sessions-api.md).
- To install, secure, and troubleshoot the gateway, read [Running a gateway](gateway-service.md).
