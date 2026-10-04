# HTTP API basics

Call the Vis gateway with HTTP requests from any language. This page shows how to get the OpenAPI
document, authenticate requests and read gateway errors. Each feature page shows the requests for
its tasks.

## When to use

- **Your program is not written in Python.** [Authenticate requests](#authenticate-requests).
  Then follow the page for your task in [Find the requests for a
  feature](#find-the-requests-for-a-feature).
- **You need the exact request and answer formats.** [Get the OpenAPI
  document](#get-the-openapi-document).
- **A request fails with an error status.** [Handle gateway errors](#handle-gateway-errors).

To use typed calls from Python, read [Python SDK basics](python-sdk.md). To install and run the
gateway, read [Running a gateway](gateway-service.md).

## Before you start

[Start a gateway](gateway-service.md#start-a-local-gateway). For a gateway on another computer,
follow [Connect from another machine](gateway-service.md#connect-from-another-machine) and get its
token from the operator through a secure channel.

## Get the OpenAPI document

The gateway serves its OpenAPI 3.1 document without a token:

```bash
curl -sS http://127.0.0.1:7890/openapi.json -o vis-gateway.json
```

Use the document for routes, request formats and answers. The feature pages explain when to use
each request.

## Authenticate requests

Each request needs these headers:

- `x-vis-protocol` with the protocol number of the gateway. `GET /v1/capabilities` returns it as
  `protocol.protocol`. An incompatible client receives `HTTP 426`.
- `Authorization: Bearer <token>` when the gateway requires a token. [Tokens and HTTP
  401](gateway-service.md#tokens-and-http-401) tells when it does.

This setup reads the token and the protocol number once. The `vis_api` function then sends both
headers. The feature pages use this function in their examples. Do not print or commit the token.

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

## Find the requests for a feature

Each concept page has an HTTP page with the same operations as its Python page.

| Concept | HTTP page |
|---|---|
| [Sessions](sessions.md) | [Sessions over HTTP](http-sessions.md) |
| [Context management](context-management.md) | [Context management over HTTP](http-context-management.md) |
| [Project instructions](project-instructions.md) | [Project instructions over HTTP](http-project-instructions.md) |
| [Drafts](drafts.md) | [Drafts over HTTP](http-drafts.md) |
| [Council](council.md) | [Council over HTTP](http-council.md) |
| [Automations](automations.md) | [Automations over HTTP](http-automations.md) |
| [Configuration](configuration.md) | [Configuration over HTTP](http-configuration.md) |

## See also

- [Python SDK basics](python-sdk.md) — the same gateway with typed Python calls.
- [Sessions over HTTP](http-sessions.md) — send messages, follow progress and manage sessions.
- [Running a gateway](gateway-service.md) — install, secure and troubleshoot the gateway.
