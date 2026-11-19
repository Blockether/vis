# Private links API

This API reads the proxy settings of the gateway and sends the HTTP requests of a program through
the proxy. The proxy opens company hosts that only the gateway computer can reach.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A script must read a company host that only the gateway reaches.** [Send a request through the
  proxy](#send-a-request-through-the-proxy).
- **A tool must know if the gateway opens a link.** [Check a link](#check-a-link).
- **A client must show the proxy settings.** [Read the proxy settings](#read-the-proxy-settings).

To turn on the proxy and choose the domains, read [Private links](private-links.md).

## Before you start

<div data-variant="python">

Connect a `GatewayClient` with the gateway token, as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client`. A client
without a token raises `ValueError`, because the proxy needs the token.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it, `VIS_GATEWAY_URL`, and `VIS_GATEWAY_TOKEN`.

</div>

## Operations

<div data-variant="python">

| Task | Typed call | Generated method |
|---|---|---|
| Read the proxy settings | `client.proxy()` | `client.routes.get_proxy()` |
| Check a link | `proxy.covers(link)` | None |
| Get the proxy URL for a link | `proxy.proxy_for(link)` | None |
| Send requests through the proxy | `proxy.opener()` | None |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Read the proxy settings | `GET /v1/proxy` |
| Send a request through the proxy | An HTTP proxy request to the proxy port |

</div>

## Read the proxy settings

<div data-variant="python">

```python
proxy = client.proxy()
if proxy is None:
    print("The proxy is off.")
else:
    print(proxy.host, proxy.port, proxy.allowed_domains, proxy.denied_domains, proxy.ports)
```

`client.proxy()` returns a `GatewayProxy` object, or `None` when the proxy is off. `host` is the
host of the client URL. Each call reads the settings again, so a change shows at once. The text of
a `GatewayProxy` object does not show the token.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/proxy"
```

The answer has these fields:

| Field | Description |
|---|---|
| `is_enabled` | `true` when the proxy runs. |
| `port` | The port of the proxy, or `null` when the proxy is off. |
| `allowed_domains` | The hosts that the proxy opens. |
| `denied_domains` | The hosts that the proxy refuses. |
| `ports` | The ports that a link can use. |

The proxy listens on the host of `VIS_GATEWAY_URL`.

</div>

## Check a link

<div data-variant="python">

```python
link = "https://grafana.acme.io/d/latency"
print(proxy.covers(link))
```

`covers(link)` returns `True` when the proxy opens the link. It uses the same rules as the gateway.

</div>

<div data-variant="http">

The gateway has no route that checks one link. Apply the rules of [Choose the domains and
ports](private-links.md#choose-the-domains-and-ports) to the answer of the proxy settings. A link
that the proxy does not open gets `403 Forbidden` from the proxy.

</div>

## Send a request through the proxy

<div data-variant="python">

```python
link = "https://wiki.corp.internal/start"
with proxy.opener().open(link) as answer:
    print(answer.status, answer.read()[:200])
```

`opener()` returns a `urllib` opener. It sends the links that the proxy opens through the proxy
and opens other links directly. It checks a redirect again, so the token never goes to a host that
is not on the list.

With another HTTP library, give the proxy URL of each link:

```python
import httpx

response = httpx.get(link, proxy=proxy.proxy_for(link))
```

`proxy_for(link)` returns `None` for a link that the proxy does not open. The proxy URL has the
token, so keep the URL out of logs.

</div>

<div data-variant="http">

```bash
PROXY_HOST=100.64.0.10
PROXY_PORT=7891
curl -x "http://$VIS_GATEWAY_TOKEN:vis@$PROXY_HOST:$PROXY_PORT" https://wiki.corp.internal/start
```

The proxy asks for the gateway token as the user name of `Proxy-Authorization: Basic`. It ignores
the password. Without the token, the proxy answers `407 Proxy Authentication Required`. An `https`
link goes through a `CONNECT` tunnel, so the page stays encrypted to the company host.

</div>

## See also

- To turn on the proxy and open private links in the Companion app, read [Private
  links](private-links.md).
- To find other gateway requests, read [HTTP API](http-api.md).
