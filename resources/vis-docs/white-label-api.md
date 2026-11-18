# White label API

Read, set, and remove the brand of the web app that a gateway serves. A brand gives the web app
your name, logo, header links, and theme.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A setup script must brand a new gateway.** Read [Set the brand](#set-the-brand).
- **A tool must add a link to the header.** Read [Read the brand](#read-the-brand). Add the link
  and set the brand again.
- **You must go back to the Vis look.** Read [Remove the brand](#remove-the-brand).

To learn what each key does and where the brand shows, read [White label](white-label.md).

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it.

</div>

## Operations

<div data-variant="python">

| Task | Typed call | Generated method |
|---|---|---|
| Read the brand | `client.web_brand()` | `client.routes.get_web_brand()` |
| Set the brand | `client.set_web_brand(...)` | `client.routes.put_web_brand(body=...)` |
| Remove the brand | `client.clear_web_brand()` | `client.routes.delete_web_brand()` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Read the brand | `GET /v1/web/brand` |
| Set the brand | `PUT /v1/web/brand` |
| Remove the brand | `DELETE /v1/web/brand` |

</div>

A set replaces the whole brand. A key that you omit goes back to the Vis default.

## Set the brand

<div data-variant="python">

```python
brand = client.set_web_brand(
    name="Acme",
    logo="https://acme.example/logo.svg",
    href="https://acme.example",
    links=[
        {"icon": "book", "label": "Handbook", "href": "https://acme.example/handbook"},
        {"icon": "chat", "label": "Support", "href": "https://acme.example/support"},
    ],
    theme={
        "base": "tokyonight-night",
        "label": "Acme Night",
        "colors": {"--bg": "#101010", "--primary": "#e11d48"},
    },
)
print(brand["name"])
```

`set_web_brand()` returns the saved brand. A value that breaks a rule raises `ValueError` before
the request, and the gateway saves nothing.

</div>

<div data-variant="http">

```bash
vis_api -X PUT "$VIS_GATEWAY_URL/v1/web/brand" -H 'content-type: application/json' \
  --data '{"name": "Acme", "logo": "https://acme.example/logo.svg",
           "href": "https://acme.example",
           "links": [{"icon": "book", "label": "Handbook",
                      "href": "https://acme.example/handbook"}],
           "theme": {"base": "tokyonight-night", "label": "Acme Night",
                     "colors": {"--bg": "#101010", "--primary": "#e11d48"}}}'
```

The answer is the saved brand. A value that breaks a rule gives status 400 with the error type
`invalid-brand` and a message, and the gateway saves nothing.

</div>

The gateway also refuses a theme `base` that Vis does not ship and a color variable that the base
theme does not have.

## Read the brand

<div data-variant="python">

```python
brand = client.web_brand()
links = [*brand.get("links", []), {"icon": "globe", "label": "Status", "href": "https://status.acme.example"}]
client.set_web_brand(**{**brand, "links": links})
```

`web_brand()` returns a dictionary with only the keys that are set. An empty dictionary means the
Vis look.

</div>

<div data-variant="http">

```bash
curl -s "$VIS_GATEWAY_URL/v1/web/brand"
```

The answer has only the keys that are set. `{}` means the Vis look. This request needs no token.

</div>

## Remove the brand

<div data-variant="python">

```python
client.clear_web_brand()
```

</div>

<div data-variant="http">

```bash
vis_api -X DELETE "$VIS_GATEWAY_URL/v1/web/brand"
```

</div>

After a reload, the web app shows the Vis look again. If your build of the web app has its own
brand, that brand stays.

## See also

- To learn what each key of a brand does and how to put a brand in a build, read [White
  label](white-label.md).
- To install the SDK, connect a client, and handle gateway errors, read [Python
  SDK](python-sdk.md).
- To authenticate requests and handle gateway errors from any language, read [HTTP
  API](http-api.md).
