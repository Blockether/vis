# Provider extensions

A provider extension connects Vis to an LLM provider that needs a custom sign-in or its own
model list. Register the provider with `vis.Provider`, and Vis offers its models next to the
built-in providers.

## When to use

- **Your LLM service needs an interactive sign-in, or its tokens expire.** Put sign-in and
  renewal in [callbacks](#callbacks) such as `auth_fn` and `refresh_token_fn`.
- **The model list must come from the service.** Extend the list with `enrich_models_fn`, in
  [Callbacks](#callbacks).
- **The extension must own the credentials of the provider**, outside `state.yml`. Register a
  [managed provider](#managed-providers).
- **Your extension saves tokens to a file, or a sign-in was lost and you must find the cause.**
  Follow [Store and renew tokens safely](#store-and-renew-tokens-safely).

If your service needs only an endpoint and an API key, no extension is necessary. Use
[Configuration](configuration.md#providers-and-models).

## Before you start

The example needs no external Python package. Replace its placeholder endpoint and model names
with a service that you are authorized to use.

## Declare and load a provider

1. Save this entry as `.vis/extensions/example_provider.py` in your project. The entry reads
   `EXAMPLE_API_KEY` only when a callback runs, not when it declares the provider.

```python
import os
import blockether.vis.extension as vis


def credential() -> vis.ProviderCredential | None:
    token = os.environ.get("EXAMPLE_API_KEY")
    return vis.ProviderCredential(token) if token else None


def status() -> vis.ProviderStatus:
    return vis.ProviderStatus(is_authenticated=bool(os.environ.get("EXAMPLE_API_KEY")),
                              source="env-var")


vis.register_extension(vis.Extension(
    name="provider-example",
    description="An OpenAI-compatible provider.",
    env=["EXAMPLE_API_KEY"],
    providers=[vis.Provider(
        id="example",
        label="Example AI",
        preset=vis.ProviderPreset(
            base_url="https://gateway.example.com/v1",
            api_style="openai",
            default_models=["example-model"],
        ),
        get_token_fn=credential,
        status_fn=status,
    )],
))
```

2. Supply `EXAMPLE_API_KEY` in the environment configuration of the project, a local `.env`
   file, or the startup environment of the gateway. Do not commit the value. Read [environment
   resolution](extension-api.md#environment).
3. Start Vis in that project or run `/reload`. Then run `vis-agent providers status example` in
   the terminal. The status callback reports that a credential is present. It does not prove
   that the service accepts the key.
4. Add `example` through **Add provider** or the `providers` configuration. Select the real
   model name. Send a request to check the connection.

The preset supplies the endpoint and the model defaults, and the callbacks supply the credential
and the status. Registration, like a positive local status, does not prove that the service
accepts the key. The example defines no interactive login.

## Records

| Record | Fields |
| --- | --- |
| `ProviderPreset` | `base_url`, `api_style`, `default_models`, `responses_path`, `llm_headers`, `extra_body`, `is_hidden`, JSON-only `extra` |
| `ProviderCredential` | `token`, optional `api_url`, `api_style`, `responses_path`, `llm_headers`, `source` |
| `ProviderStatus` | `is_authenticated`, `source`, display-only `extra` |
| `ProviderModel` | `name`, `context`, `is_tool_calls_supported`, `is_vision_supported`, JSON-only `extra` |
| `ProviderLimits` | `limits` (list of `ProviderLimit`), `rpm`, `tpm`, `note`, `error` |

`api_style` accepts the values that configuration accepts: `anthropic`, `openai`,
`openai-responses`, `gemini`, and their aliases. Configuration overrides the credential, and the
credential overrides the preset. So a credential can supply an endpoint that sign-in found, and
the user can still override it. A header map replaces the whole field at each level.

A model can use a different wire format from its provider. For example, a gateway can serve
Claude models on `/messages` and other models on the Responses API. Put `api_style` in the
`extra` of that `ProviderModel`, as in `extra={"api_style": "anthropic"}`. It accepts the values
of the preset. If a value names no wire format, Vis ignores the whole model list from
`enrich_models_fn`, logs a warning, and keeps the configured models.

`ProviderModel.is_vision_supported` works like `is_vision_supported` on a model in
[configuration](configuration.md#images):

- With `True`, Vis sends attached pictures to the model.
- With `False`, Vis sends a text description of each picture.
- With `None`, the model catalog decides.

Vis also reads the old names `is_tool_call` and `is_image_input`. Use the new names in new code.

The `repr` of a record leaves out token fields and header fields. Never log a credential or put
one in status metadata. A missing credential is `None`, not a record with an empty token.

### Model prices

Set the price of a model when your provider bills rates other than the public list prices. Put a
`pricing` map in the `extra` of `ProviderModel`:

```python
vis.ProviderModel(
    name="claude-haiku-5-5",
    context=1000000,
    is_tool_calls_supported=True,
    extra={"pricing": {"input": 0.11, "output": 0.55,
                       "input_over_100k": 0.55, "output_over_100k": 2.75}},
)
```

- Give each rate in USD for 1 million tokens.
- Use the keys `input`, `output`, `cached_input`, `cache_write`, `cache_write_5m`, and
  `cache_write_1h`.
- Add `_over_100k`, `_over_200k`, or `_over_272k` to a key for the rate above that prompt size.

Vis prices each response with the rates of the provider that served it. Without a `pricing` map,
Vis uses the built-in price list. A rate that is not a non-negative number stops the model list
of the provider, and Vis logs a warning.

## Callbacks

| Callback | Signature | Purpose |
| --- | --- | --- |
| `get_token_fn`, `detect_fn` | `() -> ProviderCredential \| None` | Read a credential. Never start a login. |
| `refresh_token_fn` | `(rejected: str \| None) -> ProviderCredential \| None` | Renew a token. A zero-argument form also works. |
| `status_fn` | `() -> ProviderStatus \| None` | Connection state, not quota. |
| `limits_fn` | `() -> ProviderLimits \| None` | Usage report. |
| `auth_fn` | `(printer) -> str \| bool \| None` | Interactive login. Print instructions with `printer(line)`. |
| `auth_prompt_fn` | `() -> Sequence[str] \| str \| None` | Static login guidance. |
| `logout_fn` | `() -> None` | Discard the credential. |
| `enrich_models_fn` | `(provider, router_opts) -> Sequence[ProviderModel] \| None` | Extend the model list. `None` keeps the defaults. |
| `on_selected_fn` | `(event) -> None` | Notification after selection. |

Each callback is optional and synchronous. Vis rejects an async function or an invalid signature
at declaration. An error in a passive callback is logged and gives no result. An authentication
error goes to the caller.

Startup, status probes, and limits polling run without a session. `vis.shell` and
`vis.jailed_shell` work there. `vis.ask` does not.

Report usage with the limits contract:

```python
def limits() -> vis.ProviderLimits:
    return vis.ProviderLimits(limits=[
        vis.ProviderLimit("daily-tokens", "Daily tokens",
                          scope="account", kind="tokens",
                          precision="exact", source="provider-api",
                          used=25.49, limit=100,
                          window=vis.ProviderLimitWindow("calendar", unit="day", size=1)),
    ])
```

`ProviderLimit` also accepts `remaining`, `is_unlimited`, `subject`, and `note`. Inspect the
result with `vis-agent providers limits <id>`.

## Managed providers

The extension of a managed provider defines the endpoint and the models, and stores the
credentials outside `state.yml`. Set `is_managed=True`:

```python
vis.Provider(
    id="corp-gateway",
    label="Corp Gateway",
    is_managed=True,
    preset=vis.ProviderPreset(base_url="https://gateway.example.com/v1",
                              api_style="openai",
                              default_models=["corp-large", "corp-small"]),
    get_token_fn=_issued_token,
    auth_fn=_browser_oauth,   # optional: provider-owned first-use login
)
```

| | Ordinary provider | Managed |
| --- | --- | --- |
| Owner | user configuration | the extension |
| **Add provider** dialog | listed | never listed |
| Appears | after configuration | when the extension loads |
| Authentication | configured flow | runtime credential, or the `auth_fn` of the extension |

To write one, do these steps:

1. Supply a `preset` with `default_models`.
2. Make `get_token_fn()` a passive read. It returns `ProviderCredential(token)`, or `None` when
   the person is signed out.
3. If the provider needs a sign-in, supply `auth_fn(printer)`. It completes the interactive
   flow, stores the credential where `get_token_fn()` reads it, and returns `"ok"` or
   `"already-authenticated"`.
4. Keep `status_fn()` and `limits_fn()` passive. Put renewal in `refresh_token_fn()`.

A request that finds no usable token runs `auth_fn` one time. Vis then calls `get_token_fn()`
again and continues the request. Concurrent requests share the flow. Status probes, startup, and
the model picker never run `auth_fn`.

Test the explicit path with `vis-agent providers auth <id>`. Test the first-use login with a
request while you are signed out.

A person can also start this login from the row of the provider: **Sign in** in the Companion or
**Authenticate** in the TUI. Vis runs `auth_fn` on the gateway machine, and the client shows
each line that `auth_fn` passes to `printer`. Print a fallback sign-in URL for the case where no
browser opens on that machine.

## Store and renew tokens safely

Many token endpoints issue a new refresh token on each renewal and reject the old one. If your
extension loses the new token, the next renewal fails and the person must sign in again.

Save each token file with `vis.write_private_file(path, text)`:

- Only the owner of the file can read it.
- The text replaces the old file in one atomic step, so a reader never sees a half-written file.
- If the write fails, the old file stays and the call raises the error.
- The call creates missing parent directories.

Log each renewal with `vis.log`, a failed renewal included. Never log a token, a response body,
or an error message, because each can hold a secret.

```python
import json
from pathlib import Path

TOKENS = Path.home() / ".example" / "tokens.json"

def refresh(rejected=None):
    saved = json.loads(TOKENS.read_text())
    try:
        fresh = exchange(saved["refresh_token"])  # your token endpoint call
    except Exception as error:
        vis.log("warn", f"example token refresh failed: {type(error).__name__}")
        raise
    vis.write_private_file(TOKENS, json.dumps(fresh))
    vis.log("info", "example token refreshed")
    return vis.ProviderCredential(fresh["access_token"])
```

## See also

- For providers in `vis.yml` and the router, read [Configuration](configuration.md).
- To write the extension of the provider, read [Extending Vis](extending.md).
- To include a provider in a custom build, read [Distributions](distributions.md).
