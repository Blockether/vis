# Provider extensions

Use a provider extension to connect Vis to an LLM provider that needs custom sign-in
or model discovery. You register it with `vis.Provider`, and Vis can select its
models alongside the built-in providers.

## When to use

- **Your LLM service needs an interactive sign-in or tokens that expire.** Handle
  sign-in and renewal in [callbacks](#callbacks) such as `auth_fn` and
  `refresh_token_fn`.
- **The service's models should come from the service itself.** Extend the model
  list with `enrich_models_fn`, described in [Callbacks](#callbacks).
- **The extension should own the provider's credentials**, outside `state.yml`.
  Register a [managed provider](#managed-providers).
- **Your extension saves tokens to a file, or you must find out why a sign-in
  was lost.** Save and log each renewal as described in
  [Store and renew tokens safely](#store-and-renew-tokens-safely).

If your service needs only an endpoint and API key, use
[Configuration](configuration.md#providers-and-models) instead. No extension is
needed.

## Before you start

The example needs no external Python dependencies, but its endpoint and model names
are placeholders: replace them with a service you are authorized to use.

## Declare and load a provider

1. Save this complete entry as `.vis/extensions/example_provider.py` in your project.
   It reads `EXAMPLE_API_KEY` only when a callback runs, not while declaring the provider.

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

2. Supply `EXAMPLE_API_KEY` through the project's environment configuration, a local
   `.env` file or the gateway's startup environment. Do not commit the value. See
   [environment resolution](extension-api.md#environment).
3. Start Vis in that project or run `/reload`, then inspect
   `vis-agent providers status example` in the terminal. The status callback reports
   whether a credential is present. It does not verify it with the service.
4. Add `example` through **Add provider** or the `providers` configuration. Select the real model
   name. Send a request to check the connection.

The preset supplies endpoint and model defaults. Callbacks supply credentials and
status. Registration or a positive local status is not proof that the service accepts
the key. No interactive login is defined in this minimal example.

## Records

| Record | Fields |
| --- | --- |
| `ProviderPreset` | `base_url`, `api_style`, `default_models`, `responses_path`, `llm_headers`, `extra_body`, `is_hidden`, JSON-only `extra` |
| `ProviderCredential` | `token`, optional `api_url`, `api_style`, `responses_path`, `llm_headers`, `source` |
| `ProviderStatus` | `is_authenticated`, `source`, display-only `extra` |
| `ProviderModel` | `name`, `context`, `is_tool_calls_supported`, `is_vision_supported`, JSON-only `extra` |
| `ProviderLimits` | `limits` (list of `ProviderLimit`), `rpm`, `tpm`, `note`, `error` |

`api_style` accepts the same values as configuration: `anthropic`, `openai`,
`openai-responses`, `gemini` and their aliases. Configuration overrides the
credential, which overrides the preset. A credential can therefore supply an
endpoint discovered during authentication while still allowing a user override.
Header maps replace the entire field at each level.

A model can use another wire format than its provider. For example, a gateway can
serve Claude models on `/messages` and other models on the Responses API. Put
`api_style` in the `extra` of that `ProviderModel`, as in
`extra={"api_style": "anthropic"}`. It accepts the same values as the preset. If
a value names no wire format, Vis ignores the whole model list from
`enrich_models_fn`, logs a warning and keeps the configured models.

`ProviderModel.is_vision_supported` tells Vis if a model reads images. With
`True`, Vis sends attached pictures to the model. With `False`, Vis sends a text
description of each picture. With `None`, the model catalog decides. This field
works like `is_vision_supported` on a model in
[configuration](configuration.md#images).

Vis also reads the old names `is_tool_call` and `is_image_input`. Use the new names in new code.

Token and header fields are excluded from a record's `repr`. Never log
credentials or put them in status metadata. A missing credential is `None`,
not a record with an empty token.

### Model prices

Set the price of a model when your provider bills other rates than the public list
prices. Put a `pricing` map in the `extra` of `ProviderModel`:

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
- Use the keys `input`, `output`, `cached_input`, `cache_write`, `cache_write_5m` and `cache_write_1h`.
- Add `_over_100k`, `_over_200k` or `_over_272k` to a key for the rate above that prompt size.

Vis prices each response with the rates of the provider that served it. Without a
`pricing` map, Vis uses the built-in price list. A rate that is not a non-negative number
stops the model list of the provider, and Vis logs a warning.

## Callbacks

| Callback | Signature | Purpose |
| --- | --- | --- |
| `get_token_fn`, `detect_fn` | `() -> ProviderCredential \| None` | passive credential read that never starts a login |
| `refresh_token_fn` | `(rejected: str \| None) -> ProviderCredential \| None` | renew a token (a zero-argument form also works) |
| `status_fn` | `() -> ProviderStatus \| None` | connection state, not quota |
| `limits_fn` | `() -> ProviderLimits \| None` | usage report |
| `auth_fn` | `(printer) -> str \| bool \| None` | interactive login (print instructions with `printer(line)`) |
| `auth_prompt_fn` | `() -> Sequence[str] \| str \| None` | static login guidance |
| `logout_fn` | `() -> None` | discard the credential |
| `enrich_models_fn` | `(provider, router_opts) -> Sequence[ProviderModel] \| None` | extend the model list (`None` keeps the defaults) |
| `on_selected_fn` | `(event) -> None` | notification after selection |

Callbacks are optional and synchronous. Async functions and invalid signatures
are rejected at declaration. Passive callback errors are logged and return no
result. Authentication errors are returned to the caller.

Startup, status probes and limits polling run without a session. `vis.shell` and `vis.jailed_shell`
work there, but `vis.ask` does not.

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

`ProviderLimit` also accepts `remaining`, `is_unlimited`, `subject` and
`note`. Inspect the result with `vis-agent providers limits <id>`.

## Managed providers

A managed provider is registered when its extension loads. The extension
defines its endpoint and models and stores credentials outside `state.yml`.
Set `is_managed=True`:

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
| Authentication | configured flow | runtime credential, or the extension's `auth_fn` |

To implement one:

1. Set `is_managed=True` and supply a `preset` with `default_models`.
2. Make `get_token_fn()` a passive read that returns `ProviderCredential(token)`
   or `None` while signed out.
3. When sign-in is needed, supply `auth_fn(printer)`. It completes the
   interactive flow, stores the credential where `get_token_fn()` reads it, and
   returns `"ok"` or `"already-authenticated"`.
4. Keep `status_fn()` and `limits_fn()` passive. Put renewal in
   `refresh_token_fn()`.

With `auth_fn`, a request that finds no usable token runs the login once,
calls `get_token_fn()` again and resumes. Concurrent requests share the flow.
Status probes, startup and opening the model picker never run `auth_fn`. Test
the explicit path with `vis-agent providers auth <id>`, then make a request
while signed out to verify first-use login.

You can also start this login from the provider's row: **Sign in** in the
Companion or **Authenticate** in the TUI. Vis runs `auth_fn` on the gateway
machine, and the client shows each line that `auth_fn` passes to `printer`.
Print a fallback sign-in URL, so that a person can finish sign-in when no
browser opens on that machine.

## Store and renew tokens safely

Many token endpoints issue a new refresh token on each renewal and stop
accepting the old one. If your extension loses the new token, the next renewal
fails and the person must sign in again. Two SDK functions prevent this and
make each renewal visible in the Vis log.

`vis.write_private_file(path, text)` saves a secret file. Only the file's owner
can read it, at every moment. The text goes to a temporary file first, and then
replaces the old file in one step. A reader never sees a half-written file. If
a step fails, the old file stays and the error is raised. The parent directory
must exist.

`vis.log_token_refresh(store, outcome, ...)` writes one log record for one
renewal. Call it once for each exchange, also for a failed one. The record
holds the process ID, the duration, the HTTP status and short fingerprints of
the tokens. It never holds a token, a response body or an error message. The
built-in providers write the same fields, so you find every renewal with one
search for `OAuth token refresh`.

```python
import json, time
from pathlib import Path

TOKENS = Path.home() / ".example" / "tokens.json"

def refresh(rejected=None):
    saved = json.loads(TOKENS.read_text())
    started = time.monotonic()
    trigger = "rejected-token" if rejected else "expired"
    try:
        fresh = exchange(saved["refresh_token"])  # your token endpoint call
    except Exception as error:
        vis.log_token_refresh("example", "failed", trigger=trigger, started=started,
                              error=error, refresh_token=saved["refresh_token"])
        raise
    try:
        vis.write_private_file(TOKENS, json.dumps(fresh))
    except OSError as error:
        vis.log_token_refresh("example", "persist-failed", trigger=trigger, started=started,
                              error=error, refresh_token=saved["refresh_token"],
                              next_refresh_token=fresh["refresh_token"])
        raise
    vis.log_token_refresh("example", "refreshed", trigger=trigger, started=started,
                          refresh_token=saved["refresh_token"],
                          next_refresh_token=fresh["refresh_token"])
    return vis.ProviderCredential(fresh["access_token"])
```

Use the outcome `rejected` when the endpoint refuses the refresh grant. Use
`no-refresh-token` when a saved sign-in has no refresh token. Pass `started` as
a `time.monotonic()` value. The engine stores the record as an extension log
line, with the fields as JSON in its message.

## See also

- [Configuration](configuration.md) — providers declared in `vis.yml` and the router.
- [Extending Vis](extending.md) — writing the provider's extension.
- [Distributions](distributions.md) — including a provider in a custom build.
