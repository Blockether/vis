# Provider extensions

A Python extension can register an LLM provider. The router selects it in the
same way as built-in providers. This page describes `vis.Provider`, callbacks
and managed providers. To add a provider without an extension, see
[Configuration](configuration.md#providers-and-models).

## Before you start

Use a provider extension when authentication or model discovery needs Python code.
For a fixed endpoint and API key, prefer [provider configuration](configuration.md#providers-and-models).
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


vis.register(vis.Extension(
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
   whether a credential is present; it does not verify it with the service.
4. Add `example` through **Add provider** or `providers` configuration, select the
   real model name and make a request to verify the connection.

The preset supplies endpoint and model defaults; callbacks supply credentials and
status. Registration or a positive local status is not proof that the service accepts
the key. No interactive login is defined in this minimal example.

## Records

| Record | Fields |
| --- | --- |
| `ProviderPreset` | `base_url`, `api_style`, `default_models`, `responses_path`, `llm_headers`, `extra_body`, `is_hidden`, JSON-only `extra` |
| `ProviderCredential` | `token`, optional `api_url`, `api_style`, `responses_path`, `llm_headers`, `source` |
| `ProviderStatus` | `is_authenticated`, `source`, display-only `extra` |
| `ProviderModel` | `name`, `context`, `is_tool_call`, `is_image_input`, JSON-only `extra` |
| `ProviderLimits` | `limits` (list of `ProviderLimit`), `rpm`, `tpm`, `note`, `error` |

`api_style` accepts the same values as configuration: `anthropic`, `openai`,
`openai-responses`, `gemini` and their aliases. Configuration overrides the
credential, which overrides the preset. A credential can therefore supply an
endpoint discovered during authentication while still allowing a user override.
Header maps replace the entire field at each level.

Token and header fields are excluded from a record's `repr`. Never log
credentials or put them in status metadata. A missing credential is `None`,
not a record with an empty token.

## Callbacks

| Callback | Signature | Purpose |
| --- | --- | --- |
| `get_token_fn`, `detect_fn` | `() -> ProviderCredential \| None` | passive credential read; never starts a login |
| `refresh_token_fn` | `(rejected: str \| None) -> ProviderCredential \| None` | renew a token; a zero-argument form is accepted |
| `status_fn` | `() -> ProviderStatus \| None` | connection state, not quota |
| `limits_fn` | `() -> ProviderLimits \| None` | usage report |
| `auth_fn` | `(printer) -> str \| bool \| None` | interactive login; print instructions with `printer(line)` |
| `auth_prompt_fn` | `() -> Sequence[str] \| str \| None` | static login guidance |
| `logout_fn` | `() -> None` | discard the credential |
| `enrich_models_fn` | `(provider, router_opts) -> Sequence[ProviderModel] \| None` | extend the model list; `None` keeps defaults |
| `on_selected_fn` | `(event) -> None` | notification after selection |

Callbacks are optional and synchronous. Async functions and invalid signatures
are rejected at declaration. Passive callback errors are logged and return no
result; authentication errors are returned to the caller.

Startup, status probes and limits polling run without a session. `vis.shell`
and `vis.jailed_shell` work there; `vis.ask` and `vis.jailed_shell_session` do
not.

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
calls `get_token_fn()` again and resumes; concurrent requests share the flow.
Status probes, startup and opening the model picker never run `auth_fn`. Test
the explicit path with `vis-agent providers auth <id>`, then make a request
while signed out to verify first-use login.

## See also

- [Configuration](configuration.md) — providers declared in `vis.yml` and the router.
- [Extending Vis](extending.md) — writing the provider's extension.
- [Distributions](distributions.md) — including a provider in a custom build.
