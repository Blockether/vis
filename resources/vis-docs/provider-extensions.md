# Provider extensions

A Python extension can register an LLM provider that the router selects like
any built-in one. This page is the reference for `vis.Provider`, its callbacks
and the managed-provider mode. For adding a provider through configuration
instead, see [Configuration](configuration.md#providers-and-models).

## Declaring a provider

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

Add `example` through **Add provider** or the `providers` configuration, then
select `example-model`. The preset supplies the endpoint, dialect and default
model names; the callbacks supply the credential and status. `vis-agent
providers status|limits|auth <id>` use the same registered provider.

## Records

| Record | Fields |
| --- | --- |
| `ProviderPreset` | `base_url`, `api_style`, `default_models`, `responses_path`, `llm_headers`, `extra_body`, `is_hidden`, JSON-only `extra` |
| `ProviderCredential` | `token`, optional `api_url`, `api_style`, `responses_path`, `llm_headers`, `source` |
| `ProviderStatus` | `is_authenticated`, `source`, display-only `extra` |
| `ProviderModel` | `name`, `context`, `is_tool_call`, `is_image_input`, JSON-only `extra` |
| `ProviderLimits` | `limits` (list of `ProviderLimit`), `rpm`, `tpm`, `note`, `error` |

`api_style` uses the vocabulary shared with configuration: `anthropic`,
`openai`, `openai-responses`, `gemini` and their aliases. Precedence is
configuration, then credential, then preset, so a credential can supply an
endpoint discovered during authentication and a user override still wins.
Header maps replace whole fields at each level.

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

All callbacks are optional and synchronous; an async function or a wrong
signature fails at declaration. Passive callback errors are logged and yield no
result, so one broken extension does not break the provider registry.
Authentication errors reach the caller.

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

A managed provider owns its whole lifecycle: it binds when the extension
loads, defines its endpoint and models, and keeps credentials outside
`state.yml`. Declare it with `is_managed=True`:

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
- [Extending Vis](extending.md) — the extension that carries the provider.
- [Distributions](distributions.md) — shipping a provider inside a custom build.
