"""Acme provider — register an LLM provider the router can call.

Demonstrates `vis.provider(...)`: a first-class provider authored entirely
in Python, the same descriptor a Clojure provider extension builds. A
static-API-key provider (the common case) needs only a `preset` (base URL /
API style / default models) and `get_token_fn`; the other callable slots are
optional.

  * `preset`    — base URL, API style, default models; flows into the router
    exactly like a built-in provider's, so adding `acme` to
    `~/.vis/config.edn`'s `:providers` (or the TUI *Add Provider* picker)
    makes the model route to it.
  * `get_token_fn` — returns the credential for each request; raising is the
    "not configured" path the CLI/TUI surfaces.
  * `status_fn`    — powers `vis providers status acme`.

Dict keys may be snake_case or kebab (`api_url` ≡ `:api-url`); `api_style`
becomes a keyword host-side. Secrets come from the environment — extension
contexts are trusted, so `os.environ` is real.
"""

import os

import vis


def _token():
    key = os.environ.get("ACME_API_KEY")
    if not key:
        raise ValueError("set ACME_API_KEY to use the acme provider")
    return {"token": key, "api_url": "https://api.acme.ai/v1"}


def _status():
    ok = bool(os.environ.get("ACME_API_KEY"))
    return {"is_authenticated": ok, "source": "env-var", "provider_id": "acme"}


vis.extension(
    name="provider-acme",
    description="Acme AI (OpenAI-compatible) provider.",
    version="0.1.0",
    kind="provider",
    providers=[
        vis.provider(
            id="acme",
            label="Acme AI",
            preset={
                "base_url": "https://api.acme.ai/v1",
                "api_style": "openai",
                "default_models": ["acme-large", "acme-small"],
            },
            get_token_fn=_token,
            status_fn=_status,
        ),
    ],
)
