"""Provider SDK declarations and callback boundaries, without a running engine."""

import inspect
from dataclasses import FrozenInstanceError, fields, is_dataclass
from typing import get_args

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


def test_provider_values_are_frozen_and_preserve_opaque_payloads():
    headers = {"X-Initiator": "agent"}
    body = {"top_p": 0.9, "nested": {"keep_this_key": [1, 2]}}
    preset = vis.ProviderPreset(
        base_url="https://gateway.example.com/v1",
        api_style="openai",
        default_models=["example"],
        llm_headers=headers,
        extra_body=body,
    )
    credential = vis.ProviderCredential(
        "fixture-private", llm_headers={"Authorization": "fixture-private"}
    )
    headers.clear()
    body["nested"]["keep_this_key"].append(3)
    for record in (
        preset,
        credential,
        vis.ProviderStatus(True),
        vis.ProviderModel("example"),
    ):
        assert is_dataclass(record) and not hasattr(record, "__dict__")
        for field in fields(record):
            with pytest.raises(FrozenInstanceError):
                setattr(record, field.name, getattr(record, field.name))
    assert "fixture-private" not in repr(credential)
    with pytest.raises(TypeError):
        preset.extra_body["nested"]["keep_this_key"] = ()
    wire = preset.to_wire()
    assert wire["llm_headers"] == {"X-Initiator": "agent"}
    assert wire["extra_body"]["nested"]["keep_this_key"] == [1, 2]
    wire["extra_body"]["nested"].clear()
    assert preset.extra_body["nested"]["keep_this_key"] == (1, 2)


def test_provider_vocabulary_is_the_canonical_vocabulary():
    for name, vocabulary in (
        ("ProviderLimitStatus", "statuses"),
        ("ProviderLimitScope", "scopes"),
        ("ProviderLimitKind", "kinds"),
        ("ProviderWindowKind", "window_kinds"),
        ("ProviderWindowUnit", "window_units"),
        ("ProviderLimitPrecision", "precisions"),
        ("ProviderLimitSource", "sources"),
    ):
        assert set(get_args(getattr(vis, name))) == set(
            _contracts._load_document("provider")["limits"][vocabulary]
        )
    assert set(get_args(vis.ProviderAPIStyle)) == set(
        _contracts._load_document("config")["api_style_aliases"]
    )


def test_provider_limits_match_the_shared_schema():
    window = vis.ProviderLimitWindow("rolling", unit="hour", size=5, resets_at_ms=1234)
    row = vis.ProviderLimit(
        "tokens",
        "Tokens",
        scope="account",
        kind="tokens",
        precision="exact",
        source="provider-api",
        used=25.49,
        limit=100,
        window=window,
        subject={"model_id": "example"},
    )
    error = vis.ProviderError("fixture", "Try later", data={"status": 429})
    report = vis.ProviderLimits(
        limits=[row],
        rpm=0,
        tpm=100,
        note="Measured",
        provider_id="example",
        fetched_at_ms=123,
        error=error,
    )
    assert _contracts.validate("provider", "limit_row", row.to_wire()) == row.to_wire()
    assert (
        _contracts.validate("provider", "report", report.to_wire()) == report.to_wire()
    )
    assert row.to_wire()["is_unlimited"] is False
    assert report.to_wire()["dynamic"]["limits"][0]["window"]["resets_at_ms"] == 1234
    assert vis.ProviderLimits().to_wire() == {
        "status": "ok",
        "static": {},
        "dynamic": {"limits": []},
    }


@pytest.mark.parametrize(
    "make",
    [
        lambda: vis.ProviderPreset(api_style="not-a-dialect"),
        lambda: vis.ProviderPreset(default_models="example"),
        lambda: vis.ProviderPreset(is_hidden="false"),
        lambda: vis.ProviderPreset(llm_headers={"X-Test": 3}),
        lambda: vis.ProviderPreset(extra_body={"number": float("nan")}),
        lambda: vis.ProviderPreset(extra={"base_url": "shadow"}),
        lambda: vis.ProviderPreset(extra={"base-url": "shadow"}),
        lambda: vis.ProviderCredential(""),
        lambda: vis.ProviderCredential(None),
        lambda: vis.ProviderStatus("false"),
        lambda: vis.ProviderStatus(True, extra={"is_authenticated": False}),
        lambda: vis.ProviderModel("example", context=True),
        lambda: vis.ProviderModel("example", context=0),
        lambda: vis.ProviderModel("example", is_tool_call="true"),
        lambda: vis.ProviderLimitWindow("unknown"),
        lambda: vis.ProviderLimitWindow("rolling", size=0),
        lambda: vis.ProviderLimitWindow("rolling", resets_at_ms=True),
        lambda: vis.ProviderLimit(
            "id",
            "label",
            scope="wrong",
            kind="tokens",
            precision="exact",
            source="static",
        ),
        lambda: vis.ProviderLimit(
            "id",
            "label",
            scope="account",
            kind="tokens",
            precision="exact",
            source="static",
            used=float("inf"),
        ),
        lambda: vis.ProviderLimits(limits=[{}]),
        lambda: vis.ProviderLimits(rpm=-1),
        lambda: vis.ProviderLimits(status="wrong"),
        lambda: vis.ProviderError("", "failure"),
        lambda: vis.Provider(
            "example", "Example", preset={"base_url": "http://127.0.0.1"}
        ),
    ],
)
def test_provider_values_reject_invalid_data(make):
    with pytest.raises((TypeError, ValueError)):
        make()


def test_typed_callback_results_cross_only_at_registration_boundary(monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    monkeypatch.setattr(vis._host, "declare_env", lambda _: "{}")
    invoked = []

    def token():
        invoked.append("token")
        return vis.ProviderCredential("fixture", api_url="http://127.0.0.1/v1")

    provider = vis.Provider(
        "example",
        "Example",
        preset=vis.ProviderPreset(api_style="openai"),
        get_token_fn=token,
        detect_fn=token,
        status_fn=lambda: vis.ProviderStatus(True),
        limits_fn=lambda: vis.ProviderLimits(),
        enrich_models_fn=lambda provider, opts: [
            vis.ProviderModel("example", context=200000, is_tool_call=True)
        ],
    )
    assert invoked == []
    assert isinstance(provider.get_token_fn(), vis.ProviderCredential)
    invoked.clear()
    vis.register(
        vis.Extension(
            name="provider-example", description="Provider", providers=[provider]
        )
    )
    assert invoked == []
    spec = vis._registration["spec"]["providers"][0]
    assert spec["get_token_fn"]() == {
        "token": "fixture",
        "api_url": "http://127.0.0.1/v1",
    }
    assert spec["detect_fn"]() == spec["get_token_fn"]()
    assert spec["status_fn"]() == {"is_authenticated": True}
    assert spec["limits_fn"]() == vis.ProviderLimits().to_wire()
    assert spec["enrich_models_fn"]({}, {}) == [
        {"name": "example", "context": 200000, "is_tool_call": True}
    ]


@pytest.mark.parametrize(
    "slot,arity",
    [
        ("get_token_fn", 0),
        ("detect_fn", 0),
        ("status_fn", 0),
        ("logout_fn", 0),
        ("limits_fn", 0),
        ("refresh_token_fn", 1),
        ("auth_fn", 1),
        ("auth_prompt_fn", 0),
        ("enrich_models_fn", 2),
        ("on_selected_fn", 1),
    ],
)
def test_async_and_wrong_arity_callbacks_are_rejected_at_declaration(slot, arity):
    async def asynchronous(*args):
        return None

    with pytest.raises((TypeError, ValueError), match=slot):
        vis.Provider("example", "Example", **{slot: asynchronous})

    def wrong(*, required):
        return None

    with pytest.raises((TypeError, ValueError), match=slot):
        vis.Provider("example", "Example", **{slot: wrong})


@pytest.mark.parametrize(
    "slot,args",
    [
        ("get_token_fn", ()),
        ("detect_fn", ()),
        ("status_fn", ()),
        ("limits_fn", ()),
        ("refresh_token_fn", ("rejected",)),
        ("enrich_models_fn", ({}, {})),
    ],
)
def test_raw_callback_results_are_not_accepted_as_typed_records(slot, args):
    provider = vis.Provider(
        "example", "Example", **{slot: lambda *args: {"token": "fixture"}}
    )
    with pytest.raises(TypeError, match=slot):
        provider._spec()[slot](*args)


def test_refresh_adapts_arity_without_retrying_exceptions():
    attempts = []

    def refresh(rejected=None):
        attempts.append(rejected)
        raise TypeError("fixture body failure")

    callback = vis.Provider("example", "Example", refresh_token_fn=refresh)._spec()[
        "refresh_token_fn"
    ]
    with pytest.raises(TypeError, match="fixture body failure"):
        callback("rejected")
    assert attempts == ["rejected"]
    for function in (
        lambda: vis.ProviderCredential("fresh"),
        lambda rejected: vis.ProviderCredential(rejected or "fresh"),
    ):
        callback = vis.Provider(
            "example", "Example", refresh_token_fn=function
        )._spec()["refresh_token_fn"]
        assert callback()["token"] == "fresh"
        assert callback("rejected")["token"] in ("fresh", "rejected")


def test_hidden_coroutine_result_is_closed_and_rejected():
    async def asynchronous():
        return vis.ProviderCredential("fixture")

    coroutine = asynchronous()
    provider = vis.Provider("example", "Example", get_token_fn=lambda: coroutine)
    with pytest.raises(TypeError, match="get_token_fn"):
        provider._spec()["get_token_fn"]()
    assert inspect.getcoroutinestate(coroutine) == inspect.CORO_CLOSED


def test_non_record_callback_contracts():
    lines = []

    def auth(printer):
        printer("Fixture sign in")
        return "ok"

    selected = []
    provider = vis.Provider(
        "example",
        "Example",
        auth_fn=auth,
        auth_prompt_fn=lambda: ["Fixture guidance"],
        on_selected_fn=selected.append,
    )
    spec = provider._spec()
    assert spec["auth_fn"](lines.append) == "ok"
    assert lines == ["Fixture sign in"]
    assert spec["auth_prompt_fn"]() == ["Fixture guidance"]
    assert spec["on_selected_fn"]({"source": "test"}) is None
    assert selected == [{"source": "test"}]


def test_provider_example_executes_with_the_installed_sdk(monkeypatch):
    example = """
import os
import blockether.vis.extension as vis


def credential() -> vis.ProviderCredential | None:
    token = os.environ.get("EXAMPLE_API_KEY")
    return vis.ProviderCredential(token) if token else None


def status() -> vis.ProviderStatus:
    return vis.ProviderStatus(
        is_authenticated=bool(os.environ.get("EXAMPLE_API_KEY")),
        source="env-var",
    )


vis.register(
    vis.Extension(
        name="provider-example",
        description="An OpenAI-compatible provider.",
        env=["EXAMPLE_API_KEY"],
        providers=[
            vis.Provider(
                id="example",
                label="Example AI",
                preset=vis.ProviderPreset(
                    base_url="https://gateway.example.com/v1",
                    api_style="openai",
                    default_models=["example-model"],
                ),
                get_token_fn=credential,
                status_fn=status,
            )
        ],
    )
)
"""
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    monkeypatch.setenv("EXAMPLE_API_KEY", "fixture-doc-credential")
    monkeypatch.setattr(vis._host, "declare_env", lambda _: "{}")
    namespace = {}
    exec(compile(example, "provider_example", "exec"), namespace)
    provider = vis._registration["spec"]["providers"][0]
    assert provider["preset"]["default_models"] == ["example-model"]
    assert provider["get_token_fn"]()["token"] == "fixture-doc-credential"
    assert provider["status_fn"]()["is_authenticated"] is True
    monkeypatch.delenv("EXAMPLE_API_KEY")
    assert provider["get_token_fn"]() is None
    assert provider["status_fn"]()["is_authenticated"] is False
