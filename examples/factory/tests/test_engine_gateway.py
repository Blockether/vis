"""Check that the gateway adapter follows the transcript page it receives."""

from __future__ import annotations

from contextlib import nullcontext
from types import SimpleNamespace

import pytest

from factory import settings
from factory.gateway import Gateway, provider_of
from factory.records import Refused


def test_transcript_uses_the_returned_offset(monkeypatch):
    """Do not repeat turns when the gateway trims rows from a large page."""
    offsets = []

    def transcript(session, query):
        """Return a first page shorter than the caller expected."""
        offset = query["offset"]
        offsets.append(offset)
        if offset == 0:
            return {"turns": [{"request": "One"}], "offset": 2, "has_more": True}
        return {"turns": [{"request": "Two"}], "offset": 3, "has_more": False}

    gateway = Gateway()
    client = SimpleNamespace(routes=SimpleNamespace(get_session_transcript=transcript))
    monkeypatch.setattr(gateway, "client", lambda: nullcontext(client))
    assert gateway.transcript("session") == [{"request": "One"}, {"request": "Two"}]
    assert offsets == [0, 3]


def test_session_of_a_run_gets_the_provider_that_has_its_model(monkeypatch):
    """A model without a provider uses the first provider of the gateway that has it, and no session starts for an unknown model."""
    created = []
    offered = (SimpleNamespace(provider="first", name="small"), SimpleNamespace(provider="second", name="large"), SimpleNamespace(provider="third", name="large"))
    client = SimpleNamespace(
        models=lambda is_available: offered, group=lambda name, root: name, session_options=lambda root: {}, create_session=lambda **fields: created.append((fields["provider"], fields["model"]))
    )
    monkeypatch.setattr(settings, "PROVIDER", "")
    gateway = Gateway()
    gateway.create(client, group="runs", key="key", title="Run", uses={"provider": None, "model": "large"})
    gateway.create(client, group="runs", key="key", title="Run", uses={"provider": "third", "model": "large"})
    gateway.create(client, group="runs", key="key", title="Run")
    assert created == [("second", "large"), ("third", "large"), (None, None)]
    with pytest.raises(Refused):
        gateway.create(client, group="runs", key="key", title="Run", uses={"provider": None, "model": "absent"})
    assert len(created) == 3
    monkeypatch.setattr(settings, "PROVIDER", "stack")
    assert provider_of(client, "absent") == "stack"
