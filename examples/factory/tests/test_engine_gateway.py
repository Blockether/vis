"""Check that the gateway adapter follows the transcript page it receives."""

from __future__ import annotations

from contextlib import nullcontext
from types import SimpleNamespace

from factory.gateway import Gateway


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
