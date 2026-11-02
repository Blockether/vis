"""Test host for the review app: the SDK LiveRecorder, checking every view against the schema."""

import json

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


class Recorder(vis.testing.LiveRecorder):
    """A LiveRecorder that checks each picture against the canonical view schema."""

    def host_live(self, envelope_json):
        answer = super().host_live(envelope_json)
        if json.loads(envelope_json).get("op") in ("open", "patch"):
            public = {
                "id": self.view_id,
                "channel_ids": ["test"],
                "timeout_ms": 0,
                "created_at": 1,
            }
            public.update(json.loads(json.dumps(self._open_view())))
            public["seq"] = self._seq
            _contracts.validate("view", "live_view", public)
        return answer

    def send(self, kind, node_id="", **fields):
        """Give the app one event, as a surface does."""
        with self._condition:
            self._event(kind, node_id, **fields)
            self._condition.notify_all()

    def ids(self):
        return [node["id"] for node in self._open_view()["nodes"]]


@pytest.fixture
def recorder(monkeypatch):
    host = Recorder(vis._host)
    monkeypatch.setattr(vis, "_host", host)
    return host
