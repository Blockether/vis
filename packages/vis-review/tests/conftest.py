"""The SDK test host for the review app."""

import blockether.vis.extension as vis
import pytest


class Recorder(vis.testing.LiveRecorder):
    """Send review-specific events through the SDK contract checks."""

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
