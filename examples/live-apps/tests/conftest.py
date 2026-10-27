"""Test host for the proof apps: the SDK LiveRecorder plus app shortcuts.

`press(key)` follows contract C1: a `node_id` shortcut presses that node like a
click, an `action` shortcut gives the owner one `shortcut` event. Every open
and every patch is checked against the canonical view schema.
"""

import json
import sys
from pathlib import Path

import blockether.vis.extension as vis
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from blockether.vis import _contracts  # noqa: E402


class AppRecorder(vis.testing.LiveRecorder):
    """A LiveRecorder that also keeps app shortcuts and checks every view."""

    def _check(self):
        view = self._open_view()
        # The recorder leaves out the engine stamps; give them for the check.
        public = {
            "id": self.view_id,
            "channel_ids": ["test"],
            "timeout_ms": 0,
            "created_at": 1,
        }
        public.update(json.loads(json.dumps(view)))
        public["seq"] = self._seq
        _contracts.validate("view", "live_view", public)

    def host_live(self, envelope_json):
        envelope = json.loads(envelope_json)
        if envelope.get("op") == "patch":
            ops = (envelope.get("patch") or {}).get("ops") or []
            rest = []
            for op in ops:
                if op.get("op") == "shortcuts":
                    with self._condition:
                        if op["shortcuts"]:
                            self._open_view()["shortcuts"] = json.loads(
                                json.dumps(op["shortcuts"])
                            )
                        else:
                            self._open_view().pop("shortcuts", None)
                else:
                    rest.append(op)
            envelope["patch"]["ops"] = rest or [{"op": "shortcuts"}]
            if not rest:
                with self._condition:
                    self._seq += 1
                    self._open_view()["seq"] = self._seq
                    self._condition.notify_all()
                self._check()
                return json.dumps(
                    {"view_id": self.view_id, "is_open": True, "seq": self._seq}
                )
            envelope_json = json.dumps(envelope)
        answer = super().host_live(envelope_json)
        if envelope.get("op") in ("open", "patch") and self._result is None:
            self._check()
            self._check_shortcuts()
        return answer

    def _check_shortcuts(self):
        view = self._open_view()
        keys = [s["key"] for s in view.get("shortcuts") or []]
        assert len(keys) == len(set(keys)), f"duplicate shortcut keys {keys}"
        for item in view.get("shortcuts") or []:
            if "node_id" in item:
                node = self.node(item["node_id"])
                assert node["type"] == "button" or (
                    node["type"] == "card" and node.get("is_selectable")
                ), item

    def shortcuts(self):
        return list(self._open_view().get("shortcuts") or [])

    def press(self, key):
        """Press one app shortcut key, as a surface does."""
        bound = next((s for s in self.shortcuts() if s["key"] == key), None)
        if bound is None:
            return {"is_accepted": False, "reason": "unbound", "key": key}
        if "node_id" in bound:
            return self.activate(bound["node_id"])
        with self._condition:
            self._event("shortcut", "", action=bound["action"], key=key)
            self._condition.notify_all()
        return {"is_accepted": True, "key": key, "shortcut": bound["action"]}

    def choose(self, node_id, ids):
        """Select table rows; the owner gets a `select` event (decision D3)."""
        answer = self.select(node_id, ids)
        with self._condition:
            self._event("select", node_id, item_ids=[str(i) for i in ids])
        return answer

    def type_text(self, node_id, value):
        return self.change(node_id, value)


@pytest.fixture
def recorder(monkeypatch):
    host = AppRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", host)
    return host
