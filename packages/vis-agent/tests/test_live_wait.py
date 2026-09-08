"""Blocking live waits cross the host seam once and wake on changes."""

import json
from concurrent.futures import ThreadPoolExecutor
from threading import Event

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside


@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_idle_wait_is_one_small_host_response(monkeypatch, host_kind):
    host = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    calls = []
    original = host.live

    def live(envelope):
        answer = original(envelope)
        calls.append((json.loads(envelope), json.loads(answer)))
        return answer

    monkeypatch.setattr(host, "live", live)
    monkeypatch.setattr(vis, "_host", host)
    with vis.live("Idle", [vis.status("now", "Ready")]) as view:
        calls.clear()
        assert view.sleep(0) is False
        assert calls == []
        assert view.sleep(0.03) is False
        assert len(calls) == 1
        request, answer = calls[0]
        assert request["timeout_ms"] == 30
        assert answer["timed_out"] is True
        assert "view" not in answer


@pytest.mark.parametrize("action", ["patch", "close"])
@pytest.mark.parametrize("host_kind", ["outside", "recorder"])
def test_wait_wakes_on_change_or_stop(monkeypatch, action, host_kind):
    recorder = (
        _outside.host if host_kind == "outside" else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", recorder)
    entered = Event()
    original = recorder.live

    def live(envelope):
        if json.loads(envelope).get("timeout_ms"):
            entered.set()
        return original(envelope)

    monkeypatch.setattr(recorder, "live", live)
    with vis.live("Waiting", [vis.status("now", "Ready")]) as view:
        with ThreadPoolExecutor(max_workers=1) as pool:
            waiter = pool.submit(view.sleep, 1)
            assert entered.wait(0.5)
            if action == "close":
                original(
                    json.dumps(
                        {
                            "op": "close",
                            "view_id": view.view_id,
                            "ending": {"reason": "interrupted"},
                        }
                    )
                )
            else:
                original(
                    json.dumps(
                        {
                            "op": "patch",
                            "view_id": view.view_id,
                            "patch": {
                                "ops": [
                                    {"op": "set", "node_id": "now", "text": "Changed"}
                                ]
                            },
                        }
                    )
                )
            assert waiter.result(timeout=0.5) is True
        assert view.is_interrupted is (action == "close")


def test_own_patch_does_not_wake_but_interleaved_human_change_does(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Changes", [vis.status("now", "Ready")], flush_ms=0) as view:
        view["now"].set(text="Running")
        assert view.sleep(0.01) is False
        recorder.host_live(
            json.dumps(
                {
                    "op": "patch",
                    "view_id": view.view_id,
                    "patch": {"ops": [{"op": "set", "node_id": "now", "tone": "ok"}]},
                }
            )
        )
        view["now"].set(text="Done")
        assert view.sleep(1) is True
        assert view.sleep(0.01) is False


@pytest.mark.parametrize("seconds", [float("nan"), float("inf"), 86401])
def test_invalid_wait_duration_is_refused_before_host_call(monkeypatch, seconds):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Invalid", [vis.status("now", "Ready")]) as view:
        before = len(recorder.said)
        with pytest.raises(ValueError, match="duration"):
            view.sleep(seconds)
        assert len(recorder.said) == before
