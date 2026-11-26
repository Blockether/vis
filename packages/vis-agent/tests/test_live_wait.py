"""Live readiness is one-shot, reusable after a wait timeout, and cancellable."""

import asyncio
import json
import threading
from concurrent.futures import wait

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside


@pytest.fixture(params=["outside", "recorder"])
def live_host(monkeypatch, request):
    host = (
        _outside.host
        if request.param == "outside"
        else vis.testing.LiveRecorder(vis._host)
    )
    monkeypatch.setattr(vis, "_host", host)
    return host


def test_changed_waits_without_cancelling_and_keeps_the_first_transition(live_host):
    async def exercise(view):
        watch = view.changed()
        done, pending = await asyncio.wait([watch], timeout=0.02)
        assert (done, pending) == (set(), {watch})
        assert not watch.cancelled()
        view["now"].set("Changed")
        view.flush()
        view.close()
        assert await asyncio.wait_for(watch, timeout=1) == "changed"
        assert await view.changed() == "closed"

    with vis.live("Idle", [vis.ui.status("now", "Ready")], flush_ms=0) as view:
        asyncio.run(exercise(view))


def test_cancelling_unsubscribes_and_does_not_close(live_host):
    with vis.live("Waiting", [vis.ui.status("now", "Ready")]) as view:
        watch = view.changed()
        assert wait([watch], timeout=0.01).not_done == {watch}
        assert watch.cancel()
        assert wait([watch], timeout=1).done == {watch}
        assert watch.cancelled()
        assert view.state()["nodes"][0]["text"] == "Ready"
        next_watch = view.changed()
        view.close()
        assert wait([next_watch], timeout=1).done == {next_watch}
        assert next_watch.result() == "closed"


def test_unread_patch_is_not_hidden_by_an_owner_patch(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Changes", [vis.ui.status("now", "Ready")], flush_ms=0) as view:
        view["now"].set(text="Running")
        watch = view.changed()
        assert not watch.done()
        watch.cancel()
        recorder.host_live(
            json.dumps({
                "op": "patch",
                "view_id": view.view_id,
                "patch": {"ops": [{"op": "set", "node_id": "now", "tone": "ok"}]},
            })
        )
        view["now"].set(text="Done")
        assert view.changed().result(timeout=1) == "changed"
        view.state()
        watch = view.changed()
        assert not watch.done()
        watch.cancel()


def test_removed_live_wait_methods_name_the_async_replacement(live_host):
    with vis.live("Wait", [vis.ui.status("now", "Ready")]) as view:
        with pytest.raises(AttributeError, match=r"asyncio\.wait\(\[view\.changed"):
            getattr(view, "sleep")  # noqa: B009
        with pytest.raises(TypeError, match=r"asyncio\.wait\(\[view\.changed"):
            view.events(timeout=1)



def test_sdk_cancel_does_not_wait_for_unwatch(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    send = recorder.live
    unwatch_sent = threading.Event()
    release_unwatch = threading.Event()
    unwatch_timed_out = threading.Event()

    def delayed_door(raw, listener=None):
        if json.loads(raw).get("op") == "unwatch":
            unwatch_sent.set()
            if not release_unwatch.wait(2):
                unwatch_timed_out.set()
        return send(raw, listener)

    monkeypatch.setattr(recorder, "live", delayed_door)

    async def exercise(view):
        watch = view.changed()
        watch.cancel()
        assert watch.cancelled()
        # Let the event loop release the door. A synchronous cancellation would
        # hold this thread until the door's timeout instead.
        assert await asyncio.to_thread(unwatch_sent.wait, 1)
        assert not unwatch_timed_out.is_set()
        release_unwatch.set()
        done, pending = await asyncio.wait([watch], timeout=0)
        assert (done, pending) == ({watch}, set())

    with vis.live("Wait", [vis.ui.status("now", "Ready")]) as view:
        # A second wait timeout guards the test while the fake door is blocked.
        async def check():
            async with asyncio.timeout(1):
                await exercise(view)

        try:
            asyncio.run(check())
        finally:
            release_unwatch.set()


def test_closed_watch_updates_interrupted_before_the_next_read(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Wait", [vis.ui.status("now", "Ready")], flush_ms=60_000) as view:
        assert view.is_interrupted is False
        watch = view.changed()
        recorder.close(reason="interrupted")
        assert watch.result(timeout=1) == "closed"
        assert view.is_interrupted is True
        assert view.reason == "interrupted"
        assert view.reason == "interrupted"
