"""Live readiness is one-shot, reusable after a wait timeout, and cancellable."""

import asyncio
import json
import threading
from concurrent.futures import InvalidStateError, wait

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


def test_live_validator_rejects_async_function_before_open():
    async def check(value):
        return None

    class Validator:
        async def __call__(self, value):
            return None

    for validator in (check, Validator()):
        with pytest.raises(TypeError, match="synchronous"):
            vis.live(
                "Validate",
                [vis.ui.plaintext("answer", label="Answer", validate=validator)],
            )


def test_recorder_activation_settles_an_armed_watch_as_an_event(monkeypatch):
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    with vis.live("Watch", [vis.ui.button("go", "Go")]) as view:
        watch = view.changed()
        assert not watch.done()
        recorder.activate("go")
        assert wait([watch], timeout=1).done == {watch}
        assert watch.result() == "event"
        assert [(event.type, event.node_id) for event in view.events()] == [
            ("activate", "go")
        ]


def test_pending_task_reads_do_not_wait():
    task = vis._HostFuture()
    guard = threading.Timer(0.15, task.set_result, args=("too late",))
    guard.start()
    try:
        with pytest.raises(InvalidStateError):
            task.result()
        with pytest.raises(InvalidStateError):
            task.exception()
    finally:
        guard.cancel()
        guard.join()


def test_cancelling_direct_await_does_not_wait_for_owned_work_to_end():
    requested = threading.Event()

    class OwnedTask(vis._HostFuture):
        _vis_cancel_on_await = True

        def cancel(self, msg=None):
            requested.set()
            return True

    async def check():
        task = OwnedTask()

        async def observe():
            await task

        waiter = asyncio.create_task(observe())
        await asyncio.sleep(0)
        waiter.cancel()
        try:
            assert await asyncio.to_thread(requested.wait, 1)
            done, pending = await asyncio.wait([waiter], timeout=0.1)
            assert (done, pending) == ({waiter}, set())
            assert waiter.cancelled()
            assert not task.done()
        finally:
            task.set_result("ended")
            await asyncio.gather(waiter, return_exceptions=True)

    asyncio.run(check())


@pytest.mark.parametrize("timeout", [0, 0.02])
def test_attached_shell_wait_for_does_not_request_cancellation(timeout):
    ended = threading.Event()
    cancelled = threading.Event()

    def call(token, op, options):
        if op == "cancel":
            cancelled.set()
            return True
        assert op == "_task_await_batch"
        return [{"state": "missing" if ended.is_set() else "running"} for _ in options]

    async def check():
        task = vis.ShellFuture(
            {
                "token": "held",
                "snapshot": {
                    "id": "held",
                    "status": "running",
                    "already_running": True,
                },
            },
            call,
        )
        try:
            with pytest.raises(TimeoutError):
                await asyncio.wait_for(task, timeout=timeout)
            assert not task.done()
            assert not cancelled.is_set()
        finally:
            ended.set()
        assert await asyncio.wait([task], timeout=1) == ({task}, set())
        with pytest.raises(RuntimeError, match="missing"):
            await asyncio.wait_for(task, timeout=0)

    asyncio.run(check())


def test_idle_shell_door_calls_do_not_grow_with_task_count():
    import time

    def measure(size):
        observed = threading.Event()
        ended = threading.Event()
        reads = []

        def door(_token, op, requests):
            assert op == "_task_await_batch"
            reads.append(len(requests))
            if len(requests) == size:
                observed.set()
            return [
                {"state": "done", "value": {"id": token, "status": "exited"}}
                if ended.is_set()
                else {"state": "running"}
                for token, _ in requests
            ]

        handles = [
            vis.ShellFuture(
                {"token": i, "snapshot": {"id": i, "status": "running"}}, door
            )
            for i in range(size)
        ]
        try:
            assert observed.wait(1)
            reads.clear()
            time.sleep(0.205)
            count = len(reads)
            assert not any(handle.done() for handle in handles)
        finally:
            ended.set()
        assert wait(handles, timeout=1).done == set(handles)
        assert [handle.result()["id"] for handle in handles] == list(range(size))
        return count

    counts = [measure(size) for size in (1, 8, 32)]
    assert min(counts) > 0 and max(counts) - min(counts) <= 1, counts


def test_idle_view_door_calls_do_not_grow_with_watch_count(monkeypatch):
    import time

    def measure(size):
        observed = threading.Event()
        reads = []
        original = _outside.host.live

        def counted(text, listener=None):
            if json.loads(text)["op"] in ("watch-result", "_task_await_batch"):
                reads.append(1)
                observed.set()
            return original(text, listener)

        with monkeypatch.context() as patch:
            patch.setattr(_outside.host, "live", counted)
            patch.setattr(vis, "_host", _outside.host)
            view = vis.live("Idle", [vis.ui.status("now", "Ready")])
            watches = [view.changed() for _ in range(size)]
            try:
                assert observed.wait(1)
                reads.clear()
                time.sleep(0.205)
                count = len(reads)
                assert not any(watch.done() for watch in watches)
            finally:
                view.close()
            assert wait(watches, timeout=1).done == set(watches)
            assert {watch.result() for watch in watches} == {"closed"}
        return count

    counts = [measure(size) for size in (1, 8, 32)]
    assert min(counts) > 0 and max(counts) - min(counts) <= 1, counts


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
            json.dumps(
                {
                    "op": "patch",
                    "view_id": view.view_id,
                    "patch": {"ops": [{"op": "set", "node_id": "now", "tone": "ok"}]},
                }
            )
        )
        view["now"].set(text="Done")
        watch = view.changed()
        assert wait([watch], timeout=1).done == {watch}
        assert watch.result() == "changed"
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
        assert wait([watch], timeout=1).done == {watch}
        assert watch.result() == "closed"
        assert view.is_interrupted is True
        assert view.reason == "interrupted"
        assert view.reason == "interrupted"
