"""The shell handle of an extension answers the calls of the sandbox handle."""

import asyncio
import gc
import json
import threading
import time
import weakref
from concurrent.futures import CancelledError, Future, as_completed, wait

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside


@pytest.fixture
def four_lines():
    return vis.shell("printf 'one\\ntwo\\nthree\\nfour\\n'").result(10)


def test_a_field_answers_by_attribute_and_a_miss_names_the_fields(four_lines):
    assert four_lines.exit == 0
    assert four_lines.out == four_lines.stdout == four_lines.stderr
    assert list(four_lines.keys()) == list(dict(four_lines))
    assert json.loads(json.dumps(four_lines)) == dict(four_lines)
    with pytest.raises(AttributeError) as attribute:
        str(four_lines.outp)
    assert "'outp' is not a field of this shell result" in str(attribute.value)
    assert "Did you mean 'out'?" in str(attribute.value)
    assert "when no dict method has its name: r['exit'] is r.exit." in str(
        attribute.value
    )
    with pytest.raises(KeyError) as key:
        four_lines["nope"]
    assert "'nope' is not a key of 'shell' result." in str(key.value)
    with pytest.raises(KeyError) as key:
        four_lines.logs()["nope"]
    assert "'nope' is not a key of '_shell_logs' result." in str(key.value)


@pytest.mark.parametrize("complete", [True, False])
def test_done_callback_reads_the_outcome_without_a_caller_waiting(complete):
    sh = vis.shell("read word; printf '%s' \"$word\"", stdin="tty")
    seen = []
    observed = threading.Event()

    def receive(future):
        seen.append((future, future.result()["out"], future.exception()))
        observed.set()

    try:
        if complete:
            sh.type("value")
            sh.result(5)
        sh.add_done_callback(receive)
        if not complete:
            sh.type("value")
        assert observed.wait(5)
        assert seen == [(sh, "value", None)]
        assert sh.result(0) is sh.result(0)
    finally:
        sh.cancel()


def test_field_reads_follow_the_process_before_and_after_completion():
    sh = vis.shell("printf start; read word; printf '%s' \"$word\"", stdin="tty")
    try:
        with pytest.raises(AttributeError, match=r"asyncio\.wait.*concurrent\.futures\.wait"):
            _ = sh.wait
        assert wait([sh], timeout=0.02).not_done == {sh}
        deadline = time.monotonic() + 5
        while "start" not in sh.logs().out and time.monotonic() < deadline:
            time.sleep(0.01)
        assert sh.status == "running"
        assert sh["out"] == "start"
        assert sh.get("exit") is None
        assert "timed_out" not in sh.keys()
        assert "timeout_secs" not in sh.keys()
        sh.type("finish")
        assert wait([sh], timeout=5).done == {sh}
        assert sh.status == "exited"
        assert sh["out"] == "startfinish"
        assert sh.get("exit") == 0
    finally:
        sh.cancel()


def test_asyncio_wait_reads_the_live_shell_without_cancelling():
    async def run():
        sh = vis.shell("printf start; read word; printf '%s' \"$word\"", stdin="tty")
        try:
            done, pending = await asyncio.wait([sh], timeout=0.02)
            assert not done and pending == {sh}
            deadline = time.monotonic() + 5
            while "start" not in sh.out and time.monotonic() < deadline:
                await asyncio.sleep(0.01)
            assert sh.status == "running"
            assert sh["out"] == "start"
            assert sh.get("exit") is None
            sh.type("finish")
            done, pending = await asyncio.wait([sh], timeout=5)
            assert done == {sh} and not pending
            assert sh.status == "exited"
            assert sh.out == "startfinish"
            assert sh.exit == 0
        finally:
            sh.cancel()

    asyncio.run(run())


def test_await_shell_result_and_callbacks_in_the_callers_loop():
    async def run():
        sh = vis.shell("read word; printf '%s' \"$word\"", stdin="tty")
        loop = asyncio.get_running_loop()
        loop.set_debug(True)
        try:
            sh.type("value")
            assert (await sh).out == "value"
            callback = loop.create_future()
            sh.add_done_callback(callback.set_result)
            assert not callback.done()
            assert await asyncio.wait_for(callback, 5) is sh
            assert await sh is sh.result(0)
        finally:
            sh.cancel()

    asyncio.run(run())


@pytest.mark.parametrize("timeout", [0, 0.02])
def test_asyncio_wait_for_cancels_the_process(timeout):
    async def run():
        sh = vis.shell("read word", stdin="tty")
        try:
            with pytest.raises(TimeoutError):
                await asyncio.wait_for(sh, timeout)
            assert sh.cancelled()
            assert sh.status == "stopped"
            with pytest.raises(asyncio.CancelledError):
                await sh
        finally:
            sh.cancel()

    asyncio.run(run())


def test_shell_completion_reaches_two_loops_after_another_loop_closes(caplog):
    sh = vis.shell("read word", stdin="tty")
    registered = threading.Barrier(2)
    answered = Future()
    callback_called = threading.Event()

    async def register_closed_loop():
        sh.add_done_callback(lambda _: callback_called.set())

    async def observe(send=False):
        asyncio.get_running_loop().set_debug(True)
        pending = asyncio.create_task(asyncio.wait([sh], timeout=5))
        await asyncio.sleep(0)
        await asyncio.to_thread(registered.wait, 5)
        if send:
            sh.type("finish")
        return await pending

    def other_thread():
        try:
            answered.set_result(asyncio.run(observe()))
        except Exception as error:
            answered.set_exception(error)

    worker = threading.Thread(target=other_thread, daemon=True)
    try:
        asyncio.run(register_closed_loop())
        worker.start()
        assert asyncio.run(observe(send=True)) == ({sh}, set())
        assert answered.result(5) == ({sh}, set())
        assert not callback_called.is_set()
        assert "Event loop is closed" not in caplog.text
    finally:
        sh.cancel()


def test_timed_out_wait_does_not_keep_the_event_loop_alive():
    sh = vis.shell("read word", stdin="tty")

    async def observe():
        assert await asyncio.wait([sh], timeout=0) == (set(), {sh})
        return weakref.ref(asyncio.get_running_loop())

    try:
        loop = asyncio.run(observe())
        gc.collect()
        assert sh.running() and loop() is None
    finally:
        sh.cancel()


def test_live_field_read_uses_the_bounded_tail():
    sh = vis.shell(
        "printf BEGIN; printf '%01000000d' 0; printf END; read word", stdin="tty"
    )
    try:
        deadline = time.monotonic() + 5
        while "END" not in sh.out and time.monotonic() < deadline:
            time.sleep(0.01)
        out = sh.out
        assert out.endswith("END") and "BEGIN" not in out
        assert len(out) <= 262144 and sh.offset > 0
        sh.type("finish")
        assert sh.result(5).out.startswith("BEGIN")
    finally:
        sh.cancel()


def test_live_field_read_does_not_wait_on_the_cancelling_process(monkeypatch):
    cancelling = threading.Event()
    release = threading.Event()
    original = _outside._Run.signal_group

    def signal(run, sig):
        cancelling.set()
        assert release.wait(5)
        original(run, sig)

    monkeypatch.setattr(_outside._Run, "signal_group", signal)
    sh = vis.shell("sleep 30")
    cancel = threading.Thread(target=sh.cancel)
    cancel.start()
    try:
        assert cancelling.wait(5)
        read = Future()
        reader = threading.Thread(
            target=lambda: read.set_result(sh.status), daemon=True
        )
        reader.start()
        assert read.result(1) == "running"
    finally:
        release.set()
        cancel.join(5)
        assert wait([sh], timeout=5).done == {sh}
    assert sh.status == "stopped"
    assert isinstance(sh.exit, int)


@pytest.mark.parametrize("collect", ["wait", "as_completed"])
def test_stdlib_wait_collects_a_shell_without_a_result_reader(collect):
    sh = vis.shell("read word; printf '%s' \"$word\"", stdin="tty")
    try:
        sh.type("observed")
        if collect == "wait":
            assert wait([sh], timeout=2).done == {sh}
        else:
            assert list(as_completed([sh], timeout=2)) == [sh]
        assert sh.result(0).out == "observed"
    finally:
        sh.cancel()


def test_cancelled_shell_notifies_stdlib_waiters_before_callbacks():
    sh = vis.shell("sleep 30")
    reported = threading.Event()
    seen = []

    def finished(future):
        seen.append(wait([future], timeout=0).done)
        reported.set()

    try:
        sh.add_done_callback(finished)
        assert sh.cancel()
        assert list(as_completed([sh], timeout=5)) == [sh]
        assert reported.wait(5)
        assert seen == [{sh}]
        with pytest.raises(CancelledError):
            sh.result(0)
    finally:
        sh.cancel()


@pytest.mark.parametrize(
    ("options", "message"),
    [
        ({"command": "true"}, "takes no command"),
        ({"text": "hi"}, "takes no text payload"),
    ],
)
def test_attach_refuses_command_and_text(options, message):
    with pytest.raises(RuntimeError, match=message):
        vis.shell(op="attach", id="attach-invalid", **options)


@pytest.mark.parametrize(
    ("alias", "cancel"), [(" attach-existing-future ", False), (413, True)]
)
def test_attach_recovers_the_live_generation_without_starting_a_process(alias, cancel):
    with pytest.raises(RuntimeError, match="No running background shell"):
        vis.shell(op="attach", id=alias)
    original = vis.shell("read word", stdin="tty", id=alias)
    try:
        recovered = vis.shell(op="attach", id=alias)
        assert isinstance(recovered, Future)
        assert (recovered.pid, recovered.already_running) == (original.pid, True)
        if cancel:
            assert recovered.cancel()
            with pytest.raises(CancelledError):
                original.result(5)
        else:
            recovered.type("done")
            assert recovered.result(5).exit == original.result(5).exit == 0
        with pytest.raises(RuntimeError, match="No running background shell"):
            vis.shell(op="attach", id=alias)
    finally:
        original.cancel()


def test_timeout_and_late_cancel_do_not_end_a_replacement():
    first = vis.shell("printf old; read word", stdin="tty", id="shared")
    try:
        assert isinstance(first, Future)
        with pytest.raises(TimeoutError):
            first.exception(0.02)
        assert first.running()
        assert first.type("finish") is first
        assert first.result(5)["out"] == "old"
        second = vis.shell("printf new; read word", stdin="tty", id="shared")
        try:
            assert not first.cancel()
            assert first.logs()["out"] == "old"
            assert not second.done()
            assert second.cancel()
            with pytest.raises(CancelledError):
                second.exception(5)
            assert second.done() and second.cancelled()
        finally:
            second.cancel()
    finally:
        first.cancel()


@pytest.mark.parametrize("door", ["shell", "jailed_shell"])
def test_a_command_string_and_keyword_options_make_one_options_map(door):
    start = getattr(vis, door)
    if door == "jailed_shell":
        with pytest.raises(_outside.Refused, match="needs the Vis host's jail"):
            start("true")
    with pytest.raises(TypeError, match=f"{door} takes a command and one options map"):
        start("ls", "src")


def test_a_log_page_walks_forward_by_lines(four_lines):
    page = four_lines.logs(0, 2)
    assert (page["out"], page["offset"], page["next_offset"]) == ("one\ntwo\n", 0, 8)
    assert page["is_eof"] is False
    more = next(page)
    assert (more["out"], more["offset"], more["is_eof"]) == ("three\nfour\n", 8, True)
    assert next(more, None) is None
    pages = [each["out"] for each in four_lines.logs(0, 1).pages()]
    assert pages == ["one\n", "two\n", "three\n", "four\n"]
    assert len(list(four_lines.logs(0, 1).pages(2))) == 2


def test_a_log_page_walks_backward_from_the_end(four_lines):
    page = four_lines.logs(lines=-1)
    assert page["out"] == "four\n"
    above = next(page)
    assert (above["out"], above["offset"]) == ("three\n", 8)
    pages = [each["out"] for each in four_lines.logs(lines=-3).pages()]
    assert pages == ["two\nthree\nfour\n", "one\n"]


def test_the_line_window_takes_each_of_its_names(four_lines):
    assert four_lines.logs(lines=1)["out"] == "four\n"
    assert four_lines.logs(tail=2)["out"] == "three\nfour\n"
    assert four_lines.logs(n=2)["out"] == "three\nfour\n"
    assert four_lines.logs(start=8)["out"] == "three\nfour\n"
    assert four_lines.logs(0, bytes=3)["out"] == "one"
    assert four_lines.logs(14, -1)["out"] == "three\n"


def test_a_log_page_reads_as_text(four_lines):
    page = four_lines.logs(0)
    assert page[-5:] == "four\n"
    assert "log: " + page == "log: one\ntwo\nthree\nfour\n"
    assert page + "end" == "one\ntwo\nthree\nfour\nend"
    assert page.splitlines()[0] == "one"
    assert page.find("three") == 8
    assert four_lines.logs[-5:] == "four\n"
    assert four_lines.logs["exit"] == 0
    assert four_lines.logs.splitlines() == ["one", "two", "three", "four"]
    assert four_lines.logs.count("o") == 3


def test_only_a_log_page_continues(four_lines):
    with pytest.raises(RuntimeError, match="shell log paging starts on a log page"):
        next(four_lines)


def test_the_outside_host_reads_the_option_names_of_the_engine(four_lines):
    def logs(**options):
        return _outside.shell({"op": "logs", "id": four_lines["id"], **options})

    assert logs(n=1)["out"] == "four\n"
    assert logs(tail=1)["out"] == "four\n"
    assert logs(start=14)["out"] == "four\n"
    assert logs(offset=0, bytes=3)["out"] == "one"
    first = logs(offset=0, lines=1)
    assert (first["out"], first["next_offset"], first["is_eof"]) == ("one\n", 4, False)
