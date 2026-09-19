"""Application-owned extensions keep their callables and state in the SDK process."""

import os
import threading
from dataclasses import dataclass
from unittest.mock import Mock

import blockether.vis.extension as vis
import pytest
from blockether.vis.engine._extensions import ClientExtensions


def extension(fn, *, activity=None, **options):
    return vis.Extension(
        name="application",
        description="Application-owned tools.",
        alias="app",
        symbols=[vis.Symbol(fn, activity=activity or vis.Activity(label="Read state"))],
        **options,
    )


def test_closure_keeps_application_pid_thread_and_state_without_registration(
    monkeypatch,
):
    state = []

    def remember(value: str, *, repeat: int = 1) -> dict:
        """Remember a value in this application; repeat defaults to one."""
        state.extend([value] * repeat)
        return {"pid": os.getpid(), "thread": threading.get_ident(), "state": state}

    monkeypatch.setattr(
        vis._host, "declare_env", Mock(side_effect=AssertionError("host registration"))
    )
    bindings = ClientExtensions([extension(remember)])
    call = {"id": "one", "name": "remember", "args": ["Ada"], "kwargs": {"repeat": 2}}
    result = bindings.dispatch(call, lambda _: True)
    assert result == {
        "status": "success",
        "result": {
            "pid": os.getpid(),
            "thread": threading.get_ident(),
            "state": ["Ada", "Ada"],
        },
    }
    assert bindings.dispatch(call, lambda _: True) == result
    assert state == ["Ada", "Ada"]
    assert "fn" not in bindings.manifest[0]["symbols"][0]
    assert bindings.manifest[0]["symbols"][0]["contract"]["name"] == "remember"


def test_object_namespace_methods_keep_the_original_instance():
    class Counter:
        def __init__(self):
            self._value = 0

        @vis.method(activity=vis.Activity(label="Count calls", show_start=False))
        def increment(self, step: int = 1) -> int:
            """Increment application state; step defaults to one."""
            self._value += step
            return self._value

    counter = Counter()
    bindings = ClientExtensions(
        [
            vis.Extension(
                name="counter",
                description="Count calls.",
                alias="count",
                symbols=[vis.Symbol(counter, name="counter")],
            )
        ]
    )
    assert bindings.manifest[0]["symbols"][0]["name"] == "counter.increment"
    assert (
        bindings.dispatch(
            {
                "id": "one",
                "name": "counter.increment",
                "args": [],
                "kwargs": {"step": 3},
            },
            lambda _: True,
        )["result"]
        == 3
    )
    assert counter._value == 3


def test_activity_running_success_failure_and_empty_are_preserved():
    phases = []
    updates = []

    def render(*, phase, result, error, **_):
        phases.append(phase)
        return vis.ActivityPresentation(
            "Read state", str(error) if error else str(result)
        )

    def read(fail: bool = False):
        """Read no data, or raise when fail is true; fail defaults to false."""
        if fail:
            raise ValueError("Unavailable")
        vis.publish_activity(vis.ActivityPresentation("Read state", "Reading"))
        return []

    bindings = ClientExtensions(
        [extension(read, activity=vis.Activity(label="Read state", render=render))]
    )
    assert bindings.dispatch(
        {"id": "one", "name": "read", "args": [], "kwargs": {}}, updates.append
    ) == {"status": "success", "result": []}
    assert phases == ["start", "success"]
    assert [item["summary"] for item in updates] == ["None", "Reading", "[]"]
    result = bindings.dispatch(
        {"id": "two", "name": "read", "args": [], "kwargs": {"fail": True}},
        updates.append,
    )
    assert result == {
        "status": "failure",
        "error": {"type": "ValueError", "message": "Unavailable"},
    }
    assert phases[-2:] == ["start", "failure"]
    assert vis._activity_publisher.get() is None


def test_end_only_activity_does_not_publish_start():
    phases = []

    def render(*, phase, **_):
        phases.append(phase)
        return None

    def read() -> None:
        """Return no data without side effects."""
        return None

    bindings = ClientExtensions(
        [
            extension(
                read,
                activity=vis.Activity(
                    label="Read state", show_start=False, render=render
                ),
            )
        ]
    )
    assert (
        bindings.dispatch(
            {"id": "one", "name": "read", "args": [], "kwargs": {}}, Mock()
        )["result"]
        is None
    )
    assert phases == ["success"]


def test_replayed_call_id_cannot_change_arguments_or_execute_twice():
    calls = []

    def record(value: int):
        """Record one integer in application memory."""
        calls.append(value)
        raise ValueError("Rejected")

    bindings = ClientExtensions([extension(record)])
    call = {"id": "one", "name": "record", "args": [1], "kwargs": {}}
    first = bindings.dispatch(call, Mock())
    assert bindings.dispatch(call, Mock()) == first
    with pytest.raises(RuntimeError, match="reused"):
        bindings.dispatch({**call, "args": [2]}, Mock())
    assert calls == [1]


@pytest.mark.parametrize(
    "options",
    [
        {"ctx": lambda: {}},
        {"activation": lambda: True},
        {"env": ["PRIVATE_TOKEN"]},
        {"prompt": lambda: "dynamic"},
        {
            "language_tools": [
                vis.LanguageSurface(
                    language="toml", format=lambda options: {"op": "format_code"}
                )
            ]
        },
    ],
)
def test_host_only_fields_fail_before_connection(options):
    with pytest.raises(ValueError, match="client extensions"):
        ClientExtensions(
            [vis.Extension(name="bad", description="Unsupported callback.", **options)]
        )


def test_every_export_requires_activity_and_names_cannot_collide():
    def read():
        """Read no application data."""
        return None

    with pytest.raises(ValueError, match="Activity"):
        ClientExtensions(
            [
                vis.Extension(
                    name="bad",
                    description="Missing Activity.",
                    alias="bad",
                    symbols=[vis.Symbol(read)],
                )
            ]
        )
    with pytest.raises(ValueError, match="duplicate|collision"):
        ClientExtensions([extension(read), extension(read)])


def test_result_records_preserve_null_fields_and_unsupported_results_fail():
    @dataclass(frozen=True)
    class Result:
        count: int
        optional: str | None = None

    def result(unsupported: bool = False):
        """Return a record; unsupported defaults to false."""
        return object() if unsupported else Result(3)

    bindings = ClientExtensions([extension(result)])
    assert bindings.dispatch(
        {"id": "one", "name": "result", "args": [], "kwargs": {}}, Mock()
    )["result"] == {"count": 3, "optional": None}
    failure = bindings.dispatch(
        {"id": "two", "name": "result", "args": [], "kwargs": {"unsupported": True}},
        Mock(),
    )
    assert failure["status"] == "failure"
    assert failure["error"]["type"] == "TypeError"


def test_async_callback_is_awaited_in_application_thread():
    # The CLI test runner itself may own an event loop. Exercise the ordinary
    # synchronous SDK calling context, not an unsupported nested event loop.
    from concurrent.futures import ThreadPoolExecutor

    def run():
        async def identify():
            """Return the current application thread."""
            return threading.get_ident()

        bindings = ClientExtensions([extension(identify)])
        result = bindings.dispatch(
            {"id": "one", "name": "identify", "args": [], "kwargs": {}}, Mock()
        )
        assert result == {"status": "success", "result": threading.get_ident()}

    with ThreadPoolExecutor(max_workers=1) as executor:
        executor.submit(run).result()


def test_failed_result_delivery_can_retry_without_reexecuting_callback():
    calls = []

    def record():
        """Record that this callback ran."""
        calls.append(True)
        return len(calls)

    bindings = ClientExtensions([extension(record)])
    call = {"id": "one", "name": "record", "args": [], "kwargs": {}}
    client = Mock()
    client.get_session_client_calls.return_value = {"calls": [call]}
    client.post_session_client_call_result.side_effect = [OSError("disconnected"), {}]
    with pytest.raises(OSError, match="disconnected"):
        bindings.drain(client, "session")
    bindings.drain(client, "session")
    assert calls == [True]
    assert client.post_session_client_call_result.call_args_list[-1].kwargs["body"] == {
        "status": "success",
        "result": 1,
    }


def test_interrupted_callback_is_not_executed_again():
    calls = []

    def interrupt():
        """Record an invocation and interrupt its caller."""
        calls.append(True)
        raise KeyboardInterrupt()

    bindings = ClientExtensions([extension(interrupt)])
    call = {"id": "one", "name": "interrupt", "args": [], "kwargs": {}}
    with pytest.raises(KeyboardInterrupt):
        bindings.dispatch(call, Mock())
    assert bindings.dispatch(call, Mock())["error"]["type"] == "KeyboardInterrupt"
    assert calls == [True]
    assert vis._activity_publisher.get() is None


def test_callback_receipts_are_bounded_without_evicting_executed_ids(monkeypatch):
    calls = []

    def record():
        """Record one callback invocation."""
        calls.append(True)
        return "retained"

    monkeypatch.setattr("blockether.vis.engine._extensions._MAX_RECEIPT_BYTES", 1)
    bindings = ClientExtensions([extension(record)])
    call = {"id": "one", "name": "record", "args": [], "kwargs": {}}
    result = bindings.dispatch(call, Mock())
    assert bindings.dispatch({**call, "id": "two"}, Mock())["status"] == "failure"
    assert bindings.dispatch(call, Mock()) == result
    assert calls == [True]


@pytest.mark.parametrize("value", [float("nan"), {1: "bad key"}, object()])
def test_nonportable_results_fail_without_serializing_object_code(value):
    def read():
        """Return the selected application object."""
        return value

    bindings = ClientExtensions([extension(read)])
    assert (
        bindings.dispatch(
            {"id": "one", "name": "read", "args": [], "kwargs": {}}, Mock()
        )["status"]
        == "failure"
    )


def test_wrong_thread_is_refused_and_clear_releases_callable_references():
    from concurrent.futures import ThreadPoolExecutor

    def identify():
        """Identify the calling application thread."""
        return threading.get_ident()

    bindings = ClientExtensions([extension(identify)])
    with ThreadPoolExecutor(max_workers=1) as executor:
        future = executor.submit(
            bindings.dispatch,
            {"id": "one", "name": "identify", "args": [], "kwargs": {}},
            Mock(),
        )
        with pytest.raises(RuntimeError, match="calling thread"):
            future.result()
    bindings.clear()
    assert not bindings.declarations
    assert not bindings.manifest
    assert not bindings._functions
