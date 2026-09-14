"""Application callback identity, discovery and lifecycle through real SDK transports.

These use the existing isolated engine and deterministic model fixture. Set
VIS_TEST_LOCAL_COMMAND to a matching built engine; no paid model calls are made.
"""

import os
import threading
from dataclasses import dataclass

import blockether.vis.extension as vis
import pytest
from blockether.vis.engine import Agent
from test_engine import activity_rows, sdk_fixture

CALLBACK_CODE = """
if "capture" not in globals():
    print("CLIENT_EXTENSION_NOT_ATTACHED")
else:
    print(apropos(r"^capture$|^counter.increment$"))
    print(doc("capture"))
    print(doc("counter.increment"))
    receipt = await capture("Ada")
    print("OWNER_PID", receipt["pid"])
    print("OWNER_THREAD", receipt["thread"])
    print("NULL_PRESERVED", receipt["optional"] is None)
    print("BOUND_VALUE", await counter.increment(step=2))
    print("EMPTY_VALUE", await empty_result())
    try:
        await fail_callback()
    except Exception as error:
        print("EXPECTED_FAILURE", "Expected application failure" in str(error))
"""


def tool_output(requests):
    return "\n".join(
        str(message["content"])
        for request in requests
        for message in request["messages"]
        if message["role"] == "tool"
    )


@pytest.mark.parametrize("transport", ["http", "stdio"])
def test_application_extensions_identity_activity_and_session_lifetime(
    tmp_path, monkeypatch, transport
):
    calls, phases = [], []
    owner_thread = threading.get_ident()

    @dataclass(frozen=True)
    class Receipt:
        pid: int
        thread: int
        optional: str | None = None

    def capture(name: str) -> Receipt:
        """Record one name in the SDK application and return its process identity."""
        calls.append((name, os.getpid(), threading.get_ident()))
        return Receipt(os.getpid(), threading.get_ident())

    def empty_result() -> list:
        """Return an empty application result without side effects."""
        return []

    def fail_callback() -> None:
        """Raise a predictable application error without changing data."""
        raise ValueError("Expected application failure")

    def presentation(label):
        def render(*, phase, result, error, **_):
            phases.append((label, phase, threading.get_ident()))
            summary = (
                str(error)
                if error is not None
                else "No items"
                if result == []
                else phase
            )
            return vis.ActivityPresentation(label, summary)

        return render

    class Counter:
        def __init__(self):
            self._value = 0

        @vis.method(activity=vis.Activity(label="Increment counter", show_start=False))
        def increment(self, *, step: int = 1) -> int:
            """Increment this application's counter; step defaults to one."""
            assert threading.get_ident() == owner_thread
            self._value += step
            return self._value

    counter = Counter()
    functions = vis.Extension(
        name="application-functions",
        alias="application",
        description="Tools backed by this application's live objects.",
        prompt="Use capture to identify the SDK application.",
        symbols=[
            vis.Symbol(
                capture,
                activity=vis.Activity(
                    label="Capture name", render=presentation("Capture name")
                ),
            ),
            vis.Symbol(
                empty_result,
                activity=vis.Activity(
                    label="Read empty result",
                    show_start=False,
                    render=presentation("Read empty result"),
                ),
            ),
            vis.Symbol(
                fail_callback,
                activity=vis.Activity(
                    label="Fail callback", render=presentation("Fail callback")
                ),
            ),
        ],
    )
    methods = vis.Extension(
        name="application-counter",
        alias="counter",
        description="An existing mutable application counter.",
        symbols=[vis.Symbol(counter, name="counter")],
    )
    with sdk_fixture(tmp_path, monkeypatch, transport, tool_code=CALLBACK_CODE) as (
        client,
        work,
        requests,
    ):
        with Agent(work, execution_layer=client, extensions=[functions]) as agent:
            # Both public forms use the same Extension, including inside a context.
            agent.register_extension(methods)
            session_id = agent.session.id
            turn = agent.send("Exercise the application callbacks.")
            seen = []
            with agent.session.events(cursor=turn.cursor, reconnects=0) as events:
                for event in events:
                    seen.append(event)
                    if event.turn_id == turn.id and event.type in {
                        "turn.completed",
                        "turn.failed",
                        "turn.cancelled",
                    }:
                        break
                    assert len(seen) < 300, [item.type for item in seen]
            result = turn.wait(timeout=60)
            assert result["status"] == "completed", result
            assert calls == [("Ada", os.getpid(), owner_thread)]
            assert counter._value == 2
            text = tool_output(requests)
            assert f"OWNER_PID {os.getpid()}" in text
            assert f"OWNER_THREAD {owner_thread}" in text
            assert "NULL_PRESERVED True" in text
            assert "BOUND_VALUE 2" in text
            assert "EMPTY_VALUE []" in text
            assert "EXPECTED_FAILURE True" in text
            assert "Record one name in the SDK application" in text
            assert "step defaults to one" in text
            assert ("Capture name", "start", owner_thread) in phases
            assert ("Capture name", "success", owner_thread) in phases
            assert ("Fail callback", "failure", owner_thread) in phases
            assert ("Read empty result", "start", owner_thread) not in phases
            rows = [row for event in seen for row in activity_rows(event.activity)]
            presentations = [row.presentation for row in rows if row.presentation]
            assert any(
                item["headline"] == "Capture name" and item["summary"] == "success"
                for item in presentations
            ), presentations
            assert any(
                item["headline"] == "Read empty result"
                and item["summary"] == "No items"
                for item in presentations
            ), presentations
            assert any(
                item["headline"] == "Fail callback"
                and "Expected application failure" in item["summary"]
                for item in presentations
            ), presentations

            # Recreating the engine environment must keep application objects alive.
            client.post_session_release(session_id)
            # Waiting, not only event iteration, must service application calls.
            assert (
                agent.run("Exercise the same callbacks again.", timeout=60)["status"]
                == "completed"
            )
            assert len(calls) == 2 and counter._value == 4
            with Agent(work, execution_layer=client) as unrelated:
                assert (
                    unrelated.run("Inspect available callbacks.", timeout=60)["status"]
                    == "completed"
                )
            assert "CLIENT_EXTENSION_NOT_ATTACHED" in tool_output(requests)
            assert len(calls) == 2 and counter._value == 4

        # Closing an Agent detaches only its callbacks, not the caller's layer.
        retained = client.session(session_id)
        assert (
            retained.send("Inspect callbacks after Agent closes.").wait(timeout=60)[
                "status"
            ]
            == "completed"
        )
        assert len(calls) == 2 and counter._value == 4
        if transport == "stdio":
            assert client._process.poll() is None


@pytest.mark.parametrize("transport", ["http", "stdio"])
def test_cancelled_turn_does_not_reexecute_a_late_application_result(
    tmp_path, monkeypatch, transport
):
    pending = {}
    executions = []

    def cancel_from_application() -> str:
        """Cancel this test turn and return a deliberately late callback result."""
        executions.append(threading.get_ident())
        pending["turn"].cancel()
        return "late result"

    extension = vis.Extension(
        name="application-cancel",
        alias="cancel",
        description="Deterministic cancellation from the SDK application.",
        symbols=[
            vis.Symbol(
                cancel_from_application, activity=vis.Activity(label="Cancel test turn")
            )
        ],
    )
    with sdk_fixture(
        tmp_path,
        monkeypatch,
        transport,
        tool_code="print(await cancel_from_application())",
    ) as (client, work, _):
        with Agent(work, execution_layer=client, extensions=[extension]) as agent:
            pending["turn"] = agent.send("Cancel from inside the application callback.")
            result = pending["turn"].wait(timeout=60)
            assert result["status"] == "cancelled", result
            assert executions == [threading.get_ident()]
            assert pending["turn"].wait(timeout=5)["status"] == "cancelled"
            assert executions == [threading.get_ident()]
