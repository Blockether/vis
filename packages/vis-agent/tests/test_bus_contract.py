"""The bus outside Vis delivers as the engine does.

`vis-contract/fixtures/bus-delivery.json` holds the runner and the engine answers.
The engine test `bus-delivery-contract-test` runs the same runner in a real worker.
"""

import json
import time

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


def test_bus_rejects_a_coroutine_function_before_subscribing(monkeypatch):
    def unexpected(*args):
        pytest.fail("an invalid handler must not be registered")

    async def handler(signal):
        pass

    monkeypatch.setattr(vis._host, "bus_subscribe", unexpected)
    with pytest.raises(TypeError, match="synchronous"):
        vis.bus.subscribe("readiness.test", handler)


TABLE = json.loads(
    (_contracts._DATA / "fixtures" / "bus-delivery.json").read_text(encoding="utf-8")
)


@pytest.fixture(scope="module")
def answers():
    """Run the runner once: the answers of the calls, then what the handlers kept."""
    runner: dict = {}
    exec("\n".join(TABLE["runner"]), runner)
    calls = runner["start"](vis)
    deadline = time.monotonic() + 30
    handlers = runner["finish"](vis)
    while handlers is None and time.monotonic() < deadline:
        time.sleep(0.05)
        handlers = runner["finish"](vis)
    assert handlers is not None, "a signal did not come in 30 s"
    return {name: {"calls": calls[name], "handlers": handlers[name]} for name in calls}


def test_the_runner_has_the_cases_of_the_fixture(answers):
    assert set(answers) == {case["name"] for case in TABLE["cases"]}


@pytest.mark.parametrize("case", TABLE["cases"], ids=lambda case: case["name"])
def test_the_outside_bus_gives_the_engine_answer(case, answers):
    assert answers[case["name"]] == case["expect"]
