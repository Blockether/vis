"""The shell handle outside Vis gives the answers of the engine.

`vis-contract/fixtures/shell-handle.json` holds the calls and the engine answers.
The engine test `shell-handle-contract-test` runs the same cases in a real worker.
"""

import json

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts

TABLE = json.loads(
    (_contracts._DATA / "fixtures" / "shell-handle.json").read_text(encoding="utf-8")
)
RUNNER: dict = {}
exec("\n".join(TABLE["runner"]), RUNNER)


@pytest.mark.parametrize("case", TABLE["cases"], ids=lambda case: case["name"])
def test_the_outside_shell_gives_the_engine_answer(case):
    # A named exception holds the answer of this host; each other case holds the
    # answer of the engine.
    expect = case.get("outside", case)["expect"]
    assert RUNNER["drive"](vis.shell, case) == expect


def test_each_exception_names_its_reason_and_is_a_real_difference():
    for case in TABLE["cases"]:
        for door in ("outside", "tools"):
            exception = case.get(door)
            if exception is not None:
                assert exception["reason"] in TABLE["exceptions"], case["name"]
                assert exception["expect"] != case["expect"], case["name"]
