"""A repair decision carries text and visible notes, not a file write."""

import blockether.vis.extension as vis
import pytest


def test_repair_is_a_pure_decision():
    assert vis.repair("(a)\n", notes=["Added a closing parenthesis."]) == {
        "marker": "repair",
        "source": "(a)\n",
        "notes": ["Added a closing parenthesis."],
    }


@pytest.mark.parametrize(
    "source,notes",
    [(None, ["Added a bracket."]), ("x", []), ("x", "note"), ("x", [""]), ("x", [1])],
)
def test_repair_rejects_invalid_decisions(source, notes):
    with pytest.raises((TypeError, ValueError)):
        vis.repair(source, notes=notes)
