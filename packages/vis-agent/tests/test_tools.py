"""`vis.tools` calls another active session tool and sends its arguments as data.

The Vis runtime builds the answer; `extension_tools_test.clj` checks it in the engine.
"""

from __future__ import annotations

import dataclasses
import functools
from types import SimpleNamespace

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside
from blockether.vis.activity import ActivityGroup


def test_calls_the_tool_python_execution_names_and_answers_the_host_value(monkeypatch):
    calls = []
    answer = object()

    def call_tool(tool, args, kwargs):
        calls.append((tool, args, kwargs))
        return answer

    monkeypatch.setattr(vis._host, "call_tool", call_tool)

    assert vis.tools.spel.reserve("x-login", profile="x-com") is answer
    assert vis.tools["spel.reserve"]("x-login") is answer
    assert calls == [
        ("spel.reserve", ["x-login"], {"profile": "x-com"}),
        ("spel.reserve", ["x-login"], {}),
    ]
    assert repr(vis.tools) == "<vis tools>"
    assert repr(vis.tools.spel.reserve) == "<vis tool spel.reserve>"


@dataclasses.dataclass(frozen=True)
class Status:
    """A model with derived data, as a project package defines it."""

    env: str
    phases: tuple[int, ...] = ()

    @property
    def next_phase(self) -> int:
        return len(self.phases) + 1

    @functools.cached_property
    def label(self) -> str:
        return self.env.lower()

    @property
    def _hidden(self) -> int:
        return 0

    def text_report(self) -> str:
        return f"STATUS: {self.env}"

    __str__ = text_report


def report(status: Status) -> str:
    return status.text_report()


def test_sends_model_properties_and_rebuilds_records_that_carry_them():
    # Regression #367: public properties travel as fields and do not block a rebuild.
    status = Status("DEV04", (1,))
    assert vis._derived_names(Status) == ("next_phase", "label")
    assert vis._derived_text(status) == "STATUS: DEV04"
    assert vis._derived_names(ActivityGroup) == ()
    assert vis._derived_text(ActivityGroup("g", "Run", ())) is None
    sent = {"env": "DEV04", "phases": [1], "next_phase": 2, "label": "dev04"}
    args, _ = vis._call_arguments(report, [sent], {})
    [record] = args
    assert isinstance(record, Status)
    assert (record.env, list(record.phases)) == ("DEV04", [1])
    with pytest.raises(TypeError, match="unknown fields other"):
        vis._call_arguments(report, [{**sent, "other": 1}], {})


def test_sends_dataclass_arguments_as_their_public_fields(monkeypatch):
    @dataclasses.dataclass(frozen=True)
    class Target:
        url: str
        _secret: str = "hidden"

    calls = []
    monkeypatch.setattr(
        vis._host, "call_tool", lambda tool, args, kwargs: calls.append((args, kwargs))
    )

    assert vis.tools.spel.open("r-1", targets=(Target("https://x.com"),)) is None
    assert calls == [
        (
            ["r-1"],
            {
                "targets": [
                    {
                        "__vis_object__": "Target",
                        "__vis_attrs__": {"url": "https://x.com"},
                    }
                ]
            },
        )
    ]
    assert vis.tools.spel.open(SimpleNamespace(id="r-2", _x=1)) is None
    assert calls[-1] == ([{"id": "r-2"}], {})


def test_needs_a_tool_name():
    with pytest.raises(TypeError, match="vis.tools is not a tool"):
        vis.tools()
    with pytest.raises(TypeError, match="takes a tool name"):
        vis.tools[""]
    with pytest.raises(AttributeError):
        _ = vis.tools._private


def test_refuses_outside_a_vis_session():
    with pytest.raises(
        _outside.Refused,
        match=r"vis\.tools needs a bound Vis session to run `spel\.reserve`",
    ):
        vis.tools.spel.reserve("x-login")
