"""`vis.tools` calls another active session tool and rebuilds its answer as classes."""

from __future__ import annotations

import dataclasses
import functools
from types import SimpleNamespace

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside
from blockether.vis.activity import ActivityGroup


def _object(name, attrs, **extra):
    return {"__vis_object__": name, "__vis_attrs__": attrs, **extra}


def test_calls_the_tool_python_execution_names_and_answers_its_class(monkeypatch):
    calls = []

    def call_tool(tool, args, kwargs):
        calls.append((tool, args, kwargs))
        return _object("Reservation", {"id": "r-1", "name": "x-login", "label": "x"})

    monkeypatch.setattr(vis._host, "call_tool", call_tool)
    first = vis.tools.spel.reserve("x-login", profile="x-com")
    second = vis.tools["spel.reserve"]("x-login")

    assert calls == [
        ("spel.reserve", ["x-login"], {"profile": "x-com"}),
        ("spel.reserve", ["x-login"], {}),
    ]
    assert repr(vis.tools) == "<vis tools>"
    assert repr(vis.tools.spel.reserve) == "<vis tool spel.reserve>"
    assert type(first).__name__ == "Reservation"
    assert dataclasses.is_dataclass(first)
    assert type(first) is type(second)
    assert first == second
    assert repr(first) == "Reservation(id='r-1', name='x-login', label='x')"
    assert first.id == first["id"] == "r-1"
    with pytest.raises(dataclasses.FrozenInstanceError):
        first.id = "other"
    with pytest.raises(AttributeError, match="available fields: id, name, label"):
        _ = first.session
    with pytest.raises(KeyError, match="Reservation has no field 'session'"):
        first["session"]
    with pytest.raises(TypeError, match="not iterable"):
        iter(first)


def test_rebuilds_nested_objects_sequences_and_json_objects(monkeypatch):
    rows = [
        _object("Browser", {"engine": "chromium"}),
        _object("Browser", {"engine": "webkit"}),
    ]
    answer = _object(
        "Page",
        {"rows": rows, "meta": {"total": 2, "tags": ["a"]}},
        __vis_sequence_field__="rows",
    )
    monkeypatch.setattr(vis._host, "call_tool", lambda tool, args, kwargs: answer)
    page = vis.tools.spel.page()

    assert type(page).__name__ == "Page"
    assert [row.engine for row in page] == ["chromium", "webkit"]
    assert len(page) == 2 and page[1].engine == "webkit"
    assert type(page.rows[0]) is type(page.rows[1])
    assert isinstance(page.meta, dict)
    assert page.meta.total == 2 and page.meta["tags"] == ["a"]
    with pytest.raises(AttributeError, match="fields: total, tags"):
        _ = page.meta.count


@pytest.mark.parametrize(
    ("sequence", "error"),
    [("_rows", ValueError), ("missing", ValueError), ("count", TypeError)],
)
def test_refuses_a_sequence_field_that_names_no_list(monkeypatch, sequence, error):
    answer = _object("Page", {"count": 1}, __vis_sequence_field__=sequence)
    monkeypatch.setattr(vis._host, "call_tool", lambda tool, args, kwargs: answer)
    with pytest.raises(error, match="sequence field"):
        vis.tools.spel.page()


def test_answers_a_model_text_as_its_str(monkeypatch):
    # Regression #367: a model's own `__str__` text crosses; the repr stays the fields.
    answers = iter(
        [
            _object("Status", {"env": "DEV04"}, __vis_text__="STATUS: DEV04"),
            _object("Status", {"env": "DEV05"}),
            _object("Status", {"env": "DEV06"}, __vis_text__=7),
        ]
    )
    monkeypatch.setattr(
        vis._host, "call_tool", lambda tool, args, kwargs: next(answers)
    )
    told, plain, odd = (vis.tools.env.status() for _ in range(3))

    assert str(told) == f"{told}" == "STATUS: DEV04"
    assert repr(told) == "Status(env='DEV04')"
    assert str(plain) == repr(plain) == "Status(env='DEV05')"
    assert str(odd) == repr(odd)
    assert type(plain) is type(odd)


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
    reservation = vis._tool_value(_object("Reservation", {"id": "r-1"}))

    assert vis.tools.spel.open(reservation, targets=(Target("https://x.com"),)) is None
    assert calls == [
        (
            [{"__vis_object__": "Reservation", "__vis_attrs__": {"id": "r-1"}}],
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
