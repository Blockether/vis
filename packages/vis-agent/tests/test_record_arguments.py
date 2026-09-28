"""Regression coverage for #289: record arguments are rebuilt from their fields."""

from __future__ import annotations

import functools
import sys
import types
from dataclasses import dataclass, field
from typing import Annotated, Optional

import blockether.vis.extension as vis
import pytest


@dataclass(frozen=True)
class Job:
    number: int


@dataclass(frozen=True)
class Build:
    number: int
    state: str


@dataclass(frozen=True)
class Owner:
    name: str


@dataclass(frozen=True)
class TriggerResult:
    http_code: int
    job_path: str
    jobs: list["Job"] = field(default_factory=list)  # noqa: UP037 — a quoted item
    owner: Optional[Owner] = None  # noqa: UP045 — typing.Optional spelling
    _cache: dict = field(default_factory=dict, compare=False)


@dataclass
class Summary:
    count: int
    total: int = field(init=False)

    def __post_init__(self):
        if self.count < 0:
            raise ValueError("count must not be negative")
        self.total = self.count * 2


FIELDS = {
    "http_code": 201,
    "job_path": "a/b",
    "jobs": [{"number": 7}],
    "owner": {"name": "ci"},
}
RECORD = TriggerResult(201, "a/b", [Job(7)], Owner("ci"))


def status(result: TriggerResult, *, verbose: bool = False) -> str:
    if not isinstance(result, TriggerResult):
        raise TypeError(f"result must be a TriggerResult, not {type(result).__name__}")
    return result.job_path


def count(summary: Summary) -> int:
    return summary.total


def rebuilt(fn, *args, **kwargs):
    return vis._call_arguments(fn, list(args), kwargs)


def test_record_argument_is_rebuilt_positionally_and_by_keyword():
    args, kwargs = rebuilt(status, FIELDS)
    assert (args, kwargs) == ([RECORD], {})
    assert isinstance(args[0].jobs[0], Job)
    assert isinstance(args[0].owner, Owner)
    assert status(*args) == "a/b"

    args, kwargs = rebuilt(status, result=FIELDS, verbose=True)
    assert (args, kwargs) == ([], {"result": RECORD, "verbose": True})
    assert rebuilt(status, RECORD)[0][0] is RECORD


def test_record_fields_may_omit_defaults_and_carry_computed_fields():
    assert rebuilt(status, {"http_code": 201, "job_path": "a/b"})[0] == [
        TriggerResult(201, "a/b")
    ]
    args, _ = rebuilt(count, {"count": 3, "total": 99})
    assert isinstance(args[0], Summary)
    assert count(*args) == 6


def test_record_constructor_errors_propagate():
    with pytest.raises(ValueError, match="must not be negative"):
        rebuilt(count, {"count": -1})


@pytest.mark.parametrize(
    "value",
    [
        {"http_code": 201},
        {**FIELDS, "extra": 1},
        "TriggerResult(http_code=201, job_path='a/b')",
        None,
        7,
    ],
)
def test_values_that_are_not_the_record_stay_as_they_arrived(value):
    assert rebuilt(status, value)[0][0] is value


def test_unions_take_the_first_fitting_member_in_declared_order():
    def pick(item: Build | Job | None) -> object:
        return item

    def keep(item: dict[str, int] | Job) -> object:
        return item

    def optional(item: Optional[Job] = None) -> object:  # noqa: UP045
        return item

    assert rebuilt(pick, {"number": 7, "state": "ok"})[0] == [Build(7, "ok")]
    assert rebuilt(pick, {"number": 7})[0] == [Job(7)]
    assert rebuilt(pick, None)[0] == [None]
    assert rebuilt(keep, {"number": 7})[0] == [{"number": 7}]
    assert rebuilt(optional, {"number": 7})[0] == [Job(7)]


def test_records_inside_containers_are_rebuilt():
    def containers(
        listed: list[Job],
        variadic: tuple[Job, ...],
        fixed: tuple[Job, Owner],
        keyed: dict[str, Job],
        unique: frozenset[Job],
        described: Annotated[Job, "The job to read"],
        numbers: tuple[int, ...],
    ) -> None:
        pass

    numbers = [1, 2]
    args, _ = rebuilt(
        containers,
        [{"number": 1}],
        [{"number": 2}],
        [{"number": 3}, {"name": "ci"}],
        {"a": {"number": 4}},
        [{"number": 5}],
        {"number": 6},
        numbers,
    )
    assert args == [
        [Job(1)],
        (Job(2),),
        (Job(3), Owner("ci")),
        {"a": Job(4)},
        frozenset({Job(5)}),
        Job(6),
        numbers,
    ]
    assert args[-1] is numbers


def test_variadic_parameters_rebuild_each_record_and_keep_the_call_shape():
    def many(first: Job, *rest: Job, **named: Owner) -> None:
        pass

    args, kwargs = rebuilt(many, {"number": 1}, {"number": 2}, lead={"name": "ci"})
    assert (args, kwargs) == ([Job(1), Job(2)], {"lead": Owner("ci")})


def test_wrapped_and_bound_callables_resolve_the_original_annotations():
    def logged(fn):
        @functools.wraps(fn)
        def call(*args, **kwargs):
            return fn(*args, **kwargs)

        return call

    class Service:
        @logged
        def status(self, result: TriggerResult) -> str:
            return result.job_path

    service = Service()
    # An activity wraps a bound method the same way `logged` wraps it here.
    for fn in (service.status, logged(service.status)):
        args, _ = rebuilt(fn, FIELDS)
        assert args == [RECORD]
        assert fn(*args) == "a/b"


def test_unbindable_calls_and_unresolved_annotations_stay_as_they_arrived():
    def unknown(result: Missing) -> None:  # noqa: F821
        pass

    args = [FIELDS, FIELDS]
    assert vis._call_arguments(status, args, {}) == (args, {})
    assert rebuilt(unknown, FIELDS)[0][0] is FIELDS


RELOADED = """
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class Job:
    number: int


@dataclass(frozen=True)
class TriggerResult:
    http_code: int
    jobs: list[Job]


def status(result: TriggerResult) -> str:
    if not isinstance(result, TriggerResult):
        raise TypeError("result must be a TriggerResult")
    return f"{result.http_code} {result.jobs[0].number}"
"""


def test_reloaded_extension_rebuilds_its_current_classes(monkeypatch):
    # A reload runs the file into a fresh namespace; a stale module left under
    # the same name must not supply the classes of the previous load.
    first = types.ModuleType("vis_record_reload")
    monkeypatch.setitem(sys.modules, "vis_record_reload", first)
    exec(RELOADED, first.__dict__)
    second = {"__name__": "vis_record_reload"}
    exec(RELOADED, second)
    assert second["TriggerResult"] is not first.TriggerResult

    args, _ = vis._call_arguments(
        second["status"], [{"http_code": 201, "jobs": [{"number": 7}]}], {}
    )
    assert type(args[0]) is second["TriggerResult"]
    assert type(args[0].jobs[0]) is second["Job"]
    assert second["status"](*args) == "201 7"
