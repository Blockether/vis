"""Issue #256: explicit sequence declarations across the real extension boundary."""

from __future__ import annotations

from dataclasses import dataclass

import blockether.vis.extension as vis


@dataclass(frozen=True, slots=True)
class Page:
    title: str


@vis.sequence(field="results")
@dataclass(frozen=True, slots=True)
class PageList:
    results: tuple[Page, ...]
    total: int
    _private: str = "not exported"

    def __iter__(self):
        raise AssertionError("Original iterator executed")

    def __len__(self):
        raise AssertionError("Original length executed")

    def remote_only(self):
        raise AssertionError("Original method executed")


@dataclass(frozen=True, slots=True)
class Plain:
    results: tuple[Page, ...]

    def __iter__(self):
        return iter(self.results)


@dataclass(frozen=True, slots=True)
class UndeclaredPages(PageList):
    pass


@dataclass(frozen=True, slots=True)
class Report:
    pages: PageList
    plain: Plain
    undeclared: UndeclaredPages


def sequence_probe_search(empty: bool = False, as_list: bool = False) -> PageList:
    """Return received pages, with a total that includes unreceived pages."""
    items = () if empty else (Page("First"), Page("Second"))
    return PageList(list(items) if as_list else items, 20)


def sequence_probe_nested() -> Report:
    """Return nested collections alongside ordinary records."""
    return Report(sequence_probe_search(), Plain(()), UndeclaredPages((), 0))


def sequence_probe_invalid(kind: str) -> PageList:
    """Refuse lazy or unsupported backing values without iterating them."""

    def lazy():
        raise AssertionError("Lazy iterator consumed")
        yield

    class TrapList(list):
        def __iter__(self):
            raise AssertionError("List subclass iterator consumed")

    values = {
        "none": None,
        "mapping": {},
        "text": "abc",
        "set": {1},
        "lazy": lazy(),
        "subclass": TrapList(),
    }
    return PageList(values[kind], 0)


vis.register_extension(
    vis.Extension(
        name="sequence-probe",
        alias="sequence_probe",
        description="Field-backed sequence boundary fixture",
        symbols=[
            vis.Symbol(fn, activity=vis.Activity(label=label, show_start=False))
            for fn, label in (
                (sequence_probe_search, "Search fixture pages"),
                (sequence_probe_nested, "Read nested fixture"),
                (sequence_probe_invalid, "Validate fixture sequence"),
            )
        ],
    )
)
