"""Issue #256: collection behavior is declared, never inferred from methods."""

import sys
from dataclasses import dataclass, make_dataclass
from pathlib import Path
from types import ModuleType

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


@dataclass(frozen=True, slots=True)
class Page:
    title: str


def test_sequence_declaration_is_inert_and_describes_the_backing_field():
    @dataclass(frozen=True, slots=True)
    class PageList:
        results: tuple[Page, ...]
        total: int

        def __iter__(self):
            raise AssertionError("Original methods must not run during discovery")

    assert vis.sequence(field="results")(PageList) is PageList

    def search() -> PageList:
        """Search one page of results."""
        pytest.fail("Discovery executed a tool")

    search.__annotations__ = {"return": PageList}
    symbol = vis.Symbol(search)
    contract = symbol.contract
    assert contract["returns"]["sequence_field"] == "results"
    assert _contracts.validate("symbol", "declaration", contract) == contract
    assert "Sequence over results" in symbol._spec()["doc"]
    assert vis.Catalog([symbol]).spec("search").returns.sequence_field == "results"
    with pytest.raises(AssertionError, match="Original methods"):
        iter(PageList((), 0))


def test_plain_records_and_subclasses_do_not_opt_in_implicitly():
    @dataclass
    class Plain:
        results: list[Page]

        def __iter__(self):
            return iter(self.results)

    def plain() -> Plain:
        """Return an ordinary record."""

    plain.__annotations__ = {"return": Plain}
    assert "sequence_field" not in vis.Symbol(plain).contract["returns"]
    assert vis.Catalog([vis.Symbol(plain)]).spec("plain").returns.sequence_field is None
    vis.sequence(field="results")(Plain)

    @dataclass
    class Child(Plain):
        pass

    def child() -> Child:
        """Return a subclass without its own opt-in."""

    child.__annotations__ = {"return": Child}
    assert "sequence_field" not in vis.Symbol(child).contract["returns"]


@pytest.mark.parametrize("field", [None, 0, "", "_private", "missing", "not a field"])
def test_sequence_requires_a_public_dataclass_field(field):
    @dataclass
    class Result:
        results: list[str]
        _private: list[str]

    with pytest.raises((TypeError, ValueError), match="field"):
        vis.sequence(field=field)(Result)
    assert "__vis_sequence_field__" not in vars(Result)


def test_sequence_rejects_non_dataclasses_and_instances():
    @dataclass
    class Result:
        results: list[str]

    for value in (object, Result([]), lambda: None):
        with pytest.raises(TypeError, match="dataclass"):
            vis.sequence(field="results")(value)


def test_canonical_schema_refuses_sequence_metadata_on_non_records():
    for value in (
        {"kind": "scalar", "name": "str", "sequence_field": "results"},
        {"kind": "record", "name": "Bad", "fields": [], "sequence_field": "_private"},
        {"kind": "record", "name": "Bad", "fields": [], "sequence_field": ""},
    ):
        with pytest.raises(ValueError):
            _contracts.validate("symbol", "type", value)


def test_generated_docs_distinguish_same_named_records_with_different_behavior():
    plain = make_dataclass("Pages", [("results", list[Page])])
    marked = vis.sequence(field="results")(
        make_dataclass("Pages", [("results", list[Page])])
    )

    def search():
        """Return two different models with the same fields."""

    search.__annotations__ = {"return": tuple[plain, marked]}
    document = vis.Symbol(search)._spec()["doc"]
    assert "Pages:" in document and "Pages [2]:" in document
    assert document.count("Sequence over results") == 1


def test_sequence_guide_example_executes_without_a_host(monkeypatch):
    repository = Path(__file__).resolve().parents[3]
    guide = (repository / "resources/vis-docs/extension-api.md").read_text()
    section = guide.split("### Field-backed sequences", 1)[1]
    example = section.split("```python\n", 1)[1].split("```", 1)[0]
    module = ModuleType("sequence_guide")
    monkeypatch.setitem(sys.modules, module.__name__, module)
    before = vis._registration["spec"]
    exec(compile(example, "sequence guide", "exec", dont_inherit=True), vars(module))
    assert module.PageList.__vis_sequence_field__ == "results"
    assert module.PageList((module.Page("First"),), 20).results[0].title == "First"
    assert vis._registration["spec"] is before
