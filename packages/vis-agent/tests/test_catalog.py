"""Issue #203: typed discovery is derived, inert, immutable and registration-aligned."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, dataclass, field
from functools import wraps
from typing import Annotated, Literal

import blockether.vis.extension as vis
import pytest


@dataclass(frozen=True, slots=True)
class Reading:
    """An observed value, not a success-shaped error."""

    value: Annotated[int, "Number of completed items."]
    labels: tuple[str, ...] = field(default_factory=lambda: pytest.fail("factory ran"))


class Items:
    @vis.method(tag="mutation")
    def write(self, value: int, /, *, note: str | None = None) -> Reading:
        """Write one value. Requires a nonnegative count; raises ValueError otherwise."""
        return Reading(value, ())

    def read(
        self, *labels: str, mode: Literal["short", "full"] = "short", **flags: bool
    ) -> Reading:
        """Read the current count. Does not change stored state."""
        pytest.fail("read executed during discovery")

    @vis.method(is_hidden=True)
    def internal(self) -> None:
        """Not part of public discovery."""
        pytest.fail("hidden tool ran")


class Store:
    def __init__(self):
        self.items = Items()


@pytest.fixture
def symbol():
    return vis.Symbol(Store(), name="store")


def test_catalog_preserves_contract_shape_and_nested_public_names(symbol):
    catalog = vis.Catalog([symbol])
    (namespace,) = catalog.spec()
    assert isinstance(namespace, vis.NamespaceSpec)
    assert namespace.name == "store"
    assert [tool.name for tool in namespace.members] == [
        "store.items.read",
        "store.items.write",
    ]
    assert catalog.spec("store.items").members == namespace.members
    write = catalog.spec("store.items.write")
    assert isinstance(write, vis.ToolSpec)
    raw = next(
        item for item in symbol.contract["members"] if item["name"] == write.name
    )
    assert (write.version, write.signature, write.tag) == (
        raw["version"],
        raw["signature"],
        "mutation",
    )
    count, note = write.parameters
    assert (count.kind, count.required, count.has_default) == (
        "positional_only",
        True,
        False,
    )
    assert (note.kind, note.required, note.has_default, note.default_is_none) == (
        "keyword_only",
        False,
        True,
        True,
    )
    assert note.type.kind == "union"
    assert write.returns.kind == "record"
    assert write.returns.fields[0].type.description == "Number of completed items."
    assert write.returns.fields[1].has_default is True
    assert write.returns.fields[1].type.variadic is True
    assert "Literal['short', 'full']" in catalog.help("store.items.read").text
    read = catalog.spec("store.items.read")
    assert [parameter.kind for parameter in read.parameters] == [
        "var_positional",
        "keyword_only",
        "var_keyword",
    ]
    assert read.parameters[1].type.values == ("short", "full")
    assert read.parameters[1].default_is_none is False


def test_catalog_values_are_deeply_immutable_snapshots(symbol):
    catalog = vis.Catalog([symbol])
    tool = catalog.spec("store.items.write")
    with pytest.raises(FrozenInstanceError):
        tool.name = "changed"
    with pytest.raises(FrozenInstanceError):
        tool.parameters[0].type.name = "changed"
    with pytest.raises(FrozenInstanceError):
        tool.returns.fields[0].required = False
    symbol.contract["members"].clear()
    assert len(catalog.spec("store").members) == 2


def test_generated_help_shares_registered_doc_and_omission_semantics(symbol):
    catalog = vis.Catalog([symbol])
    help_doc = catalog.help("store.items.write")
    assert isinstance(help_doc, vis.HelpDocument)
    assert help_doc.tool == "store.items.write"
    registered = next(
        item for item in symbol._spec()["methods"] if item["name"] == "items.write"
    )
    assert registered["doc"] in help_doc.text
    assert "positional_only; required" in help_doc.text
    assert "keyword_only; default None" in help_doc.text
    assert "Effect: mutation" in help_doc.text
    assert "Number of completed items" in help_doc.text
    assert "store.items.read" in catalog.help("store").text
    assert "store.items.internal" not in catalog.help("store").text


@pytest.mark.parametrize("name", ["missing", "store.items.internal", "", "write"])
def test_unknown_or_hidden_names_fail_explicitly(symbol, name):
    catalog = vis.Catalog([symbol])
    for lookup in (catalog.spec, catalog.help):
        with pytest.raises(ValueError, match="Unknown catalog name"):
            lookup(name)


@pytest.mark.parametrize("value", [42, {}, []])
def test_wrong_lookup_types_fail_before_any_operation(symbol, value):
    for lookup in (vis.Catalog([symbol]).spec, vis.Catalog([symbol]).help):
        with pytest.raises(TypeError):
            lookup(value)
    with pytest.raises(TypeError):
        vis.Catalog([symbol]).help(None)


def test_empty_hidden_and_duplicate_declarations(symbol):
    assert vis.Catalog([]).spec() == ()
    assert vis.Catalog([vis.Symbol(Items().internal, is_hidden=True)]).spec() == ()
    with pytest.raises(ValueError, match="Duplicate"):
        vis.Catalog([symbol, symbol])
    for invalid in ("store", [object()], symbol):
        with pytest.raises(TypeError):
            vis.Catalog(invalid)


def test_defaults_and_annotations_are_never_evaluated_or_exposed():
    class Secret:
        def __repr__(self):
            pytest.fail("default repr ran")

    def evaluate():
        pytest.fail("annotation evaluated")

    def read(token=Secret(), *, other="private-default") -> evaluate():
        """Read without disclosing defaults."""
        pytest.fail("operation ran")

    catalog = vis.Catalog([vis.Symbol(read)])
    assert catalog.spec("read").returns.kind == "unresolved"
    assert "private-default" not in repr(catalog.spec()) + catalog.help("read").text
    assert "default omitted" in catalog.help("read").text
    with pytest.raises(AssertionError, match="unresolved"):
        vis.testing.assert_catalog(catalog, names=["read"])


def test_contract_verification_checks_public_names_tags_types_and_help(symbol):
    catalog = vis.Catalog([symbol])
    names = ["store.items.read", "store.items.write"]
    vis.testing.assert_catalog(catalog, names=names, mutations=["store.items.write"])
    with pytest.raises(AssertionError, match="names"):
        vis.testing.assert_catalog(
            catalog, names=names[:1], mutations=["store.items.write"]
        )
    with pytest.raises(AssertionError, match="mutation"):
        vis.testing.assert_catalog(catalog, names=names)


def test_wrapped_methods_keep_annotations_and_actual_registered_invocation(monkeypatch):
    @wraps(Items.write)
    def wrapped(*args, **kwargs):
        return Items.write(*args, **kwargs)

    class Wrapped:
        write = wrapped

    symbol = vis.Symbol(Wrapped(), name="wrapped")
    catalog = vis.Catalog([symbol])
    assert catalog.spec("wrapped.write").returns.fields[0].name == "value"
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    vis.register(
        vis.Extension(
            name="catalog-test",
            description="Catalog test.",
            alias="wrapped",
            symbols=[symbol],
        )
    )
    registered = vis._registration["spec"]["symbols"][0]["methods"][0]
    assert registered["fn"](4, note=None) == Reading(4, ())
    with pytest.raises(TypeError):
        registered["fn"](4, "not keyword-only")


def test_catalog_methods_have_explicit_end_only_activity(symbol):
    catalog = vis.Catalog([symbol])
    for method, value in (
        (catalog.spec, catalog.spec()),
        (catalog.help, catalog.help("store")),
    ):
        activity = method.__vis_symbol_activity__
        assert activity.show_start is False
        for phase in ("start", "success", "failure"):
            presentation = activity.render(
                phase=phase, result=value, error=RuntimeError("lookup failed")
            )
            assert isinstance(presentation, vis.ActivityPresentation)
            if phase == "failure":
                assert "lookup failed" in str(presentation)
        assert "0" in activity.render(phase="success", result=()).summary
