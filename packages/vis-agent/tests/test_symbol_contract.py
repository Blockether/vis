"""Issue #176: one inert description follows declarations across the host boundary."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from typing import Annotated

import blockether.vis.extension as vis
import pytest


@dataclass(frozen=True)
class Reading:
    """A measured duration; missing elapsed_s means no measurement."""

    elapsed_s: Annotated[float | None, "Seconds, or None when not measured."]
    labels: list[str] = field(default_factory=lambda: pytest.fail("factory ran"))


def measure(
    target: str,
    /,
    *labels: str,
    timeout_s: Annotated[float | None, "Seconds; None disables the timeout."] = None,
    **options: bool,
) -> Reading:
    """Measure one target. Requires a known target; does not change it."""
    pytest.fail("tool ran during inspection")


def test_contract_covers_python_shape_without_executing_defaults_or_tools():
    contract = vis.Symbol(measure, name="probe").contract
    assert contract["name"] == "probe"
    assert contract["tag"] == "observation"
    assert contract["description"] == measure.__doc__
    parameters = {item["name"]: item for item in contract["parameters"]}
    assert parameters["target"]["kind"] == "positional_only"
    assert parameters["target"]["required"] is True
    assert parameters["target"]["has_default"] is False
    assert parameters["labels"]["kind"] == "var_positional"
    assert parameters["timeout_s"]["kind"] == "keyword_only"
    assert parameters["timeout_s"]["has_default"] is True
    assert parameters["timeout_s"]["default_is_none"] is True
    assert parameters["options"]["kind"] == "var_keyword"
    result = contract["returns"]
    assert result["kind"] == "record"
    assert result["name"] == "Reading"
    fields = {item["name"]: item for item in result["fields"]}
    assert fields["elapsed_s"]["type"]["description"].startswith("Seconds")
    assert fields["elapsed_s"]["type"]["kind"] == "union"
    assert fields["labels"]["has_default"] is True
    assert json.loads(json.dumps(contract)) == contract
    contract["name"] = "changed"
    assert vis.Symbol(measure, name="probe").contract["name"] == "probe"


def test_default_values_and_annotation_expressions_are_not_evaluated_or_exported():
    class Secret:
        def __repr__(self):
            pytest.fail("default repr ran")

    def raise_if_evaluated():
        pytest.fail("annotation ran")

    def lookup(token=Secret(), *, key="not-for-discovery") -> raise_if_evaluated():
        """Look up a result using private defaults."""
        pytest.fail("tool ran")

    contract = vis.Symbol(lookup).contract
    encoded = json.dumps(contract)
    assert "not-for-discovery" not in encoded
    assert contract["returns"]["kind"] == "unresolved"
    assert contract["parameters"][0]["default_is_none"] is False
    assert "token=..." in contract["signature"]


def test_nested_namespace_contract_uses_public_names_and_method_tags():
    class Items:
        @vis.method(tag="mutation")
        def put(self, *, value: str) -> bool:
            """Store a value. Replaces the previous value."""
            pytest.fail("method ran")

    class Tools:
        def __init__(self):
            self.items = Items()

    contract = vis.Symbol(Tools(), name="store").contract
    assert contract["name"] == "store"
    method = contract["members"][0]
    assert method["name"] == "store.items.put"
    assert method["tag"] == "mutation"
    assert [item["name"] for item in method["parameters"]] == ["value"]


def test_contract_matches_the_canonical_schema():
    from blockether.vis import _contracts

    value = vis.Symbol(measure).contract
    assert _contracts.validate("symbol", "declaration", value) == value
    value["parameters"][0]["kind"] = "unknown"
    with pytest.raises(ValueError):
        _contracts.validate("symbol", "declaration", value)


@pytest.mark.skipif(
    __import__("sys").version_info < (3, 14),
    reason="Deferred annotations require Python 3.14",
)
def test_registration_adapters_do_not_evaluate_deferred_annotations():
    calls = []
    namespace = {"evaluate": lambda: calls.append("annotation") or str}
    exec(
        compile(
            'def deferred(value: evaluate()):\n    "Read a value."\n    return value\n',
            "deferred.py",
            "exec",
            dont_inherit=True,
        ),
        namespace,
    )
    symbol = vis.Symbol(namespace["deferred"])
    assert calls == []
    assert symbol.contract["parameters"][0]["type"]["kind"] == "unresolved"
    assert calls == []


def test_future_annotations_and_recursive_records_are_portable():
    namespace = {"__name__": __name__, "Reading": Reading}
    exec(
        'from __future__ import annotations\nfrom typing import Annotated\ndef read(*, delay: Annotated[float | None, "Seconds."] = None) -> list[Reading]:\n    "Read a measurement."\n',
        namespace,
    )
    contract = vis.Symbol(namespace["read"]).contract
    assert contract["parameters"][0]["type"]["description"] == "Seconds."
    assert contract["returns"]["arguments"][0]["kind"] == "record"


def test_quoted_composites_and_recursive_string_aliases_are_bounded():
    # #176: metadata must not recurse forever or evaluate forward references.
    namespace = {"Reading": Reading, "Recursive": "list[Recursive]"}
    exec(
        'from __future__ import annotations\ndef read() -> "list[Reading]":\n    "Read results."\n'
        'def recursive() -> Recursive:\n    "Read recursive results."\n',
        namespace,
    )
    assert (
        vis.Symbol(namespace["read"]).contract["returns"]["arguments"][0]["kind"]
        == "record"
    )
    recursive = vis.Symbol(namespace["recursive"]).contract["returns"]
    assert recursive["arguments"][0]["kind"] == "reference"


def test_result_field_docs_include_records_in_containers():
    def readings() -> list[Reading]:
        """Read results."""

    doc = vis.Symbol(readings)._spec()["doc"]
    assert "elapsed_s" in doc
    assert "Seconds" in doc
