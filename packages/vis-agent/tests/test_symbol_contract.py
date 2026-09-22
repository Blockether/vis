"""Issue #176: one inert description follows declarations across the host boundary."""

from __future__ import annotations

import ast
import json
from dataclasses import dataclass, field
from typing import Annotated, Any, Literal

import blockether.vis.extension as vis
import pytest


@dataclass(frozen=True)
class Reading:
    """A measured duration; missing elapsed_s means no measurement."""

    elapsed_s: Annotated[float | None, "Seconds, or None when not measured."]
    labels: list[str] = field(default_factory=lambda: pytest.fail("factory ran"))


class Opaque:
    """Not a dataclass: crosses the boundary as an opaque name."""


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


def test_signature_text_is_static_source_with_annotations():
    # #273: the signature reads as `inspect.signature` prints it — parameters,
    # annotations and return, literal defaults — so a sandbox can stamp it onto
    # the bound tool and `inspect`/`typing` there answer the declared types.
    contract = vis.Symbol(measure, name="probe").contract
    assert contract["signature"] == (
        "(target: str, /, *labels: str, timeout_s: float | None = None,"
        " **options: bool) -> 'Reading'"
    )

    def shapes(
        flag: Literal["a", 1, True, None],
        pair: tuple[int, ...],
        table: dict[str, list[Reading]],
        raw: Opaque,
        anything: Any,
        plain,
        count: int = 3,
    ) -> Reading | None:
        """Cover every portable type kind."""
        pytest.fail("tool ran")

    signature = vis.Symbol(shapes).contract["signature"]
    assert signature == (
        "(flag: Literal['a', 1, True, None], pair: tuple[int, ...],"
        " table: dict[str, list['Reading']], raw: 'Opaque', anything: Any,"
        " plain, count: int = 3) -> 'Reading' | None"
    )
    # Only builtins and typing forms appear unquoted: every other name is a
    # forward reference the sandbox never evaluates.
    tree = ast.parse(f"def shapes{signature}: pass")
    names = {node.id for node in ast.walk(tree) if isinstance(node, ast.Name)}
    assert names == {"Literal", "tuple", "int", "dict", "str", "list", "Any"}
    assert "Reading" in ast.unparse(tree.body[0].returns)


def test_opaque_defaults_and_annotation_expressions_are_not_evaluated():
    class Secret:
        def __repr__(self):
            pytest.fail("default repr ran")

    def raise_if_evaluated():
        pytest.fail("annotation ran")

    def lookup(token=Secret(), *, key="not-for-discovery") -> raise_if_evaluated():
        """Look up a result without inspecting opaque defaults."""
        pytest.fail("tool ran")

    contract = vis.Symbol(lookup).contract
    encoded = json.dumps(contract)
    assert "not-for-discovery" in encoded
    assert contract["returns"]["kind"] == "unresolved"
    assert contract["parameters"][0]["default_is_none"] is False
    assert contract["parameters"][0]["default_source"] is None
    assert contract["parameters"][1]["default_source"] == "'not-for-discovery'"
    assert contract["signature"] == (
        "(token=..., *, key='not-for-discovery') -> 'raise_if_evaluated()'"
    )


def test_literal_defaults_are_exposed_without_opt_in():
    # #281: signatures, portable metadata and generated help share real defaults.
    def query(
        *, dry_run: bool = False, limit: int = 200, since: str = "1h", services=()
    ):
        """Query services without changing them."""
        pytest.fail("tool ran during inspection")

    symbol = vis.Symbol(query)
    contract = symbol.contract
    assert contract["signature"] == (
        "(*, dry_run: bool = False, limit: int = 200, since: str = '1h', services=())"
    )
    expected = ["False", "200", "'1h'", "()"]
    assert [item["default_source"] for item in contract["parameters"]] == expected
    catalog = vis.Catalog([symbol])
    assert [
        item.default_source for item in catalog.spec("query").parameters
    ] == expected
    for source in expected:
        assert f"default {source}" in catalog.help("query").text


@pytest.mark.parametrize(
    "value",
    [
        None,
        False,
        True,
        0,
        200,
        -200,
        2**100,
        -0.0,
        1.25,
        1e300,
        2 + 3j,
        -2 - 3j,
        "1h",
        "",
        "token-example",
        "雪",
        b"\x00\xff",
        ...,
        "quotes: '\"\n\t\\; =Ellipsis; = Ellipsis; ...",
        "'); __import__('builtins').print('not executable') #",
        (),
        (1,),
        (False, ("1h", None)),
        [],
        [1, (2, 3)],
        {},
        {"services": ["web", "api"], (1, 2): (False, None)},
        {1, 2},
        set(),
    ],
)
def test_default_source_round_trips_as_inert_python(value):
    # #281: rendering must preserve types and escaping, not replace inside strings.
    import inspect

    from blockether.vis import _contracts

    def read(argument=value):
        """Read one value."""
        return argument

    symbol = vis.Symbol(read)
    contract = symbol.contract
    parameter = contract["parameters"][0]
    source = parameter["default_source"]
    decoded = ast.literal_eval(source)
    assert type(decoded) is type(value)
    assert decoded == value
    assert _contracts.validate("symbol", "declaration", contract) == contract
    namespace = {}
    exec("def read" + contract["signature"] + ": pass", namespace)
    inspected = inspect.signature(namespace["read"]).parameters["argument"].default
    assert type(inspected) is type(value)
    assert inspected == value
    assert symbol.fn() is value
    assert symbol.fn("override") == "override"


def test_required_none_opaque_and_variadic_defaults_stay_distinct():
    from blockether.vis import _contracts

    def read(required, optional=None, opaque=object(), *args, **kwargs):
        """Read a value with optional context."""

    contract = vis.Symbol(read).contract
    assert [
        (p["required"], p["has_default"], p["default_is_none"], p["default_source"])
        for p in contract["parameters"]
    ] == [
        (True, False, False, None),
        (False, True, True, "None"),
        (False, True, False, None),
        (False, False, False, None),
        (False, False, False, None),
    ]
    assert _contracts.validate("symbol", "declaration", contract) == contract
    contract["parameters"][0]["default_source"] = "200"
    with pytest.raises(ValueError):
        _contracts.validate("symbol", "declaration", contract)


def test_default_rendering_never_calls_custom_repr_or_factories():
    # #281: exact builtin checks must not invoke subclass or metaclass hooks.
    class HostileMeta(type):
        def __eq__(self, other):
            pytest.fail("metaclass equality ran")

        def __hash__(self):
            pytest.fail("metaclass hash ran")

    class Hostile(metaclass=HostileMeta):
        def __repr__(self):
            pytest.fail("custom repr ran")

    def factory():
        pytest.fail("default factory ran")

    hostile = Hostile()
    defaults = [hostile, factory, (hostile,), [hostile], {"value": hostile}]
    for base in (str, int, float, complex, bytes, tuple, list, dict, set):
        subclass = type("CustomDefault", (base,), {"__repr__": Hostile.__repr__})
        defaults.append(subclass())
    for default in defaults:

        def read(value=default):
            """Read without running user code during inspection."""

        contract = vis.Symbol(read).contract
        assert contract["parameters"][0]["default_source"] is None
        assert contract["signature"] == "(value=...)"


def test_default_inspection_budget_and_nonliteral_values_use_placeholders():
    cyclic = []
    cyclic.append(cyclic)
    deep = None
    for _ in range(9):
        deep = (deep,)
    deep_empty = ()
    for _ in range(8):
        deep_empty = (deep_empty,)
    for default in (
        cyclic,
        deep,
        deep_empty,
        "x" * 4095,
        ("x" * 2048,) * 2,
        [0] * 4096,
        2**20000,
        float("inf"),
        float("-inf"),
        float("nan"),
        complex(1, float("inf")),
    ):

        def read(value=default):
            """Read a value with a bounded inspection representation."""

        contract = vis.Symbol(read).contract
        assert contract["parameters"][0]["default_source"] is None
        assert contract["signature"] == "(value=...)"

    def read(value="x" * 4094):
        """Read a value at the inspection budget."""

    assert len(vis.Symbol(read).contract["parameters"][0]["default_source"]) == 4096
    for default in (deep[0], deep_empty[0]):

        def read(value=default):
            """Read a value at the nesting budget."""

        source = vis.Symbol(read).contract["parameters"][0]["default_source"]
        assert ast.literal_eval(source) == default


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
    # #179: inspect follows wrappers too; unwrapping must precede deferred detection.
    from functools import wraps

    @wraps(namespace["deferred"])
    def wrapped(*args, **kwargs):
        return namespace["deferred"](*args, **kwargs)

    wrapped_symbol = vis.Symbol(wrapped)
    assert wrapped_symbol.contract["parameters"][0]["type"]["kind"] == "unresolved"
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


@pytest.mark.parametrize("bound", [False, True])
def test_cross_module_wraps_resolves_original_namespace(bound):
    # #179: wrapper globals differ from the defining module, including bound methods.
    from types import ModuleType

    decorators = ModuleType("contract_decorators")
    exec(
        "from functools import wraps\ndef wrap(fn):\n    @wraps(fn)\n    def wrapped(*args, **kwargs):\n        return fn(*args, **kwargs)\n    return wrapped\n",
        decorators.__dict__,
    )
    models = ModuleType("contract_models")
    models.Reading = Reading
    namespace = {"wrap": decorators.wrap, "models": models, "vis": vis}
    exec(
        'from __future__ import annotations\nclass Tools:\n    @wrap\n    @vis.method(tag="mutation")\n    @wrap\n    def read(self, value: models.Reading) -> tuple[models.Reading, ...]:\n        "Read records."\n        return (value,)\n',
        namespace,
    )
    tool = namespace["Tools"]().read if bound else namespace["Tools"].read
    contract = vis.Symbol(tool).contract
    assert contract["parameters"][-1]["type"]["kind"] == "record"
    result = contract["returns"]
    assert result["kind"] == "generic"
    assert result["variadic"] is True
    assert len(result["arguments"]) == 1
    assert result["arguments"][0]["fields"][0]["name"] == "elapsed_s"
    if bound:
        member = vis.Symbol(namespace["Tools"](), name="tools").contract["members"][0]
        assert member["tag"] == "mutation"
        assert [p["name"] for p in member["parameters"]] == ["value"]
    assert "tuple[Reading, ...]" in vis.Symbol(tool)._spec()["doc"]


@pytest.mark.parametrize("annotation", [tuple[str, ...], "tuple[str, ...]"])
def test_variadic_tuple_contract(annotation):
    from blockether.vis import _contracts

    def values():
        """Read values."""

    values.__annotations__ = {"return": annotation}
    contract = vis.Symbol(values).contract
    assert contract["returns"] == {
        "kind": "generic",
        "name": "tuple",
        "variadic": True,
        "arguments": [{"kind": "scalar", "name": "str"}],
    }
    assert _contracts.validate("symbol", "declaration", contract) == contract
    contract["returns"]["arguments"].append({"kind": "scalar", "name": "int"})
    with pytest.raises(ValueError):
        _contracts.validate("symbol", "declaration", contract)
    values.__annotations__ = {"return": tuple[str, int]}
    fixed = vis.Symbol(values).contract
    assert "variadic" not in fixed["returns"]
    assert len(fixed["returns"]["arguments"]) == 2
    assert _contracts.validate("symbol", "declaration", fixed) == fixed


@pytest.mark.parametrize("bound", [False, True])
def test_wrapper_cycles_are_rejected_without_calling_the_tool(bound):
    # #179: cyclic wrapper metadata must fail rather than hang or invoke a tool.
    class Tools:
        def read(self, value: str) -> str:
            """Read one value."""
            pytest.fail("tool ran during inspection")

    Tools.read.__wrapped__ = Tools.read
    tool = Tools().read if bound else Tools.read
    with pytest.raises(ValueError, match="wrapper loop"):
        vis.Symbol(tool)


@pytest.mark.parametrize(
    "annotation, rendered",
    [
        ("MissingResult", "MissingResult (unresolved)"),
        (object, "object (opaque)"),
        (list[object], "list[object (opaque)]"),
        ("tuple[MissingResult, ...]", "tuple[MissingResult (unresolved), ...]"),
    ],
)
def test_incomplete_types_are_visible_in_docs(annotation, rendered):
    # #179: a type name alone must not imply that its structure was resolved.
    def values(value):
        """Read values."""

    values.__annotations__ = {"value": annotation, "return": annotation}
    document = vis.Symbol(values)._spec()["doc"]
    assert f"- value: {rendered}" in document
    assert f"Returns: {rendered}" in document


@pytest.mark.parametrize(
    "annotation", ["os.PathLike[str]", "PathLike[str]", "str | os.PathLike[str]"]
)
@pytest.mark.parametrize("bound", [False, True])
def test_postponed_pathlike_contract(annotation, bound):
    # #198: resolve stdlib generics statically, including namespace methods.
    namespace = {}
    exec(
        "from __future__ import annotations\nimport os\nfrom os import PathLike\n"
        "import blockether.vis.extension as vis\n"
        f'def describe(path: {annotation}) -> str:\n    """Describe a path."""\n    return str(path)\n'
        "class Tools:\n    @vis.method()\n"
        f'    def describe(self, path: {annotation}) -> str:\n        """Describe a path."""\n        return str(path)\n',
        namespace,
    )
    symbol = vis.Symbol(
        namespace["Tools"]() if bound else namespace["describe"], name="paths"
    )
    contract = symbol.contract["members"][0] if bound else symbol.contract
    actual = contract["parameters"][0]["type"]
    pathlike = {
        "kind": "generic",
        "name": "PathLike",
        "arguments": [{"kind": "scalar", "name": "str"}],
    }
    expected = (
        {
            "kind": "union",
            "name": "union",
            "arguments": [{"kind": "scalar", "name": "str"}, pathlike],
        }
        if annotation.startswith("str |")
        else pathlike
    )
    assert actual == expected


@dataclass(frozen=True)
class DocCommand:
    """One supported command; no command is executed by discovery."""

    verb: str
    retries: Annotated[int, "Retries after the first attempt."]


@dataclass(frozen=True)
class DocTool:
    """A callable's command summary."""

    name: str
    commands: tuple[DocCommand, ...]


@dataclass(frozen=True)
class DocSnapshot:
    entries: tuple[DocTool, ...]


@dataclass(frozen=True)
class DocWait:
    snapshot: DocSnapshot
    failures: tuple[DocTool, ...]


@dataclass(frozen=True)
class DocLeft:
    right: DocRight | None


@dataclass(frozen=True)
class DocRight:
    left: DocLeft | None


def schema_symbol(annotation):
    """Inspect a typed result without executing its function."""

    def values(*, limit: int | None = None):
        """Read local results; never retries or changes state."""
        pytest.fail("discovery executed the tool")

    values.__annotations__["return"] = annotation
    return vis.Symbol(values)


def test_doc_deduplicates_scalar_or_sequence_union_without_changing_the_contract():
    # #234: the same element schema formerly appeared once per union branch.
    symbol = schema_symbol(DocTool | tuple[DocTool, ...])
    before = symbol.contract
    document = symbol._spec()["doc"]
    assert "Returns: DocTool | tuple[DocTool, ...]" in document
    assert "Sandbox sequences are list-like, not Python tuples" in document
    assert "Sandbox sequences" not in schema_symbol(int)._spec()["doc"]
    assert document.count("- name: str") == 1
    assert document.count("- verb: str") == 1
    assert "One supported command" in document
    assert symbol.contract == before
    assert before["returns"]["arguments"][1]["arguments"][0]["fields"]


def test_doc_shares_model_definitions_between_result_paths_and_parameters():
    # #234: snapshot entries and failures must reference the same model definition.
    symbol = schema_symbol(DocWait)
    spec = symbol._spec()
    document = spec["doc"]
    assert "- snapshot: DocSnapshot" in document
    assert "- failures: tuple[DocTool, ...]" in document
    assert document.count("- name: str") == 1
    assert document.count("- verb: str") == 1
    assert "Retries after the first attempt." in document
    contract = spec["contract"]
    contract["parameters"][0]["type"] = contract["returns"]
    document = vis._contract_doc(contract)
    assert document.count("- name: str") == 1
    assert "Effect: observation" in document
    assert "keyword_only; default None" in document
    assert "Read local results; never retries or changes state." in document


def test_doc_recursive_models_are_defined_once_across_union_branches():
    # #234: a reference and a fuller occurrence describe the same recursive model.
    document = schema_symbol(DocLeft | DocRight)._spec()["doc"]
    assert document.count("- right: DocRight | None") == 1
    assert document.count("- left: DocLeft | None") == 1


def test_doc_bounds_deep_schema_but_retains_full_nested_contract():
    # #234: the budget limits generated fields, not the semantic call envelope.
    from dataclasses import make_dataclass

    model = make_dataclass(
        "Detail", [("units", Annotated[int, "Microseconds at source."])]
    )
    for level in range(40):
        model = make_dataclass(
            f"Layer{level}", [("child", model), ("sample_count", int), ("source", str)]
        )
    symbol = schema_symbol(model)
    contract = symbol.contract
    document = symbol._spec()["doc"]
    assert "Microseconds at source." not in document
    assert "values.contract" in document
    assert "2048" in document
    assert len(document.split("Model schemas:\n", 1)[1]) <= 2048
    assert document == symbol._spec()["doc"]
    assert contract == symbol.contract
    deepest = contract["returns"]
    for _ in range(40):
        deepest = deepest["fields"][0]["type"]
    assert deepest["fields"][0]["type"]["description"] == "Microseconds at source."
    catalog = vis.Catalog([symbol])
    assert document in catalog.help("values").text
    deepest_spec = catalog.spec("values").returns
    for _ in range(40):
        deepest_spec = deepest_spec.fields[0].type
    assert deepest_spec.fields[0].type.description == "Microseconds at source."


def test_doc_preserves_usage_notes_and_distinguishes_same_named_models():
    # #234: names alone are not enough to identify a portable model shape.
    from dataclasses import make_dataclass

    left = make_dataclass("Item", [("count", int)])
    right = make_dataclass("Item", [("label", str)])
    pair = make_dataclass(
        "Pair",
        [
            ("left", Annotated[left, "Current count."]),
            ("again", Annotated[left, "Previous count."]),
            ("right", right),
        ],
    )
    document = schema_symbol(pair)._spec()["doc"]
    assert document.count("- count: int") == 1
    assert document.count("- label: str") == 1
    assert "Current count." in document and "Previous count." in document
    assert "- right: Item [2]" in document


def test_doc_schema_budget_does_not_truncate_the_callable_semantics():
    # #234: schema abbreviation cannot conceal preconditions or split field lines.
    from dataclasses import make_dataclass

    model = make_dataclass(
        "Large", [("value", Annotated[str, "Large field note. " * 300])]
    )
    contract = schema_symbol(model).contract
    contract["description"] = "Required safety condition. " * 150
    document = vis._contract_doc(contract)
    assert document.startswith(contract["description"])
    assert "Effect: observation" in document
    assert "Returns: Large" in document
    assert "- limit: int | None (keyword_only; default None)" in document
    assert "Large field note." not in document
    assert len(document.split("Model schemas:\n", 1)[1]) <= 2048
    assert "values.contract" in document


def test_abbreviation_points_to_the_full_nested_callable_attribute():
    # #234: rendering before namespace qualification produced an unusable pointer.
    from dataclasses import make_dataclass

    model = make_dataclass("Large", [("value", Annotated[str, "Detail. " * 300])])

    class Inner:
        values = staticmethod(schema_symbol(model).fn)

    class Outer:
        tools = Inner()

    symbol = vis.Symbol(Outer(), name="nested")
    method = symbol._spec()["methods"][0]
    assert method["contract"]["name"] == "nested.tools.values"
    assert "nested.tools.values.contract (dictionary, not a doc topic)" in method["doc"]
    assert "Traverse/filter ['parameters'] or ['returns'] in Python" in method["doc"]
    assert "print only matching leaf fields, never whole branches" in method["doc"]
    assert method["doc"] in vis.Catalog([symbol]).help("nested.tools.values").text


def test_schema_budget_includes_an_abbreviation_with_a_long_callable_name():
    # #234: even a valid unusually long identifier must not overflow the notice.
    from dataclasses import make_dataclass

    model = make_dataclass("Large", [("value", Annotated[str, "Detail. " * 300])])
    contract = schema_symbol(model).contract
    contract["name"] = "v" * 2048
    document = vis._contract_doc(contract)
    assert len("Model schemas:" + document.split("Model schemas:", 1)[1]) <= 2048
    assert ".contract" in document
