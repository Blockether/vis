"""The declaration against itself.

`contract.json` is generated, so these tests do not re-state it — they check that
it is INTERNALLY consistent and that the hand-written parts of this package (the
`Host` protocol, `check_host`) still describe the document that ships beside them.
The engine's own suite (`python_package_test`) is what pins the document to the
repository it was rendered from.
"""

import inspect
import json
from pathlib import Path

import pytest
import vis_contract


def protocol_methods():
    """The declared calls on `Host`, by name."""
    return {
        name: member
        for name, member in vars(vis_contract.Host).items()
        if inspect.isfunction(member) and not name.startswith("_")
    }


def complete_host():
    """A host answering every op the document declares, and nothing else."""

    class Host:
        pass

    for name in vis_contract.OPS:
        setattr(Host, name, lambda self, *args, **kwargs: None)
    return Host()


def test_the_document_is_the_file_shipped_beside_the_module():
    on_disk = json.loads(
        (Path(vis_contract.__file__).with_name("contract.json")).read_text(
            encoding="utf-8"
        )
    )
    assert on_disk == vis_contract.CONTRACT
    assert vis_contract.VERSION == on_disk["version"]


def test_every_op_is_completely_declared():
    assert vis_contract.OPS
    for name, op in vis_contract.OPS.items():
        assert op["name"] == name
        assert op["global"] == f"__vis_host_{name}__"
        assert 1 <= op["arity"] <= 3
        assert op["summary"].strip()
        assert op["outside"] in {"local", "prompt", "refuse"}


def test_a_refusal_is_written_exactly_where_an_op_refuses():
    for name, op in vis_contract.OPS.items():
        refuses = op["outside"] == "refuse"
        assert refuses == ("refusal" in op)
        assert refuses == (vis_contract.refusal(name) is not None)
        if refuses:
            # The traceback an author reads names the call they made.
            assert f"vis.{name}" in op["refusal"]


def test_the_protocol_declares_exactly_the_documents_ops():
    assert set(protocol_methods()) == set(vis_contract.OPS)


def test_the_protocol_takes_the_arguments_the_document_counts():
    for name, member in protocol_methods().items():
        positional = [
            parameter
            for parameter in inspect.signature(member).parameters.values()
            if parameter.kind
            in (parameter.POSITIONAL_ONLY, parameter.POSITIONAL_OR_KEYWORD)
        ]
        # `self` is not an argument the engine passes.
        assert len(positional) - 1 == vis_contract.OPS[name]["arity"], name


def test_every_op_carries_its_summary_into_the_protocol():
    for name, member in protocol_methods().items():
        assert (member.__doc__ or "").strip(), name


def test_the_shell_grammar_is_a_grammar():
    shell = vis_contract.SHELL
    assert shell["default_op"] in shell["spawn_ops"]
    assert not set(shell["spawn_ops"]) & set(shell["handle_ops"])
    assert "shell" in vis_contract.OPS


def test_the_human_input_vocabulary_is_whole():
    human = vis_contract.HUMAN_INPUT
    for key in (
        "field_types",
        "text_types",
        "choice_types",
        "secret_types",
        "decor_types",
        "group_directions",
    ):
        assert human[key], key
    assert human["group_type"]
    assert human["secret_handle_prefix"].endswith(":")
    assert human["otp"]["length"] <= human["otp"]["ceiling"]
    assert human["range"]["min"] < human["range"]["max"]
    # Every text, choice and secret type is a field type — one closed vocabulary.
    for key in ("text_types", "choice_types", "secret_types"):
        assert set(human[key]) <= set(human["field_types"]), key


def test_check_host_answers_a_complete_host():
    host = complete_host()
    assert vis_contract.check_host(host) is host
    assert isinstance(host, vis_contract.Host)


def test_check_host_names_the_ops_a_host_does_not_answer():
    class Partial:
        def state_get(self, key):
            return None

    with pytest.raises(TypeError) as raised:
        vis_contract.check_host(Partial())
    message = str(raised.value)
    assert "shell" in message
    assert "state_get" not in message
    assert str(vis_contract.VERSION) in message


def test_op_answers_one_entry_or_nothing():
    assert vis_contract.op("shell")["name"] == "shell"
    assert vis_contract.op("detonate") is None
    assert vis_contract.refusal("shell") is None
