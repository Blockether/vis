"""TUI key bindings that an extension declares (live apps C3)."""

import blockether.vis.extension as vis
import pytest


def test_command_binding_spec_keeps_only_given_fields():
    binding = vis.Keybinding("t", command="/test --fast", label="Run tests")
    assert binding._spec() == {
        "marker": "keybinding",
        "key": "t",
        "command": "/test --fast",
        "label": "Run tests",
    }


def test_action_binding_moves_a_tui_verb():
    spec = vis.Keybinding("N", action="new-session", group="Session")._spec()
    assert spec == {
        "marker": "keybinding",
        "key": "N",
        "action": "new-session",
        "group": "Session",
    }


def test_run_binding_hands_its_function_to_the_host():
    def deploy(ctx):
        return None

    spec = vis.Keybinding("d", run=deploy, label="Deploy", group="Release")._spec()
    assert spec == {
        "marker": "keybinding",
        "key": "d",
        "label": "Deploy",
        "group": "Release",
        "run": deploy,
    }


@pytest.mark.parametrize(
    ("args", "kwargs", "message"),
    [
        (("tt",), {"action": "new-session"}, "one printable ASCII character"),
        ((" ",), {"action": "new-session"}, "one printable ASCII character"),
        (("t",), {}, "exactly one of command"),
        (("t",), {"command": "/x", "label": "X", "action": "a"}, "exactly one"),
        (("t",), {"command": "test", "label": "X"}, "starts with '/'"),
        (("t",), {"command": "/" + "x" * 200, "label": "X"}, "at most 200"),
        (("t",), {"command": "/test"}, "requires label"),
        (("t",), {"action": "NewSession"}, "kebab case"),
        (("t",), {"command": "/test", "label": "x" * 41}, "at most 40"),
        (("t",), {"action": "new-session", "group": "x" * 25}, "at most 24"),
        (("t",), {"run": print}, "requires label"),
        (("t",), {"run": print, "command": "/x", "label": "X"}, "exactly one"),
        (("t",), {"run": "deploy", "label": "X"}, "callable"),
    ],
)
def test_binding_refuses_a_broken_field(args, kwargs, message):
    with pytest.raises(ValueError, match=message):
        vis.Keybinding(*args, **kwargs)


def test_extension_carries_its_bindings_in_order():
    first = vis.Keybinding("t", command="/test", label="Test")
    second = vis.Keybinding("n", action="new-session")
    extension = vis.Extension(
        name="keys", description="Key bindings.", keybindings=[first, second]
    )
    assert extension.keybindings == (first, second)
    assert [b["key"] for b in extension._spec()["keybindings"]] == ["t", "n"]


def test_extension_refuses_a_value_that_is_no_binding():
    with pytest.raises(TypeError, match="Keybinding declarations"):
        vis.Extension(name="keys", description="Key bindings.", keybindings=["t"])
