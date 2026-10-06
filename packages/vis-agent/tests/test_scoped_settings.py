"""Scoped declarations stay portable and never mutate settings during construction."""

from itertools import combinations

import blockether.vis.extension as vis
import pytest
from blockether.vis._contracts import definition
from blockether.vis.engine._extensions import ClientExtensions


def test_every_nonempty_scope_combination():
    scopes = definition("toggle", "scope")["enum"]
    for size in range(1, len(scopes) + 1):
        for allowed in combinations(scopes, size):
            setting = vis.Setting(
                id="test_feature", label="Test feature", default=False, scopes=allowed
            )
            spec = vis.Extension(
                name="Settings test", description="Settings test", settings=[setting]
            )._spec()
            assert spec["settings"][0]["scopes"] == list(allowed)
            assert spec["settings"][0]["default"] is False


@pytest.mark.parametrize("scopes", [[], ["session", "session"], ["unknown"], "session"])
def test_invalid_scopes(scopes):
    with pytest.raises((ValueError, TypeError)):
        vis.Setting(
            id="test_feature", label="Test feature", default=True, scopes=scopes
        )


def test_default_scope_and_outside_value():
    setting = vis.Setting(id="test_feature", label="Test feature", default=False)
    assert setting.scopes == ("global",)
    assert setting.value() is False


def test_enum_and_semantics():
    setting = vis.Setting(
        id="test_mode",
        label="Test mode",
        type="enum",
        default="on",
        choices=["off", "on"],
    )
    assert setting._spec()["choices"] == ["off", "on"]
    with pytest.raises(ValueError):
        vis.Setting(
            id="test_mode",
            label="Test mode",
            type="enum",
            default="missing",
            choices=["off", "on"],
        )
    with pytest.raises(ValueError):
        vis.Setting(id="test_mode", label="Test mode", default="on")


def test_parent_nests_a_setting():
    parent = vis.Setting(id="test_feature", label="Test feature", default=True)
    child = vis.Setting(
        id="test_detail", label="Test detail", default=False, parent="test_feature"
    )
    assert "parent" not in parent._spec()
    assert child._spec()["parent"] == "test_feature"
    with pytest.raises(ValueError):
        vis.Setting(id="test_detail", label="Test detail", default=False, parent=" ")


def test_application_bridge_rejects_host_only_settings_before_connecting():
    declaration = vis.Extension(
        name="Settings test",
        description="Settings test",
        settings=[vis.Setting(id="test_feature", label="Test feature", default=True)],
    )
    with pytest.raises(ValueError, match="host-only fields: settings"):
        ClientExtensions([declaration])
