"""Automations that an extension declares and that call a function, no model (#456)."""

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts

EVERY_HOUR = {"kind": "every", "seconds": 3600}


def sync(ctx):
    """Copy the session export to the archive."""
    return None


def test_spec_fills_name_and_doc_and_keeps_the_callback():
    spec = vis.Automation(key="iceberg-sync", run=sync, triggers=[EVERY_HOUR])._spec()
    assert spec == {
        "key": "iceberg-sync",
        "name": "iceberg-sync",
        "doc": "Copy the session export to the archive.",
        "triggers": [EVERY_HOUR],
        "run": sync,
        "session": "temporary",
        "push": True,
        "enabled": True,
    }


def test_spec_keeps_given_fields():
    triggers = [
        {"kind": "cron", "expression": "0 3 * * *", "timezone": "Europe/Warsaw"},
        {"kind": "once", "at": 1893456000000},
    ]
    spec = vis.Automation(
        key="nightly",
        run=lambda ctx: None,
        triggers=triggers,
        name="Nightly export",
        doc="Export at night.",
        session="persistent",
        push=False,
        enabled=False,
    )._spec()
    assert spec["triggers"] == triggers
    assert (spec["name"], spec["doc"], spec["session"]) == (
        "Nightly export",
        "Export at night.",
        "persistent",
    )
    assert spec["push"] is False and spec["enabled"] is False


def test_extension_lists_automations_in_its_spec():
    first = vis.Automation(key="a", run=sync, triggers=[EVERY_HOUR])
    second = vis.Automation(key="b", run=sync, triggers=[EVERY_HOUR])
    extension = vis.Extension(
        name="iceberg", description="Archive.", automations=[first, second]
    )
    assert extension.automations == (first, second)
    spec = extension._spec()["automations"]
    assert [one["key"] for one in spec] == ["a", "b"]
    assert spec[0]["run"] is sync


def test_owner_key_of_the_contract_accepts_the_declared_key():
    automation = vis.Automation(key="iceberg.sync_2", run=sync, triggers=[EVERY_HOUR])
    owner = {"extension": "iceberg", "key": automation.key, "root": ""}
    assert _contracts.problems("automations", "automation_owner", owner) == []


@pytest.mark.parametrize(
    "kwargs, message",
    [
        ({"key": "Iceberg"}, "key must match"),
        ({"key": "-sync"}, "key must match"),
        ({"key": "a" * 65}, "key must match"),
        ({"key": ""}, "key must match"),
        ({"run": "sync"}, "run must be callable"),
        ({"run": lambda ctx: None}, "give doc=<text> or a docstring"),
        ({"doc": "   "}, "give doc=<text> or a docstring"),
        ({"name": " "}, "name must be nonblank"),
        ({"session": "new"}, "session must be one of temporary, persistent"),
        ({"push": "yes"}, "push must be a bool"),
        ({"enabled": 1}, "enabled must be a bool"),
        ({"triggers": []}, "give 1 to 8 triggers"),
        ({"triggers": [EVERY_HOUR] * 9}, "give 1 to 8 triggers"),
        ({"triggers": EVERY_HOUR}, "give 1 to 8 triggers"),
        ({"triggers": [{"kind": "webhook"}]}, "kind must be one of cron, every, once"),
        ({"triggers": ["hourly"]}, "kind must be one of"),
        (
            {"triggers": [{"kind": "every", "seconds": 59}]},
            "every trigger: seconds breaks the minimum rule",
        ),
        ({"triggers": [{"kind": "cron"}]}, "expression is missing"),
    ],
)
def test_automation_refuses_a_broken_field(kwargs, message):
    args = {"key": "sync", "run": sync, "triggers": [EVERY_HOUR], **kwargs}
    with pytest.raises(ValueError, match=message):
        vis.Automation(**args)


def test_extension_refuses_a_repeated_key():
    one = vis.Automation(key="sync", run=sync, triggers=[EVERY_HOUR])
    with pytest.raises(ValueError, match="repeat the key 'sync'"):
        vis.Extension(name="iceberg", description="Archive.", automations=[one, one])


def test_extension_refuses_a_value_that_is_no_automation():
    with pytest.raises(TypeError, match="Automation declarations"):
        vis.Extension(
            name="iceberg", description="Archive.", automations=[{"key": "sync"}]
        )


def test_client_extensions_refuse_automations():
    from blockether.vis.engine._extensions import ClientExtensions

    extension = vis.Extension(
        name="iceberg",
        description="Archive.",
        alias="iceberg",
        symbols=[vis.Symbol(sync)],
        automations=[vis.Automation(key="sync", run=sync, triggers=[EVERY_HOUR])],
    )
    with pytest.raises(ValueError, match="host-only fields: automations"):
        ClientExtensions([extension])
