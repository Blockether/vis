"""Exercise issue #234's real-model fixture without paid provider calls."""

import json
import runpy
from pathlib import Path

import blockether.vis.extension as vis
import pytest


@pytest.fixture
def probe(tmp_path, monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    entry = tmp_path / ".vis/extensions/schema_probe.py"
    entry.parent.mkdir(parents=True)
    source = Path(__file__).parent / "files/.vis/extensions/schema_probe.py"
    entry.write_text(source.read_text())
    module = runpy.run_path(str(entry))
    methods = {
        method["name"]: method
        for method in vis._registration["spec"]["symbols"][0]["methods"]
    }
    return module, methods


def field(spec, name):
    return next(item["type"] for item in spec["fields"] if item["name"] == name)


def test_full_contract_retains_repeated_records_and_deep_units(probe):
    _, methods = probe
    scalar, sequence = methods["cards"]["contract"]["returns"]["arguments"]
    assert scalar == sequence["arguments"][0]
    result = methods["monitor"]["contract"]["returns"]
    snapshot = field(result, "snapshot")["arguments"][0]
    failures = field(result, "failures")["arguments"][0]
    assert snapshot == failures
    assert field(snapshot, "target") == field(result, "primary")
    sample = field(result, "diagnostics")
    for level in range(40):
        assert sample["name"] == f"SampleLevel{level:02d}"
        sample = field(sample, "sample")
    assert sample["name"] == "Lag"
    assert field(sample, "elapsed")["description"] == (
        "Microseconds since the last successful heartbeat."
    )


def test_compact_discovery_preserves_envelope_and_defers_deep_schema(probe):
    _, methods = probe
    cards = methods["cards"]["doc"]
    monitor = methods["monitor"]["doc"]
    assert "Returns: ToolCard | tuple[ToolCard, ...]" in cards
    assert "key: str | None (positional_or_keyword; default None)" in cards
    assert "Effect: observation" in monitor
    assert "Returns: MonitorResult" in monitor
    # Regression #234: the old flattened monitor document was 21,389 characters.
    assert len(monitor) <= 2200
    assert cards.count("- key: str\n") == 1
    assert "Microseconds since the last successful heartbeat." not in monitor
    assert ".contract" in monitor


def test_registered_calls_supply_the_scenario_answers(probe):
    module, methods = probe
    atlas = methods["cards"]["fn"]("atlas")
    cards = methods["cards"]["fn"]()
    monitor = methods["monitor"]["fn"]()
    assert atlas == cards[0]
    assert isinstance(atlas, module["ToolCard"])
    assert isinstance(monitor, module["MonitorResult"])
    sample = monitor.diagnostics
    for _ in range(40):
        sample = sample.sample
    actual = [
        atlas.name,
        str(len(cards)),
        str(monitor.active_jobs),
        monitor.failures[0].target.address,
        str(sample.elapsed),
        "microseconds",
    ]
    scenario = json.loads((Path(__file__).parent / "scenario.json").read_text())
    assert actual == scenario["want_answer"]
    with pytest.raises(ValueError, match="Unknown tool key"):
        methods["cards"]["fn"]("missing")


def test_activity_registration_and_empty_success_running_failure(probe):
    module, methods = probe
    for method in methods.values():
        assert method["activity"]["show_start"] is False
        assert method["activity"]["label"][0].isupper()
    cards = module["cards_activity"]
    monitor = module["monitor_activity"]
    assert cards(phase="success", result=()).summary == "Tool cards: 0"
    assert cards(phase="success", result=methods["cards"]["fn"]()).summary == (
        "Tool cards: 2"
    )
    assert cards(phase="success", result=methods["cards"]["fn"]("atlas")).summary == (
        "Tool cards: 1"
    )
    assert monitor(phase="success", result=methods["monitor"]["fn"]()).summary == (
        "7 active jobs; failures: 1"
    )
    for render in (cards, monitor):
        for phase in ("running", "failure"):
            assert render(phase=phase, result=None) is None
