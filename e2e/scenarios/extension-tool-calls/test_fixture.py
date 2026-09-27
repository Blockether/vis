"""Check the extension tool-call fixture without making provider calls."""

import json
import runpy
from dataclasses import asdict, fields, is_dataclass
from pathlib import Path

import blockether.vis.extension as vis

HERE = Path(__file__).parent


def load(tmp_path, monkeypatch, name):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    entry = tmp_path / ".vis/extensions" / f"{name}.py"
    entry.parent.mkdir(parents=True, exist_ok=True)
    entry.write_text((HERE / "files/.vis/extensions" / f"{name}.py").read_text())
    runpy.run_path(str(entry))
    symbol = vis._registration["spec"]["symbols"][0]
    return {method["name"]: method for method in symbol["methods"]}


def sealed(value):
    """Send a result as the extension boundary does: class name and public fields."""
    if is_dataclass(value):
        attrs = {
            field.name: sealed(getattr(value, field.name)) for field in fields(value)
        }
        return {"__vis_object__": type(value).__name__, "__vis_attrs__": attrs}
    return value


def test_tools_own_their_activity_presentation(tmp_path, monkeypatch):
    for name, method, show_start in (
        ("seat_desk", "hold", False),
        ("front_desk", "book", True),
    ):
        registered = load(tmp_path, monkeypatch, name)[method]
        assert registered["contract"]["tag"] == "mutation"
        assert registered["activity"]["label"][0].isupper()
        assert registered["activity"]["show_start"] is show_start


def test_booking_writes_the_exact_e2e_evidence(tmp_path, monkeypatch):
    seat_desk = load(tmp_path, monkeypatch, "seat_desk")
    front_desk = load(tmp_path, monkeypatch, "front_desk")
    calls = []

    def call_tool(tool, args, kwargs):
        calls.append((tool, args, kwargs))
        return sealed(seat_desk["hold"]["fn"](*args, **kwargs))

    monkeypatch.setattr(vis._host, "call_tool", call_tool)
    first = front_desk["book"]["fn"]("Ada", section="balcony")
    second = front_desk["book"]["fn"]("Lin", section="main")

    scenario = json.loads((HERE / "scenario.json").read_text())
    assert calls == [
        ("seat_desk.hold", ["Ada"], {"section": "balcony"}),
        ("seat_desk.hold", ["Lin"], {"section": "main"}),
    ]
    assert {"bookings": [asdict(first), asdict(second)]} == scenario["want_answer_json"]
    for name, rows in scenario["want_json_files"].items():
        written = [
            json.loads(line) for line in (tmp_path / name).read_text().splitlines()
        ]
        assert written == rows
