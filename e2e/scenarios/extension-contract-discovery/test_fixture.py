"""Check issue #232's real-model fixture without making provider calls."""

import json
import runpy
from dataclasses import asdict
from pathlib import Path

import blockether.vis.extension as vis
import pytest


@pytest.fixture
def probe(tmp_path, monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    entry = tmp_path / ".vis/extensions/contract_probe.py"
    entry.parent.mkdir(parents=True)
    source = Path(__file__).parent / "files/.vis/extensions/contract_probe.py"
    entry.write_text(source.read_text())
    module = runpy.run_path(str(entry))
    methods = {
        method["name"]: method
        for method in vis._registration["spec"]["symbols"][0]["methods"]
    }
    return module, methods, tmp_path / "contract-receipts.jsonl"


def test_short_descriptions_preserve_registered_shape(probe):
    _, methods, ledger = probe
    record = methods["record"]
    contract = record["contract"]
    assert contract["signature"] == "person, /, *, bucket, copies=..., note=None"
    assert contract["tag"] == "mutation"
    assert "copies" not in contract["description"]
    assert "bucket: str" in record["doc"]
    assert "keyword_only; required" in record["doc"]
    assert "keyword_only; default omitted" in record["doc"]
    assert "Returns: Receipt" in record["doc"]
    assert contract["returns"]["kind"] == "record"
    assert methods["status"]["contract"]["signature"] == ""
    assert not ledger.exists()
    for method in methods.values():
        assert method["activity"]["show_start"] is False
        assert method["activity"]["label"][0].isupper()


@pytest.mark.parametrize(
    "scenario_id", ["extension-contract-discovery", "extension-known-contract"]
)
def test_registered_calls_create_the_exact_e2e_receipts(probe, scenario_id):
    _, methods, ledger = probe
    assert methods["status"]["fn"]().total == 0
    first = methods["record"]["fn"]("Ada", bucket="inbox")
    second = methods["record"]["fn"]("Lin", bucket="archive", copies=3, note="urgent")
    assert (first.copies, first.note, first.total) == (2, None, 2)
    assert (second.copies, second.note, second.total) == (3, "urgent", 5)
    final = methods["status"]["fn"]()
    assert final.records == 2
    scenario_dir = Path(__file__).parent.parent / scenario_id
    scenario = json.loads((scenario_dir / "scenario.json").read_text())
    assert {
        "receipts": [asdict(first), asdict(second)],
        "final_totals": asdict(final),
    } == scenario["want_answer_json"]
    for name, expected in scenario["want_json_files"].items():
        assert [
            json.loads(line) for line in (ledger.parent / name).read_text().splitlines()
        ] == expected
    assert scenario["want_activity_sequence"] == [
        "contract_probe.status",
        "contract_probe.record",
        "contract_probe.record",
        "contract_probe.status",
    ]
    # The search already gives the semantics; #232 does not require redundant doc().
    if scenario_id == "extension-known-contract":
        assert scenario["files_from"] == "extension-contract-discovery"
        assert not (scenario_dir / "files").exists()
        assert scenario["discovery"] == {
            "known": True,
            "signatures": [],
            "contracts": [],
        }
        assert (
            "contract_probe.record(person, /, *, bucket, copies=2, note=None)"
            in scenario["prompt"]
        )
    else:
        assert scenario["discovery"]["signatures"] == [
            "contract_probe.status",
            "contract_probe.record",
        ]
        assert scenario["discovery"]["known"] is False
    assert scenario["max_form_output_chars"] == 6000
    assert scenario["max_total_output_chars"] == 10000
    assert scenario["want_requested_route"] is True
    assert scenario["want_cache_metrics"] is True
    with pytest.raises(TypeError):
        methods["record"]["fn"]("Ada", "inbox")
    with pytest.raises(ValueError, match="positive"):
        methods["record"]["fn"]("Ada", bucket="inbox", copies=0)
    assert methods["status"]["fn"]().total == 5


def test_invocation_journal_keeps_order_defaults_and_rejected_attempts(probe):
    _, methods, ledger = probe
    journal = ledger.with_name("contract-calls.jsonl")
    assert not journal.exists()
    methods["status"]["fn"]()
    methods["record"]["fn"]("Ada", bucket="inbox")
    with pytest.raises(ValueError, match="positive"):
        methods["record"]["fn"]("Lin", bucket="archive", copies=0, note="urgent")
    assert [json.loads(line) for line in journal.read_text().splitlines()] == [
        {"operation": "contract_probe.status", "arguments": {}},
        {
            "operation": "contract_probe.record",
            "arguments": {
                "person": "Ada",
                "bucket": "inbox",
                "copies": 2,
                "note": None,
            },
        },
        {
            "operation": "contract_probe.record",
            "arguments": {
                "person": "Lin",
                "bucket": "archive",
                "copies": 0,
                "note": "urgent",
            },
        },
    ]
    assert methods["status"]["fn"]().total == 2
    assert len(ledger.read_text().splitlines()) == 1


def test_activity_presentations_cover_empty_success_running_and_failure(probe):
    module, methods, _ = probe
    empty = methods["status"]["fn"]()
    status = module["status_activity"](phase="success", result=empty)
    assert status.summary == "0 records; 0 copies"
    receipt = methods["record"]["fn"]("Ada", bucket="inbox")
    record = module["record_activity"](phase="success", result=receipt)
    assert record.summary == "2 copies; 2 total"
    assert record.content[0].text == "Ada: inbox"
    for render in (module["record_activity"], module["status_activity"]):
        for phase in ("running", "failure"):
            assert render(phase=phase, result=None) is None
