"""Verify issue #239's tracker fixture and result-access regression without model calls."""

import json
import runpy
from pathlib import Path

import blockether.vis.extension as sdk
import pytest


@pytest.fixture
def probe(tmp_path, monkeypatch):
    monkeypatch.setattr(sdk, "_registration", {"spec": None})
    entry = tmp_path / ".vis/extensions/issue_fixture.py"
    entry.parent.mkdir(parents=True)
    source = Path(__file__).parent / "files/.vis/extensions/issue_fixture.py"
    entry.write_text(source.read_text())
    module = runpy.run_path(str(entry))
    methods = {
        symbol["name"]: {method["name"]: method for method in symbol["methods"]}
        for symbol in sdk._registration["spec"]["symbols"]
    }
    return module, methods, tmp_path / "issue-events.jsonl"


def test_result_access_and_recovery_do_not_repeat_the_mutation(probe):
    _, methods, ledger = probe
    find = methods["vis"]["issue_find"]["fn"]
    create = methods["vis"]["issue_create"]["fn"]
    matches = find("blockether/vis", "duplicate sessions")
    assert matches["matches"] == []
    # #239's first failure: field descriptions do not make mappings into records.
    with pytest.raises(AttributeError):
        _ = matches.matches
    with pytest.raises(KeyError):
        _ = matches["result"]
    created = create("blockether/vis", "Duplicate sessions", "Refresh repeats a row")
    # #239's reporting-session failure: the write succeeded before result access failed.
    with pytest.raises(TypeError):
        _ = created["url"]
    assert created.url == "https://github.com/Blockether/vis/issues/239"
    assert find("blockether/vis", "duplicate sessions")["matches"] == [created.url]
    events = [json.loads(line) for line in ledger.read_text().splitlines()]
    assert [event["op"] for event in events] == ["find", "create", "find"]
    scenario = json.loads((Path(__file__).parent / "scenario.json").read_text())
    for expected in scenario["want"][ledger.name]:
        assert expected in ledger.read_text()
    # The no-error E2E runner also fails any accidental Jira call or bad result access.
    assert scenario["want_tools"] == ["vis.issue_find", "vis.issue_create"]
    create("blockether/vis", "Duplicate sessions", "Repeated mutation")
    assert scenario["wantnot"][ledger.name][0] in ledger.read_text()


def test_tracker_boundaries_and_search_precondition(probe):
    _, methods, ledger = probe
    with pytest.raises(ValueError, match="Search for duplicates"):
        methods["vis"]["issue_create"]["fn"]("blockether/vis", "Title", "Body")
    with pytest.raises(ValueError, match="GitHub repository names"):
        methods["jira"]["issue_find"]["fn"]("BLOCKETHER", "duplicate sessions")
    with pytest.raises(ValueError, match="blockether/vis"):
        methods["vis"]["issue_find"]["fn"]("BLOCKETHER", "duplicate sessions")
    assert not ledger.exists()


def test_registration_and_activity_states(probe):
    module, methods, _ = probe
    search = methods["vis"]["issue_find"]
    create = methods["vis"]["issue_create"]
    assert search["contract"]["returns"]["kind"] == "generic"
    assert search["contract"]["returns"]["name"] == "dict"
    assert create["contract"]["returns"]["kind"] == "record"
    assert create["contract"]["tag"] == "mutation"
    assert "Returns: CreatedIssue" in create["doc"]
    for tracker in methods.values():
        for method in tracker.values():
            assert method["activity"]["show_start"] is False
            assert method["activity"]["label"][0].isupper()
    empty = search["fn"]("blockether/vis", "duplicate sessions")
    assert (
        module["search_activity"](phase="success", result=empty).summary == "0 matches"
    )
    receipt = create["fn"]("blockether/vis", "Title", "Body")
    presentation = module["create_activity"](phase="success", result=receipt)
    assert presentation.summary == "Issue #239"
    assert presentation.content[0].text == receipt.url
    found = search["fn"]("blockether/vis", "duplicate sessions")
    assert (
        module["search_activity"](phase="success", result=found).summary == "1 matches"
    )
    for render in (module["search_activity"], module["create_activity"]):
        for phase in ("running", "failure"):
            assert render(phase=phase, result=None) is None
