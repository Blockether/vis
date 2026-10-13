"""Tests for the gateway operations of blockether.vis.evals."""

import json

import pytest
from blockether.vis.evals import _gateway
from blockether.vis.evals._gateway import OPERATIONS, handle, plain
from test_evals_bench import summary


def roundtrip(operation, body):
    """Send the body and the answer through JSON, as the gateway does."""
    return json.loads(json.dumps(handle(operation, json.loads(json.dumps(body)))))


def test_operations_are_the_gateway_routes():
    assert sorted(OPERATIONS) == [
        "agreement",
        "compare",
        "diagnose",
        "labels",
        "leaderboard",
        "report",
    ]


def test_report_gives_the_score_as_json():
    result = roundtrip("report", {"summary": summary()})["result"]
    assert result["solved_tasks"] == 2
    assert {row["outcome"]: row["count"] for row in result["outcomes"]}["solved"] == 2


def test_diagnose_and_labels_check_the_human_causes():
    labels = {"gamma__0102": {"cause": "agent-timeout"}}
    diagnosis = roundtrip("diagnose", {"summary": summary(), "labels": labels})
    assert diagnosis["result"]["findings"]
    check = roundtrip("labels", {"summary": summary(), "labels": labels})["result"]
    assert check["labelled"] == 1


def test_compare_pairs_the_cases():
    def side(passed):
        return [
            {"task": f"t{n}", "agent": "vis", "model": "m", "passed": p, "runs": 1}
            for n, p in enumerate(passed)
        ]

    result = roundtrip(
        "compare", {"baseline": side([0] * 20), "candidate": side([1] * 20)}
    )["result"]
    assert result["paired_cases"] == 20
    assert result["verdict"] == "better"


def test_agreement_gives_the_kappa_advice():
    items = [{"name": f"i{n}", "automatic": "pass", "human": "pass"} for n in range(3)]
    result = roundtrip("agreement", {"items": items})["result"]
    assert result["labelled"] == 3
    assert result["advice"]


def test_leaderboard_reads_every_page(monkeypatch):
    replies = [
        {
            "leaderboard": {"title": "Terminal-Bench 2.0"},
            "rows": [],
            "pagination": {"total_pages": 1},
        }
    ]
    monkeypatch.setattr(
        "blockether.vis.evals.leaderboard._post_json", lambda url, body: replies.pop()
    )
    body = {"package": "terminal-bench/terminal-bench", "name": "2-0-0"}
    result = roundtrip("leaderboard", body)["result"]
    assert result["title"] == "Terminal-Bench 2.0"
    assert result["vis"] is None


@pytest.mark.parametrize(
    ("operation", "body", "message"),
    [
        ("missing", {}, "Unknown evaluation operation"),
        ("report", [], "must be a JSON object"),
        ("report", {}, "`summary` must be an object"),
        ("compare", {"baseline": [{}], "candidate": []}, "needs task, agent"),
        ("agreement", {"items": [{"name": "x"}]}, "needs name, automatic"),
        ("leaderboard", {"name": "1-0-0"}, "`package` is required"),
        ("leaderboard", {"package": "org/bench"}, "`name` is required"),
    ],
)
def test_invalid_bodies_give_an_error(operation, body, message):
    assert message in handle(operation, body)["error"]


def test_plain_converts_results_to_json_values():
    assert plain({1: (float("nan"), _gateway.PurePath("a/b"))}) == {"1": [None, "a/b"]}
