"""Tests for the grader of one Vis e2e trial."""

import dataclasses
import json

import pytest

from vis_harbor import grade
from vis_harbor.tests import atif

PROVIDER, MODEL = "zai-coding-plan", "glm-5.3-flash"
SCENARIO = {
    "want": {"notes.txt": ["fixed"]},
    "wantnot": {"notes.txt": ["broken"]},
    "want_answer": ["Done"],
    "want_forms": ["patch("],
    "want_tools": ["patch"],
}
PATCH_CODE = 'patch("notes.txt", edits)'
SESSION_USAGE = {"fold_count": 1, "samples": [{"tokens": 3}]}
GOAL = {"status": "complete", "iterations_used": 2}
PASSING_GUARDS = {
    "activities": True,
    "files": True,
    "answer": True,
    "forms": True,
    "structured": True,
    "token_metrics": True,
    "tools": True,
}


def _form(ref, code, *, operation="patch", state="succeeded", iteration=1):
    """Return one agent step that runs ``code`` as a block with one Activity row."""
    activity = atif.receipt(atif.row(f"row-{ref}", operation, state))
    return atif.step(
        iteration,
        atif.block(ref, code, stdout="ok\n", activity=activity),
        provider=PROVIDER,
        model=MODEL,
    )


def _run(*steps, **more):
    """Return the ATIF document of a run on the test route."""
    return atif.trajectory(*steps, provider=PROVIDER, model=MODEL, **more)


def _trial(document, **more):
    return atif.trial(document, wall_s=1.25, provider=PROVIDER, model=MODEL, **more)


def _grade(work, *, text="fixed\n", answer="Done.", code=PATCH_CODE, operation="patch"):
    (work / "notes.txt").write_text(text)
    document = _run(_form("t1/i1/f1", code, operation=operation), answer=answer)
    return grade.grade_trial(SCENARIO, work, _trial(document))


def test_a_complete_trial_passes_every_guard(tmp_path):
    result = _grade(tmp_path)

    assert result.is_passed
    assert result.result["guards"] == PASSING_GUARDS
    assert set(result.rewards.values()) == {1.0}
    assert result.rewards.keys() == {
        "reward",
        "converged",
        "correct",
        "no_errors",
        *PASSING_GUARDS,
    }
    assert result.result["tokens"]["uncached"] == 60
    assert result.result["provider"] == PROVIDER
    assert result.result["model"] == MODEL
    assert result.result["forms"] == 1
    assert result.result["provider_calls"] == 2
    assert result.result["edit_path"] == "patch"
    assert result.result["wall"] == 1.2


@pytest.mark.parametrize(
    ("guard", "change", "message"),
    [
        ("files", {"text": ""}, "missing notes.txt:'fixed'"),
        ("files", {"text": "fixed broken"}, "still present notes.txt:'broken'"),
        ("answer", {"answer": "No change."}, "answer missing 'Done'"),
        ("forms", {"code": "edit(notes)"}, "form containing 'patch(' not used"),
        ("tools", {"operation": "cat"}, "tool 'patch' not used"),
    ],
)
def test_each_failed_guard_fails_only_its_own_reward(tmp_path, guard, change, message):
    result = _grade(tmp_path, **change)

    assert not result.is_passed
    assert result.result["guards"] == {**PASSING_GUARDS, guard: False}
    assert message in result.result["detail"]
    assert result.rewards[guard] == 0.0
    assert result.rewards["reward"] == 0.0
    assert result.rewards["correct"] == 0.0
    assert result.rewards["converged"] == 1.0
    assert result.rewards["no_errors"] == 1.0


@pytest.mark.parametrize(
    ("state", "count"),
    [("failed", "activity_failures"), ("running", "incomplete_activities")],
)
def test_a_failed_or_unfinished_activity_is_an_error(tmp_path, state, count):
    document = _run(_form("t1/i1/f1", "cat(path)", operation="cat", state=state))
    result = grade.grade_trial({"want_answer": ["Done"]}, tmp_path, _trial(document))

    assert not result.is_passed
    assert result.result["guards"]["activities"] is False
    assert result.result["err_msgs"] == [f"activity cat {state}"]
    assert result.result[count] == 1
    assert result.rewards["no_errors"] == 0.0


@pytest.mark.parametrize(
    ("scenario", "exit_code", "message"),
    [
        ({}, None, "vis-agent timed out after 300s"),
        ({"timeout_s": 30}, None, "vis-agent timed out after 30s"),
        ({}, 2, "vis-agent exited 2: engine stopped"),
    ],
)
def test_a_timeout_or_a_failed_exit_is_an_error(tmp_path, scenario, exit_code, message):
    trial = _trial(
        _run(_form("t1/i1/f1", PATCH_CODE)),
        exit_code=exit_code,
        stderr="\nengine stopped\nmore detail\n",
    )
    result = grade.grade_trial(scenario, tmp_path, trial)

    assert not result.is_passed
    assert result.result["converged"] is True
    assert result.result["err_msgs"] == [message]
    assert result.rewards["no_errors"] == 0.0


def test_a_run_error_does_not_converge(tmp_path):
    document = _run(_form("t1/i1/f1", PATCH_CODE), answer=None, error="provider failed")
    result = grade.grade_trial({}, tmp_path, _trial(document))

    assert not result.is_passed
    assert result.result["converged"] is False
    assert result.result["err_msgs"] == ["provider failed"]


def test_a_requested_route_needs_every_call_and_the_bill_on_that_route(tmp_path):
    scenario = {"want_requested_route": True}
    document = _run(_form("t1/i1/f1", PATCH_CODE))
    assert grade.grade_trial(scenario, tmp_path, _trial(document)).is_passed

    document["agent"]["model_name"] = "other-model"
    result = grade.grade_trial(scenario, tmp_path, _trial(document))

    assert result.result["guards"]["requested_route"] is False


def _fold_run(**more):
    return _run(
        _form("t1/i1/f1", "x = 1", operation="cat"),
        _form(
            "t1/i2/f1",
            'print(fold_session("-t1/i1", "gist"))',
            operation="cat",
            iteration=2,
        ),
        **more,
    )


@pytest.mark.parametrize(("fold_count", "is_passed"), [(1, True), (2, False)])
def test_a_prefix_fold_needs_one_recorded_fold_in_the_session_usage(
    tmp_path, fold_count, is_passed
):
    trial = _trial(_fold_run(session_usage={"fold_count": fold_count}))
    result = grade.grade_trial({"want_folded_prefix": True}, tmp_path, trial)

    assert result.result["guards"]["folded_prefix"] is True
    assert result.result["guards"]["fold_count"] is is_passed
    assert result.is_passed is is_passed


@pytest.mark.parametrize(
    ("session_id", "detail"),
    [
        (None, "persistent run returned no session id"),
        ("session-1", "the trajectory has no session usage"),
    ],
)
def test_usage_guards_fail_without_a_session_or_its_usage(tmp_path, session_id, detail):
    document = _run(_form("t1/i1/f1", PATCH_CODE), session_id=session_id)
    result = grade.grade_trial(
        {"want_context_budget": True}, tmp_path, _trial(document)
    )

    assert result.result["guards"]["context_budget"] is False
    assert detail in result.result["detail"]


@pytest.mark.parametrize(
    ("goal", "is_passed"),
    [
        ({"status": "complete", "iterations_used": 2}, True),
        ({"status": "active"}, False),
    ],
)
def test_a_goal_scenario_reads_the_goal_of_the_trajectory(tmp_path, goal, is_passed):
    document = _run(_form("t1/i1/f1", PATCH_CODE), goal=goal)
    result = grade.grade_trial({"want_goal_complete": True}, tmp_path, _trial(document))

    assert result.result["guards"]["goal_complete"] is is_passed
    assert result.is_passed is is_passed


def _write_agent_logs(logs, document, *, reasoning_effort=None, stderr=""):
    logs.mkdir(parents=True, exist_ok=True)
    record = {
        "provider": PROVIDER,
        "model": MODEL,
        "reasoning_effort": reasoning_effort,
        "exit_code": 0,
        "wall_s": 2.5,
        "is_cancelled": False,
    }
    (logs / grade.RUN_FILE).write_text(json.dumps(record))
    (logs / grade.STDERR_FILE).write_text(stderr)
    (logs / grade.TRAJECTORY_FILE).write_text(json.dumps(document))


def test_load_trial_reads_the_run_record_the_stderr_and_the_trajectory(tmp_path):
    document = _run(
        _form("t1/i1/f1", PATCH_CODE), session_usage=SESSION_USAGE, goal=GOAL
    )
    _write_agent_logs(tmp_path, document, reasoning_effort="high", stderr="warning\n")

    loaded = grade.load_trial(tmp_path)

    assert dataclasses.replace(loaded, trajectory=None) == grade.TrialFiles(
        trajectory=None,
        trajectory_problem=None,
        stderr="warning\n",
        exit_code=0,
        wall_s=2.5,
        is_cancelled=False,
        provider=PROVIDER,
        model=MODEL,
        reasoning_effort="high",
    )
    assert loaded.trajectory.session_id == "session-1"
    assert loaded.trajectory.final_answer == "Done."
    assert [call.ref for call in loaded.trajectory.tool_calls] == ["t1/i1/f1"]
    assert loaded.trajectory.session_usage["fold_count"] == 1
    assert loaded.trajectory.goal["status"] == "complete"


@pytest.mark.parametrize(
    ("trajectory", "problem"),
    [
        (None, "no trajectory.json"),
        (
            "not json",
            "an invalid trajectory.json: invalid trajectory: the file is not JSON",
        ),
        ("{}", "an invalid trajectory.json: invalid trajectory: schema_version"),
    ],
)
def test_load_trial_gives_empty_values_and_the_problem_for_missing_or_invalid_files(
    tmp_path, trajectory, problem
):
    if trajectory is not None:
        (tmp_path / grade.TRAJECTORY_FILE).write_text(trajectory)

    loaded = grade.load_trial(tmp_path)

    assert loaded.trajectory_problem.startswith(problem)
    assert dataclasses.replace(loaded, trajectory_problem=None) == grade.TrialFiles(
        trajectory=None,
        trajectory_problem=None,
        stderr="",
        exit_code=None,
        wall_s=0.0,
        is_cancelled=False,
        provider=None,
        model=None,
        reasoning_effort=None,
    )


def test_a_finished_run_without_a_trajectory_is_an_error(tmp_path):
    trial = _trial(_run(), trajectory=None, trajectory_problem="no trajectory.json")
    result = grade.grade_trial({}, tmp_path, trial)

    assert not result.is_passed
    assert result.result["converged"] is False
    assert result.result["err_msgs"] == ["vis-agent wrote no trajectory.json"]


def test_an_invalid_activity_receipt_is_an_error(tmp_path):
    activity = atif.receipt(atif.row("row-1", "cat"))
    del activity["rows"][0]["summary"]
    block = atif.block("t1/i1/f1", "cat(path)", activity=activity)
    document = _run(atif.step(1, block, provider=PROVIDER, model=MODEL))
    result = grade.grade_trial({}, tmp_path, _trial(document, strict=False))

    assert not result.is_passed
    assert result.result["err_msgs"][0].startswith("invalid Activity in t1/i1/f1: ")


def test_main_writes_the_grade_and_the_rewards(tmp_path, capsys):
    scenario = tmp_path / "scenario.json"
    scenario.write_text(json.dumps({**SCENARIO, "want_goal_complete": True}))
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    (workspace / "notes.txt").write_text("fixed\n")
    agent_logs = tmp_path / "agent"
    _write_agent_logs(agent_logs, _run(_form("t1/i1/f1", PATCH_CODE), goal=GOAL))
    verifier_logs = tmp_path / "verifier"

    exit_code = grade.main(
        [
            "--scenario",
            str(scenario),
            "--scenario-id",
            "fix-notes",
            "--workspace",
            str(workspace),
            "--agent-logs",
            str(agent_logs),
            "--verifier-logs",
            str(verifier_logs),
        ]
    )

    assert exit_code == 0
    record = json.loads((verifier_logs / grade.GRADE_FILE).read_text())
    rewards = json.loads((verifier_logs / grade.REWARD_FILE).read_text())
    assert record["id"] == "fix-notes"
    assert record["passed"] is True
    assert record["guards"]["goal_complete"] is True
    assert rewards["reward"] == 1.0
    assert capsys.readouterr().out.startswith("PASS reward=1 ")
