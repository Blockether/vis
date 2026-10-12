"""Tests for the grader of one Vis e2e trial."""

import json

import pytest

from vis_harbor import grade

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


def _line(payload, event="progress"):
    return json.dumps({"event": event, "payload": payload})


def _provider_call(iteration):
    return _line(
        {
            "phase": "provider-call",
            "provider": PROVIDER,
            "model": MODEL,
            "iteration": iteration,
        }
    )


def _form(scope, code, *, operation="patch", state="succeeded", iteration=1):
    """Return the trace lines of one provider call and its executed form."""
    row = {"id": f"row-{scope}", "operation": operation, "state": state}
    return [
        _provider_call(iteration),
        _line(
            {
                "phase": "form-start",
                "scope": scope,
                "code": code,
                "iteration": iteration,
            }
        ),
        _line({"phase": "form-activity", "scope": scope, "activity": {"rows": [row]}}),
        _line(
            {
                "phase": "form-result",
                "scope": scope,
                "iteration": iteration,
                "stdout": "ok\n",
                "tool-call-id": f"call-{scope}",
            }
        ),
    ]


def _result(answer="Done.", **more):
    payload = {
        "answer": {"answer": answer},
        "session-id": "session-1",
        "tokens": {"input": 100, "cached": 40, "output": 10, "total": 110},
        "cost": {"provider": PROVIDER, "model": MODEL},
        **more,
    }
    return _line(payload, event="result")


def _trial(lines, **more):
    fields = {
        "trace": "\n".join(lines) + "\n",
        "exit_code": 0,
        "wall_s": 1.25,
        "is_cancelled": False,
        "provider": PROVIDER,
        "model": MODEL,
        "reasoning_effort": None,
        "session_usage": None,
        "goal": None,
        **more,
    }
    return grade.TrialFiles(**fields)


def _grade(work, *, text="fixed\n", answer="Done.", code=PATCH_CODE, operation="patch"):
    (work / "notes.txt").write_text(text)
    lines = [*_form("t1/i1", code, operation=operation), _result(answer)]
    return grade.grade_trial(SCENARIO, work, _trial(lines))


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
    assert result.result["provider_calls"] == 1
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
    lines = [*_form("t1/i1", "cat(path)", operation="cat", state=state), _result()]
    result = grade.grade_trial({"want_answer": ["Done"]}, tmp_path, _trial(lines))

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
    lines = ["engine stopped", *_form("t1/i1", PATCH_CODE), _result()]
    result = grade.grade_trial(scenario, tmp_path, _trial(lines, exit_code=exit_code))

    assert not result.is_passed
    assert result.result["converged"] is True
    assert result.result["err_msgs"] == [message]
    assert result.rewards["no_errors"] == 0.0


def test_an_error_block_in_the_result_does_not_converge(tmp_path):
    error = {"type": "error", "message": "provider failed"}
    lines = [*_form("t1/i1", PATCH_CODE), _result(content=[error])]
    result = grade.grade_trial({}, tmp_path, _trial(lines))

    assert not result.is_passed
    assert result.result["converged"] is False
    assert result.result["err_msgs"] == ["provider failed"]


def test_a_requested_route_needs_every_call_and_the_bill_on_that_route(tmp_path):
    scenario = {"want_requested_route": True}
    lines = [*_form("t1/i1", PATCH_CODE), _result()]
    assert grade.grade_trial(scenario, tmp_path, _trial(lines)).is_passed

    billed = {"provider": PROVIDER, "model": "other-model"}
    lines = [*_form("t1/i1", PATCH_CODE), _result(cost=billed)]
    result = grade.grade_trial(scenario, tmp_path, _trial(lines))

    assert result.result["guards"]["requested_route"] is False


def _fold_lines():
    return [
        *_form("t1/i1", "x = 1", operation="cat"),
        *_form(
            "t1/i2",
            'print(fold_session("-t1/i1", "gist"))',
            operation="cat",
            iteration=2,
        ),
        _provider_call(3),
        _result(),
    ]


@pytest.mark.parametrize(("fold_count", "is_passed"), [(1, True), (2, False)])
def test_a_prefix_fold_needs_one_recorded_fold_in_the_session_usage(
    tmp_path, fold_count, is_passed
):
    trial = _trial(_fold_lines(), session_usage={"fold_count": fold_count})
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
    lines = [*_form("t1/i1", PATCH_CODE), _result(**{"session-id": session_id})]
    result = grade.grade_trial({"want_context_budget": True}, tmp_path, _trial(lines))

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
    lines = [*_form("t1/i1", PATCH_CODE), _result()]
    result = grade.grade_trial(
        {"want_goal_complete": True}, tmp_path, _trial(lines, goal=goal)
    )

    assert result.result["guards"]["goal_complete"] is is_passed
    assert result.is_passed is is_passed


def _write_agent_logs(logs, lines, *, reasoning_effort=None):
    logs.mkdir(parents=True, exist_ok=True)
    (logs / grade.TRACE_FILE).write_text("\n".join(lines) + "\n")
    record = {
        "provider": PROVIDER,
        "model": MODEL,
        "reasoning_effort": reasoning_effort,
        "exit_code": 0,
        "wall_s": 2.5,
        "is_cancelled": False,
    }
    (logs / grade.RUN_FILE).write_text(json.dumps(record))
    trajectory = {
        "schema_version": "ATIF-v1.6",
        "session_id": "session-1",
        "agent": {"name": "vis", "version": "dev"},
        "steps": [],
        "extra": {"session_usage": SESSION_USAGE, "goal": GOAL},
    }
    (logs / grade.TRAJECTORY_FILE).write_text(json.dumps(trajectory))


def test_load_trial_reads_the_trace_the_run_record_and_the_trajectory(tmp_path):
    _write_agent_logs(tmp_path, ["line"], reasoning_effort="high")

    loaded = grade.load_trial(tmp_path)

    assert loaded == grade.TrialFiles(
        trace="line\n",
        exit_code=0,
        wall_s=2.5,
        is_cancelled=False,
        provider=PROVIDER,
        model=MODEL,
        reasoning_effort="high",
        session_usage=SESSION_USAGE,
        goal=GOAL,
    )
    assert type(loaded.session_usage) is dict
    assert type(loaded.session_usage["samples"]) is list
    assert type(loaded.session_usage["samples"][0]) is dict
    assert type(loaded.goal) is dict


@pytest.mark.parametrize("trajectory", [None, "not json", "{}"])
def test_load_trial_gives_empty_values_for_missing_or_invalid_files(
    tmp_path, trajectory
):
    if trajectory is not None:
        (tmp_path / grade.TRAJECTORY_FILE).write_text(trajectory)

    assert grade.load_trial(tmp_path) == grade.TrialFiles(
        trace="",
        exit_code=None,
        wall_s=0.0,
        is_cancelled=False,
        provider=None,
        model=None,
        reasoning_effort=None,
        session_usage=None,
        goal=None,
    )


def test_main_writes_the_grade_and_the_rewards(tmp_path, capsys):
    scenario = tmp_path / "scenario.json"
    scenario.write_text(json.dumps({**SCENARIO, "want_goal_complete": True}))
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    (workspace / "notes.txt").write_text("fixed\n")
    agent_logs = tmp_path / "agent"
    _write_agent_logs(agent_logs, [*_form("t1/i1", PATCH_CODE), _result()])
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
