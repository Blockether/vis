"""Tests for the trajectory reader of `blockether.vis.evals`. All data is synthetic."""

import dataclasses
import json
import re

import pytest
from blockether.vis.evals import (
    FunctionCall,
    StepMetrics,
    Trajectory,
    load_trajectory,
    merge_trajectories,
    parse_trajectory,
)


def _row(row_id, sequence, operation, state="succeeded", **more):
    row = {
        "id": row_id,
        "sequence": sequence,
        "operation": operation,
        "presenter": "generic",
        "signal": "observation",
        "state": state,
        "summary": f"{operation} finished",
        "resources": [],
        "evidence": [
            {"kind": "arguments", "text": f"{operation}('.')"},
            {"kind": "result", "text": "3 entries"},
        ],
    }
    row.update(more)
    return row


def _counts(succeeded=0, failed=0):
    return {"running": 0, "succeeded": succeeded, "failed": failed, "cancelled": 0}


LS_ACTIVITY = {
    "state": "succeeded",
    "counts": _counts(succeeded=1),
    "rows": [_row("row-1", 0, "ls", duration_ms=4, argument_key="a" * 64)],
}

SHELL_ACTIVITY = {
    "state": "failed",
    "counts": _counts(succeeded=1, failed=1),
    "rows": [
        _row(
            "row-2",
            0,
            "shell",
            state="failed",
            presenter="shell",
            signal="external",
            handle_id="shell-1",
            children=[
                _row("row-4", 2, "shell", state="failed", presenter="shell")
                | {
                    "evidence": [
                        {"kind": "arguments", "text": "make test"},
                        {"kind": "error", "text": "exit status 2"},
                    ]
                },
                _row("row-3", 1, "shell", presenter="shell"),
            ],
        )
    ],
}


def _document():
    return {
        "schema_version": "ATIF-v1.8",
        "session_id": "session-1",
        "trajectory_id": "trajectory-1",
        "agent": {
            "name": "vis",
            "version": "dev",
            "model_name": "model-a",
            "extra": {"provider": "provider-a", "profile": {"name": "cli"}},
        },
        "steps": [
            {
                "step_id": 1,
                "source": "user",
                "message": "Count the files.",
                "timestamp": "2026-01-01T00:00:00Z",
            },
            {
                "step_id": 2,
                "source": "agent",
                "message": "I list the files.",
                "model_name": "model-a",
                "metrics": {
                    "prompt_tokens": 1200,
                    "completion_tokens": 80,
                    "cached_tokens": 1000,
                    "cost_usd": 0.012,
                    "extra": {"cache_creation_tokens": 150, "reasoning_tokens": 20},
                },
                "extra": {"iteration": 0, "is_final": False, "provider": "provider-a"},
                "tool_calls": [
                    {
                        "tool_call_id": "call-1",
                        "function_name": "python_execution",
                        "arguments": {"code": "print(len(ls('.')))"},
                        "extra": {
                            "status": "success",
                            "ref": "form-1",
                            "started_at_ms": 10,
                            "finished_at_ms": 25,
                            "is_repaired": False,
                            "is_timeout": False,
                            "stdout_chars": 2,
                            "activity": LS_ACTIVITY,
                            "new_vis_field": 1,
                        },
                    },
                    {
                        "tool_call_id": "call-2",
                        "function_name": "python_execution",
                        "arguments": {"code": "shell('make test')"},
                        "extra": {
                            "status": "error",
                            "is_repaired": True,
                            "repaired_source": "shell('make test')\n",
                            "is_timeout": False,
                            "error": {"type": "RuntimeError", "message": "make failed"},
                            "activity": SHELL_ACTIVITY,
                        },
                    },
                ],
                "observation": {
                    "results": [
                        {"source_call_id": "call-1", "content": "3\n"},
                        {
                            "source_call_id": "call-2",
                            "content": "RuntimeError: make failed",
                        },
                    ]
                },
            },
            {
                "step_id": 3,
                "source": "agent",
                "message": "3",
                "extra": {"iteration": 1, "is_final": True},
            },
        ],
        "final_metrics": {
            "total_prompt_tokens": 1200,
            "total_completion_tokens": 80,
            "total_cached_tokens": 1000,
            "total_cost_usd": 0.012,
            "total_steps": 3,
            "extra": {
                "cache_creation_tokens": 150,
                "total_tokens": 1280,
                "iteration_count": 2,
                "duration_ms": 900,
            },
        },
        "extra": {
            "status": "success",
            "goal": {"objective": "Count the files.", "state": "complete"},
            "session_usage": {"tokens": {"input": 1200}},
            "eval": {"is_valid": True},
        },
        "subagent_trajectories": [
            {
                "session_id": "child-1",
                "agent": {"name": "vis"},
                "steps": [
                    {"step_id": 1, "source": "user", "message": "Check one file."},
                    {
                        "step_id": 2,
                        "source": "agent",
                        "message": "Done.",
                        "extra": {"iteration": 0, "is_final": True},
                        "tool_calls": [
                            {
                                "tool_call_id": "call-9",
                                "function_name": "python_execution",
                                "arguments": {"code": "ls('.')"},
                                "extra": {"activity": LS_ACTIVITY},
                            }
                        ],
                    },
                ],
                "extra": {"status": "completed"},
            }
        ],
        "notes": "An unknown key stays in the document.",
    }


def _keys(value):
    if isinstance(value, dict):
        for key, item in value.items():
            yield key
            yield from _keys(item)
    elif isinstance(value, list):
        for item in value:
            yield from _keys(item)


def test_reads_the_run_its_totals_and_its_final_answer():
    run = parse_trajectory(_document())
    assert isinstance(run, Trajectory)
    assert (run.session_id, run.agent_name, run.model_name, run.provider) == (
        "session-1",
        "vis",
        "model-a",
        "provider-a",
    )
    assert run.profile == {"name": "cli"}
    assert run.status == "success" and run.error is None
    assert run.goal["state"] == "complete"
    assert run.session_usage["tokens"]["input"] == 1200
    assert run.evaluation == {"is_valid": True}
    assert run.user_message == "Count the files."
    assert [step.step_id for step in run.agent_steps] == [2, 3]
    assert run.final_step.step_id == 3 and run.final_answer == "3"
    totals = run.final_metrics
    assert (totals.total_cost_usd, totals.total_tokens, totals.iteration_count) == (
        0.012,
        1280,
        2,
    )
    assert totals.reasoning_tokens is None and totals.subagent_cost_usd is None


def test_merge_joins_the_turns_of_one_session():
    first, second = _document(), _document()
    first["extra"]["error"] = None
    merged = merge_trajectories([first, second])
    run = parse_trajectory(merged)
    assert [step.step_id for step in run.steps] == list(
        range(1, 2 * len(first["steps"]) + 1)
    )
    assert run.user_message == "Count the files." and run.final_answer == "3"
    totals = run.final_metrics
    assert (totals.total_cost_usd, totals.total_tokens, totals.iteration_count) == (
        0.024,
        2560,
        4,
    )
    assert run.extra["turns"] == 2
    assert first["steps"][-1]["step_id"] == 3


def test_merge_keeps_the_first_turn_error():
    failed, finished = _document(), _document()
    failed["extra"]["error"] = "provider failed"
    finished["extra"].pop("error", None)
    assert parse_trajectory(merge_trajectories([failed, finished])).error == (
        "provider failed"
    )
    with pytest.raises(ValueError, match="no trajectory"):
        merge_trajectories([])


def test_each_function_call_has_its_function_arguments_result_state_and_step_cost():
    run = parse_trajectory(_document())
    calls = run.function_calls()
    assert [(c.operation, c.id, c.depth) for c in calls] == [
        ("ls", "row-1", 0),
        ("shell", "row-2", 0),
        ("shell", "row-3", 1),
        ("shell", "row-4", 1),
    ]
    first = calls[0]
    assert isinstance(first, FunctionCall)
    assert (first.arguments, first.result, first.state) == (
        "ls('.')",
        "3 entries",
        "succeeded",
    )
    assert (first.step_id, first.iteration, first.tool_call_id) == (2, 0, "call-1")
    assert first.duration_ms == 4 and first.argument_key == "a" * 64
    assert first.step_metrics == StepMetrics(1200, 80, 1000, 0.012, 150, 20)
    data = first.to_dict()
    assert data["operation"] == "ls" and data["step_metrics"]["cost_usd"] == 0.012
    assert "children" not in data["row"]


def test_child_rows_keep_their_parent_and_filters_select_calls():
    run = parse_trajectory(_document())
    failed = run.function_calls(state="failed")
    assert [(c.id, c.parent_id, c.error) for c in failed] == [
        ("row-2", None, None),
        ("row-4", "row-2", "exit status 2"),
    ]
    assert [c.id for c in run.function_calls(operation="ls")] == ["row-1"]
    assert run.function_calls(operation="grep") == ()


def test_a_failed_block_keeps_its_error_output_and_repair():
    tool = parse_trajectory(_document()).tool_calls[1]
    assert (tool.status, tool.error["type"], tool.output) == (
        "error",
        "RuntimeError",
        "RuntimeError: make failed",
    )
    assert tool.is_repaired and tool.repaired_source == "shell('make test')\n"
    assert tool.activity_state == "failed" and tool.activity_counts.failed == 1
    assert tool.code == "shell('make test')"
    assert tool.stdout == ""


def test_stdout_is_the_start_of_the_output_before_the_error_text():
    call = parse_trajectory(_document()).tool_calls[0]
    assert call.stdout == "3\n"

    def stdout(output, stdout_chars):
        return dataclasses.replace(
            call, output=output, stdout_chars=stdout_chars
        ).stdout

    assert stdout("partial\nRuntimeError: boom", 7) == "partial"
    assert stdout("3\n\nRuntimeError: boom", 2) == "3\n"
    assert stdout("RuntimeError: boom", 3) == ""
    assert stdout("RuntimeError: boom", None) == ""
    assert stdout(None, 2) == ""


def test_a_step_without_metrics_or_blocks_has_empty_values():
    final = parse_trajectory(_document()).steps[2]
    assert final.metrics is None and final.tool_calls == ()
    assert final.function_calls == () and final.is_final


def test_child_runs_are_separate_trajectories():
    run = parse_trajectory(_document())
    (child,) = run.subagent_trajectories
    assert (child.session_id, child.schema_version, child.status) == (
        "child-1",
        "ATIF-v1.8",
        "completed",
    )
    assert [c.tool_call_id for c in child.function_calls()] == ["call-9"]
    assert all(c.tool_call_id != "call-9" for c in run.function_calls())


def test_unknown_keys_stay_and_the_result_is_immutable_and_independent():
    document = _document()
    run = parse_trajectory(document)
    assert run.document["notes"] == "An unknown key stays in the document."
    assert run.tool_calls[0].extra["new_vis_field"] == 1
    document["steps"][1]["tool_calls"][0]["extra"]["status"] = "changed"
    assert run.tool_calls[0].status == "success"
    with pytest.raises(dataclasses.FrozenInstanceError):
        run.session_id = "other"
    with pytest.raises(TypeError):
        run.tool_calls[0].extra["status"] = "changed"


def test_to_dict_gives_json_data_with_snake_case_keys():
    data = parse_trajectory(_document()).to_dict()
    json.dumps(data)
    keys = set(_keys(data))
    assert {"function_calls", "step_metrics", "tool_call_id", "final_answer"} <= keys
    assert all(re.fullmatch(r"[a-z][a-z0-9_]*", key) for key in keys)


def test_a_persisted_run_without_final_marks_ends_with_its_last_answer():
    document = _document()
    for step in document["steps"][1:]:
        step["extra"].pop("is_final")
    assert parse_trajectory(document).final_answer == "3"
    document["steps"].pop()
    assert parse_trajectory(document).final_answer is None


def test_an_invalid_activity_fails_without_content_unless_strict_is_off():
    document = _document()
    document["steps"][1]["tool_calls"][0]["extra"]["activity"] = {
        "declaration": {"label": "secret-value-123"}
    }
    with pytest.raises(ValueError) as failure:
        parse_trajectory(document)
    assert "call-1" in str(failure.value)
    assert "secret-value-123" not in str(failure.value)
    tool = parse_trajectory(document, strict=False).tool_calls[0]
    assert tool.activity is None and tool.function_calls == ()
    assert "activity.projection" in tool.activity_problem


@pytest.mark.parametrize(
    "document",
    [
        {"steps": []},
        {"schema_version": "1.0", "steps": []},
        {"schema_version": "ATIF-v1.8", "steps": "none"},
        {"schema_version": "ATIF-v1.8", "steps": [{"source": "user"}]},
    ],
)
def test_rejects_a_document_that_is_not_an_atif_trajectory(document):
    with pytest.raises(ValueError, match="invalid trajectory"):
        parse_trajectory(document)


def test_load_trajectory_reads_a_file(tmp_path):
    path = tmp_path / "trajectory.json"
    path.write_text(json.dumps(_document()), encoding="utf-8")
    assert load_trajectory(path).final_answer == "3"
    path.write_text("{not json", encoding="utf-8")
    with pytest.raises(ValueError, match="not JSON"):
        load_trajectory(path)
