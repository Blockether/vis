from pathlib import Path
import importlib.util
import json
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("metrics", ROOT / "metrics.py")
metrics = importlib.util.module_from_spec(spec)
spec.loader.exec_module(metrics)  # type: ignore[union-attr]


def test_patch_bloat_counts_diff_lines():
    diff = """diff --git a/a b/a
--- a/a
+++ b/a
@@
-old
+new
+more
"""
    assert metrics.patch_bloat(diff) == {"files": 1, "added_lines": 2, "deleted_lines": 1, "total_changed_lines": 3}


def test_summarize_trace_counts_trace_chunk_iterations(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        "\n".join(
            [
                '{"event":"trace-chunk","payload":{"phase":"provider-call","iteration":1}}',
                '{"event":"trace-chunk","payload":{"phase":"reasoning","iteration":3}}',
                '{"event":"trace-chunk","payload":{"phase":"iteration-final","iteration":3}}',
            ]
        )
    )

    assert metrics.summarize_trace(trace)["iterations"] == 3


def test_summarize_trace_counts_payload_tool_invocations_once(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        "\n".join(
            [
                json.dumps(
                    {
                        "event": "trace-chunk",
                        "payload": {
                            "phase": "form-start",
                            "scope": "t1/i1/f1",
                            "tool-name": "cat",
                            "iteration": 1,
                        },
                    }
                ),
                json.dumps(
                    {
                        "event": "trace-chunk",
                        "payload": {
                            "phase": "tool-start",
                            "scope": "t1/i1/f1",
                            "tool-event": {"op": "cat"},
                            "iteration": 1,
                        },
                    }
                ),
            ]
        )
    )

    summary = metrics.summarize_trace(trace)

    assert summary["tool_calls"] == 1
    assert summary["file_reads"] == 1


def test_summarize_trace_reads_wrapped_final_usage(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        json.dumps(
            {
                "event": "trace-chunk",
                "payload": {
                    "phase": "iteration-final",
                    "iteration": 81,
                    "final": {
                        "iteration-count": 81,
                        "duration-ms": 1889654.425532,
                        "tokens": {"input": 5384504, "output": 62380, "total": 5446884},
                        "cost": {"total-cost": 7.812777599999998},
                    },
                },
            }
        )
        + "\n"
    )

    summary = metrics.summarize_trace(trace)

    assert summary["iterations"] == 81
    assert summary["tokens"]["total"] == 5446884
    assert summary["cost_usd"] == 7.812777599999998
    assert round(summary["elapsed_s"], 3) == 1889.654


def test_summarize_trace_reads_final_vis_effort_evidence(tmp_path):
    trace = tmp_path / "trace.jsonl"
    evidence = {
        "valid?": True,
        "invalid-reasons": [],
        "reasoning-effort": {
            "requested": "max",
            "iterations": [
                {
                    "provider": "zai-coding-plan",
                    "model": "glm-5.2",
                    "effective": "max",
                    "wire-fragment": {
                        "thinking": {"type": "enabled"},
                        "reasoning_effort": "max",
                    },
                    "fallback?": False,
                }
            ],
        },
    }
    trace.write_text(json.dumps({"event": "result", "payload": {"eval": evidence}}) + "\n")

    summary = metrics.summarize_trace(trace)

    assert summary["eval"] == evidence
    assert summary["eval_valid"] is True
    assert summary["reasoning_effort"] == "max"
    assert summary["reasoning_effort_explicit"] is True


def _run_metrics(tmp_path, run_dir):
    out = tmp_path / "summary.json"
    old_argv = sys.argv
    try:
        sys.argv = [
            "metrics.py",
            "--task-id",
            "task-a",
            "--run-dir",
            str(run_dir),
            "--out",
            str(out),
        ]
        assert metrics.main() == 0
    finally:
        sys.argv = old_argv
    return json.loads(out.read_text())


def test_summary_reads_copied_verifier_results(tmp_path):
    run_dir = tmp_path / "run"
    verifier_dir = run_dir / "harbor-output" / "trial" / "verifier"
    verifier_dir.mkdir(parents=True)
    (verifier_dir / "verifier_results.json").write_text(
        json.dumps({"passed": 14, "total": 15, "all_pass": False, "tests": {}})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["verifier_pass"] is False
    assert summary["verifier"] == {"passed": 14, "total": 15, "all_pass": False, "runner_errors": None}
    assert summary["failure_class"] == "verifier_failed"


def test_summary_includes_judge_status(tmp_path):
    run_dir = tmp_path / "run"
    verifier_dir = run_dir / "harbor-output" / "trial" / "verifier"
    verifier_dir.mkdir(parents=True)
    (verifier_dir / "judge_output.json").write_text(
        json.dumps({"rubric_status": "failed:api_error", "rubric_error": "bad tool_choice"})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["judge"] == {"rubric_status": "failed:api_error", "rubric_error": "bad tool_choice"}


def test_summary_reads_harbor_trial_exception(tmp_path):
    run_dir = tmp_path / "run"
    trial = run_dir / "harbor-output" / "trial"
    trial.mkdir(parents=True)
    (trial / "result.json").write_text(
        json.dumps({"exception_info": {"exception_type": "RewardFileNotFoundError", "exception_message": "missing"}})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["harbor_exception"]["exception_type"] == "RewardFileNotFoundError"
    assert summary["failure_class"] == "RewardFileNotFoundError"


def test_summary_preserves_false_reward_fields(tmp_path):
    run_dir = tmp_path / "run"
    verifier = run_dir / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (verifier / "reward.json").write_text(
        json.dumps({"resolved": False, "verifier_pass": False, "validation_agent_pass": False, "score": 0})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["resolved"] is False
    assert summary["verifier_pass"] is False
    assert summary["validation_agent_pass"] is False
    assert summary["rubric_score"] == 0


def test_summary_prefers_reward_details_over_reward(tmp_path):
    run_dir = tmp_path / "run"
    verifier = run_dir / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (verifier / "reward.json").write_text(json.dumps({"reward": 0.0}))
    (verifier / "reward_details.json").write_text(
        json.dumps({"reward": 1.0, "rubric": {"status": "ok"}, "taste": {"status": "ok"}})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["harbor_reward"] == {"reward": 1.0, "rubric": {"status": "ok"}, "taste": {"status": "ok"}}


def test_summary_classifies_docker_daemon_failure_from_harbor_log(tmp_path):
    run_dir = tmp_path / "run"
    run_dir.mkdir()
    (run_dir / "harbor.log").write_text("Docker daemon is not running. Please start Docker and try again.\n")

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["failure_class"] == "docker_daemon_unavailable"


def test_summary_classifies_preflight_failure_artifact(tmp_path):
    run_dir = tmp_path / "run"
    run_dir.mkdir()
    (run_dir / "preflight-failure.json").write_text(
        json.dumps(
            {
                "failure_class": "remote_home_mount_invalid",
                "message": "ro home is not supported",
                "stage": "pre-harbor",
            }
        )
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["failure_class"] == "remote_home_mount_invalid"
    assert summary["preflight_failure"]["message"] == "ro home is not supported"


def test_summary_treats_successful_install_only_trial_as_no_failure(tmp_path):
    run_dir = tmp_path / "run"
    trial = run_dir / "harbor-output" / "trial"
    trial.mkdir(parents=True)
    (trial / "result.json").write_text(
        json.dumps({"config": {"install_only": True}, "exception_info": None})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["install_only_success"] is True
    assert summary["failure_class"] is None


def test_summary_normalizes_pi_harbor_usage_and_completion(tmp_path):
    run_dir = tmp_path / "run"
    trial = run_dir / "harbor-output" / "trial"
    verifier = trial / "verifier"
    verifier.mkdir(parents=True)
    (run_dir / "command.json").write_text(
        json.dumps({"bench_agent_label": "pi.dev", "bench_model": "zai/glm-5.2"})
    )
    (trial / "result.json").write_text(
        json.dumps(
            {
                "agent_info": {"name": "pi", "model_info": {"provider": "zai", "name": "glm-5.2"}},
                "agent_result": {
                    "n_input_tokens": 100,
                    "n_cache_tokens": 70,
                    "n_output_tokens": 20,
                    "cost_usd": None,
                },
                "exception_info": None,
            }
        )
    )
    (verifier / "reward_details.json").write_text(json.dumps({"reward": 1.0, "correctness": 1.0}))
    (verifier / "verifier_results.json").write_text(json.dumps({"passed": 1, "total": 1, "all_pass": True}))
    (verifier / "judge_output.json").write_text(json.dumps({"rubric_status": "ok", "taste_status": "ok"}))

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["completion"] == {
        "status": "complete_pass",
        "complete": True,
        "scoreable": True,
        "passed": True,
        "score": 1.0,
        "verifier_complete": True,
        "rubric_status": "ok",
        "taste_status": "ok",
        "judges_complete": True,
        "validation_required": False,
        "validation_complete": True,
        "exception_type": None,
    }
    assert summary["agent"]["harness"] == "pi.dev"
    assert summary["agent"]["provider"] == "zai"
    assert summary["agent"]["model"] == "glm-5.2"
    assert summary["agent"]["usage"] == {
        "scope": "agent",
        "available": True,
        "complete": True,
        "source": "harbor_agent_result",
        "input_tokens": 100,
        "cached_input_tokens": 70,
        "uncached_input_tokens": 30,
        "cache_write_tokens": None,
        "output_tokens": 20,
        "reasoning_tokens": None,
        "total_tokens": 120,
        "reported_cost_usd": None,
    }


def test_summary_prefers_pi_rollout_log_for_agent_telemetry_and_usage(tmp_path):
    run_dir = tmp_path / "run"
    agent_dir = run_dir / "harbor-output" / "trial" / "agent"
    agent_dir.mkdir(parents=True)
    events = [
        {"type": "turn_end"},
        {
            "type": "message_end",
            "message": {
                "role": "assistant",
                "content": [
                    {"type": "thinking", "thinking": "inspect"},
                    {"type": "toolCall", "id": "call-1", "name": "bash", "arguments": {"command": "ls"}},
                ],
                "usage": {
                    "input": 10,
                    "cacheRead": 90,
                    "output": 5,
                    "totalTokens": 105,
                    "cost": {"total": 0.01},
                },
            },
        },
        {"type": "tool_execution_end", "toolCallId": "call-1", "result": {"isError": False}},
    ]
    (agent_dir / "pi.txt").write_text("warning\n" + "\n".join(json.dumps(event) for event in events))

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["agent_trace"]["source"] == "pi_agent_log"
    assert summary["agent_trace"]["iterations"] == 1
    assert summary["agent_trace"]["tool_calls"] == 1
    assert summary["agent_trace"]["thinking_blocks"] == 1
    assert summary["agent"]["usage"]["source"] == "pi_agent_log"
    assert summary["agent"]["usage"]["input_tokens"] == 100
    assert summary["agent"]["usage"]["reasoning_tokens"] is None


def test_summary_aggregates_instrumented_judge_usage(tmp_path):
    run_dir = tmp_path / "run"
    verifier = run_dir / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (run_dir / "command.json").write_text(
        json.dumps({"vis_bench_verifier_usage_capture": "litellm_jsonl+validation_trajectories"})
    )
    events = [
        {
            "source": "litellm",
            "purpose": "rubric_judge",
            "model": "openai/glm-5.2",
            "usage": {
                "prompt_tokens": 100,
                "completion_tokens": 20,
                "total_tokens": 120,
                "prompt_tokens_details": {"cached_tokens": 10},
                "completion_tokens_details": {"reasoning_tokens": 5},
            },
            "reported_cost_usd": 0.01,
        },
        {
            "source": "litellm",
            "purpose": "taste_judge",
            "model": "openai/glm-5.2",
            "usage": {"prompt_tokens": 200, "completion_tokens": 30, "total_tokens": 230},
            "reported_cost_usd": 0.02,
        },
    ]
    (verifier / "llm_usage.jsonl").write_text("".join(json.dumps(event) + "\n" for event in events))

    summary = _run_metrics(tmp_path, run_dir)
    usage = summary["verifier_usage"]

    assert usage["complete"] is True
    assert usage["calls"] == 2
    assert usage["purposes"] == ["rubric_judge", "taste_judge"]
    assert usage["input_tokens"] == 300
    assert usage["cached_input_tokens"] == 10
    assert usage["uncached_input_tokens"] == 90
    assert usage["output_tokens"] == 50
    assert usage["reasoning_tokens"] == 5
    assert usage["total_tokens"] == 350
    assert usage["reported_cost_usd"] == 0.03
    assert usage["token_coverage"]["cached_input_tokens"] == 1


def test_design_verifier_usage_includes_validation_agent_trajectory(tmp_path):
    run_dir = tmp_path / "run"
    verifier = run_dir / "harbor-output" / "trial" / "verifier"
    miniswe = verifier / "miniswe"
    miniswe.mkdir(parents=True)
    (verifier / "llm_usage.jsonl").write_text(
        json.dumps(
            {
                "source": "litellm",
                "purpose": "rubric_judge",
                "model": "openai/glm-5.2",
                "usage": {"prompt_tokens": 100, "completion_tokens": 20, "total_tokens": 120},
            }
        )
        + "\n"
    )
    (miniswe / "01.trajectory.json").write_text(
        json.dumps(
            {
                "messages": [
                    {
                        "role": "assistant",
                        "extra": {
                            "cost": 0.0,
                            "response": {
                                "model": "glm-5.2",
                                "usage": {
                                    "prompt_tokens": 500,
                                    "completion_tokens": 50,
                                    "total_tokens": 550,
                                    "completion_tokens_details": {"reasoning_tokens": 25},
                                },
                            },
                        },
                    }
                ]
            }
        )
    )

    usage = metrics.verifier_usage(
        run_dir,
        {"vis_bench_verifier_usage_capture": "litellm_jsonl+validation_trajectories"},
        "design",
    )

    assert usage["complete"] is True
    assert usage["calls"] == 2
    assert usage["total_tokens"] == 670
    assert usage["components"]["judges"]["total_tokens"] == 120
    assert usage["components"]["validation_agent"]["total_tokens"] == 550


def test_summary_marks_harbor_exception_unscoreable(tmp_path):
    run_dir = tmp_path / "run"
    trial = run_dir / "harbor-output" / "trial"
    trial.mkdir(parents=True)
    (trial / "result.json").write_text(
        json.dumps({"exception_info": {"exception_type": "RuntimeError", "exception_message": "agent failed"}})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["completion"]["status"] == "unscoreable_error"
    assert summary["completion"]["complete"] is False
    assert summary["completion"]["scoreable"] is False


def test_summary_does_not_mark_correctness_authoritative_when_judge_failed(tmp_path):
    run_dir = tmp_path / "run"
    verifier = run_dir / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (verifier / "reward_details.json").write_text(json.dumps({"reward": 1.0, "correctness": 1.0}))
    (verifier / "verifier_results.json").write_text(json.dumps({"passed": 1, "total": 1, "all_pass": True}))
    (verifier / "judge_output.json").write_text(
        json.dumps({"rubric_status": "failed:no_tool_use", "taste_status": "ok"})
    )

    summary = _run_metrics(tmp_path, run_dir)

    assert summary["completion"]["scoreable"] is True
    assert summary["completion"]["complete"] is False
    assert summary["completion"]["status"] == "incomplete_judge"
    assert summary["failure_class"] == "incomplete_judge"


def test_design_validation_requires_successful_agent_status():
    completion = metrics.completion_summary(
        {"reward": 1.0, "correctness": 1.0},
        None,
        False,
        {"total": 1, "all_pass": True},
        {"rubric_status": "ok", "taste_status": "ok"},
        {},
        {"validation_score": 1.0},
        "design",
    )

    assert completion["complete"] is False
    assert completion["validation_complete"] is False
    assert completion["status"] == "incomplete_validation"


def test_agent_summary_preserves_provider_prefix_before_normalizing_model():
    summary = metrics.agent_summary(
        {
            "bench_agent_label": "pi.dev",
            "bench_model": "zai/glm-5.2",
            "bench_agent_route": "zai-coding-plan",
            "bench_agent_reasoning_effort": "max",
            "bench_agent_reasoning_effort_explicit": True,
            "bench_agent_pi_thinking_level": "xhigh",
            "bench_agent_output_cap": 32768,
            "bench_agent_pi_models_sha256": "a" * 64,
        },
        {},
        {},
        {},
    )

    assert summary["provider"] == "zai"
    assert summary["model"] == "glm-5.2"
    assert summary["route"] == "zai-coding-plan"
    assert summary["reasoning_effort"] == "max"
    assert summary["reasoning_effort_explicit"] is True
    assert summary["pi_thinking_level"] == "xhigh"
    assert summary["output_cap"] == 32768
    assert summary["pi_models_sha256"] == "a" * 64
