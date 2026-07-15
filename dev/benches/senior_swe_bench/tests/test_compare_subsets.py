from pathlib import Path
import importlib.util
import json
import sys


ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("compare_subsets", ROOT / "compare_subsets.py")
compare_subsets = importlib.util.module_from_spec(spec)
sys.modules["compare_subsets"] = compare_subsets
spec.loader.exec_module(compare_subsets)  # type: ignore[union-attr]


def _ledger(
    path: Path,
    *,
    harness: str,
    provider: str,
    total_tokens: int,
    endpoint: str | None = None,
    source_dirty: bool = False,
) -> Path:
    is_pi = harness == "pi.dev"
    endpoint = endpoint or (
        "https://api.z.ai/api/coding/paas/v4"
        if is_pi
        else "https://api.z.ai/api/anthropic/v1"
    )
    artifact = {
        "native_sha256": "abc123",
        "vis_revision": "def456",
        "source_dirty": source_dirty,
        "source_status_count": 0 if not source_dirty else 1,
        "native_elf": {"machine": "aarch64"},
    }
    agent = {
        "name": harness.lower(),
        "harness": harness,
        "version": "0.73.1" if is_pi else "vis 1.0",
        "requested_version": "0.73.1" if is_pi else None,
        "version_matches_requested": True if is_pi else None,
        "provider": provider,
        "model": "glm-5.2",
        "reasoning_effort": "high",
        "reasoning_effort_explicit": True,
        "pi_thinking_level": "high" if is_pi else None,
        "output_cap": 32768,
        "vis_eval_valid": None if is_pi else True,
        "pi_models_sha256": compare_subsets.PI_GLM52_MODELS_SHA256 if is_pi else None,
        "endpoint": endpoint,
        "route": "zai-coding-plan",
        "artifact": None if is_pi else artifact,
    }
    verifier = {"provider": "zai", "judge_model": "openai/glm-5.2"}
    data = {
        "subset_name": "overnight-2",
        "total_tasks": 1,
        "comparison_ready": True,
        "spend_reporting_complete": True,
        "tool_telemetry_complete": True,
        "agent_telemetry": {
            "tasks_with_trace": 1,
            "iterations": 10,
            "tool_calls": 20,
            "shell_calls": 18,
            "file_reads": 12,
            "sources": ["vis_trace" if not is_pi else "pi_agent_log"],
        },
        "secret_redaction_complete": True,
        "completion": {
            "authoritative": True,
            "expected_tasks": 1,
            "scoreable_tasks": 1,
            "completed_tasks": 1,
            "passed_tasks": 1,
            "pass_rate": 1.0,
        },
        "usage": {
            "complete": True,
            "tasks_with_complete_usage": 1,
            "tasks_with_reported_cost": 0,
            "token_coverage": {
                "input_tokens": 1,
                "cached_input_tokens": 1,
                "uncached_input_tokens": 1,
                "output_tokens": 1,
                "total_tokens": 1,
            },
            "tokens": {
                "input_tokens": total_tokens - 10,
                "cached_input_tokens": total_tokens - 30,
                "uncached_input_tokens": 20,
                "output_tokens": 10,
                "total_tokens": total_tokens,
            },
            "reported_cost_usd": None,
        },
        "agent_usage": {
            "complete": True,
            "tasks_with_complete_usage": 1,
            "tasks_with_reported_cost": 0,
            "token_coverage": {
                "input_tokens": 1,
                "cached_input_tokens": 1,
                "uncached_input_tokens": 1,
                "output_tokens": 1,
                "total_tokens": 1,
            },
            "tokens": {
                "input_tokens": total_tokens - 10,
                "cached_input_tokens": total_tokens - 30,
                "uncached_input_tokens": 20,
                "output_tokens": 10,
                "total_tokens": total_tokens,
            },
            "reported_cost_usd": None,
        },
        "verifier_usage": {
            "complete": True,
            "tasks_with_complete_usage": 1,
            "tasks_with_reported_cost": 1,
            "token_coverage": {"input_tokens": 1, "output_tokens": 1, "total_tokens": 1},
            "tokens": {"input_tokens": 30, "output_tokens": 10, "total_tokens": 40},
            "reported_cost_usd": 0.0,
        },
        "provenance": {
            "dataset_commits": ["abc123"],
            "agents": [agent],
            "verifiers": [verifier],
            "artifacts": [] if is_pi else [artifact],
            "harbor_versions": ["0.17.0"],
            "harbor_locked_versions": ["0.17.0"],
            "consistent": True,
        },
        "tasks": [
            {
                "task_id": "task-a",
                "completion": {"status": "complete_pass", "complete": True, "scoreable": True, "passed": True},
                "usage": {"available": True, "complete": True, "total_tokens": total_tokens},
                "verifier_usage": {"available": True, "complete": True, "total_tokens": 40},
                "agent_trace": {
                    "available": True,
                    "telemetry_complete": True,
                    "iterations": 10,
                    "tool_calls": 20,
                },
                "failure_class": None,
                "agent": agent,
                "run_dir": str(path.parent / harness),
                "task_checksum": "checksum-a",
                "task_base_image": "task-a:latest",
                "selected_task_image": "",
            }
        ],
    }
    path.write_text(json.dumps(data))
    return path


def test_comparison_gates_authoritative_state_and_token_ratios(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=150)

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"] == {
        "authoritative": True,
        "data_complete": True,
        "comparison_ready": True,
        "spend_reporting_complete": True,
        "tool_telemetry_complete": True,
        "secret_redaction_complete": True,
    }
    assert report["aggregate_usage"]["total_tokens"]["delta_pi_minus_vis"] == 50
    assert report["aggregate_usage"]["total_tokens"]["ratio_pi_over_vis"] == 1.5
    assert "Evidence gates" in compare_subsets.render_markdown(report)
    assert "Agent token spend" in compare_subsets.render_html(report)
    assert "Verifier token overhead" in compare_subsets.render_html(report)


def test_comparison_accepts_zai_coding_plan_transport_pair(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai-coding-plan", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["authoritative"] is True
    assert report["gates"]["provider_route_match"]["passed"] is True


def test_comparison_rejects_provider_endpoint_mismatch(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai-coding-plan", total_tokens=100)
    pi = _ledger(
        tmp_path / "pi.json",
        harness="pi.dev",
        provider="zai",
        total_tokens=100,
        endpoint="https://api.z.ai/api/paas/v4",
    )

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["data_complete"] is True
    assert report["state"]["authoritative"] is False
    assert report["gates"]["provider_route_match"]["passed"] is False


def test_comparison_rejects_dirty_vis_artifact(tmp_path):
    vis = _ledger(
        tmp_path / "vis.json",
        harness="Vis",
        provider="zai-coding-plan",
        total_tokens=100,
        source_dirty=True,
    )
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["authoritative"] is False
    assert report["gates"]["vis_artifact_clean"]["passed"] is False


def test_comparison_rejects_missing_verifier_spend(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)
    data = json.loads(pi.read_text())
    data["spend_reporting_complete"] = False
    data["verifier_usage"] = {"complete": False}
    pi.write_text(json.dumps(data))

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["data_complete"] is True
    assert report["state"]["authoritative"] is False
    assert report["gates"]["token_spend_complete"]["passed"] is False


def test_comparison_ready_requires_rollout_telemetry(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)
    data = json.loads(pi.read_text())
    data["tool_telemetry_complete"] = False
    data["tasks"][0]["agent_trace"] = {"available": False, "telemetry_complete": False}
    pi.write_text(json.dumps(data))

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["comparison_ready"] is False
    assert report["state"]["authoritative"] is False
    assert report["gates"]["tool_telemetry_complete"]["passed"] is False


def test_comparison_rejects_missing_secret_redaction_evidence(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)
    data = json.loads(pi.read_text())
    data["secret_redaction_complete"] = False
    pi.write_text(json.dumps(data))

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["authoritative"] is False
    assert report["gates"]["secret_redaction_complete"]["passed"] is False


def test_comparison_rejects_mismatched_provider_native_effort(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)
    data = json.loads(pi.read_text())
    data["provenance"]["agents"][0]["reasoning_effort"] = "max"
    data["provenance"]["agents"][0]["pi_thinking_level"] = "xhigh"
    data["tasks"][0]["agent"]["reasoning_effort"] = "max"
    data["tasks"][0]["agent"]["pi_thinking_level"] = "xhigh"
    pi.write_text(json.dumps(data))

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["authoritative"] is False
    assert report["gates"]["reasoning_control_present"]["passed"] is True
    assert report["gates"]["reasoning_effort_match"]["passed"] is False


def test_comparison_rejects_wrong_or_missing_output_cap(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)
    data = json.loads(pi.read_text())
    data["provenance"]["agents"][0]["output_cap"] = None
    data["tasks"][0]["agent"]["output_cap"] = None
    pi.write_text(json.dumps(data))

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["authoritative"] is False
    assert report["gates"]["output_cap_match"]["passed"] is False


def test_comparison_rejects_invalid_vis_eval_evidence(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)
    data = json.loads(vis.read_text())
    data["provenance"]["agents"][0]["vis_eval_valid"] = False
    data["tasks"][0]["agent"]["vis_eval_valid"] = False
    vis.write_text(json.dumps(data))

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["authoritative"] is False
    assert report["gates"]["vis_eval_valid"]["passed"] is False


def test_comparison_rejects_missing_reasoning_control(tmp_path):
    vis = _ledger(tmp_path / "vis.json", harness="Vis", provider="zai", total_tokens=100)
    pi = _ledger(tmp_path / "pi.json", harness="pi.dev", provider="zai", total_tokens=100)
    data = json.loads(pi.read_text())
    data["provenance"]["agents"][0]["pi_models_sha256"] = None
    data["tasks"][0]["agent"]["pi_models_sha256"] = None
    pi.write_text(json.dumps(data))

    report = compare_subsets.build_comparison(vis, pi)

    assert report["state"]["authoritative"] is False
    assert report["gates"]["reasoning_control_present"]["passed"] is False
