from pathlib import Path
import importlib.util
import json
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("render_report", ROOT / "render_report.py")
render_report = importlib.util.module_from_spec(spec)
sys.modules["render_report"] = render_report
spec.loader.exec_module(render_report)  # type: ignore[union-attr]


def test_report_renders_side_by_side_solarized_summary(tmp_path):
    run = tmp_path / "run"
    verifier = run / "harbor-output" / "trial" / "verifier"
    verifier.mkdir(parents=True)
    (run / "command.json").write_text(
        json.dumps(
            {
                "run_id": "run-1",
                "task_ids": ["paperless-ngx-perf-document-counts"],
                "vis_provider": "zai-coding-plan",
                "vis_model": "glm-5.1",
                "vis_bench_config_delivery": "upload",
                "vis_bench_verifier_provider": "lmstudio",
                "vis_bench_verifier_openai_base_url": "http://host.docker.internal:1234/v1",
                "vis_bench_verifier_judge_model": "openai/local-model",
                "vis_bench_verifier_classifier_model": "openai/local-model",
                "selected_task_image": "paperless:python-dev",
            }
        )
    )
    (run / "summary.json").write_text(
        json.dumps(
            {
                "task_id": "paperless-ngx-perf-document-counts",
                "failure_class": "verifier_failed",
                "verifier_pass": False,
                "verifier": {"passed": 0, "total": 1, "all_pass": False},
                "harbor_reward": {"reward": 0.0, "correctness": 0.0, "verifier_score": 0.0},
                "patch_bloat": {"files": 2, "added_lines": 10, "deleted_lines": 2, "total_changed_lines": 12},
                "vis": {"iterations": 45, "tokens": {}, "cost_usd": None},
            }
        )
    )
    (run / "harbor-output" / "job-result.json").write_text(
        json.dumps(
            {
                "started_at": "2026-07-04T17:25:54.492828",
                "finished_at": "2026-07-04T17:41:51.533733",
                "n_total_trials": 1,
                "stats": {
                    "evals": {
                        "vis-installed__glm-5.1__tasks": {
                            "metrics": [{"reward": 0.0, "correctness": 0.0, "verifier_score": 0.0}]
                        }
                    },
                    "n_input_tokens": None,
                    "n_cache_tokens": None,
                    "n_output_tokens": None,
                    "cost_usd": None,
                },
            }
        )
    )
    (run / "harbor-output" / "trial" / "result.json").write_text(
        json.dumps(
            {
                "started_at": "2026-07-04T15:25:54.542515Z",
                "finished_at": "2026-07-04T15:41:51.522558Z",
                "agent_execution": {
                    "started_at": "2026-07-04T15:26:03.874127Z",
                    "finished_at": "2026-07-04T15:40:09.149312Z",
                },
                "agent_info": {"name": "vis-installed", "model_info": {"name": "glm-5.1", "provider": None}},
            }
        )
    )
    (verifier / "reward_details.json").write_text(
        json.dumps(
            {
                "reward": 0.0,
                "correctness": 0.0,
                "verifier": {"passed": 0, "total": 1, "all_pass": False},
                "rubric": {"status": "skipped:no_api_key"},
                "taste": {
                    "patch_bloat": {
                        "agent_sloc": 55,
                        "agent_files": 2,
                        "agent_hunks": 2,
                        "oracle_sloc": 155,
                        "oracle_files": 3,
                        "oracle_hunks": 13,
                        "bloat_ratio": 0.355,
                    }
                },
            }
        )
    )
    (verifier / "runner_shell.log").write_text(
        "FAILED ../../tests/verify/verify_tests.py::test_tags_endpoint_meets_perf_budget_for_non_superuser\n"
        "E   AssertionError: GET /api/tags/ median latency 2.087s exceeds budget 1.5s.\n"
        "============= 1 failed, 14 passed, 15 warnings in 75.16s (0:01:15) =============\n"
    )
    (verifier / "agent.patch").write_text("diff --git a/a b/a\n@@\n-old\n+new\n")

    out = tmp_path / "report.html"
    old_argv = sys.argv
    try:
        sys.argv = [
            "render_report.py",
            "--vis-run",
            str(run),
            "--out",
            str(out),
            "--fair-provider",
            "z.ai plan",
            "--fair-model",
            "glm-turbo",
        ]
        assert render_report.main() == 0
    finally:
        sys.argv = old_argv

    html = out.read_text()
    assert "#fdf6e3" in html
    assert "Vis vs pi.dev Senior SWE-Bench Report" in html
    assert "Task pass rate" in html
    assert "0 / 1 (0.0%)" in html
    assert "14 / 15 (93.3%)" in html
    assert "pi.dev" in html
    assert "pending-data" in html
    assert "z.ai plan" in html
    assert "glm-turbo" in html
    assert "lmstudio" in html
    assert "http://host.docker.internal:1234/v1" in html
    assert "openai/local-model" in html
    assert "2 files, +10 / -2 (12 changed)" in html
