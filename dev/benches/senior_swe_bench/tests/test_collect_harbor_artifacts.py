from pathlib import Path
import importlib.util
import json

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("collect_harbor_artifacts", ROOT / "collect_harbor_artifacts.py")
collect_harbor_artifacts = importlib.util.module_from_spec(spec)
spec.loader.exec_module(collect_harbor_artifacts)  # type: ignore[union-attr]


def test_collect_copies_matching_trial_verifier_reward_and_trace(tmp_path):
    task_id = "paperless-ngx-perf-document-counts"
    job = tmp_path / "jobs" / "2026-07-04__15-31-30"
    trial = job / "paperless-ngx-perf-document-coun__abc123"
    verifier = trial / "verifier"
    agent = trial / "agent"
    trace_dir = trial / "artifacts" / "vis-traces" / task_id
    run = tmp_path / "results" / "run-1"

    trial.mkdir(parents=True)
    verifier.mkdir()
    agent.mkdir()
    trace_dir.mkdir(parents=True)
    (job / "result.json").write_text('{"stats": {"n_errored_trials": 0}}')
    (job / "job.log").write_text("job log")
    (trial / "result.json").write_text(
        json.dumps(
            {
                "task_name": task_id,
                "task_id": {"path": f"/dataset/tasks/{task_id}"},
            }
        )
    )
    (trial / "trial.log").write_text("trial log")
    (verifier / "reward.json").write_text('{"reward": 1}')
    (verifier / "test-stdout.txt").write_text("pytest output")
    (agent / "pi.txt").write_text('{"type":"turn_end"}\n')
    (trace_dir / "vis.trace.jsonl").write_text('{"event":"trace-chunk"}\n')

    summary = collect_harbor_artifacts.collect(job, run, task_id)

    assert summary["job_files"] == ["result.json", "job.log"]
    assert summary["trials"][0]["verifier"] is True
    assert summary["trials"][0]["agent"] is True
    assert (run / "harbor-output" / "trial" / "verifier" / "reward.json").read_text() == '{"reward": 1}'
    assert (run / "harbor-output" / "trial" / "trial.log").read_text() == "trial log"
    assert (run / "harbor-output" / "trial" / "agent" / "pi.txt").read_text() == '{"type":"turn_end"}\n'
    assert (run / "vis-traces" / task_id / "vis.trace.jsonl").read_text() == '{"event":"trace-chunk"}\n'
    assert summary["reward_files"] == [str(run / "harbor-output" / "trial" / "verifier" / "reward.json")]
    assert (run / "harbor-output" / "collection.json").exists()


def test_collect_matches_task_by_task_path_name(tmp_path):
    task_id = "task-a"
    job = tmp_path / "jobs" / "job"
    good = job / "trial-good"
    bad = job / "trial-bad"
    run = tmp_path / "results" / "run"
    good.mkdir(parents=True)
    bad.mkdir()
    (good / "result.json").write_text(json.dumps({"task_id": {"path": f"/x/{task_id}"}}))
    (bad / "result.json").write_text(json.dumps({"task_id": {"path": "/x/task-b"}}))

    summary = collect_harbor_artifacts.collect(job, run, task_id)

    assert len(summary["trials"]) == 1
    assert summary["trials"][0]["trial_dir"].endswith("trial-good")
