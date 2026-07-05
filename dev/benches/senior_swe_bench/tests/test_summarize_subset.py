from pathlib import Path
import importlib.util
import json
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("summarize_subset", ROOT / "summarize_subset.py")
summarize_subset = importlib.util.module_from_spec(spec)
sys.modules["summarize_subset"] = summarize_subset
spec.loader.exec_module(summarize_subset)  # type: ignore[union-attr]


def _run_dir(root: Path, name: str, *, task_id: str, failure_class=None, install_only_success=True):
    run = root / name
    (run / "harbor-output").mkdir(parents=True)
    (run / "command.json").write_text(
        json.dumps(
            {
                "run_id": name,
                "task_ids": [task_id],
                "task_base_image": f"{task_id}:latest",
                "selected_task_image": "",
            }
        )
    )
    (run / "summary.json").write_text(
        json.dumps(
            {
                "task_id": task_id,
                "failure_class": failure_class,
                "install_only_success": install_only_success,
                "patch_bloat": {"files": 0, "added_lines": 0, "deleted_lines": 0, "total_changed_lines": 0},
                "vis": {"iterations": 0},
            }
        )
    )
    (run / "harbor-output" / "collection.json").write_text(
        json.dumps({"job_dir": f"jobs/{name}", "trials": [{"trial_dir": "trial"}]})
    )
    return run


def test_summarize_subset_aggregates_successful_install_only_runs(tmp_path):
    run_a = _run_dir(tmp_path, "subset-1-task-a", task_id="task-a")
    run_b = _run_dir(tmp_path, "subset-1-task-b", task_id="task-b")

    summary = summarize_subset.summarize(
        subset_name="subset",
        subset_path="subsets/subset.json",
        run_dirs=[(run_a, 0), (run_b, 0)],
    )

    assert summary["total_tasks"] == 2
    assert summary["exit_status"] == 0
    assert summary["failure_counts"] == {}
    assert summary["all_install_only_success"] is True
    assert summary["tasks"][0]["harbor_job_dir"] == "jobs/subset-1-task-a"
    assert summary["tasks"][1]["collected_trials"] == 1


def test_summarize_subset_marks_missing_summary_and_nonzero_status(tmp_path):
    run = tmp_path / "subset-1-task-a"
    run.mkdir()
    (run / "command.json").write_text(json.dumps({"run_id": "subset-1-task-a", "task_ids": ["task-a"]}))

    summary = summarize_subset.summarize(
        subset_name="subset",
        run_dirs=[(run, 2)],
    )

    assert summary["exit_status"] == 2
    assert summary["failure_counts"] == {"missing_summary": 1}
    assert summary["tasks"][0]["failure_class"] == "missing_summary"


def test_summarize_subset_infers_zero_exit_for_historical_successes(tmp_path):
    run = _run_dir(tmp_path, "subset-1-task-a", task_id="task-a")

    summary = summarize_subset.summarize(
        subset_name="subset",
        run_dirs=[(run, None)],
    )

    assert summary["exit_status"] == 0
