from pathlib import Path
import importlib.util
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("make_subset", ROOT / "make_subset.py")
make_subset = importlib.util.module_from_spec(spec)
sys.modules["make_subset"] = make_subset
spec.loader.exec_module(make_subset)  # type: ignore[union-attr]


def _task(task_id, segment, task_type, repo):
    return {
        "task_id": task_id,
        "segment": segment,
        "task_type": task_type,
        "repo": repo,
        "visibility": "public",
        "verifier_timeout_sec": 600,
    }


TASKS = [
    _task("design-a-1", "design", "feature", "repo-a"),
    _task("design-a-2", "design", "feature", "repo-a"),
    _task("design-b-1", "design", "feature", "repo-b"),
    _task("design-c-1", "design", "feature", "repo-c"),
    _task("investigate-a-1", "investigate", "bug", "repo-a"),
    _task("investigate-b-1", "investigate", "performance", "repo-b"),
    _task("investigate-c-1", "investigate", "bug", "repo-c"),
    _task("investigate-c-2", "investigate", "bug", "repo-c"),
]


def test_overnight_selection_balances_segments_and_repos():
    subset = make_subset.build_subset(
        mode="overnight",
        tasks=TASKS,
        dataset={"dataset_repo": "repo", "dataset_commit": "commit"},
        max_tasks=4,
        seed="test-seed",
    )

    selected = subset["tasks"]
    assert len(selected) == 4
    assert {task["segment"] for task in selected} == {"design", "investigate"}
    assert len({task["repo"] for task in selected}) >= 3


def test_full_selection_keeps_all_tasks_sorted_by_id():
    subset = make_subset.build_subset(
        mode="full",
        tasks=list(reversed(TASKS)),
        dataset={"dataset_repo": "repo", "dataset_commit": "commit"},
    )

    assert subset["task_ids"] == sorted(task["task_id"] for task in TASKS)


def test_filters_apply_before_selection():
    subset = make_subset.build_subset(
        mode="full",
        tasks=TASKS,
        dataset={"dataset_repo": "repo", "dataset_commit": "commit"},
        repos=["repo-c"],
        exclude_tasks=["investigate-c-2"],
    )

    assert subset["task_ids"] == ["design-c-1", "investigate-c-1"]
