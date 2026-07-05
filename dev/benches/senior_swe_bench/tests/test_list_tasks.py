from pathlib import Path
import importlib.util
import json
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("list_tasks", ROOT / "list_tasks.py")
list_tasks = importlib.util.module_from_spec(spec)
sys.modules["list_tasks"] = list_tasks
spec.loader.exec_module(list_tasks)  # type: ignore[union-attr]


def test_load_subset_accepts_cwd_relative_path(tmp_path, monkeypatch):
    subset = tmp_path / "nested" / "subset.json"
    subset.parent.mkdir()
    subset.write_text(json.dumps({"task_ids": ["task-a", "task-b"]}))
    monkeypatch.chdir(tmp_path)

    assert list_tasks.load_subset("nested/subset.json") == {"task-a", "task-b"}


def test_load_subset_accepts_package_relative_path(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)

    assert list_tasks.load_subset("subsets/smoke.json") == {"paperless-ngx-perf-document-counts"}
