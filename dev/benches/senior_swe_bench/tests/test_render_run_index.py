import importlib.util
import json
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("render_run_index", ROOT / "render_run_index.py")
renderer = importlib.util.module_from_spec(spec)
sys.modules["render_run_index"] = renderer
spec.loader.exec_module(renderer)  # type: ignore[union-attr]


def test_index_groups_comparable_runs_and_links_artifacts(tmp_path):
    results = tmp_path / "results"
    for name, harness, score in (("vis-run", "Vis", 1.0), ("pi-run", "pi.dev", 0.5)):
        run = results / name
        run.mkdir(parents=True)
        (run / "command.json").write_text(json.dumps({"task_ids": ["same-task"], "bench_agent_label": harness}))
        (run / "summary.json").write_text(json.dumps({"task_id": "same-task", "completion": {"status": "complete", "score": score, "passed": score == 1.0}}))

    output = renderer.render(results, results / "index.html")

    assert "2 runs across 1 task groups" in output
    assert "same-task" in output
    assert "Vis" in output and "pi.dev" in output
    assert "vis-run/summary.json" in output
