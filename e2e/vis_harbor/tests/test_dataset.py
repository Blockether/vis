"""Tests for the compiler from e2e scenarios to Harbor tasks."""

from __future__ import annotations

import importlib.util
import json
import stat
from pathlib import Path

import pytest
from harbor.models.task.task import Task

from vis_harbor import dataset


def _run_module():
    spec = importlib.util.spec_from_file_location(
        "vis_e2e_run", dataset.E2E_ROOT / "run.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _tree(root: Path) -> dict[str, tuple[bytes, bool]]:
    return {
        str(path.relative_to(root)): (
            path.read_bytes(),
            bool(path.stat().st_mode & stat.S_IXUSR),
        )
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def _scenario_root(tmp_path: Path, scenarios: dict[str, dict]) -> Path:
    root = tmp_path / "scenarios"
    for name, data in scenarios.items():
        (root / name).mkdir(parents=True)
        document = {"lang": "python", "prompt": f"Prompt of {name}.", **data}
        (root / name / "scenario.json").write_text(json.dumps(document))
    return root


@pytest.fixture(scope="module")
def compiled(tmp_path_factory) -> Path:
    out = tmp_path_factory.mktemp("dataset")
    dataset.compile_dataset(out)
    return out


def test_tasks_match_the_e2e_runner(compiled, tmp_path):
    run = _run_module()
    scenarios = run.load_scenarios(None)
    assert sorted(path.name for path in compiled.iterdir()) == [
        sc["id"] for sc in scenarios
    ]
    for sc in scenarios:
        task_dir = compiled / sc["id"]
        task = Task(task_dir)
        assert task.instruction == sc["prompt"]
        expected_timeout = sc.get("timeout_s", dataset.DEFAULT_AGENT_TIMEOUT_SEC)
        assert task.config.agent.timeout_sec == expected_timeout
        assert task.config.verifier.timeout_sec == dataset.VERIFIER_TIMEOUT_SEC
        assert task.config.metadata["scenario_id"] == sc["id"]
        assert task.config.metadata["lang"] == sc["lang"]
        source = Path(sc["_dir"], "scenario.json").read_bytes()
        assert (task_dir / "tests" / "scenario.json").read_bytes() == source
        work = tmp_path / sc["id"]
        work.mkdir()
        run.seed_files(sc, str(work))
        registrations = sc.get("workspace_filesystem")
        expected = {
            name: content
            for name, content in _tree(work).items()
            if "__pycache__" not in Path(name).parts
            and not (registrations and name == "vis.yml")
        }
        assert _tree(task_dir / "environment" / "workspace") == expected, sc["id"]
        registrations_file = (
            task_dir / "environment" / dataset.WORKSPACE_FILESYSTEM_FILE
        )
        if registrations:
            assert json.loads(registrations_file.read_text()) == registrations
        else:
            assert not registrations_file.exists()


def test_test_script_is_exact_and_executable(compiled):
    for task_dir in compiled.iterdir():
        script = task_dir / "tests" / "test.sh"
        assert script.read_text() == dataset.TEST_SCRIPT.format(
            scenario_id=task_dir.name
        )
        assert script.stat().st_mode & stat.S_IXUSR


def test_output_is_deterministic(compiled, tmp_path):
    again = tmp_path / "again"
    dataset.compile_dataset(again)
    stale = next(again.iterdir()) / "environment" / "workspace" / "stale.txt"
    stale.write_text("stale")
    dataset.compile_dataset(again)
    assert _tree(again) == _tree(compiled)


def test_cli_compiles_the_chosen_scenarios(tmp_path, capsys):
    first, second = [scenario.scenario_id for scenario in dataset.load_scenarios()][:2]
    out = tmp_path / "out"
    assert (
        dataset.main(["--out", str(out), "--scenario", first, "--scenario", second])
        == 0
    )
    assert sorted(path.name for path in out.iterdir()) == [first, second]
    assert "Compiled 2 tasks" in capsys.readouterr().out


def test_unknown_scenario_is_an_error(tmp_path):
    with pytest.raises(ValueError, match="unknown scenario: no-such-scenario"):
        dataset.compile_dataset(tmp_path, ["no-such-scenario"])


@pytest.mark.parametrize(
    ("data", "message"),
    [
        ({"files_from": "../outside"}, "files_from must name a sibling scenario"),
        ({"files_from": "absent"}, "files_from scenario does not exist: absent"),
        (
            {"fixture_generator": "../generate.py"},
            "fixture_generator must be a script basename",
        ),
    ],
)
def test_invalid_seed_keys_are_errors(tmp_path, data, message):
    root = _scenario_root(tmp_path, {"broken": data})
    with pytest.raises(ValueError, match=message):
        dataset.compile_dataset(tmp_path / "out", root=root)


def test_seed_files_skip_python_caches(tmp_path):
    root = _scenario_root(tmp_path, {"cached": {}})
    files = root / "cached" / "files" / "pkg"
    (files / "__pycache__").mkdir(parents=True)
    (files / "__pycache__" / "module.cpython-313.pyc").write_bytes(b"cache")
    (files / "module.py").write_text("VALUE = 1\n")
    (task_dir,) = dataset.compile_dataset(tmp_path / "out", root=root)
    workspace = task_dir / "environment" / "workspace"
    assert sorted(_tree(workspace)) == ["pkg/module.py"]
