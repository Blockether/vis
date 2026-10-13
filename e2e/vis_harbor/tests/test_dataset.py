"""Tests for the compiler from e2e scenarios to Harbor tasks."""

from __future__ import annotations

import json
import stat
from pathlib import Path

import pytest
from blockether.vis.evals import read_tasks
from harbor.models.task.task import Task

from vis_harbor import dataset


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


def test_tasks_match_the_scenario_files(compiled):
    directories = sorted(
        meta.parent for meta in dataset.SCENARIO_ROOT.glob("*/scenario.json")
    )
    assert sorted(path.name for path in compiled.iterdir()) == [
        directory.name for directory in directories
    ]
    for directory in directories:
        source = (directory / "scenario.json").read_bytes()
        sc = json.loads(source)
        task_dir = compiled / directory.name
        task = Task(task_dir)
        assert task.instruction == sc["prompt"]
        expected_timeout = sc.get("timeout_s", dataset.DEFAULT_AGENT_TIMEOUT_SEC)
        assert task.config.agent.timeout_sec == expected_timeout
        assert task.config.verifier.timeout_sec == dataset.VERIFIER_TIMEOUT_SEC
        assert task.config.metadata["scenario_id"] == directory.name
        assert task.config.metadata["lang"] == sc["lang"]
        assert (task_dir / "tests" / "scenario.json").read_bytes() == source
        fixtures = directory.parent / sc.get("files_from", directory.name) / "files"
        seeded = {
            name: content
            for name, (content, _) in _tree(fixtures).items()
            if not dataset.SKIPPED_DIRECTORIES.intersection(Path(name).parts)
        }
        workspace = {
            name: content
            for name, (content, _) in _tree(
                task_dir / "environment" / "workspace"
            ).items()
        }
        if sc.get("fixture_generator"):
            assert seeded.items() <= workspace.items(), directory.name
        else:
            assert workspace == seeded, directory.name
        registrations = sc.get("workspace_filesystem")
        registrations_file = (
            task_dir / "environment" / dataset.WORKSPACE_FILESYSTEM_FILE
        )
        if registrations:
            assert json.loads(registrations_file.read_text()) == registrations
        else:
            assert not registrations_file.exists()


def test_agent_timeout_override_reaches_the_task_and_the_grader(tmp_path):
    root = _scenario_root(tmp_path, {"short": {}, "long": {"timeout_s": 900}})
    out = tmp_path / "out"
    dataset.compile_dataset(out, root=root, agent_timeout_sec=720)
    for name in ("short", "long"):
        task = Task(out / name)
        assert task.config.agent.timeout_sec == 720
        source = json.loads((root / name / "scenario.json").read_text())
        graded = json.loads((out / name / "tests" / "scenario.json").read_text())
        assert graded == {**source, "timeout_s": 720}


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


def test_the_sdk_reads_every_scenario_as_a_harbor_case(compiled):
    # The SDK reads the compiled scenarios as standard Harbor tasks, without Harbor.
    scenarios = {
        meta.parent.name: json.loads(meta.read_text())
        for meta in dataset.SCENARIO_ROOT.glob("*/scenario.json")
    }
    tasks = {task.name: task for task in read_tasks(compiled)}
    assert sorted(tasks) == sorted(scenarios)
    for name, task in tasks.items():
        assert task.instruction == scenarios[name]["prompt"]
        assert task.agent_timeout_sec == Task(task.path).config.agent.timeout_sec
        assert task.verifier_timeout_sec == dataset.VERIFIER_TIMEOUT_SEC
        assert task.metadata["lang"] == scenarios[name]["lang"]
