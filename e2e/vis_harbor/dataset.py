"""Compile the Vis e2e scenarios into Harbor tasks.

Each ``e2e/scenarios/<id>/scenario.json`` becomes one Harbor task directory::

    <id>/task.toml
    <id>/instruction.md
    <id>/environment/workspace/...
    <id>/environment/workspace_filesystem.json  (only if the scenario registers roots)
    <id>/tests/scenario.json
    <id>/tests/test.sh

Run it with ``uv run --project e2e python -m vis_harbor.dataset --out DIR``.
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
from collections.abc import Mapping, Sequence
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Any

from harbor.models.task.config import AgentConfig, TaskConfig, VerifierConfig

E2E_ROOT = Path(__file__).resolve().parents[1]
SCENARIO_ROOT = E2E_ROOT / "scenarios"
# Harbor records the dataset folder name as the `source` of each trial.
DATASET_NAME = "vis-e2e"
DEFAULT_AGENT_TIMEOUT_SEC = 300
VERIFIER_TIMEOUT_SEC = 300
GENERATOR_TIMEOUT_SEC = 30
WORKSPACE_FILESYSTEM_FILE = "workspace_filesystem.json"
SKIPPED_DIRECTORIES = frozenset({"__pycache__"})
TEST_SCRIPT = """#!/usr/bin/env bash
# Grade one Vis e2e scenario. The host environment sets the VIS_E2E_* paths.
set -euo pipefail
exec "$VIS_E2E_PYTHON" -m vis_harbor.grade \\
  --scenario "$VIS_E2E_TESTS/scenario.json" \\
  --scenario-id {scenario_id} \\
  --workspace "$VIS_E2E_WORKSPACE" \\
  --agent-logs "$VIS_E2E_AGENT_LOGS" \\
  --verifier-logs "$VIS_E2E_VERIFIER_LOGS"
"""
_SCENARIO_ID = re.compile(r"[a-z0-9-]+")
_GENERATOR = re.compile(r"[a-zA-Z0-9_-]+\.py")


@dataclass(frozen=True, slots=True)
class Scenario:
    """One e2e scenario: its id, its directory and its scenario.json."""

    scenario_id: str
    directory: Path
    source: bytes
    data: Mapping[str, Any]

    @property
    def prompt(self) -> str:
        """The exact prompt that the agent gets."""
        return self.data["prompt"]

    @property
    def agent_timeout_sec(self) -> float:
        """The agent timeout: ``timeout_s``, else the e2e default."""
        return float(self.data.get("timeout_s", DEFAULT_AGENT_TIMEOUT_SEC))


def load_scenarios(
    scenario_ids: Sequence[str] | None = None, *, root: Path = SCENARIO_ROOT
) -> list[Scenario]:
    """Load the scenarios under ``root`` in name order, or only ``scenario_ids``."""
    wanted = set(scenario_ids or ())
    scenarios = []
    for directory in sorted(root.iterdir()):
        meta = directory / "scenario.json"
        if not meta.is_file() or (wanted and directory.name not in wanted):
            continue
        source = meta.read_bytes()
        scenarios.append(
            Scenario(directory.name, directory, source, json.loads(source))
        )
    missing = wanted - {scenario.scenario_id for scenario in scenarios}
    if missing:
        raise ValueError(f"unknown scenario: {', '.join(sorted(missing))}")
    return scenarios


def with_agent_timeout(scenario: Scenario, timeout_sec: int) -> Scenario:
    """Return ``scenario`` with the agent timeout ``timeout_sec`` in its data.

    The task copy of ``scenario.json`` gets the same value, so the grade reports the
    timeout that the agent had.
    """
    data = {**scenario.data, "timeout_s": timeout_sec}
    source = (json.dumps(data, indent=2) + "\n").encode()
    return replace(scenario, source=source, data=data)


def fixture_directory(scenario: Scenario) -> Path:
    """Return the scenario directory that holds the seed files and the generator."""
    source = scenario.data.get("files_from")
    if not source:
        return scenario.directory
    if not isinstance(source, str) or not _SCENARIO_ID.fullmatch(source):
        raise ValueError("files_from must name a sibling scenario")
    directory = scenario.directory.parent / source
    if not (directory / "scenario.json").is_file():
        raise ValueError(f"files_from scenario does not exist: {source}")
    return directory


def seed_workspace(scenario: Scenario, workspace: Path) -> None:
    """Copy the seed files into ``workspace``, then run the fixture generator."""
    fixtures = fixture_directory(scenario)
    files = fixtures / "files"
    if files.is_dir():
        for path in sorted(files.rglob("*")):
            relative = path.relative_to(files)
            if path.is_dir() or SKIPPED_DIRECTORIES.intersection(relative.parts):
                continue
            target = workspace / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(path, target)
    generator = scenario.data.get("fixture_generator")
    if generator:
        if not isinstance(generator, str) or not _GENERATOR.fullmatch(generator):
            raise ValueError("fixture_generator must be a script basename")
        subprocess.run(
            [sys.executable, str(fixtures / generator), str(workspace)],
            cwd=workspace,
            check=True,
            capture_output=True,
            text=True,
            timeout=GENERATOR_TIMEOUT_SEC,
        )


def task_config(scenario: Scenario) -> TaskConfig:
    """Return the Harbor task configuration of one scenario."""
    return TaskConfig(
        metadata={
            "scenario_id": scenario.scenario_id,
            "lang": scenario.data["lang"],
            "source": f"e2e/scenarios/{scenario.scenario_id}",
        },
        agent=AgentConfig(timeout_sec=scenario.agent_timeout_sec),
        verifier=VerifierConfig(timeout_sec=float(VERIFIER_TIMEOUT_SEC)),
    )


def compile_task(scenario: Scenario, out: Path) -> Path:
    """Write the Harbor task of one scenario into ``out`` and return its directory."""
    task_dir = out / scenario.scenario_id
    if task_dir.exists():
        shutil.rmtree(task_dir)
    environment = task_dir / "environment"
    tests = task_dir / "tests"
    (environment / "workspace").mkdir(parents=True)
    tests.mkdir()
    (task_dir / "task.toml").write_text(task_config(scenario).model_dump_toml())
    (task_dir / "instruction.md").write_text(scenario.prompt)
    seed_workspace(scenario, environment / "workspace")
    if registrations := scenario.data.get("workspace_filesystem"):
        text = json.dumps(registrations, indent=2) + "\n"
        (environment / WORKSPACE_FILESYSTEM_FILE).write_text(text)
    (tests / "scenario.json").write_bytes(scenario.source)
    script = tests / "test.sh"
    script.write_text(TEST_SCRIPT.format(scenario_id=scenario.scenario_id))
    script.chmod(0o755)
    return task_dir


def compile_dataset(
    out: Path,
    scenario_ids: Sequence[str] | None = None,
    *,
    root: Path = SCENARIO_ROOT,
    agent_timeout_sec: int | None = None,
) -> list[Path]:
    """Compile the scenarios into task directories under ``out``.

    ``agent_timeout_sec`` replaces the agent timeout of each scenario.
    """
    out.mkdir(parents=True, exist_ok=True)
    scenarios = load_scenarios(scenario_ids, root=root)
    if agent_timeout_sec is not None:
        scenarios = [
            with_agent_timeout(scenario, agent_timeout_sec) for scenario in scenarios
        ]
    return [compile_task(scenario, out) for scenario in scenarios]


def main(argv: Sequence[str] | None = None) -> int:
    """Run the compiler from the command line."""
    parser = argparse.ArgumentParser(
        prog="python -m vis_harbor.dataset",
        description="Compile the Vis e2e scenarios into Harbor tasks.",
    )
    parser.add_argument(
        "--out", required=True, type=Path, help="The dataset directory."
    )
    parser.add_argument(
        "--scenario",
        action="append",
        dest="scenario_ids",
        metavar="ID",
        help="Compile only this scenario. Repeat the option for more scenarios.",
    )
    args = parser.parse_args(argv)
    tasks = compile_dataset(args.out, args.scenario_ids)
    print(f"Compiled {len(tasks)} tasks into {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
