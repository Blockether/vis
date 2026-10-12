"""Tests for the task check with the Vis agent in the host environment."""

from __future__ import annotations

import asyncio
import json
from pathlib import Path

import pytest
from harbor.analyze.models import build_check_response_model, load_rubric
from harbor.models.task.task import Task
from harbor.models.trial.paths import TrialPaths

from vis_harbor import check, dataset
from vis_harbor.environment import HostEnvironment
from vis_harbor.run import AGENT, ENVIRONMENT


@pytest.fixture(scope="module")
def wrapper(tmp_path_factory) -> Path:
    scenario_id = sorted(
        meta.parent.name for meta in dataset.SCENARIO_ROOT.glob("*/scenario.json")
    )[0]
    tasks = tmp_path_factory.mktemp("tasks")
    dataset.compile_dataset(tasks, [scenario_id])
    rubric = load_rubric(None)
    return check.assemble(
        tasks / scenario_id,
        rubric,
        build_check_response_model(rubric),
        tmp_path_factory.mktemp("wrappers") / "check-task",
    )


def _environment(wrapper: Path, tmp_path: Path) -> HostEnvironment:
    trial_paths = TrialPaths(tmp_path / "trial")
    trial_paths.mkdir()
    return HostEnvironment(
        environment_dir=wrapper / "environment",
        environment_name="check-task",
        session_id="check-task__trial",
        trial_paths=trial_paths,
        task_env_config=Task(wrapper).config.environment,
    )


def _verify(wrapper: Path, tmp_path: Path, result: dict | None) -> tuple[str, Path]:
    environment = _environment(wrapper, tmp_path)

    async def scenario() -> tuple[str, Path]:
        await environment.start(force_build=False)
        try:
            if result is not None:
                (environment.workspace / check.checker.RESULT_FILENAME).write_text(
                    json.dumps(result)
                )
            await environment.upload_dir(wrapper / "tests", "/tests")
            await environment.exec("bash /tests/test.sh")
            reward = (environment.trial_paths.verifier_dir / "reward.txt").read_text()
            artifact = environment.host_path("/app/check-result.json")
            return reward.strip(), artifact.relative_to(environment.workspace)
        finally:
            await environment.stop(delete=True)

    return asyncio.run(scenario())


def _criteria(wrapper: Path) -> list[str]:
    return json.loads((wrapper / "tests" / "criteria.json").read_text())


def test_the_wrapper_holds_the_task_in_the_workspace(wrapper):
    assert Task.is_valid_dir(wrapper)
    reviewed = wrapper / "environment" / "workspace" / check.TASK_DIRECTORY
    assert Task.is_valid_dir(reviewed)
    assert not (wrapper / "environment" / check.TASK_DIRECTORY).exists()
    instruction = (wrapper / "instruction.md").read_text()
    assert "/app" not in instruction
    assert "instruction.md" in instruction
    assert check.checker.RESULT_FILENAME in instruction


def test_the_verifier_accepts_a_complete_result_in_the_host_environment(
    wrapper, tmp_path
):
    result = {
        name: {"outcome": "pass", "explanation": "The task meets it."}
        for name in _criteria(wrapper)
    }
    reward, artifact = _verify(wrapper, tmp_path, result)
    assert reward == "1"
    assert artifact == Path(check.checker.RESULT_FILENAME)


def test_the_verifier_refuses_an_incomplete_result(wrapper, tmp_path):
    first = _criteria(wrapper)[0]
    result = {first: {"outcome": "pass", "explanation": "Only one criterion."}}
    assert _verify(wrapper, tmp_path, result)[0] == "0"
    assert _verify(wrapper, tmp_path / "missing", None)[0] == "0"


def test_the_job_runs_the_vis_agent_in_the_host_environment(wrapper, tmp_path):
    config = check.job_config(
        [wrapper], "anthropic-coding-plan", "claude-opus-5-5", tmp_path, "check", 2
    )
    [agent] = config.agents
    assert agent.import_path == AGENT
    assert agent.model_name == "anthropic-coding-plan/claude-opus-5-5"
    assert config.environment.import_path == ENVIRONMENT
    assert [task.path for task in config.tasks] == [wrapper]
    assert (config.n_attempts, config.n_concurrent_trials) == (1, 2)
