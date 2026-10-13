"""Tests for the trial judge with the Vis agent in the host environment."""

from __future__ import annotations

import asyncio
import json
from pathlib import Path

import pytest
from blockether.vis.evals import judge
from blockether.vis.evals.environment import HostEnvironment
from harbor.analyze.models import (
    AnalyzeReport,
    AnalyzeReportResult,
    build_analyze_response_model,
    load_rubric,
)
from harbor.cli.quality_checker.models import QualityCheckModel
from harbor.models.task.task import Task
from harbor.models.trial.paths import TrialPaths


@pytest.fixture(scope="module")
def wrapper(tmp_path_factory) -> Path:
    trial = tmp_path_factory.mktemp("job") / "task__abc"
    (trial / "agent").mkdir(parents=True)
    (trial / "trial.log").write_text("")
    (trial / "result.json").write_text("{}")
    (trial / "agent" / "trajectory.json").write_text('{"steps": []}')
    rubric = load_rubric(judge.RUBRIC)
    return judge.assemble(
        trial,
        rubric,
        build_analyze_response_model(rubric),
        tmp_path_factory.mktemp("wrappers") / "judge-task",
    )


def _criteria(wrapper: Path) -> list[str]:
    return json.loads((wrapper / "tests" / "criteria.json").read_text())


def _verify(wrapper: Path, tmp_path: Path, result: dict | None) -> tuple[str, Path]:
    trial_paths = TrialPaths(tmp_path / "trial")
    trial_paths.mkdir()
    environment = HostEnvironment(
        environment_dir=wrapper / "environment",
        environment_name="judge-task",
        session_id="judge-task__trial",
        trial_paths=trial_paths,
        task_env_config=Task(wrapper).config.environment,
    )

    async def scenario() -> tuple[str, Path]:
        await environment.start(force_build=False)
        try:
            if result is not None:
                (environment.workspace / "analysis.json").write_text(json.dumps(result))
            await environment.upload_dir(wrapper / "tests", "/tests")
            await environment.exec("bash /tests/test.sh")
            reward = (environment.trial_paths.verifier_dir / "reward.txt").read_text()
            artifact = environment.host_path("/app/analysis.json")
            return reward.strip(), artifact.relative_to(environment.workspace)
        finally:
            await environment.stop(delete=True)

    return asyncio.run(scenario())


def _checks(**outcomes: str) -> dict[str, dict]:
    return {
        name: {"outcome": outcome, "explanation": "Evidence."}
        for name, outcome in outcomes.items()
    }


def test_the_failure_stage_is_the_first_failed_stage():
    assert (
        judge.failure_stage(_checks(understand="pass", write="fail", verify="fail"))
        == "write"
    )
    assert judge.failure_stage(_checks(understand="fail", write="pass")) == "understand"
    assert (
        judge.failure_stage(_checks(understand="pass", write="pass", verify="pass"))
        == judge.OTHER_STAGE
    )
    assert judge.failure_stage({}) == judge.OTHER_STAGE


def test_the_failure_stage_reads_harbor_check_models():
    checks = {
        "understand": QualityCheckModel(outcome="pass", explanation="Read it."),
        "verify": QualityCheckModel(outcome="fail", explanation="No test."),
    }
    assert judge.failure_stage(checks) == "verify"


def test_the_rubric_checks_each_stage_and_reward_hacking(wrapper):
    assert _criteria(wrapper) == [*judge.STAGES, "reward_hacking"]


def test_the_wrapper_holds_the_trial_in_the_workspace(wrapper):
    assert Task.is_valid_dir(wrapper)
    workspace = wrapper / "environment" / "workspace"
    assert (workspace / "trial" / "agent" / "trajectory.json").is_file()
    assert not (wrapper / "environment" / "trial").exists()
    instruction = (wrapper / "instruction.md").read_text()
    assert "/app" not in instruction
    assert "trial/agent/trajectory.json" in instruction
    assert "analysis.json" in instruction


def test_the_verifier_accepts_a_complete_judgement_in_the_host_environment(
    wrapper, tmp_path
):
    result = {
        "summary": "The agent edited the wrong file and stopped.",
        "checks": _checks(**dict.fromkeys(_criteria(wrapper), "pass")),
    }
    reward, artifact = _verify(wrapper, tmp_path, result)
    assert reward == "1"
    assert artifact == Path("analysis.json")


def test_the_verifier_refuses_an_incomplete_judgement(wrapper, tmp_path):
    result = {"summary": "Only one check.", "checks": _checks(understand="pass")}
    assert _verify(wrapper, tmp_path, result)[0] == "0"
    assert _verify(wrapper, tmp_path / "missing", None)[0] == "0"


def test_stages_group_the_samples_of_each_trial():
    def result(name, error=None, **outcomes):
        return AnalyzeReportResult(
            trial_name=name, checks=_checks(**outcomes), error=error
        )

    report = AnalyzeReport(
        results=[
            result("a__1", understand="pass", write="fail"),
            result("a__1", understand="fail"),
            result("b__2", understand="pass", write="pass", verify="pass"),
            result("c__3", error="no valid result"),
        ]
    )
    assert judge.stages(report) == {"a__1": ["write", "understand"], "b__2": ["other"]}
