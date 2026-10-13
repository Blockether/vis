"""Read Harbor jobs: trials, cases, a summary with the cost, and round seals.

A Harbor job directory holds `config.json`, `result.json` and one directory for each
trial. A trial directory holds `result.json`, `agent/trajectory.json` and
`verifier/reward.json`. A round directory holds one or more job directories.

This module reads the JSON files directly, so it does not need Harbor itself.
"""

from __future__ import annotations

import hashlib
import json
import time
import tomllib
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from types import MappingProxyType
from typing import Any

from .stats import cluster_interval

__all__ = [
    "HarborTask",
    "HarborTrial",
    "RunSummary",
    "case_results",
    "read_tasks",
    "read_trial",
    "read_trials",
    "seal_digest",
    "seal_round",
    "seal_rows",
    "summarize",
]

RESULT_FILE = "result.json"
TASK_FILE = "task.toml"
INSTRUCTION_FILE = "instruction.md"


@dataclass(frozen=True, slots=True)
class HarborTrial:
    """One Harbor trial, read from its `result.json`.

    `task` is the Harbor task name. `agent` is the agent name that the trial reports.
    `model` is the model name of the trial configuration, for example
    `openai/gpt-6.1-sol`. `reward` is the `reward` value of the verifier. It is None when
    the verifier gave no reward, for example after an infrastructure error. `rewards`
    holds all named rewards. `error` is the exception type of a failed trial. The token
    counts and `cost_usd` are None when the agent did not report them. `path` is the
    `result.json` file.
    """

    job: str
    trial: str
    task: str
    agent: str
    agent_version: str | None
    model: str
    reward: float | None
    rewards: Mapping[str, float]
    error: str | None
    input_tokens: int | None
    cache_tokens: int | None
    output_tokens: int | None
    cost_usd: float | None
    task_checksum: str | None
    started_at: str | None
    finished_at: str | None
    path: Path

    @property
    def is_scored(self) -> bool:
        """True when the verifier gave a reward."""
        return self.reward is not None

    @property
    def is_passed(self) -> bool:
        """True when the reward is 1 or more."""
        return self.reward is not None and self.reward >= 1


@dataclass(frozen=True, slots=True)
class RunSummary:
    """The counts, the pass rate and the cost of a set of trials.

    A case is one task with one agent and one model. `pass_rate` is `passed / scored`,
    and `pass_rate_ci95` is its 95% cluster interval over the cases. Both are
    fractions, or None without a scored trial. `errors` counts the trials with an
    exception. `cost_usd` is the sum of the reported costs, or None when no trial
    reports a cost. `cost_per_pass_usd` divides that sum by the passed trials.
    """

    trials: int
    scored: int
    passed: int
    errors: int
    cases: int
    pass_rate: float | None
    pass_rate_ci95: tuple[float, float] | None
    input_tokens: int
    cache_tokens: int
    output_tokens: int
    cost_usd: float | None
    cost_per_pass_usd: float | None
    tasks: tuple[str, ...]
    agents: tuple[str, ...]
    models: tuple[str, ...]
    started_at: str | None
    finished_at: str | None


def _integer(value: Any) -> int | None:
    return value if isinstance(value, int) and not isinstance(value, bool) else None


def _number(value: Any) -> float | None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    return float(value)


def _text(value: Any) -> str | None:
    return str(value) if value not in (None, "") else None


def read_trial(path: Path | str) -> HarborTrial | None:
    """Read one finished trial from its `result.json`, or return None for other files.

    A job `result.json`, an unfinished trial and a file that is not JSON give None.
    """
    path = Path(path)
    try:
        data = json.loads(path.read_text())
    except (OSError, ValueError):
        return None
    if not isinstance(data, dict):
        return None
    if not (
        data.get("task_name") and data.get("trial_name") and data.get("finished_at")
    ):
        return None
    agent = (data.get("config") or {}).get("agent") or {}
    info = data.get("agent_info") or {}
    usage = data.get("agent_result") or {}
    rewards = {
        str(name): number
        for name, value in (
            (data.get("verifier_result") or {}).get("rewards") or {}
        ).items()
        if (number := _number(value)) is not None
    }
    return HarborTrial(
        job=path.parent.parent.name,
        trial=str(data["trial_name"]),
        task=str(data["task_name"]),
        agent=str(
            info.get("name") or agent.get("name") or agent.get("import_path") or ""
        ),
        agent_version=_text(info.get("version")),
        model=str(agent.get("model_name") or ""),
        reward=rewards.get("reward"),
        rewards=MappingProxyType(rewards),
        error=_text((data.get("exception_info") or {}).get("exception_type")),
        input_tokens=_integer(usage.get("n_input_tokens")),
        cache_tokens=_integer(usage.get("n_cache_tokens")),
        output_tokens=_integer(usage.get("n_output_tokens")),
        cost_usd=_number(usage.get("cost_usd")),
        task_checksum=_text(data.get("task_checksum")),
        started_at=_text(data.get("started_at")),
        finished_at=_text(data.get("finished_at")),
        path=path,
    )


def read_trials(directory: Path | str) -> tuple[HarborTrial, ...]:
    """Read the finished trials of a Harbor job directory or of a round of jobs.

    A trial `result.json` is one or two levels below `directory`. The trials come in
    start order. A directory that does not exist raises `FileNotFoundError`.
    """
    root = Path(directory)
    if not root.is_dir():
        raise FileNotFoundError(f"no Harbor job or round directory at {root}")
    paths = sorted({*root.glob(f"*/{RESULT_FILE}"), *root.glob(f"*/*/{RESULT_FILE}")})
    trials = [trial for trial in map(read_trial, paths) if trial is not None]
    return tuple(
        sorted(trials, key=lambda trial: (trial.started_at or "", str(trial.path)))
    )


def case_results(
    trials: Iterable[HarborTrial],
) -> dict[tuple[str, str, str], tuple[int, int]]:
    """Count the passed and the scored trials of each case: a task, an agent and a model.

    The keys are `(task, agent, model)`. A trial without a reward does not count, so a
    case without a scored trial is absent.
    """
    cases: dict[tuple[str, str, str], tuple[int, int]] = {}
    for trial in trials:
        if not trial.is_scored:
            continue
        key = (trial.task, trial.agent, trial.model)
        passed, runs = cases.get(key, (0, 0))
        cases[key] = (passed + trial.is_passed, runs + 1)
    return cases


def summarize(trials: Iterable[HarborTrial]) -> RunSummary:
    """Summarize a set of trials: counts, the pass rate with its interval, and the cost."""
    trials = tuple(trials)
    cases = case_results(trials)
    scored = sum(trial.is_scored for trial in trials)
    passed = sum(trial.is_passed for trial in trials)
    costs = [trial.cost_usd for trial in trials if trial.cost_usd is not None]
    cost = sum(costs) if costs else None
    starts = [trial.started_at for trial in trials if trial.started_at]
    ends = [trial.finished_at for trial in trials if trial.finished_at]
    return RunSummary(
        trials=len(trials),
        scored=scored,
        passed=passed,
        errors=sum(trial.error is not None for trial in trials),
        cases=len(cases),
        pass_rate=passed / scored if scored else None,
        pass_rate_ci95=cluster_interval(list(cases.values())),
        input_tokens=sum(trial.input_tokens or 0 for trial in trials),
        cache_tokens=sum(trial.cache_tokens or 0 for trial in trials),
        output_tokens=sum(trial.output_tokens or 0 for trial in trials),
        cost_usd=cost,
        cost_per_pass_usd=cost / passed if cost is not None and passed else None,
        tasks=tuple(sorted({trial.task for trial in trials})),
        agents=tuple(sorted({trial.agent for trial in trials})),
        models=tuple(sorted({trial.model for trial in trials})),
        started_at=min(starts) if starts else None,
        finished_at=max(ends) if ends else None,
    )


def _file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def seal_rows(trials: Iterable[HarborTrial], root: Path | str) -> list[dict[str, Any]]:
    """Return one seal row for each trial, with the task checksum and the result digest.

    `result` is the path of the trial `result.json`, relative to `root`, and
    `result_sha256` is the sha256 of that file.
    """
    root = Path(root)
    return [
        {
            "agent": trial.agent,
            "model": trial.model,
            "task": trial.task,
            "reward": trial.reward,
            "error": trial.error,
            "input": trial.input_tokens,
            "cache": trial.cache_tokens,
            "output": trial.output_tokens,
            "agent_version": trial.agent_version,
            "task_checksum": trial.task_checksum,
            "started_at": trial.started_at,
            "finished_at": trial.finished_at,
            "result": trial.path.relative_to(root).as_posix(),
            "result_sha256": _file_sha256(trial.path),
        }
        for trial in sorted(
            trials,
            key=lambda trial: (trial.agent, trial.model, trial.task, str(trial.path)),
        )
    ]


def seal_digest(body: Mapping[str, Any]) -> str:
    """Return the sha256 of a seal body with sorted keys, without its `digest` key."""
    content = {key: value for key, value in body.items() if key != "digest"}
    return hashlib.sha256(json.dumps(content, sort_keys=True).encode()).hexdigest()


def seal_round(
    round_name: str,
    runs: Sequence[Mapping[str, Any]],
    trials: Iterable[HarborTrial],
    root: Path | str,
    sealed_at: str | None = None,
) -> dict[str, Any]:
    """Return the seal of a round: its run records, one row for each trial and a digest.

    Each run record holds the commit, the pins, the profiles and the artifact sha256 of
    one run. The rows hold the task checksums and the result digests. `sealed_at`
    defaults to the current UTC time.
    """
    body: dict[str, Any] = {
        "round": round_name,
        "sealed_at": sealed_at or time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "runs": [dict(run) for run in runs],
        "rows": seal_rows(trials, root),
    }
    body["digest"] = seal_digest(body)
    return body


@dataclass(frozen=True, slots=True)
class HarborTask:
    """One Harbor task, read from its `task.toml` and `instruction.md`.

    `name` is the task folder name, as in `case_results`. `instruction` is the prompt
    that the agent gets. The timeouts are in seconds, or None when `task.toml` gives
    none. `gpus` is the GPU count that the environment needs. `metadata` holds the
    `[metadata]` table, for example the `lang` of an e2e scenario.
    """

    name: str
    instruction: str
    agent_timeout_sec: float | None
    verifier_timeout_sec: float | None
    gpus: int
    metadata: Mapping[str, Any]
    path: Path


def _seconds(table: object) -> float | None:
    value = table.get("timeout_sec") if isinstance(table, Mapping) else None
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    return float(value)


def read_tasks(dataset: Path | str) -> tuple[HarborTask, ...]:
    """Read the tasks of a Harbor dataset directory, sorted by name.

    Each task is a folder with a `task.toml`. Other folders do not count. A directory
    that does not exist raises `FileNotFoundError`. The `e2e` directory of the Vis
    repository is such a dataset.
    """
    root = Path(dataset)
    if not root.is_dir():
        raise FileNotFoundError(f"no Harbor dataset directory at {root}")
    tasks = []
    for config in sorted(root.glob(f"*/{TASK_FILE}")):
        info = tomllib.loads(config.read_text(encoding="utf-8"))
        instruction = config.parent / INSTRUCTION_FILE
        environment = info.get("environment")
        gpus = environment.get("gpus") if isinstance(environment, dict) else None
        metadata = info.get("metadata")
        tasks.append(
            HarborTask(
                name=config.parent.name,
                instruction=instruction.read_text(encoding="utf-8")
                if instruction.is_file()
                else "",
                agent_timeout_sec=_seconds(info.get("agent")),
                verifier_timeout_sec=_seconds(info.get("verifier")),
                gpus=gpus
                if isinstance(gpus, int) and not isinstance(gpus, bool)
                else 0,
                metadata=MappingProxyType(
                    dict(metadata) if isinstance(metadata, dict) else {}
                ),
                path=config.parent,
            )
        )
    return tuple(tasks)
