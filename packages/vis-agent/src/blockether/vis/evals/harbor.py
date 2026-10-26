"""Write and run Harbor tasks, and read Harbor jobs: trials, cases, a summary and seals.

A Harbor task is a folder with `task.toml`, `instruction.md`, `environment/` and
`tests/`. `write_task` writes a Vis task, and `run_tasks` runs tasks with the Vis
agent on this host. A Harbor job directory holds `config.json`, `result.json` and
one directory for each trial. A trial directory holds `result.json`,
`agent/trajectory.json` and `verifier/reward.json`. A round directory holds one or
more job directories.

The read functions read the JSON files directly. Only `run_tasks` imports Harbor.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import time
import tomllib
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
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
    "run_tasks",
    "seal_digest",
    "seal_round",
    "seal_rows",
    "split_turns",
    "summarize",
    "task_problems",
    "write_task",
]

RESULT_FILE = "result.json"
TASK_FILE = "task.toml"
INSTRUCTION_FILE = "instruction.md"
CHECKS_FILE = "checks.json"
TEST_SCRIPT_FILE = "test.sh"
WORKSPACE_FILESYSTEM_FILE = "workspace_filesystem.json"
WORKSPACE_DIRECTORY = "workspace"
TURN_MARKER = "<!-- vis:turn -->"
"""A line with only this text in an instruction starts the next user turn."""
_TURN_SPLIT = re.compile(rf"^[ \t]*{re.escape(TURN_MARKER)}[ \t]*(?:\n|$)", re.M)
TASK_NAME = re.compile(r"[a-z0-9]+(?:-[a-z0-9]+)*")
AGENT = "blockether.vis.evals.agent:VisAgent"
ENVIRONMENT = "blockether.vis.evals.environment:HostEnvironment"
GRADER_SCRIPT = """#!/usr/bin/env bash
# Grade the trial. The Vis host environment sets the VIS_HARBOR_* paths.
set -euo pipefail
exec "$VIS_HARBOR_PYTHON" -m blockether.vis.evals.grade "$VIS_HARBOR_TESTS/checks.json"
"""


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

    @property
    def turns(self) -> tuple[str, ...]:
        """The user messages of the task, one for each turn; see `split_turns`."""
        return tuple(split_turns(self.instruction))


def split_turns(instruction: str) -> list[str]:
    """Return the user messages of ``instruction``, one for each turn.

    Each line with only ``TURN_MARKER`` ends one message and starts the next. An
    instruction without that line is one turn and stays unchanged. An empty message
    raises ValueError.
    """
    parts = _TURN_SPLIT.split(instruction)
    if len(parts) == 1:
        return [instruction]
    messages = [part.strip() for part in parts]
    if not all(messages):
        raise ValueError("each turn of a multi-turn instruction needs text")
    return messages


def _join_turns(instruction: str | Sequence[str]) -> str:
    if isinstance(instruction, str):
        if not instruction.strip():
            raise ValueError("instruction must not be empty")
        return instruction
    messages = list(instruction)
    if not messages:
        raise ValueError("instruction must hold at least one user message")
    for message in messages:
        if not isinstance(message, str) or not message.strip():
            raise ValueError("each user message must be nonempty text")
        if _TURN_SPLIT.search(message):
            raise ValueError(f"a user message cannot hold a {TURN_MARKER} line")
    return f"\n{TURN_MARKER}\n".join(message.strip() for message in messages)


def _seconds(table: object) -> float | None:
    value = table.get("timeout_sec") if isinstance(table, Mapping) else None
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    return float(value)


def _read_task(config: Path) -> HarborTask:
    info = tomllib.loads(config.read_text(encoding="utf-8"))
    instruction = config.parent / INSTRUCTION_FILE
    environment = info.get("environment")
    gpus = environment.get("gpus") if isinstance(environment, dict) else None
    metadata = info.get("metadata")
    return HarborTask(
        name=config.parent.name,
        instruction=instruction.read_text(encoding="utf-8")
        if instruction.is_file()
        else "",
        agent_timeout_sec=_seconds(info.get("agent")),
        verifier_timeout_sec=_seconds(info.get("verifier")),
        gpus=gpus if isinstance(gpus, int) and not isinstance(gpus, bool) else 0,
        metadata=MappingProxyType(dict(metadata) if isinstance(metadata, dict) else {}),
        path=config.parent,
    )


def read_tasks(dataset: Path | str) -> tuple[HarborTask, ...]:
    """Read the tasks of a Harbor dataset directory, sorted by name.

    Each task is a folder with a `task.toml`. Other folders do not count. A directory
    that does not exist raises `FileNotFoundError`. The `e2e` directory of the Vis
    repository is such a dataset.
    """
    root = Path(dataset)
    if not root.is_dir():
        raise FileNotFoundError(f"no Harbor dataset directory at {root}")
    return tuple(_read_task(config) for config in sorted(root.glob(f"*/{TASK_FILE}")))


def _toml_value(key: str, value: object) -> str:
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)):
        return repr(value)
    if isinstance(value, str):
        return json.dumps(value)
    raise ValueError(f"metadata `{key}` must be a string, a number or a boolean")


def _seed_path(name: str) -> PurePosixPath:
    path = PurePosixPath(name)
    if not name or path.is_absolute() or ".." in path.parts or name != str(path):
        raise ValueError(f"seed file path must stay inside the workspace: {name!r}")
    return path


def task_problems(task: Path | str) -> tuple[str, ...]:
    """Return the problems of one task folder that give a wrong score; empty is valid.

    The check reads only files and makes no model calls. It needs an executable
    `tests/test.sh` and the folder `environment/workspace`. It checks
    `tests/checks.json` with `check_problems` and needs each Python file that the
    checks name. The harness writes `.vis/config.yml` in the trial workspace, so a seed
    file there is a problem. Each problem starts with the task name. `run_tasks` and
    `python -m blockether.vis.evals.check` refuse tasks with problems.
    """
    from .grade import check_problems

    folder = Path(task)
    tests = folder / "tests"
    workspace = folder / "environment" / WORKSPACE_DIRECTORY
    problems = []
    script = tests / TEST_SCRIPT_FILE
    if not script.is_file():
        problems.append(f"no tests/{TEST_SCRIPT_FILE}")
    elif not os.access(script, os.X_OK):
        problems.append(f"tests/{TEST_SCRIPT_FILE} is not executable")
    if not workspace.is_dir():
        problems.append(f"no environment/{WORKSPACE_DIRECTORY} folder")
    for name in ("config.yml", "config.yaml"):
        if (workspace / ".vis" / name).exists():
            problems.append(
                f"environment/{WORKSPACE_DIRECTORY}/.vis/{name} belongs to the harness;"
                " put the settings of the task in vis.yml"
            )
    checks_path = tests / CHECKS_FILE
    if checks_path.is_file():
        try:
            checks = json.loads(checks_path.read_text(encoding="utf-8"))
        except ValueError as error:
            checks = None
            problems.append(f"tests/{CHECKS_FILE} is not JSON: {error}")
        if checks is not None:
            problems.extend(check_problems(checks))
        for key in ("verify", "want_answer_program"):
            program = checks.get(key) if isinstance(checks, Mapping) else None
            if isinstance(program, str) and not (tests / program).is_file():
                problems.append(f"{key} names a missing file: tests/{program}")
    return tuple(f"{folder.name}: {problem}" for problem in problems)


def write_task(
    dataset: Path | str,
    name: str,
    instruction: str | Sequence[str],
    checks: Mapping[str, Any],
    *,
    files: Mapping[str, str] | None = None,
    agent_timeout_sec: float = 300.0,
    verifier_timeout_sec: float = 300.0,
    metadata: Mapping[str, str | int | float | bool] | None = None,
    artifacts: Sequence[str] = (),
    filesystem_roots: Mapping[str, str] | None = None,
    test_files: Mapping[str, str] | None = None,
) -> HarborTask:
    """Write the Vis task `name` in the folder `dataset` and return it.

    `name` is lowercase words joined by hyphens. `instruction` is the prompt of the
    agent. A list of strings gives one user message for each turn of one session;
    `write_task` joins them with `TURN_MARKER` lines. `checks` is the `tests/checks.json` object that the Vis grader reads, for
    example `{"want": {"m.py": ["def c("]}}`. `files` maps each seed file path to its
    text. The paths are relative to `environment/workspace/`. `artifacts` lists the
    workspace files that Harbor copies to `artifacts/` in the trial folder after the
    check, for example the call record of a mock extension. `filesystem_roots` maps each
    filesystem root id to a workspace folder; the trial registers them. `test_files` maps
    each file name in `tests/` to its text, for example the verifier that the `verify`
    check names. The agent never sees these files. The task gets `tests/test.sh`, which
    runs the grader. An existing task folder, an invalid name, invalid checks or a path
    outside its folder raise an error.
    """
    if not TASK_NAME.fullmatch(name):
        raise ValueError(
            f"task name must be lowercase words joined by hyphens: {name!r}"
        )
    if not isinstance(checks, Mapping) or not checks:
        raise ValueError("checks must be a nonempty object")
    from .grade import check_problems

    if problems := check_problems(checks):
        raise ValueError("; ".join(problems))
    extra_tests = {}
    for path, text in (test_files or {}).items():
        if PurePosixPath(path).name != path or path in {CHECKS_FILE, TEST_SCRIPT_FILE}:
            raise ValueError(f"test file must be a new file name in tests/: {path!r}")
        extra_tests[path] = text
    instruction = _join_turns(instruction)
    seeds = {_seed_path(path): text for path, text in (files or {}).items()}
    kept = [str(_seed_path(path)) for path in artifacts]
    roots = {
        root: str(_seed_path(path)) for root, path in (filesystem_roots or {}).items()
    }
    lines = [
        f"{key} = {_toml_value(key, value)}" for key, value in (metadata or {}).items()
    ]
    # Harbor reads `artifacts` only as a root key, so it comes before every table.
    root = f"artifacts = {json.dumps(kept)}\n\n" if kept else ""
    tables = [
        ("metadata", lines),
        ("agent", [f"timeout_sec = {float(agent_timeout_sec)!r}"]),
        ("verifier", [f"timeout_sec = {float(verifier_timeout_sec)!r}"]),
    ]
    task = Path(dataset) / name
    task.mkdir(parents=True, exist_ok=False)
    (task / TASK_FILE).write_text(
        root
        + "\n".join(
            f"[{table}]\n" + "".join(f"{line}\n" for line in rows)
            for table, rows in tables
        ),
        encoding="utf-8",
    )
    (task / INSTRUCTION_FILE).write_text(
        instruction.rstrip("\n") + "\n", encoding="utf-8"
    )
    workspace = task / "environment" / WORKSPACE_DIRECTORY
    workspace.mkdir(parents=True)
    for path, text in seeds.items():
        target = workspace.joinpath(*path.parts)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")
    if roots:
        (workspace.parent / WORKSPACE_FILESYSTEM_FILE).write_text(
            json.dumps(roots, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
        )
    tests = task / "tests"
    tests.mkdir()
    (tests / CHECKS_FILE).write_text(
        json.dumps(checks, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )
    for path, text in extra_tests.items():
        (tests / path).write_text(text, encoding="utf-8")
    script = tests / TEST_SCRIPT_FILE
    script.write_text(GRADER_SCRIPT, encoding="utf-8")
    script.chmod(0o755)
    return _read_task(task / TASK_FILE)


def run_tasks(
    path: Path | str,
    model: str,
    *,
    jobs_dir: Path | str = "jobs",
    job_name: str | None = None,
    attempts: int = 1,
    concurrency: int = 1,
    agent_options: Mapping[str, str] | None = None,
    report: bool = True,
) -> Path:
    """Run the tasks in `path` with the Vis agent on this host and return the job folder.

    `path` is one task folder or a dataset folder of tasks. `model` is the
    `provider/model` route of the agent. Harbor writes the job to
    `jobs_dir/job_name`. Without `job_name`, Harbor names the job with the time.
    `attempts` is the number of trials for each task. `concurrency` is the number
    of trials that run at the same time. `agent_options` are the agent options of
    `harbor run --ak`, for example `{"source": "/path/to/vis"}`. The call sets
    `HARBOR_TELEMETRY=off`, so Harbor sends no usage data. Each trial makes paid
    model calls. Call it outside a running event loop. With `report`, the call then
    writes the round report `vis-report.json` in the job folder, as
    `python -m blockether.vis.evals.report` does. A report error gives a warning and
    keeps the job folder.
    """
    import asyncio
    import warnings

    from harbor.job import Job
    from harbor.models.job.config import JobConfig
    from harbor.models.trial.config import AgentConfig, EnvironmentConfig, TaskConfig

    root = Path(path)
    if (root / TASK_FILE).is_file():
        folders = [root]
    else:
        folders = [task.path for task in read_tasks(root)]
    if not folders:
        raise ValueError(f"no Harbor tasks in {root}")
    if problems := [problem for folder in folders for problem in task_problems(folder)]:
        raise ValueError("fix the tasks before a paid run:\n" + "\n".join(problems))
    os.environ["HARBOR_TELEMETRY"] = "off"
    config = JobConfig(
        jobs_dir=Path(jobs_dir),
        n_attempts=attempts,
        n_concurrent_trials=concurrency,
        quiet=True,
        agents=[
            AgentConfig(
                import_path=AGENT, model_name=model, kwargs=dict(agent_options or {})
            )
        ],
        environment=EnvironmentConfig(import_path=ENVIRONMENT),
        tasks=[TaskConfig(path=folder) for folder in folders],
        datasets=[],
    )
    if job_name is not None:
        config = config.model_copy(update={"job_name": job_name})

    async def run() -> Path:
        job = await Job.create(config)
        await job.run()
        return job.job_dir

    job_dir = Path(asyncio.run(run()))
    if report:
        from .reports import REPORT_FILE, round_report, write_report

        dataset = None if (root / TASK_FILE).is_file() else root
        try:
            write_report(
                round_report(job_dir, dataset, model=model), job_dir / REPORT_FILE
            )
        except (OSError, ValueError) as error:
            warnings.warn(f"no round report for {job_dir}: {error}", stacklevel=2)
    return job_dir
