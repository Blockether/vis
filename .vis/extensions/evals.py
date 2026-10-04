"""Evals: run and read Vis evaluations, from e2e scenarios to Terminal-Bench on Harbor.

This extension drives the two runners in this repository. It does not copy them:

    e2e/run.py                   editing scenarios under e2e/scenarios/<id>/
    benchmarks/terminal_bench/   Harbor queue (run_suite.py) and summary (summarize.py)

The first Terminal-Bench 4.0 run of Vis taught these rules, and the code enforces them:

- Paid runs start only through run_scenarios() and run_bench(). Both start a detached
  process and return at once, so a benchmark of many hours never blocks a turn.
- run_bench() needs a passing preflight(): uv, the Podman machine, a Compose v2
  provider, the pinned Linux amd64 bundle, the dataset, the API key, zstd, 12 GB free
  on the host and in the Podman VM, and no other queue.
- Without a scored trial, run_bench() runs a canary of one task first.
- A zero exit code is not a score. bench() reads the verifier results, scores each task
  once by its latest verified attempt and keeps Vis errors, timeouts and unscored
  attempts apart.
- Every rate has a 95% interval. The repeats of one task or scenario count as one
  cluster, not as independent samples.
- A round is one Vis build in its own jobs directory: run_bench(round_name=...,
  attempts=k). compare() pairs two rounds or two scenario runs case by case. It calls a
  change real only when the interval of the difference excludes zero.
- diagnose() finds causes automatically. label() records human causes, and diagnose()
  measures the agreement with Cohen's kappa before you trust the automatic causes.
- Costs are metered-API price estimates. A Coding Plan subscription charge is unknown.
- Leaderboard comparisons are product level: model and harness change together, unless
  the board has the same model under two harnesses.
- Log excerpts redact credential values: environment variables whose names contain KEY,
  TOKEN, SECRET, PASSWORD or CREDENTIAL, with at least 8 characters, not only digits.

Run records live in ~/.vis/evals/runs/<run_id>/, or under VIS_EVALS_HOME: run.json,
output.log, exit_code and, for scenario runs, traces/results.json. A named round keeps
its jobs/ and runs/ in benchmarks/terminal_bench/rounds/<round>/. Its runs/ holds
summary.json, provenance.json and the human labels in labels.json.
"""

from __future__ import annotations

import hashlib
import json
import math
import os
import re
import shlex
import shutil
import signal
import statistics
import subprocess
import time
import tomllib
import urllib.request
import uuid
from collections import Counter, defaultdict
from dataclasses import dataclass, replace
from datetime import UTC, datetime
from pathlib import Path

import blockether.vis.extension as vis

REPO = Path(__file__).resolve().parents[2]
SCENARIOS = REPO / "e2e" / "scenarios"
E2E_RUNNER = REPO / "e2e" / "run.py"
BENCH = REPO / "benchmarks" / "terminal_bench"
HOME = Path(os.environ.get("VIS_EVALS_HOME") or Path.home() / ".vis" / "evals")
MACHINE = "vis-amd64"
MIN_FREE_GB = 12.0
API_KEY_ENV = "ZAI_CODING_API_KEY"
LEADERBOARD_URL = "https://api.harborframework.com/functions/v1/leaderboard-read"

_SECRET_MARKERS = ("KEY", "TOKEN", "SECRET", "PASSWORD", "CREDENTIAL")
_SCENARIO_ID = re.compile(r"[a-z0-9]+(?:-[a-z0-9]+)*")
_SETUP_KEYS = frozenset(
    {
        "lang",
        "prompt",
        "timeout_s",
        "files_from",
        "fixture_generator",
        "workspace_filesystem",
        "measurement",
    }
)
_EXIT_SCRIPT = (
    'trap "" HUP; "$@"; code=$?; '
    'printf "%s\\n" "$code" > "$VIS_EVALS_EXIT_FILE"; exit "$code"'
)
_EARLY_STOP_SHARE = 0.25
_NEAR_MISS_RATIO = 0.8
_TAIL_LINES = 20
# A two-sided 5% test, and the normal quantile for 80% power.
_Z95 = 1.959964
_Z_POWER = 0.841621
_MIN_LABELS = 30
_KAPPA_REVIEW = 0.6
_KAPPA_TRUST = 0.8
_NOTE_LIMIT = 500
_CAUSES = (
    "dead_tools",
    "vis_errors",
    "agent_timeouts",
    "early_stops",
    "unscored",
    "other",
)
_CHILDREN: dict[str, subprocess.Popen] = {}


@dataclass(frozen=True)
class Scenario:
    """One e2e scenario: a task prompt, fixture files and the checks that grade it.

    `checks` names the oracle and guard keys that the scenario sets, such as `want`,
    `want_answer_json` or `forbid_tools`. `timeout_s` is None when the runner default
    applies. `is_measurement` means the behavior check is reported, not gated.
    """

    id: str
    lang: str
    prompt: str
    checks: tuple[str, ...]
    timeout_s: int | None
    is_measurement: bool
    path: str


@dataclass(frozen=True)
class EvalRun:
    """One background evaluation process that this extension started.

    `state` is `running`, `finished` (see `exit_code`), `stopped` (by stop()) or `lost`
    (the process ended without an exit code, for example after a reboot). For scenario
    runs, exit code 0 means every scenario passed, 1 means a failure and 2 a setup
    error. For a benchmark queue, 0 means the queue ended normally; read bench() for
    the score. `results_path` is the scenario results file, or None for a queue.

    The version fields identify what ran. `vis_commit` is the checkout commit at the
    start. `is_dirty` is true when tracked files had uncommitted changes then.
    `build_sha256` is the Linux bundle of a queue, or the native binary of a scenario
    run with native_bin. It is None when scenarios ran from the checkout. `round_name`
    is the benchmark round of a queue, and None is the main round. Runs from an older
    version of this extension have None in all four fields.
    """

    run_id: str
    kind: str
    label: str
    state: str
    exit_code: int | None
    pid: int
    started_at: str
    ended_at: str | None
    command: str
    log_path: str
    results_path: str | None
    vis_commit: str | None
    is_dirty: bool | None
    build_sha256: str | None
    round_name: str | None


@dataclass(frozen=True)
class RunStatus:
    """A run with its progress and the last lines of its log, credentials redacted."""

    run: EvalRun
    progress: str
    elapsed_s: int
    log_tail: tuple[str, ...]


@dataclass(frozen=True)
class ScenarioResult:
    """Results of one scenario on one provider and model, over all its repeats.

    A run passes when it finished, gave the correct result and had no unexpected
    errors. `failures` has one line for each failed run. Token counts are totals over
    runs with valid usage. `wall_median_s` is None without samples.
    """

    scenario: str
    provider: str
    model: str
    runs: int
    passed: int
    behavior_passed: int
    is_measurement: bool
    cached_input_percent: float
    wall_median_s: float | None
    input_tokens: int
    output_tokens: int
    failures: tuple[str, ...]


@dataclass(frozen=True)
class ScenarioReport:
    """All results of one scenario run, read from its results.json.

    A route is one provider and model. `pass_rate_percent` divides passed runs by all
    runs. `pass_rate_ci95` is its 95% Wilson interval, with the repeats of one scenario
    route as one cluster. A second run of the same size cannot detect a change below
    `detectable_change_points`. `repeats` is the most common run count of a scenario
    route. With more repeats than one, `pass_all_percent` (pass^k) and
    `pass_any_percent` (pass@k) give the share of scenario routes that passed every
    repeat or at least one. Otherwise they are None. `unstable` lists the scenario
    routes with mixed results.
    """

    run_id: str | None
    results_path: str
    total_runs: int
    passed_runs: int
    is_passed: bool
    pass_rate_percent: float
    pass_rate_ci95: tuple[float, float] | None
    detectable_change_points: float | None
    repeats: int
    pass_all_percent: float | None
    pass_any_percent: float | None
    unstable: tuple[str, ...]
    results: tuple[ScenarioResult, ...]


@dataclass(frozen=True)
class Check:
    """One preflight check. `fix` says what to do when `is_ok` is false."""

    name: str
    is_ok: bool
    detail: str
    fix: str


@dataclass(frozen=True)
class Preflight:
    """The benchmark setup checks. `is_ready` is true only when every check passed."""

    is_ready: bool
    checks: tuple[Check, ...]


@dataclass(frozen=True)
class QueuePlan:
    """The Terminal-Bench queue of one round as run_suite.py sees it.

    `attempts` is the number of scored attempts that each task needs in the round.
    `completed` counts CPU tasks with all of them, and `scored` counts CPU tasks with at
    least one. `retryable` lists failed model attempts that run_bench(retry_tasks=...)
    accepts. `live` lists trials without a result. Without a running queue, these are
    interrupted trials: `stale_live`.
    """

    model: str
    cpu_tasks: int
    attempts: int
    completed: int
    scored: int
    retryable: tuple[str, ...]
    live: tuple[str, ...]
    pending: tuple[str, ...]
    gpu_only: tuple[str, ...]
    is_queue_running: bool
    stale_live: tuple[str, ...]


@dataclass(frozen=True)
class OutcomeCount:
    """Tasks whose latest scored attempt ended the same way."""

    outcome: str
    count: int
    tasks: tuple[str, ...]


@dataclass(frozen=True)
class BenchReport:
    """The Terminal-Bench score of one round, with usage and the queue state.

    `pass_rate_percent` divides solved tasks by scored tasks, by the latest scored
    attempt of each task. `strict_pass_rate_percent` divides them by all dataset tasks,
    so unscored, unattempted and GPU tasks count as failures. The attempt pass rate
    counts every scored attempt, with the attempts of one task as one cluster. Each
    `_ci95` value is a 95% Wilson interval in percent. A second round of the same size
    cannot detect a change below `detectable_change_points`. `attempts_per_task` is the
    most common scored attempt count of a task. When it is more than one,
    `pass_at_k_percent` and `pass_all_k_percent` give the share of tasks that solved at
    least one or all of their first attempts, up to that count. Otherwise they are None.
    `unstable_tasks` lists tasks with mixed results. `estimated_metered_cost_usd` and
    `cost_per_solved_usd` use metered API prices. They are not subscription charges.
    `plan` is None when the queue plan could not be read.
    """

    dataset: str
    round_name: str | None
    model: str
    total_tasks: int
    scored_tasks: int
    solved_tasks: int
    pass_rate_percent: float
    pass_rate_ci95: tuple[float, float] | None
    strict_pass_rate_percent: float
    strict_pass_rate_ci95: tuple[float, float] | None
    attempts_per_task: int
    scored_attempts: int
    solved_attempts: int
    attempt_pass_rate_percent: float
    attempt_pass_rate_ci95: tuple[float, float] | None
    detectable_change_points: float | None
    pass_at_k_percent: float | None
    pass_all_k_percent: float | None
    unstable_tasks: tuple[str, ...]
    outcomes: tuple[OutcomeCount, ...]
    unscored_tasks: tuple[str, ...]
    gpu_tasks: tuple[str, ...]
    unattempted_tasks: int
    attempts: int
    model_attempts: int
    agent_hours: float
    input_tokens: int
    cached_input_percent: float
    output_tokens: int
    unparsed_output_tokens_estimate: int
    estimated_metered_cost_usd: float
    cost_per_solved_usd: float | None
    repeated_final_errors: tuple[str, ...]
    plan: QueuePlan | None
    summary_path: str


@dataclass(frozen=True)
class TaskNote:
    """One task in a finding.

    `checks` is passed/total verifier tests, or empty. `budget_used` is the share of
    the task's agent time limit that Vis used.
    """

    task: str
    outcome: str
    checks: str
    pass_ratio: float | None
    budget_used: float | None
    minutes: float | None
    iterations: int | None


@dataclass(frozen=True)
class Finding:
    """One problem class. `share` is the fraction of scored tasks it affects."""

    kind: str
    title: str
    task_count: int
    share: float
    detail: str
    tasks: tuple[TaskNote, ...]


@dataclass(frozen=True)
class LabelCheck:
    """How well the automatic diagnose() causes agree with human labels from label().

    `kappa` is Cohen's kappa over the labelled attempts, or None when it is undefined.
    `disagreements` names each attempt where the two causes differ. `advice` says if
    you can trust the automatic causes. Kappa needs at least 30 labels. Use the causes
    with a human review from 0.6, and without a review from 0.8.
    """

    labelled: int
    agreement_percent: float | None
    kappa: float | None
    disagreements: tuple[str, ...]
    advice: str


@dataclass(frozen=True)
class Diagnosis:
    """Where the benchmark loses tasks, largest lever first.

    `estimate` projects the effect of full attempts on Vis-error tasks. It is an
    estimate, not a measurement. `label_check` compares the causes with human labels.
    """

    scored_tasks: int
    solved_tasks: int
    findings: tuple[Finding, ...]
    estimate: str
    label_check: LabelCheck


@dataclass(frozen=True)
class LeaderboardRow:
    """One published agent and model result.

    `accuracy_percent` is over all trials of the row. Per-trial values divide the row
    totals by `trials`, and `cost_per_success_usd` divides the total cost by
    `successes`. `None` means that the board does not publish the value.
    """

    rank: int
    agent: str
    model: str
    reasoning_effort: str
    accuracy_percent: float
    ci95_half_width: float | None
    trials: int
    successes: int
    total_cost_usd: float | None
    cost_per_trial_usd: float | None
    cost_per_success_usd: float | None
    tokens_per_trial: int | None
    avg_trial_minutes: float | None
    date: str


@dataclass(frozen=True)
class VisStanding:
    """Where the local Vis benchmark result would stand on the board.

    Like the board accuracy, `pass_rate_percent` counts every scored attempt.
    `strict_pass_rate_percent` also counts each unscored, unattempted and GPU task as
    `attempts_per_task` failed attempts. Each `_ci95` value is a 95% interval with the
    attempts of one task as one cluster. The ranks insert the rates among the board
    rows, and each rank range inserts the ends of its interval. Per-attempt values use
    model attempts. `caveats` lists the limits of the comparison.
    """

    model: str
    round_name: str | None
    scored_tasks: int
    total_tasks: int
    attempts_per_task: int
    pass_rate_percent: float
    pass_rate_ci95: tuple[float, float] | None
    strict_pass_rate_percent: float
    strict_pass_rate_ci95: tuple[float, float] | None
    rank_by_pass_rate: int
    rank_range: tuple[int, int] | None
    rank_by_strict_rate: int
    strict_rank_range: tuple[int, int] | None
    cost_per_attempt_usd: float | None
    cost_per_solved_usd: float | None
    tokens_per_attempt: int | None
    avg_attempt_minutes: float | None
    caveats: tuple[str, ...]


@dataclass(frozen=True)
class Leaderboard:
    """A Harbor Hub leaderboard. `vis` is None without a local result for its dataset."""

    package: str
    name: str
    title: str
    rows: tuple[LeaderboardRow, ...]
    vis: VisStanding | None


@dataclass(frozen=True)
class CaseChange:
    """One case whose pass rate changed between the two sides, as passed/runs."""

    case: str
    baseline: str
    candidate: str


@dataclass(frozen=True)
class Comparison:
    """A paired comparison of two evaluation results over the cases that both ran.

    A case is a scenario on one route, or a benchmark task. The rates are the mean pass
    rates of the paired cases, in percent. `difference_ci95` is the Agresti-Min 95%
    interval of the paired difference, in points. `verdict` is `better` or `worse` only
    when that interval excludes zero, else `no measurable difference`. At this case
    count, a true change of `detectable_change_points` or more shows with 80% power.
    `worse` and `better` list the changed cases, largest change first. `unpaired`
    counts cases on one side only; the comparison leaves them out.
    """

    kind: str
    baseline: str
    candidate: str
    baseline_version: str
    candidate_version: str
    paired_cases: int
    baseline_rate_percent: float
    candidate_rate_percent: float
    difference_points: float
    difference_ci95: tuple[float, float]
    verdict: str
    detectable_change_points: float
    worse: tuple[CaseChange, ...]
    better: tuple[CaseChange, ...]
    unpaired: int
    caveats: tuple[str, ...]


@dataclass(frozen=True)
class TrialView:
    """One benchmark attempt, with the facts that you need to label its failure.

    `outcome` uses the summarize.py names, and `cause` is the diagnose() cause or
    `solved`. `checks` is passed/total verifier tests. `budget_used` is the share of the
    agent time limit. `tool_calls` gives the most used tools with their call counts.
    `verifier_tail` and `log_tail` are the last lines of the verifier output and of the
    Vis stderr log, credentials redacted. `label` is the human cause from label(), or
    None.
    """

    task: str
    trial: str
    round_name: str | None
    outcome: str
    cause: str
    reward: float | None
    checks: str
    minutes: float | None
    budget_used: float | None
    iterations: int | None
    error_type: str | None
    tool_calls: tuple[str, ...]
    verifier_tail: tuple[str, ...]
    log_tail: tuple[str, ...]
    trace_path: str | None
    label: str | None


@dataclass(frozen=True)
class TrialLabel:
    """A human cause for one failed attempt, kept in the labels.json of its round.

    `diagnosed_cause` is the automatic diagnose() cause of the same attempt.
    """

    task: str
    trial: str
    round_name: str | None
    cause: str
    diagnosed_cause: str
    note: str
    labelled_at: str


def _now() -> str:
    return datetime.now(UTC).isoformat(timespec="seconds")


def _secret_values() -> list[str]:
    values = {
        value
        for name, value in os.environ.items()
        if any(marker in name.upper() for marker in _SECRET_MARKERS)
        and len(value) >= 8
        and not value.isdigit()
    }
    return sorted(values, key=len, reverse=True)


def _redact(text: str) -> str:
    for value in _secret_values():
        text = text.replace(value, "[REDACTED]")
    return text


def _clip(text: object, limit: int) -> str:
    text = " ".join(str(text).split())
    return text if len(text) <= limit else text[: limit - 1] + "…"


def _one_line(text: object, limit: int = 512) -> str:
    """Return printable text as one line of at most `limit` UTF-8 bytes."""
    line = " ".join("".join(c if c.isprintable() else " " for c in str(text)).split())
    data = line.encode("utf-8")
    if len(data) <= limit:
        return line
    return data[: limit - 3].decode("utf-8", errors="ignore") + "…"


def _percent(part: float, whole: float) -> float:
    return round(100 * part / whole, 1) if whole else 0.0


def _count(number: int, noun: str) -> str:
    return f"{number} {noun}" if number == 1 else f"{number} {noun}s"


def _short(task: object) -> str:
    return str(task).rsplit("/", 1)[-1]


def _wilson(
    passed: int, runs: int, size: float | None = None
) -> tuple[float, float] | None:
    """Give the 95% Wilson interval of a pass rate, in percent, or None without runs.

    `size` replaces the run count when the runs are not independent.
    """
    size = runs if size is None else size
    if not runs or size <= 0:
        return None
    rate = passed / runs
    spread = _Z95**2 / size
    center = (rate + spread / 2) / (1 + spread)
    half = _Z95 * math.sqrt(rate * (1 - rate) / size + spread / size / 4) / (1 + spread)
    return round(max(0.0, center - half) * 100, 1), round(
        min(1.0, center + half) * 100, 1
    )


def _effective_size(units: list[tuple[int, int]]) -> float:
    """Shrink the run count of clustered pass/fail results by their design effect.

    Each unit is (passed, runs) of one task or scenario route. The repeats of one unit
    are not independent, so the size stays between the unit count and the run count.
    Without any variation, it is the unit count.
    """
    units = [(passed, runs) for passed, runs in units if runs]
    count = len(units)
    total = sum(runs for _, runs in units)
    if count < 2 or total == count:
        return float(total)
    rate = sum(passed for passed, _ in units) / total
    if rate in (0, 1):
        return float(count)
    spread = sum((passed - rate * runs) ** 2 for passed, runs in units)
    variance = count / (count - 1) * spread / total**2
    if not variance:
        return float(total)
    return min(float(total), max(float(count), rate * (1 - rate) / variance))


def _interval(units: list[tuple[int, int]]) -> tuple[float, float] | None:
    """Give the 95% interval of the pooled pass rate of clustered units, in percent."""
    passed = sum(passed for passed, _ in units)
    return _wilson(passed, sum(runs for _, runs in units), _effective_size(units))


def _detectable_change(units: list[tuple[int, int]]) -> float | None:
    """Give the smallest change, in points, that a second run of this size detects.

    The test compares two independent runs of the same size, two-sided at the 5% level
    with 80% power. None means that no change within 0-100% is detectable.
    """
    total = sum(runs for _, runs in units)
    if not total:
        return None
    rate = sum(passed for passed, _ in units) / total
    size = _effective_size(units)

    def detects(other: float) -> bool:
        pooled = (rate + other) / 2
        noise = _Z95 * math.sqrt(2 * pooled * (1 - pooled) / size)
        power = _Z_POWER * math.sqrt((rate * (1 - rate) + other * (1 - other)) / size)
        return abs(other - rate) >= noise + power

    smallest = None
    for sign in (1, -1):
        for step in range(1, 1001):
            other = rate + sign * step / 1000
            if not 0 <= other <= 1:
                break
            if detects(other):
                smallest = step if smallest is None else min(smallest, step)
                break
    return round(smallest / 10, 1) if smallest is not None else None


def _paired_change(changes: list[float]) -> tuple[tuple[float, float], float]:
    """Give the Agresti-Min 95% interval of a mean paired change and its floor.

    Each change is the candidate pass rate of one case minus its baseline pass rate.
    The floor is the smallest true change that this many cases detect with 80% power.
    Both are in points.
    """
    count = len(changes)
    mean = sum(changes) / (count + 2)
    square = (sum(change * change for change in changes) + 1) / (count + 2)
    error = math.sqrt(max(0.0, square - mean * mean) / (count + 2))
    low, high = mean - _Z95 * error, mean + _Z95 * error
    return (round(low * 100, 1), round(high * 100, 1)), round(
        (_Z95 + _Z_POWER) * error * 100, 1
    )


def _kappa(pairs: list[tuple[str, str]]) -> float | None:
    """Give Cohen's kappa of two raters over the same items, or None when undefined."""
    if not pairs:
        return None
    count = len(pairs)
    observed = sum(first == second for first, second in pairs) / count
    firsts = Counter(first for first, _ in pairs)
    seconds = Counter(second for _, second in pairs)
    expected = sum(firsts[cause] * seconds[cause] for cause in firsts) / count**2
    if expected >= 1:
        return None
    return round((observed - expected) / (1 - expected), 3)


def _mode(values: list[int]) -> int:
    """Give the most common value, the larger one on a tie, or 0 without values."""
    counts = Counter(values)
    return max(counts, key=lambda value: (counts[value], value)) if counts else 0


def _command(
    argv: list[str],
    *,
    cwd: Path | None = None,
    env: dict[str, str] | None = None,
    timeout: float = 60,
) -> subprocess.CompletedProcess | None:
    try:
        return subprocess.run(
            argv, cwd=cwd, env=env, capture_output=True, text=True, timeout=timeout
        )
    except (OSError, subprocess.SubprocessError):
        return None


def _failure_text(result: subprocess.CompletedProcess | None) -> str:
    if result is None:
        return "the command could not start or timed out"
    return _clip(_redact((result.stderr or result.stdout or "").strip()[-600:]), 600)


def _tail(path: Path, lines: int = _TAIL_LINES) -> tuple[str, ...]:
    if not path.is_file():
        return ()
    with path.open("rb") as stream:
        stream.seek(0, os.SEEK_END)
        stream.seek(max(0, stream.tell() - 64 * 1024))
        text = stream.read().decode("utf-8", errors="replace")
    return tuple(_redact(line) for line in text.splitlines()[-lines:])


def _git_state() -> tuple[str | None, bool | None]:
    """Read the checkout commit, and whether tracked files have uncommitted changes."""
    head = _command(["git", "rev-parse", "HEAD"], cwd=REPO, timeout=10)
    if head is None or head.returncode != 0:
        return None, None
    status = _command(
        ["git", "status", "--porcelain", "--untracked-files=no"], cwd=REPO, timeout=30
    )
    is_dirty = (
        bool(status.stdout.strip()) if status and status.returncode == 0 else None
    )
    return head.stdout.strip(), is_dirty


def _scenario_dirs() -> dict[str, Path]:
    if not SCENARIOS.is_dir():
        raise FileNotFoundError(f"no scenario directory at {SCENARIOS}")
    return {
        path.parent.name: path.parent
        for path in sorted(SCENARIOS.glob("*/scenario.json"))
    }


def _scenario(directory: Path) -> Scenario:
    data = json.loads((directory / "scenario.json").read_text(encoding="utf-8"))
    timeout = data.get("timeout_s")
    return Scenario(
        id=directory.name,
        lang=str(data.get("lang") or ""),
        prompt=str(data.get("prompt") or ""),
        checks=tuple(
            key
            for key, value in data.items()
            if key not in _SETUP_KEYS and value not in ({}, [], None, False, "")
        ),
        timeout_s=timeout if isinstance(timeout, int) else None,
        is_measurement=bool(data.get("measurement")),
        path=str(directory),
    )


def _new_run(kind: str) -> tuple[str, Path]:
    run_id = f"{datetime.now():%Y%m%d-%H%M%S}-{kind}-{uuid.uuid4().hex[:4]}"
    run_dir = HOME / "runs" / run_id
    run_dir.mkdir(parents=True)
    return run_id, run_dir


def _launch(
    run_id: str,
    run_dir: Path,
    *,
    kind: str,
    label: str,
    argv: list[str],
    cwd: Path,
    env: dict[str, str],
    results_path: Path | None = None,
    expected_runs: int | None = None,
    build_sha256: str | None = None,
    round_name: str | None = None,
    attempts: int | None = None,
) -> EvalRun:
    vis_commit, is_dirty = _git_state()
    with (run_dir / "output.log").open("ab") as log:
        process = subprocess.Popen(
            ["/bin/sh", "-c", _EXIT_SCRIPT, f"vis-evals:{run_id}", *argv],
            cwd=cwd,
            env={**env, "VIS_EVALS_EXIT_FILE": str(run_dir / "exit_code")},
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        )
    _CHILDREN[run_id] = process
    meta = {
        "run_id": run_id,
        "kind": kind,
        "label": label,
        "command": shlex.join(argv),
        "cwd": str(cwd),
        "pid": process.pid,
        "started_at": _now(),
        "results_path": str(results_path) if results_path else None,
        "expected_runs": expected_runs,
        "vis_commit": vis_commit,
        "is_dirty": is_dirty,
        "build_sha256": build_sha256,
        "round": round_name,
        "attempts": attempts,
    }
    (run_dir / "run.json").write_text(json.dumps(meta, indent=2) + "\n", "utf-8")
    return _run(run_dir)


def _alive(run_id: str, pid: int) -> bool:
    process = _CHILDREN.get(run_id)
    if process is not None:
        return process.poll() is None
    probe = _command(
        ["ps", "-o", "stat=", "-o", "command=", "-p", str(pid)], timeout=10
    )
    line = probe.stdout.strip() if probe and probe.returncode == 0 else ""
    return bool(line) and not line.startswith("Z") and f"vis-evals:{run_id}" in line


def _meta(run_dir: Path) -> dict:
    return json.loads((run_dir / "run.json").read_text(encoding="utf-8"))


def _run(run_dir: Path) -> EvalRun:
    meta = _meta(run_dir)
    exit_path = run_dir / "exit_code"
    stop_path = run_dir / "stopped"
    exit_code = None
    ended_at = None
    if exit_path.is_file():
        text = exit_path.read_text().strip()
        exit_code = int(text) if text.lstrip("-").isdigit() else None
        state = "finished"
        ended_at = datetime.fromtimestamp(exit_path.stat().st_mtime, UTC).isoformat(
            timespec="seconds"
        )
    elif _alive(meta["run_id"], int(meta["pid"])):
        state = "running"
    elif stop_path.is_file():
        state = "stopped"
        ended_at = stop_path.read_text().strip() or None
    else:
        state = "lost"
    return EvalRun(
        run_id=meta["run_id"],
        kind=meta["kind"],
        label=meta["label"],
        state=state,
        exit_code=exit_code,
        pid=int(meta["pid"]),
        started_at=meta["started_at"],
        ended_at=ended_at,
        command=_redact(meta["command"]),
        log_path=str(run_dir / "output.log"),
        results_path=meta.get("results_path"),
        vis_commit=meta.get("vis_commit"),
        is_dirty=meta.get("is_dirty"),
        build_sha256=meta.get("build_sha256"),
        round_name=meta.get("round"),
    )


def _run_dirs() -> list[Path]:
    root = HOME / "runs"
    if not root.is_dir():
        return []
    return sorted(path for path in root.iterdir() if (path / "run.json").is_file())


def _resolve_run(run_id: str | None, kind: str | None = None) -> Path:
    candidates = [
        path for path in _run_dirs() if kind is None or _meta(path)["kind"] == kind
    ]
    if run_id is None:
        if not candidates:
            raise LookupError(f"no {kind or 'evaluation'} runs in {HOME / 'runs'}")
        return candidates[-1]
    matches = [path for path in candidates if path.name.startswith(run_id)]
    if len(matches) != 1:
        found = "no run" if not matches else f"{len(matches)} runs"
        raise LookupError(f"{found} match {run_id!r}; use runs() to list run ids")
    return matches[0]


def _uv_env() -> dict[str, str]:
    return {**os.environ, "PYTHONPATH": str(BENCH)}


def _podman_socket() -> str | None:
    result = _command(
        [
            "podman",
            "machine",
            "inspect",
            "--format",
            "{{.ConnectionInfo.PodmanSocket.Path}}",
            MACHINE,
        ],
        timeout=30,
    )
    path = result.stdout.strip() if result and result.returncode == 0 else ""
    return path or None


def _bench_env() -> dict[str, str]:
    env = _uv_env()
    if socket_path := _podman_socket():
        # run_suite.py reclaims images on this machine, so Harbor must use it too.
        env["DOCKER_HOST"] = f"unix://{socket_path}"
    compose = BENCH / "artifacts" / "docker-compose"
    if (
        not env.get("PODMAN_COMPOSE_PROVIDER")
        and compose.is_file()
        and os.access(compose, os.X_OK)
    ):
        env["PODMAN_COMPOSE_PROVIDER"] = str(compose)
    return env


def _queue_processes() -> list[int]:
    probe = _command(["ps", "-axo", "pid=,command="], timeout=10)
    pids = []
    for line in probe.stdout.splitlines() if probe and probe.returncode == 0 else []:
        pid, _, command = line.strip().partition(" ")
        if pid.isdigit() and "run_suite.py" in command and "--dry-run" not in command:
            pids.append(int(pid))
    return pids


def _round_dir(round_name: str | None) -> Path:
    """Give the directory of a benchmark round. None is the main round in BENCH."""
    if round_name is None:
        return BENCH
    if not _SCENARIO_ID.fullmatch(round_name):
        raise ValueError(
            f"round name {round_name!r} must be lowercase letters and digits, "
            "joined by single hyphens"
        )
    return BENCH / "rounds" / round_name


def _round_attempts(round_name: str | None) -> int:
    """Give the scored attempts that each task needs in a round. The main round has 1."""
    return int(_provenance(round_name).get("attempts") or 1)


def _queue_plan(
    round_name: str | None = None, attempts: int | None = None
) -> QueuePlan:
    argv = ["uv", "run", "--locked", "python", "run_suite.py", "--dry-run", "--json"]
    if round_name is not None:
        argv += ["--jobs", str(_round_dir(round_name) / "jobs")]
    argv += ["--attempts", str(attempts or _round_attempts(round_name))]
    result = _command(argv, cwd=BENCH, env=_uv_env(), timeout=180)
    if result is None or result.returncode != 0:
        raise RuntimeError(f"could not read the queue plan: {_failure_text(result)}")
    data = json.loads(result.stdout.strip().splitlines()[-1])
    running = bool(_queue_processes())
    live = tuple(data["live"])
    return QueuePlan(
        model=str(data["model"]),
        cpu_tasks=int(data["cpu_tasks"]),
        attempts=int(data["attempts"]),
        completed=len(data["completed"]),
        scored=len(data["scored"]),
        retryable=tuple(data["retryable"]),
        live=live,
        pending=tuple(data["pending"]),
        gpu_only=tuple(data["gpu_only"]),
        is_queue_running=running,
        stale_live=() if running else live,
    )


def _check(name: str, is_ok: object, detail: str, fix: str) -> Check:
    return Check(name, bool(is_ok), detail, "" if is_ok else fix)


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def _bundle() -> Path:
    return BENCH / "artifacts" / "vis-agent-linux-amd64.tar.gz"


def _provenance(round_name: str | None = None) -> dict:
    path = _round_dir(round_name) / "runs" / "provenance.json"
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}


def _start_round(round_name: str, attempts: int, model: str, digest: str) -> None:
    """Pin a new round to the bundle, model and attempt count of its first queue."""
    path = _round_dir(round_name) / "runs" / "provenance.json"
    path.parent.mkdir(parents=True, exist_ok=True)
    vis_commit, is_dirty = _git_state()
    data = {
        "round": round_name,
        "created_at": _now(),
        "created_by": "evals.run_bench",
        "attempts": attempts,
        "model": model,
        "bundle_sha256": digest,
        "vis_commit": vis_commit,
        "is_dirty": is_dirty,
        "note": "vis_commit is the checkout when the round started. The bundle "
        "records no source commit.",
    }
    path.write_text(json.dumps(data, indent=2) + "\n", "utf-8")


def _labels_path(round_name: str | None) -> Path:
    return _round_dir(round_name) / "runs" / "labels.json"


def _labels(round_name: str | None) -> dict[str, dict]:
    try:
        data = json.loads(_labels_path(round_name).read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return data if isinstance(data, dict) else {}


def _vm_free_gb() -> float | None:
    result = _command(
        ["podman", "machine", "ssh", MACHINE, "--", "df", "-Pk", "/var/lib/containers"],
        timeout=30,
    )
    try:
        line = result.stdout.strip().splitlines()[-1]
        return int(line.split()[3]) * 1024 / 1e9
    except (AttributeError, IndexError, ValueError):
        return None


def _preflight(round_name: str | None = None) -> Preflight:
    checks = []
    uv = shutil.which("uv")
    checks.append(
        _check(
            "uv",
            uv,
            uv or "not found",
            "Install uv, then run `uv sync --locked` in benchmarks/terminal_bench.",
        )
    )
    podman = shutil.which("podman")
    checks.append(_check("Podman", podman, podman or "not found", "Install Podman."))
    state = None
    if podman:
        result = _command(
            ["podman", "machine", "inspect", "--format", "{{.State}}", MACHINE],
            timeout=30,
        )
        state = result.stdout.strip() if result and result.returncode == 0 else None
    running = state == "running"
    checks.append(
        _check(
            "Podman machine",
            running,
            f"{MACHINE}: {state or 'not found'}",
            f"Start it with `podman machine start {MACHINE}`.",
        )
    )
    idle = Check(
        "",
        False,
        f"not checked: {MACHINE} is not running",
        f"Start {MACHINE} first, then run preflight() again.",
    )
    env = _bench_env() if podman else _uv_env()
    if running:
        docker_host = env.get("DOCKER_HOST", "")
        replaced = os.environ.get("DOCKER_HOST", "")
        checks.append(
            _check(
                "Docker socket",
                docker_host.startswith("unix://")
                and Path(docker_host.removeprefix("unix://")).exists(),
                (docker_host or "DOCKER_HOST is not set")
                + (
                    f", replaces DOCKER_HOST={replaced} from the environment"
                    if replaced and replaced != docker_host
                    else ""
                ),
                f"Set DOCKER_HOST to the socket of the {MACHINE} Podman machine.",
            )
        )
        compose = _command(["podman", "compose", "ls"], env=env)
        compose_ok = compose is not None and compose.returncode == 0
        provider = env.get("PODMAN_COMPOSE_PROVIDER") or "Podman default provider"
        checks.append(
            _check(
                "Compose provider",
                compose_ok,
                provider if compose_ok else f"{provider}: {_failure_text(compose)}",
                "Set PODMAN_COMPOSE_PROVIDER to a Docker Compose v2 executable. A legacy "
                "podman-compose fails the Harbor preflight.",
            )
        )
    else:
        checks += [
            replace(idle, name=name) for name in ("Docker socket", "Compose provider")
        ]
    bundle = _bundle()
    if bundle.is_file():
        digest = _sha256(bundle)
        pinned = _provenance(round_name).get("bundle_sha256")
        same = pinned in (None, digest)
        detail = f"{bundle.stat().st_size / 1e6:.0f} MB, sha256 {digest[:12]}"
        if pinned:
            detail += (
                ", matches provenance" if same else f", provenance pins {pinned[:12]}"
            )
        elif round_name is not None:
            detail += f", round {round_name} pins it when it starts"
        checks.append(
            _check(
                "Vis bundle",
                same,
                detail,
                "Restore the pinned bundle, or start a new round with "
                "run_bench(round_name=...). Do not mix builds in one round.",
            )
        )
    else:
        checks.append(
            _check(
                "Vis bundle",
                False,
                "missing artifacts/vis-agent-linux-amd64.tar.gz",
                "Build the Linux amd64 native bundle. macOS and arm64 binaries cannot "
                "run in Harbor task images.",
            )
        )
    dataset = BENCH / "artifacts" / "datasets" / "terminal-bench"
    tasks = len(list(dataset.glob("*/task.toml"))) if dataset.is_dir() else 0
    checks.append(
        _check(
            "Dataset",
            tasks,
            f"{tasks} tasks in artifacts/datasets/terminal-bench",
            "Run `uv run harbor download terminal-bench/terminal-bench@4.0.0 "
            "--output-dir artifacts/datasets`.",
        )
    )
    has_key = bool(os.environ.get(API_KEY_ENV))
    checks.append(
        _check(
            "API key",
            has_key,
            f"{API_KEY_ENV} is {'set' if has_key else 'not set'}",
            f"Give Vis {API_KEY_ENV} through its environment. Never put the key in a "
            "file or a command argument.",
        )
    )
    zstd = shutil.which("zstd")
    checks.append(
        _check(
            "zstd",
            zstd,
            zstd or "not found",
            "Install zstd. The queue archives traces with it.",
        )
    )
    host_free = shutil.disk_usage(BENCH).free / 1e9
    checks.append(
        _check(
            "Host disk",
            host_free >= MIN_FREE_GB,
            f"{host_free:.0f} GB free",
            f"Free at least {MIN_FREE_GB:.0f} GB. Task images and traces are large.",
        )
    )
    if running:
        vm_free = _vm_free_gb()
        checks.append(
            _check(
                "Podman VM disk",
                vm_free is not None and vm_free >= MIN_FREE_GB,
                f"{vm_free:.0f} GB free" if vm_free is not None else "unknown",
                f"Remove unused images in the {MACHINE} machine, then trim its disk."
                if vm_free is not None
                else f"Read it with `podman machine ssh {MACHINE} -- df -h`.",
            )
        )
    else:
        checks.append(replace(idle, name="Podman VM disk"))
    pids = _queue_processes()
    checks.append(
        _check(
            "Single queue",
            not pids,
            f"queue running as process {pids[0]}" if pids else "no queue running",
            "Wait for the running queue, or stop it, before you start another one.",
        )
    )
    return Preflight(all(check.is_ok for check in checks), tuple(checks))


def _summary_path(round_name: str | None = None) -> Path:
    return _round_dir(round_name) / "runs" / "summary.json"


def _refresh_summary(round_name: str | None = None) -> None:
    argv = ["uv", "run", "--locked", "python", "summarize.py"]
    if round_name is not None:
        jobs = _round_dir(round_name) / "jobs"
        argv += ["--jobs", str(jobs), "--output", str(_summary_path(round_name))]
    result = _command(argv, cwd=BENCH, env=_uv_env(), timeout=900)
    if result is None or result.returncode != 0:
        raise RuntimeError(f"summarize.py failed: {_failure_text(result)}")


def _summary(refresh: bool, round_name: str | None = None) -> dict:
    if not _round_dir(round_name).is_dir():
        raise LookupError(
            f"no benchmark round {round_name!r}. Start it with "
            f"run_bench(round_name={round_name!r})."
        )
    if refresh:
        _refresh_summary(round_name)
    path = _summary_path(round_name)
    if not path.is_file():
        raise FileNotFoundError(
            f"no benchmark summary at {path}. Run a trial, then bench(refresh=True)."
        )
    return json.loads(path.read_text(encoding="utf-8"))


def _trial_model(data: dict) -> str:
    models = Counter(
        str(trial.get("model"))
        for trial in data.get("trials") or ()
        if trial.get("model")
    )
    return models.most_common(1)[0][0] if models else ""


def _bench_report(
    data: dict, plan: QueuePlan | None, round_name: str | None = None
) -> BenchReport:
    usage = data.get("usage_totals") or {}
    outcomes = sorted(
        (data.get("task_outcomes") or {}).items(),
        key=lambda item: (-len(item[1]), item[0]),
    )
    solved = len(data.get("solved_tasks") or ())
    scored = int(data.get("scored_tasks") or 0)
    total = int(data.get("total_dataset_tasks") or 0)
    input_tokens = int(usage.get("input_tokens") or 0)
    attempts = _task_attempts(data)
    units = [(sum(flags), len(flags)) for flags in attempts.values()]
    scored_attempts = sum(runs for _, runs in units)
    solved_attempts = sum(passed for passed, _ in units)
    per_task = _mode([len(flags) for flags in attempts.values()])
    complete = [
        flags[:per_task] for flags in attempts.values() if len(flags) >= per_task
    ]
    is_repeated = per_task > 1
    cost = round(float(usage.get("estimated_metered_api_cost_usd") or 0), 2)
    return BenchReport(
        dataset=str(data.get("dataset") or ""),
        round_name=round_name,
        model=_trial_model(data),
        total_tasks=total,
        scored_tasks=scored,
        solved_tasks=solved,
        pass_rate_percent=_percent(solved, scored),
        pass_rate_ci95=_wilson(solved, scored),
        strict_pass_rate_percent=_percent(solved, total),
        strict_pass_rate_ci95=_wilson(solved, total),
        attempts_per_task=per_task,
        scored_attempts=scored_attempts,
        solved_attempts=solved_attempts,
        attempt_pass_rate_percent=_percent(solved_attempts, scored_attempts),
        attempt_pass_rate_ci95=_interval(units),
        detectable_change_points=_detectable_change(units),
        pass_at_k_percent=_percent(sum(map(any, complete)), len(complete))
        if is_repeated
        else None,
        pass_all_k_percent=_percent(sum(map(all, complete)), len(complete))
        if is_repeated
        else None,
        unstable_tasks=tuple(
            sorted(
                task for task, flags in attempts.items() if 0 < sum(flags) < len(flags)
            )
        ),
        outcomes=tuple(
            OutcomeCount(outcome, len(tasks), tuple(sorted(map(_short, tasks))))
            for outcome, tasks in outcomes
        ),
        unscored_tasks=tuple(map(_short, data.get("unscored_model_tasks") or ())),
        gpu_tasks=tuple(map(_short, data.get("gpu_required_tasks") or ())),
        unattempted_tasks=len(data.get("unattempted_tasks") or ()),
        attempts=int(data.get("attempts") or 0),
        model_attempts=int(usage.get("model_attempts") or 0),
        agent_hours=round(float(usage.get("agent_hours") or 0), 1),
        input_tokens=input_tokens,
        cached_input_percent=_percent(
            int(usage.get("cached_input_tokens") or 0), input_tokens
        ),
        output_tokens=int(usage.get("output_tokens") or 0),
        unparsed_output_tokens_estimate=int(
            usage.get("unparsed_output_tokens_estimate") or 0
        ),
        estimated_metered_cost_usd=cost,
        cost_per_solved_usd=round(cost / solved_attempts, 2)
        if cost and solved_attempts
        else None,
        repeated_final_errors=tuple(
            f"{error.get('trial')}: {error.get('type')} for "
            f"{error.get('iterations')} iterations"
            for error in data.get("repeated_final_errors") or ()
        ),
        plan=plan,
        summary_path=str(_summary_path(round_name)),
    )


def _agent_limits() -> dict[str, float]:
    limits = {}
    dataset = BENCH / "artifacts" / "datasets" / "terminal-bench"
    for path in dataset.glob("*/task.toml") if dataset.is_dir() else ():
        try:
            info = tomllib.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        limit = (info.get("agent") or {}).get("timeout_sec")
        if limit:
            limits[path.parent.name] = float(limit)
    return limits


def _latest_trials(data: dict) -> dict[str, dict]:
    latest: dict[str, dict] = {}
    for trial in data.get("trials") or ():
        if not trial.get("scored"):
            continue
        task = _short(trial.get("task"))
        if str(trial.get("finished_at") or "") >= str(
            latest.get(task, {}).get("finished_at") or ""
        ):
            latest[task] = trial
    return latest


def _task_attempts(data: dict) -> dict[str, list[bool]]:
    """List the scored attempts of each task in finish order, True when solved."""
    attempts: dict[str, list[bool]] = defaultdict(list)
    trials = sorted(
        (trial for trial in data.get("trials") or () if trial.get("scored")),
        key=lambda trial: str(trial.get("finished_at") or ""),
    )
    for trial in trials:
        attempts[_short(trial.get("task"))].append(trial.get("reward") == 1)
    return dict(attempts)


def _attempt_outcome(trial: dict) -> str:
    """Name how one attempt ended, with the task outcome rules of summarize.py."""
    if not trial.get("scored"):
        return "unscored"
    if trial.get("reward") == 1:
        return "solved"
    if trial.get("exception_type") == "AgentTimeoutError":
        return "agent_timeout"
    trace = trial.get("trace") or {}
    if trace.get("vis_result_status") == "error":
        return f"vis_error:{trace.get('vis_error_type') or 'unknown'}"
    return "failed_tests"


def _dead_trials(data: dict) -> set[str]:
    return {
        _short(error.get("trial")) for error in data.get("repeated_final_errors") or ()
    }


def _cause(trial: dict, dead: set[str], limits: dict[str, float]) -> str:
    """Give the diagnose() cause of one attempt, or `solved`."""
    outcome = _attempt_outcome(trial)
    if outcome == "solved":
        return outcome
    if str(trial.get("trial")) in dead:
        return "dead_tools"
    if outcome.startswith("vis_error:"):
        return "vis_errors"
    if outcome == "agent_timeout":
        return "agent_timeouts"
    if outcome == "unscored":
        return "unscored"
    seconds = trial.get("agent_seconds")
    limit = limits.get(_short(trial.get("task")))
    if seconds and limit and seconds / limit <= _EARLY_STOP_SHARE:
        return "early_stops"
    return "other"


def _label_check(
    data: dict, labels: dict[str, dict], limits: dict[str, float]
) -> LabelCheck:
    """Compare the diagnose() causes with the human labels of the same attempts."""
    trials = {str(trial.get("trial")): trial for trial in data.get("trials") or ()}
    dead = _dead_trials(data)
    pairs = []
    disagreements = []
    for name, label in sorted(labels.items()):
        if name not in trials:
            continue
        cause, human = _cause(trials[name], dead, limits), str(label.get("cause"))
        pairs.append((cause, human))
        if cause != human:
            disagreements.append(f"{name}: diagnose() {cause}, label {human}")
    count = len(pairs)
    kappa = _kappa(pairs)
    if not count:
        advice = (
            f"No labels yet. Label at least {_MIN_LABELS} failed attempts with "
            "label() before you trust these causes."
        )
    elif count < _MIN_LABELS:
        advice = (
            f"Label {_MIN_LABELS - count} more failed attempts. Kappa from "
            f"{_count(count, 'label')} is not stable."
        )
    elif kappa is None:
        advice = (
            "Kappa is undefined, because all labels and causes are the same. Label "
            "failures of other causes too."
        )
    elif kappa < _KAPPA_REVIEW:
        advice = (
            f"Kappa {kappa:.2f} is below {_KAPPA_REVIEW}. Do not trust these causes. "
            "Correct the diagnose() rules first."
        )
    elif kappa < _KAPPA_TRUST:
        advice = (
            f"Kappa {kappa:.2f} is good enough with a human review. Read the "
            "disagreements before you act on a finding."
        )
    else:
        advice = (
            f"Kappa {kappa:.2f} is at least {_KAPPA_TRUST}. You can use these causes "
            "without a review."
        )
    return LabelCheck(
        labelled=count,
        agreement_percent=_percent(count - len(disagreements), count)
        if count
        else None,
        kappa=kappa,
        disagreements=tuple(disagreements),
        advice=advice,
    )


def _task_note(task: str, outcome: str, trial: dict, limit: float | None) -> TaskNote:
    tests = trial.get("verifier_tests") or {}
    total = int(tests.get("tests") or 0) if isinstance(tests, dict) else 0
    passed = int(tests.get("passed") or 0) if total else 0
    seconds = trial.get("agent_seconds")
    return TaskNote(
        task=task,
        outcome=outcome,
        checks=f"{passed}/{total}" if total else "",
        pass_ratio=round(passed / total, 3) if total else None,
        budget_used=round(seconds / limit, 3) if seconds and limit else None,
        minutes=round(seconds / 60, 1) if seconds else None,
        iterations=trial.get("vis_iterations"),
    )


def _diagnosis(data: dict, labels: dict[str, dict]) -> Diagnosis:
    outcomes = {
        outcome: sorted(map(_short, tasks))
        for outcome, tasks in (data.get("task_outcomes") or {}).items()
    }
    scored = int(data.get("scored_tasks") or 0)
    solved = len(outcomes.get("solved", ()))
    latest = _latest_trials(data)
    limits = _agent_limits()

    def notes(outcome: str) -> list[TaskNote]:
        return [
            _task_note(task, outcome, latest.get(task, {}), limits.get(task))
            for task in outcomes.get(outcome, ())
        ]

    def share(count: int) -> float:
        return round(count / scored, 3) if scored else 0.0

    findings = []
    errors = [
        note
        for outcome in sorted(outcomes)
        if outcome.startswith("vis_error:")
        for note in notes(outcome)
    ]
    if errors:
        kinds = Counter(note.outcome.removeprefix("vis_error:") for note in errors)
        findings.append(
            Finding(
                "vis_errors",
                f"Vis stopped with an error in {len(errors)} of {scored} scored tasks",
                len(errors),
                share(len(errors)),
                "By type: "
                + ", ".join(f"{kind} {count}" for kind, count in kinds.most_common())
                + ". These tasks did not get a full attempt. Provider failures that a "
                "retry can survive, such as a dropped stream or an output-token "
                "limit, are the cheapest score to recover.",
                tuple(errors),
            )
        )
    failed = notes("failed_tests")
    used = [note.budget_used for note in failed if note.budget_used is not None]
    early = sorted(
        (
            note
            for note in failed
            if note.budget_used is not None and note.budget_used <= _EARLY_STOP_SHARE
        ),
        key=lambda note: note.budget_used,
    )
    if early:
        findings.append(
            Finding(
                "early_stops",
                f"Vis finished early and failed the tests in {_count(len(early), 'task')}",
                len(early),
                share(len(early)),
                f"Vis ended these runs itself, without an error, after at most "
                f"{_EARLY_STOP_SHARE:.0%} of the agent time limit. The median "
                f"failed-tests run used {statistics.median(used):.0%}. Vis did not "
                "check its work against the task before it finished.",
                tuple(early),
            )
        )
    timeouts = notes("agent_timeout")
    near = sorted(
        (
            note
            for note in [*failed, *errors, *timeouts]
            if note.pass_ratio is not None
            and note.pass_ratio >= _NEAR_MISS_RATIO
            and note.pass_ratio < 1
        ),
        key=lambda note: -note.pass_ratio,
    )
    if near:
        findings.append(
            Finding(
                "near_misses",
                f"{_count(len(near), 'failed task')} passed at least "
                f"{_NEAR_MISS_RATIO:.0%} of their verifier tests",
                len(near),
                share(len(near)),
                "Rewards are all or nothing, so these close runs score zero. Read "
                "their traces first. They overlap the other findings.",
                tuple(near),
            )
        )
    if timeouts:
        findings.append(
            Finding(
                "agent_timeouts",
                f"{_count(len(timeouts), 'task')} reached the agent time limit",
                len(timeouts),
                share(len(timeouts)),
                "Harbor verified them after the timeout. Look in the trace for a loop "
                "or a dead tool.",
                tuple(timeouts),
            )
        )
    repeated = data.get("repeated_final_errors") or ()
    if repeated:
        findings.append(
            Finding(
                "dead_tools",
                f"{_count(len(repeated), 'attempt')} repeated one error until the end",
                len(repeated),
                share(len(repeated)),
                "All last iterations failed the same way, for example "
                "python-worker-retired after the sandbox Python worker exited. The "
                "model kept calling a tool that could not work, for hours.",
                tuple(
                    TaskNote(
                        _short(error.get("trial")).split("__", 1)[0],
                        str(error.get("type")),
                        "",
                        None,
                        None,
                        None,
                        error.get("iterations"),
                    )
                    for error in repeated
                ),
            )
        )
    unscored = tuple(map(_short, data.get("unscored_model_tasks") or ()))
    if unscored:
        findings.append(
            Finding(
                "unscored",
                f"{_count(len(unscored), 'model attempt')} ended without a verifier result",
                len(unscored),
                share(len(unscored)),
                "Infrastructure stopped these attempts, for example an out-of-memory "
                "kill. Retry them with run_bench(retry_tasks=[...]). A strict score "
                "counts them as failures.",
                tuple(
                    TaskNote(task, "unscored", "", None, None, None, None)
                    for task in unscored
                ),
            )
        )
    full = solved + len(failed) + len(timeouts)
    estimate = ""
    if errors and full:
        rate = solved / full
        extra = rate * len(errors)
        estimate = (
            f"{solved} of {full} tasks with a full attempt passed ({rate:.0%}). At that "
            f"rate, full attempts on the {_count(len(errors), 'Vis-error task')} would "
            f"add about {extra:.1f} solved tasks: {_percent(solved + extra, scored)}% "
            f"instead of {_percent(solved, scored)}%. This is an estimate, not a "
            "measurement."
        )
    return Diagnosis(
        scored, solved, tuple(findings), estimate, _label_check(data, labels, limits)
    )


def _post_json(url: str, payload: dict) -> dict:
    request = urllib.request.Request(
        url,
        data=json.dumps(payload).encode(),
        headers={"Content-Type": "application/json", "User-Agent": "vis-evals"},
    )
    with urllib.request.urlopen(request, timeout=30) as response:
        return json.load(response)


def _label(value: object) -> str:
    return (
        str(value.get("label") or "") if isinstance(value, dict) else str(value or "")
    )


def _number(value: object) -> float | None:
    return float(value) if isinstance(value, (int, float)) else None


def _leaderboard_row(raw: dict) -> LeaderboardRow:
    meta = raw.get("metadata") or {}
    metrics = raw.get("metrics") or {}
    trials = int(metrics.get("n_trials") or 0)
    successes = int(metrics.get("successes") or 0)
    cost = _number(metrics.get("total_cost_usd"))
    tokens = _number(metrics.get("total_tokens"))
    duration = _number(metrics.get("avg_trial_duration_sec"))
    return LeaderboardRow(
        rank=int(raw.get("rank") or 0),
        agent=_label(meta.get("agent_display")),
        model=_label(meta.get("model_display")),
        reasoning_effort=str(meta.get("reasoning_effort") or ""),
        accuracy_percent=float(metrics.get("accuracy") or 0.0),
        ci95_half_width=_number(metrics.get("accuracy_ci95_half_width")),
        trials=trials,
        successes=successes,
        total_cost_usd=cost,
        cost_per_trial_usd=round(cost / trials, 2)
        if cost is not None and trials
        else None,
        cost_per_success_usd=round(cost / successes, 2)
        if cost is not None and successes
        else None,
        tokens_per_trial=int(tokens / trials) if tokens and trials else None,
        avg_trial_minutes=round(duration / 60, 1) if duration else None,
        date=str(meta.get("date") or ""),
    )


def _model_key(text: str) -> str:
    return re.sub(r"[^a-z0-9]", "", text.lower())


def _rank(rows: tuple[LeaderboardRow, ...], rate: float) -> int:
    return 1 + sum(row.accuracy_percent > rate for row in rows)


def _rank_range(
    rows: tuple[LeaderboardRow, ...], interval: tuple[float, float] | None
) -> tuple[int, int] | None:
    return (
        None
        if interval is None
        else (_rank(rows, interval[1]), _rank(rows, interval[0]))
    )


def _standing(
    rows: tuple[LeaderboardRow, ...],
    data: dict,
    package: str,
    name: str,
    round_name: str | None = None,
) -> VisStanding | None:
    dataset_package, _, version = str(data.get("dataset") or "").partition("@")
    if dataset_package != package or version.replace(".", "-") != name:
        return None
    total = int(data.get("total_dataset_tasks") or 0)
    attempts = _task_attempts(data)
    units = [(sum(flags), len(flags)) for flags in attempts.values()]
    per_task = _mode([len(flags) for flags in attempts.values()]) or 1
    strict_units = units + [(0, per_task)] * max(0, total - len(units))
    solved = sum(passed for passed, _ in units)
    rate = _percent(solved, sum(runs for _, runs in units))
    strict = _percent(solved, sum(runs for _, runs in strict_units))
    interval = _interval(units)
    strict_interval = _interval(strict_units)
    usage = data.get("usage_totals") or {}
    model_attempts = int(usage.get("model_attempts") or 0)
    tokens = int(usage.get("input_tokens") or 0) + int(usage.get("output_tokens") or 0)
    cost = _number(usage.get("estimated_metered_api_cost_usd"))
    hours = _number(usage.get("agent_hours"))
    model = _trial_model(data)
    same_model = [
        row
        for row in rows
        if _model_key(row.model) and _model_key(row.model) in _model_key(model)
    ]
    caveats = [
        f"The board has {_count(len(same_model), 'row')} with {model}. Compare Vis "
        "with the same model to see the harness effect."
        if same_model
        else f"No board row uses {model}. Model and harness differ together, so this "
        "is a product-level comparison."
    ]
    if total and rows:
        board = statistics.median(row.trials for row in rows) / total
        if per_task < round(board):
            caveats.append(
                f"Board rows ran about {board:.0f} trials per task. Vis has "
                f"{_count(per_task, 'scored attempt')} per task, so its interval is "
                f"wider. For a matched comparison, run run_bench(attempts={board:.0f}, "
                "round_name=...)."
            )
    caveats.append(
        "The rank range places the ends of the Vis 95% interval among the board rows."
    )
    caveats.append(
        f"The pass rate counts {len(units)} scored tasks of {total}. The strict rate "
        "counts unscored, unattempted and GPU tasks as failures."
    )
    caveats.append(
        "Vis cost is a metered-API price estimate. A Coding Plan subscription charge "
        "is unknown."
    )
    return VisStanding(
        model=model,
        round_name=round_name,
        scored_tasks=len(units),
        total_tasks=total,
        attempts_per_task=per_task,
        pass_rate_percent=rate,
        pass_rate_ci95=interval,
        strict_pass_rate_percent=strict,
        strict_pass_rate_ci95=strict_interval,
        rank_by_pass_rate=_rank(rows, rate),
        rank_range=_rank_range(rows, interval),
        rank_by_strict_rate=_rank(rows, strict),
        strict_rank_range=_rank_range(rows, strict_interval),
        cost_per_attempt_usd=round(cost / model_attempts, 2)
        if cost is not None and model_attempts
        else None,
        cost_per_solved_usd=round(cost / solved, 2)
        if cost is not None and solved
        else None,
        tokens_per_attempt=tokens // model_attempts if model_attempts else None,
        avg_attempt_minutes=round(hours * 60 / model_attempts, 1)
        if hours is not None and model_attempts
        else None,
        caveats=tuple(caveats),
    )


def _failure_line(row: dict) -> str:
    reasons = []
    if not row.get("converged"):
        reasons.append("did not finish")
    if not row.get("correct"):
        reasons.append("wrong result")
    if row.get("errors"):
        reasons.append(f"{row['errors']} errors")
    detail = row.get("detail")
    if isinstance(detail, (list, tuple)):
        detail = "; ".join(map(str, detail))
    messages = row.get("err_msgs") or []
    extra = detail or (messages[0] if messages else "")
    text = f"run {row.get('repeat', 1)}: " + ", ".join(reasons or ["failed"])
    return _clip(_redact(f"{text}: {extra}" if extra else text), 300)


def _scenario_report(path: Path, run_id: str | None) -> ScenarioReport:
    data = json.loads(path.read_text(encoding="utf-8"))
    failures = defaultdict(list)
    for row in data.get("runs") or ():
        if not (row.get("converged") and row.get("correct") and not row.get("errors")):
            key = (row.get("id"), row.get("provider"), row.get("model"))
            failures[key].append(_failure_line(row))
    results = []
    for summary in data.get("summaries") or ():
        key = (summary.get("id"), summary.get("provider"), summary.get("model"))
        totals = summary.get("token_totals") or {}
        wall = summary.get("wall") or {}
        results.append(
            ScenarioResult(
                scenario=str(summary.get("id")),
                provider=str(summary.get("provider")),
                model=str(summary.get("model")),
                runs=int(summary.get("runs") or 0),
                passed=int(summary.get("passed") or 0),
                behavior_passed=int(summary.get("behavior_passed") or 0),
                is_measurement=bool(summary.get("measurement")),
                cached_input_percent=float(summary.get("cached_input_percent") or 0.0),
                wall_median_s=_number(wall.get("median")),
                input_tokens=int(totals.get("input") or 0),
                output_tokens=int(totals.get("output") or 0),
                failures=tuple(failures.get(key, ())),
            )
        )
    total = sum(result.runs for result in results)
    passed = sum(result.passed for result in results)
    measured = [result for result in results if result.runs]
    units = [(result.passed, result.runs) for result in measured]
    repeats = _mode([result.runs for result in measured])
    is_repeated = repeats > 1
    return ScenarioReport(
        run_id=run_id,
        results_path=str(path),
        total_runs=total,
        passed_runs=passed,
        is_passed=bool(total) and passed == total,
        pass_rate_percent=_percent(passed, total),
        pass_rate_ci95=_interval(units),
        detectable_change_points=_detectable_change(units),
        repeats=repeats,
        pass_all_percent=_percent(
            sum(result.passed == result.runs for result in measured), len(measured)
        )
        if is_repeated
        else None,
        pass_any_percent=_percent(
            sum(result.passed > 0 for result in measured), len(measured)
        )
        if is_repeated
        else None,
        unstable=tuple(
            f"{result.scenario} ({result.provider}/{result.model})"
            for result in measured
            if 0 < result.passed < result.runs
        ),
        results=tuple(results),
    )


def _version_text(commit: object, is_dirty: object, build: object) -> str:
    parts = []
    if commit:
        parts.append(
            f"commit {str(commit)[:12]}"
            + (" with uncommitted changes" if is_dirty else "")
        )
    if build:
        parts.append(f"build {str(build)[:12]}")
    return ", ".join(parts) or "unknown"


def _is_identified(commit: object, is_dirty: object, build: object) -> bool:
    """Tell if a build digest or a commit without uncommitted changes names the code."""
    return bool(build) or (bool(commit) and not is_dirty)


@dataclass(frozen=True)
class _Side:
    """One side of a comparison: its cases as (passed, runs), and its version.

    `is_identified` is true when a build digest or a clean commit names the code.
    """

    kind: str
    name: str
    version: str
    is_dirty: bool
    is_identified: bool
    routes: frozenset[str]
    cases: dict[tuple[str, ...], tuple[int, int]]


def _side(spec: str) -> _Side:
    """Read a scenario run id, `bench` for the main round, or `bench:<round>`."""
    if spec == "bench" or spec.startswith("bench:"):
        round_name = spec.partition(":")[2] or None
        data = _summary(False, round_name)
        pins = _provenance(round_name)
        is_dirty = bool(pins.get("is_dirty"))
        commit = pins.get("source_commit") or pins.get("vis_commit")
        build = pins.get("bundle_sha256")
        return _Side(
            "bench",
            spec,
            _version_text(commit, is_dirty, build),
            is_dirty,
            _is_identified(commit, is_dirty, build),
            frozenset({_trial_model(data)}),
            {
                (task,): (sum(flags), len(flags))
                for task, flags in _task_attempts(data).items()
            },
        )
    try:
        run = _run(_resolve_run(spec, "scenarios"))
    except LookupError as error:
        raise LookupError(
            f"{error}. Name a benchmark round as 'bench' or 'bench:<round>'."
        ) from None
    if not run.results_path or not Path(run.results_path).is_file():
        raise FileNotFoundError(f"scenario run {run.run_id} has no results.json yet")
    report = _scenario_report(Path(run.results_path), run.run_id)
    return _Side(
        "scenarios",
        run.run_id,
        _version_text(run.vis_commit, run.is_dirty, run.build_sha256),
        bool(run.is_dirty),
        _is_identified(run.vis_commit, run.is_dirty, run.build_sha256),
        frozenset(f"{result.provider}/{result.model}" for result in report.results),
        {
            (result.scenario, result.provider, result.model): (
                result.passed,
                result.runs,
            )
            for result in report.results
            if result.runs
        },
    )


def _case_name(key: tuple[str, ...]) -> str:
    return key[0] if len(key) == 1 else f"{key[0]} ({key[1]}/{key[2]})"


def _compare(baseline: str, candidate: str) -> Comparison:
    base, other = _side(baseline), _side(candidate)
    if base.kind != other.kind:
        raise ValueError(
            "compare two scenario runs or two benchmark rounds, not one of each"
        )
    if base.name == other.name:
        raise ValueError(f"both sides are {base.name}; compare two different results")
    caveats = []
    base_cases, other_cases = base.cases, other.cases
    if base.kind == "scenarios" and not base_cases.keys() & other_cases.keys():
        if len(base.routes) != 1 or len(other.routes) != 1:
            raise ValueError(
                "the runs share no scenario route, and one of them has more than one "
                "route. Compare runs with the same provider and model."
            )
        base_cases = {key[:1]: value for key, value in base_cases.items()}
        other_cases = {key[:1]: value for key, value in other_cases.items()}
        caveats.append(
            f"The runs used different routes, {min(base.routes)} and "
            f"{min(other.routes)}. Model and harness differ together."
        )
    if base.kind == "bench" and base.routes != other.routes:
        caveats.append(
            f"The rounds used different models, {min(base.routes)} and "
            f"{min(other.routes)}. Model and harness differ together."
        )
    paired = sorted(base_cases.keys() & other_cases.keys())
    if not paired:
        raise ValueError(f"{base.name} and {other.name} share no case")
    changes = []
    changed = []
    for key in paired:
        (base_passed, base_runs), (passed, runs) = base_cases[key], other_cases[key]
        change = passed / runs - base_passed / base_runs
        changes.append(change)
        if change:
            row = CaseChange(
                _case_name(key), f"{base_passed}/{base_runs}", f"{passed}/{runs}"
            )
            changed.append((change, row))
    interval, floor = _paired_change(changes)
    verdict = (
        "better"
        if interval[0] > 0
        else "worse"
        if interval[1] < 0
        else "no measurable difference"
    )
    if base.version == other.version and base.is_identified:
        caveats.append(
            "Both sides ran the same Vis version, so the difference shows run-to-run "
            "noise."
        )
    for side, label in ((base, "baseline"), (other, "candidate")):
        if side.version == "unknown":
            caveats.append(
                f"The {label} version is unknown. Its run recorded no commit and no "
                "build."
            )
        elif side.is_dirty:
            caveats.append(
                f"The {label} ran with uncommitted changes, so its commit does not "
                "identify its code."
            )
    unpaired = len(base_cases.keys() ^ other_cases.keys())
    if unpaired:
        caveats.append(
            f"{_count(unpaired, 'case')} ran on one side only. The comparison leaves "
            "them out."
        )
    count = len(paired)
    return Comparison(
        kind=base.kind,
        baseline=base.name,
        candidate=other.name,
        baseline_version=base.version,
        candidate_version=other.version,
        paired_cases=count,
        baseline_rate_percent=_percent(
            sum(passed / runs for passed, runs in map(base_cases.get, paired)), count
        ),
        candidate_rate_percent=_percent(
            sum(passed / runs for passed, runs in map(other_cases.get, paired)), count
        ),
        difference_points=_percent(sum(changes), count),
        difference_ci95=interval,
        verdict=verdict,
        detectable_change_points=floor,
        worse=tuple(
            row
            for change, row in sorted(changed, key=lambda item: item[0])
            if change < 0
        ),
        better=tuple(
            row
            for change, row in sorted(changed, key=lambda item: -item[0])
            if change > 0
        ),
        unpaired=unpaired,
        caveats=tuple(caveats),
    )


def _find_trial(data: dict, task: str) -> dict:
    """Find an attempt by trial name, or the latest scored attempt of a task."""
    trials = data.get("trials") or ()
    if "__" in task:
        matches = [trial for trial in trials if str(trial.get("trial")) == task]
    else:
        matches = [
            trial for trial in trials if _short(trial.get("task")) == _short(task)
        ]
    if not matches:
        raise LookupError(
            f"no attempt of {task!r} in the benchmark summary. Use a task name from "
            "bench() or a trial name such as task__id."
        )
    scored = [trial for trial in matches if trial.get("scored")] or matches
    return max(scored, key=lambda trial: str(trial.get("finished_at") or ""))


def _trial_view(data: dict, task: str, round_name: str | None) -> TrialView:
    trial = _find_trial(data, task)
    name = str(trial.get("trial"))
    short = _short(trial.get("task"))
    limits = _agent_limits()
    note = _task_note(short, _attempt_outcome(trial), trial, limits.get(short))
    result_path = Path(str(trial.get("result_path") or "result.json"))
    directory = _round_dir(round_name) / result_path.parent
    traces = [
        directory / "agent" / file
        for file in ("vis-trace.jsonl.zst", "vis-trace.jsonl.gz")
    ]
    trace = next((path for path in traces if path.is_file()), None)
    info = trial.get("trace") or {}
    calls = Counter(info.get("tool_calls") or {})
    return TrialView(
        task=short,
        trial=name,
        round_name=round_name,
        outcome=note.outcome,
        cause=_cause(trial, _dead_trials(data), limits),
        reward=_number(trial.get("reward")),
        checks=note.checks,
        minutes=note.minutes,
        budget_used=note.budget_used,
        iterations=note.iterations,
        error_type=info.get("vis_error_type") or trial.get("exception_type"),
        tool_calls=tuple(f"{tool} {count}" for tool, count in calls.most_common(8)),
        verifier_tail=_tail(directory / "verifier" / "test-stdout.txt"),
        log_tail=_tail(directory / "agent" / "vis-stderr.log"),
        trace_path=str(trace) if trace else None,
        label=(_labels(round_name).get(name) or {}).get("cause"),
    )


def _save_label(
    data: dict, task: str, cause: str, note: str, round_name: str | None
) -> TrialLabel:
    if cause not in _CAUSES:
        raise ValueError(f"cause must be one of {', '.join(_CAUSES)}, not {cause!r}")
    if len(note) > _NOTE_LIMIT:
        raise ValueError(
            f"the note has {len(note)} characters; the limit is {_NOTE_LIMIT}"
        )
    trial = _find_trial(data, task)
    diagnosed = _cause(trial, _dead_trials(data), _agent_limits())
    if diagnosed == "solved":
        raise ValueError(
            f"{trial.get('trial')} solved its task. Label only failed attempts."
        )
    label = TrialLabel(
        task=_short(trial.get("task")),
        trial=str(trial.get("trial")),
        round_name=round_name,
        cause=cause,
        diagnosed_cause=diagnosed,
        note=note,
        labelled_at=_now(),
    )
    labels = _labels(round_name)
    labels[label.trial] = {
        "task": label.task,
        "cause": cause,
        "note": note,
        "labelled_at": label.labelled_at,
    }
    path = _labels_path(round_name)
    path.parent.mkdir(parents=True, exist_ok=True)
    draft = path.with_name(path.name + ".tmp")
    draft.write_text(json.dumps(labels, indent=2, sort_keys=True) + "\n", "utf-8")
    draft.replace(path)
    return label


def _elapsed(run: EvalRun) -> int:
    start = datetime.fromisoformat(run.started_at)
    end = datetime.fromisoformat(run.ended_at) if run.ended_at else datetime.now(UTC)
    return max(0, int((end - start).total_seconds()))


def _progress(run: EvalRun, run_dir: Path) -> str:
    if run.kind == "scenarios":
        results = Path(run.results_path) if run.results_path else None
        if results and results.is_file():
            report = _scenario_report(results, run.run_id)
            return f"results ready: {report.passed_runs} of {report.total_runs} runs passed"
        done = len(list((run_dir / "traces").glob("*.jsonl")))
        expected = _meta(run_dir).get("expected_runs") or "?"
        if run.state == "running":
            return f"{done} of {expected} scenario runs finished"
        return f"{done} of {expected} scenario runs finished, without results.json"
    if run.state != "running":
        return f"queue {run.state}" + (
            f" with exit code {run.exit_code}" if run.exit_code is not None else ""
        )
    try:
        meta = _meta(run_dir)
        plan = _queue_plan(meta.get("round"), meta.get("attempts"))
    except (RuntimeError, ValueError, KeyError) as error:
        return f"queue running; plan unavailable: {_clip(error, 200)}"
    scored = f"{plan.completed} of {plan.cpu_tasks} CPU tasks scored"
    if plan.attempts > 1:
        scored += f" {plan.attempts} times"
    return f"{scored}, {len(plan.live)} running, {len(plan.pending)} pending"


def _presentation(headline: str, summary: str, content, **options):
    # Activity summaries are one line of at most 512 UTF-8 bytes.
    return vis.ActivityPresentation(
        headline, _one_line(summary), tuple(content), **options
    )


def _scenarios_activity(*, phase, result, kwargs=None, **_):
    if phase != "success":
        return None
    pattern = (kwargs or {}).get("pattern")
    rows = [(s.id, s.lang, _clip(", ".join(s.checks), 80)) for s in result[:30]]
    content = []
    if rows:
        content.append(vis.ActivityTable(("Scenario", "Language", "Checks"), rows))
    if len(result) > len(rows):
        content.append(
            vis.ActivityText(f"Showing {len(rows)} of {len(result)} scenarios.")
        )
    if not result:
        content.append(vis.ActivityText("No scenario matched."))
    summary = _count(len(result), "scenario") + (
        f" matching {_clip(pattern, 40)}" if pattern else ""
    )
    return _presentation("List scenarios", summary, tuple(content))


def _new_scenario_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return _presentation(
        "Create scenario",
        f"{result.id} · {result.lang} · {_count(len(result.checks), 'check')}",
        (
            vis.ActivityText(_clip(result.prompt, 1000)),
            vis.ActivityText(f"Saved in {result.path}"),
        ),
    )


def _run_content(run: EvalRun) -> tuple:
    return (
        vis.ActivityCode(_clip(run.command, 2000), language="bash"),
        vis.ActivityText(f"Run {run.run_id}: {run.state}. Log: {run.log_path}"),
    )


def _start_scenarios_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return _presentation(
        "Start scenario run",
        f"{result.run_id} · {_clip(result.label, 120)}",
        _run_content(result),
    )


def _start_bench_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return _presentation(
        "Start benchmark queue",
        f"{result.run_id} · {_clip(result.label, 120)}",
        _run_content(result),
    )


def _runs_activity(*, phase, result, **_):
    if phase != "success":
        return None
    rows = [
        (
            run.run_id,
            run.kind,
            run.state if run.exit_code is None else f"{run.state} ({run.exit_code})",
            _clip(run.label, 60),
        )
        for run in result[:20]
    ]
    content = (
        (vis.ActivityTable(("Run", "Kind", "State", "Label"), rows),)
        if rows
        else (vis.ActivityText("No evaluation runs yet."),)
    )
    running = sum(run.state == "running" for run in result)
    return _presentation(
        "List evaluation runs",
        f"{_count(len(result), 'run')} · {running} running",
        content,
    )


def _status_activity(*, phase, result, **_):
    if phase != "success":
        return None
    run = result.run
    content = [vis.ActivityText(_clip(result.progress, 400))]
    if result.log_tail:
        content.append(vis.ActivityHeading(f"Last {len(result.log_tail)} log lines"))
        content.append(vis.ActivityCode("\n".join(result.log_tail)[-6000:]))
    else:
        content.append(vis.ActivityText("No log output yet."))
    return _presentation(
        "Check evaluation run",
        f"{run.run_id} · {run.state} · {_clip(result.progress, 100)}",
        tuple(content),
    )


def _stop_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return _presentation(
        "Stop evaluation run", f"{result.run_id} · {result.state}", _run_content(result)
    )


def _interval_text(interval: tuple[float, float] | None) -> str:
    return "unknown" if interval is None else f"{interval[0]:g}-{interval[1]:g}%"


def _report_activity(*, phase, result, **_):
    if phase != "success":
        return None
    rows = [
        (
            item.scenario,
            item.model,
            f"{item.passed}/{item.runs}",
            f"{item.cached_input_percent:g}%",
            "" if item.wall_median_s is None else f"{item.wall_median_s:g}",
        )
        for item in result.results[:20]
    ]
    content = []
    if rows:
        content.append(
            vis.ActivityTable(
                ("Scenario", "Model", "Passed", "Cached input", "Wall s"), rows
            )
        )
    else:
        content.append(vis.ActivityText("The results file has no scenario summaries."))
    if len(result.results) > len(rows):
        content.append(
            vis.ActivityText(f"Showing {len(rows)} of {len(result.results)} results.")
        )
    text = (
        f"Pass rate {result.pass_rate_percent:g}%, 95% interval "
        f"{_interval_text(result.pass_rate_ci95)}."
    )
    if result.detectable_change_points is not None:
        text += (
            " A second run of this size detects a change of "
            f"{result.detectable_change_points:g} points or more."
        )
    if result.pass_all_percent is not None:
        k = result.repeats
        text += (
            f" Over {k} repeats, {result.pass_all_percent:g}% of scenario routes passed "
            f"every repeat (pass^{k}) and {result.pass_any_percent:g}% passed at least "
            f"one (pass@{k})."
        )
    if result.unstable:
        text += f" Mixed results: {_clip(', '.join(result.unstable), 400)}."
    content.append(vis.ActivityText(text))
    failures = [line for item in result.results for line in item.failures]
    if failures:
        content.append(vis.ActivityHeading(_count(len(failures), "failed run")))
        content.append(vis.ActivityText("\n".join(failures[:10])))
    return _presentation(
        "Read scenario results",
        f"{result.passed_runs} of {result.total_runs} runs passed "
        f"({result.pass_rate_percent:g}%, 95% interval "
        f"{_interval_text(result.pass_rate_ci95)})",
        tuple(content),
        verdict="passed" if result.is_passed else "failed",
    )


def _preflight_activity(*, phase, result, **_):
    if phase != "success":
        return None
    rows = [
        (check.name, "OK" if check.is_ok else "Failed", _clip(check.detail, 100))
        for check in result.checks
    ]
    fixes = [f"{check.name}: {check.fix}" for check in result.checks if not check.is_ok]
    content = [vis.ActivityTable(("Check", "Result", "Detail"), rows)]
    if fixes:
        content.append(vis.ActivityText("\n".join(fixes)))
    passed = sum(check.is_ok for check in result.checks)
    state = "ready" if result.is_ready else "not ready"
    return _presentation(
        "Check benchmark setup",
        f"{passed} of {len(result.checks)} checks passed · {state}",
        tuple(content),
        verdict="passed" if result.is_ready else "failed",
    )


def _bench_activity(*, phase, result, **_):
    if phase != "success":
        return None
    rows = [
        (item.outcome, str(item.count), _clip(", ".join(item.tasks), 100))
        for item in result.outcomes
    ]
    content = []
    if rows:
        content.append(vis.ActivityTable(("Outcome", "Tasks", "Names"), rows))
    text = (
        f"Pass rate {result.pass_rate_percent:g}%, 95% interval "
        f"{_interval_text(result.pass_rate_ci95)}. Strict pass rate "
        f"{result.strict_pass_rate_percent:g}%, 95% interval "
        f"{_interval_text(result.strict_pass_rate_ci95)}."
    )
    if result.detectable_change_points is not None:
        text += (
            " A second round of this size detects a change of "
            f"{result.detectable_change_points:g} points or more."
        )
    content.append(vis.ActivityText(text))
    if result.attempts_per_task > 1:
        k = result.attempts_per_task
        text = (
            f"{result.solved_attempts} of {result.scored_attempts} scored attempts "
            f"solved ({result.attempt_pass_rate_percent:g}%, 95% interval "
            f"{_interval_text(result.attempt_pass_rate_ci95)}). With {k} attempts for "
            f"each task, pass@{k} is {result.pass_at_k_percent:g}% and pass^{k} is "
            f"{result.pass_all_k_percent:g}%."
        )
        if result.unstable_tasks:
            text += f" Mixed results: {_clip(', '.join(result.unstable_tasks), 400)}."
        content.append(vis.ActivityText(text))
    per_solved = (
        ""
        if result.cost_per_solved_usd is None
        else f", ${result.cost_per_solved_usd:,.2f} for each solved task"
    )
    content.append(
        vis.ActivityText(
            f"{result.model_attempts} model attempts, {result.agent_hours:g} agent hours, "
            f"{result.input_tokens:,} input tokens ({result.cached_input_percent:g}% "
            f"cached), {result.output_tokens:,} output tokens. Metered-price estimate: "
            f"${result.estimated_metered_cost_usd:,.2f} in total{per_solved}. The "
            "subscription charge is unknown."
        )
    )
    if result.plan:
        plan = result.plan
        times = f" {plan.attempts} times" if plan.attempts > 1 else ""
        text = (
            f"Queue: {plan.completed} of {plan.cpu_tasks} CPU tasks scored{times}, "
            f"{len(plan.pending)} pending, {len(plan.retryable)} retryable, "
            f"{len(plan.gpu_only)} GPU-only."
        )
        if plan.stale_live:
            text += (
                f" Interrupted trials without a queue: {', '.join(plan.stale_live)}."
            )
        content.append(vis.ActivityText(text))
    where = f"Round {result.round_name}: " if result.round_name else ""
    return _presentation(
        "Read benchmark results",
        f"{where}{result.solved_tasks} of {result.scored_tasks} scored tasks solved "
        f"({result.pass_rate_percent:g}%, 95% interval "
        f"{_interval_text(result.pass_rate_ci95)}) · strict "
        f"{result.strict_pass_rate_percent:g}%",
        tuple(content),
    )


def _diagnose_activity(*, phase, result, **_):
    if phase != "success":
        return None
    content = []
    for finding in result.findings:
        content.append(vis.ActivityHeading(_clip(finding.title, 120)))
        content.append(vis.ActivityText(_clip(finding.detail, 600)))
        rows = [
            (
                note.task,
                note.outcome,
                note.checks,
                "" if note.budget_used is None else f"{note.budget_used:.0%}",
            )
            for note in finding.tasks[:12]
        ]
        if rows:
            content.append(
                vis.ActivityTable(("Task", "Outcome", "Tests", "Time used"), rows)
            )
        if len(finding.tasks) > len(rows):
            content.append(
                vis.ActivityText(f"Showing {len(rows)} of {len(finding.tasks)} tasks.")
            )
    if result.estimate:
        content.append(vis.ActivityText(result.estimate))
    if not result.findings:
        content.append(vis.ActivityText("No problem class found in the scored tasks."))
    check = result.label_check
    facts = _count(check.labelled, "labelled attempt")
    if check.agreement_percent is not None:
        facts += f", {check.agreement_percent:g}% agreement"
    if check.kappa is not None:
        facts += f", kappa {check.kappa:g}"
    content.append(vis.ActivityHeading("Label check"))
    content.append(vis.ActivityText(f"{facts}. {check.advice}"))
    if check.disagreements:
        content.append(vis.ActivityText("\n".join(check.disagreements[:10])))
    top = (
        f" · largest: {_clip(result.findings[0].title, 80)}" if result.findings else ""
    )
    kappa = "" if check.kappa is None else f" · label kappa {check.kappa:g}"
    return _presentation(
        "Diagnose benchmark",
        f"{_count(len(result.findings), 'finding')} over {result.scored_tasks} scored "
        f"tasks{top}{kappa}",
        tuple(content),
    )


def _rank_text(rank: int, interval: tuple[float, float] | None, ranks) -> str:
    text = f"{_interval_text(interval)}: rank {rank}"
    return text + (f", between ranks {ranks[0]} and {ranks[1]}" if ranks else "")


def _leaderboard_activity(*, phase, result, **_):
    if phase != "success":
        return None
    rows = [
        (
            str(row.rank),
            _clip(row.agent, 40),
            _clip(row.model, 40),
            f"{row.accuracy_percent:g}%",
            "" if row.cost_per_trial_usd is None else f"${row.cost_per_trial_usd:,.2f}",
            ""
            if row.cost_per_success_usd is None
            else f"${row.cost_per_success_usd:,.2f}",
        )
        for row in result.rows[:15]
    ]
    content = []
    if rows:
        content.append(
            vis.ActivityTable(
                (
                    "Rank",
                    "Agent",
                    "Model",
                    "Accuracy",
                    "Cost per trial",
                    "Cost per success",
                ),
                rows,
            )
        )
    if not rows:
        content.append(vis.ActivityText("The board has no rows."))
    if len(result.rows) > len(rows):
        content.append(
            vis.ActivityText(f"Showing {len(rows)} of {len(result.rows)} rows.")
        )
    summary = f"{_clip(result.title, 60)}: {_count(len(result.rows), 'row')}"
    if result.vis:
        standing = result.vis
        ranks = standing.rank_range
        summary += (
            f" · Vis {standing.pass_rate_percent:g}% would rank "
            f"{standing.rank_by_pass_rate}"
            + (f" (range {ranks[0]}-{ranks[1]})" if ranks else "")
            + f", strict {standing.rank_by_strict_rate}"
        )
        text = (
            f"Pass rate {standing.pass_rate_percent:g}%, 95% interval "
            + _rank_text(standing.rank_by_pass_rate, standing.pass_rate_ci95, ranks)
            + f". Strict pass rate {standing.strict_pass_rate_percent:g}%, 95% "
            "interval "
            + _rank_text(
                standing.rank_by_strict_rate,
                standing.strict_pass_rate_ci95,
                standing.strict_rank_range,
            )
            + "."
        )
        if standing.cost_per_solved_usd is not None:
            text += (
                " Metered-price estimate: "
                f"${standing.cost_per_solved_usd:,.2f} for each solved task."
            )
        content.append(vis.ActivityHeading("Vis"))
        content.append(vis.ActivityText(text))
        content.append(vis.ActivityText("\n".join(standing.caveats)))
    return _presentation("Compare with leaderboard", summary, tuple(content))


def _compare_activity(*, phase, result, **_):
    if phase != "success":
        return None
    low, high = result.difference_ci95
    content = [
        vis.ActivityTable(
            ("Side", "Result", "Version", "Pass rate"),
            (
                (
                    "Baseline",
                    _clip(result.baseline, 60),
                    _clip(result.baseline_version, 80),
                    f"{result.baseline_rate_percent:g}%",
                ),
                (
                    "Candidate",
                    _clip(result.candidate, 60),
                    _clip(result.candidate_version, 80),
                    f"{result.candidate_rate_percent:g}%",
                ),
            ),
        ),
        vis.ActivityText(
            f"{_count(result.paired_cases, 'paired case')}. Difference "
            f"{result.difference_points:+g} points, 95% interval {low:+g} to "
            f"{high:+g}. At this case count, a true change of "
            f"{result.detectable_change_points:g} points or more shows with 80% power."
        ),
    ]
    for title, changes in (("Worse", result.worse), ("Better", result.better)):
        if changes:
            content.append(
                vis.ActivityHeading(f"{title}: {_count(len(changes), 'case')}")
            )
            content.append(
                vis.ActivityTable(
                    ("Case", "Baseline", "Candidate"),
                    [
                        (
                            _clip(item.case, 80),
                            _clip(item.baseline, 20),
                            _clip(item.candidate, 20),
                        )
                        for item in changes[:15]
                    ],
                )
            )
    if result.caveats:
        content.append(vis.ActivityText("\n".join(result.caveats)))
    return _presentation(
        "Compare evaluation results",
        f"{_clip(result.candidate, 40)} against {_clip(result.baseline, 40)}: "
        f"{result.verdict} ({result.difference_points:+g} points)",
        tuple(content),
        verdict="failed" if result.verdict == "worse" else "passed",
    )


def _trial_activity(*, phase, result, **_):
    if phase != "success":
        return None
    facts = [
        ("Outcome", result.outcome),
        ("Cause", result.cause),
        ("Tests", result.checks),
    ]
    optional = (
        ("Reward", None if result.reward is None else f"{result.reward:g}"),
        ("Minutes", None if result.minutes is None else f"{result.minutes:g}"),
        (
            "Time used",
            None if result.budget_used is None else f"{result.budget_used:.0%}",
        ),
        ("Iterations", None if result.iterations is None else str(result.iterations)),
        ("Error", result.error_type),
        ("Tools", _clip(", ".join(result.tool_calls), 200) or None),
        ("Label", result.label),
    )
    facts += [(name, value) for name, value in optional if value]
    rows = [(name, _clip(value, 200)) for name, value in facts]
    content = [vis.ActivityTable(("Fact", "Value"), rows)]
    if result.verifier_tail:
        content.append(vis.ActivityHeading("Verifier output"))
        content.append(vis.ActivityCode("\n".join(result.verifier_tail)[-4000:]))
    if result.log_tail:
        content.append(vis.ActivityHeading("Vis log"))
        content.append(vis.ActivityCode("\n".join(result.log_tail)[-4000:]))
    if result.trace_path:
        content.append(vis.ActivityText(f"Trace: {result.trace_path}"))
    return _presentation(
        "Read benchmark trial",
        f"{result.trial} · {result.outcome} · {result.cause} · tests {result.checks}",
        tuple(content),
    )


def _label_activity(*, phase, result, **_):
    if phase != "success":
        return None
    relation = (
        "agrees with" if result.cause == result.diagnosed_cause else "differs from"
    )
    content = [
        vis.ActivityText(
            f"The label {result.cause} {relation} the diagnose() cause "
            f"{result.diagnosed_cause}."
        )
    ]
    if result.note:
        content.append(vis.ActivityText(_clip(result.note, 600)))
    where = f"round {result.round_name}" if result.round_name else "the main round"
    content.append(vis.ActivityText(f"Saved in the labels.json of {where}."))
    return _presentation(
        "Label benchmark trial",
        f"{result.trial}: {result.cause} · diagnose() {result.diagnosed_cause}",
        tuple(content),
    )


class Evals:
    """Run and read Vis evaluations: e2e scenarios and Terminal-Bench on Harbor.

    Reads are free. run_scenarios() and run_bench() make paid model calls in a
    background process. Use them only on request, and read the result with status(),
    report() and bench(). compare() judges a change between two results. label() only
    writes the labels.json of a benchmark round.
    """

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="List scenarios", show_start=False, render=_scenarios_activity
        ),
    )
    def scenarios(self, pattern: str | None = None) -> tuple[Scenario, ...]:
        """List the e2e scenarios, sorted by id.

        `pattern` is a case-insensitive regular expression that must match the id or
        the prompt. Without it, every scenario is listed.
        """
        found = [_scenario(path) for path in _scenario_dirs().values()]
        if pattern:
            regex = re.compile(pattern, re.IGNORECASE)
            found = [s for s in found if regex.search(s.id) or regex.search(s.prompt)]
        return tuple(found)

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Create scenario", show_start=False, render=_new_scenario_activity
        ),
    )
    def new_scenario(
        self,
        scenario_id: str,
        prompt: str,
        files: dict[str, str] | None = None,
        want: dict[str, list[str]] | None = None,
        wantnot: dict[str, list[str]] | None = None,
        want_answer: list[str] | None = None,
        lang: str = "python",
        timeout_s: int | None = None,
    ) -> Scenario:
        """Create e2e/scenarios/<scenario_id>/ with scenario.json and its input files.

        `files` maps relative paths to the text of the input files. The scenario needs
        an oracle: `want` (required substrings for each file), `want_answer` (required
        substrings in the final answer) or both. `wantnot` lists forbidden substrings.
        Add other guards from e2e/README.md to scenario.json later. Refuses an existing
        id, an id that is not lowercase words joined by hyphens, and a path outside
        the scenario. Does not run the scenario.
        """
        if not _SCENARIO_ID.fullmatch(scenario_id or ""):
            raise ValueError("scenario_id must be lowercase words joined by hyphens")
        if not prompt.strip():
            raise ValueError("prompt must not be blank")
        if not want and not want_answer:
            raise ValueError("give an oracle: want, want_answer or both")
        if not re.fullmatch(r"[a-z]+", lang or ""):
            raise ValueError("lang must be one lowercase word, such as python")
        if timeout_s is not None and timeout_s < 1:
            raise ValueError("timeout_s must be positive")
        directory = SCENARIOS / scenario_id
        if directory.exists():
            raise FileExistsError(f"scenario {scenario_id} already exists")
        targets = {}
        for name, text in (files or {}).items():
            relative = Path(name)
            if relative.is_absolute() or not relative.parts or ".." in relative.parts:
                raise ValueError(f"file path must stay inside the scenario: {name}")
            targets[directory / "files" / relative] = text
        data: dict = {
            "lang": lang,
            "prompt": prompt.strip(),
            "want": want or {},
            "wantnot": wantnot or {},
        }
        if want_answer:
            data["want_answer"] = list(want_answer)
        if timeout_s is not None:
            data["timeout_s"] = timeout_s
        directory.mkdir(parents=True)
        for path, text in targets.items():
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text, encoding="utf-8")
        (directory / "scenario.json").write_text(
            json.dumps(data, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
        )
        return _scenario(directory)

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Start scenario run",
            show_start=False,
            render=_start_scenarios_activity,
        ),
    )
    def run_scenarios(
        self,
        ids: list[str],
        models: list[str] | None = None,
        provider: str | None = None,
        repeats: int = 1,
        workers: int = 2,
        reasoning_effort: str | None = None,
        timeout_s: int | None = None,
        native_bin: str | None = None,
        python_native_path: str | None = None,
    ) -> EvalRun:
        """Start e2e/run.py for the named scenarios in a background process.

        Makes paid model calls: one agent run for each scenario, model and repeat.
        Without `models` and `provider`, the runner's default route applies.
        `native_bin` is the absolute path of a raw native executable, not the
        vis-agent launcher, and `python_native_path` is its Python sidecar. Each run
        writes traces and results.json into its own directory. It records the Vis
        commit and the sha256 of `native_bin`, so that compare() can name the versions.
        Returns at once; read progress with status() and results with report().
        """
        known = _scenario_dirs()
        if not ids:
            raise ValueError("name at least one scenario id; scenarios() lists them")
        if unknown := [scenario for scenario in ids if scenario not in known]:
            raise ValueError(f"unknown scenarios: {', '.join(unknown)}")
        if repeats < 1 or workers < 1:
            raise ValueError("repeats and workers must be positive")
        if timeout_s is not None and timeout_s < 1:
            raise ValueError("timeout_s must be positive")
        for path in (native_bin, python_native_path):
            if path and not (Path(path).is_absolute() and Path(path).is_file()):
                raise ValueError(f"not an absolute path to a file: {path}")
        python = shutil.which("python3")
        if not python:
            raise FileNotFoundError("python3 is not on PATH")
        run_id, run_dir = _new_run("scenarios")
        traces = run_dir / "traces"
        env = {
            **os.environ,
            "VIS_E2E_TRACES": str(traces),
            "VIS_E2E_REPEATS": str(repeats),
            "VIS_E2E_WORKERS": str(workers),
        }
        optional = {
            "VIS_PROVIDER": provider,
            "VIS_MODELS": ",".join(models) if models else None,
            "VIS_REASONING_EFFORT": reasoning_effort,
            "VIS_E2E_TIMEOUT": str(timeout_s) if timeout_s else None,
            "VIS_E2E_NATIVE_BIN": native_bin,
            "VIS_PYTHON_NATIVE_PATH": python_native_path,
        }
        env.update({name: value for name, value in optional.items() if value})
        route = f"{provider or 'default provider'} / {', '.join(models or ['default model'])}"
        label = f"{_count(len(ids), 'scenario')} on {route}" + (
            f", {repeats} repeats" if repeats > 1 else ""
        )
        return _launch(
            run_id,
            run_dir,
            kind="scenarios",
            label=label,
            argv=[python, str(E2E_RUNNER), *ids],
            cwd=REPO,
            env=env,
            results_path=traces / "results.json",
            expected_runs=len(ids) * len(models or [None]) * repeats,
            build_sha256=_sha256(Path(native_bin)) if native_bin else None,
        )

    @vis.method(
        tag="verification",
        activity=vis.Activity(
            label="Check benchmark setup", show_start=True, render=_preflight_activity
        ),
    )
    def preflight(self, round_name: str | None = None) -> Preflight:
        """Check that a Terminal-Bench queue can start and produce valid results.

        Checks uv, Podman and its machine, the Docker socket, a Compose v2 provider,
        the Linux amd64 bundle against the provenance.json of the round, the dataset,
        the API key (presence only), zstd, free disk on the host and in the Podman VM,
        and that no other queue runs. `round_name` names a round under rounds/; None
        is the main round. A new round pins the bundle when its queue starts. Changes
        nothing.
        """
        return _preflight(round_name)

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Start benchmark queue", show_start=True, render=_start_bench_activity
        ),
    )
    def run_bench(
        self,
        max_tasks: int | None = None,
        retry_tasks: list[str] | None = None,
        job_prefix: str | None = None,
        attempts: int | None = None,
        round_name: str | None = None,
    ) -> EvalRun:
        """Start the Terminal-Bench queue (run_suite.py) in a background process.

        Makes paid model calls for hours. Without `max_tasks`, the queue runs every
        pending CPU task. `retry_tasks` reruns failed model attempts first; bench()
        lists them in `plan.retryable`. `round_name` keeps a separate round in
        rounds/<round_name>/. Its first queue pins the bundle, the model and
        `attempts`, the scored attempts for each task. More than one attempt needs a
        named round. Refuses to start when preflight() fails, when a queue already
        runs, or, before the first scored attempt of a round, without max_tasks=1.
        Returns at once; read progress with status() and the score with bench().
        """
        if max_tasks is not None and max_tasks < 1:
            raise ValueError("max_tasks must be positive")
        if attempts is not None and attempts < 1:
            raise ValueError("attempts must be positive")
        if job_prefix is not None and not _SCENARIO_ID.fullmatch(job_prefix):
            raise ValueError("job_prefix must be lowercase words joined by hyphens")
        if round_name is None and (attempts or 1) > 1:
            raise ValueError(
                "more than one attempt for each task needs a named round: "
                f"run_bench(attempts={attempts}, round_name=...)"
            )
        pins = _provenance(round_name)
        is_new = round_name is not None and not pins
        count = (attempts or 1) if is_new else int(pins.get("attempts") or 1)
        if attempts is not None and attempts != count:
            raise ValueError(
                f"round {round_name} pins {count} attempts for each task; start a new "
                f"round for {attempts}"
            )
        active = [
            run.run_id
            for run in map(_run, _run_dirs())
            if run.kind == "bench" and run.state == "running"
        ]
        if active:
            raise RuntimeError(f"a benchmark queue already runs: {active[-1]}")
        check = _preflight(round_name)
        if not check.is_ready:
            failed = "; ".join(
                f"{item.name}: {item.detail}. {item.fix}"
                for item in check.checks
                if not item.is_ok
            )
            raise RuntimeError(f"preflight failed: {failed}")
        plan = _queue_plan(round_name, count)
        if plan.scored == 0 and max_tasks != 1:
            raise ValueError(
                "no task has a scored attempt in this round yet: start a canary with "
                "max_tasks=1, read it with bench(), then continue"
            )
        if invalid := sorted(set(retry_tasks or ()) - set(plan.retryable)):
            raise ValueError(
                f"not retryable: {', '.join(invalid)}; retryable: "
                f"{', '.join(plan.retryable) or 'none'}"
            )
        digest = _sha256(_bundle())
        if is_new:
            _start_round(round_name, count, plan.model, digest)
        argv = ["uv", "run", "--locked", "python", "run_suite.py"]
        if round_name is not None:
            argv += ["--jobs", str(_round_dir(round_name) / "jobs")]
        argv += ["--attempts", str(count)]
        if max_tasks is not None:
            argv += ["--max-tasks", str(max_tasks)]
        for task in retry_tasks or ():
            argv += ["--retry-task", task]
        if job_prefix:
            argv += ["--job-prefix", job_prefix]
        tasks = max_tasks or len(plan.pending) + len(retry_tasks or ())
        label = f"{_count(tasks, 'Terminal-Bench task')} on {plan.model}"
        if count > 1:
            label += f", {count} attempts each"
        if round_name is not None:
            label += f", round {round_name}"
        run_id, run_dir = _new_run("bench")
        return _launch(
            run_id,
            run_dir,
            kind="bench",
            label=label,
            argv=argv,
            cwd=BENCH,
            env=_bench_env(),
            build_sha256=digest,
            round_name=round_name,
            attempts=count,
        )

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="List evaluation runs", show_start=False, render=_runs_activity
        ),
    )
    def runs(self, limit: int = 20) -> tuple[EvalRun, ...]:
        """List the runs that this extension started, newest first.

        Includes finished, stopped and lost runs. `limit` caps the count.
        """
        return tuple(_run(path) for path in reversed(_run_dirs()[-max(1, limit) :]))

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="Check evaluation run", show_start=True, render=_status_activity
        ),
    )
    def status(self, run_id: str | None = None) -> RunStatus:
        """Read one run's state, progress and log tail.

        Without `run_id`, reads the newest run. A unique prefix of the id is enough.
        For a running queue, progress comes from the queue plan, which takes a few
        seconds.
        """
        run_dir = _resolve_run(run_id)
        run = _run(run_dir)
        return RunStatus(
            run, _progress(run, run_dir), _elapsed(run), _tail(Path(run.log_path))
        )

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Stop evaluation run", show_start=True, render=_stop_activity
        ),
    )
    def stop(self, run_id: str) -> EvalRun:
        """Stop a running run and its child processes; keep its log and results.

        Sends SIGTERM to the run's process group and SIGKILL after 15 seconds. A
        stopped queue can leave task containers in the Podman machine; check them with
        `podman ps`. A run that is not running is returned unchanged.
        """
        run_dir = _resolve_run(run_id)
        run = _run(run_dir)
        if run.state != "running":
            return run
        (run_dir / "stopped").write_text(_now() + "\n", encoding="utf-8")
        for sig in (signal.SIGTERM, signal.SIGKILL):
            try:
                os.killpg(run.pid, sig)
            except ProcessLookupError:
                break
            deadline = time.monotonic() + 15
            while time.monotonic() < deadline and _alive(run.run_id, run.pid):
                time.sleep(0.2)
            if not _alive(run.run_id, run.pid):
                break
        return _run(run_dir)

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="Read scenario results", show_start=False, render=_report_activity
        ),
    )
    def report(
        self, run_id: str | None = None, results_path: str | None = None
    ) -> ScenarioReport:
        """Read the results of a scenario run.

        Without arguments, reads the newest scenario run. `results_path` reads any
        results.json that e2e/run.py wrote, for example an older trace directory.
        Fails while the run has not written results yet.
        """
        if results_path:
            return _scenario_report(Path(results_path), None)
        run = _run(_resolve_run(run_id, "scenarios"))
        path = Path(run.results_path or "")
        if not path.is_file():
            raise FileNotFoundError(
                f"run {run.run_id} is {run.state} and has no results yet"
            )
        return _scenario_report(path, run.run_id)

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="Read benchmark results", show_start=True, render=_bench_activity
        ),
    )
    def bench(self, refresh: bool = True, round_name: str | None = None) -> BenchReport:
        """Read the Terminal-Bench score, outcomes, usage and queue plan of a round.

        With `refresh`, first rebuilds runs/summary.json with summarize.py, which can
        take a minute. `round_name` names a round under rounds/; None is the main
        round. Each task counts once, by its latest scored attempt. The attempt fields
        count every scored attempt. Raw traces and jobs stay local and private.
        """
        data = _summary(refresh, round_name)
        try:
            plan = _queue_plan(round_name)
        except (RuntimeError, ValueError, KeyError):
            plan = None
        return _bench_report(data, plan, round_name)

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="Diagnose benchmark", show_start=False, render=_diagnose_activity
        ),
    )
    def diagnose(
        self, refresh: bool = False, round_name: str | None = None
    ) -> Diagnosis:
        """Find where the benchmark loses tasks, largest lever first.

        Groups Vis errors, early stops, near misses, agent timeouts, attempts that
        repeated one error until the end, and unscored attempts. Time use comes from
        each task's agent time limit in the dataset. Reads runs/summary.json of the
        round; with `refresh`, rebuilds it first. `label_check` compares these causes
        with your label() causes. With fewer than 30 labels or a kappa below 0.6,
        review the findings yourself.
        """
        return _diagnosis(_summary(refresh, round_name), _labels(round_name))

    @vis.method(
        tag="external",
        activity=vis.Activity(
            label="Compare with leaderboard",
            show_start=True,
            render=_leaderboard_activity,
        ),
    )
    def leaderboard(
        self,
        package: str = "terminal-bench/terminal-bench",
        name: str = "4-0-0",
        round_name: str | None = None,
    ) -> Leaderboard:
        """Read a public Harbor Hub leaderboard and place a local Vis round on it.

        Sends one unauthenticated request for each page. The Vis standing is present
        only when the summary.json of the round covers the same dataset version. Rows
        report several trials per task. Read the rank range and the caveats of the
        standing before you compare.
        """
        summary = _summary_path(round_name)
        rows: list[LeaderboardRow] = []
        board: dict = {}
        page = pages = 1
        while page <= min(pages, 50):
            data = _post_json(
                LEADERBOARD_URL,
                {"package": package, "name": name, "page": page, "page_size": 100},
            )
            board = data.get("leaderboard") or board
            rows.extend(_leaderboard_row(raw) for raw in data.get("rows") or ())
            pages = int((data.get("pagination") or {}).get("total_pages") or 1)
            page += 1
        rows.sort(key=lambda row: (row.rank or 10**6, -row.accuracy_percent))
        standing = None
        if summary.is_file():
            standing = _standing(
                tuple(rows), _summary(False, round_name), package, name, round_name
            )
        return Leaderboard(
            package, name, str(board.get("title") or name), tuple(rows), standing
        )

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="Compare evaluation results",
            show_start=False,
            render=_compare_activity,
        ),
    )
    def compare(self, baseline: str, candidate: str) -> Comparison:
        """Compare two evaluation results case by case, with a paired 95% interval.

        Each side is a scenario run id or its unique prefix, `bench` for the main
        benchmark round, or `bench:<round>` for a named round. Scenario runs pair by
        scenario and route, and benchmark rounds pair by task. Reads the saved results
        as they are; run bench() first to refresh a round. The verdict is better or
        worse only when the interval excludes zero.
        """
        return _compare(baseline, candidate)

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="Read benchmark trial", show_start=False, render=_trial_activity
        ),
    )
    def trial(self, task: str, round_name: str | None = None) -> TrialView:
        """Read one benchmark attempt, with the facts that you need to label it.

        `task` is a task name, which reads its latest scored attempt, or a trial name
        (`<task>__<id>`). Reads the summary.json of the round and the job files of the
        attempt. Credentials in the log lines are redacted. Changes nothing.
        """
        return _trial_view(_summary(False, round_name), task, round_name)

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Label benchmark trial", show_start=False, render=_label_activity
        ),
    )
    def label(
        self, task: str, cause: str, note: str = "", round_name: str | None = None
    ) -> TrialLabel:
        """Record your cause of one failed benchmark attempt in labels.json.

        `cause` is dead_tools, vis_errors, agent_timeouts, early_stops, unscored or
        other. Read the attempt with trial() first. `task` names it as in trial(). A
        new label of the same attempt replaces the old one. diagnose() compares the
        labels with its own causes in `label_check`.
        """
        return _save_label(_summary(False, round_name), task, cause, note, round_name)


evals = Evals()

PROMPT = """evals surface active — evaluate Vis with e2e scenarios and Terminal-Bench on Harbor:
  evals.scenarios(pattern=None)                    list e2e scenarios
  evals.new_scenario(id, prompt, files, want=...)  add a scenario under e2e/scenarios
  evals.run_scenarios(ids, models=None, repeats=1) start a paid scenario run in the background
  evals.preflight(round_name=None)                 check Podman, bundle, dataset, key and disk
  evals.run_bench(max_tasks=None, attempts=None, round_name=None)   start the paid queue
  evals.runs() · evals.status(run_id=None) · evals.stop(run_id)   background runs
  evals.report(run_id=None)                        scenario results with a 95% interval
  evals.bench(round_name=None) · evals.diagnose(round_name=None)   score, then lost tasks
  evals.compare(baseline, candidate)               paired comparison of two runs or rounds
  evals.trial(task) · evals.label(task, cause)     read one attempt, record its real cause
  evals.leaderboard(round_name=None)               Harbor Hub board and where Vis stands
Start paid runs only on request. Judge a change with compare(), not with two pass rates.
Results are typed frozen objects."""


def _slash_evals(ctx: dict) -> dict:
    found = [_run(path) for path in reversed(_run_dirs()[-5:])]
    if not found:
        return vis.ok(
            "No evaluation runs yet. Ask Vis to run scenarios or the benchmark."
        )
    return vis.ok(
        "\n".join(
            f"{run.run_id}  {run.state}"
            + ("" if run.exit_code is None else f" (exit {run.exit_code})")
            + f"  {run.label}"
            for run in found
        )
    )


vis.register_extension(
    vis.Extension(
        name="evals",
        description="Run and read Vis evaluations: e2e scenarios and Terminal-Bench on Harbor.",
        version="0.1.0",
        kind="integration",
        alias="evals",
        symbols=[vis.Symbol(evals, name="evals", tag="observation")],
        prompt=PROMPT,
        slash_commands=[
            vis.SlashCommand(
                "evals",
                _slash_evals,
                doc="Show the five newest evaluation runs.",
                usage="/evals",
            )
        ],
        env=[API_KEY_ENV, "DOCKER_HOST", "PODMAN_COMPOSE_PROVIDER", "VIS_EVALS_HOME"],
    )
)
