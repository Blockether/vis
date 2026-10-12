"""Run the Vis e2e scenarios as one Harbor job and write the e2e results.

Run it with ``uv run --project e2e python -m vis_harbor.run [scenario-id ...]``.
Without ids, it runs every scenario. Each trial makes paid model calls.

These variables set a run:

    VIS_E2E_TRACES    directory for traces and results.json (default /tmp/vis_e2e/traces)
    VIS_E2E_REPEATS   runs of each scenario on each model (default 1)
    VIS_E2E_WORKERS   trials that run at the same time (default 5)
    VIS_PROVIDER      provider of the models (default zai-coding-plan)
    VIS_MODELS        comma-separated models; a scenario passes only if each model passes
    VIS_MODEL         the model when VIS_MODELS is not set (default glm-5.3-flash)
    VIS_E2E_TIMEOUT   agent timeout in seconds for each scenario

The agent reads ``VIS_REASONING_EFFORT``, ``VIS_E2E_NATIVE_BIN``, ``VIS_E2E_CLOJURE``
and ``VIS_PYTHON_NATIVE_PATH`` itself. The Harbor dataset and jobs are in the
``harbor`` directory next to the trace directory. When a trial finishes, its trace
goes to ``<traces>/<id>[__<model>][__run<n>].jsonl``. At the end,
``<traces>/results.json`` holds the grade record of each run and the summaries.
"""

from __future__ import annotations

import collections
import json
import os
import shutil
import statistics
import subprocess
import sys
import time
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from vis_harbor import dataset
from vis_harbor.agent import DEFAULT_MODEL, DEFAULT_PROVIDER, TRACE_FILE
from vis_harbor.environment import VIS_ROOT
from vis_harbor.grade import GRADE_FILE

AGENT = "vis_harbor.agent:VisSourceAgent"
ENVIRONMENT = "vis_harbor.environment:HostEnvironment"
DEFAULT_TRACES = "/tmp/vis_e2e/traces"
DEFAULT_WORKERS = 5
RESULTS_FILE = "results.json"
POLL_SEC = 2.0


@dataclass(frozen=True, slots=True)
class Settings:
    """The settings of one run."""

    traces: Path
    repeats: int
    workers: int
    provider: str
    models: tuple[str, ...]
    timeout_sec: int | None
    reasoning_effort: str | None
    native_bin: str | None


def _positive_int(env: Mapping[str, str], name: str, default: int | None) -> int | None:
    raw = (env.get(name) or "").strip()
    if not raw:
        return default
    try:
        value = int(raw)
    except ValueError:
        value = 0
    if value < 1:
        raise ValueError(f"{name} must be a positive integer")
    return value


def read_settings(env: Mapping[str, str]) -> Settings:
    """Read the settings from ``env``. An invalid number is a ``ValueError``."""
    model = (env.get("VIS_MODEL") or "").strip() or DEFAULT_MODEL
    models = env.get("VIS_MODELS") or model
    return Settings(
        traces=Path(env.get("VIS_E2E_TRACES") or DEFAULT_TRACES),
        repeats=_positive_int(env, "VIS_E2E_REPEATS", 1),
        workers=_positive_int(env, "VIS_E2E_WORKERS", DEFAULT_WORKERS),
        provider=(env.get("VIS_PROVIDER") or "").strip() or DEFAULT_PROVIDER,
        models=tuple(name.strip() for name in models.split(",") if name.strip()),
        timeout_sec=_positive_int(env, "VIS_E2E_TIMEOUT", None),
        reasoning_effort=(env.get("VIS_REASONING_EFFORT") or "").strip() or None,
        native_bin=env.get("VIS_E2E_NATIVE_BIN") or None,
    )


def harbor_executable() -> str:
    """Return the ``harbor`` command of this Python environment, else the one on PATH."""
    local = Path(sys.executable).parent / "harbor"
    if local.is_file():
        return str(local)
    found = shutil.which("harbor")
    if not found:
        raise FileNotFoundError("harbor is not installed: run uv sync --project e2e")
    return found


def harbor_command(
    settings: Settings, dataset_dir: Path, jobs_dir: Path, job_name: str
) -> list[str]:
    """Return the ``harbor run`` command for all scenarios and models of a run."""
    command = [
        harbor_executable(),
        "run",
        "--path",
        str(dataset_dir),
        "--agent",
        AGENT,
        "--env",
        ENVIRONMENT,
    ]
    for model in settings.models:
        command += ["--model", f"{settings.provider}/{model}"]
    return [
        *command,
        "--jobs-dir",
        str(jobs_dir),
        "--job-name",
        job_name,
        "--n-concurrent",
        str(settings.workers),
        "--n-attempts",
        str(settings.repeats),
        "--quiet",
    ]


def trace_tag(settings: Settings, scenario_id: str, model: str, repeat: int) -> str:
    """Return the trace file name of one run, without ``.jsonl``."""
    tag = scenario_id + (f"__{model}" if len(settings.models) > 1 else "")
    return tag + (f"__run{repeat}" if settings.repeats > 1 else "")


@dataclass(frozen=True, slots=True)
class Trial:
    """One finished Harbor trial."""

    directory: Path
    scenario_id: str
    model: str
    finished_at: str
    exception_type: str | None


def finished_trials(job_dir: Path) -> list[Trial]:
    """Return the finished trials of the Harbor job in ``job_dir``, in finish order."""
    trials = []
    for result_path in job_dir.glob("*/result.json"):
        try:
            result = json.loads(result_path.read_text())
        except (OSError, ValueError):
            continue
        if not result.get("finished_at"):
            continue
        agent = (result.get("config") or {}).get("agent") or {}
        route = agent.get("model_name") or ""
        exception = result.get("exception_info") or {}
        trials.append(
            Trial(
                directory=result_path.parent,
                scenario_id=result.get("task_name") or "",
                model=route.partition("/")[2] or route,
                finished_at=str(result["finished_at"]),
                exception_type=exception.get("exception_type"),
            )
        )
    return sorted(trials, key=lambda trial: (trial.finished_at, trial.directory.name))


def failure_record(
    scenario: dataset.Scenario, provider: str, model: str, message: str
) -> dict[str, Any]:
    """Return the record of a run without a grade. The run failed."""
    return {
        "id": scenario.scenario_id,
        "lang": scenario.data.get("lang"),
        "provider": provider,
        "model": model,
        "repeat": 1,
        "converged": False,
        "correct": False,
        "errors": 1,
        "err_msgs": [message],
        "wall": 0.0,
        "forms": 0,
        "provider_calls": 0,
        "tokens": None,
        "token_errors": [],
        "cache_usage": None,
        "max_form_output_chars": 0,
        "total_output_chars": 0,
        "surfaced_errors": 0,
        "expected_errors": 0,
        "expected_activity_failures": 0,
        "activity_successes": 0,
        "activity_failures": 0,
        "caught_activity_failures": 0,
        "unscoped_activity_failures": 0,
        "incomplete_activities": 0,
        "discovery": {},
        "helper_reuse": {},
        "measurement": bool(scenario.data.get("measurement")),
        "used_patch": False,
        "edit_path": "-",
        "detail": [message],
        "behavior": [],
        "evidence": [],
        "passed": False,
        "guards": {},
    }


class Collector:
    """Keep the trace and the grade record of each finished trial of a job."""

    def __init__(
        self,
        settings: Settings,
        scenarios: Sequence[dataset.Scenario],
        job_dir: Path,
    ) -> None:
        self.settings = settings
        self.scenarios = {scenario.scenario_id: scenario for scenario in scenarios}
        self.job_dir = job_dir
        self.records: dict[Path, dict[str, Any]] = {}
        self._runs: collections.Counter[tuple[str, str]] = collections.Counter()

    def collect(self) -> None:
        """Copy the trace and read the grade of each newly finished trial."""
        for trial in finished_trials(self.job_dir):
            scenario = self.scenarios.get(trial.scenario_id)
            if trial.directory in self.records or scenario is None:
                continue
            self._runs[trial.scenario_id, trial.model] += 1
            repeat = self._runs[trial.scenario_id, trial.model]
            self.settings.traces.mkdir(parents=True, exist_ok=True)
            tag = trace_tag(self.settings, trial.scenario_id, trial.model, repeat)
            trace = trial.directory / "agent" / TRACE_FILE
            target = self.settings.traces / f"{tag}.jsonl"
            if trace.is_file():
                shutil.copyfile(trace, target)
            else:
                target.write_text("")
            try:
                record = json.loads(
                    (trial.directory / "verifier" / GRADE_FILE).read_text()
                )
            except (OSError, ValueError):
                cause = trial.exception_type or "no verifier result"
                message = f"Harbor trial has no grade: {cause}"
                record = failure_record(
                    scenario, self.settings.provider, trial.model, message
                )
            record["provider"] = record.get("provider") or self.settings.provider
            record["model"] = record.get("model") or trial.model
            record["repeat"] = repeat
            self.records[trial.directory] = record

    def results(self) -> list[dict[str, Any]]:
        """Return all records, with a failure record for each run without a trial."""
        results = list(self.records.values())
        for scenario_id, scenario in self.scenarios.items():
            for model in self.settings.models:
                for repeat in range(
                    self._runs[scenario_id, model] + 1, self.settings.repeats + 1
                ):
                    record = failure_record(
                        scenario,
                        self.settings.provider,
                        model,
                        "Harbor did not finish this trial",
                    )
                    record["repeat"] = repeat
                    results.append(record)
        return sorted(results, key=lambda r: (r["id"], r["model"], r["repeat"]))


def run_job(command: Sequence[str], collector: Collector) -> int:
    """Run Harbor, collect each trial when it finishes and return the exit code."""
    with subprocess.Popen(command, cwd=VIS_ROOT, stdin=subprocess.DEVNULL) as process:
        while process.poll() is None:
            collector.collect()
            time.sleep(POLL_SEC)
    collector.collect()
    return process.returncode


def summarize_results(results):
    """Summarize all repetitions, including failures, without averaging cache ratios."""
    groups = collections.defaultdict(list)
    for result in results:
        groups[(result["id"], result["provider"], result["model"])].append(result)
    summaries = []
    for (scenario, provider, model), rows in sorted(groups.items()):
        valid = [row for row in rows if row["tokens"] and not row["token_errors"]]
        summary = {
            "id": scenario,
            "provider": provider,
            "model": model,
            "runs": len(rows),
            "passed": sum(
                row["converged"] and row["correct"] and row["errors"] == 0
                for row in rows
            ),
            "measurement": any(row.get("measurement") for row in rows),
            "behavior_passed": sum(not row.get("behavior") for row in rows),
            "token_samples": len(valid),
        }
        totals = {
            key: sum(row["tokens"][key] for row in valid)
            for key in ("input", "cached", "uncached", "output")
        }
        summary["token_totals"] = totals
        summary["cached_input_percent"] = (
            round(100 * totals["cached"] / totals["input"], 2)
            if totals["input"]
            else 0.0
        )
        for key in (
            "wall",
            "forms",
            "provider_calls",
            "max_form_output_chars",
            "total_output_chars",
            "input",
            "cached",
            "uncached",
            "output",
        ):
            values = (
                [row["tokens"][key] for row in valid]
                if key in totals
                else [row[key] for row in rows]
            )
            summary[key] = (
                {
                    "min": min(values),
                    "median": statistics.median(values),
                    "max": max(values),
                }
                if values
                else None
            )
        summaries.append(summary)
    return summaries


def print_table(
    settings: Settings, results: list[dict[str, Any]], summaries: list[dict[str, Any]]
) -> bool:
    """Print the runs and the summaries. Return True if each scenario passed the gate."""
    mw = max(8, max((len(m) for m in settings.models), default=8))
    hdr = f"{'scenario':<18}{'model':<{mw}} {'lang':<11}{'conv':<5}{'ok':<4}{'err':<4}{'path':<14}{'forms':<6}{'sec':<6}"
    print(hdr)
    print("-" * len(hdr))
    nclean = nfast = 0
    for r in results:
        nfast += r["used_patch"]
        nclean += r["errors"] == 0
        print(
            f"{r['id']:<18}{r['model']:<{mw}} {r['lang']:<11}"
            f"{'✓' if r['converged'] else '✗':<5}{'✓' if r['correct'] else '✗':<4}"
            f"{r['errors']:<4}{r['edit_path']:<14}{r['forms']:<6}{r['wall']:<6}"
        )
        for d in r["detail"]:
            print(f"    ! {d}")
        for b in r["behavior"]:
            print(f"    ~ {b}")
        for item in r["evidence"]:
            print(f"    · {item}")
        for e in r["err_msgs"]:
            print(f"    err: {e[:140]}")
    n = len(results)
    # CROSS-VALIDATION GATE: a scenario passes only if EVERY model converged,
    # produced correct output, and had no loop/tool errors. `PATCH(fast)`
    # remains a performance/adherence metric because some scenarios legitimately
    # answer from the REPL instead of editing a file. A scenario marked
    # `measurement` reports its behavior rate instead of gating on it: a model's
    # habits vary between runs, while its answer, errors and edits do not.
    by_scn = {}
    for r in results:
        by_scn.setdefault(r["id"], []).append(
            r["converged"] and r["correct"] and r["errors"] == 0
        )
    gated = sum(1 for oks in by_scn.values() if all(oks))
    ok_clean = sum(
        1 for r in results if r["converged"] and r["correct"] and r["errors"] == 0
    )
    print("-" * len(hdr))
    print(
        f"RUNS converged+correct+clean {ok_clean}/{n} "
        f"| NO-ERROR {nclean}/{n} | PATCH(fast) {nfast}/{n}"
    )
    print(
        f"GATE (scenario passes iff ALL {len(settings.models)} model(s) pass cleanly): {gated}/{len(by_scn)}"
    )
    for summary in summaries:
        print(
            f"SUMMARY {summary['id']} {summary['provider']}/{summary['model']}: {summary['passed']}/{summary['runs']} passed; token samples={summary['token_samples']}"
        )
        if summary["measurement"]:
            print(
                f"    BEHAVIOR (measured, not gated): {summary['behavior_passed']}/{summary['runs']} runs met the behavior check"
            )
        if summary["token_samples"]:
            print(
                "    token medians="
                + ", ".join(
                    f"{key}:{summary[key]['median']} [{summary[key]['min']}..{summary[key]['max']}]"
                    for key in ("input", "cached", "uncached", "output")
                )
            )
            print(
                f"    aggregate cached-input share={summary['cached_input_percent']}%; wall median={summary['wall']['median']}s"
            )
    print(f"Full measurements: {settings.traces / RESULTS_FILE}")
    return gated == len(by_scn)


def main(argv: Sequence[str] | None = None) -> int:
    """Run the scenarios ``argv``, or all, and return the exit code of the gate."""
    ids = list(sys.argv[1:] if argv is None else argv)
    try:
        settings = read_settings(os.environ)
        scenarios = dataset.load_scenarios(ids)
    except ValueError as error:
        print(error, file=sys.stderr)
        return 2
    if not scenarios:
        print(f"no scenarios found under {dataset.SCENARIO_ROOT}", file=sys.stderr)
        return 2
    harbor_dir = settings.traces.parent / "harbor"
    dataset_dir = harbor_dir / "dataset"
    shutil.rmtree(dataset_dir, ignore_errors=True)
    dataset.compile_dataset(dataset_dir, ids, agent_timeout_sec=settings.timeout_sec)
    job_name = "e2e-" + datetime.now(UTC).strftime("%Y%m%dT%H%M%S%fZ")
    jobs_dir = harbor_dir / "jobs"
    try:
        command = harbor_command(settings, dataset_dir, jobs_dir, job_name)
    except FileNotFoundError as error:
        print(error, file=sys.stderr)
        return 2
    timeout = (
        f"timeout={settings.timeout_sec}s"
        if settings.timeout_sec
        else f"default timeout={dataset.DEFAULT_AGENT_TIMEOUT_SEC}s"
    )
    print(
        f"running {len(scenarios)} scenarios × {len(settings.models)} model(s) "
        f"{list(settings.models)} on {settings.provider} through "
        f"{settings.native_bin or 'source JVM'} in Harbor job {jobs_dir / job_name} "
        f"(reasoning-effort={settings.reasoning_effort or 'default'}) "
        f"(repeats={settings.repeats}, workers={settings.workers}, {timeout})\n",
        flush=True,
    )
    collector = Collector(settings, scenarios, jobs_dir / job_name)
    code = run_job(command, collector)
    if code:
        print(f"warning: harbor exited {code}", file=sys.stderr)
    results = collector.results()
    summaries = summarize_results(results)
    settings.traces.mkdir(parents=True, exist_ok=True)
    (settings.traces / RESULTS_FILE).write_text(
        json.dumps({"runs": results, "summaries": summaries}, indent=2) + "\n"
    )
    return 0 if print_table(settings, results, summaries) else 1


if __name__ == "__main__":
    raise SystemExit(main())
