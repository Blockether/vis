"""Tests for evals — a temporary repository, short child processes and no paid calls.

Each test points the module at a temporary repository and run home. The leaderboard,
the queue plan and Podman are fakes; launched runs are short Python commands.
"""

import dataclasses
import hashlib
import importlib.util
import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path
from types import SimpleNamespace

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts

_spec = importlib.util.spec_from_file_location(
    "evals_extension", Path(__file__).with_name("evals.py")
)
evals = importlib.util.module_from_spec(_spec)
sys.modules[_spec.name] = evals
_spec.loader.exec_module(evals)
SPEC = vis._registration["spec"]
tools = evals.evals

DATASET = "terminal-bench/terminal-bench@4.0.0"
MODEL = "zai-coding-plan/glm-5.3-flash"
SHOW_START = {"preflight", "run_bench", "status", "stop", "bench", "leaderboard"}
TAGS = {
    "scenarios": "observation",
    "new_scenario": "mutation",
    "run_scenarios": "mutation",
    "preflight": "verification",
    "run_bench": "mutation",
    "runs": "observation",
    "status": "observation",
    "stop": "mutation",
    "report": "observation",
    "bench": "observation",
    "diagnose": "observation",
    "leaderboard": "external",
    "compare": "observation",
    "trial": "observation",
    "label": "mutation",
}


def write_json(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data), encoding="utf-8")
    return path


@pytest.fixture
def repo(tmp_path, monkeypatch):
    """A temporary repository with two scenarios, a benchmark directory and a run home."""
    root = tmp_path / "repo"
    scenarios = root / "e2e" / "scenarios"
    write_json(
        scenarios / "edit-config" / "scenario.json",
        {
            "lang": "python",
            "prompt": "Set the port to 8080 in config.ini.",
            "want": {"config.ini": ["port = 8080"]},
            "wantnot": {},
        },
    )
    write_json(
        scenarios / "answer-sum" / "scenario.json",
        {
            "lang": "clojure",
            "prompt": "Answer with the sum of the numbers.",
            "want": {},
            "wantnot": {},
            "want_answer": ["42"],
            "timeout_s": 120,
            "measurement": True,
        },
    )
    bench = root / "benchmarks" / "terminal_bench"
    bench.mkdir(parents=True)
    paths = {
        "REPO": root,
        "SCENARIOS": scenarios,
        "E2E_RUNNER": root / "e2e" / "run.py",
        "BENCH": bench,
        "HOME": tmp_path / "home",
    }
    for name, value in paths.items():
        monkeypatch.setattr(evals, name, value)
    return root


def render(name, result, **kwargs):
    """Render one success presentation and check it against the portable contract."""
    declaration = getattr(evals.Evals, name).__vis_symbol_activity__
    presentation = declaration.render(
        phase="success", result=result, args=(), kwargs=kwargs, error=None
    )
    assert isinstance(presentation, vis.ActivityPresentation)
    assert presentation.headline == declaration.label
    assert _contracts.validate("activity", "presentation", presentation.to_wire())
    return presentation


def words(presentation):
    """The visible text of a presentation: summary, text blocks and table cells."""
    parts = [presentation.summary]
    for block in presentation.content:
        parts.append(getattr(block, "text", ""))
        parts.extend(" ".join(row) for row in getattr(block, "rows", ()))
    return "\n".join(parts)


def launch(kind, code, *, with_results=False, expected_runs=None):
    """Start a short Python child through the extension's own launcher."""
    run_id, run_dir = evals._new_run(kind)
    run = evals._launch(
        run_id,
        run_dir,
        kind=kind,
        label=f"{kind} probe",
        argv=[sys.executable, "-c", code],
        cwd=run_dir,
        env=dict(os.environ),
        results_path=run_dir / "traces" / "results.json" if with_results else None,
        expected_runs=expected_runs,
    )
    return run, run_dir


def finish(run_dir, timeout=30):
    """Wait until a launched run leaves the running state."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        run = evals._run(run_dir)
        if run.state != "running":
            return run
        time.sleep(0.05)
    raise AssertionError(f"run {run_dir.name} still runs")


def test_registration_exports_every_tool_with_a_natural_activity():
    assert (SPEC["name"], SPEC["alias"], SPEC["kind"]) == (
        "evals",
        "evals",
        "integration",
    )
    assert SPEC["env"] == [
        evals.API_KEY_ENV,
        "DOCKER_HOST",
        "PODMAN_COMPOSE_PROVIDER",
        "VIS_EVALS_HOME",
    ]
    assert [command["name"] for command in SPEC["slash_commands"]] == ["evals"]
    exported = SPEC["symbols"][0]["methods"]
    assert {entry["name"]: entry["tag"] for entry in exported} == TAGS
    for entry in exported:
        assert f"evals.{entry['name']}(" in evals.PROMPT
        declaration = getattr(tools, entry["name"]).__vis_symbol_activity__
        assert re.fullmatch(r"[A-Z][A-Za-z ]+", declaration.label)
        # Slow work and remote reads show progress; quick local reads show only the end.
        assert declaration.show_start is (entry["name"] in SHOW_START)
        assert entry["activity"]["show_start"] is declaration.show_start
        for phase in ("start", "failure"):
            rendered = declaration.render(
                phase=phase, result=None, args=(), kwargs={}, error=RuntimeError("x")
            )
            assert rendered is None


def test_scenarios_lists_checks_and_filters_case_insensitively(repo):
    found = tools.scenarios()
    assert [scenario.id for scenario in found] == ["answer-sum", "edit-config"]
    answer, edit = found
    assert (answer.checks, answer.timeout_s, answer.is_measurement) == (
        ("want_answer",),
        120,
        True,
    )
    assert (edit.checks, edit.timeout_s, edit.lang) == (("want",), None, "python")
    assert [scenario.id for scenario in tools.scenarios("PORT")] == ["edit-config"]
    assert [scenario.id for scenario in tools.scenarios("^answer")] == ["answer-sum"]
    assert render("scenarios", found).summary == "2 scenarios"
    assert render("scenarios", (), pattern="zzz").summary == "0 scenarios matching zzz"


def test_new_scenario_writes_the_runner_layout(repo):
    created = tools.new_scenario(
        "fix-typo",
        "  Fix the typo in notes/readme.txt.  ",
        files={"notes/readme.txt": "teh value\n"},
        want={"notes/readme.txt": ["the value"]},
        wantnot={"notes/readme.txt": ["teh"]},
        timeout_s=90,
    )
    directory = evals.SCENARIOS / "fix-typo"
    assert json.loads((directory / "scenario.json").read_text()) == {
        "lang": "python",
        "prompt": "Fix the typo in notes/readme.txt.",
        "want": {"notes/readme.txt": ["the value"]},
        "wantnot": {"notes/readme.txt": ["teh"]},
        "timeout_s": 90,
    }
    assert (directory / "files" / "notes" / "readme.txt").read_text() == "teh value\n"
    assert (created.id, created.checks, created.timeout_s) == (
        "fix-typo",
        ("want", "wantnot"),
        90,
    )
    assert "fix-typo" in [scenario.id for scenario in tools.scenarios()]
    assert render("new_scenario", created).summary == "fix-typo · python · 2 checks"


@pytest.mark.parametrize(
    "changes, error, message",
    [
        ({"scenario_id": "Fix_Typo"}, ValueError, "lowercase words"),
        ({"prompt": "   "}, ValueError, "blank"),
        ({"want": None}, ValueError, "oracle"),
        ({"lang": "python 3"}, ValueError, "one lowercase word"),
        ({"timeout_s": 0}, ValueError, "positive"),
        ({"files": {"../outside.txt": "x"}}, ValueError, "inside the scenario"),
        ({"files": {"/tmp/outside.txt": "x"}}, ValueError, "inside the scenario"),
        ({"scenario_id": "edit-config"}, FileExistsError, "already exists"),
    ],
)
def test_new_scenario_refuses_unsafe_or_ungraded_input(repo, changes, error, message):
    arguments = {
        "scenario_id": "fix-typo",
        "prompt": "Fix it.",
        "files": {"a.txt": "x"},
        "want": {"a.txt": ["y"]},
    } | changes
    before = sorted(path.name for path in evals.SCENARIOS.iterdir())
    with pytest.raises(error, match=message):
        tools.new_scenario(**arguments)
    assert sorted(path.name for path in evals.SCENARIOS.iterdir()) == before


def test_run_scenarios_starts_the_runner_with_its_route(repo, monkeypatch):
    for name in (
        "VIS_PROVIDER",
        "VIS_MODELS",
        "VIS_REASONING_EFFORT",
        "VIS_E2E_TIMEOUT",
    ):
        monkeypatch.delenv(name, raising=False)
    launches = []
    monkeypatch.setattr(
        evals,
        "_launch",
        lambda run_id, run_dir, **call: launches.append((run_dir, call)) or run_id,
    )
    run_id = tools.run_scenarios(
        ["edit-config"],
        models=["glm-a", "glm-b"],
        provider="zai-coding-plan",
        repeats=2,
        workers=1,
        reasoning_effort="high",
        timeout_s=60,
    )
    assert re.fullmatch(r"\d{8}-\d{6}-scenarios-[0-9a-f]{4}", run_id)
    ((run_dir, call),) = launches
    assert call["argv"][1:] == [str(evals.E2E_RUNNER), "edit-config"]
    assert (call["cwd"], call["kind"], call["expected_runs"]) == (
        evals.REPO,
        "scenarios",
        4,
    )
    assert call["label"] == "1 scenario on zai-coding-plan / glm-a, glm-b, 2 repeats"
    assert call["results_path"] == run_dir / "traces" / "results.json"
    route = {
        "VIS_E2E_TRACES": str(run_dir / "traces"),
        "VIS_E2E_REPEATS": "2",
        "VIS_E2E_WORKERS": "1",
        "VIS_PROVIDER": "zai-coding-plan",
        "VIS_MODELS": "glm-a,glm-b",
        "VIS_REASONING_EFFORT": "high",
        "VIS_E2E_TIMEOUT": "60",
    }
    assert {name: call["env"].get(name) for name in route} == route
    tools.run_scenarios(["answer-sum", "edit-config"])
    call = launches[-1][1]
    assert "VIS_MODELS" not in call["env"] and "VIS_PROVIDER" not in call["env"]
    assert call["label"] == "2 scenarios on default provider / default model"
    assert call["expected_runs"] == 2


@pytest.mark.parametrize(
    "ids, extra, message",
    [
        ([], {}, "at least one"),
        (["missing"], {}, "unknown scenarios: missing"),
        (["edit-config"], {"repeats": 0}, "positive"),
        (["edit-config"], {"native_bin": "vis-agent"}, "absolute path"),
    ],
)
def test_run_scenarios_refuses_before_any_paid_call(
    repo, monkeypatch, ids, extra, message
):
    monkeypatch.setattr(
        evals, "_launch", lambda *args, **kwargs: pytest.fail("launched")
    )
    with pytest.raises(ValueError, match=message):
        tools.run_scenarios(ids, **extra)
    assert not (evals.HOME / "runs").exists()


def test_launched_run_finishes_with_its_exit_code_log_and_progress(repo):
    run, run_dir = launch(
        "scenarios",
        "print('hello from the runner'); raise SystemExit(3)",
        with_results=True,
        expected_runs=2,
    )
    done = finish(run_dir)
    assert (done.state, done.exit_code, done.ended_at is not None) == (
        "finished",
        3,
        True,
    )
    status = tools.status(run.run_id[:-2])
    assert status.run == done
    assert status.log_tail == ("hello from the runner",)
    assert status.progress == "0 of 2 scenario runs finished, without results.json"
    assert [item.run_id for item in tools.runs()] == [run.run_id]
    assert render("status", status).summary.startswith(f"{run.run_id} · finished")


def test_stop_ends_a_run_that_another_process_started(repo):
    run, run_dir = launch(
        "bench", "import time; print('waiting', flush=True); time.sleep(60)"
    )
    # A new Vis process has no child handle; it finds the run by its process marker.
    child = evals._CHILDREN.pop(run.run_id)
    try:
        assert evals._run(run_dir).state == "running"
        stopped = tools.stop(run.run_id)
        assert (stopped.state, stopped.exit_code) == ("stopped", None)
        assert stopped.ended_at
        assert tools.stop(run.run_id) == stopped
        assert render("stop", stopped).summary == f"{run.run_id} · stopped"
    finally:
        child.kill()
        child.wait(10)


def test_a_run_without_its_process_or_exit_code_is_lost(repo):
    run_dir = evals.HOME / "runs" / "20260101-000000-bench-abcd"
    meta = {
        "run_id": run_dir.name,
        "kind": "bench",
        "label": "old queue",
        "command": "uv run --locked python run_suite.py",
        "cwd": str(evals.BENCH),
        # This live process has no vis-evals marker, so it cannot be the run.
        "pid": os.getpid(),
        "started_at": "2026-01-01T00:00:00+00:00",
        "results_path": None,
        "expected_runs": None,
    }
    write_json(run_dir / "run.json", meta)
    write_json(run_dir.with_name("20260101-000000-bench-abce") / "run.json", meta)
    assert evals._run(run_dir).state == "lost"
    assert tools.status(run_dir.name).progress == "queue lost"
    with pytest.raises(LookupError, match="2 runs match '20260101'"):
        tools.status("20260101")
    with pytest.raises(LookupError, match="no run match 'missing'"):
        tools.status("missing")
    with pytest.raises(LookupError, match="no scenarios runs"):
        tools.report()


RESULTS = {
    "runs": [
        {
            "id": "edit-config",
            "provider": "zai",
            "model": "glm",
            "repeat": 1,
            "converged": True,
            "correct": True,
            "errors": 0,
        },
        {
            "id": "edit-config",
            "provider": "zai",
            "model": "glm",
            "repeat": 2,
            "converged": True,
            "correct": False,
            "errors": 0,
            "detail": ["want 'port = 8080' in config.ini"],
        },
        {
            "id": "answer-sum",
            "provider": "zai",
            "model": "glm",
            "repeat": 1,
            "converged": False,
            "correct": False,
            "errors": 2,
            "err_msgs": ["HTTP 401 for key sk-fixture-credential"],
        },
    ],
    "summaries": [
        {
            "id": "edit-config",
            "provider": "zai",
            "model": "glm",
            "runs": 2,
            "passed": 1,
            "behavior_passed": 1,
            "token_totals": {"input": 1000, "output": 50},
            "cached_input_percent": 80.0,
            "wall": {"min": 10, "median": 12.5, "max": 15},
        },
        {
            "id": "answer-sum",
            "provider": "zai",
            "model": "glm",
            "runs": 1,
            "passed": 0,
            "behavior_passed": 0,
            "measurement": True,
            "token_totals": {"input": 500, "output": 20},
            "cached_input_percent": 0,
            "wall": {},
        },
    ],
}


def test_report_reads_a_finished_run_and_redacts_failures(repo, monkeypatch):
    monkeypatch.setenv("EXAMPLE_API_KEY", "sk-fixture-credential")
    run, run_dir = launch("scenarios", "raise SystemExit(1)", with_results=True)
    finish(run_dir)
    with pytest.raises(FileNotFoundError, match="finished and has no results yet"):
        tools.report()
    write_json(run_dir / "traces" / "results.json", RESULTS)
    report = tools.report()
    assert (report.run_id, report.total_runs, report.passed_runs, report.is_passed) == (
        run.run_id,
        3,
        1,
        False,
    )
    edit, answer = report.results
    assert edit.failures == ("run 2: wrong result: want 'port = 8080' in config.ini",)
    assert answer.failures == (
        "run 1: did not finish, wrong result, 2 errors: HTTP 401 for key [REDACTED]",
    )
    assert (edit.wall_median_s, edit.input_tokens, edit.cached_input_percent) == (
        12.5,
        1000,
        80.0,
    )
    assert (answer.is_measurement, answer.wall_median_s) == (True, None)
    assert tools.status(run.run_id).progress == "results ready: 1 of 3 runs passed"
    view = render("report", report)
    assert view.verdict == "failed"
    assert "2 failed runs" in words(view)
    assert "sk-fixture-credential" not in words(view)
    passing = write_json(
        repo / "old" / "results.json",
        {
            "runs": RESULTS["runs"][:1],
            "summaries": [RESULTS["summaries"][0] | {"runs": 1, "passed": 1}],
        },
    )
    old = tools.report(results_path=str(passing))
    assert (old.run_id, old.is_passed) == (None, True)
    assert render("report", old).verdict == "passed"


def trial(task, *, seconds, tests, finished="2026-01-02T00:00:00", name=None, **extra):
    passed, total = tests
    return {
        "task": f"terminal-bench/{task}",
        "trial": name or f"{task}__{finished[5:10].replace('-', '')}",
        "finished_at": finished,
        "scored": True,
        "model": MODEL,
        "reward": 1.0 if passed == total else 0.0,
        "verifier_tests": {"tests": total, "passed": passed},
        "agent_seconds": seconds,
        "vis_iterations": 12,
        **extra,
    }


def summary_data():
    def tasks(*names):
        return [f"terminal-bench/{name}" for name in names]

    return {
        "dataset": DATASET,
        "total_dataset_tasks": 10,
        "scored_tasks": 6,
        "solved_tasks": tasks("alpha", "beta"),
        "task_outcomes": {
            "solved": tasks("beta", "alpha"),
            "failed_tests": tasks("gamma", "delta"),
            "vis_error:http-error": tasks("epsilon"),
            "agent_timeout": tasks("zeta"),
        },
        "unscored_model_tasks": tasks("eta"),
        "gpu_required_tasks": tasks("theta"),
        "unattempted_tasks": tasks("iota"),
        "attempts": 8,
        "usage_totals": {
            "model_attempts": 7,
            "input_tokens": 1000,
            "cached_input_tokens": 250,
            "output_tokens": 100,
            "unparsed_output_tokens_estimate": 5,
            "estimated_metered_api_cost_usd": 3.456,
            "agent_hours": 2.26,
        },
        "repeated_final_errors": [
            {
                "trial": "bulk-003/zeta__abc",
                "type": "python-worker-retired",
                "iterations": 384,
            }
        ],
        "trials": [
            trial("alpha", seconds=600, tests=(5, 5)),
            trial("beta", seconds=300, tests=(3, 3)),
            trial("gamma", seconds=10, tests=(0, 10), finished="2025-12-31T00:00:00"),
            trial("gamma", seconds=100, tests=(1, 10)),
            trial("delta", seconds=900, tests=(9, 10)),
            trial(
                "epsilon",
                seconds=50,
                tests=(8, 10),
                trace={"vis_result_status": "error", "vis_error_type": "http-error"},
            ),
            trial(
                "zeta",
                seconds=1000,
                tests=(0, 4),
                name="zeta__abc",
                exception_type="AgentTimeoutError",
            ),
            {"task": "terminal-bench/eta", "scored": False, "model": MODEL},
        ],
    }


def queue_plan(completed=3, retryable=("music-harmony",), stale_live=(), attempts=1):
    return evals.QueuePlan(
        model=MODEL,
        cpu_tasks=9,
        attempts=attempts,
        completed=completed,
        scored=completed,
        retryable=tuple(retryable),
        live=(),
        pending=("alpha", "beta"),
        gpu_only=("theta",),
        is_queue_running=False,
        stale_live=stale_live,
    )


def write_agent_limits(*tasks):
    """Give each task an agent time limit of 1000 seconds in the local dataset."""
    for task in tasks:
        path = evals.BENCH / "artifacts" / "datasets" / "terminal-bench" / task
        path.mkdir(parents=True, exist_ok=True)
        (path / "task.toml").write_text(
            "[agent]\ntimeout_sec = 1000\n", encoding="utf-8"
        )


def test_bench_scores_each_task_once_with_a_strict_rate(repo, monkeypatch):
    with pytest.raises(FileNotFoundError, match="no benchmark summary"):
        tools.bench(refresh=False)
    write_json(evals.BENCH / "runs" / "summary.json", summary_data())
    plan = queue_plan(stale_live=("kappa",))
    monkeypatch.setattr(evals, "_queue_plan", lambda *_: plan)
    report = tools.bench(refresh=False)
    assert (report.dataset, report.model, report.total_tasks, report.scored_tasks) == (
        DATASET,
        MODEL,
        10,
        6,
    )
    assert (
        report.solved_tasks,
        report.pass_rate_percent,
        report.strict_pass_rate_percent,
    ) == (
        2,
        33.3,
        20.0,
    )
    assert [(item.outcome, item.count, item.tasks) for item in report.outcomes] == [
        ("failed_tests", 2, ("delta", "gamma")),
        ("solved", 2, ("alpha", "beta")),
        ("agent_timeout", 1, ("zeta",)),
        ("vis_error:http-error", 1, ("epsilon",)),
    ]
    assert (report.unscored_tasks, report.gpu_tasks, report.unattempted_tasks) == (
        ("eta",),
        ("theta",),
        1,
    )
    assert (
        report.cached_input_percent,
        report.estimated_metered_cost_usd,
        report.agent_hours,
    ) == (25.0, 3.46, 2.3)
    assert report.repeated_final_errors == (
        "bulk-003/zeta__abc: python-worker-retired for 384 iterations",
    )
    assert report.plan == plan
    assert (report.pass_rate_ci95, report.strict_pass_rate_ci95) == (
        (9.7, 70.0),
        (5.7, 51.0),
    )
    # Each scored attempt counts once; gamma has two failed attempts.
    assert (
        report.attempts_per_task,
        report.scored_attempts,
        report.solved_attempts,
        report.attempt_pass_rate_percent,
        report.pass_at_k_percent,
        report.cost_per_solved_usd,
    ) == (1, 7, 2, 28.6, None, 1.73)
    view = render("bench", report)
    assert view.summary == (
        "2 of 6 scored tasks solved (33.3%, 95% interval 9.7-70%) · strict 20%"
    )
    assert "Interrupted trials without a queue: kappa." in words(view)
    assert "$1.73 for each solved task" in words(view)
    assert "The subscription charge is unknown." in words(view)

    def no_plan(*_):
        raise RuntimeError("uv is missing")

    monkeypatch.setattr(evals, "_queue_plan", no_plan)
    assert tools.bench(refresh=False).plan is None


def test_bench_reads_a_named_round_with_pass_at_k(repo, monkeypatch):
    with pytest.raises(LookupError, match="no benchmark round"):
        tools.bench(refresh=False, round_name="glm-k2")
    first, second = "2026-01-01T00:00:00", "2026-01-02T00:00:00"
    trials = [
        trial("alpha", seconds=60, tests=(2, 2), finished=first),
        trial("alpha", seconds=60, tests=(2, 2), finished=second),
        trial("beta", seconds=60, tests=(2, 2), finished=first),
        trial("beta", seconds=60, tests=(1, 2), finished=second),
        trial("gamma", seconds=60, tests=(0, 2), finished=first),
        trial("gamma", seconds=60, tests=(0, 2), finished=second),
    ]
    write_json(
        evals.BENCH / "rounds" / "glm-k2" / "runs" / "summary.json",
        summary_data()
        | {
            "scored_tasks": 3,
            "solved_tasks": ["terminal-bench/alpha"],
            "task_outcomes": {
                "solved": ["terminal-bench/alpha"],
                "failed_tests": ["terminal-bench/beta", "terminal-bench/gamma"],
            },
            "trials": trials,
        },
    )
    plans = []

    def plan(*args):
        plans.append(args)
        return queue_plan(attempts=2)

    monkeypatch.setattr(evals, "_queue_plan", plan)
    report = tools.bench(refresh=False, round_name="glm-k2")
    assert plans == [("glm-k2",)]
    assert (report.round_name, report.solved_tasks, report.scored_tasks) == (
        "glm-k2",
        1,
        3,
    )
    # pass@2 counts tasks with any solved attempt, pass^2 tasks with only solved ones.
    assert (
        report.attempts_per_task,
        report.scored_attempts,
        report.solved_attempts,
        report.attempt_pass_rate_percent,
        report.pass_at_k_percent,
        report.pass_all_k_percent,
        report.unstable_tasks,
    ) == (2, 6, 3, 50.0, 66.7, 33.3, ("beta",))
    view = render("bench", report)
    assert view.summary.startswith("Round glm-k2: 1 of 3 scored tasks solved")
    assert "pass@2 is 66.7% and pass^2 is 33.3%" in words(view)
    assert "Mixed results: beta." in words(view)
    assert "3 of 9 CPU tasks scored 2 times" in words(view)


def test_bench_refresh_reports_a_summarize_failure(repo, monkeypatch):
    calls = []

    def command(argv, **kwargs):
        calls.append((argv, kwargs["cwd"]))
        return subprocess.CompletedProcess(argv, 1, "", "summary broke")

    monkeypatch.setattr(evals, "_command", command)
    with pytest.raises(RuntimeError, match="summarize.py failed: summary broke"):
        tools.bench()
    assert calls == [(["uv", "run", "--locked", "python", "summarize.py"], evals.BENCH)]


def test_diagnose_ranks_where_the_benchmark_loses_tasks(repo):
    write_json(evals.BENCH / "runs" / "summary.json", summary_data())
    write_agent_limits("alpha", "beta", "gamma", "delta", "epsilon", "zeta")
    diagnosis = tools.diagnose()
    assert [
        (item.kind, item.task_count, item.title) for item in diagnosis.findings
    ] == [
        ("vis_errors", 1, "Vis stopped with an error in 1 of 6 scored tasks"),
        ("early_stops", 1, "Vis finished early and failed the tests in 1 task"),
        (
            "near_misses",
            2,
            "2 failed tasks passed at least 80% of their verifier tests",
        ),
        ("agent_timeouts", 1, "1 task reached the agent time limit"),
        ("dead_tools", 1, "1 attempt repeated one error until the end"),
        ("unscored", 1, "1 model attempt ended without a verifier result"),
    ]
    vis_errors, early, near, _, dead, _ = diagnosis.findings
    assert vis_errors.share == 0.167
    # The latest scored attempt counts, not the older one of the same task.
    gamma = early.tasks[0]
    assert (gamma.task, gamma.checks, gamma.budget_used, gamma.minutes) == (
        "gamma",
        "1/10",
        0.1,
        1.7,
    )
    assert "The median failed-tests run used 50%" in early.detail
    assert [(note.task, note.pass_ratio) for note in near.tasks] == [
        ("delta", 0.9),
        ("epsilon", 0.8),
    ]
    assert (dead.tasks[0].task, dead.tasks[0].outcome, dead.tasks[0].iterations) == (
        "zeta",
        "python-worker-retired",
        384,
    )
    assert diagnosis.estimate.startswith(
        "2 of 5 tasks with a full attempt passed (40%)."
    )
    assert (
        "on the 1 Vis-error task would add about 0.4 solved tasks" in diagnosis.estimate
    )
    assert "40.0% instead of 33.3%" in diagnosis.estimate
    view = render("diagnose", diagnosis)
    assert view.summary.startswith(
        "6 findings over 6 scored tasks · largest: Vis stopped"
    )


def board_row(rank, agent, model, accuracy, cost=None):
    return {
        "rank": rank,
        "metadata": {
            "agent_display": {"label": agent},
            "model_display": {"label": model},
            "reasoning_effort": "high",
            "date": "2026-01-01",
        },
        "metrics": {
            "accuracy": accuracy,
            "n_trials": 50,
            "successes": int(accuracy / 2),
            "total_cost_usd": cost,
            "total_tokens": 5_000_000,
            "avg_trial_duration_sec": 600,
            "accuracy_ci95_half_width": 4.0,
        },
    }


def test_leaderboard_reads_every_page_and_places_vis(repo, monkeypatch):
    write_json(evals.BENCH / "runs" / "summary.json", summary_data())
    pages = {
        1: [
            board_row(2, "Agent B", "Model B", 40.0),
            board_row(1, "Agent A", "Model A", 60.0, 100),
        ],
        2: [board_row(3, "Agent C", "GLM 5.3 Flash", 20.0)],
    }
    requests = []

    def post(url, payload):
        requests.append((url, payload))
        return {
            "leaderboard": {"title": "Terminal-Bench 4.0"},
            "rows": pages[payload["page"]],
            "pagination": {"total_pages": 2},
        }

    monkeypatch.setattr(evals, "_post_json", post)
    board = tools.leaderboard()
    assert requests == [
        (
            evals.LEADERBOARD_URL,
            {
                "package": "terminal-bench/terminal-bench",
                "name": "4-0-0",
                "page": page,
                "page_size": 100,
            },
        )
        for page in (1, 2)
    ]
    assert [row.rank for row in board.rows] == [1, 2, 3]
    top = board.rows[0]
    assert (top.cost_per_trial_usd, top.tokens_per_trial, top.avg_trial_minutes) == (
        2.0,
        100_000,
        10.0,
    )
    standing = board.vis
    # The board counts trials, so Vis counts every scored attempt: 2 of 7.
    assert (standing.pass_rate_percent, standing.rank_by_pass_rate) == (28.6, 3)
    assert (standing.strict_pass_rate_percent, standing.rank_by_strict_rate) == (
        18.2,
        4,
    )
    # The ends of the 95% interval give the ranks that the data supports.
    assert (standing.rank_range, standing.strict_rank_range) == ((1, 4), (2, 4))
    assert (
        standing.cost_per_attempt_usd,
        standing.cost_per_solved_usd,
        standing.tokens_per_attempt,
        standing.avg_attempt_minutes,
    ) == (0.49, 1.73, 157, 19.4)
    assert top.cost_per_success_usd == 3.33
    assert standing.caveats[0] == (
        f"The board has 1 row with {MODEL}. Compare Vis with the same model to see the "
        "harness effect."
    )
    assert "about 5 trials per task" in standing.caveats[1]
    assert "run_bench(attempts=5, round_name=...)" in standing.caveats[1]
    view = render("leaderboard", board)
    assert view.summary == (
        "Terminal-Bench 4.0: 3 rows · Vis 28.6% would rank 3 (range 1-4), strict 4"
    )
    assert "between ranks 1 and 4" in words(view)
    # Another dataset version still loads, without a Vis standing.
    assert tools.leaderboard(name="2-0-0").vis is None


READY = evals.Preflight(True, (evals.Check("uv", True, "/usr/bin/uv", ""),))
BUNDLE_SHA256 = hashlib.sha256(b"bundle").hexdigest()


@pytest.fixture
def bench_launches(repo, monkeypatch):
    """run_bench() with a passing preflight, a fake queue plan and a recording launcher."""
    launches = []
    monkeypatch.setattr(evals, "_preflight", lambda round_name=None: READY)
    monkeypatch.setattr(
        evals,
        "_queue_plan",
        lambda round_name=None, attempts=None: queue_plan(attempts=attempts or 1),
    )
    bundle = repo / "vis-agent-linux-amd64.tar.gz"
    bundle.write_bytes(b"bundle")
    monkeypatch.setattr(evals, "_bundle", lambda: bundle)
    monkeypatch.setattr(
        evals, "_bench_env", lambda: {"PATH": os.environ.get("PATH", "")}
    )
    monkeypatch.setattr(
        evals,
        "_launch",
        lambda run_id, run_dir, **call: launches.append(call) or run_id,
    )
    return launches


def test_run_bench_starts_the_queue_with_its_options(bench_launches):
    tools.run_bench(max_tasks=2, retry_tasks=["music-harmony"], job_prefix="retry-oom")
    tools.run_bench()
    first, full = bench_launches
    queue = ["uv", "run", "--locked", "python", "run_suite.py", "--attempts", "1"]
    assert first["argv"] == [
        *queue,
        "--max-tasks",
        "2",
        "--retry-task",
        "music-harmony",
        "--job-prefix",
        "retry-oom",
    ]
    assert (first["cwd"], first["kind"]) == (evals.BENCH, "bench")
    assert first["label"] == f"2 Terminal-Bench tasks on {MODEL}"
    # The run records the bundle digest, so that compare() can name the build.
    assert (first["build_sha256"], first["round_name"], first["attempts"]) == (
        BUNDLE_SHA256,
        None,
        1,
    )
    assert full["argv"] == queue


def test_run_bench_pins_a_named_round_with_its_attempts(bench_launches, monkeypatch):
    plans = []

    def plan(round_name=None, attempts=None):
        plans.append((round_name, attempts))
        return queue_plan(completed=0, attempts=attempts)

    monkeypatch.setattr(evals, "_queue_plan", plan)
    tools.run_bench(max_tasks=1, attempts=3, round_name="glm-k3")
    round_dir = evals.BENCH / "rounds" / "glm-k3"
    (call,) = bench_launches
    assert call["argv"] == [
        "uv",
        "run",
        "--locked",
        "python",
        "run_suite.py",
        "--jobs",
        str(round_dir / "jobs"),
        "--attempts",
        "3",
        "--max-tasks",
        "1",
    ]
    assert (call["round_name"], call["attempts"], call["build_sha256"]) == (
        "glm-k3",
        3,
        BUNDLE_SHA256,
    )
    assert (
        call["label"]
        == f"1 Terminal-Bench task on {MODEL}, 3 attempts each, round glm-k3"
    )
    assert plans == [("glm-k3", 3)]
    pins = json.loads((round_dir / "runs" / "provenance.json").read_text())
    assert (pins["round"], pins["attempts"], pins["model"], pins["bundle_sha256"]) == (
        "glm-k3",
        3,
        MODEL,
        BUNDLE_SHA256,
    )
    # The round keeps its attempt count; another count needs a new round.
    with pytest.raises(ValueError, match="round glm-k3 pins 3 attempts for each task"):
        tools.run_bench(max_tasks=1, attempts=2, round_name="glm-k3")
    tools.run_bench(max_tasks=1, round_name="glm-k3")
    assert (bench_launches[-1]["attempts"], plans[-1]) == (3, ("glm-k3", 3))


def test_run_bench_needs_a_canary_before_the_first_scored_trial(
    bench_launches, monkeypatch
):
    monkeypatch.setattr(evals, "_queue_plan", lambda *_: queue_plan(completed=0))
    with pytest.raises(ValueError, match="canary with max_tasks=1"):
        tools.run_bench(max_tasks=5)
    assert bench_launches == []
    tools.run_bench(max_tasks=1)
    assert bench_launches[-1]["argv"][-2:] == ["--max-tasks", "1"]
    assert bench_launches[-1]["label"] == f"1 Terminal-Bench task on {MODEL}"


@pytest.mark.parametrize(
    "arguments, message",
    [
        ({"max_tasks": 0}, "max_tasks must be positive"),
        ({"attempts": 0}, "attempts must be positive"),
        ({"attempts": 3}, r"needs a named round: run_bench\(attempts=3, round_name"),
        ({"round_name": "Glm K3"}, "round name 'Glm K3' must be lowercase"),
        ({"job_prefix": "Retry OOM"}, "job_prefix"),
        (
            {"retry_tasks": ["unknown-task"]},
            "not retryable: unknown-task; retryable: music-harmony",
        ),
    ],
)
def test_run_bench_refuses_invalid_options(bench_launches, arguments, message):
    with pytest.raises(ValueError, match=message):
        tools.run_bench(**arguments)
    assert bench_launches == []


def test_run_bench_refuses_a_failed_preflight(bench_launches, monkeypatch):
    failed = evals.Preflight(
        False,
        (
            evals.Check("uv", True, "/usr/bin/uv", ""),
            evals.Check("Podman machine", False, "vis-amd64: stopped", "Start it."),
        ),
    )
    monkeypatch.setattr(evals, "_preflight", lambda *_: failed)
    with pytest.raises(
        RuntimeError,
        match=r"preflight failed: Podman machine: vis-amd64: stopped\. Start it\.$",
    ):
        tools.run_bench(max_tasks=1)
    assert bench_launches == []


def test_run_bench_refuses_a_second_queue(repo, monkeypatch):
    running, _ = launch("bench", "import time; time.sleep(60)")
    monkeypatch.setattr(evals, "_preflight", lambda: pytest.fail("preflight ran"))
    try:
        with pytest.raises(
            RuntimeError, match=f"a benchmark queue already runs: {running.run_id}"
        ):
            tools.run_bench(max_tasks=1)
    finally:
        assert tools.stop(running.run_id).state == "stopped"


def fake_podman(state, socket_path="", compose_ok=True):
    """A _command fake for the Podman calls of preflight()."""
    calls = []

    def command(argv, **kwargs):
        calls.append(argv)
        if argv[:3] == ["podman", "machine", "inspect"]:
            value = state if argv[4] == "{{.State}}" else socket_path
            return subprocess.CompletedProcess(argv, 0, f"{value}\n", "")
        if argv[:3] == ["podman", "compose", "ls"]:
            return subprocess.CompletedProcess(
                argv,
                0 if compose_ok else 1,
                "NAME STATUS\n",
                "" if compose_ok else "no provider",
            )
        if argv[:3] == ["podman", "machine", "ssh"]:
            table = "Filesystem 1024-blocks Used Available Capacity Mounted on\n"
            return subprocess.CompletedProcess(
                argv, 0, table + "/dev/vda4 100000000 1 80000000 1% /var\n", ""
            )
        raise AssertionError(f"unexpected command {argv}")

    return command, calls


@pytest.fixture
def host(repo, monkeypatch):
    """Tools on PATH, a set API key and no running queue."""
    monkeypatch.setattr(evals.shutil, "which", lambda name: f"/usr/bin/{name}")
    monkeypatch.setattr(evals, "_queue_processes", lambda: [])
    monkeypatch.setenv(evals.API_KEY_ENV, "fixture-key-value")
    monkeypatch.delenv("PODMAN_COMPOSE_PROVIDER", raising=False)


def test_preflight_waits_for_the_machine_before_its_dependent_checks(host, monkeypatch):
    command, calls = fake_podman("stopped")
    monkeypatch.setattr(evals, "_command", command)
    result = tools.preflight()
    checks = {check.name: check for check in result.checks}
    assert list(checks) == [
        "uv",
        "Podman",
        "Podman machine",
        "Docker socket",
        "Compose provider",
        "Vis bundle",
        "Dataset",
        "API key",
        "zstd",
        "Host disk",
        "Podman VM disk",
        "Single queue",
    ]
    assert result.is_ready is False
    assert checks["Podman machine"].detail == "vis-amd64: stopped"
    for name in ("Docker socket", "Compose provider", "Podman VM disk"):
        assert (checks[name].is_ok, checks[name].detail, checks[name].fix) == (
            False,
            "not checked: vis-amd64 is not running",
            "Start vis-amd64 first, then run preflight() again.",
        )
    assert all(argv[:3] == ["podman", "machine", "inspect"] for argv in calls)
    assert checks["API key"].detail == f"{evals.API_KEY_ENV} is set"
    assert "fixture-key-value" not in json.dumps(dataclasses.asdict(result))
    assert render("preflight", result).verdict == "failed"


def test_preflight_passes_on_a_ready_machine_and_pins_the_bundle(
    host, monkeypatch, tmp_path
):
    socket_file = tmp_path / "vis-amd64-api.sock"
    socket_file.touch()
    command, _ = fake_podman("running", socket_path=str(socket_file))
    monkeypatch.setattr(evals, "_command", command)
    monkeypatch.setattr(
        evals.shutil, "disk_usage", lambda path: SimpleNamespace(free=100e9)
    )
    monkeypatch.setenv("DOCKER_HOST", "unix:///tmp/podman-machine-default-api.sock")
    bundle = evals.BENCH / "artifacts" / "vis-agent-linux-amd64.tar.gz"
    bundle.parent.mkdir(parents=True)
    bundle.write_bytes(b"bundle")
    digest = hashlib.sha256(b"bundle").hexdigest()
    write_json(evals.BENCH / "runs" / "provenance.json", {"bundle_sha256": digest})
    task = (
        evals.BENCH
        / "artifacts"
        / "datasets"
        / "terminal-bench"
        / "alpha"
        / "task.toml"
    )
    task.parent.mkdir(parents=True)
    task.write_text("[agent]\ntimeout_sec = 1000\n", encoding="utf-8")
    result = tools.preflight()
    assert result.is_ready, [check for check in result.checks if not check.is_ok]
    checks = {check.name: check for check in result.checks}
    # The queue reclaims images on vis-amd64, so Harbor must use its socket too.
    assert checks["Docker socket"].detail == (
        f"unix://{socket_file}, replaces "
        "DOCKER_HOST=unix:///tmp/podman-machine-default-api.sock from the environment"
    )
    assert checks["Compose provider"].detail == "Podman default provider"
    assert checks["Vis bundle"].detail.endswith(
        f"sha256 {digest[:12]}, matches provenance"
    )
    assert checks["Podman VM disk"].detail == "82 GB free"
    assert render("preflight", result).verdict == "passed"
    # A rebuilt bundle no longer matches the pinned digest, so results would mix builds.
    bundle.write_bytes(b"rebuilt bundle")
    rebuilt = {check.name: check for check in tools.preflight().checks}["Vis bundle"]
    assert not rebuilt.is_ok
    assert f"provenance pins {digest[:12]}" in rebuilt.detail


def test_redaction_hides_credentials_but_keeps_short_numbers(monkeypatch):
    monkeypatch.setenv("EXAMPLE_API_KEY", "sk-fixture-credential")
    # A numeric setting with KEY in its name must not hide every "35" in a log.
    monkeypatch.setenv("VNC_KEY_DELAY_MS", "35")
    monkeypatch.setenv("EXAMPLE_TOKEN", "short")
    text = "key sk-fixture-credential, delay 35 ms, short"
    assert evals._redact(text) == "key [REDACTED], delay 35 ms, short"


def test_statistics_match_known_values():
    # Wilson intervals of 2/6 and 0/10, as statistics tables give them.
    assert evals._wilson(2, 6) == (9.7, 70.0)
    assert evals._wilson(0, 10) == (0.0, 27.8)
    assert evals._wilson(0, 0) is None
    # Repeats that always agree count as one run; mixed repeats count in full.
    assert evals._effective_size([(2, 2), (0, 2)]) == 2.0
    assert evals._effective_size([(1, 2), (1, 2)]) == 4.0
    # 100 independent runs at 50% detect a change of about 20 points with 80% power.
    assert evals._detectable_change([(1, 1)] * 50 + [(0, 1)] * 50) == 19.4
    assert evals._detectable_change([]) is None
    # 3 of 10 paired cases broke, but the interval still includes zero.
    assert evals._paired_change([-1.0] * 3 + [0.0] * 7) == ((-54.4, 4.4), 42.1)
    assert evals._kappa([("a", "a"), ("a", "a"), ("b", "b"), ("a", "b")]) == 0.5
    assert evals._kappa([("a", "a"), ("a", "a")]) is None


def scenario_results(outcomes):
    """A results.json with one summary for each scenario: (passed, runs) on zai/glm."""
    return {
        "runs": [],
        "summaries": [
            {
                "id": name,
                "provider": "zai",
                "model": "glm",
                "runs": runs,
                "passed": passed,
                "behavior_passed": passed,
                "token_totals": {"input": 100, "output": 10},
                "cached_input_percent": 0,
                "wall": {},
            }
            for name, (passed, runs) in outcomes.items()
        ],
    }


def test_compare_pairs_scenario_runs_case_by_case(repo):
    names = [f"case-{index:02d}" for index in range(12)]
    baseline = dict.fromkeys(names, (2, 2))
    # The candidate breaks 8 cases and adds one case that the baseline did not run.
    candidate = {name: (0, 2) if name < "case-08" else (2, 2) for name in names}
    candidate["case-12"] = (1, 2)
    run_ids = []
    for outcomes in (baseline, candidate, baseline):
        run, run_dir = launch("scenarios", "raise SystemExit(0)", with_results=True)
        finish(run_dir)
        write_json(run_dir / "traces" / "results.json", scenario_results(outcomes))
        run_ids.append(run.run_id)
    comparison = tools.compare(*run_ids[:2])
    assert (comparison.kind, comparison.paired_cases, comparison.unpaired) == (
        "scenarios",
        12,
        1,
    )
    assert (
        comparison.baseline_rate_percent,
        comparison.candidate_rate_percent,
        comparison.difference_points,
    ) == (100.0, 33.3, -66.7)
    assert (
        comparison.difference_ci95,
        comparison.detectable_change_points,
        comparison.verdict,
    ) == ((-86.6, -27.7), 42.1, "worse")
    assert comparison.worse[0] == evals.CaseChange("case-00 (zai/glm)", "2/2", "0/2")
    assert (len(comparison.worse), comparison.better) == (8, ())
    assert comparison.baseline_version == "unknown"
    assert "1 case ran on one side only." in " ".join(comparison.caveats)
    view = render("compare", comparison)
    assert view.verdict == "failed"
    assert view.summary.endswith(": worse (-66.7 points)")
    assert "95% interval -86.6 to -27.7" in words(view)
    # A rerun of the baseline shows no measurable difference.
    same = tools.compare(run_ids[0], run_ids[2])
    assert (same.verdict, same.difference_points) == ("no measurable difference", 0.0)
    assert render("compare", same).verdict == "passed"
    with pytest.raises(ValueError, match="compare two different results"):
        tools.compare(run_ids[0], run_ids[0])
    # Two repeats give pass^2 and pass@2 over the 13 scenario routes.
    report = tools.report(run_ids[1])
    assert (
        report.repeats,
        report.pass_all_percent,
        report.pass_any_percent,
        report.unstable,
    ) == (2, 30.8, 38.5, ("case-12 (zai/glm)",))
    assert "(pass^2)" in words(render("report", report))


def test_compare_names_one_version_only_when_a_build_or_clean_commit_names_it(repo):
    run_ids = []
    for build in (None, None, "b" * 64, "b" * 64):
        run, run_dir = launch("scenarios", "raise SystemExit(0)", with_results=True)
        finish(run_dir)
        outcomes = {f"case-{index:02d}": (1, 1) for index in range(4)}
        write_json(run_dir / "traces" / "results.json", scenario_results(outcomes))
        meta = evals._meta(run_dir)
        meta.update(vis_commit="a" * 40, is_dirty=True, build_sha256=build)
        write_json(run_dir / "run.json", meta)
        run_ids.append(run.run_id)
    # Two runs of one commit can have different uncommitted changes.
    caveats = " ".join(tools.compare(*run_ids[:2]).caveats)
    assert "same Vis version" not in caveats
    assert "baseline ran with uncommitted changes" in caveats
    # One build digest names the same code on both sides.
    assert "same Vis version" in " ".join(tools.compare(*run_ids[2:]).caveats)


def test_compare_pairs_benchmark_rounds_by_task(repo):
    runs = evals.BENCH / "runs"
    write_json(runs / "summary.json", summary_data())
    write_json(
        runs / "provenance.json", {"bundle_sha256": "a" * 64, "source_commit": "b" * 40}
    )
    round_runs = evals.BENCH / "rounds" / "glm-k2" / "runs"
    write_json(
        round_runs / "provenance.json",
        {"bundle_sha256": "c" * 64, "vis_commit": "d" * 40, "is_dirty": True},
    )
    later = "2026-01-03T00:00:00"
    trials = [
        trial("alpha", seconds=60, tests=(5, 5)),
        trial("alpha", seconds=60, tests=(5, 5), finished=later),
        trial("gamma", seconds=60, tests=(10, 10)),
        trial("gamma", seconds=60, tests=(9, 10), finished=later),
        trial("kappa", seconds=60, tests=(1, 1)),
    ]
    write_json(round_runs / "summary.json", summary_data() | {"trials": trials})
    comparison = tools.compare("bench", "bench:glm-k2")
    # alpha and gamma ran in both rounds; five tasks ran in one round only.
    assert (comparison.kind, comparison.paired_cases, comparison.unpaired) == (
        "bench",
        2,
        5,
    )
    assert (
        comparison.baseline_rate_percent,
        comparison.candidate_rate_percent,
        comparison.difference_points,
        comparison.verdict,
    ) == (50.0, 75.0, 25.0, "no measurable difference")
    assert comparison.better == (evals.CaseChange("gamma", "0/2", "1/2"),)
    assert comparison.baseline_version == f"commit {'b' * 12}, build {'a' * 12}"
    assert comparison.candidate_version == (
        f"commit {'d' * 12} with uncommitted changes, build {'c' * 12}"
    )
    assert any("candidate ran with uncommitted" in note for note in comparison.caveats)
    with pytest.raises(LookupError, match="Name a benchmark round as 'bench'"):
        tools.compare("bench", "missing")
    with pytest.raises(LookupError, match="no benchmark round"):
        tools.compare("bench", "bench:other")


def test_trial_shows_an_attempt_and_label_records_its_cause(repo):
    write_json(evals.BENCH / "runs" / "summary.json", summary_data())
    write_agent_limits("alpha", "beta", "gamma", "delta", "epsilon", "zeta")
    # A task name reads its latest scored attempt.
    gamma = tools.trial("gamma")
    assert (
        gamma.trial,
        gamma.outcome,
        gamma.cause,
        gamma.checks,
        gamma.budget_used,
        gamma.label,
    ) == ("gamma__0102", "failed_tests", "early_stops", "1/10", 0.1, None)
    assert tools.trial("gamma__1231").checks == "0/10"
    view = render("trial", gamma)
    assert view.summary == "gamma__0102 · failed_tests · early_stops · tests 1/10"
    with pytest.raises(LookupError, match="no attempt of 'omega'"):
        tools.trial("omega")
    label = tools.label("gamma", "other", note="It edited the wrong file.")
    assert (label.trial, label.cause, label.diagnosed_cause, label.round_name) == (
        "gamma__0102",
        "other",
        "early_stops",
        None,
    )
    saved = json.loads((evals.BENCH / "runs" / "labels.json").read_text())
    assert saved["gamma__0102"]["note"] == "It edited the wrong file."
    assert tools.trial("gamma").label == "other"
    assert "differs from the diagnose() cause early_stops" in words(
        render("label", label)
    )
    with pytest.raises(ValueError, match="Label only failed attempts"):
        tools.label("alpha", "other")
    with pytest.raises(ValueError, match="cause must be one of"):
        tools.label("gamma", "flaky")
    with pytest.raises(ValueError, match="the limit is 500"):
        tools.label("gamma", "other", note="x" * 501)
    # diagnose() compares its causes with the labels.
    check = tools.diagnose().label_check
    assert (check.labelled, check.agreement_percent, check.disagreements) == (
        1,
        0.0,
        ("gamma__0102: diagnose() early_stops, label other",),
    )
    assert check.advice.startswith("Label 29 more failed attempts.")
    assert "Label check" in words(render("diagnose", tools.diagnose()))


def scenario_report(text, results):
    return evals.ScenarioReport(
        run_id=text,
        results_path=text,
        total_runs=len(results),
        passed_runs=0,
        is_passed=False,
        pass_rate_percent=0.0,
        pass_rate_ci95=None if not results else (0.0, 50.0),
        detectable_change_points=None if not results else 20.0,
        repeats=2,
        pass_all_percent=None if not results else 0.0,
        pass_any_percent=None if not results else 0.0,
        unstable=(text,) * len(results),
        results=results,
    )


def test_activities_describe_empty_results():
    assert "No evaluation runs yet." in words(render("runs", ()))
    assert "No scenario matched." in words(render("scenarios", (), pattern="zzz"))
    diagnosis = evals.Diagnosis(0, 0, (), "", evals._label_check({}, {}, {}))
    view = render("diagnose", diagnosis)
    assert "No problem class found" in words(view)
    assert "0 labelled attempts. No labels yet." in words(view)
    board = evals.Leaderboard(
        "terminal-bench/terminal-bench", "4-0-0", "Board", (), None
    )
    assert "The board has no rows." in words(render("leaderboard", board))
    empty = render("report", scenario_report(None, ()))
    assert empty.verdict == "failed" and "no scenario summaries" in words(empty)
    assert "95% interval unknown" in words(empty)
    run = evals.EvalRun(
        "20260101-000000-scenarios-abcd",
        "scenarios",
        "1 scenario on default provider / default model",
        "running",
        None,
        1,
        "2026-01-01T00:00:00+00:00",
        None,
        "python3 e2e/run.py edit-config",
        "/tmp/output.log",
        None,
        None,
        None,
        None,
        None,
    )
    status = evals.RunStatus(run, "0 of 1 scenario runs finished", 3, ())
    assert "No log output yet." in words(render("status", status))


def test_activities_stay_within_portable_limits():
    text = "🦉" * 5000
    run = evals.EvalRun(
        text,
        "scenarios",
        text,
        "running",
        None,
        1,
        text,
        None,
        text,
        text,
        text,
        text,
        True,
        text,
        text,
    )
    scenario = evals.Scenario(text, text, text, (text,), None, False, text)
    result = evals.ScenarioResult(
        text, text, text, 1, 0, 0, False, 0.0, None, 0, 0, (text,) * 20
    )
    note = evals.TaskNote(text, text, text, None, 0.5, None, None)
    row = evals.LeaderboardRow(
        1, text, text, text, 1.0, None, 1, 0, None, 1.0, 1.0, None, None, text
    )
    standing = evals.VisStanding(
        model=text,
        round_name=text,
        scored_tasks=1,
        total_tasks=1,
        attempts_per_task=1,
        pass_rate_percent=0.0,
        pass_rate_ci95=(0.0, 50.0),
        strict_pass_rate_percent=0.0,
        strict_pass_rate_ci95=(0.0, 50.0),
        rank_by_pass_rate=1,
        rank_range=(1, 30),
        rank_by_strict_rate=1,
        strict_rank_range=(1, 30),
        cost_per_attempt_usd=None,
        cost_per_solved_usd=1.0,
        tokens_per_attempt=None,
        avg_attempt_minutes=None,
        caveats=(text,) * 5,
    )
    plan = evals.QueuePlan(
        model=text,
        cpu_tasks=1,
        attempts=3,
        completed=0,
        scored=0,
        retryable=(text,),
        live=(text,),
        pending=(text,),
        gpu_only=(text,),
        is_queue_running=False,
        stale_live=(text,) * 9,
    )
    bench = evals.BenchReport(
        dataset=text,
        round_name=text,
        model=text,
        total_tasks=1,
        scored_tasks=1,
        solved_tasks=0,
        pass_rate_percent=0.0,
        pass_rate_ci95=(0.0, 50.0),
        strict_pass_rate_percent=0.0,
        strict_pass_rate_ci95=(0.0, 50.0),
        attempts_per_task=3,
        scored_attempts=3,
        solved_attempts=1,
        attempt_pass_rate_percent=33.3,
        attempt_pass_rate_ci95=(5.0, 80.0),
        detectable_change_points=50.0,
        pass_at_k_percent=100.0,
        pass_all_k_percent=0.0,
        unstable_tasks=(text,) * 30,
        outcomes=(evals.OutcomeCount(text, 30, (text,) * 30),) * 8,
        unscored_tasks=(text,),
        gpu_tasks=(text,),
        unattempted_tasks=0,
        attempts=0,
        model_attempts=0,
        agent_hours=0.0,
        input_tokens=0,
        cached_input_percent=0.0,
        output_tokens=0,
        unparsed_output_tokens_estimate=0,
        estimated_metered_cost_usd=0.0,
        cost_per_solved_usd=1.0,
        repeated_final_errors=(text,) * 5,
        plan=plan,
        summary_path=text,
    )
    label_check = evals.LabelCheck(30, 50.0, 0.1, (text,) * 30, text)
    change = evals.CaseChange(text, text, text)
    samples = {
        "scenarios": (scenario,) * 40,
        "new_scenario": scenario,
        "run_scenarios": run,
        "run_bench": run,
        "runs": (run,) * 30,
        "status": evals.RunStatus(run, text, 1, (text,) * 20),
        "stop": run,
        "report": scenario_report(text, (result,) * 30),
        "preflight": evals.Preflight(
            False, (evals.Check(text, False, text, text),) * 12
        ),
        "bench": bench,
        "diagnose": evals.Diagnosis(
            1,
            0,
            (evals.Finding(text, text, 20, 1.0, text, (note,) * 20),) * 6,
            text,
            label_check,
        ),
        "leaderboard": evals.Leaderboard(text, text, text, (row,) * 30, standing),
        "compare": evals.Comparison(
            kind=text,
            baseline=text,
            candidate=text,
            baseline_version=text,
            candidate_version=text,
            paired_cases=1,
            baseline_rate_percent=0.0,
            candidate_rate_percent=0.0,
            difference_points=0.0,
            difference_ci95=(-50.0, 50.0),
            verdict=text,
            detectable_change_points=90.0,
            worse=(change,) * 30,
            better=(change,) * 30,
            unpaired=1,
            caveats=(text,) * 5,
        ),
        "trial": evals.TrialView(
            task=text,
            trial=text,
            round_name=text,
            outcome=text,
            cause=text,
            reward=0.0,
            checks=text,
            minutes=1.0,
            budget_used=0.5,
            iterations=1,
            error_type=text,
            tool_calls=(text,) * 8,
            verifier_tail=(text,) * 40,
            log_tail=(text,) * 40,
            trace_path=text,
            label=text,
        ),
        "label": evals.TrialLabel(text, text, text, text, text, text, text),
    }
    assert set(samples) == set(TAGS)
    for name, sample in samples.items():
        render(name, sample, pattern=text)


def test_slash_command_lists_the_newest_runs(repo):
    command = SPEC["slash_commands"][0]["run"]
    assert "No evaluation runs yet" in str(command({}))
    run, run_dir = launch("scenarios", "raise SystemExit(0)")
    finish(run_dir)
    assert f"{run.run_id}  finished (exit 0)  scenarios probe" in str(command({}))
