"""Tests for the e2e runner that runs the scenarios as one Harbor job."""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest
from blockether.vis.evals import seal_digest

from vis_harbor import dataset, run

SCENARIO = "py-fix-body"
FAKE_HARBOR = """#!{python}
# Write one finished trial for each task and model, as Harbor does.
import json, sys
from pathlib import Path

args = sys.argv[2:]
options = {{}}
models = []
for name, value in zip(args[::2], args[1::2]):
    if name == "--model":
        models.append(value)
    else:
        options[name] = value
job = Path(options["--jobs-dir"]) / options["--job-name"]
grade = Path({grade!r})
for task in sorted(Path(options["--path"]).iterdir()):
    for model in models:
        for attempt in range(int(options["--n-attempts"])):
            trial = job / f"{{task.name}}__{{model.replace('/', '-')}}{{attempt}}"
            (trial / "agent").mkdir(parents=True)
            if grade.is_file():
                (trial / "verifier").mkdir()
                (trial / "verifier" / "grade.json").write_text(grade.read_text())
            result = {{
                "task_name": task.name,
                "trial_name": trial.name,
                "finished_at": f"2026-10-12T00:00:0{{attempt}}Z",
                "config": {{"agent": {{"model_name": model}}}},
                "exception_info": None if grade.is_file() else {{"exception_type": "AgentTimeoutError"}},
                "verifier_result": {{"rewards": {{"reward": 1.0}}}} if grade.is_file() else None,
            }}
            (trial / "result.json").write_text(json.dumps(result))
"""


def _settings(**changes) -> run.Settings:
    values = {
        "results": Path("/tmp/results"),
        "repeats": 1,
        "workers": 5,
        "provider": "p",
        "models": ("m",),
        "timeout_sec": None,
        "reasoning_effort": None,
        "native_bin": None,
    }
    return run.Settings(**{**values, **changes})


def _scenario() -> dataset.Scenario:
    return dataset.load_scenarios([SCENARIO])[0]


def _passed(**changes) -> dict:
    record = run.failure_record(_scenario(), "p", "m", "unused")
    record.update(
        converged=True, correct=True, errors=0, err_msgs=[], detail=[], passed=True
    )
    return {**record, **changes}


def _trial(job, name, *, finished_at, grade_text=None, model="p/m"):
    trial = job / name
    (trial / "agent").mkdir(parents=True)
    if grade_text is not None:
        (trial / "verifier").mkdir()
        (trial / "verifier" / run.GRADE_FILE).write_text(grade_text)
    exception = None if grade_text else {"exception_type": "AgentTimeoutError"}
    result = {
        "task_name": name.split(".")[0],
        "trial_name": name,
        "finished_at": finished_at,
        "config": {"agent": {"model_name": model}},
        "exception_info": exception,
    }
    (trial / "result.json").write_text(json.dumps(result))


def test_settings_keep_the_e2e_defaults():
    assert run.read_settings({}) == run.Settings(
        results=Path("/tmp/vis_e2e/results"),
        repeats=1,
        workers=5,
        provider="zai-coding-plan",
        models=("glm-5.3-flash",),
        timeout_sec=None,
        reasoning_effort=None,
        native_bin=None,
    )


def test_settings_read_the_models_and_the_numbers():
    settings = run.read_settings(
        {
            "VIS_E2E_RESULTS": "/data/results",
            "VIS_E2E_REPEATS": "3",
            "VIS_E2E_WORKERS": "2",
            "VIS_PROVIDER": "openai-codex",
            "VIS_MODEL": "ignored",
            "VIS_MODELS": "gpt-a, gpt-b,,",
            "VIS_E2E_TIMEOUT": "720",
            "VIS_REASONING_EFFORT": " low ",
            "VIS_E2E_NATIVE_BIN": "/opt/vis",
        }
    )
    assert settings == _settings(
        results=Path("/data/results"),
        repeats=3,
        workers=2,
        provider="openai-codex",
        models=("gpt-a", "gpt-b"),
        timeout_sec=720,
        reasoning_effort="low",
        native_bin="/opt/vis",
    )
    assert run.read_settings({"VIS_MODEL": "glm-5.3"}).models == ("glm-5.3",)


@pytest.mark.parametrize(
    "name", ["VIS_E2E_REPEATS", "VIS_E2E_WORKERS", "VIS_E2E_TIMEOUT"]
)
@pytest.mark.parametrize("value", ["0", "-1", "two"])
def test_invalid_numbers_stop_the_run(monkeypatch, capsys, name, value):
    monkeypatch.setenv(name, value)
    with pytest.raises(ValueError, match=f"{name} must be a positive integer"):
        run.read_settings({name: value})
    assert run.main([SCENARIO]) == 2
    assert f"{name} must be a positive integer" in capsys.readouterr().err


def test_unknown_scenario_stops_the_run(capsys):
    assert run.main(["no-such-scenario"]) == 2
    assert "unknown scenario: no-such-scenario" in capsys.readouterr().err


def test_one_harbor_job_runs_every_model(monkeypatch):
    monkeypatch.setattr(run, "harbor_executable", lambda: "harbor")
    settings = _settings(models=("a", "b"), repeats=3, workers=2)
    command = run.harbor_command(settings, Path("/d"), Path("/j"), "job")
    assert command == [
        "harbor", "run", "--path", "/d",
        "--agent", "vis_harbor.agent:VisSourceAgent",
        "--env", "vis_harbor.environment:HostEnvironment",
        "--model", "p/a", "--model", "p/b",
        "--jobs-dir", "/j", "--job-name", "job",
        "--n-concurrent", "2", "--n-attempts", "3", "--quiet",
    ]  # fmt: skip


def test_finished_trials_read_the_job_in_finish_order(tmp_path):
    job = tmp_path / "job"
    assert run.finished_trials(job) == []
    _trial(job, f"{SCENARIO}.b", finished_at="2026-10-12T00:00:02Z")
    _trial(job, f"{SCENARIO}.a", finished_at="2026-10-12T00:00:03Z")
    _trial(job, f"{SCENARIO}.c", finished_at=None)
    trials = run.finished_trials(job)
    assert [trial.trial for trial in trials] == [f"{SCENARIO}.b", f"{SCENARIO}.a"]
    assert [trial.error for trial in trials] == ["AgentTimeoutError"] * 2


def test_collector_reads_grades_and_numbers_runs_in_finish_order(tmp_path):
    job = tmp_path / "job"
    grade_text = json.dumps(_passed(model="m"))
    graded = f"{SCENARIO}.b"
    _trial(job, graded, finished_at="2026-10-12T00:00:02Z", grade_text=grade_text)
    _trial(job, f"{SCENARIO}.a", finished_at="2026-10-12T00:00:01Z")
    _trial(job, f"{SCENARIO}.c", finished_at=None)
    collector = run.Collector(_settings(repeats=2), [_scenario()], job)
    collector.collect()
    collector.collect()
    first, second = collector.results()
    assert (first["repeat"], first["passed"], first["err_msgs"]) == (
        1,
        False,
        ["Harbor trial has no grade: AgentTimeoutError"],
    )
    assert (second["repeat"], second["passed"], second["model"]) == (2, True, "m")


def test_each_missing_run_gets_a_failure_record(tmp_path):
    collector = run.Collector(
        _settings(results=tmp_path / "results", repeats=2), [_scenario()], tmp_path
    )
    records = collector.results()
    assert [(r["id"], r["model"], r["repeat"]) for r in records] == [
        (SCENARIO, "m", 1),
        (SCENARIO, "m", 2),
    ]
    assert all(
        r["err_msgs"] == ["Harbor did not finish this trial"] and not r["passed"]
        for r in records
    )
    assert run.summarize_results(records)[0]["passed"] == 0


def _run_main(monkeypatch, tmp_path, grade_record, **env):
    grade = tmp_path / "grade.json"
    if grade_record is not None:
        grade.write_text(json.dumps(grade_record))
    fake = tmp_path / "harbor"
    fake.write_text(FAKE_HARBOR.format(python=sys.executable, grade=str(grade)))
    fake.chmod(0o755)
    monkeypatch.setattr(run, "harbor_executable", lambda: str(fake))
    monkeypatch.setattr(run, "POLL_SEC", 0.05)
    results = tmp_path / "out" / "results"
    monkeypatch.setenv("VIS_E2E_RESULTS", str(results))
    monkeypatch.setenv("VIS_PROVIDER", "p")
    monkeypatch.delenv("VIS_MODEL", raising=False)
    for name, value in env.items():
        monkeypatch.setenv(name, value)
    code = run.main([SCENARIO])
    return code, results


def test_main_writes_the_results_and_passes_the_gate(monkeypatch, tmp_path, capsys):
    code, directory = _run_main(
        monkeypatch, tmp_path, _passed(model="m"), VIS_MODELS="m", VIS_E2E_TIMEOUT="7"
    )
    assert code == 0
    assert sorted(path.name for path in directory.iterdir()) == ["results.json"]
    results = json.loads((directory / "results.json").read_text())
    assert [(r["id"], r["model"], r["repeat"]) for r in results["runs"]] == [
        (SCENARIO, "m", 1)
    ]
    assert results["summaries"][0]["passed"] == 1
    task = directory.parent / "harbor" / dataset.DATASET_NAME / SCENARIO
    assert json.loads((task / "tests" / "scenario.json").read_text())["timeout_s"] == 7
    job = Path(results["job_dir"])
    assert job.parent == directory.parent / "harbor" / "jobs"
    seal = json.loads((job / run.SEAL_FILE).read_text())
    assert (seal["round"], seal["digest"]) == (job.name, seal_digest(seal))
    assert [(row["task"], row["reward"]) for row in seal["rows"]] == [(SCENARIO, 1.0)]
    record = seal["runs"][0]
    assert (record["profiles"]["models"], record["tasks"]) == (["m"], [SCENARIO])
    assert sorted(record["pins"]) == ["harbor", "vis_agent"]
    # A job gets one seal only.
    assert run.write_seal(job, record) is None
    out = capsys.readouterr().out
    assert "GATE (scenario passes iff ALL 1 model(s) pass cleanly): 1/1" in out


def test_main_fails_the_gate_when_one_model_fails(monkeypatch, tmp_path, capsys):
    code, directory = _run_main(monkeypatch, tmp_path, None, VIS_MODELS="m,n")
    assert code == 1
    runs = json.loads((directory / "results.json").read_text())["runs"]
    assert [(r["model"], r["passed"]) for r in runs] == [("m", False), ("n", False)]
    assert "GATE (scenario passes iff ALL 2 model(s) pass cleanly): 0/1" in (
        capsys.readouterr().out
    )
