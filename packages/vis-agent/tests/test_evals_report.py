"""Tests for the round report command of `blockether.vis.evals`."""

import json

from blockether.vis.evals import REPORT_FILE, render_report, round_report
from blockether.vis.evals.report import main

MODEL = "glm-5.3"


def add_trial(jobs, task, attempt, *, reward, tests=(1, 2)):
    trial = f"{task}__{attempt}"
    folder = jobs / trial
    (folder / "agent").mkdir(parents=True)
    (folder / "verifier").mkdir()
    result = {
        "task_name": f"bench/{task}",
        "trial_name": trial,
        "started_at": f"2026-01-02T00:0{attempt}:00Z",
        "finished_at": f"2026-01-02T00:0{attempt}:30Z",
        "agent_execution": {
            "started_at": f"2026-01-02T00:0{attempt}:00Z",
            "finished_at": f"2026-01-02T00:0{attempt}:20Z",
        },
        "agent_result": {"n_input_tokens": 100, "n_output_tokens": 10},
        "verifier_result": {"rewards": {"reward": reward}},
    }
    (folder / "result.json").write_text(json.dumps(result))
    atif = {
        "schema_version": "ATIF-v1.6",
        "agent": {"name": "vis", "model_name": MODEL},
        "steps": [
            {
                "step_id": 1,
                "source": "agent",
                "message": "",
                "metrics": {"completion_tokens": 10},
                "tool_calls": [],
            }
        ],
        "final_metrics": {"total_completion_tokens": 10},
    }
    (folder / "agent/trajectory.json").write_text(json.dumps(atif))
    report = {"results": {"summary": {"tests": tests[1], "passed": tests[0]}}}
    (folder / "verifier/ctrf.json").write_text(json.dumps(report))
    return trial


def round_jobs(tmp_path):
    # Task a solves 2 of 4 attempts, task b all 4 and task c none.
    jobs = tmp_path / "jobs" / "first"
    for attempt, flags in enumerate([(1, 1, 0), (1, 1, 0), (0, 1, 0), (0, 1, 0)]):
        for task, flag in zip("abc", flags, strict=True):
            add_trial(jobs, task, attempt + 1, reward=float(flag))
    return jobs


def test_round_report_gives_unbiased_pass_at_k_for_each_k(tmp_path):
    report = round_report(round_jobs(tmp_path))
    assert report.score.attempts_per_task == 4
    # pass@2 of task a is 1 - C(2,2)/C(4,2) = 5/6, and pass^2 is 1/6.
    rows = [
        (row.k, row.pass_at_k_percent, row.pass_all_k_percent) for row in report.repeats
    ]
    assert rows == [
        (1, 50.0, 50.0),
        (2, round(100 * (5 / 6 + 1) / 3, 1), round(100 * (1 / 6 + 1) / 3, 1)),
        (3, 66.7, 33.3),
        (4, 66.7, 33.3),
    ]
    assert report.judge is None


def test_round_report_checks_the_labels_and_the_judge(tmp_path):
    jobs = round_jobs(tmp_path)
    labels = {"c__1": {"cause": "other"}, "c__2": {"cause": "early_stops"}}
    (jobs / "labels.json").write_text(json.dumps(labels))
    judge = tmp_path / "judge"
    judge.mkdir()
    samples = {"c__1": ["write", "write"], "c__2": ["verify", "verify"]}
    (judge / "judge_stages.json").write_text(json.dumps(samples))
    (judge / "stage-labels.json").write_text(json.dumps({"c__1": {"stage": "write"}}))

    report = round_report(jobs, judge_dir=judge)

    check = report.diagnosis.label_check
    assert check.labelled == 2
    assert check.disagreements == ("c__2: diagnose() other, label early_stops",)
    assert report.judge.sample_agreement_percent == 100.0
    assert report.judge.sample_alpha == 1.0
    assert report.judge.labelled == 1
    text = render_report(report)
    assert "Cause labels: 2 labels" in text
    assert "Krippendorff alpha 1.0" in text
    assert "Read the trajectories of failed attempts" in text


def test_main_prints_the_report_and_writes_its_json(tmp_path, capsys):
    jobs = round_jobs(tmp_path)
    assert main([str(jobs)]) == 0
    out = capsys.readouterr().out
    assert out.startswith("Round first - dataset unknown, model glm-5.3\n")
    assert f"Report: {jobs / REPORT_FILE}" in out
    saved = json.loads((jobs / REPORT_FILE).read_text())
    assert saved["score"]["solved_tasks"] == 1
    assert saved["diagnosis"]["label_check"]["alpha"] is None

    assert main([str(jobs), "--json", "--no-write", "--round", "second"]) == 0
    printed = json.loads(capsys.readouterr().out)
    assert printed["score"]["round_name"] == "second"


def test_render_report_lists_near_misses_with_the_causes(tmp_path):
    jobs = tmp_path / "jobs" / "near"
    add_trial(jobs, "a", 1, reward=1.0)
    add_trial(jobs, "b", 1, reward=0.0, tests=(9, 10))
    text = render_report(round_report(jobs))
    assert (
        "  near_misses: 1 failed task passed at least 80% of their verifier tests (b)"
        in text
    )


def test_main_fails_without_trials(tmp_path, capsys):
    assert main([str(tmp_path)]) == 1
    assert "no Harbor trials" in capsys.readouterr().err
    assert not (tmp_path / REPORT_FILE).exists()
