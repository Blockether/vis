"""Tests for the Terminal-Bench round score and diagnosis of blockether.vis.evals."""

from blockether.vis.evals import (
    LEADERBOARD_URL,
    Agreement,
    agent_limits,
    attempt_cause,
    bench_report,
    cause_check,
    diagnose,
    find_trial,
    load_labels,
    read_leaderboard,
    save_label,
    summary_attempts,
)

MODEL = "zai/glm-5.3"


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


def summary():
    def tasks(*names):
        return [f"terminal-bench/{name}" for name in names]

    return {
        "dataset": "terminal-bench@2.0",
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


LIMITS = dict.fromkeys(("alpha", "beta", "gamma", "delta", "epsilon", "zeta"), 1000.0)


def test_bench_report_scores_the_latest_attempt_with_a_strict_rate():
    report = bench_report(summary(), "glm-k2")
    assert (report.round_name, report.model, report.solved_tasks) == (
        "glm-k2",
        MODEL,
        2,
    )
    assert (report.pass_rate_percent, report.strict_pass_rate_percent) == (33.3, 20.0)
    assert report.outcomes[0].outcome == "failed_tests"
    assert (report.cached_input_percent, report.estimated_metered_cost_usd) == (
        25.0,
        3.46,
    )
    assert summary_attempts(summary())["gamma"] == [False, False]


def test_diagnose_ranks_the_findings_and_estimates_full_attempts():
    diagnosis = diagnose(summary(), limits=LIMITS)
    assert [finding.kind for finding in diagnosis.findings] == [
        "vis_errors",
        "early_stops",
        "near_misses",
        "agent_timeouts",
        "dead_tools",
        "unscored",
    ]
    assert diagnosis.estimate.startswith("2 of 5 tasks with a full attempt passed")
    # Without time limits, no attempt counts as an early stop.
    assert "early_stops" not in [f.kind for f in diagnose(summary()).findings]


def test_attempt_cause_and_cause_check_follow_the_diagnose_rules():
    trials = {t.get("trial"): t for t in summary()["trials"]}
    causes = {
        name: attempt_cause(trials[name], summary(), LIMITS)
        for name in ("alpha__0102", "gamma__0102", "zeta__abc", "epsilon__0102")
    }
    assert causes == {
        "alpha__0102": "solved",
        "gamma__0102": "early_stops",
        "zeta__abc": "dead_tools",
        "epsilon__0102": "vis_errors",
    }
    check = cause_check(summary(), {"gamma__0102": {"cause": "early_stops"}}, LIMITS)
    assert isinstance(check, Agreement)
    assert check.labelled == 1
    assert check.advice.startswith("Label 29 more failed attempts.")


def test_agent_limits_reads_task_toml_files(tmp_path):
    (tmp_path / "alpha").mkdir()
    (tmp_path / "alpha" / "task.toml").write_text("[agent]\ntimeout_sec = 900\n")
    (tmp_path / "beta").mkdir()
    (tmp_path / "beta" / "task.toml").write_text("not toml [")
    assert agent_limits(tmp_path) == {"alpha": 900.0}
    assert agent_limits(tmp_path / "missing") == {}


def test_save_label_keeps_one_label_for_each_failed_attempt(tmp_path):
    path = tmp_path / "runs" / "labels.json"
    assert load_labels(path) == {}
    label = save_label(
        path, summary(), "gamma", "early_stops", "stopped", limits=LIMITS
    )
    assert (label.trial, label.diagnosed_cause) == ("gamma__0102", "early_stops")
    assert load_labels(path)["gamma__0102"]["cause"] == "early_stops"
    assert find_trial(summary(), "zeta__abc")["task"] == "terminal-bench/zeta"
    for task, cause, error in (
        ("alpha", "other", "solved its task"),
        ("gamma", "bad", "cause must be one of"),
        ("nothing", "other", "no attempt of"),
    ):
        try:
            save_label(path, summary(), task, cause)
        except (LookupError, ValueError) as raised:
            assert error in str(raised)
        else:
            raise AssertionError(f"{task} {cause} was saved")


def test_read_leaderboard_reads_every_page_and_places_the_round():
    def row(rank, model, accuracy):
        return {
            "rank": rank,
            "metadata": {"agent_display": {"label": "Agent"}, "model_display": model},
            "metrics": {"accuracy": accuracy, "n_trials": 50, "successes": 10},
        }

    pages = {
        1: [row(2, "Model B", 40.0), row(1, "Model A", 60.0)],
        2: [row(3, "GLM", 20.0)],
    }
    calls = []

    def post(url, payload):
        calls.append((url, payload["page"]))
        return {"rows": pages[payload["page"]], "pagination": {"total_pages": 2}}

    data = summary() | {"dataset": "terminal-bench/terminal-bench@4.0.0"}
    board = read_leaderboard(summary=data, post=post)
    assert calls == [(LEADERBOARD_URL, 1), (LEADERBOARD_URL, 2)]
    assert [row.model for row in board.rows] == ["Model A", "Model B", "GLM"]
    assert board.vis is not None and board.vis.rank_by_pass_rate == 3
    assert read_leaderboard(summary=summary(), post=post).vis is None
