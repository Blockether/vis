"""Tests for the repeat statistics of `blockether.vis.evals`."""

from pathlib import Path

from blockether.vis.evals import HarborTrial, repeat_stats, task_attempts


def _trial(task, reward, finished_at):
    return HarborTrial(
        job="job",
        trial=f"{task}-{finished_at}",
        task=task,
        agent="vis",
        agent_version=None,
        model="zai/glm",
        reward=reward,
        rewards={},
        error=None,
        input_tokens=None,
        cache_tokens=None,
        output_tokens=None,
        cost_usd=None,
        task_checksum=None,
        started_at=None,
        finished_at=finished_at,
        path=Path("result.json"),
    )


def test_task_attempts_keeps_finish_order_and_skips_unscored_trials():
    trials = [
        _trial("a", 0.0, "2"),
        _trial("a", 1.0, "1"),
        _trial("b", None, "3"),
        _trial("b", 1.0, "4"),
    ]
    assert task_attempts(trials) == {"a": [True, False], "b": [True]}


def test_repeat_stats_gives_pass_at_k_and_unstable_tasks():
    stats = repeat_stats(
        {"a": [True, False], "b": [True, True], "c": [False, False], "d": [True]}
    )
    assert stats.attempts_per_task == 2
    assert (stats.scored_attempts, stats.solved_attempts) == (7, 4)
    assert stats.attempt_pass_rate == 4 / 7
    assert (stats.pass_at_k, stats.pass_all_k) == (2 / 3, 1 / 3)
    assert stats.unstable_tasks == ("a",)
    low, high = stats.attempt_pass_rate_ci95
    assert low < 4 / 7 < high


def test_repeat_stats_without_repeats_has_no_pass_at_k():
    stats = repeat_stats({"a": [True], "b": [False]})
    assert stats.attempts_per_task == 1
    assert (stats.pass_at_k, stats.pass_all_k) == (None, None)
    empty = repeat_stats({})
    assert (empty.attempts_per_task, empty.attempt_pass_rate) == (0, None)
    assert empty.attempt_pass_rate_ci95 is None
