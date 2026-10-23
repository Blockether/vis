"""Tests for the Lost in Conversation importer. All samples are synthetic."""

import json

import pytest
from blockether.vis.evals import read_tasks
from blockether.vis.evals.grade import answer_program_problem
from blockether.vis.evals.harbor import split_turns
from blockether.vis.evals.lic import (
    EXPECTED_FILE,
    PROGRAM_FILE,
    lic_task_name,
    load_samples,
    write_lic_tasks,
)

MATH = {
    "task": "math",
    "task_id": "sharded-GSM8K/7",
    "answer": "Two boxes hold 2*6 = <<2*6=12>>12 apples.\n#### 12",
    "shards": [
        {"shard_id": 2, "shard": "Each box holds 6 apples."},
        {"shard_id": 1, "shard": "How many apples are there?"},
        {"shard_id": 3, "shard": "There are two boxes."},
    ],
}
CODE = {
    "task": "code",
    "source": "humaneval",
    "task_id": "sharded-HumanEval/3",
    "prompt": "from typing import List\n\ndef total(values: List[int]) -> int:\n    pass\n",
    "test": "def check(candidate):\n    assert candidate([1, 2, 3]) == 6\n",
    "metadata": {"func_name": "total"},
    "shards": "[{'shard_id': 1, 'shard': 'Add numbers'}, "
    "{'shard_id': 2, 'shard': 'The input is a list of integers'}]",
}
LIVECODE = {**CODE, "source": "lcb_medium", "task_id": "sharded-LCB/9"}


def _grade(dataset, name, answer, work):
    return answer_program_problem(
        str(dataset / name / "tests" / PROGRAM_FILE), answer, str(work)
    )


def test_sharded_tasks_give_one_shard_in_each_turn(tmp_path):
    dataset = tmp_path / "lic"
    written = write_lic_tasks(dataset, [MATH, CODE, LIVECODE])
    assert [task.name for task in written] == ["lic-gsm8k-7", "lic-humaneval-3"]
    math = read_tasks(dataset)[0]
    turns = split_turns(math.instruction)
    assert len(turns) == 3 and turns[0].endswith("How many apples are there?")
    assert turns[1] == "Each box holds 6 apples."
    assert turns[2].startswith("There are two boxes.")
    assert "final answer now" in turns[2]
    assert math.metadata["turns"] == 3 and math.metadata["mode"] == "sharded"
    checks = json.loads((dataset / "lic-gsm8k-7" / "tests" / "checks.json").read_text())
    assert checks == {"want_answer_program": PROGRAM_FILE}
    assert (dataset / "lic-gsm8k-7" / "tests" / EXPECTED_FILE).is_file()


def test_concat_tasks_have_the_same_names_and_one_turn(tmp_path):
    dataset = tmp_path / "concat"
    written = write_lic_tasks(dataset, [MATH, CODE], mode="concat", tasks=["math"])
    assert [task.name for task in written] == [lic_task_name(MATH)]
    (turn,) = split_turns(written[0].instruction)
    assert "- How many apples are there?" in turn and "- There are two boxes." in turn


def test_the_math_grader_follows_the_lic_evaluator(tmp_path):
    dataset = tmp_path / "lic"
    write_lic_tasks(dataset, [MATH], tasks=["math"])
    name = lic_task_name(MATH)
    assert _grade(dataset, name, "12 apples.\nFINAL ANSWER: 12", tmp_path) is None
    assert _grade(dataset, name, "FINAL ANSWER: $12.00", tmp_path) is None
    problem = _grade(dataset, name, "FINAL ANSWER: 13", tmp_path)
    assert problem.endswith("the answer 13 is not 12")
    assert _grade(dataset, name, "I need more details.", tmp_path).endswith(
        "the answer has no number"
    )


def test_the_code_grader_runs_the_hidden_tests(tmp_path):
    dataset = tmp_path / "lic"
    write_lic_tasks(dataset, [CODE], tasks=["code"])
    name = lic_task_name(CODE)
    good = (
        "```python\ndef add_all(values: List[int]) -> int:\n    return sum(values)\n```"
    )
    assert _grade(dataset, name, good, tmp_path) is None
    bad = "```python\ndef total(values):\n    return 0\n```"
    assert "the hidden tests failed" in _grade(dataset, name, bad, tmp_path)
    assert "no python code block" in _grade(dataset, name, "Use sum().", tmp_path)
    example = good + "\n\nExample:\n```python\nadd_all([1, 2])\n```"
    assert _grade(dataset, name, example, tmp_path) is None
    assert "defines a function" in _grade(
        dataset, name, "```python\nprint(1)\n```", tmp_path
    )


def test_load_samples_reads_the_cache_without_a_download(tmp_path):
    cache = tmp_path / "lic.json"
    cache.write_text(json.dumps([MATH]))
    assert load_samples(cache, url="http://127.0.0.1:9/never") == [MATH]
    cache.write_text("{}")
    with pytest.raises(ValueError, match="not a list"):
        load_samples(cache)


def test_unknown_modes_and_tasks_are_errors(tmp_path):
    with pytest.raises(ValueError, match="mode"):
        write_lic_tasks(tmp_path / "a", [MATH], mode="shuffled")
    with pytest.raises(ValueError, match="unknown LiC tasks"):
        write_lic_tasks(tmp_path / "b", [MATH], tasks=["summary"])
