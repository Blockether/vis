"""Tests for the MRCR importer. All rows are synthetic."""

import json

import pytest
from blockether.vis.evals import read_tasks
from blockether.vis.evals.grade import answer_program_problem
from blockether.vis.evals.mrcr import (
    PROGRAM_FILE,
    load_rows,
    mrcr_task_name,
    task_turns,
    write_mrcr_tasks,
)

POEM = "Tapirs walk at night\nunder the old trees,\nquiet and slow."


def _row(index, n_chars=700_000, needles=2, size=0):
    messages = [
        {"role": "user", "content": "Here are some examples." + " x" * size},
        {"role": "user", "content": "write a poem about tapirs"},
        {"role": "assistant", "content": "Tapirs sleep by day."},
        {"role": "user", "content": "write a poem about tapirs"},
        {"role": "assistant", "content": POEM},
        {
            "role": "user",
            "content": "Prepend AbC123 to the 2nd (1 indexed) poem about tapirs.",
        },
    ]
    return {
        "row_idx": index,
        "prompt": json.dumps(messages),
        "answer": "AbC123" + POEM,
        "random_string_to_prepend": "AbC123",
        "n_needles": needles,
        "n_chars": n_chars,
    }


def _grade(dataset, name, answer, work):
    return answer_program_problem(
        str(dataset / name / "tests" / PROGRAM_FILE), answer, str(work)
    )


def test_turns_paste_the_conversation_and_end_with_the_request():
    turns = task_turns(_row(0, size=400), turn_tokens=100)
    assert turns[0].startswith("Over the next messages")
    assert all(
        turn.startswith("More of the earlier conversation") for turn in turns[1:-1]
    )
    history = "\n\n".join(turns[:-1])
    order = [
        history.index(text) for text in ("examples", "Tapirs sleep", "under the old")
    ]
    assert order == sorted(order)
    assert "ASSISTANT: Tapirs walk at night" in history
    assert turns[-1].endswith(
        "Prepend AbC123 to the 2nd (1 indexed) poem about tapirs."
    )
    assert "Prepend" not in history


def test_written_tasks_grade_like_the_mrcr_grader(tmp_path):
    dataset = tmp_path / "mrcr"
    (task,) = write_mrcr_tasks(dataset, [_row(7)], turn_tokens=20)
    assert task.name == mrcr_task_name(_row(7)) == "mrcr-2n-7"
    read = read_tasks(dataset)[0]
    assert read.metadata["benchmark"] == "mrcr"
    assert read.metadata["turns"] == len(read.turns) > 2
    work = tmp_path / "work"
    work.mkdir()
    assert _grade(dataset, task.name, "AbC123" + POEM + "\n", work) is None
    assert "start with" in _grade(dataset, task.name, POEM, work)
    assert "matches" in _grade(dataset, task.name, "AbC123Tapirs sleep by day.", work)


def test_load_rows_selects_by_size_and_keeps_a_cache(tmp_path):
    sizes = {800: 50_000, 801: 700_000, 802: 900_000, 803: 700_000}
    calls = []

    def fetch(index):
        calls.append(index)
        return _row(index, n_chars=sizes[index], needles=4)

    cache = tmp_path / "rows.jsonl"
    rows = load_rows(
        cache, needles=4, min_chars=600_000, max_chars=800_000, limit=2, fetch=fetch
    )
    assert [row["row_idx"] for row in rows] == [801, 803]
    assert calls == [800, 801, 802, 803]
    # The cache keeps only the size of a row out of range.
    kept = [json.loads(line) for line in cache.read_text().splitlines()]
    assert "prompt" not in kept[0] and "prompt" in kept[1]
    again = load_rows(
        cache, needles=4, min_chars=600_000, max_chars=800_000, limit=2, fetch=fetch
    )
    assert again == rows and len(calls) == 4
    with pytest.raises(ValueError, match="needles"):
        load_rows(cache, needles=3, fetch=fetch)
