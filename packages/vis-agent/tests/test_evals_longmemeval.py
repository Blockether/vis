"""Tests for the LongMemEval importer. All questions are synthetic."""

import json

import pytest
from blockether.vis.evals import read_tasks
from blockether.vis.evals.grade import answer_program_problem
from blockether.vis.evals.longmemeval import (
    PROGRAM_FILE,
    is_gradable,
    lme_task_name,
    task_turns,
    write_lme_tasks,
)


def _chat(text, size=0):
    return [
        {"role": "user", "content": text + " x" * size},
        {"role": "assistant", "content": "Noted."},
    ]


def _question(qid, answer="Lisbon", kind="single-session-user", **more):
    question = {
        "question_id": qid,
        "question_type": kind,
        "question": "Where do I live?",
        "question_date": "2023/05/30 (Tue) 23:40",
        "answer": answer,
        "answer_session_ids": [f"answer_{qid}"],
        "haystack_dates": [
            "2023/05/21 (Sun) 10:00",
            "2023/05/20 (Sat) 09:00",
            "2023/05/22 (Mon) 11:00",
        ],
        "haystack_session_ids": [f"answer_{qid}", f"sharegpt_{qid}", f"abc_{qid}"],
        "haystack_sessions": [
            _chat("I moved to Lisbon.", 50),
            _chat("Explain recursion.", 50),
            _chat("My sister lives in Porto.", 50),
        ],
    }
    question.update(more)
    return question


def _grade(dataset, name, answer, work):
    return answer_program_problem(
        str(dataset / name / "tests" / PROGRAM_FILE), answer, str(work)
    )


@pytest.mark.parametrize(
    ("question", "gradable"),
    [
        (_question("q1"), True),
        (_question("q2", answer=3, kind="multi-session"), True),
        (_question("q3_abs"), False),
        (_question("q4", kind="single-session-preference"), False),
        (_question("q5", answer="a long answer with many words"), False),
        (_question("q6", question="Did I go to Rome or Paris first?"), False),
    ],
)
def test_is_gradable_keeps_short_answers_only(question, gradable):
    assert is_gradable(question) is gradable


def test_haystack_turns_fill_the_history_after_the_evidence():
    question = _question("q1")
    pool = [("ultrachat_fill1", json.dumps(_chat("Fill one.", 400)))] * 1 + [
        ("ultrachat_fill2", json.dumps(_chat("Fill two.", 400)))
    ]
    turns = task_turns(question, pool=pool, target_tokens=1_500, turn_tokens=400)
    history = "\n\n".join(turns[:-1])
    assert turns[0].startswith("Over the next messages")
    assert all(turn.startswith("More of my earlier chats") for turn in turns[1:-1])
    # The original sessions come in date order, then the neutral fill.
    order = [history.index(text) for text in ("recursion", "Lisbon", "Porto", "Fill")]
    assert order == sorted(order)
    assert "2023/05/30 (Tue) 23:40" in turns[-1] and turns[-1].endswith(
        "Where do I live?"
    )
    assert (
        task_turns(question, pool=pool, target_tokens=1_500, turn_tokens=400) == turns
    )


def test_a_small_target_drops_only_sessions_without_evidence():
    turns = task_turns(_question("q1"), target_tokens=60)
    history = "\n\n".join(turns[:-1])
    assert "Lisbon" in history
    assert "recursion" not in history and "Porto" not in history


def test_oracle_turns_hold_only_the_evidence():
    turns = task_turns(_question("q1"), mode="oracle")
    assert len(turns) == 2
    assert "Lisbon" in turns[0] and "Porto" not in turns[0]
    with pytest.raises(ValueError, match="mode"):
        task_turns(_question("q1"), mode="full")


def test_written_tasks_grade_the_words_of_the_answer(tmp_path):
    dataset = tmp_path / "lme"
    questions = [
        _question("q1"),
        _question("q2", answer="two", kind="multi-session"),
        _question("q3_abs"),
    ]
    written = write_lme_tasks(dataset, questions, target_tokens=400, turn_tokens=200)
    assert [task.name for task in written] == ["lme-q1", "lme-q2"]
    task = read_tasks(dataset)[0]
    assert task.metadata["benchmark"] == "longmemeval"
    assert task.metadata["turns"] == len(task.turns) > 2
    work = tmp_path / "work"
    work.mkdir()
    assert _grade(dataset, "lme-q1", "You live in Lisbon.", work) is None
    assert "lisbon" in _grade(dataset, "lme-q1", "You live in Porto.", work)
    assert _grade(dataset, "lme-q2", "You went to 2 of them.", work) is None
    assert _grade(dataset, "lme-q2", "Three.", work) is not None
    assert lme_task_name({"question_id": "Q_1"}) == "lme-q-1"
    with pytest.raises(ValueError, match="question types"):
        write_lme_tasks(dataset, questions, types=["poetry"])


def test_ids_select_questions_and_the_fill_uses_all_of_them(tmp_path):
    questions = [_question("q1"), _question("q2")]
    (task,) = write_lme_tasks(
        tmp_path, questions, ids=["q1"], target_tokens=10_000, turn_tokens=200
    )
    # The neutral chat of q2 fills the history of q1.
    assert task.name == "lme-q1" and "q2" not in task.instruction
    assert task.instruction.count("Explain recursion.") == 2
    with pytest.raises(ValueError, match="unknown question ids"):
        write_lme_tasks(tmp_path / "other", questions, ids=["q9"])
