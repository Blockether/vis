"""Write LongMemEval questions as long multi-turn Harbor tasks.

[LongMemEval](https://github.com/xiaowu0162/LongMemEval) asks about facts that a user
gave in earlier chats with an assistant. Each question has a history of chat sessions,
and some of them hold the evidence. A task pastes that history in user turns of about
`turn_tokens` tokens each, and the agent gives a short reply. The last turn gives the current
date and the question. Only the answer to that question counts.

The tasks test what a long conversation keeps. A `haystack` task fills the history
with neutral ShareGPT and UltraChat chats until it reaches `target_tokens`. The fill
comes after the original sessions, so the evidence is in the oldest part. When the
history is larger than the context budget of the agent, the agent must fold old turns
and keep the facts in its gists. An `oracle` task gives only the evidence sessions.
Both modes give the same task names, so `compare_cases` can pair them.

The importer keeps only questions with a short answer, so a program can grade them:
every word and number of the expected answer must be in the agent's answer. It skips
preference questions, abstention questions and questions that ask to choose between
two named things. The protocol differs from LongMemEval in three ways:

- The history arrives as pasted chat logs in user turns, not as one prompt.
- The grader is a word match, not the GPT-4o judge of the paper.
- The agent must not use files, so it can keep facts only in the conversation.

Importing this module makes no network calls. `load_questions` downloads the data
once, about 280 MB. Run `python -m blockether.vis.evals.longmemeval DATASET --cache
FILE` to write the tasks.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import random
import re
import shutil
import urllib.request
from collections.abc import Iterable, Mapping, Sequence
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any

from .harbor import HarborTask, write_task

__all__ = [
    "DATA_URL",
    "MODES",
    "QUESTION_TYPES",
    "is_gradable",
    "lme_task_name",
    "load_questions",
    "task_turns",
    "write_lme_tasks",
]

DATA_URL = (
    "https://huggingface.co/datasets/xiaowu0162/longmemeval-cleaned"
    "/resolve/main/longmemeval_s_cleaned.json"
)
MODES = ("haystack", "oracle")
QUESTION_TYPES = (
    "single-session-user",
    "single-session-assistant",
    "multi-session",
    "knowledge-update",
    "temporal-reasoning",
)
PROGRAM_FILE = "check_answer.py"
EXPECTED_FILE = "expected.json"
CHARS_PER_TOKEN = 4
TARGET_TOKENS = 300_000
TURN_TOKENS = 10_000
TURN_TIMEOUT_SEC = 300.0
MAX_ANSWER_WORDS = 3
FILL_PREFIXES = ("sharegpt_", "ultrachat_")
DATE_FORMAT = "%Y/%m/%d (%a) %H:%M"

_FIRST = (
    "Over the next messages I will paste my earlier chats with an AI assistant. "
    "Later I will ask you about them. Do not create or edit files: keep what you "
    "need in this conversation. Until I ask my question, a short reply is enough."
)
_MORE = "More of my earlier chats."
_LAST = (
    "That is all of my earlier chats. The current date is {date}. "
    "Answer my question in one short sentence: {question}"
)
_STOP_WORDS = frozenset(
    "a an the in of on at to for my i and is it with from by about was were".split()
)
_TOKEN = re.compile(r"[a-z0-9]+(?:\.[0-9]+)?")

_PROGRAM = r"""# Grade a LongMemEval answer by its words. The answer is on stdin.
import json
import re
import sys
from pathlib import Path

NUMBERS = {
    word: str(value)
    for value, word in enumerate(
        "zero one two three four five six seven eight nine ten eleven twelve "
        "thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty".split()
    )
}
STOP = set("a an the in of on at to for my i and is it with from by about was were".split())


def words(text):
    found = []
    for word in re.findall(r"[a-z0-9]+(?:\.[0-9]+)?", text.lower()):
        word = NUMBERS.get(word, word)
        if word in STOP:
            continue
        found.append(word[:-1] if len(word) > 3 and word.endswith("s") else word)
    return found


expected = json.loads(Path(__file__).with_name("expected.json").read_text())
answer = set(words(sys.stdin.read()))
missing = [word for word in words(expected["answer"]) if word not in answer]
if missing:
    sys.exit(f"the answer misses {missing}; expected {expected['answer']!r}")
"""


def load_questions(cache: Path | str, *, url: str = DATA_URL) -> list[dict[str, Any]]:
    """Return the LongMemEval questions from the JSON file `cache`.

    A missing file is downloaded from `url` first. The default data is about 280 MB.
    """
    path = Path(cache)
    if not path.is_file():
        path.parent.mkdir(parents=True, exist_ok=True)
        partial = path.with_name(path.name + ".part")
        request = urllib.request.Request(url, headers={"User-Agent": "vis-evals"})
        with urllib.request.urlopen(request, timeout=600) as response:
            with partial.open("wb") as target:
                shutil.copyfileobj(response, target)
        partial.replace(path)
    questions = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(questions, list):
        raise ValueError(f"{path} is not a list of LongMemEval questions")
    return questions


def lme_task_name(question: Mapping[str, Any]) -> str:
    """Return the Harbor task name of `question`, for example `lme-e47becba`."""
    slug = re.sub(r"[^a-z0-9]+", "-", str(question["question_id"]).lower())
    return "lme-" + slug.strip("-")


def _words(text: str) -> list[str]:
    return [word for word in _TOKEN.findall(text.lower()) if word not in _STOP_WORDS]


def is_gradable(question: Mapping[str, Any]) -> bool:
    """Tell if the word grader can check `question`.

    The answer has at most three words, and the question is not a preference,
    abstention or choice question.
    """
    if question.get("question_type") not in QUESTION_TYPES:
        return False
    if str(question.get("question_id", "")).endswith("_abs"):
        return False
    if " or " in str(question.get("question", "")).lower():
        return False
    words = _words(str(question.get("answer", "")))
    return 0 < len(words) <= MAX_ANSWER_WORDS


def _session_text(date: str, messages: Sequence[Mapping[str, Any]]) -> str:
    lines = [f"### Chat of {date}"]
    for message in messages:
        role = "USER" if message.get("role") == "user" else "ASSISTANT"
        lines.append(f"{role}: {str(message.get('content', '')).strip()}")
    return "\n\n".join(lines)


def _history(question: Mapping[str, Any]) -> list[tuple[str, str, str]]:
    """Return (date, session id, text) of each session, in date order."""
    rows = zip(
        question["haystack_dates"],
        question["haystack_session_ids"],
        question["haystack_sessions"],
        strict=True,
    )
    sessions = [(date, sid, _session_text(date, chat)) for date, sid, chat in rows]
    return sorted(sessions, key=lambda row: datetime.strptime(row[0], DATE_FORMAT))


def _fill_pool(questions: Iterable[Mapping[str, Any]]) -> list[tuple[str, str]]:
    """Return the neutral sessions of all questions as (session id, messages JSON)."""
    seen: dict[str, str] = {}
    for question in questions:
        for sid, chat in zip(
            question["haystack_session_ids"], question["haystack_sessions"], strict=True
        ):
            if sid.startswith(FILL_PREFIXES) and sid not in seen:
                seen[sid] = json.dumps(chat)
    return sorted(seen.items())


def _tokens(text: str) -> int:
    return len(text) // CHARS_PER_TOKEN


def _fit(
    question: Mapping[str, Any],
    history: list[tuple[str, str, str]],
    pool: Sequence[tuple[str, str]],
    target_tokens: int,
) -> list[str]:
    evidence = set(question["answer_session_ids"])
    total = sum(_tokens(text) for _, _, text in history)
    seed = int(hashlib.sha256(str(question["question_id"]).encode()).hexdigest(), 16)
    rng = random.Random(seed)
    if total > target_tokens:
        spare = [i for i, (_, sid, _) in enumerate(history) if sid not in evidence]
        rng.shuffle(spare)
        dropped = set()
        for index in spare:
            if total <= target_tokens:
                break
            dropped.add(index)
            total -= _tokens(history[index][2])
        return [text for i, (_, _, text) in enumerate(history) if i not in dropped]
    own = {sid for _, sid, _ in history}
    candidates = [row for row in pool if row[0] not in own]
    rng.shuffle(candidates)
    last = datetime.strptime(history[-1][0], DATE_FORMAT) if history else None
    asked = datetime.strptime(str(question["question_date"]), DATE_FORMAT)
    fill: list[list[Any]] = []
    for _, chat in candidates:
        if total >= target_tokens:
            break
        messages = json.loads(chat)
        fill.append(messages)
        total += _tokens(_session_text(str(question["question_date"]), messages))
    start = last or asked
    step = (asked - start) / (len(fill) + 1) if asked > start else timedelta(0)
    texts = [text for _, _, text in history]
    for number, messages in enumerate(fill, start=1):
        texts.append(
            _session_text((start + step * number).strftime(DATE_FORMAT), messages)
        )
    return texts


def task_turns(
    question: Mapping[str, Any],
    *,
    mode: str = "haystack",
    pool: Sequence[tuple[str, str]] = (),
    target_tokens: int = TARGET_TOKENS,
    turn_tokens: int = TURN_TOKENS,
) -> list[str]:
    """Return the user messages of the task for `question`.

    `pool` gives the neutral fill sessions; `write_lme_tasks` builds it from all
    questions. An unknown mode raises ValueError.
    """
    if mode not in MODES:
        raise ValueError(f"mode must be one of {MODES}: {mode!r}")
    history = _history(question)
    if mode == "oracle":
        evidence = set(question["answer_session_ids"])
        texts = [text for _, sid, text in history if sid in evidence]
    else:
        texts = _fit(question, history, pool, target_tokens)
    turns: list[list[str]] = [[]]
    size = 0
    for text in texts:
        if turns[-1] and size + _tokens(text) > turn_tokens:
            turns.append([])
            size = 0
        turns[-1].append(text)
        size += _tokens(text)
    messages = [
        f"{_FIRST if number == 0 else _MORE}\n\n" + "\n\n".join(chunk)
        for number, chunk in enumerate(turns)
        if chunk
    ]
    last = _LAST.format(date=question["question_date"], question=question["question"])
    return [*messages, last]


def write_lme_tasks(
    dataset: Path | str,
    questions: Iterable[Mapping[str, Any]],
    *,
    mode: str = "haystack",
    types: Iterable[str] = QUESTION_TYPES,
    ids: Iterable[str] | None = None,
    limit: int | None = None,
    target_tokens: int = TARGET_TOKENS,
    turn_tokens: int = TURN_TOKENS,
) -> tuple[HarborTask, ...]:
    """Write one Harbor task for each gradable question in the folder `dataset`.

    Give all questions in `questions`: the neutral fill comes from all of them.
    `types` selects the question types, and `ids` selects question ids. `limit`
    keeps the first questions of each type in question id order. An unknown mode,
    type or id raises ValueError.
    """
    if mode not in MODES:
        raise ValueError(f"mode must be one of {MODES}: {mode!r}")
    chosen = tuple(types)
    if unknown := sorted(set(chosen) - set(QUESTION_TYPES)):
        raise ValueError(f"unknown question types {unknown}; use {QUESTION_TYPES}")
    rows = list(questions)
    wanted = None if ids is None else set(ids)
    if wanted is not None and (
        unknown := wanted - {str(q["question_id"]) for q in rows}
    ):
        raise ValueError(f"unknown question ids {sorted(unknown)}")
    pool = _fill_pool(rows) if mode == "haystack" else []
    written: list[HarborTask] = []
    for kind in chosen:
        picked = sorted(
            (
                q
                for q in rows
                if q.get("question_type") == kind
                and is_gradable(q)
                and (wanted is None or str(q["question_id"]) in wanted)
            ),
            key=lambda q: str(q["question_id"]),
        )
        for question in picked[:limit]:
            turns = task_turns(
                question,
                mode=mode,
                pool=pool,
                target_tokens=target_tokens,
                turn_tokens=turn_tokens,
            )
            history = sum(_tokens(turn) for turn in turns)
            written.append(
                write_task(
                    dataset,
                    lme_task_name(question),
                    turns,
                    {"want_answer_program": PROGRAM_FILE},
                    agent_timeout_sec=TURN_TIMEOUT_SEC * len(turns),
                    metadata={
                        "benchmark": "longmemeval",
                        "question_id": str(question["question_id"]),
                        "question_type": kind,
                        "mode": mode,
                        "turns": len(turns),
                        "history_tokens": history,
                    },
                )
            )
            tests = Path(dataset) / lme_task_name(question) / "tests"
            (tests / PROGRAM_FILE).write_text(_PROGRAM, encoding="utf-8")
            expected = {
                "answer": str(question["answer"]),
                "question": str(question["question"]),
                "question_type": kind,
            }
            (tests / EXPECTED_FILE).write_text(
                json.dumps(expected, indent=2) + "\n", encoding="utf-8"
            )
    return tuple(written)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="python -m blockether.vis.evals.longmemeval",
        description="Write LongMemEval questions as long multi-turn Harbor tasks.",
    )
    parser.add_argument("dataset", type=Path, help="folder for the new tasks")
    parser.add_argument(
        "--cache", type=Path, required=True, help="LongMemEval_S JSON file"
    )
    parser.add_argument("--mode", choices=MODES, default="haystack")
    parser.add_argument(
        "--type",
        dest="types",
        action="append",
        choices=QUESTION_TYPES,
        help="question type to write; repeat it for more types (default: all)",
    )
    parser.add_argument(
        "--id",
        dest="ids",
        action="append",
        help="question id to write; repeat it for more questions (default: all)",
    )
    parser.add_argument("--limit", type=int, help="questions for each type")
    parser.add_argument("--target-tokens", type=int, default=TARGET_TOKENS)
    parser.add_argument("--turn-tokens", type=int, default=TURN_TOKENS)
    args = parser.parse_args(argv)
    written = write_lme_tasks(
        args.dataset,
        load_questions(args.cache),
        mode=args.mode,
        types=args.types or QUESTION_TYPES,
        ids=args.ids,
        limit=args.limit,
        target_tokens=args.target_tokens,
        turn_tokens=args.turn_tokens,
    )
    print(f"wrote {len(written)} tasks to {args.dataset}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
