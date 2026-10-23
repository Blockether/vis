"""Write OpenAI MRCR conversations as long multi-turn Harbor tasks.

[MRCR](https://huggingface.co/datasets/openai/mrcr) (multi-round coreference resolution)
hides several similar requests in one long conversation, for example two requests for
a poem about tapirs. The last request asks for one of the earlier answers, for example
the second poem about tapirs, word for word, with a random string at the start. The
model must find the correct answer among similar ones and repeat it exactly.

A task pastes the earlier conversation in user turns of about `turn_tokens` tokens
each, and the agent gives a short reply. The last turn gives the request. Only the
answer to that request counts. When the conversation is larger than the context budget
of the agent, the agent must fold old turns. A fold note seldom keeps a whole answer
word for word, so MRCR shows what a fold loses. Compare it with
`blockether.vis.evals.longmemeval`, which asks only for short facts.

The grader follows the MRCR grader: the answer must start with the random string, and
`difflib.SequenceMatcher` compares the rest with the expected answer. The MRCR score is
that ratio. A Harbor reward is a pass or a failure, so a task passes when the ratio is
at least `MIN_RATIO`. The protocol differs from MRCR in two ways:

- The conversation arrives as pasted text in user turns, not as one prompt with
  assistant messages.
- The score is a pass at `MIN_RATIO`, not the mean ratio.

Importing this module makes no network calls. `load_rows` downloads only the rows that
it needs, about 1 MB for each long row. Run `python -m blockether.vis.evals.mrcr DATASET
--cache FILE` to write the tasks.
"""

from __future__ import annotations

import argparse
import json
import urllib.request
from collections.abc import Callable, Iterable, Mapping, Sequence
from pathlib import Path
from typing import Any

from .harbor import HarborTask, write_task

__all__ = [
    "MIN_RATIO",
    "NEEDLES",
    "fetch_row",
    "load_rows",
    "mrcr_task_name",
    "task_turns",
    "write_mrcr_tasks",
]

ROWS_URL = (
    "https://datasets-server.huggingface.co/rows"
    "?dataset=openai/mrcr&config=default&split=train&offset={index}&length=1"
)
NEEDLES = {2: 0, 4: 800, 8: 1600}
"""The first row of each needle count. Each needle count has `ROWS_PER_NEEDLES` rows."""
ROWS_PER_NEEDLES = 800
PROGRAM_FILE = "check_answer.py"
EXPECTED_FILE = "expected.json"
CHARS_PER_TOKEN = 4
TURN_TOKENS = 10_000
TURN_TIMEOUT_SEC = 300.0
MIN_RATIO = 0.9
MIN_CHARS = 600_000
MAX_CHARS = 1_300_000

_FIRST = (
    "Over the next messages I will paste an earlier conversation between a user and "
    "an AI assistant. Later I will ask you about it. Do not create or edit files: keep "
    "what you need in this conversation. Until I give my request, a short reply is "
    "enough."
)
_MORE = "More of the earlier conversation."
_LAST = (
    "That is the whole earlier conversation. Answer my request about it. Give only the "
    "requested text, without a code block or other words: {request}"
)

_PROGRAM = r"""# Grade an MRCR answer like the MRCR grader. The answer is on stdin.
import json
import sys
from difflib import SequenceMatcher
from pathlib import Path

expected = json.loads(Path(__file__).with_name("expected.json").read_text())
prefix = expected["prefix"]
answer = sys.stdin.read().strip()
if not answer.startswith(prefix):
    sys.exit(f"the answer does not start with {prefix!r}: {answer[:80]!r}")
ratio = SequenceMatcher(
    None, answer.removeprefix(prefix), expected["answer"].removeprefix(prefix)
).ratio()
if ratio < expected["min_ratio"]:
    sys.exit(f"the answer matches {ratio:.3f} of the expected text, less than {expected['min_ratio']}")
"""


def fetch_row(index: int) -> dict[str, Any]:
    """Download the MRCR row `index` from the Hugging Face dataset server."""
    request = urllib.request.Request(
        ROWS_URL.format(index=index), headers={"User-Agent": "vis-evals"}
    )
    with urllib.request.urlopen(request, timeout=300) as response:
        rows = json.load(response)["rows"]
    if len(rows) != 1:
        raise ValueError(f"MRCR row {index} does not exist")
    return {"row_idx": index, **rows[0]["row"]}


def load_rows(
    cache: Path | str,
    *,
    needles: int = 2,
    min_chars: int = MIN_CHARS,
    max_chars: int = MAX_CHARS,
    limit: int = 5,
    fetch: Callable[[int], Mapping[str, Any]] = fetch_row,
) -> list[dict[str, Any]]:
    """Return the first `limit` MRCR rows with `needles` needles and a size in range.

    `min_chars` and `max_chars` bound the `n_chars` size of a row. The JSON Lines file
    `cache` keeps each row that `fetch` downloaded, and only the size of a row out of
    range. So a second call with the same or a smaller selection downloads nothing. An
    unknown needle count raises ValueError.
    """
    if needles not in NEEDLES:
        raise ValueError(f"needles must be one of {sorted(NEEDLES)}: {needles!r}")
    path = Path(cache)
    known: dict[int, dict[str, Any]] = {}
    if path.is_file():
        for line in path.read_text(encoding="utf-8").splitlines():
            if line.strip():
                row = json.loads(line)
                known[int(row["row_idx"])] = row
    chosen: list[dict[str, Any]] = []
    first = NEEDLES[needles]
    for index in range(first, first + ROWS_PER_NEEDLES):
        if len(chosen) >= limit:
            break
        row = known.get(index)
        if row is None:
            row = dict(fetch(index))
            row["row_idx"] = index
            if not min_chars <= int(row["n_chars"]) <= max_chars:
                row = {key: row[key] for key in ("row_idx", "n_needles", "n_chars")}
            path.parent.mkdir(parents=True, exist_ok=True)
            with path.open("a", encoding="utf-8") as target:
                target.write(json.dumps(row) + "\n")
            known[index] = row
        if "prompt" in row and min_chars <= int(row["n_chars"]) <= max_chars:
            chosen.append(row)
    return chosen


def mrcr_task_name(row: Mapping[str, Any]) -> str:
    """Return the Harbor task name of `row`, for example `mrcr-2n-17`."""
    return f"mrcr-{int(row['n_needles'])}n-{int(row['row_idx'])}"


def _tokens(text: str) -> int:
    return len(text) // CHARS_PER_TOKEN


def _message_text(message: Mapping[str, Any]) -> str:
    role = "USER" if message.get("role") == "user" else "ASSISTANT"
    return f"{role}: {str(message.get('content', '')).strip()}"


def task_turns(row: Mapping[str, Any], *, turn_tokens: int = TURN_TOKENS) -> list[str]:
    """Return the user messages of the task for `row`.

    The earlier messages of the MRCR conversation come in pasted turns of about
    `turn_tokens` tokens. A message never splits. The last turn gives the last request
    of the conversation.
    """
    messages = json.loads(row["prompt"])
    if not messages or messages[-1].get("role") != "user":
        raise ValueError(f"MRCR row {row.get('row_idx')} does not end with a request")
    turns: list[list[str]] = [[]]
    size = 0
    for message in messages[:-1]:
        text = _message_text(message)
        if turns[-1] and size + _tokens(text) > turn_tokens:
            turns.append([])
            size = 0
        turns[-1].append(text)
        size += _tokens(text)
    pasted = [
        f"{_FIRST if number == 0 else _MORE}\n\n" + "\n\n".join(chunk)
        for number, chunk in enumerate(turns)
        if chunk
    ]
    return [*pasted, _LAST.format(request=str(messages[-1]["content"]).strip())]


def write_mrcr_tasks(
    dataset: Path | str,
    rows: Iterable[Mapping[str, Any]],
    *,
    turn_tokens: int = TURN_TOKENS,
    min_ratio: float = MIN_RATIO,
) -> tuple[HarborTask, ...]:
    """Write one Harbor task for each MRCR row in the folder `dataset`.

    A task passes when the answer starts with the random string of the row and matches
    at least `min_ratio` of the expected answer.
    """
    written: list[HarborTask] = []
    for row in rows:
        turns = task_turns(row, turn_tokens=turn_tokens)
        name = mrcr_task_name(row)
        written.append(
            write_task(
                dataset,
                name,
                turns,
                {"want_answer_program": PROGRAM_FILE},
                agent_timeout_sec=TURN_TIMEOUT_SEC * len(turns),
                metadata={
                    "benchmark": "mrcr",
                    "row": int(row["row_idx"]),
                    "needles": int(row["n_needles"]),
                    "chars": int(row["n_chars"]),
                    "turns": len(turns),
                    "history_tokens": sum(_tokens(turn) for turn in turns),
                },
            )
        )
        tests = Path(dataset) / name / "tests"
        (tests / PROGRAM_FILE).write_text(_PROGRAM, encoding="utf-8")
        expected = {
            "answer": str(row["answer"]),
            "prefix": str(row["random_string_to_prepend"]),
            "min_ratio": min_ratio,
        }
        (tests / EXPECTED_FILE).write_text(
            json.dumps(expected, indent=2) + "\n", encoding="utf-8"
        )
    return tuple(written)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="python -m blockether.vis.evals.mrcr",
        description="Write OpenAI MRCR conversations as long multi-turn Harbor tasks.",
    )
    parser.add_argument("dataset", type=Path, help="folder for the new tasks")
    parser.add_argument(
        "--cache", type=Path, required=True, help="JSON Lines file of downloaded rows"
    )
    parser.add_argument("--needles", type=int, choices=sorted(NEEDLES), default=2)
    parser.add_argument("--min-chars", type=int, default=MIN_CHARS)
    parser.add_argument("--max-chars", type=int, default=MAX_CHARS)
    parser.add_argument("--limit", type=int, default=5, help="rows to write")
    parser.add_argument("--turn-tokens", type=int, default=TURN_TOKENS)
    parser.add_argument("--min-ratio", type=float, default=MIN_RATIO)
    args = parser.parse_args(argv)
    rows = load_rows(
        args.cache,
        needles=args.needles,
        min_chars=args.min_chars,
        max_chars=args.max_chars,
        limit=args.limit,
    )
    written = write_mrcr_tasks(
        args.dataset, rows, turn_tokens=args.turn_tokens, min_ratio=args.min_ratio
    )
    print(f"wrote {len(written)} tasks to {args.dataset}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
