"""Write Lost in Conversation tasks as multi-turn Harbor tasks.

[Lost in Conversation](https://github.com/microsoft/lost_in_conversation) (LiC) splits
each problem into shards: small parts of one request. A `sharded` task gives one shard
in each user turn. A `concat` task gives all shards in one turn, which is the control
of the benchmark. Both modes use the same grader, so their difference is the cost of
the conversation. Sharded tasks need the multi-turn support of
`blockether.vis.evals.agent:VisAgent`.

The module supports the `math` (GSM8K) tasks and the HumanEval `code` tasks. Their
graders follow the LiC evaluators. The protocol differs from LiC in two ways:

- The shards come in their LiC order, not from a simulated user.
- The last turn says that all details are given. Only the final answer counts.

Importing this module makes no network calls. `load_samples` downloads the data once.
Run `python -m blockether.vis.evals.lic DATASET --cache FILE` to write the tasks.
"""

from __future__ import annotations

import argparse
import ast
import json
import re
import urllib.request
from collections.abc import Iterable, Mapping
from pathlib import Path
from typing import Any

from .harbor import HarborTask, write_task

__all__ = [
    "DATA_URL",
    "MODES",
    "TASKS",
    "lic_task_name",
    "load_samples",
    "write_lic_tasks",
]

DATA_URL = (
    "https://huggingface.co/datasets/microsoft/lost_in_conversation"
    "/resolve/main/lost_in_conversation.json"
)
TASKS = ("math", "code")
MODES = ("sharded", "concat")
PROGRAM_FILE = "check_answer.py"
EXPECTED_FILE = "expected.json"
TURN_TIMEOUT_SEC = 180.0

_FORMAT = {
    "math": "Every reply that gives an answer must end with one line "
    "`FINAL ANSWER: <number>`.",
    "code": "Every reply that gives a solution must contain the complete Python "
    "function in one ```python block.",
}
_RULES = "Answer in chat only; do not create or edit files."
_LAST = "That is all the information. Give your final answer now."

_MATH_PROGRAM = r"""# Grade a GSM8K answer like the LiC math evaluator. The answer is on stdin.
import json
import re
import sys
from pathlib import Path

expected = json.loads(Path(__file__).with_name("expected.json").read_text())
text = sys.stdin.read()
finals = re.findall(r"FINAL ANSWER:\s*(.+)", text)
answer = finals[-1] if finals else text
gold = expected["answer"].split("####")[1].strip().lower()
numbers = re.findall(r"(-?[$0-9.,]{2,})|(-?[0-9]+)", answer.strip())
if not numbers:
    sys.exit("the answer has no number")
value = [part for part in numbers[-1] if part][0]
if "$" in value and value.endswith(".00"):
    value = value.removesuffix(".00")
for pattern in [",", r"\$", r"(?s).*#### ", r"\.$"]:
    value = re.sub(pattern, "", value)
    gold = re.sub(pattern, "", gold)
if value != gold:
    sys.exit(f"the answer {value} is not {gold}")
"""

_CODE_PROGRAM = r"""# Grade a HumanEval answer with its hidden tests. The answer is on stdin.
import ast
import json
import re
import subprocess
import sys
from pathlib import Path

expected = json.loads(Path(__file__).with_name("expected.json").read_text())
text = sys.stdin.read()
blocks = re.findall(r"```python\n(.*?)```", text, re.S)
if not blocks:
    sys.exit("the answer has no python code block")


def functions(code):
    try:
        tree = ast.parse(code)
    except SyntaxError:
        return []
    kinds = (ast.FunctionDef, ast.AsyncFunctionDef)
    return [node.name for node in tree.body if isinstance(node, kinds)]


# The solution is the last block with a function: a usage example can follow it.
found = [(code, functions(code)) for code in blocks if functions(code)]
if not found:
    sys.exit("no python code block defines a function")
code, names = found[-1]
entry = expected["entry_point"]
if entry not in names:
    code += f"\n\n{entry} = {names[0]}\n"
program = "\n\n".join([expected["imports"], code, expected["test"], f"check({entry})\n"])
try:
    completed = subprocess.run(
        [sys.executable, "-c", program], capture_output=True, text=True, timeout=30
    )
except subprocess.TimeoutExpired:
    sys.exit("the hidden tests ran longer than 30 s")
if completed.returncode != 0:
    lines = completed.stderr.strip().splitlines()
    sys.exit("the hidden tests failed: " + (lines[-1] if lines else "no output"))
"""


def load_samples(cache: Path | str, *, url: str = DATA_URL) -> list[dict[str, Any]]:
    """Return the LiC samples from the JSON file `cache`.

    A missing file is downloaded from `url` first. The data is about 30 MB.
    """
    path = Path(cache)
    if not path.is_file():
        path.parent.mkdir(parents=True, exist_ok=True)
        request = urllib.request.Request(url, headers={"User-Agent": "vis-evals"})
        with urllib.request.urlopen(request, timeout=120) as response:
            path.write_bytes(response.read())
    samples = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(samples, list):
        raise ValueError(f"{path} is not a list of LiC samples")
    return samples


def lic_task_name(sample: Mapping[str, Any]) -> str:
    """Return the Harbor task name of `sample`, for example `lic-gsm8k-1246`."""
    task_id = str(sample["task_id"]).removeprefix("sharded-").lower()
    return "lic-" + re.sub(r"[^a-z0-9]+", "-", task_id).strip("-")


def _supported(sample: Mapping[str, Any]) -> bool:
    if sample.get("task") == "math":
        return True
    return sample.get("task") == "code" and sample.get("source") == "humaneval"


def _shards(sample: Mapping[str, Any]) -> list[str]:
    shards = sample["shards"]
    if isinstance(shards, str):
        shards = ast.literal_eval(shards)
    return [
        str(s["shard"]).strip() for s in sorted(shards, key=lambda s: s["shard_id"])
    ]


def _instruction(sample: Mapping[str, Any], mode: str) -> str | list[str]:
    shards = _shards(sample)
    rules = f"{_RULES} {_FORMAT[sample['task']]}"
    if mode == "concat":
        details = "\n".join(f"- {shard}" for shard in shards)
        return f"I will describe a problem. {rules}\n\n{details}\n\n{_LAST}\n"
    first = f"I will describe a problem over several messages. {rules}\n\n{shards[0]}"
    turns = [first, *shards[1:]]
    turns[-1] = f"{turns[-1]}\n\n{_LAST}"
    return turns


def _expected(sample: Mapping[str, Any]) -> tuple[str, dict[str, str]]:
    if sample["task"] == "math":
        return _MATH_PROGRAM, {"answer": str(sample["answer"])}
    metadata = sample.get("metadata")
    if isinstance(metadata, str):
        metadata = ast.literal_eval(metadata)
    if not isinstance(metadata, Mapping) or "func_name" not in metadata:
        raise ValueError(f"{sample['task_id']} has no func_name in its metadata")
    imports = [
        ast.unparse(node)
        for node in ast.parse(sample["prompt"]).body
        if isinstance(node, (ast.Import, ast.ImportFrom))
    ]
    return _CODE_PROGRAM, {
        "entry_point": str(metadata["func_name"]),
        "imports": "\n".join(imports),
        "test": str(sample["test"]),
    }


def write_lic_tasks(
    dataset: Path | str,
    samples: Iterable[Mapping[str, Any]],
    *,
    mode: str = "sharded",
    tasks: Iterable[str] = TASKS,
    limit: int | None = None,
) -> tuple[HarborTask, ...]:
    """Write one Harbor task for each supported sample in the folder `dataset`.

    `mode` is `sharded` or `concat`. `tasks` selects the LiC tasks, and `limit` keeps
    the first samples of each task in task id order. Both modes give the same task
    names, so `compare_cases` can pair a sharded run with a concat run. An
    unknown mode or task raises ValueError.
    """
    if mode not in MODES:
        raise ValueError(f"mode must be one of {MODES}: {mode!r}")
    chosen = tuple(tasks)
    if unknown := sorted(set(chosen) - set(TASKS)):
        raise ValueError(f"unknown LiC tasks {unknown}; use {TASKS}")
    rows_in = list(samples)
    written: list[HarborTask] = []
    for task in chosen:
        rows = sorted(
            (s for s in rows_in if s.get("task") == task and _supported(s)),
            key=lambda s: str(s["task_id"]),
        )
        for sample in rows[:limit]:
            program, expected = _expected(sample)
            turns = len(_shards(sample)) if mode == "sharded" else 1
            written.append(
                write_task(
                    dataset,
                    lic_task_name(sample),
                    _instruction(sample, mode),
                    {"want_answer_program": PROGRAM_FILE},
                    agent_timeout_sec=TURN_TIMEOUT_SEC * turns,
                    metadata={
                        "benchmark": "lost-in-conversation",
                        "lic_task": task,
                        "lic_task_id": str(sample["task_id"]),
                        "mode": mode,
                        "turns": turns,
                    },
                )
            )
            tests = Path(dataset) / lic_task_name(sample) / "tests"
            (tests / PROGRAM_FILE).write_text(program, encoding="utf-8")
            (tests / EXPECTED_FILE).write_text(
                json.dumps(expected, indent=2) + "\n", encoding="utf-8"
            )
    return tuple(written)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="python -m blockether.vis.evals.lic",
        description="Write Lost in Conversation tasks as Harbor tasks.",
    )
    parser.add_argument("dataset", type=Path, help="The new folder for the tasks.")
    parser.add_argument("--cache", type=Path, required=True, help="The LiC JSON file.")
    parser.add_argument("--mode", choices=MODES, default="sharded")
    parser.add_argument("--task", action="append", choices=TASKS, dest="tasks")
    parser.add_argument("--limit", type=int, default=None)
    args = parser.parse_args(argv)
    written = write_lic_tasks(
        args.dataset,
        load_samples(args.cache),
        mode=args.mode,
        tasks=args.tasks or TASKS,
        limit=args.limit,
    )
    print(f"wrote {len(written)} tasks to {args.dataset}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
