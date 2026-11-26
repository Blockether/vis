"""Check recovery of the same ShellTask after folding its launch block."""

import re

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import TaskFlow, observations

COMMAND = "python3 fold_probe.py"
FINAL = "fold-result-7351"
# `shell(op="attach", id=...)` returns the same handle; it is not a launch.
ATTACH = re.compile(r'"op"\W*"attach"')


def verify(evidence):
    attempts = tasks(evidence.host_evidence, "shell")
    if len(attempts) != 1:
        return ["expected one shell Task"]
    start, end = attempts[0]
    identity = re.compile(rf"\b{re.escape(start['id'])}(?:\s|$)")
    if end.get("state") != "done":
        return ["the original shell Task did not finish"]
    rows = [
        row
        for row in evidence.host_evidence.get("shells", ())
        if row["command"] == COMMAND
    ]
    if (
        len(rows) != 1
        or rows[0].get("exit") != 0
        or FINAL not in rows[0]["output"].splitlines()
    ):
        return [
            "the terminal fixture did not accept the release line and exit successfully"
        ]
    launches = [
        call
        for call in evidence.trajectory.function_calls(operation="shell")
        if not call.row.children and not ATTACH.search(call.arguments or "")
    ]
    if len(launches) != 1 or launches[0].state != "succeeded":
        return ["expected one shell launch"]
    launched = False
    flow = TaskFlow()
    for block in evidence.trajectory.tool_calls:
        if block.tool_call_id == launches[0].tool_call_id:
            launched = not block.error and "ready" in block.stdout.splitlines()
            continue
        if block.error or not block.code:
            continue
        flow.feed(block.code)
        if not launched or not flow.recovered_await:
            continue
        lines = block.stdout.splitlines()
        for index, line in enumerate(lines):
            if line.startswith("folded through ") or not identity.search(line):
                continue
            if any(
                type(row.get("exit")) is int
                and row["exit"] == rows[0]["exit"]
                and FINAL in str(row.get("out")).splitlines()
                for row in observations("\n".join(lines[index + 1 :]))
            ):
                return []
    return [
        "missing the original Task name and confirmed exit and output after recovery"
    ]
