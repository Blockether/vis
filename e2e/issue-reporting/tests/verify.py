"""Issue #239: each tracker call in the mock record matches one succeeded tool call.

The mock record and the Activity of the trajectory are separate evidence. A row
without a tool call, or a tool call without a row, fails the task.
"""

import json

RECORD = (".vis", "calls", "issue-events.jsonl")
OPERATIONS = {"find": "vis.issue_find", "create": "vis.issue_create"}


def verify(evidence):
    record = evidence.workspace.joinpath(*RECORD)
    lines = record.read_text().splitlines() if record.is_file() else []
    rows = [json.loads(line) for line in lines if line.strip()]
    failures = []
    for op, tool in OPERATIONS.items():
        recorded = sum(row.get("op") == op for row in rows)
        called = evidence.tools.count(tool)
        if recorded != called:
            failures.append(
                f"the mock recorded {recorded} {op} calls, but {called} {tool} calls succeeded"
            )
    return failures
