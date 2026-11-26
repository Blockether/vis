"""Check a bounded wait, cancellation, and a separate process result."""

import ast
import json

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import live_form


def observations(stdout):
    for line in stdout.splitlines():
        try:
            value = json.loads(line)
        except ValueError:
            continue
        if isinstance(value, dict):
            yield value


def call_name(node):
    if not isinstance(node, ast.Call):
        return None
    if isinstance(node.func, ast.Attribute):
        return node.func.attr
    if isinstance(node.func, ast.Name):
        return node.func.id
    return None


def assigned(tree, operation, *arguments):
    for node in ast.walk(tree):
        if not isinstance(node, ast.Assign):
            continue
        value = node.value.value if isinstance(node.value, ast.Await) else node.value
        if call_name(value) != operation:
            continue
        literals = {
            item.value
            for item in (*value.args, *(arg.value for arg in value.keywords))
            if isinstance(item, ast.Constant) and isinstance(item.value, str)
        }
        if set(arguments) <= literals:
            yield from (
                target.id for target in node.targets if isinstance(target, ast.Name)
            )


def method(tree, task, name):
    return [
        node
        for node in ast.walk(tree)
        if call_name(node) == name
        and isinstance(node.func, ast.Attribute)
        and isinstance(node.func.value, ast.Name)
        and node.func.value.id == task
    ]


def direct_awaits(tree, task):
    return sorted(
        node.lineno
        for node in ast.walk(tree)
        if isinstance(node, ast.Await)
        and isinstance(node.value, ast.Name)
        and node.value.id == task
    )


def catches(tree, exception):
    return any(
        isinstance(node, ast.ExceptHandler)
        and (
            getattr(node.type, "id", None) == exception
            or getattr(node.type, "attr", None) == exception
        )
        for node in ast.walk(tree)
    )


def verify(evidence):
    attempts = tasks(evidence.host_evidence, "shell")
    if len(attempts) != 2:
        return ["expected exactly two shell Tasks"]
    hold_end, finish_end = (end for _, end in attempts)
    if (
        hold_end.get("state") != "cancelled"
        or hold_end.get("cancel_reason") != "agent"
        or finish_end.get("state") != "done"
    ):
        return ["the hold and finish Tasks did not settle as expected"]
    rows = {row["command"]: row for row in evidence.host_evidence.get("shells", ())}
    hold = rows.get("python3 shell_probe.py hold", {})
    finish = rows.get("python3 shell_probe.py finish", {})
    if (
        "hold-started" not in hold.get("output", "")
        or "hold-finished" in hold.get("output", "")
        or finish.get("exit") != 0
        or "finish-ok" not in finish.get("output", "")
    ):
        return [
            "indexed shell logs do not show the hold and successful finish commands"
        ]
    run = evidence.trajectory
    launches = [
        call for call in run.function_calls(operation="shell") if not call.row.children
    ]
    if len(launches) != 2 or any(call.state != "succeeded" for call in launches):
        return ["expected exactly two successful shell launches"]
    blocks = {block.tool_call_id: block for block in run.tool_calls}
    first, second = (live_form(blocks[call.tool_call_id].code) for call in launches)
    holds = list(assigned(first, "shell", "python3 shell_probe.py hold"))
    finishes = list(assigned(second, "shell", "python3 shell_probe.py finish"))
    if len(holds) != 1 or len(finishes) != 1:
        return ["expected the hold and finish commands in that order"]
    # Each form has its own line numbers. Give the joined source one order.
    tree = live_form("\n\n".join(block.code for block in run.tool_calls if block.code))
    task = holds[0]
    methods = {
        name: sorted(node.lineno for node in method(tree, task, name))
        for name in (
            "wait",
            "done",
            "cancel",
            "cancelled",
        )
    }
    waits, pending, cancels, terminal = (
        methods[name] for name in ("wait", "done", "cancel", "cancelled")
    )
    awaits = direct_awaits(tree, task)
    # A model can reuse the same variable for the second process.
    if (
        not all((waits, pending, cancels, terminal, awaits))
        or not waits[0] < pending[0] < cancels[0] < awaits[0] < terminal[-1]
    ):
        return ["did not wait, observe pending, cancel and await the hold Task"]
    if not catches(tree, "CancelledError") or not any(
        line > terminal[0] for line in direct_awaits(tree, finishes[0])
    ):
        return ["did not catch cancellation and await the finish Task"]
    for block in run.tool_calls:
        for row in observations(block.stdout):
            if (
                row.get("running") is True
                and row.get("cancelled") is True
                and "hold-started" in str(row.get("start_output"))
                and "hold-finished" not in str(row.get("start_output"))
                and type(row.get("exit")) is int
                and row["exit"] == 0
                and "finish-ok" in str(row.get("output"))
            ):
                return []
    return ["missing the observed ShellTask states or terminal process output"]
