"""Check eager start and a non-cancelling timed wait."""

import ast
import json

from blockether.vis.evals.task_forms import live_form, prints_marker


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


def wait_aliases(tree):
    """Names that `from asyncio import wait [as name]` binds."""
    return {
        alias.asname or alias.name
        for node in ast.walk(tree)
        if isinstance(node, ast.ImportFrom) and node.module == "asyncio"
        for alias in node.names
        if alias.name == "wait"
    }


def wait_call(node, aliases):
    return call_name(node) == "wait" or (
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id in aliases
    )


def deferred_nodes(tree):
    """Nodes in a function or lambda body: the block may never run them."""
    return {
        id(inner)
        for node in ast.walk(tree)
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda))
        for inner in ast.walk(node)
    }


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
    if evidence.trajectory is None:
        return ["no trajectory was recorded for the eager Task trial"]
    for block in evidence.trajectory.tool_calls:
        if block.error or not block.code:
            continue
        lines = block.stdout.splitlines()
        if not {"eager-started", "eager-pending"} <= set(lines) or lines.index(
            "eager-started"
        ) >= lines.index("eager-pending"):
            continue
        tree = live_form(block.code)
        aliases, deferred = wait_aliases(tree), deferred_nodes(tree)
        for task in assigned(tree, "hold", "eager"):
            start = next(node for node in ast.walk(tree) if call_name(node) == "hold")
            started = [n.lineno for n in ast.walk(tree) if call_name(n) == "started"]
            waits = [
                n.lineno
                for n in ast.walk(tree)
                if wait_call(n, aliases) and id(n) not in deferred
                and any(
                    isinstance(arg, ast.Name) and arg.id == task for arg in ast.walk(n)
                )
                and any(arg.arg == "timeout" for arg in n.keywords)
            ]
            if not started or not waits:
                continue
            markers = {
                marker: [n.lineno for n in ast.walk(tree) if prints_marker(n, marker)]
                for marker in ("eager-started", "eager-pending")
            }
            if any(
                isinstance(n, ast.Await) and n.value is start for n in ast.walk(tree)
            ):
                continue
            for cancel in method(tree, task, "cancel"):
                awaits = direct_awaits(tree, task)
                if not awaits or not any(
                    min(started) <= began < wait <= pending < cancel.lineno < awaits[0]
                    for began in markers["eager-started"]
                    for pending in markers["eager-pending"]
                    for wait in waits
                ):
                    continue
                if (
                    not catches(tree, "CancelledError")
                    or not method(tree, task, "cancelled")
                    or not method(tree, task, "done")
                ):
                    continue
                if any(
                    isinstance(row.get("name"), str)
                    and row["name"]
                    and all(
                        row.get(key) is True
                        for key in ("started", "pending", "done", "cancelled")
                    )
                    for row in observations(block.stdout)
                ):
                    return []
    return [
        "missing observed eager start, pending wait or cancellation of the hold Task"
    ]
