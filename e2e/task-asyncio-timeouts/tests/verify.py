"""Check cancellation on timeout and completion of the shielded Task."""

import ast
import json

from blockether.vis.evals.task_forms import argument, live_form, prints_marker, resolve


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


def handles_timeout(tree, wait, marker):
    for node in ast.walk(tree):
        if not isinstance(node, (ast.Try, ast.TryStar)) or not any(
            isinstance(n, ast.Await) and n.value is wait
            for statement in node.body
            for n in ast.walk(statement)
        ):
            continue
        for handler in node.handlers:
            types = (
                handler.type.elts
                if isinstance(handler.type, ast.Tuple)
                else [handler.type]
            )
            if any(
                getattr(kind, "id", None) == "TimeoutError"
                or getattr(kind, "attr", None) == "TimeoutError"
                for kind in types
            ) and any(prints_marker(n, marker) for n in ast.walk(handler)):
                return True
    return False


def waits_for(tree, task, *, shielded):
    for node in ast.walk(tree):
        if call_name(node) != "wait_for":
            continue
        value = argument(node, 0, "fut")
        if shielded:
            value = resolve(tree, value, node.lineno)
            if call_name(value) != "shield":
                continue
            value = argument(value, 0, "arg")
        if isinstance(value, ast.Name) and value.id == task:
            yield node


def verify(evidence):
    for block in evidence.trajectory.tool_calls:
        if block.error or not block.code:
            continue
        lines = block.stdout.splitlines()
        if not {"plain-timeout", "shield-timeout"} <= set(lines) or lines.index(
            "plain-timeout"
        ) >= lines.index("shield-timeout"):
            continue
        if lines[lines.index("shield-timeout") + 1 :].count("released") != 2:
            continue
        tree = live_form(block.code)
        for plain in assigned(tree, "hold", "plain"):
            if (
                not any(
                    handles_timeout(tree, wait, "plain-timeout")
                    for wait in waits_for(tree, plain, shielded=False)
                )
                or method(tree, plain, "cancel")
                or not method(tree, plain, "cancelled")
            ):
                continue
            for shielded in assigned(tree, "hold", "shielded"):
                waits = [
                    wait.lineno
                    for wait in waits_for(tree, shielded, shielded=True)
                    if handles_timeout(tree, wait, "shield-timeout")
                ]
                release = [
                    n.lineno for n in ast.walk(tree) if call_name(n) == "release"
                ]
                awaits = direct_awaits(tree, shielded)
                if (
                    not waits
                    or not release
                    or len(awaits) < 2
                    or not min(waits) < min(release) < awaits[0]
                ):
                    continue
                if not method(tree, shielded, "done"):
                    continue
                if any(
                    row
                    == {
                        "plain_cancelled": True,
                        "shielded_running": True,
                        "results": ["released", "released"],
                    }
                    for row in observations(block.stdout)
                ):
                    return []
    return [
        "missing observed wait_for timeout, shielded Task or two awaits of the original Task"
    ]
