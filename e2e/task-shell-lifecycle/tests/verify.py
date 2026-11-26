"""Check a bounded wait, cancellation, and a separate process result."""

import ast

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import argument, live_form, observations, resolve


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


def bounded_waits(tree, nodes, task):
    modules = {"asyncio"}
    imports = set()
    for node in nodes:
        if isinstance(node, ast.Import):
            modules.update(
                item.asname or item.name
                for item in node.names
                if item.name == "asyncio"
            )
        if isinstance(node, ast.ImportFrom) and node.module == "asyncio":
            imports.update(
                item.asname or item.name for item in node.names if item.name == "wait"
            )
    for node in nodes:
        if not isinstance(node, ast.Await) or not isinstance(node.value, ast.Call):
            continue
        call = node.value
        func = call.func
        if not (
            isinstance(func, ast.Name)
            and func.id in imports
            or isinstance(func, ast.Attribute)
            and func.attr == "wait"
            and isinstance(func.value, ast.Name)
            and func.value.id in modules
        ):
            continue
        handles = resolve(tree, argument(call, 0, "fs"), node.lineno)
        timeout = argument(call, 1, "timeout")
        if (
            timeout is not None
            and not (isinstance(timeout, ast.Constant) and timeout.value is None)
            and isinstance(handles, (ast.List, ast.Set, ast.Tuple))
            and any(
                isinstance(item, ast.Name) and item.id == task for item in handles.elts
            )
        ):
            yield node


def position(node):
    return node.lineno, node.col_offset


def report_checks(nodes, waits, task):
    """Find reports whose running value comes from the wait's pending set."""
    assignments = sorted(
        (node for node in nodes if isinstance(node, (ast.Assign, ast.AnnAssign))),
        key=position,
        reverse=True,
    )

    def targets(node):
        return node.targets if isinstance(node, ast.Assign) else [node.target]

    def binding(name, before):
        return next(
            (
                node
                for node in assignments
                if position(node) < before
                and any(
                    isinstance(part, ast.Name)
                    and isinstance(part.ctx, ast.Store)
                    and part.id == name
                    for target in targets(node)
                    for part in ast.walk(target)
                )
            ),
            None,
        )

    def source(value, before):
        while isinstance(value, ast.Name):
            node = binding(value.id, before)
            if node is None or not any(
                isinstance(target, ast.Name) and target.id == value.id
                for target in targets(node)
            ):
                break
            value, before = node.value, position(node)
        return value, before

    pending_sets = {
        (node, target.elts[1].id)
        for node in assignments
        if node.value in waits
        for target in targets(node)
        if isinstance(target, (ast.List, ast.Tuple))
        and len(target.elts) == 2
        and isinstance(target.elts[1], ast.Name)
    }
    for call in nodes:
        if not (
            isinstance(call, ast.Call)
            and isinstance(call.func, ast.Name)
            and call.func.id == "print"
            and len(call.args) == 1
        ):
            continue
        encoded, before = source(call.args[0], position(call))
        if call_name(encoded) != "dumps":
            continue
        report, before = source(argument(encoded, 0, "obj"), before)
        if isinstance(report, ast.Dict):
            running = next(
                (
                    value
                    for key, value in reversed(list(zip(report.keys, report.values)))
                    if isinstance(key, ast.Constant) and key.value == "running"
                ),
                None,
            )
        elif call_name(report) == "dict":
            running = argument(report, len(report.args), "running")
        else:
            continue
        test, before = source(running, before)
        if (
            isinstance(test, ast.Compare)
            and len(test.ops) == 1
            and isinstance(test.ops[0], ast.In)
        ):
            handle, pending = test.left, test.comparators[0]
        elif call_name(test) == "__contains__" and isinstance(test.func, ast.Attribute):
            handle, pending = argument(test, 0, "item"), test.func.value
        else:
            continue
        if not (
            isinstance(handle, ast.Name)
            and handle.id == task
            and isinstance(pending, ast.Name)
        ):
            continue
        wait = binding(pending.id, before)
        if (wait, pending.id) in pending_sets and binding(task, before) is binding(
            task, position(wait)
        ):
            yield call.lineno, wait.value.lineno


def reads_output(node, task):
    if isinstance(node, ast.Attribute):
        return (
            isinstance(node.value, ast.Name)
            and node.value.id == task
            and node.attr == "out"
        )
    if isinstance(node, ast.Subscript):
        owner, key = node.value, node.slice
    elif (
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Attribute)
        and node.func.attr == "get"
    ):
        owner, key = node.func.value, argument(node, 0, "key")
    else:
        return False
    return (
        isinstance(owner, ast.Name)
        and owner.id == task
        and isinstance(key, ast.Constant)
        and key.value == "out"
    )


def direct_awaits(nodes, task):
    return sorted(
        node.lineno
        for node in nodes
        if isinstance(node, ast.Await)
        and isinstance(node.value, ast.Name)
        and node.value.id == task
    )


def catches(nodes, exception):
    return any(
        isinstance(node, ast.ExceptHandler)
        and (
            getattr(node.type, "id", None) == exception
            or getattr(node.type, "attr", None) == exception
        )
        for node in nodes
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
    forms = []
    spans = {}
    line = 1
    for block in run.tool_calls:
        if block.code and not block.error:
            forms.append(block.code)
            end = line + block.code.count("\n")
            spans[block.tool_call_id] = (line, end)
            line = end + 2
    tree = live_form("\n\n".join(forms))
    task = holds[0]
    deferred = {
        inner
        for node in ast.walk(tree)
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda))
        for inner in ast.walk(node)
    }
    nodes = [node for node in ast.walk(tree) if node not in deferred]
    waits = set(bounded_waits(tree, nodes, task))
    outputs = sorted(node.lineno for node in nodes if reads_output(node, task))
    cancels, terminal = (
        sorted(node.lineno for node in method(tree, task, name) if node not in deferred)
        for name in ("cancel", "cancelled")
    )
    awaits = direct_awaits(nodes, task)
    # A model can reuse the same variable for the second process.
    if not (
        waits
        and cancels
        and awaits
        and terminal
        and any(wait.lineno < read < cancels[0] for wait in waits for read in outputs)
        and cancels[0] < awaits[0] < terminal[-1]
    ):
        return [
            "did not use asyncio.wait, read live output, cancel and await the hold Task"
        ]
    if not catches(nodes, "CancelledError") or not any(
        line > terminal[0] for line in direct_awaits(nodes, finishes[0])
    ):
        return ["did not catch cancellation and await the finish Task"]
    reports = {
        printed
        for printed, wait in report_checks(nodes, waits, task)
        if any(wait < read < cancels[0] for read in outputs)
    }
    if not reports:
        return ["printed running value did not come from the wait's pending set"]
    for block in run.tool_calls:
        start, end = spans.get(block.tool_call_id, (0, 0))
        if not any(start <= printed <= end for printed in reports):
            continue
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
