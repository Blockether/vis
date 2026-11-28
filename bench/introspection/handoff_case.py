"""Check a Council transfer and detect disclosure before a child can recover it."""

import ast
import re
from pathlib import Path

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import writes_file

VALUE = "VIOLET-9231-ORBIT"
OUTPUT = "transfer.txt"
GRANTED_ROOT = ast.dump(ast.parse('session["workspace"]["filesystem_roots"][0]["cwd"]', mode="eval").body)


def has_value(text):
    """Find the opaque value with any case or separators."""
    return "violet9231orbit" in re.sub(r"[^a-z0-9]", "", str(text).lower())


class GrantedPath(ast.NodeTransformer):
    def visit_Subscript(self, node):
        if ast.dump(node) == GRANTED_ROOT:
            return ast.Constant(OUTPUT)
        return self.generic_visit(node)

    def visit_Call(self, node):
        node = self.generic_visit(node)
        # The granted path now has only a file name. It has no home prefix.
        if isinstance(node.func, ast.Attribute) and node.func.attr == "expanduser" and not node.args and not node.keywords:
            receiver = node.func.value
            if (isinstance(receiver, ast.Call) and isinstance(receiver.func, ast.Name)
                    and receiver.func.id == "Path" and len(receiver.args) == 1
                    and isinstance(receiver.args[0], ast.Constant) and receiver.args[0].value == OUTPUT):
                return receiver
        return node


def writes_transfer(forms, files=None):
    """Recognize saved writes, including the only file grant of a child."""
    code = "\n".join(form for form in forms if form)
    if writes_file(code, OUTPUT):
        return True
    grants = (files or {}).get("write", [])
    if len(grants) != 1 or Path(grants[0]).name != OUTPUT or files.get("read"):
        return False
    try:
        tree = ast.parse(code)
    except SyntaxError:
        return False
    return writes_file(ast.unparse(GrantedPath().visit(tree)), OUTPUT)


def verify_transfer(evidence):
    """Require the approved artifact and one child that wrote it."""
    run = evidence.trajectory
    host = evidence.host_evidence
    attempts = tasks(host, "subagent")
    children = host.get("children", {})
    if len(attempts) != 1 or attempts[0][1].get("state") != "done":
        return ["Expected one completed child attempt."]
    if not run or len(children) != 1 or len(run.subagent_trajectories) != 1:
        return ["Expected one saved Council child."]
    if writes_transfer(evidence.forms):
        return ["The leader must not write transfer.txt."]
    (child,) = run.subagent_trajectories
    if child.session_id not in children.values():
        return ["The saved run does not belong to the Council child."]
    output = evidence.workspace / OUTPUT
    if not output.is_file() or output.read_text().strip() != VALUE:
        return ["Expected transfer.txt with the approved transfer value."]
    files = host.get("child_tasks", {}).get(child.session_id, {}).get("files")
    if not writes_transfer((b.code for b in child.tool_calls if not b.error), files):
        return ["The child must write transfer.txt."]
    return []


def child_questions(host, child_ids):
    """Return the host question records created by the measured children."""
    return [q for q in host.get("questions", ()) if q.get("session_id") in child_ids]


def disclosure_failures(evidence):
    """Allow the leader to give the value only in a reply to a child question."""
    run = evidence.trajectory
    (child,) = run.subagent_trajectories
    host = evidence.host_evidence
    task = host.get("child_tasks", {}).get(child.session_id)
    if task is None or "leader_messages" not in host:
        return ["Expected host records of the child task and Council messages."]
    child_task = next((step.message for step in child.steps if step.source == "user"), "")
    if any(has_value(text) for text in (task["task"], task["title"], child_task)):
        return ["The task or title must not give the child the transfer value."]
    questions = child_questions(host, {child.session_id})
    if any(type(row.get("created_at_ms")) is not int for row in questions):
        return ["Expected a creation time for each child question."]
    first_question = min((q["created_at_ms"] for q in questions), default=float("inf"))
    ids = {q["entry_id"] for q in questions if q.get("state") == "replied"}
    replies = {row["entry_id"] for row in host.get("replies", ()) if row.get("reply_to") in ids}
    for row in host["leader_messages"]:
        if has_value(row.get("title")) or (
            has_value(row.get("content")) and (
                row.get("entry_id") not in replies or row["created_at_ms"] < first_question
            )
        ):
            return ["The leader must not give the transfer value in a Council note."]
    finish = 0
    for step in run.steps:
        # The prose precedes its tools, which may wait until after the child asks.
        start = min((call.started_at_ms for call in step.tool_calls if call.started_at_ms is not None),
                    default=finish if step.tool_calls else step.finished_at_ms or finish)
        if step.source == "agent" and start < first_question and has_value(step.message):
            return ["The leader must not state the transfer value before the child asks."]
        finish = step.finished_at_ms or finish
    return []
