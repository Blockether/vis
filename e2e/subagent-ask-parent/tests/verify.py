"""Check the question, the leader's reply, the child's write and its answer in order."""

import ast
import re
from datetime import datetime

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import live_form, writes_file


def _epoch_ms(timestamp):
    try:
        return round(datetime.fromisoformat(timestamp).timestamp() * 1000)
    except (TypeError, ValueError):
        return None


def _is_ask(call):
    return isinstance(call, ast.Call) and ast.unparse(call.func) == "council.ask"


def _asked(block):
    if any(call.operation == "council.ask" and call.state == "succeeded"
           for call in block.function_calls):
        return True
    try:
        return any(_is_ask(node) for node in ast.walk(live_form(block.code)))
    except SyntaxError:
        return False


def _writes_after_ask(code):
    """Recognize a write after an awaited question in the same block."""
    try:
        body = ast.parse(code).body
    except SyntaxError:
        return False
    for i, stmt in enumerate(body):
        if writes_file(ast.unparse(stmt), "code.txt"):
            return False
        awaited = getattr(stmt, "value", None)
        if isinstance(awaited, ast.Await) and _is_ask(awaited.value):
            return writes_file(ast.unparse(ast.Module(body=body[i + 1:], type_ignores=[])), "code.txt")
    return False


def verify(evidence):
    run = evidence.trajectory
    host = evidence.host_evidence
    attempts = tasks(host, "subagent")
    children = host.get("children", {})
    if len(attempts) != 1 or attempts[0][1].get("state") != "done":
        return ["Expected one completed child attempt."]
    if not run or len(children) != 1 or len(run.subagent_trajectories) != 1:
        return ["Expected one saved Council child."]
    if any(writes_file(form, "code.txt") for form in evidence.forms):
        return ["The leader must not write code.txt."]
    (child,) = run.subagent_trajectories
    if child.session_id not in children.values():
        return ["The saved run does not belong to the Council child."]
    codes = [
        value
        for block in run.tool_calls
        for value in re.findall(r"^release-code=[ \t]*([0-9a-f]{32})$", block.stdout, re.M)
    ]
    output = evidence.workspace / "code.txt"
    if len(codes) != 1 or not output.is_file() or output.read_text().split() != codes:
        return ["Expected code.txt to contain only the code printed by the leader."]
    task = str(next((step.message for step in child.steps if step.source == "user"), ""))
    if codes[0] in re.sub(r"[^0-9a-f]", "", task.lower()):
        return ["The leader must not give the code before the child asks."]
    questions = [
        row for row in host.get("questions", ()) if row["session_id"] == child.session_id
    ]
    if len(questions) != 1 or questions[0]["state"] != "replied":
        return ["Expected one child question with a reply."]
    question = questions[0]
    if not question.get("content", "").strip():
        return ["The child must ask for the code."]
    replies = [
        row for row in host.get("replies", ()) if row["reply_to"] == question["entry_id"]
    ]
    if len(replies) != 1 or replies[0]["content"].strip() != codes[0]:
        return ["The leader must reply to the child's question with the code."]
    reply_ms = replies[0].get("created_at_ms")
    blocks = [(step, block) for step in child.agent_steps for block in step.tool_calls]
    asks = [i for i, (_, block) in enumerate(blocks) if _asked(block)]
    if not asks:
        return ["Expected one council.ask in the child run."]
    if type(reply_ms) is not int:
        return ["Expected the time of the leader's reply."]
    if any(blocks[i][1].started_at_ms is not None and reply_ms < blocks[i][1].started_at_ms
           for i in asks):
        return ["The leader must reply after the child asks."]

    def after_reply(i, step, block):
        if i in asks:
            return (block.finished_at_ms is not None and block.finished_at_ms >= reply_ms
                    and _writes_after_ask(block.code))
        if block.started_at_ms is not None:
            return block.started_at_ms >= reply_ms and not any(j > i for j in asks)
        # An older saved run has no block times. Require a later step and a timestamp.
        stamp = _epoch_ms(step.timestamp)
        return (len(asks) == 1 and blocks[asks[0]][0].step_id < step.step_id
                and stamp is not None and stamp >= reply_ms)

    writes = [(i, step, block) for i, (step, block) in enumerate(blocks)
              if not block.error and writes_file(block.code, "code.txt")]
    if not writes or not all(after_reply(*write) for write in writes):
        return ["The child must write code.txt after the reply."]
    if not re.match(r"\s*done\b", child.final_answer or "", re.I):
        return ["The child must answer done."]
    return []
