"""Check the question, the leader's reply, the child's write and its answer in order."""

import re

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import writes_file


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
    asks = [
        block
        for block in child.tool_calls
        for call in block.function_calls
        if call.operation == "council.ask" and call.state == "succeeded"
    ]
    if len(asks) != 1:
        return ["Expected one successful council.ask call."]
    reply_ms = replies[0]["created_at_ms"]
    ask_ms = asks[0].started_at_ms
    if ask_ms is None or reply_ms < ask_ms:
        return ["The leader must reply after the child asks."]
    writes = [
        block
        for block in child.tool_calls
        if not block.error and writes_file(block.code, "code.txt")
    ]
    # The asking block can also write: it ends after the reply. A later block starts after it.
    ask_at = child.tool_calls.index(asks[0])

    def after_reply(block):
        if child.tool_calls.index(block) == ask_at:
            return block.finished_at_ms is not None and block.finished_at_ms >= reply_ms
        return (
            child.tool_calls.index(block) > ask_at
            and block.started_at_ms is not None
            and block.started_at_ms >= reply_ms
        )

    if not writes or not all(after_reply(block) for block in writes):
        return ["The child must write code.txt after the reply."]
    if not child.final_answer or child.final_answer.strip().lower().rstrip(".!") != "done":
        return ["The child must answer done."]
    return []
