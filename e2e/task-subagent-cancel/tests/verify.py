"""Check a real child question and cancellation of its running Task."""

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import TaskFlow, observations

QUESTION = "task-cancel-ready"


def verify(evidence):
    attempts = tasks(evidence.host_evidence, "subagent")
    if len(attempts) != 1:
        return ["expected one Council Task"]
    start, end = attempts[0]
    task_id = start["id"]
    if start.get("detached") or end.get("state") != "cancelled" or end.get("cancel_reason") != "agent":
        return ["the Council Task did not settle after agent cancellation"]
    watches = [pair for pair in tasks(evidence.host_evidence, "tool")
               if "council.changed" in pair[0]["name"] and pair[1].get("state") == "done"]
    if not watches:
        return ["no completed Council readiness Task"]
    run = evidence.trajectory
    children = evidence.host_evidence.get("children", {})
    if len(children) != 1:
        return ["expected one child for the Council Task"]
    child_id = next(iter(children.values()))
    if not any(child.session_id == child_id for child in run.subagent_trajectories):
        return ["the Council child is missing from the trajectory"]
    questions = [
        q
        for q in evidence.host_evidence.get("questions", ())
        if q["session_id"] == child_id and q["content"] == QUESTION
    ]
    if not questions:
        return ["no real reply-required child question addressed to the leader"]
    flow = TaskFlow()
    for block in run.tool_calls:
        if block.error:
            continue
        if block.code:
            flow.feed(block.code)
        if not (flow.record_read and flow.caught_cancel):
            continue
        for row in observations(block.stdout):
            if not (
                row.get("task_id") == task_id
                and row.get("session_id") == child_id
                and row.get("question") == QUESTION
                and row.get("ready") is True
                and row.get("cancelled") is True
                and row.get("error") == "CancelledError"
            ):
                continue
            if type(row.get("question_entry_id")) is int and any(
                row["question_entry_id"] == q["entry_id"] for q in questions
            ):
                return []
    return ["missing matching request evidence and the caught cancellation observation"]
