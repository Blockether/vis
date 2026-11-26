"""Check a real child question and cancellation of its detached Task."""

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import TaskFlow, observations

QUESTION = "task-cancel-ready"


def verify(evidence):
    attempts = tasks(evidence.host_evidence, "subagent")
    if len(attempts) != 1:
        return ["expected one Council Task"]
    start, end = attempts[0]
    task_id = start["id"]
    detached = any(
        event["type"] == "task.updated"
        and event["task"]["id"] == task_id
        and event["task"].get("detached") is True
        and event["task"]["state"] == "running"
        for event in evidence.host_evidence.get("events", ())
    )
    if (
        not detached
        or end.get("state") != "cancelled"
        or end.get("cancel_reason") != "agent"
    ):
        return ["the Council Task did not detach and settle after agent cancellation"]
    run = evidence.trajectory
    children = evidence.host_evidence.get("children", {})
    if len(children) != 1:
        return ["expected one child for the Council Task"]
    agent, child_id = next(iter(children.items()))
    if not any(child.session_id == child_id for child in run.subagent_trajectories):
        return ["the Council child is missing from the trajectory"]
    questions = [
        q
        for q in evidence.host_evidence.get("questions", ())
        if q["session_id"] == child_id and q["content"] == QUESTION
    ]
    if not questions:
        return ["no real reply-required child question addressed to the leader"]
    notice = (
        f' runs in the background. {agent} asks: "{QUESTION}". '
        f"Reply, then await the same Task ({task_id})."
    )
    delivered = any(q["state"] == "delivered" for q in questions)
    flow = TaskFlow()
    for block in run.tool_calls:
        if block.error:
            continue
        delivered |= any(
            line.startswith(f"[Vis] {task_id} ") and line.endswith(notice)
            for line in block.stdout.splitlines()
        )
        if block.code:
            flow.feed(block.code)
        if not (flow.record_read and flow.caught_cancel):
            continue
        for row in observations(block.stdout):
            if not (
                row.get("task_id") == task_id
                and row.get("session_id") == child_id
                and row.get("question") == QUESTION
                and row.get("detached") is True
                and row.get("cancelled") is True
                and row.get("error") == "CancelledError"
            ):
                continue
            # A reply can replace the delivered state in SQLite. In that case,
            # match the actual record's entry ID, not just the known question text.
            if delivered or (
                type(row.get("question_entry_id")) is int
                and any(row["question_entry_id"] == q["entry_id"] for q in questions)
            ):
                return []
    return [
        "missing a delivered or read question and the caught cancellation observation"
    ]
