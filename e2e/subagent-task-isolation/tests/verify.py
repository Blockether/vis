"""Check first-attempt task completion without using the stale value."""

import re
from datetime import datetime

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import writes_file


def _finished_record(step, block, child, ended_at, live):
    """Accept the child's own completed report, not a separate print of its value."""
    end = block.finished_at_ms
    if end is None and step.timestamp:
        end = round(datetime.fromisoformat(step.timestamp).timestamp() * 1000)
    lines = [line for line in block.stdout.splitlines() if live in line]
    report = (child.final_answer or "").splitlines()
    return type(ended_at) is int and end is not None and ended_at <= end and all(
        line in report for line in lines
    )


def verify(evidence):
    run = evidence.trajectory
    attempts = tasks(evidence.host_evidence, "subagent")
    children = evidence.host_evidence.get("children", {})
    if len(attempts) != 1 or attempts[0][1].get("state") != "done":
        return ["Expected one completed child attempt."]
    if not run or len(children) != 1 or len(run.subagent_trajectories) != 1:
        return ["Expected one saved Council child."]
    if any(writes_file(form, "answer.txt") for form in evidence.forms):
        return ["The leader must not write answer.txt."]
    (child,) = run.subagent_trajectories
    if child.session_id not in children.values():
        return ["The saved run does not belong to the Council child."]
    decoys = [
        value
        for block in run.tool_calls
        for value in re.findall(r"^note=STALE-[ \t]*([0-9a-f]{32})$", block.stdout, re.M)
    ]
    if len(decoys) != 1:
        return ["Expected one stale note in the leader's output."]
    task = str(next((step.message for step in child.steps if step.source == "user"), ""))
    # The task can name the value more than one time.
    live = sorted(set(re.findall(r"\bLIVE-([0-9a-f]{32})\b", task)))
    if len(live) == 1 and live != decoys:
        value = live[0]
        early_print = any(
            value in block.stdout and not _finished_record(
                step, block, child, attempts[0][1].get("ended_at"), value
            )
            for step in run.agent_steps for block in step.tool_calls
        )
        if early_print or value in (run.user_message or "") or value in evidence.answer:
            return ["The leader must not print the live value."]
    output = evidence.workspace / "answer.txt"
    child_wrote = any(
        not block.error and writes_file(block.code, "answer.txt")
        for block in child.tool_calls
    )
    if (
        len(live) != 1
        or "answer.txt" not in task
        or live == decoys
        or not child_wrote
        or not output.is_file()
        or output.read_text().split() != [f"LIVE-{live[0]}"]
        or not re.match(r"\s*written\b", child.final_answer or "", re.I)
        or decoys[0] in child.final_answer.lower()
    ):
        return ["The child did not write the live value from its task and report success."]
    return []
