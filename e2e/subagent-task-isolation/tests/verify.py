"""Check first-attempt task completion without using the stale value."""

import re

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import writes_file


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
    shown = [block.stdout for block in run.tool_calls] + [evidence.answer]
    if len(live) == 1 and live != decoys and any(live[0] in text for text in shown):
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
        or not child.final_answer
        or child.final_answer.strip().lower().rstrip(".!") != "written"
    ):
        return ["The child did not write the live value from its task and report success."]
    return []
