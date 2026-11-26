"""Check guest cancellation of the hold that reached the local MCP server."""

import re

from blockether.vis.evals.task_evidence import tasks
from blockether.vis.evals.task_forms import observations

HOLD = re.compile(r'^\["harbor-probe"\s+"hold"(?:\s|\])')


def verify(evidence):
    attempts = [
        (start, end)
        for start, end in tasks(evidence.host_evidence, "mcp")
        if start.get("name") == f"{start['id']} harbor_probe.hold"
    ]
    if len(attempts) != 1:
        return ["expected exactly one Task for harbor_probe.hold"]
    start, end = attempts[0]
    if end.get("state") != "cancelled" or end.get("cancel_reason") != "agent":
        return ["the local hold did not settle after agent cancellation"]

    run = evidence.trajectory
    cancelled = [
        call
        for call in run.function_calls(operation="call", state="cancelled")
        if call.depth == 0
        and HOLD.match(call.arguments or "")
        and any(
            ref.type == "task" and ref.id == start["id"] for ref in call.row.resources
        )
    ]
    if len(cancelled) != 1:
        return ["expected a cancelled MCP receipt linked to the local hold Task"]
    receipt_seen = False
    for block in run.tool_calls:
        receipt_seen |= block.tool_call_id == cancelled[0].tool_call_id
        if (
            receipt_seen
            and not block.error
            and any(
                row == {"cancelled": True, "holds": 1, "cancels": 1}
                for row in observations(block.stdout)
            )
        ):
            return []
    return ["missing observation of cancellation on the MCP hold Task"]
