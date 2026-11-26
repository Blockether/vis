"""Check observed test failure after the prefix fold."""

import re

from blockether.vis.evals.task_evidence import tasks

COLOR = re.compile(r"\x1b\[[0-9;]*m")


def shows_seeded_failure(output):
    plain = COLOR.sub("", output)
    return "test_discount_caps_at_fifty" in plain and "FAILED (failures=1)" in plain


def verify(evidence):
    attempts = tasks(evidence.host_evidence, "shell")
    if len(attempts) != 1 or attempts[0][1].get("state") != "done":
        return ["the shell Task did not complete"]
    logs = evidence.host_evidence.get("shells", ())
    if (
        len(logs) != 1
        or logs[0].get("exit") != 1
        or not shows_seeded_failure(logs[0]["output"])
    ):
        return ["indexed process output does not show the seeded unittest failure"]
    run = evidence.trajectory
    launches = run.function_calls(operation="shell", state="succeeded")
    if len(launches) == 1 and any(
        not block.error
        and block.tool_call_id == launches[0].tool_call_id
        and shows_seeded_failure(block.stdout)
        for block in run.tool_calls
    ):
        return []
    return ["missing the observed failing unittest result from the shell launch block"]
