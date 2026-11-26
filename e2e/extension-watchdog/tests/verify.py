"""Check the short control and the poll that exceeds the host watchdog."""

import json
import math

from blockether.vis.evals.task_forms import observations


def verify(evidence):
    trace = evidence.workspace / ".vis/calls/watchdog.jsonl"
    actual = (
        [json.loads(line) for line in trace.read_text().splitlines()]
        if trace.is_file()
        else []
    )
    if len(actual) != 2 or {row["operation"]: row["duration"] for row in actual} != {
        "poll": 1,
        "poll_async": 310,
    }:
        return [
            "the extension did not run the required one-second and 310-second polls"
        ]
    if sorted(row["count"] for row in actual) != [1, 2] or any(
        row["timed_out"] is not False for row in actual
    ):
        return ["the watchdog polls did not both return a normal observation"]
    intervals = [(row.get("started"), row.get("finished")) for row in actual]
    if any(
        type(start) not in (int, float)
        or type(finish) not in (int, float)
        or not math.isfinite(start)
        or not math.isfinite(finish)
        or start >= finish
        for start, finish in intervals
    ) or max(start for start, _ in intervals) >= min(finish for _, finish in intervals):
        return ["the recorded poll intervals did not overlap"]
    run = evidence.trajectory
    polls = [
        run.function_calls(operation=f"watchdog_probe.{name}", state="succeeded")
        for name in ("poll", "poll_async")
    ]
    if any(len(calls) != 1 for calls in polls):
        return ["expected one receipt for each poll"]
    short, long = (calls[0] for calls in polls)
    if (
        not 750 <= (short.duration_ms or 0) < 30_000
        or (long.duration_ms or 0) < 300_000
    ):
        return [
            "poll durations do not show the short control and a wait over five minutes"
        ]
    if short.tool_call_id != long.tool_call_id:
        return ["both polls must start in the same Python block"]
    if any(
        not block.error
        and block.tool_call_id == long.tool_call_id
        and "EXTENSION_WATCHDOG_OK" in block.stdout.splitlines()
        and {"counts": [1, 2], "timed_out": [False, False]}
        in observations(block.stdout)
        for block in run.tool_calls
    ):
        return []
    return ["missing structured observations of both polls"]
