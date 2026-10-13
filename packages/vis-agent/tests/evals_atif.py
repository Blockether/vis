"""Build the ATIF trajectory of one Vis run, as Vis writes it, for grader tests.

All data is synthetic. Each builder returns plain JSON data, so a test can change a
field before `trial` parses the document.
"""

from blockether.vis.evals import grade, parse_trajectory

PROVIDER, MODEL = "test-provider", "test-model"
TOKENS = {"input": 100, "cached": 40, "output": 10, "total": 110}
STATES = ("running", "succeeded", "failed", "cancelled")


def row(row_id, operation, state="succeeded", *, sequence=1, **more):
    """Return one valid Activity row."""
    return {
        "id": row_id,
        "sequence": sequence,
        "operation": operation,
        "presenter": "generic",
        "signal": "observation",
        "state": state,
        "summary": operation,
        "resources": [],
        "evidence": [],
        **more,
    }


def receipt(*rows):
    """Return the Activity receipt of one block with the top-level ``rows``."""
    counts = dict.fromkeys(STATES, 0)
    for item in rows:
        counts[item["state"]] += 1
    state = next(
        (state for state in ("running", "failed", "cancelled") if counts[state]),
        "succeeded",
    )
    return {"state": state, "counts": counts, "rows": list(rows)}


def block(ref, code="", *, stdout="", error=None, activity=None, call_id=None):
    """Return one code block: its ATIF tool call and its observation result.

    ``error`` is the error message or the whole error map, or None. ``activity``
    is a receipt, or None for a block without Activity.
    """
    call_id = call_id or f"call-{ref}"
    extra = {"is_repaired": False, "is_timeout": False, "stdout_chars": len(stdout)}
    if ref is not None:
        extra["ref"] = ref
    if isinstance(error, str):
        error = {"type": "error", "message": error}
    if error is not None:
        extra["error"] = error
    if activity is not None:
        extra["activity"] = activity
    message = error.get("message") if error is not None else None
    output = "\n".join(text for text in (stdout, message) if text and text.strip())
    return {
        "tool_call": {
            "tool_call_id": call_id,
            "function_name": "python_execution",
            "arguments": {"code": code},
            "extra": extra,
        },
        "result": {"source_call_id": call_id, "content": output},
    }


def step(iteration, *blocks, provider=PROVIDER, model=MODEL, answer=None):
    """Return one agent step. An ``answer`` makes it the final step of the run."""
    extra = {"iteration": iteration, "provider": provider}
    if answer is not None:
        extra["is_final"] = True
    value = {
        "source": "agent",
        "model_name": model,
        "message": answer or "",
        "extra": extra,
    }
    if blocks:
        value["tool_calls"] = [item["tool_call"] for item in blocks]
        value["observation"] = {"results": [item["result"] for item in blocks]}
    return value


def trajectory(
    *steps,
    answer="Done.",
    provider=PROVIDER,
    model=MODEL,
    session_id="session-1",
    tokens=TOKENS,
    **extra,
):
    """Return the ATIF document of one run.

    ``answer`` adds a final step after ``steps``. None leaves the run without a
    final step. ``extra`` gives the run fields, for example ``error``, ``eval``,
    ``goal`` and ``session_usage``.
    """
    steps = list(steps)
    if answer is not None:
        iteration = max((value["extra"]["iteration"] for value in steps), default=0)
        steps.append(step(iteration + 1, provider=provider, model=model, answer=answer))
    numbered = [
        {"step_id": 1, "source": "user", "message": "Do the task."},
        *({"step_id": index, **value} for index, value in enumerate(steps, 2)),
    ]
    document = {
        "schema_version": "ATIF-v1.8",
        "agent": {
            "name": "vis",
            "version": "dev",
            "model_name": model,
            "extra": {"provider": provider},
        },
        "steps": numbered,
        "extra": {key: value for key, value in extra.items() if value is not None},
    }
    if session_id is not None:
        document["session_id"] = session_id
    if tokens is not None:
        document["final_metrics"] = {
            "total_prompt_tokens": tokens.get("input"),
            "total_completion_tokens": tokens.get("output"),
            "total_cached_tokens": tokens.get("cached"),
            "total_steps": len(numbered),
            "extra": {
                "cache_creation_tokens": tokens.get("cache_created"),
                "reasoning_tokens": tokens.get("reasoning"),
                "total_tokens": tokens.get("total"),
            },
        }
    return document


def trial(document, *, strict=True, **more):
    """Return the files of a finished trial whose run wrote ``document``.

    ``strict`` rejects an invalid Activity receipt, so a fixture cannot break the
    Activity contract by mistake.
    """
    fields = {
        "trajectory": parse_trajectory(document, strict=strict),
        "trajectory_problem": None,
        "stderr": "",
        "exit_code": 0,
        "wall_s": 1.0,
        "is_cancelled": False,
        "provider": PROVIDER,
        "model": MODEL,
        "reasoning_effort": None,
        **more,
    }
    return grade.TrialFiles(**fields)
