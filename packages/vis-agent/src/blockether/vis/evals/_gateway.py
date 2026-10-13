"""Run one evaluation operation for the gateway `/v1/evals/...` routes.

The engine loads this package into its Python runtime and calls `handle` with the
operation name and the JSON body of the request. Each operation is a pure function
of its body: it reads no files and keeps no state. Only `leaderboard` uses the
network. `handle` gives `{"result": ...}` with plain JSON values, or
`{"error": message}` when the body is not valid for the operation.
"""

from __future__ import annotations

import dataclasses
import math
from collections.abc import Callable, Mapping
from pathlib import PurePath

from .bench import bench_report, cause_check, diagnose
from .compare import compare_cases
from .labels import agreement
from .leaderboard import read_leaderboard

__all__ = ["OPERATIONS", "handle", "plain"]


def plain(value):
    """Convert a result to JSON values: dataclasses to objects, tuples to arrays."""
    if dataclasses.is_dataclass(value) and not isinstance(value, type):
        return {
            field.name: plain(getattr(value, field.name))
            for field in dataclasses.fields(value)
        }
    if isinstance(value, Mapping):
        return {str(key): plain(item) for key, item in value.items()}
    if isinstance(value, (list, tuple, set, frozenset)):
        return [plain(item) for item in value]
    if isinstance(value, PurePath):
        return str(value)
    if isinstance(value, float) and not math.isfinite(value):
        return None
    return value


def _mapping(body: Mapping, key: str, *, required: bool = False) -> dict:
    value = body.get(key)
    if value is None and not required:
        return {}
    if not isinstance(value, Mapping):
        raise ValueError(f"`{key}` must be an object.")
    return dict(value)


def _text(body: Mapping, key: str) -> str | None:
    value = body.get(key)
    if value is not None and not isinstance(value, str):
        raise ValueError(f"`{key}` must be a string.")
    return value


def _word(body: Mapping, key: str, default: str) -> str:
    value = _text(body, key)
    return default if value is None else value


def _required_text(body: Mapping, key: str) -> str:
    value = _text(body, key)
    if not value:
        raise ValueError(f"`{key}` is required.")
    return value


def _cases(body: Mapping, key: str) -> dict[tuple[str, ...], tuple[int, int]]:
    rows = body.get(key)
    if not isinstance(rows, list):
        raise ValueError(f"`{key}` must be an array of cases.")
    cases = {}
    for row in rows:
        if not isinstance(row, Mapping):
            raise ValueError(f"Each case in `{key}` must be an object.")
        try:
            name = (str(row["task"]), str(row["agent"]), str(row["model"]))
            cases[name] = (int(row["passed"]), int(row["runs"]))
        except (KeyError, TypeError, ValueError) as error:
            raise ValueError(
                f"Each case in `{key}` needs task, agent, model, passed and runs."
            ) from error
    return cases


def _report(body: Mapping):
    return bench_report(
        _mapping(body, "summary", required=True), _text(body, "round_name")
    )


def _diagnose(body: Mapping):
    return diagnose(
        _mapping(body, "summary", required=True),
        _mapping(body, "labels"),
        _mapping(body, "limits"),
    )


def _labels(body: Mapping):
    return cause_check(
        _mapping(body, "summary", required=True),
        _mapping(body, "labels", required=True),
        _mapping(body, "limits"),
    )


def _compare(body: Mapping):
    return compare_cases(
        _cases(body, "baseline"),
        _cases(body, "candidate"),
        baseline_name=_word(body, "baseline_name", "baseline"),
        candidate_name=_word(body, "candidate_name", "candidate"),
    )


def _agreement(body: Mapping):
    rows = body.get("items")
    if not isinstance(rows, list):
        raise ValueError("`items` must be an array.")
    items = []
    for row in rows:
        if not isinstance(row, Mapping) or not {"name", "automatic", "human"} <= set(
            row
        ):
            raise ValueError("Each item needs name, automatic and human.")
        items.append((str(row["name"]), str(row["automatic"]), str(row["human"])))
    return agreement(
        items,
        source=_word(body, "source", "automatic"),
        noun=_word(body, "noun", "results"),
        labeller=_word(body, "labeller", "a person"),
        fix=_word(body, "fix", "Correct the automatic rules first."),
    )


def _leaderboard(body: Mapping):
    summary = body.get("summary")
    return read_leaderboard(
        _required_text(body, "package"),
        _required_text(body, "name"),
        None if summary is None else _mapping(body, "summary"),
        _text(body, "round_name"),
    )


OPERATIONS: dict[str, Callable[[Mapping], object]] = {
    "agreement": _agreement,
    "compare": _compare,
    "diagnose": _diagnose,
    "labels": _labels,
    "leaderboard": _leaderboard,
    "report": _report,
}


def handle(operation: str, body: object) -> dict:
    """Run `operation` on the request `body` and give a JSON envelope."""
    run = OPERATIONS.get(operation)
    if run is None:
        return {"error": f"Unknown evaluation operation: {operation}."}
    if not isinstance(body, Mapping):
        return {"error": "The request body must be a JSON object."}
    try:
        return {"result": plain(run(body))}
    except ValueError as error:
        return {"error": str(error)}
