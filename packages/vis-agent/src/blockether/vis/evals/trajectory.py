"""Read a Vis run from an ATIF trajectory and check each function call.

Vis writes a run as an ATIF trajectory (JSON) when you give `--trajectory PATH`.
Use this module in evaluations and graders:

- `Trajectory` has the steps, the totals and the status of the run.
- `ToolCall` is one code block that the model ran, with its output and its Activity.
- `FunctionCall` is one host function call in a block: its operation, arguments,
  result, state and duration, with the tokens and the cost of its step.

All names and `to_dict()` keys are snake_case. Importing this module starts no
process and makes no network calls.
"""

from __future__ import annotations

import json
import os
from collections.abc import Mapping
from dataclasses import dataclass, fields
from pathlib import Path
from types import MappingProxyType
from typing import Any

from .._wire import freeze, to_wire
from ..activity import ActivityCounts, ActivityProjection, ActivityRow

__all__ = [
    "FunctionCall",
    "Step",
    "StepMetrics",
    "ToolCall",
    "Trajectory",
    "TrajectoryMetrics",
    "load_trajectory",
    "parse_trajectory",
]

_EMPTY: Mapping[str, Any] = MappingProxyType({})


def _int(value: Any) -> int | None:
    return value if isinstance(value, int) and not isinstance(value, bool) else None


def _float(value: Any) -> float | None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    return float(value)


def _str(value: Any) -> str | None:
    return value if isinstance(value, str) else None


def _map(value: Any) -> Mapping[str, Any]:
    return value if isinstance(value, Mapping) else _EMPTY


def _text(value: Any) -> str | None:
    """Return the text of an ATIF message or observation content."""
    if isinstance(value, str):
        return value
    if isinstance(value, tuple):
        texts = [
            part["text"]
            for part in value
            if isinstance(part, Mapping) and isinstance(part.get("text"), str)
        ]
        return "\n".join(texts) if texts else None
    return None


def _plain(value: Any) -> dict[str, Any]:
    return {field.name: getattr(value, field.name) for field in fields(value)}


def _invalid(detail: str) -> ValueError:
    return ValueError(f"invalid trajectory: {detail}")


@dataclass(frozen=True, slots=True)
class StepMetrics:
    """The tokens and the cost of one provider call.

    `prompt_tokens` includes `cached_tokens`. A value is None when the run did not
    record it. These metrics belong to the whole step, so do not add them up over
    the function calls of the step.
    """

    prompt_tokens: int | None = None
    completion_tokens: int | None = None
    cached_tokens: int | None = None
    cost_usd: float | None = None
    cache_creation_tokens: int | None = None
    reasoning_tokens: int | None = None

    @classmethod
    def _from_atif(cls, value: Any) -> StepMetrics | None:
        if not isinstance(value, Mapping):
            return None
        extra = _map(value.get("extra"))
        return cls(
            _int(value.get("prompt_tokens")),
            _int(value.get("completion_tokens")),
            _int(value.get("cached_tokens")),
            _float(value.get("cost_usd")),
            _int(extra.get("cache_creation_tokens")),
            _int(extra.get("reasoning_tokens")),
        )

    def to_dict(self) -> dict[str, Any]:
        """Return the metrics as plain JSON data. An unknown value is None."""
        return _plain(self)


@dataclass(frozen=True, slots=True)
class TrajectoryMetrics:
    """The totals of a run: tokens, cost, steps and time.

    `total_prompt_tokens` includes `total_cached_tokens`. A value is None when the
    run did not record it.
    """

    total_prompt_tokens: int | None = None
    total_completion_tokens: int | None = None
    total_cached_tokens: int | None = None
    total_cost_usd: float | None = None
    total_steps: int | None = None
    cache_creation_tokens: int | None = None
    reasoning_tokens: int | None = None
    total_tokens: int | None = None
    iteration_count: int | None = None
    duration_ms: int | None = None
    subagent_cost_usd: float | None = None

    @classmethod
    def _from_atif(cls, value: Any) -> TrajectoryMetrics | None:
        if not isinstance(value, Mapping):
            return None
        extra = _map(value.get("extra"))
        return cls(
            _int(value.get("total_prompt_tokens")),
            _int(value.get("total_completion_tokens")),
            _int(value.get("total_cached_tokens")),
            _float(value.get("total_cost_usd")),
            _int(value.get("total_steps")),
            _int(extra.get("cache_creation_tokens")),
            _int(extra.get("reasoning_tokens")),
            _int(extra.get("total_tokens")),
            _int(extra.get("iteration_count")),
            _int(extra.get("duration_ms")),
            _float(extra.get("subagent_cost_usd")),
        )

    def to_dict(self) -> dict[str, Any]:
        """Return the totals as plain JSON data. An unknown value is None."""
        return _plain(self)


@dataclass(frozen=True, slots=True)
class FunctionCall:
    """One host function call in a code block: one Activity row and its place in the run.

    `operation` names the function. `arguments`, `result` and `error` are the text
    evidence that the engine observed, or None. `state` is the outcome. A linked
    call, such as a shell poll, has the `id` of its parent row in `parent_id`.
    `step_metrics` has the tokens and the cost of the whole step, not of this call.
    """

    row: ActivityRow
    step_id: int
    iteration: int | None
    tool_call_id: str
    parent_id: str | None
    depth: int
    step_metrics: StepMetrics | None

    def _evidence(self, kind: str) -> str | None:
        for item in self.row.evidence:
            if item.kind == kind:
                return item.text
        return None

    @property
    def id(self) -> str:
        """The Activity row id. It is unique in its code block."""
        return self.row.id

    @property
    def sequence(self) -> int:
        """The order of the call in its code block."""
        return self.row.sequence

    @property
    def operation(self) -> str:
        """The name of the function, for example `grep` or `shell`."""
        return self.row.operation

    @property
    def state(self) -> str:
        """The outcome: idle, running, succeeded, failed or cancelled."""
        return self.row.state

    @property
    def arguments(self) -> str | None:
        """The arguments as the engine showed them, or None."""
        return self._evidence("arguments")

    @property
    def result(self) -> str | None:
        """The result as the engine showed it, or None."""
        return self._evidence("result")

    @property
    def error(self) -> str | None:
        """The error text of a failed call, or None."""
        return self._evidence("error")

    @property
    def summary(self) -> str:
        """The display summary. Use `state` for the outcome."""
        return self.row.summary

    @property
    def result_summary(self) -> str | None:
        """A short result text, or None."""
        return self.row.result_summary

    @property
    def error_summary(self) -> str | None:
        """A short error text, or None."""
        return self.row.error_summary

    @property
    def duration_ms(self) -> int | None:
        """The run time of the call in milliseconds, or None."""
        return self.row.duration_ms

    @property
    def presenter(self) -> str:
        """The Activity presenter of the call."""
        return self.row.presenter

    @property
    def signal(self) -> str:
        """The kind of effect: observation, mutation, verification, external or generic."""
        return self.row.signal

    @property
    def argument_key(self) -> str | None:
        """A key that is equal for calls with identical arguments, or None."""
        return self.row.argument_key

    def to_dict(self) -> dict[str, Any]:
        """Return the call as plain JSON data.

        `row` has the other Activity fields, without `children`: each child is a
        separate `FunctionCall`.
        """
        row = to_wire(self.row)
        row.pop("children", None)
        return {
            "step_id": self.step_id,
            "iteration": self.iteration,
            "tool_call_id": self.tool_call_id,
            "id": self.id,
            "parent_id": self.parent_id,
            "depth": self.depth,
            "sequence": self.sequence,
            "operation": self.operation,
            "state": self.state,
            "arguments": self.arguments,
            "result": self.result,
            "error": self.error,
            "summary": self.summary,
            "result_summary": self.result_summary,
            "error_summary": self.error_summary,
            "duration_ms": self.duration_ms,
            "presenter": self.presenter,
            "signal": self.signal,
            "argument_key": self.argument_key,
            "step_metrics": (
                self.step_metrics.to_dict() if self.step_metrics is not None else None
            ),
            "row": row,
        }


@dataclass(frozen=True, slots=True)
class ToolCall:
    """One code block that the model ran, and its outcome.

    `output` is the observation that the model saw: the standard output and the
    error text. `activity` is the Activity receipt of the block, or None when the
    run did not record one. Each host function call in the block is in
    `function_calls`. `extra` keeps every Vis field of the call, also unknown ones.
    """

    step_id: int
    iteration: int | None
    tool_call_id: str
    function_name: str
    arguments: Mapping[str, Any]
    output: str | None
    status: str | None
    ref: str | None
    started_at_ms: int | None
    finished_at_ms: int | None
    is_repaired: bool
    is_timeout: bool
    stdout_chars: int | None
    error: Mapping[str, Any] | None
    repaired_source: str | None
    activity: ActivityProjection | None
    activity_problem: str | None
    step_metrics: StepMetrics | None
    extra: Mapping[str, Any]

    @property
    def code(self) -> str | None:
        """The Python source of the block, or None."""
        return _str(self.arguments.get("code"))

    @property
    def activity_state(self) -> str | None:
        """The state of the whole block, or None without Activity."""
        return self.activity.state if self.activity is not None else None

    @property
    def activity_counts(self) -> ActivityCounts | None:
        """The function calls of the block by outcome, or None without Activity."""
        return self.activity.counts if self.activity is not None else None

    @property
    def function_calls(self) -> tuple[FunctionCall, ...]:
        """Each host function call of the block in run order, child rows included."""
        if self.activity is None:
            return ()
        found: list[FunctionCall] = []

        def visit(
            rows: tuple[ActivityRow, ...], parent_id: str | None, depth: int
        ) -> None:
            for row in sorted(rows, key=lambda item: item.sequence):
                found.append(
                    FunctionCall(
                        row,
                        self.step_id,
                        self.iteration,
                        self.tool_call_id,
                        parent_id,
                        depth,
                        self.step_metrics,
                    )
                )
                if row.children:
                    visit(row.children, row.id, depth + 1)

        visit(self.activity.rows, None, 0)
        return tuple(found)

    def to_dict(self) -> dict[str, Any]:
        """Return the block and its function calls as plain JSON data."""
        counts = self.activity_counts
        return {
            "step_id": self.step_id,
            "iteration": self.iteration,
            "tool_call_id": self.tool_call_id,
            "function_name": self.function_name,
            "arguments": to_wire(self.arguments),
            "output": self.output,
            "status": self.status,
            "ref": self.ref,
            "started_at_ms": self.started_at_ms,
            "finished_at_ms": self.finished_at_ms,
            "is_repaired": self.is_repaired,
            "is_timeout": self.is_timeout,
            "stdout_chars": self.stdout_chars,
            "error": to_wire(self.error) if self.error is not None else None,
            "repaired_source": self.repaired_source,
            "activity_state": self.activity_state,
            "activity_counts": _plain(counts) if counts is not None else None,
            "activity_problem": self.activity_problem,
            "step_metrics": (
                self.step_metrics.to_dict() if self.step_metrics is not None else None
            ),
            "function_calls": [call.to_dict() for call in self.function_calls],
        }


@dataclass(frozen=True, slots=True)
class Step:
    """One ATIF step: the user request, or one agent iteration and its code blocks.

    `message` is text for Vis runs. On the final agent step it is the final answer.
    `extra` keeps every Vis field of the step, also unknown ones.
    """

    step_id: int
    source: str
    message: Any
    timestamp: str | None
    model_name: str | None
    reasoning_effort: str | None
    reasoning_content: str | None
    metrics: StepMetrics | None
    tool_calls: tuple[ToolCall, ...]
    extra: Mapping[str, Any]

    @property
    def iteration(self) -> int | None:
        """The Vis iteration number, or None for the user step."""
        return _int(self.extra.get("iteration"))

    @property
    def is_final(self) -> bool:
        """True when Vis marked this step as the final answer."""
        return self.extra.get("is_final") is True

    @property
    def provider(self) -> str | None:
        """The provider of this step's model call, or None."""
        return _str(self.extra.get("provider"))

    @property
    def error(self) -> str | None:
        """The error text of this step, or None."""
        return _str(self.extra.get("error"))

    @property
    def finished_at_ms(self) -> int | None:
        """The end time of the step in epoch milliseconds, or None."""
        return _int(self.extra.get("finished_at_ms"))

    @property
    def text(self) -> str | None:
        """The message as text, or None."""
        return _text(self.message)

    @property
    def function_calls(self) -> tuple[FunctionCall, ...]:
        """Each host function call of this step in run order."""
        return tuple(call for tool in self.tool_calls for call in tool.function_calls)

    def to_dict(self) -> dict[str, Any]:
        """Return the step, its blocks and its function calls as plain JSON data."""
        return {
            "step_id": self.step_id,
            "source": self.source,
            "message": to_wire(self.message),
            "timestamp": self.timestamp,
            "model_name": self.model_name,
            "reasoning_effort": self.reasoning_effort,
            "reasoning_content": self.reasoning_content,
            "iteration": self.iteration,
            "is_final": self.is_final,
            "provider": self.provider,
            "error": self.error,
            "finished_at_ms": self.finished_at_ms,
            "metrics": self.metrics.to_dict() if self.metrics is not None else None,
            "tool_calls": [tool.to_dict() for tool in self.tool_calls],
        }


@dataclass(frozen=True, slots=True)
class Trajectory:
    """One Vis run read from an ATIF trajectory.

    The typed fields hold what evaluations check. `document` keeps the whole input,
    also the keys that this reader does not know. Child runs of Council subagents
    are in `subagents`.
    """

    schema_version: str
    session_id: str | None
    trajectory_id: str | None
    agent: Mapping[str, Any]
    steps: tuple[Step, ...]
    final_metrics: TrajectoryMetrics | None
    extra: Mapping[str, Any]
    subagents: tuple[Trajectory, ...]
    document: Mapping[str, Any]

    @property
    def agent_name(self) -> str | None:
        """The agent name, for example `vis`."""
        return _str(self.agent.get("name"))

    @property
    def agent_version(self) -> str | None:
        """The agent version, or None."""
        return _str(self.agent.get("version"))

    @property
    def model_name(self) -> str | None:
        """The model of the run, or None."""
        return _str(self.agent.get("model_name"))

    @property
    def provider(self) -> str | None:
        """The provider of the run, or None."""
        return _str(_map(self.agent.get("extra")).get("provider"))

    @property
    def profile(self) -> Mapping[str, Any] | None:
        """The run options that shaped the agent, or None."""
        value = _map(self.agent.get("extra")).get("profile")
        return value if isinstance(value, Mapping) else None

    @property
    def status(self) -> str | None:
        """The run status that Vis recorded, or None."""
        return _str(self.extra.get("status"))

    @property
    def error(self) -> str | None:
        """The error text of the run, or None."""
        return _str(self.extra.get("error"))

    @property
    def goal(self) -> Mapping[str, Any] | None:
        """The saved session goal of a persisted run, or None."""
        value = self.extra.get("goal")
        return value if isinstance(value, Mapping) else None

    @property
    def session_usage(self) -> Mapping[str, Any] | None:
        """The usage rollup of a persisted session, or None."""
        value = self.extra.get("session_usage")
        return value if isinstance(value, Mapping) else None

    @property
    def evaluation(self) -> Mapping[str, Any] | None:
        """The structured answer check (`extra.eval`), or None."""
        value = self.extra.get("eval")
        return value if isinstance(value, Mapping) else None

    @property
    def user_message(self) -> str | None:
        """The text of the first user step, or None."""
        for step in self.steps:
            if step.source == "user":
                return step.text
        return None

    @property
    def agent_steps(self) -> tuple[Step, ...]:
        """The agent steps in run order."""
        return tuple(step for step in self.steps if step.source == "agent")

    @property
    def final_step(self) -> Step | None:
        """The step with the final answer, or None when the run has no final answer.

        A trace run marks that step with `is_final`. A persisted run has no marks:
        then it is the last agent step, if that step has no code blocks and no error.
        """
        agent_steps = self.agent_steps
        if any("is_final" in step.extra for step in agent_steps):
            return next((s for s in reversed(agent_steps) if s.is_final), None)
        if not agent_steps or self.error is not None:
            return None
        last = agent_steps[-1]
        return last if not last.tool_calls and last.error is None else None

    @property
    def final_answer(self) -> str | None:
        """The final answer text, or None."""
        step = self.final_step
        return step.text if step is not None else None

    @property
    def tool_calls(self) -> tuple[ToolCall, ...]:
        """Each code block of the run in order."""
        return tuple(tool for step in self.steps for tool in step.tool_calls)

    def function_calls(
        self, *, operation: str | None = None, state: str | None = None
    ) -> tuple[FunctionCall, ...]:
        """Each host function call of this run in order, optionally filtered.

        Args:
            operation: Keep only calls of this function, for example `grep`.
            state: Keep only calls with this outcome, for example `failed`.

        The calls of subagents are in `subagents`, not here.
        """
        return tuple(
            call
            for step in self.steps
            for call in step.function_calls
            if (operation is None or call.operation == operation)
            and (state is None or call.state == state)
        )

    def to_dict(self) -> dict[str, Any]:
        """Return the typed view of the run as plain JSON data.

        For the original ATIF document, use `document`.
        """
        return {
            "schema_version": self.schema_version,
            "session_id": self.session_id,
            "trajectory_id": self.trajectory_id,
            "agent_name": self.agent_name,
            "agent_version": self.agent_version,
            "model_name": self.model_name,
            "provider": self.provider,
            "status": self.status,
            "error": self.error,
            "final_answer": self.final_answer,
            "final_metrics": (
                self.final_metrics.to_dict() if self.final_metrics is not None else None
            ),
            "steps": [step.to_dict() for step in self.steps],
            "subagents": [child.to_dict() for child in self.subagents],
        }


def _tool_call(
    raw: Any,
    step_id: int,
    iteration: int | None,
    metrics: StepMetrics | None,
    outputs: Mapping[str, Any],
    strict: bool,
) -> ToolCall:
    if not isinstance(raw, Mapping):
        raise _invalid(f"a tool call of step {step_id} is not an object")
    tool_call_id = _str(raw.get("tool_call_id"))
    function_name = _str(raw.get("function_name"))
    if tool_call_id is None or function_name is None:
        raise _invalid(f"a tool call of step {step_id} has no id or function name")
    extra = _map(raw.get("extra"))
    activity = None
    problem = None
    if extra.get("activity") is not None:
        try:
            activity = ActivityProjection.from_wire(to_wire(extra["activity"]))
        except ValueError as failure:
            if strict:
                raise _invalid(
                    f"the Activity of call {tool_call_id} in step {step_id}: {failure}"
                ) from None
            problem = str(failure)
    error = extra.get("error")
    return ToolCall(
        step_id=step_id,
        iteration=iteration,
        tool_call_id=tool_call_id,
        function_name=function_name,
        arguments=_map(raw.get("arguments")),
        output=_text(outputs.get(tool_call_id)),
        status=_str(extra.get("status")),
        ref=_str(extra.get("ref")),
        started_at_ms=_int(extra.get("started_at_ms")),
        finished_at_ms=_int(extra.get("finished_at_ms")),
        is_repaired=extra.get("is_repaired") is True,
        is_timeout=extra.get("is_timeout") is True,
        stdout_chars=_int(extra.get("stdout_chars")),
        error=error if isinstance(error, Mapping) else None,
        repaired_source=_str(extra.get("repaired_source")),
        activity=activity,
        activity_problem=problem,
        step_metrics=metrics,
        extra=extra,
    )


def _step(raw: Any, index: int, strict: bool) -> Step:
    if not isinstance(raw, Mapping):
        raise _invalid(f"step {index} is not an object")
    step_id = _int(raw.get("step_id"))
    source = _str(raw.get("source"))
    if step_id is None or source is None:
        raise _invalid(f"step {index} has no integer step_id or no source")
    extra = _map(raw.get("extra"))
    metrics = StepMetrics._from_atif(raw.get("metrics"))
    results = _map(raw.get("observation")).get("results")
    outputs = {
        result["source_call_id"]: result.get("content")
        for result in (results if isinstance(results, tuple) else ())
        if isinstance(result, Mapping) and isinstance(result.get("source_call_id"), str)
    }
    calls = raw.get("tool_calls")
    iteration = _int(extra.get("iteration"))
    return Step(
        step_id=step_id,
        source=source,
        message=raw.get("message"),
        timestamp=_str(raw.get("timestamp")),
        model_name=_str(raw.get("model_name")),
        reasoning_effort=_str(raw.get("reasoning_effort")),
        reasoning_content=_str(raw.get("reasoning_content")),
        metrics=metrics,
        tool_calls=tuple(
            _tool_call(call, step_id, iteration, metrics, outputs, strict)
            for call in (calls if isinstance(calls, tuple) else ())
        ),
        extra=extra,
    )


def _trajectory(
    document: Mapping[str, Any], strict: bool, inherited: str | None
) -> Trajectory:
    version = _str(document.get("schema_version")) or inherited
    if version is None or not version.startswith("ATIF-"):
        raise _invalid("schema_version is not an ATIF version")
    steps = document.get("steps")
    if not isinstance(steps, tuple):
        raise _invalid("steps is not a list")
    children = document.get("subagent_trajectories")
    return Trajectory(
        schema_version=version,
        session_id=_str(document.get("session_id")),
        trajectory_id=_str(document.get("trajectory_id")),
        agent=_map(document.get("agent")),
        steps=tuple(_step(step, index, strict) for index, step in enumerate(steps)),
        final_metrics=TrajectoryMetrics._from_atif(document.get("final_metrics")),
        extra=_map(document.get("extra")),
        subagents=tuple(
            _trajectory(child, strict, version)
            for child in (children if isinstance(children, tuple) else ())
            if isinstance(child, Mapping)
        ),
        document=document,
    )


def parse_trajectory(document: Mapping[str, Any], *, strict: bool = True) -> Trajectory:
    """Build a `Trajectory` from an ATIF document, a decoded JSON object.

    The result is immutable and independent of `document`. With `strict=True`, an
    Activity receipt that breaks the Activity contract raises ValueError. With
    `strict=False`, that block gets `activity=None` and the reason in
    `activity_problem`, so older files stay readable.

    Raises:
        ValueError: The document is not an ATIF trajectory, or an Activity receipt
            is invalid. The message never contains run content.
    """
    if not isinstance(document, Mapping):
        raise _invalid("the document is not a JSON object")
    return _trajectory(freeze(to_wire(document)), strict, None)


def load_trajectory(path: str | os.PathLike[str], *, strict: bool = True) -> Trajectory:
    """Read an ATIF trajectory file (UTF-8 JSON) and parse it.

    Raises:
        OSError: The file cannot be read.
        ValueError: The file is not JSON or not a valid ATIF trajectory.
    """
    text = Path(path).read_text(encoding="utf-8")
    try:
        document = json.loads(text)
    except json.JSONDecodeError as failure:
        raise _invalid(f"the file is not JSON (line {failure.lineno})") from None
    return parse_trajectory(document, strict=strict)
