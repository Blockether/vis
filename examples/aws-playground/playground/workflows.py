"""Durable workflows on the Vis SDK.

A workflow is a Python function with `@workflow`. Its steps are functions with
`@activity`. One run is one Vis session: an activity sends requests to that session
with the SDK and gets text or a validated Pydantic model back.

A run is durable. The playground records the result of each completed activity in S3.
When a worker stops in the middle of a run, the next worker starts the function again
and each recorded activity returns its result at once. So write the function like a
Temporal workflow: decide only from its input and from the results of activities.

    class Verdict(BaseModel):
        decision: Literal["build", "clarify"]
        reason: str

    @activity(title="Analysis of the story")
    def analyse(ctx, story: str) -> Verdict:
        return ctx.ask(f"Analyse this user story: {story}", Verdict)

    @workflow(title="Story to SPEC")
    def story_to_spec(ctx, story: str = "As a user ..."):
        verdict = analyse(ctx, story)
        if verdict.decision == "build":
            return write_spec(ctx, story)
"""

from __future__ import annotations

import functools
import time
import traceback
import typing
from collections.abc import Callable
from dataclasses import asdict, dataclass
from typing import Any, TypeVar

from blockether.vis.engine import GatewayError, Session, Turn, final_answer
from pydantic import BaseModel

from playground.client import Playground

Model = TypeVar("Model", bound=BaseModel)
# The statuses of a turn that does not run any more.
ENDED = ("completed", "failed", "cancelled", "suspended", "error")

__all__ = ["ActivityError", "Artifact", "Context", "activity", "workflow"]


class ActivityError(RuntimeError):
    """An activity did not get a usable answer from its session."""


@dataclass(frozen=True)
class Artifact:
    """A document of a run. It lives in S3 and shows in the transcript and in the run."""

    name: str
    session: str
    size: int

    @property
    def path(self) -> str:
        return f"/play/api/artifacts/{self.session}/{self.name}"


def workflow(title: str = "", description: str = "", model: str | None = None) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    """Mark the function that a run calls. `model` selects the model of the session."""

    def mark(function: Callable[..., Any]) -> Callable[..., Any]:
        function.__workflow__ = {"title": title or function.__name__, "description": description, "model": model}  # type: ignore[attr-defined]
        return function

    return mark


def activity(title: str = "") -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    """Make a function a recorded step. Its first argument is the context of the run."""

    def wrap(function: Callable[..., Any]) -> Callable[..., Any]:
        @functools.wraps(function)
        def call(ctx: Context, *arguments: Any, **named: Any) -> Any:
            return ctx._step(function, title or function.__name__, arguments, named)

        return call

    return wrap


def encode(value: Any) -> dict[str, Any]:
    """Make a result that JSON can hold."""
    if isinstance(value, BaseModel):
        return {"kind": "model", "type": type(value).__name__, "value": value.model_dump(mode="json")}
    if isinstance(value, Artifact):
        return {"kind": "artifact", "value": asdict(value)}
    if isinstance(value, (str, int, float, bool, list, dict)) or value is None:
        return {"kind": "json", "value": value}
    return {"kind": "json", "value": str(value)}


def decode(recorded: dict[str, Any], function: Callable[..., Any]) -> Any:
    """Build the result of an activity again from its record."""
    value = recorded["value"]
    if recorded["kind"] == "artifact":
        return Artifact(**value)
    if recorded["kind"] == "model":
        returns = typing.get_type_hints(function).get("return")
        if isinstance(returns, type) and issubclass(returns, BaseModel):
            return returns.model_validate(value)
    return value


class Context:
    """The run that a workflow function works in."""

    def __init__(self, run: dict[str, Any], session: Session, api: Playground) -> None:
        self.run_id, self.input, self.session = run["id"], run["input"], session
        self._api, self._next, self._current, self._turns = api, 0, None, 0
        # For each request of an activity: the number of session turns before it.
        self._asked: dict[str, int] = dict(run.get("asked") or {})

    def _event(self, kind: str, **fields: Any) -> Any:
        return self._api.call("POST", f"runs/{self.run_id}/events", {"type": kind, **fields})

    def _step(self, function: Callable[..., Any], title: str, arguments: tuple[Any, ...], named: dict[str, Any]) -> Any:
        if self._current is not None:  # an activity in an activity is a plain call
            return function(self, *arguments, **named)
        seq, self._next = self._next, self._next + 1
        recorded = self._api.call("GET", f"runs/{self.run_id}/results/{seq}")
        if recorded and recorded["name"] == function.__name__:
            return decode(recorded["result"], function)
        self._event("activity_started", seq=seq, name=function.__name__, title=title)
        self._current, self._turns = seq, 0
        try:
            result = function(self, *arguments, **named)
        except Exception as failure:
            self._event("activity_failed", seq=seq, error=f"{type(failure).__name__}: {failure}", trace=traceback.format_exc()[-1500:])
            raise
        finally:
            self._current = None
        self._event("activity_completed", seq=seq, name=function.__name__, result=encode(result))
        return result

    def ask(self, request: str, model: type[Model] | None = None, *, timeout: float = 900) -> Any:
        """Send a request to the session of the run.

        With a Pydantic model, return a validated instance: the SDK asks the session to
        correct an answer that does not fit. Without a model, return the answer text.
        """
        earlier = self._earlier(timeout)
        if model is not None:
            try:
                return model.model_validate_json(final_answer(earlier or {}) or "")
            except ValueError:  # no earlier answer, or one that does not fit: the SDK asks
                return self.session.run(request, response_model=model, timeout=timeout)
        turn = earlier if earlier and earlier.get("status") == "completed" else self.session.send(request).wait(timeout=timeout)
        if turn.get("status") != "completed":
            raise ActivityError(f"the turn ended with the status {turn.get('status')!r}")
        return final_answer(turn) or ""

    def _earlier(self, timeout: float) -> dict[str, Any] | None:
        """Return the end of the turn that an earlier process of this run started for this request.

        A worker stops before the Lambda time limit and parks the running turn. The gateway
        of the next worker continues that turn. Thus the new run process waits for the turn
        and sends no second request.
        """
        self._turns += 1
        key = f"{self._current}:{self._turns}"
        turns = self.session.turns()
        if key not in self._asked:
            self._asked[key] = len(turns)
            self._event("asked", key=key, turns=len(turns))
            return None
        end = time.monotonic() + timeout
        while sent := turns[self._asked[key] :]:
            if sent[-1]["status"] in ENDED:
                return sent[-1]
            try:
                return Turn(self.session, sent[-1]["turn_id"]).wait(timeout=max(end - time.monotonic(), 1))
            except GatewayError as failure:  # the new gateway did not take the turn yet
                if failure.code != "turn-not-found":
                    raise
            time.sleep(2)
            turns = self.session.turns()
        return None

    def artifact(self, name: str, content: str) -> Artifact:
        """Save a document of the run in S3."""
        body = {"session": self.session.id, "name": name, "content": content, "source": "workflow"}
        return Artifact(**self._api.call("POST", "artifacts", body))

    def document(self, name: str, request: str, *, timeout: float = 900) -> Artifact:
        """Let the session write a document and save it as an artifact.

        The session saves the document with the `playground.save_artifact` tool, so the
        transcript shows it as a Vis activity. When the session only answers with the
        text, the playground saves that answer.
        """
        answer = self.ask(
            f"{request}\n\nWrite the complete document as Markdown. Save it with one Python block that calls "
            f"`playground.save_artifact({name!r}, content)`. Then answer with a summary of at most three sentences.",
            timeout=timeout,
        )
        saved = self._api.call("GET", f"artifacts?session={self.session.id}")["artifacts"]
        found = next((item for item in saved if item["name"] == name), None)
        if found:
            return Artifact(name, self.session.id, found["size"])
        return self.artifact(name, answer)

    def note(self, text: str) -> None:
        """Add a line to the log of the run."""
        self._event("note", text=str(text)[:2000], seq=self._current)
