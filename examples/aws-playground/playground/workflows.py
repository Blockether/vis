"""Durable workflows on the Vis SDK.

A workflow is a Python function with `@workflow`. Its steps are functions with
`@activity`. One run is one Vis session: an activity sends requests to that session
with the SDK and gets text or a validated Pydantic model back.

A run is durable. The playground records the result of each completed activity in S3.
When a worker stops in the middle of a run, the next worker starts the function again
and each recorded activity returns its result at once. So write the function like a
Temporal workflow: decide only from its input and from the results of activities.

A document of a run is a native Vis attachment: the session calls `attach`, and the
activity reads the files of that turn with `Session.attachments` of the SDK. A run can
also wait for a file from a person with `ctx.receive`. A waiting run has no process.

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
import typing
from collections.abc import Callable
from dataclasses import asdict, dataclass, field
from typing import Any, TypeVar

from blockether.vis.engine import Attachment, GatewayError, Session, Turn, final_answer
from pydantic import BaseModel

from playground.client import Playground

Model = TypeVar("Model", bound=BaseModel)
# The statuses of a turn that does not run any more.
ENDED = ("completed", "failed", "cancelled", "suspended", "error")

__all__ = ["ActivityError", "Context", "Document", "Upload", "Waiting", "activity", "workflow"]


class ActivityError(RuntimeError):
    """An activity did not get a usable answer from its session."""


class Waiting(BaseException):
    """The run waits for a person. It is not an `Exception`, so no activity catches it by accident."""


@dataclass(frozen=True)
class Document:
    """A document that the session of a run attached. S3 holds a copy for the pages."""

    name: str
    session: str
    size: int
    version: int = 1


@dataclass(frozen=True)
class Upload:
    """A file that a person gave to a run. The record of the activity keeps only its name and size."""

    key: str
    name: str
    size: int
    text: str = field(repr=False)


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
    if isinstance(value, Document):
        return {"kind": "document", "value": asdict(value)}
    if isinstance(value, Upload):
        return {"kind": "upload", "value": {"key": value.key, "name": value.name, "size": value.size}}
    if isinstance(value, (str, int, float, bool, list, dict)) or value is None:
        return {"kind": "json", "value": value}
    return {"kind": "json", "value": str(value)}


def decode(recorded: dict[str, Any], function: Callable[..., Any], upload: Callable[[str], Upload | None]) -> Any:
    """Build the result of an activity again from its record. `upload` reads a file that a person gave."""
    value = recorded["value"]
    if recorded["kind"] == "document":
        return Document(**value)
    if recorded["kind"] == "upload":
        return upload(value["key"])
    if recorded["kind"] == "model":
        returns = typing.get_type_hints(function).get("return")
        if isinstance(returns, type) and issubclass(returns, BaseModel):
            return returns.model_validate(value)
    return value


class Context:
    """The run that a workflow function works in."""

    def __init__(self, run: dict[str, Any], session: Session, api: Playground) -> None:
        self.run_id, self.input, self.session = run["id"], run["input"], session
        # The number of the next activity, the activity that runs, and its requests until now.
        self._api, self._next, self._current, self._turns = api, 0, None, 0
        # For each request of an activity: the number of session turns before it.
        self._asked: dict[str, int] = dict(run.get("asked") or {})

    def _event(self, kind: str, **fields: Any) -> Any:
        return self._api.event(self.run_id, kind, **fields)

    def _step(self, function: Callable[..., Any], title: str, arguments: tuple[Any, ...], named: dict[str, Any]) -> Any:
        """Run an activity and record its result, or return the result that an earlier process recorded."""
        if self._current is not None:  # an activity in an activity is a plain call
            return function(self, *arguments, **named)
        seq, self._next = self._next, self._next + 1
        recorded = self._api.call("GET", f"runs/{self.run_id}/results/{seq}")
        if recorded and recorded["name"] == function.__name__:
            return decode(recorded["result"], function, self._upload)
        self._event("activity_started", seq=seq, name=function.__name__, title=title)
        self._current, self._turns = seq, 0
        try:
            result = function(self, *arguments, **named)
        except Exception as failure:
            self._event("activity_failed", seq=seq, error=f"{type(failure).__name__}: {failure}")
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
        return final_answer(self._turn(request, timeout, earlier)) or ""

    def _turn(self, request: str, timeout: float, earlier: dict[str, Any] | None) -> dict[str, Any]:
        """Return the completed turn of a request. Send the request when no earlier process completed it."""
        turn = earlier if earlier and earlier.get("status") == "completed" else self.session.send(request).wait(timeout=timeout)
        if turn.get("status") != "completed":
            raise ActivityError(f"the turn ended with the status {turn.get('status')!r}")
        return turn

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
        # The turns of this request end where the next request of the activity starts.
        after = self._asked.get(f"{self._current}:{self._turns + 1}")
        while sent := turns[self._asked[key] : after]:
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

    def document(self, name: str, request: str, *, timeout: float = 900) -> Document:
        """Let the session write a document and attach it.

        The session attaches the file with the native `attach` verb of Vis, so the
        transcript shows it. The SDK gives the files of exactly that turn. A document
        with a name that the session used before becomes the next version of that file.
        """
        how = f"Write the complete document as Markdown. Attach it with one Python block that calls `attach(content.encode(), {name!r})`. Then answer with a summary of at most three sentences."
        found = self._attached(name, f"{request}\n\n{how}", timeout) or self._attached(name, f"That turn attached no file {name!r}. {how}", timeout)
        if found is None:
            raise ActivityError(f"the session attached no file {name!r}")
        # This read makes the API copy the file to S3 and show it in the activity.
        self._api.call("GET", f"attached?session={self.session.id}")
        return Document(name, self.session.id, found.size, found.version)

    def _attached(self, name: str, request: str, timeout: float) -> Attachment | None:
        """Send a request and return the newest file with a name that its turn attached."""
        turn = self._turn(request, timeout, self._earlier(timeout))
        files = [file for file in self.session.attachments(turn["turn_id"]) if file.filename == name]
        return max(files, key=lambda file: file.version, default=None)

    def receive(self, key: str, title: str, *, accept: str = "") -> Upload:
        """Wait for a file from a person. `accept` names the file types for the page, such as `.md,.txt`.

        The run process ends while the run waits, so a waiting run holds no worker.
        When the person gives the file, the playground starts the run again and this
        call returns the file.
        """
        found = self._upload(key)
        if found is None:
            self._event("waiting", seq=self._current, key=key, title=title, accept=accept)
            raise Waiting(key)
        return found

    def _upload(self, key: str) -> Upload | None:
        given = self._api.call("GET", f"runs/{self.run_id}/inputs/{key}")
        return Upload(key, given["name"], given["size"], given["content"]) if given else None

    def note(self, text: str) -> None:
        """Add a line to the log of the run."""
        self._event("note", text=str(text)[:2000], seq=self._current)
