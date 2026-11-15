"""Durable workflows on the Vis SDK.

A workflow is a Python function with `@workflow`. Its steps are functions with
`@activity`. One run is one Vis session: an activity sends requests to that session
with the SDK and gets text or a validated Pydantic model back.

A run is durable, and the transcript of its session is the record. Each request of an
activity is one turn, and a decision at a gate is one turn too. When a worker stops in
the middle of a run, the next worker starts the function again: a request that the
transcript has returns the answer of its turn at once, and only a new request goes to
the session. So write the function like a Temporal workflow: decide only from its input
and from the answers of the session. The Python code of an activity runs again in each
new process.

No work runs again by itself. A person can retry a failed run, and a workflow can
retry an activity: `@activity(retries=2)`. A retry forks the session: the fork has the
transcript before the failed activity, and the run continues in the fork. The session
of the failed attempt stays as it is. The files of the work directory do not go back.

A document of a run is a native Vis attachment: the session calls `attach`, and the
activity reads the files of that turn with `Session.attachments` of the SDK.

One request can use another language model: `ctx.ask(request, llm="qwen3.8-max")`.
`ctx.delegate(tasks)` gives each task to its own subagent of the session and returns
their answers.

A gate stops a run until a person decides: `ctx.gate` returns the decision, approve or
reject, with the comment of the person. A waiting run has no process.

    class Work(BaseModel):
        song: str
        tests_passed: bool

    @activity(title="Write the song sheet")
    def specify(ctx, prompt: str) -> Document:
        return ctx.document("SONG.md", f"Write the song sheet for: {prompt}")

    @activity(title="Approve the song sheet")
    def approve(ctx, sheet: Document) -> Decision:
        return ctx.gate("sheet", "Approve the song sheet", document=sheet)

    @activity(title="Record the song")
    def record(ctx) -> Work:
        return ctx.ask("Record the song of SONG.md as code and run the tests.", Work)

    @workflow(title="Song factory")
    def song_factory(ctx, prompt: str = "A slow song about a night train"):
        sheet = specify(ctx, prompt)
        if approve(ctx, sheet).approved:
            return record(ctx)
"""

from __future__ import annotations

import functools
import json
import time
import typing
from collections.abc import Callable
from dataclasses import asdict, dataclass
from typing import Any, TypeVar

from blockether.vis.engine import Attachment, GatewayError, Session, Turn, final_answer
from pydantic import BaseModel, ValidationError

from durable.settings import CORRECTIONS, PROVIDER, TURN_S
from durable.workflows.client import Durable

Model = TypeVar("Model", bound=BaseModel)
# The statuses of a turn that does not run any more.
ENDED = ("completed", "failed", "cancelled", "suspended", "error")
# The fields of a task of `ctx.delegate`. They are the fields of a task of `council.run` in a session.
TASK = ("task", "title", "role", "model", "provider", "iteration_budget", "files")
# The start of the output line with which the session of a run shows the records of its subagents. A JSON list follows.
DELEGATED = "DELEGATED "
# The start of the request that puts the decision of a person in the transcript. A JSON object follows on the same line.
GATE = "The decision of a person at a gate of this workflow run: "

__all__ = ["ActivityError", "Context", "Decision", "Document", "Waiting", "activity", "workflow"]


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
class Decision:
    """What a person decided at a gate: approve or reject, with a comment."""

    key: str
    approved: bool
    comment: str = ""


def route(llm: str | None, provider: str | None) -> dict[str, str]:
    """Return the turn fields that select the model `llm` for one request. Without a provider, it is the provider of the stack."""
    if llm is None:
        if provider is not None:
            raise ValueError("give `llm` with `provider`")
        return {}
    return {"provider": provider or PROVIDER, "model": llm}


def workflow(title: str = "", description: str = "", model: str | None = None) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    """Mark the function that a run calls. `model` selects the model of the session."""

    def mark(function: Callable[..., Any]) -> Callable[..., Any]:
        """Record the title, the description and the model on the function."""
        function.__workflow__ = {"title": title or function.__name__, "description": description, "model": model}  # type: ignore[attr-defined]
        return function

    return mark


def activity(title: str = "", retries: int = 0) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    """Make a function a step of a run. Its first argument is the context of the run.

    No activity runs again by itself. With `retries`, a failed activity runs again at
    most that number of times. Each retry forks the session: the fork has the
    transcript before the activity, and the run continues in the fork.
    """

    def wrap(function: Callable[..., Any]) -> Callable[..., Any]:
        """Return the function as a step of the run."""

        @functools.wraps(function)
        def call(ctx: Context, *arguments: Any, **named: Any) -> Any:
            """Run the step in the context of the run, which reports it."""
            return ctx._step(function, title or function.__name__, retries, arguments, named)

        return call

    return wrap


def encode(value: Any) -> dict[str, Any]:
    """Make a result that JSON can hold."""
    if isinstance(value, BaseModel):
        return {"kind": "model", "type": type(value).__name__, "value": value.model_dump(mode="json")}
    if isinstance(value, Document):
        return {"kind": "document", "value": asdict(value)}
    if isinstance(value, Decision):
        return {"kind": "decision", "value": asdict(value)}
    if isinstance(value, (str, int, float, bool, list, dict)) or value is None:
        return {"kind": "json", "value": value}
    return {"kind": "json", "value": str(value)}


def decode(recorded: dict[str, Any], function: Callable[..., Any]) -> Any:
    """Build the result of an activity again from its record."""
    value = recorded["value"]
    if recorded["kind"] == "document":
        return Document(**value)
    if recorded["kind"] == "decision":
        return Decision(**value)
    if recorded["kind"] == "model":
        returns = typing.get_type_hints(function).get("return")
        if isinstance(returns, type) and issubclass(returns, BaseModel):
            return returns.model_validate(value)
    return value


def decided(request: str, key: str) -> dict[str, Any] | None:
    """Return the decision that a request of the transcript holds, when it is the decision at the gate `key`."""
    if not request.startswith(GATE):
        return None
    try:
        made = json.loads(request[len(GATE) :].partition("\n")[0])
    except ValueError:
        return None
    return made if isinstance(made, dict) and made.get("key") == key else None


class Context:
    """The run that a workflow function works in."""

    def __init__(self, run: dict[str, Any], session: Session, api: Durable, fresh: Callable[[], Session] | None = None) -> None:
        """Keep the run, its Vis session and the client of the Durable Lambda API. `fresh` makes a new session for the run."""
        self.run_id, self.input, self.session = run["id"], run["input"], session
        # The number of the next activity, the activity that runs, and its start that the API does not have yet.
        self._api, self._fresh, self._next, self._current, self._start = api, fresh, 0, None, None
        # The transcript of the run: the turns of its session after the turns of a person, and the place of the next request.
        turns, base = session.turns(), run.get("base") or 0
        self._turns, self._at = turns[base:], 0
        # The last turn that a fork of the session keeps.
        self._kept = turns[base - 1]["turn_id"] if base else None
        steps = run.get("activities") or []
        # The activities that the run shows as completed, and the decisions that the transcript does not have yet.
        self._done = {step["seq"]: step["name"] for step in steps if step["status"] == "completed"}
        self._decisions: dict[str, Any] = dict(run.get("decisions") or {})
        # A retried run forks its session where its first failed activity starts.
        self._retry = bool(run.get("retry"))
        self._failed = min((step["seq"] for step in steps if step["status"] == "failed"), default=None)

    def _event(self, kind: str, **fields: Any) -> Any:
        """Report to the API what this run process did."""
        return self._api.event(self.run_id, kind, **fields)

    def _report(self) -> None:
        """Tell the API one time that the activity works."""
        if self._start:
            self._event("activity_started", **self._start)
            self._start = None

    def _leave(self) -> None:
        """Leave the transcript: the run does new work from here. A run that a person retried does it in a fork of its session."""
        del self._turns[self._at :]
        if self._retry:
            self._retry = False
            self._fork()

    def _fork(self) -> None:
        """Continue the run in a fork of its session. The fork has the transcript through the last turn that the run keeps."""
        del self._turns[self._at :]
        if self._kept:
            self.session = self.session.fork(self._kept)
        elif self._fresh:  # the transcript has no turn to keep
            self.session = self._fresh()
        else:
            return
        # A fork gives each turn a new id.
        turns = self.session.turns()
        self._kept = turns[-1]["turn_id"] if turns else None
        self._event("forked", session_id=self.session.id)

    def _step(self, function: Callable[..., Any], title: str, retries: int, arguments: tuple[Any, ...], named: dict[str, Any]) -> Any:
        """Run an activity. The transcript answers the requests that an earlier process of the run made."""
        if self._current is not None:  # an activity in an activity is a plain call
            return function(self, *arguments, **named)
        seq, self._next = self._next, self._next + 1
        self._current, self._start = seq, {"seq": seq, "name": function.__name__, "title": title}
        if self._retry and seq == self._failed:
            self._leave()
        try:
            for left in range(retries, -1, -1):
                # The place in the transcript where the activity starts. A retry forks the session there.
                at, kept = self._at, self._kept
                try:
                    result = function(self, *arguments, **named)
                    break
                except Exception as failure:
                    error = f"{type(failure).__name__}: {failure}"
                    self._report()
                    if not left:
                        self._event("activity_failed", seq=seq, error=error)
                        raise
                    self._event("note", seq=seq, text=f"The activity failed: {error}. It runs again in a fork of the session."[:2000])
                    self._at, self._kept = at, kept
                    self._fork()
        finally:
            self._current = None
        if self._start and self._done.get(seq) == function.__name__:
            self._start = None  # the transcript had each request, and the run shows the result
            return result
        self._report()
        self._event("activity_completed", seq=seq, name=function.__name__, result=encode(result))
        return result

    def _recorded(self, is_it: Callable[[str], bool], timeout: float) -> dict[str, Any] | None:
        """Return the completed turn that the transcript has for the next request, or None.

        A worker stops before the Lambda time limit and parks the running turn. The gateway
        of the next worker continues that turn. Thus the run waits for a turn that did not
        end and sends no second request.
        """
        for at in range(self._at, len(self._turns)):
            turn = self._turns[at]
            if not is_it(turn.get("request") or ""):
                continue
            if turn["status"] not in ENDED:
                turn = self._end(turn["turn_id"], timeout)
            if turn["status"] == "completed":
                self._at, self._kept = at + 1, turn["turn_id"]
                return turn
        return None

    def _end(self, turn_id: str, timeout: float) -> dict[str, Any]:
        """Wait for a turn that an earlier process of the run started, and return its end."""
        end = time.monotonic() + timeout
        while True:
            try:
                return Turn(self.session, turn_id).wait(timeout=max(end - time.monotonic(), 1))
            except GatewayError as failure:  # the new gateway did not take the turn yet
                if failure.code != "turn-not-found" or time.monotonic() > end:
                    raise
            time.sleep(2)

    def _request(self, request: str, timeout: float, model: dict[str, str] | None = None) -> dict[str, Any]:
        """Return the completed turn of a request: the turn of the transcript, or a new turn of the session.

        `model` has the provider and the model of a new turn. Without it, the turn uses the model of the session.
        """
        turn = self._recorded(lambda text: text == request, timeout)
        if turn is None:
            self._leave()
            self._report()
            turn = self.session.send(request, **(model or {})).wait(timeout=timeout)
            if turn.get("status") != "completed":
                raise ActivityError(f"the turn ended with the status {turn.get('status')!r}")
            self._kept = turn["turn_id"]
        return turn

    def ask(self, request: str, model: type[Model] | None = None, *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> Any:
        """Send a request to the session of the run.

        With a Pydantic model, return a validated instance: the session gets the JSON
        Schema of the model, and a correction request for an answer that does not fit.
        Without a model, return the answer text. Each request is one turn of the transcript.

        `llm` names the language model of this request only, and `provider` its provider
        when it is not the provider of the stack. The next request uses the model of
        the session again. A request on another model does not use the prompt cache.
        """
        using = route(llm, provider)
        if model is None:
            return final_answer(self._request(request, timeout, using)) or ""
        form = f"Answer with exactly one JSON value and no other text, without a Markdown fence. The value must match this JSON Schema:\n{json.dumps(model.model_json_schema())}"
        request = f"{request}\n\n{form}"
        for _ in range(CORRECTIONS + 1):
            try:
                return model.model_validate_json(final_answer(self._request(request, timeout, using)) or "")
            except ValidationError as invalid:
                problems = "\n".join(f"- {'.'.join(map(str, problem['loc'])) or '$'}: {problem['msg']}" for problem in invalid.errors(include_url=False, include_input=False))
                request = f"Your answer is not valid:\n{problems}\n\nKeep the completed work. {form}"
        raise ActivityError(f"the session gave no valid {model.__name__}")

    def delegate(self, tasks: list[str | dict[str, Any]], *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> list[str]:
        """Give each task to its own subagent of the session. Return the answer of each subagent, in the order of the tasks.

        A task is a text, or a dict with `task` and optional `title`, `role` (research,
        implement or verify), `model`, `provider`, `iteration_budget` and `files`. A
        subagent has the context of the session and works in the same directory. With
        `model`, a subagent uses that language model.

        The session starts the subagents in one turn with a Python block that this call
        gives. The answers come from the tool output of that turn, not from the text of
        the session, so the transcript has them: a new process of the run and a fork
        start no second subagent. A subagent that does not complete fails the activity.
        `llm` and `provider` select the model of that turn, as in `ask`.
        """
        given = [{"task": task} if isinstance(task, str) else dict(task) for task in tasks]
        for task in given:
            if not isinstance(task.get("task"), str) or set(task) - set(TASK):
                raise ValueError(f"a task is a text, or a dict with `task` and these optional fields: {', '.join(TASK[1:])}")
            if task.get("model") and not task.get("provider"):
                task["provider"] = PROVIDER
        if not given:
            return []
        using = route(llm, provider)
        block = (
            "import json\n"
            f"records = await council.run(json.loads({json.dumps(given)!r}))\n"
            'while any(record["status"] in ("queued", "running") for record in records):\n'
            '    records = await council.wait(session_ids=[record["session_id"] for record in records])\n'
            f'print({DELEGATED!r} + json.dumps([{{key: record.get(key) for key in ("id", "status", "answer", "error")}} for record in records]))'
        )
        how = (
            "Do not do the work of a task yourself. Run exactly this Python block one time, without a change:\n\n"
            f"```python\n{block}\n```\n\n"
            "When a subagent has the status asking, answer its question from its task. Then run the block again from the line with `while`. "
            "Then answer with one short line."
        )
        request = f"Give each task of this Python block to its own subagent. {how}"
        for _ in range(CORRECTIONS + 1):
            records = self._delegated(self._request(request, timeout, using)["turn_id"])
            if records is not None:
                break
            request = f"That turn started no subagent: its tool output has no line that starts with {DELEGATED!r}. {how}"
        else:
            raise ActivityError("the session started no subagent")
        if len(records) != len(given):
            raise ActivityError(f"the session started {len(records)} subagents for {len(given)} tasks")
        for record in records:
            if record.get("status") != "completed":
                raise ActivityError(f"the subagent {record.get('id')} ended with the status {record.get('status')!r}: {record.get('error')}")
        return [record.get("answer") or "" for record in records]

    def _delegated(self, turn_id: str) -> list[dict[str, Any]] | None:
        """Return the records of the subagents that the tool output of a turn shows, in the order of their tasks. None: the turn started no subagent."""
        steps = self.session.client.get_session_turn_trace(self.session.id, turn_id)["iterations"]
        shown = [line for step in steps for form in step.get("forms") or [] for line in (form.get("stdout") or "").splitlines() if line.startswith(DELEGATED)]
        if not shown:
            return None
        try:
            records = json.loads(shown[-1][len(DELEGATED) :])
        except ValueError:
            raise ActivityError("the tool output of the session does not have the complete answers of the subagents") from None
        # The session gives each subagent the next id: a1, a2 and onward.
        return sorted(records, key=lambda record: int("".join(filter(str.isdigit, str(record.get("id")))) or 0))

    def document(self, name: str, request: str, *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> Document:
        """Let the session write a document and attach it.

        The session attaches the file with the native `attach` verb of Vis, so the
        transcript shows it. The SDK gives the files of exactly that turn. A document
        with a name that the session used before becomes the next version of that file.
        `llm` and `provider` select the model of the request, as in `ask`.
        """
        using = route(llm, provider)
        how = f"Write the complete document as Markdown. Attach it with one Python block that calls `attach(content.encode(), {name!r})`. Then answer with a summary of at most three sentences."
        found = self._attached(name, f"{request}\n\n{how}", timeout, using) or self._attached(name, f"That turn attached no file {name!r}. {how}", timeout, using)
        if found is None:
            raise ActivityError(f"the session attached no file {name!r}")
        # This read makes the API copy the file to S3 and show it in the activity.
        self._api.call("GET", f"attached?session={self.session.id}")
        return Document(name, self.session.id, found.size, found.version)

    def _attached(self, name: str, request: str, timeout: float, model: dict[str, str]) -> Attachment | None:
        """Make a request and return the newest file with a name that its turn attached."""
        turn = self._request(request, timeout, model)
        files = [file for file in self.session.attachments(turn["turn_id"]) if file.filename == name]
        return max(files, key=lambda file: file.version, default=None)

    def gate(self, key: str, title: str, *, about: str = "", document: Document | None = None) -> Decision:
        """Wait for the decision of a person: approve or reject, with a comment.

        `title` names the gate. `about` tells the person what to check, and `document`
        is the file to read first. Use each key one time in a run.

        The run process ends while the run waits, so a waiting run holds no worker.
        When the person decides, Durable Lambda starts the run again. This call then
        sends the decision to the session as one turn, so the transcript has it, and
        returns the decision. The workflow selects what a rejection does.
        """
        turn = self._recorded(lambda text: decided(text, key) is not None, TURN_S)
        made = decided(turn["request"], key) if turn else self._decisions.get(key)
        if made is None:
            self._report()
            self._event("waiting", seq=self._current, key=key, title=title, about=about, document=document.name if document else None)
            raise Waiting(key)
        if turn is None:
            line = json.dumps({"key": key, "title": title, "approved": made["approved"], "comment": made["comment"]})
            self._request(f"{GATE}{line}\n\nAnswer with one short line. Do no other work.", TURN_S)
        return Decision(key, bool(made["approved"]), str(made["comment"]))

    def note(self, text: str) -> None:
        """Add a line to the log of the run."""
        self._report()
        self._event("note", text=str(text)[:2000], seq=self._current)
