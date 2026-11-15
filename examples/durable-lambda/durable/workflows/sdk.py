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

A workflow can have middleware, as a Ring handler has: `@workflow(activities=[...],
requests=[...])`. A middleware of `activities` is `(ctx, step, call)` around each
activity, and a middleware of `requests` is `(ctx, request, send)` around each request
of an activity. An activity still returns its typed result.

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
from collections.abc import Callable, Mapping, Sequence
from dataclasses import asdict, dataclass, field
from typing import Concatenate, Literal, NotRequired, ParamSpec, Protocol, TypedDict, TypeVar, cast, overload

from blockether.vis.engine import Attachment, GatewayError, JSONValue, Session, Turn, final_answer
from pydantic import BaseModel, ValidationError

from durable.settings import CORRECTIONS, PROVIDER, TURN_S
from durable.workflows.client import Durable

Model = TypeVar("Model", bound=BaseModel)
# The arguments of an activity after its context, and its result.
Given = ParamSpec("Given")
Result = TypeVar("Result")
# A workflow function. Its first argument is the context of the run.
Entry = TypeVar("Entry", bound="Callable[Concatenate[Context, ...], object]")
# The statuses of a turn that does not run any more.
ENDED = ("completed", "failed", "cancelled", "suspended", "error")
# The start of the output line with which the session of a run shows the records of its subagents. A JSON list follows.
DELEGATED = "DELEGATED "
# The fields of a subagent record in that line. A session reads the last two to answer a subagent that asks.
SHOWN = ("id", "status", "answer", "error", "question", "question_entry_id")
# The start of the request that puts the decision of a person in the transcript. A JSON object follows on the same line.
GATE = "The decision of a person at a gate of this workflow run: "

__all__ = ["ActivityError", "Around", "Context", "Decision", "Document", "Files", "Reply", "Request", "Role", "Send", "Step", "Task", "Through", "Waiting", "activity", "workflow"]


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


# What a subagent does. The gateway refuses another role.
Role = Literal["research", "implement", "verify"]


class Files(TypedDict, total=False):
    """The file rules of a subagent: the paths or globs that it can write, can only read and cannot use."""

    write: list[str]
    read_only: list[str]
    deny: list[str]


class Task(TypedDict):
    """One task of `ctx.delegate`. Its fields are the fields of a task of `council.run` in a session."""

    task: str
    title: NotRequired[str]
    role: NotRequired[Role]
    model: NotRequired[str]
    provider: NotRequired[str]
    iteration_budget: NotRequired[int]
    files: NotRequired[Files]


# The names of the fields of a task. The text of the task is the first one.
TASK = tuple(Task.__annotations__)


class Subagent(TypedDict):
    """The record of a subagent, as the session of a run prints it."""

    id: str
    status: str
    answer: str | None
    error: object


class Recorded(TypedDict):
    """An activity of a run, as the run shows it."""

    seq: int
    name: str
    status: str


class Made(TypedDict):
    """The decision of a person at a gate, and the document that the gate showed: its name, its version and its size."""

    approved: bool
    comment: str
    document: NotRequired[str | None]
    version: NotRequired[int | None]
    size: NotRequired[int | None]


class Run(TypedDict):
    """The record of a run that the API gives to its process."""

    id: str
    workflow: str
    source: str
    title: str
    input: dict[str, JSONValue]
    session_id: NotRequired[str | None]
    base: NotRequired[int | None]
    activities: NotRequired[list[Recorded]]
    decisions: NotRequired[dict[str, Made]]
    retry: NotRequired[bool]


class TurnRow(TypedDict):
    """A turn of a session, as the Vis SDK gives it."""

    turn_id: str
    status: str
    request: NotRequired[str | None]
    content: NotRequired[list[dict[str, JSONValue]]]


class Started(TypedDict):
    """The start of an activity, as the run process reports it."""

    seq: int
    name: str
    title: str


class Encoded(TypedDict):
    """A result of an activity or of a run, in a form that JSON can hold."""

    kind: Literal["model", "document", "decision", "json"]
    value: JSONValue
    type: NotRequired[str]


@dataclass(frozen=True)
class Step:
    """One activity of a run, as its middleware gets it: its number in the run, its function name, its title and its arguments."""

    seq: int
    name: str
    title: str
    arguments: tuple[object, ...] = ()
    named: Mapping[str, object] = field(default_factory=dict[str, object])


@dataclass(frozen=True)
class Request:
    """One request to the session of a run, as its middleware gets it. Without `llm`, the turn uses the model of the session."""

    text: str
    llm: str | None = None
    provider: str | None = None
    timeout: float = TURN_S


@dataclass(frozen=True)
class Reply:
    """The answer text of a completed turn, and the id of that turn."""

    text: str
    turn_id: str


# The next function of a request middleware: it sends the request and returns the answer of its turn.
Send = Callable[[Request], Reply]
# A middleware around each request: `(ctx, request, send)`.
Through = Callable[["Context", Request, Send], Reply]


class Around(Protocol):
    """A middleware around each activity: `(ctx, step, call)`.

    `call()` runs the activity and returns its result. The middleware returns a result
    of the same type, so an activity keeps its result type in each middleware.
    """

    def __call__(self, ctx: Context, step: Step, call: Callable[[], Result], /) -> Result:
        """Run the activity with `call()` and return its result."""
        ...


@dataclass(frozen=True)
class Marks:
    """What `@workflow` says about a workflow function."""

    title: str
    description: str
    model: str | None
    activities: tuple[Around, ...]
    requests: tuple[Through, ...]


def marked(function: object) -> Marks | None:
    """Return what `@workflow` says about a function, or None for a function without `@workflow`."""
    found = getattr(function, "__workflow__", None)
    return found if isinstance(found, Marks) else None


def within(ctx: Context, through: Through, send: Send) -> Send:
    """Return `send` in the request middleware `through`."""
    return lambda request: through(ctx, request, send)


def inside(ctx: Context, around: Around, step: Step, call: Callable[[], Result]) -> Callable[[], Result]:
    """Return `call` in the activity middleware `around`."""
    return lambda: around(ctx, step, call)


def entries(value: object) -> dict[str, object] | None:
    """Return a JSON object as a dict with text keys, or None for another value."""
    return cast("dict[str, object]", value) if isinstance(value, dict) else None


def printed(trace: JSONValue) -> list[str] | None:
    """Return each line that the tools of a turn printed, from the trace of the turn. None: the gateway gave no trace."""
    steps = trace.get("iterations") if isinstance(trace, dict) else None
    if not isinstance(steps, list):
        return None
    lines: list[str] = []
    for step in steps:
        forms = step.get("forms") if isinstance(step, dict) else None
        for form in forms if isinstance(forms, list) else []:
            output = form.get("stdout") if isinstance(form, dict) else None
            if isinstance(output, str):
                lines += output.splitlines()
    return lines


def is_task(task: Mapping[str, object]) -> bool:
    """Tell if a dict has the text of a task and only the fields of a task. A workflow file can give a dict that no type checker saw."""
    return isinstance(task.get("task"), str) and not set(task) - set(TASK)


def subagent(item: dict[str, object]) -> Subagent:
    """Return the record of a subagent from one object of the line that the session printed."""
    answer = item.get("answer")
    return Subagent(id=str(item.get("id")), status=str(item.get("status")), answer=answer if isinstance(answer, str) else None, error=item.get("error"))


def route(llm: str | None, provider: str | None) -> dict[str, str]:
    """Return the turn fields that select the model `llm` for one request. Without a provider, it is the provider of the stack."""
    if llm is None:
        if provider is not None:
            raise ValueError("give `llm` with `provider`")
        return {}
    return {"provider": provider or PROVIDER, "model": llm}


def workflow(title: str = "", description: str = "", model: str | None = None, *, activities: Sequence[Around] = (), requests: Sequence[Through] = ()) -> Callable[[Entry], Entry]:
    """Mark the function that a run calls. `model` selects the model of the session.

    `activities` and `requests` are the middleware of the run. A middleware of
    `activities` is `(ctx, step, call)`: it gets each activity as a `Step`, and `call()`
    runs the activity and returns its result. A middleware of `requests` is
    `(ctx, request, send)`: it gets each request of an activity as a `Request`, and
    `send(request)` returns the `Reply` of its turn. The first middleware of a list is
    the outer one.

    A middleware runs again in each new process of the run, as the code of an activity
    does. The transcript knows a request by the text that the last middleware sends.
    Thus a middleware that changes the text must give the same text each time.
    """

    def mark(function: Entry) -> Entry:
        """Record the title, the description, the model and the middleware on the function."""
        function.__workflow__ = Marks(title or function.__name__, description, model, tuple(activities), tuple(requests))  # type: ignore[attr-defined]
        return function

    return mark


def activity(title: str = "", retries: int = 0) -> Callable[[Callable[Concatenate[Context, Given], Result]], Callable[Concatenate[Context, Given], Result]]:
    """Make a function a step of a run. Its first argument is the context of the run. The step has the arguments and the result type of the function.

    No activity runs again by itself. With `retries`, a failed activity runs again at
    most that number of times. Each retry forks the session: the fork has the
    transcript before the activity, and the run continues in the fork.
    """

    def wrap(function: Callable[Concatenate[Context, Given], Result]) -> Callable[Concatenate[Context, Given], Result]:
        """Return the function as a step of the run."""

        @functools.wraps(function)
        def call(ctx: Context, *arguments: Given.args, **named: Given.kwargs) -> Result:
            """Run the step in the context of the run, which reports it."""
            return ctx._step(function, title or function.__name__, retries, *arguments, **named)

        return call

    return wrap


def encode(value: object) -> Encoded:
    """Make a result that JSON can hold."""
    if isinstance(value, BaseModel):
        return {"kind": "model", "type": type(value).__name__, "value": value.model_dump(mode="json")}
    if isinstance(value, Document):
        return {"kind": "document", "value": asdict(value)}
    if isinstance(value, Decision):
        return {"kind": "decision", "value": asdict(value)}
    if isinstance(value, (str, int, float, bool, list, dict)) or value is None:
        return {"kind": "json", "value": cast("JSONValue", value)}
    return {"kind": "json", "value": str(value)}


def decided(request: str, key: str) -> Made | None:
    """Return the decision that a request of the transcript holds, when it is the decision at the gate `key`."""
    if not request.startswith(GATE):
        return None
    try:
        made = entries(json.loads(request[len(GATE) :].partition("\n")[0]))
    except ValueError:
        return None
    if made is None or made.get("key") != key:
        return None
    return Made(approved=bool(made.get("approved")), comment=str(made.get("comment") or ""))


class Context:
    """The run that a workflow function works in."""

    def __init__(self, run: Run, session: Session, api: Durable, fresh: Callable[[], Session] | None = None, activities: Sequence[Around] = (), requests: Sequence[Through] = ()) -> None:
        """Keep the run, its Vis session and the client of the Durable Lambda API.

        `fresh` makes a new session for the run. `activities` and `requests` are the middleware of the workflow.
        """
        self.run_id, self.input, self.session = run["id"], run["input"], session
        # The activity that runs, for a middleware. Between two activities, it is None.
        self.step: Step | None = None
        self._around, self._through = tuple(activities), tuple(requests)
        # The number of the next activity, the activity that runs, and its start that the API does not have yet.
        self._api, self._fresh, self._next = api, fresh, 0
        self._current: int | None = None
        self._start: Started | None = None
        # The transcript of the run: the turns of its session after the turns of a person, and the place of the next request.
        turns: list[TurnRow] = session.turns()
        base = run.get("base") or 0
        self._turns, self._at = turns[base:], 0
        # The last turn that a fork of the session keeps.
        self._kept: str | None = turns[base - 1]["turn_id"] if base else None
        steps = run.get("activities") or []
        # The activities that the run shows as completed, and the decisions that the transcript does not have yet.
        self._done = {step["seq"]: step["name"] for step in steps if step["status"] == "completed"}
        self._decisions: dict[str, Made] = dict(run.get("decisions") or {})
        # A retried run forks its session where its first failed activity starts.
        self._retry = bool(run.get("retry"))
        self._failed = min((step["seq"] for step in steps if step["status"] == "failed"), default=None)

    def _event(self, kind: str, **fields: object) -> None:
        """Report to the API what this run process did."""
        self._api.event(self.run_id, kind, **fields)

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
        turns: list[TurnRow] = self.session.turns()
        self._kept = turns[-1]["turn_id"] if turns else None
        self._event("forked", session_id=self.session.id)

    def _step(self, function: Callable[Concatenate[Context, Given], Result], title: str, retries: int, /, *arguments: Given.args, **named: Given.kwargs) -> Result:
        """Run an activity. The transcript answers the requests that an earlier process of the run made."""
        if self._current is not None:  # an activity in an activity is a plain call
            return function(self, *arguments, **named)
        seq, self._next = self._next, self._next + 1
        self._current, self._start = seq, {"seq": seq, "name": function.__name__, "title": title}
        self.step = Step(seq, function.__name__, title, arguments, named)
        # The activity in the middleware of the workflow. The first middleware is the outer one.
        call: Callable[[], Result] = functools.partial(function, self, *arguments, **named)
        for around in reversed(self._around):
            call = inside(self, around, self.step, call)
        if self._retry and seq == self._failed:
            self._leave()
        try:
            left = max(retries, 0)
            while True:
                # The place in the transcript where the activity starts. A retry forks the session there.
                at, kept = self._at, self._kept
                try:
                    result = call()
                    break
                except Exception as failure:
                    error = f"{type(failure).__name__}: {failure}"
                    self._report()
                    if not left:
                        self._event("activity_failed", seq=seq, error=error)
                        raise
                    self._event("note", seq=seq, text=f"The activity failed: {error}. It runs again in a fork of the session."[:2000])
                    left, self._at, self._kept = left - 1, at, kept
                    self._fork()
        finally:
            self._current = self.step = None
        if self._start and self._done.get(seq) == function.__name__:
            self._start = None  # the transcript had each request, and the run shows the result
            return result
        self._report()
        self._event("activity_completed", seq=seq, name=function.__name__, result=encode(result))
        return result

    def _recorded(self, is_it: Callable[[str], bool], timeout: float) -> TurnRow | None:
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

    def _end(self, turn_id: str, timeout: float) -> TurnRow:
        """Wait for a turn that an earlier process of the run started, and return its end."""
        end = time.monotonic() + timeout
        while True:
            try:
                ended: TurnRow = Turn(self.session, turn_id).wait(timeout=max(end - time.monotonic(), 1))
                return ended
            except GatewayError as failure:  # the new gateway did not take the turn yet
                if failure.code != "turn-not-found" or time.monotonic() > end:
                    raise
            time.sleep(2)

    def _send(self, request: Request) -> Reply:
        """Return the answer of a request: of its turn in the transcript, or of a new turn of the session."""
        using = route(request.llm, request.provider)
        turn = self._recorded(lambda text: text == request.text, request.timeout)
        if turn is None:
            self._leave()
            self._report()
            sent: TurnRow = self.session.send(request.text, **using).wait(timeout=request.timeout)
            if sent["status"] != "completed":
                raise ActivityError(f"the turn ended with the status {sent['status']!r}")
            turn, self._kept = sent, sent["turn_id"]
        return Reply(final_answer(dict(turn)) or "", turn["turn_id"])

    def _request(self, text: str, timeout: float, llm: str | None = None, provider: str | None = None) -> Reply:
        """Send a request of an activity through the middleware of the workflow. The first middleware is the outer one."""
        send: Send = self._send
        for through in reversed(self._through):
            send = within(self, through, send)
        return send(Request(text, llm, provider, timeout))

    @overload
    def ask(self, request: str, model: None = None, *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> str: ...

    @overload
    def ask(self, request: str, model: type[Model], *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> Model: ...

    def ask(self, request: str, model: type[Model] | None = None, *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> str | Model:
        """Send a request to the session of the run.

        With a Pydantic model, return a validated instance: the session gets the JSON
        Schema of the model, and a correction request for an answer that does not fit.
        Without a model, return the answer text. Each request is one turn of the transcript.

        `llm` names the language model of this request only, and `provider` its provider
        when it is not the provider of the stack. The next request uses the model of
        the session again. A request on another model does not use the prompt cache.
        """
        if model is None:
            return self._request(request, timeout, llm, provider).text
        form = f"Answer with exactly one JSON value and no other text, without a Markdown fence. The value must match this JSON Schema:\n{json.dumps(model.model_json_schema())}"
        request = f"{request}\n\n{form}"
        for _ in range(CORRECTIONS + 1):
            try:
                return model.model_validate_json(self._request(request, timeout, llm, provider).text)
            except ValidationError as invalid:
                problems = "\n".join(f"- {'.'.join(map(str, problem['loc'])) or '$'}: {problem['msg']}" for problem in invalid.errors(include_url=False, include_input=False))
                request = f"Your answer is not valid:\n{problems}\n\nKeep the completed work. {form}"
        raise ActivityError(f"the session gave no valid {model.__name__}")

    def delegate(self, tasks: Sequence[str | Task], *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> list[str]:
        """Give each task to its own subagent of the session. Return the answer of each subagent, in the order of the tasks.

        A task is a text, or a `Task`: a dict with `task` and optional `title`, `role`
        (research, implement or verify), `model`, `provider`, `iteration_budget` and
        `files`. A subagent has the context of the session and works in the same
        directory. With `model`, a subagent uses that language model.

        The session starts the subagents in one turn with a Python block that this call
        gives. The answers come from the tool output of that turn, not from the text of
        the session, so the transcript has them: a new process of the run and a fork
        start no second subagent. A subagent that does not complete fails the activity.
        `llm` and `provider` select the model of that turn, as in `ask`.
        """
        given = [Task(task=task) if isinstance(task, str) else task.copy() for task in tasks]
        for task in given:
            if not is_task(task):
                raise ValueError(f"a task is a text, or a dict with `task` and these optional fields: {', '.join(TASK[1:])}")
            if task.get("provider") and not task.get("model"):
                raise ValueError("give `model` with `provider` in a task")
            if task.get("model") and not task.get("provider"):
                task["provider"] = PROVIDER
        if not given:
            return []
        # A backtick in a task must not end the code block of the request.
        listed = json.dumps(given).replace("`", "\\u0060")
        block = (
            "import json\n\n\n"
            "async def settle(records):\n"
            '    """Wait until each subagent ends or one of them asks. Print the records."""\n'
            '    while any(record["status"] in ("queued", "running") for record in records) and not any(record["status"] in ("asking", "held") for record in records):\n'
            '        records = await council.wait(session_ids=[record["session_id"] for record in records])\n'
            f"    print({DELEGATED!r} + json.dumps([{{key: record.get(key) for key in {SHOWN!r}}} for record in records]))\n"
            "    return records\n\n\n"
            f"records = await settle(await council.run(json.loads({listed!r})))"
        )
        again = 'records = await settle(await council.wait(session_ids=[record["session_id"] for record in records]))'
        how = (
            "Do not do the work of a task yourself. Run exactly this Python block one time, without a change:\n\n"
            f"```python\n{block}\n```\n\n"
            f"When the printed line shows a subagent with the status asking, answer it from its task. Then run `{again}`. Do that until no subagent asks. "
            "Then answer with one short line."
        )
        request = f"Give each task of this Python block to its own subagent. {how}"
        for _ in range(CORRECTIONS + 1):
            records = self._delegated(self._request(request, timeout, llm, provider).turn_id)
            if records is not None:
                break
            request = f"That turn started no subagent: its tool output has no line that starts with {DELEGATED!r}. {how}"
        else:
            raise ActivityError("the session started no subagent")
        if len(records) != len(given):
            raise ActivityError(f"the session started {len(records)} subagents for {len(given)} tasks")
        for record in records:
            if record["status"] != "completed":
                raise ActivityError(f"the subagent {record['id']} ended with the status {record['status']!r}: {record['error']}")
        return [record["answer"] or "" for record in records]

    def _delegated(self, turn_id: str) -> list[Subagent] | None:
        """Return the records of the subagents that the tool output of a turn shows, in the order of their tasks. None: the turn started no subagent."""
        lines = printed(self.session.client.get_session_turn_trace(self.session.id, turn_id))
        if lines is None:  # without the trace, a second request can start each subagent again
            raise ActivityError("the gateway gave no trace of the turn")
        shown = [line for line in lines if line.startswith(DELEGATED)]
        if not shown:
            return None
        try:
            found: object = json.loads(shown[-1][len(DELEGATED) :])
        except ValueError:
            found = None
        # The line is complete when it is a JSON list of objects.
        items = [entries(item) for item in cast("list[object]", found)] if isinstance(found, list) else [None]
        records = [subagent(item) for item in items if item is not None]
        if len(records) != len(items):
            raise ActivityError("the tool output of the session does not have the complete answers of the subagents")
        # The session gives each subagent the next id: a1, a2 and onward.
        return sorted(records, key=lambda record: int("".join(filter(str.isdigit, record["id"])) or 0))

    def document(self, name: str, request: str, *, timeout: float = TURN_S, llm: str | None = None, provider: str | None = None) -> Document:
        """Let the session write a document and attach it.

        The session attaches the file with the native `attach` verb of Vis, so the
        transcript shows it. The SDK gives the files of exactly that turn. A document
        with a name that the session used before becomes the next version of that file.
        `llm` and `provider` select the model of the request, as in `ask`.
        """
        how = f"Write the complete document as Markdown. Attach it with one Python block that calls `attach(content.encode(), {name!r})`. Then answer with a summary of at most three sentences."
        found = self._attached(name, Request(f"{request}\n\n{how}", llm, provider, timeout)) or self._attached(name, Request(f"That turn attached no file {name!r}. {how}", llm, provider, timeout))
        if found is None:
            raise ActivityError(f"the session attached no file {name!r}")
        # This read makes the API copy the file to S3 and show it in the activity.
        self._api.call("GET", f"attached?session={self.session.id}")
        return Document(name, self.session.id, found.size, found.version)

    def _attached(self, name: str, request: Request) -> Attachment | None:
        """Make a request and return the newest file with a name that its turn attached."""
        reply = self._request(request.text, request.timeout, request.llm, request.provider)
        files = [file for file in self.session.attachments(reply.turn_id) if file.filename == name]
        return max(files, key=lambda file: file.version, default=None)

    def gate(self, key: str, title: str, *, about: str = "", document: Document | None = None) -> Decision:
        """Wait for the decision of a person: approve or reject, with a comment.

        `title` names the gate. `about` tells the person what to check, and `document`
        is the file to read first. Use each key one time in a run.

        The run process ends while the run waits, so a waiting run holds no worker.
        When the person decides, Durable Lambda starts the run again. This call then
        sends the decision to the session as one turn, so the transcript has it, and
        returns the decision. The workflow selects what a rejection does.

        A decision is for the document that the person read. When the run has another
        version of the document now, the decision does not apply and the run waits again.
        """
        turn = self._recorded(lambda text: decided(text, key) is not None, TURN_S)
        made = decided(turn.get("request") or "", key) if turn else self._decisions.get(key)
        shown = (document.name, document.version, document.size) if document else (None, None, None)
        # The transcript does not have the decision yet: the run applies it only to the document that the gate showed.
        if turn is None and made is not None and (made.get("document"), made.get("version"), made.get("size")) != shown:
            self.note("The decision at this gate was for another version of its document. The run waits for a new decision.")
            made = None
        if made is None:
            self._report()
            self._event("waiting", seq=self._current, key=key, title=title, about=about, document=shown[0], version=shown[1], size=shown[2])
            raise Waiting(key)
        if turn is None:
            line = json.dumps({"key": key, "title": title, "approved": made["approved"], "comment": made["comment"]})
            # The decision is not a request of an activity: it does not go through the middleware.
            self._send(Request(f"{GATE}{line}\n\nAnswer with one short line. Do no other work."))
        return Decision(key, made["approved"], made["comment"])

    def note(self, text: str) -> None:
        """Add a line to the log of the run."""
        # The run has the activity that it shows as completed: a new process reports it again only for new work.
        if not (self._start and self._done.get(self._start["seq"]) == self._start["name"]):
            self._report()
        self._event("note", text=str(text)[:2000], seq=self._current)
