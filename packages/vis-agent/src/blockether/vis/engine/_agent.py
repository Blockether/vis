"""One conversation using an injected execution layer or an owned local engine."""

import json
import math
import re
import time
from functools import lru_cache
from itertools import islice
from typing import Any, TypeVar, overload

from jsonschema import Draft202012Validator, validators
from jsonschema.exceptions import ValidationError, best_match
from pydantic import BaseModel
from pydantic_core import ErrorDetails, SchemaError, SchemaValidator, core_schema
from pydantic_core import ValidationError as PydanticValidationError
from referencing import Registry
from referencing.exceptions import Unresolvable

from ._client import ExecutionLayer, Session, TransportError, Turn, _duration
from ._local import LocalEngine

ResponseModel = TypeVar("ResponseModel", bound=BaseModel)

_PROBLEM_LIMIT = 100
_PROBLEMS_SHOWN = 10
_FIRST_TURN_ONLY = frozenset({"attachments", "idempotency_key"})
_IDENTIFIER = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\Z")


def _shorten(text: str, limit: int = 300) -> str:
    return text if len(text) <= limit else text[: limit - 1] + "…"


def _is_scalar(value: Any) -> bool:
    return value is None or isinstance(value, (bool, int, float, str))


def _brief(value: Any) -> str:
    return _shorten(json.dumps(value, ensure_ascii=False), 80)


def _json_type(value: Any) -> str:
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "boolean"
    if isinstance(value, int):
        return "integer"
    if isinstance(value, float):
        return "number"
    if isinstance(value, str):
        return "string"
    return "array" if isinstance(value, list) else "object"


def _path(parts) -> str:
    text = "$"
    for part in parts:
        if isinstance(part, int) and not isinstance(part, bool):
            text += f"[{part}]"
        elif _IDENTIFIER.match(str(part)):
            text += f".{part}"
        else:
            text += f"[{_brief(str(part))}]"
    return text


def _problem(source: str, parts, message: str) -> dict:
    return {"path": _path(parts), "message": _shorten(message), "source": source}


def _problem_lines(problems: list[dict]) -> str:
    lines = [f"\n- {item['path']}: {item['message']}" for item in problems]
    hidden = len(lines) - _PROBLEMS_SHOWN
    if hidden > 0:
        lines[_PROBLEMS_SHOWN:] = [f"\n- ...and {hidden} more"]
    return "".join(lines)


class StructuredOutputError(ValueError):
    """No valid structured result: a turn did not complete or answers stayed invalid.

    `errors` lists the problems in the last turn as dictionaries. Each has a
    JSONPath-like `path` (`$` is the whole value), a `message` and a `source`. The
    source is `turn`, `answer`, `json`, `schema` or `pydantic`. `attempts` has one
    dictionary per turn. It holds the `turn` record, the final prose `answer` (None when
    absent) and `errors`. `turn` is the last record.

    The message lists the first problems. Messages include short scalar values from the
    answer, but never whole objects or arrays. If the response model sets Pydantic's
    `hide_input_in_errors`, messages include no answer values.
    """

    def __init__(self, reason: str, attempts: list[dict]):
        self.attempts = attempts
        self.turn = attempts[-1]["turn"]
        self.errors = attempts[-1]["errors"]
        super().__init__(
            f"{reason} (turn {self.turn.get('turn_id', 'unknown')}, "
            f"status {self.turn.get('status', 'unknown')})"
            f"{_problem_lines(self.errors)}"
        )


@lru_cache(maxsize=256)
def _regex(pattern: str) -> SchemaValidator:
    """Compile like Pydantic: linear-time Rust regex, else Python `re`."""
    try:
        return SchemaValidator(core_schema.str_schema(pattern=pattern))
    except SchemaError:
        return SchemaValidator(
            core_schema.str_schema(pattern=pattern, regex_engine="python-re")
        )


def _search(pattern: str, text: str) -> bool:
    try:
        _regex(pattern).validate_python(text)
    except PydanticValidationError:
        return False
    return True


def _pattern(validator, pattern, instance, schema):
    if validator.is_type(instance, "string") and not _search(pattern, instance):
        yield ValidationError(f"does not match {pattern!r}")


def _pattern_properties(validator, patterns, instance, schema):
    if validator.is_type(instance, "object"):
        for pattern, subschema in patterns.items():
            for name, value in instance.items():
                if _search(pattern, name):
                    yield from validator.descend(
                        value, subschema, path=name, schema_path=pattern
                    )


_AnswerSchema = validators.extend(
    Draft202012Validator,
    {"pattern": _pattern, "patternProperties": _pattern_properties},
)


def _external_references(node: Any):
    if isinstance(node, dict):
        for key, value in node.items():
            if key in {"$ref", "$dynamicRef"} and isinstance(value, str):
                if not value.startswith("#"):
                    yield value
            else:
                yield from _external_references(value)
    elif isinstance(node, list):
        for value in node:
            yield from _external_references(value)


def _relevant_errors(error: ValidationError):
    """Expand an anyOf or oneOf failure to every error in its closest branch."""
    closest = best_match([error]) if error.context else error
    if closest is error:
        yield error
        return
    while closest.parent is not None and closest.parent is not error:
        closest = closest.parent
    branch = closest.relative_schema_path[0] if closest.relative_schema_path else None
    for child in error.context:
        if child.relative_schema_path and child.relative_schema_path[0] == branch:
            yield from _relevant_errors(child)


def _schema_problems(error: ValidationError, show_input: bool):
    """Describe a JSON Schema error without printing whole objects or arrays."""
    path = tuple(error.absolute_path)
    keyword, expected, value = error.validator, error.validator_value, error.instance
    if keyword == "required" and isinstance(value, dict) and isinstance(expected, list):
        for name in expected:
            if name not in value:
                yield _problem("schema", (*path, name), "required property is missing")
        return
    if keyword in {"additionalProperties", "unevaluatedProperties"}:
        message = error.message
    elif keyword is None:
        message = "no value is allowed here"
    elif keyword == "type":
        types = " or ".join(expected) if isinstance(expected, list) else expected
        message = f"expected {types}, got {_json_type(value)}"
    else:
        message = f"does not satisfy {keyword} {_brief(expected)}"
    if show_input and _is_scalar(value) and keyword is not None:
        message += f" (input: {_brief(value)})"
    yield _problem("schema", path, message)


def _unique(problems) -> list[dict]:
    seen, result = set(), []
    for item in problems:
        key = (item["path"], item["message"])
        if key not in seen:
            seen.add(key)
            result.append(item)
    return result


def _unique_members(pairs):
    members = {}
    for name, value in pairs:
        if name in members:
            raise ValueError(f"object has duplicate member {_brief(name)}")
        members[name] = value
    return members


def _integer(text: str) -> int:
    try:
        return int(text)
    except ValueError:
        raise ValueError(
            f"an integer with {len(text)} characters is too long"
        ) from None


def _reject_constant(name: str):
    raise ValueError(f"{name} is not valid JSON")


def _syntax_message(answer: str, error: json.JSONDecodeError) -> str:
    where = f"line {error.lineno}, column {error.colno}"
    start = len(answer) - len(answer.lstrip())
    if answer.startswith("```", start):
        return (
            "the answer is wrapped in a Markdown code fence; send only the JSON value"
        )
    if error.msg == "Extra data":
        return f"text follows the JSON value at {where}; send exactly one JSON value"
    if error.msg == "Expecting value" and error.pos == start:
        return "the answer starts with text instead of a JSON value"
    return f"not valid JSON: {error.msg} at {where}"


def _final_answer(turn: dict) -> str | None:
    content = turn.get("content")
    if not isinstance(content, list):
        return None
    for block in reversed(content):
        if isinstance(block, dict) and block.get("type") == "prose":
            answer = block.get("markdown")
            return answer if isinstance(answer, str) and answer.strip() else None
    return None


def _turn_problem(turn: dict) -> dict:
    content = turn.get("content")
    detail = next(
        (
            block["message"]
            for block in reversed(content if isinstance(content, list) else [])
            if isinstance(block, dict)
            and block.get("type") == "error"
            and isinstance(block.get("message"), str)
        ),
        turn.get("error") if isinstance(turn.get("error"), str) else None,
    )
    message = f"the turn ended with status {turn.get('status', 'unknown')}"
    return _problem("turn", (), f"{message}: {detail}" if detail else message)


class _ResponseContract:
    """Prompts and validation for one `response_model`."""

    def __init__(self, model: Any):
        if not isinstance(model, type) or not issubclass(model, BaseModel):
            raise TypeError("response_model must be a Pydantic BaseModel subclass")
        schema = model.model_json_schema(mode="validation")
        _AnswerSchema.check_schema(schema)
        for reference in _external_references(schema):
            raise ValueError(
                "response_model JSON Schema must be self-contained; "
                f"unsupported reference {reference!r}"
            )
        self.model = model
        self.schema_text = json.dumps(schema, ensure_ascii=False)
        self.validator = _AnswerSchema(schema, registry=Registry())
        self.show_input = not model.model_config.get("hide_input_in_errors", False)

    def request(self, request: str) -> str:
        return (
            f"{request}\n\n"
            "Return the final answer as exactly one JSON value matching the schema. "
            "Do not include Markdown fences, commentary, or text outside the JSON. "
            f"Match this JSON Schema:\n{self.schema_text}"
        )

    def correction(self, problems: list[dict]) -> str:
        return (
            "Your previous final answer is not a valid structured result "
            f"($ is the whole JSON value):{_problem_lines(problems)}\n\n"
            "Answer the previous request again with exactly one corrected JSON value. "
            "Do not include Markdown fences, commentary, or text outside the JSON. "
            "Keep completed work; repeat tools or file changes only if the correction "
            f"requires them. Match this JSON Schema:\n{self.schema_text}"
        )

    def _finite(self, text: str) -> float:
        value = float(text)
        if not math.isfinite(value):
            shown = f"number {_shorten(text, 40)}" if self.show_input else "a number"
            raise ValueError(f"{shown} is outside the finite JSON number range")
        return value

    def validate(self, answer: str | None) -> tuple[Any, list[dict]]:
        """Return the model instance, or no instance and the answer's problems."""
        if answer is None:
            return None, [_problem("answer", (), "the turn has no final text answer")]
        try:
            payload = json.loads(
                answer,
                object_pairs_hook=_unique_members,
                parse_constant=_reject_constant,
                parse_float=self._finite,
                parse_int=_integer,
            )
            problems = _unique(
                islice(
                    (
                        problem
                        for error in self.validator.iter_errors(payload)
                        for leaf in _relevant_errors(error)
                        for problem in _schema_problems(leaf, self.show_input)
                    ),
                    _PROBLEM_LIMIT,
                )
            )
        except json.JSONDecodeError as error:
            return None, [_problem("json", (), _syntax_message(answer, error))]
        except ValueError as error:
            return None, [_problem("json", (), str(error))]
        except RecursionError:
            return None, [_problem("json", (), "the JSON value is nested too deeply")]
        except Unresolvable as error:
            raise ValueError(
                f"response_model JSON Schema has an unresolvable reference: {error}"
            ) from None
        if problems:
            return None, problems
        try:
            return self.model.model_validate_json(answer), []
        except PydanticValidationError as error:
            details = islice(error.errors(include_url=False), _PROBLEM_LIMIT)
        return None, _unique(
            _problem("pydantic", item["loc"], self._pydantic_message(item))
            for item in details
        )

    def _pydantic_message(self, item: ErrorDetails) -> str:
        value = item.get("input")
        if self.show_input and item["type"] != "missing" and _is_scalar(value):
            return f"{item['msg']} (input: {_brief(value)})"
        return item["msg"]


class Agent:
    """Run one conversation, with optional application-owned tools.

    Choose this entry point for sequential requests that should share history.
    Use `send` to get a `blockether.vis.engine.Turn` immediately, or `run` to wait
    for its result. Pass `response_model` to `run` for a validated Pydantic model.
    Access `session` for transcripts, attachments and progress.

    Args:
        project: Project directory. Local execution resolves an existing directory
            immediately. Gateway execution requires an absolute path on the gateway.
        execution_layer: A `blockether.vis.engine.LocalEngine` or
            `blockether.vis.engine.GatewayClient` to borrow. When omitted, the agent
            creates and owns a local engine. Configure executable, credentials and
            transport timeouts on an explicit layer.
        extensions: Iterable of `blockether.vis.extension.Extension` declarations.
            Their Python callables stay in your application and execute on the
            calling thread while you read, wait or iterate events.

    Construction does not start a process or connect. Session access, context
    entry, `send` or `run` starts the conversation. Closing the default agent stops
    its private process and discards its temporary session database, not file edits.
    An explicitly supplied layer is borrowed: you must close it yourself, after
    all agents using it have finished. An agent cannot be reused after `close`.

    Raises:
        TypeError: The supplied layer does not implement
            `blockether.vis.engine.ExecutionLayer`, or an extension is unsupported.
        ValueError: A project or extension declaration is invalid.
        FileNotFoundError: The local project does not exist.
        NotADirectoryError: The local project is not a directory.
    """

    def __init__(
        self,
        project=".",
        *,
        execution_layer: ExecutionLayer | None = None,
        extensions=(),
    ):
        from ._extensions import ClientExtensions

        self._extensions = ClientExtensions(extensions)
        if execution_layer is not None and not isinstance(
            execution_layer, ExecutionLayer
        ):
            raise TypeError("execution_layer must be an ExecutionLayer")
        self._owns_execution_layer = execution_layer is None
        self.execution_layer = (
            LocalEngine(root=project) if execution_layer is None else execution_layer
        )
        self._session_options = self.execution_layer.session_options(project)
        self.project = self._session_options["root"]
        self._session = None
        self._closed = False

    @property
    def session(self) -> Session:
        """The same conversation for all requests. It connects on first access."""
        if self._closed:
            raise TransportError("agent is closed")
        if self._session is None:
            try:
                self.execution_layer.connect()
                self._session = self.execution_layer.create_session(
                    **self._session_options
                )
                self._mount_extensions(self._session, self._extensions)
            except BaseException:
                try:
                    self.close()
                except Exception:
                    pass
                raise
        return self._session

    def _mount_extensions(self, session: Session, extensions):
        if extensions.manifest:
            self.execution_layer._ensure_client_lease()
            self.execution_layer.put_session_client_extensions(
                session.id, body={"extensions": extensions.manifest}
            )
        self.execution_layer._client_extensions[session.id] = extensions

    def register_extension(self, extension):
        """Add an application-owned Extension before this Agent's first request.

        Callables keep their Python objects and execute on the SDK calling thread
        while you read, wait or iterate events. No code or closures are uploaded.
        Host-only extension fields are refused before connecting.
        """
        from ._extensions import ClientExtensions

        if self._closed:
            raise TransportError("agent is closed")
        if self._extensions.started:
            raise RuntimeError("register extensions before the Agent's first request")
        candidate = ClientExtensions((*self._extensions.declarations, extension))
        if self._session is not None:
            self._mount_extensions(self._session, candidate)
        self._extensions = candidate

    def send(self, request: str, **options) -> Turn:
        """Submit a request without waiting for the model to finish.

        Args:
            request: User message or an explicit slash command.
            **options: Forwarded to `blockether.vis.engine.Session.send`, including
                `provider`, `model`, `attachments` and `idempotency_key`.

        Returns:
            A `blockether.vis.engine.Turn` bound to this conversation. Call its
            `read`, `wait` or `cancel` method to track or control this request.
            Further calls reuse the same conversation and its history.

        Submission can make network calls and raise transport or gateway errors.
        No mutation is automatically retried. If you retry an uncertain submission,
        reuse an explicit idempotency key rather than accidentally starting it twice.
        """
        conversation = self.session
        self._extensions.started = True
        return conversation.send(request, **options)

    @overload
    def run(
        self,
        request: str,
        *,
        timeout: float = 300,
        response_model: None = None,
        **options,
    ) -> dict: ...

    @overload
    def run(
        self,
        request: str,
        *,
        response_model: type[ResponseModel],
        timeout: float = 300,
        max_corrections: int = 2,
        **options,
    ) -> ResponseModel: ...

    def run(
        self,
        request: str,
        *,
        timeout: float = 300,
        response_model: type[ResponseModel] | None = None,
        max_corrections: int = 2,
        **options,
    ) -> dict | ResponseModel:
        """Submit a request and wait for its result in the same conversation.

        Without `response_model`, return the turn dictionary in contract form. Check its
        `status`. Completed, failed, cancelled, suspended and error all end the wait.
        Failed model work is returned as a record, not raised.

        With a Pydantic `BaseModel` subclass, the request includes its validation JSON
        Schema. The answer is validated against that schema, then by Pydantic with your
        Python validators. The call returns the validated instance.

        This is a prompt, not provider-enforced JSON mode. The final prose block must be
        one complete JSON value, without fences or commentary. Other content block types
        are ignored. Duplicate object members and non-finite numbers are invalid.

        An invalid answer starts a correction turn in the same conversation. The
        correction lists each problem and repeats the schema. This repeats up to
        `max_corrections` times, and 0 disables corrections. Each correction is another
        model call. The agent is asked to keep completed work, but tool calls and file
        edits are never undone.

        `StructuredOutputError` reports an unsuccessful turn, an answer that is still
        invalid, or a deadline that left no time for a correction. Its `errors` and
        `attempts` say what went wrong.

        `timeout` is a positive finite wait deadline in seconds for the answer and any
        corrections. It is separate from the layer's transport timeout. Submission
        options such as `provider`, `model` and `attachments` are passed unchanged to
        `send`. Corrections reuse them, except `attachments` and `idempotency_key`.

        `VisTimeout` does not cancel the turn, and transport errors propagate. Closing
        an owned local agent stops unfinished work.
        """
        if response_model is None:
            return self.send(request, **options).wait(timeout=timeout)

        contract = _ResponseContract(response_model)
        if isinstance(max_corrections, bool) or not isinstance(max_corrections, int):
            raise TypeError("max_corrections must be an integer")
        if max_corrections < 0:
            raise ValueError("max_corrections must not be negative")
        wait = _duration(timeout)
        follow_up = {
            name: value
            for name, value in options.items()
            if name not in _FIRST_TURN_ONLY
        }
        name = response_model.__name__
        attempts: list[dict] = []
        pending = self.send(contract.request(request), **options)
        deadline = time.monotonic() + wait
        while True:
            turn = pending.wait(timeout=max(deadline - time.monotonic(), 0.001))
            if turn.get("status") != "completed":
                attempts.append(
                    {"turn": turn, "answer": None, "errors": [_turn_problem(turn)]}
                )
                raise StructuredOutputError(f"{name} turn did not complete", attempts)
            answer = _final_answer(turn)
            result, problems = contract.validate(answer)
            if not problems:
                return result
            attempts.append({"turn": turn, "answer": answer, "errors": problems})
            if len(attempts) > max_corrections:
                count = len(attempts)
                raise StructuredOutputError(
                    f"{name} answer is invalid after {count} "
                    f"attempt{'s' if count > 1 else ''}",
                    attempts,
                )
            if deadline <= time.monotonic():
                raise StructuredOutputError(
                    f"{name} answer is invalid and the timeout left no time "
                    "for a correction",
                    attempts,
                )
            pending = self.send(contract.correction(problems), **follow_up)

    def close(self):
        """Detach callbacks and release only resources owned by this agent.

        Repeated calls are safe. The default local engine stops, and its temporary
        session database is removed. Project file edits stay. A borrowed execution layer
        and other agents that use it stay open. After closing, requests through this
        agent raise `blockether.vis.engine.TransportError`.
        """
        if not self._closed:
            self._closed = True
            try:
                if self._session is not None:
                    for stream in tuple(self.execution_layer._streams):
                        if getattr(stream, "session", None) == self._session:
                            stream.close()
                    if self._extensions.manifest and not self.execution_layer._closed:
                        self.execution_layer.delete_session_client_extensions(
                            self._session.id
                        )
            finally:
                if self._session is not None:
                    self.execution_layer._client_extensions.pop(self._session.id, None)
                self._extensions.clear()
                if self._owns_execution_layer:
                    self.execution_layer.close()

    def __enter__(self):
        _ = self.session
        return self

    def __exit__(self, *_):
        self.close()
