"""One conversation using an injected execution layer or an owned local engine."""

import json
from typing import TypeVar, overload

from jsonschema import Draft202012Validator, ValidationError
from pydantic import BaseModel

from ._client import ExecutionLayer, Session, TransportError, Turn
from ._local import LocalEngine

ResponseModel = TypeVar("ResponseModel", bound=BaseModel)


class StructuredOutputError(ValueError):
    """A completed turn had no valid structured result, or did not complete.

    `turn` retains the canonical record for inspection without printing its
    potentially private content in the exception message. No retry is performed.
    """

    def __init__(self, reason: str, turn: dict):
        self.turn = turn
        super().__init__(
            f"{reason} (turn {turn.get('turn_id', 'unknown')}, "
            f"status {turn.get('status', 'unknown')})"
        )


class Agent:
    """Run one conversation, with optional application-owned tools.

    Choose this entry point for sequential requests that should share history.
    Use `send` to get a `blockether.vis.engine.Turn` immediately, or `run` to wait
    for its result. Pass `response_model` to `run` for a validated Pydantic model.
    Access `session` for transcripts, attachments and progress.

    Args:
        project: Project directory. Local execution resolves an existing directory
            immediately; gateway execution requires an absolute path on the gateway.
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
        """The same conversation for all requests; connects on first access."""
        if self._closed:
            raise TransportError("agent is closed")
        if self._session is None:
            try:
                self.execution_layer.connect()
                self._session = self.execution_layer.create_session(
                    **self._session_options
                )
                self._mount_extensions(self._extensions)
            except BaseException:
                try:
                    self.close()
                except Exception:
                    pass
                raise
        return self._session

    def _mount_extensions(self, extensions):
        if extensions.manifest:
            self.execution_layer._ensure_client_lease()
            self.execution_layer.put_session_client_extensions(
                self._session.id, body={"extensions": extensions.manifest}
            )
        self.execution_layer._client_extensions[self._session.id] = extensions

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
            self._mount_extensions(candidate)
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
        **options,
    ) -> ResponseModel: ...

    def run(
        self,
        request: str,
        *,
        timeout: float = 300,
        response_model: type[ResponseModel] | None = None,
        **options,
    ) -> dict | ResponseModel:
        """Submit a request and wait for its result in the same conversation.

        Without `response_model`, return the canonical turn dictionary. Check its
        `status`: completed, failed, cancelled, suspended and error all end the
        wait; failed model work is returned as a record, not raised.

        With a Pydantic `BaseModel` subclass, include its validation JSON Schema
        in the request and return an instance validated against the schema and
        Python validators. This is a prompt, not provider-enforced JSON mode:
        the final prose block must be one complete JSON value, without fences
        or commentary. Other content block types are ignored. Unsuccessful
        turns and invalid answers raise `StructuredOutputError` with the
        original `turn` record. Validation never retries or undoes file edits.

        `timeout` is a positive finite wait deadline in seconds, separate from
        the layer's transport timeout. Submission options such as `provider`,
        `model` and `attachments` are forwarded unchanged to `send`.
        `VisTimeout` does not cancel the turn; transport errors propagate.
        Closing an owned local agent stops unfinished work.
        """
        if response_model is None:
            return self.send(request, **options).wait(timeout=timeout)

        if not isinstance(response_model, type) or not issubclass(
            response_model, BaseModel
        ):
            raise TypeError("response_model must be a Pydantic BaseModel subclass")
        schema = response_model.model_json_schema(mode="validation")
        Draft202012Validator.check_schema(schema)

        prompt = (
            f"{request}\n\n"
            "Return the final answer as exactly one JSON value matching the schema. "
            "Do not include Markdown fences, commentary, or text outside the JSON. "
            "Match this JSON Schema:\n"
            f"{json.dumps(schema, ensure_ascii=False)}"
        )
        turn = self.send(prompt, **options).wait(timeout=timeout)
        if turn.get("status") != "completed":
            raise StructuredOutputError("turn did not complete", turn)

        content = turn.get("content")
        prose = (
            next(
                (
                    block.get("markdown")
                    for block in reversed(content)
                    if isinstance(block, dict) and block.get("type") == "prose"
                ),
                None,
            )
            if isinstance(content, list)
            else None
        )
        if not isinstance(prose, str) or not prose.strip():
            raise StructuredOutputError("missing final prose answer", turn)

        def reject_constant(value):
            raise ValueError(f"invalid JSON constant {value}")

        try:
            payload = json.loads(prose, parse_constant=reject_constant)
            Draft202012Validator(schema).validate(payload)
            return response_model.model_validate_json(prose)
        except (ValueError, ValidationError) as exc:
            raise StructuredOutputError("invalid structured answer", turn) from exc

    def close(self):
        """Detach callbacks and release only resources owned by this agent.

        Repeated calls are safe. The default local engine is stopped and its
        temporary session database removed; project file edits remain. A borrowed
        execution layer and other agents using it remain open. Requests through
        this agent after closing raise `blockether.vis.engine.TransportError`.
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
