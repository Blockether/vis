"""One conversation using an injected execution layer or an owned local engine."""

from ._client import ExecutionLayer, Session, TransportError, Turn
from ._local import LocalEngine


class Agent:
    """Run one conversation, with optional application-owned tools.

    Choose this entry point for sequential requests that should share history.
    Use `send` to get a `blockether.vis.engine.Turn` immediately, or `run` to wait
    for its result. Access `session` for transcripts, attachments and progress.

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

    def run(self, request: str, *, timeout=300, **options):
        """Submit a request and return its final or suspended turn record.

        Args:
            request: User message or slash command, as for `send`.
            timeout: Positive finite wait deadline in seconds; defaults to 300.
                This is separate from the layer's transport timeout.
            **options: Submission options forwarded to `send`.

        Returns:
            The canonical turn dictionary, not a string. Inspect `status`:
            `completed`, `failed`, `cancelled`, `suspended` and `error` all end the
            wait. Failed model work is returned as a record, not raised as an
            exception by this method.

        Raises:
            blockether.vis.engine.VisTimeout: The wait deadline expired. The turn
                was not cancelled; use `send` followed by `Turn.wait` when you
                need to retain a handle for cancellation or a later wait.

        Transport and gateway errors also propagate. Closing this agent stops
        unfinished work only when it owns the default local engine.
        """
        return self.send(request, **options).wait(timeout=timeout)

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
