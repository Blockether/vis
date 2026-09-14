"""One conversation using an injected execution layer or an owned local engine."""

from ._client import ExecutionLayer, Session, TransportError, Turn
from ._local import LocalEngine


class Agent:
    """Run one conversation through an execution layer.

    Agent() uses an owned LocalEngine in the current project. Session access,
    context entry, send() or run() starts it. Closing stops that private process
    and discards its temporary session database, not file edits.

    An explicitly supplied execution_layer is borrowed. The caller owns its
    lifetime; closing an Agent never closes a shared layer or another Agent's
    conversation. Configure launcher, gateway URL, credentials and transport
    timeouts on the layer. Its session_options() defines project path rules.
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
        """Submit a request, returning a Turn for progress, waiting or cancellation.

        Options are passed to Session.send(), including provider, model and
        idempotency_key. Follow-up requests use this agent's existing session.
        """
        conversation = self.session
        self._extensions.started = True
        return conversation.send(request, **options)

    def run(self, request: str, *, timeout=300, **options):
        """Submit and wait, returning the canonical turn record, not a text string.

        Inspect status: failed, cancelled or suspended records are returned too.
        A wait timeout raises VisTimeout without cancelling the turn. Closing
        stops unfinished work only when this Agent owns its default local engine.
        """
        return self.send(request, **options).wait(timeout=timeout)

    def close(self):
        """Detach this Agent's callbacks; close only an owned default layer."""
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
