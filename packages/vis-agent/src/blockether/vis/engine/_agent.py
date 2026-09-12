"""One project-local conversation backed by an owned engine process."""

from pathlib import Path

from ._client import Session, TransportError, Turn
from ._local import LocalEngine


class Agent:
    """Run tasks in a project, defaulting to the current directory.

    Construction resolves the existing project directory but starts nothing.
    Context entry, send(), run() or session starts a private LocalEngine and one
    conversation. Use a context manager or close() to stop it and discard its
    temporary session database. File edits are not undone; environment, provider
    credentials and extension configuration are inherited. Linux/macOS only.

    executable is the installed launcher (default: vis-agent on PATH), or an argv
    sequence. Use an absolute path for a launcher outside PATH. timeout bounds
    transport operations; startup_timeout bounds boot. Instances use one calling
    thread. Use GatewayClient directly for shared or remote sessions.
    """

    def __init__(
        self, project=".", *, executable="vis-agent", timeout=30, startup_timeout=120
    ):
        self.project = Path(project).resolve(strict=True)
        if not self.project.is_dir():
            raise NotADirectoryError(str(self.project))
        self._engine = LocalEngine(
            executable=executable,
            root=self.project,
            timeout=timeout,
            startup_timeout=startup_timeout,
        )
        self._session = None
        self._closed = False

    @property
    def session(self) -> Session:
        """The same conversation for all requests; starts the engine on first access."""
        if self._closed:
            raise TransportError("agent is closed")
        if self._session is None:
            try:
                self._engine.connect()
                self._session = self._engine.create_session(root=str(self.project))
            except BaseException:
                self.close()
                raise
        return self._session

    def send(self, request: str, **options) -> Turn:
        """Submit a request, returning a Turn for progress, waiting or cancellation.

        Options are passed to Session.send(), including provider, model and
        idempotency_key. Follow-up requests use this agent's existing session.
        """
        return self.session.send(request, **options)

    def run(self, request: str, *, timeout=300, **options):
        """Submit and wait, returning the canonical turn record, not a text string.

        Inspect status: failed, cancelled or suspended records are returned too.
        A wait timeout raises VisTimeout without cancelling the turn. Context
        exit still closes this agent's engine, including any unfinished work.
        """
        return self.send(request, **options).wait(timeout=timeout)

    def close(self):
        """Stop the owned engine and discard its session database; safe to repeat."""
        if not self._closed:
            self._closed = True
            self._engine.close()

    def __enter__(self):
        _ = self.session
        return self

    def __exit__(self, *_):
        self.close()
