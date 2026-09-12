"""One conversation using an owned stdio engine or an existing HTTP gateway."""

from pathlib import Path, PurePosixPath

from ._client import GatewayClient, Session, TransportError, Turn
from ._local import LocalEngine


class Agent:
    """Run tasks locally, or connect to a separately running gateway.

    Agent() resolves the existing local project directory (default: ".") at
    construction. Context entry, send(), run() or session starts a private
    LocalEngine and one conversation, without an HTTP listener. close() stops
    that process and discards its temporary session database, not file edits.
    Environment, provider credentials and extensions are inherited. Linux/macOS.

    With gateway_url, project must be an absolute POSIX path on that gateway;
    it is never resolved or checked on the client machine. No local engine is
    needed. The agent creates one app-channel session through GatewayClient.
    close() releases the client lease, not the saved session or remote work.
    The URL and optional bearer token are explicit; no environment discovery.

    executable (launcher path or argv) and startup_timeout are local-only.
    timeout bounds transport operations in either mode. Instances use one
    calling thread. Use GatewayClient directly to resume or manage sessions.
    """

    def __init__(
        self,
        project=".",
        *,
        gateway_url=None,
        token=None,
        executable="vis-agent",
        timeout=30,
        startup_timeout=120,
    ):
        if gateway_url is None:
            if token is not None:
                raise ValueError("token requires gateway_url")
            self.project = Path(project).resolve(strict=True)
            if not self.project.is_dir():
                raise NotADirectoryError(str(self.project))
            self._client = LocalEngine(
                executable=executable,
                root=self.project,
                timeout=timeout,
                startup_timeout=startup_timeout,
            )
            self._session_options = {"root": str(self.project)}
        else:
            if executable != "vis-agent" or startup_timeout != 120:
                raise ValueError("executable and startup_timeout are local-only")
            self.project = PurePosixPath(project)
            if not self.project.is_absolute():
                raise ValueError("project must be an absolute path on the gateway")
            self._client = GatewayClient(gateway_url, token=token, timeout=timeout)
            self._session_options = {"root": str(self.project), "channel": "app"}
        self._session = None
        self._closed = False

    @property
    def session(self) -> Session:
        """The same conversation for all requests; connects on first access."""
        if self._closed:
            raise TransportError("agent is closed")
        if self._session is None:
            try:
                self._client.connect()
                self._session = self._client.create_session(**self._session_options)
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
        exit stops unfinished local work; remote work can outlive the client.
        """
        return self.send(request, **options).wait(timeout=timeout)

    def close(self):
        """Close the owned process or gateway client, never a remote session or service."""
        if not self._closed:
            self._closed = True
            self._client.close()

    def __enter__(self):
        _ = self.session
        return self

    def __exit__(self, *_):
        self.close()
