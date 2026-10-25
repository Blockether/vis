"""Phase 8 fixture for #405: extension calls into vis.council, vis.sessions and vis.run."""

import dataclasses
import hashlib
import threading
import time

import blockether.vis.extension as vis
from blockether.vis.engine import GatewayError


def _record(value):
    return dataclasses.asdict(value)


def _refusal(call):
    try:
        call()
    except GatewayError as error:
        return {"refused": True, "code": error.code, "status": error.status}
    except RuntimeError as error:
        return {"refused": True, "code": type(error).__name__, "message": str(error)}
    return {"refused": False}


def _session_ids():
    return {row.get("id") or row.get("session_id") for row in vis.sessions.list()}


def _render(*, result, error, **_):
    return vis.ActivityPresentation("E2E sessions", str(error) if error else "Done")


def _activity(label, slow=True):
    return vis.Activity(label=label, show_start=slow, render=_render)


class Sessions:
    """Session and Council calls that the Phase 8 scenarios make."""

    @vis.method(tag="mutation", activity=_activity("Spawn children"))
    def spawn(
        self,
        task: str,
        key: str | None = None,
        repeat: int = 1,
        iterations: int | None = None,
    ) -> dict:
        """Spawn one child `repeat` times with the same input, as a host retry does."""
        ids = [
            vis.council.spawn(task, key=key, iteration_budget=iterations).session_id
            for _ in range(repeat)
        ]
        return {"ids": ids, "children": [a.session_id for a in vis.council.subagents()]}

    @vis.method(activity=_activity("Wait for children"))
    def wait(self, ids: list[str] | None = None, timeout: int = 240) -> list:
        """Wait until no child runs, then return their records.

        A Council wait also ends at each child event, so wait again until the deadline.
        """
        deadline = time.monotonic() + timeout
        while True:
            left = max(1, int(deadline - time.monotonic()))
            records = [_record(r) for r in vis.council.wait(ids, timeout_secs=left)]
            if (
                all(r["status"] != "running" for r in records)
                or time.monotonic() >= deadline
            ):
                return records

    @vis.method(tag="mutation", activity=_activity("Extend a child", slow=False))
    def extend(self, session_id: str, iterations: int) -> dict:
        """Give a child more iterations."""
        return _record(vis.council.extend(session_id, iterations))

    @vis.method(tag="mutation", activity=_activity("Resume a child"))
    def resume(self, session_id: str, task: str) -> dict:
        """Give a finished child a follow-up task and wait for it."""
        return _record(vis.council.resume(session_id, task))

    @vis.method(tag="mutation", activity=_activity("Deny a request", slow=False))
    def deny(self, session_id: str, reason: str) -> dict:
        """Refuse the open request of a child."""
        return _record(vis.council.deny(session_id, reason))

    @vis.method(tag="mutation", activity=_activity("Cancel a child", slow=False))
    def cancel(self, session_id: str) -> dict:
        """Cancel a child."""
        return _record(vis.council.cancel(session_id))

    @vis.method(activity=_activity("Spawn from a thread", slow=False))
    def thread_spawn(self, task: str) -> dict:
        """Spawn from a thread that the extension created; the host must refuse it."""
        out = {}

        def work():
            out.update(_refusal(lambda: vis.council.spawn(task)))

        thread = threading.Thread(target=work)
        thread.start()
        thread.join(60)
        out["children"] = [a.session_id for a in vis.council.subagents()]
        return out

    @vis.method(tag="mutation", activity=_activity("Run a session"))
    def run(self, request: str) -> dict:
        """Run one turn in a new independent session."""
        before = _session_ids()
        turn = vis.run(request)
        created = sorted(_session_ids() - before)
        info = vis.sessions.get(created[0]).read() if len(created) == 1 else {}
        return {
            "created": created,
            "status": turn.get("status"),
            "origin": info.get("origin"),
            "parent_id": info.get("parent_id"),
        }

    @vis.method(activity=_activity("Read settings", slow=False))
    def settings(self) -> dict:
        """Call a route that extensions cannot use."""
        return _refusal(lambda: vis._host_layer().get_settings())

    @vis.method(activity=_activity("Change another session", slow=False))
    def mutate(self, session_id: str) -> dict:
        """Send a turn to a session that this extension did not create."""
        return _refusal(lambda: vis.sessions.get(session_id).send("Say hello."))

    @vis.method(activity=_activity("Export a session"))
    def export(self, path: str, session_id: str | None = None) -> dict:
        """Write a snapshot of a session to `path` and return its summary."""
        data = vis.sessions.export(session_id)
        with open(path, "wb") as out:
            out.write(data)
        return {"bytes": len(data), "sha": hashlib.sha256(data).hexdigest()}

    @vis.method(tag="mutation", activity=_activity("Restore a session"))
    def restore(self, path: str, key: str | None = None) -> dict:
        """Restore a snapshot file and return the new session id."""
        with open(path, "rb") as source:
            session = vis.sessions.restore(source.read(), key=key)
        return {"session_id": session.id}

    @vis.method(activity=_activity("List models", slow=False))
    def models(self) -> list:
        """Return the model catalog that extensions see."""
        return [
            {"provider": m.provider, "name": m.name, "is_available": m.is_available}
            for m in vis.session.client.models()
        ]


vis.register_extension(
    vis.Extension(
        name="e2e-sessions",
        alias="e2e",
        description="Phase 8 fixture for the session and Council API",
        symbols=[vis.Symbol(Sessions(), name="e2e")],
    )
)
