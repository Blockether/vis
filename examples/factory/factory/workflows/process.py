"""Work on one run: `python -m factory.workflows.process <run id>`. With `--check <name>`, only load the source on the standard input."""

from __future__ import annotations

import sys
import traceback
import types
from collections.abc import Callable

from blockether.vis.engine import GatewayError, Session

from factory import settings
from factory.gateway import Gateway
from factory.workflows.client import Durable
from factory.workflows.sdk import Context, Marks, Run, Waiting, encode, marked
from factory.workflows.source import check, describe


def load(name: str, source: str) -> tuple[Callable[..., object], Marks]:
    """Run the source of a workflow. Return its `@workflow` function and what `@workflow` says about it."""
    module = types.ModuleType(f"workflow_{name}")
    module.__file__ = f"{name}.py"
    sys.modules[module.__name__] = module
    exec(compile(source, module.__file__, "exec"), module.__dict__)  # noqa: S102 - the source is the workflow
    found = [(value, about) for value in vars(module).values() if callable(value) and (about := marked(value))]
    if len(found) != 1:
        raise ValueError("a workflow file needs exactly one function with @workflow")
    return found[0]


def loads(name: str) -> int:
    """Load the source on the standard input as a run does. Write the fault, if there is one."""
    try:
        load(name, sys.stdin.buffer.read().decode())
    except (Exception, SystemExit) as failure:
        print(f"{type(failure).__name__}: {failure}", file=sys.stderr)
        return 1
    return 0


def main(run_id: str) -> int:
    """Work on one run until it ends or waits for a person. Return the exit code of the process.

    The transcript of the session has each request of the run, so a new process of
    the same run continues after the last completed turn.
    """
    api = Durable()
    run: Run = api.call("GET", f"runs/{run_id}?worker=1")
    try:
        # A trigger gives the fields of its event, so the same rule as for a person applies before any work.
        check(describe(run["workflow"], run["source"]), run["input"])
        function, about = load(run["workflow"], run["source"])
        gateway = Gateway()
        with gateway.client() as client:
            fresh_count = 0

            def fresh() -> Session:
                """Make a grouped session. The first creation can be repeated safely."""
                nonlocal fresh_count
                fresh_count += 1
                key = f"factory-run-{run_id}"
                if run.get("session_id") or fresh_count > 1:
                    key += f"-{run['starts']}-{fresh_count}"
                return gateway.create(client, group=settings.RUNS_GROUP, key=key, title=f"{run['title']} · run {run_id[:8]}", model=about.model)

            known = run.get("session_id")
            if known:
                session = client.session(known)
                try:
                    turns = len(session.turns())
                except GatewayError as failure:
                    if failure.status != 404 or not run.get("own"):
                        raise
                    session, turns = fresh(), 0
                    run["base"] = 0
            else:
                session, turns = fresh(), 0
                run["session_id"] = session.id
            base = run.get("base")
            if base is None:
                base = run["base"] = turns
            api.event(run_id, "started", session_id=session.id, base=base)
            result = function(Context(run, session, api, fresh, about.activities, about.requests), **run["input"])
            api.event(run_id, "completed", result=encode(result))
            return 0
    except Waiting:  # the API has the event: this process ends and a new one continues the run
        return 0
    except Exception as failure:
        api.event(run_id, "failed", error=f"{type(failure).__name__}: {failure}", trace=traceback.format_exc()[-3000:])
        return 1


if __name__ == "__main__":
    sys.exit(loads(sys.argv[2]) if sys.argv[1] == "--check" else main(sys.argv[1]))
