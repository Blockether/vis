"""One workflow run: `python -m durable.workflows.process <run id>`.

The worker starts this process next to the gateway. Its environment has no AWS
credentials. It talks to the gateway with the Vis SDK and to the Durable Lambda API.
"""

from __future__ import annotations

import os
import sys
import traceback
import types
from collections.abc import Callable

from blockether.vis.engine import GatewayClient, Session

from durable.settings import GATEWAY_URL, PROVIDER, RUN_GROUP, TOKEN_FILE
from durable.workflows.client import Durable
from durable.workflows.sdk import Context, Marks, Run, Waiting, encode, marked


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


def main(run_id: str) -> int:
    """Work on one run until it ends or waits for a person. Return the exit code of the process.

    The transcript of the session has each request of the run, so a new process of
    the same run continues after the last completed turn.
    """
    api = Durable()
    run: Run = api.call("GET", f"runs/{run_id}")
    try:
        function, about = load(run["workflow"], run["source"])
        with open(TOKEN_FILE, encoding="utf-8") as file:
            client = GatewayClient(GATEWAY_URL, token=file.read().strip())

        def fresh() -> Session:
            """Make a session for the run. The run works in its own directory. Its session is in the group of the runs, in the one project."""
            options = client.session_options(os.getcwd())
            if RUN_GROUP:
                options["group_id"] = RUN_GROUP
            session = client.create_session(provider=PROVIDER if about.model else None, model=about.model, **options)
            session.update(title=f"{run['title']} · run {run_id[:8]}")
            return session

        # An earlier process of this run made the session, or a person selected it.
        known = run.get("session_id")
        session = client.session(known) if known else fresh()
        base = run.get("base")
        if base is None:  # the turns that the session had before the run are not the transcript of the run
            base = run["base"] = len(session.turns())
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
    sys.exit(main(sys.argv[1]))
