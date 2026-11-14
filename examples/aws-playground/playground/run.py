"""One workflow run: `python -m playground.run <run id>`.

The worker starts this process next to the gateway. The process has no AWS
credentials. It talks to the gateway with the Vis SDK and to the playground API.
"""

from __future__ import annotations

import os
import sys
import traceback
import types
from collections.abc import Callable
from typing import Any

from blockether.vis.engine import GatewayClient

from playground.client import Playground
from playground.gateway import HOST, PORT, PROVIDER, TOKEN_FILE
from playground.workflows import Context, encode


def load(name: str, source: str) -> Callable[..., Any]:
    """Run the source of a workflow and return its `@workflow` function."""
    module = types.ModuleType(f"workflow_{name}")
    module.__file__ = f"{name}.py"
    sys.modules[module.__name__] = module
    exec(compile(source, module.__file__, "exec"), module.__dict__)  # noqa: S102 - the source is the workflow
    found = [value for value in vars(module).values() if callable(value) and hasattr(value, "__workflow__")]
    if len(found) != 1:
        raise ValueError("a workflow file needs exactly one function with @workflow")
    return found[0]


def main(run_id: str) -> int:
    api = Playground()
    run = api.call("GET", f"runs/{run_id}")

    def event(kind: str, **fields: Any) -> None:
        api.call("POST", f"runs/{run_id}/events", {"type": kind, **fields})

    try:
        function = load(run["workflow"], run["source"])
        with open(TOKEN_FILE, encoding="utf-8") as file:
            client = GatewayClient(f"http://{HOST}:{PORT}", token=file.read().strip())
        if run.get("session_id"):
            session = client.session(run["session_id"])
        else:
            options = client.session_options(os.getcwd())
            model = function.__workflow__["model"]
            session = client.create_session(provider=PROVIDER if model else None, model=model, **options)
            session.update(title=f"{run['title']} · run {run_id[:8]}")
        event("started", session_id=session.id)
        result = function(Context(run, session, api), **run["input"])
        event("completed", result=encode(result))
        return 0
    except Exception as failure:
        event("failed", error=f"{type(failure).__name__}: {failure}", trace=traceback.format_exc()[-3000:])
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
