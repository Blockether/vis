"""Start the Lambda runtime with the handler of this function's role."""

from __future__ import annotations

import importlib

from durable import runtime
from durable.env import ROLE

ROLES = {
    "front": "durable.front",
    "worker": "durable.worker",
    "llm": "durable.llm",
    "repo": "durable.repo",
    "sync": "durable.sync",
    "app": "durable.app",
}

if __name__ == "__main__":
    try:
        handler = importlib.import_module(ROLES[ROLE]).handler
    except BaseException as error:
        runtime.init_error(error)
        raise
    runtime.serve(handler)
