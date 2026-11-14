"""Start the Lambda runtime with the handler of this function's role."""

from __future__ import annotations

import importlib

from playground import runtime
from playground.env import ROLE

ROLES = {
    "front": "playground.front",
    "worker": "playground.worker",
    "llm": "playground.llm",
    "repo": "playground.repo",
    "sync": "playground.sync",
    "app": "playground.app",
}

if __name__ == "__main__":
    try:
        handler = importlib.import_module(ROLES[ROLE]).handler
    except BaseException as error:
        runtime.init_error(error)
        raise
    runtime.serve(handler)
