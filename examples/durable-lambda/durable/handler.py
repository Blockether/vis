"""The handler of each function of the stack. One image has all the functions: `VIS_ROLE` selects one."""

from __future__ import annotations

import importlib

from durable import lambda_runtime
from durable.settings import ROLE

# The module that has the handler of each role. A function imports only its own module.
ROLES = {
    "front": "durable.dashboard.front",
    "worker": "durable.worker",
    "llm": "durable.llm",
    "repo": "durable.git",
    "sync": "durable.worktree",
}


def main() -> None:
    """Start the Lambda runtime with the handler of the role of this function."""
    try:
        handler = importlib.import_module(ROLES[ROLE]).handler
    except BaseException as error:
        lambda_runtime.init_error(error)
        raise
    lambda_runtime.serve(handler)
