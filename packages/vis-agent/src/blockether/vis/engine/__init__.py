"""Synchronous agent, session, execution-layer and Council APIs.

For installation, workflows and lifecycle choices, use the
[Python SDK guide](https://vis.blockether.com/python-sdk.html).
"""

from ._agent import Agent, StructuredOutputError
from ._agents import Subagent, SubagentRecord
from ._client import (
    Event,
    Events,
    ExecutionLayer,
    GatewayClient,
    GatewayError,
    JobEvent,
    JobEvents,
    JSONValue,
    ProtocolError,
    Query,
    Response,
    Session,
    TransportError,
    Turn,
    VisTimeout,
)
from ._council import (
    Council,
    CouncilEntry,
    CouncilKind,
    CouncilMember,
    CouncilPage,
    CouncilReply,
    CouncilSource,
    CouncilThread,
)
from ._install import InstallError, ensure_installed, install
from ._local import LocalEngine
from ._models import Model, SessionModel

__all__ = [
    "Agent",
    "Subagent",
    "SubagentRecord",
    "Council",
    "CouncilEntry",
    "CouncilKind",
    "CouncilMember",
    "CouncilPage",
    "CouncilReply",
    "CouncilSource",
    "CouncilThread",
    "Event",
    "Events",
    "ExecutionLayer",
    "GatewayClient",
    "GatewayError",
    "InstallError",
    "JobEvent",
    "JobEvents",
    "JSONValue",
    "LocalEngine",
    "Model",
    "ProtocolError",
    "Query",
    "Response",
    "Session",
    "SessionModel",
    "StructuredOutputError",
    "TransportError",
    "Turn",
    "VisTimeout",
    "ensure_installed",
    "install",
]
