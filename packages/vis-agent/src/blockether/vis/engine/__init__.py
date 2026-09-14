"""Vis agents, engine clients, lifecycle records, streams and transport errors.

``Agent`` runs one conversation locally or through an existing gateway.
``GatewayClient`` manages gateway sessions; ``LocalEngine`` owns a local stdio
process. Importing these starts nothing and never binds an extension host.
"""

from ._agent import Agent
from ._agents import Subagent
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
from ._local import LocalEngine

__all__ = [
    "Agent",
    "Subagent",
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
    "JobEvent",
    "JobEvents",
    "JSONValue",
    "LocalEngine",
    "ProtocolError",
    "Query",
    "Response",
    "Session",
    "TransportError",
    "Turn",
    "VisTimeout",
]
