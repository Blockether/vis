"""Explicit Vis engine clients, lifecycle records, streams and transport errors.

``GatewayClient`` connects to an existing gateway; ``LocalEngine`` owns a local
stdio process. Importing either starts nothing and never binds an extension host.
"""

from ._client import (
    Event,
    Events,
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
