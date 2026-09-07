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
from ._local import LocalEngine

__all__ = [
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
