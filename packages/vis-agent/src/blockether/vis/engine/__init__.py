"""Synchronous agent, session, execution-layer and Council APIs.

For installation, workflows and lifecycle choices, use the
[Python SDK guide](https://vis.blockether.com/python-sdk.html).
"""

from ._agent import Agent, StructuredOutputError, final_answer
from ._agents import Subagent, SubagentRecord
from ._client import (
    Attachment,
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
    SessionOptions,
    TransportError,
    Turn,
    VisTimeout,
    connect,
)
from ._council import (
    Council,
    CouncilEntry,
    CouncilMember,
    CouncilPage,
    CouncilReply,
    CouncilSource,
    CouncilThread,
)
from ._groups import SessionGroup
from ._install import InstallError, ensure_installed, install
from ._models import Model, SessionModel
from ._service import start, stop_shared
from ._sounds import SoundPreset, Sounds, SoundSettings
from ._speech import Speech, SpeechAudio, Transcript, Voice

__all__ = [
    "Agent",
    "Attachment",
    "Subagent",
    "SubagentRecord",
    "Council",
    "CouncilEntry",
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
    "Model",
    "ProtocolError",
    "Query",
    "Response",
    "Session",
    "SessionGroup",
    "SessionModel",
    "SessionOptions",
    "SoundPreset",
    "Sounds",
    "SoundSettings",
    "Speech",
    "SpeechAudio",
    "StructuredOutputError",
    "Transcript",
    "TransportError",
    "Turn",
    "VisTimeout",
    "Voice",
    "connect",
    "ensure_installed",
    "final_answer",
    "install",
    "start",
    "stop_shared",
]
