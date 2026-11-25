"""Synchronous agent, session, client and Council APIs.

For installation, workflows and lifecycle choices, use the
[Python SDK guide](https://vis.blockether.com/python-sdk.html).
"""

from ._agent import Agent, StructuredOutputError, final_answer
from ._agents import Subagent, SubagentRecord, SubagentTask
from ._client import (
    Attachment,
    EngineClient,
    Event,
    Events,
    GatewayClient,
    GatewayError,
    JobEvent,
    JobEvents,
    JSONValue,
    ProtocolError,
    Response,
    Session,
    SessionOptions,
    TransportError,
    Turn,
    VisTimeout,
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
from ._local import StdioEngineClient
from ._models import Model, SessionModel
from ._proxy import GatewayProxy
from ._routes import Query, Routes
from ._service import HttpLocalEngineClient
from ._sounds import SoundPreset, Sounds, SoundSettings
from ._speech import Speech, SpeechAudio, Transcript, Voice

__all__ = [
    "Agent",
    "Attachment",
    "Subagent",
    "SubagentRecord",
    "SubagentTask",
    "Council",
    "CouncilEntry",
    "CouncilMember",
    "CouncilPage",
    "CouncilReply",
    "CouncilSource",
    "CouncilThread",
    "Event",
    "Events",
    "EngineClient",
    "GatewayClient",
    "GatewayError",
    "GatewayProxy",
    "HttpLocalEngineClient",
    "InstallError",
    "JobEvent",
    "JobEvents",
    "JSONValue",
    "Model",
    "ProtocolError",
    "Query",
    "Response",
    "Routes",
    "Session",
    "SessionGroup",
    "SessionModel",
    "SessionOptions",
    "SoundPreset",
    "Sounds",
    "SoundSettings",
    "Speech",
    "SpeechAudio",
    "StdioEngineClient",
    "StructuredOutputError",
    "Transcript",
    "TransportError",
    "Turn",
    "VisTimeout",
    "Voice",
    "ensure_installed",
    "final_answer",
    "install",
]
