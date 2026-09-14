"""Run conversations, follow progress and coordinate managed agents from Python.

Start with `Agent` for one conversation. Choose an execution layer only when you
need a custom local executable or an existing gateway. Importing this module does
not start a process, connect to a gateway or register extensions.

## Choose an entry point

| Your task | API | Result |
| --- | --- | --- |
| Submit a request and wait | `Agent.run` | Turn record with a `status` field |
| Submit without waiting | `Agent.send` | `Turn` for reading, waiting or cancellation |
| Reuse a conversation | `Agent.session`, `Session` | History, artifacts, input and follow-up requests |
| Configure a private local process | `LocalEngine` | Owned process with a temporary session database |
| Use a running gateway | `GatewayClient` | Explicit authenticated connection; remote sessions survive close |
| Follow progress | `Session.events`, `Event` | Iterable typed event envelopes |
| Communicate or delegate | `Session.council`, `Council` | Messages, replies and managed `Subagent` records |

## Run your first conversation

Install the Vis engine as well as the SDK and configure a model provider before
running requests. Requests can incur model costs and edit files in the project.
Use a context manager so the private process is stopped even when your code raises.

```python
from blockether.vis.engine import Agent

with Agent(project=".") as agent:
    result = agent.run("Summarize this project without changing files.")
    print(result["status"])
```

The result is a structured turn record, **not a text string**. A returned record
can describe failure or suspension; inspect its status rather than treating a
normal return as success. See `Turn.wait` for completion states and `Agent.send`
when you need a handle before waiting.

## Ownership and failures

`Agent` owns its default `LocalEngine`, but borrows a layer you explicitly pass.
Closing the default agent removes its temporary conversation database, not file
edits. Closing a `GatewayClient` releases this connection, not the remote server.
Use each layer and its handles from one calling thread.

`GatewayError` reports an HTTP failure through safe `status` and `code` fields.
`TransportError` covers connection failures, `ProtocolError` incompatible or
malformed replies, and `VisTimeout` an expired deadline. A wait deadline is not
a cancellation request; see `Turn.cancel` for explicit cancellation.

For installation and complete task recipes, use the
[Python SDK guide](https://vis.blockether.com/python-sdk.html).
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
