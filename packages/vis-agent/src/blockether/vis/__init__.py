"""Run Vis from Python or give an agent tools backed by your application.

Use `blockether.vis.engine.Agent` for a conversation, or declare your own tools
with `blockether.vis.extension.Extension`. Importing the SDK starts no process,
connects to no gateway and makes no model calls.

## First request

Install a matching SDK and Vis engine and configure a model provider first;
see the [Python SDK guide](https://vis.blockether.com/python-sdk.html).
Running an agent makes model calls that may incur charges. Its tools can read or
change files in the selected project, subject to the engine's permissions.

```python
from blockether.vis.engine import Agent

with Agent(project=".") as agent:
    result = agent.run("Explain the project structure without changing files.")
    print(result["status"])
    print(result["content"])
```

`Agent.run` waits for a turn record, not a plain string. Check `status` before
using `content`: failed, cancelled and suspended turns also return records.
The context manager closes this agent's private engine; it does not undo file
changes. To follow progress or cancel work, use `Agent.send` and the returned
`blockether.vis.engine.Turn` instead.

## Choose an API

| You want to | Start with | What it gives you |
| --- | --- | --- |
| Run a conversation or send follow-ups | `blockether.vis.engine.Agent` | One session, with a local engine by default |
| Reuse an engine or connect to a gateway | `blockether.vis.engine.LocalEngine`, `blockether.vis.engine.GatewayClient` | An execution layer whose lifetime you control |
| Read history, stream progress or answer a prompt | `blockether.vis.engine.Session`, `blockether.vis.engine.Turn`, `blockether.vis.engine.Events` | Session operations and typed progress events |
| Delegate to agents and exchange messages | `blockether.vis.engine.Council` | Managed subagents and session conversations |
| Expose your Python functions to an agent | `blockether.vis.extension.Extension`, `blockether.vis.extension.Symbol` | Named tools with signatures and activity presentations |
| Show tool progress and results | `blockether.vis.extension.Activity` | A tool's label and progress or result presentation |
| Read tool outcomes and evidence | `blockether.vis.activity.ActivityProjection` | Immutable activity receipts with counts and grouping |
| Read input and live-view events | `blockether.vis.views` | Typed view snapshots, updates and results |
| Check an extension package before distribution | `blockether.vis.extension_package` | Package metadata parsing and validation |

For complete application examples, use the
[SDK guide](https://vis.blockether.com/python-sdk.html). For installable extension
projects, start with [Write an extension](https://vis.blockether.com/extending.html).
The reference pages below describe the individual classes, methods and values.

## Runtime and versions

This reference is generated from the SDK source on `main`; it can describe APIs
not yet released on PyPI. Follow the SDK guide's installation note before using
an example, and use matching engine and SDK builds for development APIs.

An `Agent` with no supplied execution layer owns a temporary local engine.
An explicitly supplied layer is borrowed: close that layer yourself when all
agents using it have finished. A gateway project path belongs to the gateway
host, not necessarily the machine running Python.

Application-owned extension callables remain in your Python process. Host-only
features such as live views need an attached Vis host; importing their types
does not create one. Each module explains that boundary before its examples.
"""
