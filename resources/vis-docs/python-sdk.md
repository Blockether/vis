# Python SDK

Use `Agent()` to run tasks in your current project without a gateway, or pass
`gateway_url` to use a separately running gateway. Both modes provide `run()`,
`send()` and one conversation for follow-up requests.

## Install the SDK

You need Python 3.11 or newer. The `Agent` API is available in PyPI version
`0.2.3` and newer. Install the released SDK:

```bash
python3 -m venv .venv
. .venv/bin/activate
python -m pip install --upgrade "vis-agent>=0.2.3"
```

The Python package does **not** install the engine. For local use on Linux or
macOS, install the [Vis runtime](distributions.md), put `vis-agent` on `PATH` and
configure a [provider and model](configuration.md). A remote client needs only
the Python package; its provider runs on the gateway machine.

Requests can incur model charges and use the engine's tools and files. Choose an
appropriate account and [access policy](jail.md) before running a task. Keep
credentials and engine state outside the project you ask the agent to inspect.

## Let your program own a private agent

Save this as `local_task.py` and run `python local_task.py` from your project:

```python
# local_task.py
import json

from blockether.vis.engine import Agent


def main():
    with Agent(project=".") as agent:
        result = agent.run("Summarize this project without changing files.")
        print(f"Session: {agent.session.id}")
        print(f"Status: {result['status']}")
        print(json.dumps(result["content"], indent=2))


if __name__ == "__main__":
    main()
```

`.` means the current directory when you construct the agent. `Agent()` is
identical. Entering the context starts a private engine with no HTTP listener;
leaving it stops that process and discards its temporary session database.
It does not undo file edits or isolate your credentials and configuration.

You should see a session ID, `Status: completed` and the answer's content blocks.
`run()` returns a turn record, not a text string. A failed, cancelled or suspended
turn also returns a record: always check `status`.

## Connect to a gateway and run a task

Use a gateway when conversations must survive your script or be shared with the
Vis app. You need only the Python SDK on the client machine, not a local engine.
[Start a gateway](gateway-service.md#start-a-local-gateway), then set these
variables in your private terminal for a local, default-state installation:

```bash
export VIS_GATEWAY_URL=http://127.0.0.1:7890
export VIS_GATEWAY_TOKEN="$(cat "$HOME/.vis/gateway.token")"
export VIS_PROJECT_ROOT="$PWD"
```

Do not print or commit the token. For a [remote gateway](gateway-service.md#connect-from-another-machine),
use its HTTPS origin and obtain its token securely from the operator.
`VIS_PROJECT_ROOT` must be an **absolute path on the gateway machine**, not a path
on your laptop. Remote Agent rejects `.` rather than guessing a server directory.

Save this independent example as `gateway_task.py` and run `python gateway_task.py`:

```python
# gateway_task.py
import json
import os

from blockether.vis.engine import Agent


def main():
    with Agent(
        project=os.environ["VIS_PROJECT_ROOT"],
        gateway_url=os.environ["VIS_GATEWAY_URL"],
        token=os.environ["VIS_GATEWAY_TOKEN"],
    ) as agent:
        result = agent.run("Summarize this project without changing files.")
        print(f"Session: {agent.session.id}")
        print(f"Status: {result['status']}")
        print(json.dumps(result["content"], indent=2))


if __name__ == "__main__":
    main()
```

The script reads the environment variables; `Agent` does not discover a gateway
or load a local token. Pass an HTTP(S) **origin** with no path prefix, query,
fragment or URL credentials. TLS verification stays enabled and redirects are
refused. Local-only `executable` and `startup_timeout` options cannot be combined
with `gateway_url`.

Closing a remote Agent closes its event streams and releases its client lease.
It does **not** stop the gateway, cancel a turn or delete the saved conversation.
The session uses the `app` channel, so it is visible in the app on that gateway.
Keep the printed session ID: to resume it later, open a
`GatewayClient(url, token=...)` context and use `client.session(session_id)`.
Each new Agent creates a new session; it does not implicitly resume an old one.

## Give the agent your functions

Register your Python functions as a **project extension** to let the agent use
business rules, query your data or call your services. `Agent` runs the
conversation; the extension adds the capabilities. The SDK does not accept
client-side callbacks or a `tools` argument on `Agent`.

The functions run in the **engine's extension worker**, not in your SDK process.
Extensions are trusted Python code: they run with the engine user's permissions,
outside the model's sandbox. Review the code and its dependencies before loading
it, especially when a function can change data or contact a service.

### Register a function

Create this file under the project you pass to `Agent`. For a gateway-backed
Agent, put it in that project **on the gateway machine**, not just on your laptop.
This example returns a delivery price in cents without network calls or file
changes:

```python
# .vis/extensions/delivery_tools.py
import blockether.vis.extension as vis


def delivery_quote(weight_grams: int, *, express: bool = False) -> int:
    """Return a delivery price in cents for a positive weight in grams.

    Charge 500 cents plus 100 per started kilogram. express defaults to False;
    True adds 500 cents. Raise ValueError for zero or negative weight.
    This function makes no network calls and changes no files or other data.
    """
    if weight_grams <= 0:
        raise ValueError("weight_grams must be positive")
    kilograms = (weight_grams + 999) // 1000
    return 500 + 100 * kilograms + (500 if express else 0)


def quote_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return vis.ActivityPresentation("Quote delivery", f"{result} cents")


vis.register(
    vis.Extension(
        name="delivery",
        description="Delivery prices for this project.",
        alias="delivery",
        prompt="Use delivery_quote for delivery prices.",
        symbols=[
            vis.Symbol(
                delivery_quote,
                activity=vis.Activity(
                    label="Quote delivery",
                    show_start=False,
                    render=quote_activity,
                ),
            )
        ],
    )
)
```

`vis.Symbol` exposes the function's name, annotations and docstring to the agent.
Here the callable is `delivery_quote`, not `delivery.delivery_quote`: `alias`
identifies the extension, not a function-name prefix. The `prompt` tells the
agent when to use it; the `symbols` registration makes it callable. Every exported
function needs an Activity presentation. This quick calculation shows its price
on completion rather than adding a running indicator; failures retain their
error details.

To expose an existing function, import it into this entry file and pass it to
`vis.Symbol` in the same way. For methods, packages and other extension features,
see [the extension API](extension-api.md). Install third-party dependencies in the
**engine's** Python environment, not only in your SDK application's environment;
[develop an extension](extension-development.md) and
[package an extension](extension-packages.md) cover that setup. Python extensions
do not require a native-image rebuild.

### Ask the agent to use it

Create the extension before starting the Agent. Save this script in the project
and run `python delivery_task.py`:

```python
# delivery_task.py
from blockether.vis.engine import Agent


def quote_delivery(agent: Agent) -> dict:
    result = agent.run(
        "Use delivery_quote to quote express delivery for a 1200-gram parcel. "
        "Report the returned price in cents."
    )
    print("Status:", result["status"])
    print(agent.session.transcript(format="markdown").content.decode())
    return result


if __name__ == "__main__":
    with Agent(project=".") as agent:
        quote_delivery(agent)
```

The function returns **1200 cents**. The agent can discover its contract through
`apropos` and `doc`, call it through `python_execution`, and use the returned value
in its answer. The Activity reads **Quote delivery · 1200 cents**. This is a model
request, so it uses your configured provider and may incur model charges.

For a remote agent, call the same `quote_delivery(agent)` inside the gateway-backed
`Agent` context shown above. Both modes load capabilities from the selected engine
project; neither uploads this file, your dependencies or local Python closures.
Calling `vis.register()` in the SDK client process does not register a remote tool.

After editing an extension, restart your private local Agent, or use `/reload` in
Vis for an existing gateway conversation. Already-loaded extension code does not
change just because you edited the file. See
[add your first extension](extending.md#your-first-extension) for discovery and
reload behavior.

## Continue a conversation

Reuse either kind of Agent for follow-up requests. Use `send()` instead of `run()`
when you want a turn handle for progress, waiting or cancellation:

```python
with Agent(project=".") as agent:
    first = agent.run("Explain the test setup without changing files.")
    if first["status"] == "completed":
        turn = agent.send("Which test should I run first?")
        result = turn.wait(timeout=300)
        print(result["status"], result["content"])
```

`agent.session` exposes the underlying session, including its ID and transcript.
Export history before closing a local agent if you need to keep it.
For example, inside the context, `agent.session.transcript(format="markdown").content`
returns bytes you can save to a file.

By default, requests use the engine's configured provider and model. Both `run()`
and `send()` accept `provider` and `model` to choose configured alternatives for a
request. They also accept `Session.send()` options such as `idempotency_key`.

## Show progress while a turn runs

Pass `agent.session` and the turn from `agent.send(...)`, or the session and turn
from a gateway client, to this function:

```python
# progress.py
def watch_turn(conversation, turn):
    terminal = {"turn.completed", "turn.failed", "turn.cancelled"}
    with conversation.events(cursor=turn.cursor) as events:
        for event in events:
            print(event.type)
            if event.turn_id == turn.id and event.type in terminal:
                break
    return turn.wait(timeout=300)
```

`turn.cursor` was captured before submission, so even a fast answer can be replayed.
The stream follows the **session** and does not end automatically with a turn;
stop only for the matching turn. Closing the stream does not cancel work.
Save `events.cursor` if you need to reconnect.

For structured progress, inspect `event.activity` and `event.view`. If the turn
needs a person's answer, use `conversation.input_views()` and
`conversation.answer(view_id, values)`; see [Forms and user input](human-input.md).
Do not automatically approve credential or permission requests.

## Handle failures and choose a lifecycle

| Situation | Meaning and next step |
| --- | --- |
| `ProtocolError` | SDK and gateway protocols disagree; install compatible versions |
| `GatewayError` | Inspect `status` and `code` for authentication, permissions or request errors |
| `TransportError` | Check the executable or gateway, network and TLS setup |
| `VisTimeout` from `run()` or `turn.wait()` | Waiting ended, not necessarily the turn; inspect it or call `turn.cancel()` |
| A record whose `status` is not `completed` | The task did not complete normally; inspect its content and input requirements |

A wait timeout is separate from the client's transport `timeout`. A local pipe
timeout stops the owned engine; leaving a local Agent context also stops unfinished
work. A remote turn can outlive the client. When retrying a submission, reuse the
same explicit `idempotency_key`; a new key means a new request.

| API | Use it for | Closing it |
| --- | --- | --- |
| `Agent(project=".")` | One local conversation, no gateway or HTTP listener | Stops its engine and discards session history |
| `Agent(project="/srv/project", gateway_url=..., token=...)` | The same task interface on an existing gateway | Releases its client lease; keeps remote work and history |
| `LocalEngine(executable=..., root=...)` | Several sessions in one owned stdio process | Stops that process and discards its session database |
| `GatewayClient(url, token=...)` | Persistent or shared sessions on a separately running gateway | Closes streams and releases its client lease |

For a launcher outside `PATH`, pass its absolute path as `Agent(executable=...)`.
`Agent` and `LocalEngine` also accept an argv list and add `sdk-stdio` themselves.
Use the complete installed wrapper, not a bare native binary without its Python
sidecar. Each client and its session handles use one calling thread.
`conversation.delete()` is a separate, destructive operation.

## See also

- [Running a gateway](gateway-service.md) — install and secure a shared agent service.
- [Java and Clojure SDK](jvm-sdk.md) — call Vis from a JVM application.
- [Native builds for JVM extensions](jvm-native-image.md) — rebuild Vis only when adding Java/Clojure capabilities.
- [Extending Vis](extending.md) — add tools inside the agent.
