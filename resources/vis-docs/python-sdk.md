# Python SDK

Use `Agent()` to run tasks in your current project, or give it a `GatewayClient`
to use a separately running gateway. Add your application's functions with
`extensions=[...]`; they keep access to your Python objects in either mode.
Both modes provide `run()`, `send()` and one conversation for follow-up requests.

For classes, methods, signatures and type annotations, browse the
[generated Python SDK API reference](https://vis.blockether.com/python-sdk-api/).
It is rebuilt from `main` and may include APIs not yet released on PyPI.
Use this guide for installation and task examples.

## Install the SDK

**The execution-layer and application-extension API on this page is unreleased.**
It requires an SDK and engine built from the same source revision. An older
engine cannot execute application callbacks, even if you update only the Python
package. Do not use these examples with an older published runtime.

You need Python 3.11 or newer. Published SDKs are installed with the command
below; version `0.2.3` introduced the earlier Agent API, not the new API shown here:

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

## Use stdio from the Python SDK

The local `Agent` example above starts `vis-agent stdio` for you. This is a
working transport for a Python-owned Vis process, not a command for asking the
agent a question at your terminal and not an MCP server. It does not start an
HTTP gateway. The SDK and engine must come from compatible revisions (the
execution-layer API in this guide is not yet released).

To choose the executable yourself or share one local process between agents,
use `LocalEngine`:

```python
from blockether.vis.engine import Agent, LocalEngine

with LocalEngine(executable="vis-agent", root=".") as layer:
    with Agent(project=".", execution_layer=layer) as agent:
        result = agent.run("Summarize this project without changing files.")
        print(result["status"])
```

`LocalEngine` appends `stdio` to the executable command, sets `VIS_DB_PATH` to
an isolated temporary SQLite database, and stops its process and removes that
database when its context closes. Running a task can incur provider charges
and grant the engine access to project files. If the executable is not on
`PATH`, pass its absolute launcher path to `executable`.

You can run `vis-agent stdio --help` without starting the transport. Direct
invocation requires `VIS_DB_PATH` set to an isolated, writable SQLite file
path. The process writes a protocol handshake and newline-delimited JSON
(NDJSON) replies to stdout, reads NDJSON requests from stdin, and exits when
stdin closes. Keep stdout free of other output; use the SDK instead of typing
requests by hand. Neither starting nor stopping this mode affects an existing
gateway.

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

from blockether.vis.engine import Agent, GatewayClient


def main():
    with GatewayClient(
        os.environ["VIS_GATEWAY_URL"],
        token=os.environ["VIS_GATEWAY_TOKEN"],
    ) as execution_layer:
        with Agent(
            project=os.environ["VIS_PROJECT_ROOT"],
            execution_layer=execution_layer,
        ) as agent:
            result = agent.run("Summarize this project without changing files.")
            print(f"Session: {agent.session.id}")
            print(f"Status: {result['status']}")
            print(json.dumps(result["content"], indent=2))


if __name__ == "__main__":
    main()
```

The script reads the environment variables; neither object discovers a gateway
or loads a local token. `GatewayClient` accepts an HTTP(S) **origin** with no
path prefix, query, fragment or URL credentials. TLS verification stays enabled
and redirects are refused. Transport options belong to the execution layer,
not to `Agent`.

An Agent borrows the execution layer you pass in. Closing it detaches its
application extensions; closing the outer `GatewayClient` releases the client
lease and closes its streams. Neither action stops the gateway or deletes the
saved conversation. A running turn may continue, but it cannot call application
functions after their Agent closes.

The session uses the `app` channel, so it is visible in the app on that gateway.
Keep the printed session ID: to resume it later, open a
`GatewayClient(url, token=...)` context and use `client.session(session_id)`.
Each new Agent creates a new session; it does not implicitly resume an old one.

## Give the agent your functions

Pass an `Extension` to your Agent to let it use business rules, query your data
or call your services. You do not need a `.vis/extensions/` file or a global
registration call. The extension belongs to that Agent's conversation, not to
other agents sharing its execution layer.

**Your functions run in your application process, on the SDK calling thread.**
They can capture existing objects such as a database client or an in-memory
list. This also works with a remote gateway: function code, closures and local
objects are not uploaded. Install their dependencies in your application's
Python environment.

These functions run with **your application's permissions**, outside the model's
sandbox. Review what they expose. Arguments and returned data cross to the
engine and can become model context; do not return credentials or unrelated
private data.

### Register a function

Save this complete application as `delivery_task.py`. Its closure records quoted
weights in a list owned by the application. No code is installed on the gateway:

```python
# delivery_task.py
import blockether.vis.extension as vis
from blockether.vis.engine import Agent


def quote_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return vis.ActivityPresentation("Quote delivery", f"{result} cents")


def make_delivery_extension(quoted_weights: list[int]) -> vis.Extension:
    def delivery_quote(weight_grams: int, *, express: bool = False) -> int:
        """Return a delivery price in cents and remember the quoted weight.

        Charge 500 cents plus 100 per started kilogram. express defaults to
        False; True adds 500 cents. Raise ValueError for zero or negative weight.
        Append valid weights to application memory; no network or file changes.
        """
        if weight_grams <= 0:
            raise ValueError("weight_grams must be positive")
        quoted_weights.append(weight_grams)
        kilograms = (weight_grams + 999) // 1000
        return 500 + 100 * kilograms + (500 if express else 0)

    return vis.Extension(
        name="delivery",
        description="Delivery prices from this application.",
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


def quote_delivery(agent: Agent) -> dict:
    result = agent.run(
        "Use delivery_quote to quote express delivery for a 1200-gram parcel. "
        "Report the returned price in cents."
    )
    print("Status:", result["status"])
    print(agent.session.transcript(format="markdown").content.decode())
    return result


def main():
    quoted_weights = []
    extension = make_delivery_extension(quoted_weights)
    with Agent(extensions=[extension]) as agent:
        quote_delivery(agent)
    print("Quoted weights:", quoted_weights)


if __name__ == "__main__":
    main()
```

`vis.Symbol` exposes the function's name, annotations and docstring to the agent.
Here the callable is `delivery_quote`, not `delivery.delivery_quote`: `alias`
identifies the extension, not a function-name prefix. `prompt` explains when to
use the function; `symbols` makes it callable. Every exported function needs an
Activity presentation. This quick calculation shows its price on completion
rather than adding a running indicator; failures retain their error details.

### Ask the agent to use it

Run `python delivery_task.py` from your project. The function returns **1200
cents**, the Activity reads **Quote delivery · 1200 cents**, and your application
prints `Quoted weights: [1200]` after one call. The agent can discover the contract
through `apropos` and `doc`, call the function through `python_execution`, and use
its result in the answer. This is a model request and can incur provider charges.

You can also add an extension after entering the Agent context, before its first
request: call `agent.register_extension(extension)`. Both forms use the same
`Extension` declaration. For a remote agent, pass the same extension object to
`Agent(execution_layer=execution_layer, extensions=[extension], ...)` inside the
gateway context above. It still executes in your application.

### Understand callback lifetime and supported declarations

Callbacks run while your program drives the SDK: `run()`, turn waiting, session
or turn reads, and event iteration service pending calls. `send()` alone does not
start a background thread that executes your application code. Keep driving the
SDK and keep the application alive while the agent needs its functions.

Closing the Agent detaches its extensions. Disconnecting or cancelling releases
the engine's wait, but cannot forcibly interrupt a synchronous Python function
already running in your process. A repeated delivery of the same pending call
uses its retained result rather than executing the function again; this is not
an exactly-once guarantee across application restarts or new agent requests.

Client extensions support functions, bound methods and object namespaces declared
with `Symbol`, an explicit Activity for every exported method, and a static
`prompt`. For an object namespace, use `Symbol(object, name="inventory")` and
annotate its methods with `@vis.method(activity=...)`; see the
[extension API](extension-api.md). Host-only `activation`, `ctx`, `env`, providers,
op hooks, network filters, slash commands and callable prompts are rejected, not
silently ignored. Use an [engine-side extension](extending.md) for those features;
its registration entry point is `vis.register_extension(...)`.

Arguments must be JSON data. Results can be JSON values, tuples or dataclass
instances; tuples become lists and dataclass fields become ordinary data, keeping
`None` fields. Live object identity stays in your application, not in the returned
value. Async functions are awaited on the SDK calling thread, which must not
already be running an asyncio event loop.

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

### Read Activity and view receipts

Use `event.activity` to read an Activity receipt with immutable rows, outcome counts
and evidence. Its `groups` property groups invocations by operation;
`argument_groups` groups calls with identical arguments. These reader views leave
`rows` and serialization unchanged. A receipt can be one page of history: check
`history` and `omitted` before treating it as complete.

`event.view` decodes view lifecycle events. The records describe input forms, live
interfaces, patches and closure results; they are not Python UI widgets. To create
an interface, follow [Forms and user input](human-input.md) or [Live views](live-views.md).

When you have saved JSON rather than an event, use the record's `from_wire()` method.
It validates the data and makes nested values immutable; `to_wire()` returns a fresh
JSON-compatible copy. For example, this reads a completed live-view receipt without
starting Vis, opening a view or making a model call:

```python
# view_receipt.py
from blockether.vis.views import LiveResult

result = LiveResult.from_wire(
    {
        "view_id": "build-one",
        "is_completed": True,
        "reason": "completed",
        "is_from_human": False,
        "view": {
            "title": "Build",
            "nodes": [{"id": "status", "type": "status", "text": "Done", "tone": "ok"}],
        },
    }
)
assert result.view.nodes[0]["text"] == "Done"
assert result.to_wire()["view"]["title"] == "Build"
```

Both assertions pass for this receipt. Invalid data raises `ValueError`; decoding
never assigns engine IDs, sequence numbers, timeouts or terminal outcomes.

## Handle failures and choose a lifecycle

| Situation | Meaning and next step |
| --- | --- |
| `ProtocolError` | SDK and gateway protocols disagree; install compatible versions |
| `GatewayError` | Inspect `status` and `code` for authentication, permissions or request errors |
| `TransportError` | Check the executable or gateway, network and TLS setup |
| `VisTimeout` from `run()` or `turn.wait()` | Waiting ended, not necessarily the turn; inspect it or call `turn.cancel()` |
| A record whose `status` is not `completed` | The task did not complete normally; inspect its content and input requirements |

A wait timeout is separate from the execution layer's transport `timeout`. A
local pipe timeout stops its engine. Leaving a default local Agent context also
stops unfinished work; an Agent with a borrowed layer leaves that layer running.
A remote turn can outlive the client. When retrying a submission, reuse the same
explicit `idempotency_key`; a new key means a new request.

| API | Use it for | Closing it |
| --- | --- | --- |
| `Agent(project=".")` | One local conversation, no gateway or HTTP listener | Stops its engine and discards session history |
| `Agent(project=..., execution_layer=layer)` | One conversation on a caller-owned local engine or gateway client | Detaches its application extensions; leaves the layer and saved conversation open |
| `LocalEngine(executable=..., root=...)` | Several sessions in one owned stdio process | Stops that process and discards its session database |
| `GatewayClient(url, token=...)` | Persistent or shared sessions on a separately running gateway | Closes streams and releases its client lease |

For a launcher outside `PATH`, configure `LocalEngine(executable=..., root=...)`
and pass that layer to `Agent`. `LocalEngine` accepts a launcher path or argv list
and adds `stdio` itself. Use an outer `with LocalEngine(...) as layer:` context
to own its lifetime, as the gateway example does with `GatewayClient`.

Both implementations share the `ExecutionLayer` contract; `Agent` does not choose
a transport from a mixture of gateway and process options. Use the complete
installed wrapper, not a bare native binary without its Python sidecar. Each
client and its session handles use one calling thread.
`conversation.delete()` is a separate, destructive operation.

## See also

- [Running a gateway](gateway-service.md) — install and secure a shared agent service.
- [Java and Clojure SDK](jvm-sdk.md) — call Vis from a JVM application.
- [Native builds for JVM extensions](jvm-native-image.md) — rebuild Vis only when adding Java/Clojure capabilities.
- [Extending Vis](extending.md) — add tools inside the agent.
