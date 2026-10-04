# Python SDK basics

Use `Agent()` to embed Vis in your program and run tasks in your current project. To use a gateway
that runs separately, give `Agent()` a `GatewayClient`. Add your application's functions with
`extensions=[...]`. They keep access to your Python objects in both modes. Both modes provide
`run()`, `send()` and one conversation for follow-up requests.

For classes, methods, signatures and type annotations, browse the
[generated Python SDK API reference](https://vis.blockether.com/python-sdk-api/).
It is rebuilt from `main` and may include APIs not yet released on PyPI.
Use this guide for installation and task examples.

## When to use

- **Your script or application should give Vis a task in your project and use the
  answer.** Start with [a private agent](#let-your-program-own-a-private-agent) that
  your program owns.
- **Your program needs fields it can check, not prose.** Pass a Pydantic model to
  get a [validated result](#get-a-validated-result).
- **The agent needs your application's rules, data or services.** [Give it your
  functions](#give-the-agent-your-functions) without installing an extension.
- **Conversations must outlive your script or appear in the Vis app.** [Connect to a
  gateway](#connect-to-a-gateway-and-run-a-task).
- **Your program must work with sessions, Council, automations or settings.** [Find the calls for a
  feature](#find-the-calls-for-a-feature).

For a program in another language, read [HTTP API basics](http-api.md).

## Install the SDK

**The execution-layer and application-extension API on this page is unreleased.**
It requires an SDK and engine built from the same source revision. An older
engine cannot execute application callbacks, even if you update only the Python
package. Do not use these examples with an older published runtime.

You need Python 3.11 or newer. Published SDKs are installed with the command
below. Version `0.2.3` introduced the earlier Agent API, not the new API shown here:

```bash
python3 -m venv .venv
. .venv/bin/activate
python -m pip install --upgrade "vis-agent>=0.2.3"
```

The Python package does **not** install the engine. For local use on Linux or macOS, do these
steps:

1. Install the [Vis runtime](distributions.md).
2. Put `vis-agent` on `PATH`.
3. Configure a [provider and model](configuration.md).

A remote client needs only the Python package. Its provider runs on the gateway machine.

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
identical. Entering the context starts a private engine with no HTTP listener.
Leaving it stops that process and discards its temporary session database.
It does not undo file edits or isolate your credentials and configuration.

You should see a session ID, `Status: completed` and the answer's content blocks.
By default, `run()` returns a turn record, not a text string. A failed,
cancelled or suspended turn also returns a record: always check `status`.

## Get a validated result

When your program needs fields rather than a prose answer, define a Pydantic
model and pass it to `Agent.run()`. Pydantic is installed with `vis-agent`.
There is no separate extra to enable:

```python
# structured_quote.py
from pydantic import BaseModel, Field

from blockether.vis.engine import Agent


class Quote(BaseModel):
    cents: int = Field(ge=0)
    express: bool


def quote_delivery(agent: Agent) -> Quote:
    return agent.run(
        "For a 1000 gram parcel, quote 1200 cents with express false. "
        "Do not inspect or change files.",
        response_model=Quote,
    )


def main():
    with Agent(project=".") as agent:
        quote = quote_delivery(agent)
        print(quote.cents, quote.express)


if __name__ == "__main__":
    main()
```

Run `python structured_quote.py` in a project with a configured provider. A valid result prints
`1200 False`.

With `response_model=Quote`, `run()` does three things. It includes the model's validation JSON
Schema in the request. It checks the final prose block against that schema. Then it uses Pydantic to
run your Python validators and return a `Quote`. Without `response_model`, `run()` returns the
original turn record. Both modes work with a private local engine or a borrowed `GatewayClient`.

The schema is a **request to the model**, not provider-enforced JSON mode: Vis
currently uses plain-text completions. The result must be one complete JSON
value matching your model (an object for `Quote`), without a Markdown fence
or commentary.

### Handle invalid answers

If the answer is not valid JSON, `run()` asks the agent to correct it in the same conversation. It
does the same when the answer does not match the schema or fails one of your validators. The request
lists each problem with its location, where `$` is the whole value. It also repeats the schema. For
example, the answer `{"cents": -5, "express": "no"}` gets this feedback:

```text
- $.cents: does not satisfy minimum 0 (input: -5)
- $.express: expected boolean, got string (input: "no")
```

By default, `run()` sends up to two corrections. To change the limit, set `max_corrections`.
`max_corrections=0` accepts only the first answer. Each correction is another model call, which can
cost money. The `timeout` covers the first answer and all corrections.

The agent is asked to keep work it has already done. Corrections do not undo earlier tool calls and
file edits. Closing a private agent discards its session database, not those file edits.

`run()` raises `StructuredOutputError` in these cases:

- A turn fails, is cancelled or suspends.
- The answer is still invalid after the last correction.
- The timeout leaves no time for another correction.

Its message lists up to 10 problems, and its attributes show what went wrong:

| Attribute | Contents |
| --- | --- |
| `errors` | Problems in the last answer, each with a `path`, a `message` and a `source` |
| `attempts` | One entry per turn, with its `turn` record, final prose `answer` and `errors` |
| `turn` | The last turn record |

The `source` names the check that found the problem:

- `turn`: a turn that did not complete.
- `answer`: a turn without a final prose answer.
- `json`: text that is not one JSON value.
- `schema`: a schema mismatch.
- `pydantic`: your model's validators.

Messages quote short values from the answer, such as numbers and short strings, but never whole
objects or arrays. If your model sets Pydantic's `hide_input_in_errors`, messages quote no values.

## Use stdio from the Python SDK

The local `Agent` example above starts `vis-agent stdio` for you. This is a working transport for a
Vis process that Python owns. It is not a command to ask the agent a question at your terminal, and
it is not an MCP server. It does not start an HTTP gateway. The SDK and engine must come from
compatible revisions. The execution-layer API in this guide is not released yet.

To choose the executable yourself or share one local process between agents,
use `LocalEngine`:

```python
from blockether.vis.engine import Agent, LocalEngine

with LocalEngine(executable="vis-agent", root=".") as layer:
    with Agent(project=".", execution_layer=layer) as agent:
        result = agent.run("Summarize this project without changing files.")
        print(result["status"])
```

`LocalEngine` appends `stdio` to the executable command. It sets `VIS_DB_PATH` to an isolated
temporary SQLite database. It writes the engine's standard error to a file next to that database.
When its context closes, it stops its process and removes both files. If the engine fails, the
error message shows the engine's exit status and the end of its standard error.

Running a task can cost provider charges, and it gives the engine access to project files. If the
executable is not on `PATH`, pass its absolute launcher path to `executable`.

You can run `vis-agent stdio --help` without starting the transport. Direct
invocation requires `VIS_DB_PATH` set to an isolated, writable SQLite file
path. The process writes a protocol handshake and newline-delimited JSON
(NDJSON) replies to stdout, reads NDJSON requests from stdin, and exits when
stdin closes. Keep stdout free of other output. Use the SDK instead of typing
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

Do not print or commit the token. For a [remote
gateway](gateway-service.md#connect-from-another-machine), use its HTTPS origin. Get its token from
the operator through a secure channel. `VIS_PROJECT_ROOT` must be an **absolute path on the gateway
machine**, not a path on your laptop. Remote Agent rejects `.` and does not guess a server
directory.

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

The script reads the environment variables. Neither object discovers a gateway
or loads a local token. `GatewayClient` accepts an HTTP(S) **origin** with no
path prefix, query, fragment or URL credentials. TLS verification stays enabled
and redirects are refused. Transport options belong to the execution layer,
not to `Agent`.

An Agent borrows the execution layer you pass in. Closing it detaches its
application extensions. Closing the outer `GatewayClient` releases the client
lease and closes its streams. Neither action stops the gateway or deletes the
saved conversation. A running turn may continue, but it cannot call application
functions after their Agent closes.

The session uses the `app` channel, so it is visible in the app on that gateway.
Keep the printed session ID: to resume it later, open a
`GatewayClient(url, token=...)` context and use `client.session(session_id)`.
Each new Agent creates a new session. It does not implicitly resume an old one.

When the `with` block starts, `GatewayClient` calls `get_capabilities()` and checks the protocol. It
then sends the token with each request. The client also has one generated method for each gateway
route. The method name is the HTTP method and the words of the route path, for example
`get_session_usage()`. Each generated method returns the parsed JSON answer.

To follow the progress of a turn, read [Follow progress](python-sessions.md#follow-progress).

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
engine and can become model context. Do not return credentials or unrelated
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

`vis.Symbol` exposes the function's name, annotations and docstring to the agent. Here the callable
is `delivery_quote`, not `delivery.delivery_quote`, because `alias` identifies the extension, not a
function-name prefix. `prompt` explains when to use the function. `symbols` makes it callable.

Every exported function needs an Activity presentation. This quick calculation shows its price when
it completes and adds no running indicator. Failures keep their error details.

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

Closing the Agent detaches its extensions. Disconnecting or cancelling releases the engine's wait,
but it cannot stop a synchronous Python function that is already running in your process. A repeated
delivery of the same pending call uses its saved result and does not run the function again. This is
not an exactly-once guarantee across application restarts or new agent requests.

Client extensions support functions, bound methods and object namespaces declared
with `Symbol`, an explicit Activity for every exported method, and a static
`prompt`. For an object namespace, use `Symbol(object, name="inventory")` and
annotate its methods with `@vis.method(activity=...)`. See the
[extension API](extension-api.md). Host-only `activation`, `ctx`, `env`, providers,
op hooks, network filters, slash commands and callable prompts are rejected, not
silently ignored. Use an [engine-side extension](extending.md) for those features.
Its registration entry point is `vis.register_extension(...)`.

Arguments must be JSON data. Results can be JSON values, tuples or dataclass
instances. Tuples become lists and dataclass fields become ordinary data, keeping
`None` fields. Live object identity stays in your application, not in the returned
value. Async functions are awaited on the SDK calling thread, which must not
already be running an asyncio event loop.

## Handle failures and choose a lifecycle

| Situation | Meaning and next step |
| --- | --- |
| `ProtocolError` | SDK and gateway protocols disagree. Install compatible versions |
| `GatewayError` | Inspect `status` and `code` for authentication, permissions or request errors |
| `TransportError` | Read the engine's exit status and standard error in the message, if present. Then check the executable or gateway, network and TLS setup |
| `VisTimeout` from `run()` or `turn.wait()` | The wait ended, but the turn can still be running. Inspect it or call `turn.cancel()` |
| `StructuredOutputError` from `run()` | No valid structured result. Read its `errors` and `attempts` |
| A record whose `status` is not `completed` | The task did not complete normally. Inspect its content and input requirements |

A wait timeout is separate from the execution layer's transport `timeout`. A local pipe timeout
stops its engine. Leaving a default local Agent context also stops unfinished work. An Agent with a
borrowed layer leaves that layer running. A remote turn can outlive the client.

When you retry a submission, reuse the same explicit `idempotency_key`. A new key means a new
request.

### Catch gateway errors

```python
import os

from blockether.vis.engine import GatewayClient, GatewayError, ProtocolError, TransportError

try:
    with GatewayClient(os.environ["VIS_GATEWAY_URL"], token=os.environ["VIS_GATEWAY_TOKEN"]) as client:
        client.get_capabilities()
except GatewayError as error:
    print("HTTP error:", error.status, error.code)
except ProtocolError:
    print("Update the SDK or the gateway to compatible versions.")
except TransportError:
    print("The gateway is not reachable.")
```

`GatewayError` has `status` and `code`, but not the message of the gateway. `ProtocolError` and
`VisTimeout` are kinds of `TransportError`, so catch them first.

### Choose a lifecycle

| API | Use it for | Closing it |
| --- | --- | --- |
| `Agent(project=".")` | One local conversation, no gateway or HTTP listener | Stops its engine and discards session history |
| `Agent(project=..., execution_layer=layer)` | One conversation on a caller-owned local engine or gateway client | Detaches its application extensions, but leaves the layer and saved conversation open |
| `LocalEngine(executable=..., root=...)` | Several sessions in one owned stdio process | Stops that process and discards its session database |
| `GatewayClient(url, token=...)` | Persistent or shared sessions on a separately running gateway | Closes streams and releases its client lease |

For a launcher outside `PATH`, configure `LocalEngine(executable=..., root=...)`
and pass that layer to `Agent`. `LocalEngine` accepts a launcher path or argv list
and adds `stdio` itself. Use an outer `with LocalEngine(...) as layer:` context
to own its lifetime, as the gateway example does with `GatewayClient`.

Both implementations share the `ExecutionLayer` contract. `Agent` does not choose
a transport from a mixture of gateway and process options. Use the complete
installed wrapper, not a bare native binary without its Python sidecar. Each
client and its session handles use one calling thread.
`conversation.delete()` is a separate, destructive operation.

## Find the calls for a feature

Each concept page has a Python page with the same operations as its HTTP page.

| Concept | Python page |
|---|---|
| [Sessions](sessions.md) | [Sessions in Python](python-sessions.md) |
| [Context management](context-management.md) | [Context management in Python](python-context-management.md) |
| [Project instructions](project-instructions.md) | [Project instructions in Python](python-project-instructions.md) |
| [Drafts](drafts.md) | [Drafts in Python](python-drafts.md) |
| [Council](council.md) | [Council in Python](python-council.md) |
| [Automations](automations.md) | [Automations in Python](python-automations.md) |
| [Configuration](configuration.md) | [Configuration in Python](python-configuration.md) |

## See also

- [HTTP API basics](http-api.md) — the same gateway from any language.
- [Sessions in Python](python-sessions.md) — send messages, follow progress and manage sessions.
- [Running a gateway](gateway-service.md) — install and secure a shared agent service.
- [Decision models](decision-models.md) — download Laya, train both heads and publish a verified FP32 version.
- [Native builds for JVM extensions](jvm-native-image.md) — rebuild Vis only when adding Java/Clojure capabilities.
- [Extending Vis](extending.md) — add tools inside the agent.
