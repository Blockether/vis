# Python SDK basics

Use `Agent()` to embed Vis in your program and run tasks in your current project. To use a gateway
that runs separately, give `Agent()` a `GatewayClient`. Add the functions of your application with
`extensions=[...]`. They keep access to your Python objects in both modes. Both modes have
`run()`, `send()`, and one conversation for follow-up requests.

For classes, methods, signatures, and type annotations, browse the
[generated Python SDK API reference](https://vis.blockether.com/python-sdk-api/). The reference
follows `main`, so it can show APIs that the newest PyPI release does not have.

## When to use

- **Your script or application must give Vis a task in your project and use the answer.** Start
  with [a private agent](#let-your-program-own-a-private-agent) that your program owns.
- **Your program needs fields that it can check, not prose.** Pass a Pydantic model to get a
  [validated result](#get-a-validated-result).
- **The agent needs the rules, data, or services of your application.**
  [Give it your functions](#give-the-agent-your-functions).
- **Conversations must outlive your script or appear in the Vis app.**
  [Connect to a gateway](#connect-to-a-gateway-and-run-a-task).
- **The agent must not use some tools, such as `py.repl_start`.** Pass `tools` to `Agent()`, as
  [Let your program own a private agent](#let-your-program-own-a-private-agent) shows.
- **The first `Agent()` takes a long time or raises `VisTimeout`.** Read
  [The first start of a private agent](#the-first-start-of-a-private-agent).
- **Your scripts run many short tasks and each start of the engine takes time.**
  [Share one local gateway](#share-one-local-gateway-between-your-programs).
- **Your application must refuse some tool calls, such as `git push`.**
  [Refuse a tool call from your application](#refuse-a-tool-call-from-your-application).
- **Your program must find the session that it created or show events while Vis works.**
  [List sessions and follow events](#list-sessions-and-follow-events).
- **Your program must work with sessions, Council, automations, or settings.** Read
  [Sessions API](sessions-api.md), [Council API](council-api.md),
  [Automations API](automations-api.md), or [Configuration API](configuration-api.md).

For a program in another language, read [HTTP API](http-api.md).

## Install the SDK

You need Python 3.12 or newer on Linux or macOS:

```bash
python3 -m venv .venv
. .venv/bin/activate
python -m pip install --upgrade vis-agent
```

You do not have to install Vis first. When your program starts a private agent, the SDK uses the
Vis engine of its own release:

- If the installed `vis-agent` runs that release, the SDK uses it.
- If not, the SDK downloads that release one time into `~/.vis/sdk` and uses that copy.

The download has only the engine and its Python runtime, not the terminal, web, or desktop app.
The SDK checks the download against the release checksums. It does not change your shell profile
or your installed Vis. Only the first run needs network access. To turn off the download, set
`VIS_NO_AUTO_INSTALL=1`.

To download the engine ahead of time, for example in a container image, call `install()`:

```python
from blockether.vis.engine import ensure_installed, install

print(install())  # downloads now and prints the path of the vis-agent command
print(ensure_installed())  # the command that a private agent uses, downloaded if missing
```

`install()` always uses the copy in `~/.vis/sdk`, also when you have Vis installed. Pass
`force=True` to download the engine again. Both functions raise `InstallError` when the download
fails.

### The first start of a private agent

The first `Agent()` with a new engine copy takes longer than the next ones. On entry to its
context, `Agent()` does these steps:

1. It downloads the engine if no matching copy is installed. The copy uses about 420 MB of disk
   space. Each network read must continue within 120 seconds.
2. It starts `vis-agent stdio` and waits up to `startup_timeout` for the engine to answer. The
   default is 120 seconds.
3. It creates the session. The engine prepares each installed Python extension one time for each
   version of its bundled Python: it installs the dependencies and compiles the code. This step can
   take more than one minute. `Agent()` waits up to `startup_timeout` for it. A different engine
   copy with the same bundled Python, for example a second virtual environment, uses the prepared
   extensions again.

A project with its own extensions can need this preparation one time too. When the engine does not
answer in time, `Agent()` raises `VisTimeout` and stops the engine. To wait longer, give a
`LocalEngine` with a longer `startup_timeout`:

```python
from blockether.vis.engine import Agent, LocalEngine

with LocalEngine(root=".", startup_timeout=300) as layer:
    with Agent(project=".", execution_layer=layer) as agent:
        print(agent.run("Summarize this project without changing files.")["status"])
```

To start the engine one time for many runs of your programs, use
[a shared local gateway](#share-one-local-gateway-between-your-programs).

Before the first task, configure a [provider and model](configuration.md). A gateway client needs
only the Python package, because the provider runs on the gateway machine.

A request can cost model charges, and it uses the tools and the files of the engine. Choose an
account and an [access policy](jail.md) before you run a task. Keep credentials and engine state
outside the project that the agent inspects.

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

You see a session ID, `Status: completed`, and the content blocks of the answer.

`.` is the current directory when you construct the agent, and `Agent()` means the same. On entry
to the context, a private engine starts, without an HTTP listener. On exit, the engine stops and
discards its temporary session database. The exit does not undo file edits. A private agent does
not isolate your credentials or your configuration.

By default, `run()` returns a turn record, not a text string. A failed, cancelled, or suspended
turn also returns a record, so always check `status`.

The answer is in `content`, a list of blocks, and the record has no `answer` field. For the answer as
text, call `final_answer(result)` from `blockether.vis.engine`. It returns the Markdown of the last
`prose` block, or `None` when the turn has no answer.

To continue the conversation, call `agent.run()` again on the same `Agent`. Each call is a new turn
in the same session. The agent sees your earlier messages, its answers, and its tool results:

```python
# conversation.py
from blockether.vis.engine import Agent, final_answer


def main():
    with Agent(project=".") as agent:
        for message in [
            "List the files in this folder. Do not change any file.",
            "Which of those files is the largest?",
        ]:
            result = agent.run(message)
            if result["status"] != "completed":
                raise SystemExit(f"Turn ended with status {result['status']}")
            print(final_answer(result))


if __name__ == "__main__":
    main()
```

The second answer names one of the files from the first answer. To test such a conversation on
many tasks and get a score, read
[Write a task with several turns](evals.md#write-a-task-with-several-turns).

To turn off some tools, pass `tools`. For example,
`Agent(tools=["-py.repl_start", "-py.repl_stop"])` turns off those two tools of the `py` extension.
[Choose single tools](configuration.md#choose-single-tools) describes the names and the rules. To
change the list later, call `agent.session.update(tools=[...])`.

To pin the model of the conversation, give `provider` and `model` together, for example
`Agent(provider="anthropic", model="claude-sonnet-4-5")`. Give both or neither, or `Agent()` raises
`ValueError`. A `provider` and a `model` option of one `run()` or `send()` call change the model of
that turn only. [Choose the model of a session](sessions-api.md#choose-the-model-of-a-session)
lists the models.

## Get a validated result

Define a Pydantic model and pass it to `Agent.run()`. Pydantic comes with `vis-agent`:

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

With `response_model=Quote`, `run()` does three things:

1. It includes the validation JSON Schema of `Quote` in the request.
2. It checks the final prose block against that schema.
3. It runs your Python validators with Pydantic and returns a `Quote`.

Without `response_model`, `run()` returns the turn record. Both forms work with a private local
engine and with a borrowed `GatewayClient`.

The schema is a request to the model, not a JSON mode that the provider enforces. Vis uses
plain-text completions. The answer must be one complete JSON value that matches your model, an
object for `Quote`, with no Markdown fence and no commentary.

### Handle invalid answers

`run()` asks the agent for a correction, in the same conversation, in three cases. The answer is
not valid JSON, it does not match the schema, or it fails one of your validators. The correction
request lists each problem with its location, where `$` is the whole value. For example, the answer `{"cents": -5, "express": "no"}` gets this feedback:

```text
- $.cents: does not satisfy minimum 0 (input: -5)
- $.express: expected boolean, got string (input: "no")
```

By default, `run()` sends up to two corrections. `max_corrections` changes the limit, and
`max_corrections=0` accepts only the first answer. Each correction is one more model call, and a
model call can cost money. The `timeout` covers the first answer and all corrections.

A correction does not undo tool calls or file edits.

`run()` raises `StructuredOutputError` in these cases:

- A turn fails, is cancelled, or suspends.
- The answer is still invalid after the last correction.
- The timeout leaves no time for another correction.

The error message lists up to 10 problems. The error has these attributes:

| Attribute | Contents |
| --- | --- |
| `errors` | The problems in the last answer, each with a `path`, a `message`, and a `source` |
| `attempts` | One entry for each turn, with its `turn` record, its final prose `answer`, and its `errors` |
| `turn` | The last turn record |

The `source` names the check that found the problem:

| `source` | Problem |
| --- | --- |
| `turn` | A turn that did not complete |
| `answer` | A turn without a final prose answer |
| `json` | Text that is not one JSON value |
| `schema` | A schema mismatch |
| `pydantic` | A failure in the validators of your Pydantic model |

Messages quote short values from the answer, such as numbers and short strings, but never whole
objects or arrays. If your Pydantic model sets `hide_input_in_errors`, messages quote no values.

## Use stdio from the Python SDK

The local `Agent` example starts `vis-agent stdio` for you. In this mode, Python owns a private Vis
process and talks to it over standard input and output. The process starts no HTTP gateway. It is
not an MCP server or a terminal chat.

To choose the executable yourself, or to share one local process between agents, use
`LocalEngine`:

```python
from blockether.vis.engine import Agent, LocalEngine

with LocalEngine(executable="vis-agent", root=".") as layer:
    with Agent(project=".", execution_layer=layer) as agent:
        result = agent.run("Summarize this project without changing files.")
        print(result["status"])
```

`LocalEngine` gives the process a temporary session database and a log file for its standard
error. When the context closes, `LocalEngine` stops the process and removes both files. If the
engine fails, the error shows the exit status and the end of the log.

With the default `executable="vis-agent"`, the SDK uses the installed command or
[downloads it](#install-the-sdk). To use another Vis build, pass the absolute path of its
`vis-agent` launcher, not the bare native binary. The launcher also loads the bundled Python
runtime. A start or a stop of this process does not affect a running gateway.

## Share one local gateway between your programs

Each `Agent()` without an execution layer starts its own engine, which takes about 1.5 seconds.
When your scripts run many short tasks, use `LocalService.ensure()`. It starts one local gateway
the first time and connects to it in milliseconds after that:

```python
from blockether.vis.engine import Agent, LocalService

with LocalService.ensure() as layer:
    with Agent(project=".", execution_layer=layer) as agent:
        print(agent.run("Summarize this project without changing files.")["status"])
```

`LocalService.ensure()` returns a connected `GatewayClient`. Programs and threads that call it at
the same time get the same gateway, because a file lock lets only one of them start it.

The gateway has these properties:

- It keeps its database, token, runtime folder, logs, and push state in `home`. The default is
  `$VIS_HOME/sdk/service`.
- It uses the provider settings of `VIS_HOME`. It does not use the session database, the gateway,
  or the paired devices of your own Vis. Its turns send no notifications to your phone or browser.
- It listens only on 127.0.0.1, on a free port, and requires its token.
- It stops when no client holds a lease and no turn runs. A client that you do not close keeps its
  lease for 2 minutes after its process ends. A gateway that no client used stops after 30 seconds.
- Its sessions stay in its database. The next `ensure()` starts the gateway again and finds them.

Keep one client open while you work to keep the gateway ready. `LocalService.discover()` returns a
client of the running gateway, or `None`, and never starts one. `LocalService.stop()` stops the
gateway for all programs that use it, including their running turns, and returns `True` when a
gateway ran. All three methods take `home`. `LocalService` works on Linux and macOS.

`ensure()` takes the same `executable` as `LocalEngine`, a request `timeout` of 30 seconds, and a
`startup_timeout` of 120 seconds. When the gateway does not answer in `startup_timeout`, it raises
`VisTimeout`. When the engine cannot start, it raises `TransportError` with the end of the log.
When a newer SDK uses another engine, `ensure()` replaces the old gateway only when no client
uses it and no turn runs.

## Connect to a gateway and run a task

Use a gateway when conversations must survive your script or appear in the Vis app. The client
machine needs only the Python SDK, not a local engine.

Do not print or commit the token. [Start a gateway](gateway-service.md#start-a-local-gateway).
Then set these variables in your private terminal. The values are for a local installation with
the default state:

```bash
export VIS_GATEWAY_URL=http://127.0.0.1:7890
export VIS_GATEWAY_TOKEN="$(cat "$HOME/.vis/gateway.token")"
export VIS_PROJECT_ROOT="$PWD"
```

For a [remote gateway](gateway-service.md#connect-from-another-machine), use its HTTPS origin, and
get its token from the operator through a secure channel. `VIS_PROJECT_ROOT` must be an absolute
path on the gateway machine. A remote `Agent` rejects `.`.

Save this as `gateway_task.py` and run `python gateway_task.py`:

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

Neither object discovers a gateway or loads a local token. `GatewayClient` accepts an HTTP or HTTPS origin with no path prefix, query, fragment,
or URL credentials. TLS verification stays on, and the client refuses redirects. Transport options
belong to the execution layer, not to `Agent`.

When the `with` block starts, `GatewayClient` calls `get_capabilities()` and checks the protocol.
It then sends the token with each request. The client also has one generated method for each
gateway route. The method name is the HTTP method and the words of the route path, for example
`get_session_usage()`. Each generated method returns the parsed JSON answer.

The session uses the `app` channel, so the app on that gateway shows it. Each new `Agent` creates a
new session. Keep the printed session ID. To resume the session
later, open a `GatewayClient(url, token=...)` context and call `client.session(session_id)`.

To follow the progress of a turn, read [Follow progress](sessions-api.md#follow-progress).

## List sessions and follow events

Save this as `session_events.py`. Both functions work with a `GatewayClient` and with the
execution layer of an `Agent`:

```python
# session_events.py
def recent_session_ids(layer):
    page = layer.list_sessions(limit=20, include_empty="true")
    return [row["id"] for row in page["sessions"]]


def print_events(conversation, seconds=30):
    with conversation.events(idle_timeout=seconds) as events:
        for event in events:
            print(event.type)
```

`recent_session_ids(agent.execution_layer)` returns the IDs of up to 20 recent sessions. Without
`include_empty="true"`, the list leaves out a session that has no turns yet, also the session of
your `Agent`.

`print_events(agent.session)` prints the type of each event of the session. The loop ends normally
when no event comes for 30 seconds. To stop a stream from another thread, call its `close()` method.
In a coroutine, iterate the stream with `async for`.

[Find a saved session](sessions-api.md#find-a-saved-session) gives the list options.
[Stop following events](sessions-api.md#stop-following-events) gives the stream options and an
`async` example.

### What a close or a gateway restart changes

[Choose a lifecycle](#choose-a-lifecycle) lists what each close does. A close of an `Agent` or a
`GatewayClient` does not stop the gateway or delete the saved conversation. A running turn can continue, but it
cannot call application functions after their `Agent` closes.

A `GatewayClient` continues to work when the gateway restarts. When the gateway no longer holds
the client lease, the client takes a new lease.
Application extensions cannot move to a new lease. A client with an `Agent` that has extensions
then raises `TransportError` for each request. Create a new client and a new `Agent`.

## Give the agent your functions

Pass an `Extension` to your `Agent` to let it use business rules, query your data, or call your
services. You need no `.vis/extensions/` file and no global registration call. The extension
belongs to the conversation of that `Agent`, not to other agents that share its execution layer.

Your functions run in your application process, on the SDK calling thread. They can capture
existing objects, such as a database client or an in-memory list. The same is true with a remote
gateway, because Vis uploads no function code, closures, or local objects. Install their
dependencies in the Python environment of your application.

These functions run with the permissions of your application, outside the sandbox of the model.
Review what they expose. Arguments and returned data cross to the engine and can become model
context. Do not return credentials or unrelated private data.

### Register a function

Save this application as `delivery_task.py`. Its closure records the quoted weights in a list that
the application owns:

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

`vis.Symbol` gives the agent the name, the annotations, and the docstring of the function. The
callable is `delivery_quote`, not `delivery.delivery_quote`. The `alias` identifies the extension
and is not a prefix of function names. `prompt` explains when to use the function, and `symbols`
makes it callable.

Every exported function needs an Activity presentation. This short calculation shows its price
when it completes and adds no running indicator. A failure keeps its error details.

### Ask the agent to use it

This run is a model request and can cost provider charges. Run `python delivery_task.py` from your
project. The function returns 1200 cents, and the Activity reads **Quote delivery · 1200 cents**.
After one call, your application prints `Quoted weights: [1200]`.

The agent finds the contract through `apropos` and `doc`, calls the function through
`python_execution`, and uses the result in the answer.

### Add or change a function between turns

To add an extension after you enter the `Agent` context, call `agent.register_extension(extension)`.
To replace the static prompt of an extension, call `agent.update_extension_prompt(name, prompt)`.
`prompt=None` removes the prompt, and the functions of the extension stay the same:

```python
with Agent(project=".") as agent:
    agent.run("Read the project layout. Do not change files.")
    agent.register_extension(make_delivery_extension(quoted_weights))
    agent.update_extension_prompt("delivery", "Quote only standard delivery.")
    agent.run("Quote delivery for a 1200-gram parcel.")
```

Call both methods only while no turn runs. The new functions and the new prompt apply from the next
turn. During a turn, both methods raise `GatewayError` with the status 409 and the code
`client_extension_busy`. `update_extension_prompt()` raises `ValueError` when no registered extension
has that name.

For a remote agent, pass the same extension object to
`Agent(execution_layer=execution_layer, extensions=[extension], ...)` inside the gateway context.

### When your functions run

Your functions run while your program drives the SDK. `run()`, a turn wait, a session or turn
read, and event iteration each service the pending calls. `send()` alone starts no background
thread for your application code. While the agent needs its functions, keep driving the SDK and
keep the application alive.

A disconnect or a cancel releases the wait of the engine, but it cannot stop a synchronous Python
function that already runs in your process. A repeated delivery of the same pending call uses its
saved result and does not run the function again. That is not an exactly-once guarantee across
application restarts or new agent requests.

An async function is awaited on the SDK calling thread. That thread must not already run an
asyncio event loop.

### Refuse a tool call from your application

Add a `vis.OpHook` to a client extension when your application must refuse some tool calls. The
function runs before the call and returns `None` to allow it or `vis.block(reason)` to refuse it.
The model sees the reason as a tool failure. The function cannot change the arguments or the
result.

```python
import blockether.vis.extension as vis


def no_push(call):
    command = call["args"][0] if call["args"] else ""
    if "git push" in str(command):
        return vis.block("This application does not allow git push.")
    return None
```

To use it, add `op_hooks=[vis.OpHook(["shell"], no_push)]` to the `vis.Extension` that
`make_delivery_extension` returns.

The function receives `{"op", "args"}`, as [op hooks](extension-api.md#op-hooks) describe. It runs
in your application process, on the SDK calling thread, while your program drives the SDK. Vis
refuses the call in these cases:

- The function raises an exception.
- Your application detaches.
- The function does not answer in 300 seconds.

Name an operation, such as `"shell"`, or a pattern with a prefix, such as `"linear.*"`. The gateway
refuses a pattern such as `"*"` with the status 400 and the code `client_gate_too_broad`. Only
`phase="before"` is available. A client extension still needs at least one function in `symbols`.

### What a client extension can declare

A client extension declares functions, bound methods, and object namespaces with `Symbol`. It
needs an explicit Activity for every exported method, and it can have a static `prompt`. For an
object namespace, use `Symbol(object, name="inventory")` and annotate its methods with
`@vis.method(activity=...)`. The [extension API](extension-api.md) has the details.

Vis rejects the host-only features: `activation`, `ctx`, `env`,
providers, op hooks with `phase="after"`, network filters, slash commands, and callable prompts. For those features,
write an [engine-side extension](extending.md). Its registration entry point is
`vis.register_extension(...)`.

Arguments must be JSON data. A result can be a JSON value, a tuple, or a dataclass instance. A
tuple becomes a list, and dataclass fields become ordinary data that keeps `None` fields. The
identity of a live object stays in your application.

## Handle failures and choose a lifecycle

| Situation | Meaning and next step |
| --- | --- |
| `ProtocolError` | The SDK and the gateway have no common protocol. Install compatible versions |
| `GatewayError` | Read `status` and `code` for an authentication, permission, or request error |
| `TransportError` | Read the exit status and the standard error of the engine in the message, if present. Then check the executable or the gateway, the network, and the TLS setup |
| `VisTimeout` from `run()` or `turn.wait()` | The wait ended, but the turn can still run. Inspect the turn or call `turn.cancel()` |
| `VisTimeout` when a private `Agent` starts | The engine did not answer in `startup_timeout`. Read [The first start of a private agent](#the-first-start-of-a-private-agent) |
| `StructuredOutputError` from `run()` | No structured result was valid. Read its `errors` and `attempts` |
| A record whose `status` is not `completed` | The task did not complete normally. Inspect its content and its input requirements |

A wait timeout is separate from the transport `timeout` of the execution layer. A local pipe
timeout stops its engine. The exit from a default local `Agent` context also stops unfinished work.
An `Agent` with a borrowed layer leaves that layer running. A remote turn can outlive the client.

When you retry a submission, send the same explicit `idempotency_key`. A new key means a new
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

| API | Use it for | When it closes |
| --- | --- | --- |
| `Agent(project=".")` | One local conversation, with no gateway and no HTTP listener | It stops its engine and discards the session history |
| `Agent(project=..., execution_layer=layer)` | One conversation on a local engine or a gateway client that the caller owns | It detaches its application extensions. The layer and the saved conversation stay open |
| `LocalEngine(executable=..., root=...)` | Several sessions in one stdio process that you own | It stops that process and discards its session database |
| `LocalService.ensure()` | Many runs of your programs on one local gateway that starts one time | It releases its lease. The gateway stops after its last lease |
| `GatewayClient(url, token=...)` | Persistent or shared sessions on a gateway that runs separately | It closes its streams and releases its client lease |

Pass a `LocalEngine` or a `GatewayClient` when the default private engine does not fit. Each layer
and its session handles use one calling thread. Only the `close()` of an event stream is safe from
another thread. To delete a saved conversation, call `conversation.delete()`.

Always close an `Agent` or a layer, for example with `with`. If you do not close a `LocalEngine`,
its process stops only when Python collects the object or exits.

## See also

- For the same gateway from any language, read [HTTP API](http-api.md).
- To send messages, follow progress, and manage sessions, read [Sessions API](sessions-api.md).
- To install and secure a shared agent service, read [Running a gateway](gateway-service.md).
- To choose a provider and a model before the first task, read [Configuration](configuration.md).
- To add tools inside the agent, read [Extending Vis](extending.md).
