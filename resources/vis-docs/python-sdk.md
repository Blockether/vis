# Python SDK

Use `Agent()` to run tasks in your current project without a gateway, or pass
`gateway_url` to use a separately running gateway. Both modes provide `run()`,
`send()` and one conversation for follow-up requests.

## Install the SDK

You need Python 3.11 or newer. `Agent` is new on `main`; PyPI version `0.2.2`
contains `GatewayClient` and `LocalEngine`, but not `Agent`. To use every example
on this page, install the SDK from source:

```bash
python3 -m venv .venv
. .venv/bin/activate
python -m pip install "git+https://github.com/Blockether/vis.git#subdirectory=packages/vis-agent"
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
