# Python SDK

Use the Python SDK to put Vis inside your script, web service or background
worker. You can connect to a shared gateway or let your program own a private
agent process. Both give you sessions, turns and progress events through the
same synchronous API.

## Choose how the agent runs

| You want to… | Use | Who owns the process? |
| --- | --- | --- |
| Share sessions with the Vis app or other programs | `GatewayClient` | You or a service manager run the gateway separately |
| Run one isolated session database for a job | `LocalEngine` | Your Python program starts and stops a private engine |
| Add a tool the agent can call | The [extension API](extending.md) | Vis loads your Python extension |

The SDK is a client, not a model provider. Configure a provider and model on the
machine running Vis first. A request can incur model charges and use that
machine's tools and files; use a suitable account and
[access policy](jail.md), even when the request asks only for a summary.

## Install the SDK

You need Python 3.11 or newer:

```bash
python3 -m venv .venv
. .venv/bin/activate
python -m pip install vis-agent
```

For an existing uv project, use `uv add vis-agent` instead. The
[PyPI package](https://pypi.org/project/vis-agent/) installs the Python API. It
does **not** install the `vis-agent` executable, download an engine or start a
gateway. Install the [Vis runtime](distributions.md) separately for local use;
a client connecting to a remote gateway needs only the Python package.

## Connect to a gateway and run a task

First [start a gateway](gateway-service.md#start-a-local-gateway). For a local
gateway using the default state directory, prepare these variables in your
private terminal. Do not print the token or commit it to a file:

```bash
export VIS_GATEWAY_URL=http://127.0.0.1:7890
export VIS_GATEWAY_TOKEN="$(cat "$HOME/.vis/gateway.token")"
export VIS_PROJECT_ROOT="$PWD"
```

For a remote gateway, use its HTTPS origin and a token supplied securely by its
operator. `VIS_PROJECT_ROOT` must be an existing directory **on the gateway
machine**, such as `/srv/vis-project`, not a directory on your laptop. See
[remote access](gateway-service.md#connect-from-another-machine).

Save this as `gateway_task.py`. The `run_task` function is a small agent wrapper:
your application passes a client, a workspace and a request, then receives the
session ID and the settled turn record.

```python
# gateway_task.py
import json
import os

from blockether.vis.engine import GatewayClient


def run_task(client, project, request):
    conversation = client.create_session(
        root=project, title="SDK task", channel="app"
    )
    print(f"Session: {conversation.id}")
    turn = conversation.send(request)
    result = turn.wait(timeout=300)
    if result["status"] != "completed":
        raise RuntimeError(f"Turn {turn.id}: {result['status']}")
    return conversation.id, result


def main():
    with GatewayClient(
        os.environ["VIS_GATEWAY_URL"],
        token=os.environ["VIS_GATEWAY_TOKEN"],
    ) as client:
        _, result = run_task(
            client,
            os.environ["VIS_PROJECT_ROOT"],
            "Summarize this project without changing files.",
        )
        print(json.dumps(result["content"], indent=2))


if __name__ == "__main__":
    main()
```

Run `python gateway_task.py`. You should see a session ID followed by the turn's
content blocks. The result is a record, not a plain answer string: inspect its
`status` and `content`. The `app` channel makes the session available in the app's
session list on the same gateway.

The example reads environment variables itself. `GatewayClient` does not read
them, discover a local gateway or load its token for you. Pass an HTTP or HTTPS
**origin**, without a path prefix, query, fragment or credentials in the URL.
TLS verification stays enabled and redirects are refused.

## Let your program own a private agent

Use `LocalEngine` for a command-line tool or job that should not attach to a
shared gateway. It works on Linux and macOS and talks to a child process over
standard input and output, with no HTTP listener.

Install the [complete runtime](distributions.md), then set its launcher path:

```bash
export VIS_EXECUTABLE="$HOME/.local/bin/vis-agent"
```

Save this beside `gateway_task.py` as `local_task.py`. It writes `session.md` in
the current directory, replacing that file if it already exists.

```python
# local_task.py
import os
from pathlib import Path

from blockether.vis.engine import LocalEngine
from gateway_task import run_task


def main():
    project = Path.cwd().resolve()
    with LocalEngine(
        executable=os.environ["VIS_EXECUTABLE"],
        root=project,
        startup_timeout=120,
    ) as engine:
        session_id, _ = run_task(
            engine, str(project), "Summarize this project without changing files."
        )
        transcript = engine.session(session_id).transcript(format="markdown")
        Path("session.md").write_bytes(transcript.content)


if __name__ == "__main__":
    main()
```

Run `python local_task.py`. The engine uses a temporary session database, so
export anything you want to keep **before** leaving the `with` block. Closing
it stops the owned process and removes that database. It does not undo file
edits or isolate all configuration: the child inherits your environment,
provider credentials and extension configuration.

`executable` also accepts an argument list for a custom launcher. Do not append
`sdk-stdio`; `LocalEngine` adds it. Use the installed `vis-agent` wrapper rather
than copying `vis-agent-native` alone: native releases need their bundled Python
runtime. The same API can launch a [JVM development runtime](distributions.md#native-vs-jvm).

## Show progress while a turn runs

Instead of waiting immediately after `conversation.send(...)`, pass the session
and returned turn to this function. It prints event types as they arrive and
returns the settled record when that turn finishes:

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

Start at `turn.cursor`, which was captured before submission, so fast events are
not missed. An event stream follows the **session**, not just one turn, and does
not close automatically when a turn finishes. Closing a stream does not cancel
the turn. For reconnecting consumers, save `events.cursor`; the SDK updates it
from replay and subscription reset messages.

Use `event.activity` and `event.view` for structured progress and controls.
If a task needs a person's answer, show `conversation.input_views()` and submit
chosen values with `conversation.answer(view_id, values)`. Live controls use
`conversation.live_views()` and `conversation.view_action(view_id, action, ...)`.
Do not automatically approve requests for credentials or permission. See
[Forms and user input](human-input.md) and [Live views](live-views.md).

## Resume work and handle failures

Keep the session ID to resume a gateway conversation with
`client.session(session_id)`. Call `conversation.read()` for its state,
`conversation.turns()` for turns or `conversation.send(...)` for a follow-up.
`client.list_sessions()` returns one page; use its `next_cursor` to request more.

| Situation | What to do |
| --- | --- |
| `ProtocolError` during connection | Install compatible SDK and gateway versions; do not bypass the handshake |
| `GatewayError` | Inspect `status` and `code`; check authentication, permissions or the request |
| `TransportError` | Check the URL, TLS, network and running gateway |
| `VisTimeout` from `turn.wait(...)` | Waiting stopped; the remote turn may still run. Keep its ID to inspect it or call `turn.cancel()` explicitly |
| A settled turn is not `completed` | Inspect its status and content; `wait()` does not turn a failed task into an exception |

The client's `timeout` controls transport waits, including idle event streams;
it is separate from the deadline passed to `turn.wait`. A local pipe timeout
closes the owned engine to prevent a late response being used for another call.
For a retried submission, reuse the same explicit `idempotency_key` in
`conversation.send(...)`; a new key means a new request.

Use one calling thread per client and its session handles. Give each worker its
own client. Closing `GatewayClient` closes streams and releases its client lease;
it does not delete sessions, cancel work or explicitly stop the gateway. An
idle, automatically managed gateway may exit after its last client leaves;
a foreground service stays running. `conversation.delete()` is a separate,
destructive operation.

## See also

- [Running a gateway](gateway-service.md) — install, secure and supervise a shared agent service.
- [Java and Clojure SDK](jvm-sdk.md) — call Vis from a JVM application.
- [Building the native binary](jvm-native-image.md) — build and package a native runtime for your wrapper.
- [Extending Vis](extending.md) — add tools inside the agent rather than control it from outside.
