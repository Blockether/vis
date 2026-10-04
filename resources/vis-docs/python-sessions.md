# Sessions in Python

Create [sessions](sessions.md), send messages and follow the work from a Python program. Typed
calls return checked Python records. Each generated `GatewayClient` method sends one gateway request
and returns the parsed JSON answer.

## When to use

- **Your program must start work and wait for the answer.** [Create a
  session](#create-a-session). Then [send a message and wait](#send-a-message-and-wait).
- **Your program must show progress while Vis works.** [Follow progress](#follow-progress).
- **A turn waits for a person.** [Answer a form](#answer-a-form).
- **Your program must stop work or change waiting messages.** [Queue and cancel
  messages](#queue-and-cancel-messages).
- **Your program must find, copy or sort saved sessions.** [Find a saved
  session](#find-a-saved-session), [fork a session](#fork-a-session) or [organize sessions into
  groups](#organize-sessions-into-groups).
- **Your program must keep a record of the work.** [Export a session](#export-a-session) or [read
  Activity history](#read-activity-history).

To learn what a session is and how to use it in the terminal or the app, read
[Sessions](sessions.md). To send the same requests from another language, read [Sessions over
HTTP](http-sessions.md).

## Before you start

[Install the SDK](python-sdk.md#install-the-sdk). Then set `VIS_GATEWAY_URL`, `VIS_GATEWAY_TOKEN`
and `VIS_PROJECT_ROOT` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task).

```python
import os

from blockether.vis.engine import GatewayClient

with GatewayClient(os.environ["VIS_GATEWAY_URL"], token=os.environ["VIS_GATEWAY_TOKEN"]) as client:
    for row in client.list_sessions(limit=5)["sessions"]:
        print(row["id"], row["title"])
```

The next examples are calls on this `client` inside the `with` block. A `session` is the handle that
`client.create_session()` or `client.session(session_id)` returns.

## Operations

Typed calls check the answers and return records. Generated methods return the JSON answer of one
request. [Sessions over HTTP](http-sessions.md) lists the same operations as HTTP requests.

| Task | Typed call | Generated method |
|---|---|---|
| Create a session | `client.create_session(...)` | `post_sessions(body=...)` |
| Read a session | `session.read()` | `get_session(sid)` |
| Send a message | `session.send(request)` | `get_session_seq(sid)`, then `post_session_turns(sid, body=...)` |
| Read a turn | `turn.read()`, `turn.wait()` | `get_session_turn(sid, tid)` |
| Follow events | `session.events(cursor=...)` | `get_session_events_since(sid, query=...)` |
| List open forms | `session.input_views()` | `get_session_views_input(sid)` |
| Answer a form or interrupt a view | `session.answer(...)`, `session.view_action(...)` | `post_session_view(sid, view_id, body=...)` |
| List turns or queued messages | `session.turns(...)` | `get_session_turns(sid, query=...)` |
| Change a queued message | — | `patch_session_turn(sid, tid, body=...)` |
| Remove a queued message | — | `delete_session_turn(sid, tid)` |
| Cancel a turn | `turn.cancel()` | `post_session_turn_cancel(sid, tid)` |
| Cancel the running turn without its ID | — | `post_session_cancel_current(sid, body=...)` |
| Start the oldest queued message | — | `post_session_drain_queue(sid)` |
| Resume a paused queue | — | `post_session_resume_queue(sid)` |
| Search sessions | — | `get_sessions_search(query=...)` |
| List sessions | `client.list_sessions(...)` | `get_sessions(query=...)` |
| List the turns that a fork can keep | — | `get_session_forks(sid)` |
| Fork a session | — | `post_session_forks(sid, body=...)` |
| List groups | — | `get_session_groups(query=...)` |
| Create a group | — | `post_session_groups(body=...)` |
| Change or archive a group | — | `patch_session_group(gid, body=...)` |
| Delete a group | — | `delete_session_group(gid, query=...)` |
| Move a session to a group | — | `put_session_group(sid, body=...)` |
| Rename, star or archive a session | `session.update(...)` | `patch_session(sid, body=...)` |
| Delete a session | `session.delete()` | `delete_session(sid)` |
| Export a transcript | `session.transcript(format=...)` | `get_session_transcript(sid)`, `get_session_transcript_md(sid)`, `get_session_transcript_html(sid)` |
| List live views | `session.live_views()` | `get_session_views_live(sid)` |
| Read a page of a log node | — | `get_session_views_live_log(sid, view_id, node_id, query=...)` |
| Read one page of Activity history | — | `get_session_activity(sid, aid, query=...)` |
| Export the full Activity history | — | `get_session_activity_export(sid, aid, query=...)` |

## Create a session

```python
session = client.create_session(**client.session_options(os.environ["VIS_PROJECT_ROOT"]))
print(session.id)
```

`session_options()` checks the project path and selects the `app` channel, so the session shows in
the app. The path must be an absolute path on the gateway machine. `post_sessions(body=...)` sends
the same request and returns the new session as JSON.

To continue a saved session, call `client.session(session_id)`. It returns a handle and does not
create a session.

## Send a message and wait

```python
turn = session.send("Summarize TODO.md without changing files.")
result = turn.wait(timeout=300)
print(result["status"], result["content"])
```

`send()` returns a `Turn` handle when the gateway accepts the message. The model has not finished
yet. `turn.wait()` reads the turn until its status is `completed`, `failed`, `cancelled`,
`suspended` or `error`. A failed status is data, not an exception. Check it before you use
`content`.

When the time ends first, `wait()` raises `VisTimeout`. The turn continues to run. Wait again, or
call `turn.cancel()`.

A message can also be a slash command, for example `/rename Release notes`. `send()` also takes
`provider` and `model` to select a configured alternative for this message. To retry after a network
failure without a second message, send the same `idempotency_key` again. Without a key, each call
sends a new message.

### Continue a conversation with an Agent

An `Agent` from [Python SDK basics](python-sdk.md#let-your-program-own-a-private-agent) has the same
calls. Use `send()` instead of `run()` when you want a turn handle for progress, waiting or
cancellation:

```python
with Agent(project=".") as agent:
    first = agent.run("Explain the test setup without changing files.")
    if first["status"] == "completed":
        turn = agent.send("Which test should I run first?")
        result = turn.wait(timeout=300)
        print(result["status"], result["content"])
```

`agent.session` is the session of the agent, with its ID and transcript. Export history before you
close a local agent if you must keep it. For example, inside the context,
`agent.session.transcript(format="markdown").content` returns bytes that you can save to a file.

By default, requests use the configured provider and model of the engine. Both `run()` and `send()`
accept `provider`, `model` and the other `Session.send()` options, such as `idempotency_key`.

## Follow progress

Pass a session and the turn from its `send()` call to this function. The session can be a gateway
session or `agent.session` of an `Agent`:

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
The stream follows the **session** and does not end automatically with a turn.
Stop only for the matching turn. Closing the stream does not cancel work.
Save `events.cursor` if you need to reconnect.

For structured progress, inspect `event.activity` and `event.view`. If the turn
needs a person's answer, use `conversation.input_views()` and
`conversation.answer(view_id, values)`. See [Forms and user input](human-input.md).
Do not automatically approve credential or permission requests.

### Read Activity and view receipts

Use `event.activity` to read an Activity receipt with immutable rows, outcome counts
and evidence. Its `groups` property groups invocations by operation.
`argument_groups` groups calls with identical arguments. These reader views leave
`rows` and serialization unchanged.

A row with a persistent `handle_id` can represent several calls in `children`. Read the head for the
latest outcome. Then expand the children to see each call with its own state and evidence. The
handle is scoped to its extension and Python form, so it is not a session-wide identifier. For a
Python extension example, see [Link receipts for one
operation](extension-api.md#link-receipts-for-one-operation).

A receipt can be one page of history. Check `history` before you treat the receipt
as complete.

`event.view` decodes view lifecycle events. The records describe input forms, live
interfaces, patches and closure results. They are not Python UI widgets. To create
an interface, follow [Forms and user input](human-input.md) or [Live views](live-views.md).

When you have saved JSON rather than an event, use the record's `from_wire()` method.
It validates the data and makes nested values immutable. `to_wire()` returns a fresh
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

Both assertions pass for this receipt. Invalid data raises `ValueError`. Decoding
never assigns engine IDs, sequence numbers, timeouts or terminal outcomes.

## Answer a form

A turn can stop and wait for a person, for example when an extension asks for a value. The question
is an input form:

```python
for form in session.input_views():
    print(form.id, form.title)
    session.answer(form.id, {"env": "staging"})
```

`input_views()` returns typed `InputView` records. The values are keyed by the field `name`, as in
[The answer](human-input.md#the-answer). To close a form without values, call
`session.view_action(form.id, "cancel")`. `get_session_views_input(sid)` and
`post_session_view(sid, view_id, body=...)` send the same requests and return JSON.

Do not answer credential or permission forms automatically. A person must decide them.

## Queue and cancel messages

A message that you send while a turn runs waits in a queue. Queued messages run in the order that
you sent them.

```python
for queued in session.turns(status="queued")["turns"]:
    print(queued["turn_id"], queued["request"])
```

Without `status="queued"`, the answer contains every turn with its full content. Use these calls to
change the queue or stop work:

- `patch_session_turn(sid, tid, body={"request": "New text"})` changes a queued message.
- `delete_session_turn(sid, tid)` removes a queued message.
- `turn.cancel()` or `post_session_turn_cancel(sid, tid)` cancels a turn.
- `post_session_cancel_current(sid, body={"idempotency_key": key})` cancels the running turn when
  your program lost its turn ID. The key must be the `idempotency_key` of the message that started
  the turn.
- `post_session_resume_queue(sid)` resumes a queue that a failed turn paused. It starts the next
  queued message.
- `post_session_drain_queue(sid)` starts the oldest queued message when no turn runs.

A change to a turn that is no longer queued raises `GatewayError` with the status 409. The queue is
stored in memory, so a gateway restart clears it.

## Find a saved session

```python
after = None
while True:
    query = {"q": "release notes", "limit": 20}
    if after is not None:
        query["after"] = after
    page = client.get_sessions_search(query=query)
    for row in page["sessions"]:
        print(row["id"], row["title"])
    if not page["has_more"]:
        break
    after = page["next_cursor"]
```

The `query` dictionary takes the parameters that [Find a saved session over
HTTP](http-sessions.md#find-a-saved-session) lists. The rows have the same fields as the rows of
`get_sessions()`. For one page of recent sessions without a search, call `client.list_sessions()`.

## Fork a session

```python
points = client.get_session_forks(session.id)["turns"]
fork = client.post_session_forks(session.id, body={"through_turn_id": points[0]["turn_id"]})
print(fork["session"]["id"])
```

`get_session_forks()` lists the turns of a session, oldest first, with `turn_id` and `request`. The
fork keeps the turn that you name and every turn before it. Without `through_turn_id`, the fork keeps
all turns. [What a fork keeps](sessions.md#what-a-fork-keeps) describes the copy.

A session without turns, or a turn of another session, raises `GatewayError` with the status 409.

## Organize sessions into groups

```python
group = client.post_session_groups(
    body={"name": "Release apps", "root": os.environ["VIS_PROJECT_ROOT"]}
)
client.put_session_group(session.id, body={"group_id": group["id"]})
```

A group belongs to one project. Name the project with `root` or `project_id`. A second group with the
same name in a project raises `GatewayError` with the status 409.

- `get_session_groups(query={"root": path})` lists the groups of one project. Use `project` with a
  project ID instead of `root`. `archived` is `exclude`, `include` or `only`.
- `patch_session_group(gid, body=...)` changes `name`, `color`, `position` or `archived`.
- `delete_session_group(gid, query={"sessions": "detach"})` deletes a group and keeps its sessions.
  With `"delete"`, the sessions are deleted too.
- `put_session_group(sid, body={"group_id": None})` removes a session from its group.

## Rename, star, archive or delete a session

```python
session.update(title="Release notes for 3.2")
session.update(is_favorite=True)
session.update(archived=True)
```

Send one field in each call. The gateway refuses to archive a session while a turn runs.
`session.delete()` deletes the session permanently. `patch_session(sid, body=...)` and
`delete_session(sid)` send the same requests.

## Export a session

```python
from pathlib import Path

Path("session.md").write_bytes(session.transcript(format="markdown").content)
```

`format` is `json`, `markdown` or `html`. The default is `json`. The answer keeps the bytes and the
headers of the gateway. `get_session_transcript(sid)`, `get_session_transcript_md(sid)` and
`get_session_transcript_html(sid)` send the same requests.

Vis does not remove private data from an export. Read an export before you share it. To remove
private details, follow [Reporting a bug](reporting-bugs.md#sharing-a-transcript).

## Watch live views

```python
for view in client.session(session_id).live_views():
    print(view.id, view.title, len(view.nodes))
```

`live_views()` returns typed `LiveView` records. `get_session_views_live(session_id)` returns the
same views as JSON in `views`. A list is a snapshot. It does not follow later updates.

### Read a log page

```python
page = client.get_session_views_live_log(
    session_id, view_id, "build/log", query={"query": "error", "from": 0, "limit": 100}
)
for number, line in zip(page["line_numbers"], page["lines"]):
    print(number, line)
```

The method sends the node ID as the `node` query parameter. The answer has `lines`, `line_numbers`,
`matched` and `total`.

### Interrupt a view

```python
client.session(session_id).view_action(view_id, "interrupt", note="Stop monitoring")
```

`view_action` checks the body before it sends it. `post_session_view(session_id, view_id,
body={"action": "interrupt", "note": "Stop monitoring"})` sends the same request without the check.

## Read Activity history

```python
after, revision = 0, None
while after is not None:
    query = {"after": after, "limit": 32}
    if revision is not None:
        query["revision"] = revision
    page = client.get_session_activity(session_id, activity_id, query=query)
    for row in page["rows"]:
        print(row)
    revision = page["history"]["revision"]
    after = page["history"]["next_after"]
```

The `revision` keeps all pages on the same version of the history. A changed history raises
`GatewayError` with the status 409. Read the history again from the first page.

### Export Activity history

```python
response = client.get_session_activity_export(session_id, activity_id, query={"revision": revision})
print(response.content.decode())
```

An incomplete export raises `ProtocolError`. Read the history again and retry.

## See also

- [Sessions](sessions.md) — what a session keeps and how to use it in the terminal and the apps.
- [Sessions over HTTP](http-sessions.md) — the same operations as HTTP requests.
- [Python SDK basics](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [Live views](live-views.md) — build the views that your program watches.
