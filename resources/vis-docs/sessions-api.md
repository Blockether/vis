# Sessions API

Your program creates [sessions](sessions.md), sends messages, and follows the work of the agent.

<div data-variant="python">

A typed call checks the answer and returns a Python record. Each generated `GatewayClient` method
sends one gateway request and returns the parsed JSON answer.

</div>

<div data-variant="http">

The requests and the answers are JSON, except the event stream and the exports.

</div>

The examples use the [Python SDK](python-sdk.md). For the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **Your program must start work and wait for the answer.** [Create a session](#create-a-session).
  Then [send a message and wait](#send-a-message-and-wait).
- **Your program must show progress while Vis works.** [Follow progress](#follow-progress).
- **A turn waits for a person.** [Answer a form](#answer-a-form).
- **A session must stop using some tools, or use them again.**
  [Change the tools of a session](#change-the-tools-of-a-session).
- **Your terminal client must know the keys that extensions add.**
  [Read the key bindings](#read-the-key-bindings).
- **A session must use a cheaper or stronger model.**
  [Choose the model of a session](#choose-the-model-of-a-session).
- **A session must work in another directory.**
  [Change the directory of a session](#change-the-directory-of-a-session).
- **Your program must stop work or change waiting messages.**
  [Queue and cancel messages](#queue-and-cancel-messages).
- **Your program must find, copy, or sort saved sessions.**
  [Find a saved session](#find-a-saved-session), [fork a session](#fork-a-session), or
  [organize sessions into groups](#organize-sessions-into-groups).
- **Your program needs a checked answer in a fixed shape.**
  [Run a task and read a typed answer](#run-a-task-and-read-a-typed-answer).
- **Your program needs the files that the agent made.**
  [Read the files of a turn](#read-the-files-of-a-turn).
- **Your program must keep a record of the work.** [Export a session](#export-a-session) or
  [read Activity history](#read-activity-history).
- **A session must move to another gateway, or you need a full backup.**
  [Save and restore a session snapshot](#save-and-restore-a-session-snapshot).
- **Your program must own an app that people use in the session.**
  [Run an app from a program](#run-an-app-from-a-program).

To learn what a session is and how to use it in the terminal or the app, read
[Sessions](sessions.md).

## Before you start

<div data-variant="python">

[Install the SDK](python-sdk.md#install-the-sdk). Then set `VIS_GATEWAY_URL`, `VIS_GATEWAY_TOKEN`,
and `VIS_PROJECT_ROOT` as in
[Connect to a gateway and run a task](python-sdk.md#connect-to-a-gateway-and-run-a-task).

```python
import os

from blockether.vis.engine import GatewayClient

with GatewayClient(os.environ["VIS_GATEWAY_URL"], token=os.environ["VIS_GATEWAY_TOKEN"]) as client:
    for row in client.list_sessions(limit=5)["sessions"]:
        print(row["id"], row["title"])
```

The other examples are calls on this `client` inside the `with` block. A `session` is the handle
that `client.create_session()` or `client.session(session_id)` returns. When the gateway refuses a
request with an error status, the call raises `GatewayError` with that status.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples also use `SESSION_ID`, the ID of a session that you created or found.

</div>

## Operations

<div data-variant="python">

| Task | Typed call | Generated method |
|---|---|---|
| Create a session | `client.create_session(...)` | `client.routes.post_sessions(body=...)` |
| List models with prices and limits | `client.models(...)` | `client.routes.get_models()` |
| Read the limits of a provider | `client.model_limits(provider)` | `client.routes.get_provider_limits(provider_id)` |
| Read the model of a session | `session.model()` | `client.routes.get_session_model(sid)` |
| Pin or clear the model of a session | `session.set_model(provider, model)` | `client.routes.patch_session_model(sid, body=...)` |
| Read the directory of a session | No typed call | `client.routes.get_session_workspace(sid)` |
| Change the directory of a session | No typed call | `client.routes.patch_session_workspace_root(sid, body=...)` |
| Read a session | `session.read()` | `client.routes.get_session(sid)` |
| Send a message | `session.send(request)` | `client.routes.get_session_seq(sid)`, then `client.routes.post_session_turns(sid, body=...)` |
| Send a message and wait for a checked answer | `session.run(request, ...)` | No generated method |
| Read a turn | `turn.read()`, `turn.wait()` | `client.routes.get_session_turn(sid, tid)` |
| Follow events | `session.events(cursor=...)` | `client.routes.get_session_events_since(sid, query=...)` |
| List open forms | `session.input_views()` | `client.routes.get_session_views_input(sid)` |
| Answer a form or interrupt a view | `session.answer(...)`, `session.view_action(...)` | `client.routes.post_session_view(sid, view_id, body=...)` |
| List turns or queued messages | `session.turns(...)` | `client.routes.get_session_turns(sid, query=...)` |
| Change a queued message | No typed call | `client.routes.patch_session_turn(sid, tid, body=...)` |
| Remove a queued message | No typed call | `client.routes.delete_session_turn(sid, tid)` |
| List the files that the agent attached | `session.attachments(...)`, `turn.attachments()` | `client.routes.get_session_artifacts(sid)` |
| Download one file | `attachment.read()`, `session.download_attachment(iid, index)` | `client.routes.get_session_iteration_attachments(sid, iid, idx)` |
| Cancel a turn | `turn.cancel()` | `client.routes.post_session_turn_cancel(sid, tid)` |
| Cancel the running turn without its ID | No typed call | `client.routes.post_session_cancel_current(sid, body=...)` |
| Start the oldest queued message | No typed call | `client.routes.post_session_drain_queue(sid)` |
| Resume a paused queue | No typed call | `client.routes.post_session_resume_queue(sid)` |
| Search sessions | No typed call | `client.routes.get_sessions_search(query=...)` |
| List sessions | `client.list_sessions(...)` | `client.routes.get_sessions(query=...)` |
| List the turns that a fork can keep | No typed call | `client.routes.get_session_forks(sid)` |
| Fork a session | `session.fork(...)` | `client.routes.post_session_forks(sid, body=...)` |
| List groups | `client.groups(...)` | `client.routes.get_session_groups(query=...)` |
| Create a group | `client.group(...)`, `client.create_group(...)` | `client.routes.post_session_groups(body=...)` |
| Change or archive a group | `client.update_group(...)` | `client.routes.patch_session_group(gid, body=...)` |
| Delete a group | `client.delete_group(...)` | `client.routes.delete_session_group(gid, query=...)` |
| Move a session to a group | `session.set_group(...)`, `client.create_session(group=...)` | `client.routes.put_session_group(sid, body=...)` |
| Rename, star, or archive a session | `session.update(...)` | `client.routes.patch_session(sid, body=...)` |
| Archive many sessions | No typed call | `client.routes.post_sessions_archive(body=...)` |
| Delete a session | `session.delete()` | `client.routes.delete_session(sid)` |
| Export a transcript | `session.transcript(format=...)` | `client.routes.get_session_transcript(sid)`, `client.routes.get_session_transcript_md(sid)`, `client.routes.get_session_transcript_html(sid)` |
| Save a snapshot | `session.export()` | `client.routes.get_session_snapshot(sid)` |
| Read the manifest of a snapshot | `client.snapshot_info(data)` | No generated method |
| Restore a snapshot | `client.restore_session(data, ...)` | `client.routes.post_sessions_import(query=..., content=...)` |
| List live views | `session.live_views()` | `client.routes.get_session_views_live(sid)` |
| Read one live view or app | `session.live_view(view_id)` | `client.routes.get_session_views_live_view(sid, view_id)` |
| Read a page of a log node | No typed call | `client.routes.get_session_views_live_log(sid, view_id, node_id, query=...)` |
| Open or adopt a global app | `client.attach_app(app, owner=...)` | `client.routes.post_apps(body=...)` |
| List the global apps | `client.apps(...)` | `client.routes.get_apps(query=...)` |
| Read one global app | `client.app(view_id)` | `client.routes.get_app(view_id)` |
| Press a button in a global app | `client.app_action(view_id, action, ...)` | `client.routes.post_app_actions(view_id, body=...)` |
| Read one page of Activity history | No typed call | `client.routes.get_session_activity(sid, aid, query=...)` |
| Export the full Activity history | No typed call | `client.routes.get_session_activity_export(sid, aid, query=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Create a session | `POST /v1/sessions` |
| List models with prices and limits | `GET /v1/models` |
| Read the limits of a provider | `GET /v1/providers/{provider-id}/limits` |
| Read the model of a session | `GET /v1/sessions/{sid}/model` |
| Pin or clear the model of a session | `PATCH /v1/sessions/{sid}/model` |
| Read the directory of a session | `GET /v1/sessions/{sid}/workspace` |
| Change the directory of a session | `PATCH /v1/sessions/{sid}/workspace/root` |
| Read a session | `GET /v1/sessions/{sid}` |
| Read the event cursor | `GET /v1/sessions/{sid}/seq` |
| Send a message | `POST /v1/sessions/{sid}/turns` |
| Read a turn | `GET /v1/sessions/{sid}/turns/{tid}` |
| Follow events as a stream | `GET /v1/events` |
| Read events as pages | `GET /v1/sessions/{sid}/events-since` |
| List open forms | `GET /v1/sessions/{sid}/views/input` |
| Answer a form or interrupt a view | `POST /v1/sessions/{sid}/views/{view-id}/actions` |
| List turns or queued messages | `GET /v1/sessions/{sid}/turns` |
| Change a queued message | `PATCH /v1/sessions/{sid}/turns/{tid}` |
| Remove a queued message | `DELETE /v1/sessions/{sid}/turns/{tid}` |
| List the files that the agent attached | `GET /v1/sessions/{sid}/artifacts` |
| Download one file | `GET /v1/sessions/{sid}/iterations/{iid}/attachments/{idx}` |
| Cancel a turn | `POST /v1/sessions/{sid}/turns/{tid}/cancel` |
| Cancel the running turn without its ID | `POST /v1/sessions/{sid}/cancel-current` |
| Start the oldest queued message | `POST /v1/sessions/{sid}/drain-queue` |
| Resume a paused queue | `POST /v1/sessions/{sid}/resume-queue` |
| Search sessions | `GET /v1/sessions/actions/search` |
| List sessions | `GET /v1/sessions` |
| List the turns that a fork can keep | `GET /v1/sessions/{sid}/forks` |
| Fork a session | `POST /v1/sessions/{sid}/forks` |
| List groups | `GET /v1/session-groups` |
| Create a group | `POST /v1/session-groups` |
| Change or archive a group | `PATCH /v1/session-groups/{gid}` |
| Delete a group | `DELETE /v1/session-groups/{gid}` |
| Move a session to a group | `PUT /v1/sessions/{sid}/group` |
| Rename, star, or archive a session, or change its tools | `PATCH /v1/sessions/{sid}` |
| Archive many sessions | `POST /v1/sessions/actions/archive` |
| Delete a session | `DELETE /v1/sessions/{sid}` |
| Export a transcript | `GET /v1/sessions/{sid}/transcript`, `GET /v1/sessions/{sid}/transcript.md`, `GET /v1/sessions/{sid}/transcript.html` |
| Save a snapshot | `GET /v1/sessions/{sid}/snapshot` |
| Restore a snapshot | `POST /v1/sessions/actions/import` |
| List live views | `GET /v1/sessions/{sid}/views/live` |
| Read one live view or app | `GET /v1/sessions/{sid}/views/live/{view-id}` |
| Read a page of a log node | `GET /v1/sessions/{sid}/views/live/{view-id}/log` |
| Open or adopt a global app | `POST /v1/apps` |
| List the global apps | `GET /v1/apps` |
| Read one global app | `GET /v1/apps/{view-id}` |
| Press a button in a global app | `POST /v1/apps/{view-id}/actions` |
| Read one page of Activity history | `GET /v1/sessions/{sid}/activity/{aid}` |
| Export the full Activity history | `GET /v1/sessions/{sid}/activity/{aid}/export` |

</div>

## Create a session

<div data-variant="python">

```python
session = client.create_session(**client.session_options(os.environ["VIS_PROJECT_ROOT"]))
print(session.id)
```

`session_options()` checks the project path and selects the `app` channel, so the app shows the
session. `client.routes.post_sessions(body=...)` sends the same request and returns the new session as JSON. To
continue a saved session, call `client.session(session_id)`. It returns a handle and creates no
session.

To leave out configuration tiers or extensions, add `sources` and `extensions`:

```python
session = client.create_session(
    **client.session_options(os.environ["VIS_PROJECT_ROOT"]),
    sources=["project"],
    extensions=["-spel"],
)
```

To turn off single tools of an extension, add `tools`. `Agent(tools=[...])` sends the same list
for its conversation:

```python
session = client.create_session(
    **client.session_options(os.environ["VIS_PROJECT_ROOT"]),
    tools=["-py.lint_code", "-py.repl_stop"],
)
```

If you retry a create after a timeout, add `idempotency_key`. Use one key for each session that
you want:

```python
session = client.create_session(
    **client.session_options(os.environ["VIS_PROJECT_ROOT"]),
    idempotency_key="shop-review-2026-10-15",
)
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions" -H 'content-type: application/json' \
  --data '{"root": "/srv/projects/shop", "channel": "app"}'
```

The answer has the status 201 and the new session with its `id`. The `app` channel makes the app
show the session. Add `group_id` to put the new session in a group. To continue a saved session,
use its `id` in the paths of the next requests.

To leave out configuration tiers or extensions, add `sources` and `extensions`:

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions" -H 'content-type: application/json' \
  --data '{"root": "/srv/projects/shop", "channel": "app", "sources": ["project"], "extensions": ["-spel"]}'
```

If you retry a create after a timeout, add `idempotency_key`. Use one key for each session that
you want:

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions" -H 'content-type: application/json' \
  --data '{"root": "/srv/projects/shop", "channel": "app", "idempotency_key": "shop-review-2026-10-15"}'
```

To turn off single tools of an extension, add `tools`:

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions" -H 'content-type: application/json' \
  --data '{"root": "/srv/projects/shop", "channel": "app", "tools": ["-py.lint_code", "-py.repl_stop"]}'
```

</div>

The project root must be an absolute path on the gateway machine.

| Option | Effect |
|---|---|
| `sources` | The configuration tiers that the session reads: `global`, `project`, both, or none. [Leave out global or project configuration](configuration.md#leave-out-global-or-project-configuration) describes each tier |
| `extensions` | Keeps the named extensions, or turns off each `-name` entry. An empty list turns off every optional extension |
| `tools` | Keeps only the named tools of the extensions that they name, or turns off each `-name` entry. A `*` matches any part of a name, as in `py.repl_*`. [Choose single tools](configuration.md#choose-single-tools) describes the rules |
| `idempotency_key` | For 10 minutes, the same key returns the session that the first request made, also while that request still runs. A key has 1 to 200 characters |

A failed request does not keep its key, so a retry creates the session.

The gateway refuses these values with 400: an unknown tier, an unknown extension, and an unknown
tool. It also refuses an empty `tools` list and a `tools` list that mixes the two forms. A `root`
that is not an existing directory gets 400 with the type `not-a-directory`, and the gateway creates
no session.

## Change the tools of a session

<div data-variant="python">

```python
session = client.session(session_id)
session.update(tools=["-py.repl_start", "-py.repl_stop"])
```

For an `Agent`, call `agent.session.update(tools=[...])`.

</div>

<div data-variant="http">

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID" -H 'content-type: application/json' \
  --data '{"tools": ["-py.repl_start", "-py.repl_stop"]}'
```

The answer has the status 200 and the session. A value that is not a list returns 400.

</div>

The new list replaces the saved list. An idle session gets the new tools at once, and a busy
session gets them at its next step. Resumed sessions keep the new list. To
turn every tool on again, send `["*"]`.

The gateway refuses an unknown tool, or a list that mixes the two forms, with 400. The old list
then stays. An archived session refuses the change with 409.

## Read the key bindings

The extensions of a session can give terminal keys to slash commands and terminal verbs.
[Key bindings](extension-api.md#key-bindings) explains how an extension declares them.

<div data-variant="python">

```python
for binding in client.session(session_id).keybindings():
    print(binding["key"], binding.get("command") or binding.get("action"), binding["extension"])
```

`keybindings()` returns dicts, and `client.routes.get_session_keybindings(session_id)` returns the same JSON.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/keybindings"
```

`GET /v1/sessions/{sid}/keybindings` answers `keybindings`. An unknown session returns 404.

</div>

The bindings come in extension order, then declaration order. Each binding has `key` and
`extension`. It also has `command` with `label` and `group`, or it has `action`. A function binding
has `run` set to true, with `label` and `group`.

### Run a key binding

Run a function binding when your program must do what its key does, without a terminal.

<div data-variant="python">

```python
result = client.session(session_id).run_keybinding("deploy", "D", group="Extensions")
print(result["status"], result.get("title"))
```

`run_keybinding(extension, key, group=None)` calls the function of the binding in the session and
returns its result. `client.routes.post_session_keybindings_run(session_id, body=...)` sends the same request.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/keybindings/run" \
  -H 'Content-Type: application/json' \
  -d '{"extension": "deploy", "key": "D", "group": "Extensions"}'
```

`POST /v1/sessions/{sid}/keybindings/run` calls the function of one binding and answers its
result. Give `group` as the listing shows it. An unknown session or binding returns 404, and an
invalid body returns 400.

</div>

In the result, `status` is `ok` or `error`. `title` and `body` are present when the function
returns them.

## Choose the model of a session

A model request can cost money. Read `pricing` before you choose a model for many turns.

<div data-variant="python">

```python
models = client.models(is_available=True)
priced = [model for model in models if model.pricing]
cheap = min(priced, key=lambda model: model.pricing["input"])
session.set_model(cheap.provider, cheap.name)
print(session.model())
```

`models()` returns `Model` records. `provider` keeps only the models of one provider, and
`is_available=True` keeps the models of providers that are signed in.

`client.model_limits(provider)` reads a new limits report. A failed read gives an error report, not an exception.

`set_model(provider, model)` pins the model of the next turns, and `set_model(None, None)` removes
the pin. `model()` returns a `SessionModel` with `provider`, `model`, `is_pinned`,
`default_provider`, and `default_model`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/models"
vis_api "$VIS_GATEWAY_URL/v1/providers/$PROVIDER/limits"
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/model" \
  -H 'content-type: application/json' \
  --data "{\"provider\": \"$PROVIDER\", \"model\": \"$MODEL\"}"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/model"
```

The first answer has `models`, and each model also has `is_available`. The limits request reads a
new limits report. A failed read gives an error report, not an HTTP error.

The `PATCH` body pins the model of the next turns, and `null` for both fields removes the pin. The
model answer has `model`, the pin or `null`. It also has `default`, the pair that the session uses
without a pin.

</div>

The models have the order of the model pickers in the TUI and the app. Each model has these
fields:

| Field | Value |
|---|---|
| `provider`, `name`, `label` | The identity of the model |
| `is_reasoning`, `is_tool_call`, `is_image_input` | Examples of the capabilities |
| `context`, `input_limit`, `output_limit` | The token limits |
| `billing` | `metered`, `subscription`, or `local` |
| `pricing` | US dollars for each million tokens: `input`, `output`, `cache_read`, and `cache_write`. `None` in Python and `null` in JSON when Vis has no price for the model |
| `limits` | The limits report of the provider |

A running or queued turn keeps its model. The gateway refuses a provider that it does not serve
with 400 and the code `unknown-provider`. An archived session refuses the change with 409.

## Change the directory of a session

<div data-variant="python">

```python
workspace = client.routes.get_session_workspace(session.id)["workspace"]
print(workspace["root"])
moved = client.routes.patch_session_workspace_root(session.id, body={"path": "/Users/me/projects/api"})
print(moved["workspace"]["root"])
```

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/workspace"
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/workspace/root" \
  -H 'content-type: application/json' --data '{"path": "/Users/me/projects/api"}'
```

</div>

The `workspace` object has the `root` of the
session and the `git` state of that directory. The change does the same as `/cd` in a
conversation. For its effects, read
[Change the directory of a session](sessions.md#change-the-directory-of-a-session).

## Send a message and wait

<div data-variant="python">

```python
turn = session.send("Summarize TODO.md without changing files.")
result = turn.wait(timeout=300)
print(result["status"], result["content"])
```

`send()` returns a `Turn` handle when the gateway accepts the message. `turn.wait()` reads the
turn until it has a final status.

When the time ends first, `wait()` raises `VisTimeout`. The turn continues to run. Wait again, or
call `turn.cancel()`.

`send()` also takes `provider` and `model` to select a configured alternative for this message.
Without an `idempotency_key`, each call sends a new message.

</div>

<div data-variant="http">

```bash
cursor="$(vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/seq" |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["seq"])')"
turn_id="$(vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" \
  -H 'content-type: application/json' \
  --data '{"request": "Summarize TODO.md without changing files.", "idempotency_key": "todo-1"}' |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["turn_id"])')"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns/$turn_id"
```

Read the cursor before you send the message. [Follow progress](#follow-progress) uses the cursor
to replay the events of this turn. The `turn_id` comes back when the gateway accepts the message.

Read the turn again until it has a final status. A running turn has the status `streaming`, and a
waiting message has `queued`. The body also takes `provider` and `model` to select a configured
alternative for this message.

</div>

When the gateway accepts the message, the model has not finished yet. The final statuses are
`completed`, `failed`, `cancelled`, `suspended`, and `error`. A failed status is data, not an
exception or an HTTP error. Check the status before you use `content`.

A message can also be a slash command, for example `/rename Release notes`. To retry after a
network failure without a second message, send the same `idempotency_key` again.

### Run a task and read a typed answer

<div data-variant="python">

```python
from pydantic import BaseModel


class Summary(BaseModel):
    title: str
    open_items: list[str]


summary = session.run("Summarize TODO.md without changing files.", response_model=Summary)
print(summary.open_items)
```

`run()` sends the message and waits for the turn, as `Agent.run()` does. Without `response_model`,
it returns the turn as a dictionary. With a Pydantic model, it checks the final answer and returns
an instance of the model.

An answer that does not match starts a correction turn, and each correction turn costs model
tokens. `max_corrections` sets the number of correction turns, 2 by default. When no answer
matches, `run()` raises `StructuredOutputError` with each attempt.
[Handle invalid answers](python-sdk.md#handle-invalid-answers) has the details.

</div>

<div data-variant="http">

Send the message and read the turn as in [Send a message and wait](#send-a-message-and-wait). Then
check the `content` of the turn in your program. If it does not match, send a correction as the
next message of the same session.

</div>

### Read the files of a turn

<div data-variant="python">

```python
turn = session.send("Write the specification and attach it as SPEC.md.")
turn.wait(timeout=600)

for item in turn.attachments():
    print(item.filename, item.version, item.size)

spec = turn.attachments()[0].text()
```

`turn.attachments()` lists the files that the agent attached in that turn. `session.attachments()`
lists the files oldest turn first. Give it turns to select a part:

| Call | Files |
|---|---|
| `session.attachments()` | The whole conversation |
| `session.attachments(-1)` | The last turn |
| `session.attachments(turn)` | One turn, as a `Turn` or as a turn ID |
| `session.attachments([first, second])` | Several turns |

An integer is an index into `session.turns()`: 0 is the first turn and -1 is the last turn. Each
record has `filename`, `media_type`, `kind`, `size`, `version`, `turn_id`, `iteration_id`, and
`index`. The list has no file content. `read()` downloads the bytes and `text()` decodes them.

For a typed answer with files, call `session.run()` with a `response_model`, then read
`session.attachments(-1)`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/artifacts"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/iterations/$ITERATION_ID/attachments/0" -o SPEC.md
```

The first request returns `artifacts`, a list with one row for each file of the conversation. A
row has `turn_id`, `turn`, `iteration_id`, `index`, `filename`, `media_type`, `kind`, `size`, and
`version`. `turn` is the number of the turn, counted from 1. To get the files of one turn, keep
the rows with its `turn_id`. The second request returns the bytes of the file at `index` of that
iteration.

</div>

When the agent attaches a filename again, the list has each version. A file that you uploaded with
a message is not in the list.

### Continue a conversation with an Agent

<div data-variant="python">

An `Agent` from [Python SDK](python-sdk.md#let-your-program-own-a-private-agent) has the same
calls. Use `send()` and not `run()` when you want a turn handle for progress, a wait, or a
cancel:

```python
with Agent(project=".") as agent:
    first = agent.run("Explain the test setup without changing files.")
    if first["status"] == "completed":
        turn = agent.send("Which test should I run first?")
        result = turn.wait(timeout=300)
        print(result["status"], result["content"])
```

`agent.session` is the session of the agent, with its ID and its transcript. A local agent
discards its history when it closes, so export the history first if you must keep it. For example,
inside the context, `agent.session.transcript(format="markdown").content` returns bytes that you
can save to a file.

By default, requests use the configured provider and model of the engine. Both `run()` and
`send()` accept `provider`, `model`, and the other `Session.send()` options, such as
`idempotency_key`.

</div>

<div data-variant="http">

An `Agent` is a Python SDK record. Over HTTP, send the next message to the same session, as in
[Send a message and wait](#send-a-message-and-wait).

</div>

## Follow progress

<div data-variant="python">

Pass a session and the turn from its `send()` call to this function. The session can be a gateway
session or the `agent.session` of an `Agent`:

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

`send()` captures `turn.cursor` before it submits the message, so the stream can replay also a
fast answer. To reconnect, save `events.cursor`.

For structured progress, inspect `event.activity` and `event.view`. If the turn needs the answer
of a person, use `conversation.input_views()` and `conversation.answer(view_id, values)`, as in
[Forms and user input](human-input.md). Do not approve credential or permission requests
automatically.

</div>

<div data-variant="http">

```bash
vis_api -N "$VIS_GATEWAY_URL/v1/events?sids=$SESSION_ID:$cursor&replay=full"
```

The answer is a Server-Sent Events stream. Each event has an `id` line with its sequence number,
an `event` line with its type, and a `data` line with the event as JSON. `replay=full` also
replays every stored event after the cursor. To reconnect, send the last sequence number as the
cursor.

A `content.block.delta` event carries only new text for one `block_id` and `field`. Keep your text
before `offset` and add `text` after it. The `offset` counts UTF-16 code units from the start of
the block text. A repeated event then changes nothing, and an `offset` of 0 starts the text again.

Add `app_patches=elided` when your client shows only some of the apps. The stream then sends a
large `view.patch` event of an app without its `patch`. The event has `"is_elided": true` and
`last_seq`, the sequence number of the patch that it left out. Read the app again when you show
it. Small patches arrive in full.

Add `app_opens=summary` to get a large app in a `view.open` event without its body. The `view` of
the event then has `"is_summary": true` and no `nodes`, `log`, `submissions`, or `agent_note`. If
you hold the app at the same `seq`, keep its body and take the other fields from the event. If you
do not, read the app when you show it. A small app arrives in full.

The stream does not keep the `view.patch` events of an app, so a reconnect does not send them
again. After each reconnect, list the apps with `summary=true` and compare each `seq` with the app
that you hold.

A client that cannot keep a stream open can read pages:

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/events-since?cursor=$cursor&replay=full"
```

The answer has `events`. Each event has `seq`, `ts`, `session_id`, and `type`. Send the `seq` of
the last event as the next `cursor`.

</div>

The stream follows the whole session and does not end with a turn. Stop when a `turn.completed`,
`turn.failed`, or `turn.cancelled` event has the ID of your turn. A closed stream does not cancel
work.

### Stop following events

<div data-variant="python">

To end the loop when no event comes for some time, give `idle_timeout` in seconds:

```python
with session.events(cursor=turn.cursor, idle_timeout=60) as events:
    for event in events:
        print(event.type)
print(turn.read()["status"])
```

The loop then ends normally, with no exception. The time starts when you call `events()` and again
after each event. Heartbeats of the gateway do not count as events. After the loop, the turn can
still run, so check its `status`.

Without `idle_timeout`, the stream follows the session until you close it. A stream of a private
engine, or of an `Agent` with application functions, reads pages. Without `idle_timeout`, that
stream raises `VisTimeout` when no event comes for the `timeout` of the client. The
default `timeout` is 30 seconds.

To stop a stream from another thread, call `events.close()`. The loop ends at once, and the stream
does not connect again.

In a coroutine, use `async with` and `async for`:

```python
# follow_async.py
TERMINAL = {"turn.completed", "turn.failed", "turn.cancelled"}


async def follow(conversation, turn, seconds=60):
    async with conversation.events(cursor=turn.cursor, idle_timeout=seconds) as events:
        async for event in events:
            print(event.type)
            if event.turn_id == turn.id and event.type in TERMINAL:
                return event.type
    return None
```

`follow()` returns the type of the last event of your turn, or `None` when no event came for
`seconds`. The stream reads on one worker thread that the stream owns. Do not iterate one stream
from two tasks. When you cancel the task, the stream closes.

</div>

<div data-variant="http">

To stop, close the connection. To limit the time of the whole request, give `curl` a limit in
seconds:

```bash
vis_api -N --max-time 60 "$VIS_GATEWAY_URL/v1/events?sids=$SESSION_ID:$cursor&replay=full"
```

The gateway sends heartbeat comment lines while no event comes. To end the read when no event comes
for some time, measure the time since the last `data` line in your client.

</div>

### Read Activity and view receipts

<div data-variant="python">

`event.activity` is an Activity receipt with immutable rows, outcome counts, and evidence. It
always holds the whole receipt. Its
`groups` property groups invocations by operation, and `argument_groups` groups calls with
identical arguments. These two properties leave `rows` and serialization unchanged.

A row with a persistent `handle_id` can represent several calls in `children`. Read the head for
the latest outcome. Then expand the children to see each call with its own state and evidence. The
handle is scoped to its extension and its Python form, so it is not an identifier for the whole
session. For a Python extension example, read
[Link receipts for one operation](extension-api.md#link-receipts-for-one-operation).

A receipt can be one page of history. Check `history` before you treat the receipt as complete.

`event.view` decodes view lifecycle events. The records describe input forms, live interfaces,
patches, and closure results. To create an interface, follow
[Forms and user input](human-input.md) or [Live views](live-views.md).

For saved JSON and not an event, use the `from_wire()` method of the record. It validates the data
and makes nested values immutable. `to_wire()` returns a new JSON-compatible copy. This example
reads a completed live-view receipt without a call to Vis:

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

Both assertions pass for this receipt. Invalid data raises `ValueError`. Decoding never assigns
engine IDs, sequence numbers, timeouts, or terminal outcomes.

</div>

<div data-variant="http">

Over HTTP, Activity receipts and view lifecycle records arrive as JSON events in the stream from
[Follow progress](#follow-progress).
The gateway schema in [Get the OpenAPI document](http-api.md#get-the-openapi-document) describes
each field.

On one stream connection, the first `block.activity` event of a form carries the whole receipt.
Later events of that form add `row_ids`, every row id in order. Their `rows` hold only the rows
that changed since the previous event of that form. Keep the rows of each form, and build the
receipt in the order of `row_ids`.

An event without `row_ids` replaces the whole receipt, and a new connection starts with whole
receipts. The page reads always return whole receipts.

</div>

## Answer a form

Do not answer credential or permission forms automatically. A person must decide them.

A turn can stop and wait for a person, for example when an extension asks for a value. The
question is an input form:

<div data-variant="python">

```python
for form in session.input_views():
    print(form.id, form.title)
    session.answer(form.id, {"env": "staging"})
```

`input_views()` returns typed `InputView` records. To close a form without values, call
`session.view_action(form.id, "cancel")`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/input"
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/$VIEW_ID/actions" \
  -H 'content-type: application/json' \
  --data '{"action": "submit", "values": {"env": "staging"}}'
```

The first answer has `requests`, one entry for each open form, with its `id`, `title`, and
`nodes`. To close a form without values, send `{"action": "cancel"}`.

</div>

The values are keyed by the field `name`, as in [The answer](human-input.md#the-answer).

## Queue and cancel messages

A message that you send while a turn runs waits in a queue. Queued messages run in the order that
you sent them. The queue stays after a gateway restart.

<div data-variant="python">

```python
for queued in session.turns(status="queued")["turns"]:
    print(queued["turn_id"], queued["request"])
```

Without `status="queued"`, the answer contains every turn with its full content. These calls
change the queue or stop work:

- `client.routes.patch_session_turn(sid, tid, body={"request": "New text"})` changes a queued message.
- `client.routes.delete_session_turn(sid, tid)` removes a queued message.
- `turn.cancel()` or `client.routes.post_session_turn_cancel(sid, tid)` cancels a turn.
- `client.routes.post_session_cancel_current(sid, body={"idempotency_key": key})` cancels the running turn when
  your program lost its turn ID. The key must be the `idempotency_key` of the message that started
  the turn.
- `client.routes.post_session_resume_queue(sid)` resumes a queue that a failed turn paused. It starts the next
  queued message.
- `client.routes.post_session_drain_queue(sid)` starts the oldest queued message when no turn runs.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns?status=queued"
```

The answer has `turns`. Without `status=queued`, it contains every turn with its full content.
These requests change the queue or stop work:

- `PATCH /v1/sessions/{sid}/turns/{tid}` with `{"request": "New text"}` changes a queued message.
- `DELETE /v1/sessions/{sid}/turns/{tid}` removes a queued message.
- `POST /v1/sessions/{sid}/turns/{tid}/cancel` cancels a turn.
- `POST /v1/sessions/{sid}/cancel-current` with `{"idempotency_key": "todo-1"}` cancels the running
  turn when your program lost its turn ID. The key must be the `idempotency_key` of the message
  that started the turn.
- `POST /v1/sessions/{sid}/resume-queue` resumes a queue that a failed turn paused. It starts the
  next queued message.
- `POST /v1/sessions/{sid}/drain-queue` starts the oldest queued message when no turn runs.

</div>

The gateway refuses a change to a turn that is no longer queued with 409.

## Find a saved session

<div data-variant="python">

```python
after = None
while True:
    query = {"q": "release notes", "limit": 20}
    if after is not None:
        query["after"] = after
    page = client.routes.get_sessions_search(query=query)
    for row in page["sessions"]:
        print(row["id"], row["title"])
    if not page["has_more"]:
        break
    after = page["next_cursor"]
```

The `query` dictionary takes the parameters of the table below. The rows have the same fields as
the rows of `client.routes.get_sessions()`. For one page of recent sessions without a search, call
`client.list_sessions()`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/actions/search?q=release%20notes&limit=20"
```

The rows have the same fields as the rows of `GET /v1/sessions`.

</div>

The answer lists session rows in order of recent
activity, the most recent first. With an empty `q`, the answer lists your recent sessions. With
words in `q`, it lists the sessions whose title or conversation text matches. Each of those rows
also has a `match` object. It tells where the words matched and gives short text around each
match.

| Parameter | Effect |
|---|---|
| `q` | The search words |
| `project_id` | Restricts the results to one saved project |
| `root` | Restricts the results to one project directory. An empty `root` selects sessions without a project |
| `group_ids` | Selects any group in a comma-separated list. An empty value selects no groups |
| `archived` | `exclude` lists active sessions, `include` lists all sessions, and `only` lists archived sessions. The default is `exclude` |
| `limit` | The page size, from 1 to 1000. The default is 50 |
| `after` | The `next_cursor` value of the previous page |

Without `project_id`, `root`, and `group_ids`, the search includes every project and group. These
scopes apply before the gateway calculates `total`, the page window, and its cursor. `total` is
the number of sessions in all pages. When `has_more` is false, there are no more pages.

### List recent sessions

<div data-variant="python">

```python
page = client.list_sessions(limit=20, include_empty="true")
for row in page["sessions"]:
    print(row["id"], row["title"])
```

`client.routes.get_sessions(query=...)` sends the same request.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions?limit=20&include_empty=true"
```

</div>

The answer lists one page of active sessions in the order of the app. By default, the list leaves
out a session that has no title, no turns, and no running turn. A session that you created and did
not use yet is such a session. To find it, add `include_empty=true`.

| Parameter | Effect |
|---|---|
| `limit` | The page size. Without `limit` and without a scope, the page has 20 sessions |
| `after` | The `next_cursor` value of the previous page |
| `root` | Restricts the list to one project directory |
| `project_id` | Restricts the list to one saved project |
| `include_empty` | `true` also lists the sessions with no title, no turns, and no running turn. The default is `false`. Another value gets 400 with the type `invalid-request` |

`total`, `next_cursor`, and `has_more` count the same sessions as the page.

## Fork a session

<div data-variant="python">

```python
points = client.routes.get_session_forks(session.id)["turns"]
fork = session.fork(through_turn_id=points[0]["turn_id"])
print(fork.id)
```

`client.routes.get_session_forks()` lists the turns, and `fork()` returns the handle of the new session.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/forks"
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/forks" \
  -H 'content-type: application/json' --data "{\"through_turn_id\": \"$TURN_ID\"}"
```

The first answer has `turns`. The second answer has `session`, the new session.

</div>

The turns come oldest first, each with `turn_id` and `request`. The fork keeps the turns through
`through_turn_id`. Without `through_turn_id`, the fork keeps all turns.
[What a fork keeps](sessions.md#what-a-fork-keeps) describes the copy.

The gateway refuses a session without turns, or a turn of another session, with 409.

## Organize sessions into groups

A group belongs to one project. Name the project with `root` or `project_id`.

<div data-variant="python">

```python
root = os.environ["VIS_PROJECT_ROOT"]
group = client.group("Release apps", root=root)
session.set_group(group)
new_session = client.create_session(group=group, **client.session_options(root))
```

`group()` returns the group with that name and creates it when it does not exist. The answer is a
`SessionGroup` with `id`, `name`, `project_id`, `color`, `session_count`, and `archived_at`.

- `create_session(group=...)` creates a session in a group. The session is in the project of the
  group. A `root` option can still give the session a different work folder.
- `session.set_group(group)` moves a session to a group. `session.set_group(None)` removes it from
  its group.
- `create_group(name, root=path)` always creates a group.
- `groups(root=path)` lists the groups of one project.
- `update_group(group, ...)` changes `name`, `color`, or `archived`.
- `delete_group(group)` deletes a group and keeps its sessions. With `delete_sessions=True`, it
  deletes the sessions too.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/session-groups" -H 'content-type: application/json' \
  --data '{"name": "Release apps", "root": "/srv/projects/shop"}'
vis_api -X PUT "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/group" \
  -H 'content-type: application/json' --data "{\"group_id\": \"$GROUP_ID\"}"
```

The first answer has the status 201 and the new group with its `id`. `POST /v1/sessions` with
`group_id` creates a session in a group.

- `GET /v1/session-groups?root=/srv/projects/shop` lists the groups of one project. For a project
  ID, use `project` and not `root`.
- `PATCH /v1/session-groups/{gid}` changes `name`, `color`, or `archived`.
- `DELETE /v1/session-groups/{gid}?sessions=detach` deletes a group and keeps its sessions. With
  `sessions=delete`, it deletes the sessions too.
- `PUT /v1/sessions/{sid}/group` with `{"group_id": null}` removes a session from its group.

</div>

The gateway refuses a second group with the same name in a project with 409. In a list of groups,
`archived` is `exclude`, `include`, or `only`. The group with the newest activity comes first, and
a new group counts as new activity.

## Rename, star, archive, or delete a session

<div data-variant="python">

```python
session.update(title="Release notes for 3.2")
session.update(is_favorite=True)
session.update(archived=True)
```

`session.delete()` deletes the session.

</div>

<div data-variant="http">

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID" \
  -H 'content-type: application/json' --data '{"title": "Release notes for 3.2"}'
```

The fields are `title`, `is_favorite`, and `archived`. `DELETE /v1/sessions/{sid}` deletes the
session.

</div>

Send one field in each request. The gateway refuses to archive a session while a turn runs. A
delete is permanent.

## Archive many sessions

<div data-variant="python">

```python
scope = {"root": "/srv/projects/shop", "loose": True}
preview = client.routes.post_sessions_archive(body={**scope, "dry_run": True})
result = client.routes.post_sessions_archive(body=scope)
print(len(result["archived"]), "archived,", len(result["working"]), "still working")
```

Use the count of the dry run to ask for confirmation.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/actions/archive" \
  -H 'content-type: application/json' \
  --data '{"root": "/srv/projects/shop", "loose": true, "dry_run": true}'
```

The answer is `{"archived": [...], "working": [...], "dry_run": ...}`. A request without a scope
returns 400.

</div>

Name the sessions with `root`, `project_id`, or `ids`. A blank `root` names the sessions outside a
project. With `loose` set to true, sessions in a group stay. The gateway does not archive a session
that is still working, and it lists that session under `working`.

With `dry_run` set to true, nothing changes, and `archived` lists the sessions that a real request
archives.

## Export a session

Vis does not remove private data from an export. Before you share one, read
[Export a session](sessions.md#export-a-session).

<div data-variant="python">

```python
from pathlib import Path

Path("session.md").write_bytes(session.transcript(format="markdown").content)
```

`format` is `json`, `markdown`, or `html`, and the default is `json`. The answer keeps the bytes
and the headers of the gateway.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/transcript.md" -o session.md
```

`transcript` returns JSON, `transcript.md` returns Markdown, and `transcript.html` returns a
self-contained HTML page.

</div>

## Save and restore a session snapshot

A snapshot is one `.vissnap` file with a session, the child sessions that it manages, and their
Council messages. Use it to move a session to another gateway or to keep a full backup.

A snapshot holds the whole conversation, also its private data. Keep it as safe as the session.

<div data-variant="python">

```python
from pathlib import Path

Path("release.vissnap").write_bytes(session.export())

data = Path("release.vissnap").read_bytes()
info = client.snapshot_info(data)
print(info["root_session_id"], info["counts"])
restored = client.restore_session(data, root=os.environ["VIS_PROJECT_ROOT"], key="release-1")
print(restored.id)
```

`export()` returns the snapshot bytes. `snapshot_info()` reads the manifest without a gateway
request. The manifest has `vis_version`, `created_at`, `root_session_id`, `sessions`, `counts`,
and `sizes`. `snapshot_info()` raises `ValueError` when the bytes are not a snapshot.

`restore_session()` creates a new session tree and returns the handle of its root session. `root`
sets the project folder, and `group_id` sets the session group.

A refused request raises `GatewayError` with the status and the code of the table below.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/snapshot" -o release.vissnap
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/actions/import" \
  --url-query "root=$VIS_PROJECT_ROOT" --url-query "key=release-1" \
  -H 'content-type: application/octet-stream' --data-binary @release.vissnap
```

The snapshot answer and the import body are binary, with the type `application/octet-stream`.
The query takes the optional `root`, `group_id`, and `key`. The import answer has
the status 201 and the new root session, with its `id`.

</div>

A second restore with the same `key` returns the first session and creates nothing. Without a key,
each restore creates a new tree.

A restore changes these things:

- Each restored session gets a new ID. `imported_from` names the source session, the SHA-256 of
  the snapshot, and the key.
- Your own permissions apply, not the permissions in the snapshot.
- A turn that ran at export time comes back as interrupted. Send a new message to continue the
  work.
- A Council question that waited for an answer comes back as interrupted.

A snapshot does not hold the prompt cache, the workspace files, or provider credentials. It also
does not hold sessions and messages outside the session tree. Restore the workspace files
separately.

The **Session snapshot limit** setting, `session_snapshot_limit`, sets the largest snapshot that
the machine exports or restores. The choices are `256_mib`, `512_mib`, and `1_gib`. The default is
`1_gib`.

| Status | Code | Meaning |
|---|---|---|
| 400 | `invalid-snapshot` | The file is not a Vis session snapshot, or it is damaged. |
| 400 | `unsupported-version` | A newer version of Vis made the snapshot. Update Vis on this machine. |
| 409 | `snapshot-incomplete` | The export cannot read an attachment of the session. |
| 413 | `snapshot-too-large` | The snapshot is larger than the size limit. |

## Watch live views

<div data-variant="python">

```python
for view in client.session(session_id).live_views(placement="panel"):
    print(view.id, view.title, len(view.nodes))
```

`live_views()` returns typed `LiveView` records. `client.routes.get_session_views_live(session_id)` returns the
same views as JSON in `views`. `session.live_view(view_id)` reads one view or app, also a closed
app.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/live?placement=panel"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/live/$VIEW_ID"
```

The first answer has `views`. Each view has `id`, `title`, and `nodes`. A `placement` value that
the gateway does not know returns 400.

Add `summary=true` to list each app without its body. Such an app has `"is_summary": true` and no
`nodes`, `log`, `submissions`, or `agent_note`. Read one app to get its body. The other views stay
complete. A value other than `true` or `false` returns 400.

`GET /v1/sessions/{sid}/views/live/{view-id}` answers one view or app in `view`, also a closed
app. A view of another session returns 404.

Add `since` when your client holds an app and a `view.patch` event tells it to read the app again.
Give the sequence number of the app that your client holds. The gateway then answers `patches`
and not `view`. `patches` has the frames that your client missed, oldest first. An empty list
means that your client is up to date. Each frame has `first_seq`, `patch`, and `splices`.

A splice gives one long string of a `set` or `add-node` operation as edits of a base string. The
operation comes without that key. `op_index` and `key` name the operation and its key. `node_id`
names the node when the operation adds nodes. That node is the node of `node_spec` or a node in
its `children`.

The base is the string that the node has for the key before the operation. If an earlier operation
of the frame removed the node, the base is the string that the node had when the frame started.
First compare `base_hash` with the hash of your base string. If the two are different, read the
whole app.

Then apply the edits to the base string, in their order. Each edit removes `delete` UTF-16 code
units at `start` and puts `insert` there. Each `start` is a position in the base string. A string
with changes in many places comes as many edits.

The gateway answers `view` when it does not hold the frames that your client missed, or when those
frames are not shorter than the app. A `since` that is not a whole number returns 400.

</div>

A list is a copy of the views at one moment. `placement` keeps
only the `inline`, `panel`, or `attachment` views, and
[Choose a placement](live-views.md#choose-a-placement) explains each one. Without `placement`,
you get every view of the session except the archived apps. With `archived` set to true, you get
only the archived [apps](live-views.md#apps).

A panel view also has `placement`, `key`, `extension`, and `is_stale`. `is_stale` is true for the
saved state of a view after a gateway restart, until its extension loads again.

### Change an input or press a button

A program can send the actions of a person who uses a view in the terminal or the app.

<div data-variant="python">

```python
session = client.session(session_id)
session.view_action(view_id, "change", node_id="env", value="staging")
session.view_action(view_id, "activate", node_id="refresh")
session.view_action(view_id, "submit", node_id="deploy", values={"env": "staging"})
```

`session.view_action(app_id, "shortcut", key="n")` presses an `action` shortcut of an
[app](live-views.md#add-shortcuts-to-an-app). For a `node_id` shortcut, send `activate` for that
node. A closed or disconnected app raises `GatewayError` with `status` 409, and `code` names the
reason: `closed` or `disconnected`.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/$VIEW_ID/actions" \
  -H 'content-type: application/json' --data '{"action": "change", "node_id": "env", "value": "staging"}'
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/$VIEW_ID/actions" \
  -H 'content-type: application/json' --data '{"action": "activate", "node_id": "refresh"}'
```

`submit` sends `node_id` and `values`. `{"action": "shortcut", "key": "n"}` presses an `action`
shortcut of an [app](live-views.md#add-shortcuts-to-an-app). A closed app returns 409 with
`reason` `closed`. An invalid key returns 400.

</div>

| Action | Effect |
|---|---|
| `change` | Sets the `value` of one input |
| `activate` | Presses a button or a selectable card |
| `submit` | Presses a submit button. [Collect input in a view](live-views.md#collect-input-in-a-view) lists the `values` that it sends |
| `shortcut` | Presses a shortcut. An accepted press answers the `shortcut` id. A key without a shortcut answers `is_accepted` false and `reason` `unbound` |

### Run an app from a program

Your program can own an [app](live-views.md#run-an-app-from-your-own-program) in a session. It
opens the app, patches it, and reads its events. Only the owner that opened an app can change it.

<div data-variant="python">

```python
import blockether.vis.extension as vis

session = client.session(session_id)
app = session.attach(
    vis.ui.app("Release checklist", vis.ui.button("ship", label="Ship"), key="release"),
    owner="release-bot",
)
app.run(lambda event: print(event.type, event.node_id))
```

`session.attach(app, *, owner, on_event=None)` returns a `LiveAttachment`. `owner` has 1 to 64
characters of `a-z`, `0-9`, `.`, `_`, and `-`. With `key`, a second call returns the open app of
the same owner and key. `on_event=callback` reads the events on a background thread until the app
closes. `client.routes.post_session_apps(session_id, body=...)` sends one raw envelope.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/apps" \
  -H 'content-type: application/json' \
  --data '{"owner": "release-bot", "envelope": {"op": "open", "view": {"title": "Release checklist", "placement": "attachment", "key": "release", "nodes": [{"type": "button", "id": "ship", "label": "Ship"}]}}}'
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/apps" \
  -H 'content-type: application/json' \
  --data '{"owner": "release-bot", "envelope": {"op": "events", "view_id": "'"$APP_ID"'", "after": 0, "timeout_ms": 25000}}'
```

`POST /v1/sessions/{sid}/apps` takes `owner` and one `envelope`. The `open` envelope answers
`view_id`, `is_open`, and `view`. Use `view_id` in the `patch`, `state`, `events`, `close`, and
`archive` envelopes.

- Vis records the owner as `sdk/<owner>`. An app of another owner or session returns 403.
- Only `placement` `attachment` opens. An unknown session returns 404, and a malformed body
  returns 400.
- An `events` envelope waits at most 30000 ms in `timeout_ms`. It answers `cursor` and `events`.
  Send the `cursor` as `after` in the next poll.
- An `open` envelope answers `events_cursor`. Send it as `after` in your first poll, so that you
  get no event from before your open.
- After a gateway restart, an `events` envelope answers `is_stale` true and no events. Send your
  `open` envelope again with the same `key`, and poll from its new `events_cursor`.
- While the app is stale, a press, a selection, an input change, or a shortcut answers 409 with
  `reason` `disconnected`.

</div>

### Run a global app from a program

A global app belongs to no session. People open it with the global apps button in the TUI and in
the Companion app. Use it for a program that serves every session, for example a build monitor.
Only a program opens a global app, and the app needs a `key`.

<div data-variant="python">

```python
import blockether.vis.extension as vis

app = client.attach_app(
    vis.ui.app("Build lights", vis.ui.button("refresh", label="Refresh"), key="lights"),
    owner="ci-bot",
)
app.run(lambda event: print(event.type, event.node_id))
```

`client.attach_app(app, *, owner, on_event=None)` returns a `LiveAttachment`. It works like
`session.attach`, and it opens the app as `scope` `global`. A second call with the same owner and
key returns the open app.

- By default, a global app comes back after a gateway restart. It stays stale until its owner
  opens the same key again.
- Give `vis.ui.app(..., scope="global", is_persistent=False)` for an app that ends at a restart.
- Keep the data of your app in your own store. Open the key again with that data after a restart.
- `client.apps()` lists the global apps, and `client.app(view_id)` reads one.
- `client.app_action(view_id, action, ...)` sends an operator action, as `session.view_action`
  does.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/apps" \
  -H 'content-type: application/json' \
  --data '{"owner": "ci-bot", "envelope": {"op": "open", "view": {"title": "Build lights", "placement": "attachment", "key": "lights", "nodes": [{"type": "button", "id": "refresh", "label": "Refresh"}]}}}'
vis_api "$VIS_GATEWAY_URL/v1/apps"
```

`POST /v1/apps` takes the body of `POST /v1/sessions/{sid}/apps`. It opens the app with `scope`
`global`, and the other envelopes work in the same way.

- An `open` envelope needs a `key`. An app with a `session_id` returns 403.
- Give `"is_persistent": false` in the view for an app that ends at a gateway restart.
- `GET /v1/apps` answers `views`, oldest first. Add `archived=true` to list only the archived
  apps. The list is empty while the person turns global apps off.
- Add `summary=true` to `GET /v1/apps` to list each app without its body, as for the apps of a
  session.
- `GET /v1/apps/{view-id}` answers one `view`. It takes `since` as the read of a session app
  does. `POST /v1/apps/{view-id}/actions` takes an operator action.
- The fleet stream, `GET /v1/events?scope=fleet`, carries the `view.open`, `view.patch`, and
  `view.close` events of global apps, without `session_id`.

</div>

### Read a log page

<div data-variant="python">

```python
page = client.routes.get_session_views_live_log(
    session_id, view_id, "build/log", query={"query": "error", "from": 0, "limit": 100}
)
for number, line in zip(page["line_numbers"], page["lines"]):
    print(number, line)
```

The method sends the node ID as the `node` query parameter.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/live/$VIEW_ID/log?node=build%2Flog&query=error&from=0&limit=100"
```

The `node` query parameter names the log node, because a node ID can contain `/`. Encode `/` as
`%2F`.

</div>

The answer has `lines`, `line_numbers`, `matched`, and `total`.

### Interrupt a view

<div data-variant="python">

```python
client.session(session_id).view_action(view_id, "interrupt", note="Stop monitoring")
```

`view_action` checks the body before it sends the request.
`client.routes.post_session_view(session_id, view_id, body={"action": "interrupt", "note": "Stop monitoring"})`
sends the same request without the check.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/$VIEW_ID/actions" \
  -H 'content-type: application/json' --data '{"action": "interrupt", "note": "Stop monitoring"}'
```

</div>

## Read Activity history

<div data-variant="python">

```python
after, revision = 0, None
while after is not None:
    query = {"after": after, "limit": 32}
    if revision is not None:
        query["revision"] = revision
    page = client.routes.get_session_activity(session_id, activity_id, query=query)
    for row in page["rows"]:
        print(row)
    revision = page["history"]["revision"]
    after = page["history"]["next_after"]
```

The `revision` keeps all pages on the same version of the history.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/activity/$ACTIVITY_ID?after=0&limit=32&q=timeout"
```

The answer has `rows` and `history`. Send `history.next_after` as `after` to read the next page,
and send `history.revision` as `revision`.

</div>

The gateway refuses a page of a changed history with 409. Read the history again from the first
page.

### Export Activity history

<div data-variant="python">

```python
response = client.routes.get_session_activity_export(session_id, activity_id, query={"revision": revision})
print(response.content.decode())
```

An incomplete export raises `ProtocolError`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/activity/$ACTIVITY_ID/export?revision=$REVISION" \
  -o activity.txt
```

An export that ends with the line `INCOMPLETE EXPORT: Activity changed. Reload and retry.` is not
complete.

</div>

After an incomplete export, read the history again and retry.

## See also

- To learn what a session keeps and how to use it in the terminal and the apps, read
  [Sessions](sessions.md).
- To install the SDK, connect a client, and handle gateway errors, read [Python SDK](python-sdk.md).
- To build the views that your program watches, read [Live views](live-views.md).
- For authentication, the OpenAPI document, and gateway errors, read [HTTP API](http-api.md).
