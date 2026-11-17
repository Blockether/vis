# Project instructions API

Your program runs prompt templates, skills, and goals with the same commands that you type in a
session.

The examples use the [Python SDK](python-sdk.md). For the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **Your program must run a saved prompt template or a skill.**
  [Run a prompt template or a skill](#run-a-prompt-template-or-a-skill) with one message.
- **A script must show the commands that a session accepts.**
  [List slash commands](#list-slash-commands).
- **A batch job must continue until a result is verified.** [Set a goal](#set-a-goal) with an
  iteration budget.
- **A dashboard must show the progress of a goal.** [Read the goal state](#read-the-goal-state).
- **A program must stop or continue a goal.**
  [Pause, resume or cancel a goal](#pause-resume-or-cancel-a-goal).

To learn how project rules, prompt templates, skills, and goals work, read
[Project instructions](project-instructions.md).

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in
[Connect to a gateway and run a task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The
examples use `client` and `session`, a `Session` from `client.session(session_id)`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples also use `SESSION_ID`, the ID of a session that you created or found.

</div>

Your program does not send `AGENTS.md` or the system prompt files. Vis adds them to every message,
as [Project rules: AGENTS.md](project-instructions.md#project-rules-agents-md) explains.

## Operations

<div data-variant="python">

A typed call checks the answer and returns a record. A generated method returns the JSON answer of
one request.

| Task | Typed call | Generated method |
|---|---|---|
| List slash commands | No typed call | `get_session_slashes(sid, query=...)` |
| Run a prompt template or a skill | `session.send(request)` | `post_session_turns(sid, body=...)` |
| Set a goal | `session.goal(objective, ...)` | `post_session_turns(sid, body=...)` |
| Read the goal state | `session.read()` | `get_session(sid)` |
| Pause, resume or cancel a goal | `session.send(request)` | `post_session_turns(sid, body=...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| List slash commands | `GET /v1/sessions/{sid}/slashes` |
| Run a prompt template or a skill | `POST /v1/sessions/{sid}/turns` |
| Set a goal | `POST /v1/sessions/{sid}/turns` |
| Read the goal state | `GET /v1/sessions/{sid}` |
| Pause, resume or cancel a goal | `POST /v1/sessions/{sid}/turns` |

</div>

## List slash commands

<div data-variant="python">

```python
commands = client.get_session_slashes(session.id, query={"channel": "tui"})["commands"]
for command in commands:
    print(command["name"], command["doc"])
```

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/slashes?channel=tui"
```

The answer has `commands`, with `name` and `doc` for each command.

</div>

The list has the commands of the extensions, the commands of the channel, and the prompt templates
of the project. The `channel` is `web` or `tui`, and the default is `web`. Skills are not in the
list. Each skill has the command `/skill:<name>`, as [Skills](skills.md) explains.

A command with `saveable` set to true is prompt content, such as `/goal`. A client can keep that
command in its prompt history. Other commands do not have the field.

## Run a prompt template or a skill

<div data-variant="python">

```python
review = session.send("/review error handling")
print(review.wait(timeout=600)["status"])

checklist = session.send("/skill:release-checklist Prepare the 2.4.0 release")
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/review error handling", "idempotency_key": "review-1"}'
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/skill:release-checklist Prepare the 2.4.0 release", "idempotency_key": "release-1"}'
```

Each answer has the `turn_id` of the message.

</div>

A template sends its file as the message, with `$ARGUMENTS` replaced by the text after the name. A
slash command of an extension wins over a template with the same name. A skill command loads the
skill and gives it the task after the name.

## Set a goal

<div data-variant="python">

```python
turn = session.goal("Implement the parser fix and run its regression tests", iteration_budget=30)
result = turn.wait(timeout=3600)
print(result["status"])
```

`goal()` sends a `/goal` command and returns the same `Turn` as `send()`. `iteration_budget` limits
the model iterations of the goal. Follow the work with `turn.wait()` or the event stream in
[Follow progress](sessions-api.md#follow-progress).

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/goal --budget 30 -- Implement the parser fix and run its regression tests", "idempotency_key": "goal-1"}'
```

A goal is a message with the `/goal` command. `--budget` limits the model iterations of the goal.
Read the turn, or follow the event stream as in [Follow progress](sessions-api.md#follow-progress).

</div>

A session has one goal at a time, and a new goal replaces it. Without a budget, the goal has no
limit of its own. [Explicit goals](project-instructions.md#explicit-goals-goal) explains how a goal
runs and ends.

## Read the goal state

<div data-variant="python">

```python
goal = session.read()["goal"]
if goal:
    print(goal["status"], goal["iterations_used"], goal["iteration_budget"])
```

The session detail and each row of `client.list_sessions()` have `goal`, an object or `None`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID" |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["goal"])'
```

The session detail and each row of the session list have `goal`, an object or `null`.

</div>

The event stream also sends `goal` in the `subscription.ready` event, also when the session is
idle. The goal has no reason text. The final reply of the turn gives the evidence or the blocker.

| Field | Value |
|---|---|
| `status` | `active`, `paused`, `blocked`, `budget_limited`, `complete`, `cancelled`, or `cleared`. The apps show `budget_limited` as "iteration-limit reached" and show no goal for `cleared` |
| `iteration_budget` | A positive integer, or `None` in Python and `null` in JSON |
| `iterations_used` | Zero or more |
| `tokens_used` | The measured input, output, and cached input. It is for statistics and does not limit the work |
| `time_used_ms` | The active time up to `updated_at` |

While the goal is active, the elapsed time is `time_used_ms + max(0, now - updated_at)`
milliseconds.

## Pause, resume, or cancel a goal

<div data-variant="python">

```python
session.send("/goal --pause").wait(timeout=60)
session.send("/goal --resume")
session.send("/goal --cancel")
session.send("/goal --clear")
```

To stop the work at once, cancel the running turn as in
[Queue and cancel messages](sessions-api.md#queue-and-cancel-messages). The SDK has no other
methods to read or change a goal.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/goal --pause", "idempotency_key": "goal-pause-1"}'
```

Send `/goal --resume`, `/goal --cancel`, or `/goal --clear` in the same way. To stop the work at
once, cancel the running turn as in
[Queue and cancel messages](sessions-api.md#queue-and-cancel-messages). The API has no other
requests to read or change a goal.

</div>

These commands wait in the normal message queue. `--clear` forgets the goal. Its status becomes
`cleared`, and the model no longer sees it.

## See also

- To learn how project rules, prompt templates, skills, and goals work, read
  [Project instructions](project-instructions.md).
- To install the SDK, connect a client, and handle gateway errors, read [Python SDK](python-sdk.md).
- For authentication, the OpenAPI document, and gateway errors, read [HTTP API](http-api.md).
