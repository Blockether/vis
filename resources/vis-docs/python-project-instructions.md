# Project instructions in Python

Run prompt templates, skills and goals from a Python program. The calls send the same commands that
you type in a session.

## When to use

- **Your program must run a saved prompt template or a skill.** [Run a prompt template or a
  skill](#run-a-prompt-template-or-a-skill) with one message.
- **A script must show the commands that a session accepts.** [List slash
  commands](#list-slash-commands).
- **A batch job must continue until a result is verified.** [Set a goal](#set-a-goal) with an
  iteration budget.
- **A dashboard must show the progress of a goal.** [Read the goal state](#read-the-goal-state).
- **A program must stop or continue a goal.** [Pause, resume or cancel a
  goal](#pause-resume-or-cancel-a-goal).

To learn how project rules, prompt templates, skills and goals work, read [Project
instructions](project-instructions.md). To send the same requests from another language, read
[Project instructions over HTTP](http-project-instructions.md).

## Before you start

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client` and `session`, a
`Session` from `client.session(session_id)`.

Vis adds the project rules in `AGENTS.md` and the system prompt files to every message. Your program
does not send them. [Project rules: AGENTS.md](project-instructions.md#project-rules-agents-md)
explains the files.

## Operations

Typed calls check the answers and return records. Generated methods return the JSON answer of one
request. [Project instructions over HTTP](http-project-instructions.md) lists the same operations as
HTTP requests.

| Task | Typed call | Generated method |
|---|---|---|
| List slash commands | — | `get_session_slashes(sid, query=...)` |
| Run a prompt template or a skill | `session.send(request)` | `post_session_turns(sid, body=...)` |
| Set a goal | `session.goal(objective, ...)` | `post_session_turns(sid, body=...)` |
| Read the goal state | `session.read()` | `get_session(sid)` |
| Pause, resume or cancel a goal | `session.send(request)` | `post_session_turns(sid, body=...)` |

## List slash commands

```python
commands = client.get_session_slashes(session.id, query={"channel": "tui"})["commands"]
for command in commands:
    print(command["name"], command["doc"])
```

The list has the commands of the extensions, the commands of the channel and the prompt templates of
the project. The `channel` is `web` or `tui`, `web` by default. Skills are not in the list. Each
skill has the command `/skill:<name>`, as [Skills](skills.md) explains.

## Run a prompt template or a skill

```python
review = session.send("/review error handling")
print(review.wait(timeout=600)["status"])

checklist = session.send("/skill:release-checklist Prepare the 2.4.0 release")
```

A template sends its file as the message, with `$ARGUMENTS` replaced by the text after the name. A
slash command of an extension wins over a template with the same name. A skill command loads the
skill and gives it the task after the name.

## Set a goal

```python
turn = session.goal("Implement the parser fix and run its regression tests", iteration_budget=30)
result = turn.wait(timeout=3600)
print(result["status"])
```

`goal()` sends a `/goal` command and returns the same `Turn` as `send()`. A session has one goal at a
time, and a new goal replaces it. `iteration_budget` limits the model iterations of the goal. Without
it, the goal has no limit of its own. Follow the work with `turn.wait()` or the event stream in
[Follow progress](python-sessions.md#follow-progress).

## Read the goal state

```python
goal = session.read()["goal"]
if goal:
    print(goal["status"], goal["iterations_used"], goal["iteration_budget"])
```

The session detail and each row of `client.list_sessions()` have `goal`, an object or `None`. The
event stream also sends it in the `subscription.ready` event, also when the session is idle.

- `status` is `active`, `paused`, `blocked`, `budget_limited`, `complete` or `cancelled`. The apps
  show `budget_limited` as "iteration-limit reached".
- `iteration_budget` is a positive integer or `None`. `iterations_used` is zero or more.
- `tokens_used` counts the measured input, output and cached input. It is for statistics and does
  not limit the work.
- `time_used_ms` is the active time up to `updated_at`. While the goal is active, the elapsed time
  is `time_used_ms + max(0, now - updated_at)` milliseconds.

## Pause, resume or cancel a goal

```python
session.send("/goal --pause").wait(timeout=60)
session.send("/goal --resume")
session.send("/goal --cancel")
```

These commands wait in the normal message queue. To stop the work at once, cancel the running turn
as in [Queue and cancel messages](python-sessions.md#queue-and-cancel-messages). The SDK has no other
methods to read or change a goal.

## See also

- [Project instructions](project-instructions.md) — project rules, prompt templates, skills and goals.
- [Project instructions over HTTP](http-project-instructions.md) — the same operations as HTTP
  requests.
- [Python SDK basics](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
