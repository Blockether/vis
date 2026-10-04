# Project instructions over HTTP

Run prompt templates, skills and goals with HTTP requests from any language. The requests send the
same commands that you type in a session.

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
instructions](project-instructions.md). To use typed calls from Python, read [Project instructions in
Python](python-project-instructions.md).

## Before you start

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. They also use `SESSION_ID` for the ID of a session that you created or found.

Vis adds the project rules in `AGENTS.md` and the system prompt files to every message. Your program
does not send them. [Project rules: AGENTS.md](project-instructions.md#project-rules-agents-md)
explains the files.

## Operations

The HTTP API has the same operations as the [Python SDK](python-project-instructions.md).

| Task | Method and path |
|---|---|
| List slash commands | `GET /v1/sessions/{sid}/slashes` |
| Run a prompt template or a skill | `POST /v1/sessions/{sid}/turns` |
| Set a goal | `POST /v1/sessions/{sid}/turns` |
| Read the goal state | `GET /v1/sessions/{sid}` |
| Pause, resume or cancel a goal | `POST /v1/sessions/{sid}/turns` |

## List slash commands

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/slashes?channel=tui"
```

The answer has `commands`, with `name` and `doc` for each command. The list has the commands of the
extensions, the commands of the channel and the prompt templates of the project. The `channel` is
`web` or `tui`, `web` by default. Skills are not in the list. Each skill has the command
`/skill:<name>`, as [Skills](skills.md) explains.

## Run a prompt template or a skill

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/review error handling", "idempotency_key": "review-1"}'
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/skill:release-checklist Prepare the 2.4.0 release", "idempotency_key": "release-1"}'
```

Each answer has the `turn_id` of the message. A template sends its file as the message, with
`$ARGUMENTS` replaced by the text after the name. A slash command of an extension wins over a
template with the same name. A skill command loads the skill and gives it the task after the name.

## Set a goal

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/goal --budget 30 -- Implement the parser fix and run its regression tests", "idempotency_key": "goal-1"}'
```

A goal is a message with the `/goal` command. A session has one goal at a time, and a new goal
replaces it. `--budget` limits the model iterations of the goal. Without it, the goal has no limit of
its own. Read the turn, or follow the event stream as in [Follow
progress](http-sessions.md#follow-progress).

## Read the goal state

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID" |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["goal"])'
```

The session detail and each row of the session list have `goal`, an object or `null`. The event
stream also sends it in the `subscription.ready` event, also when the session is idle.

- `status` is `active`, `paused`, `blocked`, `budget_limited`, `complete` or `cancelled`. The apps
  show `budget_limited` as "iteration-limit reached".
- `iteration_budget` is a positive integer or `null`. `iterations_used` is zero or more.
- `tokens_used` counts the measured input, output and cached input. It is for statistics and does
  not limit the work.
- `time_used_ms` is the active time up to `updated_at`. While the goal is active, the elapsed time
  is `time_used_ms + max(0, now - updated_at)` milliseconds.

## Pause, resume or cancel a goal

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" -H 'content-type: application/json' \
  --data '{"request": "/goal --pause", "idempotency_key": "goal-pause-1"}'
```

Send `/goal --resume` or `/goal --cancel` in the same way. These commands wait in the normal message
queue. To stop the work at once, cancel the running turn as in [Queue and cancel
messages](http-sessions.md#queue-and-cancel-messages). The API has no other requests to read or
change a goal.

## See also

- [Project instructions](project-instructions.md) — project rules, prompt templates, skills and goals.
- [Project instructions in Python](python-project-instructions.md) — the same operations as typed
  Python calls.
- [HTTP API basics](http-api.md) — authentication, the OpenAPI document and gateway errors.
