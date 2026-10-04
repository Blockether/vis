# Sessions over HTTP

Create [sessions](sessions.md), send messages and follow the work with HTTP requests from any
language. The requests and answers are JSON, except the event stream and the exports.

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
[Sessions](sessions.md). To use typed calls from Python, read [Sessions in
Python](python-sessions.md).

## Before you start

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. They also use `SESSION_ID` for the ID of a session that you created or found.

## Operations

The HTTP API has the same operations as the [Python SDK](python-sessions.md).

| Task | Method and path |
|---|---|
| Create a session | `POST /v1/sessions` |
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
| Rename, star or archive a session | `PATCH /v1/sessions/{sid}` |
| Delete a session | `DELETE /v1/sessions/{sid}` |
| Export a transcript | `GET /v1/sessions/{sid}/transcript`, `GET /v1/sessions/{sid}/transcript.md`, `GET /v1/sessions/{sid}/transcript.html` |
| List live views | `GET /v1/sessions/{sid}/views/live` |
| Read a page of a log node | `GET /v1/sessions/{sid}/views/live/{view-id}/log` |
| Read one page of Activity history | `GET /v1/sessions/{sid}/activity/{aid}` |
| Export the full Activity history | `GET /v1/sessions/{sid}/activity/{aid}/export` |

## Create a session

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions" -H 'content-type: application/json' \
  --data '{"root": "/srv/projects/shop", "channel": "app"}'
```

The answer has the status 201 and the new session with its `id`. `root` must be an absolute path on
the gateway machine. The `app` channel makes the session visible in the app. Add `group_id` to put
the new session in a group.

To continue a saved session, use its `id` in the paths of the next requests.

## Send a message and wait

```bash
cursor="$(vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/seq" |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["seq"])')"
turn_id="$(vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns" \
  -H 'content-type: application/json' \
  --data '{"request": "Summarize TODO.md without changing files.", "idempotency_key": "todo-1"}' |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["turn_id"])')"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns/$turn_id"
```

Read the cursor before you send the message. [Follow progress](#follow-progress) uses it to replay
the events of this turn. The `turn_id` comes back when the gateway accepts the message. The model has
not finished yet.

Read the turn again until its `status` is `completed`, `failed`, `cancelled`, `suspended` or
`error`. A running turn has the status `streaming`, and a waiting message has `queued`. A failed
status is data in the answer, not an HTTP error. Check it before you use `content`.

A message can also be a slash command, for example `/rename Release notes`. The body also takes
`provider` and `model` to select a configured alternative for this message. To retry after a network
failure without a second message, send the same `idempotency_key` again.

## Follow progress

```bash
vis_api -N "$VIS_GATEWAY_URL/v1/events?sids=$SESSION_ID:$cursor&replay=full"
```

The answer is a Server-Sent Events stream. Each event has an `id` line with its sequence number, an
`event` line with its type and a `data` line with the event as JSON. `replay=full` also replays every
stored event after the cursor.

The stream follows the whole session and does not end with a turn. Stop when a `turn.completed`,
`turn.failed` or `turn.cancelled` event has your `turn_id`. Closing the stream does not cancel work.
To reconnect, send the last sequence number as the cursor.

A client that cannot keep a stream open can read pages instead:

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/events-since?cursor=$cursor&replay=full"
```

The answer has `events`. Each event has `seq`, `ts`, `session_id` and `type`. Send the `seq` of the
last event as the next `cursor`.

## Answer a form

A turn can stop and wait for a person, for example when an extension asks for a value. The question
is an input form:

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/input"
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/$VIEW_ID/actions" \
  -H 'content-type: application/json' \
  --data '{"action": "submit", "values": {"env": "staging"}}'
```

The first answer has `requests`, one entry for each open form, with its `id`, `title` and `fields`.
The values are keyed by the field `name`, as in [The answer](human-input.md#the-answer). To close a
form without values, send `{"action": "cancel"}`.

Do not answer credential or permission forms automatically. A person must decide them.

## Queue and cancel messages

A message that you send while a turn runs waits in a queue. Queued messages run in the order that
you sent them.

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/turns?status=queued"
```

The answer has `turns`. Without `status=queued`, it contains every turn with its full content. Use
these requests to change the queue or stop work:

- `PATCH /v1/sessions/{sid}/turns/{tid}` with `{"request": "New text"}` changes a queued message.
- `DELETE /v1/sessions/{sid}/turns/{tid}` removes a queued message.
- `POST /v1/sessions/{sid}/turns/{tid}/cancel` cancels a turn.
- `POST /v1/sessions/{sid}/cancel-current` with `{"idempotency_key": "todo-1"}` cancels the running
  turn when your program lost its turn ID. The key must be the `idempotency_key` of the message that
  started the turn.
- `POST /v1/sessions/{sid}/resume-queue` resumes a queue that a failed turn paused. It starts the
  next queued message.
- `POST /v1/sessions/{sid}/drain-queue` starts the oldest queued message when no turn runs.

A change to a turn that is no longer queued returns 409. The queue is stored in memory, so a gateway
restart clears it.

## Find a saved session

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/actions/search?q=release%20notes&limit=20"
```

The terminal and the apps use the same search. The answer lists session rows with the same fields
as the rows of `GET /v1/sessions`. The rows are in order of
recent activity, with the most recent first.

- With an empty `q`, the answer lists your recent sessions.
- With words in `q`, the answer lists the sessions whose title or conversation text
  matches. Each of these rows also has a `match` object. It tells where the words
  matched and gives short text around each match.
- `project_id` restricts results to one saved project.
- `root` restricts results to one project directory. An empty `root` selects sessions without a project.
- `group_ids` selects any group in a comma-separated list. An empty value selects no groups.
- Without these parameters, the search includes every project and group.
- Scopes apply before `total`, the page window and its cursor are calculated.
- `limit` sets the page size, from 1 to 1000. The default is 50.
- `total` is the number of sessions in all pages.
- To read the next page, send the `next_cursor` value as `after`. When `has_more`
  is `false`, there are no more pages.
- `archived=exclude` lists active sessions, `include` lists all sessions and `only`
  lists archived sessions. The default is `exclude`.

## Fork a session

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/forks"
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/forks" \
  -H 'content-type: application/json' --data "{\"through_turn_id\": \"$TURN_ID\"}"
```

The first answer has `turns`, oldest first, with `turn_id` and `request`. The fork keeps the turn
that you name and every turn before it. Without `through_turn_id`, the fork keeps all turns. The
answer has `session`, the new session. [What a fork keeps](sessions.md#what-a-fork-keeps) describes
the copy.

A session without turns, or a turn of another session, returns 409.

## Organize sessions into groups

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/session-groups" -H 'content-type: application/json' \
  --data '{"name": "Release apps", "root": "/srv/projects/shop"}'
vis_api -X PUT "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/group" \
  -H 'content-type: application/json' --data "{\"group_id\": \"$GROUP_ID\"}"
```

A group belongs to one project. Name the project with `root` or `project_id`. The first answer has
the status 201 and the new group with its `id`. A second group with the same name in a project
returns 409.

- `GET /v1/session-groups?root=/srv/projects/shop` lists the groups of one project. Use `project`
  with a project ID instead of `root`. `archived` is `exclude`, `include` or `only`.
- `PATCH /v1/session-groups/{gid}` changes `name`, `color`, `position` or `archived`.
- `DELETE /v1/session-groups/{gid}?sessions=detach` deletes a group and keeps its sessions. With
  `sessions=delete`, the sessions are deleted too.
- `PUT /v1/sessions/{sid}/group` with `{"group_id": null}` removes a session from its group.

## Rename, star, archive or delete a session

```bash
vis_api -X PATCH "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID" \
  -H 'content-type: application/json' --data '{"title": "Release notes for 3.2"}'
```

Send one field in each request: `title`, `is_favorite` or `archived`. The gateway refuses to archive
a session while a turn runs. `DELETE /v1/sessions/{sid}` deletes the session permanently.

## Export a session

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/transcript.md" -o session.md
```

`transcript` returns JSON, `transcript.md` returns Markdown and `transcript.html` returns a
self-contained HTML page.

Vis does not remove private data from an export. Read an export before you share it. To remove
private details, follow [Reporting a bug](reporting-bugs.md#sharing-a-transcript).

## Watch live views

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/live"
```

The answer has `views`. Each view has `id`, `title` and `nodes`. A list is a snapshot. It does not
follow later updates.

### Read a log page

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/live/$VIEW_ID/log?node=build%2Flog&query=error&from=0&limit=100"
```

The `node` query parameter names the log node, because a node ID can contain `/`. Encode `/` as
`%2F`. The answer has `lines`, `line_numbers`, `matched` and `total`.

### Interrupt a view

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/views/$VIEW_ID/actions" \
  -H 'content-type: application/json' --data '{"action": "interrupt", "note": "Stop monitoring"}'
```

## Read Activity history

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/activity/$ACTIVITY_ID?after=0&limit=32&q=timeout"
```

The answer has `rows` and `history`. Send `history.next_after` as `after` to read the next page, and
send `history.revision` as `revision`. A changed history returns 409. Read the history again from the
first page.

### Export Activity history

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/activity/$ACTIVITY_ID/export?revision=$REVISION" \
  -o activity.txt
```

An export that ends with the line `INCOMPLETE EXPORT: Activity changed. Reload and retry.` is not
complete. Read the history again and retry.

## See also

- [Sessions](sessions.md) — what a session keeps and how to use it in the terminal and the apps.
- [Sessions in Python](python-sessions.md) — the same operations as typed Python calls.
- [HTTP API basics](http-api.md) — authentication, the OpenAPI document and gateway errors.
- [Live views](live-views.md) — build the views that your program watches.
