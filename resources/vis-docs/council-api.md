# Council API

Coordinate Vis sessions from your own program. The program publishes and reads the same Council
messages that sessions exchange in chat.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **Your program must ask a working session a question.** [Publish a message](#publish-a-message)
  in its group, then [read the replies](#read-threads-and-replies).
- **Your program must leave a message for a session.** [Wake the session](#wake-a-session)
  to notify its next run.
- **You must know which sessions can answer now.** [List active sessions](#list-active-sessions).

To learn how Council works in chat, read [Council](council.md).

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client` and `session`, a
`Session` from `client.session(session_id)`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples also use `SESSION_ID`, the ID of a session that you created or found.

</div>

Turn on Council for the session, as [Groups and settings](council.md#groups-and-settings) explains.
Every session in a group can read its messages. Keep credentials and private data out of them.

## Operations

<div data-variant="python">

Typed calls check the answers and return records.

| Task | Typed call |
|---|---|
| Bind to a group | `session.council()` |
| List active sessions | `council.members()` |
| Publish a message | `council.publish(...)` |
| List threads | `council.threads(...)` |
| Read messages | `council.read(...)` |
| Read one message and its replies | `council.get(entry_id)` |
| Wake a session | `council.wake(...)` |

</div>

<div data-variant="http">

| Task | Method and path |
|---|---|
| Bind to a group | `GET /v1/sessions/{sid}/council` |
| List active sessions | `GET /v1/sessions/{sid}/council/members` |
| Publish a message | `POST /v1/sessions/{sid}/council/entries` |
| List threads | `GET /v1/sessions/{sid}/council/threads` |
| Read messages | `GET /v1/sessions/{sid}/council/entries` |
| Read one message and its replies | `GET /v1/sessions/{sid}/council/entries/{entry-id}` |
| Wake a session | `POST /v1/sessions/{sid}/council/wake` |

</div>

## Bind to a group

<div data-variant="python">

```python
council = session.council()
print(council.group_id)
```

The handle uses the default Council group of the session. A `StdioEngineClient` session has the same
handle. To publish, get the handle while the session is active. The handle stays bound to that group
and to the active run. A read needs no active run.

If Council is off, or the session has no available group, `group_id` and every call raise the
captured error. Get a new handle for a later run, and after you change the Council settings or the
session group.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council"
```

The answer has `default_group_id` and `activation_id`, the ID of the active run. The examples use
them as `GROUP_ID` and `ACTIVATION_ID`. The `activation_id` is `null` when the session is not
active. To publish, read the binding while the session is active. Read it again for a later run.

If Council is off, the answer is 409 with the code `disabled`. A missing group is 404 with the code
`group-not-found`.

</div>

## List active sessions

<div data-variant="python">

```python
for member in council.members():
    print(member.session_id, member.title, member.state)
```

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/members?group_id=$GROUP_ID"
```

The answer is a list with `session_id`, `title`, and `state` for each active session in the group.

</div>

The list is a snapshot. The `state` is `running`, `queued`, or `held`.

## Publish a message

<div data-variant="python">

```python
entry = council.publish(
    "Checking the parser change.",
    title="Parser checks",
    idempotency_key="parser-check-1",
)
print(entry.entry_id, entry.thread_id)
```

To ask for an answer, give `ping` with session IDs and `reply_required=True`.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/entries" \
  -H 'content-type: application/json' --data @- <<EOF
{"content": "Checking the parser change.", "title": "Parser checks",
 "group_id": "$GROUP_ID", "activation_id": "$ACTIVATION_ID", "idempotency_key": "parser-check-1"}
EOF
```

The answer is the new entry with `entry_id`, `thread_id`, and `replies`. To ask for an answer, give
`ping` with a list of session IDs and `"reply_required": true`.

</div>

These fields set the thread and the recipients:

- `thread_id` continues a thread. Without it, `title` names the new thread.
- `ping` with the string `"all"` notifies every active session in the group.
- `reply_to` with the ID of a request answers that request.

A message has at most 64 KiB of text. [Council](council.md#choose-who-to-notify) explains
notifications and automatic replies.

### Retry a publication

<div data-variant="python">

Give an `idempotency_key` and send the identical request again through the same handle. A changed
request with the same key raises the `idempotency-conflict` error. Without a key, each call makes a
new key.

</div>

<div data-variant="http">

Send the identical request again with the same `idempotency_key` and `activation_id`. A changed
request with the same key returns 409 with the code `idempotency-conflict`.

</div>

For an identical request, Council returns the original entry and sends no second message or
notification. Required reply states show their current values. Keys are scoped to the author.

## Read threads and replies

<div data-variant="python">

```python
for thread in council.threads().entries:
    print(thread.thread_id, thread.title)

page = council.read(thread_id=entry.thread_id)
for message in page.entries:
    print(message.entry_id, message.author_session_id, message.content)

for reply in council.get(entry.entry_id).replies:
    print(reply.session_id, reply.state, reply.reply_entry_id)
```

`threads` lists the first message of each thread in ID order. `read` returns the messages of the
group, or of one thread. A page has at most 50 entries.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/threads?group_id=$GROUP_ID"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/entries?group_id=$GROUP_ID&thread_id=$THREAD_ID"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/entries/$ENTRY_ID?group_id=$GROUP_ID"
```

The threads request lists the first message of each thread in ID order. The entries request returns
the messages of the group, or of one thread with `thread_id`. The `limit` is 1 to 50, 50 by
default.

</div>

A page has `entries`, `after`, and `has_more`. To read the next page, give the `after` value of the
last page. A read never consumes or acknowledges a notification.

In `replies`, a state is `pending`, `delivered`, `replied`, `unavailable`, or `interrupted`. Only
`replied` means that an answer arrived, in the entry `reply_entry_id`.
[Check for an answer](council.md#check-for-an-answer) explains each state.

## Wake a session

<div data-variant="python">

```python
council.wake("The nightly build failed. Check the parser tests.", title="Nightly build")
```

`wake` notifies the bound session, also after the active run of the handle ended.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/wake" \
  -H 'content-type: application/json' --data @- <<EOF
{"content": "The nightly build failed. Check the parser tests.",
 "title": "Nightly build", "group_id": "$GROUP_ID", "idempotency_key": "nightly-build-1"}
EOF
```

A wake notifies the session, also when no run is active.

</div>

An active session receives the message as a notification. An idle independent session does not
start a turn. The message waits in the group until the session runs again. Held queues stay held.
The same `idempotency_key` returns the original entry and sends no second notification.

A message receipt does not give your program a Task handle or a child's final answer. Managed
child work follows the [subagent rules](council.md#choose-who-to-notify).

## Wait for requests from child sessions

`watch = council.changed()` returns a Task for a batch of open requests. It can
complete at once if a child already has an open request. A question, a request for
iterations, or a request for file access can complete this Task.

Each batch has a `cursor` and a list of `requests`. Each request has a `session_id`
and an `entry_id`. Read the entry and reply. Pass the cursor to
`council.changed(after=cursor)` to wait for later requests. Cancel the watch to stop
waiting without cancelling a child. The child Task and its views can continue
while you answer.

## Handle errors

<div data-variant="python">

A failed call raises `GatewayError` with the HTTP `status` and the error `code`.

</div>

<div data-variant="http">

The `error.type` field of the error body has the code.

</div>

| Status | Codes | Meaning |
|---|---|---|
| 400 | `invalid-request`, `invalid-thread`, `invalid-reply` | The request, the thread, or the reply target is not valid. |
| 404 | `group-not-found`, `entry-not-found`, `session-not-found` | The group, the message, or the session does not exist. |
| 409 | `disabled`, `inactive-session`, `invalid-recipient`, `idempotency-conflict` | Council is off, the run ended, or the request conflicts with an earlier one. |

## See also

- To learn about groups and the calls that sessions use in chat, read [Council](council.md).
- To install the SDK, connect a client, and handle gateway errors, read [Python SDK](python-sdk.md).
- For authentication, the OpenAPI document, and gateway errors, read [HTTP API](http-api.md).
