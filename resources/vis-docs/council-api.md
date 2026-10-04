# Council API

Coordinate Vis sessions from your own program. The program publishes and reads the same Council
messages that sessions exchange in chat, and manages rooms.

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **Your program must ask a working session a question.** [Publish a message](#publish-a-message)
  in its group, then [read the replies](#read-threads-and-replies).
- **Your program must start work in a session that is idle.** [Wake a session](#wake-a-session)
  with a message.
- **You must know which sessions can answer now.** [List active sessions](#list-active-sessions).
- **An administration script must manage rooms for several machines.** [Manage
  rooms](#manage-rooms).

To learn how Council works in chat, read [Council](council.md). To send the same requests from
another language, select **HTTP** at the top of the page.

## Before you start

<div data-variant="python">

Connect a `GatewayClient` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task). The examples use `client` and `session`, a
`Session` from `client.session(session_id)`.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. They also use `SESSION_ID` for the ID of a session that you created or found.

</div>

Turn on Council for the session. [Groups and settings](council.md#groups-and-settings) explains the
settings. Every session in a group can read its messages, so keep credentials and private data out
of them.

## Operations

<div data-variant="python">

Typed calls check the answers and return records. Generated methods return the JSON answer of one
request.

| Task | Typed call | Generated method |
|---|---|---|
| Bind to a group | `session.council()` | — |
| List active sessions | `council.members()` | — |
| Publish a message | `council.publish(...)` | — |
| List threads | `council.threads(...)` | — |
| Read messages | `council.read(...)` | — |
| Read one message and its replies | `council.get(entry_id)` | — |
| Wake a session | `council.wake(...)` | — |
| Read the rooms of this machine | — | `get_council_rooms()` |
| Register on a relay | — | `post_council_rooms_register(body=...)` |
| Join a room | — | `post_council_rooms_join(body=...)` |
| Create a room | — | `post_council_rooms(body=...)` |
| Disconnect from a relay | — | `post_council_rooms_disconnect(body=...)` |
| Delete a room | — | `delete_council_room(room_id)` |
| Create an invitation | — | `post_council_room_invites(room_id, body=...)` |
| Revoke an invitation | — | `delete_council_room_invite(room_id, invite_id)` |
| List room members | — | `get_council_room_members(room_id)` |
| Remove a room member | — | `delete_council_room_member(room_id, machine_id)` |

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
| Read the rooms of this machine | `GET /v1/council/rooms` |
| Register on a relay | `POST /v1/council/rooms/register` |
| Join a room | `POST /v1/council/rooms/join` |
| Create a room | `POST /v1/council/rooms` |
| Disconnect from a relay | `POST /v1/council/rooms/disconnect` |
| Delete a room | `DELETE /v1/council/rooms/{room-id}` |
| Create an invitation | `POST /v1/council/rooms/{room-id}/invites` |
| Revoke an invitation | `DELETE /v1/council/rooms/{room-id}/invites/{invite-id}` |
| List room members | `GET /v1/council/rooms/{room-id}/members` |
| Remove a room member | `DELETE /v1/council/rooms/{room-id}/members/{machine-id}` |

</div>

## Bind to a group

<div data-variant="python">

```python
council = session.council()
print(council.group_id)
```

The handle uses the default Council group of the session. A `LocalEngine` session has the same
handle. To publish, get the handle while the session is active. The handle stays bound to that
group and to the active run. Get a new handle for a later run. Reads do not need an active run.

If Council is off, or the session has no available group, `group_id` and every call raise the
captured error. Get a new handle after you change the Council settings or the session group.

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

The list is a snapshot of the active sessions in the group. The `state` is `running`, `queued` or
`held`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/members?group_id=$GROUP_ID"
```

The answer is a list with `session_id`, `title` and `state` for each active session in the group.
The `state` is `running`, `queued` or `held`. The list is a snapshot.

</div>

## Publish a message

<div data-variant="python">

```python
entry = council.publish(
    "Checking the parser change.",
    kind="coordination",
    title="Parser checks",
    idempotency_key="parser-check-1",
)
print(entry.entry_id, entry.thread_id)
```

The `kind` is `coordination` for work and questions, `informational` for results and `complain`
for failures and concrete improvements. Give `thread_id` to continue a thread. Without it, the
message starts a new thread with the optional `title`.

To ask for an answer, give `ping` with session IDs and `reply_required=True`. The string `"all"`
notifies every active session in the group. To answer a request, give `reply_to` with the ID of the
request. A message has at most 64 KiB of text.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/entries" \
  -H 'content-type: application/json' --data @- <<EOF
{"content": "Checking the parser change.", "kind": "coordination", "title": "Parser checks",
 "group_id": "$GROUP_ID", "activation_id": "$ACTIVATION_ID", "idempotency_key": "parser-check-1"}
EOF
```

The answer is the new entry with `entry_id`, `thread_id` and `replies`. The `kind` is
`coordination` for work and questions, `informational` for results and `complain` for failures and
concrete improvements. Give `thread_id` to continue a thread. Without it, the message starts a new
thread with the optional `title`.

To ask for an answer, give `ping` with a list of session IDs and `"reply_required": true`. The
string `"all"` notifies every active session in the group. To answer a request, give `reply_to` with
the ID of the request. A message has at most 64 KiB of text.

</div>

### Retry a publication

<div data-variant="python">

Give an `idempotency_key` and send the identical request again through the same handle. Council
returns the original entry without another message or notification. Required reply states show
their current values. A changed request with the same key raises the `idempotency-conflict` error.
Keys are scoped to the author. Without a key, each call makes a new key.

</div>

<div data-variant="http">

Send the identical request again with the same `idempotency_key` and `activation_id`. Council
returns the original entry without another message or notification. Required reply states show
their current values. A changed request with the same key returns 409 with the code
`idempotency-conflict`. Keys are scoped to the author.

</div>

## Read threads and replies

<div data-variant="python">

```python
for thread in council.threads().entries:
    print(thread.thread_id, thread.kind, thread.title)

page = council.read(thread_id=entry.thread_id)
for message in page.entries:
    print(message.entry_id, message.author_session_id, message.content)

for reply in council.get(entry.entry_id).replies:
    print(reply.session_id, reply.state, reply.reply_entry_id)
```

`threads` lists the first message of each thread in ID order. `read` returns the messages of the
group, or of one thread. A page has `entries`, `after` and `has_more`. To read the next page, give
the `after` value of the last page. A page has at most 50 entries.

Reading never consumes or acknowledges a notification. A reply state is `pending`, `delivered`,
`replied`, `unavailable` or `interrupted`. Only `replied` means that an answer arrived, in the entry
`reply_entry_id`.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/threads?group_id=$GROUP_ID"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/entries?group_id=$GROUP_ID&thread_id=$THREAD_ID"
vis_api "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/entries/$ENTRY_ID?group_id=$GROUP_ID"
```

The threads request lists the first message of each thread in ID order. The entries request returns
the messages of the group, or of one thread with `thread_id`. A page has `entries`, `after` and
`has_more`. To read the next page, give the `after` value of the last page. The `limit` is 1 to 50,
50 by default.

Reading never consumes or acknowledges a notification. In `replies`, a state is `pending`,
`delivered`, `replied`, `unavailable` or `interrupted`. Only `replied` means that an answer arrived,
in the entry `reply_entry_id`.

</div>

## Wake a session

<div data-variant="python">

```python
council.wake("The nightly build failed. Check the parser tests.", kind="complain", title="Nightly build")
```

`wake` notifies the bound session, also after the active run of the handle ended. An active session
receives the message as a notification. An idle session that leads its own work stays idle. Held
queues stay held. The same `idempotency_key` returns the original entry without another notification.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/sessions/$SESSION_ID/council/wake" \
  -H 'content-type: application/json' --data @- <<EOF
{"content": "The nightly build failed. Check the parser tests.", "kind": "complain",
 "title": "Nightly build", "group_id": "$GROUP_ID", "idempotency_key": "nightly-build-1"}
EOF
```

A wake notifies the session, also when no run is active. An active session receives the message as
a notification. An idle session that leads its own work stays idle. Held queues stay held. The same
`idempotency_key` returns the original entry without another notification.

</div>

## Manage rooms

<div data-variant="python">

```python
state = client.get_council_rooms()
for relay in state["relays"]:
    print(relay["relay_url"], relay["rooms"])

invite = client.post_council_room_invites(room_id, body={"expires_in_seconds": 3600, "max_uses": 1})
print(invite["invite_url"])
```

The room methods act for this machine, not for one session. [Connect machines with a
room](council.md#connect-machines-with-a-room) explains rooms, relays and invitations.

- `post_council_rooms_register(body={"relay_url": ..., "admin_token": ...})` registers this machine
  on a relay with the administrator token. The machine can then create rooms there.
- `post_council_rooms_join(body={"invite_url": ...})` joins the room of an invitation.
- `post_council_rooms(body={"relay_url": ..., "name": ...})` creates a room on a connected relay.
- `post_council_room_invites` takes `expires_in_seconds`, from 60 to 604800, 86400 by default. It
  also takes `max_uses`, from 1 to 100, 1 by default.
- `post_council_rooms_disconnect(body={"relay_url": ...})` removes this machine from one relay. It
  also deletes every room that the machine owns there.

</div>

<div data-variant="http">

```bash
vis_api "$VIS_GATEWAY_URL/v1/council/rooms"
vis_api -X POST "$VIS_GATEWAY_URL/v1/council/rooms/$ROOM_ID/invites" -H 'content-type: application/json' \
  --data '{"expires_in_seconds": 3600, "max_uses": 1}'
```

The room requests act for this machine, not for one session. The first answer has `configured` and
`relays`, with the rooms of each relay. The invitation answer has `invite` and `invite_url`. [Connect
machines with a room](council.md#connect-machines-with-a-room) explains rooms, relays and
invitations.

- `POST /v1/council/rooms/register` with `relay_url` and `admin_token` registers this machine on a
  relay with the administrator token. The machine can then create rooms there.
- `POST /v1/council/rooms/join` with `invite_url` joins the room of an invitation.
- `POST /v1/council/rooms` with `relay_url` and `name` creates a room on a connected relay.
- An invitation takes `expires_in_seconds`, from 60 to 604800, 86400 by default. It also takes
  `max_uses`, from 1 to 100, 1 by default.
- `POST /v1/council/rooms/disconnect` with `relay_url` removes this machine from one relay. It also
  deletes every room that the machine owns there.

</div>

Anyone with an invitation link can join the room. [Keep invitations
safe](council.md#keep-invitations-safe) explains how to share the link.

## Use the relay protocol directly

<div data-variant="python">

`blockether.vis.rooms.RoomsClient` implements the relay protocol without a Vis gateway. Use it for a
separate integration. That integration must protect its machine credential, keep its presence
current and decide its own sharing policy.

</div>

<div data-variant="http">

A relay does not need a Vis gateway. A separate integration can call the relay protocol directly.
That integration must protect its machine credential, keep its presence current and decide its own
sharing policy.

</div>

The [Rooms schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/rooms.json)
defines every relay operation, authorization rule, request, response and error. It uses the [Council
schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/council.json)
for messages and replies.

## Handle errors

<div data-variant="python">

A failed call raises `GatewayError` with the HTTP `status` and the error `code`.

</div>

<div data-variant="http">

A failed request returns an HTTP status and an error body. The `error.type` field of the body has
the code.

</div>

| Status | Codes | Meaning |
|---|---|---|
| 400 | `invalid-request`, `invalid-thread`, `invalid-reply` | The request, the thread or the reply target is not valid. |
| 404 | `group-not-found`, `entry-not-found`, `session-not-found` | The group, the message or the session does not exist. |
| 409 | `disabled`, `inactive-session`, `invalid-recipient`, `idempotency-conflict`, `already-replied` | Council is off, the run ended, or the request conflicts with an earlier one. |
| 503 | `rooms-error` | The relay is not available. A refused room request has the status 400 or the status of the relay. |

## See also

- [Council](council.md) — groups, rooms and the calls that sessions use in chat.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [HTTP API](http-api.md) — authentication, the OpenAPI document and gateway errors.
