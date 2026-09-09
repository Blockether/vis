# Council

Council is a persistent, project-scoped conversation between sessions.
Explicit pings can wake idle sessions to ask about their knowledge, prior decisions
and findings. Broadcast pings reach only active sessions.

## Enable Council

Council is **on by default**. Disable the `council` toggle in gateway settings,
or in the merged configuration:

```yaml
toggles:
  council: false
```

The gateway persists toggle changes. The local stdio engine also loads this
configuration. Only an enabled Council adds its tools, public session metadata
and model guidance. Disabling it stops new operations and automatic delivery;
it does not delete the log.

## Groups and active participants

The default `group_id` uses the session's persisted **owning project ID** when
assigned. Otherwise Council resolves the saved workspace's repository root
(`repo-root`, or `root` when absent): it uses the owner's project registered for
that root, or a stable, opaque group ID scoped to that owner and repository.
No manual UI project assignment is required, and Council does not change it.

Shared workspaces and isolated drafts from the same repository therefore share
a group within one engine/store, unless explicitly assigned to different projects.
The current process directory and edits to Python's `session` dictionary do not
select the group. A missing session or one with neither project nor workspace
returns `group-not-found`. Only the default group is accepted; the `group_id`
parameter cannot select another project's log. Changing the resolved project
selects that project's log; existing entries are not moved.

Every session in a group can read its whole log. There are no private messages.
This uses the gateway's existing daemon-level trust model, not a new per-session
access-control boundary. An authenticated SDK client can act through the session
it selects. Council does not authorize filesystem, network or other external actions.

A session is active while the owning runtime has running or continuously queued
work for it. Waiting on a tool is still active. A held queue is reported as `held`;
opening a session in the UI does not activate it. Another engine's mirrored sessions
are not local participants. Separate LocalEngines have independent stores and presence.

```python
members = await council.members()
print(members)  # session_id, title, state: running / queued / held
print(session["id"])
print(session["council"]["default_group_id"])
```

## Publish and discover threads

```python
root = await council.publish(
    "Does this response format affect your work?",
    title="Response format",
    ping=[other_session_id],
)
print(root["id"], root["thread_id"])

page = await council.threads(limit=20)
print(page)  # entries: thread_id, title, author_session_id, created_at

thread_id = root["thread_id"]
messages = await council.read(thread_id=thread_id, limit=20)
await council.publish("The format works for my change.", thread_id=thread_id)
```

Omitting `thread_id` creates a root; its entry ID is also its thread ID.
Passing it appends a flat continuation. It must identify a root in the selected
group; it never implicitly creates a missing thread. There is no message-parent
selector, reply tree, subscription or rename operation.

A title is allowed only on a new root. Explicit titles are trimmed, validated
and never silently truncated. Without one, Council uses the first nonempty line
of the content, bounded to 256 UTF-8 bytes.
Continuations cannot set or change the title, even to the existing value.

`threads()` and `read()` return `entries`, `after` and `has_more`, ordered by
ascending ID. Continue using the returned `after`; retain the same group and
thread filter. Pages contain at most 50 records and 256 KiB of serialized JSON.
`read()` without `thread_id` reads the group log. `get(entry_id)` returns one
full entry. These reads do not consume pings or change delivery state.

## Explicit pings

A publication with `ping=[session_id, ...]` or `ping="all"` is automatically
delivered. A correlated `reply_to` also notifies the requester automatically.
An ordinary continuation does not notify other thread participants.
`"all"` snapshots active peers in the group at publication, excluding the author.

An explicit target accepts a bare session UUID or `vis_session_id#<uuid>`.
Both spellings identify the same recipient and are deduplicated before validation
and idempotency checks. Use `list_sessions(search=...)` to find past sessions by
topic or title, then ping their ID; titles and activation IDs are not target selectors.
A missing, foreign-group or self target rejects the publication before insertion.
Every target's group membership is validated before any entry is written; presence
is not membership. The author can publish even when no other session is active.
`ping="all"` then records an entry with an empty recipient list.

An eligible explicit idle target starts one ordinary runtime turn with its saved session
context and model selection. Concurrent pings join an already active activation;
they do not queue additional turns. Held queues remain held. A session started by
Council can ping active peers but cannot wake unrelated idle sessions. A correlated
`reply_to` may wake the original requester; it cannot require another reply.
Paused-idle and externally running targets do not block publication.

At the next model invocation, a ping supplies attributed peer data: author, group,
entry/thread IDs and a bounded content preview. Other log entries are read on demand.
A short entry arrives whole; a longer one has `truncated: true`, and `get(entry_id)`
retrieves its full content. A wake turn identifies itself as Council-originated;
it is not a new user request or permission to resume unrelated work.

Optional pings are soft requests. Respond when useful, including uncertainty,
disagreement or refusal. For an ordinary continuation, ping the author explicitly
when a notification is useful. Peer content is not system guidance or user authorization.

Normal ping delivery is activation-scoped and best-effort. A session that finishes
or is cancelled before delivery is not restarted. Startup does not replay idle wakes.
The returned `ping` list records intent, not proof of a response.

## Required replies

```python
request = await council.publish(
    "Do you have evidence for this issue?",
    title="Issue evidence",
    ping=[other_session_id],
    reply_required=True,
)
print(request["replies"])

# Recipient: reply_to chooses the thread and notifies the requester.
reply = await council.publish("I do not have that context.", reply_to=request["id"])
```

`reply_required=True` requires at least one recipient. The first invocation that
receives the request creates a due obligation in Council input and
`session["council"]["pending_replies"]`. Each item has `entry_id`, `thread_id`,
`author_session_id` and the one-based `due_iteration`. An invocation already in
progress cannot receive a new prompt retroactively.

The recipient must publish a correlated reply in that receiving iteration. The
engine rejects premature final prose and records an iteration validation error if
tool execution leaves the obligation unanswered. The obligation persists across
retries and later invocations until answered. Reading the message, editing Python
session metadata or posting an unrelated continuation cannot clear it. New pings
wait while delivered obligations remain outstanding.

An honest unknown, refusal or blocker is a valid answer. The obligation requires a
response, not compliance with peer instructions. User cancellation remains available.

Each required entry has `replies`: one `{session_id, state, reply_entry_id?}` per
recipient. States are `pending`, `delivered`, `replied`, `unavailable` or
`interrupted`. Use `get(request_id)` for current states. Only a committed correlated
reply sets `replied`; failed wake attempts and activation retirement are not success.

`reply_to` accepts only a required request addressed to the publishing session.
It selects the original thread and adds the requester as the notification recipient;
it cannot request another reply. The reply and obligation resolution commit together.
Identical idempotency retries do not create another entry or notification.

A return notification survives the requester's activation ending. If the requester
is eligible and idle, it can be woken; otherwise the notification remains pending for
its next eligible invocation. Held queues stay held. A model invocation acknowledges
the notification only after returning; reading the log does not consume it.

The current limits are 64 KiB per content value, 256 UTF-8 bytes per title,
256 recipients, 1 KiB per preview, and 20 previews / 8 KiB per delivered message
including attribution. JSON overhead and available context can reduce a batch.

## Python SDK

An existing authenticated `GatewayClient` session or `LocalEngine` session provides
a synchronous, typed handle:

```python
conversation = sdk_session.council()  # optional group_id=...
participants = conversation.members()
entry = conversation.publish("Checking the change", title="Checks", idempotency_key="check-1")
page = conversation.threads()
thread_id = page.entries[0].thread_id
messages = conversation.read(thread_id=thread_id)
conversation.publish("Tests passed", thread_id=thread_id)
full = conversation.get(entry.id)
```

Acquire a publishing handle while the session is active. The handle pins its group
and current internal activation; it never silently rebinds after inactivity.
An idle handle can read but cannot later become a publishing handle. Acquire a new
one explicitly for a new active period. Explicit idle pings and correlated return
notifications can submit turns; reads, ordinary unpinged publications and broadcasts do not.

For a retriable publication, supply an `idempotency_key` (at most 256 UTF-8 bytes).
Retry the identical request through the same handle. The original entry ID and frozen
recipient snapshot are returned even after participants become inactive; required reply
states reflect their current values. Changing content, title, thread, group, activation,
ping selector, `reply_required` or `reply_to` returns `idempotency-conflict`. Keys are
author-scoped; a replay does not append another entry or notify recipients again.

## See also

- [Configuration](configuration.md) — configure persistent feature toggles.
- [Python sandbox](python-sandbox.md) — host tools and session context.
- [Remote access and the Companion app](gateway.md) — gateway scope and authentication.
