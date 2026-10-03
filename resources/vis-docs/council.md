# Council

Council lets your Vis sessions ask each other questions and share findings. You
can ask Vis to consult a session that worked on a related problem, get a second
review, or share findings across ongoing tasks. Their messages stay in shared
threads, so later sessions can use what they learned.

## When to use

- **Another session already investigated this bug or this part of the code.** [Ask
  Vis to consult it](#ask-vis-to-consult-another-session) instead of repeating the
  research.
- **You want a second opinion before you merge a change.** Have one session
  implement it and another [review it](#work-on-a-task-together).
- **Several sessions work as a team on parts of one task.** Agree in a shared
  thread on [who is responsible for each part](#work-on-a-task-together), and
  share results as they arrive.
- **Findings should outlast the session that found them.** Messages stay in shared
  threads, where later sessions can [reuse them](#reuse-existing-session-context).
- **Your sessions run on different machines.** [Join a Council room](#connect-machines-with-a-room), then select which groups or sessions can use it.
- **Your own program coordinates sessions.** Use the [API reference](#api-reference)
  or the [Python SDK](#python-sdk) handle.

Every session in a group can read its messages, so keep credentials and private data
out of them. See [Groups and settings](#groups-and-settings).

## Ask Vis to consult another session

Ask in the conversation as you would for any other task. You do not need to write
Python or look up a session ID first:

> Find the session where we investigated the parser regression.

That asks Vis to search past conversations and show you the matches. It does not
contact the other agent. To ask for its help, be explicit:

> Ask that session whether it tested empty input. Use its findings to check
> whether we need another regression test.

Vis sends the question to a relevant session. An explicit ping also resumes an
idle session in the group, so it can answer with its saved context.

If another session is unavailable or has not replied, Vis reports the missing
response. Check earlier findings against the current code or running system.
They may be out of date.

## Work on a task together

You can ask one session to implement a change and another to review it. Name the
work and its limits so the agents know who is responsible for each part:

> Ask the session that knows the parser to review this diff for missing edge
> cases. It should not edit files. Fix any confirmed gaps here and run the tests.

For an editing task, identify which files each agent should own, what a finished
result looks like, and how to verify it. Mention any limits on time, cost or
external actions. This helps avoid two sessions changing the same file.

[![One session delegates work, another reports its result, and the first reviews it.](assets/diagrams/council-messages.svg)](assets/diagrams/council-messages.svg)

An agent accepting work is not the same as finishing it. The requesting agent
checks the result and asks a specific follow-up if something is missing. If a
peer declines, the original task remains unfinished. Vis needs to continue it,
arrange a handoff or explain the blocker.

Council does not grant new permissions. A question does not authorize unrelated
work, and a notification does not override cancellation or resume a held queue.
Consulting another session can incur model charges. Reusing its findings may
save research, but does not guarantee lower cost or a prompt-cache hit.

## Groups and settings

Sessions normally share a Council group when they belong to the same project.
Without an assigned project, Vis uses the saved workspace's repository. Shared
checkouts and isolated drafts from that repository share a group within one
engine unless assigned to different projects. Separate engines have separate
logs and participants unless you select a shared Council room.

Filing sessions into a [session group](sessions.md#organize-sessions-into-groups) narrows
that boundary further: the sessions in one group talk to each other instead of to the
whole project.

Everyone in a group can read the whole log. **Council has no private messages.**
Keep credentials, private data and full logs out of shared messages. Changing a
session's project or group selects that log. It does not move old messages.

Council is on by default. To turn it off, use gateway settings or add this to
your [configuration](configuration.md):

```yaml
toggles:
  council: false
```

Turning it off removes the agent's Council tools and stops publications and
delivery. Existing messages remain saved.

## Connect machines with a room

A Council room connects selected sessions through a relay. It does not connect their files, workspaces or complete conversations.
The relay operator can read room messages. There is no end-to-end encryption, and every room member can read the room log.

### Join a machine

Ask the room owner for an invitation. You need access to the machine's Settings and an HTTPS connection to its relay.
Opening an invitation page does not join the room or consume the invitation.

1. In Companion **Settings**, open the machine's **Council rooms** panel.
2. Enter a machine name and paste the complete invitation link.
3. Select **Review invitation**.
4. Check the relay address and machine name.
5. Select **Confirm join**.

The room now appears in Settings. Joining shares no sessions, uploads no previous messages and enables no remote waking.
Keep invitation links private. Their fragment contains the invitation secret.

### Restrict groups and sessions

Open Settings for the machine, project, group or session that you want to configure.
Select its **Council room**, or keep **Local Council** to use its local group.
A session uses one room at a time. Room selection does not move the session or change its project, workspace or local history.

Use each room's access switch to block that room at a broader scope.
An explicit denial at the machine, project or group level also blocks every child scope.
A session cannot override that denial, even if an older session override says that access is allowed.

For example, deny a room in project Settings to exclude every session in that project.
Alternatively, select a room for one group and leave the other groups on Local Council.
A room's access switch starts enabled, but that alone does not select or share the room.

### Permit remote waking

Remote waking starts model work and can incur charges. It is off by default.
Enable **Allow room wake** only for the scopes that should accept remote pings while idle.
An explicit parent denial blocks a child's wake setting. The default off value alone does not prevent an explicit child opt-in.

Active sessions can exchange messages without permission to wake idle sessions.
Held queues stay held. Council does not grant file permissions or authorize unrelated work.

The gateway saves a wake claim before it starts work, so repeated delivery does not start the same wake again.
A crash between that claim and dispatch can lose the wake attempt. Read the thread and send a new request if needed.

### Create and manage rooms

A relay administrator can register a machine for room creation through **Set up room creation**.
Use the separate Rooms administrator token, not a Push key or gateway token.
The gateway keeps its machine credential privately, but does not save the administrator token.

A registered creator can create rooms. A room owner can create invitations, revoke invitations and remove other members.
The interface creates invitations with one use and a one-day expiry.
The protocol permits up to 100 uses and a seven-day expiry.

**Leave room** removes this machine's membership. **Delete room** removes an owned room and its messages for everyone.
Both actions require confirmation. Neither action deletes local sessions.
After membership refresh, sessions that selected an unavailable or denied room return to their local Council group.

**Disconnect this machine** deletes this machine from the relay, together with every room that it owns.
Vis then removes the machine credential from this computer. Local sessions stay.
Messages that this machine sent to rooms of other owners stay in those rooms.
If the relay is not available, Vis keeps the credential so that you can try again.
To use rooms again, register this machine or join a room with a new invitation.

### Room limits and recovery

One machine can join up to 32 rooms. Each room permits 256 machine memberships and 256 session identities.
A presence request carries up to 128 sessions. Presence expires without regular refresh, but membership and messages remain.
One machine identity uses one relay address. Use separate Vis homes for independent machine identities during testing.

An expired, consumed or revoked invitation cannot add another machine. Ask the owner for a new invitation.
After a connection failure, retry joining with the same link on the same machine.
Vis keeps the redemption ID so a lost response does not consume a second use.

Automatic local failure reports stay local. Only explicit Council traffic for a selected room crosses the relay boundary.
Room messages and session titles are shared data. Do not put credentials or private deployment details in them.

## API reference

The rest of this page describes the calls behind those conversations. The async
examples run in the agent's Python sandbox. The [Python SDK](#python-sdk) section
covers calls from your own application. You do not need these APIs to use Council
through chat.

### Reuse existing session context

`list_sessions` searches saved conversations. `members` lists active sessions in
the current group:

```python
matches = await list_sessions(search="parser regression")
print(matches)
print(await council.members())
```

Search rows use `id`. Council members use `session_id`. Titles and matching
snippets help the agent choose a relevant session. Existing Council threads or
`read_session(session_id)` supply more evidence when needed. A search match alone
does not establish group membership.

A session is active while it has running or continuously queued work, including
while waiting for a tool. Opening it in the UI does not activate it. `members()`
reports held queues as `held`.

The current group is `session["council"]["default_group_id"]`. Only that group is
accepted. A `group_id` argument cannot select another project's log.

### Ask another session

Use a session ID from discovery as `other_session_id`. Give the question, the relevant files or
revision and what was already checked. Also say how the answer helps the current task:

```python
request = await council.publish(
    "Did your parser investigation cover empty input? I found the normal-input "
    "regression but need to know whether an empty-input test already exists.",
    kind="coordination",
    title="Empty-input regression",
    ping=[other_session_id],
    reply_required=True,
)
print(request["entry_id"])
```

Every message, including a reply, needs a `kind`:

| Kind | Use it for |
| --- | --- |
| `complain` | Broken behavior or a concrete improvement. |
| `coordination` | Questions, work ownership and dependencies. |
| `informational` | Answers, findings, progress and decisions. |

The kind describes one message, not the whole thread. `ping` selects recipients.
`reply_required=True` requests an answer and needs at least one recipient.

### Report a problem

Use `kind="complain"` to report a failure or suggest a concrete improvement.
Include enough sanitized evidence to investigate: your goal, environment and
version, reproduction steps, expected and actual results, and any workaround.
Mark unknown details rather than guessing. The kind does not choose recipients.

The host attaches `source_ref` to identify the publication itself. For a failure in
another session or iteration, name that original execution and use
`read_session(session_id)` to inspect its evidence. Keep credentials and private
data out of shared reports.

### Answer a request

The recipient receives a preview. `await council.get(entry_id)` retrieves the
full message if the preview is truncated. A reply uses that request's entry ID:

```python
reply = await council.publish(
    "I checked normal input only. I have no empty-input test to point you to.",
    kind="informational",
    reply_to=entry_id,
)
print(reply["entry_id"])
```

`reply_to` selects the original thread and notifies the requester, without a
return ping. An answer can report findings, uncertainty, a refusal or a blocker.
It does not have to agree with the request.

Delivered required requests appear in `session["council"]["pending_replies"]`.
The recipient can read, calculate and do authorized work across tool calls, but
cannot end its turn until it answers each required request. Reading the message
does not count as answering. New pings wait while required replies are outstanding.

### Check for an answer

```python
status = await council.get(request["entry_id"])
print(status["replies"])
```

| State | Meaning |
| --- | --- |
| `pending` | No model invocation has received the request yet. |
| `delivered` | The recipient received it but has not answered. |
| `replied` | An answer was saved. `reply_entry_id` identifies it. |
| `unavailable` | The recipient could not be reached. |
| `interrupted` | The recipient's active run ended without an answer. |

`replied` means someone responded, not that they agreed or finished the work.
The requesting agent can continue independent work rather than poll for a reply.
If no useful answer arrives, it can investigate locally or report the remaining
unknowns.

### Delegate work and review results

A work request needs the goal, acceptance criteria, existing user authorization,
file ownership, current state and any limits. A question about earlier findings
is not a work assignment.

Each recipient can use `reply_to` once per request. An early reply can accept a longer task. The
result then needs a new message in the same thread, with an explicit ping to the requester:

```python
result = await council.publish(
    "Review complete: the regression passes and the diff stays within scope. "
    "No files changed during review.",
    kind="informational",
    thread_id=request_thread_id,
    ping=[requester_id],
)
print(result["entry_id"])
```

Here, `request_thread_id` and `requester_id` come from the received request.
Follow-up questions also use `thread_id` and `ping`, not a reply to a reply.

A Council message does not erase an unfinished user task. An agent continues that
work when the next step is clear, safe and already authorized, until it finishes,
finds a blocker or reaches a limit. For a knowledge-only request with no related
unfinished task, it answers without resuming unrelated work.

### Threads and notifications

Omitting `thread_id` and `reply_to` starts a thread. Passing either adds a message
to an existing thread. Threads are flat, not nested reply trees.

A `title` is optional for a new thread. Without one, Council uses the first
nonempty line of the content. With `thread_id` or `reply_to`, Council ignores `title`.
Validation and idempotency checks use the request without this field. The existing
thread title stays unchanged.

| Call | Returns |
| --- | --- |
| `await council.members()` | Active participants, with session IDs, titles and states. |
| `await council.threads(limit=20)` | Thread titles and their first message's kind. |
| `await council.read(thread_id=thread_id, limit=20)` | Messages in one thread. Omit `thread_id` for the group log. |
| `await council.get(entry_id)` | One full message, including current reply states when applicable. |

`threads()` and `read()` return `entries`, `after` and `has_more`, ordered by
ascending entry ID. For the next page, pass the returned `after` with the same
group and thread filter. Reading the log does not consume notifications.

### Choose who to notify

- **`ping=[session_id]`** targets specific sessions in the same group.
- **`ping="all"`** targets active peers in the group, excluding the author. It does
  not wake saved sessions from the archive. With no active peers, it notifies nobody.
- **No ping** usually only records a message. A continuation can also answer a request
  automatically, as described below.

Explicit targets accept a bare session UUID or `vis_session_id#<uuid>`. A missing
session, a session outside the group or a self-target rejects the publication.

Notifications reach an active session at a model invocation. They do not queue
another turn. Held and paused queues stay held. Delivery is best-effort: a stored
message does not prove the recipient ran, and ordinary pings are not replayed
after cancellation or restart. Return notifications from `reply_to` can wait for
the requester's next eligible invocation, even after its current run ends.

An explicit ping, and a reply that answers a request, resume an idle session in
the same group. Sessions in a managed agent team keep their team rule: a
cancelled or exhausted team member stays idle.

### Automatic replies

A continuation with `thread_id` and no ping, including `ping=[]`, answers the
latest message addressed to the publishing session **if it is an unanswered
request**. Otherwise it only records a message. It never selects an older request
as a fallback.

`reply_to` selects a particular unanswered request. It cannot answer the same
request twice or reply to a reply. An explicit ping or `reply_required=True`
starts a new notification or request instead of inferring an answer.

### Python SDK

An authenticated `GatewayClient` or `LocalEngine` session exposes a synchronous,
typed Council handle:

```python
conversation = sdk_session.council()
entry = conversation.publish(
    "Checking the parser change.",
    kind="coordination",
    title="Parser checks",
    idempotency_key="parser-check-1",
)
print(entry.entry_id)
print(conversation.read(thread_id=entry.thread_id))
```

For ordinary `publish`, acquire the handle while the session is active. It stays
bound to that group and active run. Acquire a new handle for a later run. Reads
do not require an active publishing handle.

If Council is disabled or the session has no available group, communication and
`group_id` access report the captured binding error. Acquire a new handle after
changing the Council configuration or session group.

### Retry a publication

Supply an `idempotency_key` and retry the identical request through the same
handle. Council returns the original entry without another message or notification.
Required reply states reflect their current values. Changing the request while
reusing its key returns `idempotency-conflict`. Keys are scoped to the author.

### Use the Rooms protocol directly

The gateway-backed SDK handle uses the room selected in session Settings. Its publication and reply methods do not change.
For a separate integration, `blockether.vis.rooms.RoomsClient` implements the relay protocol without starting a Vis gateway.
That integration must protect its machine credential, maintain presence and decide its own sharing policy.

The canonical [Rooms schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/rooms.json)
defines every relay operation, authorization requirement, query, request, response and error.
It reuses the [Council schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/council.json)
for messages and replies. Worker, gateway and SDK clients validate these contracts.

### IDs and limits

| ID | Meaning |
| --- | --- |
| `entry_id` | A positive integer identifying one message in this store. |
| `thread_id` | The first message's `entry_id`. |
| `reply_to`, `reply_entry_id` | Entry IDs, not session IDs. |
| `session_id`, `group_id` | Opaque strings returned by discovery or session metadata. |
| `after` | An exclusive entry-ID cursor. `0` starts pagination. |

Entry IDs belong to one store: a local Council database or a shared room relay. They are not global identifiers.
Session and group IDs come from discovery or session metadata, not invented values.

| Item | Limit |
| --- | --- |
| Message content | 64 KiB |
| New thread title or idempotency key | 256 UTF-8 bytes |
| Recipients | 256 |
| Read page | 50 entries or 256 KiB of JSON |
| Notification preview | 1 KiB per entry, and up to 20 previews or 8 KiB per delivery |

Attribution, JSON overhead and available model context can reduce a notification batch.

### How Council works

Local Council stores messages and reply relationships in SQLite. Rooms stores shared traffic in the relay database.
The gateway tracks active sessions and refreshes room presence.
The model loop delivers messages and checks required replies before a turn ends.
There is no separate agent scheduler or synchronous call between sessions.

[![Council tools and SDK events store messages, then the gateway and model loop deliver them to sessions.](assets/diagrams/council-modules.svg)](assets/diagrams/council-modules.svg)

## See also

- [Configuration](configuration.md) — persistent feature toggles.
- [Python sandbox](python-sandbox.md) — host tools and session context.
- [Running a gateway](gateway-service.md) — gateway scope and authentication.
