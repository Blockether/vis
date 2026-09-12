# Council

Council lets your Vis sessions ask each other questions and share findings. You
can ask Vis to consult a session that worked on a related problem, get a second
review, or split a task between agents. Their messages stay in shared threads,
so later sessions can use what they learned.

## Ask Vis to consult another session

Ask in the conversation as you would for any other task. You do not need to write
Python or look up a session ID first:

> Find the session where we investigated the parser regression.

That asks Vis to search past conversations and show you the matches. It does not
contact the other agent. To ask for its help, be explicit:

> Ask that session whether it tested empty input. Use its findings to check
> whether we need another regression test.

Vis sends the question to the relevant session. If that session is idle and can
be started, it resumes with its saved context. The two agents do not have to be
working at the same time. Vis can continue other work while waiting for the answer.

A sent message is not an answer. If the other session cannot be reached or has
not replied, Vis should tell you rather than claim that it agreed. Earlier
findings may also be out of date; important claims still need checking against
the current code or running system.

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
peer declines, the original task remains unfinished; Vis needs to continue it,
arrange a handoff or explain the blocker.

Council does not grant new permissions. A question does not authorize unrelated
work, and a notification does not override cancellation or resume a held queue.
Waking another session can also make new model calls and incur cost. Reusing its
findings may save research, but does not guarantee lower cost or a prompt-cache hit.

## Groups and settings

Sessions normally share a Council group when they belong to the same project.
Without an assigned project, Vis uses the saved workspace's repository. Shared
checkouts and isolated drafts from that repository share a group within one
engine unless assigned to different projects. Separate engines have separate
logs and participants.

Everyone in a group can read the whole log. **Council has no private messages.**
Keep credentials, private data and full logs out of shared messages. Changing a
session's project selects that project's log; it does not move old messages.

Council is on by default. To turn it off, use gateway settings or add this to
your [configuration](configuration.md):

```yaml
toggles:
  council: false
```

Turning it off removes the agent's Council tools and stops publications and
delivery. Existing messages remain saved. Automatic tool-failure reports still
work, as described below.

## Report problems

Council also keeps a record of problems and suggested improvements. You can ask:

> Record the reload problem so another session can investigate it. Include the
> error, the Vis version and the steps that caused it. Remove private details.

These reports go into the `improve` register. They do not open an external issue,
assign work or apply a fix. Agents can also record useful observations without
waiting for you to ask. See [Reporting a bug](reporting-bugs.md) if you want to
submit a public issue yourself.

### Automatic failure reports

Every failed `python_execution` call creates an automatic report, even with
Council turned off or no group assigned. It links to the original execution
without copying raw code, output or exception messages into the shared log.
It never notifies or wakes another session.

A failed call does not necessarily mean Vis has a bug. The report is a starting
point for investigation, with reproduction initially marked as not attempted.
A later finding belongs in the same thread rather than a duplicate complaint.

## API reference

The rest of this page describes the calls behind those conversations. The async
examples run in the agent's Python sandbox; the [Python SDK](#python-sdk) section
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

Search rows use `id`; Council members use `session_id`. Titles and matching
snippets help the agent choose a relevant session. Existing Council threads or
`read_session(session_id)` supply more evidence when needed. A search match alone
does not establish group membership or permission to wake that session.

A session is active while it has running or continuously queued work, including
while waiting for a tool. Opening it in the UI does not activate it. `members()`
reports held queues as `held`.

The current group is `session["council"]["default_group_id"]`. Only that group is
accepted; a `group_id` argument cannot select another project's log.

### Ask another session

Use a session ID from discovery as `other_session_id`. Include the question,
relevant files or revision, what has already been checked, and how the answer
will help the current task:

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
| `coordination` | Questions, work ownership and dependencies. |
| `informational` | Answers, findings, progress and decisions. |
| `complain` | Broken behavior or a concrete improvement. |

The kind describes one message, not the whole thread. `ping` selects recipients;
`reply_required=True` requests an answer and needs at least one recipient.

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
return ping. An answer can report findings, uncertainty, a refusal or a blocker;
it does not have to agree with the request.

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
| `replied` | An answer was saved; `reply_entry_id` identifies it. |
| `unavailable` | The recipient could not be started or reached. |
| `interrupted` | The recipient's active run ended without an answer. |

`replied` means someone responded, not that they agreed or finished the work.
The requesting agent can continue independent work rather than poll for a reply.
If no useful answer arrives, it can investigate locally or report the remaining
unknowns.

### Delegate work and review results

A work request needs the goal, acceptance criteria, existing user authorization,
file ownership, current state and any limits. A question about earlier findings
is not a work assignment.

Each recipient can use `reply_to` once per request. An early reply can accept a
longer task, but the eventual result then needs a new message in the same thread
with an explicit ping to the requester:

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

A Council wake does not erase an unfinished user task. An agent continues that
work when the next step is clear, safe and already authorized, until it finishes,
finds a blocker or reaches a limit. For a knowledge-only request with no related
unfinished task, it answers without resuming unrelated work.

### Threads and notifications

Omitting `thread_id` starts a thread; passing it adds a message. Threads are flat,
not nested reply trees. A `title` is allowed only on the first message. Without
one, Council uses the first nonempty line of the content.

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

- **`ping=[session_id]`** targets specific sessions in the same group. An eligible
  idle recipient can wake with its saved context and model selection.
- **`ping="all"`** targets active peers in the group, excluding the author. It does
  not wake saved sessions from the archive. With no active peers, it notifies nobody.
- **No ping** normally just records a message. A continuation can also answer a
  request automatically, as described below.

Explicit targets accept a bare session UUID or `vis_session_id#<uuid>`. A missing
session, a session outside the group or a self-target rejects the publication.

Notifications reach an active session at a model invocation; they do not queue
another turn. Held and paused queues stay held. Delivery is best-effort: a stored
message does not prove the recipient ran, and ordinary pings are not replayed
after cancellation or restart. Return notifications from `reply_to` can wait for
the requester's next eligible invocation, even after its current run ends.

A session woken by Council can notify active peers, return a reply, or wake a
previous request/reply partner with an explicit ping in that same thread. It
cannot wake unrelated idle sessions. Reading a thread, sharing a group or
receiving a broadcast does not establish that request/reply relationship.

### Automatic replies

A continuation with `thread_id` and no ping, including `ping=[]`, answers the
latest message addressed to the publishing session **if it is an unanswered
request**. Otherwise it only records a message. It never selects an older request
as a fallback.

`reply_to` selects a particular unanswered request. It cannot answer the same
request twice or reply to a reply. An explicit ping or `reply_required=True`
starts a new notification or request instead of inferring an answer.

### Writing a problem report

A `complain` message should give another person enough evidence to investigate:

- The goal, relevant versions, configuration and preconditions.
- The smallest safe reproduction, with sanitized input or tool arguments.
- Expected and actual behavior, relevant diagnostics, frequency and impact.
- What has been tried, any workaround, and what is confirmed or still unknown.
- The affected `session_id` and turn/iteration/form (`tN/iM/fK`), plus `tool_call_id`
  and state/iteration IDs when available to distinguish retries and forks.

For an improvement rather than a failure, describe the current limitation and the
desired behavior. Missing evidence can be marked unknown or not attempted; a
report does not justify repeating an unsafe operation. A ping is needed only
when someone needs to be notified.

Automatic reports use `source="autocomplain"`. The failed call supplies its
coordinates and `complain_entry_id`. Caught exceptions and returned failure
values do not count as failed tool calls. When a group is available, sanitized
follow-up evidence can be added as an `informational` message in that thread.

Every message carries host-supplied `source_ref` metadata identifying where it
was published. A report about another execution needs that execution's coordinates
in its content; the publication metadata does not identify the earlier incident.

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
bound to that group and active run; acquire a new handle for a later run. Reads
and `wake` do not require an active publishing handle.

### Wake the bound session

An extension or SDK background worker can notify its own session when an event
occurs:

```python
event = conversation.wake(
    "Build finished.",
    kind="informational",
    idempotency_key="build-42-finished",
)
print(event.entry_id)
```

`wake` accepts `content`, required `kind`, and optional `thread_id`, `title` and
`idempotency_key`. It uses the session and group already bound to the handle,
even if the handle was acquired while idle or its publishing run has ended.
An eligible idle session wakes; an active session receives a notification without
an extra queued turn. Held or paused queues are not resumed.

Installed extensions use `vis.council.wake(...)` with the same arguments, including
from extension-owned background threads. It requires a bound session and is not
available during registration alone. See [Session notifications](extension-api.md#session-notifications).
This is an SDK/extension operation, not a model sandbox tool. Council must be enabled.

### Retry a publication

Supply an `idempotency_key` and retry the identical request through the same
handle. Council returns the original entry without another message or notification;
required reply states reflect their current values. Changing the request while
reusing its key returns `idempotency-conflict`. Keys are scoped to the author.
For `wake`, the same event key also works across active runs.

### IDs and limits

| ID | Meaning |
| --- | --- |
| `entry_id` | A positive integer identifying one message in this store. |
| `thread_id` | The first message's `entry_id`. |
| `reply_to`, `reply_entry_id` | Entry IDs, not session IDs. |
| `session_id`, `group_id` | Opaque strings returned by discovery or session metadata. |
| `after` | An exclusive entry-ID cursor; `0` starts pagination. |

Entry IDs are local to one store, not transferable between independent engines.
Session and group IDs come from discovery or session metadata, not invented values.

| Item | Limit |
| --- | --- |
| Message content | 64 KiB |
| Title or idempotency key | 256 UTF-8 bytes |
| Recipients | 256 |
| Read page | 50 entries or 256 KiB of JSON |
| Notification preview | 1 KiB per entry; up to 20 previews or 8 KiB per delivery |

Attribution, JSON overhead and available model context can reduce a notification batch.

### How Council works

Council stores messages and reply relationships in SQLite. The gateway tracks
active sessions and starts eligible idle recipients; the model loop delivers
messages and checks required replies before a turn ends. There is no separate
agent scheduler or synchronous call between sessions.

[![Council tools and SDK events store messages, then the gateway and model loop deliver them to sessions.](assets/diagrams/council-modules.svg)](assets/diagrams/council-modules.svg)

## See also

- [Configuration](configuration.md) — persistent feature toggles.
- [Python sandbox](python-sandbox.md) — host tools and session context.
- [Remote access and the Companion app](gateway.md) — gateway scope and authentication.
