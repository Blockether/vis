# Queue, cancel & quit

Vis lets you keep typing while the agent is working. Messages you send during a
running turn are **queued** and fire in order once the turn finishes — so you can
line up follow-ups without waiting. This page explains how the queue behaves, how
it handles a failing provider, how to cancel a turn, and how to quit cleanly. The
same queue runs in the TUI and the web/mobile companion; both show the same state.

## Sending & queueing

Press **Enter** to send. What happens depends on whether a turn is already
running:

- **Idle** — the message starts a new turn immediately.
- **Busy** (a turn is in flight) — the message is **queued**, not dropped. The
  queued items appear as a small `Queued · N` list under the live progress bubble
  so you can see exactly what is waiting.

On a clean completion the queue drains **oldest-first**: the running turn finishes,
the head of the queue starts automatically, and so on until the queue is empty.
Only a **clean success advances** the queue.

## When the provider fails

Svar owns provider retry, failover, and failure classification. Once Svar returns a
failure to Vis, the gateway records and renders that terminal result **once**. Vis
does not classify it again, replay the failed request, or add another retry ladder.

If distinct messages were queued while the failed request was running, Vis pauses
that backlog instead of sending it into the same failure. The failed request stays
failed and is never put back at the head. An explicit resume starts only the next
queued request; there is no gateway backoff timer or automatic resume.

This boundary prevents one submitted message from appearing twice in the transcript
or causing repeated provider calls. It also means there is no `message_queue` retry
tuning in `vis.yml`: provider retry policy belongs to Svar, while Vis owns only the
queue of distinct user requests.

## Seeing & editing the queue

The queue block under the progress bubble shows every message waiting to send.

- **TUI** — press **↑** to pop the newest queued message back into the composer for
  editing. Send it again to re-queue it, or clear it to drop it.
- **Web / mobile** — tap a queued message to edit it in place (Enter saves, Escape
  cancels), or tap **×** to remove it.

## Resuming a session with a backlog

The queue is **memory-only by design** — it is never written to disk. If the
gateway restarts, any queued (and paused) backlog is gone; startup never
reconstructs or resubmits messages from the persisted transcript. This is
deliberate: a restart can't resurrect and fire work into a provider that may still
be unhealthy.

Within a running gateway, if you open or resume a session that has queued messages
but **nothing currently running** — and the queue is not paused — Vis auto-drains
the head immediately. This covers a backlog built on another device (the web
channel, a sibling TUI): attach from a fresh terminal and it just starts.

## Cancelling a turn

Press **Esc** (or **Ctrl+G**) to cancel the running turn. Cancel means **stop**,
not "skip ahead":

- The in-flight turn is stopped.
- **Every message queued before the cancel comes back into the input box** — in
  the TUI composer and in the web/mobile composer, appended after whatever you had
  already typed, as a draft that is never auto-sent. It does not matter who queued
  it, which channel pressed stop, or which session you are looking at when it
  happens: the words land in the editor of the session they belonged to. Nothing
  queued is ever thrown away by a cancel.
- Stop means stop — Vis will not start a turn you queued *before* you decided to
  abort. Deleting a queued row yourself still just deletes it; only a cancel
  restores.

If you actually want the backlog to run, resend it — or resume the session later,
where the auto-drain above takes over.

## Quitting (Ctrl+C)

**Ctrl+C** is context-sensitive so a single key does the sensible thing:

| Situation | Ctrl+C does |
|---|---|
| Empty composer, nothing running | **Quits** the TUI |
| Composer has a draft | **Clears the draft** (press again to quit) |
| A turn is running | **Cancels the turn** (your escape hatch — no orphaned worker) |
| A cancel is already in flight | **Quits immediately** |

That last row is the important one: after you hit **Esc** to cancel, the turn
stays marked "cancelling…" until the daemon confirms it. During that window a
second **Ctrl+C** no longer re-fires the cancel and makes you wait — it quits
right away, firing the cancel token on the way out so nothing is left running.
