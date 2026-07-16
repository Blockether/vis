# Queue, cancel & quit

Vis lets you keep typing while the agent is working. Messages you send during a
running turn are **queued** and fire in order once the turn finishes — so you can
line up follow-ups without waiting. This page explains how the queue behaves in
the TUI, how to cancel a turn, and how to quit cleanly.

## Sending & queueing

Press **Enter** to send. What happens depends on whether a turn is already
running:

- **Idle** — the message starts a new turn immediately.
- **Busy** (a turn is in flight) — the message is **queued**, not dropped. The
  composer confirms with *"Queued — will send after current turn"*, and the
  queued items appear as a small list under the live progress bubble so you can
  see exactly what is waiting.

Queued turns drain **oldest-first**: when the running turn completes, the head of
the queue starts automatically, and so on until the queue is empty.

## Seeing & editing the queue

The queue block under the progress bubble shows every message waiting to send.
To revise one, press **↑** — the newest queued message pops back into the
composer for editing. Send it again to re-queue it (it goes back in line), or
clear it to drop it.

## Resuming a session with a backlog

If you open or resume a session that has queued messages but **nothing currently
running**, Vis **auto-drains the head of the queue immediately** — the oldest
queued turn starts as soon as you attach, and you see it running live. You no
longer have to send an extra message to "wake up" a session that already had work
lined up.

This covers the common case where a queue was built up on another device (the web
channel, a sibling TUI) and you attach from a fresh terminal: the backlog just
starts.

## Cancelling a turn

Press **Esc** (or **Ctrl+G**) to cancel the running turn. Cancel means **stop**,
not "skip ahead":

- The in-flight turn is stopped.
- Any messages **you had queued before the cancel** are **restored to the
  composer** (the newest one lands in the editor, ready to edit or resend) rather
  than silently auto-running. Stop means stop — Vis will not start a turn you
  queued *before* you decided to abort.

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
