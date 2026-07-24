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

A queue that blindly fired the next message after every failure would empty itself
into a sick provider — one 429/503/stall after another, each burning a real turn.
Vis does not do that. On a terminal **failure** the queue **pauses** instead of
advancing, and a `Queue paused` / `Provider unhealthy` banner shows how many
messages are held.

- **Transient failure** (rate-limit, transport error, stream stall) — the failed
  message is **re-queued at the head** and retried after a short backoff
  (2s → 8s → 30s), honouring the provider's `Retry-After` when it sends one. The
  retry re-runs the **same** message; nothing is skipped.
- **Circuit breaker** — after **3** consecutive transient failures the breaker
  trips **open**: it stops the fast backoff and instead probes once a minute
  (a *half-open* probe). A lasting outage still self-heals when the provider
  recovers, but a persistently sick provider is probed once a minute, never
  hammered.
- **Terminal failure** (auth, bad request, tool-schema) — retrying can't help, so
  the dead message is dropped and the **rest of the backlog is held** for you to
  decide.

Press **Retry now** (web) or resume the session to clear the pause immediately; an
explicit resume also re-arms the breaker.

## Tuning the failure handling

The breaker and backoff numbers are gateway config, set in `vis.yml` under a
`message_queue:` block. Every value is a number; omit the block or any key to keep the
default. A `/reload` applies changes.

```yaml
message_queue:
  breaker_threshold: 3        # consecutive fails before the breaker holds the backlog
  retry_backoff_ms: [2000, 8000, 30000]  # head-retry backoff (ms) after a transient fail
  halfopen_probe_ms: 60000    # half-open probe cadence (ms) once the breaker is OPEN
  retry_after_cap_ms: 120000  # clamp (ms) on a provider-supplied Retry-After
```

The backlog itself is per-session runtime state, not config; only the tuning
above is read from `vis.yml`.

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
