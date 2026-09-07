# Controlling a session

You can keep typing while Vis is working. Messages sent during a running turn
are queued and run in order when the turn finishes.

## Queue a message

Press **Enter** to send. If nothing is running, the message starts a turn. If a
turn is running, the message joins the queue shown under the progress bubble.

Queued messages run oldest first, one after another. If a turn fails, the queue
pauses so the same failure is not repeated. Resume it yourself to send the next
message.

To edit a queued message:

- **Terminal:** press **↑** to move the newest queued message back into the
  composer.
- **Companion app:** tap the message to edit it, or tap **×** to remove it.

The queue is kept in memory. Restarting the gateway drops it.

## Cancel a turn

Press **Esc** or **Ctrl+G** to cancel the running turn.

Cancelling stops the turn and moves every queued message back into the composer
as a draft. Nothing queued is sent automatically after a cancel; resend what you
still want.

## Quit

**Ctrl+C** depends on the current state:

| State | Ctrl+C |
|---|---|
| Nothing typed, nothing running | Quits |
| A draft in the composer | Clears the draft; press again to quit |
| A turn is running | Cancels the turn |
| A cancel is in progress | Quits immediately |

## See also

- [Remote access and the Companion app](gateway.md) — the same queue from the Companion app or another machine.
- [Project instructions](context-and-prompts.md) — slash commands and shell shortcuts you can queue.
