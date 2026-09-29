# Controlling a session

You can keep typing while Vis is working. Messages sent during a running turn
are queued and run in order when the turn finishes. You can also interrupt the
running turn or quit the session.

## When to use

- **You think of a follow-up while Vis is still working.** Send it now. It [waits in
  the queue](#queue-a-message) and runs when the current turn finishes.
- **A queued message has a mistake.** Edit it before it runs. In the terminal, [move it back into
  the composer](#queue-a-message). In the Companion app, tap it to edit or remove it.
- **Vis is going in the wrong direction and you want to stop it.** [Cancel the
  turn](#cancel-a-turn). Your queued messages return to the composer so you can
  send them again.
- **You want to leave the session.** See [Quit](#quit) for what **Ctrl+C** does in
  each state.

## Queue a message

Press **Enter** to send. If no turn is running, the message starts one.
Otherwise it is added to the queue below the progress display.

Queued messages run in submission order. The queue pauses after a failed turn.
Resume it manually to send the next message.

To edit a queued message:

- **Terminal:** press **↑** to move the newest queued message back into the
  composer.
- **Companion app:** tap the message to edit it, or tap **×** to remove it.

The queue is stored in memory and cleared when the gateway restarts.

## Cancel a turn

Press **Esc** or **Ctrl+G** to cancel the running turn.

Cancellation stops the turn and returns queued messages to the composer as a
draft. To run them, submit the draft again.

## Quit

**Ctrl+C** depends on the current state:

| State | Ctrl+C |
|---|---|
| Nothing typed, nothing running | Quits |
| A draft in the composer | Clears the draft. A second press quits |
| A turn is running | Cancels the turn |
| A cancel is in progress | Quits immediately |

## See also

- [Desktop and mobile setup](index.md#connecting-the-companion-app) — the same queue from the Companion app or another machine.
- [Project instructions](context-and-prompts.md) — slash commands and shell shortcuts you can queue.
