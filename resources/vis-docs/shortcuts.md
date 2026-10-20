# Siri and Shortcuts

The Vis app for iPhone adds Siri and Shortcuts actions. Talk to Vis from the Action Button or ask
Vis a question with your voice. Your shortcuts can also get a text or JSON answer from Vis. The
actions use the same machines and sessions as the app.

## When to use

- **You want to start a voice conversation with one press.** [Assign Talk to Vis to the Action
  Button](#talk-to-vis-from-the-action-button).
- **You want an answer without opening the app.** [Ask Vis with Siri](#ask-vis-with-siri).
- **A shortcut must use the answer, for example to fill a note or a calendar event.** [Get a
  structured answer](#get-a-structured-answer) as JSON that matches your schema.
- **You want a specific model or session.** [Choose the session and the
  model](#choose-the-session-and-the-model).
- **A long task must not block the shortcut.** [Send now and wait later](#send-now-and-wait-later).

To control Vis from your own program, use the [Python SDK](python-sdk.md) or the [Sessions
API](sessions-api.md). To run a prompt on a schedule, use [Automations](automations.md).

## Before you start

- Install the Vis app on an iPhone with iOS 16 or later.
- Pair the app with at least one machine. The actions use the machines that the app knows.

Most actions have an optional **Machine** parameter. If you leave it empty, the action uses your
primary machine.

## Talk to Vis from the Action Button

**Talk to Vis** opens the app in a session and starts a voice conversation. Tap the microphone to
speak.

1. Open **Settings** > **Action Button**.
2. Select **Shortcut**, then select **Talk to Vis**.
3. Press the Action Button.

Set **Session** to **New session**, **Last session** or **Chosen session**. You can also say "Talk
to Vis" to Siri.

## Ask Vis with Siri

Say "Ask Vis". Siri asks "What should Vis do?". Say your request. Siri reads the final answer to you
when the task ends.

In the Shortcuts app, **Ask Vis** returns the final answer as text. Use that text in the next
action, for example **Create Note** or **Send Message**.

**Ask Vis** waits up to 300 seconds for the answer. Change **Timeout (seconds)** for longer tasks.

## Get a structured answer

Put a JSON Schema in the **Response schema** parameter of **Ask Vis**. Vis then returns JSON that
matches the schema, not free text. This works like `response_schema` in the [Python
SDK](python-sdk.md).

For example, this schema asks for a city and a temperature:

```json
{
  "type": "object",
  "properties": {
    "city": {"type": "string"},
    "temperature": {"type": "number"}
  },
  "required": ["city", "temperature"]
}
```

Vis checks each answer against the schema. If the answer does not match, Vis asks the model to
correct it. **Max corrections** sets the number of tries, 2 by default. If no answer matches, the
action stops with an error.

Use **Get Dictionary from Input** on the result to read each field in the shortcut.

## Choose the session and the model

**Ask Vis** and **Send Vis Request** have a **Session** parameter:

| Session | Effect |
|---|---|
| New session | Vis creates a session for this request. This is the default. |
| Last session | Vis continues the last session that a Vis action used on this machine. Without one, Vis creates a session. |
| Chosen session | Vis continues the session in **Chosen session**. |

Set **Model** to choose the model for the request. Use these actions to work with models and
sessions:

| Action | Result |
|---|---|
| Get Vis Models | The models of the machine. **Only available** keeps the models of signed-in providers. |
| Create Vis Session | A new session, optionally with a model. |
| Find Vis Sessions | The sessions that match a search. |
| Get Vis Session Model | The model of the last or the chosen session. |
| Set Vis Session Model | Changes the model of the last or the chosen session. |

Each model has the properties **Provider**, **Name**, **Label**, **Billing**, **Is available** and
**Is default**.

## Send now and wait later

**Send Vis Request** gives the request to Vis and returns its turn at once. Vis works on the request
while the shortcut continues. Later, give that turn to **Wait for Vis Answer** to get the final
answer as text.

Use this pattern when a task takes longer than a shortcut can wait.

## Share text or a link

**Send to Vis** opens the app and puts text or a link in the message field. Use it from a shortcut,
or say "Send to Vis" to Siri.

## See also

- [Python SDK basics](python-sdk.md) — the same requests, structured answers and model choice from
  Python.
- [Sessions](sessions.md) — follow, continue and organize the sessions that the actions create.
- [Configuration](configuration.md) — sign in to providers and set the default model.
