# Project instructions

Vis reads Markdown instructions from your project and home directory. Changes
apply on the next turn without a restart.

| File | Purpose |
|---|---|
| `AGENTS.md` or `CLAUDE.md` | Rules for working in a project |
| `.vis/SYSTEM.md`, `.vis/APPEND_SYSTEM.md` | Replace or extend the system prompt |
| `.vis/prompts/*.md` | Reusable prompts invoked with `/name` |

## Project rules: AGENTS.md

Put instructions for your codebase in `AGENTS.md` at the project root: how to
run tests, coding conventions and files that must not change. Vis includes the
file in every turn.

Several files can apply at once, from broadest to narrowest:

1. `~/.vis/AGENTS.md` — your personal rules, in every project.
2. `AGENTS.md` in each parent directory of the project, so a monorepo root and a
   subproject both apply.
3. `AGENTS.md` in the project root.

Nearer files come later and override broader ones on conflict. `CLAUDE.md` is
read only where no `AGENTS.md` exists.

## System prompt files

To change the system prompt itself, add files under `.vis/` in the project or
under `~/.vis/`:

| File | Effect |
|---|---|
| `SYSTEM.md` | Replaces the built-in system prompt |
| `APPEND_SYSTEM.md` | Adds text after the system prompt |

A project file overrides a user file. Append files are applied user first,
project last.

The built-in prompt describes Vis's tools. Use `APPEND_SYSTEM.md` or
`AGENTS.md` to add instructions without removing those descriptions.

## Prompt templates: /name

A Markdown file in `.vis/prompts/` becomes a slash command named after the file.
Project templates override those in `~/.vis/prompts/`.

```markdown
---
description: Review the current branch against main
---

Review the current branch against main. Focus on: $ARGUMENTS

End with a verdict: approve or request changes.
```

Typing `/review error handling` sends the file as a message with `$ARGUMENTS`
replaced by `error handling`. If the template has no `$ARGUMENTS`, the text is
appended at the end.

Slash commands registered by extensions take precedence over templates.

## Explicit goals: /goal

Use `/goal` when you want Vis to keep working toward a stated result, rather than
stop after an ordinary reply. A session has one goal at a time; setting a new one
replaces it. Ordinary messages never create a goal.

```text
/goal Implement the parser fix and run its regression tests
/goal --budget 30 -- Implement the parser fix and run its regression tests
/goal Implement the parser fix and run its regression tests --budget 30
/goal --pause
/goal --resume
/goal --cancel
```

The goal appears in the Companion header and in the TUI's **Goal: <status>**
button. Open it to see the objective, status, iterations used and their limit,
elapsed time, and any completion evidence or blocker. The TUI's separate
**Limits** button shows provider quotas and reset times. No toggle is needed;
goal details appear when the session has a goal.

Pause, resume and cancel commands use the normal message queue. To interrupt
work immediately, use **Stop**. An interrupted or failed goal turn pauses the
goal. Sending a new message resumes a paused or blocked goal if it has budget
left, keeping its objective and usage. Use `/goal --resume` to resume without
adding instructions.

Completed, cancelled and budget-limited goals stay stopped. Command-only turns
and Council notifications do not resume goals. A goal does not give Vis extra
permissions or override your newer instructions, and there is no background
scheduler to restart it without a new user message.

### Iteration budget

`--budget N` limits **model responses, not tokens**. One response and its tool
calls count as one iteration, even if it calls several tools. Prose-only or empty
responses, rejected final answers and the completion summary also count. Provider
retries within a request do not count separately.

The final allowed iteration can still run tools and update the goal. Vis checks
the limit before making another model request. If the goal remains unresolved,
its status becomes `budget_limited`, not complete. If the last tools resolved it,
Vis returns the recorded evidence or blocker without another model call.

Pausing and resuming preserve the count. Once the budget is spent, set a new
goal and budget to continue. Without `--budget`, there is no goal-specific
iteration limit; provider limits and failure handling still apply.

The budget option can go before or after the objective. A leading `--` keeps
everything after it as literal objective text, including any `--budget` text.
Quotes and newlines are also preserved.

### Model command and continuation

Vis gives the agent the current goal in `session["goal"]`. The agent records
completion with verification evidence, or reports a concrete blocker, through
`update_goal`:

```python
g = session["goal"]
print(update_goal(g["id"], g["version"], "complete", "Parser regression tests pass."))
```

The agent can set only `complete` or `blocked`. It cannot use this command to
create, cancel, replace, resume or enlarge a goal. Stale goal IDs and lifecycle
versions are rejected. Saying "complete" in a reply does not update the goal;
even a recorded completion is the model's assessment, not an independent check.

While the goal is active, you can receive a progress reply without ending the
work. Vis keeps that reply and automatically continues the same turn; it does
not reject the reply or require the agent to mark the goal `blocked` to answer.

Before reporting `blocked`, the agent is instructed to check the whole objective
for progress, a verified operation still running, or no progress. The same
concrete blocker must prevent all meaningful authorized work for at least three
consecutive goal continuations. New user input, a resume or new progress resets
that audit. One waiting branch or missing verification is not enough: the agent
should continue other available work, without repeatedly polling just to reach
the count. This is a model-assessed policy, not an independent engine check.

Repeated empty replies stop the turn and pause an unresolved goal. An exhausted
iteration budget instead leaves it `budget_limited`.

### Session data and interfaces

In the Python SDK, `session.goal("Implement and verify the change", iteration_budget=30)`
creates a goal and returns the same `Turn` as `session.send()`. Use `turn.wait()`
or its event stream to follow the work. `session.read()["goal"]` contains the
current state; `session.send("/goal --pause")` submits a control command. There
are no separate public SDK getter or updater methods for goals.

Session detail and list responses include `goal` as an object or `null`.
`get_session()`, `list_sessions()` and the session summary in `read_session()`
expose the same data. Reconnecting clients receive it in `subscription.ready`,
even when the session is idle. The shared JSON contract and OpenAPI schema
define the fields and labels.

Statuses are `active`, `paused`, `blocked`, `budget_limited` (shown as
"iteration-limit reached"), `complete` and `cancelled`. `iteration_budget` is
a positive integer or `null`; `iterations_used` is a nonnegative integer.
`tokens_used` records measured input, output and cached input for statistics;
it does not limit execution and is not shown in goal details.

`time_used_ms` records active wall-clock time through `updated_at`, including
model requests, tools and time between iterations. While active, the duration is
`time_used_ms + max(0, now - updated_at)` in milliseconds. The clock stops for
paused, blocked, budget-limited, complete and cancelled goals. Resuming continues
from the saved duration; replacing the goal resets it. Companion updates the
clock every second while goal details are open.

## Skills: /skill:name

Every [skill](skills.md) is available as `/skill:<name> [task]`. Skills are
hidden from the initial `/` list but appear when you search by name.

## Shell shortcuts: ! and !&

A message starting with `!` runs a shell command directly, without sending
anything to the model:

```text
!git status        # run and wait for the output
!&npm run dev      # start in the background and return immediately
```

Use `!&` for servers, watchers and long test runs. The output is stored in the
transcript, so later turns can refer to it.

Shell shortcuts need the **Shell commands** toggle enabled in settings.

## See also

- [Skills](skills.md) — instructions loaded on demand.
- [Configuration → System prompt](configuration.md#system-prompt) — the equivalent config keys.
- [Extension API → Slash commands](extension-api.md#slash-commands) — commands provided by extensions.
