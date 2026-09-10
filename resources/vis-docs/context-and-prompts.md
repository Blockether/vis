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

A goal is created only by `/goal` or `Session.goal()` in the Python SDK. Ordinary
messages never create one. A session has one goal; setting another replaces it.

```text
/goal Implement the change and verify all acceptance criteria
/goal --budget 30 -- Implement the change and verify it
/goal --pause
/goal --resume
/goal --cancel
```

`--` separates options from the objective. Quotes and newlines remain part of the
objective. Pause/resume/cancel use the normal command queue; use **Stop** to interrupt
current work immediately. An interrupted or failed goal turn pauses the goal.

In the SDK, `session.goal("Implement and verify the change", iteration_budget=30)`
returns the same `Turn` as `session.send()`. Use `turn.wait()` or its event stream.
There are no separate public SDK goal getter or updater methods. `session.read()["goal"]`
contains the current state; `session.send("/goal --pause")` submits a control command.

### Model command and continuation

The system prompt explains goal execution whenever the session has a goal. Its current
state is supplied automatically in `session["goal"]`. The model has the Python host command
`update_goal(goal_id, version, status, reason)`:

```python
g = session["goal"]
print(update_goal(g["id"], g["version"], "complete", "All acceptance criteria verified; affected tests pass."))
```

Only `complete` (with verification evidence) and `blocked` (with a concrete blocker) are
model-settable. The command cannot create, cancel, replace, resume or enlarge a goal.
Writing "complete" in prose does not update the goal. Completion is a model assessment,
not an independent verifier. A stale goal identity or lifecycle version is rejected.

While a goal is active, a final reply without tools is rejected with feedback and the
engine continues **the same turn**. An empty reply also counts as an iteration. Repeated
empty replies stop the turn and pause an unresolved goal rather than leaving it active
or declaring completion. A spent iteration budget instead leaves it `budget_limited`.
Goals never add permission to act or override a new user instruction. There is no
background scheduler or automatic restart. `/goal --resume` starts a normal turn again.

### Iteration budget

`--budget N` is a budget of **loop iterations, not tokens**. One iteration is a model
response together with its tool calls, if any. Prose-only replies, rejected final answers,
empty replies and the same turn's completion summary count. Multiple tools in one response
count as one iteration; provider retries within that request do not count separately.

The last allowed iteration can run its tools and call `update_goal`. The engine checks
the limit before the **next** model request. At the limit, an unresolved goal becomes
`budget_limited`, not `complete`. If the last tools resolved the goal, its recorded evidence
or blocker is returned without an extra model call. No `--budget` means no goal-specific
iteration limit; provider limits and failure/stop handling still apply.

The canonical fields are `iteration_budget` (positive integer or `null`) and
`iterations_used` (nonnegative integer). Pause/resume preserves usage. Exhausted goals
cannot resume without a newly specified goal and iteration budget. `tokens_used` is a
statistic only, including measured input, output and cached input; it never limits goal
execution. `time_used_ms` records provider time, excluding tool execution time.

### Session data and interfaces

`goal` is canonical session data: session detail and list responses carry either the
persisted object or `null`. The shared JSON contract defines its fields, statuses and
labels and supplies the OpenAPI schema. `get_session()`, `list_sessions()` and the
session summary in `read_session()` expose the same goal. Reconnecting clients receive
the current goal in `subscription.ready`, including when the session is idle.

The Companion header shows the goal beside the connection status. The TUI has separate
`Limits` and `Goal: <status>` buttons in the second footer row, without an extra header
row. Limits opens the provider quota and reset details. Goal opens the full objective,
status, iterations used and their limit, token statistics and completion evidence or
blocker. Counts stay in those details, not the button label. No toggle is required:
the Goal button appears when a session has a goal. Statuses are active, paused, blocked,
iteration-limit reached (`budget_limited`), complete and cancelled.

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
