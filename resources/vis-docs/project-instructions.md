# Project instructions

Vis reads Markdown instructions from your project and your home directory, and it turns prompt
templates into slash commands. A change to a file applies on the next turn, without a restart.

| File | Purpose |
|---|---|
| `AGENTS.md` or `CLAUDE.md` | Rules for work in a project |
| `.vis/SYSTEM.md`, `.vis/APPEND_SYSTEM.md` | Replace or extend the system prompt |
| `.vis/prompts/*.md` | Reusable prompts that you run with `/name` |

## When to use

- **Vis keeps missing a project convention.** Write it in [`AGENTS.md`](#project-rules-agents-md).
- **You type the same request again and again.** Save it as a
  [prompt template](#prompt-templates-name).
- **A task must continue until the result is verified.** Set a [goal](#explicit-goals-goal) with
  an iteration budget.
- **A project needs different or extra system instructions.** Add
  [system prompt files](#system-prompt-files) under `.vis/`.
- **You want to change how Vis reports progress or writes its replies.** Read
  [Progress notes and writing style](#progress-notes-and-writing-style).
- **Vis answers in a different language than you write in.** Read how Vis chooses
  [the language of a reply](#the-language-of-a-reply).
- **You want to run a shell command without the model.** Start the message with
  [`!`](#shell-shortcuts-and).
- **Your own program must run templates, skills, or goals.** Use the
  [Project instructions API](project-instructions-api.md).

For a procedure that Vis reads only when a task needs it, write a [skill](skills.md).

## Project rules: AGENTS.md

Put the rules for your codebase in `AGENTS.md` at the project root. Examples are test commands,
code conventions, files that must not change, and the format of results. Vis includes the file in
every turn.

Several files can apply at once, in this order:

1. `~/.vis/AGENTS.md`, your personal rules for every project.
2. `AGENTS.md` in each parent directory of the project. A monorepo root and a subproject both
   apply.
3. `AGENTS.md` in the project root.

A nearer file comes later and wins on a conflict. Vis reads `CLAUDE.md` only where no `AGENTS.md`
exists.

## System prompt files

To change the system prompt itself, add these files under `.vis/` in the project or under
`~/.vis/`.

| File | Effect |
|---|---|
| `SYSTEM.md` | Replaces the built-in system prompt |
| `APPEND_SYSTEM.md` | Adds text after the system prompt |

A project file overrides a user file. Vis applies both append files, the user file first.

The built-in prompt describes the tools of Vis, and `SYSTEM.md` removes those descriptions. To add
instructions and keep the descriptions, use `APPEND_SYSTEM.md` or `AGENTS.md`.

## Progress notes and writing style

The built-in system prompt sets how Vis reports its work and how it writes. Do not copy these
rules into `AGENTS.md`, because they apply in every project. A `SYSTEM.md` file replaces them
with the rest of the built-in prompt.

While a task runs, Vis writes short progress notes:

- Before substantial work, Vis says the first step. Before an edit, it says the change.
- When a finding, a decision, or a blocker changes the next step, Vis writes a note.
- Each note has one or two sentences of useful facts. Routine reads and searches get no note.

The last reply starts with the answer, and you can read it without the progress notes.

### The language of a reply

Vis writes each progress note and each answer in the language of your latest request, unless you
ask for another language. If that request is short or mixes languages, Vis keeps the language of
your earlier requests. A technical term from another language, such as English, does not make a
request mixed.

Only your requests set the language. The system prompt, project rules, quoted text, code, logs,
tool output, files, and messages from other sessions do not change it.

### The writing style

Vis writes in a simple style, in every language. It uses these rules:

1. Keep sentences short.
2. Put one action in each step.
3. Say who does what.
4. Give each thing one name, and use only that name.

To get a different style for one task, ask for it in your message. To change the style for a
project, write the rule in `AGENTS.md`.

### Where each interface shows the notes and the answer

| Interface | Progress notes | Final answer |
|---|---|---|
| Terminal | Above the code of each step | After the last step |
| Desktop and Companion apps | Above the code of each step | After the last step |
| Phone and desktop alerts | No alert | The alert shows the answer |
| [Python SDK](sessions-api.md#follow-progress) | In the event stream after `send()` | In the turn record from `run()` or `turn.wait()` |
| [HTTP API](sessions-api.md#follow-progress) | In the event stream after the turn request | In the turn record |

Your [notification settings](distributions.md#notifications-on-the-desktop) control the alerts.

## Prompt templates: /name

A Markdown file in `.vis/prompts/` becomes a slash command with the name of the file. A project
template overrides a template with the same name in `~/.vis/prompts/`.

```markdown
---
description: Review the current branch against main
---

Review the current branch against main. Focus on: $ARGUMENTS

End with a verdict: approve or request changes.
```

`/review error handling` sends the file as a message, with `$ARGUMENTS` replaced by
`error handling`. If the template has no `$ARGUMENTS`, Vis adds the text at the end.

A slash command that an extension registers wins over a template with the same name.

## Explicit goals: /goal

Use `/goal` when Vis must work toward a stated result and not stop after an ordinary reply. A
session has one goal at a time, and a new goal replaces it. An ordinary message never creates a
goal. A goal gives Vis no extra permissions, and your newer messages win over it.

```text
/goal Implement the parser fix and run its regression tests
/goal --budget 30 -- Implement the parser fix and run its regression tests
/goal Implement the parser fix and run its regression tests --budget 30
/goal --pause
/goal --resume
/goal --cancel
/goal --clear
```

The Companion header and the TUI show the goal as a **Goal: <status> - <time>** button. The button
does not show the objective. Open the button to see the status,
the iterations used with their limit, and the elapsed time. When the goal is complete, the button
goes away.

The separate **Limits** button of the TUI shows provider quotas and reset times. It needs no
toggle.

### Pause, resume, and cancel a goal

The pause, resume, and cancel commands wait in the normal message queue. To interrupt the work at once, use **Stop**.

A resumed goal keeps its objective and its usage. To resume without new instructions, send
`/goal --resume`.

Completed, cancelled, cleared, and budget-limited goals stay stopped. Command-only turns and
Council notifications do not resume a goal, and no background scheduler restarts one. Vis keeps
no goal history. The conversation holds each goal that you set and each reply about it.

### Forget a goal

You cannot undo `/goal --clear`. Use it when Vis must no longer see the current goal. The command
stops the goal work.

### Iteration budget

`--budget N` limits model responses, not tokens. One response counts as one iteration, also when
it runs several tools. A pause and a resume keep the count. Without `--budget`, the goal has no
limit of its own.

Put the budget before or after the objective. A leading `--` makes the rest objective text, a
later `--budget` included.

### How a goal ends

Vis marks a goal complete only when it has evidence, such as passing tests. If Vis cannot
continue, it marks the goal blocked.

A progress reply does not end the goal, and Vis continues the work in the same turn.

### Goal states

| State | Meaning |
|---|---|
| `active` | Vis works on the goal |
| `paused` | You paused it, or a turn failed or was stopped. A new message resumes it |
| `blocked` | Vis cannot continue. Its reply names the blocker. A new message resumes it if budget remains |
| `budget_limited` | The iteration budget is used up. Set a new goal to continue |
| `complete` | Vis verified the work. Its reply gives the evidence |
| `cancelled` | You cancelled it |
| `cleared` | You forgot it with `--clear`. The apps show no goal |

Elapsed time counts only while the goal is active. To set, read, and control goals from your own
program, read [Project instructions API](project-instructions-api.md#set-a-goal).

## Skills: /skill:name

Each [skill](skills.md) is available as `/skill:<name> [task]`. The `/` list shows
skills only when you search by name.

## Shell shortcuts: ! and !&

Shell shortcuts need the **Shell commands** toggle on in settings. A message that starts with `!`
runs a shell command and sends nothing to the model.

```text
!git status        # run and wait for the output
!&npm run dev      # start in the background and return immediately
```

Use `!&` for servers, watchers, and long test runs. Vis stores the output in the transcript, so
later turns can refer to it.

## See also

- For instructions that Vis loads only when a task needs them, read [Skills](skills.md).
- To run templates, skills, and goals from your own program, read
  [Project instructions API](project-instructions-api.md).
- For the configuration keys that do the same as the system prompt files, read
  [System prompt in Configuration](configuration.md#system-prompt).
- For slash commands that extensions provide, read
  [Slash commands in Extension API](extension-api.md#slash-commands).
