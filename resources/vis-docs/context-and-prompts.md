# Context files & prompts

Vis reads plain markdown files to learn your rules: **context files**
(`AGENTS.md` / `CLAUDE.md`) for project conventions, **system prompt files**
(`SYSTEM.md` / `APPEND_SYSTEM.md`) to change the core prompt itself, and
**prompt templates** (`.vis/prompts/*.md`) for reusable slash-invokable
prompts. All of them auto-refresh: Vis stat-checks the files each turn and
re-reads only when something changed — no restart, no manual reload.

## Context files: AGENTS.md / CLAUDE.md

Vis stacks guidance from three layers into every turn's
`PROJECT-INSTRUCTIONS` system block, outermost first:

1. **User-global** — `~/.vis/AGENTS.md` (or `~/.vis/CLAUDE.md`): your personal
   house rules, applied in every project.
2. **Ancestor directories** — `AGENTS.md` / `CLAUDE.md` in every parent
   directory of the workspace root, walking down from the filesystem root. In
   a monorepo this means the repo-root `AGENTS.md` *and* the subproject's
   `AGENTS.md` both apply.
3. **Workspace root** — `AGENTS.md` / `CLAUDE.md` in the directory you opened.

Per directory the precedence is strict: `AGENTS.md` wins, `CLAUDE.md` is only
read when `AGENTS.md` is absent there. Across layers nothing is dropped —
nearer files render **later**, so on conflict the more specific rules override
the outer ones (and the CORE engine contract always wins over both).

Files are inlined **verbatim, untruncated**; each one is labeled with its
origin and path so the model knows which layer a rule came from. Provider
prompt caching amortizes the cost across the session.

## System prompt files: SYSTEM.md / APPEND_SYSTEM.md

To change the core system prompt itself (not project rules), drop markdown
files under a `.vis/` directory:

| File | Effect |
|---|---|
| `<project>/.vis/SYSTEM.md` | **Replaces** the core system prompt (project) |
| `~/.vis/SYSTEM.md` | **Replaces** the core system prompt (global) |
| `~/.vis/APPEND_SYSTEM.md` | **Appends** to the system prompt (global) |
| `<project>/.vis/APPEND_SYSTEM.md` | **Appends** to the system prompt (project, rendered last) |

Replacement precedence: project `SYSTEM.md` > global `SYSTEM.md` > the
config `system_prompt: {is_replace: true}` form > the built-in core prompt.
Append files always apply, global first, project last — the nearest text sits
closest to the conversation. The `system_prompt` string form (see
[Configuration](configuration.md)) still works and is appended before the
`APPEND_SYSTEM.md` files.

Replacing the core prompt is a sharp tool — Vis's prompt teaches the whole
tool surface (file tools, `python_execution`, structural editors). Prefer
`APPEND_SYSTEM.md` or `AGENTS.md` unless you know you want a full rewrite.

## Prompt templates: `.vis/prompts/*.md`

A prompt template is a markdown file that expands into a user message when you
type `/<name> [arguments]` in any channel (TUI, CLI):

1. `<project>/.vis/prompts/*.md` — project templates (win name collisions)
2. `~/.vis/prompts/*.md` — user-global templates

The template name is the filename stem, overridable with frontmatter; a
`description` documents it:

```markdown
---
description: Review a PR branch against main
---

Review the current branch against main. Focus on: $ARGUMENTS

- Correctness first, style second.
- End with a verdict: approve / request changes.
```

Typing `/review error handling` expands the file and runs it as a normal LLM
turn. Argument handling:

- Every `$ARGUMENTS` occurrence is substituted with the raw argument string
  (empty when none given).
- A body without `$ARGUMENTS` gets non-blank arguments appended as a trailing
  paragraph.

Registered slash commands (from extensions) always win over templates — a
template only fires for a `/name` no extension claimed.

## Skill invocations: `/<name>`

Every discovered [skill](skills.md) is also exposed as a dynamic template
named `<name>`, so you can load a skill explicitly instead of waiting
for the model to pick it:

```
/setup-pre-commit          # load the skill, follow its instructions
/setup-pre-commit for husky  # load it with a task appended
```

The expansion explicitly injects the full `SKILL.md` plus its bundled resource
paths into that user message. The model reaches the same text on its own with
`doc(name)` — a skill is a document in the retrieval corpus, never a verb.

The injection is **idempotent per session**: the first `/<name>` carries the
body, every later one expands to a pointer at the copy already in the
conversation plus your new task, so re-invoking a skill costs a sentence. The
pointer names `doc(name)` as the way back if the earlier text has been folded
away, and an edited `SKILL.md` (different content) is injected in full again.

## Shell shortcuts: `!` and `!&`

A line that starts with `!` is a shell escape — the command runs directly,
**without an LLM round-trip**, so it's instant and costs no tokens. It's the
shell analogue of `/slash`, and works the same way in the **TUI** and the
**companion** app:

```
!git status            # run and print its output (blocks)
!&npm run dev          # spawn and return immediately, under a resource id
```

- `!<cmd>` runs the command and blocks until it exits, printing its output. Use
  it for short bounded commands.
- `!&<cmd>` spawns under an auto-generated resource id (`background-<hex>`) and
  returns right away. Prefer it for commands that may take a while: builds, test
  suites, servers, watchers, and interactive processes.
- Both reach the same place the model does. There is no shell TOOL: a process is
  started only from `python_execution`, where every shell run is a background run
  and the result is a HANDLE:
  `sh = await shell("npm run dev", id="dev")`, then
  `sh.wait(30)` (the only wait there is), `sh.logs(offset=0)`, `sh.type("y")`,
  `sh.stop()`. Every answer already carries that shell's status — running or exited,
  since when, its `log_path`, and the live `cpu_ms`/`cpu_percent`/`rss_bytes` of its
  process tree — so nothing has to ask again. There is no `wait` knob on the request — a request cannot select a
  mode. Every shell keeps its log by id for the session, so a finished run is
  still readable a turn later.
- A bare `!` (or `!&`) with no command is ordinary prose and runs as a normal
  LLM turn.

While you're typing a shell shortcut, the composer flags it visually so a
shell command never looks like an ordinary message — with **no layout shift**.
The TUI tints just the leading `!`/`!&` marker in the shell tool color; the
companion composer tints its frame and shows a small `shell` / `shell &` pill in the
corner. The cue only lights up once a real command follows the marker (a bare
`!`/`!&` stays neutral), mirroring the run/no-run rule above.

The command output renders as the turn's answer bubble and is persisted in the
transcript. Crucially, the result **lands in context exactly like a
model-issued `shell` call** — so a later turn can reason over
what the command printed, just as if the model had run it itself.

Both shortcuts require the **shell layer** to be enabled — the user-owned
`shell` toggle (settings dialog → *Shell commands*). When it's off, the
shortcut refuses cleanly with a note on how to
turn it on, and nothing runs.
