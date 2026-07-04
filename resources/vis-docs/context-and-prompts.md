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

Replacement precedence: project `SYSTEM.md` > global `SYSTEM.md` > the EDN
config `:system-prompt {:replace? true}` form > the built-in core prompt.
Append files always apply, global first, project last — the nearest text sits
closest to the conversation. The EDN `:system-prompt` string form (see
[Configuration](configuration.md)) still works and is appended before the
`APPEND_SYSTEM.md` files.

Replacing the core prompt is a sharp tool — Vis's prompt teaches the whole
tool surface (file tools, `python_execution`, structural editors). Prefer
`APPEND_SYSTEM.md` or `AGENTS.md` unless you know you want a full rewrite.

## Prompt templates: `.vis/prompts/*.md`

A prompt template is a markdown file that expands into a user message when you
type `/<name> [arguments]` in any channel (TUI, web, Telegram, CLI):

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

## Skill invocations: `/skill:<name>`

Every discovered [skill](skills.md) is also exposed as a dynamic template
named `skill:<name>`, so you can load a skill explicitly instead of waiting
for the model to pick it:

```
/skill:setup-pre-commit          # load the skill, follow its instructions
/skill:setup-pre-commit for husky  # load it with a task appended
```

The expansion injects the full `SKILL.md` (once per session — an
already-loaded skill gets a pointer instead of a re-injection) plus its
bundled resource paths, exactly like the model-facing `skill(name)` verb.
