# Skills

Vis can discover and load the **skills** you (or other AI coding harnesses)
leave on disk — no vis-specific format required. This is a built-in
compatibility layer, the `foundation-harness` foundation module (ships in core,
always available). It is the sibling of the shell layer's POSIX compatibility, but for the
skill markdown that Claude Code, pi, opencode, and the
[agents standard](https://agentskills.io) already define.

Skills are exposed through a model-facing verb, **`skill(name)`**, and a
user-facing invocation, **`/<name> [task]`** (see
[Context files & prompts](context-and-prompts.md)).

## What a skill is

A skill is a directory containing a `SKILL.md` file with YAML-ish `---`
frontmatter plus a body of instructions, alongside any bundled resource files
(scripts, templates, reference docs):

```
my-skill/
├── SKILL.md              # required: frontmatter + instructions
├── scripts/
│   └── process.sh
└── references/
    └── guide.md
```

```markdown
---
name: my-skill
description: What this skill does and WHEN to use it. Be specific.
---

# My Skill

## Usage

Run `scripts/process.sh <input>` to …
```

Only `name` and `description` are read from the frontmatter. A missing `name`
falls back to the skill's directory name; a missing `description` is fine but
makes the skill harder for the model to pick. The parser is a minimal,
dependency-free reader — a folded (multi-line) `description` is joined into one
line.

## Progressive disclosure

Skills are **progressive**, so they cost almost nothing until used:

1. The system prompt lists every skill as `name — description` (descriptions
   clipped to ~180 chars). This is the only always-present cost.
2. When a task matches, the model calls `skill("name")`, which loads the **full
   `SKILL.md` body** plus the absolute paths of every bundled resource.
3. The model reads those resource paths with its normal file tools and follows
   the instructions.

The first activation returns the **full `SKILL.md` body**. While that exact body
remains on the provider-visible tape, another `skill("name")` call returns only
a compact `already-active` receipt. The original tool call and result are left
byte-for-byte in their append-only position, so deduplication does not rewrite
the cached prefix.

The activation index is rebuilt from the post-fold provider wire, not inferred
from the mere existence of a database row. If a legacy fold removed the body,
if a new user turn no longer replays that tool result, or if the file's digest
changed, the next call returns the full body exactly once and establishes a new
live activation. `session_fold` excludes a currently active skill iteration
from broad fold targets; other selected iterations still compact normally.

## Where skills come from

Vis scans a fixed, ordered registry of source roots. Precedence is **source
order, first-name-wins**: vis' own project-local skills win, then other
harnesses' project dirs, then their user dirs, then plugin caches.

| Precedence | Tool | Location | Scope |
|---|---|---|---|
| 1 | vis | `.vis/skills` | **project / dir-local** |
| 2 | Claude Code | `.claude/skills` | project |
| 3 | Claude Code | `~/.claude/skills` | user |
| 4 | Claude Code | `~/.claude/plugins/cache/**/skills` | installed plugins |
| 5 | pi | `.pi/skills` | project |
| 6 | pi | `~/.pi/agent/skills` | user |
| 7 | agents standard | `.agents/skills` | project **+ ancestors up to the git root** |
| 8 | agents standard | `~/.agents/skills` | user |
| 9 | opencode | `.opencode/skill` | project |
| 10 | opencode | `~/.config/opencode/skill` | user |

Project-relative roots (`.vis/skills`, `.claude/skills`, …) resolve against the
**active workspace root** (the directory the session works in), so a skill
checked into a repo travels with it. The `.agents/skills` root additionally
walks the workspace root's ancestor directories up to the git repo root
(nearest wins) — in a monorepo, repo-level skills apply inside every
subproject. The scan is defensive: missing directories are simply skipped and
it never throws.

Discovery is **live**: the source roots are stat-checked on each access and
re-scanned when a `SKILL.md` (or agent file) appears, changes, or disappears —
a skill added mid-session shows up without a restart.

### Project-local `.vis/skills`

Drop a skill dir straight into your repo under `.vis/skills/<name>/SKILL.md` and
it takes precedence over anything with the same name from a global or
other-harness location — the natural place for skills specific to one project.

## Availability

Skill discovery, the prompt catalog, and `skill(name)` are built in and always
available. There is no skills feature toggle.

## Invoking a skill yourself

The model picks skills from the prompt listing on its own, but you can force
one: every skill is also a prompt template named `<name>`, so typing
`/setup-pre-commit for husky` in any channel loads that skill's full
`SKILL.md` directly into that user message and runs your task with it. Details in
[Context files & prompts](context-and-prompts.md).

## Using skills from other harnesses

Because discovery already scans Claude Code, pi, the agents standard, and
opencode roots, skills you authored for any of those tools are reused as-is —
there is nothing vis-specific to add. To make a skill vis-first, put it under
`.vis/skills/` (project) so it wins on precedence.
