# Skills

Vis can discover and load the **skills** you (or other AI coding harnesses)
leave on disk — no vis-specific format required. This is a droppable
compatibility layer, the `foundation-harness` extension: drop the jar, drop the
feature. It is the sibling of the shell layer's POSIX compatibility, but for the
skill markdown that Claude Code, pi, opencode, and the
[agents standard](https://agentskills.io) already define.

Skills are exposed through a model-facing verb, **`skill(name)`**, and a
user-facing invocation, **`/<name> [task]`** (see
[Context files & prompts](context-and-prompts.md)) — both gated by the
`:vis/harness-skills` feature toggle (**ON** by default).

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

1. While `:vis/harness-skills` is ON, the system prompt lists every skill as
   `name — description` (descriptions clipped to ~180 chars). This is the only
   always-present cost.
2. When a task matches, the model calls `skill("name")`, which loads the **full
   `SKILL.md` body** plus the absolute paths of every bundled resource.
3. The model reads those resource paths with its normal file tools and follows
   the instructions.

A skill loads **once per session**. The load is recorded on the durable session
context (`:session/loaded-skills`), which rides the session snapshot in the
database — so it survives resume. A second `skill("name")` call acknowledges
"already-loaded" without re-injecting the body (a ✓ marks loaded skills in the
prompt listing).

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

## Toggle

The feature lives in **Settings → Feature Toggles** (owned by this layer, so
removing the jar removes the toggle):

- **`:vis/harness-skills`** — discover + load skills. ON by default.

When the toggle is OFF the prompt listing and the `skill(name)` verb both
disappear, costing zero tokens.

## Invoking a skill yourself

The model picks skills from the prompt listing on its own, but you can force
one: every skill is also a prompt template named `<name>`, so typing
`/setup-pre-commit for husky` in any channel loads that skill's full
`SKILL.md` (once per session) and runs your task with it. Details in
[Context files & prompts](context-and-prompts.md).

## Using skills from other harnesses

Because discovery already scans Claude Code, pi, the agents standard, and
opencode roots, skills you authored for any of those tools are reused as-is —
there is nothing vis-specific to add. To make a skill vis-first, put it under
`.vis/skills/` (project) so it wins on precedence.
