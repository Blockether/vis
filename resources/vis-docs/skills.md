# Skills

Vis can discover and load the **skills** you (or other AI coding harnesses)
leave on disk — no vis-specific format required. This is a built-in
compatibility layer, the `foundation-harness` foundation module (ships in core,
always available). It is the sibling of the shell layer's POSIX compatibility, but for the
skill markdown that Claude Code, pi, opencode, and the
[agents standard](https://agentskills.io) already define.

A skill is a **document**, not a verb. Every discovered `SKILL.md` joins the one
retrieval corpus, so the model finds it with **`apropos(text)`** and reads it
whole with **`doc(name)`** — the same two verbs that answer for a function
contract or a Vis documentation page. There is nothing to activate and no
session state to undo. You can also inject one yourself with **`/<name> [task]`**
(see [Context files & prompts](context-and-prompts.md)).

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
2. When a task matches, the model calls `doc("name")` and gets the **whole
   `SKILL.md`**: the frontmatter description as its first line, then the body
   verbatim. `apropos(text)` searches those bodies, so a rule the description
   never mentions is still findable.
3. The model reads the skill's bundled files with its normal file tools and
   follows the instructions.

Reading is the whole of using: there is no activation call, no receipt, no
`already-active` state, and no fold protection — a skill body is ordinary
context that the model can re-read with `doc(name)` whenever it wants it back.

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

Skill discovery, the prompt catalog, and the `doc`/`apropos` corpus entries are
built in and always available. There is no skills feature toggle.

## Invoking a skill yourself

The model picks skills from the prompt listing on its own, but you can force
one: every skill is also a prompt template named `<name>`, so typing
`/setup-pre-commit for husky` in any channel expands to

```
Use the skill "setup-pre-commit" for this task: read it with doc("setup-pre-commit")
unless its SKILL.md is already in this conversation, then follow it as written.

Task: for husky
```

and runs your task with it. Details in
[Context files & prompts](context-and-prompts.md).

The slash **names** the skill; it never pastes the body. Whether the
instructions still have to be fetched is the model's call — it is the only
party that can see whether that text is still in front of it, and `doc(name)`
prints it whole every time with no session effect. So every `/<name>` expands
to the same sentence: nothing is remembered between two of them. A skill owned
by a nested project also gets the sentence naming that project (the turn is
re-rooted there), and bundled resource paths are listed, because neither is
derivable from the body `doc` prints.

## Using skills from other harnesses

Because discovery already scans Claude Code, pi, the agents standard, and
opencode roots, skills you authored for any of those tools are reused as-is —
there is nothing vis-specific to add. To make a skill vis-first, put it under
`.vis/skills/` (project) so it wins on precedence.
