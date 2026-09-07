# Skills

A skill is a folder with a `SKILL.md` file that tells Vis how to do a specific
kind of task. Vis lists every skill it finds in the system prompt and reads the
full instructions only when a task needs them.

Skills written for Claude Code, pi, opencode or the
[agent skills standard](https://agentskills.io) work without changes.

## Write a skill

```text
.vis/skills/release-checklist/
├── SKILL.md
└── scripts/
    └── verify.sh
```

```markdown
---
name: release-checklist
description: Use when the user requests a release. Verifies, tags and publishes.
---

# Release checklist

1. Run `scripts/verify.sh`.
2. Update the version in `VERSION`.
3. Tag and push.
```

Vis reads `name` and `description` from the frontmatter. State when the skill
should be used in `description`; the model uses it to select a skill. If `name`
is missing, Vis uses the folder name.

Bundled files (scripts, templates, references) are read with ordinary file tools
when the skill is used.

## Where Vis looks

Vis searches these locations in order. The first skill with a given name is
used; later matches are ignored.

| Location | Scope |
|---|---|
| `.vis/skills` | Project |
| `.claude/skills` | Project |
| `~/.claude/skills` | User |
| `~/.claude/plugins/cache/**/skills` | Installed Claude Code plugins |
| `.pi/skills` | Project |
| `~/.pi/agent/skills` | User |
| `.agents/skills` | Project |
| `~/.agents/skills` | User |
| `.opencode/skills`, then `.opencode/skill` | Project |
| `~/.config/opencode/skills`, then `~/.config/opencode/skill` | User |

Project locations are also searched in parent directories up to the Git root.
Changes on disk are loaded without restarting Vis.

## Use a skill explicitly

The model chooses skills on its own. To name one yourself, type `/skill:<name>`
followed by an optional task:

```text
/skill:release-checklist
/skill:release-checklist for the 2.1 branch
```

Skills are hidden from the initial `/` list but appear when you search by name.

## See also

- [Project instructions](context-and-prompts.md) — project rules and prompt templates.
- [Extending Vis](extending.md) — when a task needs a tool rather than instructions.
