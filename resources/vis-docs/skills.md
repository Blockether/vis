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
description: Use when the user wants to cut a release. Verifies, tags and publishes.
---

# Release checklist

1. Run `scripts/verify.sh`.
2. Bump the version in `VERSION`.
3. Tag and push.
```

Only `name` and `description` are read from the frontmatter. The description is
what the model sees when deciding whether to use the skill, so say *when* to use
it, not just what it is. A missing `name` falls back to the folder name.

Bundled files (scripts, templates, references) are read with ordinary file tools
when the skill is used.

## Where Vis looks

Skills are found in these folders, in order. The first skill with a given name
wins.

| Location | Scope |
|---|---|
| `.vis/skills` | Project |
| `.claude/skills`, `.pi/skills`, `.agents/skills`, `.opencode/skill` | Project |
| `~/.claude/skills`, `~/.pi/agent/skills`, `~/.agents/skills`, `~/.config/opencode/skill` | User |
| `~/.claude/plugins/cache/**/skills` | Installed Claude Code plugins |

`.agents/skills` is also searched in parent directories up to the Git root, so
repository-level skills apply inside every subproject.

Changes on disk are picked up without restarting Vis.

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
