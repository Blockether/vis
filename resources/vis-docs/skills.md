# Skills

A skill gives Vis reusable instructions for a task, such as reviewing a change
or preparing a release. It is a folder containing a `SKILL.md` file, with any
scripts or templates the task needs. Vis lists available skills in its prompt
and reads the full instructions when a task needs one.

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

Vis searches these sources in order. The first skill with a given name is used;
later matches are ignored.

| Location | Scope |
|---|---|
| `.vis/skills` | Project |
| `.claude/skills` | Project |
| `.pi/skills` | Project |
| `.agents/skills` | Project |
| `.opencode/skills`, then `.opencode/skill` | Project |
| Nested projects using those same locations | When the session is at the Git root |
| `~/.claude/skills` | User |
| `~/.claude/plugins/cache/**/skills` | Installed Claude Code plugins |
| `~/.pi/agent/skills` | User |
| `~/.agents/skills` | User |
| `~/.config/opencode/skills`, then `~/.config/opencode/skill` | User |
| Registered extension packages | Qualified `package/skill` names |

Project locations are also searched in parent directories up to the Git root.
Changes to local skills are loaded without restarting Vis. Skills bundled with
an extension update when you run `/reload`; if reload fails, Vis keeps the last
working version. A local skill with the same qualified name overrides the
package skill without changing the package's tools.

See [bundled skills](extension-packages.md#bundled-skills) to ship a procedure and
its resources with an extension. Installing a package or reading `doc(name)` does
not execute its skill or grant permission for actions described by it.

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
