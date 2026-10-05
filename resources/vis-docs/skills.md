# Skills

A skill gives Vis reusable instructions for a task, such as reviewing a change
or preparing a release. It is a folder containing a `SKILL.md` file, with any
scripts or templates the task needs. Vis lists available skills in its prompt
and reads the full instructions when a task needs one.

Skills written for Claude Code, pi, opencode or the
[agent skills standard](https://agentskills.io) work without changes.

## When to use

- **You explain the same workflow to Vis for every release, review or migration.** [Write it once as
  a skill](#write-a-skill), with the scripts and templates it needs. Vis reads the skill when a task
  needs it.
- **The procedure is too long to keep in `AGENTS.md`**, which Vis includes in every
  turn. Vis lists only a skill's name and description until a task needs the full
  instructions.
- **You already use skills with Claude Code, pi or opencode.** Vis searches [the
  same folders](#where-vis-looks), so they work without changes.
- **You want Vis to follow a particular skill now.** Name it with
  [`/skill:<name>`](#use-a-skill-explicitly).
- **A skill should not be offered for this task or project.** Control its
  [availability](#control-availability) without deleting the skill files.

Put rules that apply to every task in
[`AGENTS.md`](project-instructions.md#project-rules-agents-md). When the task needs a
tool that Vis can call rather than instructions, write an [extension](extending.md).

## Control availability

Open global settings or the current project, group or session's settings and
find the skill under **Skills**. Switch it off to remove it from future discovery,
`doc()` lookups, prompt inventories and slash expansion. Use **Use inherited
value** to follow the parent scope again. The global switch affects sessions
without a more specific override.

An extension can bundle a skill without its own switch. That skill follows the
extension's Auto, On or Off choice. Switch the extension off to remove the skill.

Availability does not erase instructions already in the conversation and is not
a filesystem permission. See [scoped settings](configuration.md#project-group-and-session-settings)
for inheritance and [the process jail](jail.md) for access restrictions.

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
should be used in `description`. The model uses it to select a skill. If `name`
is missing, Vis uses the folder name.

Bundled files (scripts, templates, references) are read with ordinary file tools
when the skill is used.

## Where Vis looks

Vis searches these sources in order. The first skill with a given name is used.
Later matches are ignored.

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
an extension update when you run `/reload`. If reload fails, Vis keeps the last
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

The `/` list does not show skills at first. They appear when you search by name.

## See also

- [Project instructions](project-instructions.md) — project rules and prompt templates.
- [Extending Vis](extending.md) — when a task needs a tool rather than instructions.
