# Skills

A skill is a folder with a `SKILL.md` file that holds reusable instructions for a task, such as a
review or a release. The folder also holds the scripts and templates that the task needs. Vis lists
the available skills in its prompt and reads the full instructions when a task needs one.

Skills written for Claude Code, pi, opencode, or the
[agent skills standard](https://agentskills.io) work without changes.

## When to use

- **You explain the same workflow to Vis for every release, review, or migration.**
  [Write it once as a skill](#write-a-skill), with its scripts and templates.
- **The procedure is too long for `AGENTS.md`.** Vis includes `AGENTS.md` in every turn. For a
  skill, Vis lists only the name and the description until a task needs the full instructions.
- **You already use skills with Claude Code, pi, or opencode.** Vis searches
  [the same folders](#where-vis-looks).
- **You want Vis to follow a particular skill now.** Name it with
  [`/skill:<name>`](#use-a-skill-explicitly).
- **A skill must not be offered for this task or project.**
  [Switch it off](#control-availability). The skill files stay.

Put rules that apply to every task in
[`AGENTS.md`](project-instructions.md#project-rules-agents-md). When the task needs a tool that Vis
can call, write an [extension](extending.md).

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

Vis reads `name` and `description` from the frontmatter. In `description`, state when to use the
skill. The model selects a skill by its description. If `name` is missing, Vis uses the folder name.

When Vis uses the skill, it reads the bundled scripts, templates, and references with the ordinary
file tools.

## Where Vis looks

Vis searches these sources in order and uses the first skill with a given name.

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

Vis also searches the project locations in each parent directory up to the Git root. At the Git
root, Vis skips nested projects that Git ignores. It also skips projects in dependency directories,
such as `.venv`, `venv`, `site-packages`, and `node_modules`. Vis does not load skills that
installed packages ship.

Vis loads changes to local skills without a restart. Skills that an extension bundles update when
you run `/reload`. If the reload fails, Vis keeps the last version that worked. A local skill with
the same qualified name overrides the package skill. The tools of the package do not change.

To ship a procedure and its resources with an extension, read
[bundled skills](extension-packages.md#bundled-skills). To install a package or to read `doc(name)`
does not run a skill, and it gives no permission for the actions that the skill describes.

## Use a skill explicitly

The model chooses skills itself. To name one, type `/skill:<name>`, then an optional task.

```text
/skill:release-checklist
/skill:release-checklist for the 2.1 branch
```

The `/` list does not show skills at first. Skills appear when you search by name.

## Control availability

Open the settings of the scope: global, the current project, the group, or the session. Find the
skill under **Skills**. Switch the skill off to remove it from future discovery, `doc()` lookups,
prompt inventories, and slash expansion. To follow the parent scope again, choose
**Use inherited value**. The global switch applies to each session that has no more specific
override.

A skill that an extension bundles has no switch of its own. The skill follows the Auto, On, or Off
choice of the extension. To remove the skill, switch the extension off.

A switch does not erase instructions that are already in the conversation, and it is not a
filesystem permission. For inheritance, read
[scoped settings](configuration.md#project-group-and-session-settings). To restrict access, read
[the process jail](jail.md).

## See also

- For project rules and prompt templates, read [Project instructions](project-instructions.md).
- When a task needs a tool and not instructions, read [Extending Vis](extending.md).
