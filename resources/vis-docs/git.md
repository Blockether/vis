# Git tool

Vis exposes one host-Git tool, `git`, when the workspace is in or contains a
Git repository. It runs the `git` executable already on the host `PATH`, from
the active workspace root.

## Direct native call

Use the native tool for a small, fixed batch. Its only input is `commands`: a
non-empty list of non-empty argument lists. Each inner list is the arguments
*after* `git`; do not include the executable and do not pass a shell string.

```json
{
  "commands": [
    ["status", "--short"],
    ["diff", "--stat"]
  ]
}
```

Arguments are literal tokens, so spaces stay safe:

```json
{
  "commands": [
    ["add", "docs/git guide.md"],
    ["commit", "-m", "docs: explain the Git tool"]
  ]
}
```

Commands run serially in the given order. This makes a mutation sequence such
as `add` then `commit` reliable. A non-zero exit is result data, not a tool
transport failure: inspect it and expect later commands in the same batch to
still run.

`git` IS a user of the `shell` tool: every command runs through the shell's own
runner, so it inherits one working directory (`cwd`), one process jail, one
capped capture and one timeout. It takes the same `commands` key, in the same
input order, and returns the same `{"commands": [...]}` envelope. Only the item
shape differs, and it has to: a git item is an argv list (no shell, nothing to
quote), a shell item is one `bash -lc` command line.

## Python sandbox call

The same engine-bound tool is available in `python_execution`. Await it and
read ordinary Python dict/list data:

```python
result = await git([
    ["status", "--short"],
    ["diff", "--stat"],
])

for command in result["commands"]:
    if command["exit"] != 0:
        print(command["args"], command["stderr"])
```

`await git(...)` is the canonical form. It returns one object with a
`"commands"` list in request order. Every command entry has these keys:

| Key | Meaning |
| --- | --- |
| `"cmd"` | Display form, including `git`. |
| `"args"` | The literal argument list supplied to Git. |
| `"stdout"` / `"stderr"` | Output for that command only; empty strings when empty. |
| `"exit"` | Exit code, or `None` when that command timed out. |
| `"duration_ms"` | Command duration in milliseconds. |
| `"timed_out"` | Whether that command exceeded its deadline. |
| `"timeout_secs"` | Deadline used for that command. |

Use the direct native tool for one simple action. Use `python_execution` when
you need to inspect, filter, or combine Git results without sending every
intermediate result into the conversation context.
