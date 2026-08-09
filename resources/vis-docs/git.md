# Git tool

Vis exposes one host-Git tool, `git`, when the workspace is in or contains a
Git repository. It runs the `git` executable already on the host `PATH`, from
the active workspace root.

## Direct native call

One call runs ONE command. Its only input is `command`: a non-empty list of the
arguments *after* `git`; do not include the executable and do not pass a shell
string.

```json
{"command": ["status", "--short"]}
```

Arguments are literal tokens, so spaces stay safe:

```json
{"command": ["commit", "-m", "docs: explain the Git tool"]}
```

In `python_execution`, `git` also takes exactly one options map —
`await git({"command": ["status", "--short"]})`; never pass a bare string or an
argument array positionally. A sequence such as `add` then `commit` is two
calls, in that order; a non-zero exit is result data, not a tool transport
failure.

`git` IS a user of the `shell` tool: every command runs through the shell's own
runner, so it inherits one working directory (`cwd`), one process jail, one
capped capture and one timeout. It takes the same single-`command` shape and
answers with the same flat result. One thing differs, and has to: a git command
is an argv list (no shell, nothing to quote) while a shell command is one
`bash -lc` command line.

## Python sandbox call

The same engine-bound tool is available in `python_execution`. Await it and
read ordinary Python dict data:

```python
result = await git({"command": ["status", "--short"]})

if result["exit"] != 0:
    print(result["args"], result["stderr"])
```

`await git({...})` is the canonical form. It returns one flat object:

| Key | Meaning |
| --- | --- |
| `"command"` | Display form, including `git`. |
| `"args"` | The literal argument list supplied to Git. |
| `"stdout"` / `"stderr"` | Output for that command; empty strings when empty. |
| `"exit"` | Exit code, or `None` when the command timed out. |
| `"duration_ms"` | Command duration in milliseconds. |
| `"timed_out"` | Whether the command exceeded its deadline. |

Use the direct native tool for one simple action. Use `python_execution` when
you need to inspect, filter, or combine Git results without sending every
intermediate result into the conversation context.
