# Extending Vis

An extension is a Python plugin that adds custom tools, user commands or
integrations to Vis. Start with one file and one tool. Add a package only when
you need dependencies, reusable code or distribution. You can write it yourself
or ask Vis to build it.

## When to use

- **Vis rebuilds the same steps in every session**, such as running your tests with
  the right options or querying a service. Turn them into a tool with one clear
  result. [Your first extension](#your-first-extension) takes one file.
- **A rule must be checked every time, not only remembered.** Put the check in code, such as a [hook
  that runs after each edit](extension-design.md#check-code-complexity-after-edits).
- **You want a command that you run yourself from the chat.** Add a [slash
  command](extension-api.md#slash-commands).
- **You would rather not write the code.** [Ask Vis to build the
  extension](#ask-vis-to-build-an-extension) from a description of the task.

If Vis needs only instructions, not code, write a [skill](skills.md) or add the rule
to [`AGENTS.md`](context-and-prompts.md#project-rules-agents-md). [Choose what you
need](#choose-what-you-need) compares the options.

## Choose what you need

| I want to… | Use | Read next |
| --- | --- | --- |
| Let the agent call Python code or an external service | A tool, declared with `vis.Symbol` | [Your first extension](#your-first-extension) |
| Add a command a person types, such as `/hello` | `vis.SlashCommand` | [Slash commands](extension-api.md#slash-commands) |
| Explain when to use an extension's tools | The extension's short `prompt` | [Prompts and discovery](extension-api.md#prompts-and-discovery) |
| Describe a reusable, multi-step procedure | A skill (`SKILL.md`), with no Python extension | [Skills](skills.md) |
| Check calls before they run or inspect results afterward | `vis.OpHook` | [Op hooks](extension-api.md#op-hooks) |
| Ask for input or display progress | `vis.ask` or `vis.live` inside a tool | [Forms](human-input.md) · [Live views](live-views.md) |
| Add an LLM service with custom authentication | `vis.Provider` | [Provider extensions](provider-extensions.md) |

Tools do work. Prompts and skills explain when or how to use them. Instructions alone do not
register a callable, run a procedure or authorize its side effects.

If a rule must be checked every time, put the check in a domain function or hook. Do not rely on the
agent to remember it. You decide what counts as a valid result. Before hooks can refuse calls. After
hooks inspect outcomes and can give context for the next step, but they do not undo completed work.
For an example that covers patches and plain Python writes, see the [tested post-edit complexity
check](extension-design.md#check-code-complexity-after-edits).

## Your first extension

**Before you start:** install Vis and open a project you trust. This example uses
only Python's standard library and the SDK supplied by Vis: no pip install, uv,
manifest or separate virtual environment is needed.

**Review extensions before you load them.** They run with your user permissions, outside the model's
jail. This includes their dependencies. It also includes extension files that come with a project
that you check out.

### 1. Create the entry file

In your project, create `.vis/extensions/` if needed, then save this complete file
as `.vis/extensions/greeting_tools.py`:

```python
# .vis/extensions/greeting_tools.py
from __future__ import annotations

import blockether.vis.extension as vis


def hello(name: str, *, uppercase: bool = False) -> str:
    """Greet one person without sending a message.

    name must be nonblank; surrounding whitespace is removed.
    Preserves capitalization unless uppercase is requested.
    Raises ValueError for a blank name. Does not modify stored state.
    """
    if not name.strip():
        raise ValueError("name must not be blank")
    text = f"Hello, {name.strip()}!"
    return text.upper() if uppercase else text


def greeting_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return vis.ActivityPresentation(
        "Greet person", f"{len(result)} characters",
        (vis.ActivityText(result[:1000]),),
    )


vis.register_extension(
    vis.Extension(
        name="greeting",
        description="Generate greetings without sending messages.",
        alias="greeting",
        symbols=[vis.Symbol(
            hello, activity=vis.Activity(
                label="Greet person", show_start=False, render=greeting_activity
            )
        )],
    )
)
```

The filename identifies the entry file. `name` identifies the extension. `Symbol(hello)` exposes the
callable as `hello`. `alias` does not add a prefix. Keep entry filenames different from the packages
that they import.

Declare an Activity next to every tool binding. Activities are for people to read, so use clear
sentence-case English labels, not Python identifiers or serialized objects. The callback shows the
greeting and a useful count, and the return value stays available to Python.

This fast greeting uses `show_start=False`, so only its end
result is shown. Quick local reads and patches also need no running row. Slow
work and network requests should keep `show_start=True`. The engine still tracks
start/end internally and preserves failures and cancellation.
See [Activity presentation](extension-api.md#activity-presentation) for human-readable
summaries, object methods, intermediate updates, limits and testing.

### 2. Load it

Start Vis in that project, or type `/reload` in an existing session. Reloaded tools
become available at the next turn boundary. If loading fails, run
`vis-agent doctor` in your terminal and follow [troubleshooting](extension-troubleshooting.md).

### 3. Discover and call it

Ask Vis: **“Use the hello tool to greet Ada, first normally and then in uppercase.”**
The agent can inspect and call it in `python_execution`:

```python
print(apropos(r"^hello$"))
print(doc("hello"))
print(await hello("Ada"))
print(await hello("Ada", uppercase=True))
```

The two calls print `Hello, Ada!` and `HELLO, ADA!`. `apropos()` filters public
names, not descriptions. `doc()` combines the registered signature and resolved
types with the short semantic docstring. It shows parameter kinds, default presence,
return type and effect tag without requiring that information to be copied into
prose. `hello.contract` exposes the same contract as structured data when needed.

A displayed default of `...` means an optional argument's value was withheld,
not that you should pass `Ellipsis`. Omit the argument to use its real default.
The docstring explains the resulting behavior: capitalization is preserved.
See [documenting defaults](extension-design.md#document-default-behavior).

### 4. Check an edit

Change the greeting text, run `/reload`, and ask for another greeting on the next turn. Check both
the result and `doc("hello")`. If a reload fails, Vis keeps the last working version and marks it
stale. Fix the error before you test the edit. Registration or a successful reload alone does not
prove that a tool call works.

## Ask Vis to build an extension

Describe the task, inputs, expected result and permitted side effects. For example:

```text
Build a project-local Vis extension that lists this project's open GitHub issues.
Read doc("extending") first. Reuse the existing authenticated gh CLI.
Return typed issue summaries; do not create or modify issues.
Choose the smallest suitable layout. Test the implementation and a registered
call, explain omitted arguments, and tell me which reload steps remain.
Do not publish or install it globally.
```

When implementing a requested extension:

1. Check whether an existing tool or skill already covers the task.
2. Choose one file for a small integration or an existing Python package for reusable logic. Choose
   a distributable package when the request includes sharing it.
3. Register once, and annotate inputs and results. In docstrings, describe meaning, preconditions,
   side effects and failures. Do not copy signatures or schemas into them.
4. Test ordinary Python behavior, then discovery and a real tool call in Vis.
5. Report any untested boundary or required reload. Do not substitute registration
   success for execution or add publishing steps the user did not request.

## Continue by task

| Task | Guide available on the site and through `doc(name)` |
| --- | --- |
| Design useful tools, typed results and tests | [Extension design](extension-design.md) · `doc("extension-design")` |
| Install, reload, remove or share a package | [Installing and sharing extensions](extension-packages.md) · `doc("extension-packages")` |
| Connect an existing editable uv project | [Using an existing Python project](extension-development.md) · `doc("extension-development")` |
| Look up declarations, defaults, callbacks and host operations | [Extension API](extension-api.md) · `doc("extension-api")` |
| Diagnose a missing tool, stale result or import error | [Extension troubleshooting](extension-troubleshooting.md) · `doc("extension-troubleshooting")` |

## See also

- [Extension design](extension-design.md) — move beyond the one-file example.
- [Installing and sharing extensions](extension-packages.md) — locations, dependencies and distribution.
