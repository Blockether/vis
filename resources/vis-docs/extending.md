# Extending Vis

An extension adds Python tools, user commands or integrations to Vis. Start with
one file and one tool; add a package only when you need dependencies, reusable
code or distribution. You can write it yourself or ask Vis to build it.

## Choose what you need

| I want to… | Use | Read next |
| --- | --- | --- |
| Let the agent call Python code or an external service | A tool, declared with `vis.Symbol` | [Your first extension](#your-first-extension) |
| Add a command a person types, such as `/hello` | `vis.SlashCommand` | [Slash commands](extension-api.md#slash-commands) |
| Explain when to use an extension's tools | The extension's short `prompt` | [Prompts and discovery](extension-api.md#prompts-and-discovery) |
| Describe a reusable, multi-step procedure | A skill (`SKILL.md`); no Python extension required | [Skills](skills.md) |
| Check calls before they run or inspect results afterward | `vis.OpHook` | [Op hooks](extension-api.md#op-hooks) |
| Ask for input or display progress | `vis.ask` or `vis.live` inside a tool | [Forms](human-input.md) · [Live views](live-views.md) |
| Add an LLM service with custom authentication | `vis.Provider` | [Provider extensions](provider-extensions.md) |

Tools do work; prompts and skills explain when or how to use them. Instructions
alone do not register a callable, run a procedure or authorize its side effects.
If a rule needs to be checked every time, implement the check in a domain
function or hook rather than rely on the agent remembering it. You decide what
counts as a valid result. See the [tested post-edit complexity check](extension-design.md#check-code-complexity-after-edits)
for an example that covers patches and plain Python writes. Before hooks can
refuse calls; after hooks inspect outcomes and can supply context for what to do
next. They do not undo completed work.

## Your first extension

**Before you start:** install Vis and open a project you trust. This example uses
only Python's standard library and the SDK supplied by Vis: no pip install, uv,
manifest or separate virtual environment is needed.

**Review extensions before loading them.** They run with your user permissions,
outside the model's jail. This includes their dependencies and extension files
already present in a project you have just checked out.

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
    uppercase defaults to False, preserving the recipient's capitalization.
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


vis.register(
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

The filename identifies the entry file; `name` identifies the extension;
`Symbol(hello)` exposes the callable as `hello`. `alias` does not add a prefix.
Keep entry filenames different from the packages they import. Declare an Activity
beside every tool binding. Activities are meant for human consumption: use clear
sentence-case English labels, not Python identifiers or serialized objects. The
callback shows the greeting and a useful count while the return value stays
available to Python.

This fast greeting uses `show_start=False`, so only its end
result is shown. Quick local reads and patches also need no running row; slow
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
names, not descriptions. `doc()` gives the complete tool documentation.
`hello.contract` exposes the same description as structured data when needed.

A displayed default of `...` means an optional argument's value was withheld,
not that you should pass `Ellipsis`. Omit the argument to use its real default.
The docstring explicitly explains the public `False` default in this example.
See [documenting defaults](extension-design.md#document-default-behavior).

### 4. Check an edit

Change the greeting text, run `/reload`, and ask for another greeting on the next
turn. Check both the result and `doc("hello")`. If a reload fails, Vis retains the
last working version and marks it stale; fix the error before testing the edit.
Registration or a successful reload alone does not prove that a tool call works.

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
2. Choose one file for a small integration, an existing Python package for reusable
   logic, or a distributable package when sharing it is part of the request.
3. Register once; annotate tools and document inputs, defaults, results and failures.
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
