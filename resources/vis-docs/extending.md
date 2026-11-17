# Extending Vis

An extension is a Python plugin that adds custom tools, slash commands, or integrations to Vis.
Start with one file and one tool. Add a package only when you need dependencies, reusable code, or
distribution. Write the extension yourself, or ask Vis to build it.

## When to use

- **Vis rebuilds the same steps in every session.** Turn the steps into a tool with one clear
  result, such as a test run with the right options. [Your first
  extension](#your-first-extension) takes one file.
- **A rule must be checked every time.** Put the check in code, such as a [hook that runs after
  each edit](extension-design.md#check-code-complexity-after-edits).
- **You want a command that you run yourself from the chat.** Add a [slash
  command](extension-api.md#slash-commands).
- **You do not want to write the code.** [Ask Vis to build the
  extension](#ask-vis-to-build-an-extension) from a description of the task.

If Vis needs only instructions and no code, write a [skill](skills.md) or add the rule to
[`AGENTS.md`](project-instructions.md#project-rules-agents-md).

## Choose what you need

| Goal | Use | Read next |
| --- | --- | --- |
| Let the agent call Python code or an external service | A tool, declared with `vis.Symbol` | [Your first extension](#your-first-extension) |
| Add a command that a person types, such as `/hello` | `vis.SlashCommand` | [Slash commands](extension-api.md#slash-commands) |
| Run your Python code on a schedule, without a model | `vis.Automation` | [Automations](extension-api.md#automations) |
| Explain when to use the tools of an extension | The short `prompt` of the extension | [Prompts and discovery](extension-api.md#prompts-and-discovery) |
| Describe a reusable procedure of many steps | A skill (`SKILL.md`), with no Python extension | [Skills](skills.md) |
| Check calls before they run, or inspect results afterward | `vis.OpHook` | [Op hooks](extension-api.md#op-hooks) |
| Start the processes of a tool with more environment variables | `vis.with_env` in a before `vis.OpHook` | [Environment variables](extension-api.md#add-environment-variables-to-a-tool-call) |
| Ask for input or show progress | `vis.ask` or `vis.live` inside a tool | [Forms](human-input.md) and [Live views](live-views.md) |
| Add an LLM service with custom authentication | `vis.Provider` | [Provider extensions](provider-extensions.md) |

A tool does work. A prompt or a skill explains when and how to use the tool. Instructions do not
register a callable, run a procedure, or authorize its side effects.

A before hook can refuse a call. An after hook inspects the outcome and can give context for the
next step, but it does not undo completed work.

## Your first extension

**Before you start.** Install Vis and open a project that you trust. The example uses only the
Python standard library and the SDK that Vis supplies. You need no pip install, uv, manifest, or
separate virtual environment.

**Review an extension before you load it.** An extension and its dependencies run with your user
permissions, outside the jail of the model. So do the extension files of a project that you check
out.

### 1. Create the entry file

In your project, create `.vis/extensions/` if needed. Save this complete file as
`.vis/extensions/greeting_tools.py`:

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

`name` identifies the extension. `Symbol(hello)` exposes the callable as `hello`. `alias` does not
add a prefix. Give an entry file a name that differs from each package that it imports.

Declare an Activity next to every tool binding. Write its label as clear English in sentence case,
not as a Python identifier or a serialized object.

The greeting is fast, so `show_start=False` shows only its end result. [Design the Activity with the
tool](extension-design.md#design-the-activity-with-the-tool) has the rules. [Activity
presentation](extension-api.md#activity-presentation) covers summaries, object methods, intermediate
updates, limits, and tests.

### 2. Load it

Start Vis in the project, or type `/reload` in an existing session. Reloaded tools become available
at the next turn boundary. If the load fails, run `vis-agent doctor` in your terminal and follow
[Extension troubleshooting](extension-troubleshooting.md).

### 3. Discover and call it

Ask Vis: "Use the hello tool to greet Ada, first normally and then in uppercase." The agent
inspects and calls the tool in `python_execution`:

```python
print(apropos(r"^hello$"))
print(doc("hello"))
print(await hello("Ada"))
print(await hello("Ada", uppercase=True))
```

The two calls print `Hello, Ada!` and `HELLO, ADA!`. `apropos()` searches public names and the
first sentence of each docstring. `doc()` shows the signature, the types, and the defaults with
your docstring, so the docstring does not repeat them. `hello.contract` gives the same data as a
structure.

A default shown as `...` is a withheld value, not `Ellipsis`. To use the real default, omit the
argument, as [Document default behavior](extension-design.md#document-default-behavior) explains.

### 4. Check an edit

1. Change the greeting text.
2. Run `/reload`.
3. On the next turn, ask for another greeting.
4. Check the result and `doc("hello")`.

If a reload fails, Vis keeps the last working version and marks it stale. Fix the error before you
test the edit. Registration or a successful reload does not prove that a tool call works.

## Ask Vis to build an extension

Describe the task, the inputs, the expected result, and the permitted side effects:

```text
Build a project-local Vis extension that lists this project's open GitHub issues.
Read doc("extending") first. Reuse the existing authenticated gh CLI.
Return typed issue summaries; do not create or modify issues.
Choose the smallest suitable layout. Test the implementation and a registered
call, explain omitted arguments, and tell me which reload steps remain.
Do not publish or install it globally.
```

Vis then works in these steps:

1. It checks whether an existing tool or skill does the task.
2. It chooses one file for a small integration, or a package for reusable or shared code.
3. It writes the tool with typed inputs and results, and a docstring that explains effects and
   failures.
4. It tests the Python code, then calls the registered tool in Vis.
5. It tells you what it could not test and which reload steps remain.

## Continue by task

| Task | Guide on the site | Guide through `doc(name)` |
| --- | --- | --- |
| Design useful tools, typed results, and tests | [Extension design](extension-design.md) | `doc("extension-design")` |
| Install, reload, remove, or share a package | [Installing and sharing extensions](extension-packages.md) | `doc("extension-packages")` |
| Connect an existing editable uv project | [Using an existing Python project](extension-development.md) | `doc("extension-development")` |
| Look up declarations, defaults, callbacks, and host operations | [Extension API](extension-api.md) | `doc("extension-api")` |
| Diagnose a missing tool, a stale result, or an import error | [Extension troubleshooting](extension-troubleshooting.md) | `doc("extension-troubleshooting")` |

## See also

- To go past the one-file example, read [Extension design](extension-design.md).
- For locations, dependencies, and distribution, read [Installing and sharing
  extensions](extension-packages.md).
