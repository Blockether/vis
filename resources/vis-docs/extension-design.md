# Extension design

Design tools around tasks the agent needs to complete, then test their Python
behavior and their Vis integration. Start with the [one-file tutorial](extending.md);
use the [tested greeter package](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter)
when the implementation grows beyond a small entry file.

## Choose a useful tool boundary

Prefer one operation with a clear result over a sequence the agent must assemble
for every call. Return Python values, not printed CLI output or JSON text that
callers must parse. Use a scalar for a scalar result; use a frozen dataclass when
several fields have distinct meanings. Do not introduce a class just to wrap one string.

Keep reads separate from mutations. Mark state-changing tools with
`tag="mutation"`, or `@vis.method(tag="mutation")` on a namespace method. The tag
describes the operation; it does not grant permission or enforce a policy.
Validate domain constraints in the implementation and raise a useful exception
when they fail. Type annotations describe the API; they do not validate calls.

## Describe structure once

Use the signature for parameter names and kinds, annotations for types, and prose
for meaning. A callable's nonblank docstring becomes its `doc()` page. Its first
line supplies the short `apropos()` preview, so begin with what the tool does.
Search matches tool names, not that preview: choose names the agent can predict.

A useful tool description answers:

- When should I call this, and what must already be true?
- What does each input mean, including units, limits and omitted values?
- What does the result contain, and what do empty or missing values mean?
- Does the call change anything, ask the human, or access an external service?
- What failures should the caller handle?

Use `Annotated[T, "meaning"]` for parameter and result-field descriptions. Keep
preconditions, side effects and failure conditions in the docstring. Do not copy
the signature into prose or maintain a second schema. Use
`from __future__ import annotations` and module-level result classes for safely
inspectable annotations on supported Python versions.

The packaged example's `src/vis_greeter/__init__.py` is ordinary Python with no Vis dependency:

```python
"""Ordinary Python code, usable and testable without a Vis session."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Annotated


@dataclass(frozen=True)
class Greeting:
    """The generated greeting, without any external side effects."""

    text: Annotated[str, "Greeting text, ready to display."]
    characters: Annotated[int, "Number of Unicode code points in text, not bytes."]


class Greeter:
    def hello(
        self,
        name: Annotated[str, "Nonblank name of the recipient."],
        *,
        uppercase: bool = False,
    ) -> Greeting:
        """Greet one person. Requires a nonblank name; raises ValueError otherwise.

        uppercase defaults to False, preserving the recipient's capitalization.
        Does not send a message or modify stored state.
        """
        if not name.strip():
            raise ValueError("name must not be blank")
        text = f"Hello, {name.strip()}!"
        if uppercase:
            text = text.upper()
        return Greeting(text, len(text))
```

## Document default behavior

**Explain what happens when an argument is omitted.** A caller needs that behavior,
not merely a statement that the parameter is optional. Public constants belong
in the documentation: `uppercase` defaults to `False` in the example. Test the
omitted-argument call as well as an explicit override so the prose stays accurate.

For contextual defaults, name the resolution rule: for example, “Omitting `repo`
uses the current project's repository.” For `None`, say whether it means automatic
selection, no limit, or absence. Avoid vague phrases such as “uses the default.”

Vis does not automatically export non-`None` default values or call their `repr()`:
a host default can be a credential, client or other private object. This restriction
is not a ban on documenting known public defaults. Write their meaning in the
docstring or `Annotated` description; never copy a resolved credential or environment
value there. The [contract reference](extension-api.md#defaults-and-introspection)
explains `...`, `has_default` and sandbox introspection.

## Keep the entrypoint small

For reusable code, keep business logic in an importable package and registration
in `extension.py`. A CLI, if useful, is another caller of the same functions.
Do not import the entrypoint from domain code, change `sys.path`, or start login,
network requests or background work during registration.

The packaged example connects the implementation to Vis with this entire entrypoint:

```python
"""Vis entrypoint; business logic lives in vis_greeter, not this file."""

import blockether.vis.extension as vis
from vis_greeter import Greeter


def greeting_activity(*, phase, result, **_):
    """Show the greeting and its character count, not the result object's repr."""
    if phase != "success":
        return None
    return vis.ActivityPresentation(
        "Greet person",
        f"{result.characters} characters",
        (vis.ActivityText(result.text[:1000]),),
    )


Greeter.hello = vis.method(
    activity=vis.Activity(label="Greet person", render=greeting_activity)
)(Greeter.hello)

vis.register(
    vis.Extension(
        name="vis-greeter",
        description="Typed greeting tools and an optional greeting procedure.",
        alias="greet",
        symbols=[vis.Symbol(Greeter(), name="greet")],
    )
)
```

`Symbol(Greeter(), name="greet")` exports `greet.hello(...)`. The public namespace
comes from `Symbol.name`, not `Extension.alias`. Declare import roots in the
[package manifest](extension-packages.md#package-manifest), or use an
[editable project](extension-development.md); do not combine both import strategies
for the same source.

## Design the activity with the tool

Every registered callable owns its Activity presentation, including every public
method in an object namespace. Declare it at the binding, not as a later UI task.
The example decorates `Greeter.hello` in the entrypoint so its ordinary Python
implementation stays independent of Vis.

Use capitalized natural-language labels and headlines, such as "Greet person" or
"Run tests", with consistent terminology and no profanity or vulgarity. Show a
meaningful target, count or status in the summary. Put selected content behind
disclosure rather than serializing the returned object. The engine supplies
execution state and errors, but no generic result view. Test empty results,
running updates, success and failure; verify that presentation errors cannot
change the tool result. Follow the canonical
[Activity API](extension-api.md#activity-presentation) for declarations and limits.

## Give each kind of instruction one owner

| Information | Owner |
| --- | --- |
| A tool's inputs, defaults, result and failure conditions | Its annotations and docstring |
| When to use this extension and where to discover its tools | A short extension `prompt` |
| A multi-step procedure spanning tools | A skill with a clear trigger in its description |
| Project-wide rules | Project instructions, not every tool's prompt |

Link from a skill to `doc("greet.hello")` instead of copying the API reference.
Keep references and templates beside `SKILL.md` and read them when needed.
Installing or reading a skill never executes its procedure or supplies authorization.
See [prompts and discovery](extension-api.md#prompts-and-discovery) and
[bundled skills](extension-packages.md#bundled-skills).

## Test both boundaries

### Test the Python implementation

From a development environment containing the SDK and pytest, run the example's
tests from the copied `greeter/` directory:

```bash
python -m pip install vis-agent pytest
python -m pytest tests
```

Cover the normal result, omitted arguments, explicit overrides, invalid input and
side effects. The example's tests run without a gateway. Use your project's own
test environment; installing the SDK there does not install an extension into Vis.

Outside the engine, `vis.state`, `vis.log` and `vis.shell` use a local host, and
`vis.ask` prompts in the terminal. Supply answers with
`vis.outside.answer_with({...})` or `VIS_OUTSIDE_ANSWERS` JSON. Set
`VIS_OUTSIDE_NONINTERACTIVE=1` to make input requests undeliverable. Session-only
operations cannot be proved by an outside test. For views, use the existing
[LiveRecorder](live-views.md#testing-a-view).

### Test the registered tool

1. [Install the local package](extension-packages.md#install-a-package), then start
   Vis in the target project or run `/reload`.
2. On the next turn, inspect `apropos(r"^greet\.")`, `doc("greet.hello")` and
   `greet.hello.contract`. Confirm the inputs, public default behavior and result fields.
3. Call `await greet.hello("Ada")` and `await greet.hello("Ada", uppercase=True)`.
   Check the returned `.text`, not only registration or the extension list.
4. After an edit, reload and repeat a call. If the last working version was retained
   as stale, resolve the load failure before claiming that the edit works.

The repository's regression suite executes the documented package and loads it
through the real host, including discovery, sandbox results, skill reload and
removal. Your integration also needs a representative call against its own
required dependencies. For tests in Vis's shared environment, use
`vis-agent python -m pytest`, not `vis-agent python pytest`.

## See also

- [Extension API](extension-api.md) — exact declarations, contracts and callback rules.
- [Using an existing Python project](extension-development.md) — editable imports and dependency preparation.
- [Installing and sharing extensions](extension-packages.md) — the distributable package layout.
