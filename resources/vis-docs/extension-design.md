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
    activity=vis.Activity(
        label="Greet person", show_start=False, render=greeting_activity
    )
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

Activities are meant for human consumption. A person should understand what
happened without knowing a Python method name or result type. Use sentence-case
English labels such as "Greet person" or "Run tests", preserve proper names and
acronyms, and use consistent terminology without profanity or vulgarity. Show a
meaningful target, count or outcome in the summary. Put selected evidence behind
disclosure rather than serializing the returned object. Never change the case of
code, paths or returned content merely to format a label.

Choose whether a person needs to see the operation begin. Fast local reads,
patches and greetings use `show_start=False`: only the end result is visible.
The example uses this policy because generating a greeting is immediate. Slow
work, network requests and user input keep `show_start=True` and can publish
meaningful intermediate updates. The engine always tracks start/end internally;
hiding progress never hides failures, cancellation or the final result.

Test empty results, the chosen start visibility, success, failure and cancellation;
verify that presentation errors cannot change the tool result. The engine supplies
execution state and errors, but no generic result view. Follow the canonical
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

## Typed catalog and generated help

`vis.Catalog(symbols)` is an immutable-data adapter over the same `Symbol` contracts
used for registration. It is not a second registry. `spec()` returns typed top-level
entries; `spec("counter.write")` returns a `ToolSpec`; `help("counter.write")` returns
`HelpDocument(tool, text)`. A namespace's `members` retain full public names, including
nested capabilities. Hidden tools are excluded. A wrong name type raises `TypeError`;
an unknown or hidden name raises `ValueError`. Discovery does not validate configuration,
authenticate, create files or call the described tools.

This complete, tested entrypoint includes a read and a mutation. Both ordinary Python
callers and Vis invoke the same methods. An optional CLI should call those methods too,
not parse generated help. No CLI framework or inheritance is required.

```python
# counter.py
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Annotated

import blockether.vis.extension as vis


@dataclass(frozen=True, slots=True)
class Count:
    """The nonnegative count stored in a caller-owned file."""

    value: Annotated[int, "Number of completed items."]


class CounterError(RuntimeError):
    """The file could not be read or written; no success result is returned."""


def present_count(*, phase, result=None, error=None, **_):
    if phase == "failure":
        return vis.ActivityPresentation("Counter", str(error))
    if phase == "start":
        return vis.ActivityPresentation("Counter", "Accessing the count file")
    return vis.ActivityPresentation("Counter", f"{result.value} completed items")


class Counter:
    @vis.method(activity=vis.Activity(label="Read counter", show_start=False, render=present_count))
    def read(self, path: Annotated[str, "Caller-owned UTF-8 count file."]) -> Count:
        """Read a count; a missing file means zero. Does not create files.

        Raises TypeError for a non-string path, ValueError for a blank path,
        and CounterError for unreadable or invalid stored data.
        """
        self._validate_path(path)
        try:
            value = int(Path(path).read_text(encoding="utf-8"))
            if value < 0:
                raise ValueError("Stored count must be nonnegative")
            return Count(value)
        except FileNotFoundError:
            return Count(0)
        except (OSError, ValueError) as error:
            raise CounterError("Cannot read count") from error

    @vis.method(tag="mutation", activity=vis.Activity(label="Write counter", show_start=False, render=present_count))
    def write(self, path: str, *, value: Annotated[int, "Nonnegative completed-item count."]) -> Count:
        """Replace the caller-owned file's count; its parent directory must exist.

        Validate before IO: wrong types raise TypeError; blank paths and negative
        counts raise ValueError. Write failures raise CounterError. No retries.
        """
        self._validate_path(path)
        if type(value) is not int:
            raise TypeError("value must be an integer")
        if value < 0:
            raise ValueError("value must be nonnegative")
        try:
            Path(path).write_text(str(value) + "\n", encoding="utf-8")
        except OSError as error:
            raise CounterError("Cannot write count") from error
        return Count(value)

    def _validate_path(self, path):
        if not isinstance(path, str):
            raise TypeError("path must be a string")
        if not path.strip():
            raise ValueError("path must not be blank")


symbols = (vis.Symbol(Counter(), name="counter"),)
catalog = vis.Catalog(symbols)
vis.register(vis.Extension(
    name="counter-example", description="A counter with generated tool help.",
    alias="counter", symbols=(*symbols, vis.Symbol(catalog, name="doctor")),
))
```

The same callable metadata supplies structured values, generated help and Vis `doc()`.
`ParameterSpec` preserves parameter kind, requiredness, `has_default` and
`default_is_none`; it never contains the actual default value. `TypeSpec` recursively
describes result fields, containers, literals and `Annotated` meaning. Records and
nested collections returned by the catalog are frozen dataclasses and tuples. Rebuild
a catalog when declarations change; lookup does not inspect mutable runtime state.

In Vis, inspect `await doctor.spec("counter.write")` and
`await doctor.help("counter.write")`, then compare `doc("counter.write")`. The catalog
covers the `symbols` passed to it; the separate `doctor` discovery adapter is not itself
in that snapshot. Test the same scope against the actual registered public names:

```python
vis.testing.assert_catalog(
    catalog,
    names=["counter.read", "counter.write"],
    mutations=["counter.write"],
)
```

`assert_catalog` checks name and mutation parity, generated help and unresolved types,
including decorated methods and nested result fields. It allows explicitly opaque or
`Any` data; it is not a runtime type checker. Pass names obtained from registration or
`apropos()` in integration tests, rather than treating successful construction as proof.
Also invoke registered tools, test keyword-only binding, invalid input before IO,
operational exceptions and result immutability. The SDK suite executes the code above
and the real host verifies `spec`, `help`, `doc`, discovery and invocation together.

For long-running observations, use the [synchronous monitoring recipe](live-views.md#monitor-a-fixed-build-set)
and its cancellation tests. Catalog inspection must not start its readers. The catalog
regression also invokes that registered observation, cancels it and checks reader cleanup;
cancellation is never converted into a successful observation. Activity is the human
presentation, not a replacement for typed result data. Serialize only at a transport edge.

## See also

- [Extension API](extension-api.md) — exact declarations, contracts and callback rules.
- [Using an existing Python project](extension-development.md) — editable imports and dependency preparation.
- [Installing and sharing extensions](extension-packages.md) — the distributable package layout.
