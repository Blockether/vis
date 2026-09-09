# Extension design

Use one declaration for each callable, with structure derived from Python and
meaning written beside it. The [quickstart](extending.md) and this guide use the
same [tested package](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter).

## Keep the entrypoint small

Keep business logic in an ordinary importable package. `extension.py` imports that
package and calls `vis.register()` once. Do not import the entrypoint from domain
code, construct service clients with login side effects during registration, or
change `sys.path` yourself. Declare import roots in the [manifest](extension-packages.md).
A CLI, if useful, is another adapter to the same functions; tools return Python
values, not CLI output or JSON text for callers to parse.

## Describe structure once

- Annotate parameters and results. Use `from __future__ import annotations` for
  inert, inspectable annotations on every supported Python version.
- Use `Annotated[T, "meaning"]` for units, sentinel values and what `None` means.
- Put preconditions, side effects and failure conditions in the callable docstring.
  Its first line supplies the concise `apropos()` preview; do not repeat the signature.
- Return a frozen dataclass when fields need explanation. Describe each field with
  `Annotated`; use the class docstring for the result's overall meaning.
- Mark state-changing tools with `tag="mutation"`, or `@vis.method(tag="mutation")`
  on a namespace method. This describes the operation; it does not grant permission.

The example's `src/vis_greeter/__init__.py` has no Vis dependency:

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

        Does not send a message or modify stored state.
        """
        if not name.strip():
            raise ValueError("name must not be blank")
        text = f"Hello, {name.strip()}!"
        if uppercase:
            text = text.upper()
        return Greeting(text, len(text))
```

## One description, two readers

`doc("greet.hello")` renders the callable's docstring, parameter types and result
fields from the same description exposed as `greet.hello.contract` in the sandbox.
Outside Vis, inspect `vis.Symbol(Greeter(), name="greet").contract` without
registering it. Namespace descriptions contain full public member names.

`.contract` is the portable structured API; `doc()` renders it. Python-extension
callable proxies in the sandbox expose parameter names, kinds and redacted defaults
through `inspect.signature()`, but no parameter or return annotations.
`__annotations__` and `typing.get_type_hints()` return empty dictionaries;
`__signature__` is not supplied. `get_type_hints()` is therefore not a supported
way to discover extension types. The original host class identity does not cross
the boundary. Read parameter types, result types and record fields from `.contract`.

The description covers positional-only, positional-or-keyword, keyword-only,
`*args` and `**kwargs` parameters; requiredness; absence of a default versus a
`None` default; return types; dataclass fields; and observation/mutation tags.
Supported type structure includes unions, common containers, `Literal` and
`Annotated`. Variadic `tuple[T, ...]` is a generic tuple with `variadic: true` and
one entry in `arguments` for `T`; fixed-length tuples retain each item type and
omit `variadic`. An annotation that cannot be resolved safely renders as
`Name (unresolved)`; it is not guessed, evaluated or imported. A known class whose
structure is not described renders as `Name (opaque)`. These suffixes also appear
in parameter types, nested containers and record fields, not only top-level results.

Non-None default values and their `repr()` are never exported: they may contain
credentials. `has_default` and `default_is_none` distinguish no default, a `None`
default and another default. `doc()` renders other defaults as `...`;
`inspect.signature()` displays `Ellipsis`. A `None` default stays `None`. The
original defaults still apply at call time. Keep public annotations and docstrings
free of secrets.

Python 3.14's deferred annotation functions can execute code even when asked for
strings. Without `from __future__ import annotations`, these annotations are
reported as unresolved instead of evaluated. Local forward references that are
not available in the defining module also remain unresolved. Prefer module-level
result classes. Recursive records use references instead of infinitely expanding.
Cross-module `functools.wraps` decorators resolve annotations in the wrapped
function's defining module, including bound namespace methods and qualified names
such as `models.Result`. Wrapper chains are followed; cyclic `__wrapped__` chains
are rejected with `ValueError`. For example:

```python
# decorators.py
from functools import wraps


def traced(fn):
    @wraps(fn)
    def call(*args, **kwargs):
        return fn(*args, **kwargs)
    return call
```

```python
# tools.py
from __future__ import annotations
from dataclasses import dataclass
from decorators import traced


@dataclass(frozen=True)
class Result:
    text: str


class Tools:
    @traced
    def read(self, text: str = "ready") -> tuple[Result, ...]:
        """Read one result without changing state."""
        return (Result(text),)
```

`vis.Symbol(Tools(), name="tools").contract` expands `Result.text` beneath the
variadic tuple. The SDK regression suite executes these two snippets directly.
Host-to-sandbox coverage additionally checks nested records, invocation, redacted
defaults, introspection and refreshed metadata after reload.

This is a documentation contract, not JSON invocation or runtime type validation.
Python still binds arguments; your code validates domain constraints. There is no
manual signature/schema override that can disagree with the callable. The portable
shape is defined by the [symbol schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/symbol.json).

## Test both boundaries

From the copied `greeter/` directory, install the SDK and pytest in your development
environment, then run `python -m pytest tests`. The example's tests exercise the
same source shown above, including result immutability and invalid input.

Then install the package in Vis, reload and make a representative tool call. Check
`apropos(r"^greet\.")`, `doc(hit)` for a returned row, and `.contract` too. Search rows
carry the public name, kind and a short description; parameters and result fields
belong in the complete document and contract. Registration alone, including
`vis-agent extension list`, is not a tool execution test. Vis's own regression suite
loads this example through the real host, verifies discovery and the sandbox result,
and checks its bundled skill across reload and removal.

For dependencies installed into Vis, the spelling is `vis-agent python -m pytest`,
not `vis-agent python pytest`. See [package preparation](extension-packages.md).

## Skills describe procedures

Put multi-step usage in a bundled skill, with a description that says when to use
it. Link to `doc("greet.hello")` instead of copying the tool's API reference. Keep
references and templates beside `SKILL.md`; use them only when needed. A skill is
not an automatic startup hook or additional authorization.

## Developing outside Vis

The engine's `blockether.vis.extension` module is also published on PyPI as
[`vis-agent`](https://pypi.org/project/vis-agent/). Install it to import, test
and lint extensions outside Vis:

```bash
pip install vis-agent
```

Outside the engine, `vis.state`, `vis.log` and `vis.shell` use a local host
implementation, and `vis.ask` prompts in the terminal. Sandbox-only operations
return errors. Supply test answers with `vis.outside.answer_with({...})` or the
`VIS_OUTSIDE_ANSWERS` JSON variable. Set `VIS_OUTSIDE_NONINTERACTIVE=1` to make
input requests return an undeliverable status.

## See also

- [Extension API](extension-api.md).
- [Extension packages](extension-packages.md).
