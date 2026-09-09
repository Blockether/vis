# Extending Vis

Python extensions add tools, commands, guards and providers. A package can also
ship skills: optional procedures for using those tools. Start with the
[tested greeter package](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter), not a second tool registry.

## Your first extension

1. Copy the complete example directory into a checkout as `greeter/`. Review its
   code and dependencies. Install Vis and put `uv` on the gateway's `PATH`.
2. From that checkout, link the package into the current project:

   ```bash
   vis-agent extension install ./greeter --project --trust
   ```

3. Start Vis there, or run `/reload`. Dependency preparation happens before registration.
4. Ask for a greeting. In `python_execution`, inspect and call the tool:

   ```python
   hits = apropos(r"^greet\.")
   print(hits)
   print(doc(hits[0]))
   print(greet.hello.contract)
   result = await greet.hello("Ada", uppercase=True)
   print(result.text)
   ```

The result is `HELLO, ADA!`. Reading a contract does not call the tool.
The optional procedure is `doc("vis-greeter/greeting")`; installing or reading
it does not authorize executing its instructions.

The example's entire `extension.py` only connects ordinary Python code to Vis:

```python
"""Vis entrypoint; business logic lives in vis_greeter, not this file."""

import blockether.vis.extension as vis
from vis_greeter import Greeter

vis.register(
    vis.Extension(
        name="vis-greeter",
        description="Typed greeting tools and an optional greeting procedure.",
        alias="greet",
        symbols=[vis.Symbol(Greeter(), name="greet")],
    )
)
```

`alias` identifies the extension; it does not prefix tools. `Symbol(..., name="greet")`
creates the public namespace. Keep entrypoint filenames distinct from imported packages.

## Choose the next page

| Task | Canonical guide |
| --- | --- |
| Design tools, result types and tests | [Extension design](extension-design.md) |
| Dependencies, installation, publishing and bundled skills | [Extension packages](extension-packages.md) |
| Declaration and host API details | [Extension API](extension-api.md) |
| Diagnose import, reload or manifest errors | [Troubleshooting](extension-troubleshooting.md) |

These same pages are available through `doc("extension-design")`,
`doc("extension-packages")`, `doc("extension-api")` and
`doc("extension-troubleshooting")`. The SDK README links here rather than
maintaining another authoring guide.

## Where extensions load

| Directory | Scope |
| --- | --- |
| `~/.vis/extensions/` | Every project |
| `<project>/.vis/extensions/` | That project |

A project extension with the same registered name overrides the global extension.
Load failures appear in `vis-agent doctor`; a failed reload keeps the last working
version. Review project extensions before starting Vis in an unfamiliar checkout:
entrypoints and dependencies run with your user permissions, not the model's jail.

## See also

- [Extension design](extension-design.md).
- [Extension packages](extension-packages.md).
