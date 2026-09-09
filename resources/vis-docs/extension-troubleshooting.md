# Extension troubleshooting

Start with `vis-agent doctor` and the failed extension's load message. Do not mask
errors by retrying registration or changing global import ordering.

## Already registered

`vis.register() may only be called once per file` names the already registered
extension. Keep one registration in the entrypoint. If it occurs during a tool's
import, check whether the entrypoint shadows the imported package: rename
`demo.py` to `demo_bridge.py`, while keeping the public alias and tool names.
Do not import an entrypoint from domain code or ignore the second registration.
See the [entrypoint design](extension-design.md#keep-the-entrypoint-small).

## Imports and preparation

- **Circular import or partially initialized module:** name the entry differently
  from the package it imports, for example `einmal_tools.py`, not `einmal.py`.
  The extension directory is an import root and a same-named file can shadow the package.
- **Missing import:** check the build backend, editable source configuration and
  `package.__file__`. Sync must use the gateway's OS user, runtime and package directory.
- **Missing or stale Vis environment:** the message identifies changed readiness inputs
  or installed distributions. Review dependency changes and update the lock when needed,
  then use `/reload --sync` to prepare declared uv projects through the host, including
  when assistant shell access is disabled. Alternatively, run the printed sync command
  and `/reload`. Ordinary reload and imports never install manual project dependencies.
- **Import works in the CLI or extension but not the sandbox:** inspect
  [filesystem access](jail.md#filesystem-access) and the package's native operations.
  An editable install does not widen the sandbox policy.
- **Old tool result after an edit:** use `/reload` and invoke the tool on the next
  turn. Check whether the import came from the checkout, a frozen source snapshot
  or an ordinary installed wheel. Copied wheels need another install; editable
  Python source and declared `source_paths` need `/reload`. A failed reload explicitly
  marks retained tools and docs as stale, with loaded/requested source fingerprints in
  the reload result, doctor and assistant context. Resolve that failure before checking
  the new API; a successful retry clears the warning at the next turn boundary.

## Contract metadata

A missing docstring or invalid public name fails declaration with the callable's
name. Annotate keyword-only parameters just like positional parameters. Keep result
classes at module scope and add `from __future__ import annotations`; dynamic or
unresolvable annotations stay explicitly unresolved. Vis never evaluates an
annotation expression to discover a type. See [the contract rules](extension-design.md#one-description-two-readers).

## Package skills

A `skills` path must contain `SKILL.md` inside the selected package. Names become
`package/skill`; search that qualified name. Duplicate declared paths, duplicate
skill names, invalid names or escaping resources are errors, not silent overwrites.
After editing a skill, use `/reload`. A failed reload intentionally retains the
previous working skill together with its code. An ordinary local skill with the
same qualified name takes precedence. See [bundled skills](extension-packages.md#bundled-skills).

## Registration is not execution

After unit tests pass, check both `doc("greet.hello")` and an actual call. Dependencies
and native libraries must work in the trusted session worker, not just in the
registration worker or a development virtual environment. The [API boundary](extension-api.md#filesystem-and-processes)
explains which permissions and values cross into the model's sandbox.

## See also

- [Extension packages](extension-packages.md).
- [Extension API](extension-api.md).
