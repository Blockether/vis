# Extension troubleshooting

Find the failing stage before changing code: loading, discovery, invocation or
reload. Run `vis-agent doctor` in the target project's terminal and read the
extension's load message. Fix the reported cause rather than masking registration
errors or changing global import ordering.

## Tool missing or not chosen

1. Check `vis-agent extension list` and `vis-agent doctor` in the intended project.
   Confirm the entry is in a [loaded location](extension-packages.md#where-extensions-load)
   and that a same-named project extension has not overridden the global one.
2. Run `/reload`, then check discovery on the next turn. `Symbol.name` determines
   the public tool name; `Extension.alias` does not prefix it.
3. Search the **public name**, for example `apropos(r"^greet\.")`, not a phrase from
   the description. `apropos()` does not search document bodies.
4. Check `is_hidden` and the extension's `activation` callback. If the tool is visible
   but the agent does not choose it, name it explicitly and inspect `doc(name)`.
   Improve its first-line summary and [short prompt](extension-api.md#prompts-and-discovery),
   not a duplicate schema or a longer list of signatures.

A prompt is instruction text, not a callable or startup hook. A skill describes a
procedure; installing or reading it does not run it. Register the operation with
`Symbol` when Python execution is needed.

## Old code or documentation after an edit

Use `/reload` and invoke the tool on the next turn. Check the load result: a failed
reload intentionally retains last-good code, contracts, docs and package skills as
**stale**. The reload result, doctor and assistant context report source fingerprints
and the failure. A successful retry clears the warning at the next turn boundary.

Check the import's origin. An ordinary wheel needs another install after rebuilding;
editable source and declared `source_paths` need reload. Moving a checkout or changing
its dependencies needs [preparation](extension-development.md#prepare-the-project-environment)
again. In-flight calls may finish using old imports.

The built-in guide returned by `doc("extending")` is bundled with the running Vis
build. Editing repository Markdown or publishing the website does not update an
already running gateway's bundled docs. `/reload` refreshes Python extensions,
not the gateway binary. Check which build is running before treating a source/site
and `doc()` difference as an extension reload failure.

## Already registered

`vis.register() may only be called once per file` identifies the previously
registered extension. Keep one registration in the entrypoint. If a tool import
triggers it, check for a filename collision: an entry named `demo.py` can shadow an
imported `demo` package. Rename the entry to `demo_tools.py`.

Do not import an entrypoint from domain code, ignore a second registration, or
alter `sys.path` to hide a collision. See [entrypoint design](extension-design.md#keep-the-entrypoint-small).

## Imports or dependency preparation fail

| Symptom | Check and next action |
| --- | --- |
| Circular import or partially initialized module | Give the entry a different filename from the package it imports |
| Missing package | Check the build backend, dependency mode and `package.__file__`; project extensions use the environment selected by uv |
| Missing/stale manual uv environment | Read the `uv sync --check` diagnostic, then sync followed by `/reload`, or use `/reload --sync` |
| Automatic package preparation fails | Read the reported uv phase and diagnostics. Vis bundles uv; reinstall the runtime if its executable is missing |
| PEP 723 dependency has no wheel | This mode installs wheels only; it does not fall back to a source build |
| Import works in a project environment but not Vis | Point `tool.vis.project` at that project; check interpreter compatibility and reload |
| Import works in the extension but not the sandbox | Project dependencies belong to the extension's environment, not shared sandbox packages; installation does not widen [filesystem access](jail.md#filesystem-access) |

Do not combine package manifests, script dependencies and `tool.vis.project` in one
entry. [Choose a layout](extension-packages.md#choose-a-layout), then follow that
mode's preparation steps. Plain reload and imports do not prepare manual uv projects.

## Default hidden or type incomplete

| What you see | Meaning and action |
| --- | --- |
| `parameter=...` in `doc()` | The parameter is optional; omit it to use its original default, not `Ellipsis` |
| No explanation of the omitted-argument behavior | Fix the tool's docstring or `Annotated` description; public defaults should be documented |
| Empty `__annotations__` or `get_type_hints()` on a sandbox proxy | Use `.contract`; these proxy attributes do not describe the host's types |
| `Name (unresolved)` | Keep result classes at module scope, use `from __future__ import annotations`, and check decorator metadata |
| `Name (opaque)` | The class is known but has no described structure; use an annotated dataclass when callers need fields |
| Missing docstring or invalid public name | Fix the named callable; declaration rejects it |

Vis does not evaluate annotation expressions to discover types. See the exact
[contract and introspection rules](extension-api.md#tool-contracts), including
cross-module decorators and Python 3.14 deferred annotations.

## Package skill missing

A declared skill path must contain `SKILL.md` inside the selected package. Discover
its qualified name, for example `apropos("vis-greeter/")` and
`doc("vis-greeter/greeting")`. Duplicate paths/names, invalid names and escaping
resources fail loading, rather than silently overwriting another skill.

After editing, reload. Last-good retention applies to skills and code together;
an ordinary local skill with the exact qualified name takes precedence. See
[bundled skills](extension-packages.md#bundled-skills).

## Registration works but the call fails

Test an actual invocation after checking `doc(name)`. Dependencies and native
libraries must work in the trusted session worker, not only the registration worker
or your development environment. Review the [execution boundary](extension-api.md#filesystem-and-processes)
and the returned error. Registration success alone is not an integration test.

Forms and live views need a calling session in Vis. Do not open them from registration
or passive provider callbacks. Test cancellation and an unavailable UI as well as a
successful response; see [forms](human-input.md) and [live views](live-views.md).

## See also

- [Installing and sharing extensions](extension-packages.md) — locations and dependency modes.
- [Using an existing Python project](extension-development.md) — explicit sync and editable imports.
- [Extension API](extension-api.md) — exact declaration and callback contracts.
