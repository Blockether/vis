# Extension troubleshooting

If your extension does not load, a tool is missing or an edit has not taken
effect, start with `vis-agent doctor` in your project's terminal. Its load message
usually identifies which stage failed. The sections below explain what to check
for each symptom.

## When to use

- **Your tool is missing, or Vis never chooses it.** See [Tool missing or not
  chosen](#tool-missing-or-not-chosen).
- **An edit has not taken effect**, and Vis still runs old code or shows old
  documentation. See [Old code or documentation after an
  edit](#old-code-or-documentation-after-an-edit).
- **Loading fails** because the extension is [already
  registered](#already-registered), or because [imports or dependency preparation
  fail](#imports-or-dependency-preparation-fail).
- **`doc()` hides a default or shows an incomplete type.** See [Default hidden or type
  incomplete](#default-hidden-or-type-incomplete).
- **A package skill is missing.** See [Package skill missing](#package-skill-missing).
- **The extension registers, but a call fails.** See [Registration works but the call
  fails](#registration-works-but-the-call-fails).
- **A live view does not update.** See [Live-view updates](#live-view-updates).

If Vis itself fails rather than your extension, see [Reporting a
bug](reporting-bugs.md).

## Tool missing or not chosen

1. Check `vis-agent extension list` and `vis-agent doctor` in the intended project.
   Confirm the entry is in a [loaded location](extension-packages.md#where-extensions-load)
   and that a same-named project extension has not overridden the global one.
2. Run `/reload`, then check discovery on the next turn. `Symbol.name` determines
   the public tool name. `Extension.alias` does not prefix it.
3. Search the **public name**, for example `apropos(r"^greet\.")`, not a phrase from
   the description. `apropos()` does not search document bodies.
4. Check `is_hidden` and the extension's `activation` callback. If the tool is visible
   but the agent does not choose it, name it explicitly and inspect `doc(name)`.
   Improve its first-line summary and [short prompt](extension-api.md#prompts-and-discovery),
   not a duplicate schema or a longer list of signatures.

A prompt is instruction text, not a callable or startup hook. A skill describes a
procedure. Installing or reading it does not run it. Register the operation with
`Symbol` when Python execution is needed.

## Old code or documentation after an edit

Use `/reload` and call the tool on the next turn. Check the load result. A failed reload keeps the
last good code, contracts, docs and package skills on purpose, and marks them **stale**. The reload
result, doctor and assistant context report source fingerprints and the failure. A successful retry
clears the warning at the next turn boundary.

Check the import's origin. An ordinary wheel needs another install after rebuilding.
Editable source and declared `source_paths` need reload. Moving a checkout or changing
its dependencies needs [preparation](extension-development.md#prepare-the-project-environment)
again. In-flight calls may finish using old imports.

The built-in guide returned by `doc("extending")` is bundled with the running Vis
build. Editing repository Markdown or publishing the website does not update an
already running gateway's bundled docs. `/reload` refreshes Python extensions,
not the gateway binary. Check which build is running before treating a source/site
and `doc()` difference as an extension reload failure.

## Already registered

The error `vis.register_extension() may only be called once per file` names the extension that
registered first. Keep one registration in the entrypoint. If a tool import causes this error, look
for a filename collision. For example, an entry named `demo.py` can shadow an imported `demo`
package. Rename the entry to `demo_tools.py`.

Do not import an entrypoint from domain code, ignore a second registration, or
alter `sys.path` to hide a collision. See [entrypoint design](extension-design.md#keep-the-entrypoint-small).

## Imports or dependency preparation fail

| Symptom | Check and next action |
| --- | --- |
| Circular import or partially initialized module | Give the entry a different filename from the package it imports |
| Missing package | Check the build backend, dependency mode and `package.__file__`. Project extensions use the environment that uv selects |
| Missing or stale uv environment | Project admission and `/reload` prepare it automatically. Read the reported uv diagnostic, fix the dependency or lockfile problem, then retry |
| Automatic package preparation fails | Read the reported uv phase and diagnostics. Vis bundles uv. If its executable is missing, reinstall the runtime |
| PEP 723 dependency has no wheel | This mode installs wheels only. It does not fall back to a source build |
| Import works in a project environment but not Vis | Point `tool.vis.project` at that project. Check interpreter compatibility, then reload |
| Import works in the extension but not the sandbox | Project dependencies belong to the extension's environment, not to shared sandbox packages. Installation does not widen [filesystem access](jail.md#filesystem-access) |

Do not combine package manifests, script dependencies and `tool.vis.project` in one
entry. [Choose a layout](extension-packages.md#choose-a-layout), then follow that
mode's preparation steps. Plain reload and imports do not prepare manual uv projects.

## Default hidden or type incomplete

| What you see | Meaning and action |
| --- | --- |
| `parameter=...` in `doc()` | The parameter is optional. Omit it to use its original default, not `Ellipsis` |
| No explanation of the omitted-argument behavior | Fix the tool's docstring or `Annotated` description. Document public defaults |
| A quoted string such as `'Results'` in `__annotations__`, or `NameError` from `get_type_hints()` | Record, opaque and unresolved types stay forward-reference strings on the proxy. Use `.contract` for their fields, or pass `localns` to `get_type_hints()` |
| `Name (unresolved)` | Keep result classes at module scope, add `from __future__ import annotations` to helper and package modules that define tools, and check decorator metadata |
| `Name (opaque)` | The class is known but has no described structure. When callers need fields, use an annotated dataclass |
| Missing docstring or invalid public name | Fix the named callable, because the declaration rejects it |

Vis does not evaluate annotation expressions to discover types. See the exact
[contract and introspection rules](extension-api.md#tool-contracts), including
cross-module decorators and Python 3.14 deferred annotations.

## Package skill missing

A declared skill path must contain `SKILL.md` inside the selected package. Discover
its qualified name, for example `apropos("vis-greeter/")` and
`doc("vis-greeter/greeting")`. Duplicate paths/names, invalid names and escaping
resources fail loading, rather than silently overwriting another skill.

After editing, reload. Last-good retention applies to skills and code together.
An ordinary local skill with the exact qualified name takes precedence. See
[bundled skills](extension-packages.md#bundled-skills).

## Registration works but the call fails

Test an actual invocation after checking `doc(name)`. Dependencies and native
libraries must work in the trusted session worker, not only the registration worker
or your development environment. Review the [execution boundary](extension-api.md#filesystem-and-processes)
and the returned error. Registration success alone is not an integration test.

Forms and live views need a calling session in Vis. Do not open them from registration
or passive provider callbacks. Test cancellation, an unavailable UI and a successful
response. See [forms](human-input.md) and [live views](live-views.md).

## Live-view updates

| Symptom | What to do |
| --- | --- |
| A status still shows its previous detail | Pass `detail=""` to `.set(...)` to clear it. If you omit `detail`, the old detail stays. |
| A layout builder raises `ValueError` for missing children | Declare at least one child in a row, column or disclosure. Add the group when its first child is available. |
| A new node appears beside a group instead of inside it | Use a child’s id in `view.add(node, after=...)`. `after` names a sibling, not a destination container. |

See [status updates](live-views.md#nodes) and
[dynamic layouts](live-views.md#layout-and-text) for examples.

## See also

- [Installing and sharing extensions](extension-packages.md) — locations and dependency modes.
- [Using an existing Python project](extension-development.md) — project environments and editable imports.
- [Extension API](extension-api.md) — exact declaration and callback contracts.
