# Extension troubleshooting

If your extension does not load, a tool is missing, or an edit has no effect, run
`vis-agent doctor` in the terminal of your project. In most cases, its load message names the
stage that failed.

## When to use

- **Your tool is missing, or Vis never chooses it.** Read [Tool missing or not
  chosen](#tool-missing-or-not-chosen).
- **An edit has no effect**, and Vis still runs old code or shows old documentation. Read [Old
  code or documentation after an edit](#old-code-or-documentation-after-an-edit).
- **Loading fails.** The extension is [already registered](#already-registered), or [imports or
  dependency preparation fail](#imports-or-dependency-preparation-fail).
- **`doc()` hides a default or shows an incomplete type.** Read [Default hidden or type
  incomplete](#default-hidden-or-type-incomplete).
- **A package skill is missing.** Read [Package skill missing](#package-skill-missing).
- **The extension registers, but a call fails.** Read [Registration works but the call
  fails](#registration-works-but-the-call-fails).
- **A live view does not update.** Read [Live-view updates](#live-view-updates).

If Vis fails and not your extension, read [Reporting a bug](reporting-bugs.md).

## Tool missing or not chosen

1. Run `vis-agent extension list` and `vis-agent doctor` in the project where the tool must
   load. The entry must be in a [loaded location](extension-packages.md#where-extensions-load).
2. Run `/reload`, then look for the tool on the next turn. `Symbol.name` sets the public tool
   name. `Extension.alias` does not prefix it.
3. Search for the public name, for example `apropos(r"^greet\.")`.
   [Discovery](extension-api.md#prompts-and-discovery) explains what `apropos()` matches.
4. Check `is_hidden` and the `activation` callback of the extension.
5. If the agent sees the tool but does not choose it, name the tool in your request and read
   `doc(name)`. Improve its first-line summary and its [short
   prompt](extension-api.md#prompts-and-discovery). Do not add a second schema or a longer list
   of signatures.

A prompt is instruction text, and a skill describes a procedure. Neither runs code. To run
Python, register the operation with `Symbol`.

## Old code or documentation after an edit

Run `/reload`, call the tool on the next turn, and read the load result. After a [failed
reload](extension-packages.md#reload-update-or-remove), Vis keeps the last good code and marks
it **stale**. A successful retry clears the warning at the next turn boundary.

Check the origin of the import:

- An ordinary wheel needs a new install after each rebuild.
- Editable source and declared `source_paths` need a reload.
- A moved checkout or changed dependencies need
  [preparation](extension-development.md#prepare-the-project-environment) again.
- A call that is in flight may finish with the old imports.

A guide such as `doc("extending")` ships with the running Vis build. An edit to the website or
to a Markdown file does not change it, nor does `/reload`. If `doc()` differs from the website,
check the Vis version before you suspect your extension.

## Already registered

The error `vis.register_extension() may only be called once per file` names the extension that
registered first. Keep one registration, in the entrypoint. If the import of a tool causes the
error, look for a filename collision. For example, an entry named `demo.py` can shadow an
imported `demo` package. Rename the entry to `demo_tools.py`.

Do not import an entrypoint from domain code, ignore a second registration, or change `sys.path`
to hide a collision. Read [entrypoint design](extension-design.md#keep-the-entrypoint-small).

## Imports or dependency preparation fail

| Symptom | Check and next action |
| --- | --- |
| Circular import or partially initialized module | Give the entry a different filename from the package that it imports. |
| Missing package | Check the build backend, the dependency mode, and `package.__file__`. A project extension uses the environment that uv selects. |
| Missing or stale uv environment | Project admission and `/reload` prepare it. Read the uv diagnostic, fix the dependency or the lockfile, then retry. |
| Automatic package preparation fails | Read the uv phase and diagnostics. Vis bundles uv. If its executable is missing, install the runtime again. |
| PEP 723 dependency has no wheel | This mode installs only wheels. |
| Import works in a project environment but not in Vis | Point `tool.vis.project` at that project. Check that the interpreter is compatible, then reload. |
| Import works in the extension but not in the sandbox | Project dependencies belong to the environment of the extension, not to the shared sandbox packages. An installation does not widen [filesystem access](jail.md#filesystem-access). |

Do not combine package manifests, script dependencies, and `tool.vis.project` in one entry.
[Choose a layout](extension-packages.md#choose-a-layout), then follow the preparation steps of
that mode. A plain reload or import does not prepare a manual uv project.

## Default hidden or type incomplete

| What you see | Meaning and action |
| --- | --- |
| `parameter=...` in `doc()` | The parameter is optional. Omit it to get its real default, not `Ellipsis`. |
| No text on what an omitted argument does | Fix the docstring or the `Annotated` description of the tool. Document each public default. |
| A quoted string such as `'Results'` in `__annotations__`, or `NameError` from `get_type_hints()` | Read `.contract` for their fields, or pass `localns` to `get_type_hints()`. |
| `Name (unresolved)` | Keep result classes at module scope. Add `from __future__ import annotations` to each helper or package module that defines tools. Check the decorator metadata. |
| `Name (opaque)` | If callers need fields, use an annotated dataclass. |
| Missing docstring or invalid public name | Fix the callable that the error names. The declaration rejects it. |

Vis does not evaluate annotation expressions to find types. The [contract and introspection
rules](extension-api.md#tool-contracts) cover cross-module decorators and Python 3.14 deferred
annotations.

## Package skill missing

A declared skill path must contain `SKILL.md` inside the selected package. Find its qualified
name, for example with `apropos("vis-greeter/")`, and reload after each edit. [Bundled
skills](extension-packages.md#bundled-skills) lists what fails the load. Vis does not overwrite another skill.

A [local skill](skills.md#where-vis-looks) with the same qualified name wins.

## Registration works but the call fails

Read `doc(name)`, then call the tool. Dependencies and native libraries must work in the trusted
session worker. Success in the registration worker or in your development environment does not
prove that. Read the returned error and the [execution
boundary](extension-api.md#filesystem-and-processes).

[Forms](human-input.md) and [live views](live-views.md) need a calling session in Vis. Do not
open them from registration or from a passive provider callback. Test a cancellation, an
unavailable UI, and a successful response.

## Live-view updates

| Symptom | What to do |
| --- | --- |
| A status still shows its previous detail | Pass `detail=""` to `.set(...)` to clear it. If you omit `detail`, the old detail stays. |
| A layout builder raises `ValueError` for missing children | Declare at least one child in a row, column, or disclosure. Add the group when its first child is available. |
| A new node appears beside a group and not inside it | Use the id of a child in `view.add(node, after=...)`. `after` names a sibling, not a destination container. |

For examples, read [status updates](live-views.md#nodes) and [dynamic
layouts](live-views.md#layout-and-text).

## See also

- For load locations and dependency modes, read [Installing and sharing
  extensions](extension-packages.md).
- For project environments and editable imports, read [Using an existing Python
  project](extension-development.md).
- For exact declaration and callback contracts, read [Extension API](extension-api.md).
