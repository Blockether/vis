# Installing and sharing extensions

Install an extension for one project or every project, reload changes, and package
code with optional skills for sharing. To write your first tool, start with
[Extending Vis](extending.md). Connecting an existing uv package has its own
[development workflow](extension-development.md).

## Choose a layout

| Situation | Layout | Dependency preparation |
| --- | --- | --- |
| Small local tool using the standard library | `.vis/extensions/greeting_tools.py` | None; the SDK is supplied by Vis |
| Local script with third-party wheels or source roots | [PEP 723 entry file](#standalone-scripts) | Automatic wheel installation at load/reload |
| Package to install or share, optionally with skills | [`pyproject.toml` and `extension.py`](#package-manifest) | Automatic uv preparation at load/reload |
| Existing uv project with an editable implementation | Entry file declaring `tool.vis.project` | Explicit sync; see [existing Python projects](extension-development.md) |

Choose one dependency mode per entrypoint. A package does not need a skill, a
provider does not need tools, and a small tool does not need a package.

## Where extensions load

| Directory | Scope |
| --- | --- |
| `~/.vis/extensions/` | Every project |
| `<project>/.vis/extensions/` | That project |

A project extension with the same registered name overrides the global extension.
Single `.py` files are entries; a package directory loads only `extension.py`.
Test files (`test_*.py` and `*_test.py`) are not extension entries.

**Trust boundary:** entrypoints and dependencies run with your user permissions,
not the model's jail. Review project extensions before starting Vis in an unfamiliar
checkout. `--trust` acknowledges this execution; validation is not a security review.

## Install a package

**Prerequisites:** Vis installed, reviewed source and dependencies, and `uv` on the
gateway's `PATH`. GitHub installs also need Git on `PATH`.

1. In the target project, link the local [greeter example](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter)
   after copying its complete directory to `greeter/`:

   ```bash
   vis-agent extension install ./greeter --project --trust
   ```

   The local checkout is linked, not copied. You may also pass its `pyproject.toml`.
   Omit `--project` only when you intend a global installation.

2. Start Vis there, or run `/reload`. Vis prepares dependencies before registration.
3. On the next turn, ask Vis to inspect `doc("greet.hello")` and call
   `await greet.hello("Ada")`. The result's `.text` is `Hello, Ada!`.
   `vis-agent extension list` checks registration, not execution.

### Install reviewed GitHub source

The [Extension Center](https://vis.blockether.com/extensions/) lists public GitHub
projects. After reviewing source and dependencies, copy its commit-pinned install
command. For your own repository, the command has this form:

```bash
vis-agent extension install https://github.com/example/vis-greeter --project --trust
vis-agent extension install https://github.com/example/extensions --subdirectory tools/greeting --project --trust
```

Replace the placeholder repository. These commands select its default branch; add
`--revision` with the reviewed full lowercase 40-character commit SHA to pin a
version. Only HTTPS `github.com/owner/repository` URLs are accepted. Pass a project
folder with `--subdirectory`, not as a GitHub file or tree URL.

GitHub installation stages a checkout and atomically installs only the selected
project. The catalog stores no source bundles and is not contacted during installation.
Submodules and Git LFS are not fetched; symlinks are refused. Keep required source
and portable dependency paths inside the selected project, within the limits of
4096 entries and 64 MiB.

## Reload, update or remove

| Change | Action |
| --- | --- |
| Edit an entry, helper module, declared source root or bundled skill | `/reload`; call the tool on the next turn |
| Change a package's dependencies | Deliberately update `uv.lock` if needed, then `/reload` |
| Change a manually prepared editable project's dependencies | Follow the [explicit sync workflow](extension-development.md#prepare-the-project-environment) |
| Replace an installed GitHub revision | Preserve any local work, remove the installed directory, install the reviewed revision, then `/reload` |
| Uninstall | Remove only the installed link or directory, then `/reload`; do not delete a linked development checkout |

Install never overwrites an existing destination. `/reload` does not fetch a newer
GitHub revision. It rebuilds extension contexts from installed source; already
running calls may finish with old code. Live sessions switch at the next turn boundary.

`vis.state` survives reload and restarts. A failed reload retains the last working
code, contracts, docs and package skills, marked stale with the failure reason and
loaded/requested source fingerprints. A successful retry clears the warning.
See [troubleshooting](extension-troubleshooting.md#old-code-or-documentation-after-an-edit).

Vis runs admitted copies of entries and declared source roots. Writes beside those
files affect a private copy, not durable project data; use `vis.state` for persistence.
Editable projects instead import their live checkout. Reload does not replace the
running gateway binary or its startup environment, and is not a native-library reload.

## Package manifest

A distributable package keeps `pyproject.toml` and `extension.py` together, at
repository root or in a selected subdirectory:

```text
greeter/
  pyproject.toml
  uv.lock
  extension.py
  src/vis_greeter/__init__.py
  tests/test_greeter.py
  skills/greeting/SKILL.md
  skills/greeting/references/style.md
```

The [tested example](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter)
uses this manifest. Generate and commit `uv.lock` before publishing:

```toml
[project]
name = "vis-greeter"
version = "1.0.0"
description = "Typed greeting tools and an optional greeting procedure."
requires-python = ">=3.11"
dependencies = ["vis-agent>=0.1.45"]

[tool.vis]
category = "tools"
source_paths = ["src"]
skills = ["skills/greeting"]

[tool.pytest.ini_options]
pythonpath = ["src"]
```

| Field | Requirement or behavior |
| --- | --- |
| `project.name` | The normalized package name must equal the registered extension name |
| `project.description`, `project.version` | Supply displayed package metadata |
| `project.requires-python` | Must allow Vis's embedded interpreter; Vis does not download another Python |
| `project.dependencies` | Must include an unconditional `vis-agent` requirement compatible with the running release |
| `tool.vis.category` | `providers`, `tools` or `workflows` |
| `tool.vis.source_paths` | Import roots inside the package, such as `src`, not the `vis_greeter` package directory |
| `tool.vis.skills` | Optional relative skill directories; omit when no procedure is needed |

Keep the implementation under the selected package directory. Do not put a PEP 723
block in this package's `extension.py`. See [Extension design](extension-design.md#keep-the-entrypoint-small)
for the complete registration and implementation.

At startup and `/reload`, Vis runs bundled upstream `uv sync` for these packages,
selecting the gateway's embedded Python with `--python`. uv manages the project's
lock and environment, including default dependency groups and removal of extraneous
packages. It can update an existing lock. Configure indexes through uv's own project
configuration, environment or CLI; `python.index_url` applies only to pip.

Each loaded project uses its own trusted worker and imports dependencies from its uv
environment, normally `.venv`. These dependencies are not installed into shared sandbox
packages. Source-only edits need reload. Build backends and executable `.pth` files are
trusted code. Imports in `python_execution` never install packages.

## Bundled skills

Declare up to 64 relative directories containing `SKILL.md` in `tool.vis.skills`.
Paths and resource symlinks must stay inside the package. Duplicate paths, duplicate
skill names and escaping resources fail loading. PEP 723 scripts do not declare skills.

A skill is discovered as `<normalized-project-name>/<skill-name>`, for example
`vis-greeter/greeting`. Its frontmatter name, or directory name when absent, uses
letters, digits, underscores and hyphens and starts with a letter or digit.
`doc()` identifies the package version and bundled resource directory.

Read it with `doc("vis-greeter/greeting")`, discover it with
`apropos("vis-greeter/")`, or request `/skill:vis-greeter/greeting`. An ordinary
skill with that exact qualified name takes precedence; otherwise different packages
do not collide. The procedure does not change the working project or widen filesystem
access. Installing, listing or reading it never executes its instructions.

Code, skills and resources share the admitted source snapshot and the same reload,
last-good retention and removal behavior. See [Skills](skills.md) for authoring and
precedence; put tool reference material in docstrings rather than duplicating it here.

## Standalone scripts

Keep a thin entry file under `.vis/extensions/` and the implementation in its
own source tree. Declare both import roots and dependencies in a
[PEP 723 script metadata block](https://peps.python.org/pep-0723/), before imports:

```text
project/
  vis.yml
  einmal/src/einmal/__init__.py
  .vis/extensions/einmal_tools.py
```

```python
# .vis/extensions/einmal_tools.py
# /// script
# requires-python = ">=3.11"
# dependencies = ["httpx==0.28.1"]
# [tool.vis]
# source_paths = ["../../einmal/src"]
# ///
import blockether.vis.extension as vis
from einmal import status

vis.register(vis.Extension(
    name="einmal",
    description="Company tools.",
    alias="einmal",
    symbols=[vis.Symbol(status)],
))
```

`status` must have a docstring, like any exported tool. The metadata is parsed
without executing the entry. Vis validates it, snapshots the source files,
installs dependencies, then evaluates the entry and registers its tools.

- `source_paths` names **import roots**: directories containing the packages or
  modules you import, not the package directories themselves. Relative paths are
  resolved against the extension entry's directory, not the current working
  directory. Absolute paths are also accepted.
- Each declared root's contents are merged into the frozen extension directory.
  Missing directories, duplicate relative file names and roots containing the
  extension directory are rejected. Use narrow source roots such as `src`, not
  the whole checkout or a virtual environment.
- `dependencies` accepts standard package requirements, including version pins,
  extras and environment markers. Vis uses its bundled **pip**, installing only
  wheels into `~/.vis/python/packages`, shared with `python_execution` and other
  extensions. It does not modify the project's `.venv`.
- Set the index with [`python.index_url` in `vis.yml`](configuration.md#python-package-index).
  Normal pip authentication and certificate settings still apply. A missing
  wheel or failed install is a load failure, not a fallback to source builds.
- `requires-python` checks the embedded interpreter. Vis does not download
  another Python version to satisfy it.
- `/reload` takes new source snapshots and resolves declared dependencies again;
  an unchanged loader scan does neither. Source edits are not used by existing
  tools until reload. A failed reload retains the last working extension.

Both embedded workers import shared installed dependencies; a tool call does not
install them again. Each process has its own module cache. After package changes,
reload to rebuild session workers; an already running call may keep its old imports.

## Publish a package

1. Verify the package tests and a registered tool call as described in
   [Extension design](extension-design.md#test-both-boundaries).
2. Commit the implementation, manifest, lockfile and any declared skills to a public
   GitHub repository. Keep credentials and private deployment details out of it.
3. In the Extension Center, choose **Add a repository** and enter its HTTPS URL.
   Leave **Project folder** empty for repository root, or provide the directory
   containing both `pyproject.toml` and `extension.py`.
4. Review the resolved commit and submit it for moderation.

The catalog reads metadata without executing project code. New entries and updates
stay private until approved; resubmission does not replace a published listing.
Different folders can have separate entries. Publishing is separate from local
installation and should only be done when requested.

## See also

- [Extension design](extension-design.md) — implementation and integration tests.
- [Using an existing Python project](extension-development.md) — manual uv preparation.
- [Extension troubleshooting](extension-troubleshooting.md) — loading, imports and stale tools.
