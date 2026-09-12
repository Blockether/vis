# Installing and sharing extensions

You can keep an extension in one project, install it for all your projects or
share it with other people. This guide covers those choices and how to reload
changes. For your first tool, start with [Extending Vis](extending.md). If you
already have a uv package, follow [Using an existing Python project](extension-development.md).

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

**Review extensions before loading them.** Their entry files and dependencies
run with your user permissions, outside the model's jail. Check project
extensions before starting Vis in an unfamiliar checkout. `--trust` confirms
that you accept running the code; validation is not a security review.

## Declare packages in configuration

Use the same `extensions` map in project `vis.yml` and global `~/.vis/config.yml`
(or `~/.vis/vis.yml`). Keys are normalized package names from `pyproject.toml`:

```yaml
extensions:
  vis-tools:
    source: https://github.com/example/vis-tools
    subdirectory: extensions/vis-tools
    version: "0.1.0"
  vis-greeter:
    source: ./tools/greeter
```

Replace the example repository and version with a reviewed, approved release. A declaration accepts
`source`, optional `subdirectory`, and either `version` or a full lowercase Git
`revision`. Selectors apply only to GitHub sources. Local paths are relative to the
YAML file declaring them, not the shell's working directory.

```bash
vis-agent extension sync --dry-run
vis-agent extension sync --trust
vis-agent extension sync --project --trust
vis-agent extension sync --global --trust
vis-agent extension sync --refresh --trust
```

By default, sync manages both scopes: global declarations go to `~/.vis/extensions/`,
project declarations to `<project>/.vis/extensions/`. Local project overrides in
`.vis/config.yml` replace complete declarations by name, not individual fields.
The project registration wins when both scopes use the same extension name.
Reading configuration never installs packages; sync requires explicit `--trust`.

Sync installs missing packages, reconciles changed declarations and prepares their
`uv` environments without importing entrypoints or reloading a running gateway.
It reports `installed`, `updated`, `cached`, `orphaned` or `failed` per package;
a failure exits nonzero. Start Vis or use `/reload` to activate prepared code.

Without a selector, the first sync pins the latest approved stable release. An
unchanged declaration reuses that SHA without fetching Git or the catalog; only
`--refresh` checks for a newer approved release. Explicit versions stay fixed,
including when refresh is requested. Changing a version can intentionally downgrade.
Dependencies use `uv sync --check --offline` first, falling back to normal `uv sync`
only when preparation is needed. `uv` owns its lockfile, environment and download cache.

Removing a declaration retains its installation as `orphaned`. Preview removals with
`sync --dry-run --prune`, then explicitly use `sync --trust --prune` to unlink them.
Only links still owned by sync can be removed. Source checkouts and previous Git
snapshots are retained. Manually installed or externally changed links are never
adopted or replaced: preserve them and resolve the conflict before syncing.
Do not edit the private `.sync.json` receipt or run manual update/rollback on
sync-owned packages; change their declarations instead. Dry-run writes nothing
and does not fetch sources, install dependencies or run extension code.

## Install a package

**Prerequisites:** Vis installed and reviewed source and dependencies. Vis supplies
`uv`; GitHub installs also need Git on `PATH`.

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

### Install an approved GitHub Release

The [Extension Center](https://vis.blockether.com/extensions/) lists approved releases
of public GitHub projects. **Publishing an extension on PyPI is not required.**
Review the selected version's source, manifest and dependencies before installing:

```bash
vis-agent extension versions https://github.com/example/vis-greeter
vis-agent extension install https://github.com/example/vis-greeter --version 1.2.0 --project --trust
vis-agent extension install https://github.com/example/extensions --subdirectory tools/greeting --version 1.2.0 --project --trust
```

Replace the example repository and version with an approved listing. In the Extension
Center, choose **Version** to see that release's README, dependencies, full commit and
install command. Older approved releases remain selectable and linkable.

Without `--version`, a GitHub install selects the **latest approved stable version**,
never the default branch. Prereleases require explicit `--version`. Unknown, pending
or rejected versions fail without falling back to another version, tag or branch.
The catalog resolves the version to its approved full SHA; Git fetches that commit
and the installer checks the manifest identity and runtime requirements again.
Moving or deleting a GitHub tag cannot change an approved version's SHA.

For source outside the catalog, use `--revision` with a reviewed full lowercase
40-character commit SHA instead of `--version`. This deliberately bypasses catalog
approval, not trust or manifest checks. Only HTTPS `github.com/owner/repository` URLs
are accepted. Select a monorepo folder with `--subdirectory`, not a file or tree URL.

GitHub installation stages and validates only the selected project before atomically
activating a source snapshot. The catalog stores no source distributions. Submodules
and Git LFS are not fetched; symlinks are refused. Keep required source and portable
dependency paths inside the selected project, within 4096 entries and 64 MiB.

### Check for updates and roll back

Use the normalized installed package name, not the repository name:

```bash
vis-agent extension versions vis-greeter --project
vis-agent extension update vis-greeter --project --trust
vis-agent extension update vis-greeter --version 1.3.0 --project --trust
vis-agent extension rollback vis-greeter --project --trust
vis-agent extension rollback vis-greeter --version 1.1.0 --project --trust
```

`versions` shows the installed version, approved history, latest stable version and
whether an update is available. `update` selects a newer approved stable release;
`--version` explicitly selects a newer release, including a prerelease. It never
downgrades implicitly. `rollback` restores the previous installation's pinned source,
or an older approved version selected with `--version`. The previous-source form
needs GitHub but not the catalog; it fetches the saved SHA again, not the current tag.

All source changes are explicit and require `--trust`. A failed fetch, compatibility
check or activation leaves the active installation unchanged. After success, start
Vis or use `/reload` to prepare dependencies and activate the code in running sessions.
Source rollback does not undo extension state, external side effects or dependency
changes already made by a build backend. Review release notes before changing versions.

Managed GitHub installations use a link in `.vis/extensions/` and retain source
snapshots and receipts in its hidden `.versions/<package>/` directory. Previous
snapshots preserve local edits but are not used as release source when rolling back.
Do not edit receipts or delete snapshots you still need. Local development links and
unmanaged directories are never replaced by `update` or `rollback`; use their source
workflow instead. A pre-existing unmanaged GitHub copy must be preserved and removed
explicitly before installing it as a managed package.

## Publish a package

### Publish and maintain releases

There is no `vis-agent extension publish` command. Publication uses GitHub Releases
and catalog moderation; local installation and activation are separate steps.

Extension Center displays each extension as lowercase `owner/repository`, using
GitHub's repository owner rather than a package author or submitter-provided name.
The project folder distinguishes packages in a monorepo. This catalog name is
separate from `project.name`: keep the Python package name for installation,
`versions`, `update`, `rollback` and package-prefixed release tags.

1. Commit `pyproject.toml`, `extension.py`, required source and optional skills together
   in a public GitHub repository. Use a static `project.version`, and commit `uv.lock`
   for reproducible dependencies. Keep credentials and private deployment details out of it.
2. Test the package with the supported Vis and Python versions, including a
   [registered tool call](extension-design.md#test-both-boundaries). Bump the manifest
   version for each new release; update the lockfile when needed.
3. Tag that commit and **publish a GitHub Release**, not only a Git tag or a draft.
   Use `v1.2.0`, or `PACKAGE-NAME/v1.2.0` for an independently versioned monorepo package.
   For example, an `extensions/vis-greeting` package can use `vis-greeting/v1.2.0`.
   The version suffix must exactly match `project.version`.
4. Submit the repository and project folder to Extension Center **once**. The optional
   **Release tag** field selects a monorepo release or prerelease; otherwise review uses
   GitHub's latest stable release. Review the pinned metadata and submit for moderation.
5. After approval, publish subsequent GitHub Releases in the same repository and folder.
   Scheduled discovery validates them and queues new versions for moderation without
   another submission form. The current approved version stays available during review.
6. After approval, use `vis-agent extension versions` with the repository URL and optional
   `--subdirectory` to confirm the available version and its reviewed commit SHA.

Catalog releases use canonical `MAJOR.MINOR.PATCH`, optionally followed by `aN`, `bN`
or `rcN`, for example `1.3.0rc1`. Mark prereleases on GitHub too. Local package validation
still uses the SDK's PEP 440 rules. GitHub Release notes are the place for changelogs,
breaking changes and supported runtime versions. No release asset, wheel, PyPI account
or publisher credential in Vis is required; declared dependencies may still use PyPI.

Discovery runs every five minutes, checking one registered listing and one page of up
to 20 releases, with at most five new release inspections per tick. Its cursor resumes
within a page and rotates across listings and older pages, so detection is not immediate
and depends on catalog size and GitHub availability. Only approved
versions are public. Rejection is retained, so the same release is not repeatedly
queued. A version's commit cannot be replaced: fix a rejected or moved-tag release by
publishing a **new version**, never by retagging an approved version. Listing checks
metadata and required files; it is not a code audit or an endorsement.

Vis catalog maintainers can use the
[Publish reviewed extension workflow](https://github.com/Blockether/vis/actions/workflows/extension-publish.yml)
after reviewing the source and dependencies. It requires access to the Vis repository
and its `docs` environment; it is not an automatic approval path for publishers.
The [operator guide](https://github.com/Blockether/vis/blob/main/apps/vis-docs/README.md)
documents its inputs, authenticated moderation and verification.

Publishing or approving a release does not install it or update installed copies.
Users must explicitly install or update a managed package, or reconcile its
[configuration declaration](#declare-packages-in-configuration), then start Vis or use
`/reload`. Reload activates installed source; it does not fetch a newer release.
Use [the update and rollback commands](#check-for-updates-and-roll-back) for an existing
managed installation. Local source links follow their development workflow instead.

## Reload, update or remove

| Change | Action |
| --- | --- |
| Edit an entry, helper module, declared source root or bundled skill | `/reload`; call the tool on the next turn |
| Change a package's dependencies | Deliberately update `uv.lock` if needed, then `/reload` |
| Change a manually prepared editable project's dependencies | Follow the [explicit sync workflow](extension-development.md#prepare-the-project-environment) |
| Change a managed GitHub version | Explicit `extension update` or `extension rollback`, then `/reload` |
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
packages. It can update an existing lock. Vis's
[`python.index_url`](configuration.md#python-package-index) supplies uv's default
index unless its index environment is already set. Override it with uv's
`--default-index`; uv still manages named indexes and package source configuration.

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

def status_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return vis.ActivityPresentation("Check integration status", f"Status: {result[:160]}")


vis.register(vis.Extension(
    name="einmal",
    description="Company tools.",
    alias="einmal",
    symbols=[vis.Symbol(
        status, activity=vis.Activity(
            label="Check integration status", show_start=False, render=status_activity
        )
    )],
))
```

This example checks local status and uses `show_start=False`. A status check that
waits for a network response should keep `show_start=True`. Package layout does
not replace the obligation to give every tool a human-readable
[Activity presentation](extension-api.md#activity-presentation).

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

## See also

- [Extension design](extension-design.md) — implementation and integration tests.
- [Using an existing Python project](extension-development.md) — manual uv preparation.
- [Extension troubleshooting](extension-troubleshooting.md) — loading, imports and stale tools.
