# Installing and sharing extensions

You can keep an extension in one project, install it for all your projects or
share it with other people. This guide covers those choices and how to reload
changes.

## When to use

- **An extension needs third-party dependencies or more than one file.** [Choose a
  layout](#choose-a-layout) and describe the package in a
  [manifest](#package-manifest).
- **You want an extension in every project, not only one.** Install it for all your projects. [Where
  extensions load](#where-extensions-load) explains how project and global copies combine.
- **Everyone on your team should get the same extensions.** [Save the installation
  in the project's configuration](#save-an-installation-in-configuration).
- **You want to install an extension someone else wrote.** Prefer an [approved
  release](#install-an-approved-github-release). Otherwise, pin the commit that you reviewed.
  [Install source that is not in the catalog](#install-source-that-is-not-in-the-catalog) shows how.
- **An update broke something.** [Roll back](#check-for-updates-and-roll-back) to
  the version that worked.
- **You want to share your extension.** [Publish it](#publish-a-package) through
  GitHub Releases.

For your first tool, start with [Extending Vis](extending.md). If you already have a
uv package, follow [Using an existing Python project](extension-development.md).

## Choose a layout

| Situation | Layout | Dependency preparation |
| --- | --- | --- |
| Small local tool using the standard library | `.vis/extensions/greeting_tools.py` | None (Vis supplies the SDK) |
| Local script with third-party wheels or source roots | [PEP 723 entry file](#standalone-scripts) | Automatic wheel installation at load/reload |
| Package to install or share, optionally with skills | [`pyproject.toml` and `extension.py`](#package-manifest) | Automatic uv preparation at load/reload |
| Existing uv project with an editable implementation | Entry file declaring `tool.vis.project` | Automatic preparation on project admission or reload. See [existing Python projects](extension-development.md) |

Choose one dependency mode per entrypoint. A package does not need a skill, a
provider does not need tools, and a small tool does not need a package.

## Where extensions load

| Directory | Scope |
| --- | --- |
| `~/.vis/extensions/` | Every project |
| `<project>/.vis/extensions/` | That project |

A project extension with the same registered name overrides the global extension.
Top-level `.py` files are entries. Installed packages live at
`.vis/extensions/<name>/<version>/`, with source files directly inside the version
directory. A `current` link inside `<name>/` selects the active version. Vis loads
only its `extension.py`, never every installed version. Test files (`test_*.py` and
`*_test.py`) are not extension entries.

For example, installing `vis-greeter` version `1.0.0` creates:

```text
.vis/extensions/
  vis-greeter/
    1.0.0/
      extension.py
      pyproject.toml
      src/
    current -> 1.0.0
```

GitHub versions contain copied source and an installation receipt. Local versions
link to your development checkout instead, so edits stay in that checkout. The
installer adds Git ignore rules for installed package directories and its private
metadata. Loose `.py` extensions remain visible to Git. Commit package declarations
in `vis.yml`, not downloaded sources or installation receipts.

**Review extensions before adding or opening a project.** Their entry files and
dependencies run with your user permissions, outside the model's jail. Adding a
project or opening its session authorizes Vis to install and load its configured
extensions. Review `vis.yml` and local overrides before opening an unfamiliar
checkout. CLI installation and sync commands require `--trust`. Validation is not
a security review.

## Declare packages in configuration

Use the same `extensions` map in project `vis.yml` and global `~/.vis/config.yml`
(or `~/.vis/vis.yml`). Keys are normalized package names from `pyproject.toml`:

```yaml
extensions:
  vis-tools:
    source: https://github.com/example/vis-tools
    subdirectory: extensions/vis-tools
    version: "0.1.0"
  vis-notes:
    source: https://github.com/example/vis-notes
    version: latest
  vis-greeter:
    source: ./tools/greeter
```

Replace the example repository and version with a reviewed, approved release. A declaration accepts
`source`, optional `subdirectory`, and either `version` or a full lowercase Git `revision`. A
`version` is an approved release such as `"0.1.0"`, or `latest` to track the newest approved stable
release.

The `source` can also carry the folder (`example/vis-tools/extensions/vis-tools`). Name the folder
only once. Selectors apply only to GitHub sources. Local paths are relative to the YAML file that
declares them, not to the shell's working directory.

When you add a folder in **Projects**, or open a session in that project, the gateway does these
steps:

1. It reads the merged configuration of the project.
2. It installs missing declared packages.
3. It prepares their Python dependencies.
4. It makes the tools available.

Preparation can need network access and can take longer on the first open. If installation or
loading fails, Vis reports the failure. It does not open a session with incomplete tools. Fix the
declaration or dependency problem and try again.

One gateway can serve several projects at once. Each session resolves tools,
providers, environment values, and toggles from its own project, not the directory
where the gateway started. Project extensions override same-name global extensions
only in that project. Opening another project does not replace those tools.
Package and declared uv project environments are separate per extension. PEP 723
wheels and shared sync installs go to `~/.vis/python/packages`, which every project
on the gateway shares.

Use `/reload` in a project session to adopt later configuration or source changes.
It reloads that project and shared global extensions, not other projects' local
catalogs. Reopening an already admitted project reuses its loaded code.

To preview or prepare packages without opening a session, use the CLI:

```bash
vis-agent extension sync --dry-run
vis-agent extension sync --trust
vis-agent extension sync --project --trust
vis-agent extension sync --global --trust
vis-agent extension sync --refresh --trust
```

By default, sync manages both scopes. Global declarations go to `~/.vis/extensions/`, and project
declarations go to `<project>/.vis/extensions/`. Local project overrides in `.vis/config.yml`
replace complete declarations by name, not individual fields. When both scopes use the same
extension name, the project registration wins.

Reading configuration alone never installs packages. Preparation happens when you add or open a
project, and when you reload explicitly. Standalone CLI sync requires `--trust`.

Sync installs missing packages, reconciles changed declarations and prepares their
`uv` environments without importing entrypoints or reloading a running gateway.
It reports `installed`, `updated`, `cached`, `orphaned` or `failed` per package.
A failure exits nonzero. Start Vis or use `/reload` to activate prepared code.

Without a selector, the first sync pins the latest approved stable release. An unchanged declaration
reuses that SHA without fetching Git or the catalog. Only `--refresh` checks for a newer approved
release. Explicit versions stay fixed, also when you request a refresh. Changing a version can
downgrade on purpose.

To track releases instead of pinning, use `version: latest`. Then every sync checks the catalog.
When the newest approved stable release changes, sync installs it without `--refresh`. It never
selects a prerelease. `sync --dry-run` reports it as `would-sync`, because a dry run makes no
network calls.

Dependencies use `uv sync --check --offline` first. They fall back to normal `uv sync` only when
preparation is needed. `uv` owns its lockfile, environment and download cache.

When you remove a declaration, sync keeps its installation as `orphaned`. Preview removals with
`sync --dry-run --prune`. Then use `sync --trust --prune` to unlink them. Sync can remove only
current links that it still owns. It keeps development checkouts and installed version directories.

Sync never adopts or replaces links that you installed manually or that changed outside sync. Keep
them and resolve the conflict before you sync. Do not edit the private `.sync.json` receipt. Do not
run a manual update or rollback on sync-owned packages. Change their declarations instead. A dry run
writes nothing and does not fetch sources, install dependencies or run extension code.

## Install a package

**Prerequisites:** an installed Vis, and source and dependencies that you reviewed. Vis supplies
`uv`. GitHub installs also need Git on `PATH`.

By default, `install` installs a package directly without changing configuration.
Add `--save` to also record it under `extensions:`. `--project` and `--global`
choose the scope. `--save` chooses whether to persist the declaration.

1. In the target project, link the local [greeter example](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter)
   after copying its complete directory to `greeter/`:

   ```bash
   vis-agent extension install ./greeter \
     --project \
     --trust
   ```

   The local checkout is linked, not copied. You may also pass its `pyproject.toml`.
   Prefix a relative checkout path with `./` when it could look like `owner/repository`.
   A bare repository slug always selects GitHub, even if a matching local directory exists.
   Choose `--project` for this project or `--global` for every project. Without either
   flag, installation is global, even when your current directory contains `vis.yml`.

2. Start Vis there, or run `/reload`. Vis prepares dependencies before registration.
3. On the next turn, ask Vis to inspect `doc("greet.hello")` and call
   `await greet.hello("Ada")`. The result's `.text` is `Hello, Ada!`.
   `vis-agent extension list` checks registration, not execution.

### Save an installation in configuration

Use `--project --save` to install a package and record it in your project's `vis.yml`
(or the existing `vis.yaml`):

```bash
vis-agent extension install example/greeting \
  --version 1.2.0 \
  --project \
  --save \
  --trust
```

The key is the package name from `pyproject.toml`. Approved GitHub releases save an exact version.
`--revision` saves the reviewed commit instead. Local sources save a path relative to the project
configuration.

With `--global --save`, Vis writes to its global machine store, `~/.vis/state.yml`. It does not
rewrite the global configuration that you wrote by hand. Without a scope flag, the scope is still
global.

Saved packages are managed by [`extension sync`](#declare-packages-in-configuration).
Commit the project declaration to share it with your team. To update a saved package,
edit its declaration and run `vis-agent extension sync --project --trust`. Do not
use manual `update` or `rollback` on a saved package.

Saving preserves existing project comments and unrelated settings. Conflicting
same-name declarations and configuration layouts that cannot be safely edited are
refused rather than overwritten. If saving fails after source admission, Vis rolls
back that installation. You can rerun the same installation with `--save` to record
an existing local link or managed GitHub snapshot after its source identity is
verified. Unrelated links, copied directories and different source revisions are
refused rather than adopted or replaced.

### Install an approved GitHub Release

The [Extension Center](https://vis.blockether.com/extensions/) lists approved releases
of public GitHub projects. **Publishing an extension on PyPI is not required.**
Review the selected version's source, manifest and dependencies before installing.
Choose where you want to use it:

**Project** — run this from the target project:

```bash
vis-agent extension install example/greeting \
  --version 1.2.0 \
  --project \
  --trust
```

**Global** — make the extension available in every project:

```bash
vis-agent extension install example/greeting \
  --version 1.2.0 \
  --global \
  --trust
```

For an extension inside a monorepo, add its project folder to the identifier:

```bash
vis-agent extension install example/extensions/tools/greeting \
  --version 1.2.0 \
  --project \
  --trust
```

Replace the example repository and version with an approved listing. In the Extension
Center, choose **Version** and the **Global** or **Project** scope, then copy the
install command. Older approved releases remain selectable and linkable. Use
`vis-agent extension versions example/greeting` to list them in your terminal.

Without `--version`, a GitHub install selects the **latest approved stable version**,
never the default branch. Prereleases require explicit `--version`. Unknown, pending
or rejected versions fail without falling back to another version, tag or branch.
The catalog resolves the version to its approved full SHA. Git fetches that commit
and the installer checks the manifest identity and runtime requirements again.
Moving or deleting a GitHub tag cannot change an approved version's SHA.

Use one of these to name the extension:

- The catalog identifier shown on the extension page: `owner/repository` or
  `owner/repository/folder`.
- The `github.com/owner/repository` HTTPS URL of the extension.
- The extension page URL itself.

Do not use a file or tree URL. Source that is not listed installs from [a commit you pin
yourself](#install-source-that-is-not-in-the-catalog).

GitHub installation stages and validates only the selected project before atomically
activating a source snapshot. The catalog stores no source distributions. Submodules
and Git LFS are not fetched. Symlinks are refused. Keep required source and portable
dependency paths inside the selected project, within 4096 entries and 64 MiB.

### Install source that is not in the catalog

A listing is not required. When an extension, or the version you need, is not in the
catalog, pin the exact commit you reviewed with `--revision` instead of `--version`:

```bash
vis-agent extension install example/greeting \
  --revision 8f4c1d2e5a9b70c3e61d84af2b5079cc31de6a04 \
  --subdirectory extensions/greeting \
  --project \
  --trust
```

Copy the SHA from the commit on GitHub, or read it with `git ls-remote`. It must be the
full, lowercase 40-character commit SHA. Branch and tag names are refused because they can
move. `--subdirectory` names the package folder in a repository, and `--save` records the
commit in your configuration for [`extension sync`](#declare-packages-in-configuration).

Vis fetches that commit, verifies it is the one you named and applies the same manifest and
runtime checks as a listed release. Only catalog moderation is skipped, so review the source
and its dependencies before you pass `--trust`.

`versions`, `update` and `rollback` work on approved releases and report none for a pinned
commit. Move to newer code by installing its SHA, or by editing `revision:` in your
declaration and running `vis-agent extension sync --trust`. The source must carry a new
`project.version`: an installed name and version cannot be rebound to another commit.

When the code is your own, a local checkout is simpler than a pinned commit.
`vis-agent extension install ./greeting --project --trust` links the directory. After `/reload`,
your edits are live.

### Check for updates and roll back

Use the GitHub `owner/repository` slug, such as `example/greeting`, not the Python
package name (`vis-greeter`). Repository names are case-insensitive. Project folders
keep their case. A full HTTPS GitHub repository URL also works.

```bash
vis-agent extension versions example/greeting --project
vis-agent extension update example/greeting --project --trust
vis-agent extension update example/greeting --version 1.3.0 --project --trust
vis-agent extension rollback example/greeting --project --trust
vis-agent extension rollback example/greeting --version 1.1.0 --project --trust
```

Vis finds the installed repository in the selected global or project scope. If you
installed several extensions from that repository, name the folder in the identifier to
select one. Vis refuses to guess. Use `--subdirectory .` to select its root project.
Before installation, `versions` needs the folder shown in the catalog, for example:

```bash
vis-agent extension versions example/extensions/tools/greeting
```

`versions` shows the installed version, approved history, latest stable version and whether an
update is available. `update` selects a newer approved stable release. `--version` explicitly
selects a newer release, including a prerelease. It never downgrades implicitly.

`rollback` restores the previous installed version, or an older approved version that you select
with `--version`. Installed versions are reused, including any local edits. A version that is not
installed is fetched and validated. A package name and version cannot be reused for a different Git
commit.

All source changes are explicit and require `--trust`. A failed fetch, compatibility
check or activation leaves the active installation unchanged. After success, start
Vis or use `/reload` to prepare dependencies and activate the code in running sessions.
Source rollback does not undo extension state, external side effects or dependency
changes already made by a build backend. Review release notes before changing versions.

The Python manifest name and normalized version determine the installation path.
The name is not the GitHub command identifier. There is no separate version store
or nested project directory. Updating changes the `current` link atomically and
keeps previous version directories available for rollback.

Do not edit receipts or delete versions you still need. Local development links and
unmanaged directories are never replaced by `update` or `rollback`. Use their source
workflow instead. Old installation layouts are not loaded or migrated. Preserve any
local edits before removing an old installation and installing it again.

## Publish a package

### Publish and maintain releases

There is no `vis-agent extension publish` command. Publication uses GitHub Releases
and catalog moderation. Local installation and activation are separate steps.

Extension Center displays each extension as lowercase `owner/repository`, using
GitHub's repository owner rather than a package author or submitter-provided name.
The project folder distinguishes packages in a monorepo. Use this repository identity
for `install`, `versions`, `update` and `rollback`. Keep `project.name` as the Python
distribution name for dependency metadata, internal storage and package-prefixed release tags.

1. Commit `pyproject.toml`, `extension.py`, required source and optional skills together
   in a public GitHub repository. Use a static `project.version`, and commit `uv.lock`
   for reproducible dependencies. Keep credentials and private deployment details out of it.
2. Test the package with the supported Vis and Python versions, including a
   [registered tool call](extension-design.md#test-both-boundaries). Bump the manifest
   version for each new release. Update the lockfile when needed.
3. Tag that commit and **publish a GitHub Release**, not only a Git tag or a draft.
   Use `v1.2.0`, or `PACKAGE-NAME/v1.2.0` for an independently versioned monorepo package.
   For example, an `extensions/vis-greeting` package can use `vis-greeting/v1.2.0`.
   The version suffix must exactly match `project.version`.
4. Submit the repository and project folder to Extension Center **once**. The optional
   **Release tag** field selects a monorepo release or prerelease. Otherwise review uses
   GitHub's latest stable release. Review the pinned metadata and submit for moderation.
5. After approval, publish later GitHub Releases in the same repository and folder. Scheduled
   discovery validates them and queues new versions for moderation. You do not submit another form.
   The current approved version stays available during review.
6. After approval, run `vis-agent extension versions` with the identifier from the extension page.
   Confirm the available version and its reviewed commit SHA.

Catalog releases use canonical `MAJOR.MINOR.PATCH`, optionally followed by `aN`, `bN`
or `rcN`, for example `1.3.0rc1`. Mark prereleases on GitHub too. Local package validation
still uses the SDK's PEP 440 rules. GitHub Release notes are the place for changelogs,
breaking changes and supported runtime versions. No release asset, wheel, PyPI account
or publisher credential in Vis is required. Declared dependencies may still use PyPI.

Discovery runs every five minutes. Each run checks one registered listing and one page of up to 20
releases. It inspects at most five new releases per run. Its cursor resumes within a page and
rotates across listings and older pages. So detection is not immediate, and it depends on catalog
size and GitHub availability.

Only approved versions are public. Vis keeps each rejection, so the same release does not go into
the queue again. A version's commit cannot be replaced. To fix a rejected or moved-tag release,
publish a **new version**. Never retag an approved version.

Listing checks metadata and required files. It is not a code audit or an endorsement.

Vis catalog maintainers can use the
[Publish reviewed extension workflow](https://github.com/Blockether/vis/actions/workflows/extension-publish.yml)
after reviewing the source and dependencies. It requires access to the Vis repository
and its `docs` environment. It is not an automatic approval path for publishers.
The [operator guide](https://github.com/Blockether/vis/blob/main/apps/vis-docs/README.md)
documents its inputs, authenticated moderation and verification.

Publishing or approving a release does not install it or update installed copies. Install or update
a managed package explicitly, or use a [configuration
declaration](#declare-packages-in-configuration). Project admission and `/reload` reconcile
declarations. Unchanged pinned declarations keep their installed revision, but `version: latest` can
fetch a newer approved release. For an existing managed installation, use [the update and rollback
commands](#check-for-updates-and-roll-back). Local source links follow their development workflow
instead.

## Reload, update or remove

| Change | Action |
| --- | --- |
| Edit an entry, helper module, declared source root or bundled skill | `/reload`, then call the tool on the next turn |
| Change dependencies in an existing package environment | Deliberately update `uv.lock` if needed, then `/reload` |
| Change dependencies used from shared packages | Run [shared sync](extension-development.md#install-a-project-into-shared-packages), then `/reload` |
| Change an editable project's dependencies | Update its dependency declaration and lockfile, then `/reload` |
| Change a managed GitHub version | Explicit `extension update` or `extension rollback`, then `/reload` |
| Uninstall | Remove the package’s `current` link, then `/reload`. Keep version directories or remove them separately. Do not delete a linked development checkout |

Install never overwrites an existing destination. `/reload` reconciles configured
packages and rebuilds extension contexts from the selected source. Unchanged pins
do not fetch a newer revision. Already running calls may finish with old code. Live
sessions switch at the next turn boundary.

`vis.state` survives reload and restarts. A failed reload keeps the last working code, contracts,
docs and package skills. It marks them stale with the failure reason and the loaded and requested
source fingerprints. A successful retry clears the warning. See
[troubleshooting](extension-troubleshooting.md#old-code-or-documentation-after-an-edit).

Vis runs admitted copies of entries and declared source roots. Writes beside those
files affect a private copy, not durable project data. Use `vis.state` for persistence.
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
keywords = ["greetings", "example"]
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
| `project.keywords` | Optional catalog tags. Supply at most two unique tags, each 1–24 lowercase letters or digits, with single hyphens between words |
| `project.requires-python` | Must allow Vis's embedded interpreter. Vis does not download another Python |
| `project.dependencies` | Must include an unconditional `vis-agent` requirement compatible with the running release |
| `tool.vis.category` | `providers`, `tools` or `workflows` |
| `tool.vis.source_paths` | Import roots inside the package, such as `src`, not the `vis_greeter` package directory |
| `tool.vis.skills` | Optional relative skill directories. Omit it when no procedure is needed |

Choose tags that describe what your extension helps people do, such as `browser` or `testing`.
Tags appear on catalog cards and extension pages, and you can search for them.
You can use your own tags. Tags do not grant the “Vis Official” badge.
Publish a new release to change an approved extension's tags.

Keep the implementation under the selected package directory. Do not put a PEP 723
block in this package's `extension.py`. See [Extension design](extension-design.md#keep-the-entrypoint-small)
for the complete registration and implementation.

Vis prepares the package's uv environment before it registers the package. It does this at these
times:

- At startup, for global packages.
- When you add or open a project, for the packages of that project.
- On `/reload`.

Vis reports a preparation failure. The package does not fall back to `~/.vis/python/packages`.

To prepare the environment, Vis runs bundled upstream `uv sync`. It selects the gateway's embedded
Python with `--python`. uv manages the project's lock and environment, including default dependency
groups and removal of extra packages. It can update an existing lock.

Vis's [`python.index_url`](configuration.md#python-package-index) supplies uv's default index,
unless the index environment of uv is already set. To override it, use uv's `--default-index`. uv
still manages named indexes and package source configuration.

A project with an environment uses its own trusted worker and imports dependencies
from that environment, normally `.venv`, without shared-package fallback. These
dependencies are not installed into shared sandbox packages. Source-only edits need
reload. Build backends and executable `.pth` files are trusted code. Imports in
`python_execution` never install packages.

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
skill with that exact qualified name takes precedence. Otherwise different packages
do not collide. The procedure does not change the working project or widen filesystem
access. Installing, listing or reading it never executes its instructions.

Code, skills and resources share the admitted source snapshot and the same reload,
last-good retention and removal behavior. See [Skills](skills.md) for authoring and
precedence. Put tool reference material in docstrings rather than duplicating it here.

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


vis.register_extension(vis.Extension(
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
- `dependencies` accepts standard package requirements, including version pins, extras and
  environment markers. Vis uses its bundled **pip** to install only wheels into
  `~/.vis/python/packages`. That directory is shared with `python_execution` and other extensions.
  Vis does not change the project's `.venv`.
- Set the index with [`python.index_url` in `vis.yml`](configuration.md#python-package-index).
  Normal pip authentication and certificate settings still apply. A missing
  wheel or failed install is a load failure, not a fallback to source builds.
- `requires-python` checks the embedded interpreter. Vis does not download
  another Python version to satisfy it.
- `/reload` takes new source snapshots and resolves declared dependencies again. An unchanged loader
  scan does neither. Existing tools use source edits only after a reload. A failed reload keeps the
  last working extension.

Both embedded workers import shared installed dependencies. A tool call does not
install them again. Each process has its own module cache. After package changes,
reload to rebuild session workers. An already running call may keep its old imports.

## See also

- [Extension design](extension-design.md) — implementation and integration tests.
- [Using an existing Python project](extension-development.md) — manual uv preparation.
- [Extension troubleshooting](extension-troubleshooting.md) — loading, imports and stale tools.
