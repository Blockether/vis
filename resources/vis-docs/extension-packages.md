# Installing and sharing extensions

Keep an extension in one project, install it for all your projects, or share it with other people.
This guide covers those choices and how to reload changes.

## When to use

- **An extension needs third-party dependencies or more than one file.** [Choose a
  layout](#choose-a-layout) and describe the package in a [manifest](#package-manifest).
- **You want an extension in every project.** Install it globally. [Where extensions
  load](#where-extensions-load) explains how project copies and global copies combine.
- **Everyone on your team must get the same extensions.** [Save the installation in the project
  configuration](#save-an-installation-in-configuration).
- **You want to install an extension that someone else wrote.** Prefer an [approved
  release](#install-an-approved-github-release). Otherwise, [pin the commit that you
  reviewed](#install-source-that-is-not-in-the-catalog).
- **An update broke something.** [Roll back](#check-for-updates-and-roll-back) to the version that
  worked.
- **You want to share your extension.** [Publish it](#publish-and-maintain-releases) through GitHub
  Releases.
- **You do not want your installs to count as downloads.** [Download counts](#download-counts)
  explains what Vis reports and how to turn it off.
- **Settings show a switch for a skill that your extension always needs.** [Leave the skill out of
  settings](#leave-a-skill-out-of-settings).
- **Your workspace holds several repositories, each with its own extensions.** [Load extensions
  from nested projects](#load-extensions-from-nested-projects).

For your first tool, start with [Extending Vis](extending.md). If you already have a uv package,
follow [Using an existing Python project](extension-development.md).

## Choose a layout

| Situation | Layout | Dependency preparation |
| --- | --- | --- |
| Small local tool that uses the standard library | `.vis/extensions/greeting_tools.py` | None. Vis supplies the SDK |
| Local script with third-party wheels or source roots | [PEP 723 entry file](#standalone-scripts) | Automatic wheel installation at load and reload |
| Package to install or share, with or without skills | [`pyproject.toml` and `extension.py`](#package-manifest) | Automatic uv preparation at load and reload |
| Existing uv project with an editable implementation | Entry file that declares `tool.vis.project` | Automatic preparation on project admission or reload. Read [Using an existing Python project](extension-development.md) |

Choose one dependency mode for each entrypoint.

## Where extensions load

| Directory | Scope |
| --- | --- |
| `~/.vis/extensions/` | Every project |
| `<project>/.vis/extensions/` | That project |

A project extension overrides the global extension with the same registered name, only in that
project. Each top-level `.py` file is an entry. An installed package is at
`.vis/extensions/<name>/<version>/`. A `current` link in `<name>/` selects the active version, and
Vis loads only the `extension.py` of that version. Test files (`test_*.py` and `*_test.py`) are not
entries.

For example, an installation of `vis-greeter` version `1.0.0` creates:

```text
.vis/extensions/
  vis-greeter/
    1.0.0/
      extension.py
      pyproject.toml
      src/
    current -> 1.0.0
```

A GitHub version holds copied source and an installation receipt. A local version links to your
development checkout. The installer adds Git ignore rules for
installed package directories and its private metadata. Loose `.py` extensions stay visible to Git.
Commit package declarations in `vis.yml`, not downloaded sources or installation receipts.

**Review extensions before you add or open a project.** Entry files and their dependencies run with
your user permissions, outside the jail of the model. When you add a project or open its session,
you authorize Vis to install and load its configured extensions. Review `vis.yml` and local
overrides before you open an unfamiliar checkout. The CLI installation and sync commands require
`--trust`. Validation is not a security review.

## Declare packages in configuration

Use the same `extensions` map in the project `vis.yml` and in the global `~/.vis/config.yml` or
`~/.vis/vis.yml`. Each key is a normalized package name from `pyproject.toml`:

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

Replace the example repository and version with a reviewed, approved release. A declaration takes
`source`, an optional `subdirectory`, and either `version` or `revision`:

- `version` is an approved release such as `"0.1.0"`, or `latest` to track the newest approved
  stable release.
- `revision` is a full lowercase Git commit SHA.
- `source` can also carry the folder, as in `example/vis-tools/extensions/vis-tools`. Name the
  folder only once.
- Selectors apply only to GitHub sources.
- A local path is relative to the YAML file that declares it, not to the working directory of the
  shell.

Vis installs a declared package only when you add or open a project, reload, or run sync.

### What Vis does when you open a project

When you add a folder in **Projects**, or open a session in that project, the gateway does these
steps:

1. It reads the merged configuration of the project.
2. It installs missing declared packages.
3. It prepares their Python dependencies.
4. It makes the tools available.

Preparation can need network access and can take longer on the first open. If installation or
loading fails, Vis reports the failure and does not open a session with incomplete tools. Fix the
declaration or the dependency problem and try again.

One gateway can serve several projects at once. Each session resolves tools, providers, environment
values, and toggles from its own project, not from the directory where the gateway started.

Each extension has its own package environment or declared uv project environment. PEP 723 wheels
and shared sync installs go to `~/.vis/python/packages`, which every project on the gateway shares.

To adopt later changes to configuration or source, run `/reload` in a project session. It reloads
that project and the shared global extensions, not the local catalogs of other projects.

### Sync packages from the command line

To preview or prepare packages without a session, use the CLI:

```bash
vis-agent extension sync --dry-run
vis-agent extension sync --trust
vis-agent extension sync --project --trust
vis-agent extension sync --global --trust
vis-agent extension sync --refresh --trust
```

Sync works as follows:

- By default, sync manages both scopes. Global declarations go to `~/.vis/extensions/`, and project
  declarations go to `<project>/.vis/extensions/`.
- A local project override in `.vis/config.yml` replaces a complete declaration by name, not single
  fields.
- Sync installs missing packages, reconciles changed declarations, and prepares their `uv`
  environments. It imports no entrypoint and does not reload a running gateway. To activate the
  prepared code, start Vis or run `/reload`.
- Sync reports `installed`, `updated`, `cached`, `orphaned`, or `failed` for each package. A failure
  exits nonzero.
- A dry run writes nothing. It does not fetch sources, install dependencies, or run extension code.

Dependencies use `uv sync --check --offline` first, and a normal `uv sync` only when preparation is
needed. [Reload after changes](extension-development.md#reload-after-changes) says when Vis skips
uv.

### Which version sync installs

- **No selector.** The first sync pins the latest approved stable release. An unchanged declaration
  reuses that SHA and fetches neither Git nor the catalog. Only `--refresh` checks for a newer
  approved release.
- **An explicit version.** The version stays fixed, also with `--refresh`. A changed version can
  downgrade on purpose.
- **`version: latest`.** Every sync checks the catalog and installs a new approved stable release
  without `--refresh`. It never selects a prerelease. `sync --dry-run` reports `would-sync`, because
  a dry run makes no network calls. A start of Vis and `/reload` keep the installed release and do
  not check the catalog.

### Remove packages and old versions

When you remove a declaration, sync keeps its installation as `orphaned`. Preview removals with
`sync --dry-run --prune`, then unlink them with `sync --trust --prune`. Sync removes only current
links that it still owns. It never removes a development checkout.

Each GitHub update installs the new version in a new directory with its own environment, and the
old directory stays. `--prune` also removes these old versions. It keeps the active version and one
other version, which is the rollback version or else the newest one. It also keeps each version
that a running Vis process still uses. The output shows each removed version as `pruned`, and a dry
run shows it as `would-prune`.

Sync never adopts or replaces a link that you installed manually or that changed outside sync. Keep
the link and resolve the conflict before you sync. Do not edit the private `.sync.json` receipt.

When you copy or move a project, sync reuses the unchanged GitHub versions in its `.vis/extensions`
directory. If sync refuses a package, the error names the package directory. Keep your local edits
from that directory, remove it, and run sync again.

If the version directory of an installed GitHub package is gone, sync installs the package again.
If the folder of a local `source` is gone, the error names that folder. Restore the folder or
change `source`, then run sync again.

## Load extensions from nested projects

A workspace can hold several repositories, and each can keep its own extensions in
`<repository>/.vis/extensions/`. Vis does not search those directories. Declare them in the
workspace `vis.yml` with a glob `source`:

```yaml
extensions:
  repositories:
    source: ./repositories/*/.vis/extensions/*.py
  local-packages:
    source: ./packages/*
```

Vis expands each glob at each load and each `/reload`. The key, such as
`repositories`, only names the declaration in error reports. The glob gives these results:

- A matched `.py` file loads as a single-file extension. Test files are not entries.
- A matched directory loads its `extension.py`, or its `current/extension.py`.

A glob is relative to the YAML file that declares it. Each `*`, `?`, or `[...]` matches inside one
directory level, and Vis refuses `**`. A pattern matches a hidden name only when its own level
starts with `.`, as `.vis` does above. A glob declaration accepts only `source`. Vis loads matches
in place and never installs them, so `extension sync` ignores them.

A file that fails to load, or a glob that Vis refuses, appears in the load report and in
`vis-agent doctor`. The other matches still load. Each extension needs its own
registered name. If a matched file uses the name of a loaded extension, the matched file fails with
an error that names both files.

Each extension also gets its own project root, the nearest directory above its file that holds a
`.vis` directory. In the example, that directory is `repositories/<name>`. Extension code reads it
with [`vis.project_root()`](extension-api.md#project-root). So the same file works in the workspace
and in a session that opens the repository directly.

When a nested extension is active, the project rules of the session list the `AGENTS.md` or
`CLAUDE.md` file of its project. The model reads that file before it uses the tools of the project
or works in the project. Vis does not copy the file into each request.

## Install a package

**Prerequisites.** You need Vis, and source and dependencies that you reviewed. Vis supplies `uv`.
A GitHub install also needs Git on `PATH`.

`install` does not change configuration unless you add `--save`, which records the package under
`extensions:`. `--project` installs for this project, and `--global` for every project. Without
either flag, the installation is global, even when the current directory contains `vis.yml`.

1. Copy the complete directory of the [greeter
   example](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter) to
   `greeter/` in the target project, then link it:

   ```bash
   vis-agent extension install ./greeter \
     --project \
     --trust
   ```

   You can also pass its `pyproject.toml`.
   Prefix a relative checkout path with `./` when it can look like `owner/repository`. A bare
   repository slug always selects GitHub, even if a local directory has the same name.

2. Start Vis there, or run `/reload`.
3. On the next turn, ask Vis to inspect `doc("greet.hello")` and call `await greet.hello("Ada")`.
   The `.text` of the result is `Hello, Ada!`. `vis-agent extension list` checks registration, not
   execution.

### Save an installation in configuration

To install a package and record it in the `vis.yml` of your project, or in the existing `vis.yaml`,
use `--project --save`:

```bash
vis-agent extension install example/greeting \
  --version 1.2.0 \
  --project \
  --save \
  --trust
```

An approved GitHub release saves an exact version, and `--revision` saves the reviewed commit. A
local source saves a path relative to the project configuration.

With `--global --save`, Vis writes to its global machine store, `~/.vis/state.yml`. It does not
rewrite the global configuration that you wrote by hand.

[`extension sync`](#declare-packages-in-configuration) manages saved packages, so do not use manual
`update` or `rollback` on them. Commit the project declaration to share it with your team. To
update a saved package, edit its declaration and run `vis-agent extension sync --project --trust`.

A save keeps your comments and other settings. Vis never overwrites a different declaration with
the same name. If the save fails, Vis also removes the new installation. To record a package that
you already installed, run the same install command again with `--save`.

### Install an approved GitHub Release

The [Extension Center](https://vis.blockether.com/extensions/) lists approved releases of public
GitHub projects. Before you install, review the source, the manifest, and the dependencies of the
selected version.

For one project, run this command from that project:

```bash
vis-agent extension install example/greeting \
  --version 1.2.0 \
  --project \
  --trust
```

For every project:

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

In the Extension Center, choose **Version** and the **Global** or **Project** scope, then copy the
install command. Older
approved releases stay selectable and linkable. `vis-agent extension versions example/greeting`
lists them in your terminal.

Vis selects the version as follows:

- Without `--version`, a GitHub install selects the latest approved stable version, never the
  default branch.
- A prerelease requires an explicit `--version`.
- An unknown, pending, or rejected version fails. Vis does not fall back to another version, tag,
  or branch.
- The catalog resolves the version to its approved full SHA. A moved or deleted GitHub tag cannot
  change that SHA.
- The installer checks the manifest identity and the runtime requirements again.

Name the extension in one of these ways:

- The catalog identifier on the extension page, `owner/repository` or `owner/repository/folder`.
- The `github.com/owner/repository` HTTPS URL of the extension.
- The URL of the extension page.

Do not use a file URL or a tree URL. Source that is not listed installs from [a commit that you
pin](#install-source-that-is-not-in-the-catalog).

A GitHub installation stages and validates only the selected project, then activates a source
snapshot atomically. Vis does not fetch submodules or Git
LFS, and it refuses symlinks. Keep required source and portable dependency paths inside the selected
project, within 4096 entries and 64 MiB.

### Install source that is not in the catalog

When the catalog does not list an extension, or the version that you need,
pin the commit that you reviewed with `--revision`:

```bash
vis-agent extension install example/greeting \
  --revision 8f4c1d2e5a9b70c3e61d84af2b5079cc31de6a04 \
  --subdirectory extensions/greeting \
  --project \
  --trust
```

Copy the SHA from the commit on GitHub, or read it with `git ls-remote`. It must be the full,
lowercase 40-character commit SHA. Vis refuses branch and tag names.
`--subdirectory` names the package folder in a repository. `--save` records the commit in your
configuration for [`extension sync`](#declare-packages-in-configuration).

Vis fetches that commit, verifies its SHA, and applies the manifest and runtime checks of a listed
release. Only catalog moderation is skipped, so review the source and its dependencies before you
pass `--trust`.

`versions`, `update`, and `rollback` work on approved releases and report none for a pinned commit.
To move to newer code, install its SHA, or edit `revision:` in your declaration and run
`vis-agent extension sync --trust`. The source must carry a new `project.version`, because an
installed name and version cannot bind to another commit.

For your own code, link a local checkout with
`vis-agent extension install ./greeting --project --trust`. After `/reload`, your edits are live.

### Check for updates and roll back

Use the GitHub `owner/repository` slug, such as `example/greeting`, not the Python package name,
such as `vis-greeter`. Repository names ignore case, and project folders keep their case. A full
HTTPS GitHub repository URL also works.

```bash
vis-agent extension versions example/greeting --project
vis-agent extension update example/greeting --project --trust
vis-agent extension update example/greeting --version 1.3.0 --project --trust
vis-agent extension rollback example/greeting --project --trust
vis-agent extension rollback example/greeting --version 1.1.0 --project --trust
```

Vis finds the installed repository in the selected global or project scope. If you installed
several extensions from that repository, name the folder in the identifier to select one.
`--subdirectory .` selects the root project. Before installation, `versions` needs the folder that
the catalog shows, for example:

```bash
vis-agent extension versions example/extensions/tools/greeting
```

| Command | Behavior |
| --- | --- |
| `versions` | Shows the installed version, the approved history, the latest stable version, and whether an update is available |
| `update` | Selects a newer approved stable release. `--version` selects a named newer release, prereleases included. `update` never downgrades |
| `rollback` | Restores the previous installed version, or an older approved version that you select with `--version` |

A rollback reuses an installed version with its local edits. Vis fetches and validates a version
that is not installed.

An explicit update or rollback keeps the synchronization record of the package consistent with its
active version. If configuration pins a version or revision, change that declaration to keep a
different version after synchronization.

Every source change is explicit and requires `--trust`. A failed fetch, compatibility check, or
activation leaves the active installation unchanged. After success, start Vis or run `/reload` to
prepare dependencies and activate the code in running sessions. A source rollback does not undo
extension state, external side effects, or dependency changes that a build backend already made.
Review the release notes before you change versions.

Do not edit receipts or delete versions that you still need. `update` and `rollback` never replace
a local development link or an unmanaged directory. Use the source workflow of that directory. Vis
does not load or migrate old installation layouts. Save your local edits before you remove an old
installation and install it again.

### Download counts

The Extension Center shows how many times each extension was downloaded. Vis reports one download
after it fetches an approved release to install, update, roll back, or sync an extension. The report
contains only the catalog identifier of the extension. A failed report does not affect the
installation.

The Extension Center counts each network address one time for each extension in one UTC day. It
does not store the address. It keeps a private code for that day only, to prevent repeated counts.
Vis does not report a version that is already on your computer, a pinned commit, or a local folder.

To stop these reports, set `DO_NOT_TRACK=1` in your environment:

```bash
DO_NOT_TRACK=1 vis-agent extension install example/greeting --project --trust
```

## Publish and maintain releases

Vis has no `vis-agent extension publish` command. Publication uses GitHub Releases and catalog
moderation.

The Extension Center shows each extension as lowercase `owner/repository`. The owner is the
repository owner on GitHub, not a package author or a name that the submitter gives. The project
folder distinguishes the packages of a monorepo. Use this repository identity for `install`,
`versions`, `update`, and `rollback`. Keep `project.name` as the Python distribution name for
dependency metadata, internal storage, and release tags with a package prefix.

1. Commit `pyproject.toml`, `extension.py`, the required source, and optional skills together in a
   public GitHub repository. Use a static `project.version`, and commit `uv.lock` for reproducible
   dependencies. Keep credentials and private deployment details out of the repository.
2. Test the package with the supported Vis and Python versions, with a [registered tool
   call](extension-design.md#test-both-boundaries) included. Raise the manifest version for each
   new release. Update the lockfile when needed.
3. Tag that commit and publish a GitHub Release. A Git tag alone or a draft is not enough. Use
   `v1.2.0`, or `PACKAGE-NAME/v1.2.0` for a monorepo package with its own versions. For example, an
   `extensions/vis-greeting` package can use `vis-greeting/v1.2.0`. The version suffix must match
   `project.version` exactly.
4. Submit the repository and the project folder to the Extension Center one time. The optional
   **Release tag** field selects a monorepo release or a prerelease. Without it, the review uses the
   latest stable release on GitHub. Review the pinned metadata and submit for moderation.
5. After approval, publish later GitHub Releases in the same repository and folder. Scheduled
   discovery validates them and queues new versions for moderation, so you submit no other form.
   The current approved version stays available during review.
6. After approval, run `vis-agent extension versions` with the identifier from the extension page.
   Confirm the available version and its reviewed commit SHA.

A catalog release uses canonical `MAJOR.MINOR.PATCH`, with an optional `aN`, `bN`, or `rcN` suffix,
for example `1.3.0rc1`. Mark a prerelease on GitHub too. Local package validation still uses the PEP
440 rules of the SDK. Put changelogs, breaking changes, and supported runtime versions in the GitHub
Release notes. You need no release asset, wheel, PyPI account, or publisher credential in Vis.

Discovery runs every five minutes. Each run checks one registered listing and one page of up to 20
releases, and inspects at most five new releases. So detection is not immediate, and it depends on
the catalog size and on GitHub availability.

Only approved versions are public. Vis keeps each rejection, so the same release does not go into
the queue again. The commit of a version cannot be replaced. To fix a rejected release or a release
with a moved tag, publish a new version. Never retag an approved version.

A listing checks metadata and required files. It is not a code audit or an endorsement.

Vis catalog maintainers can use the [Publish reviewed extension
workflow](https://github.com/Blockether/vis/actions/workflows/extension-publish.yml) after they
review the source and the dependencies. The workflow requires access to the Vis repository and its
`docs` environment. The [operator
guide](https://github.com/Blockether/vis/blob/main/apps/vis-docs/README.md) documents its inputs,
authenticated moderation, and verification.

A published or approved release does not install itself or update installed copies. Project
admission and `/reload` reconcile [configuration declarations](#declare-packages-in-configuration).
For an existing managed installation, use [the update and rollback
commands](#check-for-updates-and-roll-back).

## Reload, update, or remove

| Change | Action |
| --- | --- |
| Edit an entry, helper module, declared source root, or bundled skill | `/reload`, then call the tool on the next turn |
| Change dependencies in an existing package environment | Update `uv.lock` on purpose if needed, then `/reload` |
| Change dependencies used from shared packages | Run [shared sync](extension-development.md#install-a-project-into-shared-packages), then `/reload` |
| Change the dependencies of an editable project | Update its dependency declaration and lockfile, then `/reload` |
| Change a managed GitHub version | Explicit `extension update` or `extension rollback`, then `/reload` |
| Uninstall | Remove the `current` link of the package, then `/reload`. Keep the version directories or remove them separately. Do not delete a linked development checkout |

An install never overwrites an existing destination. `/reload` reconciles configured packages and
rebuilds extension contexts from the selected source. Live sessions switch at the next turn
boundary, as [Reload after changes](extension-development.md#reload-after-changes) describes.

If a package cannot prepare or load, Vis reports its name, location, and error. Other extensions
and the core tools stay available, so you can open the project and use Vis to repair the files or
the configuration. `vis-agent doctor` shows the failures. After the repair, run `/reload`. A
successful retry clears the warning without a restart of Vis.

A failed `/reload` shows the affected files and causes in chat. The report stays in the transcript
and enters the next model request, so the agent can repair the extension. Current failures stay in the context of the agent
until recovery.

`vis.state` survives a reload and a restart. A failed reload keeps the last working code,
contracts, docs, and package skills. It marks them stale with the failure reason and the loaded and
requested source fingerprints. The reload result, `vis-agent doctor`, and the assistant context
show them. Read [Old code or documentation after an
edit](extension-troubleshooting.md#old-code-or-documentation-after-an-edit).

Vis runs admitted copies of entries and declared source roots. A write beside those files changes a
private copy, not durable project data, so use `vis.state` for persistence. An editable project
imports its live checkout. A reload does not replace the running gateway binary or its startup
environment, and it does not reload native libraries.

## Package manifest

A package that you distribute keeps `pyproject.toml` and `extension.py` together, at the repository
root or in a selected subdirectory:

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
uses this manifest. Generate and commit `uv.lock` before you publish:

```toml
[project]
name = "vis-greeter"
version = "1.0.0"
description = "Typed greeting tools and an optional greeting procedure."
keywords = ["greetings", "example"]
requires-python = ">=3.12"
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
| `project.description`, `project.version` | The displayed package metadata |
| `project.keywords` | Optional catalog tags. At most two unique tags, each of 1 to 24 lowercase letters or digits, with single hyphens between words |
| `project.requires-python` | Must allow the embedded interpreter of Vis. Vis does not download another Python |
| `project.dependencies` | Must include an unconditional `vis-agent` requirement that is compatible with the running release |
| `tool.vis.category` | Required package metadata, one of `providers`, `tools`, or `workflows`. Catalog filters use tags |
| `tool.vis.source_paths` | Import roots inside the package, such as `src`, not the `vis_greeter` package directory |
| `tool.vis.skills` | Optional relative skill directories, or tables with `path` and [`settings`](#leave-a-skill-out-of-settings). Omit it when the package has no procedure |

Choose tags that say what your extension helps people do, such as `browser` or `testing`. You can
use your own tags. Tags appear on catalog cards and extension pages, and they do not grant the
"Official" badge. To change the tags of an approved extension, publish a new release.

In the catalog sidebar, select a tag to filter extensions. Select **Official** to show the
extensions that the Vis team maintains. Search combines with these filters, and the counters show
the matching extensions.

Keep the implementation under the selected package directory. Do not put a PEP 723 block in the
`extension.py` of this package. [Keep the entrypoint
small](extension-design.md#keep-the-entrypoint-small) shows the complete registration and
implementation.

Vis prepares the uv environment of the package before it registers the package, at these times:

- At startup, for global packages.
- When you add or open a project, for the packages of that project.
- On `/reload`.

Vis reports a preparation failure. The package does not fall back to `~/.vis/python/packages`.

To prepare a private environment, Vis runs the bundled upstream `uv sync`. It selects the embedded
Python of the gateway with `--python` and blocks interpreter downloads. uv manages the lock and the
environment of the project, default dependency groups and removal of extra packages included. uv
can update an existing lock.

[`python.index_url`](configuration.md#python-package-index) supplies the default index of uv. That
section also has the override rules.

A source-only package, where uv selects no dependency beyond the bundled Vis SDK, uses shared
packages and gets no `.venv`. [Project and shared
environments](extension-development.md#project-and-shared-environments) has the rules for the
trusted worker, sandbox imports, and trusted build code.

## Bundled skills

Declare up to 64 relative directories that contain `SKILL.md` in `tool.vis.skills`. Paths and
resource symlinks must stay inside the package. A duplicate path, a duplicate skill name, or a
resource that leaves the package fails the load. A PEP 723 script cannot declare skills.

Vis discovers a skill as `<normalized-project-name>/<skill-name>`, for example
`vis-greeter/greeting`. The skill name is the frontmatter name, or the directory name when the
frontmatter has none. It uses letters, digits, `_`, and `-`, and starts with a letter or a digit.
`doc()` identifies the package version and the bundled resource directory.

Read the skill with `doc("vis-greeter/greeting")`, find it with `apropos("vis-greeter/")`, or
request `/skill:vis-greeter/greeting`. An ordinary skill with that exact qualified name takes
precedence. Otherwise, different packages do not collide. The procedure does not change the working
project or widen filesystem access. Vis never runs the instructions when you install, list, or read
the skill.

Code, skills, and resources share the admitted source snapshot and the same reload, last-good
retention, and removal behavior. [Skills](skills.md) covers authoring and precedence. Put tool
reference material in docstrings, not in a skill.

### Leave a skill out of settings

Settings list each bundled skill under its extension, with its own switch. When the skill is the
main purpose of your extension, a second switch has no use. Write the entry of the skill as a table
with `settings = false`:

```toml
[tool.vis]
category = "tools"
skills = [{ path = "skills/browser", settings = false }, "skills/greeting"]
```

Settings then show no switch for `skills/browser`. The skill follows the [choice of the
extension](skills.md#control-availability). Off removes the skill, and Auto or On keep it. A value
saved earlier for the skill no longer applies. The other entry keeps its own switch. Vis versions
without this option reject the table form.

## Standalone scripts

Keep a thin entry file under `.vis/extensions/` and the implementation in its own source tree.
Declare the import roots and the dependencies in a [PEP 723 script metadata
block](https://peps.python.org/pep-0723/), before the imports:

```text
project/
  vis.yml
  einmal/src/einmal/__init__.py
  .vis/extensions/einmal_tools.py
```

```python
# .vis/extensions/einmal_tools.py
# /// script
# requires-python = ">=3.12"
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

This example checks local status and uses `show_start=False`. A status check that waits for a
network response keeps `show_start=True`. Every tool needs an [Activity
presentation](extension-api.md#activity-presentation) that people can read, whatever the package
layout.

`status` needs a docstring, like any exported tool. Vis validates the metadata, snapshots the source
files, and installs the dependencies before it runs the entry.

- `source_paths` names import roots. An import root is a directory that contains the packages or
  modules that you import, not a package directory. Vis resolves a relative path against the
  directory of the extension entry, not the current working directory. It also accepts an absolute
  path.
- Vis merges the contents of each declared root into the frozen extension directory. It rejects a
  missing directory, duplicate relative file names, and a root that contains the extension
  directory. Use narrow source roots such as `src`, not the whole checkout or a virtual environment.
- `dependencies` accepts standard package requirements, with version pins, extras, and environment
  markers. Vis uses its bundled pip to install only wheels into `~/.vis/python/packages`.
  `py3` and other extensions share that directory. Vis does not change the `.venv` of
  the project.
- Set the index with [`python.index_url` in `vis.yml`](configuration.md#python-package-index).
  Normal pip authentication and certificate settings still apply. A missing wheel or a failed
  install is a load failure. Vis does not fall back to a source build.
- `requires-python` checks the embedded interpreter. Vis does not download another Python version
  to satisfy it.
- `/reload` takes new source snapshots and resolves declared dependencies again for each extension
  that changed or failed. `/reload --all` does so for every extension. An unchanged loader scan
  does neither.

Both embedded workers import shared installed dependencies, and a tool call does not install them
again. After package changes, reload to rebuild the session workers. Read
[Reload after changes](extension-development.md#reload-after-changes) for calls that are already running.

## See also

- For implementation and integration tests, read [Extension design](extension-design.md).
- To prepare a uv project by hand, read [Using an existing Python
  project](extension-development.md).
- To fix loading, imports, and stale tools, read [Extension
  troubleshooting](extension-troubleshooting.md).
