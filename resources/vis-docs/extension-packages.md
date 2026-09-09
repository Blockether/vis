# Extension packages

Use a package for publication or for tools that ship with skills. For a small local
script or an existing editable uv project, use the alternatives below. Do not mix
these dependency modes in one entrypoint.

## Extension Center projects

The [tested greeter package](https://github.com/Blockether/vis/tree/main/packages/vis-agent/examples/greeter) is the canonical starter:

```text
greeter/
  pyproject.toml
  extension.py
  src/vis_greeter/__init__.py
  tests/test_greeter.py
  skills/greeting/SKILL.md
  skills/greeting/references/style.md
```

Its `pyproject.toml` declares import roots and skill directories without executing code:

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

Keep `pyproject.toml` and `extension.py` together at repository root or in a selected
subdirectory. Commit `uv.lock` for repeatable dependency resolution. The complete
entrypoint is shown in the [quickstart](extending.md); the implementation is in
[Extension design](extension-design.md).

The manifest must declare an unconditional `vis-agent` dependency. Its version
constraint checks compatibility with the running Vis release; `requires-python`
checks the embedded interpreter. Vis does not download another Python interpreter.
The registered extension name must match the normalized project name. The manifest
supplies the displayed version, description and category: `providers`, `tools` or
`workflows`. Keep any additional Python implementation under the package directory.
Do not combine this layout with a PEP 723 block in `extension.py`.

The Extension Center aggregates public GitHub repositories. Choose **Add a repository**,
enter an HTTPS repository URL and leave **Project folder** empty for repository root.
For a monorepo, specify the folder containing both `pyproject.toml` and `extension.py`,
for example `extensions/greeting`. The Worker reads metadata without executing code.
Review the resolved commit and submit it for moderation. New entries and updates remain
private until approved; resubmission never replaces a published listing automatically.
Separate folders can have separate entries.
See the [standalone app instructions](https://github.com/Blockether/vis/tree/main/apps/vis-extension-center).

After reviewing the source and dependencies, copy the catalog's commit-pinned install
command. You can also install a GitHub project's default branch or link local source.
These examples use a placeholder public repository; replace it with your own:

```bash
vis-agent extension install https://github.com/example/vis-greeter --trust
vis-agent extension install https://github.com/example/extensions --subdirectory tools/greeting --trust
vis-agent extension install ./vis-greeter/pyproject.toml --project --trust
```

For a reviewed immutable source version, add `--revision` followed by its full lowercase
40-character Git commit SHA. GitHub installs require Git on `PATH`; only HTTPS
`github.com/owner/repository` URLs are accepted. Specify a folder separately instead of
pasting a GitHub file or tree URL. Submodules and Git LFS are not fetched. Keep required
source and portable dependency paths within the selected project. Symlinks are not
accepted in downloaded projects; selected contents are limited to 4096 entries and 64 MiB.

Installation defaults to `~/.vis/extensions/`; `--project` selects the current
workspace's `.vis/extensions/`. A Git checkout is staged and only the selected project
is installed atomically. A local checkout is linked rather than copied, so edits become
available on `/reload`. Existing destinations are never overwritten. To replace one,
explicitly remove the installed link or directory first, preserving source work.
The catalog stores no source bundles and is not consulted during installation.

At gateway startup and on `/reload`, Vis automatically prepares these projects using
`uv` from `PATH`. It creates `uv.lock` if absent, respects an existing lock, and skips
installation when its readiness record still matches the project, runtime, index and
installed distributions. A stale supplied lock is an error: update it deliberately
with `uv lock` rather than expecting reload to rewrite it. Source-only edits need
`/reload`, not another install command. Use `python.index_url` to select the index.
The connecting terminal reports startup preparation and dependency stages; status
is also included in authenticated gateway administration responses.

`--trust` permits extension code and dependency build backends to run with your
user permissions. Validation is not a security review. Dependencies use the shared
`~/.vis/python/packages` directory, not isolated per-extension environments. A failed
reload retains the last working extension definition but cannot roll back shared
package changes. Fix the dependency error before retrying `/reload`.

## Bundled skills

`tool.vis.skills` is an optional list of up to 64 relative directories containing
`SKILL.md`. Paths and resource symlinks must stay inside the package; duplicate
paths are rejected. Scripts using PEP 723 metadata do not declare bundled skills.

Each skill is discovered as `<normalized-project-name>/<skill-name>`, for example
`vis-greeter/greeting`. The frontmatter name, or directory name when absent, must
contain only letters, digits, underscores and hyphens, starting with a letter or
digit. Duplicate skill names within a package fail its load. Package names must
already match the extension's registered name.

Existing project, user and plugin skills keep their current precedence. A skill
with the exact qualified name can explicitly override a package skill; otherwise
names from different packages do not collide. `doc()` identifies the package
version and bundled resource directory. Package procedures do not change the
session's working project.

Skills and resources are read from the same admitted source snapshot as the
extension. Edits take effect on successful `/reload`; a failed reload retains the
last working code and skills. Updating the installed revision and reloading updates
both. Removing the installed package/link and reloading removes its skills too.
Never remove a linked development checkout when you only intend to uninstall its link.

`doc("vis-greeter/greeting")`, `apropos("vis-greeter/")` and
`/skill:vis-greeter/greeting` use the existing skill mechanism. Resources remain
subject to normal filesystem access; declaring a skill does not widen the sandbox.
Installing, listing or reading a skill never executes its procedure. See [Skills](skills.md).

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

Source snapshots contain extension code, not installed dependencies. Both embedded
workers import the same shared package directory; calling a tool does not install
packages again. Each process has its own module cache and permissions. Dependency
versions are shared across all sessions and extensions, not isolated per project.
A failed reload retains the extension definition, but does not roll back shared
package updates. After changing installed packages, use `/reload` to rebuild the
session workers; an already running call can retain its imported modules until it ends.

## uv projects

For a manually prepared uv package, keep normal Python packaging metadata in the package
and a thin Vis entry file beside the workspace. The implementation does not need
to depend on Vis; only the entry imports the host-provided `blockether.vis.extension`.

```text
project/
  einmal/
    pyproject.toml
    uv.lock                       # generated by uv lock
    src/einmal/__init__.py
    tests/test_status.py
  .vis/extensions/einmal_tools.py
```

This complete example uses setuptools with an editable `src/` layout:

```toml
# einmal/pyproject.toml
[project]
name = "einmal"
version = "0.1.0"
requires-python = ">=3.12"
dependencies = []

[build-system]
requires = ["setuptools>=64"]
build-backend = "setuptools.build_meta"

[tool.setuptools.packages.find]
where = ["src"]
```

```python
# einmal/src/einmal/__init__.py
def status() -> str:
    """Return the integration status."""
    return "ready"
```

```python
# .vis/extensions/einmal_tools.py
# /// script
# requires-python = ">=3.12"
# dependencies = []
# [tool.vis]
# project = "../../einmal"
# ///
import blockether.vis.extension as vis
from einmal import status

vis.register(vis.Extension(
    name="einmal",
    description="Package example.",
    alias="einmal",
    symbols=[vis.Symbol(status)],
))
```

`project` is relative to the entry file, not the working directory. Absolute paths
also work. It must contain both `pyproject.toml` and `uv.lock`. Project mode rejects
nonempty script dependencies; declare dependencies in `pyproject.toml` instead.
Do not add this package's `src` to `tool.vis.source_paths`: its editable install
already provides the import root. `source_paths` is the alternative for source
that Vis snapshots without installing it as a package.

Install uv on the **sync command's PATH**. From `project/`, generate and commit the
lockfile, then prepare the package for Vis:

```bash
uv lock --project ./einmal
vis-agent python uv sync --project ./einmal --locked
vis-agent python -c "import einmal; print(einmal.status(), einmal.__file__)"
vis-agent extension list
```

The import prints `ready` and the path to `einmal/src/einmal/__init__.py` in this
checkout, not a copied module under `~/.vis/python/packages`. The extension list
includes `einmal`. Start Vis in `project/`, or run `/reload` there, to load its tools.

Run sync as the same OS user and with the same Vis runtime and package-directory
settings as the gateway. It uses the embedded Python and runs
`uv export --locked --no-default-groups --format pylock.toml`, then
`uv pip install --target` on the exported lock. It preserves editable local
sources, resolved dependencies, artifact hashes and named indexes while retaining
unrelated packages. Python downloads are disabled. The temporary export is removed;
`uv.lock` and the project's `.venv` are unchanged. Default dependency groups and
optional extras are not installed. `--offline` and `--no-cache` are supported;
other uv sync options are rejected.

A project needs a build backend to install its own package. Without `[build-system]`
(or when uv is configured not to package the project), preparing dependencies does
not install the project's source. For a local dependency, declare it in
`project.dependencies` and explicitly select editable mode in the project's TOML:

```toml
[tool.uv.sources]
shared-tools = { path = "../shared-tools", editable = true }
```

The sibling package must have its own packaging metadata. A plain path dependency
without `editable = true` is not a promise of live source imports. Published wheels
are normal installed dependencies, not editable source trees.

Vis installs into **`~/.vis/python/packages`**. Editable installs place `.pth` files
or backend import hooks there, rather than copying the implementation. Both the
sandbox and trusted extension worker activate them. They do **not** grant access
to the referenced source: sandbox imports still require that checkout to be in an
allowed [workspace filesystem root](jail.md#filesystem-access).
Keep the checkout at its installed path; moving it requires another sync.

`~/.vis/python/packages` is shared across projects, so dependency versions are
not isolated. Startup and plain `/reload` never install manually selected uv projects.
Readiness checks name changed inputs: project location, `pyproject.toml`, `uv.lock`,
runtime, interpreter path, packages directory, default index or installed distribution
metadata. Only distributions named in the exported project lock are tracked; updating
an unrelated shared package or editing editable Python source does not require sync.
A failed load does not roll back shared package changes.

After reviewing dependency changes, use **`/reload --sync`** to prepare the uv projects
declared by the configured extensions and retry loading them. This explicit host operation
works even when the assistant's shell is disabled. It runs trusted build backends with the
gateway's user, interpreter and package directory, and requires uv on the gateway's PATH.
It respects supplied locks; update a stale lock deliberately before retrying. Ordinary
`/reload` and imports do not authorize this preparation.

A failed reload reports whether an extension was not loaded or its last-known-good tools
and docs were retained as stale. The reload result, doctor and assistant context include
loaded/requested source fingerprints and the failure reason. Successful retry clears the
warning; execution, signatures, contracts and docs update at the next turn boundary.

Build backends and executable `.pth` lines are trusted package code, not inert
configuration. Review projects and dependencies before installing them. Builds run
during explicit sync; `.pth` setup runs when a worker activates the package site.
Imports in `python_execution` never install packages, and the shared directory is
read-only to sandbox code.

`python.index_url` supplies uv's default index; named source indexes are not replaced.
Keep credentials in uv's supported credential configuration or the sync process's
environment, not committed URLs. Installer diagnostics are suppressed because they
may contain credentials. Loading a prepared project does not require uv on the
gateway's PATH.

### Explicit installation workflow

| Change | Required action |
| --- | --- |
| First use of a checkout | Generate `uv.lock`, run `vis-agent python uv sync --project ./einmal --locked`, then start Vis or `/reload` |
| Edit existing editable Python source or an extension entry | `/reload`; no reinstall or gateway restart |
| Change dependencies, packaging metadata or the checkout location | Update the lock if needed, sync again, then `/reload` |
| Change Vis runtime, package directory or index | Sync using the intended runtime and settings, then load the extension |
| Replace compiled extension code | Rebuild and install it, then use a fresh Vis process; Python source reload is not a native-library reload |

`/reload` updates tools in live sessions at the next turn boundary.
Already running calls may finish with old code. `/reload` does not replace a running
gateway's binary or startup environment; adopting a new Vis build or changing
startup location overrides requires starting the gateway with those settings once.

Editable packages use the live checkout, not Vis's frozen source snapshots. Cached
imports can retain old code until reload, while a first import can read edits sooner.
Use `/reload` as the update step; do not rely on editing a file alone to refresh an
already imported function. Ordinary installed dependencies are not cleared from
the registration worker's module cache by editable reload.

This manual installation workflow is selected by `tool.vis.project`. Script-only
PEP 723 `dependencies` still use the automatic pip loader. A plain external `uv sync`
prepares a separate project environment; it does not install packages for Vis.

### Keep business logic outside the entry file

Keep the implementation in the ordinary Python package, as in the example above,
and test it independently:

```python
# einmal/tests/test_status.py
from einmal import status


def test_status():
    assert status() == "ready"
```

After the initial sync, run from `project/`:

```bash
vis-agent python -m pip install pytest
vis-agent python -m pytest einmal/tests/ -q
```

No `PYTHONPATH` or extra `source_paths` is needed for this editable package. The
spelling is `vis-agent python -m pytest`, not `vis-agent python pytest`. The command
uses the shared Vis packages, not the project's `.venv`; dev groups in `uv.lock`
are not installed by the sync command. Update the test expectation when changing
`status()`'s result. For source used only through `tool.vis.source_paths`, configure
import roots separately in the package's own test environment.

After unit tests pass, verify extension registration and a tool call in a Vis
session: explicit sync, `/reload`, then call the tool. `vis-agent extension list`
checks registration only. A passing package test alone does not prove that the
prepared dependencies can be imported and called in the trusted extension worker.

Trusted extension workers support native calls through `ctypes`, including SciPy's
callback initialization. The model sandbox remains a separate, confined process.
Verify a representative calculation through the extension tool; installation or
registration alone is not a compatibility check.

## Reloading

`/reload` closes Python extension contexts and loads the current files.
`vis.state` persists, and live sessions can use the new tools and commands.
File edits do not affect a running extension until it reloads.

At load, Vis copies the extension's directory to a private location and runs
that copy. Changes to helper modules also require `/reload`. Files written
next to the extension are created in the private copy; use `vis.state` for
persistent data.


Only `extension.py` is loaded from a package directory. Test files (`test_*.py` and
`*_test.py`) are not extensions. `/test` runs extension tests with pytest, installed
on first use. Test the ordinary package and an actual tool call as described in
[Extension design](extension-design.md#test-both-boundaries).

## See also

- [Extending Vis](extending.md).
- [Extension troubleshooting](extension-troubleshooting.md).
