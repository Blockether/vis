"""Inert Vis manifest validation and explicit GitHub/local source installation.

The catalog stores links and metadata, never source distributions. A project has
pyproject.toml and extension.py together, at repository root or in a selected
subdirectory. Installation requires trust; dependency preparation runs on reload.
"""

import json  # noqa: F401 -- the embedded engine invokes install via json.loads.
import os
import re
import shutil
import subprocess
import sys
import tempfile
import tomllib
from pathlib import Path
from urllib.parse import urlsplit

try:
    from packaging.requirements import Requirement
    from packaging.specifiers import SpecifierSet
    from packaging.utils import canonicalize_name
    from packaging.version import Version
except ImportError:
    from pip._vendor.packaging.requirements import Requirement
    from pip._vendor.packaging.specifiers import SpecifierSet
    from pip._vendor.packaging.utils import canonicalize_name
    from pip._vendor.packaging.version import Version

CATEGORIES = ("tools", "providers", "workflows")
MAX_METADATA = 128 * 1024
_EXCLUDED = {".git", ".venv", "venv", "__pycache__", "node_modules", ".DS_Store"}


def _relative(value):
    if not isinstance(value, str) or not value or "\\" in value or ":" in value:
        raise ValueError("Project paths must be relative portable paths")
    if len(value) > 512 or any(ord(c) < 32 or ord(c) == 127 for c in value):
        raise ValueError("Project paths are too long or contain control characters")
    if any(
        p in {"", ".", ".."} or p in _EXCLUDED or p.startswith(".env")
        for p in value.split("/")
    ):
        raise ValueError("Project contains an unsafe or excluded path")
    return value


def project_subdirectory(value=""):
    """An empty value or dot selects the repository root; otherwise use a portable path."""
    return "" if value in ("", ".") else _relative(value)


def github_repository(value):
    """Accept only an HTTPS GitHub owner/repository URL, without credentials or redirects."""
    if not isinstance(value, str) or any(c.isspace() or ord(c) < 32 for c in value):
        raise ValueError(
            "Use a GitHub repository URL: https://github.com/owner/repository"
        )
    parts = urlsplit(value)
    path = parts.path.rstrip("/").removesuffix(".git")
    if (
        parts.scheme != "https"
        or parts.netloc != "github.com"
        or parts.query
        or parts.fragment
        or not re.fullmatch(
            r"/[A-Za-z0-9][A-Za-z0-9-]{0,38}/[A-Za-z0-9_.-]{1,100}", path
        )
        or path.rsplit("/", 1)[-1] in {".", ".."}
    ):
        raise ValueError(
            "Use a GitHub repository URL, not a file or tree URL; set the project folder separately"
        )
    return "https://github.com" + path


def manifest_metadata(text, vis_version=None, python_version=None):
    """Parse TOML without executing code; optionally check the installing runtime."""
    if len(text.encode()) > MAX_METADATA:
        raise ValueError("pyproject.toml exceeds 128 KiB")
    try:
        data = tomllib.loads(text)
        project = data["project"]
        vis = data["tool"]["vis"]
        name = project["name"]
        version = project["version"]
        description = project["description"]
        dependencies = project["dependencies"]
        requires_python = project["requires-python"]
        category = vis["category"]
        if not re.fullmatch(r"[A-Za-z0-9](?:[A-Za-z0-9._-]{0,98}[A-Za-z0-9])?", name):
            raise ValueError("Invalid package name")
        version = str(Version(version))
        if len(version) > 80:
            raise ValueError("Package version is too long")
        if not isinstance(description, str) or not 1 <= len(description) <= 240:
            raise ValueError("Description must contain 1–240 characters")
        if not isinstance(requires_python, str) or not requires_python:
            raise ValueError("Declare requires-python")
        python_spec = SpecifierSet(requires_python)
        if python_version and not python_spec.contains(python_version):
            raise ValueError("Package requires a different Python version")
        if not isinstance(dependencies, list) or len(dependencies) > 128:
            raise ValueError("dependencies must be a list of at most 128 requirements")
        requirements = [Requirement(item) for item in dependencies]
        sdk = [r for r in requirements if canonicalize_name(r.name) == "vis-agent"]
        if not sdk or any(r.url or r.marker or r.extras for r in sdk):
            raise ValueError("Declare an unconditional vis-agent version requirement")
        if vis_version and any(not r.specifier.contains(vis_version) for r in sdk):
            raise ValueError(
                "Package requires a different Vis version; update Vis first"
            )
        if category not in CATEGORIES:
            raise ValueError("category must be providers, tools or workflows")
        if set(vis) - {"category", "source_paths", "skills"}:
            raise ValueError("tool.vis accepts category, source_paths and skills")
        paths = vis.get("source_paths", [])
        if not isinstance(paths, list) or len(paths) > 16:
            raise ValueError("source_paths must be a list of at most 16 directories")
        paths = [_relative(p) for p in paths]
        skills = vis.get("skills", [])
        if not isinstance(skills, list) or len(skills) > 64:
            raise ValueError("skills must be a list of at most 64 skill directories")
        skills = [_relative(p) for p in skills]
        if len(set(skills)) != len(skills):
            raise ValueError("skills must not repeat a directory")
        return {
            "name": canonicalize_name(name),
            "version": version,
            "description": description,
            "category": category,
            "requires_python": requires_python,
            "dependencies": dependencies,
            "source_paths": paths,
            "skills": skills,
        }
    except (KeyError, TypeError, AttributeError) as exc:
        raise ValueError(
            "Declare project name, version, description, requires-python, dependencies and tool.vis.category"
        ) from exc


def inspect_source(directory, vis_version=None, python_version=None):
    """Validate a checkout without importing its entry point or build backend."""
    directory = Path(directory).resolve(strict=True)
    for name in ("pyproject.toml", "extension.py"):
        file = directory / name
        if not file.is_file():
            raise ValueError(f"Project needs {name} in the selected folder")
        if not file.resolve().is_relative_to(directory):
            raise ValueError(f"{name} must stay inside the project")
    metadata = manifest_metadata(
        (directory / "pyproject.toml").read_text(encoding="utf-8"),
        vis_version,
        python_version,
    )
    for path in metadata["source_paths"]:
        resolved = (directory / path).resolve(strict=True)
        if not resolved.is_relative_to(directory) or not resolved.is_dir():
            raise ValueError("source_paths must be directories inside the project")
    for path in metadata["skills"]:
        skill = directory / path
        if not skill.is_dir() or not (skill / "SKILL.md").is_file():
            raise ValueError(
                f"skills entry {path!r} needs a directory containing SKILL.md"
            )
        for resource in [skill, *skill.rglob("*")]:
            if not resource.resolve().is_relative_to(directory):
                raise ValueError(
                    "skills and their resources must stay inside the project"
                )
            _relative(resource.relative_to(directory).as_posix())
    return metadata


def _git(*args):
    env = {
        **os.environ,
        "GIT_CONFIG_NOSYSTEM": "1",
        "GIT_CONFIG_GLOBAL": os.devnull,
        "GIT_TERMINAL_PROMPT": "0",
        "GIT_CONFIG_COUNT": "0",
    }
    # Do not inherit caller repository/worktree routing or external transport overrides.
    for key in (
        "GIT_DIR",
        "GIT_WORK_TREE",
        "GIT_INDEX_FILE",
        "GIT_OBJECT_DIRECTORY",
        "GIT_ALTERNATE_OBJECT_DIRECTORIES",
        "GIT_SSH",
        "GIT_SSH_COMMAND",
        "GIT_CONFIG_PARAMETERS",
    ):
        env.pop(key, None)
    try:
        return subprocess.run(
            [
                "git",
                "-c",
                "core.hooksPath=" + os.devnull,
                "-c",
                "credential.helper=",
                "-c",
                "protocol.file.allow=never",
                "-c",
                "protocol.ext.allow=never",
                *map(str, args),
            ],
            env=env,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            check=True,
            timeout=120,
        ).stdout.strip()
    except FileNotFoundError as exc:
        raise ValueError("Install Git and make it available on PATH") from exc
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as exc:
        raise ValueError(
            "Could not fetch the GitHub repository or revision; check the URL and connection"
        ) from exc


def _checkout(repository, directory, revision):
    _git("init", "--quiet", directory)
    _git(
        "-C",
        directory,
        "fetch",
        "--quiet",
        "--depth=1",
        "--",
        repository,
        revision or "HEAD",
    )
    _git("-C", directory, "checkout", "--quiet", "--detach", "FETCH_HEAD")
    actual = _git("-C", directory, "rev-parse", "HEAD")
    if revision and actual != revision:
        raise ValueError("Fetched revision does not match the reviewed commit")
    return actual


def _copy_project(source, destination):
    def excluded(_directory, names):
        return {name for name in names if name in _EXCLUDED or name.startswith(".env")}

    count, size = 0, 0
    for base, directories, files in os.walk(source):
        directories[:] = [
            p for p in directories if p not in excluded(base, directories)
        ]
        for name in [*directories, *files]:
            if name in excluded(base, [name]):
                continue
            path = Path(base) / name
            _relative(path.relative_to(source).as_posix())
            if path.is_symlink():
                raise ValueError(
                    "GitHub projects cannot contain symlinks; keep source inside the project"
                )
            count += 1
            if path.is_file():
                size += path.stat().st_size
            if count > 4096 or size > 64 * 1024 * 1024:
                raise ValueError("Selected project exceeds 4096 entries or 64 MiB")
    shutil.copytree(source, destination, ignore=excluded)


def install(
    source, directory, trust=False, subdirectory="", revision=None, vis_version=None
):
    """Atomically install a trusted GitHub revision or link a local project; never overwrite.

    GitHub source is fetched using Git, without submodules, hooks or archive transport.
    A catalog command pins a full commit SHA. Without a pin, the default branch is used.
    Only the selected project is installed. Local checkouts stay linked for /reload.
    """
    if not trust:
        raise ValueError(
            "Extensions and build backends run with your permissions; review the source and pass --trust"
        )
    subdirectory = project_subdirectory(subdirectory)
    if revision is not None and (
        not isinstance(revision, str) or not re.fullmatch(r"[0-9a-f]{40}", revision)
    ):
        raise ValueError("revision must be a full, lowercase Git commit SHA")
    path = Path(source).expanduser()
    if path.is_file() and path.name == "pyproject.toml":
        if subdirectory:
            raise ValueError(
                "Select a directory with --subdirectory, or a pyproject.toml file, not both"
            )
        path = path.parent
    repository = None if path.is_dir() else github_repository(source)
    if not repository and revision:
        raise ValueError(
            "revision applies only to a GitHub repository, not a local directory"
        )
    directory = Path(directory).expanduser()
    directory.mkdir(parents=True, exist_ok=True)
    python_version = ".".join(map(str, sys.version_info[:3]))
    with tempfile.TemporaryDirectory(prefix=".install-", dir=directory) as temporary:
        stage = Path(temporary)
        if repository:
            path = stage / "repository"
            revision = _checkout(repository, path, revision)
        selected = (path / subdirectory).resolve(strict=True)
        if not selected.is_relative_to(path.resolve()):
            raise ValueError("Selected folder must stay inside the repository")
        metadata = inspect_source(selected, vis_version, python_version)
        destination = directory / metadata["name"]
        lock = directory / ("." + metadata["name"] + ".install-lock")
        fd = os.open(lock, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        os.close(fd)
        try:
            if os.path.lexists(destination):
                raise FileExistsError(
                    "Extension already exists; remove it explicitly before replacing it"
                )
            if repository:
                _copy_project(selected, stage / "project")
                (stage / "project").rename(destination)
            else:
                destination.symlink_to(selected, target_is_directory=True)
        finally:
            lock.unlink()
    return {
        "name": metadata["name"],
        "version": metadata["version"],
        "path": str(destination),
        "mode": "github" if repository else "source",
        "revision": revision,
        "next": "Start Vis or /reload to prepare dependencies and load the extension",
    }
