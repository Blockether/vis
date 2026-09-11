"""Inert Vis manifest validation and explicit GitHub/local source installation.

The catalog stores links and metadata, never source distributions. A project has
pyproject.toml and extension.py together, at repository root or in a selected
subdirectory. Installation requires trust; dependency preparation runs on reload.
"""

import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import tomllib
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener

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
    """Validate source without importing code; managed installs retain their GitHub identity."""
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
    snapshot = directory.parent
    store = snapshot.parent.parent
    if directory.name == "project" and store.name == ".versions":
        active = store.parent / metadata["name"]
        if active.is_symlink() and active.resolve() == directory:
            _, record = _managed(store.parent, metadata["name"])
            metadata["repository"] = github_repository(record["repository_url"])[
                len("https://github.com/") :
            ].lower()
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


CATALOG = "https://vis.blockether.com"


class _NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def _catalog(repository, subdirectory):
    identity = hashlib.sha256(
        (repository.lower() + "\n" + subdirectory).encode()
    ).hexdigest()[:24]
    request = Request(
        CATALOG + "/api/extensions/" + identity,
        headers={"Accept": "application/json", "User-Agent": "Vis-Extension-Installer"},
    )
    try:
        with build_opener(_NoRedirect()).open(request, timeout=20) as response:
            body = response.read(4 * 1024 * 1024 + 1)
        if len(body) > 4 * 1024 * 1024:
            raise ValueError("Catalog response is too large")
        return json.loads(body)
    except HTTPError as exc:
        if exc.code == 404:
            raise ValueError(
                "No approved releases for this repository and project folder"
            ) from exc
        raise ValueError(
            "Extension Center is unavailable; no code was changed"
        ) from exc
    except (URLError, TimeoutError, OSError, json.JSONDecodeError) as exc:
        raise ValueError(
            "Extension Center is unavailable; no code was changed"
        ) from exc


def _releases(repository, subdirectory):
    data = _catalog(repository, subdirectory)
    if not isinstance(data, dict) or not isinstance(data.get("releases"), list):
        raise ValueError("Invalid release metadata from Extension Center")
    releases = []
    versions = set()
    for item in data["releases"]:
        if (
            not isinstance(item, dict)
            or github_repository(item.get("repository_url")).lower()
            != repository.lower()
            or project_subdirectory(item.get("subdirectory", "")) != subdirectory
            or not re.fullmatch(r"[0-9a-f]{40}", item.get("revision", ""))
            or not isinstance(item.get("version"), str)
        ):
            raise ValueError("Invalid release metadata from Extension Center")
        version = Version(item["version"])
        if version in versions:
            raise ValueError("Catalog contains ambiguous package versions")
        versions.add(version)
        releases.append(item)
    return sorted(releases, key=lambda item: Version(item["version"]), reverse=True)


def _select(releases, version=None):
    if version is not None:
        wanted = Version(version)
        selected = next(
            (item for item in releases if Version(item["version"]) == wanted), None
        )
    else:
        selected = next(
            (
                item
                for item in releases
                if not item.get("prerelease")
                and not Version(item["version"]).is_prerelease
                and not Version(item["version"]).is_devrelease
            ),
            None,
        )
    if selected is None:
        raise ValueError(
            "Requested version is not approved in Extension Center"
            if version
            else "No approved stable release; select a prerelease explicitly with --version"
        )
    return selected


def _name(name):
    if not isinstance(name, str) or not re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", name):
        raise ValueError("Use the normalized installed extension name")
    return name


def _managed(directory, name):
    destination = directory / _name(name)
    store = (directory / ".versions" / name).resolve()
    if not destination.is_symlink():
        raise ValueError(
            "Update and rollback require a managed GitHub install, not a local source link or directory"
        )
    project = destination.resolve(strict=True)
    snapshot = project.parent
    if project.name != "project" or snapshot.parent != store:
        raise ValueError("Update and rollback never replace local source links")
    record = json.loads((snapshot / "receipt.json").read_text(encoding="utf-8"))
    if record.get("name") != name or not re.fullmatch(
        r"[0-9a-f]{40}", record.get("revision", "")
    ):
        raise ValueError("Invalid installed package receipt")
    return snapshot, record


def versions(source, subdirectory="", directory=None):
    """List approved releases and update availability for a repository or installed name."""
    installed = None
    if not source.startswith("https://"):
        if directory is None:
            raise ValueError(
                "Use a GitHub repository URL or an installed extension name"
            )
        _, installed = _managed(Path(directory).expanduser().resolve(), source)
        source, subdirectory = installed["repository_url"], installed["subdirectory"]
    repository = github_repository(source)
    subdirectory = project_subdirectory(subdirectory)
    releases = _releases(repository, subdirectory)
    try:
        latest = _select(releases)["version"]
    except ValueError:
        latest = None
    return {
        "repository_url": repository,
        "subdirectory": subdirectory,
        "installed": installed["version"] if installed else None,
        "latest": latest,
        "update_available": bool(
            installed and latest and Version(latest) > Version(installed["version"])
        ),
        "releases": releases,
    }


def _trust(trust):
    if not trust:
        raise ValueError(
            "Extensions and build backends run with your permissions; review the source and pass --trust"
        )


def _admit(
    source,
    directory,
    subdirectory,
    revision,
    vis_version,
    release=None,
    replacing=None,
    expected=None,
    expected_name=None,
    expected_target=None,
):
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
    directory = Path(directory).expanduser().resolve()
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
        if expected_name is not None and metadata["name"] != expected_name:
            raise ValueError("Configured name does not match the package manifest")
        if release and (
            metadata["version"] != str(Version(release["version"]))
            or metadata["name"] != release["name"]
        ):
            raise ValueError("Fetched manifest does not match the approved release")
        if replacing and metadata["name"] != replacing:
            raise ValueError("An update cannot change the installed extension name")
        destination = directory / metadata["name"]
        lock = directory / ("." + metadata["name"] + ".install-lock")
        fd = os.open(lock, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        os.close(fd)
        try:
            previous = None
            if expected_target is not None:
                if (
                    not destination.is_symlink()
                    or str(destination.resolve()) != expected_target
                ):
                    raise ValueError(
                        "Installation changed during preparation; retry the operation"
                    )
            elif replacing:
                active, _ = _managed(directory, replacing)
                if active.name != expected:
                    raise ValueError(
                        "Installation changed during preparation; retry the operation"
                    )
                previous = active.name
            elif os.path.lexists(destination):
                raise FileExistsError(
                    "Extension already exists; use extension update or rollback explicitly"
                )
            if repository:
                snapshot = directory / ".versions" / metadata["name"] / stage.name
                snapshot.parent.mkdir(parents=True, exist_ok=True)
                prepared = stage / "prepared"
                prepared.mkdir()
                _copy_project(selected, prepared / "project")
                record = {
                    "name": metadata["name"],
                    "version": metadata["version"],
                    "repository_url": repository,
                    "subdirectory": subdirectory,
                    "revision": revision,
                    "release_tag": release.get("release_tag") if release else None,
                    "previous": previous,
                }
                (prepared / "receipt.json").write_text(
                    json.dumps(record), encoding="utf-8"
                )
                prepared.rename(snapshot)
                pointer = stage / "active"
                try:
                    pointer.symlink_to(snapshot / "project", target_is_directory=True)
                    os.replace(pointer, destination)
                except BaseException:
                    shutil.rmtree(snapshot)
                    raise
            else:
                if expected_target is not None:
                    pointer = stage / "active"
                    pointer.symlink_to(selected, target_is_directory=True)
                    os.replace(pointer, destination)
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


def install(
    source,
    directory,
    trust=False,
    subdirectory="",
    revision=None,
    vis_version=None,
    version=None,
):
    """Install an approved version, an explicit SHA, or a linked local project.

    No selector means the latest approved stable release, never a moving branch.
    Release selection does not import publisher code. Dependency preparation is on reload.
    """
    _trust(trust)
    subdirectory = project_subdirectory(subdirectory)
    if revision is not None and (
        not isinstance(revision, str) or not re.fullmatch(r"[0-9a-f]{40}", revision)
    ):
        raise ValueError("revision must be a full, lowercase Git commit SHA")
    if revision and version is not None:
        raise ValueError("Choose --version or --revision, not both")
    path = Path(source).expanduser()
    local = path.is_dir() or (path.is_file() and path.name == "pyproject.toml")
    release = None
    if local and version is not None:
        raise ValueError(
            "version applies only to a GitHub repository, not a local directory"
        )
    if not local and not revision:
        release = _select(_releases(github_repository(source), subdirectory), version)
        revision = release["revision"]
    return _admit(source, directory, subdirectory, revision, vis_version, release)


def update(name, directory, trust=False, version=None, vis_version=None):
    """Explicitly replace a managed package with an approved newer stable or selected release."""
    _trust(trust)
    directory = Path(directory).expanduser().resolve()
    active, current = _managed(directory, name)
    release = _select(
        _releases(current["repository_url"], current["subdirectory"]), version
    )
    older = Version(release["version"]) < Version(current["version"])
    if older and version is not None:
        raise ValueError(
            "Selected release is older; use extension rollback --version explicitly"
        )
    if older or release["revision"] == current["revision"]:
        return {
            "name": name,
            "version": current["version"],
            "mode": "github",
            "revision": current["revision"],
            "next": "No newer approved stable release; no changes made"
            if older
            else "Already installed; no changes made",
        }
    if Version(release["version"]) == Version(current["version"]):
        raise ValueError("A published version cannot change its approved commit")
    return _admit(
        current["repository_url"],
        directory,
        current["subdirectory"],
        release["revision"],
        vis_version,
        release,
        name,
        active.name,
    )


def rollback(name, directory, trust=False, version=None, vis_version=None):
    """Restore the previous pinned source, or select an older approved catalog version.

    Source is re-fetched and validated before the atomic pointer change. Previous
    snapshots are retained, including local edits; dependencies are resolved on /reload.
    """
    _trust(trust)
    directory = Path(directory).expanduser().resolve()
    active, current = _managed(directory, name)
    if version is not None:
        release = _select(
            _releases(current["repository_url"], current["subdirectory"]), version
        )
        if Version(release["version"]) >= Version(current["version"]):
            raise ValueError(
                "Rollback version must be older than the installed version"
            )
    else:
        previous = current.get("previous")
        if not isinstance(previous, str) or not re.fullmatch(
            r"\.install-[a-zA-Z0-9_-]+", previous
        ):
            raise ValueError(
                "No previous installation; choose an approved older --version"
            )
        release = json.loads(
            (active.parent / previous / "receipt.json").read_text(encoding="utf-8")
        )
        if (
            release.get("repository_url") != current["repository_url"]
            or release.get("subdirectory") != current["subdirectory"]
            or release.get("name") != name
            or not re.fullmatch(r"[0-9a-f]{40}", release.get("revision", ""))
        ):
            raise ValueError("Invalid previous package receipt")
    return _admit(
        current["repository_url"],
        directory,
        current["subdirectory"],
        release["revision"],
        vis_version,
        release,
        name,
        active.name,
    )


def _sync_spec(spec):
    if not isinstance(spec, dict) or set(spec) - {
        "source",
        "subdirectory",
        "version",
        "revision",
    }:
        raise ValueError("Invalid extension declaration")
    source = spec.get("source")
    if not isinstance(source, str) or not source or any(ord(c) < 32 for c in source):
        raise ValueError("Extension source must be a nonempty path or GitHub URL")
    remote = "://" in source
    source = (
        github_repository(source)
        if remote
        else str(Path(source).expanduser().resolve())
    )
    folder = project_subdirectory(spec.get("subdirectory", ""))
    version, revision = spec.get("version"), spec.get("revision")
    if version is not None:
        if not isinstance(version, str):
            raise ValueError("version must be a string")
        version = str(Version(version))
    if revision is not None and (
        not isinstance(revision, str) or not re.fullmatch(r"[0-9a-f]{40}", revision)
    ):
        raise ValueError("revision must be a full, lowercase Git commit SHA")
    if version is not None and revision is not None:
        raise ValueError("Choose version or revision, not both")
    if not remote and (version is not None or revision is not None):
        raise ValueError("version and revision apply only to GitHub repositories")
    return {
        "source": source,
        "subdirectory": folder,
        "version": version,
        "revision": revision,
    }


def _sync_records(directory):
    path = directory / ".sync.json"
    if not path.exists():
        return {}
    if path.is_symlink() or path.stat().st_size > 1024 * 1024:
        raise ValueError("Invalid extension sync receipt")
    records = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(records, dict) or len(records) > 128:
        raise ValueError("Invalid extension sync receipt")
    for name, record in records.items():
        _name(name)
        if (
            not isinstance(record, dict)
            or not isinstance(record.get("target"), str)
            or not isinstance(record.get("result"), dict)
            or not isinstance(record.get("spec"), dict)
        ):
            raise ValueError("Invalid extension sync receipt")
    return records


def _save_sync_records(directory, records):
    with tempfile.NamedTemporaryFile(
        mode="w", encoding="utf-8", dir=directory, prefix=".sync-", delete=False
    ) as output:
        temporary = Path(output.name)
        json.dump(records, output, sort_keys=True)
    try:
        os.replace(temporary, directory / ".sync.json")
    finally:
        temporary.unlink(missing_ok=True)


def _sync_owned(destination, record):
    return (
        record is not None
        and destination.is_symlink()
        and str(destination.resolve()) == record["target"]
    )


def _sync_one(name, spec, directory, current, refresh, vis_version):
    destination = directory / name
    exists = os.path.lexists(destination)
    if exists and not _sync_owned(destination, current):
        raise ValueError(
            "Existing extension is not owned by sync or was changed externally; no files replaced"
        )
    if exists and current["spec"] == spec and not refresh:
        metadata = inspect_source(
            destination, vis_version, ".".join(map(str, sys.version_info[:3]))
        )
        if metadata["name"] != name:
            raise ValueError("Configured name does not match the package manifest")
        return {**current["result"], "version": metadata["version"], "status": "cached"}
    source, folder = spec["source"], spec["subdirectory"]
    remote = source.startswith("https://")
    release, revision = None, spec["revision"]
    active = None
    if remote:
        if revision is None:
            release = _select(_releases(source, folder), spec["version"])
            revision = release["revision"]
        if exists and current["result"]["mode"] == "github":
            active, receipt = _managed(directory, name)
            if (
                receipt["repository_url"].lower() == source.lower()
                and receipt["subdirectory"] == folder
                and receipt["revision"] == revision
            ):
                metadata = inspect_source(
                    destination, vis_version, ".".join(map(str, sys.version_info[:3]))
                )
                if (
                    metadata["name"] != name
                    or release
                    and metadata["version"] != release["version"]
                ):
                    raise ValueError(
                        "Installed source no longer matches the selected release"
                    )
                return {**current["result"], "status": "cached"}
    result = _admit(
        source,
        directory,
        folder,
        revision,
        vis_version,
        release,
        name if active else None,
        active.name if active else None,
        expected_name=name,
        expected_target=current["target"] if exists and active is None else None,
    )
    return {**result, "status": "updated" if exists else "installed"}


def sync(
    configured,
    directory,
    trust=False,
    refresh=False,
    prune=False,
    dry_run=False,
    vis_version=None,
):
    """Reconcile one YAML scope; reuse pins until refresh and prune only owned links.

    Receipts and pointers are atomic. Old Git snapshots and local source are retained.
    A failed package is reported without removing its previous source or other packages.
    Dry-run performs no writes, imports or network calls. Dependencies are prepared by
    the host after source admission, using upstream uv's own readiness/cache checks.
    """
    if not dry_run:
        _trust(trust)
    if not isinstance(configured, dict) or len(configured) > 128:
        raise ValueError("extensions must be a map of at most 128 named packages")
    specs = {_name(name): _sync_spec(spec) for name, spec in configured.items()}
    directory = Path(directory).expanduser().resolve()
    if not directory.exists() and not specs:
        return []
    if not dry_run:
        directory.mkdir(parents=True, exist_ok=True)
    lock = directory / ".sync-lock"
    if not dry_run:
        fd = os.open(lock, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        os.close(fd)
    try:
        records = _sync_records(directory)
        results = []
        for name in sorted(set(specs) | set(records)):
            spec, current = specs.get(name), records.get(name)
            destination = directory / name
            try:
                if spec is None:
                    result = {"name": name, "status": "orphaned"}
                    if prune:
                        if os.path.lexists(destination) and not _sync_owned(
                            destination, current
                        ):
                            raise ValueError(
                                "Extension changed externally; refusing to prune it"
                            )
                        result["status"] = "would-remove" if dry_run else "removed"
                        if not dry_run:
                            destination.unlink(missing_ok=True)
                            del records[name]
                            _save_sync_records(directory, records)
                elif dry_run:
                    unchanged = (
                        _sync_owned(destination, current)
                        and current["spec"] == spec
                        and not refresh
                    )
                    result = {
                        "name": name,
                        "status": "cached" if unchanged else "would-sync",
                    }
                else:
                    result = _sync_one(
                        name, spec, directory, current, refresh, vis_version
                    )
                    record = {
                        "spec": spec,
                        "target": str(destination.resolve()),
                        "result": {k: v for k, v in result.items() if k != "status"},
                    }
                    if records.get(name) != record:
                        records[name] = record
                        _save_sync_records(directory, records)
                results.append(result)
            except (ValueError, OSError) as error:
                results.append(
                    {"name": name, "status": "failed", "error": str(error)[:1024]}
                )
        return results
    finally:
        if not dry_run:
            lock.unlink()
