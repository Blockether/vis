"""Find the `vis-agent` command, or download the matching Vis release once.

A local engine needs the Vis command. When `vis-agent` is not on `PATH` or in
`~/.local/bin`, the SDK downloads the release launcher into its own cache. It checks
the launcher against the release `SHA256SUMS`, and the launcher checks the native
engine bundle. The download does not change shell profiles, the user's installed
track or a running gateway. Set `VIS_NO_AUTO_INSTALL=1` to turn it off.
"""

from __future__ import annotations

import fcntl
import hashlib
import importlib.metadata
import os
import re
import shutil
import subprocess
import sys
import tempfile
import urllib.error
import urllib.request
from pathlib import Path

_COMMAND = "vis-agent"
_RELEASE = re.compile(r"^\d+\.\d+\.\d+$")
_TAG = re.compile(r"^v\d+\.\d+\.\d+$")
_OFF = {"1", "true", "yes", "on"}
# The native bundle is large; a slow network still completes in this time.
_UPDATE_TIMEOUT = 1800


class InstallError(RuntimeError):
    """The SDK could not find or download the Vis command."""


def _repository() -> str:
    return os.environ.get("VIS_REPO_SLUG") or "Blockether/vis"


def _cache_root() -> Path:
    home = os.environ.get("VIS_HOME") or str(Path.home() / ".vis")
    return Path(home) / "sdk"


def _installed() -> str | None:
    found = shutil.which(_COMMAND)
    if found:
        return found
    candidate = Path.home() / ".local/bin" / _COMMAND
    if candidate.is_file() and os.access(candidate, os.X_OK):
        return str(candidate)
    return None


def _complete(directory: Path) -> bool:
    launcher = directory / _COMMAND
    return (
        launcher.is_file()
        and os.access(launcher, os.X_OK)
        and (directory / "vis-agent-native").is_file()
        and (directory / "vis-agent-native.build").is_file()
    )


def _sdk_tag() -> str | None:
    """Return the release tag of this SDK, or None for a development build."""
    try:
        version = importlib.metadata.version("vis-agent")
    except importlib.metadata.PackageNotFoundError:
        return None
    return f"v{version}" if _RELEASE.match(version) else None


def _fetch(url: str) -> bytes:
    with urllib.request.urlopen(url, timeout=60) as response:
        return response.read()


def _latest_tag() -> str:
    url = f"https://github.com/{_repository()}/releases/latest"
    with urllib.request.urlopen(url, timeout=60) as response:
        tag = response.geturl().rstrip("/").rsplit("/", 1)[-1]
    if not _TAG.match(tag):
        raise InstallError(f"could not find the latest Vis release at {url}")
    return tag


def _expected_hash(sums: bytes, name: str) -> str:
    for line in sums.decode("utf-8", "replace").splitlines():
        parts = line.split()
        if len(parts) == 2 and parts[1].lstrip("*") == name:
            return parts[0].lower()
    raise InstallError(f"SHA256SUMS has no entry for {name}")


def _download(tag: str, directory: Path) -> None:
    base = f"https://github.com/{_repository()}/releases/download/{tag}"
    launcher = _fetch(f"{base}/{_COMMAND}")
    expected = _expected_hash(_fetch(f"{base}/SHA256SUMS"), _COMMAND)
    if hashlib.sha256(launcher).hexdigest() != expected:
        raise InstallError(f"the {_COMMAND} command of {tag} does not match SHA256SUMS")
    directory.mkdir(parents=True, exist_ok=True)
    target = directory / _COMMAND
    temporary = directory / f"{_COMMAND}.tmp"
    temporary.write_bytes(launcher)
    temporary.chmod(0o755)
    temporary.replace(target)
    # A private VIS_HOME keeps the user's track, components and gateway untouched.
    state = directory / "home"
    state.mkdir(exist_ok=True)
    env = {**os.environ, "VIS_HOME": str(state)}
    command = [str(target), "update", "--track", "release", tag, "--components", "none"]
    with tempfile.TemporaryFile() as log:
        result = subprocess.run(
            command,
            env=env,
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=subprocess.STDOUT,
            timeout=_UPDATE_TIMEOUT,
        )
        if result.returncode != 0 or not _complete(directory):
            log.seek(0)
            tail = log.read().decode("utf-8", "replace")[-2000:]
            raise InstallError(f"could not install Vis {tag}:\n{tail}")


def ensure_vis_agent() -> str:
    """Return the path of the `vis-agent` command and download it when missing.

    Use an installed command first. Otherwise use the SDK cache under
    `$VIS_HOME/sdk/<tag>`, and download the release of this SDK version (or the
    latest release for a development build) into it.
    """
    installed = _installed()
    if installed:
        return installed
    root = _cache_root()
    tag = _sdk_tag()
    if tag and _complete(root / tag):
        return str(root / tag / _COMMAND)
    if (os.environ.get("VIS_NO_AUTO_INSTALL") or "").strip().lower() in _OFF:
        raise InstallError(
            f"the {_COMMAND} command is not installed, and VIS_NO_AUTO_INSTALL "
            "turns off the download. Install Vis or pass executable=..."
        )
    if sys.platform not in ("linux", "darwin"):
        raise InstallError("the Vis download supports Linux and macOS")
    try:
        return _download_once(root, tag or _latest_tag())
    except urllib.error.HTTPError as error:
        # An SDK version without release assets falls back to the latest release.
        if not tag or error.code != 404:
            raise InstallError(f"could not download Vis {tag}: {error}") from None
    except OSError as error:
        raise InstallError(f"could not download Vis: {error}") from None
    try:
        return _download_once(root, _latest_tag())
    except OSError as error:
        raise InstallError(f"could not download Vis: {error}") from None


def _download_once(root: Path, tag: str) -> str:
    directory = root / tag
    root.mkdir(parents=True, exist_ok=True)
    with open(root / f"{tag}.lock", "w") as lock:
        # A second process waits here and then uses the finished download.
        fcntl.flock(lock, fcntl.LOCK_EX)
        if not _complete(directory):
            print(f"vis-agent: downloading Vis {tag} into {directory}", file=sys.stderr)
            _download(tag, directory)
    return str(directory / _COMMAND)
