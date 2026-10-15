"""Install the Vis engine that matches this SDK.

`ensure_installed()` returns the `vis-agent` command for this SDK. It uses an
installed Vis only when that build is the same release as the SDK. Otherwise it
downloads the release once into `$VIS_HOME/sdk/<tag>`. `install()` makes that
download now, for example while you build a container image.

The download is the engine bundle of one platform: the `vis-agent` launcher, the
native engine and its Python runtime. It has no terminal client, web app or desktop
app. The SDK checks the archive against the release `SHA256SUMS` and the build stamp
against the release tag. It does not change shell profiles, the installed track or a
running gateway. Set `VIS_NO_AUTO_INSTALL=1` to turn off the automatic download.
"""

from __future__ import annotations

import fcntl
import hashlib
import importlib.metadata
import importlib.resources
import json
import os
import platform
import re
import shutil
import sys
import tarfile
import tempfile
import urllib.error
import urllib.request
from pathlib import Path

_COMMAND = "vis-agent"
_NATIVE = "vis-agent-native"
# A launcher with this file beside it always runs the native engine beside it.
_PINNED = "vis-agent.pinned"
_RELEASE = re.compile(r"^\d+\.\d+\.\d+$")
_STABLE = re.compile(r"^v\d+\.\d+\.\d+$")
_TAG = re.compile(r"^(v\d+\.\d+\.\d+|beta-[0-9a-f]{40})$")
_OFF = {"1", "true", "yes", "on"}
_CHUNK = 1 << 20
# Each network read must progress within this time; the engine bundle is large.
_READ_TIMEOUT = 120


class InstallError(RuntimeError):
    """The SDK could not find or download the Vis engine."""


def _repository() -> str:
    return os.environ.get("VIS_REPO_SLUG") or "Blockether/vis"


def _vis_home() -> Path:
    return Path(os.environ.get("VIS_HOME") or Path.home() / ".vis")


def _cache_root() -> Path:
    return _vis_home() / "sdk"


def _embedded_tag() -> str | None:
    """Return the release tag that the wheel build recorded, if any."""
    try:
        text = (
            importlib.resources.files("blockether.vis.engine")
            .joinpath("release-tag")
            .read_text(encoding="utf-8")
        )
    except (OSError, ModuleNotFoundError):
        return None
    tag = text.strip()
    return tag if _TAG.match(tag) else None


def _editable(distribution: importlib.metadata.Distribution) -> bool:
    try:
        url = json.loads(distribution.read_text("direct_url.json") or "{}")
    except ValueError:
        return False
    return bool(url.get("dir_info", {}).get("editable"))


def _sdk_tag() -> str | None:
    """Return the release tag of this SDK, or None for a development build.

    A release SDK `X.Y.Z` uses `vX.Y.Z`. A beta SDK `X.Y.Z.devN` uses the
    `beta-<sha>` tag that its wheel build recorded. An editable checkout is a
    development build.
    """
    try:
        distribution = importlib.metadata.distribution("vis-agent")
    except importlib.metadata.PackageNotFoundError:
        return None
    if _editable(distribution):
        return None
    version = distribution.version
    if _RELEASE.match(version):
        return f"v{version}"
    return _embedded_tag()


def _open(url: str):
    return urllib.request.urlopen(url, timeout=_READ_TIMEOUT)


def _fetch(url: str) -> bytes:
    with _open(url) as response:
        return response.read()


def _fetch_file(url: str, target: Path) -> str:
    """Stream `url` into `target` and return its SHA-256."""
    digest = hashlib.sha256()
    with _open(url) as response, open(target, "wb") as output:
        while chunk := response.read(_CHUNK):
            digest.update(chunk)
            output.write(chunk)
    return digest.hexdigest()


def _latest_tag() -> str:
    url = f"https://github.com/{_repository()}/releases/latest"
    with _open(url) as response:
        tag = response.geturl().rstrip("/").rsplit("/", 1)[-1]
    if not _STABLE.match(tag):
        raise InstallError(f"could not find the latest Vis release at {url}")
    return tag


def _expected_hash(sums: bytes, name: str) -> str:
    for line in sums.decode("utf-8", "replace").splitlines():
        parts = line.split()
        if len(parts) == 2 and parts[1].lstrip("*") == name:
            return parts[0].lower()
    raise InstallError(f"SHA256SUMS has no entry for {name}")


def _platform_asset() -> str:
    """Return the engine bundle name for this computer."""
    machine = platform.machine().lower()
    if sys.platform == "darwin" and machine == "arm64":
        return "vis-agent-macos-arm64.tar.gz"
    if sys.platform == "linux" and machine in ("x86_64", "amd64"):
        return "vis-agent-linux-x64.tar.gz"
    if sys.platform == "linux" and machine in ("aarch64", "arm64"):
        return "vis-agent-linux-arm64.tar.gz"
    raise InstallError(
        f"Vis has no engine for {sys.platform} {machine}. "
        "Use Linux (x64 or arm64) or macOS on Apple silicon."
    )


def _stamp_matches(stamp: Path, tag: str) -> bool:
    """True when the build stamp `version commit track …` is the build of `tag`."""
    try:
        fields = stamp.read_text(encoding="utf-8").split()
    except OSError:
        return False
    if len(fields) < 3:
        return False
    version, commit, track = fields[:3]
    if tag.startswith("beta-"):
        return track == "beta" and commit == tag.removeprefix("beta-")
    return track == "release" and version == tag.removeprefix("v")


def _executable(path: Path) -> bool:
    return path.is_file() and os.access(path, os.X_OK)


def _complete(directory: Path, tag: str) -> bool:
    return (
        _executable(directory / _COMMAND)
        and _executable(directory / _NATIVE)
        and (directory / _PINNED).is_file()
        and (directory / "vis-agent-python" / "python").is_dir()
        and _stamp_matches(directory / f"{_NATIVE}.build", tag)
    )


def _installed() -> str | None:
    found = shutil.which(_COMMAND)
    if found:
        return found
    candidate = Path.home() / ".local/bin" / _COMMAND
    return str(candidate) if _executable(candidate) else None


def _installed_matches(command: str, tag: str) -> bool:
    """True when the launcher `command` runs the native build of `tag`.

    This follows the launcher: a pinned bundle runs the engine beside it, the `dev`
    track runs source, and otherwise the engine beside the launcher wins over the
    one in `$VIS_HOME/install`.
    """
    bin_dir = Path(command).resolve().parent
    install_dir = _vis_home() / "install"
    bundled = bin_dir / _NATIVE
    if not ((bin_dir / _PINNED).is_file() and _executable(bundled)):
        try:
            track = (install_dir / "track").read_text(encoding="utf-8").strip()
        except OSError:
            track = ""
        if track == "dev":
            return False
    for native in (bundled, install_dir / _NATIVE):
        if _executable(native):
            return _stamp_matches(native.with_name(f"{_NATIVE}.build"), tag)
    return False


def _check_bundle(directory: Path, tag: str) -> None:
    missing = [
        name for name in (_COMMAND, _NATIVE) if not _executable(directory / name)
    ]
    python = directory / "vis-agent-python"
    if not (python / "python").is_dir() or not list(python.glob("libvispython.*")):
        missing.append("vis-agent-python")
    if missing:
        raise InstallError(f"the Vis {tag} bundle has no {', '.join(missing)}")
    if not _stamp_matches(directory / f"{_NATIVE}.build", tag):
        raise InstallError(
            f"the Vis {tag} bundle has the build stamp of another release"
        )


def _download(tag: str, directory: Path) -> None:
    """Download, check and unpack the engine bundle of `tag` into `directory`."""
    asset = _platform_asset()
    base = f"https://github.com/{_repository()}/releases/download/{tag}"
    expected = _expected_hash(_fetch(f"{base}/SHA256SUMS"), asset)
    with tempfile.TemporaryDirectory(dir=directory.parent, prefix=f".{tag}-") as work:
        archive = Path(work) / asset
        if _fetch_file(f"{base}/{asset}", archive) != expected:
            raise InstallError(f"the {asset} of {tag} does not match SHA256SUMS")
        bundle = Path(work) / "bundle"
        with tarfile.open(archive) as tar:
            tar.extractall(bundle, filter="data")
        _check_bundle(bundle, tag)
        (bundle / _PINNED).write_text(f"{tag}\n", encoding="utf-8")
        if directory.exists():
            shutil.rmtree(directory)
        bundle.replace(directory)


def _download_once(tag: str, *, force: bool = False) -> str:
    root = _cache_root()
    directory = root / tag
    root.mkdir(parents=True, exist_ok=True)
    with open(root / f"{tag}.lock", "w") as lock:
        # A second process waits here and then uses the finished download.
        fcntl.flock(lock, fcntl.LOCK_EX)
        if force or not _complete(directory, tag):
            print(f"vis-agent: downloading Vis {tag} into {directory}", file=sys.stderr)
            try:
                _download(tag, directory)
            except urllib.error.HTTPError as error:
                if error.code == 404:
                    raise InstallError(
                        f"Vis {tag} has no engine at {error.url}. "
                        "Install the SDK version of a published Vis release."
                    ) from None
                raise InstallError(f"could not download Vis {tag}: {error}") from None
            except (OSError, tarfile.TarError) as error:
                raise InstallError(f"could not download Vis {tag}: {error}") from None
    return str(directory / _COMMAND)


def install(*, force: bool = False) -> str:
    """Download the Vis engine of this SDK now and return its `vis-agent` path.

    Use it to prepare a computer or a container image before the first run. It
    downloads into `$VIS_HOME/sdk/<tag>` even when another Vis is installed, and
    reuses a complete download unless `force` is true. A development build of the
    SDK installs the latest stable release. `VIS_NO_AUTO_INSTALL` does not apply.

    Raises:
        InstallError: The platform has no engine, the release has no bundle, or the
            download fails its checks.
    """
    return _download_once(_sdk_tag() or _latest_tag(), force=force)


def ensure_installed() -> str:
    """Return the `vis-agent` command that matches this SDK, and download it when missing.

    An installed `vis-agent` is used only when it runs the same release as the SDK.
    Otherwise the SDK uses, or downloads, its own copy in `$VIS_HOME/sdk/<tag>`. A
    development build of the SDK uses any installed command, or the latest stable
    release.

    Raises:
        InstallError: The download is turned off with `VIS_NO_AUTO_INSTALL`, or
            `install()` fails.
    """
    tag = _sdk_tag()
    installed = _installed()
    if tag is None and installed:
        return installed
    if tag is not None:
        if installed and _installed_matches(installed, tag):
            return installed
        if _complete(_cache_root() / tag, tag):
            return str(_cache_root() / tag / _COMMAND)
    if (os.environ.get("VIS_NO_AUTO_INSTALL") or "").strip().lower() in _OFF:
        wanted = f"Vis {tag}" if tag else f"the {_COMMAND} command"
        raise InstallError(
            f"{wanted} is not installed, and VIS_NO_AUTO_INSTALL turns off the "
            "download. Install it, call install(), or pass executable=..."
        )
    try:
        return install()
    except OSError as error:
        raise InstallError(f"could not download Vis: {error}") from None
