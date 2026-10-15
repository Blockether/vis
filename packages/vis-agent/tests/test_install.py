"""The SDK runs the Vis engine of its own release and installs only that engine.

The tests serve fake release assets, so they need no network.
"""

import hashlib
import io
import json
import tarfile
import urllib.error

import pytest
from blockether.vis.engine import (
    InstallError,
    LocalEngine,
    TransportError,
    _install,
    ensure_installed,
    install,
)

TAG = "v9.9.9"
SHA = "a" * 40
ASSET = "vis-agent-linux-x64.tar.gz"
BASE = f"https://github.com/Blockether/vis/releases/download/{TAG}"


def _bundle(stamp=f"9.9.9 {SHA} release 2026-01-01T00:00:00Z"):
    """Return a gzip tar with the layout of `bin/stage-release-bundle`."""
    files = {
        "vis-agent": (b"#!/bin/sh\n", 0o755),
        "vis-agent-native": (b"#!/bin/sh\n", 0o755),
        "vis-agent-native.build": (f"{stamp}\n".encode(), 0o644),
        "install-vis-agent": (b"#!/bin/sh\n", 0o755),
        "vis-agent-python/libvispython.so": (b"", 0o644),
        "vis-agent-python/python/bin/uv": (b"", 0o755),
    }
    buffer = io.BytesIO()
    with tarfile.open(fileobj=buffer, mode="w:gz") as tar:
        for name, (data, mode) in files.items():
            info = tarfile.TarInfo(f"./{name}")
            info.size = len(data)
            info.mode = mode
            tar.addfile(info, io.BytesIO(data))
    return buffer.getvalue()


@pytest.fixture
def isolated(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path / "home"))
    monkeypatch.setenv("VIS_HOME", str(tmp_path / "vis"))
    monkeypatch.setenv("PATH", str(tmp_path / "path"))
    monkeypatch.delenv("VIS_NO_AUTO_INSTALL", raising=False)
    monkeypatch.setattr(_install, "_sdk_tag", lambda: TAG)
    monkeypatch.setattr(_install, "_platform_asset", lambda: ASSET)
    return tmp_path


def _serve(monkeypatch, bundle=None, digest=None, missing=()):
    bundle = _bundle() if bundle is None else bundle
    digest = digest or hashlib.sha256(bundle).hexdigest()
    files = {
        "SHA256SUMS": f"{digest}  {ASSET}\n0000  vis-tui-linux-x64.tar.gz\n".encode(),
        ASSET: bundle,
    }
    requested = []

    def open_url(url):
        requested.append(url)
        name = url.rsplit("/", 1)[-1]
        if name in missing or name not in files:
            raise urllib.error.HTTPError(url, 404, "Not Found", None, None)
        return io.BytesIO(files[name])

    monkeypatch.setattr(_install, "_open", open_url)
    return requested


def _installed_vis(root, stamp, track=None):
    """Install a launcher on PATH with a native engine in `$VIS_HOME/install`."""
    command = root / "path" / "vis-agent"
    command.parent.mkdir(parents=True, exist_ok=True)
    command.write_text("#!/bin/sh\n")
    command.chmod(0o755)
    install_dir = root / "vis" / "install"
    install_dir.mkdir(parents=True, exist_ok=True)
    native = install_dir / "vis-agent-native"
    native.write_text("#!/bin/sh\n")
    native.chmod(0o755)
    (install_dir / "vis-agent-native.build").write_text(f"{stamp}\n")
    if track:
        (install_dir / "track").write_text(f"{track}\n")
    return str(command)


def test_installed_vis_of_the_same_release_is_used_without_download(
    isolated, monkeypatch
):
    command = _installed_vis(isolated, f"9.9.9 {SHA} release now", track="release")
    requested = _serve(monkeypatch)
    assert ensure_installed() == command
    assert requested == []


@pytest.mark.parametrize(
    ("stamp", "track"),
    [
        (f"9.9.8 {SHA} release now", "release"),
        (f"9.9.9 {SHA} beta now", "beta"),
        (f"9.9.9 {SHA} release now", "dev"),
    ],
    ids=["other-version", "beta-build", "dev-track"],
)
def test_installed_vis_of_another_build_gets_the_sdk_release(
    isolated, monkeypatch, stamp, track
):
    command = _installed_vis(isolated, stamp, track=track)
    _serve(monkeypatch)
    path = ensure_installed()
    assert path != command
    assert path == str(isolated / "vis" / "sdk" / TAG / "vis-agent")


def test_missing_command_downloads_only_the_engine_bundle_once(isolated, monkeypatch):
    requested = _serve(monkeypatch)
    path = ensure_installed()
    directory = isolated / "vis" / "sdk" / TAG
    assert path == str(directory / "vis-agent")
    # No terminal client, web app or desktop app: only the engine of this platform.
    assert requested == [f"{BASE}/SHA256SUMS", f"{BASE}/{ASSET}"]
    assert sorted(entry.name for entry in directory.iterdir()) == [
        "install-vis-agent",
        "vis-agent",
        "vis-agent-native",
        "vis-agent-native.build",
        "vis-agent-python",
        "vis-agent.pinned",
    ]
    # The launcher runs the engine beside it, whatever track the user selected.
    assert (directory / "vis-agent.pinned").read_text() == f"{TAG}\n"
    requested.clear()
    assert ensure_installed() == path
    assert requested == []


def test_checksum_mismatch_installs_nothing(isolated, monkeypatch):
    _serve(monkeypatch, digest="0" * 64)
    with pytest.raises(InstallError, match="does not match SHA256SUMS"):
        ensure_installed()
    assert not (isolated / "vis" / "sdk" / TAG).exists()


def test_bundle_of_another_release_installs_nothing(isolated, monkeypatch):
    _serve(monkeypatch, bundle=_bundle(f"9.9.8 {SHA} release now"))
    with pytest.raises(InstallError, match="build stamp of another release"):
        ensure_installed()
    assert not (isolated / "vis" / "sdk" / TAG).exists()


def test_release_without_an_engine_never_falls_back_to_another_version(
    isolated, monkeypatch
):
    requested = _serve(monkeypatch, missing=("SHA256SUMS",))
    monkeypatch.setattr(_install, "_latest_tag", lambda: pytest.fail("no fallback"))
    with pytest.raises(InstallError, match=f"Vis {TAG} has no engine"):
        ensure_installed()
    assert requested == [f"{BASE}/SHA256SUMS"]


def test_opt_out_reports_the_missing_release(isolated, monkeypatch):
    monkeypatch.setenv("VIS_NO_AUTO_INSTALL", "1")
    requested = _serve(monkeypatch)
    with pytest.raises(InstallError, match=f"Vis {TAG} is not installed.*install()"):
        ensure_installed()
    assert requested == []


def test_install_downloads_now_and_force_downloads_again(isolated, monkeypatch):
    _installed_vis(isolated, f"9.9.9 {SHA} release now", track="release")
    monkeypatch.setenv("VIS_NO_AUTO_INSTALL", "1")
    requested = _serve(monkeypatch)
    path = install()
    assert path == str(isolated / "vis" / "sdk" / TAG / "vis-agent")
    assert len(requested) == 2
    assert install() == path
    assert len(requested) == 2
    assert install(force=True) == path
    assert len(requested) == 4


def test_local_engine_reports_a_failed_download(isolated, monkeypatch):
    monkeypatch.setenv("VIS_NO_AUTO_INSTALL", "1")
    engine = LocalEngine(root=isolated)
    with pytest.raises(
        TransportError, match="could not start local engine: .*VIS_NO_AUTO_INSTALL"
    ):
        engine.connect()
    engine.close()


def test_development_build_uses_any_installed_command(isolated, monkeypatch):
    monkeypatch.setattr(_install, "_sdk_tag", lambda: None)
    command = _installed_vis(isolated, f"9.9.8 {SHA} release now", track="dev")
    requested = _serve(monkeypatch)
    assert ensure_installed() == command
    assert requested == []


class _Distribution:
    def __init__(self, version, direct_url=None):
        self.version = version
        self._direct_url = direct_url

    def read_text(self, name):
        return json.dumps(self._direct_url) if self._direct_url else None


@pytest.mark.parametrize(
    ("version", "direct_url", "embedded", "expected"),
    [
        ("0.2.46", None, None, "v0.2.46"),
        ("0.2.47.dev12", None, f"beta-{SHA}", f"beta-{SHA}"),
        ("0.2.47.dev12", None, None, None),
        ("0.2.46", {"dir_info": {"editable": True}}, None, None),
    ],
    ids=["release", "beta", "unrecorded-dev", "editable"],
)
def test_sdk_tag(monkeypatch, version, direct_url, embedded, expected):
    monkeypatch.setattr(
        _install.importlib.metadata,
        "distribution",
        lambda _: _Distribution(version, direct_url),
    )
    monkeypatch.setattr(_install, "_embedded_tag", lambda: embedded)
    assert _install._sdk_tag() == expected
