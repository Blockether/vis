"""The SDK finds `vis-agent`, or downloads the matching release, without network in tests."""

import hashlib
import os

import pytest
from blockether.vis.engine import LocalEngine, TransportError, _install

# The fake release launcher records its arguments and VIS_HOME, then "installs" the engine.
LAUNCHER = b"""#!/bin/sh
here="${0%/*}"
printf '%s\\n' "$*" > "$here/args"
printf '%s\\n' "$VIS_HOME" > "$here/vis-home"
: > "$here/vis-agent-native"
: > "$here/vis-agent-native.build"
"""


@pytest.fixture
def isolated(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path / "home"))
    monkeypatch.setenv("VIS_HOME", str(tmp_path / "vis"))
    monkeypatch.setenv("PATH", str(tmp_path / "empty-path"))
    monkeypatch.delenv("VIS_NO_AUTO_INSTALL", raising=False)
    monkeypatch.setattr(_install, "_sdk_tag", lambda: "v9.9.9")
    return tmp_path


def _serve(monkeypatch, launcher=LAUNCHER, digest=None):
    digest = digest or hashlib.sha256(launcher).hexdigest()
    files = {
        "vis-agent": launcher,
        "SHA256SUMS": f"{digest}  vis-agent\n".encode(),
    }
    requested = []

    def fetch(url):
        requested.append(url)
        return files[url.rsplit("/", 1)[-1]]

    monkeypatch.setattr(_install, "_fetch", fetch)
    return requested


def test_installed_command_is_used_without_download(isolated, monkeypatch):
    command = isolated / "empty-path" / "vis-agent"
    command.parent.mkdir()
    command.write_text("#!/bin/sh\n")
    command.chmod(0o755)
    requested = _serve(monkeypatch)
    assert _install.ensure_vis_agent() == str(command)
    assert requested == []


def test_missing_command_downloads_the_sdk_release_once(isolated, monkeypatch):
    requested = _serve(monkeypatch)
    path = _install.ensure_vis_agent()
    directory = isolated / "vis" / "sdk" / "v9.9.9"
    assert path == str(directory / "vis-agent")
    assert requested == [
        "https://github.com/Blockether/vis/releases/download/v9.9.9/vis-agent",
        "https://github.com/Blockether/vis/releases/download/v9.9.9/SHA256SUMS",
    ]
    # The update gets the pinned tag, no extra components and a private VIS_HOME.
    assert (directory / "args").read_text().split() == [
        "update",
        "--track",
        "release",
        "v9.9.9",
        "--components",
        "none",
    ]
    assert (directory / "vis-home").read_text().strip() == str(directory / "home")
    requested.clear()
    assert _install.ensure_vis_agent() == path
    assert requested == []


def test_checksum_mismatch_runs_nothing(isolated, monkeypatch):
    _serve(monkeypatch, digest="0" * 64)
    with pytest.raises(_install.InstallError, match="does not match SHA256SUMS"):
        _install.ensure_vis_agent()
    assert not (isolated / "vis" / "sdk" / "v9.9.9" / "vis-agent").exists()


def test_opt_out_reports_the_missing_command(isolated, monkeypatch):
    monkeypatch.setenv("VIS_NO_AUTO_INSTALL", "1")
    requested = _serve(monkeypatch)
    with pytest.raises(_install.InstallError, match="VIS_NO_AUTO_INSTALL"):
        _install.ensure_vis_agent()
    assert requested == []


def test_local_engine_reports_a_failed_download(isolated, monkeypatch):
    monkeypatch.setenv("VIS_NO_AUTO_INSTALL", "1")
    engine = LocalEngine(root=isolated)
    with pytest.raises(
        TransportError, match="could not start local engine: .*VIS_NO_AUTO_INSTALL"
    ):
        engine.connect()
    engine.close()


def test_development_build_uses_the_latest_release(monkeypatch):
    monkeypatch.setattr(
        _install.importlib.metadata, "version", lambda _: "0.2.47.dev12"
    )
    assert _install._sdk_tag() is None
    monkeypatch.setattr(_install.importlib.metadata, "version", lambda _: "0.2.46")
    assert _install._sdk_tag() == "v0.2.46"
    assert os.name == "posix"


def test_sdk_version_without_assets_falls_back_to_latest(isolated, monkeypatch):
    import urllib.error

    served = _serve(monkeypatch)
    inner = _install._fetch

    def fetch(url):
        if "/v9.9.9/" in url:
            raise urllib.error.HTTPError(url, 404, "Not Found", None, None)
        return inner(url)

    monkeypatch.setattr(_install, "_fetch", fetch)
    monkeypatch.setattr(_install, "_latest_tag", lambda: "v9.9.10")
    path = _install.ensure_vis_agent()
    assert path == str(isolated / "vis" / "sdk" / "v9.9.10" / "vis-agent")
    assert served[-1].endswith("/v9.9.10/SHA256SUMS")
