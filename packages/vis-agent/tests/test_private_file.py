"""Secret files and refresh logs for extension authors, without a running engine."""

import json
import os
import stat
import threading
import time

import blockether.vis.extension as vis
import pytest


def _mode(path):
    return stat.S_IMODE(os.stat(path).st_mode)


def test_write_private_file_replaces_a_readable_file_with_an_owner_only_one(tmp_path):
    path = tmp_path / "auth.json"
    path.write_text("old")
    path.chmod(0o644)
    vis.write_private_file(path, "")
    assert path.read_text() == ""
    for i in range(20):
        vis.write_private_file(str(path), f"token {i} \u2713")
    assert path.read_text(encoding="utf-8") == "token 19 \u2713"
    assert _mode(path) == 0o600
    assert sorted(p.name for p in tmp_path.iterdir()) == ["auth.json"]


def test_write_private_file_never_shows_a_reader_a_partial_file(tmp_path):
    path = tmp_path / "auth.json"
    alpha, bravo = "a" * 400_000, "b" * 400_000
    vis.write_private_file(path, alpha)
    torn, stop = [], threading.Event()

    def read():
        while not stop.is_set():
            if path.read_text() not in (alpha, bravo):
                torn.append(True)

    reader = threading.Thread(target=read)
    reader.start()
    for i in range(40):
        vis.write_private_file(path, alpha if i % 2 else bravo)
    stop.set()
    reader.join()
    assert torn == []


@pytest.mark.skipif(os.geteuid() == 0, reason="root writes into a read-only directory")
def test_write_private_file_keeps_the_previous_file_when_the_replace_fails(tmp_path):
    path = tmp_path / "auth.json"
    vis.write_private_file(path, "previous")
    tmp_path.chmod(0o500)
    try:
        with pytest.raises(PermissionError):
            vis.write_private_file(path, "next")
    finally:
        tmp_path.chmod(0o700)
    assert path.read_text() == "previous"
    assert sorted(p.name for p in tmp_path.iterdir()) == ["auth.json"]


def test_write_private_file_writes_through_a_symbolic_link(tmp_path):
    target, link = tmp_path / "real.json", tmp_path / "link.json"
    target.write_text("old")
    link.symlink_to(target)
    vis.write_private_file(link, "new")
    assert link.is_symlink()
    assert target.read_text() == "new"
    dangling = tmp_path / "dangling.json"
    dangling.symlink_to(tmp_path / "missing.json")
    with pytest.raises(FileNotFoundError):
        vis.write_private_file(dangling, "new")


def test_write_private_file_refuses_a_missing_parent_and_non_text(tmp_path):
    with pytest.raises(FileNotFoundError):
        vis.write_private_file(tmp_path / "missing" / "auth.json", "x")
    with pytest.raises(TypeError):
        vis.write_private_file(tmp_path / "auth.json", b"x")
    assert list(tmp_path.iterdir()) == []


def _logged(monkeypatch, **kwargs):
    lines = []
    monkeypatch.setattr(
        vis._host, "log", lambda level, message: lines.append((level, message))
    )
    vis.log_token_refresh(**kwargs)
    ((level, message),) = lines
    return level, message, json.loads(message.split(" ", 6)[-1])


def test_log_token_refresh_names_the_process_and_hides_the_tokens(monkeypatch):
    sent, received = "old-secret-refresh-token-0123", "next-secret-refresh-token-9876"
    level, message, record = _logged(
        monkeypatch,
        store="example",
        outcome="persist-failed",
        trigger="rejected-token",
        started=time.monotonic(),
        status=200,
        error=OSError("disk full"),
        refresh_token=sent,
        next_refresh_token=received,
    )
    assert level == "error"
    assert message.startswith("OAuth token refresh of example: persist-failed ")
    assert {
        k: record[k] for k in ("store", "outcome", "trigger", "pid", "role", "status")
    } == {
        "store": "example",
        "outcome": "persist-failed",
        "trigger": "rejected-token",
        "pid": os.getpid(),
        "role": "extension",
        "status": 200,
    }
    assert record["duration-ms"] >= 0
    assert record["error-class"] == "builtins.OSError"
    assert record["refresh-token-sha"] != record["next-refresh-token-sha"]
    assert len(record["refresh-token-sha"]) == 8
    assert "secret" not in message and "disk full" not in message


def test_log_token_refresh_fingerprints_one_token_alike(monkeypatch):
    first = _logged(
        monkeypatch, store="s", outcome="refreshed", refresh_token="token-a"
    )
    second = _logged(
        monkeypatch, store="s", outcome="refreshed", refresh_token="token-a"
    )
    assert first[2]["refresh-token-sha"] == second[2]["refresh-token-sha"]
    assert first[0] == "info"
    assert (
        "refresh-token-sha" not in _logged(monkeypatch, store="s", outcome="failed")[2]
    )


def test_log_token_refresh_refuses_an_unknown_outcome_or_trigger():
    with pytest.raises(ValueError):
        vis.log_token_refresh("s", "ok")
    with pytest.raises(ValueError):
        vis.log_token_refresh("s", "refreshed", trigger="rejected_token")
