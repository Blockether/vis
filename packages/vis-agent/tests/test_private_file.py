"""Secret files for extension authors, without a running engine."""

import os
import stat
import threading

import blockether.vis.extension as vis
import pytest


def _mode(path):
    return stat.S_IMODE(os.stat(path).st_mode)


def test_write_private_file_replaces_a_readable_file_with_an_owner_only_one(tmp_path):
    path = tmp_path / "auth.json"
    path.write_text("old")
    path.chmod(0o644)
    vis.write_private_file(str(path), "token \u2713")
    assert path.read_text(encoding="utf-8") == "token \u2713"
    assert _mode(path) == 0o600
    assert sorted(p.name for p in tmp_path.iterdir()) == ["auth.json"]


def test_write_private_file_creates_missing_parents_owner_only(tmp_path):
    path = tmp_path / "example" / "nested" / "auth.json"
    vis.write_private_file(path, "token")
    assert path.read_text() == "token"
    assert _mode(path.parent) == _mode(path.parent.parent) == 0o700


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
    dangling.symlink_to(tmp_path / "missing" / "auth.json")
    with pytest.raises(FileNotFoundError):
        vis.write_private_file(dangling, "new")
    assert not (tmp_path / "missing").exists()
