"""Verify lossless archiving and preservation of interrupted traces."""

import gzip
import hashlib
import json
import shutil
import subprocess

import pytest
from archive_traces import archive_trace
from summarize import trace_summary


def write_trace(folder, contents):
    """Create a finished gzip JSONL trace in a disposable trial directory."""
    folder.mkdir(parents=True, exist_ok=True)
    source = folder / "vis-trace.jsonl.gz"
    with gzip.open(source, "wb") as stream:
        stream.write(contents)
    return source


@pytest.mark.skipif(shutil.which("zstd") is None, reason="zstd executable required")
def test_archiving_verifies_raw_bytes_and_removes_only_complete_gzip(tmp_path):
    frame = (
        json.dumps(
            {
                "event": "trace-chunk",
                "payload": {
                    "phase": "provider-call",
                    "provider": "zai-coding-plan",
                    "model": "glm-5.3-flash",
                },
            }
        )
        + "\n"
    )
    contents = (frame * 30).encode()
    source = write_trace(tmp_path / "agent", contents)
    manifest = archive_trace(source)
    target = source.with_suffix(".zst")
    assert not source.exists()
    assert target.is_file()
    assert manifest["raw_sha256"] == hashlib.sha256(contents).hexdigest()
    assert manifest["raw_bytes"] == len(contents)
    assert json.loads((source.parent / "trace-archive.json").read_text()) == manifest
    summary = trace_summary(source.with_suffix(""), tmp_path)
    assert summary["path"] == "agent/vis-trace.jsonl.zst"
    assert summary["provider_calls"] == {"zai-coding-plan/glm-5.3-flash": 30}
    assert summary["truncated"] is False


def test_incomplete_gzip_keeps_the_original_and_no_archive(tmp_path):
    source = write_trace(tmp_path / "agent", b'{"event":"trace-chunk"}\n')
    source.write_bytes(source.read_bytes()[:-8])
    with pytest.raises(EOFError):
        archive_trace(source)
    assert source.is_file()
    assert not source.with_suffix(".zst").exists()
    assert not (source.parent / "trace-archive.json").exists()


@pytest.mark.skipif(shutil.which("zstd") is None, reason="zstd executable required")
def test_preexisting_archive_is_checked_before_deleting_gzip(tmp_path):
    contents = b'{"event":"trace-chunk"}\n'
    source = write_trace(tmp_path / "agent", contents)
    target = source.with_suffix(".zst")
    target.write_bytes(
        subprocess.run(
            ["zstd", "-q", "-c"], input=contents, capture_output=True, check=True
        ).stdout
    )
    assert archive_trace(source)["trace"] == target.name
    assert not source.exists()
    assert target.is_file()


@pytest.mark.skipif(shutil.which("zstd") is None, reason="zstd executable required")
def test_mismatched_archive_preserves_original_gzip(tmp_path):
    source = write_trace(tmp_path / "agent", b'{"event":"trace-chunk"}\n')
    target = source.with_suffix(".zst")
    target.write_bytes(
        subprocess.run(
            ["zstd", "-q", "-c"], input=b"wrong\n", capture_output=True, check=True
        ).stdout
    )
    with pytest.raises(RuntimeError, match="digests differ"):
        archive_trace(source)
    assert source.is_file()
    assert target.is_file()
    assert not (source.parent / "trace-archive.json").exists()
