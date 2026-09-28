"""Credential-safe streaming and serialization without real provider calls."""

import base64
import gzip
import json
import os
import subprocess
import sys
from pathlib import Path
from urllib.parse import quote

import pytest
from capture_trace import Redactor, capture, credential_variants, redact_value


@pytest.mark.parametrize("size", [1, 2, 7, 31, 65536])
def test_credentials_redacted_across_every_chunk_boundary(size):
    key = 'fixture-credential/with+quotes"\nż'
    variants = credential_variants({"ZAI_CODING_API_KEY": key, "HOME": "/keep"})
    original = b" safe ".join(variants) + b" /keep\n"
    redactor = Redactor(variants)
    parts = []
    for offset in range(0, len(original), size):
        parts.append(redactor.feed(original[offset : offset + size]))
        assert len(redactor.pending) <= redactor.keep
    parts.append(redactor.feed(b"", final=True))
    output = b"".join(parts)
    assert output == b" safe ".join([b"[REDACTED]"] * len(variants)) + b" /keep\n"
    assert redactor.pending == b""


def test_redaction_keeps_nonsecret_bytes_and_numeric_types():
    assert Redactor(()).feed(b"ordinary output") == b"ordinary output"
    variants = credential_variants({"OTHER_API_KEY": "123"})
    assert redact_value({"123": [123, True, None, "123"]}, variants) == {
        "[REDACTED]": [123, True, None, "[REDACTED]"]
    }


@pytest.mark.parametrize("compressed", [False, True])
def test_capture_drains_both_streams_and_preserves_failure(
    tmp_path, monkeypatch, compressed
):
    key = "fixture-credential-without-real-provider"
    monkeypatch.setenv("ZAI_CODING_API_KEY", key)
    out = tmp_path / ("trace.gz" if compressed else "harbor.log")
    err = tmp_path / "stderr.log" if compressed else None
    command = [
        sys.executable,
        "-c",
        "import os, sys; key = os.environ['ZAI_CODING_API_KEY']; "
        "[(os.write(1, ('stdout ' + key + '\\n').encode()), "
        "os.write(2, ('stderr ' + key + '\\n').encode())) for _ in range(5000)]; "
        "sys.exit(7)",
    ]
    assert capture(command, out, err) == 7
    if compressed:
        with gzip.open(out, "rb") as stream:
            stdout = stream.read()
        stderr = err.read_bytes()
    else:
        stdout, stderr = out.read_bytes(), b""
    assert key.encode() not in stdout + stderr
    assert (stdout + stderr).count(b"[REDACTED]") == 10000
    assert b"stdout " in stdout and b"stderr " in stdout + stderr


def test_capture_fails_closed_without_credential(tmp_path, monkeypatch):
    monkeypatch.delenv("ZAI_CODING_API_KEY", raising=False)
    out = tmp_path / "trace.gz"
    with pytest.raises(RuntimeError, match="required for safe benchmark capture"):
        capture(["must-not-run"], out)
    assert not out.exists()


def test_cli_failure_never_prints_sensitive_exception(tmp_path, monkeypatch):
    key = "fixture-secret-in-missing-command"
    monkeypatch.setenv("ZAI_CODING_API_KEY", key)
    script = Path(__file__).parents[1] / "capture_trace.py"
    result = subprocess.run(
        [
            sys.executable,
            str(script),
            "--stdout",
            str(tmp_path / "trace.gz"),
            "--",
            key,
        ],
        capture_output=True,
        timeout=10,
    )
    assert result.returncode == 1
    assert key.encode() not in result.stdout + result.stderr
    assert b"Credential-safe benchmark capture failed" in result.stderr


def test_serialized_credentials_are_redacted_in_reports(monkeypatch):
    key = 'fixture/secret+"quoted"'
    monkeypatch.setenv("ZAI_CODING_API_KEY", key)
    value = {
        "plain": key,
        "json": json.dumps(key),
        "url": quote(key, safe=""),
        "base64": base64.b64encode(key.encode()).decode(),
    }
    assert redact_value(value) == {
        "plain": "[REDACTED]",
        "json": '"[REDACTED]"',
        "url": "[REDACTED]",
        "base64": "[REDACTED]",
    }


def test_console_pytest_collects_without_pythonpath():
    # CI invokes the console entrypoint, unlike the language tool's module runner.
    environment = {
        key: value for key, value in os.environ.items() if key != "PYTHONPATH"
    }
    result = subprocess.run(
        [
            str(Path(sys.executable).with_name("pytest")),
            "--collect-only",
            "-q",
            "tests",
        ],
        cwd=Path(__file__).parents[1],
        env=environment,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "tests collected" in result.stdout
