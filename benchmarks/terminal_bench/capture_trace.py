"""Capture benchmark output with credential redaction before bytes reach disk."""

from __future__ import annotations

import argparse
import base64
import gzip
import json
import os
import re
import selectors
import subprocess
import sys
from contextlib import ExitStack
from pathlib import Path
from urllib.parse import quote, quote_plus

REDACTED = b"[REDACTED]"


def credential_variants(environ=None) -> tuple[bytes, ...]:
    """Recognize environment credentials and common serialized representations."""
    environ = os.environ if environ is None else environ
    variants = set()
    for name, value in environ.items():
        if not value or not re.search(
            r"KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL", name.upper()
        ):
            continue
        raw = value.encode("utf-8")
        variants.update(
            (
                raw,
                json.dumps(value, ensure_ascii=True)[1:-1].encode("utf-8"),
                json.dumps(value, ensure_ascii=False)[1:-1].encode("utf-8"),
                repr(value)[1:-1].encode("utf-8"),
                quote(value, safe="").encode("ascii"),
                quote_plus(value, safe="").encode("ascii"),
                base64.b64encode(raw),
                base64.urlsafe_b64encode(raw),
            )
        )
    return tuple(sorted(variants, key=lambda value: (-len(value), value)))


class Redactor:
    """Retain only the suffix that could start a credential split across reads."""

    def __init__(self, variants: tuple[bytes, ...]):
        self.pattern = (
            re.compile(b"|".join(map(re.escape, variants))) if variants else None
        )
        self.keep = max(map(len, variants), default=1) - 1
        self.pending = b""

    def feed(self, chunk: bytes, *, final: bool = False) -> bytes:
        if self.pattern is None:
            return chunk
        data = self.pending + chunk
        boundary = len(data) if final else max(0, len(data) - self.keep)
        output = []
        cursor = 0
        for match in self.pattern.finditer(data):
            if match.start() >= boundary:
                break
            output.extend((data[cursor : match.start()], REDACTED))
            cursor = match.end()
        boundary = max(boundary, cursor)
        output.append(data[cursor:boundary])
        self.pending = data[boundary:]
        return b"".join(output)


def redact_value(value, variants=None):
    """Redact strings and mapping keys without changing numeric report fields."""
    variants = credential_variants() if variants is None else variants
    if isinstance(value, str):
        return (
            Redactor(variants).feed(value.encode("utf-8"), final=True).decode("utf-8")
        )
    if isinstance(value, dict):
        return {
            redact_value(key, variants): redact_value(item, variants)
            for key, item in value.items()
        }
    if isinstance(value, list):
        return [redact_value(item, variants) for item in value]
    return value


def capture(command, stdout_path: Path, stderr_path: Path | None = None, *, cwd=None):
    """Drain both pipes, preserve exit status and never persist unredacted output."""
    variants = credential_variants()
    if not os.environ.get("ZAI_CODING_API_KEY"):
        raise RuntimeError("ZAI_CODING_API_KEY is required for safe benchmark capture")
    with ExitStack() as stack:
        if stdout_path.suffix == ".gz":
            stdout = stack.enter_context(gzip.open(stdout_path, "wb", compresslevel=1))
        else:
            stdout = stack.enter_context(stdout_path.open("wb"))
        stderr = stack.enter_context(stderr_path.open("wb")) if stderr_path else None
        selector = stack.enter_context(selectors.DefaultSelector())
        process = stack.enter_context(
            subprocess.Popen(
                command,
                cwd=cwd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE if stderr else subprocess.STDOUT,
            )
        )
        selector.register(
            process.stdout, selectors.EVENT_READ, (stdout, Redactor(variants))
        )
        if stderr:
            selector.register(
                process.stderr, selectors.EVENT_READ, (stderr, Redactor(variants))
            )
        try:
            while selector.get_map():
                for key, _ in selector.select():
                    chunk = os.read(key.fd, 65536)
                    destination, redactor = key.data
                    destination.write(redactor.feed(chunk, final=not chunk))
                    destination.flush()
                    if not chunk:
                        selector.unregister(key.fileobj)
            status = process.wait()
            return status if status >= 0 else 128 - status
        finally:
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--stdout", required=True, type=Path)
    parser.add_argument("--stderr", type=Path)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not command:
        parser.error("a command is required")
    try:
        return capture(command, args.stdout, args.stderr)
    except Exception:
        # An exception may itself contain credentials (for example in a path).
        print("Credential-safe benchmark capture failed", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
