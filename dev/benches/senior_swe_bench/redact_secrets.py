#!/usr/bin/env python3
"""Redact benchmark credentials from copied text and JSON artifacts."""
from __future__ import annotations

import argparse
import json
import os
import re
from pathlib import Path


ENV_SECRET = re.compile(
    r"^\s*(?:export\s+)?[A-Za-z_][A-Za-z0-9_]*(?:API_KEY|TOKEN|SECRET)\s*=\s*(.*?)\s*$",
    re.MULTILINE,
)
EDN_SECRET = re.compile(r":(?:api-key|token|secret)\s+\"([^\"]+)\"")
DEFAULT_ENV_KEYS = (
    "ANTHROPIC_API_KEY",
    "ANTHROPIC_AUTH_TOKEN",
    "OPENAI_API_KEY",
    "PORTKEY_API_KEY",
    "ZAI_API_KEY",
    "ZAI_CODING_API_KEY",
)
REDACTED = b"<redacted>"


def secrets_from_sources(paths: list[Path]) -> set[bytes]:
    secrets: set[bytes] = set()
    for path in paths:
        if not path.is_file():
            continue
        text = path.read_text(errors="replace")
        for pattern in (ENV_SECRET, EDN_SECRET):
            for value in pattern.findall(text):
                clean = value.strip().strip("\"'")
                if len(clean) >= 8:
                    secrets.add(clean.encode())
    for key in DEFAULT_ENV_KEYS:
        value = os.environ.get(key)
        if value and len(value) >= 8:
            secrets.add(value.encode())
    return secrets


def redact(paths: list[Path], secrets: set[bytes]) -> dict[str, object]:
    changed: list[str] = []
    replacements = 0
    remaining_occurrences = 0
    candidates = []
    for path in paths:
        if path.is_file():
            candidates.append(path)
        elif path.is_dir():
            candidates.extend(item for item in path.rglob("*") if item.is_file() and not item.is_symlink())
    for path in sorted(set(candidates)):
        original = path.read_bytes()
        sanitized = original
        for secret in secrets:
            count = sanitized.count(secret)
            if count:
                replacements += count
                sanitized = sanitized.replace(secret, REDACTED)
        remaining_occurrences += sum(sanitized.count(secret) for secret in secrets)
        if sanitized != original:
            path.write_bytes(sanitized)
            changed.append(str(path))
    return {
        "clean": bool(secrets) and bool(candidates) and remaining_occurrences == 0,
        "changed_files": changed,
        "files_changed": len(changed),
        "remaining_occurrences": remaining_occurrences,
        "replacements": replacements,
        "scanned_files": len(candidates),
        "secret_values_loaded": len(secrets),
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--path", action="append", type=Path, default=[])
    parser.add_argument("--secret-source", action="append", type=Path, default=[])
    parser.add_argument("--out", type=Path)
    args = parser.parse_args()
    result = redact(args.path, secrets_from_sources(args.secret_source))
    text = json.dumps(result, indent=2, sort_keys=True) + "\n"
    if args.out:
        args.out.parent.mkdir(parents=True, exist_ok=True)
        args.out.write_text(text)
    print(text, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
