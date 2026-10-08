#!/usr/bin/env python3
"""Write SHA256SUMS for every uploaded asset of a draft release (#332).

GitHub records the SHA-256 digest of each asset at upload. The file lists each
asset once, in `sha256sum` format, so `sha256sum --check` verifies downloads.
An asset without a digest stops publication: no release ships unverifiable."""

import argparse
import json
import re
import sys
from pathlib import Path

SUMS = "SHA256SUMS"


def checksum_lines(release: dict) -> list[str]:
    lines = []
    for asset in release.get("assets", []):
        name = asset.get("name", "")
        if name == SUMS:
            continue
        if asset.get("state") != "uploaded" or asset.get("size", 0) <= 0:
            raise ValueError(f"asset is not completely uploaded: {name}")
        digest = asset.get("digest") or ""
        match = re.fullmatch(r"sha256:([0-9a-f]{64})", digest)
        if not match:
            raise ValueError(f"asset has no SHA-256 digest: {name}")
        lines.append(f"{match.group(1)}  {name}")
    if not lines:
        raise ValueError("the release has no assets")
    return sorted(lines, key=lambda line: line.split("  ", 1)[1])


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("metadata", type=Path, help="GitHub release API JSON")
    parser.add_argument("output", type=Path, help=f"path of the {SUMS} file to write")
    args = parser.parse_args()
    try:
        with args.metadata.open() as source:
            lines = checksum_lines(json.load(source))
    except (ValueError, TypeError, KeyError, OSError) as error:
        parser.exit(1, f"Checksums refused: {error}\n")
    args.output.write_text("\n".join(lines) + "\n")
    print(f"{SUMS}: {len(lines)} assets", file=sys.stderr)


if __name__ == "__main__":
    main()
