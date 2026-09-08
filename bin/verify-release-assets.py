#!/usr/bin/env python3
"""Refuse stable publication until every supported product artifact is uploaded."""

import argparse
import json
import re
from pathlib import Path


def required_assets(tag: str) -> set[str]:
    if not re.fullmatch(r"v[0-9]+\.[0-9]+\.[0-9]+", tag):
        raise ValueError("expected an immutable vX.Y.Z release tag")
    version = tag[1:]
    names = {"install-vis-agent", "vis-agent"}
    for platform in ("linux-x64", "linux-arm64", "macos-arm64"):
        names.update(
            f"{product}-{platform}.tar.gz" for product in ("vis-agent", "vis-tui")
        )
    names.add(f"vis-companion-{version}-macos-universal.dmg")
    for arch in ("x64", "arm64"):
        names.update(
            f"vis-companion-{version}-linux-{arch}.{ext}" for ext in ("deb", "AppImage")
        )
    names.update(
        (f"vis-companion-{version}-ios.ipa", f"vis-companion-{version}-android.aab")
    )
    return names


def verify_release(release: dict, tag: str, *, draft: bool = True) -> set[str]:
    expected = required_assets(tag)
    if release.get("tag_name") != tag:
        raise ValueError("release tag does not match the verified source")
    if release.get("draft") is not draft or release.get("prerelease") is not False:
        state = "draft" if draft else "published release"
        raise ValueError(
            f"expected a stable {state}; published versions must not be modified"
        )
    uploaded = {
        asset["name"]
        for asset in release.get("assets", [])
        if asset.get("state") == "uploaded" and asset.get("size", 0) > 0
    }
    missing = expected - uploaded
    if missing:
        raise ValueError(
            "missing or incomplete release assets: " + ", ".join(sorted(missing))
        )
    return expected


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("metadata", type=Path, help="GitHub release API JSON")
    parser.add_argument("tag")
    parser.add_argument(
        "--published",
        action="store_true",
        help="verify published installer source, not draft promotion",
    )
    args = parser.parse_args()
    try:
        with args.metadata.open() as source:
            count = len(
                verify_release(json.load(source), args.tag, draft=not args.published)
            )
    except (ValueError, TypeError, KeyError, AttributeError) as error:
        parser.exit(1, f"Release refused: {error}\n")
    print(f"Complete stable release {args.tag}: {count} required assets uploaded.")


if __name__ == "__main__":
    main()
