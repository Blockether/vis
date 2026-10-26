"""Write the requirements that check shared packages against a project's ranges (Blockether/vis#391).

Every extension lock installs into the same shared packages, so an exact lock pin
fails as soon as another lock installs another version. This script keeps the set of
locked packages but replaces each registry pin with the declared ranges:

- A registry package becomes its bare name.
- A git, archive or local package keeps its exact locked source, so uv stays offline.
- The ranges of the project and its workspace members come from `uv.lock`.

Arguments: PYLOCK UV_LOCK REQUIREMENTS_OUT OVERRIDES_OUT [SKIPPED_PACKAGE ...]
"""

import re
import sys
import tomllib

LOCAL_SOURCES = ("virtual", "editable", "directory")
SOURCE_KEYS = ("git", "url", "path", "editable", "directory", "workspace")


def key(name):
    return re.sub(r"[-_.]+", "-", name).lower()


def suffix(marker):
    return f" ; {marker}" if marker else ""


def locked_lines(pylock, skipped):
    """Answer the requirement lines and the exact source overrides of a pylock."""
    lines, overrides = [], []
    with open(pylock, "rb") as f:
        packages = tomllib.load(f).get("packages", [])
    for package in packages:
        name = package["name"]
        if key(name) in skipped:
            continue
        marker = suffix(package.get("marker"))
        if "vcs" in package:
            vcs = package["vcs"]
            line = f"{name} @ {vcs['type']}+{vcs['url']}@{vcs['commit-id']}{marker}"
            lines.append(line)
            # A dependency can name the same repository by tag; the commit keeps uv offline.
            overrides.append(line)
        elif "archive" in package:
            archive = package["archive"]
            line = f"{name} @ {archive.get('url') or archive['path']}{marker}"
            lines.append(line)
            overrides.append(line)
        elif "directory" in package:
            directory = package["directory"]
            editable = "-e " if directory.get("editable") else ""
            lines.append(f"{editable}{directory['path']}{marker}")
        else:
            lines.append(name + marker)
    return lines, overrides


def declared_ranges(uv_lock, skipped):
    """Answer the registry requirements that local packages declare in `uv.lock`."""
    with open(uv_lock, "rb") as f:
        packages = tomllib.load(f).get("package", [])
    lines = []
    for package in packages:
        if not any(k in package.get("source", {}) for k in LOCAL_SOURCES):
            continue
        metadata = package.get("metadata", {})
        entries = list(metadata.get("requires-dist", []))
        for group in metadata.get("requires-dev", {}).values():
            entries += group
        for entry in entries:
            if key(entry["name"]) in skipped or "specifier" not in entry:
                continue
            if any(k in entry for k in SOURCE_KEYS):
                continue
            extras = ",".join(entry.get("extras", []))
            extras = f"[{extras}]" if extras else ""
            lines.append(
                entry["name"]
                + extras
                + entry["specifier"]
                + suffix(entry.get("marker"))
            )
    return lines


def main(argv):
    pylock, uv_lock, requirements_out, overrides_out, *skipped = argv
    skipped = {key(name) for name in skipped}
    lines, overrides = locked_lines(pylock, skipped)
    lines += declared_ranges(uv_lock, skipped)
    with open(requirements_out, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")
    with open(overrides_out, "w", encoding="utf-8") as f:
        f.write("\n".join(overrides) + "\n")


if __name__ == "__main__":
    main(sys.argv[1:])
