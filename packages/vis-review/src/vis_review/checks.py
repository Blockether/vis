"""Review checks of a change: rules on the added lines and the language tools.

`diff_checks` reads only the diffs. `language_checks` asks the language tools
through `call(tool, verb, **arguments)`, which each door gives: an extension
calls `vis.tools`, an SDK program imports the tool packages. A tool that does not
run gives one `info` finding instead of a failure.
"""

from __future__ import annotations

import os
import re
from collections.abc import Callable, Iterable, Sequence
from dataclasses import dataclass

from vis_review.journal import FileDiff

LEVELS = ("error", "warning", "info")
# Added or changed content above this size is worth a look in a review.
LARGE_BYTES = 1024 * 1024

# Each language tool and the file suffixes it checks.
LANGUAGES = {
    "py": (".py", ".pyi"),
    "clj": (".clj", ".cljs", ".cljc", ".bb"),
}

_CONFLICT = re.compile(r"^(<{7} |={7}$|>{7} )")
_SECRETS = (
    ("AWS access key", re.compile(r"\bAKIA[0-9A-Z]{16}\b")),
    ("private key", re.compile(r"-----BEGIN (?:[A-Z]+ )?PRIVATE KEY-----")),
    (
        "GitHub token",
        re.compile(r"\b(?:gh[pousr]_[A-Za-z0-9]{36}|github_pat_[A-Za-z0-9_]{22,})"),
    ),
    ("Slack token", re.compile(r"\bxox[abprs]-[A-Za-z0-9-]{10,}")),
    ("API key", re.compile(r"\bsk-(?:ant-|proj-)?[A-Za-z0-9_-]{20,}")),
    ("Google API key", re.compile(r"\bAIza[0-9A-Za-z_-]{35}\b")),
)
_DEBUG = {
    (".py",): re.compile(r"\bbreakpoint\(\)|\bpdb\.set_trace\(|^\s*import i?pdb\b"),
    (".js", ".jsx", ".ts", ".tsx", ".mjs", ".cjs"): re.compile(
        r"\bconsole\.log\(|^\s*debugger;?\s*$"
    ),
    (".clj", ".cljs", ".cljc", ".bb"): re.compile(r"\(prn\s"),
}
_TODO = re.compile(r"\b(TODO|FIXME|XXX|HACK)\b")
_TRAILING = re.compile(r"[ \t]+$")
_HUNK = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@")


@dataclass(frozen=True)
class Finding:
    """One thing a review check found."""

    check: str
    level: str
    path: str
    line: int  # 1-based line in the new file; 0 for the whole file
    message: str
    source: str = "diff"  # diff, language or ai

    def __post_init__(self):
        if self.level not in LEVELS:
            raise ValueError(f"level must be one of {LEVELS}, not {self.level!r}")


def added_lines(patch: str) -> list[tuple[int, str]]:
    """`(line number in the new file, text)` of each line a unified patch adds."""
    found, line = [], 0
    for row in patch.splitlines():
        hunk = _HUNK.match(row)
        if hunk:
            line = int(hunk.group(1))
        elif row.startswith("+++") or row.startswith("---") or not line:
            continue
        elif row.startswith("+"):
            found.append((line, row[1:]))
            line += 1
        elif row.startswith(" "):
            line += 1
    return found


def diff_checks(
    diffs: Iterable[FileDiff], content: Callable[[str], bytes | None]
) -> list[Finding]:
    """Findings on the added lines and the new content of each changed file.

    `content(digest)` gives a file content from the journal store.
    """
    findings: list[Finding] = []
    for diff in diffs:
        if diff.status == "deleted":
            continue
        suffix = os.path.splitext(diff.path)[1].lower()
        data = content(diff.after) if diff.after else None
        if data is not None and len(data) > LARGE_BYTES:
            findings.append(
                Finding(
                    "large-file",
                    "warning",
                    diff.path,
                    0,
                    f"The file is {len(data) // 1024} KiB.",
                )
            )
        if diff.is_binary:
            if diff.status == "added":
                findings.append(
                    Finding(
                        "binary-file", "info", diff.path, 0, "A binary file is added."
                    )
                )
            continue
        if data and not data.endswith(b"\n"):
            findings.append(
                Finding(
                    "final-newline",
                    "warning",
                    diff.path,
                    0,
                    "The file does not end with a newline.",
                )
            )
        debug = next(
            (rule for suffixes, rule in _DEBUG.items() if suffix in suffixes), None
        )
        for line, text in added_lines(diff.patch):
            findings.extend(_line_findings(diff.path, suffix, line, text, debug))
    return findings


def _line_findings(path, suffix, line, text, debug):
    if _CONFLICT.match(text):
        yield Finding(
            "conflict-marker", "error", path, line, "A merge conflict marker is added."
        )
    for name, rule in _SECRETS:
        if rule.search(text):
            yield Finding(
                "secret", "error", path, line, f"The line looks like a {name}."
            )
    if debug is not None and debug.search(text):
        yield Finding(
            "debug-leftover",
            "warning",
            path,
            line,
            "Debugging code is added: " + text.strip()[:80],
        )
    todo = _TODO.search(text)
    if todo:
        yield Finding("todo", "info", path, line, f"A {todo.group(1)} note is added.")
    if suffix not in (".md", ".markdown") and _TRAILING.search(text):
        yield Finding(
            "trailing-whitespace",
            "warning",
            path,
            line,
            "The line ends with spaces or tabs.",
        )


def languages_of(paths: Iterable[str]) -> dict[str, list[str]]:
    """`{tool: paths}` of the changed files that each language tool checks."""
    found: dict[str, list[str]] = {}
    for path in paths:
        suffix = os.path.splitext(path)[1].lower()
        for tool, suffixes in LANGUAGES.items():
            if suffix in suffixes:
                found.setdefault(tool, []).append(path)
    return found


def language_checks(
    root: str,
    paths: Sequence[str],
    call: Callable[..., object],
    *,
    is_tested: bool = False,
) -> list[Finding]:
    """Lint and format findings of the changed files, and test failures when asked.

    `paths` are relative to `root`. Tests run the whole suite of each language
    that the change touches, because a change can break a test it did not touch.
    """
    findings: list[Finding] = []
    for tool, files in languages_of(paths).items():
        absolute = [os.path.join(root, path) for path in files]
        lint = _run(findings, tool, "lint_code", call, paths=absolute, cwd=root)
        for item in getattr(lint, "diagnostics", ()) or ():
            rule = getattr(item, "rule", "")
            findings.append(
                Finding(
                    "lint",
                    _level(getattr(item, "level", "warning")),
                    _relative(root, getattr(item, "path", "")),
                    int(getattr(item, "line", 0) or 0),
                    (f"{rule}: " if rule else "") + str(getattr(item, "message", "")),
                    "language",
                )
            )
        formatted = _run(findings, tool, "format_code", call, paths=absolute, cwd=root)
        for path in getattr(formatted, "changed", ()) or ():
            findings.append(
                Finding(
                    "format",
                    "warning",
                    _relative(root, path),
                    0,
                    f"The {tool} formatter would change this file.",
                    "language",
                )
            )
        if is_tested:
            tests = _run(findings, tool, "run_tests", call, cwd=root)
            for failure in getattr(tests, "failures", ()) or ():
                message = str(getattr(failure, "message", "")).strip().splitlines()
                findings.append(
                    Finding(
                        "test",
                        "error",
                        _relative(root, getattr(failure, "path", "")),
                        int(getattr(failure, "line", 0) or 0),
                        f"{getattr(failure, 'test', '')}: {message[0] if message else 'failed'}",
                        "language",
                    )
                )
            if (
                tests is not None
                and not getattr(tests, "failures", ())
                and not getattr(tests, "is_passed", True)
            ):
                findings.append(
                    Finding(
                        "test",
                        "error",
                        "",
                        0,
                        f"The {tool} tests did not pass.",
                        "language",
                    )
                )
    return findings


def _run(findings, tool, verb, call, **arguments):
    try:
        return call(tool, verb, **arguments)
    except Exception as error:  # noqa: BLE001 - a missing tool is a finding, not a crash
        findings.append(
            Finding(
                "tool",
                "info",
                "",
                0,
                f"{tool}.{verb} did not run: {type(error).__name__}: {error}"[:300],
                "language",
            )
        )
        return None


def _level(level) -> str:
    return level if level in LEVELS else "warning"


def _relative(root: str, path) -> str:
    path = os.fspath(path or "")
    if os.path.isabs(path):
        relative = os.path.relpath(path, root)
        if not relative.startswith(".."):
            return relative.replace(os.sep, "/")
    return path


def summary(findings: Iterable[Finding]) -> dict[str, int]:
    """Counts of the findings at each level."""
    counts = dict.fromkeys(LEVELS, 0)
    for finding in findings:
        counts[finding.level] += 1
    return counts
