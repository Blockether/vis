"""Read and change one saved board for each workspace."""

import fcntl
import hashlib
import json
import os
import re
import subprocess
import tempfile
import time
import uuid
from collections.abc import Callable
from pathlib import Path

import blockether.vis.extension as vis

CHANGES = frozenset(
    {
        "title",
        "column",
        "kind",
        "priority",
        "assignee",
        "epic",
        "points",
        "labels",
        "text",
        "due",
        "is_flagged",
    }
)
CLEARABLE = frozenset({"assignee", "epic", "points", "due"})


def board_node(data, issues):
    """Build and check the board with the SDK."""
    return vis.ui.board(
        "issues",
        data["columns"],
        issues,
        people=data["people"],
        epics=data["epics"],
        is_editable=True,
        is_selectable=True,
    )


def _initial(workspace):
    prefix = re.sub(r"[^a-zA-Z]", "", workspace.name).upper()[:4] or "VIS"
    people = []
    try:
        name = subprocess.run(
            ["git", "config", "--get", "user.name"],
            cwd=workspace,
            capture_output=True,
            encoding="utf-8",
            errors="replace",
            timeout=5,
            check=True,
        ).stdout.strip()[:120]
    except (OSError, subprocess.SubprocessError):
        name = ""
    if name:
        people.append(vis.ui.board.person("owner", name))
    return {
        "key_prefix": prefix,
        "next_number": 1,
        "columns": [
            vis.ui.board.column("todo", "To do"),
            vis.ui.board.column("progress", "In progress", tone="running", limit=3),
            vis.ui.board.column("review", "In review", tone="warn"),
            vis.ui.board.column("done", "Done", tone="ok", is_done=True),
        ],
        "people": people,
        "epics": [],
        "issues": [],
    }


def _save(path, data):
    """Replace the old file only after the new file is complete."""
    fd, name = tempfile.mkstemp(dir=path.parent, prefix=".board-")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as stream:
            json.dump(data, stream, ensure_ascii=False, allow_nan=False)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(name, path)
    finally:
        Path(name).unlink(missing_ok=True)


class Store:
    """Lock each read or change, also when another process has the board open."""

    def __init__(self, workspace, home=None):
        self.workspace = Path(workspace).resolve()
        if home is None:
            home = os.environ.get("VIS_HOME") or Path.home() / ".vis"
        key = hashlib.sha256(os.fsencode(self.workspace)).hexdigest()
        self.path = Path(home).expanduser() / "board" / f"{key}.json"

    def read(self):
        """Read the latest board. Make it if the workspace has no board."""
        return self._access(None)

    def change(self, apply: Callable):
        """Apply a change to the latest board and save it as one operation."""
        return self._access(apply)

    def _access(self, apply):
        self.path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        with self.path.with_suffix(".lock").open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            try:
                data = json.loads(self.path.read_text(encoding="utf-8"))
            except FileNotFoundError:
                data = _initial(self.workspace)
                is_new = True
            else:
                is_new = False
            result = data if apply is None else apply(data)
            if is_new or apply is not None:
                board_node(data, data["issues"])
                _save(self.path, data)
            return result


def matching(issues, *, query="", assignees=(), epic=None, flagged=False):
    """Find issues that match all the given filters."""
    words = query.casefold().split()

    def matches(issue):
        text = " ".join(
            [issue["title"], issue.get("key", ""), *issue.get("labels", [])]
        )
        return (
            all(word in text.casefold() for word in words)
            and (not assignees or issue.get("assignee") in assignees)
            and (not epic or issue.get("epic") == epic)
            and (not flagged or issue.get("is_flagged", False))
        )

    return [issue for issue in issues if matches(issue)]


def _column(data, column):
    if not any(one["id"] == column for one in data["columns"]):
        raise ValueError(f"No column with ID {column!r}.")


def _issue(data, reference):
    for issue in data["issues"]:
        if reference == issue["id"] or (reference and reference == issue.get("key")):
            return issue
    raise ValueError(f"No issue with ID or key {reference!r}.")


def _last_rank(data, column):
    return 1 + max(
        (one.get("rank", 0) for one in data["issues"] if one["column"] == column),
        default=-1,
    )


def edit(data, values, reference=None, *, remove=False):
    """Make, change or delete an issue. Return the issue that changed."""
    if remove:
        issue = _issue(data, reference)
        data["issues"].remove(issue)
        return issue
    if unknown := values.keys() - CHANGES:
        raise ValueError(f"Unknown issue fields: {', '.join(sorted(unknown))}.")
    if reference is None:
        number = data["next_number"]
        issue = {"id": uuid.uuid4().hex, "key": f"{data['key_prefix']}-{number}"}
    else:
        issue = _issue(data, reference)
    column = values.get("column", issue.get("column", data["columns"][0]["id"]))
    _column(data, column)
    if issue.get("column") != column:
        issue.update(column=column, rank=_last_rank(data, column))
    for field, value in values.items():
        if value is None and field in CLEARABLE:
            issue.pop(field, None)
        else:
            issue[field] = value
    issue["updated_at"] = time.time_ns() // 1_000_000
    if reference is None:
        data["issues"].append(issue)
        data["next_number"] += 1
    return issue


def move(data, reference, column, index, *, visible=None):
    """Move an issue between visible neighbors, or use the full column order."""
    if type(index) is not int or index < 0:
        raise ValueError("The place must be a non-negative integer.")
    _column(data, column)
    issue = _issue(data, reference)
    others = sorted(
        (one for one in data["issues"] if one is not issue and one["column"] == column),
        key=lambda one: one.get("rank", 0),
    )
    places = [
        i for i, one in enumerate(others) if visible is None or one["id"] in visible
    ]
    if index < len(places):
        at = places[index]
    elif places:
        at = places[-1] + 1
    else:
        at = len(others)
    issue.update(column=column, updated_at=time.time_ns() // 1_000_000)
    others.insert(at, issue)
    for rank, one in enumerate(others):
        one["rank"] = rank
    return issue
