"""Read and change one saved board for each project folder."""

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
from blockether.vis import _contracts

_BOARD = _contracts.schema("view")["$defs"]
PRIORITIES = tuple(_BOARD["board_priority"]["enum"])
KINDS = tuple(_BOARD["board_issue_kind"]["enum"])
GROUPS = tuple(_BOARD["board_group"]["enum"])
DEFAULT_PRIORITY = _BOARD["board_priority"]["default"]
DEFAULT_KIND = _BOARD["board_issue_kind"]["default"]

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

# The SDLC flow of every board. An issue goes through these columns in order.
COLUMNS = (
    ("backlog", "Backlog", {}),
    ("review", "Review", {"tone": "warn"}),
    ("ready", "Ready for implementation", {}),
    ("implementing", "Implementing", {"tone": "running", "limit": 3}),
    ("verification", "Verification", {"tone": "warn"}),
    ("done", "Done", {"tone": "ok", "is_done": True}),
)
COLUMN_IDS = tuple(one[0] for one in COLUMNS)
# The columns of the first boards, and their place in the SDLC flow.
OLD_COLUMNS = {
    "todo": "backlog",
    "progress": "implementing",
    "review": "verification",
    "done": "done",
}


def columns():
    """The columns of the SDLC flow, built with the SDK."""
    return [vis.ui.board.column(i, title, **options) for i, title, options in COLUMNS]


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
        "columns": columns(),
        "people": people,
        "epics": [],
        "issues": [],
    }


def _migrated(data, workspace):
    """Give `data` the SDLC columns and its project folder. True when it changed."""
    changed = False
    ids = tuple(one["id"] for one in data["columns"])
    if ids != COLUMN_IDS:
        moved = OLD_COLUMNS if set(ids) == set(OLD_COLUMNS) else {}
        for issue in data["issues"]:
            column = moved.get(issue["column"], issue["column"])
            issue["column"] = column if column in COLUMN_IDS else COLUMN_IDS[0]
        data["columns"] = columns()
        changed = True
    if data.get("workspace") != str(workspace):
        data.update(workspace=str(workspace), name=workspace.name)
        changed = True
    return changed


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


def board_dir(home=None):
    """The folder of the saved boards."""
    if home is None:
        home = os.environ.get("VIS_HOME") or Path.home() / ".vis"
    return Path(home).expanduser() / "board"


class Store:
    """Lock each read or change, also when another process has the board open."""

    def __init__(self, workspace, home=None):
        self.workspace = Path(workspace).resolve()
        key = hashlib.sha256(os.fsencode(self.workspace)).hexdigest()
        self.path = board_dir(home) / f"{key}.json"

    @classmethod
    def all(cls, home=None):
        """The saved boards that name their project folder, by folder name.

        A board from before the folder was saved gets it at its next use in a
        session of its project.
        """
        stores = []
        for path in sorted(board_dir(home).glob("*.json")):
            try:
                workspace = json.loads(path.read_text(encoding="utf-8")).get(
                    "workspace"
                )
            except (OSError, ValueError):
                continue
            if isinstance(workspace, str) and workspace:
                store = cls(workspace, home)
                if store.path == path:
                    stores.append(store)
        return sorted(
            stores, key=lambda one: (one.workspace.name.casefold(), str(one.workspace))
        )

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
            is_migrated = _migrated(data, self.workspace)
            result = data if apply is None else apply(data)
            if is_new or is_migrated or apply is not None:
                board_node(data, data["issues"])
                _save(self.path, data)
            return result


def matching(
    issues,
    *,
    query="",
    assignees=(),
    epic=None,
    flagged=False,
    label=None,
    priority=None,
    kind=None,
    since=None,
    hidden_columns=(),
):
    """Find issues that match all the given filters.

    `None` in `assignees` matches an issue without an assignee. `since` is a time
    in epoch milliseconds: only issues changed at or after it match.
    """
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
            and (not label or label in issue.get("labels", []))
            and (not priority or issue.get("priority", DEFAULT_PRIORITY) == priority)
            and (not kind or issue.get("kind", DEFAULT_KIND) == kind)
            and (since is None or issue.get("updated_at", 0) >= since)
            and issue["column"] not in hidden_columns
        )

    return [issue for issue in issues if matches(issue)]


def _column(data, column):
    if not any(one["id"] == column for one in data["columns"]):
        raise ValueError(f"No column with ID {column!r}.")


def reference_matches(issue, reference):
    """True when `reference` is the ID or the key of `issue`."""
    return reference == issue["id"] or bool(reference and reference == issue.get("key"))


def _issue(data, reference):
    for issue in data["issues"]:
        if reference_matches(issue, reference):
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
