"""List and change issues of the board of the active project folder."""

import blockether.vis.extension as vis

from vis_board.app import BOARD
from vis_board.start import project_folder
from vis_board.store import Store, edit, matching, move


def _store():
    """The board of the project. A linked worktree shares the board of its project."""
    return Store(project_folder(vis.workspace_root()))


def list_issues(
    query: str = "",
    assignees: list[str] | None = None,
    epic: str | None = None,
    flagged: bool = False,
) -> list[dict]:
    """List issues. Search by title, key or label. Filter by person ID, epic ID or flag."""
    data = _store().read()
    issues = matching(
        data["issues"], query=query, assignees=assignees, epic=epic, flagged=flagged
    )
    columns = {column["id"]: i for i, column in enumerate(data["columns"])}
    return sorted(
        issues, key=lambda issue: (columns[issue["column"]], issue.get("rank", 0))
    )


def _change(apply):
    issue = _store().change(apply)
    BOARD.refresh()
    return issue


def create_issue(
    title: str, column: str = "backlog", values: dict | None = None
) -> dict:
    """Make an issue. Give a column ID and optional fields such as text or assignee.

    The columns are the SDLC flow: backlog, review, ready (for implementation),
    implementing, verification and done.
    """
    return _change(
        lambda data: edit(data, {**(values or {}), "title": title, "column": column})
    )


def change_issue(issue: str, values: dict) -> dict:
    """Change an issue by key or ID. Give only the fields to change."""
    return _change(lambda data: edit(data, values, issue))


def move_issue(issue: str, column: str, index: int) -> dict:
    """Move an issue by key or ID to a column ID of the SDLC flow. Index 0 is the top."""
    return _change(lambda data: move(data, issue, column, index))


def delete_issue(issue: str) -> dict:
    """Delete an issue by key or ID. Return the issue that was deleted."""
    return _change(lambda data: edit(data, {}, issue, remove=True))
