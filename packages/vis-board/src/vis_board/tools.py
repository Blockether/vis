"""List and change issues of the active workspace."""

import blockether.vis.extension as vis

from vis_board.app import BOARDS
from vis_board.store import Store, edit, matching, move


def list_issues(
    query: str = "",
    assignees: list[str] | None = None,
    epic: str | None = None,
    flagged: bool = False,
) -> list[dict]:
    """List issues. Search by title, key or label. Filter by person ID, epic ID or flag."""
    data = Store(vis.workspace_root()).read()
    issues = matching(
        data["issues"], query=query, assignees=assignees, epic=epic, flagged=flagged
    )
    columns = {column["id"]: i for i, column in enumerate(data["columns"])}
    return sorted(
        issues, key=lambda issue: (columns[issue["column"]], issue.get("rank", 0))
    )


def _change(apply):
    workspace = vis.workspace_root()
    issue = Store(workspace).change(apply)
    BOARDS.refresh(workspace)
    return issue


def create_issue(title: str, column: str = "todo", values: dict | None = None) -> dict:
    """Make an issue. Give a column ID and optional fields such as text or assignee."""
    return _change(
        lambda data: edit(data, {**(values or {}), "title": title, "column": column})
    )


def change_issue(issue: str, values: dict) -> dict:
    """Change an issue by key or ID. Give only the fields to change."""
    return _change(lambda data: edit(data, values, issue))


def move_issue(issue: str, column: str, index: int) -> dict:
    """Move an issue by key or ID to a column. Index 0 is the top."""
    return _change(lambda data: move(data, issue, column, index))


def delete_issue(issue: str) -> dict:
    """Delete an issue by key or ID. Return the issue that was deleted."""
    return _change(lambda data: edit(data, {}, issue, remove=True))
