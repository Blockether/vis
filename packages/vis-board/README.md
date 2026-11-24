# Board

Board shows the issues of a workspace in four columns: To do, In progress,
In review and Done. Open **Board** from the session apps menu. Select an issue
to read it and change it. Move issues within a column or to a different column.
Use the search and filter controls to show only the issues that you need.

Sessions for the same workspace share one board. Board saves its data under
`$VIS_HOME/board/` (`~/.vis/board/` by default). In progress shows a warning
when it contains more than three issues. It does not prevent a move.

## Use the tools

Use `board.list_issues`, `board.create_issue`, `board.change_issue`,
`board.move_issue` and `board.delete_issue` in a session. For example:

```python
issue = board.create_issue("Check the build", values={"labels": ["build"]})
board.move_issue(issue["key"], "progress", 0)
```

The column IDs are `todo`, `progress`, `review` and `done`. An issue key is
unique in its workspace and stays the same when the issue moves.
