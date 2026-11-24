# Board

Board is a global app. It shows the issues of all your projects on one board.
Open **Board** from the global apps button in the header, between Search and
Settings. Select a project in the filters to show only its issues. Select an
issue to read it and change it. Move issues within a column or to a different
column. Use the search and filter controls to show only the issues that you need.

Each issue goes through the SDLC flow in six columns:

1. Backlog: the issue is recorded.
2. Review: people review the issue before work starts.
3. Ready for implementation: the issue is clear and can start.
4. Implementing: the work is in progress. A warning shows when the column
   contains more than three issues. It does not prevent a move.
5. Verification: the work is checked.
6. Done: the work is complete.

Each project folder has its own board. Board saves the boards under
`$VIS_HOME/board/` (`~/.vis/board/` by default). A board from an earlier version
moves its issues to the new columns: To do to Backlog, In progress to
Implementing, In review to Verification and Done to Done.

## Use the tools

Use `board.list_issues`, `board.create_issue`, `board.change_issue`,
`board.move_issue` and `board.delete_issue` in a session. The tools change the
board of the project folder of the session. For example:

```python
issue = board.create_issue("Check the build", values={"labels": ["build"]})
board.move_issue(issue["key"], "implementing", 0)
```

The column IDs are `backlog`, `review`, `ready`, `implementing`, `verification`
and `done`. An issue key is unique in its project and stays the same when the
issue moves.
