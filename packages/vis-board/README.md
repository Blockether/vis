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

## Start the work of an issue

Move an issue to **Implementing** to start its work. Board then does these
steps, as the settings of the **vis-board** extension say:

1. Board makes the place of the work. **Start works in** chooses it:
   - `worktree`, the default: a new git worktree beside the project folder, for
     example `~/vis-vis-12` on the branch `board/vis-12-fix-the-login`.
   - `branch`: a new branch in the project folder. The project folder must be a
     git repository.
   - `folder`: the project folder as it is.
2. Board makes an issue in GitHub or Linear. **Sync started issues with**
   chooses `off`, the default, `github` or `linear`. GitHub uses the `gh` CLI in
   the project folder. Linear needs **Linear team key** and **Linear API key**.
   A failed sync shows in the board log, and the start goes on.
3. Board gives the issue to an agent or to a person. **Start gives the issue
   to** chooses it:
   - `agent`, the default: the issue goes to **Vis agent**. A new Vis session in
     the place of the work implements the issue and moves it to
     **Verification**. The session can cost money and changes files in that
     place.
   - `person`: the issue keeps its assignee, and no session starts.

Board saves the place, the session and the external issue with the board. If
the issue comes back to **Implementing**, Board uses the same place and sends a
new request to the same session. A move to **Done** closes the GitHub or Linear
issue. A tool call of an agent does not start an issue. A move in the Board
app does.

## Use the tools

Use `board.list_issues`, `board.create_issue`, `board.change_issue`,
`board.move_issue` and `board.delete_issue` in a session. The tools change the
board of the project folder of the session. A session in a linked git worktree
changes the board of the main worktree. For example:

```python
issue = board.create_issue("Check the build", values={"labels": ["build"]})
board.move_issue(issue["key"], "implementing", 0)
```

The column IDs are `backlog`, `review`, `ready`, `implementing`, `verification`
and `done`. An issue key is unique in its project and stays the same when the
issue moves.
