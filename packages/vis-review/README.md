# vis-review

Review what a session changed: a write journal of every block, diffs, checks, an AI review and undo. It needs no Git.

One package `vis_review` holds all the logic. Two thin doors use it:

- `extension.py`: the Vis extension. It records the journal from op hooks and opens the app at the first step of a session.
- `python -m vis_review`: an SDK program. It records the journal by polling and opens the same app through the gateway.

## Write journal

The journal keeps checkpoints of the files of one root, without Git refs.

- The first checkpoint is the baseline. It records every file of the root.
- A later checkpoint records only the files that changed. A step that changes nothing records nothing.
- The extension records a checkpoint after each Python block and each patch.
- Before each block, it also records the changes that a person or another program made since the last block.
- The SDK program records a checkpoint each time it looks, every `--every` seconds.

The file list follows the ignore rules of the workspace (`vis_lang_interface.tree.listing`). A file stat finds the changed files.

Data is under `$VIS_HOME/review/`, else `~/.vis/review/`:

- `objects/`: file contents, by SHA-256 and compressed. Each content is kept once.
- One journal file for each session and root. It is append-only JSON lines.
- One state file next to each journal: the scope of the app and the results of the last checks and AI review. After a restart, the app shows them again.
- A file larger than 8 MiB keeps only its digest. Undo cannot bring it back.

## The app

The first Python block or patch of a session opens the app of that session, so the session apps always list it. `/review` (key `R`) opens it again after you closed it. After `/reload`, the open app connects again by itself. After a gateway restart, the app connects when you open it. The app shows:

- The checkpoints. Pick one to see its changes, or pick **All changes**.
- The changed files with their added and removed line counts. Pick one to see only its diff.
- The diff. A comment on a line becomes a review thread. When you send the review, the session gets the threads.
- The findings of the checks and of the AI review, on their lines.
- The state of the last run, for example **AI review in progress**. The Companion app shows it in the header of the app, next to the name. A result stays until you write or change a comment.
- The review notes below the diff: the summary of the AI review and each finding that no shown line holds.
- The changed pictures (PNG, JPEG, GIF and WebP), as a list with `+` for an added file, `-` for a deleted file and `•` for a modified file. Select a file of the list to open its picture, and select it again to close it. A changed picture shows its old and new version side by side. A picture over 2 MiB shows a note instead. The app holds 8 MiB of open pictures: when a new picture needs the room, the pictures that you opened first close.
- The log of the app: how long each check and AI review took, each failure with its stack trace, and each refused action.

The buttons and their keys. Each key presses its button, and an upper-case key needs Shift.
The terminal shows the keys in its key legend instead of a row of buttons.

| Button | Key | Action |
| --- | --- | --- |
| This session | `1` | Show only the changes that this session made and that the base branch does not have yet. This is the first view of `/review`. Edits of other sessions in the same folder stay out. |
| All changes | `2` | Show every change since the first checkpoint, also the edits of other sessions. |
| Latest change | `3` | Show only what the newest checkpoint changed. |
| Uncommitted | `4` | Compare the files with the git commit `HEAD`. |
| Against base branch | `5` | Compare the files with the commit where `HEAD` left the default branch. |
| Show full files | `F` | Show every unchanged line too. Click again to show only the changes. |
| Include merged changes | `M` | In **This session**, show also the changes that the base branch already has. Click again to hide them. |
| Refresh | `R` | Record a checkpoint now. |
| Run checks | `C` | Run the diff checks and the language checks on the changed files. |
| Run tests | `T` | Run the checks, and also the tests of the language tools. |
| AI review | `A` | Ask a model to review the diff. Its findings become threads on their lines. A second review checks each open finding again. A finding that the code still has stays, and a fixed finding goes away. |
| Undo file | `U` | Undo the changes of the chosen file: in the chosen checkpoint, in this session, or in all changes. It is off in a git comparison. |
| Undo checkpoint | `Z` | Undo only what the chosen checkpoint changed. A file that changed again later is a conflict and stays as it is. |
| Restore to here | `B` | Bring every file back to how the chosen checkpoint left it. |

An undo records a checkpoint first, so you can undo the undo. A repeated undo changes nothing.

**This session** compares the files of the session with the base branch (`origin/HEAD`, else the default branch):

- A file that the base branch has with the same content is merged, and it stays out.
- When the base branch is old, the view shows more files, never fewer.
- Without Git, the view shows every change of the session.
- A step can also bring changes of others: a pull, or an edit of a session that runs beside it. They go to a separate checkpoint, **Changes of others during a step**, and stay out.
- When two sessions run a step at the same time, the journal does not know which session made a new file. The session whose step ends first keeps the file. The other session keeps it only when its step names the file: the path of a patch, or the file name in the code of a block. **All changes** shows the file in each session.

## Checks

- Diff checks on the added lines and the new content:
  - conflict markers, debug leftovers and secrets,
  - new TODO and FIXME notes,
  - trailing whitespace and a missing final newline,
  - binary and large files.
- Language checks: lint and format of the changed files.
  - In the extension, through `vis.tools.py` and `vis.tools.clj`.
  - In the SDK program, through `vis_lang_python` and `vis_lang_clojure` when they are installed. Else the app shows that the tool is missing.

## SDK program

```bash
python -m vis_review --session <session-id> --root <folder>
python -m vis_review --global --root <folder>
```

- `--session` opens the app in that session. `--global` opens a global app, which belongs to no session.
- `--url` is the gateway address (default: `$VIS_GATEWAY_URL`, else `http://127.0.0.1:8090`).
- `--seconds` stops the program after that time.

In your own code, call `vis_review.program.serve(client, root, session_id=...)`. The client can be a `GatewayClient`, a `StdioEngineClient`, or an `HttpLocalEngineClient`.

## Tests

```bash
cd packages/vis-review
python -m pytest tests -q
```
