# vis-review

Review what a session changed: a write journal of every block, diffs, checks, an AI review and undo. It needs no Git.

One package `vis_review` holds all the logic. Two thin doors use it:

- `extension.py`: the Vis extension. It records the journal from op hooks and opens the app with `/review`.
- `python -m vis_review`: an SDK program. It records the journal by polling and opens the same app through the gateway.

## Write journal

The journal keeps checkpoints of the files of one root, like T3 Code checkpoints, but without Git refs.

- The first checkpoint is the baseline. It records every file of the root.
- A later checkpoint records only the files that changed. A step that changes nothing records nothing.
- The extension records a checkpoint after each Python block and each patch.
- Before each block, it also records the changes that a person or another program made since the last block.
- The SDK program records a checkpoint each time it looks, every `--every` seconds.

The file list follows the ignore rules of the workspace (`vis_lang_interface.tree.listing`). A file stat finds the changed files.

Data is under `$VIS_HOME/review/`, else `~/.vis/review/`:

- `objects/`: file contents, by SHA-256 and compressed. Each content is kept once.
- One journal file for each session and root. It is append-only JSON lines.
- A file larger than 8 MiB keeps only its digest. Undo cannot bring it back.

## The app

`/review` (key `R`) opens the app of the session. The app shows:

- The checkpoints. Pick one to see its changes, or pick **All changes**.
- The changed files with their added and removed line counts. Pick one to see only its diff.
- The diff. A comment on a line becomes a review thread. When you send the review, the session gets the threads.
- The findings of the checks and of the AI review, on their lines.

The buttons:

| Button | Action |
| --- | --- |
| Refresh | Record a checkpoint now. |
| Run checks | Run the diff checks and the language checks on the changed files. |
| Run tests | Run the checks, and also the tests of the language tools. |
| AI review | Ask a model to review the diff. Its findings become threads on their lines. |
| Undo file | Undo the changes of the chosen file: in the chosen checkpoint, or in all changes. |
| Undo checkpoint | Undo only what the chosen checkpoint changed. A file that changed again later is a conflict and stays as it is. |
| Restore to here | Bring every file back to how the chosen checkpoint left it. |

An undo records a checkpoint first, so you can undo the undo. A repeated undo changes nothing.

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

In your own code, call `vis_review.program.serve(client, root, session_id=...)`. The client can be a `GatewayClient` or a `LocalEngine`.

## Tests

```bash
cd packages/vis-review
python -m pytest tests -q
```
