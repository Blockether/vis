# Live views

A live view displays status, progress, tables or logs while an extension runs.
`vis.live(...)` opens the view and returns a handle for updates. The user can
watch it in the terminal or Companion app and stop it at any time.

## In the terminal

The example below updates a single pane as jobs finish, rather than printing a
new message on every poll. This capture shows example job data in the actual
Vis terminal renderer. Select an image to view it full size.

[![Vis live view showing CI run 42, one of three jobs complete and a job status table](assets/screenshots/live-running.png)](assets/screenshots/live-running.png)

Press Escape to open the stop confirmation. You can add a note for the agent
before stopping the view; cancelling the confirmation keeps it running.

[![Vis live view stop confirmation with an optional note for the agent](assets/screenshots/live-stop.png)](assets/screenshots/live-stop.png)

## Example

```python
import json
import time

import blockether.vis.extension as vis

TONES = {"": "running", "success": "ok", "skipped": "idle"}  # anything else is an error


def poll(run_id):
    done = vis.shell({"op": "background", "id": "gh",
                      "command": f"gh run view {run_id} --json jobs,status,conclusion,url"}).wait(60)
    return json.loads(done["out"])


def watch_run(run_id):
    run = poll(run_id)
    with vis.live(
        f"CI · run {run_id}",
        [
            vis.status("run", "Watching", tone="running"),
            vis.progress("progress"),
            vis.table("jobs", columns=[vis.table_column("job", "Job"),
                                       vis.table_column("state", "Status")]),
            vis.link("links", links=[{"id": "run", "label": "This run", "target": run["url"]}]),
        ],
        description=f"{len(run['jobs'])} jobs",
    ) as view:
        seen = {}
        counts = None
        while True:
            if view.is_interrupted:
                return view.close(summary="Stopped watching before the run completed")
            for job in run["jobs"]:
                state = job["conclusion"] or job["status"]
                if seen.get(job["databaseId"]) != state:
                    seen[job["databaseId"]] = state
                    view["jobs"].upsert(str(job["databaseId"]), [job["name"], state],
                                        tone=TONES.get(job["conclusion"], "error"))
            completed = sum(1 for j in run["jobs"] if j["status"] == "completed")
            total = len(run["jobs"])
            if total and (completed, total) != counts:
                view["progress"].set(done=completed, total=total)
                counts = (completed, total)
            if run["status"] == "completed":
                break
            deadline = time.monotonic() + 5
            while (remaining := deadline - time.monotonic()) > 0:
                view.sleep(remaining)
                if view.is_interrupted:
                    return view.close(summary="Stopped watching before the run completed")
            run = poll(run_id)
        view["run"].set(run["conclusion"], tone=TONES.get(run["conclusion"], "error"))
        return view.close(summary=f"Run result: {run['conclusion']}")
```

GitHub may return a run before publishing its jobs. Declare progress without a
`total` while the count is unknown; `total=0` is invalid. Set both `done` and
`total` when jobs appear, and update the total if more jobs are added. The example
keeps the last known counts across later empty polls.

For a complete implementation, see `.vis/extensions/gh.py` in the Vis repository.

## Nodes

A view declares its nodes once, each with an id, and addresses them by id.
`view[id]` returns a typed handle with only the verbs its node type supports:

| Builder | Shows | Verbs on `view[id]` |
| --- | --- | --- |
| `vis.status(id, text, tone=…, detail=…)` | one line | `.set(text, tone=, detail=, label=)` |
| `vis.progress(id, total=…)` | a bar | `.set(value=, done=, total=)` |
| `vis.stat(id, stats=[…])` | a strip of counters | `.set(stat_id, value_text, label=, tone=)`, `.remove(*ids)`, `.clear()` |
| `vis.steps(id, steps=[…])` | a checklist | `.set(step_id, tone=, label=, detail=, value=)`, `.remove(*ids)`, `.clear()` |
| `vis.output(id, label=…)` | streamed lines | `.write(*lines)`, `.clear()` |
| `vis.table(id, columns=[vis.table_column(…)])` | rows keyed by id | `.upsert(row_id, cells, tone=, branch=)`, `.select(*row_ids)`, `.remove(*ids)`, `.clear()` |
| `vis.link(id, links=[…])` | links a person can open | `.add(link_id, label, target, target_kind=, tone=)` |

`vis.output(...)` builds a `log` node; it is named `output` so it never shadows
`vis.log`, the engine log line.

Keyed updates insert new ids or update existing ones without changing their
position. Log lines have no ids and are appended. `window_lines` limits the
recent lines retained by a client; `.clear()` removes both displayed and
recorded lines.

Tables support `order="insertion"` (default), `"newest-first"` or
`{"by": "duration", "dir": "desc"}`. Rows sharing a `branch="Release apps"`
appear under one collapsible parent. With `is_selectable=True`, users can
select rows; read the selection with `view.state()`.

When a view has exactly one node of a type, its verb is available on the view
itself: `view.status(...)`, `view.progress(...)`, `view.write(...)`,
`view.row(...)`. Add nodes with `view.add(node, after=id)` and remove them with
`view.drop(id)`.

## Layout and text

`vis.row(id, *nodes)` and `vis.column(id, *nodes)` arrange nodes horizontally
or vertically. Every group has an id. Rows place children side by side when
space permits and vertically on narrow screens. `view.add(node, after="hosts")`
inserts into the group containing `hosts`. Removing a group removes its
children.

Display text accepts inline Markdown: `` `code` ``, `**bold**`, `_italic_` and
links. Status text wraps and is justified within its column.
Log lines remain verbatim. There is no arbitrary Markdown node; each node type
has its own Markdown output.

## Updating

- `view.sleep(seconds)` blocks in the host until the view changes, closes or the
  timeout expires. It returns `True` for a change or close, `False` for a timeout.
  There is no periodic state polling while it waits, and an unchanged timeout
  returns no view payload. Use it instead of `time.sleep` in view loops.
- Waiting does not publish view updates or activity. Send only changed data;
  external services still need their own polling interval. A click or Stop
  should not trigger another GitHub request.
  The example keeps a five-second deadline even when a view event wakes it early.
- Wait durations must be finite and at most 86400 seconds. Nonpositive durations
  return `False` without a host call.
- `with view.batch(): ...` sends multiple node changes as one patch.
- The first update is sent immediately. Further updates within `flush_ms`
  (100 ms by default) are batched. Set the interval with
  `vis.live(..., flush_ms=…)`. Reads and `view.close(...)` flush pending updates.
- Live views and the blocks displaying them have no timeout. Close the view
  when the job completes, the process exits or the user stops watching.

## Interruption

The user can stop watching at any time. In the terminal, `Escape` opens a
stop confirmation with an optional note. `Escape` or `Enter` confirms;
`Backspace` on an empty line resumes watching. The Companion app's Interrupt
button opens the same input.

- `view.is_interrupted` is true if the view ended without the extension closing
  it. Reading it uses at most one host call per batching interval.
- `view.is_from_human` indicates a user-initiated stop. `view.note` contains
  their note or `None`.
- Updating an ended view raises `vis.Interrupted` with the note, even if the
  loop does not check the flag.

## Closing

`view.close(reason=…, summary=…, error=…, artifact_id=…, selection_snapshots=…,
model_result=…)` ends the view and returns `is_completed`, `reason`,
`is_from_human`, `note`, the final view state and `summary`.

- `model_result` replaces the model's result with a string. Clients and the
  saved artifact still receive the full view state. A concurrent user stop
  takes precedence and returns an interruption result.
- Repeated closes return the first result. Exiting a context manager closes
  the view; an exception closes it with reason `failed`.
- `selection_snapshots` stores alternate states for selectable rows in the
  artifact, so users can inspect them after the extension exits. The limit is
  500 snapshots and 1 MiB.

## Testing a view

Outside Vis, a live view writes its transcript to stderr and maintains the
same state. `vis.testing.LiveRecorder` records updates and simulates user
actions:

```python
import blockether.vis.extension as vis

recorder = vis.testing.LiveRecorder(vis._host)
monkeypatch.setattr(vis, "_host", recorder)

run_extension()
recorder.select("jobs", ["macos"])       # a human action
assert recorder.node("jobs")["selected_ids"] == ["macos"]
assert recorder.patched()                # ops the extension emitted
result = recorder.close(reason="interrupted")
```

`recorder.ops()` returns the recorded operations for comparison with expected
output. `vis.testing.assert_tree(actual, expected)` reports nested differences.

## See also

- [Extending Vis](extending.md) — the extension that opens a view.
- [Asking the human](human-input.md) — the same layout builders, used for questions.
- [Content-block protocol](content-blocks.md) — how a finished view reaches a transcript.
