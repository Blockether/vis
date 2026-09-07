# Live views

A live view shows a person what an extension is doing while it runs: a status
line, a progress bar, a table of jobs, streamed log lines. `vis.live(...)`
mounts the view and returns a handle the extension pushes into; the person
watches in the terminal, in the Companion app, or both, and can stop the view
at any time. This page is the reference for nodes, updates, interruption and
testing.

## Example

```python
import json

import blockether.vis.extension as vis

TONES = {"": "running", "success": "ok", "skipped": "idle"}  # anything else is an error


def poll(run_id):
    done = vis.shell({"op": "background", "id": "gh",
                      "command": f"gh run view {run_id} --json jobs,status,url"}).wait(60)
    return json.loads(done["out"])


def watch_run(run_id):
    run = poll(run_id)
    with vis.live(
        f"CI · run {run_id}",
        [
            vis.status("run", "Watching", tone="running"),
            vis.progress("progress", done=0, total=len(run["jobs"])),
            vis.table("jobs", columns=[vis.table_column("job", "Job"),
                                       vis.table_column("state", "Status")]),
            vis.link("links", links=[{"id": "run", "label": "This run", "target": run["url"]}]),
        ],
        description=f"{len(run['jobs'])} jobs",
    ) as view:
        seen = {}
        while run["status"] != "completed":
            if view.is_interrupted:
                return view.close(summary="stopped while the run was still going")
            for job in run["jobs"]:
                state = job["conclusion"] or job["status"]
                if seen.get(job["databaseId"]) != state:
                    seen[job["databaseId"]] = state
                    view["jobs"].upsert(str(job["databaseId"]), [job["name"], state],
                                        tone=TONES.get(job["conclusion"], "error"))
            view["progress"].set(done=sum(1 for j in run["jobs"] if j["status"] == "completed"))
            view.sleep(5)
            run = poll(run_id)
        view["run"].set(run["conclusion"], tone=TONES.get(run["conclusion"], "error"))
        return view.close(summary=f"the run {run['conclusion']}")
```

The Vis repository ships the complete version as `.vis/extensions/gh.py`.

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

Every keyed verb is an upsert: writing an id that exists updates it in place
and keeps its position, so a table does not reshuffle while someone reads it.
Log lines have no id and concatenate; `window_lines` limits how much of the
tail a surface keeps hot, and `.clear()` restarts both the window and the
record.

Tables: `order` is `insertion` (default), `newest-first` or `{"by": "duration",
"dir": "desc"}`. Rows that share a `branch="Release apps"` paint under one
collapsible parent. `is_selectable=True` makes rows tappable; the extension
reads the current selection from `view.state()`.

When a view has exactly one node of a type, its verb is available on the view
itself: `view.status(...)`, `view.progress(...)`, `view.write(...)`,
`view.row(...)`. A view can grow with `view.add(node, after=id)` and shrink
with `view.drop(id)`.

## Layout and text

`vis.row(id, *nodes)` and `vis.column(id, *nodes)` arrange nodes the way a form
arranges fields; a view's group carries an id because every node is
addressable. A row stands its children side by side where there is room and
stacks them on a narrow phone. `view.add(node, after="hosts")` lands inside the
group that holds `hosts`; dropping a group drops what it arranged.

Every human-facing string takes inline markdown: `` `code` ``, `**bold**`,
`_italic_` and links. A status node's text wraps and justifies to its column.
Log lines are machine output and stay verbatim. There is no free markdown node,
because every node has one markdown form that reads back as its type.

## Updating

- `view.sleep(seconds)` waits in short slices and returns `True` as soon as a
  surface changed the view, so a tap is handled at once. Never use
  `time.sleep` in a view loop.
- `with view.batch(): ...` publishes several node changes as one patch.
- Pushes are batched: the first op after a quiet stretch crosses immediately and
  later ops within `flush_ms` (100 ms by default; `vis.live(..., flush_ms=…)`)
  ride the next push. Every read and `view.close(...)` flush first.
- A view has no deadline, and neither does the block showing it. End it on a
  signal you can observe: the run reports completion, the process exits, or the
  person stops it. Do not invent a duration.

## Interruption

The person can always stop watching. In the terminal, `Escape` arms the stop
and opens one line for a note; `Escape` or `Enter` sends the stop, and
`Backspace` on an empty line returns to watching. The Companion app's Interrupt
button opens the same line.

- `view.is_interrupted` is true once the view ended without the extension
  closing it. It costs at most one host call per batching window.
- `view.is_from_human` says a person ended it; `view.note` carries their note,
  or `None`.
- Pushing into an ended view raises `vis.Interrupted` with the same note, so a
  loop that ignores the flag still stops.

## Closing

`view.close(reason=…, summary=…, error=…, artifact_id=…, selection_snapshots=…,
model_result=…)` ends the view and returns the verdict as data: `is_completed`,
`reason`, `is_from_human`, `note`, the final picture and the `summary`.

- `model_result` replaces the returned value with one string for the model; the
  full picture still reaches the surfaces and the durable artifact. A human
  stop that races the close wins and returns the interruption verdict.
- Closing twice returns the first result. As a context manager, the view closes
  itself; an exception inside closes it as `failed`.
- `selection_snapshots` seals alternative pictures for selectable rows into the
  artifact (at most 500 snapshots, 1 MiB), so a finished view stays inspectable
  after the extension is gone.

## Testing a view

Outside Vis, a live view writes its transcript to stderr and materializes the
same state. `vis.testing.LiveRecorder` records what an extension emitted and
simulates surface actions:

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

`recorder.ops()` gives stable envelope goldens and
`vis.testing.assert_tree(actual, expected)` an exact nested diff.

## See also

- [Extending Vis](extending.md) — the extension that opens a view.
- [Asking the human](human-input.md) — the same layout builders, used for questions.
- [Content-block protocol](content-blocks.md) — how a finished view reaches a transcript.
