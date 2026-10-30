# Live views

A live view lets someone follow a long-running operation, such as a build,
without reading a new chat message for every update. Your extension opens it with
`vis.live(...)` and updates its progress, tables, logs or controls as work proceeds.
The view appears in the terminal or Companion app and can be stopped at any time.

## When to use

- **Your tool waits for a CI run or deployment that takes minutes.** Show its jobs
  as they finish, as in [Watch a CI run](#watch-a-ci-run).
- **You want a dashboard for several builds at once.** Keep them in [one
  view](#monitor-a-fixed-build-set) instead of one message per update.
- **Errors get lost in a long log.** [Mark each line's
  severity](#add-severity-to-streaming-output) so failures stand out.
- **The person watching needs to act or stop.** Add
  [buttons](#add-spinners-and-buttons), and handle [interruption](#interruption)
  when they stop watching.
- **Your extension needs a view that stays open between tool calls, such as a board.** Open it
  in the [side panel](#choose-a-placement).
- **People must review a change or answer you after your code returns.** Attach an
  [app](#apps). They save drafts and send them when they are ready.
- **People must act in an app with single keys.** Add [shortcuts](#add-shortcuts-to-an-app).
- **Your program must show a moving 3D world, such as a game.** Draw a [scene](#draw-a-3d-scene).
- **The model must know what people do in an app or an HTML page.** Give it a
  [note](#tell-the-model-what-people-see).
- **Your own program, not an extension, must own an app.** [Run it from your
  program](#run-an-app-from-your-own-program).
- **The person must fill in values while the work runs.** [Collect input in the
  view](#collect-input-in-a-view).
- **Your own program must list, read or interrupt live views.** Read [Sessions
  API](sessions-api.md#watch-live-views).

For a tool's ordinary status, choose [Activity
presentation](extension-api.md#activity-presentation) instead. Use a live view when
the user needs to watch or interact with ongoing work. To ask a question and wait
for the answer, use a [form](human-input.md).

## Before you start

Open an inline live view inside a registered tool or user command with a calling session, not
during extension registration. A [panel view](#choose-a-placement) is different: open it
from any extension callback, also at registration. Stopping the view stops watching, not necessarily
the external job.

## In the terminal

The example below updates a single pane as jobs finish. Select the image to view
it full size.

[![Vis live view showing CI run 42, one of three jobs complete and a job status table](assets/screenshots/live-running.png)](assets/screenshots/live-running.png)

Press F3 for keyboard controls: expand a disclosure, select a row or activate a button.
Press Escape to open the stop confirmation. You can add a note for the agent
before stopping the view. Cancelling the confirmation keeps it running.

[![Vis live view stop confirmation with an optional note for the agent](assets/screenshots/live-stop.png)](assets/screenshots/live-stop.png)

## Watch a CI run

This implementation belongs in a trusted extension module. Register `watch_run`
with `vis.Symbol` to call it from Vis. It requires the GitHub CLI, authentication
and a working directory inside the target repository. `run_id` is a numeric Actions
run ID. The function returns the final view receipt, not a GitHub run object.
For a complete integration with typed results and error handling, use the
[repository's GitHub extension](https://github.com/Blockether/vis/blob/main/.vis/extensions/gh.py).

```python
import json
import time

import blockether.vis.extension as vis

TONES = {"": "running", "success": "ok", "skipped": "idle"}  # anything else is an error


def poll(run_id):
    done = vis.shell({"op": "background", "id": "gh",
                      "command": f"gh run view {run_id} --json jobs,status,conclusion,url"}).wait(60)
    return json.loads(done["out"])


def watch_run(run_id: int) -> dict:
    """Watch a GitHub Actions run; stopping the view does not cancel the run."""
    run = poll(run_id)
    with vis.live(
        f"CI · run {run_id} · {len(run['jobs'])} jobs",
        [
            vis.ui.status("run", "Watching", tone="running"),
            vis.ui.progress("progress"),
            vis.ui.table("jobs", columns=[vis.ui.table_column("job", "Job"),
                                       vis.ui.table_column("state", "Status")]),
            vis.ui.link("links", links=[{"id": "run", "label": "This run", "target": run["url"]}]),
        ],
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

GitHub can return a run before it publishes its jobs. While the count is unknown, declare progress
without a `total`. `total=0` is invalid. When jobs appear, set both `done` and `total`. If more jobs
are added, update the total. The example keeps the last known counts across later empty polls.


## Monitor a fixed build set

Use one synchronous tool and one live view to observe several **already selected** builds. The
coordinator below runs at most two read-only requests at the same time. It uses each completed read
without waiting for a slower one, and it keeps the last observation of each build. It does not
launch deployments, schedule dependencies, or cancel remote jobs. The model does not need general
shell access.

Supply a domain adapter `read_once(build, *, deadline, stop)` that returns
`"running"`, `"succeeded"` or `"failed"` for that exact environment and build ID.
This adapter is your CI client's code, **not a Vis API**. It must:

- Use pinned identities, not a moving alias such as “latest”.
- Enforce the absolute `time.monotonic()` deadline across connection, reads,
  retries and pagination, and cooperate with the `threading.Event` named `stop`.
- Close responses, sockets and any per-request client in `finally` or `with`.
- Make only read requests. Keep authentication in the trusted extension's configuration. Do not
  return credentials or raw exception messages.
- Avoid Vis host calls from the reader threads. Only the invoking thread owns
  and updates the view.

A socket read timeout alone is not a total request deadline. Python cannot forcibly
stop a running thread. Use a client with bounded operations and test its cleanup.

```python
# monitor.py
import math
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import asdict, dataclass
from threading import Event

import blockether.vis.extension as vis


@dataclass(frozen=True)
class Build:
    environment: str
    build_id: str


def watch_builds(builds, read_once, *, timeout_s=600.0, poll_s=5.0):
    """Observe a fixed set; return after all locally owned readers have stopped."""
    builds = tuple(builds)
    if not builds or len(set(builds)) != len(builds):
        raise ValueError("Choose a nonempty set of distinct environment/build pairs")
    if any(not b.environment.strip() or not b.build_id.strip() for b in builds):
        raise ValueError("Environment and build ID must be explicit")
    if any(not math.isfinite(n) or n <= 0 for n in (timeout_s, poll_s)):
        raise ValueError("Timeout and poll interval must be finite and positive")
    last = [{**asdict(b), "state": "unobserved", "read_error": None} for b in builds]
    due = [0.0] * len(builds)
    pending = {}
    stop = Event()
    deadline = time.monotonic() + timeout_s
    outcome = "completed"
    with vis.live("Watch builds", [
        vis.ui.status("status", "Watching pinned builds", tone="running"),
        vis.ui.table("builds", columns=[vis.ui.table_column("environment", "Environment"),
                                    vis.ui.table_column("build", "Build"),
                                    vis.ui.table_column("state", "Last observation")]),
    ]) as view:
        pool = ThreadPoolExecutor(max_workers=2, thread_name_prefix="ci-reader")
        try:
            for i, row in enumerate(last):
                view["builds"].upsert(str(i), [row["environment"], row["build_id"],
                                              row["state"]])
            while True:
                if view.is_interrupted:
                    outcome = "interrupted"
                    break
                # Never call result() on an unfinished future or use ordered map().
                for future in [f for f in pending if f.done()]:
                    i = pending.pop(future)
                    try:
                        state = future.result()
                        if state not in {"running", "succeeded", "failed"}:
                            raise ValueError("Unknown build state")
                        last[i]["state"] = state
                    except Exception as error:
                        # A read error is not a failed CI build or a successful read.
                        last[i]["read_error"] = type(error).__name__
                    due[i] = time.monotonic() + poll_s
                    row = last[i]
                    view["builds"].upsert(str(i), [row["environment"], row["build_id"],
                                                  row["read_error"] or row["state"]])
                if any(row["state"] == "failed" for row in last):
                    outcome = "failed"
                    break
                if any(row["read_error"] for row in last):
                    outcome = "observation_error"
                    break
                if all(row["state"] == "succeeded" for row in last):
                    break
                now = time.monotonic()
                if now >= deadline:
                    outcome = "timeout"
                    break
                for i, build in enumerate(builds):
                    if len(pending) == 2:
                        break
                    if (i not in pending.values() and due[i] <= now
                            and last[i]["state"] != "succeeded"):
                        pending[pool.submit(read_once, build,
                                            deadline=min(deadline, now + 5.0),
                                            stop=stop)] = i
                # Futures do not wake the view. Check them within 100 ms; Stop
                # wakes this host wait immediately. UI events do not reset due[].
                view.sleep(min(0.1, max(0.0, deadline - time.monotonic())))
        except vis.Interrupted:
            # Stop can race with any view update, not just the explicit flag read.
            outcome = "interrupted"
        except BaseException:
            outcome = "failed"
            raise
        finally:
            stop.set()
            try:
                reason = "failed" if outcome == "observation_error" else outcome
                receipt = view.close(reason=reason, summary=f"Monitor: {outcome}")
            finally:
                pool.shutdown(wait=True, cancel_futures=True)
        if receipt["reason"] == "interrupted":
            outcome = "interrupted"  # a concurrent human Stop takes precedence
    return {"outcome": outcome, "observations": last, "view": receipt}
```

Register only the typed wrapper, not the callback-taking coordinator. For example,
with `monitor.py` and your domain client installed in the extension's
[project environment](extension-development.md#prepare-the-project-environment):

```python
# .vis/extensions/ci_monitor.py
import blockether.vis.extension as vis
from monitor import Build, watch_builds
from my_ci import read_build  # your bounded, read-only adapter described above


def watch(builds: list[Build], timeout_s: float = 600.0) -> dict:
    """Watch these exact builds together. Stop watching never cancels remote jobs."""
    return watch_builds(builds, read_build, timeout_s=timeout_s)


def present(*, phase, args, kwargs, result, error):
    if phase == "start":
        summary = "Watching the selected build set"
    elif phase == "failure":
        summary = f"Monitoring could not finish: {type(error).__name__}"
    else:
        summary = f"{result['outcome']}: {len(result['observations'])} builds"
    return vis.ActivityPresentation("Watch builds", summary)


vis.register_extension(vis.Extension(
    name="ci-monitor", alias="ci", description="Read-only monitoring of pinned CI builds.",
    symbols=[vis.Symbol(watch, name="watch_builds",
                        activity=vis.Activity(label="Watch builds", render=present))],
))
```

**Fail-fast and cleanup are separate.** When a completed read reports failure, the coordinator
schedules no new work. The view closes before the coordinator joins slower readers. The caller
returns only after those readers finish their bounded cleanup. `Future.cancel()` cannot kill a
request that is already running.

Do not replace this with `shutdown(wait=False)`. That leaves work running after the return, with no
sign of it. Snapshots are the last observations used before the stop. `unobserved` does not mean
running or successful. Late results during cleanup do not overwrite the snapshots.

**This is not durable background execution.** The open live view suspends the
invoking block's ordinary wall-time limit, not network deadlines or the recipe's
explicit overall timeout. A synchronous call still occupies its caller: the
model cannot call a second “stop” tool on that same blocked call. Use the UI or
an independent SDK client. There is no monitoring after return and no recovery
across a worker or gateway restart.

Before you ship your adapter, test these cases:

- A fast failure beside a slow read.
- Successful completion.
- Stop during waiting and during updating.
- Request errors.
- The overall timeout.
- Release of every locally owned thread and response.

The repository's
[recipe tests](https://github.com/Blockether/vis/blob/main/packages/vis-agent/tests/test_monitor_recipe.py)
run these code blocks with controlled readers and `LiveRecorder`. They do not check your CI service
or credentials.

## Nodes

A view declares its nodes once, each with an id, and addresses them by id.
`view[id]` returns a typed handle with only the verbs its node type supports:

| Builder | Shows | Verbs on `view[id]` |
| --- | --- | --- |
| `vis.ui.status(id, text, tone=…, detail=…)` | status text with an optional detail line | `.set(text, tone=, detail=, label=)` |
| `vis.ui.progress(id, total=…)` | a bar | `.set(value=, done=, total=)` |
| `vis.ui.stat(id, stats=[…])` | a strip of counters | `.set(stat_id, value_text, label, tone=)`, `.remove(*ids)`, `.clear()` |
| `vis.ui.steps(id, steps=[…])` | a checklist | `.set(step_id, label, tone=, detail=, value=)`, `.remove(*ids)`, `.clear()` |
| `vis.ui.output(id, label=…, default_expanded=False)` | independently collapsible retained lines | `.write(*lines, tone=)`, `.clear()` |
| `vis.ui.table(id, columns=[vis.ui.table_column(…)], groups=[vis.ui.table_group(…)])` | rows keyed by id, optionally folded into groups | `.upsert(row_id, cells, tone=, parent=)`, `.group(group_id, label=, tone=, order=, is_open=)`, `.replace(rows)`, `.select(*row_ids)`, `.remove(*ids)`, `.clear()` |
| `vis.ui.link(id, links=[…])` | links a person can open | `.add(link_id, label, target, target_kind=, tone=)` |
| `vis.ui.paragraph(id, text)` | a paragraph with inline formatting | `.set(text)` |
| `vis.ui.heading(id, text, level=2)` | a heading at level 1–6 | `.set(text, level=)` |
| `vis.ui.divider(id)` | a horizontal rule between sections | none (use `view.add(...)` or `view.drop(id)`) |
| `vis.ui.code(id, text, language=None)` | literal, whitespace-preserving code | `.set(text, language=)` |
| `vis.ui.spinner(id, text="Working", variant="braille")` | an explicit activity indicator | `.set(text=, variant=, is_active=)` |
| `vis.ui.button(id, label, is_disabled=False, variant=None)` | an operator action; `variant` is `primary`, `secondary` or `destructive` | `.set(label=, is_disabled=)` |
| `vis.ui.disclosure(id, label, *nodes, default_expanded=False)` | a collapsible column | children update by their own ids |
| `vis.ui.scene(id, width, height, objects=[vis.ui.scene_object(…)])` | a 3D world, [drawn on every surface](#draw-a-3d-scene) | `.put(object_id, **changes)`, `.mesh(mesh_id, vertices, faces)`, `.set(background=, fov=, is_wrapping=, tick_ms=)`, `.remove(*ids)`, `.clear()` |

`vis.ui.output(...)` builds a `log` node. It is named `output` so it never shadows
`vis.log`, the engine log line.

To update a status without changing its detail, omit `detail`. To remove the
detail line, pass `detail=""`:

```python
view["state"].set("Polling", detail="Waiting for jobs")
view["state"].set("Running")             # Keeps the detail.
view["state"].set("Complete", detail="")  # Clears the detail.
```

Keyed updates insert new ids or update existing ones without changing their position. Log lines have
no ids and are appended. `.clear()` removes both displayed and recorded lines.

Each log starts collapsed and has its own expanded state. Expanding one never opens another. Patches
keep the reader's choice. `default_expanded=True` changes only the first active state. Completed
receipts start collapsed again. When you expand one, its saved output is still there.

A live view shows the whole log. When you open a log, Companion and the TUI both read the record
back to its first line. So they show output that arrived before your surface attached, not only a
count of it. `window_lines` only sets the size of two tails: the tail the model reads and the tail a
client gets when it attaches. It never hides output from you.

While the earlier lines are still loading, one line tells you so. The same line also tells you when
a read fails. Search reaches the record in both cases.

Expanded logs offer **Search** in Companion and **Search log** through the TUI's F3 controls or
pointer. Search finds a literal, case-insensitive substring across the saved record, including lines
outside `window_lines` and closed views. Results include original one-based line numbers and come in
pages of 200. They are a snapshot, and **Refresh results** adds new output. Clearing a log resets
its searchable history. Search never sends an action to the producer.

In Companion, **Clear search** or Escape in the search field restores normal output. `/` while the
output is focused moves focus to search. Without a record loader, the panel
explicitly limits search to loaded lines. In the TUI, an empty query browses the
record, Enter opens a full wrapped line, and Escape cancels an in-flight read.

A client reads the record of a log node in pages with the [Sessions
API](sessions-api.md#watch-live-views). A request names the log node and takes `query`, `from` and
`limit`. `from` is a zero-based match offset. An empty query matches all lines.

The response includes `lines`, `line_numbers`, `matched` and `total`. The gateway caps pages at the
default log-window size. It streams the record and keeps only the requested result page. Styled
pages also include `line_tones`, aligned with `lines`. A `null` entry means plain text.

### Add severity to streaming output

Use `view["log"].write(..., tone="error")` to distinguish compiler failures,
warnings and successful steps without treating a mixed build log as one source
language. The typed `vis.LogTone` values are `"idle"`, `"running"`, `"ok"`, `"warn"`
and `"error"`. Omitting `tone` keeps the plain-text default. A tone applies to all
lines in that call, not to later calls.

```python
with vis.live("Build output", [vis.ui.output("log", label="Build log")]) as view:
    view["log"].write("10:42:00 INFO $ npm test")
    view["log"].write("10:42:01 WARN cache unavailable", tone="warn")
    view["log"].write("10:42:02 ERROR compiler failed", tone="error")
    view["log"].write("    at compile (src/build.ts:42:7)")
```

Keep severity words in the text so the log also makes sense without color. TUI and Companion use
their theme's semantic colors. Plain stderr and Markdown receipts keep the same readable text.
Companion's search results keep colors. The TUI's search and full-line dialogs provide a plain-text
fallback. Completed live-view receipts keep the style metadata and the original line numbers.

Redact secrets **before** you call `write` or seed `lines`. Engine presentation redaction is an
extra safeguard. It does not detect every kind of secret. Styles are separate metadata. They never
divide, hide or replace text before redaction, search or copying. HTML and URLs in output stay
literal, not links or executable markup.

ANSI is not a styling API. C0/C1 controls other than tab and newline show as visible `\uXXXX` text
(for example, Escape becomes `\u001b`). Cursor moves, OSC links, clipboard escapes and color escapes
are never executed.

Each argument to `write` is one complete line of the record, not a raw network fragment. In your
adapter, decode and assemble partial lines before you redact and append them. Append incremental
output. Do not clear and replace snapshots.

Successive batches keep their own tones, even when updates are coalesced. Styling does not change
`window_lines`, retention or search pagination. The earlier pages that a surface reads back keep the
tones they were written with. Use `vis.ui.code(..., language=...)` for a separate snippet in a known
language, not to reinterpret the mixed log.

For raw wire clients, an `append` operation can carry `tone` only for a log node. Log snapshots can
carry `line_tones`, with one tone or `null` per line. Clojure uses `:tone` on log append operations
and `:line-tones` on seeded log nodes. These closed enums accept no CSS, free-form color values,
HTML or terminal commands.

### Add spinners and buttons

Spinner variants are `braille`, `dots`, `line` and `pulse`. Set `is_active=False`
to stop one without removing its text. Completed receipts never animate, and the
app respects reduced-motion preferences.

Buttons are real actions, not links or serialized callbacks. An accepted press
increments that node's `clicks` in shared state and wakes `view.sleep(...)`.
Read it with `view.state()`, then let the producer decide what to do. Disabled
buttons and completed receipts reject activation. Treat an unconfirmed network
request as uncertain: check state before retrying a consequential action.

```python
with vis.live("Review", [
    vis.ui.heading("title", "Build review", level=1),
    vis.ui.paragraph("intro", "Read the output before continuing."),
    vis.ui.code("example", "print('ready')", language="python"),
    vis.ui.spinner("waiting", "Waiting for review", variant="dots"),
    vis.ui.disclosure("details", "Build details",
                   vis.ui.output("tests", label="Test output"),
                   vis.ui.output("build", label="Build output")),
    vis.ui.divider("review-actions"),
    vis.ui.button("continue", "Continue"),
]) as view:
    view["tests"].write("Tests passed")
    while not view.is_interrupted:
        state = view.state()
        if not state:
            break
        pressed = next(n for n in state["nodes"] if n["id"] == "continue")
        if pressed["clicks"]:
            view["continue"].set(is_disabled=True)
            view["waiting"].set("Review accepted", is_active=False)
            break
        view.sleep(60)
```

With one argument, `vis.ui.heading(text)` and `vis.ui.paragraph(text)` remain form
decorations. Two positional arguments declare addressed live nodes.

Tables support `order="insertion"` (default), `"newest-first"` or
`{"by": "duration", "dir": "desc"}`. Rows that share a `parent="tests"` appear under one collapsible
group. The group stays closed until the reader opens it. Every surface shows that group the same
way, with its name and the number of rows it holds. So keep the name stable and let Vis do the
counting.

A group that you never declare gets its id as its name. It sorts after the declared groups, in the
order its first row arrived.

Declare a group when you want to name, color or order it. When you build the table, use
`vis.ui.table_group("tests", label="Tests", tone="error", order=0)`. While the run moves, use
`view["jobs"].group("tests", tone="error")`.

Declarations merge by id, so a later call changes only the keys it passes. Renaming a group keeps
the reader's fold. `is_open=True` opens the group when it first arrives. After that, the reader's
own choice stands. A declared group that holds no rows is not shown.

With `is_selectable=True`, users can select rows. Read the selection with
`view.state()`.

To show a new set of rows, for example after a filter changes, call `.replace(rows)`. It sets
every row in one step. A selected row that is in the new set stays selected. Rows that are not in
the new set disappear, and so does their selection. `.clear()` followed by `.upsert(...)` drops the
whole selection instead.

```python
view["chats"].replace([vis.ui.table_row(chat.id, [chat.title, chat.category])
                       for chat in chats if chat.category == category])
```

An empty list empties the table. A table holds at most `max_rows` rows, so a larger set is refused.

When a view has exactly one node of a type, its verb is available on the view
itself: `view.status(...)`, `view.progress(...)`, `view.write(...)`,
`view.row(...)`. To change the layout while a view runs, see
[Layout and text](#layout-and-text).

### Draw a 3D scene

A `scene` node shows a small 3D world, for example a game board or a model of a machine. Your
Python code keeps all logic. It sends only shapes, places, turns and colors, and the surfaces draw
them. No code of yours runs in the Companion app or the terminal.

```python
field = vis.ui.scene(
    "field", 48, 24,
    label="Field",
    fov=30,
    is_wrapping=True,
    tick_ms=100,
    objects=[vis.ui.scene_object("ship", "cone", x=24, y=12, scale=1.2, color="#7dd3fc")],
)
app = vis.attach(vis.ui.app("Asteroids", field, key="asteroids"))

app["field"].put("rock-1", mesh="icosahedron", x=10, y=6, scale=2, color="#a8a29e")
app["field"].put("ship", x=25, rz=-45)  # Sends only the changed keys.
app["field"].remove("rock-1")
```

- The axes are x to the right, y up and z toward the viewer. The camera shows the rectangle from
  `(0, 0)` to `(width, height)` at `z=0`.
- `fov` is the vertical field of view in degrees, from 0 to 120. The default is 40. Use 0 for a
  flat, orthographic view.
- `rx`, `ry` and `rz` turn an object in degrees, first about x, then about y, then about z.
- The builtin meshes are `box`, `tetrahedron`, `octahedron`, `icosahedron`, `sphere` and `cone`. The
  tip of a cone points to +y. Add your own shape with `vis.ui.scene_mesh(mesh_id, vertices, faces)` or
  `.mesh(...)`. List the corners of each face counterclockwise, as seen from outside.
- One light shades each face. An object with `is_emissive=True` shows its own color without shade.
- A scene holds at most 32 custom meshes and 512 objects.

`.put(object_id, **changes)` adds an object or changes only the keys that you give. A new object
needs `mesh`. `.objects` returns the objects as Vis holds them, by id. `.set(...)` changes
`background`, `fov`, `is_wrapping` or `tick_ms`.

Send one update for each tick of your loop. The Companion app draws the scene with WebGL. It moves
each object smoothly from its last place to its new place in `tick_ms`, at the frame rate of the
screen. With `is_wrapping=True`, an object that leaves one edge moves smoothly in at the other
edge. To move an object without the smooth motion, give it a new id.

The terminal draws the same scene with its own renderer. Each character shows two pixels with the
`▀` half block in true color.

## Layout and text

Use `vis.ui.row(id, *nodes)`, `vis.ui.column(id, *nodes)`, `vis.ui.grid(id, *nodes)` and
`vis.ui.wrap(id, *nodes)` to arrange nodes with the same layout vocabulary as
[Ask forms](human-input.md#layout).
Every Live group has an id. Groups can nest. Each nested group uses the width
available inside its parent, not the full screen width.

In Companion, rows use equal-width columns with a minimum width of `12rem`,
wrapping into fewer columns as space narrows and eventually stacking. A single
column can shrink below that minimum to fit a very narrow panel. In the TUI,
rows use equal-width columns when each child has at least 24 terminal text cells
after spacing. Otherwise the entire row stacks vertically. Columns always stack.

Use a grid for many nodes of the same kind, such as cards. It shows equal columns
and continues on new rows. `vis.ui.grid(id, *nodes, max_columns=3)` limits the
columns. A narrow panel shows fewer columns, and a very narrow panel stacks them.
In the TUI, each grid column also needs 24 terminal text cells.

Use a wrap for nodes that keep their own width, such as buttons, stats and
statuses. They stand side by side and continue on a new line when the next one
does not fit. Buttons in a grid or a wrap stay interactive on both surfaces.

```python
vis.ui.column("board",
           vis.ui.grid("services", *cards, max_columns=3),
           vis.ui.wrap("actions",
                    vis.ui.button("approve", "Approve release"),
                    vis.ui.button("rerun", "Run checks again")))
```

Declare at least one child in every `vis.ui.row`, `vis.ui.column`, `vis.ui.grid`,
`vis.ui.wrap` and `vis.ui.disclosure`. These Python builders raise `ValueError` immediately when
called without children. If you do not have any nodes yet, wait to add the
group until its first child is available.

Use `view.add(node)` to append a top-level node. With `after=`, the new node
becomes the named node’s next sibling in the same container:

```python
view.add(vis.ui.column("builds", vis.ui.status("linux", "Queued")))
view.add(vis.ui.status("macos", "Queued"), after="linux")  # Inside builds.
view.add(vis.ui.status("summary", "Watching"), after="builds")  # After the group.
```

To grow a group, name one of its children in `after=`, not the group itself.
Remove a node with `view.drop(id)`. Removing a group also removes its children.
Groups preserve their children’s live interactions and do not produce form
answer values.

Use `vis.ui.divider("section-break")` between sections, like an HTML `<hr>`. It fills
its current container’s width in the terminal and Companion, including inside
columns and disclosures, and remains visible in completed receipts. A divider
has only an id: it has no label, content, style options or update verbs. Use a
heading for a section title, and `view.drop("section-break")` to remove the rule.
The Clojure builder is `(view/divider "section-break")`. The wire node is
`{"id": "section-break", "type": "divider"}`.

Display text accepts inline Markdown: `` `code` ``, `**bold**`, `_italic_` and links. Status text
wraps and is justified within its column.

Log lines and code blocks are never executed. Terminal controls in logs show as visible escapes.
Other text stays literal. There is no general Markdown node. Each node type has its own Markdown
output.

Vis redacts text that looks like a labeled credential in every view, also in your own text. When a
word that contains `token`, `password`, `secret`, `cookie` or `otp` comes before `:` or `=`, the
value after it shows as `[REDACTED]`. For example, `Tokens: 5` shows as `Tokens: [REDACTED]`. To
show such a value, word the label differently, for example `5 tokens left`.

## Updating

- `view.sleep(seconds)` blocks in the host until the view changes, closes or the
  timeout expires. It returns `True` for a change or close, `False` for a timeout.
  There is no periodic state polling while it waits, and an unchanged timeout
  returns no view payload. Use it instead of `time.sleep` in view loops.
- Waiting does not publish view updates or activity. Send only changed data.
  External services still need their own polling interval. A click or Stop
  should not trigger another GitHub request.
  The example keeps a five-second deadline even when a view event wakes it early.
- Wait durations must be finite and at most 86400 seconds. Nonpositive durations
  return `False` without a host call.
- `with view.batch(): ...` sends multiple node changes as one patch.
- The first update is sent immediately. Further updates within `flush_ms`
  (100 ms by default) are batched. Set the interval with
  `vis.live(..., flush_ms=…)`. Reads and `view.close(...)` flush pending updates.
- An open live view suspends the invoking block's ordinary wall-time limit.
  Set explicit deadlines for external requests and for monitoring as a whole,
  and close the view when work ends. This is not a durable worker lifetime.

## Interruption

The user can stop watching at any time. In the terminal, `Escape` opens a stop confirmation with an
optional note. `Escape` or `Enter` confirms. `Backspace` on an empty line resumes watching. The
Companion app's Interrupt button opens the same input.

- `view.is_interrupted` is true if the view ended without the extension closing
  it. Reading it uses at most one host call per batching interval.
- `view.is_from_human` is true when the user stopped the view. `view.note` contains
  their note or `None`.
- After the user stops a view, the agent step that opened it cannot open another
  live view. Vis adds the stop and the note to the output that the agent reads.
- Updating an ended view raises `vis.Interrupted` with the note, even if the
  loop does not check the flag.
- From the producer, `view.close(reason="interrupted", summary="Stopped monitoring")`
  closes the owned view. It does not kill threads or remote jobs.
- From an independent client, interrupt the view with the [Sessions
  API](sessions-api.md#watch-live-views). The action is `interrupt`, not `cancel`. The producer must
  still handle `view.is_interrupted` or `vis.Interrupted` and clean up its resources.

## Closing

`view.close(reason=…, summary=…, error=…, artifact_id=…, selection_snapshots=…,
model_result=…)` ends the view and returns `is_completed`, `reason`,
`is_from_human`, `note`, the final view state and `summary`. In Vis, the
model's returned view is budgeted. Clients and saved artifacts preserve the
group hierarchy and the current log window.

- `model_result` replaces the model's result with a string. Clients and the
  saved artifact still receive the full view state. A concurrent user stop
  takes precedence and returns an interruption result.
- Repeated closes return the first result. Exiting a context manager closes
  the view. An exception closes it with reason `failed`.
- `selection_snapshots` stores alternate states for selectable rows in the
  artifact, so users can inspect them after the extension exits. The limit is
  500 snapshots and 1 MiB.

## Choose a placement

`placement` decides where a view appears and how long it stays open.

| Placement | Where it appears | Lifetime |
| --- | --- | --- |
| `inline` (default) | In the transcript, under the step that opened it | Closes when its tool call or block ends |
| `panel` | In the side panel of one session | Stays open until your extension closes it or unloads |
| `attachment` | In the transcript and the attachments of the session, as an app | Stays until your code closes or archives it, also after a gateway restart |

Only extension code can open a `panel` view. The `python_execution` sandbox refuses it. A panel
view needs a `key`. Your extension and the key own one view, so a second
`vis.live` with the same key replaces the first view.

```python
import blockether.vis.extension as vis

BOARD = {}


def handle(event):
    if event.type == "activate" and event.node_id == "refresh":
        BOARD["view"]["state"].set("Refreshing", tone="running")


def open_board(session_id):
    BOARD["view"] = vis.live(
        "Release board",
        [vis.ui.status("state", "Idle", tone="idle"), vis.ui.button("refresh", "Refresh")],
        placement="panel",
        key="release-board",
        session_id=session_id,
        on_event=handle,
    )
```

- Open the view in a callback, such as a tool call, a command or registration code. After that,
  threads of your extension can also change, read and close it.
- `session_id` selects the session of a panel view.
- Vis saves the last state of a panel view. After a gateway restart, the view shows that state as
  stale until your extension loads again. Open your views again when your extension loads. If the
  load does not open the same key, Vis closes the stale view.

An `attachment` view is an app. Open it with `vis.attach(vis.ui.app(...))`, as [Apps](#apps) shows.

## Collect input in a view

A live view can hold the same [inputs](components.md#inputs) as a form. Each input keeps its
`value` while the view is open, and every surface shows the same value.

```python
def choose_target() -> str | None:
    """Show a deploy form in a live view and return the chosen target."""
    with vis.live("Deploy", [
        vis.ui.group("target",
                  vis.ui.select("env", ["staging", "prod"], label="Target", is_required=True),
                  vis.ui.multiline("notes", label="Notes"),
                  vis.ui.button("deploy", "Deploy", action="submit")),
        vis.ui.status("state", "Waiting for a target", tone="idle"),
    ]) as view:
        while not view.is_interrupted:
            for event in view.events(timeout=60):
                if event.type == "submit" and event.node_id == "deploy":
                    view["state"].set(f"Deploying to {event.values['env']}", tone="running")
                    return event.values["env"]
    return None
```

A person action becomes a `vis.LiveEvent`:

| `type` | When | Fields |
| --- | --- | --- |
| `change` | A person changes an input | `node_id` and `value`. A secret input gives only `is_set`. |
| `activate` | A person presses a button or a selectable card | `node_id` |
| `submit` | A person presses a button with `action="submit"` | `node_id` and `values` of the inputs in the group of the button |
| `select` | A person selects rows in a selectable table | `node_id` and `item_ids`, the selected row ids |

- `view.values()` returns the current value of each input by id. Secret inputs are left out.
- `view.events(timeout=None)` returns the events since the last call. With `timeout`, it waits up to
  that many seconds for the first new event.
- `view.on_event(callback)` calls `callback(event)` for each event that `events` or `sleep` reads.
- In a panel view, `vis.live(..., on_event=callback)` gets each event from the host. The
  callback runs as an extension callback with the view session bound. So it can call
  `Session.send` or `vis.sessions.create`.
- `view[id].value` reads one input. `view[id].set(value)` changes it.

A submit checks each required input and each validator of the group first. A bad value shows an
error at its input, and the view stays open. A `password` or `otp` value never reaches the
transcript, the saved view or a log. The `submit` event gives a `vis-secret:` handle for it.

Give an input `validate=` to check it with your own code, as in `vis.ask`:

```python
vis.ui.plaintext("port", label="Port",
                 validate=lambda v: None if v.isdigit() else "Use digits only.")
```

- The function gets the value, or the value and every value of the group. It answers None when the
  value is good, or the error message.
- Your code runs the function when the person submits, before Vis accepts the submit. The
  function never leaves your process.
- An extension and an SDK program get the question from the host. In the `python_execution`
  sandbox, your code answers it while it reads events with `events`, `sleep` or `run`.
- When your code does not answer in 5 seconds, the submit is refused with "The app did not
  answer. Try again." at the submit button.
- A validator lives only in the process that declared it. After a restart, open the view again
  with the same key to give it back.
- An input that `view.add(...)` adds with `validate=` gets its validator too. `view.drop(...)`
  removes the validator with the input.
- When you open a view without `on_event` and without a validated input, an extension answers
  later validators only while it reads events.

## Side panel

People find panel views in these places:

- **Terminal.** Ctrl+X b p opens the side panel, then moves the focus between the panel and the
  transcript. The panel docks at the right when the chat keeps 60 columns, else it opens over the
  chat. [Keyboard shortcuts](keyboard-shortcuts.md) lists the keys of the panel.
- **Companion app on a desktop or tablet.** The side panel docks at the right of the session, and
  you can drag its edge to resize it.
- **Companion app on a phone.** The panel icon next to Settings opens the panel as a full page.

When views of several extensions are in one panel, each extension gets its own tab.

## Apps

An app is a live view that stays in the session after your code returns. People open it from the
transcript or from the attachments of the session. They comment on a diff, write in a thread or
fill in inputs. Then they send their work to you. Vis saves the state of the app, also across
gateway restarts.

```python
import blockether.vis.extension as vis

app = vis.attach(vis.ui.app(
    "Review the search change",
    vis.ui.diff("change", patch, old_revision="abc1234", new_revision="def5678"),
    vis.ui.thread("general", label="Questions"),
    key="search-review",
))
```

- `vis.ui.app(title, *nodes, description=None, key=None)` declares an app. Its nodes are the
  [components](components.md) of a live view, plus `vis.ui.diff` and `vis.ui.thread`.
- `vis.ui.diff(node_id, patch)` shows a unified diff. People comment on one line of it.
  `old_revision` and `new_revision` name the compared versions.
- `vis.ui.thread(node_id)` shows one conversation.
- `vis.attach(...)` opens the app and returns a `vis.LiveAttachment` handle. `app.id` is the id of
  the app. An app takes no other argument of `vis.attach`.
- With `key`, `vis.attach` returns the open app of your extension with the same key in the same
  session. The app keeps its state. Patch it to change it.
- In the `python_execution` sandbox, import the builders with
  `from blockether.vis.extension import ui`. Then call `attach(ui.app(...))`.

### Use an app

- **Terminal.** Click the app row in the transcript, or press Ctrl+X a i and open the app there.
  Select a line, press Enter to write a comment and press Ctrl+S to save it. Press s to send.
  [Use an app](keyboard-shortcuts.md#use-an-app) lists every key.
- **Companion app.** Tap the app row in the transcript, or open the **Artifacts** sheet from the
  session header. The sheet lists the apps first.

A saved comment is a draft. Every surface shows the same drafts, so you can start in the terminal
and send from the Companion app. A Send posts the drafts as messages and sends the values of the
inputs. Vis gives each Send one receipt, also when a surface tries it again after a network error.
An app keeps its newest 50 receipts. Vis matches a retry only to a Send in that list.

### Answer a Send

Each Send gives your code one `submit` event. `event.submission_id` names its receipt, and
`event.comments` holds the messages that it posted. Each comment has `node_id`, `thread_id`,
`message_id` and `body`. A comment on a diff line also has `anchor`.

```python
def answer_sends(app):
    """Answer each Send of `app` in the thread of its first comment."""
    for event in app.events(timeout=60):
        if event.type != "submit" or event.submission_id is None:
            continue
        app.set_status(event.submission_id, "processing", note="Reading your comments")
        if not event.comments:
            app.set_status(event.submission_id, "answered")
            continue
        first = event.comments[0]
        app.reply(first["node_id"], "Thanks. I changed it.",
                  thread_id=first["thread_id"], submission_id=event.submission_id)
```

An app that the agent opens in `python_execution` has no code that waits for events. When a
person sends it, Vis adds one short message to the session of the agent. The message names the
app, the receipt and the calls that read it, but not the comments. When the agent is busy, the
message waits in the queue of the session.

People see the stage of each receipt:

| Stage | Meaning |
| --- | --- |
| `accepted` | Vis saved the Send. |
| `delivered` | Your code read the event, or the session of the agent got the message. |
| `processing` | Your code works on the Send. |
| `answered` | Your code answered. |
| `failed` | Your code could not answer. `note` tells people why. |

- `app.reply(node_id, body, *, thread_id=None, anchor=None, submission_id=None)` answers in a
  thread. On a diff, `anchor` (`{"path", "side", "line"}`) starts a new thread on that line. With
  `submission_id`, the receipt of that Send moves to `answered`.
- `app.set_status(submission_id, status, *, note=None)` moves a receipt forward. A receipt never
  moves back. `note` holds at most 500 characters.
- `app.submissions(status=None)` returns the receipts, oldest first.
  `app.submissions(status="accepted")` lists the Sends that your code did not read yet, for
  example after a gateway restart.
- `app.submission(submission_id)` returns one receipt. `app.thread(node_id, thread_id=None)`
  returns one thread.

### Add shortcuts to an app

A shortcut gives one key of an app a job. People press it in the terminal or on a hardware
keyboard in the Companion app, and the surface shows the shortcut legend.

```python
import blockether.vis.extension as vis

app = vis.attach(vis.ui.app(
    "Triage the failures",
    vis.ui.table("failures", columns=[vis.ui.table_column("test", "Test"),
                                      vis.ui.table_column("error", "Error")]),
    vis.ui.button("rerun", label="Run again"),
    key="triage",
    shortcuts=[
        vis.ui.shortcut("r", node_id="rerun", label="Run again"),
        vis.ui.shortcut("n", "next-failure", label="Next failure"),
    ],
))
```

- `vis.ui.shortcut(key, action=None, *, node_id=None, label=None)` declares one shortcut. Give
  exactly one of `action` and `node_id`.
- With `node_id`, the key presses that button or selectable card, like a click. Your code gets an
  `activate` event for that node.
- With `action`, the key sends a `shortcut` event to your code. `event.action` is the id, and
  `event.key` is the key.
- `key` is one printable character from `!` to `~` or one named key. The named keys are
  `Enter`, `Space`, `Backspace`, `Delete`, `ArrowUp`, `ArrowDown`, `ArrowLeft`, `ArrowRight`,
  `Home`, `End`, `PageUp` and `PageDown`. A letter is case-sensitive, so `R` is Shift+r. Esc and
  Tab stay with the surface.
- An app holds at most 32 shortcuts, each with its own key. `label` holds at most 40 characters.
- `app.set_shortcuts([...])` replaces the list. `app.set_shortcuts([])` removes it.
- Before you remove a node that a shortcut presses, remove that shortcut in the same patch.

[Use an app](keyboard-shortcuts.md#use-an-app) lists the keys that the terminal keeps for itself.

### Get the events of an app

An app gets its events in one of three ways:

- `vis.attach(app_declaration, on_event=callback)` runs `callback(event)` for each change, submit,
  activate, select or shortcut, as `vis.live(..., on_event=...)` does. In an extension, each call runs
  with the session of the app bound, also after your tool returns.
- `app.events(timeout=60)` returns the events that arrived since the last call.
- `app.run(callback, *, timeout=None)` serves events and blocks. It returns when the app closes,
  when `callback` returns `False`, after `timeout` seconds or on Ctrl+C. It returns the number of
  events that it served.

```python
def on_key(event):
    """Show the next failure for each press of n."""
    if event.type == "shortcut" and event.action == "next-failure":
        show_next_failure(app)

app.run(on_key)
```

### Tell the model what people see

The events of an app go to its owner, the code that opened it. They do not reach the model. The
model also does not see what a person does in an HTML page. A note gives the model the current
state as a short text. Vis sends the newest note to the model with the next message of the person.

```python
app.set_agent_note("Card 3 of 7 is open. Cards 1 and 2 are done.")
```

- `app.set_agent_note(text)` replaces the note of the app. An empty text removes the note.
- A note holds at most 4000 characters. Vis removes the spaces at its start and end.
- Write the current state, not a history. Each note replaces the note before it.
- A closed or archived app gives no note.

A script in an HTML page that you attach sets the note of that page with `vis.setAgentNote(text)`.
Only the Companion app runs this script. The terminal opens the page in the system viewer, where
`vis` does not exist. Check for it before the call:

```xml
<script>
  function showCard(number) {
    document.getElementById("card").textContent = "Card " + number;
    if (window.vis) {
      window.vis.setAgentNote("Card " + number + " of 7 is open.");
    }
  }
</script>
```

- `vis.setAgentNote` takes a string of at most 4000 characters. A value that is not a string
  raises a `TypeError`, and a longer text raises a `RangeError`.
- The Companion app saves the note after a short pause. Many fast calls give one save.
- In each session, Vis keeps the notes of the 8 pages that changed their notes last.

The model reads the notes in `session["apps"]`. Each entry has `id`, `kind` (`app` or `page`),
`title` and `agent_note`. Vis sends the notes with each new message of the person. In a long turn,
the model also sees a changed note at its next step.

### Run an app from your own program

Your own program can own an app in a session through the Python SDK. It uses the same
`vis.ui` components and the same `LiveAttachment` handle as an extension.

```python
import blockether.vis.extension as vis
from blockether.vis.engine import Agent

agent = Agent()
session = agent.session
app = session.attach(
    vis.ui.app("Release checklist", vis.ui.steps("plan", steps=[...]), key="release"),
    owner="release-bot",
)
app.run(handle_event)
```

- `owner` names your program, with 1 to 64 characters of `a-z`, `0-9`, `.`, `_` and `-`. Only
  this owner can change the app.
- With `key`, a restarted program gets its open app back with the same state.
- `on_event=callback` runs the callback on a background thread until the app closes. Vis logs an
  error of the callback and continues.
- Inside a tool of `Agent(extensions=[...])`, `vis.attach(vis.ui.app(...))` opens the app in the
  session of that call. The name of the extension is the owner.
- [Sessions API](sessions-api.md#run-an-app-from-a-program) shows the same steps over HTTP.

### After a gateway restart

The surfaces show an app of an extension or a program as disconnected after a gateway restart. The
app keeps its state, but its owner does not listen yet.

- While an app is disconnected, Vis refuses presses, selections, input changes and shortcuts with
  the reason `disconnected`. Nothing in the app changes.
- People can still save comments and send their work. A Send waits for the owner.
- Open the app again with the same `key` and the same owner. The app is then connected again, and
  the owner gets each Send that waited.
- An extension has no separate start hook. Open your apps again with their keys when your extension
  loads.
- A program with `on_event` waits for the gateway and opens a keyed app again by itself. An app
  without a key cannot come back to its program: Vis logs an error and stops the events of that app.
- An owner that opens an app again gets only the events that come after that open.

### Keep app data apart from its picture

Vis keeps the picture of an app: the nodes that people see, and the Sends of people. It does not
keep the data of your app. Keep that data yourself, and draw the picture from it.

- Vis keeps only the last picture of an open app. When the record of a busy app grows large, Vis
  rewrites it as that picture. A log node keeps every line.
- Store the data that must outlive your process in your own store, for example
  [`vis.state`](extension-api.md#durable-state) in an extension.
- Write that data when it changes, not on each tick. A game that writes on each tick fills the
  store with pictures.
- After a restart, read your data, draw the picture again and update the adopted app.

The Asteroids app of the live apps example stores only its best score. A restart starts a new game
with that best score.

### Keep and end an app

- `vis.attach(row)` with the app row from `vis.list_attachments()` returns a new handle for the
  same app. Use it after a gateway restart or in a later `python_execution` block.
- `app.close()` ends the app. People can still read it, but they cannot comment or send.
- `app.archive()` ends the app and moves it to the archived list of the session.
- While an app of an extension is open, Vis keeps that extension running in the session, also when
  the session is idle. Close or archive an app that you no longer use.
- A program reads apps with `live_views(placement="attachment")` and `live_view(view_id)`, as
  [Sessions API](sessions-api.md#watch-live-views) shows.

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
recorder.press_shortcut("n")             # an app shortcut
assert recorder.node("jobs")["selected_ids"] == ["macos"]
assert recorder.patched()                # ops the extension emitted
result = recorder.close(reason="interrupted")
```

`recorder.ops()` returns the recorded operations for comparison with expected
output. `vis.testing.assert_tree(actual, expected)` reports nested differences.

## See also

- [Extending Vis](extending.md) — the extension that opens a view.
- [Forms and user input](human-input.md) — the same components, used for questions.
- [Components](components.md) — every component and its builder.
- [Keyboard shortcuts](keyboard-shortcuts.md#use-an-app) — the keys of an app in the terminal.
- [Sessions API](sessions-api.md#watch-live-views) — list, read and interrupt live views from a
  program.
