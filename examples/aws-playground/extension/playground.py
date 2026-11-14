"""Vis entrypoint of the playground extension.

It gives every session of the playground three things:

- A copy in S3 of each file that a step wrote in the project directory or in `/tmp`.
  The copy is temporary: S3 removes it after some days.
- The `playground` tools: list and start workflows, save an artifact and publish the
  commits of the project to GitHub.
- The `/workflows` command, which opens a panel that starts workflows.

The extension has no AWS credentials. It calls the playground API of the worker on
the loopback interface. The file needs only the standard library and the Vis SDK.
"""

import base64
import json
import logging
import os
import stat
import threading
import time
import urllib.error
import urllib.parse
import urllib.request

import blockether.vis.extension as vis

LOG = logging.getLogger("playground")
# Directories that are not the work of a session.
SKIPPED = {".git", "__pycache__", "node_modules", ".cpcache", ".venv", "target", ".pytest_cache", ".ruff_cache", ".clj-kondo", ".lsp"}
# Entries of /tmp that belong to the worker (it names them) or to Vis and the tools.
PRIVATE_TMP = set(os.environ.get("PLAYGROUND_PRIVATE_TMP", "").split(":"))
INTERNAL_TMP = (".", "vis-gateway-", "vis-turn-trace-", "uv-", "hsperfdata_")
FILE_LIMIT, CALL_LIMIT, SCAN_LIMIT = 1_000_000, 4_000_000, 20_000
_seen = {}
_lock = threading.Lock()
_panels = {}


def call(method, path, body=None, timeout=120):
    """Call the playground API and return the bytes of the answer."""
    data = None if body is None else json.dumps(body).encode()
    address = os.environ["PLAYGROUND_BROKER"] + "/play/api/" + path
    request = urllib.request.Request(address, data, {"content-type": "application/json"}, method=method)
    try:
        with urllib.request.urlopen(request, timeout=timeout) as answer:
            return answer.read()
    except urllib.error.HTTPError as failure:
        try:
            message = json.loads(failure.read())["error"]
        except (ValueError, KeyError, TypeError):
            message = f"HTTP {failure.code}"
        raise RuntimeError(message) from None


def api(method, path, body=None, timeout=120):
    """Call the playground API and return the JSON answer."""
    return json.loads(call(method, path, body, timeout) or b"null")


def _session_id():
    try:
        return vis.session.id
    except RuntimeError:
        return None


# -- copy the written files to S3 ----------------------------------------------


def _roots():
    """Return the label and the path of each directory that a session writes in."""
    return {"project": str(vis.workspace_root()), "tmp": "/tmp"}


def _private(name):
    return name in PRIVATE_TMP or name.startswith(INTERNAL_TMP)


def _scan(label, root):
    """Return the size and the time of each file below root."""
    found = {}
    for folder, folders, names in os.walk(root):
        private = _private if label == "tmp" and folder == root else (lambda name: False)
        folders[:] = [f for f in folders if f not in SKIPPED and not private(f)]
        for name in names:
            if private(name):
                continue
            path = os.path.join(folder, name)
            try:
                info = os.lstat(path)
            except OSError:
                continue
            # A link can name a file outside the directory: copy only regular files.
            if not stat.S_ISREG(info.st_mode):
                continue
            found[os.path.relpath(path, root)] = (info.st_size, info.st_mtime_ns)
            if len(found) >= SCAN_LIMIT:
                return found
    return found


def before_step(call):
    """Remember the files before the first step, so the first copy holds only new work."""
    try:
        with _lock:
            for label, root in _roots().items():
                if root not in _seen:
                    _seen[root] = _scan(label, root)
    except Exception:  # noqa: BLE001 - a copy error must never stop a step
        LOG.exception("playground: before %s", call.get("op"))


def after_step(call):
    """Copy each file that changed to `tmp/<session>/` in S3."""
    session = _session_id()
    if not session:
        return
    try:
        with _lock:
            changes = []
            for label, root in _roots().items():
                current = _scan(label, root)
                previous = _seen.get(root, current)
                _seen[root] = current
                changed = [path for path, state in current.items() if previous.get(path) != state]
                removed = [f"{label}/{path}" for path in previous if path not in current]
                changes.append((label, root, changed, removed))
        for label, root, changed, removed in changes:
            _copy(session, label, root, changed, removed)
    except Exception:  # noqa: BLE001 - a copy error must never stop a step
        LOG.exception("playground: after %s", call.get("op"))


def _copy(session, label, root, changed, removed):
    batch, size = [], 0
    for path in changed:
        try:
            with open(os.open(os.path.join(root, path), os.O_RDONLY | os.O_NOFOLLOW), "rb") as file:
                if os.fstat(file.fileno()).st_size > FILE_LIMIT:
                    continue
                data = file.read(FILE_LIMIT + 1)
        except OSError:
            continue
        if len(data) > FILE_LIMIT:
            continue
        if size + len(data) > CALL_LIMIT and batch:
            api("POST", "tmp", {"session": session, "files": batch})
            batch, size = [], 0
        batch.append({"path": f"{label}/{path}", "data": base64.b64encode(data).decode()})
        size += len(data)
    if batch or removed:
        api("POST", "tmp", {"session": session, "files": batch, "removed": removed})


# -- tools ----------------------------------------------------------------------


def _input(about, value):
    """Return the input of a run from a dict, a JSON text or a plain text."""
    if isinstance(value, dict):
        return value
    text = str(value or "").strip()
    if text.startswith("{"):
        return json.loads(text)
    return {about["inputs"][0]["name"]: text} if text and about["inputs"] else {}


def _present(headline, summary, *content, **options):
    return vis.ActivityPresentation(headline, str(summary)[:500], tuple(content), **options)


def _failure(headline, error):
    return _present(headline, f"Failed: {error}", verdict="failed")


def render_workflows(phase, args, kwargs, result, error):
    if phase == "failure":
        return _failure("List workflows", error)
    if phase != "success":
        return None
    rows = [(flow["name"], flow["title"], ", ".join(step["title"] for step in flow["activities"])) for flow in result]
    table = vis.ActivityTable(("Name", "Title", "Activities"), tuple(rows)) if rows else vis.ActivityText("No workflows.")
    return _present("List workflows", f"{len(result)} workflows in the playground", table)


def render_run(phase, args, kwargs, result, error):
    name = (args[0] if args else kwargs.get("name")) or "workflow"
    if phase == "failure":
        return _failure(f"Start workflow {name}", error)
    if phase != "success":
        return _present(f"Start workflow {name}", "Starting the run")
    text = json.dumps(result["input"], indent=2)
    return _present(f"Start workflow {name}", f"Run {result['id'][:8]} of {result['title']} is {result['status']}", vis.ActivityCode(text, language="json"), outcome=result["status"])


def render_status(phase, args, kwargs, result, error):
    if phase == "failure":
        return _failure("Read workflow run", error)
    if phase != "success":
        return None
    rows = [(step["title"], step["status"], ", ".join(step["artifacts"])) for step in result["activities"]]
    table = vis.ActivityTable(("Activity", "Status", "Artifacts"), tuple(rows)) if rows else vis.ActivityText("No activity started.")
    return _present(f"Workflow run {result['id'][:8]}", f"{result['title']} is {result['status']}", table, outcome=result["status"])


def render_artifact(phase, args, kwargs, result, error):
    name = (args[0] if args else kwargs.get("name")) or "artifact"
    if phase == "failure":
        return _failure(f"Save artifact {name}", error)
    if phase != "success":
        return None
    content = args[1] if len(args) > 1 else kwargs.get("content", "")
    block = vis.ActivityMarkdown(content) if name.endswith(".md") else vis.ActivityCode(content)
    return _present(f"Save artifact {name}", f"Saved {result['size']} bytes to S3", block, outcome="saved", handle_id=f"artifact:{name}")


def render_read(phase, args, kwargs, result, error):
    name = (args[0] if args else kwargs.get("name")) or "artifact"
    if phase != "success":
        return _failure(f"Read artifact {name}", error) if phase == "failure" else None
    block = vis.ActivityMarkdown(result) if name.endswith(".md") else vis.ActivityCode(result)
    return _present(f"Read artifact {name}", f"{len(result)} characters from S3", block, handle_id=f"artifact:{name}")


def render_artifacts(phase, args, kwargs, result, error):
    if phase != "success":
        return _failure("List artifacts", error) if phase == "failure" else None
    rows = tuple((item["name"], f"{item['size']} bytes") for item in result)
    table = vis.ActivityTable(("Artifact", "Size"), rows) if rows else vis.ActivityText("No artifacts.")
    return _present("List artifacts", f"{len(result)} artifacts of this session", table)


def render_publish(phase, args, kwargs, result, error):
    if phase == "failure":
        return _failure("Publish to GitHub", error)
    if phase != "success":
        return _present("Publish to GitHub", "Pushing the commits of the project")
    where = "the main branch" if result["merged"] else f"the new branch {result['branch']}"
    text = f"Pushed {result['commits']} commit(s) to {where}: [{result['sha'][:12]}]({result['url']})"
    return _present("Publish to GitHub", text, summary_format="markdown", outcome=result["branch"])


class Playground:
    """The tools of the playground. The sandbox has them as `playground.<name>`."""

    @vis.method(activity=vis.Activity(label="List workflows", render=render_workflows, show_start=False))
    def workflows(self) -> list:
        """List the workflows of the playground with their inputs and activities."""
        return api("GET", "workflows")["workflows"]

    @vis.method(tag="mutation", activity=vis.Activity(label="Start workflow", render=render_run))
    def run_workflow(self, name: str, input: dict | str | None = None) -> dict:  # noqa: A002 - the tool argument is `input`
        """Start a run of a workflow. `input` is a dict of its inputs, or a text for its first input.

        The run works in its own Vis session. This call returns at once with the run id.
        Read the progress with `run_status`.
        """
        about = api("GET", f"workflows/{name}")
        return api("POST", "runs", {"workflow": name, "input": _input(about, input)})

    @vis.method(activity=vis.Activity(label="Read workflow run", render=render_status, show_start=False))
    def run_status(self, run_id: str) -> dict:
        """Return a run: its status, its activities with their results, and its artifacts."""
        run = api("GET", f"runs/{run_id}")
        run.pop("source", None)
        return run

    @vis.method(tag="mutation", activity=vis.Activity(label="Save artifact", render=render_artifact, show_start=False))
    def save_artifact(self, name: str, content: str) -> dict:
        """Save a document of this session, such as SPEC.md, as an artifact in S3.

        The artifact shows in the transcript, on the sessions page and in the run of a workflow.
        """
        return api("POST", "artifacts", {"session": vis.session.id, "name": name, "content": content, "source": "session"})

    @vis.method(activity=vis.Activity(label="List artifacts", render=render_artifacts, show_start=False))
    def artifacts(self) -> list:
        """List the artifacts of this session."""
        return api("GET", f"artifacts?session={vis.session.id}")["artifacts"]

    @vis.method(activity=vis.Activity(label="Read artifact", render=render_read, show_start=False))
    def read_artifact(self, name: str) -> str:
        """Return the text of one artifact of this session."""
        return call("GET", f"artifacts/{vis.session.id}/{urllib.parse.quote(name)}").decode()

    @vis.method(tag="external", activity=vis.Activity(label="Publish to GitHub", render=render_publish))
    def publish(self, message: str) -> dict:
        """Commit the work in the project directory and push it to the GitHub repository of the playground.

        The push goes to the main branch. When main moved, it goes to a new branch
        `playground/<id>`. The result names the branch, the commit and its URL.
        """
        return api("POST", "publish", {"root": str(vis.workspace_root()), "message": message}, timeout=300)


# -- the /workflows panel --------------------------------------------------------


# The tone of a run status. The names are the live tones of the Vis view contract.
TONES = {"queued": "idle", "running": "running", "completed": "ok", "failed": "error", "cancelled": "warn"}


def _rows():
    lines = api("GET", "runs")["runs"][:12]
    return [
        vis.ui.table_row(
            line["id"],
            [line["title"], line["status"], f"{sum(1 for _, status in line['activities'] if status == 'completed')}/{len(line['activities'])}"],
            tone=TONES[line["status"]],
        )
        for line in lines
    ]


def _follow(view, run_id):
    """Show the progress of a run in the panel until the run ends."""
    while True:
        time.sleep(3)
        try:
            run, rows = api("GET", f"runs/{run_id}"), _rows()
        except (OSError, RuntimeError):  # the worker stopped: the next panel reads the runs again
            return
        done = sum(1 for step in run["activities"] if step["status"] == "completed")
        view["runs"].replace(rows)
        view["state"].set(f"{run['title']}: {run['status']}. Activities completed: {done}", tone=TONES[run["status"]])
        # A live view sends the first update at once and keeps the next for a later call.
        # Without this call, the last status of a run stays in the buffer.
        view.flush()
        if run["status"] not in ("queued", "running"):
            return


def _start(view, name, text):
    about = api("GET", f"workflows/{name}")
    run = api("POST", "runs", {"workflow": name, "input": _input(about, text)})
    view["state"].set(f"{run['title']}: run {run['id'][:8]} started", tone="running")
    threading.Thread(target=_follow, args=(view, run["id"]), daemon=True).start()
    return run


def _panel_event(session):
    def handle(event):
        view = _panels.get(session)
        if view is None or event.type != "submit" or event.node_id != "run":
            return
        try:
            _start(view, event.values["workflow"], event.values.get("input"))
        except Exception as failure:  # noqa: BLE001 - shown to the person
            view["state"].set(f"The run did not start: {failure}", tone="error")

    return handle


def open_workflows(ctx):
    """Open the workflows panel, or start a run with `/workflows <name> <input>`."""
    session = str(ctx.get("session_id") or "")
    if not session:
        return vis.err("Open /workflows in a session")
    flows = api("GET", "workflows")["workflows"]
    words = ctx.get("args") or []
    if words:
        if words[0] not in {flow["name"] for flow in flows}:
            return vis.err(f"No workflow {words[0]}", "Workflows: " + ", ".join(flow["name"] for flow in flows))
        about = api("GET", f"workflows/{words[0]}")
        run = api("POST", "runs", {"workflow": words[0], "input": _input(about, " ".join(words[1:]))})
        return vis.ok(f"Run {run['id'][:8]} of {run['title']} started", "Open **Workflows** in the playground to follow it.", {"run_id": run["id"]})
    view = vis.live(
        "Workflows",
        [
            vis.ui.paragraph("intro", "Start a workflow of the playground. One run is one Vis session."),
            vis.ui.group(
                "start",
                vis.ui.select("workflow", [vis.ui.option(flow["name"], flow["title"]) for flow in flows], label="Workflow", is_required=True),
                vis.ui.multiline("input", label="Input", description="A text for the first input, or a JSON object."),
                vis.ui.button("run", "Run workflow", action="submit", variant="primary"),
            ),
            vis.ui.status("state", "Ready", tone="idle"),
            vis.ui.table(
                "runs",
                [vis.ui.table_column("workflow", "Workflow"), vis.ui.table_column("status", "Status"), vis.ui.table_column("done", "Activities")],
                rows=_rows(),
            ),
        ],
        placement="panel",
        key="playground-workflows",
        session_id=session,
        on_event=_panel_event(session),
    )
    _panels[session] = view
    return vis.ok("Workflows are open", f"{len(flows)} workflows. Select one and press Run workflow.")


PROMPT = """The playground runs on AWS Lambda. The project directory is a clone of the GitHub repository of the playground.
- There is no internet access. Do not install packages.
- `playground.save_artifact(name, content)` saves a document such as SPEC.md. Use it for each document that a person asks for.
- `playground.artifacts()` lists the saved documents and `playground.read_artifact(name)` returns the text of one.
- `playground.publish(message)` commits the project and pushes it to GitHub. Use it only when a person asks to publish.
- `playground.workflows()` and `playground.run_workflow(name, input)` list and start the workflows."""

STEPS = ["python_execution", "patch", "shell"]

vis.register_extension(
    vis.Extension(
        name="playground",
        description="The Vis playground on AWS Lambda: workflows, artifacts in S3 and publishing to GitHub.",
        alias="playground",
        prompt=PROMPT,
        symbols=[vis.Symbol(Playground(), name="playground")],
        slash_commands=[
            vis.SlashCommand("workflows", open_workflows, doc="Open the workflows of the playground, or start one.", usage="/workflows [name] [input]")
        ],
        op_hooks=[vis.OpHook(STEPS, before_step), vis.OpHook(STEPS, after_step, phase="after")],
    )
)
