"""Vis entrypoint of the playground extension.

It gives every session of the playground three things:

- A copy in S3 of each file that a step wrote in the project directory or in `/tmp`.
  The copy is temporary: S3 removes it after 7 days.
- The `playground` tools: list and start workflows, read a run with `run_status`, and
  publish the commits of the project to GitHub.
- The `/workflows` command, which starts a workflow. The Workflows app of Vis is not here:
  the worker owns it (`playground/apps.py`), because a global app belongs to no session.

The extension has no AWS credentials in its environment (code of a session can read
those of the worker process: see the README). It calls the playground API of the worker on
the loopback interface. The file needs only the standard library and the Vis SDK.

A document of a session is a native Vis attachment: the model calls `attach`. The
extension adds no tool for it. The API copies each attached file to S3 for the pages.
"""

import base64
import json
import logging
import os
import stat
import threading
import urllib.error
import urllib.request

import blockether.vis.extension as vis

LOG = logging.getLogger("playground")
# Directories that are not the work of a session.
SKIPPED = {".git", "__pycache__", "node_modules", ".cpcache", ".venv", "target", ".pytest_cache", ".ruff_cache", ".clj-kondo", ".lsp"}
# Entries of /tmp that belong to the worker (it names them) or to Vis and the tools.
PRIVATE_TMP = set(os.environ.get("PLAYGROUND_PRIVATE_TMP", "").split(":"))
INTERNAL_TMP = (".", "vis-gateway-", "vis-turn-trace-", "uv-", "hsperfdata_")
FILE_LIMIT, CALL_LIMIT, SCAN_LIMIT = 1_000_000, 4_000_000, 20_000
# For each directory: the size and the time of its files at the last scan.
_seen = {}
_lock = threading.Lock()


def call(method, path, body=None, timeout=120):
    """Call the playground API and return the bytes of the answer.

    The gateway loads this file alone, without the `playground` package of the worker.
    Thus it cannot share this call with `playground/client.py`.
    """
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
    """Return the session of this step, or None when a step runs outside a session."""
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


def before_step(step):
    """Remember the files before the first step, so the first copy holds only new work."""
    try:
        with _lock:
            for label, root in _roots().items():
                if root not in _seen:
                    _seen[root] = _scan(label, root)
    except Exception:  # noqa: BLE001 - a copy error must never stop a step
        LOG.exception("playground: before %s", step.get("op"))


def after_step(step):
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
        LOG.exception("playground: after %s", step.get("op"))


def _copy(session, label, root, changed, removed):
    """Send the changed files of one directory to the API, in calls of at most CALL_LIMIT bytes."""
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


def _run(name, value):
    """Start a run of a workflow and return the run. The input is a dict, a JSON text or a plain text."""
    return api("POST", "runs", {"workflow": name, "input": value or {}})


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
    rows = [(step["title"], step["status"], ", ".join(step["files"])) for step in result["activities"]]
    table = vis.ActivityTable(("Activity", "Status", "Files"), tuple(rows)) if rows else vis.ActivityText("No activity started.")
    return _present(f"Workflow run {result['id'][:8]}", f"{result['title']} is {result['status']}", table, outcome=result["status"])


def render_publish(phase, args, kwargs, result, error):
    if phase == "failure":
        return _failure("Publish to GitHub", error)
    if phase != "success":
        return _present("Publish to GitHub", "Pushing the commits of the project")
    where = "the main branch" if result["merged"] else f"the branch {result['branch']}"
    did = f"Pushed {result['commits']} commit(s) to {where}" if result["pushed"] else f"Nothing to push, because {where} has the commits"
    text = f"{did}: [{result['sha'][:12]}]({result['url']})"
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
        return _run(name, input)

    @vis.method(activity=vis.Activity(label="Read workflow run", render=render_status, show_start=False))
    def run_status(self, run_id: str) -> dict:
        """Return a run: its status, its activities with their results, and its files."""
        run = api("GET", f"runs/{run_id}")
        run.pop("source", None)
        return run

    @vis.method(tag="external", activity=vis.Activity(label="Publish to GitHub", render=render_publish))
    def publish(self, message: str) -> dict:
        """Commit the work in the project directory and push it to the GitHub repository of the playground.

        The push goes to the main branch. When main moved, it goes to a new branch
        `playground/<id>`. The result names the branch, the commit and its URL.
        """
        return api("POST", "publish", {"root": str(vis.workspace_root()), "message": message}, timeout=300)


# -- the /workflows command ------------------------------------------------------


def start_workflow(ctx):
    """Start a run with `/workflows <name> <input>`, or list the workflows."""
    flows = api("GET", "workflows")["workflows"]
    names = ", ".join(flow["name"] for flow in flows)
    words = ctx.get("args") or []
    if not words:
        return vis.ok(f"{len(flows)} workflows", f"Open **Workflows** with the global apps button in the header of Vis, or start one with `/workflows <name> <input>`. Workflows: {names}.")
    if words[0] not in {flow["name"] for flow in flows}:
        return vis.err(f"No workflow {words[0]}", "Workflows: " + names)
    run = _run(words[0], " ".join(words[1:]))
    return vis.ok(f"Run {run['id'][:8]} of {run['title']} started", "Open **Workflows** with the global apps button in the header of Vis to follow it.", {"run_id": run["id"]})


PROMPT = """The playground runs on AWS Lambda. The project directory is a clone of the GitHub repository of the playground.
- There is no internet access. Do not install packages.
- `attach(text.encode(), "SPEC.md")` saves a document. Use it for each document that a person asks for. The playground shows each attached file on its pages.
- `list_attachments()` lists the documents of the session and `read_attachment(name)` returns the bytes of one. Attach a new version with the same name.
- `playground.publish(message)` commits the project and pushes it to GitHub. Use it only when a person asks to publish.
- `playground.workflows()` and `playground.run_workflow(name, input)` list and start the workflows."""

# The operations that can write a file.
STEPS = ["python_execution", "patch", "shell"]

vis.register_extension(
    vis.Extension(
        name="playground",
        description="The Vis playground on AWS Lambda: workflows, copies of the session files in S3 and publishing to GitHub.",
        alias="playground",
        prompt=PROMPT,
        symbols=[vis.Symbol(Playground(), name="playground")],
        slash_commands=[vis.SlashCommand("workflows", start_workflow, doc="Start a workflow of the playground, or list the workflows.", usage="/workflows [name] [input]")],
        op_hooks=[vis.OpHook(STEPS, before_step), vis.OpHook(STEPS, after_step, phase="after")],
    )
)
