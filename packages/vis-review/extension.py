"""Vis entrypoint: a write journal of every block and patch, and the `/review` app.

Before each Python block, the journal records the changes made since the last
block, by a person or another program. After each block and each patch, it
records what that step changed. `/review` opens the app of this session: the
checkpoints, the changed files and their diff, the checks, an AI review and undo.
The logic lives in `vis_review`; the SDK program `python -m vis_review` uses it too.
"""

import hashlib
import logging

import blockether.vis.extension as vis

from vis_review.app import ReviewApp, extension_opener
from vis_review.recorder import Recorder, block_label, patch_label

LOG = logging.getLogger("vis_review")
RECORDER = Recorder()


def _session_id():
    """The bound session, or None outside a session."""
    try:
        return vis.session.id
    except RuntimeError:
        return None


def _record(kind, label_of):
    session_id = _session_id()
    if not session_id:
        return
    root = vis.workspace_root()
    RECORDER.record(session_id, root, kind, label_of(str(root)))


def before_block(call):
    """Record the changes made since the last block, before this block runs."""
    try:
        _record("outside", lambda _root: "Changes between blocks")
    except Exception:  # noqa: BLE001 - a journal error must never stop a block
        LOG.exception("vis-review: before python_execution")


def after_step(call):
    """Record what a block or a patch changed."""
    try:
        if call.get("op") == "patch":
            if call.get("result") is not None:
                _record("patch", lambda root: patch_label(call.get("args"), root))
        else:
            _record("block", lambda _root: block_label(call.get("args")))
    except Exception:  # noqa: BLE001 - a journal error must never stop a block
        LOG.exception("vis-review: after %s", call.get("op"))


def _call(tool, verb, **arguments):
    return vis.tools[tool][verb](**arguments)


def _ask(request, model):
    return vis.run(request, response_model=model)


def run_review(ctx):
    """Open the review app of this session."""
    session_id = str(ctx.get("session_id") or "")
    if not session_id:
        return vis.err("Open /review in a session", "The journal records one session.")
    root = str(vis.workspace_root())
    journal = RECORDER.journal(session_id, root)
    app = RECORDER.app(session_id, journal.root)
    if app is None:
        session = vis.sessions.get(session_id)
        app = ReviewApp(
            journal,
            extension_opener,
            call=_call,
            ask=_ask,
            send=session.send,
            key="review-" + hashlib.sha256(journal.root.encode()).hexdigest()[:8],
        )
        RECORDER.keep(session_id, app)
    try:
        app.open()
    except Exception as error:  # noqa: BLE001 - shown to the person and logged
        LOG.exception("/review failed")
        return vis.err("The review did not open", f"{type(error).__name__}: {error}")
    count = len(journal.entries)
    return vis.ok(
        "Review is open",
        f"{count} checkpoints of {journal.root}.",
        {"view_id": app.view.id, "checkpoints": count},
    )


vis.register_extension(
    vis.Extension(
        name="vis-review",
        description="Review what a session changed: a journal of every block, diffs, checks, an AI review and undo.",
        slash_commands=[
            vis.SlashCommand(
                "review",
                run_review,
                doc="Open the review of this session: changes, checks and undo.",
                usage="/review",
            )
        ],
        keybindings=[vis.Keybinding("R", command="/review", label="Review")],
        op_hooks=[
            vis.OpHook(["python_execution"], before_block),
            vis.OpHook(["python_execution", "patch"], after_step, phase="after"),
        ],
    )
)
