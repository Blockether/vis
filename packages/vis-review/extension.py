"""Vis entrypoint: a write journal of every block and patch, and the Review app.

Before each Python block, the journal records the changes made since the last
block, by a person or another program. After each block and each patch, it
records what that step changed. Each session lists the app from its start: the
checkpoints, the changed files and their diff, the checks, an AI review and undo.
The app opens when a person opens it, or at the first step of the session. After a
reload, the `on_reload` hook of the app connects it again.
The logic lives in `vis_review`; the SDK program `python -m vis_review` uses it too.
"""

import hashlib
import logging
import threading

import blockether.vis.extension as vis

from vis_review.app import ReviewApp, extension_opener
from vis_review.recorder import (
    Recorder,
    block_label,
    block_wrote,
    patch_label,
    patch_wrote,
)

LOG = logging.getLogger("vis_review")
RECORDER = Recorder()
# A block hook and `reopen` can ask for the app of one journal at the same time.
_MAKING = threading.Lock()


def _session_id():
    """The bound session, or None outside a session."""
    try:
        return vis.session.id
    except RuntimeError:
        return None


def _record(kind, label_of, wrote_of=None):
    session_id = _session_id()
    if not session_id:
        return
    root = vis.workspace_root()
    wrote = wrote_of(str(root)) if wrote_of else None
    RECORDER.record(session_id, root, kind, label_of(str(root)), wrote)
    # The first step opens the app, so a person sees it at once.
    journal = RECORDER.journal(session_id, root)
    if RECORDER.app(session_id, journal.root) is None:
        _app(session_id, journal).open()


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
                _record(
                    "patch",
                    lambda root: patch_label(call.get("args"), root),
                    lambda root: patch_wrote(call.get("args"), root),
                )
        else:
            _record(
                "block",
                lambda _root: block_label(call.get("args")),
                lambda _root: block_wrote(call.get("args")),
            )
    except Exception:  # noqa: BLE001 - a journal error must never stop a block
        LOG.exception("vis-review: after %s", call.get("op"))


def _call(tool, verb, **arguments):
    return vis.tools[tool][verb](**arguments)


def _ask(request, model):
    return vis.run(request, response_model=model)


def _key(journal):
    """The app key of a journal: one review app for each root of a session."""
    return "review-" + hashlib.sha256(journal.root.encode()).hexdigest()[:8]


def _app(session_id, journal):
    """The review app of a session and a journal, made once and not yet open."""
    with _MAKING:
        app = RECORDER.app(session_id, journal.root)
        if app is None:
            app = ReviewApp(
                journal,
                _opener,
                call=_call,
                ask=_ask,
                is_session_scoped=True,
                key=_key(journal),
            )
            RECORDER.keep(session_id, app)
        return app


def _opener(declaration, on_event):
    """Open the app with the hook that connects it again after a reload."""
    return extension_opener(declaration, on_event, on_reload=reopen)


def reopen(app):
    """The `on_reload` hook: connect the review app of a session again."""
    session_id = str(app.get("session_id") or "")
    if not session_id:
        return
    journal = RECORDER.journal(session_id, vis.workspace_root())
    # Another root of this session has another key, and its own app.
    if app.get("key") == _key(journal):
        _app(session_id, journal).open()


def open_review(app):
    """The `open` function of the session app: open the review app of a session."""
    session_id = str(app.get("session_id") or "")
    if session_id:
        journal = RECORDER.journal(session_id, vis.workspace_root())
        _app(session_id, journal).open()


vis.register_extension(
    vis.Extension(
        name="vis-review",
        description="Review what a session changed: a journal of every block, diffs, checks, an AI review and undo.",
        session_app=vis.SessionApp(
            "review",
            "Review",
            open_review,
            icon="review",
            surfaces={"inline": False, "session": "sidebar"},
        ),
        op_hooks=[
            vis.OpHook(["python_execution"], before_block),
            vis.OpHook(["python_execution", "patch"], after_step, phase="after"),
        ],
    )
)
