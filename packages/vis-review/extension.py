"""Vis entrypoint: a write journal of every block, and the Review app.

After each Python block, the journal records what changed before and during it,
without delaying the block. Each leader session lists the app from its start:
the checkpoints, the changed files and their diff, the checks, an AI review and undo.
The app opens when a person opens it, or at the first step of the session. After a
reload, the `on_reload` hook of the app connects it again. A subagent has no app:
its journal names its leader, and the app of the leader shows its changes as own.
The sessions of the AI review record nothing.
The logic lives in `vis_review`; the SDK program `python -m vis_review` uses it too.
"""

import hashlib
import threading

import blockether.vis.extension as vis

from vis_review.app import ReviewApp, extension_opener
from vis_review.journal import member_key
from vis_review.recorder import (
    Recorder,
    block_label,
    block_wrote,
)

RECORDER = Recorder()
# A block hook and `reopen` can ask for the app of one journal at the same time.
_MAKING = threading.Lock()
# The journal key of each session; None for a session that records nothing.
_KEYS: dict[str, str | None] = {}


def _session_id():
    """The bound session, or None outside a session."""
    try:
        return vis.session.id
    except RuntimeError:
        return None


def _journal_key(session_id):
    """The journal key of a session: its id, or the member key of a subagent.

    None for a session of the AI review, which changes no file.
    """
    if session_id not in _KEYS:
        record = vis.session.read()
        agent = record.get("agent") or {}
        origin = record.get("origin") or {}
        if origin.get("extension") == "vis-review":
            _KEYS[session_id] = None
        elif agent.get("role") == "subagent" and agent.get("leader_id"):
            _KEYS[session_id] = member_key(agent["leader_id"], session_id)
        else:
            _KEYS[session_id] = session_id
    return _KEYS[session_id]


def after_step(call):
    """Record the changes since the last scan without delaying the block."""
    session_id = call.get("session_id") or _session_id()
    key = _journal_key(session_id) if session_id else None
    if not key:
        return
    root = vis.workspace_root()
    args = call.get("args")
    RECORDER.record(
        key,
        root,
        "block",
        block_label(args),
        block_wrote(args),
        started_at=call.get("started_at"),
        ended_at=call.get("ended_at"),
        turn=call.get("turn"),
        iteration=call.get("iteration"),
        call=call.get("call_id"),
    )
    # An open that an interrupt cut short leaves an app without a view.
    if key == session_id:
        journal = RECORDER.journal(key, root)
        app = RECORDER.app(key, journal.root)
        if app is None or app.view is None:
            _app(key, journal).open(checkpoint=False)


def _call(tool, verb, **arguments):
    return vis.tools[tool][verb](**arguments)


def _ask(request, model):
    return vis.run(request, response_model=model, is_child=True)


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
    if not session_id or _journal_key(session_id) != session_id:
        return
    journal = RECORDER.journal(session_id, vis.workspace_root())
    # Another root of this session has another key, and its own app.
    if app.get("key") == _key(journal):
        _app(session_id, journal).open()


def open_review(app):
    """The `open` function of the session app: open the review app of a leader session.

    A subagent opens no app, also one that it saved before it lost the app.
    """
    session_id = str(app.get("session_id") or "")
    if session_id and _journal_key(session_id) == session_id:
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
            roles=("leader",),
        ),
        op_hooks=[
            vis.OpHook(["py3"], after_step, phase="after", is_async=True),
        ],
    )
)
