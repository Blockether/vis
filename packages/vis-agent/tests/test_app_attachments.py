"""Apps: `vis.attach(vis.ui.app(...))` opens a live view that stays in the session.

#421: people save drafts and Send in the TUI and in the Companion app. The owner
answers with `reply` and moves each Send receipt with `set_status`.
"""

import json

import blockether.vis.extension as vis
import pytest
from blockether.vis._contracts import validate

PATCH = (
    "diff --git a/src/a.py b/src/a.py\n--- a/src/a.py\n+++ b/src/a.py\n"
    "@@ -1,2 +1,2 @@\n x = 1\n-y = 2\n+y = 3\n"
)
LINE = {"path": "src/a.py", "side": "new", "line": 2}
APP_ROW = {"media_type": "application/vnd.vis.app+json", "view_id": "app-1"}
SEND = {
    "type": "submit",
    "node_id": "change",
    "seq": 1,
    "values": {},
    "submission_id": "sub-1",
    "comments": [
        {
            "node_id": "change",
            "thread_id": "t1",
            "message_id": "m1",
            "body": "Why y = 3?",
            "anchor": LINE,
        }
    ],
}


class AppHost:
    """Answer the app ops of the engine seam from one app in memory."""

    def __init__(self, real):
        self._real = real
        self.sent = []
        self.view = None

    def __getattr__(self, name):
        return getattr(self._real, name)

    def live(self, envelope_json, listener=None):
        envelope = json.loads(envelope_json)
        self.sent.append(envelope)
        return json.dumps(getattr(self, "_" + envelope["op"])(envelope))

    def ops(self, name):
        return [envelope for envelope in self.sent if envelope["op"] == name]

    def _bump(self):
        self.view["seq"] += 1
        return self.view["seq"]

    def _node(self, node_id):
        return next(node for node in self.view["nodes"] if node["id"] == node_id)

    def _open(self, envelope):
        receipt = {
            "id": "sub-1",
            "idempotency_key": "send-1",
            "status": "accepted",
            "created_at": 1,
            "updated_at": 1,
        }
        self.view = dict(envelope["view"], id="app-1", seq=0, submissions=[receipt])
        return {"view_id": "app-1", "is_open": True, "view": self.view}

    def _state(self, envelope):
        is_open = not self.view.get("is_closed")
        return {"view_id": "app-1", "is_open": is_open, "view": self.view}

    def _events(self, envelope):
        return {"view_id": "app-1", "is_open": True, "cursor": 1, "events": [SEND]}

    def _patch(self, envelope):
        return {"view_id": "app-1", "is_open": True, "seq": self._bump()}

    def _reply(self, envelope):
        node = self._node(envelope["node_id"])
        message = {"id": f"m-{len(self.sent)}", "body": envelope["body"]}
        node.setdefault("messages", []).append(message)
        return {
            "view_id": "app-1",
            "node_id": node["id"],
            "thread_id": envelope.get("thread_id") or node["id"],
            "message_id": message["id"],
            "seq": self._bump(),
        }

    def _submission(self, envelope):
        receipt = next(
            item
            for item in self.view["submissions"]
            if item["id"] == envelope["submission_id"]
        )
        receipt.update(status=envelope["status"], updated_at=2)
        return {"view_id": "app-1", "submission": dict(receipt), "seq": self._bump()}

    def _archive(self, envelope):
        self.view.update(is_archived=True, is_closed=True)
        return {"view_id": "app-1", "is_open": False, "is_archived": True}

    def _close(self, envelope):
        self.view["is_closed"] = True
        return {"view_id": "app-1", "is_open": False}


@pytest.fixture
def host(monkeypatch):
    fake = AppHost(vis._host)
    monkeypatch.setattr(vis, "_host", fake)
    return fake


def test_diff_and_thread_builders_are_valid_view_components():
    diff = vis.ui.diff(
        "change", PATCH, label="Change", old_revision="abc1234", new_revision="def5678"
    )
    thread = vis.ui.thread("general", label="General")
    for component in (diff, vis.ui.diff("bare", PATCH), thread):
        validate("view", "component", component)
    assert (diff["type"], diff["patch"], diff["new_revision"]) == (
        "diff",
        PATCH,
        "def5678",
    )
    assert (thread["type"], thread["id"], thread["messages"]) == (
        "thread",
        "general",
        [],
    )


def test_app_declaration_is_an_attachment_placement():
    thread = vis.ui.thread("general")
    app = vis.ui.app("Review", thread, description="Check it", key="review")
    assert app == {
        "placement": "attachment",
        "title": "Review",
        "nodes": [thread],
        "description": "Check it",
        "key": "review",
    }


def test_attach_opens_one_app_and_answers_its_handle(host):
    app = vis.attach(
        vis.ui.app("Review", vis.ui.diff("change", PATCH), vis.ui.thread("general"))
    )
    (opened,) = host.ops("open")
    assert opened["view"]["placement"] == "attachment"
    assert [node["type"] for node in opened["view"]["nodes"]] == ["diff", "thread"]
    assert isinstance(app, vis.LiveAttachment)
    assert app.id == app.view_id == "app-1"
    assert repr(app) == "<vis app 'app-1' open>"


def test_reply_sends_only_the_given_keys(host):
    app = vis.attach(vis.ui.app("Review", vis.ui.diff("change", PATCH)))
    answer = app.reply(
        "change", "Line 2 follows the spec.", anchor=LINE, submission_id="sub-1"
    )
    assert host.ops("reply") == [
        {
            "op": "reply",
            "view_id": "app-1",
            "node_id": "change",
            "body": "Line 2 follows the spec.",
            "anchor": LINE,
            "submission_id": "sub-1",
        }
    ]
    assert answer["message_id"] and answer["seq"] == 1


def test_reply_refuses_a_blank_or_non_text_body_before_the_host(host):
    app = vis.attach(vis.ui.app("Review", vis.ui.thread("general")))
    sent = len(host.sent)
    with pytest.raises(ValueError, match="blank"):
        app.reply("general", "  \n")
    with pytest.raises(TypeError, match="string"):
        app.reply("general", None)
    assert len(host.sent) == sent


def test_set_status_moves_a_receipt_and_answers_it(host):
    app = vis.attach(vis.ui.app("Review", vis.ui.thread("general")))
    receipt = app.set_status("sub-1", "processing", note="Reading the comments")
    assert host.ops("submission") == [
        {
            "op": "submission",
            "view_id": "app-1",
            "submission_id": "sub-1",
            "status": "processing",
            "note": "Reading the comments",
        }
    ]
    assert receipt["status"] == "processing"
    with pytest.raises(ValueError, match="status must be one of"):
        app.set_status("sub-1", "sent")
    assert len(host.ops("submission")) == 1


def test_submissions_read_the_receipts_from_the_app_state(host):
    app = vis.attach(vis.ui.app("Review", vis.ui.thread("general")))
    host.view["submissions"].append(
        {
            "id": "sub-2",
            "idempotency_key": "send-2",
            "status": "answered",
            "created_at": 2,
            "updated_at": 3,
        }
    )
    assert [receipt["id"] for receipt in app.submissions()] == ["sub-1", "sub-2"]
    assert [receipt["id"] for receipt in app.submissions(status="answered")] == [
        "sub-2"
    ]
    assert app.submission("sub-1")["idempotency_key"] == "send-1"
    with pytest.raises(KeyError):
        app.submission("sub-9")
    with pytest.raises(ValueError, match="status must be one of"):
        app.submissions(status="sent")


def test_a_send_event_names_its_receipt_and_its_comments(host):
    # #421: the owner answers the Send that the event names.
    app = vis.attach(vis.ui.app("Review", vis.ui.diff("change", PATCH)))
    pushed = []
    app.on_event(pushed.append)
    (event,) = app.events()
    assert (event.type, event.node_id, event.submission_id) == (
        "submit",
        "change",
        "sub-1",
    )
    assert event.comments == SEND["comments"]
    assert pushed == [event]


def test_archive_and_close_end_the_app(host):
    app = vis.attach(vis.ui.app("Review", vis.ui.thread("general")))
    app.archive()
    assert host.ops("archive") == [{"op": "archive", "view_id": "app-1"}]
    assert repr(app) == "<vis app 'app-1' closed>"
    other = vis.attach(vis.ui.app("Notes", vis.ui.thread("general")))
    other.close()
    assert host.ops("close")[-1]["view_id"] == "app-1"


def test_attach_of_an_app_row_answers_the_same_app_again(host):
    first = vis.attach(vis.ui.app("Review", vis.ui.thread("general")))
    again = vis.attach(dict(APP_ROW, view_id=first.id))
    assert again.id == first.id
    assert len(host.ops("open")) == 1
    assert host.ops("state")[-1] == {"op": "state", "view_id": "app-1"}
    with pytest.raises(ValueError, match="view_id"):
        vis.attach(dict(APP_ROW, view_id=" "))


def test_an_app_takes_no_file_arguments(host):
    with pytest.raises(TypeError, match="an app takes no"):
        vis.attach(vis.ui.app("Review"), filename="review.json")
    with pytest.raises(TypeError, match="an app takes no"):
        vis.attach(vis.ui.app("Review"), audience="user")
    with pytest.raises(
        TypeError, match="source must be a path, bytes, an image or a figure"
    ):
        vis.attach({"title": "Not an app", "nodes": []})
    assert host.sent == []


def test_live_with_attachment_placement_answers_an_app_handle(host):
    app = vis.live("Review", [vis.ui.thread("general")], placement="attachment")
    assert isinstance(app, vis.LiveAttachment)
    assert host.ops("open")[0]["view"]["placement"] == "attachment"


def test_diff_and_thread_node_handles_drive_their_app(host):
    app = vis.attach(
        vis.ui.app(
            "Review",
            vis.ui.diff("change", PATCH),
            vis.ui.thread("general"),
            flush_ms=0,
        )
    )
    app["change"].set(PATCH, new_revision="def5678")
    app.flush()
    (patch,) = host.ops("patch")
    assert '"new_revision": "def5678"' in json.dumps(patch)
    assert app["change"].threads == []
    app["general"].reply("Thanks, it is fixed.")
    assert [message["body"] for message in app["general"].messages] == [
        "Thanks, it is fixed."
    ]
    assert app.thread("general")["messages"][0]["body"] == "Thanks, it is fixed."


def test_a_sandbox_door_carries_every_envelope_of_its_app(monkeypatch):
    door = AppHost(vis._host)

    def refuse(*_):
        raise AssertionError("the extension host must not carry a sandbox app")

    monkeypatch.setattr(vis._host, "live", refuse)
    declaration = vis.ui.app("Review", vis.ui.thread("general"))
    app = vis.LiveAttachment._from(declaration, door=door.live)
    app.reply("general", "Done.")
    assert [envelope["op"] for envelope in door.sent] == ["open", "reply"]
