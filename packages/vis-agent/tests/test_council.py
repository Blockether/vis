"""Council's session-bound SDK, using the existing HTTP fixture."""

import json

import pytest
from blockether.vis._contracts import validate
from blockether.vis.engine import GatewayClient
from test_client import compatible, endpoint


def test_council_session_handle_and_thread_workflow():
    publications = []

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path == "/v1/sessions/A/council":
            return 200, {"activation_id": "active-1", "default_group_id": "project-1"}
        if method == "POST":
            request = json.loads(body)
            publications.append(request)
            validate("council", "publish", request)
            assert "?" not in path
            assert request["activation_id"] == "active-1"
            assert request["idempotency_key"]
            assert "author_session_id" not in request
            return 200, {
                "entry_id": 142,
                "thread_id": 142,
                "group_id": "project-1",
                "content": request["content"],
                "kind": request["kind"],
                "author_session_id": "A",
                "created_at": 1,
                "source": "sdk",
                "ping": [],
            }
        if "/threads" in path:
            return 200, {
                "entries": [
                    {
                        "thread_id": 142,
                        "title": "API",
                        "kind": "coordination",
                        "author_session_id": "A",
                        "created_at": 1,
                    }
                ],
                "after": 142,
                "has_more": False,
            }
        return 200, {"entries": [], "after": 0, "has_more": False}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        assert hasattr(client.session("A"), "council"), "Session.council is missing"
        council = client.session("A").council()
        root = council.publish("API change", kind="coordination", title="API\n")
        assert root.entry_id == root.thread_id == 142
        assert council.threads().entries[0].thread_id == root.thread_id
        assert council.read(thread_id=root.thread_id).entries == ()
        council.publish(
            "Works",
            kind="coordination",
            thread_id=root.thread_id,
            ping=[],
            idempotency_key="retry",
        )
        common = {
            "kind": "coordination",
            "group_id": "project-1",
            "activation_id": "active-1",
        }
        assert publications == [
            {
                **common,
                "content": "API change",
                "title": "API\n",
                "idempotency_key": publications[0]["idempotency_key"],
            },
            {
                **common,
                "content": "Works",
                "thread_id": root.thread_id,
                "ping": [],
                "idempotency_key": "retry",
            },
        ]
        with pytest.raises(TypeError):
            council.publish("Wrong", kind="coordination", parent_id=142)
        assert sum(path == "/v1/sessions/A/council" for _, path, _, _ in calls) == 1
        assert not any("/turns" in path for _, path, _, _ in calls)


def test_council_explicit_group_and_read_only_handle():
    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path.startswith("/v1/sessions/A/council?"):
            return 200, {"activation_id": None, "default_group_id": "G"}
        return 200, {"entries": [], "after": 0, "has_more": False}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        assert hasattr(client.session("A"), "council"), "Session.council is missing"
        council = client.session("A").council(group_id="G")
        assert not council.threads().has_more
        assert not council.read(after=4, limit=2).has_more
        assert all(
            "group_id=G" in path for _, path, _, _ in calls if "/council" in path
        )


def test_bound_default_group_is_not_resolved_again():
    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path == "/v1/sessions/A/council":
            return 200, {"activation_id": None, "default_group_id": "original"}
        return 200, {"entries": [], "after": 0, "has_more": False}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        council = client.session("A").council()
        council.read()
        council.threads()
        assert all(
            "group_id=original" in path
            for _, path, _, _ in calls
            if "/council/" in path
        )


def test_entry_decoder_does_not_invent_missing_authorship():
    from blockether.vis.engine._council import CouncilEntry

    with pytest.raises(ValueError):
        CouncilEntry.from_wire(
            {
                "kind": "coordination",
                "entry_id": 1,
                "thread_id": 1,
                "group_id": "G",
                "content": "x",
            }
        )


def test_required_reply_and_automatic_return_notification():
    publications = []

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path == "/v1/sessions/A/council":
            return 200, {"activation_id": "active", "default_group_id": "G"}
        request = json.loads(body)
        publications.append(request)
        validate("council", "publish", request)
        entry = {
            "entry_id": len(publications),
            "thread_id": 1,
            "group_id": "G",
            "content": request["content"],
            "kind": request["kind"],
            "author_session_id": "A",
            "created_at": 1,
            "source": "sdk",
            "ping": ["B"],
        }
        if request.get("reply_required"):
            entry.update(
                reply_required=True, replies=[{"session_id": "B", "state": "pending"}]
            )
        else:
            entry["reply_to"] = request["reply_to"]
        return 200, entry

    with endpoint(respond) as (url, _), GatewayClient(url) as client:
        council = client.session("A").council()
        request = council.publish(
            "Evidence?", kind="coordination", ping=["B"], reply_required=True
        )
        assert request.reply_required
        assert request.replies[0].state == "pending"
        reply = council.publish(
            "Unknown", kind="informational", reply_to=request.entry_id
        )
        assert reply.kind == "informational"
        assert request.kind == "coordination"
        assert reply.reply_to == request.entry_id
        assert "ping" not in publications[1]
        assert "thread_id" not in publications[1]
        with pytest.raises(ValueError):
            council.publish("Wrong", kind="coordination", reply_required="true")


@pytest.mark.parametrize("kind", ["potential_issue", "coordination", "informational"])
def test_explicit_message_kind_and_identifier_domains(kind):
    from typing import get_args

    from blockether.vis.engine import Council, CouncilEntry, CouncilKind

    class Session:
        def _call(self, method, path, **kwargs):
            assert (method, path) == ("POST", "/council/entries")
            request = kwargs["body"]
            assert request["kind"] == kind
            return {
                "entry_id": 4,
                "thread_id": 4,
                "kind": request["kind"],
                "group_id": "workspace:opaque",
                "content": request["content"],
                "author_session_id": "session",
                "created_at": 1,
                "source": "sdk",
                "ping": [],
            }

    council = Council(Session(), "workspace:opaque", "active")
    entry = council.publish("Evidence and uncertainty", kind=kind)
    assert entry.entry_id == entry.thread_id == 4
    assert entry.kind == kind
    assert not hasattr(entry, "id")
    assert set(get_args(CouncilKind)) == {
        "potential_issue",
        "coordination",
        "informational",
    }
    with pytest.raises(TypeError):
        council.publish("Missing kind")
    for invalid in (None, "question", 1):
        with pytest.raises(ValueError):
            council.publish("Invalid kind", kind=invalid)
    for invalid in (None, 0, -1, True, 1.5, "4"):
        with pytest.raises(ValueError):
            council.get(invalid)
    with pytest.raises(ValueError):
        CouncilEntry.from_wire(
            {
                "id": 4,
                "thread_id": 4,
                "kind": kind,
                "group_id": "workspace:opaque",
                "content": "Old name",
                "author_session_id": "session",
                "created_at": 1,
                "source": "sdk",
                "ping": [],
            }
        )
