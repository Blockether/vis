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
                "id": 142,
                "thread_id": 142,
                "group_id": "project-1",
                "content": request["content"],
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
        root = council.publish("API change", title="API\n")
        assert root.id == root.thread_id == 142
        assert council.threads().entries[0].thread_id == root.thread_id
        assert council.read(thread_id=root.thread_id).entries == ()
        council.publish(
            "Works", thread_id=root.thread_id, ping=[], idempotency_key="retry"
        )
        common = {"group_id": "project-1", "activation_id": "active-1"}
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
            council.publish("Wrong", parent_id=142)
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
            {"id": 1, "thread_id": 1, "group_id": "G", "content": "x"}
        )
