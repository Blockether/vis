"""Council's session-bound SDK, using the existing HTTP fixture."""

import base64
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
        assert root.entry_id == root.thread_id == 142
        assert council.threads().entries[0].thread_id == root.thread_id
        assert council.read(thread_id=root.thread_id).entries == ()
        council.publish(
            "Works",
            thread_id=root.thread_id,
            ping=[],
            idempotency_key="retry",
        )
        common = {
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
            council.publish("Wrong", parent_id=142)
        assert sum(path == "/v1/sessions/A/council" for _, path, _, _ in calls) == 1
        assert not any("/turns" in path for _, path, _, _ in calls)


@pytest.mark.parametrize("activation", [None, "ended-activation"])
def test_council_wakes_its_bound_session_between_turns(activation):
    # #202: a retained extension/SDK handle needs neither a target nor a fresh activation.
    publications = []

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path == "/v1/sessions/A/council":
            return 200, {"activation_id": activation, "default_group_id": "G"}
        assert method == "POST" and path == "/v1/sessions/A/council/wake"
        request = json.loads(body)
        validate("council", "wake", request)
        publications.append(request)
        return 200, {
            "entry_id": 1,
            "thread_id": 1,
            "group_id": "G",
            "content": request["content"],
            "author_session_id": "A",
            "created_at": 1,
            "source": "sdk",
            "ping": ["A"],
        }

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        council = client.session("A").council()
        entry = council.wake("Build finished", idempotency_key="build-1")
        assert entry.ping == ("A",)
        assert council.wake("Build finished", idempotency_key="build-1") == entry
        assert (
            publications
            == [
                {
                    "content": "Build finished",
                    "group_id": "G",
                    "idempotency_key": "build-1",
                }
            ]
            * 2
        )
        assert sum(path == "/v1/sessions/A/council" for _, path, _, _ in calls) == 1
        with pytest.raises(TypeError):
            council.wake("Wrong target", session_id="B")


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


@pytest.mark.parametrize("activation", [None, "original-run"])
def test_bound_default_group_is_not_resolved_again(activation):
    binding = {"activation_id": activation, "default_group_id": "original"}

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path == "/v1/sessions/A/council":
            return 200, dict(binding)
        return 200, {"entries": [], "after": 0, "has_more": False}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        council = client.session("A").council()
        assert any("/council" in path for _, path, _, _ in calls)
        binding.update(activation_id="later-run", default_group_id="later-group")
        assert council._activation_id == activation
        assert council.group_id == "original"
        council.read()
        council.threads()
        assert sum(path == "/v1/sessions/A/council" for _, path, _, _ in calls) == 1
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
        request = council.publish("Evidence?", ping=["B"], reply_required=True)
        assert request.reply_required
        assert request.replies[0].state == "pending"
        reply = council.publish("Unknown", reply_to=request.entry_id)
        assert reply.reply_to == request.entry_id
        assert "ping" not in publications[1]
        assert "thread_id" not in publications[1]
        with pytest.raises(ValueError):
            council.publish("Wrong", reply_required="true")


def test_message_identifier_domains():
    from blockether.vis.engine import Council, CouncilEntry

    class Session:
        def _call(self, method, path, **kwargs):
            assert (method, path) == ("POST", "/council/entries")
            request = kwargs["body"]
            assert "kind" not in request
            return {
                "entry_id": 4,
                "thread_id": 4,
                "group_id": "workspace:opaque",
                "content": request["content"],
                "author_session_id": "session",
                "created_at": 1,
                "source": "sdk",
                "ping": [],
            }

    council = Council(Session(), "workspace:opaque", "active")
    entry = council.publish("Evidence and uncertainty")
    assert entry.entry_id == entry.thread_id == 4
    assert not hasattr(entry, "id")
    # Council messages have no kind: a caller that still passes one is refused.
    with pytest.raises(TypeError):
        council.publish("Retired", kind="informational")
    for invalid in (None, 0, -1, True, 1.5, "4"):
        with pytest.raises(ValueError):
            council.get(invalid)
    with pytest.raises(ValueError):
        CouncilEntry.from_wire(
            {
                "id": 4,
                "thread_id": 4,
                "group_id": "workspace:opaque",
                "content": "Old name",
                "author_session_id": "session",
                "created_at": 1,
                "source": "sdk",
                "ping": [],
            }
        )


@pytest.mark.parametrize("scope", [None, {"turn": 2, "iter": 3, "next_form": 4}])
def test_source_identity(scope):
    from blockether.vis.engine import CouncilEntry

    source = {
        "session_id": "session-soul",
        "scope": scope,
        "session_state_id": "session-state",
        "session_turn_soul_id": "turn-soul",
        "session_turn_state_id": "turn-state",
        "session_turn_iteration_id": "iteration",
        "tool_call_id": "call",
    }
    entry = CouncilEntry.from_wire(
        {
            "entry_id": 8,
            "thread_id": 8,
            "group_id": None,
            "author_session_id": "session-soul",
            "content": "Python execution failed at t2/i3/f4",
            "created_at": 1,
            "source": "host",
            "source_ref": source,
            "ping": [],
        }
    )
    assert entry.source_ref.session_turn_soul_id == "turn-soul"
    assert entry.source_ref.session_turn_state_id == "turn-state"
    assert entry.source_ref.session_turn_iteration_id == "iteration"
    assert entry.source_ref.tool_call_id == "call"
    assert entry.source_ref.turn == (2 if scope else None)
    assert entry.source_ref.iteration == (3 if scope else None)
    assert entry.source_ref.form == (4 if scope else None)
    assert entry.group_id is None
    with pytest.raises(ValueError):
        validate(
            "council",
            "publish",
            {"content": "Report", "source_ref": source},
        )


def _host_reply(status, value):
    content = base64.b64encode(json.dumps(value).encode()).decode()
    headers = {"content-type": "application/json"}
    return json.dumps({"status": status, "headers": headers, "content": content})


def test_extension_council_wakes_its_bound_session_through_the_host(monkeypatch):
    # #405: an extension uses the engine Council through its host door, without a token.
    from blockether.vis import extension as vis

    frames = []

    def request(frame):
        frame = json.loads(frame)
        frames.append(frame)
        if frame["route"] == "/v1/sessions/A/council":
            return _host_reply(200, {"activation_id": None, "default_group_id": "G"})
        validate("council", "wake", frame["body"])
        return _host_reply(
            200,
            {
                "entry_id": 1,
                "thread_id": 1,
                "group_id": "G",
                "content": frame["body"]["content"],
                "author_session_id": "A",
                "created_at": 1,
                "source": "sdk",
                "ping": ["A"],
            },
        )

    monkeypatch.setattr(vis._host, "request", request)
    monkeypatch.setattr(vis._host, "bound_session", lambda: "A")
    monkeypatch.setattr(vis, "_LAYER", [])
    entry = vis.council.wake("Build finished", idempotency_key="build-1")
    assert entry.ping == ("A",)
    assert [(frame["method"], frame["route"]) for frame in frames] == [
        ("GET", "/v1/sessions/A/council"),
        ("POST", "/v1/sessions/A/council/wake"),
    ]
    assert frames[1]["body"]["idempotency_key"] == "build-1"
    assert vis.session.id == "A"


def test_extension_host_refusal_raises_gateway_error(monkeypatch):
    # #405: the host refuses a change to a session that the extension does not own.
    from blockether.vis import extension as vis
    from blockether.vis.engine import GatewayError

    refusal = {"error": {"type": "forbidden-session", "message": "Not yours"}}
    monkeypatch.setattr(vis._host, "request", lambda frame: _host_reply(403, refusal))
    monkeypatch.setattr(vis, "_LAYER", [])
    with pytest.raises(GatewayError) as raised:
        vis.sessions.get("B").send("Hello")
    assert (raised.value.status, raised.value.code) == (403, "forbidden-session")


def test_extension_sessions_create_list_and_run_through_the_host(monkeypatch):
    # #405: vis.sessions and vis.run send each SDK request through the host door.
    from blockether.vis import extension as vis

    frames = []
    session = {"id": "N", "title": None, "status": "idle"}

    def request(frame):
        frame = json.loads(frame)
        frames.append((frame["method"], frame["route"], frame.get("query")))
        if frame["method"] == "POST" and frame["route"] == "/v1/sessions":
            return _host_reply(201, session)
        if frame["route"] == "/v1/sessions":
            return _host_reply(200, {"sessions": [session], "next_cursor": None})
        if frame["route"] == "/v1/sessions/actions/search":
            return _host_reply(200, {"sessions": []})
        if frame["route"] == "/v1/models":
            return _host_reply(200, {"models": []})
        raise AssertionError(frame)

    monkeypatch.setattr(vis._host, "request", request)
    monkeypatch.setattr(vis, "_LAYER", [])
    assert vis.sessions.create().id == "N"
    assert vis.sessions.get("N").id == "N"
    assert [row["id"] for row in vis.sessions.list()] == ["N"]
    assert vis.sessions.list(search="deploy") == []
    assert vis.sessions.models(is_available=True) == ()
    assert frames == [
        ("POST", "/v1/sessions", None),
        ("GET", "/v1/sessions", None),
        ("GET", "/v1/sessions/actions/search", "q=deploy"),
        ("GET", "/v1/models", None),
    ]


def test_extension_run_creates_a_session_and_runs_one_turn(monkeypatch):
    # #405: vis.run passes the model pair to create and the other options to Session.run.
    from blockether.vis import extension as vis

    calls = []

    class Created:
        def run(self, request, **options):
            calls.append(("run", request, options))
            return {"status": "completed"}

    def create(**options):
        calls.append(("create", options))
        return Created()

    monkeypatch.setattr(vis.sessions, "create", create)
    assert vis.run("Do it", provider="p", model="m", timeout=5, title="x") == {
        "status": "completed"
    }
    assert calls == [
        ("create", {"provider": "p", "model": "m"}),
        (
            "run",
            "Do it",
            {
                "timeout": 5,
                "response_model": None,
                "max_corrections": 2,
                "title": "x",
            },
        ),
    ]


def test_extension_council_refuses_without_a_host_session():
    from blockether.vis import extension as vis

    with pytest.raises(RuntimeError, match="bound Vis session"):
        getattr(vis, "council")  # noqa: B009 - the attribute access is the call under test.
