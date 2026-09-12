"""Public remote SDK, exercised through real loopback HTTP (never a user gateway)."""

import json
import threading
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit

import pytest
from blockether.vis._contracts import GATEWAY
from blockether.vis.engine import (
    GatewayClient,
    GatewayError,
    ProtocolError,
    TransportError,
    VisTimeout,
)


@contextmanager
def endpoint(respond):
    calls = []

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *args):
            pass

        def handle_request(self):
            body = self.rfile.read(int(self.headers.get("Content-Length", "0")))
            calls.append((self.command, self.path, dict(self.headers), body))
            result = respond(self.command, self.path, body)
            status, value = result[:2]
            kind = result[2] if len(result) > 2 else "application/json"
            data = value if isinstance(value, bytes) else json.dumps(value).encode()
            self.send_response(status)
            self.send_header("Content-Type", kind)
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            try:
                self.wfile.write(data)
            except (BrokenPipeError, ConnectionResetError):
                pass

        do_GET = do_POST = do_PATCH = do_PUT = do_DELETE = handle_request

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}", calls
    finally:
        server.shutdown()
        server.server_close()
        thread.join(2)


def compatible(method, path, body):
    if path == "/v1/capabilities":
        return 200, {
            "protocol": {
                "protocol": GATEWAY["protocol"]["version"],
                "min_client": GATEWAY["protocol"]["minimum_client"],
            }
        }
    if path == "/v1/clients":
        return 201, {"client_id": "sdk-lease"}
    if path == "/v1/clients/sdk-lease":
        return 200, {"is_released": True}
    return None


def test_session_lifecycle_and_headers():
    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path == "/v1/sessions" and method == "POST":
            assert json.loads(body) == {"title": "Example", "root": "/work"}
            return 201, {"id": "session-one"}
        if path == "/v1/sessions/session-one/seq":
            return 200, {"seq": 3}
        if path == "/v1/sessions/session-one/turns" and method == "POST":
            assert json.loads(body)["request"] == "Hello"
            assert json.loads(body)["idempotency_key"]
            return 202, {"turn_id": "turn-one", "status": "queued"}
        if path == "/v1/sessions/session-one/turns/turn-one":
            return 200, {"turn_id": "turn-one", "status": "completed", "content": []}
        if path.endswith("/cancel"):
            return 200, {"status": "cancelling"}
        if method == "DELETE":
            return 204, b""
        return 200, {"id": "session-one", "title": "Example"}

    with endpoint(respond) as (url, calls):
        with GatewayClient(url, token="test-credential") as client:
            session = client.create_session(title="Example", root="/work")
            assert session.id == "session-one"
            assert session.read()["title"] == "Example"
            turn = session.send("Hello")
            assert turn.id == "turn-one"
            assert turn.wait(timeout=1)["status"] == "completed"
            assert turn.cancel()["status"] == "cancelling"
            session.delete()
        headers = {k.lower(): v for k, v in calls[2][2].items()}
        assert headers["authorization"] == "Bearer test-credential"
        assert headers[GATEWAY["headers"]["protocol"]] == str(
            GATEWAY["protocol"]["version"]
        )
        assert headers[GATEWAY["headers"]["client_id"]] == "sdk-lease"
        assert calls[-1][0:2] == ("DELETE", "/v1/clients/sdk-lease")
        with pytest.raises(TransportError, match="closed"):
            session.read()


@pytest.mark.parametrize("protocol,minimum", [(1, 1), (99, 99), (None, 12)])
def test_incompatible_handshake_never_registers_a_client(protocol, minimum):
    with endpoint(
        lambda *_: (200, {"protocol": {"protocol": protocol, "min_client": minimum}})
    ) as (url, calls):
        with pytest.raises(ProtocolError):
            GatewayClient(url).connect()
        assert len(calls) == 1


def test_auth_error_is_typed_and_mutations_are_not_retried():
    with endpoint(
        lambda *_: (401, {"error": {"type": "unauthorized", "message": "sign in"}})
    ) as (url, calls):
        with pytest.raises(GatewayError) as error:
            GatewayClient(url, token="test-credential").connect()
        assert error.value.status == 401
        assert error.value.code == "unauthorized"
        assert "test-credential" not in repr(error.value)
        assert len(calls) == 1


def test_contract_call_encodes_identifiers_and_binary_payloads():
    def respond(method, path, body):
        return compatible(method, path, body) or (200, body, "application/octet-stream")

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        result = client._request(
            "POST",
            "/v1/sessions/:sid/attachments",
            path={"sid": "space /?#"},
            query={"filename": "a b.txt", "media_type": "text/plain"},
            content=b"hello",
        )
        assert result.content == b"hello"
        sent = urlsplit(calls[-1][1])
        assert sent.path == "/v1/sessions/space%20%2F%3F%23/attachments"
        assert parse_qs(sent.query)["filename"] == ["a b.txt"]
        count = len(calls)
        with pytest.raises(ValueError):
            client._request("POST", "/v1/admin/stop")
        with pytest.raises(ValueError):
            client._request("GET", "/v1/sessions/:sid", path={"sid": ".."})
        with pytest.raises(ValueError):
            client._request("GET", "/not-a-contract-route")
        assert len(calls) == count


def test_form_and_live_actions_use_one_route():
    from test_views import fixtures

    samples = fixtures()

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if path.endswith("/views/input"):
            return 200, {"requests": [samples["input"]]}
        if path.endswith("/views/live"):
            return 200, {"views": [samples["live"]]}
        return 200, {"is_accepted": True, **json.loads(body)}

    with endpoint(respond) as (url, _), GatewayClient(url) as client:
        session = client.session("session-one")
        assert session.input_views()[0].id == "input-one"
        assert session.live_views()[0].id == "live-one"
        assert session.answer("input-one", {"name": "Ada"})["values"] == {"name": "Ada"}
        assert session.view_action("live-one", "interrupt", note="stop")["is_accepted"]


def frame(value):
    return ("data: " + json.dumps(value) + "\n\n").encode()


def test_stream_resume_deduplicates_and_honors_a_restarted_generation():
    streams = []
    sid = "session-one"

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        streams.append(parse_qs(urlsplit(path).query)["sids"][0])
        if len(streams) == 1:
            data = [
                {"type": "subscription.ready", "session_id": sid, "cursor": 7},
                {"type": "turn.delta", "session_id": sid, "seq": 8},
                {"type": "turn.delta", "session_id": sid, "seq": 8},
            ]
        else:
            data = [
                {"type": "subscription.ready", "session_id": sid, "cursor": 0},
                {"type": "turn.completed", "session_id": sid, "seq": 1},
            ]
        return 200, b": heartbeat\n\n" + b"".join(map(frame, data)), "text/event-stream"

    with endpoint(respond) as (url, _), GatewayClient(url) as client:
        with client.session(sid).events(
            cursor=7, reconnects=1, retry_delay=0
        ) as events:
            first = next(events)
            assert first.type == "subscription.ready"
            assert next(events).seq == 8
            assert next(events).cursor == 0
            assert next(events).seq == 1
        assert streams == ["session-one:7", "session-one:8"]


def test_stream_budget_and_malformed_frames_fail_visibly():
    def respond(method, path, body):
        return compatible(method, path, body) or (
            200,
            b"data: not-json\n\n",
            "text/event-stream",
        )

    with endpoint(respond) as (url, _), GatewayClient(url) as client:
        with client.session("s").events(reconnects=0) as events:
            with pytest.raises(ProtocolError):
                next(events)


def test_timeout_is_not_a_second_post():
    release = threading.Event()

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        release.wait(1)
        return 200, {}

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        try:
            with pytest.raises(VisTimeout):
                client.create_session(title="slow", timeout=0.02)
            assert (
                sum(row[0] == "POST" and row[1] == "/v1/sessions" for row in calls) == 1
            )
        finally:
            release.set()


@pytest.mark.parametrize(
    "url",
    ["file:///tmp/vis", "https://name:password@example.com", "http://127.0.0.1?q=1"],
)
def test_endpoint_validation_happens_without_network(url):
    with pytest.raises(ValueError):
        GatewayClient(url)


def test_reconnect_exhaustion_and_client_close_release_streams():
    def respond(method, path, body):
        return compatible(method, path, body) or (200, b"", "text/event-stream")

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        stream = client.session("s").events(reconnects=1, retry_delay=0)
        with pytest.raises(TransportError, match="budget"):
            next(stream)
        assert sum("/v1/events?" in row[1] for row in calls) == 2
        stream.close()
        other = client.session("s").events()
        client.close()
        with pytest.raises(StopIteration):
            next(other)


def test_oversized_frame_is_refused():
    def respond(method, path, body):
        return compatible(method, path, body) or (
            200,
            b"data: " + b"x" * 1048576,
            "text/event-stream",
        )

    with endpoint(respond) as (url, _), GatewayClient(url) as client:
        with client.session("s").events() as stream:
            with pytest.raises(ProtocolError, match="1 MiB"):
                next(stream)


@pytest.mark.parametrize(
    "method,route,kwargs",
    [
        ("GET", "/v1/sessions", {"body": {}}),
        ("POST", "/v1/sessions", {"content": b"x"}),
        ("POST", "/v1/voice", {"body": {}}),
        ("POST", "/v1/voice", {"content": "not bytes"}),
        ("GET", "/v1/events", {}),
    ],
)
def test_route_payload_contract_is_checked_before_io(method, route, kwargs):
    with endpoint(lambda *_: (200, {})) as (url, calls):
        with pytest.raises(ValueError):
            GatewayClient(url)._request(method, route, **kwargs)
        assert calls == []


def test_session_binary_and_history_helpers():
    payload = b"\x00\xffexample"

    def respond(method, path, body):
        if method == "POST":
            assert body == payload
            query = parse_qs(urlsplit(path).query)
            assert query == {
                "filename": ["example.bin"],
                "media_type": ["application/octet-stream"],
            }
            return 201, {"upload_id": "upload-one", "size": len(body)}
        if "/attachments/" in path:
            return 200, payload, "application/octet-stream"
        if path.endswith("transcript.md"):
            return 200, b"# Example", "text/markdown"
        return 200, {"turns": [], "artifacts": []}

    with endpoint(respond) as (url, calls):
        session = GatewayClient(url).session("s")
        assert (
            session.upload(
                payload, filename="example.bin", media_type="application/octet-stream"
            )["upload_id"]
            == "upload-one"
        )
        assert session.download_attachment("iteration-one", 0).content == payload
        assert session.transcript(format="markdown").content == b"# Example"
        assert session.turns() == []
        assert session.artifacts() == {"turns": [], "artifacts": []}
        count = len(calls)
        with pytest.raises(ValueError):
            session.transcript(format="unknown")
        assert len(calls) == count


def test_every_non_streaming_sdk_operation_uses_the_canonical_contract():
    # This proves dispatch coverage, not the behavior of the real gateway handlers.
    with endpoint(lambda *_: (200, b"result", "application/octet-stream")) as (
        url,
        calls,
    ):
        client = GatewayClient(url)
        expected = 0
        for route in GATEWAY["routes"]:
            if route["audience"] != "sdk":
                continue
            path = {
                s[1:]: "example" for s in route["path"].split("/") if s.startswith(":")
            }
            for method, operation in route["operations"].items():
                if operation["response"] == "sse":
                    continue
                kwargs = {}
                if operation["request"] == "json":
                    kwargs["body"] = {"example": True}
                elif operation["request"] == "binary":
                    kwargs["content"] = b"\x00\xff"
                result = client._request(method, route["path"], path=path, **kwargs)
                assert result.content == b"result"
                assert calls[-1][0] == method.upper()
                expected += 1
        assert len(calls) == expected


@pytest.mark.parametrize("payload", [None, [], {}, {"id": ""}, {"id": 4}])
def test_malformed_session_creation_is_a_protocol_error(payload):
    with endpoint(lambda *_: (201, payload)) as (url, calls):
        with pytest.raises(ProtocolError):
            GatewayClient(url).create_session()
        assert len(calls) == 1  # A malformed reply must not repeat the mutation.


@pytest.mark.parametrize("payload", [None, [], {}, {"seq": True}, {"seq": -1}])
def test_malformed_cursor_prevents_turn_submission(payload):
    with endpoint(lambda *_: (200, payload)) as (url, calls):
        with pytest.raises(ProtocolError):
            GatewayClient(url).session("s").send("Hello")
        assert [call[0] for call in calls] == ["GET"]


@pytest.mark.parametrize(
    "options",
    [{"cursor": True}, {"cursor": 1.5}, {"reconnects": 1.5}, {"reconnects": True}],
)
def test_replay_options_require_integer_counts(options):
    client = GatewayClient("http://127.0.0.1:1")
    with pytest.raises(ValueError):
        client.session("s").events(**options)
    assert not client._streams


@pytest.mark.parametrize("payload", [None, [], {}, {"turn_id": ""}, {"turn_id": 4}])
def test_malformed_turn_creation_is_not_retried(payload):
    def respond(method, path, body):
        return (200, {"seq": 0}) if path.endswith("/seq") else (202, payload)

    with endpoint(respond) as (url, calls):
        with pytest.raises(ProtocolError):
            GatewayClient(url).session("s").send("Hello")
        assert [call[0] for call in calls] == ["GET", "POST"]


@pytest.mark.parametrize("operation", ["turns", "input_views", "live_views"])
@pytest.mark.parametrize("payload", [None, [], {}])
def test_malformed_collection_responses_are_protocol_errors(operation, payload):
    with endpoint(lambda *_: (200, payload)) as (url, _):
        session = GatewayClient(url).session("s")
        with pytest.raises(ProtocolError):
            getattr(session, operation)()


def test_malformed_turn_status_does_not_spin_until_timeout():
    from blockether.vis.engine import Turn

    with endpoint(lambda *_: (200, {})) as (url, calls):
        turn = Turn(GatewayClient(url).session("s"), "t")
        with pytest.raises(ProtocolError):
            turn.wait(timeout=1)
        assert len(calls) == 1


def test_dedicated_public_operations_replace_raw_call():
    with endpoint(lambda *_: (200, {"models": []})) as (url, calls):
        client = GatewayClient(url)
        assert not hasattr(client, "call")
        assert client.get_models() == {"models": []}
        assert client.get_provider_status("example") == {"models": []}
        assert calls[-1][1] == "/v1/providers/example/status"


def test_machine_order_preserves_request_and_response():
    body = {"machine_ids": ["new", "primary"]}
    reply = {"machine_ids": ["primary", "new"]}
    with endpoint(lambda *_: (200, reply)) as (url, calls):
        assert GatewayClient(url).post_machines_order(body=body) == reply
        assert calls[-1][:2] == ("POST", "/v1/machines/order")
        assert json.loads(calls[-1][3]) == body


def test_dedicated_methods_cover_every_public_nonstreaming_operation():
    import inspect

    expected = {
        (method.upper(), route["path"])
        for route in GATEWAY["routes"]
        if route["audience"] == "sdk" and not route["path"].startswith("/v1/clients")
        for method, op in route["operations"].items()
        if op["response"] != "sse"
    }
    seen = set()
    with endpoint(lambda *_: (200, {"ok": True})) as (url, calls):
        client = GatewayClient(url)
        for _, method in inspect.getmembers(client, inspect.ismethod):
            words = (method.__doc__ or "").split()
            if len(words) < 2 or (words[0], words[1]) not in expected:
                continue
            signature = inspect.signature(method)
            assert signature.return_annotation is not inspect.Signature.empty
            assert all(
                p.annotation is not inspect.Parameter.empty
                for p in signature.parameters.values()
            )
            positional = [
                "example"
                for p in signature.parameters.values()
                if p.kind == p.POSITIONAL_OR_KEYWORD
            ]
            kwargs = {}
            if "body" in signature.parameters:
                kwargs["body"] = {}
            if "content" in signature.parameters:
                kwargs["content"] = b"example"
            method(*positional, **kwargs)
            assert calls[-1][0] == words[0]
            expected_path = "/".join(
                "example" if p.startswith(":") else p for p in words[1].split("/")
            )
            assert calls[-1][1] == expected_path
            seen.add((words[0], words[1]))
    # Council intentionally lives on a session-bound handle rather than raw client mutations.
    entry = {
        "entry_id": 1,
        "kind": "informational",
        "thread_id": 1,
        "group_id": "G",
        "content": "Entry",
        "author_session_id": "example",
        "created_at": 1,
        "source": "sdk",
        "ping": [],
    }

    def council_response(method, path, _body):
        route = path.split("?")[0]
        if route.endswith("/council"):
            return 200, {"default_group_id": "G", "activation_id": "active"}
        if route.endswith("/members"):
            return 200, []
        if route.endswith("/1") or method == "POST":
            return 200, entry
        return 200, {"entries": [], "after": 0, "has_more": False}

    with endpoint(council_response) as (url, calls):
        council = GatewayClient(url).session("example").council()
        council.members()
        council.threads()
        council.read()
        council.get(1)
        council.publish("Entry", kind="informational")
        council.wake("Finished", kind="informational")
        seen.update(
            (
                method,
                path.split("?")[0]
                .replace("/example/", "/:sid/")
                .replace("/entries/1", "/entries/:entry-id"),
            )
            for method, path, _, _ in calls
            if "/council" in path
        )
    assert seen == expected


def test_lease_is_renewed_while_idle_and_thread_stops(monkeypatch):
    monkeypatch.setitem(GATEWAY["client_lease"], "keepalive_ms", 20)
    renewed = threading.Event()
    caps = 0

    def respond(method, path, body):
        nonlocal caps
        if path == "/v1/capabilities":
            caps += 1
            if caps > 1:
                renewed.set()
        return compatible(method, path, body)

    with endpoint(respond) as (url, calls):
        client = GatewayClient(url).connect()
        try:
            assert renewed.wait(2), "an idle attached client must retain its lease"
            heartbeat = client._heartbeat
        finally:
            client.close()
        assert not heartbeat.is_alive()
        assert calls[-1][:2] == ("DELETE", "/v1/clients/sdk-lease")
        assert len([c for c in calls if c[:2] == ("POST", "/v1/clients")]) == 1


def test_failed_keepalive_is_surfaced_without_retrying_a_mutation(monkeypatch):
    monkeypatch.setitem(GATEWAY["client_lease"], "keepalive_ms", 20)
    caps = 0

    def respond(method, path, body):
        nonlocal caps
        if path == "/v1/capabilities":
            caps += 1
            if caps > 1:
                return 503, {"error": {"type": "unavailable"}}
        return compatible(method, path, body) or (201, {"id": "unexpected"})

    with endpoint(respond) as (url, calls):
        with GatewayClient(url) as client:
            client._heartbeat.join(2)
            with pytest.raises(TransportError, match="lease"):
                client.create_session(title="must not be sent")
            with pytest.raises(TransportError, match="lease"):
                client.connect()
        assert not any(c[1] == "/v1/sessions" for c in calls)


@pytest.mark.parametrize(
    "method, args, route, event_name",
    [
        ("speech_events", ("job-one",), "/v1/speech/jobs/job-one/events", "speech.job"),
        ("voice_events", ("job-one",), "/v1/voice/jobs/job-one/events", "voice.job"),
        (
            "session_speech_events",
            ("session-one", "job-one"),
            "/v1/sessions/session-one/speech/jobs/job-one/events",
            "speech.job",
        ),
        (
            "session_voice_events",
            ("session-one", "job-one"),
            "/v1/sessions/session-one/voice/jobs/job-one/events",
            "voice.job",
        ),
    ],
)
def test_all_job_streams_preserve_event_name_and_stop_on_terminal(
    method, args, route, event_name
):
    def respond(verb, path, body):
        if result := compatible(verb, path, body):
            return result
        assert path == route
        jobs = [
            {"id": "job-one", "phase": "running", "is_done": False},
            {"id": "job-one", "phase": "done", "is_done": True},
        ]
        return (
            200,
            b"".join(
                (f"event: {event_name}\ndata: " + json.dumps(job) + "\n\n").encode()
                for job in jobs
            ),
            "text/event-stream",
        )

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        with getattr(client, method)(*args, reconnects=0) as events:
            values = list(events)
        assert [v.type for v in values] == [event_name, event_name]
        assert values[-1].id == "job-one"
        assert values[-1].is_done
        assert len([c for c in calls if c[1] == route]) == 1


@pytest.mark.parametrize("incomplete", [False, True])
def test_activity_export_rejects_marked_partial_response(incomplete):
    # Regression #212: a transport can finish HTTP 200 after the stream was marked incomplete.
    text = b"ACTIVITY\n" + b"Complete retained operation details\n" * 3000
    if incomplete:
        text += b"\n\nINCOMPLETE EXPORT: Activity changed. Reload and retry.\n"
    route = "/v1/sessions/session-one/activity/history-one/export"

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        assert method == "GET" and path == route
        return 200, text, "text/plain; charset=utf-8"

    with endpoint(respond) as (url, calls), GatewayClient(url) as client:
        if incomplete:
            with pytest.raises(ProtocolError, match="incomplete"):
                client.get_session_activity_export("session-one", "history-one")
        else:
            response = client.get_session_activity_export("session-one", "history-one")
            assert response.content == text
            assert len(response.content) > 65536
        assert len([c for c in calls if c[1] == route]) == 1
