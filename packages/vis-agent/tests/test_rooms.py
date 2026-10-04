"""Canonical relay requests, retry identity and secret-safe failures."""

import io
import json
from urllib.error import HTTPError, URLError
from uuid import uuid4

import pytest
from blockether.vis._contracts import schema, validate
from blockether.vis.rooms import MachineIdentity, RoomsClient, RoomsError


def identity():
    return MachineIdentity.generate("Test gateway")


def entry(sid, room):
    return {
        "entry_id": 1,
        "thread_id": 1,
        "group_id": room,
        "kind": "informational",
        "title": "Ready",
        "content": "Ready",
        "author_session_id": sid,
        "created_at": 1,
        "source": "sdk",
        "ping": [],
    }


class Responses:
    def __init__(self, *values):
        self.values = iter(values)
        self.requests = []

    def open(self, request, **_):
        self.requests.append(request)
        value = next(self.values)
        if isinstance(value, Exception):
            raise value
        return io.BytesIO(json.dumps(value).encode())


def test_credentials_are_private_and_origins_are_strict():
    machine = identity()
    assert machine.credential not in repr(machine)
    for url in [
        "http://gateway.example.com",
        "https://secret@gateway.example.com",
        "https://gateway.example.com/path",
    ]:
        with pytest.raises(ValueError):
            RoomsClient(url, machine.credential)
    assert (
        RoomsClient("http://127.0.0.1:7777", machine.credential).base_url
        == "http://127.0.0.1:7777"
    )


def test_invite_parse_errors_do_not_echo_fragments():
    machine = identity()
    client = RoomsClient("https://gateway.example.com", machine.credential)
    with pytest.raises(ValueError) as caught:
        client.join("https://gateway.example.com/rooms/join#private-fragment", machine)
    assert "private-fragment" not in str(caught.value)


def test_publication_retry_reuses_its_identity_and_returns_council_types():
    machine = identity()
    room = str(uuid4())
    sid = str(uuid4())
    client = RoomsClient("https://gateway.example.com", machine.credential)
    replies = Responses(URLError("connection closed"), entry(sid, room))
    client._opener = replies
    result = client.publish(room, sid, "Ready", kind="informational")
    assert result.entry_id == 1
    assert replies.requests[0].data == replies.requests[1].data
    body = json.loads(replies.requests[0].data)
    validate("rooms", "publication", body)
    assert body["publication"]["idempotency_key"]
    assert "credential" not in body


def test_inbox_and_wake_use_the_canonical_protocol():
    machine = identity()
    room, sid = str(uuid4()), str(uuid4())
    client = RoomsClient("https://gateway.example.com", machine.credential)
    client._opener = Responses(
        {"entries": [entry(sid, room)], "after": 1, "has_more": False}, entry(sid, room)
    )
    assert client.inbox(room, sid).entries[0].entry_id == 1
    assert client.wake(room, sid, "Ready", kind="informational").entry_id == 1
    validate("rooms", "wake", json.loads(client._opener.requests[1].data))
    with pytest.raises(ValueError):
        client.inbox(room, sid, after=-1)


def test_machine_deletion_uses_the_canonical_protocol():
    machine = identity()
    client = RoomsClient("https://gateway.example.com", machine.credential)
    client._opener = Responses(
        {"machine_id": machine.machine_id, "deleted_rooms": 2, "retained_history": True}
    )
    result = client.delete_machine(machine.machine_id)
    assert (result.deleted_rooms, result.retained_history) == (2, True)
    request = client._opener.requests[0]
    assert (request.get_method(), request.full_url, request.data) == (
        "DELETE",
        f"https://gateway.example.com/v1/rooms/machines/{machine.machine_id}",
        None,
    )
    with pytest.raises(ValueError):
        client.delete_machine("not-a-machine")


def test_machine_rename_keeps_the_identity():
    machine = identity()
    client = RoomsClient("https://gateway.example.com", machine.credential)
    client._opener = Responses(
        {
            "machine_id": machine.machine_id,
            "name": "Desk",
            "can_create_rooms": False,
            "created_at": 1,
        }
    )
    assert client.rename("Desk").machine_id == machine.machine_id
    request = client._opener.requests[0]
    assert (request.get_method(), request.full_url, json.loads(request.data)) == (
        "PATCH",
        "https://gateway.example.com/v1/rooms/machine",
        {"name": "Desk"},
    )
    with pytest.raises(ValueError):
        client.rename("Bad\nname")


def test_unknown_fields_invalid_responses_and_redirects_fail_closed():
    machine = identity()
    room, sid = str(uuid4()), str(uuid4())
    client = RoomsClient("https://gateway.example.com", machine.credential)
    with pytest.raises(ValueError):
        client.publish(room, sid, "Ready", kind="informational", access_token="private")
    client._opener = Responses({"credential": "private"})
    with pytest.raises(ValueError):
        client.machine()
    client._opener = Responses(
        HTTPError(
            "https://gateway.example.com", 302, "private", {}, io.BytesIO(b"private")
        )
    )
    with pytest.raises(RoomsError) as caught:
        client.machine()
    assert "private" not in str(caught.value)


def test_response_byte_cap():
    machine = identity()
    client = RoomsClient("https://gateway.example.com", machine.credential)
    client._opener = Responses(
        "x" * (schema("rooms")["x-vis-limits"]["response_bytes"] + 1)
    )
    with pytest.raises(ValueError, match="byte limit"):
        client.machine()


def test_real_http_redirect_never_forwards_credentials():
    from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
    from threading import Thread

    requests = []

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            requests.append(self.path)
            self.send_response(302)
            self.send_header("Location", "/credential-leak")
            self.send_header("Content-Length", "2")
            self.end_headers()
            self.wfile.write(b"{}")

        def log_message(self, *_):
            pass

    with ThreadingHTTPServer(("127.0.0.1", 0), Handler) as server:
        thread = Thread(target=server.serve_forever, daemon=True)
        thread.start()
        try:
            client = RoomsClient(
                f"http://127.0.0.1:{server.server_port}", identity().credential
            )
            with pytest.raises(RoomsError):
                client.machine()
            assert requests == ["/v1/rooms/machine"]
        finally:
            server.shutdown()
            thread.join(timeout=5)
