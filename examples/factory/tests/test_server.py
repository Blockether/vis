"""Check the HTTP boundary with an app and a gateway on loopback."""

from __future__ import annotations

import http.client
import json
import threading
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlencode

import pytest

from factory import server, settings


class FakeApp:
    """Return the call that reached the factory API."""

    def handle(self, method, target, body, headers):
        """Echo the method and target, and check the token of a hook."""
        if "/hooks/" in target and headers.get("x-factory-token") != settings.TOKEN:
            return 401, "application/json", b'{"error":"wrong token"}'
        return 200, "application/json", json.dumps({"method": method, "target": target, "data": body.decode()}).encode()

    def tick(self):
        """Take no action."""


@contextmanager
def listening(handler):
    """Run a local HTTP server for the duration of the caller's check."""
    with ThreadingHTTPServer(("127.0.0.1", 0), handler) as http:
        thread = threading.Thread(target=http.serve_forever, kwargs={"poll_interval": 0.01}, daemon=True)
        thread.start()
        try:
            yield http.server_port
        finally:
            http.shutdown()
            thread.join()


@pytest.fixture
def factory(monkeypatch):
    """Start the HTTP handler with an app that does not need other services."""
    monkeypatch.setattr(settings, "HOST", "127.0.0.1")
    monkeypatch.setattr(settings, "TOKEN", "factory-test-token")
    with listening(server.handler(FakeApp())) as port:
        yield port


def request(port, method, target, body=None, headers=None):
    """Read one complete answer."""
    conn = http.client.HTTPConnection("127.0.0.1", port, timeout=3)
    try:
        conn.request(method, target, body=body, headers=headers or {})
        answer = conn.getresponse()
        return answer.status, {key.lower(): value for key, value in answer.getheaders()}, answer.read()
    finally:
        conn.close()


def bearer():
    """Return the factory token for direct API calls."""
    return {"Authorization": f"Bearer {settings.TOKEN}"}


def login(port, token=None):
    """Submit the sign-in form as a browser behind an HTTPS proxy."""
    return request(
        port, "POST", "/factory/login", urlencode({"token": settings.TOKEN if token is None else token}), {"Content-Type": "application/x-www-form-urlencoded", "X-Forwarded-Proto": "https"}
    )


def test_sign_in(factory):
    """The right token signs in without putting that token in the cookie."""
    status, _, page = request(factory, "GET", "/factory")
    assert status == 200 and b'form class="panel-body" action="/factory/login"' in page
    assert request(factory, "GET", "/factory/api/status")[0] == 401
    assert request(factory, "GET", "/factory/api/status", headers={"Authorization": "Bearer wrong"})[0] == 401
    assert login(factory, "wrong")[0] == 401
    status, head, _ = login(factory)
    assert status == 303 and head["location"] == "/factory"
    cookie = head["set-cookie"]
    assert settings.TOKEN not in cookie
    assert all(flag in cookie for flag in ("HttpOnly", "SameSite=Lax", "Secure"))
    signed = {"Cookie": cookie.split(";", 1)[0]}
    assert request(factory, "GET", "/factory/api/status", headers=signed)[0] == 200
    assert request(factory, "GET", "/factory/api/status", headers={"Cookie": signed["Cookie"] + "0"})[0] == 401
    assert request(factory, "GET", "/factory/api/status", headers=bearer())[0] == 200


def test_only_a_post_hook_reaches_the_app_without_sign_in(factory):
    """The API checks the hook secret even if the caller has no cookie."""
    target = "/factory/api/hooks/build"
    assert request(factory, "GET", target)[0] == 401
    assert request(factory, "POST", target, "{}")[0] == 401
    status, _, body = request(factory, "POST", target, "{}", {"X-Factory-Token": settings.TOKEN})
    assert status == 200 and json.loads(body)["target"] == target


def test_encoded_attachment_path_reaches_the_api(factory):
    """A file name with a space still reaches the API."""
    target = "/factory/api/attached/session/release%20notes.md"
    status, _, body = request(factory, "GET", target, headers=bearer())
    assert status == 200 and json.loads(body)["target"] == target


def test_assets_stay_in_their_directory(factory):
    """A parent path cannot leave the static directory."""
    for path in ("../login.html", "../../factory/settings.py", "%2e%2e/%2e%2e/factory/settings.py"):
        status, _, body = request(factory, "GET", f"/factory/assets/{path}", headers=bearer())
        assert status == 404 and b"FACTORY_TOKEN" not in body


def test_gateway_stream_sends_parts_before_it_ends(factory, monkeypatch, tmp_path):
    """The relay sends gateway events as they arrive and replaces caller credentials."""
    received = {}
    release = threading.Event()
    token = tmp_path / "token"
    token.write_text("gateway-test-token\n")
    monkeypatch.setattr(settings, "TOKEN_FILE", str(token))

    class Gateway(BaseHTTPRequestHandler):
        """Send one event now, and a second event only when the test asks."""

        def do_GET(self):
            """Record the request, then send two event parts."""
            received.update((name.lower(), value) for name, value in self.headers.items())
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Set-Cookie", "gateway-cookie=bad")
            self.send_header("Transfer-Encoding", "chunked")
            self.end_headers()
            self.wfile.write(b"9\r\ndata: 1\n\n\r\n")
            self.wfile.flush()
            release.wait(5)
            self.wfile.write(b"9\r\ndata: 2\n\n\r\n0\r\n\r\n")

    with listening(Gateway) as port:
        monkeypatch.setattr(settings, "GATEWAY_URL", f"http://127.0.0.1:{port}")
        client = http.client.HTTPConnection("127.0.0.1", factory, timeout=3)
        try:
            client.request("GET", "/v1/events", headers={**bearer(), "Cookie": "caller=private", "Accept": "text/event-stream"})
            answer = client.getresponse()
            assert answer.status == 200 and answer.getheader("Transfer-Encoding") == "chunked"
            assert answer.getheader("Set-Cookie") is None
            assert answer.getheader("Content-Security-Policy").startswith("sandbox")
            assert answer.read(9) == b"data: 1\n\n"
            assert not release.is_set()
            release.set()
            assert answer.read() == b"data: 2\n\n"
            assert received["authorization"] == "Bearer gateway-test-token"
            assert "cookie" not in received
        finally:
            release.set()
            client.close()


def test_vis_launch_keeps_the_gateway_token_on_the_server(factory, monkeypatch, tmp_path):
    """A signed-in browser opens the gateway page and redeems a token-free grant."""
    token = tmp_path / "token"
    token.write_text("gateway-test-token")
    monkeypatch.setattr(settings, "TOKEN_FILE", str(token))
    calls = []

    class Gateway(BaseHTTPRequestHandler):
        """Serve the gateway page, launch and redeem routes."""

        def send(self, mime, data):
            """Send the response with its content type and length."""
            calls.append((self.command, self.path, self.headers["Authorization"]))
            self.send_response(200)
            self.send_header("Content-Type", mime)
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_GET(self):
            """Serve the gateway's root page."""
            self.send("text/html", b"<html><head></head><body>Vis</body></html>")

        def do_POST(self):
            """Mint a code or return the grant for that code."""
            if self.path == "/v1/web/launches":
                self.send("application/json", b'{"code":"one-time-code","gateway_id":"gateway-id"}')
            elif self.path == "/v1/web/launches/redeem":
                self.send("application/json", b'{"token":"gateway-test-token","gateway_id":"gateway-id","require_token":true}')
            else:
                self.send_error(404)

    with listening(Gateway) as port:
        monkeypatch.setattr(settings, "GATEWAY_URL", f"http://127.0.0.1:{port}")
        status, headers, page = request(factory, "GET", "/factory/vis", headers=bearer())
        assert status == 200 and b"one-time-code" in page and b"gateway-test-token" not in page
        assert "sandbox" not in headers["content-security-policy"]
        status, _, grant = request(factory, "POST", "/v1/web/launches/redeem", '{"code":"one-time-code"}', bearer())
        assert status == 200 and json.loads(grant) == {"gateway_id": "gateway-id", "token": "", "require_token": False}
        assert calls == [("GET", "/", "Bearer gateway-test-token"), ("POST", "/v1/web/launches", "Bearer gateway-test-token"), ("POST", "/v1/web/launches/redeem", "Bearer gateway-test-token")]


def test_tick_continues_after_a_failure(monkeypatch):
    """A scheduler that fails on one look can check for work again."""
    stop = threading.Event()
    monkeypatch.setattr(settings, "TICK_S", 0)

    class App(FakeApp):
        """Finish on the tick after the first tick fails."""

        calls = 0

        def tick(self):
            """Fail one time, then stop the scheduler."""
            self.calls += 1
            if self.calls == 1:
                raise RuntimeError
            stop.set()

    app = App()
    thread = threading.Thread(target=server.scheduler, args=(app, stop), daemon=True)
    thread.start()
    thread.join(1)
    assert not thread.is_alive() and app.calls == 2
