"""The SDK routes a link through the gateway proxy exactly as the gateway decides it.

Each case of `vis-contract/fixtures/proxy-routes.json` holds the SDK, the gateway
and the Companion app to one answer.
"""

from __future__ import annotations

import base64
import http.server
import json
import threading

import pytest
from blockether.vis import _contracts
from blockether.vis.engine import GatewayProxy
from blockether.vis.engine._proxy import from_answer

_FIXTURE = json.loads(
    (_contracts._DATA / "fixtures" / "proxy-routes.json").read_text(encoding="utf-8")
)
_CASES = [(group, case) for group in _FIXTURE["groups"] for case in group["cases"]]


def _proxy(group, *, port=1, token="tok"):
    return GatewayProxy(
        host="127.0.0.1",
        port=port,
        token=token,
        allowed_domains=tuple(group["allowed_domains"]),
        denied_domains=tuple(group["denied_domains"]),
        ports=tuple(group["ports"]),
    )


@pytest.mark.parametrize(("group", "case"), _CASES, ids=[c["url"] for _, c in _CASES])
def test_routes_each_fixture_case(group, case):
    proxy = _proxy(group)
    assert proxy.covers(case["url"]) is case["is_proxied"]
    assert (proxy.proxy_for(case["url"]) is not None) is case["is_proxied"]


def test_an_answer_of_a_proxy_that_is_off_gives_no_proxy():
    off = {
        "is_enabled": False,
        "port": None,
        "allowed_domains": [],
        "denied_domains": [],
        "ports": [],
    }
    assert from_answer(off, host="h", token="t") is None


def test_the_token_is_the_proxy_user_and_stays_out_of_repr():
    proxy = _proxy(_FIXTURE["groups"][0], port=7891, token="a/b c")
    assert proxy.url == "http://a%2Fb%20c:vis@127.0.0.1:7891"
    assert "a/b" not in repr(proxy) and "a%2F" not in repr(proxy)


class _Recorder(http.server.BaseHTTPRequestHandler):
    seen: list = []

    def do_GET(self):
        type(self).seen.append(
            (self.requestline, self.headers.get("Proxy-Authorization"))
        )
        body = b"ok"
        self.send_response(200)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *args):
        pass


def _serve(handler):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server


def test_the_opener_sends_only_covered_links_through_the_gateway():
    class Gateway(_Recorder):
        seen = []

    class Direct(_Recorder):
        seen = []

    gateway, direct = _serve(Gateway), _serve(Direct)
    try:
        group = {
            "allowed_domains": ["corp.internal"],
            "denied_domains": [],
            "ports": [80],
        }
        opener = _proxy(group, port=gateway.server_address[1]).opener()
        with opener.open("http://wiki.corp.internal/page", timeout=5) as answer:
            assert answer.read() == b"ok"
        with opener.open(
            f"http://127.0.0.1:{direct.server_address[1]}/x", timeout=5
        ) as answer:
            assert answer.read() == b"ok"
        assert Gateway.seen == [
            (
                "GET http://wiki.corp.internal/page HTTP/1.1",
                "Basic " + base64.b64encode(b"tok:vis").decode(),
            )
        ]
        assert Direct.seen == [("GET /x HTTP/1.1", None)]
    finally:
        gateway.shutdown()
        direct.shutdown()
