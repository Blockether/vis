"""Serve a provider that refuses every request with 429, to test subagent rerouting."""

from __future__ import annotations

import json
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import blockether.vis.extension as vis

JOURNAL = Path(__file__).resolve().parents[2] / "busy-calls.jsonl"
MODEL = "busy-1"
REFUSAL = json.dumps(
    {
        "error": {
            "message": f"Rate limit reached for {MODEL}.",
            "type": "rate_limit_error",
            "code": "rate_limit_exceeded",
        }
    }
).encode()


class BusyHandler(BaseHTTPRequestHandler):
    def _send(self, status, body, headers=()):
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        for name, value in headers:
            self.send_header(name, value)
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        self._send(200, json.dumps({"data": [{"id": MODEL}]}).encode())

    def do_POST(self):
        raw = self.rfile.read(int(self.headers.get("Content-Length") or 0))
        try:
            model = json.loads(raw or b"{}").get("model")
        except ValueError:
            model = None
        with JOURNAL.open("a") as stream:
            stream.write(json.dumps({"model": model, "status": 429}) + "\n")
        self._send(429, REFUSAL, [("Retry-After", "1")])

    def log_message(self, *_):
        pass


SERVER = ThreadingHTTPServer(("127.0.0.1", 0), BusyHandler)
threading.Thread(target=SERVER.serve_forever, name="busy-provider", daemon=True).start()
URL = f"http://127.0.0.1:{SERVER.server_address[1]}/v1"


def credential():
    return vis.ProviderCredential("busy-local", api_url=URL, api_style="openai")


def models(_provider, _router_opts):
    # A large window lets the first request reach the server and get its 429.
    return [vis.ProviderModel(MODEL, context=200000, is_tool_calls_supported=True)]


vis.register_extension(
    vis.Extension(
        name="busy-provider",
        description="A local provider that answers every model request with 429.",
        providers=[
            vis.Provider(
                id="busy",
                label="Busy",
                is_managed=True,
                preset=vis.ProviderPreset(
                    base_url=URL, api_style="openai", default_models=[MODEL]
                ),
                get_token_fn=credential,
                enrich_models_fn=models,
            )
        ],
    )
)
