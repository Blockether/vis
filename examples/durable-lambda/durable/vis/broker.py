"""The broker: a loopback HTTP server in the worker.

The gateway and the tools of a session have no AWS credentials in their environment
and no route to the internet. The broker is their only way out. It has two parts:

    /v1/         the model provider of the gateway. The broker sends each call to the
                 llm function (see `llm.py`), which holds the provider key.
    /play/api/   the Durable Lambda API, for the workflow runs and the Vis extension.

The address of the broker is in the Vis configuration as the one provider, so each
session gets its models from Vis and no code of a session sees the key.
"""

from __future__ import annotations

import base64
import gzip
import itertools
import json
import struct
import threading
from collections.abc import Iterator
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any

from durable import aws, wire
from durable.app import Api
from durable.helpers import log
from durable.settings import LLM_FUNCTION, LOOPBACK, PROVIDER_HEADERS


def payloads(events: Any) -> Iterator[bytes]:
    """Yield the bytes of a streamed Lambda answer."""
    for event in events:
        if "PayloadChunk" in event:
            yield event["PayloadChunk"]["Payload"]
        elif event.get("InvokeComplete", {}).get("ErrorCode"):
            raise RuntimeError(f"the llm function failed: {event['InvokeComplete']['ErrorCode']}")


def ask(method: str, path: str, headers: dict[str, str], body: bytes) -> tuple[dict[str, Any], Iterator[bytes]]:
    """Send one provider call to the llm function. Return the head and the body parts."""
    event = {"method": method, "path": path, "headers": headers}
    if body:
        event["body"] = base64.b64encode(gzip.compress(body, 3)).decode()
    answer = aws.client("lambda").invoke_with_response_stream(FunctionName=LLM_FUNCTION, Payload=json.dumps(event).encode())
    stream, buffer = payloads(answer["EventStream"]), b""
    for chunk in stream:
        buffer += chunk
        if len(buffer) >= 4 and len(buffer) >= 4 + struct.unpack(">I", buffer[:4])[0]:
            head, first = wire.unframe(buffer)
            return head, itertools.chain([first] if first else [], stream)
    raise RuntimeError("the llm function sent no answer")


class Handler(BaseHTTPRequestHandler):
    """One call of the gateway, of a workflow run or of the Vis extension."""

    protocol_version = "HTTP/1.1"
    api: Api

    def log_message(self, *args: Any) -> None:  # the worker log has one line for each failure only
        """Write no line for a call that worked."""
        pass

    def serve(self) -> None:
        """Answer a call. After a failure the connection closes, because the answer can be in the middle."""
        body = self.rfile.read(int(self.headers.get("content-length") or 0))
        try:
            if self.path.startswith("/v1/"):
                self.model(body)
            elif self.path.startswith("/play/api/"):
                self.complete(*self.api.handle(self.command, self.path, body))
            else:
                self.complete(404, wire.JSON, b'{"error":"no such path"}')
        except (BrokenPipeError, ConnectionResetError):
            self.close_connection = True
        except Exception as failure:  # the caller gets a plain answer, the log gets the cause
            log("broker failed", self.command, self.path, repr(failure))
            self.close_connection = True
            if not getattr(self, "started", False):
                self.complete(502, wire.JSON, b'{"error":{"message":"the broker failed"}}')

    do_GET = do_POST = do_PUT = do_DELETE = serve

    def complete(self, status: int, headers: list[list[str]], body: bytes) -> None:
        """Send a complete answer with its length."""
        self.send_response(status)
        for name, value in headers:
            self.send_header(name, value)
        self.send_header("content-length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def model(self, body: bytes) -> None:
        """Send a model call to the llm function and stream its answer in chunks."""
        sent = {name: self.headers[name] for name in PROVIDER_HEADERS if self.headers.get(name)}
        head, chunks = ask(self.command, self.path[len("/v1") :].split("?")[0], sent, body)
        self.send_response(head["status"])
        for name, value in head["headers"]:
            self.send_header(name, value)
        self.send_header("transfer-encoding", "chunked")
        self.end_headers()
        self.started = True
        for chunk in chunks:
            self.wfile.write(b"%x\r\n%s\r\n" % (len(chunk), chunk))
            self.wfile.flush()
        self.wfile.write(b"0\r\n\r\n")


class Broker:
    """The server. It listens on a free port of the loopback interface."""

    def __init__(self, api: Api) -> None:
        """Bind the server and keep its address. The API answers the calls of `/play/api/`."""
        handler = type("BrokerHandler", (Handler,), {"api": api})
        self.server = ThreadingHTTPServer((LOOPBACK, 0), handler)
        self.server.daemon_threads = True
        self.url = f"http://{LOOPBACK}:{self.server.server_port}"

    def start(self) -> None:
        """Serve in a thread until `stop`."""
        threading.Thread(target=self.server.serve_forever, daemon=True).start()

    def stop(self) -> None:
        """Stop the server."""
        self.server.shutdown()
