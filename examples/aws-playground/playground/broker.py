"""The broker: a loopback HTTP server in the worker.

The gateway and the tools of a session have no AWS credentials and no route to the
internet. The broker is their only way out. `/v1/` is the model provider of the
gateway: the broker sends each call to the llm function. `/play/api/` is the
playground API, for the workflow runs and the Vis extension.
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

from playground import aws, wire
from playground.api import Api
from playground.env import LLM_FUNCTION, log


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
    protocol_version = "HTTP/1.1"
    api: Api

    def log_message(self, *args: Any) -> None:  # the worker log has one line for each failure only
        pass

    def serve(self) -> None:
        body = self.rfile.read(int(self.headers.get("content-length") or 0))
        try:
            if self.path.startswith("/v1/"):
                self.model(body)
            elif self.path.startswith("/play/api/"):
                self.complete(*self.api.handle(self.command, self.path, body))
            else:
                self.complete(404, [["content-type", "application/json"]], b'{"error":"no such path"}')
        except (BrokenPipeError, ConnectionResetError):
            self.close_connection = True
        except Exception as failure:  # the caller gets a plain answer, the log gets the cause
            log("broker failed", self.command, self.path, repr(failure))
            self.close_connection = True
            if not getattr(self, "started", False):
                self.complete(502, [["content-type", "application/json"]], b'{"error":{"message":"the broker failed"}}')

    do_GET = do_POST = do_PUT = do_DELETE = serve

    def complete(self, status: int, headers: list[list[str]], body: bytes) -> None:
        self.send_response(status)
        for name, value in headers:
            self.send_header(name, value)
        self.send_header("content-length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def model(self, body: bytes) -> None:
        sent = {name: self.headers[name] for name in ("content-type", "accept") if self.headers.get(name)}
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
    def __init__(self, api: Api) -> None:
        handler = type("BrokerHandler", (Handler,), {"api": api})
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), handler)
        self.server.daemon_threads = True
        self.url = f"http://127.0.0.1:{self.server.server_port}"

    def start(self) -> None:
        threading.Thread(target=self.server.serve_forever, daemon=True).start()

    def stop(self) -> None:
        self.server.shutdown()
