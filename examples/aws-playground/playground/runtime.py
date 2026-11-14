"""The AWS Lambda custom runtime of the playground, in plain Python.

Vis runs this module with its embedded Python (`vis-agent python -m playground`). The
module is a client of the Lambda Runtime API: it gets one invocation, calls the handler
and sends the answer. The image has no AWS base image and no Lambda adapter.

A handler takes `(event, context)`. It returns a value that JSON can hold, or an
`Stream` when the caller reads the answer in parts.
"""

from __future__ import annotations

import base64
import http.client
import json
import os
import sys
import time
import traceback
from collections.abc import Callable, Iterable
from dataclasses import dataclass, field
from typing import Any

BASE = "/2018-06-01/runtime"
HTTP_INTEGRATION = "application/vnd.awslambda.http-integration-response"
PRELUDE_END = b"\x00" * 8


@dataclass(frozen=True)
class Context:
    """The facts of one invocation."""

    request_id: str
    deadline_ms: int
    function_arn: str

    def remaining_s(self) -> float:
        """Return the seconds that remain before Lambda stops this invocation."""
        return self.deadline_ms / 1000 - time.time()


@dataclass
class Stream:
    """An answer that the runtime sends in parts, for `InvokeWithResponseStream`."""

    chunks: Iterable[bytes] = ()
    content_type = "application/octet-stream"

    def parts(self) -> Iterable[bytes]:
        return (chunk for chunk in self.chunks if chunk)


@dataclass
class HttpStream(Stream):
    """An HTTP answer that the runtime sends to a Function URL in parts."""

    status: int = 200
    headers: dict[str, str] = field(default_factory=dict)
    cookies: list[str] = field(default_factory=list)
    content_type = HTTP_INTEGRATION

    def parts(self) -> Iterable[bytes]:
        head = {"statusCode": self.status, "headers": self.headers, "cookies": self.cookies}
        yield json.dumps(head).encode() + PRELUDE_END
        yield from super().parts()


def _api() -> http.client.HTTPConnection:
    return http.client.HTTPConnection(os.environ["AWS_LAMBDA_RUNTIME_API"], timeout=None)


def _post(path: str, body: bytes, headers: dict[str, str] | None = None) -> None:
    conn = _api()
    try:
        conn.request("POST", BASE + path, body=body, headers=headers or {})
        conn.getresponse().read()
    finally:
        conn.close()


def next_invocation() -> tuple[Any, Context]:
    """Wait for the next invocation and return its event and context."""
    conn = _api()
    try:
        conn.request("GET", BASE + "/invocation/next")
        answer = conn.getresponse()
        event = json.loads(answer.read() or b"null")
        trace = answer.getheader("Lambda-Runtime-Trace-Id")
        if trace:
            os.environ["_X_AMZN_TRACE_ID"] = trace
        return event, Context(
            request_id=answer.getheader("Lambda-Runtime-Aws-Request-Id", ""),
            deadline_ms=int(answer.getheader("Lambda-Runtime-Deadline-Ms", "0")),
            function_arn=answer.getheader("Lambda-Runtime-Invoked-Function-Arn", ""),
        )
    finally:
        conn.close()


def _error_body(error: BaseException) -> bytes:
    # The invoker gets the type. The text of an error can hold data of the function, so it stays in the log.
    return json.dumps({"errorType": type(error).__name__, "errorMessage": "the function failed. Its log has the cause."}).encode()


def send_error(path: str, error: BaseException) -> None:
    """Report a failure. The answer has the error type, not the message and not the stack."""
    _post(path, _error_body(error), {"Lambda-Runtime-Function-Error-Type": type(error).__name__})


def send_stream(request_id: str, stream: Stream) -> None:
    """Send an answer in chunks. A failure in the middle goes into the trailers."""
    conn = _api()
    try:
        conn.putrequest("POST", f"{BASE}/invocation/{request_id}/response")
        conn.putheader("Lambda-Runtime-Function-Response-Mode", "streaming")
        conn.putheader("Content-Type", stream.content_type)
        conn.putheader("Transfer-Encoding", "chunked")
        conn.putheader("Trailer", "Lambda-Runtime-Function-Error-Type, Lambda-Runtime-Function-Error-Body")
        conn.endheaders()
        trailers = b""
        try:
            for chunk in stream.parts():
                conn.send(b"%x\r\n%s\r\n" % (len(chunk), chunk))
        except Exception as error:  # the status line is gone: only a trailer can report it
            traceback.print_exc()
            trailers = (
                b"Lambda-Runtime-Function-Error-Type: %s\r\n" % type(error).__name__.encode()
                + b"Lambda-Runtime-Function-Error-Body: %s\r\n" % base64.b64encode(_error_body(error))
            )
        conn.send(b"0\r\n" + trailers + b"\r\n")
        conn.getresponse().read()
    finally:
        conn.close()


def serve(handler: Callable[[Any, Context], Any]) -> None:
    """Run the runtime loop. It ends only when Lambda stops the environment."""
    while True:
        event, context = next_invocation()
        try:
            result = handler(event, context)
            if isinstance(result, Stream):
                send_stream(context.request_id, result)
            else:
                _post(f"/invocation/{context.request_id}/response", json.dumps(result).encode())
        except Exception as error:
            traceback.print_exc()
            sys.stderr.flush()
            send_error(f"/invocation/{context.request_id}/error", error)


def init_error(error: BaseException) -> None:
    """Tell Lambda that the runtime cannot start."""
    traceback.print_exc()
    send_error("/init/error", error)
