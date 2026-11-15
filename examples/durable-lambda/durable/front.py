"""The front function: the only public entry of the stack.

It answers the Function URL. It serves the Vis web app and the Durable Lambda pages,
and relays every API call to the live worker. It has no sign-in: each caller of the
address can use the stack. A call goes to SQS and
its answer comes back as S3 objects (see `wire.py`). It also takes the webhook of
GitHub and the tick of the schedule. The front function has no network path to the
worker or the file system, and no permission to read the provider key.
"""

from __future__ import annotations

import base64
import hmac
import json
import mimetypes
import os
import time
import uuid
from collections.abc import Iterator
from dataclasses import dataclass
from typing import Any

from durable import aws, wire
from durable.env import (
    ANSWER_S,
    DUE_S,
    ENV,
    LIFETIME,
    QUEUE,
    REPO_FUNCTION,
    STREAM_S,
    WORKER_FUNCTION,
    log,
)
from durable.runtime import Context, HttpStream

VIS_WEB, PLAY_WEB, VENDOR = ENV.get("VIS_WEB_DIR", "/opt/vis-web"), "/opt/durable/web", "/opt/durable/vendor"
# The pages of Durable Lambda, by path.
PAGES = {"/play": "sessions.html", "/play/workflows": "workflows.html", "/play/factory": "factory.html", "/play/architecture": "architecture.html"}
# The directories of static files, by path prefix.
PUBLIC = {"/play/assets/": f"{PLAY_WEB}/assets", "/play/vendor/": VENDOR}
SECURITY = {
    "strict-transport-security": "max-age=63072000",
    "x-content-type-options": "nosniff",
    "referrer-policy": "no-referrer",
    "x-frame-options": "DENY",
}
# The Durable Lambda pages load only their own files.
PAGE_POLICY = (
    "default-src 'self'; img-src 'self' data: blob:; style-src 'self' 'unsafe-inline'; "
    "script-src 'self'; connect-src 'self'; font-src 'self'; object-src 'none'; "
    "base-uri 'none'; form-action 'self'; frame-ancestors 'none'"
)
# The Vis web app starts with inline scripts. It shows text that a session wrote, so
# it loads images and data only from this address: a link in a transcript cannot
# send data to another host.
WEB_POLICY = (
    "default-src 'self'; script-src 'self' 'unsafe-inline' 'wasm-unsafe-eval'; style-src 'self' 'unsafe-inline'; "
    "img-src 'self' data: blob:; font-src 'self' data:; media-src 'self' data: blob:; connect-src 'self'; "
    "worker-src 'self' blob:; frame-src 'self' blob: data:; object-src 'none'; base-uri 'self'; "
    "form-action 'self'; frame-ancestors 'none'"
)
# An answer of the worker can hold text from a session. As a document, it gets no
# network, no cookie and no access to the pages of this address.
RELAY_POLICY = "sandbox allow-downloads; default-src 'none'; style-src 'unsafe-inline'; img-src data: blob:; font-src data:; base-uri 'none'; form-action 'none'; frame-ancestors 'none'"
# The headers of a worker answer that the browser gets. The worker runs the code of a
# session, so it cannot set a cookie, a redirect or a header that changes the browser rules.
RELAYED = {"content-type", "content-encoding", "content-disposition", "content-range", "accept-ranges", "cache-control", "etag", "last-modified", "retry-after", "x-vis-client-lease-unknown"}
# The webhook secret that this function read, and the time of the last worker start.
_state: dict[str, Any] = {"woke": 0.0}


@dataclass
class Request:
    """One call of the Function URL."""

    method: str
    path: str
    query: str
    headers: dict[str, str]
    body: bytes

    @classmethod
    def of(cls, event: dict[str, Any]) -> Request:
        body = event.get("body") or ""
        raw = base64.b64decode(body) if event.get("isBase64Encoded") else body.encode()
        return cls(
            method=event["requestContext"]["http"]["method"].upper(),
            path=event.get("rawPath") or "/",
            query=event.get("rawQueryString") or "",
            headers={k.lower(): v for k, v in (event.get("headers") or {}).items()},
            body=raw,
        )

    @property
    def target(self) -> str:
        """The path and the query, as the gateway gets them."""
        return self.path + ("?" + self.query if self.query else "")


def answer(status: int, body: bytes | str, ctype: str = "application/json", **headers: str) -> HttpStream:
    """Build a complete answer with the security headers."""
    data = body.encode() if isinstance(body, str) else body
    return HttpStream(status=status, headers={**SECURITY, "content-type": ctype, **headers}, chunks=[data])


def error(status: int, message: str) -> HttpStream:
    return answer(status, json.dumps({"error": message}), **{"cache-control": "no-store"})


def pushed(request: Request) -> HttpStream:
    """Start a fetch of the repository when GitHub reports a push. GitHub signs the report."""
    name = ENV.get("VIS_WEBHOOK_PARAMETER")
    if not name:
        return error(404, "no such path")
    if "webhook" not in _state:
        _state["webhook"] = aws.parameter(name)
    signature = "sha256=" + hmac.new(_state["webhook"].encode(), request.body, "sha256").hexdigest()
    if not hmac.compare_digest(signature.encode(), request.headers.get("x-hub-signature-256", "").encode()):
        return error(401, "the signature is not valid")
    # The report only starts the fetch. The repo function reads the commit from GitHub itself.
    aws.start(REPO_FUNCTION, action="fetch")
    return answer(202, b'{"fetch":"started"}')


def static(root: str, relative: str, **headers: str) -> HttpStream | None:
    """Return a file below root, or None. A path cannot leave root."""
    file = os.path.realpath(os.path.join(root, relative))
    if not file.startswith(root + "/") or not os.path.isfile(file):
        return None
    with open(file, "rb") as source:
        data = source.read()
    # Hashed bundle files and pinned vendor files never change. The browser loads every other file again.
    fixed = root == VENDOR or (root == VIS_WEB and relative.startswith("assets/"))
    cache = "public, max-age=31536000, immutable" if fixed else "no-cache"
    ctype = mimetypes.guess_type(file)[0] or "application/octet-stream"
    if ctype.startswith("text/") or ctype.endswith(("javascript", "json")):
        ctype += "; charset=utf-8"
    return answer(200, data, ctype, **{"cache-control": cache, **headers})


def route(request: Request, context: Context) -> HttpStream:
    """Answer a call of the Function URL. Only the webhook checks its caller."""
    path = request.path.rstrip("/") or "/"
    if path == "/hooks/github" and request.method == "POST":
        return pushed(request)
    if request.method in ("GET", "HEAD"):
        for prefix, root in PUBLIC.items():
            if path.startswith(prefix):
                return static(root, path[len(prefix) :]) or error(404, "no such file")
        if path in PAGES:
            return static(PLAY_WEB, PAGES[path], **{"content-security-policy": PAGE_POLICY}) or error(404, "no such page")
        if not path.startswith(("/v1/", "/play/")):
            found = static(VIS_WEB, "index.html" if path == "/" else path.lstrip("/"), **{"content-security-policy": WEB_POLICY})
            if found:
                return found
    return relay(request, context)


def wake() -> None:
    """Start a worker when no live worker holds the lease (at most every 5 s)."""
    if time.time() - _state["woke"] < 5:
        return
    _state["woke"] = time.time()
    if not aws.alive(aws.lease()[0]):
        log("waking worker")
        aws.start(WORKER_FUNCTION, kind="wake")


def parts(rid: str, deadline: float) -> Iterator[tuple[bytes, bool]]:
    """Yield each part of an answer and whether it is the last part."""
    index, wait, started = 0, 0.03, time.time()
    while time.time() < deadline:
        chunk, _ = aws.get(f"relay/{rid}/{index}")
        if chunk is None:
            if index == 0:
                if time.time() - started > ANSWER_S:
                    return
                wake()
            time.sleep(wait)
            wait = min(wait * 1.5, 0.25)
            continue
        last = chunk[:1] == wire.LAST
        yield chunk[1:], last
        if last:
            return
        index, wait = index + 1, 0.03


def relay(request: Request, context: Context) -> HttpStream:
    """Send the call to the worker and stream its answer."""
    stream = "text/event-stream" in request.headers.get("accept", "")
    rid = str(uuid.uuid4())
    deadline = time.time() + min(STREAM_S if stream else LIFETIME, context.remaining_s() - 10)
    message: dict[str, Any] = {
        "id": rid,
        "method": request.method,
        "path": request.target,
        "deadline": deadline,
        # After this time no client waits for the answer, so the worker drops the call.
        "answer_by": time.time() + ANSWER_S,
        # The worker adds its own gateway token: a token of the caller never reaches the gateway.
        "headers": {k: v for k, v in request.headers.items() if k not in wire.HOP | {"cookie", "authorization"} and not k.startswith("x-amz")},
    }
    if len(request.body) > 150_000:
        aws.put(f"relay/{rid}/request", request.body)
        message["body_key"] = f"relay/{rid}/request"
    else:
        message["body"] = base64.b64encode(request.body).decode()
    aws.client("sqs").send_message(QueueUrl=QUEUE, MessageBody=json.dumps(message))
    source = parts(rid, deadline)
    first = next(source, None)
    if first is None:
        return error(504, "the Vis worker did not answer")
    head, data = wire.unframe(first[0])
    status = head["status"]
    if type(status) is not int or not 100 <= status <= 599:
        return error(502, "the Vis worker sent a status that is not valid")
    if 300 <= status < 400 and status != 304:
        return error(502, "the Vis worker sent a redirect")
    relayed = {k.lower(): v for k, v in head["headers"] if k.lower() in RELAYED}
    headers = {**relayed, **SECURITY, "content-security-policy": RELAY_POLICY}

    def body() -> Iterator[bytes]:
        yield data
        for chunk, _ in source:
            yield chunk

    return HttpStream(status=status, headers=headers, chunks=body())


def tick() -> dict[str, str]:
    """Start a worker when work waits and no worker lives. A schedule calls this each minute."""
    data, _ = aws.lease()
    due = data.get("next_due")
    if aws.alive(data) or not (data.get("running") or data.get("parked") or data.get("runs") or (due and due - time.time() < DUE_S)):
        return {"tick": "nothing to do"}
    aws.start(WORKER_FUNCTION, kind="tick")
    return {"tick": "worker started"}


def handler(event: dict[str, Any], context: Context) -> Any:
    if "requestContext" not in event:  # the schedule, not the Function URL
        return tick()
    try:
        return route(Request.of(event), context)
    except Exception as failure:  # the browser gets a plain answer, the log gets the cause
        log("front failed", repr(failure))
        return error(500, "the front function failed")
