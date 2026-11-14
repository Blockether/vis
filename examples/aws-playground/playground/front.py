"""The front function: the only public entry of the stack.

It answers the Function URL. It checks the password, serves the Vis web app and the
playground pages, and relays every API call to the live worker. A call goes to SQS and
its answer comes back as S3 objects (see `wire.py`). The front function has no network
path to the worker, the file system or the model provider.
"""

from __future__ import annotations

import base64
import hmac
import http.cookies
import json
import mimetypes
import os
import time
import urllib.parse
import uuid
from collections.abc import Iterator
from dataclasses import dataclass
from typing import Any

from playground import aws, wire
from playground.env import (
    ANSWER_S,
    ENV,
    LIFETIME,
    QUEUE,
    REPO_FUNCTION,
    STREAM_S,
    WORKER_FUNCTION,
    log,
)
from playground.runtime import Context, HttpStream

VIS_WEB, PLAY_WEB, VENDOR = ENV.get("VIS_WEB_DIR", "/opt/vis-web"), "/opt/playground/web", "/opt/playground/vendor"
# The pages of the playground, by path.
PAGES = {"/play": "sessions.html", "/play/workflows": "workflows.html", "/play/architecture": "architecture.html"}
SAFE = {"GET", "HEAD", "OPTIONS"}
SECURITY = {
    "strict-transport-security": "max-age=63072000",
    "x-content-type-options": "nosniff",
    "referrer-policy": "no-referrer",
    "x-frame-options": "DENY",
}
# The playground pages load only their own files.
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
RELAY_POLICY = (
    "sandbox allow-downloads; default-src 'none'; style-src 'unsafe-inline'; img-src data: blob:; "
    "font-src data:; base-uri 'none'; form-action 'none'; frame-ancestors 'none'"
)
# The headers of a worker answer that the browser gets. The worker runs the code of a
# session, so it cannot set a cookie, a redirect or a header that changes the browser rules.
RELAYED = {"content-type", "content-encoding", "content-disposition", "content-range", "accept-ranges", "cache-control", "etag", "last-modified", "retry-after", "x-vis-client-lease-unknown"}
SESSION_S, SECRET_S = 604800, 300
_state: dict[str, Any] = {"woke": 0.0}


def password() -> str:
    """Return the password. Read it again after 5 minutes, so a new password takes effect."""
    if time.time() - _state.get("read", 0.0) > SECRET_S:
        try:
            _state["token"] = aws.parameter(ENV["VIS_TOKEN_PARAMETER"])
        except (aws.ClientError, aws.BotoCoreError):
            if "token" not in _state:
                raise
            log("the password read failed: the earlier password stays in effect")
        _state["read"] = time.time()
    return _state["token"]


def cookie(expires: int) -> str:
    """Return a cookie value: its end time and a signature. The cookie never holds the password."""
    signature = hmac.new(password().encode(), f"vis-web.{expires}".encode(), "sha256").hexdigest()
    return f"{expires}.{signature}"


def signed(value: str) -> bool:
    """True for a cookie of /login that has not ended."""
    expires = value.partition(".")[0]
    return expires.isdigit() and len(expires) <= 12 and int(expires) > time.time() and hmac.compare_digest(value.encode(), cookie(int(expires)).encode())


@dataclass
class Request:
    """One call of the Function URL."""

    method: str
    path: str
    query: str
    headers: dict[str, str]
    cookies: dict[str, str]
    body: bytes

    @classmethod
    def of(cls, event: dict[str, Any]) -> Request:
        body = event.get("body") or ""
        raw = base64.b64decode(body) if event.get("isBase64Encoded") else body.encode()
        jar = http.cookies.SimpleCookie("; ".join(event.get("cookies") or []))
        return cls(
            method=event["requestContext"]["http"]["method"].upper(),
            path=event.get("rawPath") or "/",
            query=event.get("rawQueryString") or "",
            headers={k.lower(): v for k, v in (event.get("headers") or {}).items()},
            cookies={name: morsel.value for name, morsel in jar.items()},
            body=raw,
        )

    @property
    def target(self) -> str:
        return self.path + ("?" + self.query if self.query else "")


def answer(status: int, body: bytes | str, ctype: str = "application/json", **headers: str) -> HttpStream:
    """Build a complete answer with the security headers."""
    data = body.encode() if isinstance(body, str) else body
    return HttpStream(status=status, headers={**SECURITY, "content-type": ctype, **headers}, chunks=[data])


def error(status: int, message: str) -> HttpStream:
    return answer(status, json.dumps({"error": message}), **{"cache-control": "no-store"})


def authed(request: Request) -> bool:
    """True for the bearer token, or for the cookie that /login sets."""
    bearer = request.headers.get("authorization", "")
    return hmac.compare_digest(bearer.encode(), b"Bearer " + password().encode()) or signed(request.cookies.get("vis", ""))


def same_origin(request: Request) -> bool:
    """A browser call that changes state must come from a page of this site."""
    # A page cannot set Sec-Fetch-Site. With `Referrer-Policy: no-referrer`, a form POST has `Origin: null`.
    site = request.headers.get("sec-fetch-site")
    if site is not None:
        return site in ("same-origin", "none")
    origin = request.headers.get("origin")
    return origin is None or urllib.parse.urlsplit(origin).netloc == request.headers.get("host")


def login_page(message: str = "", status: int = 200) -> HttpStream:
    with open(os.path.join(PLAY_WEB, "login.html"), encoding="utf-8") as file:
        page = file.read().replace("<!--error-->", message)
    return answer(status, page, "text/html; charset=utf-8", **{"content-security-policy": PAGE_POLICY, "cache-control": "no-store"})


def login(request: Request) -> HttpStream:
    given = urllib.parse.parse_qs(request.body.decode(errors="replace")).get("password", [""])[0]
    if not same_origin(request) or not hmac.compare_digest(given.encode(), password().encode()):
        return login_page("Wrong password.", 401)
    reply = answer(303, b"", "text/plain", location="/play")
    reply.cookies = [f"vis={cookie(int(time.time()) + SESSION_S)}; Path=/; Max-Age={SESSION_S}; HttpOnly; Secure; SameSite=Strict"]
    return reply


def logout() -> HttpStream:
    reply = answer(303, b"", "text/plain", location="/")
    reply.cookies = ["vis=; Path=/; Max-Age=0; HttpOnly; Secure; SameSite=Strict"]
    return reply


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
    aws.client("lambda").invoke(FunctionName=REPO_FUNCTION, InvocationType="Event", Payload=b'{"action":"fetch"}')
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
    path = request.path.rstrip("/") or "/"
    if path == "/login" and request.method == "POST":
        return login(request)
    if path == "/logout" and request.method == "POST":
        return logout() if same_origin(request) else error(403, "this call must come from the playground")
    if path == "/hooks/github" and request.method == "POST":
        return pushed(request)
    known = authed(request)
    if request.method in ("GET", "HEAD"):
        if path.startswith("/play/assets/"):
            found = static(f"{PLAY_WEB}/assets", path[len("/play/assets/") :])
            return found or error(404, "no such file")
        if path.startswith("/play/vendor/"):
            found = static(VENDOR, path[len("/play/vendor/") :])
            return found or error(404, "no such file")
        if path in PAGES or path == "/":
            if not known:
                return login_page()
            if path in PAGES:
                return static(PLAY_WEB, PAGES[path], **{"content-security-policy": PAGE_POLICY}) or error(404, "no such page")
        # The files of the web app are public release assets. Only the API needs the password.
        if not path.startswith(("/v1/", "/play/")):
            found = static(VIS_WEB, "index.html" if path == "/" else path.lstrip("/"), **{"content-security-policy": WEB_POLICY})
            if found:
                return found
    if not known:
        return error(401, "sign in first")
    if request.method not in SAFE and not same_origin(request):
        return error(403, "this call must come from the playground")
    return relay(request, context)


def wake() -> None:
    """Start a worker when no live worker holds the lease (at most every 5 s)."""
    if time.time() - _state["woke"] < 5:
        return
    _state["woke"] = time.time()
    if not aws.alive(aws.lease()[0]):
        log("waking worker")
        aws.client("lambda").invoke(FunctionName=WORKER_FUNCTION, InvocationType="Event", Payload=b'{"kind":"wake"}')


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
        # The worker adds its own gateway token. The password stays in this function.
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
    if aws.alive(data) or not (data.get("running") or data.get("parked") or data.get("runs") or (due and due - time.time() < 120)):
        return {"tick": "nothing to do"}
    aws.client("lambda").invoke(FunctionName=WORKER_FUNCTION, InvocationType="Event", Payload=b'{"kind":"tick"}')
    return {"tick": "worker started"}


def handler(event: dict[str, Any], context: Context) -> Any:
    if "requestContext" not in event:
        return tick()
    try:
        return route(Request.of(event), context)
    except Exception as failure:  # the browser gets a plain answer, the log gets the cause
        log("front failed", repr(failure))
        return error(500, "the front function failed")
