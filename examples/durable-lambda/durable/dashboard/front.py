"""The front function: the only public entry of the stack.

It answers the Function URL. It serves the Vis web app and the Durable Lambda pages,
and relays every API call to the live worker (see `relay.py`). It has no sign-in: each
caller of the address can use the stack. It also takes the webhook of GitHub and the
tick of the schedule. The front function has no network path to the worker or the
file system, and no permission to read the provider key.
"""

from __future__ import annotations

import hmac
import mimetypes
import os
import time
from typing import Any

from durable import aws
from durable.dashboard.relay import relay
from durable.dashboard.web import Request, answer, error
from durable.helpers import log
from durable.lambda_runtime import Context, HttpStream
from durable.settings import DUE_S, ENV, REPO_FUNCTION, WORKER_FUNCTION

VIS_WEB, PLAY_WEB, VENDOR = ENV.get("VIS_WEB_DIR", "/opt/vis-web"), "/opt/durable/web", "/opt/durable/vendor"
# The pages of Durable Lambda, by path.
PAGES = {"/play": "sessions.html", "/play/workflows": "workflows.html", "/play/factory": "factory.html", "/play/architecture": "architecture.html"}
# The directories of static files, by path prefix.
PUBLIC = {"/play/assets/": f"{PLAY_WEB}/assets", "/play/vendor/": VENDOR}
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
# The webhook secret that this function read.
_state: dict[str, Any] = {}


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
