"""The llm function: the only code of the stack that holds the model provider key.

The worker has no route to the internet and no access to the key. It invokes this
function with `InvokeWithResponseStream`. The function accepts only the calls that a
chat model needs, adds the key and streams the answer of the provider back.
"""

from __future__ import annotations

import base64
import http.client
import json
import re
import urllib.parse
import zlib
from collections.abc import Iterator
from typing import Any

from playground import aws, wire
from playground.env import ENV, MODELS, log
from playground.runtime import Context, Stream

ALLOWED = {("GET", "/models"), ("POST", "/chat/completions")}
PASSED = ("content-type", "accept")
# The form of a header value that goes to the provider.
HEADER = re.compile(r"[\w/+.;=,* -]{1,100}", re.ASCII)
LIMIT = 5_000_000
_state: dict[str, Any] = {}


def provider() -> tuple[urllib.parse.SplitResult, str]:
    """Return the base URL and the key of the provider."""
    if not _state:
        url = urllib.parse.urlsplit(aws.parameter(ENV["VIS_LLM_URL_PARAMETER"]).rstrip("/"))
        key = aws.parameter(ENV["VIS_LLM_KEY_PARAMETER"]).strip()
        # An error of the HTTP client quotes a bad header value. The key must never be in such a text.
        if not re.fullmatch(r"[\x21-\x7e]+", key):
            raise RuntimeError("the provider key has a character that a header cannot hold")
        if url.scheme != "https":
            raise RuntimeError("the provider URL must use https")
        _state.update(url=url, key=key)
    return _state["url"], _state["key"]


def problem(status: int, message: str) -> bytes:
    """Return a complete answer that reports a problem."""
    head = {"status": status, "headers": [["content-type", "application/json"]]}
    return wire.frame(head, json.dumps({"error": {"message": message}}).encode())


def checked(method: str, path: str, body: bytes) -> tuple[str | None, bytes]:
    """Return why the function refuses a call, and the body that the provider gets.

    The provider gets the JSON that this function read, not the bytes of the caller.
    Thus the two cannot read different model names from one request.
    """
    if (method, path) not in ALLOWED:
        return f"the playground does not allow {method} {path}", b""
    if len(body) > LIMIT:
        return "the request is too large", b""
    if method != "POST":
        return None, b""
    try:
        request = json.loads(body)
        model = request.get("model")
    except (ValueError, AttributeError):
        return "the request is not a JSON object", b""
    limits = {m["name"]: m["output_limit"] for m in MODELS}
    if not isinstance(model, str) or model not in limits:
        return f"the playground does not allow the model {model!r}", b""
    if request.get("n", 1) != 1:
        return "the playground allows one answer for each call", b""
    # A call cannot ask for more output than the model of the stack has.
    for field in ("max_tokens", "max_completion_tokens"):
        if field in request:
            if not isinstance(request[field], int) or request[field] < 1:
                return f"{field} must be a positive number", b""
            request[field] = min(request[field], limits[model])
    return None, json.dumps(request).encode()


def forward(method: str, path: str, headers: dict[str, str], body: bytes) -> Iterator[bytes]:
    """Call the provider and yield the frame, then the body in parts."""
    url, key = provider()
    connection = http.client.HTTPSConnection(url.netloc, timeout=600)
    try:
        sent = {name: headers[name] for name in PASSED if HEADER.fullmatch(str(headers.get(name, "")))}
        connection.request(method, url.path + path, body or None, {**sent, "authorization": f"Bearer {key}"})
        answer = connection.getresponse()
        kept = [[k, v] for k, v in answer.getheaders() if k.lower() in ("content-type", "retry-after")]
        yield wire.frame({"status": answer.status, "headers": kept})
        while chunk := answer.read1(65536):
            yield chunk
    finally:
        connection.close()


def answer(method: str, path: str, headers: dict[str, str], body: bytes) -> Iterator[bytes]:
    """Yield the answer of the provider. A failure before the first byte becomes a 502 answer."""
    started = False
    try:
        for chunk in forward(method, path, headers, body):
            started = True
            yield chunk
    except (OSError, ValueError, http.client.HTTPException) as failure:
        log("provider failed", repr(failure))
        if not started:
            yield problem(502, "the model provider did not answer")


def handler(event: dict[str, Any], context: Context) -> Stream:
    method, path = str(event.get("method", "")).upper(), str(event.get("path", ""))
    try:
        # Stop at the limit: a small gzip body can hold much more data than the function has memory.
        body = zlib.decompressobj(31).decompress(base64.b64decode(event["body"]), LIMIT + 1) if event.get("body") else b""
    except (zlib.error, ValueError):
        return Stream([problem(400, "the body is not gzip data")])
    refusal, body = checked(method, path, body)
    if refusal:
        log("refused", method, path, refusal)
        return Stream([problem(403, refusal)])
    return Stream(answer(method, path, event.get("headers") or {}, body))
