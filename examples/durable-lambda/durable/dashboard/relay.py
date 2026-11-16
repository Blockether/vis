"""The front side of the relay: a call goes to the worker of its shard and its answer comes back as S3 objects (see `wire.py`).

The worker side is `vis/relay.py`. `shards.py` names the shard of each session and of each run.
"""

from __future__ import annotations

import base64
import dataclasses
import json
import queue
import re
import threading
import time
import urllib.parse
import uuid
from collections.abc import Iterator
from typing import Any

from durable import aws, shards, wire
from durable.dashboard import navigator
from durable.dashboard.web import RELAY_POLICY, RELAYED, SECURITY, Request, error
from durable.domain import RUN, SESSION
from durable.helpers import log
from durable.lambda_runtime import Context, HttpStream
from durable.settings import ANSWER_S, HOLD_S, LIFETIME, MAIN, MESSAGE_LIMIT, PING_S, STREAM_S

# A call for one session, and a call that changes one run. Each other call goes to `main`:
# a look at a run reads S3, so it does not start the worker of the run.
ONE = re.compile(rf"/(?:v1|dashboard/api)/sessions/({SESSION.pattern})(?:/.*)?")
ACT = re.compile(rf"/dashboard/api/runs/({RUN.pattern})/.+")
# The event stream of the gateway. It can have sessions of more than one shard.
EVENTS = "/v1/events"
# A setting, an MCP server and a reload of the extensions name their session in the query or in a JSON body.
SETTINGS = "/v1/settings"
NAMED = (SETTINGS, "/v1/mcp/servers", "/v1/extensions/reload")
# The session whose own scopes mark the rows of a settings answer.
CONTEXT = "context_session_id"
# The headers of an answer that are not true after the front changed its body.
CHANGED = {"content-encoding", "etag", "last-modified"}
# The head and the comment of an event stream that the front holds for a shard without a worker.
HELD = {"status": 200, "headers": [["content-type", "text/event-stream"], ["cache-control", "no-cache"]]}
PING = b": ping\n\n"


def named(request: Request) -> dict[str, Any]:
    """Return the fields of the query and of a JSON body. A field of the body wins, as in the gateway."""
    fields: dict[str, Any] = dict(urllib.parse.parse_qsl(request.query))
    try:
        body = json.loads(request.body) if request.body else None
    except ValueError:
        body = None
    return {**fields, **body} if isinstance(body, dict) else fields


def place(request: Request, is_fresh: bool = False) -> str:
    """Return the shard whose worker answers a call."""
    found = ONE.fullmatch(request.path) or (request.method == "POST" and ACT.fullmatch(request.path))
    if not found and request.path.startswith(NAMED):
        # The target of a project or of a group has no record, so `main` answers.
        target = named(request).get("target_id")
        found = SESSION.fullmatch(target) if isinstance(target, str) else None
        return shards.owner(found.group(0), is_fresh) if found else MAIN
    return shards.owner(found.group(1), is_fresh) if found else MAIN


def known(request: Request, shard: str) -> Request:
    """Remove the context session of a settings call when the worker that answers does not have it.

    The gateway refuses a session that it does not know. The context only marks the
    rows that the session takes from its own scopes, so the answer stays right without it.
    """
    if not request.path.startswith(SETTINGS):
        return request
    session = named(request).get(CONTEXT)
    if not isinstance(session, str) or not SESSION.fullmatch(session) or shards.owner(session) == shard:
        return request
    query = urllib.parse.urlencode([(name, value) for name, value in urllib.parse.parse_qsl(request.query, keep_blank_values=True) if name != CONTEXT])
    if CONTEXT.encode() not in request.body:
        return dataclasses.replace(request, query=query)
    body = json.dumps({name: value for name, value in json.loads(request.body).items() if name != CONTEXT}).encode()
    # The body has a new length: the connection of the worker counts it.
    return dataclasses.replace(request, query=query, body=body, headers={k: v for k, v in request.headers.items() if k != "content-length"})


def call(request: Request, shard: str, deadline: float, wakes: bool = True) -> Iterator[tuple[bytes, bool]]:
    """Send a call to the worker of a shard. Return the parts of its answer. `wakes` starts a worker for the call."""
    rid = str(uuid.uuid4())
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
    if len(request.body) > MESSAGE_LIMIT:
        aws.put(f"relay/{rid}/request", request.body)
        message["body_key"] = f"relay/{rid}/request"
    else:
        message["body"] = base64.b64encode(request.body).decode()
    shards.send(shard, message)
    return shards.parts(rid, deadline, shard, wakes)


def split(request: Request) -> list[tuple[str, Request, bool]]:
    """Return the call of an event stream for each shard, and True for a call that gets only the fleet feed.

    Each shard streams its own sessions. The fleet feed is the status of the sessions
    of people, so it comes from `main`.
    """
    fields = urllib.parse.parse_qsl(request.query, keep_blank_values=True)
    asked = dict(fields)
    scope, groups = (asked.get("scope") or "").strip(), {}
    for token in filter(None, (asked.get("sids") or "").split(",")):
        session = token.partition(":")[0].strip().lower()
        groups.setdefault(shards.owner(session) if SESSION.fullmatch(session) else MAIN, []).append(token)
    # The parts of a compressed stream do not join, and the front cannot hold such a stream.
    headers = {name: value for name, value in request.headers.items() if name != "accept-encoding"}
    if scope == "fleet" or not groups or (len(groups) == 1 and (MAIN in groups or scope != "both")):
        shard = next(iter(groups), MAIN) if scope != "fleet" else MAIN
        return [(shard, request if shard == MAIN else dataclasses.replace(request, headers=headers), False)]
    calls = []
    for shard, tokens in groups.items():
        query = [(name, ",".join(tokens) if name == "sids" else value) for name, value in fields if name != "scope" or shard == MAIN]
        calls.append((shard, dataclasses.replace(request, query=urllib.parse.urlencode(query), headers=headers), False))
    if scope == "both" and MAIN not in groups:
        query = [(name, "fleet" if name == "scope" else value) for name, value in fields if name != "sids"]
        calls.append((MAIN, dataclasses.replace(request, query=urllib.parse.urlencode(query), headers=headers), True))
    return calls


def tagged(events: bytes) -> bytes:
    """Mark each event of the fleet feed, as the gateway marks it in a stream that also has sessions."""
    lines = []
    for line in events.split(b"\n"):
        if line.startswith(b"data:"):
            try:
                event = json.loads(line[5:])
            except ValueError:
                event = None
            if isinstance(event, dict):
                line = b"data: " + json.dumps({**event, "scope": "fleet"}, separators=(",", ":")).encode()
        lines.append(line)
    return b"\n".join(lines)


def merged(calls: list[tuple[str, Request, bool]], deadline: float) -> HttpStream:
    """Join the event streams of the shards in one answer.

    The answer starts with the first stream that starts, and each other stream joins
    when its worker answers. It ends when one stream ends: the client connects again
    with its cursors, and each worker continues from them.

    An event stream alone does not start the worker of a run. The front holds the
    stream of such a shard until a call starts its worker, then the stream joins.
    """
    # The head of a stream, complete events of it, or None for its end.
    found: queue.Queue[dict[str, Any] | bytes | None] = queue.Queue()

    def pump(shard: str, request: Request, is_fleet: bool) -> None:
        """Read the stream of one shard and give its complete events to the answer."""
        try:
            if shard != MAIN and not shards.live(shard):
                found.put(HELD)
                while not shards.live(shard):
                    if time.time() > deadline - HOLD_S:
                        return
                    time.sleep(HOLD_S)
            source = call(request, shard, deadline, wakes=shard == MAIN)
            part = next(source, None)
            if part is None:
                return
            head, pending = wire.unframe(part[0])
            found.put(head)
            while head["status"] == 200:
                cut = pending.rfind(b"\n\n") + 2
                if cut > 1:
                    found.put(tagged(pending[:cut]) if is_fleet else pending[:cut])
                    pending = pending[cut:]
                part = None if part[1] else next(source, None)
                if part is None:
                    return
                pending += part[0]
        except Exception as failure:  # the client connects again
            log("the event stream of a shard failed", shard, repr(failure))
        finally:
            found.put(None)

    for one in calls:
        threading.Thread(target=pump, args=one, daemon=True).start()
    try:
        head = found.get(timeout=max(1.0, deadline - time.time()))
    except queue.Empty:
        head = None
    if not isinstance(head, dict) or head["status"] != 200:
        return error(504, "the Vis worker did not answer")
    relayed = {k.lower(): v for k, v in head["headers"] if k.lower() in RELAYED - {"content-encoding"}}

    def body() -> Iterator[bytes]:
        """Yield the events of each stream until one stream ends, and a comment when no stream sent bytes for PING_S."""
        sent = time.time()
        while time.time() < deadline:
            try:
                item = found.get(timeout=1)
            except queue.Empty:
                if time.time() - sent > PING_S:
                    sent = time.time()
                    yield PING
                continue
            if item is None:
                return
            if isinstance(item, bytes):
                sent = time.time()
                yield item

    return HttpStream(status=200, headers={**relayed, **SECURITY, "content-security-policy": RELAY_POLICY}, chunks=body())


def fetch(request: Request, path: str, query: str, deadline: float) -> bytes | None:
    """Make one more GET call to `main` for the caller of a call. Return the body of its answer, or None."""
    source = call(dataclasses.replace(request, method="GET", path=path, query=query, body=b""), MAIN, deadline)
    first = next(source, None)
    if first is None:
        return None
    head, data = wire.unframe(first[0])
    return data + b"".join(chunk for chunk, _ in source) if head["status"] == 200 else None


def relay(request: Request, context: Context) -> HttpStream:
    """Send the call to the worker of its shard and stream its answer."""
    stream = "text/event-stream" in request.headers.get("accept", "")
    deadline = time.time() + min(STREAM_S if stream else LIFETIME, context.remaining_s() - 10)
    shard = place(request)
    request = known(request, shard)
    is_list = navigator.wanted(request)
    if is_list:
        # The front changes this answer: it must be complete, not compressed and not an answer of a cache.
        request = dataclasses.replace(request, headers={k: v for k, v in request.headers.items() if k not in {"accept-encoding", "if-none-match", "if-modified-since"}})
    if request.path.rstrip("/") == EVENTS:
        calls = split(request)
        if len(calls) > 1 or calls[0][0] != MAIN:
            return merged(calls, deadline)
        shard, request, _ = calls[0]
    source = call(request, shard, deadline)
    first = next(source, None)
    if first is None:
        return error(504, "the Vis worker did not answer")
    head, data = wire.unframe(first[0])
    status = head["status"]
    if status == 404 and shard == MAIN and place(request, is_fresh=True) != MAIN:
        # Another shard made the session after the last look of this function.
        return relay(request, context)
    if type(status) is not int or not 100 <= status <= 599:
        return error(502, "the Vis worker sent a status that is not valid")
    if 300 <= status < 400 and status != 304:
        return error(502, "the Vis worker sent a redirect")
    relayed = {k.lower(): v for k, v in head["headers"] if k.lower() in RELAYED - (CHANGED if is_list else set())}
    headers = {**relayed, **SECURITY, "content-security-policy": RELAY_POLICY}
    if is_list and status == 200:
        whole = navigator.joined(request, data + b"".join(chunk for chunk, _ in source), lambda path, query: fetch(request, path, query, deadline))
        return HttpStream(status=200, headers=headers, chunks=iter([whole]))

    def body() -> Iterator[bytes]:
        """Yield the first bytes, then each later part of the answer."""
        yield data
        for chunk, _ in source:
            yield chunk

    return HttpStream(status=status, headers=headers, chunks=body())
