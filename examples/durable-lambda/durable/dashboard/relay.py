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
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from durable import aws, shards, wire
from durable.dashboard import navigator
from durable.dashboard.web import RELAY_POLICY, RELAYED, SECURITY, Request, error
from durable.domain import RUN, SESSION
from durable.helpers import log
from durable.lambda_runtime import Context, HttpStream
from durable.settings import ANSWER_S, HOLD_S, JOIN_S, LIFETIME, MAIN, MESSAGE_LIMIT, PING_S, SPREAD, STREAM_S

# A call for one session, and a call that changes one run. Each other call goes to `main`:
# a look at a run reads S3, so it does not start the worker of the run.
ONE = re.compile(rf"/(?:v1|dashboard/api)/sessions/({SESSION.pattern})(?:/.*)?")
ACT = re.compile(rf"/dashboard/api/runs/({RUN.pattern})/.+")
# The call that makes a session, and the calls that change many sessions: the archive of many sessions and a call for one group.
CREATE = re.compile(r"/v1/sessions/?")
ARCHIVE = "/v1/sessions/actions/archive"
GROUP = re.compile(rf"/v1/session-groups/({SESSION.pattern})/?")
# The fields of their answers that name sessions, and the field that counts the sessions of a group.
LISTS, COUNT = ("archived", "working", "deleted_session_ids", "scattered_session_ids"), "session_count"
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


def fresh(request: Request) -> str:
    """Return the shard of a new session: each session that a person starts has its own worker.

    A repeated call has the key of the first call, so it gets the same shard. A call
    that names a workspace goes to `main`, which has the workspaces of the machine.
    """
    asked = named(request)
    if asked.get("workspace_id"):
        return MAIN
    key = asked.get("idempotency_key")
    return uuid.uuid5(uuid.NAMESPACE_URL, key).hex if isinstance(key, str) and key else shards.fresh()


def place(request: Request, is_fresh: bool = False) -> str:
    """Return the shard whose worker answers a call."""
    if request.method == "POST" and CREATE.fullmatch(request.path):
        return fresh(request)
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


def call(request: Request, shard: str, deadline: float, wakes: bool = True, patience: float = ANSWER_S) -> Iterator[tuple[bytes, bool]]:
    """Send a call to the worker of a shard. Return the parts of its answer.

    `wakes` starts a worker for the call, and the worker has `patience` seconds to start its answer.
    A read that a tab repeats gets the record of a shard whose worker stopped, so it starts no worker (see `shards.keep`).
    """
    read = shards.quiet(request.method, request.target, request.body)
    found = shards.kept(shard, read) if read else None
    if found:
        return iter([(found, True)])
    rid = str(uuid.uuid4())
    message: dict[str, Any] = {
        "id": rid,
        "method": request.method,
        "path": request.target,
        "deadline": deadline,
        # After this time no client waits for the answer, so the worker drops the call.
        "answer_by": time.time() + patience,
        # The worker adds its own gateway token: a token of the caller never reaches the gateway.
        "headers": {k: v for k, v in request.headers.items() if k not in wire.HOP | {"cookie", "authorization"} and not k.startswith("x-amz")},
    }
    if len(request.body) > MESSAGE_LIMIT:
        aws.put(f"relay/{rid}/request", request.body)
        message["body_key"] = f"relay/{rid}/request"
    else:
        message["body"] = base64.b64encode(request.body).decode()
    shards.send(shard, message)
    return shards.parts(rid, deadline, shard, wakes, patience, read)


def split(request: Request) -> tuple[list[tuple[str, Request, bool]], tuple[Request, bool] | None]:
    """Return the call of an event stream for each shard, with True for a call that gets only the fleet feed of a stream that also has sessions.

    Each shard streams its own sessions and its own part of the fleet feed, which is the
    status of each session. The second result is the call for the fleet feed of each
    other shard that has a worker, or None for a stream without the feed (see `merged`).
    """
    fields = urllib.parse.parse_qsl(request.query, keep_blank_values=True)
    asked = dict(fields)
    scope, groups = (asked.get("scope") or "").strip(), {}
    for token in filter(None, (asked.get("sids") or "").split(",")):
        session = token.partition(":")[0].strip().lower()
        groups.setdefault(shards.owner(session) if SESSION.fullmatch(session) else MAIN, []).append(token)
    # The parts of a compressed stream do not join, and the front cannot hold such a stream.
    request = dataclasses.replace(request, headers={name: value for name, value in request.headers.items() if name != "accept-encoding"})
    if scope == "fleet" or not groups:
        return [(MAIN, request, False)], ((request, False) if scope == "fleet" else None)
    calls = [(shard, dataclasses.replace(request, query=urllib.parse.urlencode([(name, ",".join(tokens) if name == "sids" else value) for name, value in fields])), False) for shard, tokens in groups.items()]
    if scope != "both":
        return calls, None
    feed = dataclasses.replace(request, query=urllib.parse.urlencode([(name, "fleet" if name == "scope" else value) for name, value in fields if name != "sids"]))
    return calls + ([] if MAIN in groups else [(MAIN, feed, True)]), (feed, True)


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


def merged(calls: list[tuple[str, Request, bool]], feed: tuple[Request, bool] | None, deadline: float) -> HttpStream:
    """Join the event streams of the shards in one answer.

    The answer starts with the first stream that starts, and each other stream joins
    when its worker answers. It ends when one stream of `calls` ends: the client connects
    again with its cursors, and each worker continues from them.

    An event stream alone does not start and does not keep a worker. The front holds
    the stream of a shard without a worker until a call starts its worker, then the stream joins.
    `feed` is the call for the fleet feed: each other shard that has a worker gets it, also a worker
    that starts later. The end of such a stream does not end the answer.
    """
    # The head of a stream, complete events of it, or None for its end.
    found: queue.Queue[dict[str, Any] | bytes | None] = queue.Queue()
    # The shards whose stream is a part of the answer, and the end of the answer.
    joined, ended = {shard for shard, _, _ in calls}, threading.Event()

    def pump(shard: str, request: Request, is_fleet: bool, is_extra: bool = False) -> None:
        """Read the stream of one shard and give its complete events to the answer."""
        try:
            part = None
            while part is None:
                if not shards.live(shard):
                    if is_extra:
                        return
                    found.put(HELD)
                    while not shards.live(shard):
                        if time.time() > deadline - HOLD_S:
                            return
                        time.sleep(HOLD_S)
                source = call(request, shard, deadline, wakes=False, patience=JOIN_S)
                part = next(source, None)
                if part is None:
                    # The worker stops or starts: it did not take the call.
                    if is_extra or time.time() > deadline - JOIN_S:
                        return
                    found.put(HELD)
            head, pending = wire.unframe(part[0])
            if not is_extra:
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
            if is_extra:
                # The shard joins again when it has a worker again.
                joined.discard(shard)
            else:
                found.put(None)

    def join() -> None:
        """Give the fleet feed of each other shard that can have a worker to the answer. Look again each JOIN_S."""
        while feed and not ended.is_set() and time.time() < deadline - JOIN_S:
            try:
                for shard in {shard for shard, _ in shards.marked()} - joined:
                    joined.add(shard)
                    threading.Thread(target=pump, args=(shard, *feed, True), daemon=True).start()
            except aws.ClientError as failure:
                log("no list of the shards", repr(failure))
            ended.wait(JOIN_S)

    for one in calls:
        threading.Thread(target=pump, args=one, daemon=True).start()
    threading.Thread(target=join, daemon=True).start()
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
        try:
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
        finally:
            ended.set()

    return HttpStream(status=200, headers={**relayed, **SECURITY, "content-security-policy": RELAY_POLICY}, chunks=body())


def reach(request: Request) -> list[dict[str, Any]] | None:
    """Return the rows of the other shards that a call for many sessions names, or None for another call."""
    asked, group = named(request), GROUP.fullmatch(request.path)
    if group and request.method == "DELETE":
        # The gateway deletes the sessions of the group with the group, or it leaves them without a group (see `navigator.filed`).
        return [row for row in shards.shown() if row.get("group_id") == group.group(1)]
    if group and request.method == "PATCH" and asked.get("archived"):
        # The gateway refuses the archive of a group while one of its sessions works. Each other row follows the group (see `navigator.filed`).
        return [row for row in shards.shown() if row.get("group_id") == group.group(1) and working(row)]
    if request.method != "POST" or request.path.rstrip("/") != ARCHIVE:
        return None
    ids, loose = asked.get("ids"), asked.get("loose") is True
    scope = {field: asked[name] for name, field in (("root", "project_root"), ("project_id", "project_id")) if asked.get(name) is not None}
    if not (scope or isinstance(ids, list)):
        return None
    active = navigator.filed(shards.shown())[0]
    return [row for row in active if (not isinstance(ids, list) or row["id"] in ids) and all(row.get(field) == value for field, value in scope.items()) and not (loose and row.get("group_id"))]


def working(row: dict[str, Any]) -> bool:
    """Return True for the row of a session with work that a person waits for. The gateway does not archive such a session."""
    return bool(row.get("live") or row.get("is_awaiting_input") or row.get("status") in ("running", "queued"))


def whole(request: Request, shard: str, deadline: float) -> tuple[int, list[list[str]], bytes]:
    """Send a call to the worker of a shard. Return the status, the headers and the complete body of its answer."""
    source = call(request, shard, deadline)
    first = next(source, None)
    if first is None:
        return 504, wire.JSON, b'{"error": "the Vis worker did not answer"}'
    head, data = wire.unframe(first[0])
    return head["status"], head["headers"], data + b"".join(chunk for chunk, _ in source)


def spread(request: Request, rows: list[dict[str, Any]], deadline: float) -> HttpStream:
    """Send a call for many sessions to each shard that has one of the `rows`, then to `main`. Return one answer.

    The other shards answer first: `main` is the writer of the groups, so a group changes only after
    each shard changed its sessions. An answer that refuses the call ends it, and a repeated call does the rest.
    A check of an archive (`dry_run`) and the delete of a group that keeps its sessions read the rows, so they start no worker of another shard.
    """
    asked, found = named(request), {name: [] for name in LISTS}
    if request.method == "POST" and asked.get("dry_run") is True:
        for row in rows:
            found["working" if working(row) else "archived"].append(row["id"])
    elif request.method == "DELETE" and asked.get("sessions") != "delete":
        found["scattered_session_ids"] = [row["id"] for row in rows]
    else:
        with ThreadPoolExecutor(SPREAD) as pool:
            answers = list(pool.map(lambda shard: whole(request, shard, deadline), sorted({shards.owner(row["id"]) for row in rows} - {MAIN})))
        for status, headers, data in answers:
            # A shard that does not have the group has no session of it.
            if status >= 300 and not (status == 404 and request.method != "POST"):
                return answered(status, headers, data)
            for name, ids in (json.loads(data) if status < 300 else {}).items():
                if name in LISTS:
                    found[name] += ids or []
    status, headers, data = whole(request, MAIN, deadline)
    if status == 200:
        answer = json.loads(data)
        answer.update({name: (answer.get(name) or []) + ids for name, ids in found.items() if name in answer})
        if COUNT in answer:
            answer[COUNT] += len(found["deleted_session_ids"]) + len(found["scattered_session_ids"])
        data, headers = json.dumps(answer).encode(), [pair for pair in headers if pair[0].lower() not in CHANGED]
    shards.changed()
    return answered(status, headers, data)


def answered(status: int, headers: list[list[str]], data: bytes) -> HttpStream:
    """Return the complete answer of a worker as the answer of the front."""
    relayed = {k.lower(): v for k, v in headers if k.lower() in RELAYED}
    return HttpStream(status=status, headers={**relayed, **SECURITY, "content-security-policy": RELAY_POLICY}, chunks=iter([data]))


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
    shard, asked = place(request), request
    request = known(request, shard)
    is_list = navigator.wanted(request)
    if is_list:
        # The front changes this answer: it must be complete, not compressed and not an answer of a cache.
        request = navigator.paged(dataclasses.replace(request, headers={k: v for k, v in request.headers.items() if k not in {"accept-encoding", "if-none-match", "if-modified-since"}}))
    if request.path.rstrip("/") == EVENTS:
        return merged(*split(request), deadline)
    rows = reach(request)
    if rows is not None:
        # The body of a call is not compressed, and each answer must be complete and not compressed, so the parts join.
        return spread(dataclasses.replace(request, headers={k: v for k, v in request.headers.items() if k != "accept-encoding"}), rows, deadline)
    source = call(request, shard, deadline)
    first = next(source, None)
    if first is None:
        return error(504, "the Vis worker did not answer")
    if request.method != "GET":
        shards.changed()
    head, data = wire.unframe(first[0])
    status = head["status"]
    if status == 404 and shard == MAIN and place(request, is_fresh=True) != MAIN:
        # Another shard made the session after the last look of this function.
        return relay(asked, context)
    if type(status) is not int or not 100 <= status <= 599:
        return error(502, "the Vis worker sent a status that is not valid")
    if 300 <= status < 400 and status != 304:
        return error(502, "the Vis worker sent a redirect")
    relayed = {k.lower(): v for k, v in head["headers"] if k.lower() in RELAYED - (CHANGED if is_list else set())}
    headers = {**relayed, **SECURITY, "content-security-policy": RELAY_POLICY}
    if is_list and status == 200:
        whole = navigator.joined(asked, data + b"".join(chunk for chunk, _ in source), lambda path, query: fetch(request, path, query, deadline))
        return HttpStream(status=200, headers=headers, chunks=iter([whole]))

    def body() -> Iterator[bytes]:
        """Yield the first bytes, then each later part of the answer."""
        yield data
        for chunk, _ in source:
            yield chunk

    return HttpStream(status=status, headers=headers, chunks=body())
