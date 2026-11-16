"""The relay of the worker: it answers the calls that the front function sends.

A call arrives as one message of the inbox of the shard (see `shards.py`). The worker sends it to the gateway, or to the
Software factory API for `/dashboard/api/`, and writes the answer as S3 objects (see `wire.py`).
An event stream becomes a series of objects, so the browser gets each event at once.
"""

from __future__ import annotations

import base64
import http.client
import json
import queue
import re
import threading
import time
from collections.abc import Callable
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from blockether.vis.engine import GatewayError

from durable import aws, shards, wire
from durable.app import Api
from durable.domain import SESSION
from durable.helpers import log
from durable.settings import ANSWERED_S, CHUNK, COLLECT_S, GATEWAY_PORT, LOADED, LOOPBACK, MAIN, OPENED, PAUSE_S
from durable.vis.gateway import Gateway
from durable.vis.machine import CHANGES, Machine

# The gateway calls that make a session and start a turn, and the calls that delete one session or many sessions.
CREATE, TURN = re.compile(r"/v1/sessions/?"), re.compile(r"/v1/sessions/([0-9a-f-]{36})/turns/?")
# The call that makes a fork of a session.
FORK = re.compile(r"/v1/sessions/[0-9a-f-]{36}/forks/?")
ONE, MANY = re.compile(r"/v1/sessions/([0-9a-f-]{36})/?"), re.compile(r"/v1/(session-groups|projects)/[^/]+/?")


def reply(rid: str, head: dict[str, Any], body: bytes) -> None:
    """Write a complete answer: the one part of a call."""
    aws.put(f"relay/{rid}/0", wire.LAST + wire.frame(head, body))


class Relay:
    """The calls that this worker answers, and their messages in the inbox."""

    def __init__(self, gateway: Gateway, api: Api, inbox: shards.Inbox, reads: list[str], machine: Machine) -> None:
        """Start without an open call.

        `gateway` gets the calls of Vis, `api` gets the calls of `/dashboard/api/`, `inbox` has the messages,
        `reads` are the reads of the last record and `machine` is the state that each worker shares.
        """
        self.gateway, self.api, self.inbox, self.machine = gateway, api, inbox, machine
        self.inflight, self.last = 0, time.time()
        # The open event streams: they are a part of `inflight`.
        self.streams = 0
        # True when the worker stops: each open event stream then ends.
        self.closing = False
        # The reads that a tab makes without a person: those of the last record, then those that this worker answered.
        self.reads: dict[str, None] = dict.fromkeys(read for read in reads if not read.startswith("PUT "))
        self._lock = threading.Lock()
        self._receipts: dict[str, str] = {}
        self._answered: dict[str, float] = {}

    def accept(self, message: dict[str, Any]) -> dict[str, Any] | None:
        """Return the call of a message, or None for a repeated or expired message."""
        call = json.loads(message["Body"])
        rid, now = call["id"], time.time()
        with self._lock:
            known = rid in self._receipts
            self._receipts[rid] = message["ReceiptHandle"]
            if rid in self._answered or call["answer_by"] < now:
                known = True
                self._delete(rid)
            for old in [key for key, at in self._answered.items() if now - at > ANSWERED_S]:
                del self._answered[old]
            # The call is open from this point, so the worker does not stop between the message and its answer.
            self.inflight += not known
        return None if known else call

    def _delete(self, rid: str) -> None:
        """Remove the message of a call from the inbox. The caller holds the lock."""
        receipt = self._receipts.pop(rid, None)
        self._answered[rid] = time.time()
        if receipt:
            try:
                self.inbox.delete(receipt)
            except aws.ClientError as failure:
                log("delete message failed", repr(failure))

    def calm(self, timeout_s: float) -> None:
        """Wait until each open call that is not an event stream has its answer."""
        end = time.time() + timeout_s
        while self.open() and time.time() < end:
            time.sleep(0.05)

    def settle(self, timeout_s: float) -> None:
        """End each open event stream and wait until each call wrote its last part."""
        self.closing = True
        end = time.time() + timeout_s
        while self.inflight and time.time() < end:
            time.sleep(0.1)

    def open(self) -> int:
        """Return the number of the open calls that are not event streams."""
        with self._lock:
            return self.inflight - self.streams

    def record(self) -> None:
        """Record for the front the answer of the gateway to each call that a tab makes without a person (see `shards.keep`).

        The calls are the reads that a tab made, in `main` the reads of the machine, the reads of a tab that opens a
        session, for each session of this worker, and the call that says that a person saw a session, for each session
        that the person saw to its end.
        """
        with self._lock:
            names = dict.fromkeys([*self.reads, *(LOADED if self.machine.is_main else ())])
        try:
            for row in self.gateway.sessions():
                if row.get("archived_at"):
                    continue
                names.update(dict.fromkeys(target.format(id=row["id"]) for target in OPENED))
                if not row.get("is_unread"):
                    names[f"PUT /v1/sessions/{row['id']}/read"] = None

            def ask(name: str) -> list[Any] | None:
                """Return the head and the body of the answer to one call, or None for a call that the gateway refused."""
                method, target, body = [*name.split(" ", 2), "{}"][:3] if name.startswith(("PUT ", "POST ")) else ("GET", name, None)
                status, headers, data = self.gateway.read(method, self.gateway.inward(target.encode()).decode(), body and json.loads(body))
                if status != 200:
                    return None
                return [{"status": status, "headers": [pair for pair in headers if pair[0].lower() not in wire.HOP]}, base64.b64encode(self.gateway.outward(data)).decode()]

            with ThreadPoolExecutor(8) as pool:
                answers = {name: found for name, found in zip(names, pool.map(ask, names), strict=True) if found}
            shards.keep(self.api.shard, answers)
        except (GatewayError, OSError, http.client.HTTPException, aws.ClientError) as failure:
            log("no record of the reads", repr(failure))

    def done(self, rid: str) -> None:
        """Remove the message of an answered call from the inbox."""
        with self._lock:
            self._delete(rid)

    def handle(self, call: dict[str, Any]) -> None:
        """Answer one call that `accept` gave. A failure becomes a 502 answer."""
        rid, is_stream, is_quiet = call["id"], False, bool(shards.quiet(call["method"], call["path"]))
        try:
            is_stream = self.answer(call)
        except Exception as failure:  # the browser gets a plain answer, the log gets the cause
            log("call failed", call["method"], call["path"], repr(failure))
            reply(rid, {"status": 502, "headers": wire.JSON}, b'{"error": "the Vis worker could not answer the call"}')
        finally:
            self.done(rid)
            with self._lock:
                self.inflight -= 1
                # An event stream and a read that a tab repeats do not keep a worker.
                if not (is_stream or is_quiet):
                    self.last = time.time()

    def assigned(self, status: int, data: bytes) -> None:
        """Record the shard of a session that a call made. The front must know it before a client asks for the session."""
        made = json.loads(data) if status < 300 else {}
        found = (made.get("id") or (made.get("session") or {}).get("id")) if isinstance(made, dict) else None
        if isinstance(found, str) and SESSION.fullmatch(found):
            self.gateway.route(found, self.api.shard)

    def body(self, call: dict[str, Any]) -> bytes:
        """Return the body of a call. A large body comes in its own S3 object."""
        if "body_key" in call:
            data, _ = aws.get(call["body_key"])
            aws.delete(call["body_key"])
            return data or b""
        return base64.b64decode(call.get("body") or "")

    def answer(self, call: dict[str, Any]) -> bool:
        """Send a call to the API or to the gateway and write its answer. Return True when the answer was an event stream.

        A call that makes, uses or deletes a session also changes its work directory:
        see `vis/sessions.py`.
        """
        rid, body = call["id"], self.body(call)
        if call["path"].startswith("/dashboard/api/"):
            status, headers, data = self.api.handle(call["method"], call["path"], body)
            reply(rid, {"status": status, "headers": headers}, data)
            return False
        if not self.machine.is_main:
            self.machine.follow()
        sent, body, target = body, self.gateway.inward(body), self.gateway.inward(call["path"].encode()).decode()
        headers = {**call["headers"], **self.gateway.headers(), "host": f"{LOOPBACK}:{GATEWAY_PORT}"}
        if "content-type" in call["headers"]:  # the gateway headers name JSON: the type of the call wins
            headers["content-type"] = call["headers"]["content-type"]
        method, path = call["method"], call["path"].partition("?")[0]
        # What the worker does with the answer of the gateway, for a call that makes or deletes a session.
        after: Callable[[int, bytes], Any] | None = None
        turn, one = TURN.fullmatch(path), ONE.fullmatch(path)
        if method == "POST" and CREATE.fullmatch(path):
            body, made = self.api.sessions.place(body)
            # The body has a new length: the connection counts it.
            headers = {name: value for name, value in headers.items() if name.lower() != "content-length"}
            after = lambda status, data: (self.assigned(status, data), made and self.api.sessions.placed(made, status, data))  # noqa: E731
        elif method == "POST" and turn:
            self.api.sessions.touch(turn.group(1))
        elif method == "DELETE" and one:
            roots = self.api.sessions.leaving(one.group(1))
            after = lambda status, data: status < 300 and (self.api.sessions.reap(roots), self.api.shard != MAIN and shards.forget(one.group(1)))  # noqa: E731
        elif method == "DELETE" and MANY.fullmatch(path):
            after = lambda status, data: status < 300 and self.api.sessions.reap(None)  # noqa: E731
        elif method == "POST" and self.api.shard != MAIN and FORK.fullmatch(path):
            after = self.assigned
        # The call goes to the gateway as it is: the SDK does not relay the call of another client.
        connection = http.client.HTTPConnection(LOOPBACK, GATEWAY_PORT, timeout=max(5.0, call["deadline"] - time.time()))
        try:
            connection.request(call["method"], target, body or None, headers)
            answer = connection.getresponse()
            head = {"status": answer.status, "headers": [[k, v] for k, v in answer.getheaders() if k.lower() not in wire.HOP]}
            if "text/event-stream" not in (answer.getheader("content-type") or ""):
                data = self.gateway.outward(answer.read())
                if self.machine.is_main and method != "GET":
                    # The front gives the answer after the record, so the next call of another shard has the change.
                    self.machine.record(bool(CHANGES.fullmatch(path)))
                elif method != "GET":
                    # The list of the front has the change before the client reads the list again.
                    self.gateway.publish(self.api.shard, is_full=False)
                # A 304 counts: the record then has the complete answer.
                name = shards.quiet(method, call["path"], sent)
                # The record gets the call of a session that a person saw from the list rows, not from here (see `record`).
                if answer.status in (200, 304) and name and method != "PUT":
                    with self._lock:
                        self.reads[name] = None
                if after:
                    try:
                        after(answer.status, data)
                    except Exception as failure:  # the gateway answered: the call keeps its answer
                        log("no work directory change", method, path, repr(failure))
                reply(rid, head, data)
                return False
            with self._lock:
                self.streams += 1
            try:
                self.stream(call, head, answer)
            finally:
                with self._lock:
                    self.streams -= 1
            return True
        finally:
            connection.close()

    def stream(self, call: dict[str, Any], head: dict[str, Any], answer: http.client.HTTPResponse) -> None:
        """Write an event stream as S3 objects until it ends or the deadline comes."""
        rid, chunks = call["id"], queue.Queue()

        def read() -> None:
            """Read the stream in a thread, so the writer can stop at the deadline."""
            try:
                while chunk := answer.read1(CHUNK):
                    chunks.put(chunk)
            # The writer closes the connection at the deadline: `http.client` then fails with one of these.
            except (OSError, http.client.HTTPException, AttributeError, ValueError):
                pass
            chunks.put(None)

        threading.Thread(target=read, daemon=True).start()
        index, last, pending, held = 0, False, wire.frame(head), b""
        while not last:
            started = time.time()
            last = self.closing or started > call["deadline"] - 2
            try:
                chunk = chunks.get(timeout=1)
                while True:
                    if chunk is None:
                        last = True
                        break
                    held += chunk
                    chunk = chunks.get(timeout=max(0.0, COLLECT_S - (time.time() - started)))
            except queue.Empty:
                pass
            # An id can be in two chunks, so only the complete lines go out.
            ready, end, held = (held, b"", b"") if last else held.rpartition(b"\n")
            pending += self.gateway.outward(ready + end)
            if pending or last:
                aws.put(f"relay/{rid}/{index}", (wire.LAST if last else wire.MORE) + pending)
                if index == 0:
                    self.done(rid)
                index, pending = index + 1, b""
                time.sleep(PAUSE_S)
