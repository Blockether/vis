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
from typing import Any

from durable import aws, shards, wire
from durable.app import Api
from durable.domain import SESSION
from durable.helpers import log
from durable.settings import ANSWERED_S, CHUNK, COLLECT_S, GATEWAY_PORT, LOOPBACK, MAIN, PAUSE_S
from durable.vis.gateway import Gateway

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

    def __init__(self, gateway: Gateway, api: Api, inbox: shards.Inbox) -> None:
        """Start without an open call. `gateway` gets the calls of Vis, `api` gets the calls of `/dashboard/api/` and `inbox` has the messages."""
        self.gateway, self.api, self.inbox = gateway, api, inbox
        self.inflight, self.last = 0, time.time()
        # True after a call that is not a read.
        self.wrote = False
        # True when the worker stops: each open event stream then ends.
        self.closing = False
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

    def settle(self, timeout_s: float) -> None:
        """End each open event stream and wait until each call wrote its last part."""
        self.closing = True
        end = time.time() + timeout_s
        while self.inflight and time.time() < end:
            time.sleep(0.1)

    def done(self, rid: str) -> None:
        """Remove the message of an answered call from the inbox."""
        with self._lock:
            self._delete(rid)

    def handle(self, call: dict[str, Any]) -> None:
        """Answer one call. A failure becomes a 502 answer."""
        with self._lock:
            self.inflight += 1
            self.wrote = self.wrote or call["method"] != "GET"
        rid = call["id"]
        try:
            self.answer(call)
        except Exception as failure:  # the browser gets a plain answer, the log gets the cause
            log("call failed", call["method"], call["path"], repr(failure))
            reply(rid, {"status": 502, "headers": wire.JSON}, b'{"error": "the Vis worker could not answer the call"}')
        finally:
            self.done(rid)
            with self._lock:
                self.inflight -= 1
                self.last = time.time()

    def forked(self, status: int, data: bytes) -> None:
        """Record the shard of a fork. The front must know it before a client asks for the fork."""
        made = json.loads(data) if status < 300 else {}
        found = (made.get("id") or (made.get("session") or {}).get("id")) if isinstance(made, dict) else None
        if isinstance(found, str) and SESSION.fullmatch(found):
            shards.assign(found, self.api.shard)

    def body(self, call: dict[str, Any]) -> bytes:
        """Return the body of a call. A large body comes in its own S3 object."""
        if "body_key" in call:
            data, _ = aws.get(call["body_key"])
            aws.delete(call["body_key"])
            return data or b""
        return base64.b64decode(call.get("body") or "")

    def answer(self, call: dict[str, Any]) -> None:
        """Send a call to the API or to the gateway and write its answer.

        A call that makes, uses or deletes a session also changes its work directory:
        see `vis/sessions.py`.
        """
        rid, body = call["id"], self.body(call)
        if call["path"].startswith("/dashboard/api/"):
            status, headers, data = self.api.handle(call["method"], call["path"], body)
            return reply(rid, {"status": status, "headers": headers}, data)
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
            if made:
                after = lambda status, data: self.api.sessions.placed(made, status, data)  # noqa: E731
        elif method == "POST" and turn:
            self.api.sessions.touch(turn.group(1))
        elif method == "DELETE" and one:
            roots = self.api.sessions.leaving(one.group(1))
            after = lambda status, data: status < 300 and (self.api.sessions.reap(roots), self.api.shard != MAIN and shards.forget(one.group(1)))  # noqa: E731
        elif method == "DELETE" and MANY.fullmatch(path):
            after = lambda status, data: status < 300 and self.api.sessions.reap(None)  # noqa: E731
        elif method == "POST" and self.api.shard != MAIN and FORK.fullmatch(path):
            after = self.forked
        # The call goes to the gateway as it is: the SDK does not relay the call of another client.
        connection = http.client.HTTPConnection(LOOPBACK, GATEWAY_PORT, timeout=max(5.0, call["deadline"] - time.time()))
        try:
            connection.request(call["method"], call["path"], body or None, headers)
            answer = connection.getresponse()
            head = {"status": answer.status, "headers": [[k, v] for k, v in answer.getheaders() if k.lower() not in wire.HOP]}
            if "text/event-stream" not in (answer.getheader("content-type") or ""):
                data = answer.read()
                if after:
                    try:
                        after(answer.status, data)
                    except Exception as failure:  # the gateway answered: the call keeps its answer
                        log("no work directory change", method, path, repr(failure))
                return reply(rid, head, data)
            self.stream(call, head, answer)
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
            except (OSError, http.client.HTTPException):
                pass
            chunks.put(None)

        threading.Thread(target=read, daemon=True).start()
        index, last, pending = 0, False, wire.frame(head)
        while not last:
            started = time.time()
            last = self.closing or started > call["deadline"] - 2
            try:
                chunk = chunks.get(timeout=1)
                while True:
                    if chunk is None:
                        last = True
                        break
                    pending += chunk
                    chunk = chunks.get(timeout=max(0.0, COLLECT_S - (time.time() - started)))
            except queue.Empty:
                pass
            if pending or last:
                aws.put(f"relay/{rid}/{index}", (wire.LAST if last else wire.MORE) + pending)
                if index == 0:
                    self.done(rid)
                index, pending = index + 1, b""
                time.sleep(PAUSE_S)
