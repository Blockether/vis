"""The relay of the worker: it answers the calls that the front function sends.

A call arrives as one SQS message. The worker sends it to the gateway, or to the
playground API for `/play/api/`, and writes the answer as S3 objects (see `wire.py`).
An event stream becomes a series of objects, so the browser gets each event at once.
"""

from __future__ import annotations

import base64
import http.client
import json
import queue
import threading
import time
from typing import Any

from playground import aws, wire
from playground.api import Api
from playground.env import QUEUE, log
from playground.gateway import HOST, PORT, Gateway

# An event stream collects its bytes for this time, then writes one S3 object.
COLLECT_S, PAUSE_S = 0.2, 0.15


class Relay:
    def __init__(self, gateway: Gateway, api: Api) -> None:
        self.gateway, self.api = gateway, api
        self.inflight, self.last = 0, time.time()
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
            for old in [key for key, at in self._answered.items() if now - at > 1200]:
                del self._answered[old]
        return None if known else call

    def _delete(self, rid: str) -> None:
        receipt = self._receipts.pop(rid, None)
        self._answered[rid] = time.time()
        if receipt:
            try:
                aws.client("sqs").delete_message(QueueUrl=QUEUE, ReceiptHandle=receipt)
            except aws.ClientError as failure:
                log("delete message failed", repr(failure))

    def done(self, rid: str) -> None:
        """Remove the message of an answered call from the queue."""
        with self._lock:
            self._delete(rid)

    def handle(self, call: dict[str, Any]) -> None:
        """Answer one call. A failure becomes a 502 answer."""
        with self._lock:
            self.inflight += 1
        rid = call["id"]
        try:
            self.answer(call)
        except Exception as failure:  # the browser gets the cause as a plain answer
            log("call failed", call["method"], call["path"], repr(failure))
            body = json.dumps({"error": "the Vis worker could not answer the call"}).encode()
            aws.put(f"relay/{rid}/0", wire.LAST + wire.frame({"status": 502, "headers": [["content-type", "application/json"]]}, body))
        finally:
            self.done(rid)
            with self._lock:
                self.inflight -= 1
                self.last = time.time()

    def body(self, call: dict[str, Any]) -> bytes:
        if "body_key" in call:
            data, _ = aws.get(call["body_key"])
            aws.delete(call["body_key"])
            return data or b""
        return base64.b64decode(call.get("body") or "")

    def answer(self, call: dict[str, Any]) -> None:
        rid, body = call["id"], self.body(call)
        if call["path"].startswith("/play/api/"):
            status, headers, data = self.api.handle(call["method"], call["path"], body)
            aws.put(f"relay/{rid}/0", wire.LAST + wire.frame({"status": status, "headers": headers}, data))
            return
        headers = {**call["headers"], **self.gateway.headers(), "host": f"{HOST}:{PORT}"}
        if "content-type" in call["headers"]:
            headers["content-type"] = call["headers"]["content-type"]
        connection = http.client.HTTPConnection(HOST, PORT, timeout=max(5.0, call["deadline"] - time.time()))
        try:
            connection.request(call["method"], call["path"], body or None, headers)
            answer = connection.getresponse()
            head = {"status": answer.status, "headers": [[k, v] for k, v in answer.getheaders() if k.lower() not in wire.HOP]}
            if "text/event-stream" not in (answer.getheader("content-type") or ""):
                aws.put(f"relay/{rid}/0", wire.LAST + wire.frame(head, answer.read()))
                return
            self.stream(call, head, answer)
        finally:
            connection.close()

    def stream(self, call: dict[str, Any], head: dict[str, Any], answer: http.client.HTTPResponse) -> None:
        """Write an event stream as S3 objects until it ends or the deadline comes."""
        rid, chunks = call["id"], queue.Queue()

        def read() -> None:
            try:
                while chunk := answer.read1(65536):
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
