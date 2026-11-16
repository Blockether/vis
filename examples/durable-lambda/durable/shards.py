"""The shards: each shard has its own worker, lease, home checkpoint and calls.

The shard `main` has the sessions of people. Each workflow run that makes its own
session gets a new shard, and the forks and the subagents of that session stay in it.
So one run does not wait for another run, and a run does not slow the sessions of people.

lease.json                the lease of `main`
shards/<shard>/lease.json the lease of another shard
routes/<id>               the shard of a session or of a run. Without it, the shard is `main`
pending/<shard>           a shard that can have work: the schedule starts its worker again
calls/<shard>/<id>        a call for the worker of a shard. `main` gets its calls from the queue
bells/<shard>             the id of the last call of a shard
lists/<shard>             the list rows of the sessions of a shard: the front shows them and starts no worker
texts/<shard>             the requests and the replies of the sessions of a shard: the front searches them and starts no worker
seen/<shard>              the answers of the last worker of a shard to the reads that a tab repeats: the front gives them and starts no worker

A queue for each shard would stay in the account after its run, so the calls of a
shard are S3 objects. The worker reads the one bell object, which costs less than a list.
"""

from __future__ import annotations

import base64
import json
import re
import threading
import time
import uuid
from collections.abc import Iterator
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from durable import aws, wire
from durable.domain import SESSION, Answer
from durable.helpers import log
from durable.settings import ANSWER_S, HOLD_S, INBOX_LIST_S, INBOX_S, LEASE, MAIN, QUEUE, ROUTE_S, WAKE_S, WORKER_FUNCTION

# The form of the name of a shard that is not `main`.
SHARD = re.compile(r"[0-9a-f]{32}")
# The reads that a tab of Vis web makes without a person, at its load, when its event stream starts again and each 30 s:
# the health, the list, its groups, one session with its views and its model, the settings and the parts of the machine.
QUIET = re.compile(
    r"/healthz|/v1/(?:sessions|session-groups|settings(?:/[a-z_]+)?|apps|router|mcp/servers|devices|automations|capabilities|web/brand)/?"
    rf"|/v1/sessions/{SESSION.pattern}(?:/views/[a-z]+|/model)?/?"
)
# The time of the gateway in an answer.
CLOCK = re.compile(rb'("server_time_ms":\s*)\d+')
# The shard of each id that S3 named, the time of each id that S3 did not name and the time of the last start of each worker.
_routes: dict[str, str] = {}
_missed: dict[str, float] = {}
_woke: dict[str, float] = {}
# For the list rows and for the texts: the object of each shard with its version, and the time of the last look at S3.
_shown: dict[str, dict[str, tuple[Any, Any]]] = {"lists/": {}, "texts/": {}}
_looked: dict[str, float] = {}
_lock = threading.Lock()


def fresh() -> str:
    """Return the name of a new shard."""
    return uuid.uuid4().hex


def valid(shard: Any) -> bool:
    """Return True for the name of a shard."""
    return shard == MAIN or (isinstance(shard, str) and bool(SHARD.fullmatch(shard)))


def lease_key(shard: str) -> str:
    """Return the S3 key of the lease of a shard."""
    return LEASE if shard == MAIN else f"shards/{shard}/lease.json"


def lease(shard: str = MAIN) -> tuple[dict[str, Any], str | None]:
    """Return the lease of a shard and its ETag."""
    raw, etag = aws.get(lease_key(shard))
    return (json.loads(raw) if raw else {}), etag


# -- the shards that can have work ---------------------------------------------------


def mark(shard: str) -> None:
    """Record that a shard can have work."""
    aws.put(f"pending/{shard}", b"")


def unmark(shard: str) -> None:
    """Record that a shard has no work."""
    aws.delete(f"pending/{shard}")


def marked() -> list[tuple[str, float]]:
    """Return each shard that can have work, with the time of its mark."""
    found = [(item["Key"].partition("/")[2], item["LastModified"].timestamp()) for item in aws.keys("pending/")]
    return [(shard, at) for shard, at in found if SHARD.fullmatch(shard)]


# -- the shard of a session or of a run ----------------------------------------------


def assign(name: str, shard: str) -> None:
    """Record the shard of a session or of a run. `main` needs no record."""
    if shard != MAIN:
        aws.put(f"routes/{name}", shard.encode())
        with _lock:
            _routes[name] = shard


def owner(name: str, is_fresh: bool = False) -> str:
    """Return the shard of a session or of a run. An id that S3 does not name is in `main`."""
    with _lock:
        if name in _routes:
            return _routes[name]
        if not is_fresh and time.time() - _missed.get(name, 0.0) < ROUTE_S:
            return MAIN
    raw, _ = aws.get(f"routes/{name}")
    shard = (raw or b"").decode()
    with _lock:
        if valid(shard):
            _routes[name] = shard
            return shard
        if len(_missed) > 10_000:
            _missed.clear()
        _missed[name] = time.time()
    return MAIN


def forget(name: str) -> None:
    """Remove the record of a session that its shard deleted."""
    aws.delete(f"routes/{name}")
    with _lock:
        _routes.pop(name, None)


# -- the list rows of the sessions of a shard ----------------------------------------


def show(shard: str, rows: list[dict[str, Any]]) -> None:
    """Record the list rows of the sessions of a shard."""
    aws.put(f"lists/{shard}", json.dumps(rows).encode())


def tell(shard: str, texts: dict[str, list[list[Any]]]) -> None:
    """Record the texts of the sessions of a shard: for each session, the side, the time and the text of each request and reply."""
    aws.put(f"texts/{shard}", json.dumps(texts).encode())


def recorded(prefix: str) -> list[Any]:
    """Return the object of each shard below `lists/` or `texts/`. S3 gives only the objects that changed."""
    known = _shown[prefix]
    with _lock:
        if time.time() - _looked.get(prefix, 0.0) < ROUTE_S:
            return [value for _, value in known.values()]
    found = {item["Key"]: item.get("ETag") or (item["LastModified"], item["Size"]) for item in aws.keys(prefix)}
    with _lock:
        changed = [key for key, version in found.items() if key not in known or known[key][0] != version]
    with ThreadPoolExecutor(8) as pool:
        read = list(pool.map(lambda key: json.loads(aws.get(key)[0] or b"null"), changed))
    with _lock:
        for key in set(known) - set(found):
            del known[key]
        known.update({key: (found[key], value) for key, value in zip(changed, read, strict=True)})
        _looked[prefix] = time.time()
        return [value for _, value in known.values()]


def shown() -> list[dict[str, Any]]:
    """Return the list rows of the sessions of each shard that is not `main`."""
    return [row for rows in recorded("lists/") for row in rows or []]


def told() -> dict[str, list[list[Any]]]:
    """Return the texts of each session of each shard that is not `main`."""
    return {session: texts for one in recorded("texts/") for session, texts in (one or {}).items()}


# -- the reads that a tab repeats ----------------------------------------------------


def quiet(method: str, target: str) -> bool:
    """Return True for a read that a tab makes without a person. Such a read does not keep a worker, and does not start one when the record has its answer."""
    return method == "GET" and bool(QUIET.fullmatch(target.partition("?")[0]))


def keep(shard: str, answers: dict[str, list[Any]]) -> None:
    """Record the answers of a worker that stops: for each read, its head and its body.

    Without a worker nothing changes the sessions of a shard, so the record stays true.
    The next worker starts with an empty record and with the reads of this record (see `reads`).
    """
    aws.put(f"seen/{shard}", json.dumps(answers).encode())


def reads(shard: str) -> list[str]:
    """Return the reads of the record of a shard, the newest last.

    A tab asks most of them only when its event stream starts again, which is when a
    worker stops. So the next worker must record them too, although no tab asked it.
    """
    raw, _ = aws.get(f"seen/{shard}")
    return list(json.loads(raw)) if raw else []


def kept(shard: str, target: str) -> bytes | None:
    """Return the recorded answer to a read as part 0 of an answer, with the time of now, or None."""
    raw, _ = aws.get(f"seen/{shard}")
    found = json.loads(raw).get(target) if raw else None
    if not found:
        return None
    return wire.frame(found[0], CLOCK.sub(rb"\g<1>%d" % int(time.time() * 1000), base64.b64decode(found[1])))


# -- the calls of a shard ------------------------------------------------------------


def send(shard: str, message: dict[str, Any]) -> None:
    """Give a call to the worker of a shard."""
    if shard == MAIN:
        aws.client("sqs").send_message(QueueUrl=QUEUE, MessageBody=json.dumps(message))
    else:
        aws.put(f"calls/{shard}/{message['id']}", json.dumps(message).encode())
        # The bell comes after the call, so a worker that hears it finds the call.
        aws.put(f"bells/{shard}", message["id"].encode())


def live(shard: str) -> bool:
    """Return True when a live worker holds the lease of a shard."""
    return aws.alive(lease(shard)[0])


def wake(shard: str) -> None:
    """Start the worker of a shard when no live worker holds its lease, at most each WAKE_S."""
    with _lock:
        if time.time() - _woke.get(shard, 0.0) < WAKE_S:
            return
        _woke[shard] = time.time()
    if not live(shard):
        log("waking worker", shard)
        aws.start(WORKER_FUNCTION, kind="wake", shard=shard)


def parts(rid: str, deadline: float, shard: str, wakes: bool = True, patience: float = ANSWER_S, read: str | None = None) -> Iterator[tuple[bytes, bool]]:
    """Yield each part of the answer of a call and whether it is the last part.

    `wakes` starts a worker for the call, and the first part has `patience` seconds to come.
    A worker that stops does not take a call. `read` is the target of a read that a
    tab repeats: the record of that worker then has its answer.
    """
    index, wait, started, looked = 0, 0.03, time.time(), time.time()
    while time.time() < deadline:
        chunk, _ = aws.get(f"relay/{rid}/{index}")
        if chunk is None:
            if index == 0:
                if time.time() - started > patience:
                    return
                if read and time.time() - looked > HOLD_S:
                    looked, found = time.time(), kept(shard, read)
                    if found:
                        yield found, True
                        return
                if wakes:
                    wake(shard)
            time.sleep(wait)
            wait = min(wait * 1.5, 0.25)
            continue
        last = chunk[:1] == wire.LAST
        yield chunk[1:], last
        if last:
            return
        index, wait = index + 1, 0.03


def ask(shard: str, method: str, path: str, body: bytes) -> Answer:
    """Send a call of the Software factory API to the worker of another shard and return its answer."""
    rid, now = str(uuid.uuid4()), time.time()
    message = {"id": rid, "method": method, "path": path, "deadline": now + ANSWER_S, "answer_by": now + ANSWER_S}
    send(shard, {**message, "headers": {"content-type": "application/json"}, "body": base64.b64encode(body).decode()})
    first = next(parts(rid, now + ANSWER_S, shard), None)
    if first is None:
        return 504, wire.JSON, b'{"error": "the worker of the run did not answer"}'
    head, data = wire.unframe(first[0])
    return head["status"], head["headers"], data


class Inbox:
    """The calls that wait for the worker of one shard."""

    def __init__(self, shard: str) -> None:
        """Start without a call."""
        self.shard, self.prefix = shard, f"calls/{shard}/"
        # The keys that this worker gave to its loop, the bell that it heard last and the time of its last list.
        self.seen: set[str] = set()
        self.bell: bytes | None = b""
        self.listed = 0.0

    def receive(self) -> list[dict[str, str]]:
        """Wait about one second for calls. Return each new call as its body and its receipt."""
        if self.shard == MAIN:
            return aws.client("sqs").receive_message(QueueUrl=QUEUE, MaxNumberOfMessages=10, WaitTimeSeconds=1).get("Messages", [])
        end = time.time() + 1
        while True:
            bell, _ = aws.get(f"bells/{self.shard}")
            if bell != self.bell or time.time() - self.listed > INBOX_LIST_S:
                self.bell, self.listed, found = bell, time.time(), []
                for key in (item["Key"] for item in aws.keys(self.prefix)):
                    if key not in self.seen:
                        self.seen.add(key)
                        raw, _ = aws.get(key)
                        if raw:
                            found.append({"Body": raw.decode(), "ReceiptHandle": key})
                if found:
                    return found
            if time.time() + INBOX_S > end:
                return []
            time.sleep(INBOX_S)

    def delete(self, receipt: str) -> None:
        """Remove an answered call."""
        if self.shard == MAIN:
            aws.client("sqs").delete_message(QueueUrl=QUEUE, ReceiptHandle=receipt)
        else:
            aws.delete(receipt)
            self.seen.discard(receipt)
