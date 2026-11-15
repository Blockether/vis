"""The front side of the relay: a call goes to SQS and its answer comes back as S3 objects (see `wire.py`).

The worker side is `vis/relay.py`.
"""

from __future__ import annotations

import base64
import json
import time
import uuid
from collections.abc import Iterator
from typing import Any

from durable import aws, wire
from durable.dashboard.web import RELAY_POLICY, RELAYED, SECURITY, Request, error
from durable.helpers import log
from durable.lambda_runtime import Context, HttpStream
from durable.settings import ANSWER_S, LIFETIME, MESSAGE_LIMIT, QUEUE, STREAM_S, WAKE_S, WORKER_FUNCTION

# The time of the last worker start.
_state = {"woke": 0.0}


def wake() -> None:
    """Start a worker when no live worker holds the lease, at most each WAKE_S."""
    if time.time() - _state["woke"] < WAKE_S:
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
    if len(request.body) > MESSAGE_LIMIT:
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
        """Yield the first bytes, then each later part of the answer."""
        yield data
        for chunk, _ in source:
            yield chunk

    return HttpStream(status=status, headers=headers, chunks=body())
