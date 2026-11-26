"""Record a local MCP call and its cancellation notification."""

import json
import os
import sys
import threading
from pathlib import Path

LOG = Path(sys.argv[1])
LOCK = threading.Lock()
STARTED = threading.Event()
CANCELLED = threading.Event()
PENDING = set()
holds = 0
cancels = 0


def log(event, **fields):
    with LOG.open("a", encoding="utf-8") as stream:
        stream.write(json.dumps({"event": event, **fields}) + "\n")
        stream.flush()
        os.fsync(stream.fileno())


def reply(request_id, result):
    print(
        json.dumps({"jsonrpc": "2.0", "id": request_id, "result": result}), flush=True
    )


def tool_reply(request_id, value):
    reply(request_id, {"content": [{"type": "text", "text": json.dumps(value)}]})


def observe(request_id, name):
    if name == "started":
        value = STARTED.wait(3)
        with LOCK:
            log("started")
            tool_reply(request_id, value)
    else:
        CANCELLED.wait(3)
        with LOCK:
            value = {"holds": holds, "cancels": cancels}
            log("summary", **value)
            tool_reply(request_id, value)


def receive(message):
    global holds, cancels
    method = message["method"]
    request_id = message.get("id")
    params = message.get("params", {})
    if method == "initialize":
        with LOCK:
            reply(
                request_id,
                {
                    "protocolVersion": params["protocolVersion"],
                    "capabilities": {"tools": {}},
                    "serverInfo": {"name": "harbor_probe", "version": "1"},
                },
            )
    elif method == "tools/list":
        with LOCK:
            reply(
                request_id,
                {
                    "tools": [
                        {
                            "name": name,
                            "description": description,
                            "inputSchema": {"type": "object", "properties": {}},
                        }
                        for name, description in (
                            ("hold", "Start a request that waits for cancellation."),
                            (
                                "started",
                                "Return true when the hold has reached the server.",
                            ),
                            (
                                "summary",
                                "Return the counts of holds and matching cancellations.",
                            ),
                        )
                    ]
                },
            )
    elif method == "tools/call":
        name = params["name"]
        if name == "hold":
            with LOCK:
                holds += 1
                PENDING.add((type(request_id), request_id))
                log("hold")
                STARTED.set()
        elif name in {"started", "summary"}:
            threading.Thread(
                target=observe, args=(request_id, name), daemon=True
            ).start()
    elif method == "notifications/cancelled":
        request = params["requestId"]
        target = (type(request), request)
        with LOCK:
            matched = target in PENDING
            log("cancel", matched=matched)
            if matched:
                PENDING.remove(target)
                cancels += 1
                CANCELLED.set()


for line in sys.stdin:
    receive(json.loads(line))
