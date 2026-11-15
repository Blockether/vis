"""The wire format between the front function and the worker.

A call goes to the worker as one SQS message. The answer comes back as S3 objects
`relay/<id>/<index>`. The first byte of each object is `E` for the last part or `M`
when more parts follow. Part 0 starts with a frame: the length of a JSON head in 4 bytes,
the head `{status, headers}`, then the first bytes of the body.
"""

from __future__ import annotations

import json
import struct
from typing import Any

HOP = {"connection", "content-length", "transfer-encoding", "host", "keep-alive"}
LAST, MORE = b"E", b"M"
# The headers of a JSON answer, as pairs of name and value.
JSON = [["content-type", "application/json"]]


def frame(head: dict[str, Any], body: bytes = b"") -> bytes:
    """Join a JSON head and the first bytes of a body."""
    raw = json.dumps(head).encode()
    return struct.pack(">I", len(raw)) + raw + body


def unframe(data: bytes) -> tuple[dict[str, Any], bytes]:
    """Split a frame into its head and the body bytes."""
    size = struct.unpack(">I", data[:4])[0]
    return json.loads(data[4 : 4 + size]), data[4 + size :]
