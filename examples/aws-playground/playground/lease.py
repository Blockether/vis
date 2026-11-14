"""The lease: one S3 object that names the one live worker and its last checkpoint.

Every write is conditional on the ETag that this worker read. A second worker that
starts by mistake loses the first conditional write and stops. This makes S3 a
database with one writer.
"""

from __future__ import annotations

import json
import time
import uuid
from typing import Any

from playground import aws
from playground.env import LEASE, LEASE_TTL


class Lease:
    """The lease of this worker. `data` is the last content that it wrote."""

    def __init__(self, data: dict[str, Any], etag: str | None) -> None:
        self.id = str(uuid.uuid4())
        self.data, self.etag, self.saves = data, etag, 0

    def write(self, **fields: Any) -> None:
        """Write the lease with new fields. A lost lease raises ClientError."""
        data = {**self.data, **fields, "owner": self.id}
        condition = {"IfMatch": self.etag} if self.etag else {"IfNoneMatch": "*"}
        self.etag = aws.put(LEASE, json.dumps(data).encode(), **condition)
        self.data = data

    def renew(self, **fields: Any) -> None:
        """Keep the lease for LEASE_TTL more seconds."""
        self.write(expires=time.time() + LEASE_TTL, **fields)

    def release(self, **fields: Any) -> None:
        """Give the lease up, so the next worker starts without a wait."""
        self.write(expires=0, **fields)

    @classmethod
    def acquire(cls) -> Lease | None:
        """Take the lease, or return None when another worker holds it."""
        data, etag = aws.lease()
        if aws.alive(data):
            return None
        held = cls({k: v for k, v in data.items() if k != "owner"}, etag)
        try:
            held.renew()
        except aws.ClientError:
            return None
        return held
