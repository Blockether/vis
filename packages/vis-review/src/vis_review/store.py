"""File contents by their SHA-256: one compressed blob for each distinct content.

Every root and every journal shares one store, so a file that many checkpoints
keep costs one blob. A blob is written to a temporary file first and then moved
into place, so a reader never sees half a blob.
"""

from __future__ import annotations

import contextlib
import hashlib
import os
import tempfile
import zlib
from pathlib import Path

# zlib level 1: about 4x smaller for source text at a fraction of the cost of level 6.
LEVEL = 1
# A larger file keeps only its digest: the journal shows that it changed, not how.
MOST_BYTES = 8 * 1024 * 1024


def digest(data: bytes) -> str:
    """The SHA-256 of `data` as hex."""
    return hashlib.sha256(data).hexdigest()


class Store:
    """Blobs under `<base>/objects/<first two hex digits>/<rest>`."""

    def __init__(self, base):
        self.objects = Path(base) / "objects"

    def _path(self, sha: str) -> Path:
        return self.objects / sha[:2] / sha[2:]

    def put(self, data: bytes) -> str:
        """Keep `data` and answer its digest; a known or too large content writes nothing."""
        sha = digest(data)
        if len(data) > MOST_BYTES:
            return sha
        path = self._path(sha)
        if path.exists():
            return sha
        path.parent.mkdir(parents=True, exist_ok=True)
        fd, temporary = tempfile.mkstemp(dir=path.parent, prefix=".tmp-")
        try:
            with os.fdopen(fd, "wb") as out:
                out.write(zlib.compress(data, LEVEL))
            os.replace(temporary, path)
        except BaseException:
            with contextlib.suppress(OSError):
                os.unlink(temporary)
            raise
        return sha

    def get(self, sha: str) -> bytes | None:
        """The content with digest `sha`, or None when the store does not keep it."""
        try:
            return zlib.decompress(self._path(sha).read_bytes())
        except FileNotFoundError:
            return None
