"""File contents by their SHA-256: one compressed blob for each distinct content.

Every root and every journal shares one store, so a file that many checkpoints
keep costs one blob. A blob is written to a temporary file first and then moved
into place, so a reader never sees half a blob.
"""

from __future__ import annotations

import contextlib
import hashlib
import json
import os
import tempfile
import time
import zlib
from pathlib import Path

# zlib level 1: about 4x smaller for source text at a fraction of the cost of level 6.
LEVEL = 1
# A larger file keeps only its digest: the journal shows that it changed, not how.
MOST_BYTES = 8 * 1024 * 1024
# Seconds for a writer to add a journal reference to a stored object.
GRACE_S = 3600


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
        try:
            # A new checkpoint can refer to old content before its line is written.
            if time.time() - path.stat().st_mtime > GRACE_S / 2:
                os.utime(path, None)
            return sha
        except FileNotFoundError:
            pass
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

    def sweep(self) -> int:
        """Delete old blobs that no journal under this store references."""
        cutoff = time.time() - GRACE_S
        live = set()
        for journal in self.objects.parent.glob("*/*.jsonl"):
            try:
                with journal.open(encoding="utf-8") as source:
                    for line in source:
                        try:
                            live.update(
                                sha
                                for sha in json.loads(line).get("files", {}).values()
                                if isinstance(sha, str)
                            )
                        except (ValueError, AttributeError):
                            continue  # An interrupted append has no complete line.
            except OSError:
                return 0  # No deletion if a journal could not be read.
        removed = 0
        for path in self.objects.glob("[0-9a-f][0-9a-f]/*"):
            if path.parent.name + path.name in live or len(path.name) != 62:
                continue
            try:
                if path.stat().st_mtime >= cutoff:
                    continue
                path.unlink()
                removed += 1
            except OSError:
                continue
        return removed

    def get(self, sha: str) -> bytes | None:
        """The content with digest `sha`, or None when the store does not keep it."""
        try:
            return zlib.decompress(self._path(sha).read_bytes())
        except FileNotFoundError:
            return None
