"""Compile host-supplied Python source once for each worker process.

The host sends the SDK body and extension source as text, not as files. The
bytecode cache of the import system does not cover text, so each extension
context compiled the same source again (#407). `code` keeps each compiled
source in memory for the process. It also keeps the marshalled code under
`sys.pycache_prefix`, so a new worker loads it without a compile.
"""

import hashlib
import importlib.util
import marshal
import os
import sys

_CODE = {}


def _cache_file(digest):
    """The file for one compiled source, or None without a bytecode cache."""
    prefix = sys.pycache_prefix
    return os.path.join(prefix, "vis-code", digest + ".bin") if prefix else None


def _load(path):
    try:
        with open(path, "rb") as handle:
            return marshal.loads(handle.read())
    except Exception:
        return None


def _store(path, compiled):
    """Write `compiled` atomically. A failed write costs only speed."""
    temporary = f"{path}.{os.getpid()}.{os.urandom(4).hex()}.tmp"
    try:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(temporary, "wb") as handle:
            handle.write(marshal.dumps(compiled))
        os.replace(temporary, path)
    except Exception:
        try:
            os.unlink(temporary)
        except Exception:
            pass


def code(source, filename, flags=0):
    """The code object for `source`, as `compile(source, filename, "exec", flags)` gives it."""
    digest = hashlib.sha256(
        b"\0".join(
            (
                importlib.util.MAGIC_NUMBER,
                str(flags).encode(),
                filename.encode("utf-8", "surrogatepass"),
                source.encode("utf-8", "surrogatepass"),
            )
        )
    ).hexdigest()
    compiled = _CODE.get(digest)
    if compiled is None:
        path = _cache_file(digest)
        compiled = _load(path) if path else None
        if compiled is None:
            compiled = compile(source, filename, "exec", flags=flags, dont_inherit=True)
            if path:
                _store(path, compiled)
        _CODE[digest] = compiled
    return compiled
