"""Restart support beside the runtime's session snapshot.

The runtime snapshot keeps saved values to a fixed pickle budget, and it names
each value that does not fit. The host can keep large text and bytes values in
spill files beside the snapshot, so these functions find them, hand their data
to the host and load them back. After a restore, `dependents` names the restored
helpers and classes that read a name the restore lost.

Only the host calls these functions. A value never leaves the sandbox unless the
snapshot already dropped it for its size.
"""

import ast
import base64
import dis
import hashlib
import os
import sys
import types
import zlib

#: The types a spill file can hold. A spill file is raw data, never a pickle.
KINDS = {bytes: "bytes", str: "str"}

#: Snapshot line that names every value the snapshot could not save.
UNSAVED_PREFIX = "\n__vis_unsaved__ = "

#: Instructions that read or write a module global by name.
GLOBAL_OPS = frozenset({"LOAD_GLOBAL", "LOAD_NAME", "STORE_GLOBAL", "DELETE_GLOBAL"})

#: `name -> ((id, length, hash), digest)`, so an unchanged value costs no new digest.
_digests = {}

#: Snapshot line that holds the saved values; the deflate pieces split after it.
VALUES_PREFIX = "\n__vis_values__ = "

#: Text between two saved values. A value piece ends after its closing quote.
VALUE_GAP = "', '"

#: Characters in one value piece at least, so a piece costs more than its lookup.
PIECE_CHARS = 16384

#: `first 48 chars -> [piece, start, deflate run]` for each piece of the last large
#: snapshot text, at its place in that text. The run is None until a call needs it.
_pieces = {}

#: `[id, narrow]` of the last large snapshot text. The host can hold that text as a
#: base, and a later snapshot then sends only what changed.
_base = [None, False]


def _unsaved(text):
    """The `{name: reason}` map at the end of snapshot `text`, `{}` when it has none."""
    at = text.rfind(UNSAVED_PREFIX)
    if at < 0:
        return {}
    line = text[at + len(UNSAVED_PREFIX) :].split("\n", 1)[0]
    try:
        value = ast.literal_eval(line)
    except Exception:
        return {}
    return value if isinstance(value, dict) else {}


def _data(value):
    """The raw spill bytes of `value`. Text keeps lone surrogates."""
    if type(value) is str:
        return value.encode("utf-8", "surrogatepass")
    return value


def _digest(name, value):
    key = (id(value), len(value), hash(value))
    known = _digests.get(name)
    if known is not None and known[0] == key:
        return known[1]
    digest = hashlib.blake2b(_data(value), digest_size=16).hexdigest()
    _digests[name] = (key, digest)
    return digest


def _adler_combine(a1, a2, len2):
    """zlib's `adler32_combine`: the checksum of A+B from adler(A), adler(B) and len(B)."""
    base = 65521
    rem = len2 % base
    s1 = a1 & 0xFFFF
    s2 = (rem * s1) % base
    s1 += (a2 & 0xFFFF) + base - 1
    s2 += ((a1 >> 16) & 0xFFFF) + ((a2 >> 16) & 0xFFFF) + base - rem
    if s1 >= base:
        s1 -= base
    if s1 >= base:
        s1 -= base
    if s2 >= base << 1:
        s2 -= base << 1
    if s2 >= base:
        s2 -= base
    return s1 | (s2 << 16)


def _narrow(text):
    """Is each character of `text` one UTF-16 unit, so the host indexes it the same way?

    CPython keeps a text that has a character past U+FFFF at 4 bytes for each
    character. A cached UTF-8 copy can only make the answer False, never True.
    """
    return sys.getsizeof(text) < 4 * len(text)


def _split(text):
    """The pieces of `text` as `(entry, start in the last text or -1)`.

    After a block most of the snapshot is unchanged. A piece of the last text that
    starts at the same 48 characters and still matches is used again, with its
    deflate run. The pieces of `text` replace the pieces of the last text.
    """
    out = []
    used = {}
    n = len(text)
    head = text.find(VALUES_PREFIX)
    head = n if head < 0 else head + len(VALUES_PREFIX)
    # The values end before the `}` that closes them, so a value added at the end
    # leaves the piece of the last value unchanged.
    tail = text.rfind(UNSAVED_PREFIX)
    tail = n if tail < head else tail - (text[tail - 1 : tail] == "}")
    pos = 0
    while pos < n:
        key = text[pos : pos + 48]
        known = _pieces.get(key)
        if known is not None and text.startswith(known[0], pos):
            entry = [known[0], pos, known[2]]
            old = known[1]
        else:
            if pos < head:
                end = head
            elif pos < tail:
                end = text.find(VALUE_GAP, pos + PIECE_CHARS, tail)
                end = tail if end < 0 else end + 1
            else:
                end = n
            entry = [text[pos:end], pos, None]
            old = -1
        used[key] = entry
        out.append((entry, old))
        pos += len(entry[0])
    _pieces.clear()
    _pieces.update(used)
    return out


def _deflate(pieces):
    """A zlib stream of the joined pieces.

    Each piece is a separate raw deflate run that ends on a byte boundary, so the
    runs join into one valid stream, and an unchanged piece costs no new run.
    """
    adler = 1
    parts = [b"\x78\x01"]
    for entry, _old in pieces:
        run = entry[2]
        if run is None:
            data = entry[0].encode("utf-8")
            packer = zlib.compressobj(1, zlib.DEFLATED, -15)
            run = entry[2] = (
                packer.compress(data) + packer.flush(zlib.Z_SYNC_FLUSH),
                zlib.adler32(data),
                len(data),
            )
        adler = _adler_combine(adler, run[1], run[2])
        parts.append(run[0])
    parts.append(b"\x03\x00")
    parts.append(adler.to_bytes(4, "big"))
    return b"".join(parts)


def _delta(pieces):
    """The pieces as delta ops: `[start, end]` copies that part of the last text, and a
    string is new text."""
    ops = []
    for entry, old in pieces:
        if old < 0:
            if ops and type(ops[-1]) is str:
                ops[-1] += entry[0]
            else:
                ops.append(entry[0])
        elif ops and type(ops[-1]) is list and ops[-1][1] == old:
            ops[-1][1] = old + len(entry[0])
        else:
            ops.append([old, old + len(entry[0])])
    return ops


def snapshot(namespace, limit, total, base=None):
    """The runtime snapshot text and the values the host can spill.

    A candidate is a `bytes` or `str` global that the snapshot did not save. One
    candidate holds at most `limit` bytes, and all of them at most `total`.
    """
    text = namespace["__vis_defs_snapshot__"]()
    spill = []
    room = total
    for name in sorted(_unsaved(text) if isinstance(text, str) else {}):
        value = namespace.get(name)
        kind = KINDS.get(type(value))
        if kind is None:
            continue
        size = len(_data(value)) if kind == "str" else len(value)
        if size > limit or size > room:
            continue
        room -= size
        spill.append(
            {"name": name, "kind": kind, "size": size, "digest": _digest(name, value)}
        )
    for name in set(_digests) - {item["name"] for item in spill}:
        del _digests[name]
    # Large snapshots cross JSON after every block. When the host still holds the
    # last text as `base`, send only what changed; else compress the whole text.
    if not isinstance(text, str) or len(text) < 65536:
        _pieces.clear()
        _base[:] = [None, False]
        return {"text": text, "spill": spill}
    pieces = _split(text)
    held, held_narrow = _base
    narrow = _narrow(text)
    _base[:] = [os.urandom(8).hex(), narrow]
    if base is not None and base == held and held_narrow and narrow:
        return {
            "text-delta": _delta(pieces),
            "length": len(text),
            "id": _base[0],
            "spill": spill,
        }
    packed = base64.b64encode(_deflate(pieces)).decode("ascii")
    if len(packed) < len(text):
        return {"text-zlib": packed, "id": _base[0], "spill": spill}
    return {"text": text, "id": _base[0], "spill": spill}


def spill_data(namespace, name):
    """The spill bytes of global `name` as base64 text."""
    return base64.b64encode(_data(namespace[name])).decode("ascii")


def unspill(namespace, name, kind, data):
    """Bind global `name` to the spilled value in base64 `data`. Answers True."""
    raw = base64.b64decode(data)
    namespace[name] = raw.decode("utf-8", "surrogatepass") if kind == "str" else raw
    return True


def _codes(value):
    """The code objects of a function, or of the methods and properties of a class."""
    parts = list(vars(value).values()) if isinstance(value, type) else [value]
    out = []
    for part in parts:
        if isinstance(part, (staticmethod, classmethod)):
            part = part.__func__
        if isinstance(part, property):
            out.extend(
                f.__code__ for f in (part.fget, part.fset, part.fdel) if f is not None
            )
            continue
        code = getattr(part, "__code__", None)
        if isinstance(code, types.CodeType):
            out.append(code)
    return out


def _globals_read(code):
    """The global names that `code` and its nested code objects use."""
    found = set()
    stack = [code]
    while stack:
        current = stack.pop()
        for ins in dis.get_instructions(current):
            if ins.opname in GLOBAL_OPS and isinstance(ins.argval, str):
                found.add(ins.argval)
        stack.extend(c for c in current.co_consts if isinstance(c, types.CodeType))
    return found


def dependents(namespace, restored, lost):
    """`{restored name: [lost names]}` for restored helpers and classes that use a lost name.

    A helper that calls another dependent helper depends on its lost names too.
    """
    lost = set(lost)
    uses = {}
    for name in restored:
        value = namespace.get(name)
        if value is None:
            continue
        try:
            names = set()
            for code in _codes(value):
                names |= _globals_read(code)
        except Exception:
            continue
        uses[name] = names
    found = {name: names & lost for name, names in uses.items()}
    changed = True
    while changed:
        changed = False
        for name, names in uses.items():
            more = set().union(*(found[n] for n in names if n in found and n != name))
            if not more <= found[name]:
                found[name] |= more
                changed = True
    return {name: sorted(names) for name, names in sorted(found.items()) if names}
