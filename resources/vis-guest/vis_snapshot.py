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
import types

#: The types a spill file can hold. A spill file is raw data, never a pickle.
KINDS = {bytes: "bytes", str: "str"}

#: Snapshot line that names every value the snapshot could not save.
UNSAVED_PREFIX = "\n__vis_unsaved__ = "

#: Instructions that read or write a module global by name.
GLOBAL_OPS = frozenset({"LOAD_GLOBAL", "LOAD_NAME", "STORE_GLOBAL", "DELETE_GLOBAL"})

#: `name -> ((id, length, hash), digest)`, so an unchanged value costs no new digest.
_digests = {}


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


def snapshot(namespace, limit, total):
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
    return {"text": text, "spill": spill}


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
