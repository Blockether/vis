"""Discovery for the sandbox: `apropos(pattern)` and `doc(target)`.

The corpus is the HOST's - every documentation page, every skill and every
tool's contract lives there, and is read on the call that asks so an edit made
mid-session answers with what it says now. What only THIS side knows is what
the session can actually reach: which globals are callable, which modules were
published, what its own `def`s say, and the prose that lives on a live object.

So the facts are gathered here and merged there, in one call. It is one call
because a host tool runs while the block waits inside it and cannot ask the
interpreter anything: whatever the host needs has to travel with the question.
"""

import json

import vis_runtime

#: Names that are globals but not tools: the async runtime a block imports, and
#: Python's own builtins, which a TOOL-discovery surface must never list.
NON_TOOLS = frozenset({"asyncio"})


def _ask(namespace, tool, args):
    """Call the host tool `tool` with `args`, answering the value it replied with.

    The boundary carries TEXT and one envelope shape: the session is named
    because the host binds a tool per session, and a host failure comes back as
    a `RuntimeError` the caller can catch instead of a reply nobody reads.
    """
    payload = json.dumps(
        {"session": namespace.get("__vis_session__"), "args": args}, default=str
    )
    reply = json.loads(vis_runtime.host_call(tool, payload))
    if "error" in reply:
        raise RuntimeError(reply["error"])
    return reply.get("value")


def _tables(namespace):
    """The metadata tables the engine seeded, as plain dicts."""
    out = {}
    for key in ("docs", "calls", "sigs", "kinds", "keys"):
        table = namespace.get(f"__vis_{key}__")
        out[key] = dict(table) if isinstance(table, dict) else {}
    return out


def _called(namespace, name):
    """The dict a zero-argument runtime helper answers, `{}` when it cannot."""
    fn = namespace.get(name)
    if not callable(fn):
        return {}
    try:
        value = fn()
    except BaseException:
        return {}
    return dict(value) if isinstance(value, dict) else {}


def _names(namespace):
    """Every callable this session can reach, plus the modules it published."""
    builtins_mod = namespace.get("__builtins__")
    builtin_names = set(dir(builtins_mod)) if builtins_mod is not None else set()
    found = set()
    for name, value in list(namespace.items()):
        if name.startswith("_") or name in builtin_names or name in NON_TOOLS:
            continue
        if callable(value):
            found.add(name)
    published = namespace.get("__vis_shims__")
    if isinstance(published, (list, tuple, set)):
        found.update(str(n) for n in published)
    return sorted(found)


def _facts(namespace):
    facts = _tables(namespace)
    facts["names"] = _names(namespace)
    facts["def_docs"] = _called(namespace, "__vis_def_docs__")
    facts["def_calls"] = _called(namespace, "__vis_def_calls__")
    return facts


def _live_doc(namespace, target):
    """The prose on the OBJECT itself, which the host has no way to read."""
    reader = namespace.get("__vis_dotted_doc__")
    if not callable(reader) or not target:
        return ""
    try:
        return reader(str(target)) or ""
    except BaseException:
        return ""


def install(namespace):
    """Bind `apropos` and `doc` into `namespace`, closing over it.

    A module's own globals are not the session's, so the namespace is handed in
    rather than looked up: one interpreter holds many sessions and each answers
    for what IT can reach.
    """

    def apropos(pattern=""):
        rows = _ask(
            namespace,
            "__vis_apropos__",
            [_facts(namespace), "" if pattern is None else str(pattern)],
        )
        item = namespace.get("__vis_AproposItem__")
        if item is None:
            return rows
        return [item(row["kind"], row["name"], row["body"]) for row in rows]

    def doc(target=None):
        name = getattr(target, "name", None)
        if name is None:
            name = getattr(target, "__name__", target)
        name = "" if name is None else str(name)
        return _ask(
            namespace,
            "__vis_doc__",
            [_facts(namespace), name, _live_doc(namespace, name)],
        )

    namespace["apropos"] = apropos
    namespace["doc"] = doc
    return ["apropos", "doc"]
