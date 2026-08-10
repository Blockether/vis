# The scannable `apropos` TABLE rendered by the native dispatch. Capabilities the
# provider schema already advertises are omitted here; in-Python `apropos(query)`
# stays the complete dict.
#
# ONE answer is not a table. With the shell tools OFF the whole `shell` group is
# gone, so a shell-shaped query used to answer "no unadvertised capabilities
# match" — silence the model reads as "no process can be started in this product".
# It can: that toggle closes the MODEL's door only, and a trusted Python extension
# keeps `vis.shell`. The sentences come from `__vis_process_surface__` (the Python
# view of `env_python/PROCESS_SURFACE`), so no wording is spelled twice.


def __vis_apropos_table__(query=""):
    hidden = set(globals().get("__vis_advertised_native_tools__") or ())
    d = {k: v for k, v in apropos(query).items() if k not in hidden}

    def __shell_note():
        # Looked up at CALL time, like the POSIX refusal: the toggle can flip
        # between two blocks of one session. Bound means advertised, and an
        # advertised tool needs no note.
        if globals().get("shell") is not None:
            return None
        q = str(query or "").strip().lower()
        # `apropos`' own rule — a query matches a NAME substring or a whole GROUP,
        # and for the shell tools both are "shell".
        if q and q not in "shell":
            return None
        words = globals().get("__vis_process_surface__") or {}
        if not words:
            return None
        return "### shell\n" + words["off"] + " " + words["extension"]

    note = __shell_note()

    if not d:
        if note:
            return note
        return "apropos(" + repr(query) + "): no unadvertised capabilities match."

    groups = globals().get("__vis_groups__") or {}

    def __cell(s):
        return str(s).replace("\n", " ").replace("|", "\\|")

    by_group = {}
    for k in d:
        by_group.setdefault(str(groups.get(k, "engine")), []).append(k)

    out = []
    for g in sorted(by_group):
        out.append("### " + __cell(g))
        out.append("| capability | gist |")
        out.append("| --- | --- |")
        for k in by_group[g]:
            out.append("| `" + __cell(k) + "` | " + __cell(d[k]) + " |")
        out.append("")
    if note:
        out.append(note)
        out.append("")
    out.append(
        "Groups: " + ", ".join("`" + __cell(g) + "`" for g in sorted(by_group)) + "."
    )
    out.append(
        'One group at a time: `apropos("providers")`. One contract: `doc(name)`.'
    )
    return "\n".join(out)
