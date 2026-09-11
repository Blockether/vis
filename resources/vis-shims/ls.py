# vis sandbox directory-listing shim: ls.
#
# Mapping a tree is the question a model asks most, so it costs a Python call
# inside the block it is already running rather than a wire round trip.
# The walk stays on the HOST (fff: .gitignore/.ignore aware, directories first),
# and failures use the standard host-tool error boundary.


def __vis_install_ls__():
    import json as _json
    import os as _os

    def _as_path(value):
        """`value` as a filesystem string when it is path-like, else None."""
        if isinstance(value, (str, bytes)) or hasattr(value, "__fspath__"):
            return _os.fsdecode(_os.fspath(value))
        return None

    def _as_spec(entry):
        """One request entry: a path-like becomes its string, a dict keeps its options."""
        path = _as_path(entry)
        if path is not None:
            return path
        if isinstance(entry, dict):
            nested = _as_path(entry.get("path"))
            if nested is not None:
                return {**entry, "path": nested}
        return entry

    def _size(nbytes):
        """A file size in at most 4 characters: `812`, `7.2k`, `40k`, `2.1M`."""
        n = float(nbytes or 0)
        if n < 1000:
            return str(int(n))
        for unit in ("k", "M", "G", "T"):
            n /= 1024.0
            if n < 1000:
                return ("%.1f%s" % (n, unit)) if n < 10 else ("%d%s" % (round(n), unit))
        return "%dT" % round(n)

    def _render(entries, prefix, out):
        """Append one tree line per entry, recursing into listed children."""
        last = len(entries) - 1
        for index, entry in enumerate(entries):
            children = entry.get("children")
            if entry.get("type") == "dir":
                label = entry.get("name", "") + "/"
                if children is not None:
                    label += " %d" % len(children)
            else:
                label = entry.get("name", "") + "  " + _size(entry.get("size"))
            out.append(prefix + ("\u2514 " if index == last else "\u251c ") + label)
            if children:
                _render(children, prefix + ("  " if index == last else "\u2502 "), out)

    def _section(path, entries):
        """One directory: the `path  Nd Nf` header, then its tree."""
        home = _os.path.expanduser("~")
        shown = "~" + path[len(home) :] if home and path.startswith(home) else path
        dirs = sum(1 for e in entries if e.get("type") == "dir")
        head = "%s  %dd %df" % (shown, dirs, len(entries) - dirs)
        out = [head if entries else shown + "  empty"]
        _render(entries, "", out)
        return "\n".join(out)

    def ls(paths=".", depth=1, is_hidden=False, *, hidden=None, pattern=None):
        """Map a tree through the host's ignore-aware walk, as a compact STRING.

        ls(dir) returns a ready-to-print tree: a `path  Nd Nf` header, then one
        line per entry with directories first then alphabetical. A directory is
        `name/` (plus its child count once depth expanded it), a file is
        `name  size` with the size in at most four characters (`812`, `7.2k`,
        `2.1M`). Branches are two characters wide, so depth costs little width.
        ls([dir, ...]) renders one such section per directory, in request order,
        separated by a blank line; an entry may be a dict
        {"path": dir, "depth": 2} whose own options override the shared ones. A
        path is a str or any os.PathLike, so pathlib.Path works wherever a
        string does.

        pattern=None leaves the listing unchanged. A string filters file and
        directory basenames with a case-sensitive glob (*, ?, [abc], {a,b}),
        at every requested depth; ancestors of matches remain visible.
        A per-path spec may override pattern, including None to disable it.
        Dotfiles need is_hidden=True (alias: hidden=True). If supplied, hidden
        overrides is_hidden; gitignored entries are never listed.
        Start at a known parent and batch only confirmed directories. A missing,
        protected or non-directory path fails the batch with a host tool error.
        A missing path names its nearest existing parent; read files with cat.
        """
        if hidden is not None:
            is_hidden = hidden
        bridge = globals().get("__vis_list_directories__")
        if bridge is None:
            raise RuntimeError("ls: listing bridge not bound in this sandbox")
        one = _as_path(paths) is not None
        request = [paths] if one else list(paths)
        payload = bridge(
            _json.dumps(
                {
                    "paths": [_as_spec(entry) for entry in request],
                    "depth": int(depth),
                    "is_hidden": bool(is_hidden),
                    "pattern": pattern,
                }
            )
        )
        rows = _json.loads(str(payload))
        if one:
            return _section(rows[0]["path"], rows[0]["entries"])
        return "\n\n".join(_section(r["path"], r["entries"]) for r in rows)

    g = globals()
    g["ls"] = ls

    docs = g.setdefault("__vis_docs__", {})
    docs["ls"] = (
        "ls(paths='.', depth=1, is_hidden=False, *, hidden=None, pattern=None): directory contents from the "
        "host's ignore-aware walk, rendered as a compact printable STRING. "
        "ls(dir) -> a `path  Nd Nf` header then one tree line per entry, "
        "directories first then alphabetical: a directory is `name/` (with its "
        "child count once depth expanded it), a file is `name  size` "
        "(`812`, `7.2k`, `2.1M`); ls([dir, ...]) -> one such section per "
        "directory in request order, blank-line separated. Optional pattern=None "
        "leaves the listing unchanged; a string is a case-sensitive basename glob "
        "(*, ?, [abc], {a,b}), not a regex. Applied at every requested depth, keeping "
        "ancestors of matches. Per-path specs override pattern (None disables it). "
        "Example: ls(dir, pattern='*snapshot*'). Dotfiles need "
        "is_hidden=True (alias: hidden=True). When not None, hidden overrides "
        "is_hidden; gitignored entries are never listed. Start at a known "
        "parent; batch only confirmed directories. One missing, protected or "
        "non-directory path fails the batch with a host tool error; a missing path "
        "names the nearest existing directory. Read files with cat. A path is a "
        "str or a pathlib.Path."
    )

    # ONE text for one handle: `help(ls)` and `doc("ls")` read the same
    # string, so neither can go stale against the other.
    ls.__doc__ = docs["ls"]


__vis_install_ls__()
del __vis_install_ls__
