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

    # Indentation, not box-drawing branches: this tree is read by a MODEL, where a
    # branch glyph costs a token of its own and says nothing the two spaces of
    # indent do not already say.
    def _render(entries, prefix, out):
        """Append one tree line per entry, indenting children two spaces per level."""
        for entry in entries:
            children = entry.get("children")
            if entry.get("type") == "dir":
                label = entry.get("name", "") + "/"
                if children is not None:
                    label += " %d" % len(children)
            else:
                label = entry.get("name", "") + " " + _size(entry.get("size"))
            out.append(prefix + label)
            if children:
                _render(children, prefix + "  ", out)

    def _paths(root, entries, out):
        """Append every entry as a full path; a directory keeps a trailing `/`."""
        for entry in entries:
            full = _os.path.join(root, entry.get("name", ""))
            if entry.get("type") == "dir":
                out.append(full + "/")
                children = entry.get("children")
                if children:
                    _paths(full, children, out)
            else:
                out.append(full)

    def _section(path, entries):
        """One directory: the `path  Nd Nf` header, then its tree."""
        home = _os.path.expanduser("~")
        shown = "~" + path[len(home) :] if home and path.startswith(home) else path
        dirs = sum(1 for e in entries if e.get("type") == "dir")
        head = "%s  %dd %df" % (shown, dirs, len(entries) - dirs)
        out = [head if entries else shown + "  empty"]
        _render(entries, "", out)
        return "\n".join(out)

    def _decoded(payload):
        """Listing rows from the bridge; a non-JSON answer names the boundary."""
        text = str(payload)
        try:
            return _json.loads(text)
        except ValueError as exc:
            # A bare JSONDecodeError would point at the caller's own `ls(...)`
            # line and throw the answer away, so the boundary names itself.
            raise RuntimeError(
                "ls: the listing bridge answered %s, not JSON (%s): %.200r"
                % (type(payload).__name__, exc, text)
            ) from exc

    def ls(
        paths=".",
        depth=1,
        is_hidden=False,
        *,
        hidden=None,
        pattern=None,
        as_paths=False,
    ):
        """List directories as a compact tree STRING. `docs["ls"]` below replaces this text."""
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
        rows = _decoded(payload)
        if as_paths:
            flat = []
            for row in rows:
                _paths(row["path"], row["entries"], flat)
            return flat
        if one:
            return _section(rows[0]["path"], rows[0]["entries"])
        return "\n\n".join(_section(r["path"], r["entries"]) for r in rows)

    g = globals()
    g["ls"] = ls

    docs = g.setdefault("__vis_docs__", {})
    docs["ls"] = (
        "ls(paths='.', depth=1, is_hidden=False, *, hidden=None, pattern=None, "
        "as_paths=False) lists directory contents from the ignore-aware walk of the "
        "host, as a compact printable STRING. `ls(dir)` gives a `path  Nd Nf` header, "
        "then one tree line per entry, with directories first and then in alphabetical "
        "order. A directory is `name/`, with its child count when depth expanded it. A "
        "file is `name size`, for example `812`, `7.2k` or `2.1M`. Children indent two "
        "spaces per level."
        "\n\n`ls([dir, ...])` gives one such section per directory, in request order, "
        "separated by a blank line. A path is a str or a pathlib.Path. A batch entry "
        'can also be a per-path spec, such as `{"path": dir, "depth": 2}`, whose '
        "options override the shared ones."
        "\n\nStart at a known parent, and batch only confirmed directories. One "
        "missing, protected or non-directory path fails the batch with a host tool "
        "error. A missing path names the nearest existing directory. Read files with "
        "cat."
        "\n\npattern=None leaves the listing unchanged. A string is a case-sensitive "
        "basename glob (*, ?, [abc], {a,b}), not a regex. It applies at every "
        "requested depth and keeps the ancestors of matches. A per-path spec can "
        "override pattern, and None disables it there. Example: `ls(dir, "
        "pattern='*snapshot*')`."
        "\n\nDotfiles need is_hidden=True (alias: hidden=True). When hidden is not "
        "None, it overrides is_hidden. The listing never includes gitignored entries."
        "\n\nas_paths=True returns the same walk as a flat list of paths instead of "
        "the tree, in the same order. Each path is the requested path joined with the "
        "entry name, and directories end in '/'. So a relative request gives relative "
        "paths. `ls(dir, depth=3, as_paths=True)` is the whole file list, ready for "
        "cat, grep or Path()."
    )

    # ONE text for one handle: `help(ls)` and `doc("ls")` read the same
    # string, so neither can go stale against the other.
    ls.__doc__ = docs["ls"]


__vis_install_ls__()
del __vis_install_ls__
