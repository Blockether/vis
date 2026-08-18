# vis sandbox directory-listing shim: ls.
#
# Mapping a tree is the question a model asks most, so it costs a Python call
# inside the block it is already running rather than a wire round trip.
# The walk stays on the HOST (fff: .gitignore/.ignore aware, directories first),
# and errors cross the boundary as DATA - a kind the shim turns into the real
# Python exception, since GraalPy does not route host exceptions through except.


def __vis_install_ls__():
    import json as _json
    import os as _os

    __vis_ls_errors = {
        "denied": PermissionError,
        "missing": FileNotFoundError,
        "file": NotADirectoryError,
        "args": ValueError,
    }

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
        shown = "~" + path[len(home):] if home and path.startswith(home) else path
        dirs = sum(1 for e in entries if e.get("type") == "dir")
        head = "%s  %dd %df" % (shown, dirs, len(entries) - dirs)
        out = [head if entries else shown + "  empty"]
        _render(entries, "", out)
        return "\n".join(out)

    def ls(paths=".", depth=1, is_hidden=False):
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

        Dotfiles need is_hidden=True; gitignored entries are never listed. A
        file raises NotADirectoryError (read it with cat), a path that does not
        exist raises FileNotFoundError naming the nearest existing directory,
        and a path an extension protects raises PermissionError.
        """
        bridge = globals().get("__vis_list_directories__")
        if bridge is None:
            raise RuntimeError("ls: listing bridge not bound in this sandbox")
        one = _as_path(paths) is not None
        request = [paths] if one else list(paths)
        env = bridge(
            _json.dumps(
                {
                    "paths": [_as_spec(entry) for entry in request],
                    "depth": int(depth),
                    "is_hidden": bool(is_hidden),
                }
            )
        )
        if not env[0]:
            raise __vis_ls_errors.get(env[2], RuntimeError)(str(env[1]))
        rows = _json.loads(str(env[1]))
        if one:
            return _section(rows[0]["path"], rows[0]["entries"])
        return "\n\n".join(_section(r["path"], r["entries"]) for r in rows)

    g = globals()
    g["ls"] = ls

    docs = g.setdefault("__vis_docs__", {})
    docs["ls"] = (
        "ls(paths='.', depth=1, is_hidden=False): directory contents from the "
        "host's ignore-aware walk, rendered as a compact printable STRING. "
        "ls(dir) -> a `path  Nd Nf` header then one tree line per entry, "
        "directories first then alphabetical: a directory is `name/` (with its "
        "child count once depth expanded it), a file is `name  size` "
        "(`812`, `7.2k`, `2.1M`); ls([dir, ...]) -> one such section per "
        "directory in request order, blank-line separated. Dotfiles need "
        "is_hidden=True and gitignored entries are never listed. Raises "
        "NotADirectoryError for a file, FileNotFoundError naming the nearest "
        "existing directory, PermissionError when an extension protects it. A "
        "path is a str or a pathlib.Path."
    )


__vis_install_ls__()
del __vis_install_ls__
