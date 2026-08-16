# vis sandbox directory-listing shim: ls.
#
# Mapping a tree is the question a model asks most, so it costs a Python call
# inside the block it is already running rather than a wire round trip.
# The walk stays on the HOST (fff: .gitignore/.ignore aware, directories first),
# and errors cross the boundary as DATA - a kind the shim turns into the real
# Python exception, since GraalPy does not route host exceptions through except.


def __vis_install_ls__():
    import json as _json

    __vis_ls_errors = {
        "denied": PermissionError,
        "missing": FileNotFoundError,
        "file": NotADirectoryError,
        "args": ValueError,
    }

    def ls(paths=".", depth=1, is_hidden=False):
        """List directory contents through the host's ignore-aware walk.

        ls(dir) returns that ONE directory's entries, directories first then
        alphabetical: [{"name", "path", "type", "size"}], with nested rows under
        "children" when depth > 1. ls([dir, ...]) returns one
        {"path", "entries"} row per directory, in request order; an entry may be
        a dict {"path": dir, "depth": 2} whose own options override the shared
        ones.

        Dotfiles need is_hidden=True; gitignored entries are never listed. A
        file raises NotADirectoryError (read it with cat), a path that does not
        exist raises FileNotFoundError naming the nearest existing directory,
        and a path an extension protects raises PermissionError.
        """
        bridge = globals().get("__vis_list_directories__")
        if bridge is None:
            raise RuntimeError("ls: listing bridge not bound in this sandbox")
        one = isinstance(paths, (str, bytes))
        request = [paths] if one else list(paths)
        env = bridge(
            _json.dumps(
                {
                    "paths": request,
                    "depth": int(depth),
                    "is_hidden": bool(is_hidden),
                }
            )
        )
        if not env[0]:
            raise __vis_ls_errors.get(env[2], RuntimeError)(str(env[1]))
        rows = _json.loads(str(env[1]))
        return rows[0]["entries"] if one else rows

    g = globals()
    g["ls"] = ls

    docs = g.setdefault("__vis_docs__", {})
    docs["ls"] = (
        "ls(paths='.', depth=1, is_hidden=False): directory contents from the "
        "host's ignore-aware walk. ls(dir) -> that directory's entries "
        "[{name, path, type, size}], directories first then alphabetical, "
        "nested rows under children when depth > 1; ls([dir, ...]) -> one "
        "{path, entries} row per directory in request order. Dotfiles need "
        "is_hidden=True and gitignored entries are never listed. Raises "
        "NotADirectoryError for a file, FileNotFoundError naming the nearest "
        "existing directory, PermissionError when an extension protects it."
    )


__vis_install_ls__()
del __vis_install_ls__
