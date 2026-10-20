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

    # The tree is read by a MODEL, so every line and every word costs tokens. One
    # line per entry spent a newline and an indent on each file, and its size was a
    # quarter of the text. A directory's files therefore share ONE line, and a size
    # appears only where it changes the next step: a file too large to read whole.
    _BIG = 64 * 1024

    def _name(entry):
        """The entry name, quoted when whitespace would split it on a shared line."""
        name = entry.get("name", "")
        if not name or name[0] in '"+' or any(c.isspace() for c in name):
            return _json.dumps(name, ensure_ascii=False)
        return name

    def _label(entry):
        """One entry on a shared line: `dir/ N`, `dir/`, `file` or `file 120k`."""
        if entry.get("type") == "dir":
            children = entry.get("children")
            count = "" if children is None else " %d" % len(children)
            return _name(entry) + "/" + count
        size = entry.get("size") or 0
        return _name(entry) + (" " + _size(size) if size >= _BIG else "")

    def _kinds(entries):
        """What a `+K more` hides: `(28 .md, 1 .edn, 2 dirs)`, the four largest kinds."""
        kinds = {}
        for entry in entries:
            if entry.get("type") == "dir":
                kind = "dirs"
            else:
                stem, dot, ext = entry.get("name", "").rpartition(".")
                kind = "." + ext if dot and stem else "other"
            kinds[kind] = kinds.get(kind, 0) + 1
        ranked = sorted(kinds.items(), key=lambda kv: (-kv[1], kv[0]))
        text = ", ".join(
            "%d %s" % (n, "dir" if kind == "dirs" and n == 1 else kind)
            for kind, n in ranked[:4]
        )
        return " (%s%s)" % (text, ", …" if len(ranked) > 4 else "")

    def _breadth_order(entries):
        """Number every entry level by level, the order in which pages fill."""
        order = {}
        level = list(entries)
        while level:
            deeper = []
            for entry in level:
                order[id(entry)] = len(order)
                deeper.extend(entry.get("children") or ())
            level = deeper
        return order

    def _mark_open(entries, lo, hi, order, opened):
        """Collect directories with a descendant on the page; answer if any entry is shown."""
        shown = False
        for entry in entries:
            children = entry.get("children")
            if children and _mark_open(children, lo, hi, order, opened):
                opened.add(id(entry))
                shown = True
            elif lo <= order[id(entry)] < hi:
                shown = True
        return shown

    def _render(entries, lo, hi, order, opened, prefix, out):
        """Append one directory of the page: its shared line, then each open subdirectory."""
        shared, later, deeper = [], [], []
        for entry in entries:
            if id(entry) in opened:
                deeper.append(entry)
            elif order[id(entry)] >= hi:
                later.append(entry)
            elif order[id(entry)] >= lo:
                shared.append(_label(entry))
        if later:
            shared.append("+%d more%s" % (len(later), _kinds(later)))
        if shared:
            out.append(prefix + "  ".join(shared))
        for entry in deeper:
            out.append(prefix + _name(entry) + "/ %d" % len(entry["children"]))
            _render(entry["children"], lo, hi, order, opened, prefix + "  ", out)

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

    def _requested(spec):
        """The directory one request entry names, `~` expanded: `as_paths` joins to it."""
        return _os.path.expanduser(spec["path"] if isinstance(spec, dict) else spec)

    def _section(path, entries, offset, limit):
        """One directory page: the header line, then its tree. Answers `(text, next_offset)`.

        The header comes first and names the continuation, because a printed block is
        clipped from the end and a trailing note would be lost first."""
        home = _os.path.expanduser("~")
        shown = "~" + path[len(home) :] if home and path.startswith(home) else path
        if not entries:
            return shown + "  empty", None
        dirs = sum(1 for e in entries if e.get("type") == "dir")
        head = "%s  %dd %df" % (shown, dirs, len(entries) - dirs)
        order = _breadth_order(entries)
        total = len(order)
        hi = total if limit is None else min(total, offset + limit)
        if offset >= total:
            return head + "  · offset %d is past the last entry (%d)" % (
                offset,
                total,
            ), None
        following = hi if hi < total else None
        if following is not None:
            head += "  %d-%d of %d → next(r), or the same call with offset=%d" % (
                offset + 1,
                hi,
                total,
                hi,
            )
        elif offset:
            head += "  %d-%d of %d" % (offset + 1, hi, total)
        out = [head]
        opened = set()
        _mark_open(entries, offset, hi, order, opened)
        _render(entries, offset, hi, order, opened, "", out)
        return "\n".join(out), following

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

    def _walk(specs, shared):
        """Rows of the host walk, one per spec, in request order."""
        bridge = globals().get("__vis_list_directories__")
        if bridge is None:
            raise RuntimeError("ls: listing bridge not bound in this sandbox")
        return _decoded(bridge(_json.dumps({"paths": specs, **shared})))

    class _LsPage(str):
        """A listing page that continues itself, with the paging protocol of grep.

        `__iter__` stays the str's own, so `"".join(page)` and `list(page)` keep their
        meaning. Pages are walked by name: `next(page)`, `page.pages()`, `page.all()`."""

        def __reduce__(self):
            # The session snapshot keeps the text. The page class is local to the shim.
            return (str, (str(self),))

        def __next__(self):
            """The next page; StopIteration after the last, so `next(r, None)` is the sentinel form."""
            rest = [i for i, nxt in enumerate(self._following) if nxt is not None]
            if not rest:
                raise StopIteration
            return _page(
                [self._specs[i] for i in rest],
                self._shared,
                [self._following[i] for i in rest],
                self._limit,
                self._one,
            )

        def pages(self, max_pages=10):
            """This page, then each following page, lazily, at most `max_pages`."""
            page, count = self, 0
            while page is not None and count < int(max_pages):
                yield page
                count += 1
                page = next(page, None)

        def all(self):
            """The rest of the listing from this page, as ONE unpaged page."""
            return _page(self._specs, self._shared, self._starts, None, self._one)

    def _page(specs, shared, offsets, limit, one):
        """Walk `specs` and render each directory from its own offset."""
        rows = _walk(specs, shared)
        parts, following = [], []
        for row, offset in zip(rows, offsets):
            text, nxt = _section(row["path"], row["entries"], offset, limit)
            parts.append(text)
            following.append(nxt)
        page = _LsPage("\n\n".join(parts))
        page._specs, page._shared, page._one = specs, shared, one
        page._starts, page._following, page._limit = list(offsets), following, limit
        page.next_offset = following[0] if one else following
        page.is_capped = any(nxt is not None for nxt in following)
        return page

    def _count(name, value, least, none_ok):
        """`value` as an int of at least `least`; a wrong value names the argument."""
        if value is None and none_ok:
            return None
        if isinstance(value, bool) or not isinstance(value, int) or value < least:
            raise ValueError(
                "ls: %s must be an int >= %d%s, got %r"
                % (name, least, " or None" if none_ok else "", value)
            )
        return value

    def ls(
        paths=".",
        depth=1,
        is_hidden=False,
        *,
        hidden=None,
        is_ignored=False,
        pattern=None,
        as_paths=False,
        limit=250,
        offset=0,
    ):
        """List directories as a compact tree STRING. `docs["ls"]` below replaces this text."""
        if hidden is not None:
            is_hidden = hidden
        limit = _count("limit", limit, 1, True)
        offset = _count("offset", offset, 0, False)
        one = _as_path(paths) is not None
        request = [paths] if one else list(paths)
        specs = [_as_spec(entry) for entry in request]
        shared = {
            "depth": int(depth),
            "is_hidden": bool(is_hidden),
            "is_ignored": bool(is_ignored),
            "pattern": pattern,
        }
        if as_paths:
            # A row's `path` is the host's display form, relative inside the
            # workspace, so each entry joins the path the caller asked for.
            flat = []
            for spec, row in zip(specs, _walk(specs, shared)):
                _paths(_requested(spec), row["entries"], flat)
            return flat
        return _page(specs, shared, [offset] * len(specs), limit, one)

    g = globals()
    g["ls"] = ls

    docs = g.setdefault("__vis_docs__", {})
    docs["ls"] = (
        "ls(paths='.', depth=1, is_hidden=False, *, hidden=None, is_ignored=False, "
        "pattern=None, as_paths=False, limit=250, offset=0) lists directory contents "
        "from the ignore-aware walk of the host, as a compact printable STRING. "
        "`ls(dir)` gives a `path  Nd Nf` header, then the tree."
        "\n\nThe files and closed directories of one directory share one line, "
        "separated by two spaces. Directories come first, then files, each in "
        "alphabetical order. Each open directory follows on its own line as `name/ N`, "
        "with its child count, and its children indent two spaces. A closed directory "
        "is `name/ N`, or `name/` at the depth limit. A file is `name`, or `name 120k` "
        "from 64k up. A name with whitespace is quoted."
        "\n\nA page shows at most `limit` entries, counted level by level, so the "
        "shallow levels are complete first. `+K more (28 .md, 2 dirs)` ends a directory "
        "that continues on the next page. A capped header names the next call: "
        "`1-250 of 625 → next(r), or the same call with offset=250`. limit=None lists "
        "everything."
        "\n\n`next(r)` gives the next page and raises StopIteration after the last, so "
        "use `next(r, None)` as the sentinel form. `r.pages()` walks the pages with a "
        "bound of 10. `r.all()` gives the rest in one unpaged page. `r.next_offset` is "
        "where the next page starts, or None."
        "\n\n`ls([dir, ...])` gives one such section per directory, in request order, "
        "separated by a blank line. A path is a str or a pathlib.Path. A batch entry "
        'can also be a per-path spec, such as `{"path": dir, "depth": 2}`, whose '
        "options override the shared ones. Each directory pages on its own, and "
        "`r.next_offset` is then a list with one value per directory."
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
        "None, it overrides is_hidden. By default, the listing skips gitignored entries. "
        "is_ignored=True also lists entries that .gitignore, .ignore and the host's "
        "cache rules exclude, such as build output. A per-path spec can set it too."
        "\n\nas_paths=True returns the same walk as a flat list of paths instead of "
        "the tree, in the same order. Each path is the requested path joined with the "
        "entry name, and directories end in '/'. A relative request gives relative "
        "paths, and an absolute or `~` request gives absolute paths. "
        "as_paths ignores limit and offset: `ls(dir, depth=3, as_paths=True)` is the "
        "whole file list, ready for cat, grep or Path()."
    )

    # ONE text for one handle: `help(ls)` and `doc("ls")` read the same
    # string, so neither can go stale against the other.
    ls.__doc__ = docs["ls"]


__vis_install_ls__()
del __vis_install_ls__
