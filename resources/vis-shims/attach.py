# vis sandbox attachment shim: attach.
#
# A tool that PRODUCES an artifact (image/csv/json/pdf/wav/...) persists it as a
# durable iteration attachment (a session_iteration_attachment DB row) so it
# survives a restart and, for image media-types, replays to a vision model. The
# bytes are read through the sandbox's own CONFINED open, so a path outside the
# filesystem roots raises the normal sandbox error.


def __vis_install_attach__():
    import os as _os
    import base64 as _b64

    # A PDF and an HTML page are DOCUMENTS: bytes a HUMAN reads, never an image
    # block on the model's wire. The engine clamps their audience to "user" in
    # `attachments/attachment-audience`; this is that same closed table on the
    # sandbox side, so audience='model' is refused at the call site instead of
    # quietly attaching something nobody will ever see.
    __vis_doc_media_types = (
        "application/pdf",
        "text/html",
        "application/xhtml+xml",
    )

    def __vis_is_doc(mt):
        head = str(mt or "").split(";")[0].strip().lower()
        return head in __vis_doc_media_types

    def __vis_kind_for(mt):
        if str(mt or "").startswith("image/"):
            return "image"
        return "doc" if __vis_is_doc(mt) else "file"

    def __vis_guess_media_type(name, data):
        head = bytes(data[:16])

        def starts(sig):
            s = bytes(sig)
            return head[: len(s)] == s

        if starts([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]):
            return "image/png"
        if starts([0xFF, 0xD8, 0xFF]):
            return "image/jpeg"
        if starts([0x47, 0x49, 0x46, 0x38]):
            return "image/gif"
        if starts([0x42, 0x4D]):
            return "image/bmp"
        if starts([0x25, 0x50, 0x44, 0x46]):
            return "application/pdf"
        if starts([0x50, 0x4B, 0x03, 0x04]) or starts([0x50, 0x4B, 0x05, 0x06]):
            return "application/zip"
        if starts([0x1F, 0x8B]):
            return "application/gzip"
        if starts([0x52, 0x49, 0x46, 0x46]) and head[8:12] == bytes(
            [0x57, 0x45, 0x42, 0x50]
        ):
            return "image/webp"
        if starts([0x52, 0x49, 0x46, 0x46]) and head[8:12] == bytes(
            [0x57, 0x41, 0x56, 0x45]
        ):
            return "audio/wav"
        if starts([0x4F, 0x67, 0x67, 0x53]):
            return "audio/ogg"
        if starts([0x49, 0x44, 0x33]) or starts([0xFF, 0xFB]):
            return "audio/mpeg"
        try:
            probe = bytes(data[:256]).lstrip().lower()
            if probe.startswith(b"<!doctype html") or probe.startswith(b"<html"):
                return "text/html"
        except Exception:
            pass
        import mimetypes

        mt = mimetypes.guess_type(str(name))[0]
        if mt:
            return mt
        try:
            bytes(data).decode("utf-8")
            return "text/plain"
        except Exception:
            return "application/octet-stream"

    def __vis_caption(label):
        # A caption is exactly ONE line: the `vis-image`/`vis-table` fences are
        # line-structured, so a newline inside the label would corrupt the header
        # the renderer parses.
        text = " ".join(str(label).split()) if label is not None else ""
        return text or None

    def __vis_human_bytes(n):
        n = float(n)
        for unit in ("B", "KB", "MB"):
            if n < 1024.0 or unit == "MB":
                return (str(int(n)) + " B") if unit == "B" else ("%.1f %s" % (n, unit))
            n = n / 1024.0

    def __vis_emit_image_fence(disp, name, mt, nbytes, label=None):
        # A `vis-image` fence (the same shape plt.show() emits): 5 header lines
        # (summary / host path / mime / WxH / size) a graphical TUI/web reads to
        # paint the picture inline, with the closing fence. No backslash escapes
        # in this shim, so the lines are joined with chr(10).
        try:
            path = str(disp[0])
            w = int(disp[1])
            h = int(disp[2])
        except Exception:
            return

        size = __vis_human_bytes(nbytes)
        summary = (
            "[Image: " + str(name) + " " + str(w) + "×" + str(h) + ", " + size + "]"
        )
        if label:
            summary = summary + " " + str(label)
        fence = "`" * 4
        lines = [
            fence + "vis-image",
            summary,
            path,
            str(mt),
            str(w) + "x" + str(h),
            size,
            fence,
        ]
        print(chr(10).join(lines))

    def __vis_emit_doc_fence(disp, name, mt, nbytes, label=None):
        # A `vis-doc` fence: a PDF or an HTML page is a DOCUMENT, so it rides
        # the transcript as 5 header lines (summary / host path / mime / name /
        # size) and NO payload. The TUI opens the host file in the system
        # viewer; the companion renders it inside a sandboxed frame that cannot
        # reach the app's own DOM or styles. The model only ever sees the
        # headline - the bytes are one read_attachment away.
        # Returns True when a fence was printed.
        try:
            path = str(disp[0])
        except Exception:
            return False
        if not path:
            return False
        size = __vis_human_bytes(nbytes)
        doc = "PDF" if "pdf" in str(mt).lower() else "HTML"
        summary = "[Document: " + str(name) + " " + doc + ", " + size + "]"
        if label:
            summary = summary + " " + str(label)
        fence = "`" * 4
        lines = [
            fence + "vis-doc",
            summary,
            path,
            str(mt),
            str(name),
            size,
            fence,
        ]
        print(chr(10).join(lines))
        return True

    # A table fence carries at most this many DATA rows: enough to explore a
    # result set inline, small enough that a 100k-row export cannot flood the
    # transcript. The header line always reports the TRUE row count.
    __vis_table_max_rows = 500

    def __vis_emit_table_fence(name, mt, data, nbytes, label=None):
        # A `vis-table` fence: a CSV/TSV artifact is DATA, not a picture, so it
        # rides the TRANSCRIPT as a real grid — 5 header lines (summary / name /
        # mime / COLSxROWS / size) then the payload as normalized CSV, which the
        # TUI and the companion paint as a sortable, pageable, selectable table.
        # Those rows are for the HUMAN only: the model wire keeps the `[Table: …]`
        # headline and drops the payload (engine-side `elide-table-fences`), so a
        # 500-row sheet is never re-billed on every later request.
        # Returns True when a fence was printed.
        import csv as _csv
        import io as _io

        lower = str(name).lower()
        tsv = lower.endswith(".tsv") or str(mt) == "text/tab-separated-values"
        if not (tsv or lower.endswith(".csv") or str(mt) == "text/csv"):
            return False
        try:
            text = bytes(data).decode("utf-8")
            reader = _csv.reader(_io.StringIO(text), delimiter=chr(9) if tsv else ",")
            rows = [r for r in reader if any(str(c).strip() for c in r)]
        except Exception:
            return False
        if not rows:
            return False

        cols = max(len(r) for r in rows)
        total = len(rows) - 1
        shown = rows[1 : 1 + __vis_table_max_rows]
        buf = _io.StringIO()
        writer = _csv.writer(buf, lineterminator=chr(10))
        for row in [rows[0]] + shown:
            writer.writerow([str(c) for c in row] + [""] * (cols - len(row)))
        size = __vis_human_bytes(nbytes)
        summary = (
            "[Table: "
            + str(name)
            + " "
            + str(total)
            + (" row" if total == 1 else " rows")
            + " × "
            + str(cols)
            + (" col" if cols == 1 else " cols")
            + ", "
            + size
            + "]"
        )
        if len(shown) < total:
            summary = summary + " first " + str(len(shown)) + " rows"
        if label:
            summary = summary + " " + str(label)
        fence = "`" * 4
        lines = [
            fence + "vis-table",
            summary,
            str(name),
            str(mt),
            str(cols) + "x" + str(total),
            size,
            buf.getvalue().rstrip(chr(10)),
            fence,
        ]
        print(chr(10).join(lines))
        return True

    def __vis_audience(audience, mt=None):
        aud = str(audience if audience is not None else "both").strip().lower()
        if aud not in ("both", "user", "model"):
            raise ValueError(
                "attach: audience must be 'both', 'user' or 'model', got "
                + repr(audience)
            )
        if __vis_is_doc(mt):
            # A document can never be an image block, so 'model' would mean
            # "send it to nobody". Refuse that outright; every other spelling
            # settles on 'user' - the human reads the document and the model is
            # told the file exists.
            if aud == "model":
                raise ValueError(
                    "attach: "
                    + str(mt)
                    + " is a document for the human, so audience='model' is "
                    "impossible - it is never sent as an image. Attach it with "
                    "audience='user'; the model is told the file exists and "
                    "opens it with read_attachment(id)."
                )
            return "user"
        return aud

    def __vis_attach_data(data, name, kind, media_type, label, audience):
        mt = media_type or __vis_guess_media_type(name, data)
        knd = kind or __vis_kind_for(mt)
        cap = __vis_caption(label)
        aud = __vis_audience(audience, mt)
        b64 = _b64.b64encode(data).decode("ascii")
        rec = globals().get("__vis_record_attachment__")
        if rec is None:
            raise RuntimeError("attach: capture bridge not bound in this sandbox")
        env = rec(knd, mt, b64, name, len(data), aud, cap)
        if not env[0]:
            raise RuntimeError("attach: " + str(env[1]))
        if aud == "model":
            # audience='model': the bytes ride the next request and NOTHING is
            # painted for the human. Staying silent here is the whole point.
            return None
        disp = env[1] if len(env) > 1 else None
        if __vis_is_doc(mt):
            if not __vis_emit_doc_fence(disp, name, mt, len(data), cap):
                print("[Attached: " + name + "]" + ((" " + cap) if cap else ""))
        elif disp:
            __vis_emit_image_fence(disp, name, mt, len(data), cap)
        elif not __vis_emit_table_fence(name, mt, data, len(data), cap):
            if cap:
                # No inline fence (a non-image, non-tabular artifact, or an image
                # the host could not probe): the caption still has to reach
                # whoever reads the block.
                print("[Attached: " + name + "] " + cap)
        return None

    def attach(
        source,
        filename=None,
        kind=None,
        media_type=None,
        label=None,
        audience="both",
    ):
        # ONE attach verb, three shapes of source: a confined PATH, in-memory
        # BYTES (a str is a path, so encode text you produced), or anything with
        # `savefig` - the matplotlib idiom `attach(fig, 'plot.png')`.
        if isinstance(source, (bytes, bytearray, memoryview)):
            return __vis_attach_data(
                bytes(source),
                str(filename) if filename else "artifact",
                kind,
                media_type,
                label,
                audience,
            )
        if hasattr(source, "savefig"):
            import io

            buf = io.BytesIO()
            source.savefig(buf, format="png")
            return __vis_attach_data(
                buf.getvalue(),
                str(filename) if filename else "figure.png",
                kind,
                media_type,
                label,
                audience,
            )
        # A PATH in any spelling a human types: a str, an os.PathLike (pathlib),
        # with `~` and $VARS expanded - the string that works in a shell works
        # here - and a plain sentence instead of a bare OSError when it is wrong.
        raw = _os.fspath(source) if hasattr(source, "__fspath__") else str(source)
        path = _os.path.expanduser(_os.path.expandvars(raw))
        if _os.path.isdir(path):
            raise IsADirectoryError(
                "attach: " + path + " is a directory - attach one file"
            )
        try:
            with open(path, "rb") as f:
                data = f.read()
        except FileNotFoundError:
            raise FileNotFoundError("attach: no such file: " + path) from None
        name = filename or _os.path.basename(path) or "artifact"
        return __vis_attach_data(data, str(name), kind, media_type, label, audience)

    def __vis_attachment_rows():
        lst = globals().get("__vis_list_attachments__")
        if lst is None:
            raise RuntimeError(
                "list_attachments: reader bridge not bound in this sandbox"
            )
        env = lst()
        if not env[0]:
            raise RuntimeError("list_attachments: " + str(env[1]))
        import json as _json

        rows = _json.loads(env[1])
        return [{str(k).replace("-", "_"): v for k, v in r.items()} for r in rows]

    def __vis_thread(rows, name):
        # One NAME is one artifact: every cut of it, oldest first.
        thread = [r for r in rows if r.get("filename") == str(name)]
        thread.sort(key=lambda r: int(r.get("version") or 1))
        return thread

    def list_attachments(name=None):
        rows = __vis_attachment_rows()
        return rows if name is None else __vis_thread(rows, name)

    def __vis_locate(caller, target, version=None):
        # ONE addressing rule for the whole family, so "is this an id or a name?"
        # is never a question the caller has to answer: an id names ONE stored
        # cut, a filename names the ARTIFACT and resolves to its latest cut, and
        # a version only ever qualifies a filename.
        rows = __vis_attachment_rows()
        wanted = str(target)
        if version is None:
            for r in rows:
                if str(r.get("id")) == wanted:
                    return r
        thread = __vis_thread(rows, wanted)
        if not thread:
            raise LookupError(
                caller
                + ": no attachment with id or filename "
                + repr(wanted)
                + " in this session"
            )
        if version is None:
            return thread[-1]
        want = int(version)
        # -1 is the latest cut, -2 the one before it: the walk backwards a Python
        # list already means.
        if -len(thread) <= want < 0:
            return thread[want]
        for r in thread:
            if int(r.get("version") or 1) == want:
                return r
        raise LookupError(
            caller
            + ": "
            + repr(wanted)
            + " has no version "
            + str(version)
            + " (versions: "
            + ", ".join(str(int(r.get("version") or 1)) for r in thread)
            + ")"
        )

    def get_attachment(target, version=None):
        return __vis_locate("get_attachment", target, version)

    def show_attachment(target, version=None):
        reinsp = globals().get("__vis_reinspect_attachment__")
        if reinsp is None:
            raise RuntimeError(
                "show_attachment: reader bridge not bound in this sandbox"
            )
        row = __vis_locate("show_attachment", target, version)
        env = reinsp(str(row.get("id")))
        if not env[0]:
            raise RuntimeError("show_attachment: " + str(env[1]))
        out = env[1]
        return {"id": out[0], "filename": out[1], "media_type": out[2], "size": out[3]}

    def read_attachment(target, version=None):
        rd = globals().get("__vis_read_attachment__")
        if rd is None:
            raise RuntimeError(
                "read_attachment: reader bridge not bound in this sandbox"
            )
        row = __vis_locate("read_attachment", target, version)
        env = rd(str(row.get("id")))
        if not env[0]:
            raise RuntimeError("read_attachment: " + str(env[1]))
        b64 = env[1]
        # BYTES, nothing else: the descriptor is one get_attachment() away, so
        # printing this call can never spill a metadata map nobody asked for.
        return _b64.b64decode(b64) if b64 else b""

    attach.__doc__ = (
        "Persist a produced artifact as a durable attachment. source is a confined "
        "PATH, in-memory BYTES (name them with filename), or a matplotlib figure. "
        "SAME DOCUMENT, SAME NAME: re-attaching a filename stores the next "
        "VERSION of that artifact, never report_v2.png beside report.png; a new "
        "name is a different document. Attach one or two artifacts per turn - "
        "compose many images into ONE sheet. audience routes it: 'both' "
        "(default), 'user' (human only), 'model' (context only). A CSV/TSV "
        "becomes a transcript table whose rows stay out of the model's context; "
        "a *.pdf/*.html is a human-only document and refuses audience='model'. "
        "kind, media_type and filename override inference; label is a one-line "
        "caption. Returns None - call it, do not print it."
    )
    list_attachments.__doc__ = (
        "Descriptor dicts for this session's artifacts, or - given a filename - "
        "that one artifact's versions, oldest first ([] if never attached)."
    )
    get_attachment.__doc__ = (
        "ONE artifact's descriptor dict, never its bytes. target is the filename "
        "you attached under (the artifact: latest cut unless you pass version, "
        "negative counting back) or an id out of a descriptor (one exact cut). "
        "LookupError when it does not exist."
    )
    read_attachment.__doc__ = (
        "The artifact's raw BYTES, for Python. Same target/version addressing as "
        "get_attachment."
    )
    show_attachment.__doc__ = (
        "Put a stored IMAGE in front of the model for exactly the NEXT request, "
        "then stored-only again; nothing is re-stored. Images replay only while "
        "they fit the request's image budget, and this is the way back to one "
        "that dropped out. Same addressing as get_attachment."
    )

    g = globals()
    g["attach"] = attach
    g["list_attachments"] = list_attachments
    g["get_attachment"] = get_attachment
    g["read_attachment"] = read_attachment
    g["show_attachment"] = show_attachment
    g["__vis_guess_media_type"] = __vis_guess_media_type
    g["__vis_kind_for"] = __vis_kind_for

    docs = g.setdefault("__vis_docs__", {})
    docs["attach"] = (
        "attach(source, filename=None, kind=None, media_type=None, label=None, "
        "audience='both'): persist an artifact durably across restarts. source is "
        "a confined PATH, BYTES (name them with filename), or a matplotlib "
        "figure. SAME DOCUMENT, SAME NAME - re-attaching a filename stores the "
        "next VERSION of that artifact, never report_v2.png beside report.png; a "
        "new name is a different document. One or two artifacts per turn: "
        "compose many images into ONE sheet. audience='both'|'user'|'model' "
        "routes who sees it. CSV/TSV becomes a transcript table whose rows stay "
        "out of the model's context; *.pdf/*.html is a human-only document and "
        "refuses audience='model'. label is a one-line caption. Returns None."
    )
    docs["list_attachments"] = (
        "list_attachments(name=None): descriptor dicts (id, filename, version, "
        "media_type, kind, size, audience, turn_id, ...) for this session's "
        "artifacts, or one filename's versions oldest first."
    )
    docs["get_attachment"] = (
        "get_attachment(target, version=None): ONE descriptor dict, no bytes. "
        "target is the FILENAME you attached under - the artifact, latest cut "
        "unless you name a version (negative counts back) - or an id out of a "
        "descriptor, one exact stored cut. That same addressing holds for every "
        "read call here. LookupError when it does not exist."
    )
    docs["read_attachment"] = (
        "read_attachment(target, version=None): the artifact's raw BYTES and "
        "nothing else, addressed like get_attachment."
    )
    docs["show_attachment"] = (
        "show_attachment(target, version=None): put a stored IMAGE in front of "
        "the model for exactly the NEXT request, then stored-only again. Images "
        "replay only while they fit the request's image budget; this is the way "
        "back to one that dropped out, and it re-stores nothing."
    )


__vis_install_attach__()
del __vis_install_attach__
