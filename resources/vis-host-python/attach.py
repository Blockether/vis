# vis sandbox attachment shim: attach.
#
# A tool that PRODUCES an artifact (image/csv/json/pdf/wav/...) persists it as a
# durable iteration attachment (a session_iteration_attachment DB row) so it
# survives a restart and, for image media-types, replays to a vision model. The
# bytes are read through the sandbox's own CONFINED open, so a path outside the
# filesystem roots raises the normal sandbox error.
#
# The five verbs are the verbs of the SDK (`_attachment_verbs` in
# `blockether/vis/extension.py`), bound to the bridges of this sandbox. This file
# owns only what an extension does not have: the preview that a block prints.


def __vis_install_attach__():
    import importlib as _importlib
    import types as _types

    # ONE implementation of the five verbs: `_attachment_verbs` of the SDK module,
    # which `vis.attach` of an extension also uses. This shim gives it the host
    # doors of this session, prints the transcript fence of a stored artifact and
    # holds the `doc()` pages.
    __vis_sdk = _importlib.import_module("blockether.vis.extension")

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

    def __vis_present(
        row, *, name, data, size, label, audience, display, is_document, path
    ):
        # Show what `attach` stored to whoever reads the block. `data` is None for
        # a REFERENCE: the file stays on disk and no byte of it was read.
        if data is None:
            print(
                "[Attached: "
                + name
                + "] "
                + __vis_human_bytes(size)
                + " kept on disk as a reference: "
                + str(row.get("path") or path)
                + ((" " + label) if label else "")
            )
            return
        if audience == "model":
            # audience='model': the bytes ride the next request and NOTHING is
            # painted for the human. Staying silent here is the whole point.
            return
        mt = row.get("media_type")
        if is_document:
            if not __vis_emit_doc_fence(display, name, mt, size, label):
                print("[Attached: " + name + "]" + ((" " + label) if label else ""))
        elif display:
            __vis_emit_image_fence(display, name, mt, size, label)
        elif not __vis_emit_table_fence(name, mt, data, size, label):
            if label:
                # No inline fence (a non-image, non-tabular artifact, or an image
                # the host could not probe): the caption still has to reach
                # whoever reads the block.
                print("[Attached: " + name + "] " + label)

    def __vis_bridge(bridge, verb):
        # One host door of this session. The engine binds the bridge into these
        # globals, so the lookup happens at each call.
        def door(*args):
            bound = globals().get(bridge)
            if bound is None:
                raise RuntimeError(verb + ": bridge not bound in this sandbox")
            return bound(*args)

        return door

    __vis_door = _types.SimpleNamespace(
        attach=__vis_bridge("__vis_record_attachment__", "attach"),
        attach_reference=__vis_bridge("__vis_record_attachment_reference__", "attach"),
        attachment_limit=__vis_bridge("__vis_attachment_limit__", "attach"),
        list_attachments=__vis_bridge("__vis_list_attachments__", "list_attachments"),
        get_attachment=__vis_bridge("__vis_get_attachment__", "get_attachment"),
        read_attachment=__vis_bridge("__vis_read_attachment__", "read_attachment"),
        show_attachment=__vis_bridge("__vis_reinspect_attachment__", "show_attachment"),
    )

    (
        attach,
        list_attachments,
        get_attachment,
        read_attachment,
        show_attachment,
    ) = __vis_sdk._attachment_verbs(
        lambda: __vis_door,
        present=__vis_present,
        app_door=__vis_bridge("__vis_live_app__", "attach"),
    )

    # ONE page per callable, in the shape every documented Vis verb wears: the
    # CALL LINE and the KEYS are STRUCTURE - `doc(name)` prints them above the
    # document, independently of the `apropos` name filter - and the page itself
    # is prose plus the raw-result contract. The call line is DERIVED from the
    # live `def` rather than retyped, because a signature spelled twice drifts
    # one edit later; the keys line is where requiredness is stated, because a
    # Python default can be a spelling detail (`source=None` exists only so that
    # `path=` can reach it as a keyword).
    def __vis_call_line(fn):
        import inspect as _inspect

        shown = [
            p
            for p in _inspect.signature(fn).parameters.values()
            if p.kind is not p.VAR_KEYWORD
        ]
        return fn.__name__ + "(" + ", ".join(str(p) for p in shown) + ")"

    __vis_target_keys = (
        "Keys: target (REQUIRED — filename, id or descriptor)"
        " · version (one cut, negative counts back)"
    )

    __vis_pages = (
        (
            attach,
            "Keys: source (REQUIRED — path, bytes, PIL image, figure, ui.app, app row)"
            " · filename (same name stores the next version)"
            " · kind · media_type (overrides what the bytes sniff as)"
            " · label (one-line caption)"
            " · audience ('both', 'user' or 'model')"
            " · on_event (the listener of an app, only in an extension)",
            "Store a produced artifact as a durable attachment that survives restarts. "
            "source is a confined PATH, in-memory BYTES (name them with filename), or "
            "an in-memory image or figure object. SAME DOCUMENT, SAME NAME: "
            "re-attaching a filename stores the next VERSION of that artifact. Never "
            "put report_v2.png beside report.png, because a new name is a different "
            "document. Attach one or two artifacts per turn, and compose many images "
            "into ONE sheet."
            "\n\naudience routes it: 'both' (default), 'user' (human only) "
            "or 'model' (context only). A CSV/TSV becomes a transcript table whose "
            "rows stay out of the model's context. A *.pdf/*.html is a human-only "
            "document and refuses audience='model'. A script in an HTML page can still "
            "call vis.setAgentNote(text) to tell the model what the page shows. Only the "
            "Companion app runs it, so check window.vis first."
            "\n\nkind, media_type and filename override inference, and label is a "
            "one-line caption. A near-miss spelling FOLDS onto the keyword it meant, "
            "so the call does not fail. The folds are name/file_name/fname -> "
            "filename, path/file/src -> source, title/caption/description/alt -> "
            "label, mime/mime_type/content_type -> media_type and type -> kind. Naming "
            "one idea twice, the main name and an alias in the same call, is refused."
            "\n\nA FILE larger than 32 MiB is not copied: Vis keeps a REFERENCE to its "
            "path, stores no byte of it, and people stream it from disk. A reference "
            "is for people only, so it refuses audience='model', and read_attachment "
            "and show_attachment refuse it with its path; open the path itself. "
            "In-memory bytes larger than 32 MiB are refused: write them to a file first."
            "\n\nRaw result: the DESCRIPTOR dict of that artifact, with id, filename, "
            "version, media_type, kind, size and audience. A reference also has "
            "storage='reference' and path. Every read "
            "verb here takes it as its target, also in this same block."
            "\n\nAPPS: attach(ui.app(title, *nodes)) opens an app, after "
            "`from blockether.vis.extension import ui`. An app is a live view that "
            "stays in this session. It uses the same ui components, plus ui.diff and "
            "ui.thread. ui.scene(node_id, width, height, objects=[ui.scene_object(...)]) "
            "draws a 3D world from shapes, places and colors. People save drafts and "
            "send comments in the TUI and the Companion app.\n\nattach returns a LiveAttachment handle. Its submissions() "
            "reads the Send receipts, and reply(node_id, body, thread_id=, anchor=, "
            "submission_id=) answers them. set_status(submission_id, status) moves a "
            "receipt, and archive() or close() ends the app. A Send reaches this "
            "session as a new turn that names the app and its receipt. attach(row) "
            "with the app row from list_attachments() gives the handle again."
            "\n\nNOTES: set_agent_note(text) tells the model the current state of the "
            "app, and a blank text removes it. The newest note of each open app and HTML "
            "page reaches session['apps'] with every user message.",
        ),
        (
            list_attachments,
            "Keys: name (one artifact's versions, oldest first)",
            "List the artifacts of this session, including the ones that this block "
            "attached. Given a filename, list the versions of that ONE artifact, "
            "oldest first."
            "\n\nRaw result: a list of descriptor dicts (id, filename, version, "
            "media_type, kind, size, audience, turn_id, is_pending, ...). It is [] "
            "when nothing was ever attached under that name.",
        ),
        (
            get_attachment,
            __vis_target_keys,
            "Get the descriptor of ONE artifact, never its bytes. target is the "
            "FILENAME you attached under, an id, or a descriptor that attach() "
            "returned. A filename means the latest cut, unless you name a version "
            "(negative counts back). A descriptor is one exact cut. Every read verb "
            "here uses the same addressing."
            "\n\nRaw result: one descriptor dict, without bytes. If nothing in this "
            "session has that target, you get a catchable host error.",
        ),
        (
            read_attachment,
            __vis_target_keys,
            "Get the raw BYTES of the artifact for Python. This is the only way to get "
            "them. The target works exactly as in get_attachment."
            "\n\nRaw result: bytes and nothing else, so printing this call never shows "
            "a metadata map that nobody asked for.",
        ),
        (
            show_attachment,
            __vis_target_keys,
            "Show a stored IMAGE to the model for exactly the NEXT request. After "
            "that, it is stored-only again, and nothing is stored twice. A local path "
            "is a source, not an attachment address, so use "
            "show_attachment(attach(path)). Images replay only while they fit the "
            "image budget of the request. This verb brings back an image that dropped "
            "out. The target works as in get_attachment."
            "\n\nAn image that this block attached for the model is already on the "
            "wire, so showing it does nothing."
            "\n\nRaw result: {id, filename, media_type, size} for the image now queued "
            "for the next request.",
        ),
    )

    g = globals()
    docs = g.setdefault("__vis_docs__", {})
    calls = g.setdefault("__vis_calls__", {})
    keys = g.setdefault("__vis_keys__", {})

    for fn, keyline, page in __vis_pages:
        name = fn.__name__
        g[name] = fn
        docs[name] = page
        calls[name] = __vis_call_line(fn)
        keys[name] = keyline
        # ONE text for one handle: `help(attach)` and `doc("attach")` read the
        # same string, so neither can go stale against the other.
        fn.__doc__ = page


__vis_install_attach__()
del __vis_install_attach__
