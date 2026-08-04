# vis sandbox attachment shim: vis_attach / vis_attach_bytes.
#
# A tool that PRODUCES an artifact (image/csv/json/pdf/wav/...) persists it as a
# durable iteration attachment (a session_iteration_attachment DB row) so it
# survives a restart and, for image media-types, replays to a vision model. The
# bytes are read through the sandbox's own CONFINED open, so a path outside the
# filesystem roots raises the normal sandbox error.


def __vis_install_attach__():
    import os as _os
    import base64 as _b64

    def __vis_kind_for(mt):
        return "image" if str(mt or "").startswith("image/") else "file"

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

    def __vis_audience(audience, in_answer):
        aud = str(audience if audience is not None else "both").strip().lower()
        if aud not in ("both", "user", "model"):
            raise ValueError(
                "vis_attach: audience must be 'both', 'user' or 'model', got "
                + repr(audience)
            )
        if in_answer and aud == "model":
            raise ValueError(
                "vis_attach: in_answer=True needs a human-visible audience "
                "('both' or 'user'), not 'model'"
            )
        return aud

    def vis_attach_bytes(
        data,
        filename,
        kind=None,
        media_type=None,
        label=None,
        audience="both",
        in_answer=False,
    ):
        if isinstance(data, str):
            data = data.encode("utf-8")
        data = bytes(data)
        name = str(filename) if filename else "artifact"
        mt = media_type or __vis_guess_media_type(name, data)
        knd = kind or __vis_kind_for(mt)
        cap = __vis_caption(label)
        aud = __vis_audience(audience, in_answer)
        b64 = _b64.b64encode(data).decode("ascii")
        rec = globals().get("__vis_record_attachment__")
        if rec is None:
            raise RuntimeError("vis_attach: capture bridge not bound in this sandbox")
        env = rec(knd, mt, b64, name, len(data), aud, bool(in_answer), cap)
        if not env[0]:
            raise RuntimeError("vis_attach: " + str(env[1]))
        if aud == "model":
            # audience='model': the bytes ride the next request and NOTHING is
            # painted for the human. Staying silent here is the whole point.
            return None
        if in_answer:
            # Painted exactly once, in the answer's gallery, so the human reads
            # the figures where the conclusion is instead of scrolling the run.
            print("[Answer gallery: " + name + "]" + ((" " + cap) if cap else ""))
            return None
        disp = env[1] if len(env) > 1 else None
        if disp:
            __vis_emit_image_fence(disp, name, mt, len(data), cap)
        elif not __vis_emit_table_fence(name, mt, data, len(data), cap):
            if cap:
                # No inline fence (a non-image, non-tabular artifact, or an image
                # the host could not probe): the caption still has to reach
                # whoever reads the block.
                print("[Attached: " + name + "] " + cap)
        return None

    def vis_attach(
        path,
        kind=None,
        media_type=None,
        filename=None,
        label=None,
        audience="both",
        in_answer=False,
    ):
        if hasattr(path, "savefig"):
            import io

            # Support the natural matplotlib idiom `vis_attach(fig, 'plot.png')`
            # without requiring a sandbox-visible temporary file.
            buf = io.BytesIO()
            path.savefig(buf, format="png")
            if (
                filename is None
                and isinstance(kind, str)
                and kind.lower().endswith(".png")
            ):
                filename, kind = kind, None
            return vis_attach_bytes(
                buf.getvalue(),
                filename or "figure.png",
                kind=kind,
                media_type=media_type,
                label=label,
                audience=audience,
                in_answer=in_answer,
            )
        with open(path, "rb") as f:
            data = f.read()
        name = filename or _os.path.basename(str(path)) or "artifact"
        return vis_attach_bytes(
            data,
            name,
            kind=kind,
            media_type=media_type,
            label=label,
            audience=audience,
            in_answer=in_answer,
        )

    def vis_attachments():
        lst = globals().get("__vis_list_attachments__")
        if lst is None:
            raise RuntimeError(
                "vis_attachments: reader bridge not bound in this sandbox"
            )
        env = lst()
        if not env[0]:
            raise RuntimeError("vis_attachments: " + str(env[1]))
        import json as _json

        rows = _json.loads(env[1])

        return [{str(k).replace("-", "_"): v for k, v in r.items()} for r in rows]

    def vis_reinspect_attachment(attachment_id, detail="auto"):
        if detail not in ("auto", "low", "high"):
            raise ValueError("detail must be auto, low, or high")
        reinsp = globals().get("__vis_reinspect_attachment__")
        if reinsp is None:
            raise RuntimeError(
                "vis_reinspect_attachment: reader bridge not bound in this sandbox"
            )
        env = reinsp(str(attachment_id), detail)
        if not env[0]:
            raise RuntimeError("vis_reinspect_attachment: " + str(env[1]))
        row = env[1]
        return {"id": row[0], "filename": row[1], "media_type": row[2], "size": row[3]}

    def vis_read_attachment(attachment_id):
        rd = globals().get("__vis_read_attachment__")
        if rd is None:
            raise RuntimeError(
                "vis_read_attachment: reader bridge not bound in this sandbox"
            )
        env = rd(str(attachment_id))
        if not env[0]:
            raise RuntimeError("vis_read_attachment: " + str(env[1]))
        row = env[1]
        b64 = row[0]
        data = _b64.b64decode(b64) if b64 else None
        return {
            "bytes": data,
            "media_type": row[1],
            "filename": row[2],
            "kind": row[3],
            "size": row[4],
            "id": row[5],
            "storage_uri": row[6],
        }

    vis_attach.__doc__ = (
        "Persist a produced file as a durable attachment. ATTACH ONE OR TWO "
        "ARTIFACTS PER TURN: a human reviews a figure, not a filmstrip, so "
        "COMPOSE many images into a SINGLE sheet (a matplotlib grid of subplots, "
        "one montage PNG) and attach that instead of N separate shots. audience "
        "routes it: 'both' (default) shows the human AND sends it to the model, "
        "'user' shows the human only and never enters the model's context, "
        "'model' sends it to the model only and paints nothing for the human. "
        "in_answer=True holds it back for the gallery under the FINAL ANSWER, "
        "painted exactly once where the human reads the conclusion - the only way "
        "to put an image IN the answer. The sandbox-confined path is read, its "
        "media type inferred, and its bytes stored across restarts; images can "
        "replay to vision models. A CSV/TSV file attaches as a live TABLE: the "
        "transcript paints it as a sortable, pageable grid, and its rows never "
        "enter the model's context - vis_read_attachment(id) reads them back. "
        "kind, media_type and filename override inference. label is a one-line "
        "caption printed with the artifact, so a series of shots says which shot "
        "is which. Returns None: call directly, do not print. Use "
        "vis_attachments() for metadata and vis_attach_bytes() for in-memory "
        "bytes/str."
    )
    vis_attach_bytes.__doc__ = (
        "Persist bytes (or a UTF-8 str) as a durable attachment without a "
        "temporary file; filename drives media-type inference. ATTACH ONE OR TWO "
        "ARTIFACTS PER TURN and COMPOSE many images into a single sheet rather "
        "than attaching each one. audience is 'both' (human and model), 'user' "
        "(human only, never in the model's context) or 'model' (model only, "
        "nothing painted for the human); in_answer=True defers it to the gallery "
        "under the final answer. Name it *.csv/*.tsv and it attaches as a live "
        "TABLE the transcript paints as a grid, with the rows kept out of the "
        "model's context. label is a one-line caption printed with the artifact. "
        "Returns None: call directly, do not print. Use vis_attachments() for "
        "metadata."
    )

    g = globals()
    g["vis_attach"] = vis_attach
    g["vis_attach_bytes"] = vis_attach_bytes
    g["vis_attachments"] = vis_attachments
    g["vis_read_attachment"] = vis_read_attachment
    g["vis_reinspect_attachment"] = vis_reinspect_attachment
    g["__vis_guess_media_type"] = __vis_guess_media_type
    g["__vis_kind_for"] = __vis_kind_for

    docs = g.setdefault("__vis_docs__", {})
    docs["vis_attach"] = (
        "vis_attach(path, kind=None, media_type=None, filename=None, label=None, "
        "audience='both', in_answer=False): persist a produced file as a durable "
        "attachment across restarts. ATTACH ONE OR TWO ARTIFACTS PER TURN - a "
        "human cannot review a filmstrip: COMPOSE many images into ONE sheet (a "
        "matplotlib subplot grid, a single montage PNG) and attach that, never N "
        "separate shots. audience='both' shows the human and sends it to the "
        "model; 'user' shows the human ONLY and never enters the model's context; "
        "'model' sends it to the model ONLY and paints nothing for the human. "
        "in_answer=True holds it back for the gallery under the FINAL ANSWER, "
        "painted exactly once where the human reads the conclusion - prefer it "
        "for the one or two figures that make your point. Images replay to vision "
        "models; a CSV/TSV attaches as a live TABLE the transcript paints as a "
        "sortable, pageable grid whose rows stay OUT of the model's context "
        "(vis_read_attachment(id) reads them back). label is a one-line caption "
        "printed with the artifact. Returns None; call, do not print. Use "
        "vis_attachments() for metadata."
    )
    docs["vis_attach_bytes"] = (
        "vis_attach_bytes(data, filename, kind=None, media_type=None, label=None, "
        "audience='both', in_answer=False): persist bytes/str as a durable "
        "attachment. ATTACH ONE OR TWO ARTIFACTS PER TURN and COMPOSE many images "
        "into ONE sheet instead of attaching each one. audience routes it to "
        "'both', 'user' (human only, out of the model's context) or 'model' "
        "(model only, nothing painted for the human); in_answer=True defers it to "
        "the gallery under the FINAL ANSWER. A *.csv/*.tsv filename attaches as a "
        "live TABLE in the transcript with its rows out of context. label is a "
        "one-line caption printed with the artifact. Returns None; call, do not "
        "print. Use vis_attachments() for metadata."
    )
    docs["vis_reinspect_attachment"] = (
        "vis_reinspect_attachment(id, detail='auto'): queue this session's persisted "
        "image for exactly the NEXT provider request, then return it to stored-only. "
        "detail is auto, low, or high; unknown/non-image ids raise RuntimeError."
    )
    docs["vis_attachments"] = (
        "vis_attachments(): list THIS session's persisted artifact metadata: id, "
        "filename, media_type, kind, size, position, tool_call_id, iteration_id. "
        "Pass an id to vis_read_attachment()."
    )
    docs["vis_read_attachment"] = (
        "vis_read_attachment(id): fetch persisted artifact bytes and metadata as "
        "{bytes, media_type, filename, kind, size, id, storage_uri}."
    )


__vis_install_attach__()
del __vis_install_attach__
