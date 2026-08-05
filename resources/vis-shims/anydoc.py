def __vis_install_anydoc__():
    import base64, os, sys, types

    _bi = sys.modules["builtins"]
    _markdown = __vis_anydoc_markdown__
    _detect = __vis_anydoc_detect__

    class AnydocError(Exception):
        """A document the converter refused: unknown format, or a corrupt file."""

    def _realize(value):
        is_foreign = globals().get("__vis_is_foreign__")
        if is_foreign is None or not is_foreign(value):
            return value
        if hasattr(value, "keys"):
            try:
                return {key: _realize(item) for key, item in value.items()}
            except Exception:
                return value
        try:
            return [_realize(item) for item in value]
        except Exception:
            return value

    def _call(fn, *args):
        result = fn(*args)
        if not result[0]:
            raise AnydocError(result[1])
        return _realize(result[1])

    def _as_bytes(data):
        if isinstance(data, (bytes, bytearray, memoryview)):
            return bytes(data)
        raise TypeError(
            "anydoc needs bytes-like document data, not %s" % type(data).__name__
        )

    def _b64(data):
        return base64.b64encode(_as_bytes(data)).decode("ascii")

    def _text(value):
        return "" if value is None else str(value)

    class Asset:
        """One binary embedded in a document (an image, a media part)."""

        __slots__ = ("id", "media_type", "origin_part", "size", "bytes")

        def __init__(self, id, media_type, origin_part, size, data):
            self.id = id
            self.media_type = media_type
            self.origin_part = origin_part
            self.size = size
            self.bytes = data

        def __len__(self):
            return len(self.bytes)

        def __repr__(self):
            return "Asset(id=%r, media_type=%r, size=%r)" % (
                self.id,
                self.media_type,
                self.size,
            )

    class Document:
        """A converted document: its Markdown, how it was identified, its assets."""

        __slots__ = ("format", "source", "chars", "markdown", "assets")

        def __init__(self, format, source, chars, markdown, assets):
            self.format = format
            self.source = source
            self.chars = chars
            self.markdown = markdown
            self.assets = assets

        def __str__(self):
            return self.markdown

        def __repr__(self):
            return "Document(format=%r, source=%r, chars=%r, assets=%d)" % (
                self.format,
                self.source,
                self.chars,
                len(self.assets),
            )

    def _asset(entry):
        raw = entry.get("bytes")
        return Asset(
            entry.get("id"),
            entry.get("media_type"),
            entry.get("origin_part"),
            entry.get("size"),
            base64.b64decode(raw) if raw else b"",
        )

    def to_document(data, format=None, name=None, assets=True, max_assets=0):
        """Convert document bytes into a `Document` (Markdown plus its assets)."""
        payload = _call(
            _markdown,
            _b64(data),
            _text(format),
            _text(name),
            bool(assets),
            int(max_assets or 0),
        )
        return Document(
            payload.get("format"),
            payload.get("source"),
            payload.get("chars"),
            payload.get("markdown") or "",
            [_asset(entry) for entry in (payload.get("assets") or [])],
        )

    def to_markdown_bytes(data, format=None, name=None):
        """GitHub-Flavored Markdown for document bytes."""
        payload = _call(_markdown, _b64(data), _text(format), _text(name), False, 0)
        return payload.get("markdown") or ""

    def to_markdown(path, format=None):
        """GitHub-Flavored Markdown for a document on disk."""
        with open(path, "rb") as handle:
            data = handle.read()
        return to_markdown_bytes(data, format=format, name=os.path.basename(str(path)))

    def read(path, format=None, assets=True, max_assets=0):
        """`to_document` for a document on disk."""
        with open(path, "rb") as handle:
            data = handle.read()
        return to_document(
            data,
            format=format,
            name=os.path.basename(str(path)),
            assets=assets,
            max_assets=max_assets,
        )

    def detect(data=b"", name=None, format=None):
        """Identify a document without converting it: `{format, source, formats}`.

        The container's own signature is asked first, because a signature cannot
        lie and an extension routinely does; `format` is None when nothing
        recognised the input.
        """
        return _call(_detect, _b64(data), _text(format), _text(name))

    def format_from_bytes(data):
        """The format a document's own signature reports, or None."""
        return detect(data)["format"]

    def format_from_extension(extension):
        """The format a file extension claims, or None."""
        return detect(b"", name=str(extension))["format"]

    def format_from_path(path):
        """The format of a file on disk: signature first, extension second."""
        with open(path, "rb") as handle:
            head = handle.read(4096)
        return detect(head, name=os.path.basename(str(path)))["format"]

    def formats():
        """Every format this converter understands."""
        return tuple(detect()["formats"])

    def __getattr__(name):
        # Lazy so importing the module costs no host call at all.
        if name == "FORMATS":
            value = formats()
            mod.FORMATS = value
            return value
        raise AttributeError("module 'anydoc' has no attribute %r" % name)

    mod = types.ModuleType("anydoc")
    mod.__doc__ = (
        "Any document (Word, PDF, EPUB, presentations, spreadsheets, CSV) as "
        "GitHub-Flavored Markdown, via Vis's Rust converter."
    )
    mod.__version__ = "vis"
    mod.__all__ = [
        "AnydocError",
        "Asset",
        "Document",
        "detect",
        "format_from_bytes",
        "format_from_extension",
        "format_from_path",
        "formats",
        "read",
        "to_document",
        "to_markdown",
        "to_markdown_bytes",
    ]
    mod.__getattr__ = __getattr__
    mod.AnydocError = AnydocError
    mod.Asset = Asset
    mod.Document = Document
    mod.detect = detect
    mod.format_from_bytes = format_from_bytes
    mod.format_from_extension = format_from_extension
    mod.format_from_path = format_from_path
    mod.formats = formats
    mod.read = read
    mod.to_document = to_document
    mod.to_markdown = to_markdown
    mod.to_markdown_bytes = to_markdown_bytes
    sys.modules["anydoc"] = mod
    _bi.anydoc = mod


__vis_install_anydoc__()
del __vis_install_anydoc__
