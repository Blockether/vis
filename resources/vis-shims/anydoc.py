def __vis_install_anydoc__():
    import base64, os, re, sys, types

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

    class Citation:
        """One hit: the document it is in, the line it starts on, what matched.

        `document_id` is how the caller names that document (a path, a mapping
        key), so a citation stays readable once it has left the search that
        produced it; `str(citation)` is the `id:line: text` line itself.
        """

        __slots__ = (
            "document_id",
            "format",
            "query",
            "match",
            "line",
            "column",
            "offset",
            "text",
            "before",
            "after",
        )

        def __init__(
            self,
            document_id,
            format,
            query,
            match,
            line,
            column,
            offset,
            text,
            before,
            after,
        ):
            self.document_id = document_id
            self.format = format
            self.query = query
            self.match = match
            self.line = line
            self.column = column
            self.offset = offset
            self.text = text
            self.before = before
            self.after = after

        def __str__(self):
            return "%s:%d: %s" % (self.document_id, self.line, self.text)

        def __repr__(self):
            return "Citation(document_id=%r, line=%r, column=%r, match=%r)" % (
                self.document_id,
                self.line,
                self.column,
                self.match,
            )

    class Document:
        """A converted document: its Markdown, how it was identified, its assets."""

        __slots__ = ("id", "format", "source", "chars", "markdown", "assets")

        def __init__(self, id, format, source, chars, markdown, assets):
            self.id = id
            self.format = format
            self.source = source
            self.chars = chars
            self.markdown = markdown
            self.assets = assets

        def search(self, query, **options):
            """Cite this one document, without converting it again."""
            return search(query, {self.id: self}, **options)

        def lines(self):
            """This document's Markdown, as `(line_number, text)` pairs."""
            return tuple(enumerate(self.markdown.split("\n"), 1))

        def __str__(self):
            return self.markdown

        def __repr__(self):
            return "Document(id=%r, format=%r, source=%r, chars=%r, assets=%d)" % (
                self.id,
                self.format,
                self.source,
                self.chars,
                len(self.assets),
            )

    class SearchResults:
        """Every citation one query earned, plus the documents it searched.

        Iterating yields citations; `documents` maps each id to the `Document`
        that was read, so a second question costs no conversion, and `skipped`
        names every file a directory walk could not read, because a corpus with
        one unreadable file in it is still an answer.
        """

        __slots__ = ("query", "citations", "documents", "skipped")

        def __init__(self, query, citations, documents, skipped):
            self.query = query
            self.citations = citations
            self.documents = documents
            self.skipped = skipped

        def by_document(self):
            """Citations grouped by `document_id`, in the order they were found."""
            grouped = {}
            for citation in self.citations:
                grouped.setdefault(citation.document_id, []).append(citation)
            return grouped

        def __iter__(self):
            return iter(self.citations)

        def __len__(self):
            return len(self.citations)

        def __bool__(self):
            return bool(self.citations)

        def __getitem__(self, index):
            return self.citations[index]

        def __repr__(self):
            return "SearchResults(query=%r, citations=%d, documents=%d, skipped=%d)" % (
                self.query,
                len(self.citations),
                len(self.documents),
                len(self.skipped),
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

    def to_document(data, format=None, name=None, assets=True, max_assets=0, id=None):
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
            str(id) if id is not None else (str(name) if name else "document"),
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

    def read(path, format=None, assets=True, max_assets=0, id=None):
        """`to_document` for a document on disk; its id is the path you gave."""
        with open(path, "rb") as handle:
            data = handle.read()
        document = to_document(
            data,
            format=format,
            name=os.path.basename(str(path)),
            assets=assets,
            max_assets=max_assets,
        )
        document.id = str(path) if id is None else str(id)
        return document

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

    _known_extensions = {}

    def _extension_format(name):
        # One host call per distinct extension, not per file in the corpus.
        stem, dot, extension = str(name).rpartition(".")
        if not dot or not stem:
            return None
        extension = extension.lower()
        if extension not in _known_extensions:
            _known_extensions[extension] = format_from_extension(
                "document." + extension
            )
        return _known_extensions[extension]

    def _query_text(query):
        if hasattr(query, "pattern"):
            return query.pattern
        if isinstance(query, (list, tuple, set, frozenset)):
            return " OR ".join(str(term) for term in query)
        return str(query)

    def _pattern(query, regex, ignore_case, whole_word):
        """The query compiled ONCE, whatever spelling it arrived in."""
        if hasattr(query, "search") and hasattr(query, "pattern"):
            return query
        terms = (
            list(query) if isinstance(query, (list, tuple, set, frozenset)) else [query]
        )
        terms = [str(term) for term in terms if str(term) != ""]
        if not terms:
            raise ValueError("anydoc.search needs something to look for")
        body = "|".join(term if regex else re.escape(term) for term in terms)
        body = "(?:%s)" % body
        if whole_word:
            body = r"\b%s\b" % body
        return re.compile(body, re.IGNORECASE if ignore_case else 0)

    def _cite(document, pattern, query, limit, context):
        lines = document.markdown.split("\n")
        found = []
        offset = 0
        for index, line in enumerate(lines):
            for hit in pattern.finditer(line):
                found.append(
                    Citation(
                        document.id,
                        document.format,
                        query,
                        hit.group(0),
                        index + 1,
                        hit.start() + 1,
                        offset + hit.start(),
                        line,
                        tuple(lines[max(0, index - context) : index])
                        if context
                        else (),
                        tuple(lines[index + 1 : index + 1 + context])
                        if context
                        else (),
                    )
                )
                if limit and len(found) >= limit:
                    return found
            offset += len(line) + 1
        return found

    def _load_path(path, format):
        return lambda: read(path, format=format, assets=False)

    def _walk(directory, format):
        for root, directories, names in os.walk(str(directory)):
            directories[:] = sorted(
                name for name in directories if not name.startswith(".")
            )
            for name in sorted(names):
                if name.startswith("."):
                    continue
                if _extension_format(name) is None:
                    continue
                path = os.path.join(root, name)
                yield path, _load_path(path, format), False

    def _sources(sources, format):
        """One document, a few, a mapping of ids, or a directory of many.

        Yields `(id, load, is_explicit)`. Explicit means the caller named this
        document, so its failure is raised; a file merely FOUND under a
        directory is reported as skipped instead of ending the search.
        """
        if isinstance(sources, Document):
            yield sources.id, (lambda document=sources: document), True
        elif isinstance(sources, (bytes, bytearray, memoryview)):
            data = bytes(sources)
            yield (
                "document",
                lambda: to_document(data, format=format, assets=False),
                True,
            )
        elif isinstance(sources, str) or hasattr(sources, "__fspath__"):
            path = os.fspath(sources) if hasattr(sources, "__fspath__") else sources
            if os.path.isdir(path):
                for found in _walk(path, format):
                    yield found
            else:
                yield path, _load_path(path, format), True
        elif hasattr(sources, "items"):
            for name, source in sources.items():
                for _, load, _explicit in _sources(source, format):
                    yield str(name), load, True
        elif hasattr(sources, "__iter__"):
            for source in sources:
                for found in _sources(source, format):
                    yield found
        else:
            raise TypeError(
                "anydoc.search needs a path, a directory, bytes, a Document, a "
                "list of them or a mapping of ids to them, not %s"
                % type(sources).__name__
            )

    def _unique(taken, id):
        if id not in taken:
            return id
        index = 2
        while "%s#%d" % (id, index) in taken:
            index += 1
        return "%s#%d" % (id, index)

    def search(
        query,
        sources,
        regex=False,
        ignore_case=True,
        whole_word=False,
        context=0,
        limit=0,
        per_document=0,
        format=None,
    ):
        """Search one document, several, or a whole directory, and cite the hits.

        `query` is literal text by default (a list of terms is an OR, `regex=True`
        takes it as a pattern, a compiled pattern is used as it is). Every hit
        comes back as a `Citation` carrying the document's id, the 1-based line
        it starts on and that line's text, so an answer can point at the page it
        came from. `limit` caps the whole search and `per_document` caps each
        document, so a corpus cannot drown one answer.
        """
        pattern = _pattern(query, regex, ignore_case, whole_word)
        text = _query_text(query)
        documents = {}
        citations = []
        skipped = []
        for id, load, is_explicit in _sources(sources, format):
            if limit and len(citations) >= limit:
                break
            try:
                document = load()
            except (AnydocError, OSError, ValueError) as error:
                if is_explicit:
                    raise
                skipped.append({"id": id, "error": str(error)})
                continue
            document.id = _unique(documents, id)
            documents[document.id] = document
            cap = int(per_document or 0)
            if limit:
                room = limit - len(citations)
                cap = room if cap == 0 else min(cap, room)
            citations.extend(_cite(document, pattern, text, cap, context))
        return SearchResults(text, citations, documents, skipped)

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
        "GitHub-Flavored Markdown, via Vis's Rust converter, plus a search that "
        "cites the document and line every hit came from."
    )
    mod.__version__ = "vis"
    mod.__all__ = [
        "AnydocError",
        "Asset",
        "Citation",
        "Document",
        "SearchResults",
        "detect",
        "format_from_bytes",
        "format_from_extension",
        "format_from_path",
        "formats",
        "read",
        "search",
        "to_document",
        "to_markdown",
        "to_markdown_bytes",
    ]
    mod.__getattr__ = __getattr__
    mod.AnydocError = AnydocError
    mod.Asset = Asset
    mod.Citation = Citation
    mod.Document = Document
    mod.SearchResults = SearchResults
    mod.detect = detect
    mod.format_from_bytes = format_from_bytes
    mod.format_from_extension = format_from_extension
    mod.format_from_path = format_from_path
    mod.formats = formats
    mod.read = read
    mod.search = search
    mod.to_document = to_document
    mod.to_markdown = to_markdown
    mod.to_markdown_bytes = to_markdown_bytes
    sys.modules["anydoc"] = mod
    _bi.anydoc = mod


__vis_install_anydoc__()
del __vis_install_anydoc__
