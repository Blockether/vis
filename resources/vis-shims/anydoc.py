def __vis_install_anydoc__():
    import base64, difflib, math, os, re, sys, types, unicodedata

    _bi = sys.modules["builtins"]
    _markdown = __vis_anydoc_markdown__
    _detect = __vis_anydoc_detect__
    _cache = __vis_anydoc_cache__

    # Errors — every refusal says which document, which character, and what to
    # do about it. Each one is also the plain Python error a caller would have
    # written `except` for, so old code keeps working.

    class AnydocError(Exception):
        """Base class: anything anydoc refuses."""

    class DocumentError(AnydocError):
        """A document the converter could not read.

        `document_id`, `source` and `format` say WHICH one, so a failure inside
        a corpus walk is still attributable after the fact.
        """

        def __init__(self, message, document_id=None, source=None, format=None):
            AnydocError.__init__(self, message)
            self.message = message
            self.document_id = document_id
            self.source = source
            self.format = format

    class SourceError(AnydocError, TypeError):
        """Something handed to anydoc cannot be read as a document."""

    class QueryError(AnydocError, ValueError):
        """A query the parser refused, pointing at the character it choked on."""

        def __init__(self, message, query="", position=None, hint=None):
            self.message = message
            self.query = query
            self.position = position
            self.hint = hint
            AnydocError.__init__(self, self._render())

        def _render(self):
            lines = [self.message]
            if self.query and self.position is not None:
                lines.append("    " + self.query)
                lines.append("    " + " " * max(int(self.position), 0) + "^")
            elif self.query:
                lines.append("    " + self.query)
            if self.hint:
                lines.append(self.hint)
            return "\n".join(lines)

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
            raise DocumentError(result[1])
        return _realize(result[1])

    def _as_bytes(data):
        if isinstance(data, (bytes, bytearray, memoryview)):
            return bytes(data)
        raise SourceError(
            "anydoc needs bytes-like document data, not %s; a document on disk "
            "is anydoc.to_markdown(path) / anydoc.read(path)" % type(data).__name__
        )

    def _source(source, name=None, head=0):
        """`(bytes, name, path)` for a document however the caller holds it.

        A `str`/`os.PathLike` is a file to open, and its base name is what tells
        the converter that a signature-less `.csv` is a spreadsheet; bytes are
        the document itself; an open binary file is read. Which of the three a
        caller has is an accident of where the document came from, so every
        reading door takes all three instead of making the caller convert.
        Opening a path that is not there raises the usual `OSError`, naming it.
        """
        if isinstance(source, (bytes, bytearray, memoryview)):
            return bytes(source), name, None
        if isinstance(source, str) or hasattr(source, "__fspath__"):
            path = os.fspath(source) if hasattr(source, "__fspath__") else source
            with open(path, "rb") as handle:
                data = handle.read(head) if head else handle.read()
            return data, name or os.path.basename(path) or None, path
        if hasattr(source, "read"):
            data = _as_bytes(source.read())
            opened = getattr(source, "name", None)
            if name is None and isinstance(opened, str):
                name = os.path.basename(opened) or None
            return data, name, None
        raise SourceError(
            "anydoc needs a path, bytes or an open binary file, not %s"
            % type(source).__name__
        )

    def _document_error(error, path, format, id=None):
        """The same refusal, told with the file it came from."""
        if path is None:
            return error
        return DocumentError(
            "%s: %s" % (path, error.message),
            document_id=str(id) if id is not None else path,
            source=path,
            format=format,
        )

    def _b64(data):
        return base64.b64encode(_as_bytes(data)).decode("ascii")

    def _text(value):
        return "" if value is None else str(value)

    # Folding — the reason a search finds what a human means
    #
    # Both the corpus and the query go through the SAME fold, and every folded
    # character remembers the original it came from, so a citation still points
    # at the raw line and column. Folding: NFKD (ligature `ﬃ` -> `ffi`), accents
    # dropped (`Zürich` == `Zurich`, however it was encoded), curly quotes and
    # dashes straightened, soft hyphens and zero-widths removed, a hyphen at a
    # line break healed (`quar-\nterly` -> `quarterly`), every whitespace run
    # (including newlines and NBSP) collapsed to one space so a phrase crosses a
    # wrap, and case folded (`Hauptstraße` == `HAUPTSTRASSE`).

    _QUOTES = {
        "\u2018": "'",
        "\u2019": "'",
        "\u201a": "'",
        "\u201b": "'",
        "\u2032": "'",
        "\u201c": '"',
        "\u201d": '"',
        "\u201e": '"',
        "\u2033": '"',
        "\u2010": "-",
        "\u2011": "-",
        "\u2012": "-",
        "\u2013": "-",
        "\u2014": "-",
        "\u2015": "-",
        "\u2212": "-",
        "\u2026": "...",
    }
    _INVISIBLE = "\u00ad\u200b\u200c\u200d\u2060\ufeff"
    # ASCII runs are the common case and are copied at C speed; everything else
    # falls to the per-character path below.
    _PLAIN_RE = re.compile(r"[\x21-\x7e]+")
    _GAP_RE = re.compile(r"[\s" + _INVISIBLE + r"]+")
    _WORD_RE = re.compile(r"[^\W_]+", re.UNICODE)

    def _fold(text, ignore_case=True, fold=True):
        """`(folded_text, origin)` — `origin[i]` is where folded char `i` began.

        `origin` has one extra entry (the length of `text`) so the END of a
        match maps too. `fold=False` keeps the characters as they are and only
        collapses whitespace, for a caller who must match exactly.
        """
        out = []
        origin = []
        i = 0
        size = len(text)
        while i < size:
            match = _PLAIN_RE.match(text, i)
            if match:
                chunk = match.group(0)
                out.append(chunk.lower() if ignore_case else chunk)
                origin.extend(range(i, match.end()))
                i = match.end()
                continue
            match = _GAP_RE.match(text, i)
            if match:
                run = match.group(0)
                i = match.end()
                nxt = text[i] if i < size else ""
                blank = run.strip(_INVISIBLE) == ""
                shy = "\u00ad" in run
                broke = "\n" in run or "\r" in run
                joins = nxt.isalnum() and (
                    blank or shy or (broke and out and out[-1].endswith("-"))
                )
                if joins:
                    if not blank and out and out[-1].endswith("-"):
                        out[-1] = out[-1][:-1]
                        origin.pop()
                        if not out[-1]:
                            out.pop()
                    continue
                if blank:
                    continue
                if out:
                    out.append(" ")
                    origin.append(match.start())
                continue
            char = text[i]
            piece = _QUOTES.get(char, char) if fold else char
            if fold:
                piece = unicodedata.normalize("NFKD", piece)
                piece = "".join(c for c in piece if not unicodedata.combining(c))
            if ignore_case:
                piece = piece.casefold()
            for c in piece:
                out.append(c)
                origin.append(i)
            i += 1
        folded = "".join(out)
        while folded.endswith(" "):
            folded = folded[:-1]
            origin.pop()
        origin.append(size)
        return folded, origin

    def _fold_query(term, ignore_case=True, fold=True):
        return _fold(term, ignore_case, fold)[0]

    def _singular(term):
        """The plural-insensitive root of one term, or the term itself.

        Deliberately ONLY plurals: stripping `-ing`/`-ed` too would make
        `March` find `Marching`, which is a different word to the human who
        asked. `results.explain()` prints the root it used.
        """
        low = term
        if len(low) > 4 and low.endswith(("ses", "xes", "zes", "ches", "shes")):
            return low[:-2]
        if len(low) > 4 and low.endswith("ies"):
            return low[:-3] + "y"
        if len(low) > 3 and low.endswith("s") and not low.endswith(("ss", "us", "is")):
            return low[:-1]
        return low

    # Query language

    _SCOPES = {
        "heading": ("heading",),
        "title": ("heading",),
        "paragraph": ("paragraph",),
        "text": ("paragraph",),
        "table": ("table-row",),
        "row": ("table-row",),
        "list": ("list-item",),
        "code": ("code",),
        "note": ("note",),
        "footnote": ("note",),
    }
    _FIELDS = tuple(sorted(set(list(_SCOPES) + ["section", "page", "any"])))

    _TOKEN_RE = re.compile(
        r"""\s*(?:
              (?P<op>[+-])?
              (?:(?P<field>[A-Za-z_]+):)?
              (?:
                  "(?P<phrase>[^"]*)"
                | /(?P<regex>(?:[^/\\]|\\.)*)/
                | (?P<near>[Nn][Ee][Aa][Rr]\s*\([^)]*\))
                | (?P<word>[^\s"]+)
              )
            )""",
        re.VERBOSE,
    )
    _NEAR_RE = re.compile(r"[Nn][Ee][Aa][Rr]\s*\((?P<body>[^)]*)\)")

    class Clause:
        """One demand a query makes, already compiled against folded text."""

        __slots__ = (
            "kind",
            "raw",
            "terms",
            "roots",
            "occur",
            "kinds",
            "slop",
            "prefix",
            "pattern",
            "idf",
            "key",
        )

        def __init__(
            self,
            kind,
            raw,
            terms,
            roots,
            occur,
            kinds,
            slop,
            prefix,
            pattern,
        ):
            self.kind = kind
            self.raw = raw
            self.terms = terms
            self.roots = roots
            self.occur = occur
            self.kinds = kinds
            self.slop = slop
            self.prefix = prefix
            self.pattern = pattern
            self.idf = 0.0
            # Identity inside ONE parsed query: the key every per-document match map
            # is filed under. Stamped by `_parse_query`; never `clause.key`, which
            # a loop variable named `id` would shadow.
            self.key = 0

        @property
        def is_positive(self):
            return self.occur != "must_not"

        def __repr__(self):
            return "Clause(%s %r, %s)" % (self.kind, self.raw, self.occur)

        def describe(self):
            where = ""
            if self.kinds:
                where += " in " + "/".join(self.kinds)

            role = {"must": "required", "must_not": "excluded"}.get(
                self.occur, "optional"
            )

            return "%-8s %-28s %-9s%s" % (self.kind, self.raw, role, where)

    def _term_body(term, stem, prefix):
        escaped = re.escape(term)
        if prefix:
            return escaped + r"[^\W_]*"
        if stem and len(term) >= 4:
            return escaped + r"(?:e?s)?"
        return escaped

    def _term_pattern(term, stem, whole_word, prefix):
        body = _term_body(term, stem, prefix)
        return re.compile((r"\b%s\b" if whole_word and not prefix else r"%s") % body)

    def _phrase_pattern(words, stem, whole_word):
        """A phrase, tolerant of what a converter puts BETWEEN its words.

        The gap is one folded space, but a table row joins its cells with
        ` | ` and a list keeps its bullet, so `March 12.4` must still find
        `| March | 12.4 |`. Anything that is not a word character is allowed
        between two words of a phrase.
        """
        gap = r"[\W_]{1,4}"
        body = gap.join(_term_body(word, stem, False) for word in words)
        return re.compile((r"\b%s\b" if whole_word else r"%s") % body)

    def _split_terms(value, ignore_case, fold):
        folded = _fold_query(value, ignore_case, fold)
        return [word for word in _WORD_RE.findall(folded) if word]

    def _clause(raw, field, op, kind, terms, roots, pattern, slop=0, prefix=False):
        kinds = _SCOPES[field] if (field and field in _SCOPES) else None
        occur = {"+": "must", "-": "must_not"}.get(op, "should")
        return Clause(kind, raw, terms, roots, occur, kinds, slop, prefix, pattern)

    def _parse_near(raw, query, at, ignore_case, fold, stem, whole_word):
        body = _NEAR_RE.match(raw).group("body")
        slop = 5
        if "," in body:
            body, _, tail = body.rpartition(",")
            try:
                slop = int(tail.strip())
            except ValueError:
                raise QueryError(
                    "NEAR wants a word distance after the comma, not %r" % tail.strip(),
                    query,
                    at,
                    hint="example: NEAR(revenue march, 10)",
                )
        words = _split_terms(body.replace('"', " "), ignore_case, fold)
        if len(words) < 2:
            raise QueryError(
                "NEAR needs at least two words to measure a distance between",
                query,
                at,
                hint="example: NEAR(revenue march, 10)",
            )
        roots = [_singular(word) if stem else word for word in words]
        patterns = tuple(_term_pattern(root, stem, whole_word, False) for root in roots)
        return words, roots, patterns, slop

    def _stamped(clauses):
        """Give every clause its identity inside THIS query, then hand them back."""
        for index, clause in enumerate(clauses):
            clause.key = index
        return clauses

    def _parse_query(
        query, regex=False, ignore_case=True, fold=True, stem=True, whole_word=True
    ):
        """Parse a query string into clauses. Raises `QueryError`, never guesses.

        Grammar (everything is optional but the terms):

            march revenue        bare terms — ANY of them may match (OR)
            "quarterly revenue"  a phrase, across line wraps and table cells
            +march  /  AND       the document MUST contain it
            -draft  /  NOT       the document must NOT contain it
            rev*                 prefix
            NEAR(revenue march, 8)   both, within 8 words of each other
            /reven[us]e?/        a regular expression, over folded text
            heading:march        only in headings (also table: list: code:
                                 note: paragraph: section: page:3)
        """
        if hasattr(query, "pattern"):
            body = query.pattern
            return (
                body,
                _stamped(
                    [
                        _clause(
                            body,
                            None,
                            None,
                            "regex",
                            [],
                            [],
                            re.compile(body, query.flags),
                        )
                    ]
                ),
                {"pages": set(), "sections": []},
            )
        if isinstance(query, (list, tuple, set, frozenset)):
            query = " ".join(
                ('"%s"' % term) if " " in str(term) else str(term) for term in query
            )
        query = "" if query is None else str(query)
        if not query.strip():
            raise QueryError("an empty query cannot cite anything", query, 0)
        if regex:
            try:
                pattern = re.compile(
                    _fold_query(query, ignore_case, fold),
                    re.IGNORECASE if ignore_case else 0,
                )
            except re.error as error:
                raise QueryError(
                    "this is not a valid regular expression: %s" % error, query, 0
                )
            return (
                query,
                _stamped([_clause(query, None, None, "regex", [], [], pattern)]),
                {"pages": set(), "sections": []},
            )

        clauses = []
        filters = {"pages": set(), "sections": []}
        at = 0
        pending = None
        while at < len(query):
            match = _TOKEN_RE.match(query, at)
            if not match or match.end() == at:
                if not query[at:].strip():
                    break
                raise QueryError("cannot read the query from here", query, at)
            at = match.end()
            op = match.group("op") or pending
            field = match.group("field")
            if field is not None:
                field = field.lower()
                if field not in _FIELDS:
                    raise QueryError(
                        "unknown field %r" % field,
                        query,
                        match.start("field"),
                        hint="fields: " + ", ".join(_FIELDS),
                    )
            word = match.group("word")
            if word is not None and field is None and word in ("AND", "OR", "NOT"):
                pending = {"AND": "+", "NOT": "-", "OR": None}[word]
                continue
            pending = None

            if match.group("phrase") is not None:
                raw = match.group("phrase")
                if field == "section":
                    filters["sections"].append(_fold_query(raw, ignore_case, fold))
                    continue
                words = _split_terms(raw, ignore_case, fold)
                if not words:
                    raise QueryError(
                        "an empty phrase cannot cite anything", query, match.start()
                    )
                roots = [_singular(w) if stem else w for w in words]
                if field == "page":
                    raise QueryError(
                        "page: wants a number, not a phrase", query, match.start()
                    )
                pattern = (
                    _phrase_pattern(roots, stem, whole_word)
                    if len(words) > 1
                    else _term_pattern(roots[0], stem, whole_word, False)
                )
                clauses.append(_clause(raw, field, op, "phrase", words, roots, pattern))
                continue

            if match.group("regex") is not None:
                raw = match.group("regex")
                try:
                    # The corpus is matched FOLDED (casefolded), so a regex
                    # written with capitals has to be case-blind too or
                    # `/Marc\w+/` could never find `Marching`.
                    pattern = re.compile(raw, re.IGNORECASE if ignore_case else 0)
                except re.error as error:
                    raise QueryError(
                        "this is not a valid regular expression: %s" % error,
                        query,
                        match.start("regex"),
                    )
                clauses.append(_clause(raw, field, op, "regex", [], [], pattern))
                continue

            if match.group("near") is not None:
                raw = match.group("near")
                words, roots, patterns, slop = _parse_near(
                    raw, query, match.start("near"), ignore_case, fold, stem, whole_word
                )
                clauses.append(
                    _clause(raw, field, op, "near", words, roots, patterns, slop=slop)
                )
                continue

            raw = word
            if field == "page":
                if not raw.isdigit():
                    raise QueryError(
                        "page: wants a page number, not %r" % raw,
                        query,
                        match.start("word"),
                        hint="`page:` FILTERS the search, as in `page:7 revenue`",
                    )
                filters["pages"].add(int(raw))
                continue
            if field == "section":
                filters["sections"].append(_fold_query(raw, ignore_case, fold))
                continue
            prefix = raw.endswith("*")
            words = _split_terms(raw[:-1] if prefix else raw, ignore_case, fold)
            if not words:
                raise QueryError(
                    "%r has nothing to search for in it" % raw, query, match.start()
                )
            roots = [_singular(w) if stem and not prefix else w for w in words]
            if len(words) > 1:
                pattern = _phrase_pattern(roots, stem, whole_word)
                clauses.append(_clause(raw, field, op, "phrase", words, roots, pattern))
            else:
                pattern = _term_pattern(roots[0], stem, whole_word, prefix)
                clauses.append(
                    _clause(
                        raw, field, op, "term", words, roots, pattern, prefix=prefix
                    )
                )
        if not clauses:
            raise QueryError(
                "this query asks for nothing",
                query,
                0,
                hint=(
                    "`page:` and `section:` only NARROW a search; add something to "
                    "look for, e.g. `page:7 revenue`"
                )
                if (filters["pages"] or filters["sections"])
                else None,
            )
        if not any(clause.is_positive for clause in clauses):
            raise QueryError(
                "a query made only of exclusions has nothing to cite",
                query,
                0,
                hint="add something to look FOR, e.g. `revenue -draft`",
            )
        return query, _stamped(clauses), filters

    # Documents

    class Skipped:
        """A source a corpus walk could not read, and the reason it could not.

        A corpus with one corrupt file in it is still an answer, so a walked file
        that fails conversion lands here instead of ending the search. A document
        the caller NAMED still raises — the difference is whether you asked for
        this file or merely stood next to it.
        """

        __slots__ = ("id", "reason")

        def __init__(self, id, reason):
            self.id = id
            # `DocumentError` already names the file; saying it twice reads like
            # two different failures.
            head = "%s: " % id
            self.reason = reason[len(head) :] if reason.startswith(head) else reason

        def __str__(self):
            return "%s: %s" % (self.id, self.reason)

        def __repr__(self):
            return "Skipped(id=%r, reason=%r)" % (self.id, self.reason)

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

    class Cell:
        """One cell of a table row a citation landed in."""

        __slots__ = ("column", "name", "text", "start", "end")

        def __init__(self, entry):
            self.column = entry.get("column")
            self.name = entry.get("name")
            self.text = entry.get("text") or ""
            self.start = entry.get("start")
            self.end = entry.get("end")

        def __str__(self):
            return "%s=%s" % (self.name, self.text) if self.name else self.text

        def __repr__(self):
            return "Cell(column=%r, name=%r, text=%r)" % (
                self.column,
                self.name,
                self.text,
            )

    class Block:
        """One addressable piece of a document: a heading, a paragraph, a list
        item, a table row, a code block or a note.

        `start`/`end` are character offsets into `document.text`, `line` is the
        1-based line there, `path` is the heading breadcrumb above it and
        `page` is the PDF page it was printed on.
        """

        __slots__ = (
            "index",
            "kind",
            "text",
            "start",
            "end",
            "line",
            "path",
            "level",
            "anchor",
            "page",
            "marker",
            "list_depth",
            "quote_depth",
            "checked",
            "lang",
            "note",
            "note_kind",
            "row",
            "is_header",
            "cells",
        )

        def __init__(self, entry):
            self.index = entry.get("index")
            self.kind = entry.get("kind")
            self.text = entry.get("text") or ""
            self.start = int(entry.get("start") or 0)
            self.end = int(entry.get("end") or 0)
            self.line = int(entry.get("line") or 1)
            self.path = tuple(entry.get("path") or ())
            self.level = entry.get("level")
            self.anchor = entry.get("anchor")
            self.page = entry.get("page")
            self.marker = entry.get("marker")
            self.list_depth = entry.get("list_depth")
            self.quote_depth = entry.get("quote_depth")
            self.checked = entry.get("checked")
            self.lang = entry.get("lang")
            self.note = entry.get("note")
            self.note_kind = entry.get("note_kind")
            self.row = entry.get("row")
            self.is_header = entry.get("is_header")
            self.cells = tuple(Cell(cell) for cell in (entry.get("cells") or ()))

        @property
        def section(self):
            """The heading breadcrumb above this block, as one string."""
            return " \u203a ".join(self.path)

        def cell_at(self, offset):
            """The cell of this table row containing document offset `offset`."""
            for cell in self.cells:
                if cell.start is not None and cell.start <= offset < cell.end:
                    return cell
            return None

        def __str__(self):
            return self.text

        def __repr__(self):
            return "Block(%d, %r, line=%d, page=%r, path=%r)" % (
                self.index if self.index is not None else -1,
                self.kind,
                self.line,
                self.page,
                self.path,
            )

    class Citation:
        """One hit, addressed the way a human would quote it.

        `document_id`, `line` and `column` say where — both 1-based, the way
        an editor counts, and both into `text`, the document's PLAIN text, not
        its Markdown. `page` and `section` say where in the document itself; `snippet` is the passage and `highlight` is the
        same passage with the match marked. `score` ranks it against every
        other citation the query earned.
        """

        __slots__ = (
            "document_id",
            "format",
            "query",
            "match",
            "line",
            "column",
            "offset",
            "end",
            "text",
            "before",
            "after",
            "page",
            "section",
            "path",
            "block_kind",
            "block_index",
            "cell",
            "snippet",
            "highlight",
            "score",
            "clause",
        )

        def __init__(self, **fields):
            for slot in self.__slots__:
                setattr(self, slot, fields.get(slot))

        @property
        def location(self):
            """`p.3 line 12 › Revenue` — everything but the document id."""
            parts = []
            if self.page:
                parts.append("p.%d" % self.page)
            parts.append("line %d" % self.line)
            if self.path:
                parts.append(" \u203a ".join(self.path))
            if self.cell is not None and self.cell.name:
                parts.append("column %s" % self.cell.name)
            return " \u203a ".join(parts)

        def __str__(self):
            return "%s %s: %s" % (
                self.document_id,
                self.location,
                self.snippet or self.text,
            )

        def __repr__(self):
            return (
                "Citation(document_id=%r, page=%r, line=%r, column=%r, match=%r, score=%.3f)"
                % (
                    self.document_id,
                    self.page,
                    self.line,
                    self.column,
                    self.match,
                    self.score or 0.0,
                )
            )

    class Document:
        """A converted document: its Markdown, its plain text, its blocks.

        `markdown` is for reading, `text` is what search matches (the same
        content with none of Markdown's punctuation, so `**March**` is just a
        word), and `blocks` is what a citation is addressed in.
        """

        __slots__ = (
            "id",
            "format",
            "source",
            "chars",
            "markdown",
            "text",
            "blocks",
            "pages",
            "assets",
            "_folded",
            "_folded_blocks",
            "_lines",
        )

        def __init__(
            self,
            id,
            format,
            source,
            chars,
            markdown,
            assets,
            text=None,
            blocks=(),
            pages=None,
        ):
            self.id = id
            self.format = format
            self.source = source
            self.chars = chars
            self.markdown = markdown
            self.text = markdown if text is None else text
            self.blocks = tuple(blocks)
            self.pages = pages
            self.assets = assets
            self._folded = {}
            self._folded_blocks = {}
            self._lines = None

        def search(self, query, **options):
            """Cite this one document, without converting it again."""
            return search(query, {self.id: self}, **options)

        def folded(self, ignore_case=True, fold=True):
            """This document's plain text folded for matching, memoized."""
            key = (bool(ignore_case), bool(fold))
            if key not in self._folded:
                self._folded[key] = _fold(self.text, ignore_case, fold)
            return self._folded[key]

        def folded_blocks(self, ignore_case=True, fold=True):
            """`(block, folded, origin, word_starts)` per block, memoized.

            `origin[i]` is the character in the BLOCK's own text that folded
            character `i` came from, so a match found in folded space is reported
            at the position a human can point at.
            """
            key = (bool(ignore_case), bool(fold))
            rows = self._folded_blocks.get(key)
            if rows is None:
                rows = []
                for block in self.blocks:
                    text, origin = _fold(block.text, ignore_case, fold)
                    starts = [match.start() for match in _WORD_RE.finditer(text)]
                    rows.append((block, text, origin, starts))
                self._folded_blocks[key] = rows
            return rows

        def lines(self):
            """This document's plain text, as `(line_number, text)` pairs."""
            if self._lines is None:
                self._lines = tuple(enumerate(self.text.split("\n"), 1))
            return self._lines

        def line_at(self, offset):
            """`(line_number, line_text, line_start)` for a character offset."""
            start = self.text.rfind("\n", 0, offset) + 1
            end = self.text.find("\n", offset)
            end = len(self.text) if end < 0 else end
            return self.text.count("\n", 0, start) + 1, self.text[start:end], start

        def neighbours(self, line, context):
            """The `context` nearest non-blank lines before and after `line`."""
            if not context:
                return [], []
            rows = self.lines()
            before = [text for _, text in rows[: line - 1] if text.strip()]
            after = [text for _, text in rows[line:] if text.strip()]
            return before[-int(context) :], after[: int(context)]

        def block_at(self, offset):
            """The block a character offset falls in (binary search)."""
            low, high = 0, len(self.blocks) - 1
            found = None
            while low <= high:
                mid = (low + high) // 2
                block = self.blocks[mid]
                if offset < block.start:
                    high = mid - 1
                elif offset >= block.end:
                    found = block
                    low = mid + 1
                else:
                    return block
            return found

        def sections(self):
            """Every heading in reading order, as `(level, path)` pairs."""
            return tuple(
                (block.level, block.path + (block.text,))
                for block in self.blocks
                if block.kind == "heading"
            )

        def outline(self):
            """The document's headings as an indented string."""
            return "\n".join(
                "  " * max(int(level or 1) - 1, 0) + path[-1]
                for level, path in self.sections()
            )

        def __str__(self):
            return self.markdown

        def __repr__(self):
            return (
                "Document(id=%r, format=%r, source=%r, chars=%r, blocks=%d, pages=%r, assets=%d)"
                % (
                    self.id,
                    self.format,
                    self.source,
                    self.chars,
                    len(self.blocks),
                    self.pages,
                    len(self.assets),
                )
            )

    class SearchResults:
        """Every citation one query earned, ranked, plus how it got them.

        Iterating yields citations best-first. `documents` maps each id to the
        `Document` that was read, so a second question costs no conversion;
        `ranking` is `(document_id, score)` best-first; `total_matches` and
        `is_truncated` say whether a `limit` hid anything; `skipped` names
        every file the walk could not read; `suggestions` answers a typo; and
        `explain()` prints exactly how the query parsed and why each document
        scored what it did.
        """

        __slots__ = (
            "query",
            "clauses",
            "citations",
            "documents",
            "skipped",
            "total_matches",
            "is_truncated",
            "ranking",
            "suggestions",
            "stats",
        )

        def __init__(
            self,
            query,
            clauses,
            citations,
            documents,
            skipped,
            total_matches,
            is_truncated,
            ranking,
            suggestions,
            stats,
        ):
            self.query = query
            self.clauses = clauses
            self.citations = citations
            self.documents = documents
            self.skipped = skipped
            self.total_matches = total_matches
            self.is_truncated = is_truncated
            self.ranking = ranking
            self.suggestions = suggestions
            self.stats = stats

        def by_document(self):
            """Citations grouped by `document_id`, best document first."""
            grouped = {}
            for citation in self.citations:
                grouped.setdefault(citation.document_id, []).append(citation)
            return grouped

        def best(self, count=1):
            """The `count` best citations."""
            return self.citations[: max(int(count), 0)]

        def texts(self):
            """Just the snippets, in rank order — for pasting into an answer."""
            return [citation.snippet or citation.text for citation in self.citations]

        def explain(self):
            """Why this is the answer: the parse, the ranking, the misses."""
            lines = ["query: %s" % self.query]
            for clause in self.clauses:
                lines.append("  %s  idf %.2f" % (clause.describe(), clause.idf))
            lines.append(
                "matching: %s"
                % ", ".join(
                    "%s=%s" % (key, self.stats.get(key))
                    for key in ("fold", "ignore_case", "stem", "whole_word", "regex")
                    if key in self.stats
                )
            )
            lines.append(
                "%d document(s) searched, %d converted this call, %d match(es)%s"
                % (
                    len(self.documents),
                    self.stats.get("converted", 0),
                    self.total_matches,
                    " (showing %d)" % len(self.citations) if self.is_truncated else "",
                )
            )
            for document_id, score in self.ranking:
                document = self.documents.get(document_id)
                lines.append(
                    "  %-32s score %6.3f  %s"
                    % (
                        document_id,
                        score,
                        "%s, %d words"
                        % (
                            document.format if document else "?",
                            self.stats.get("words", {}).get(document_id, 0),
                        ),
                    )
                )
            for entry in self.skipped:
                lines.append("  skipped %s — %s" % (entry.id, entry.reason))
            for term, close in sorted(self.suggestions.items()):
                lines.append(
                    "  no hit for %r — did you mean %s?" % (term, ", ".join(close))
                )
            return "\n".join(lines)

        def __iter__(self):
            return iter(self.citations)

        def __len__(self):
            return len(self.citations)

        def __bool__(self):
            return bool(self.citations)

        def __getitem__(self, index):
            return self.citations[index]

        def __repr__(self):
            return (
                "SearchResults(query=%r, citations=%d, total=%d, documents=%d, skipped=%d)"
                % (
                    self.query,
                    len(self.citations),
                    self.total_matches,
                    len(self.documents),
                    len(self.skipped),
                )
            )

    # Conversion

    def _asset(entry):
        raw = entry.get("bytes")
        return Asset(
            entry.get("id"),
            entry.get("media_type"),
            entry.get("origin_part"),
            entry.get("size"),
            base64.b64decode(raw) if raw else b"",
        )

    def to_document(
        source,
        format=None,
        name=None,
        assets=True,
        max_assets=0,
        id=None,
        blocks=True,
    ):
        """Convert a document into a `Document`: a path, bytes or an open file.

        `blocks=True` (the default) also asks for the plain `text`, the block
        structure and — for a PDF — the page count, which is what citations are
        addressed in. The host caches every conversion on the CONTENT hash of
        the bytes, so converting the same document twice is free.
        """
        data, name, path = _source(source, name)
        try:
            payload = _call(
                _markdown,
                _b64(data),
                _text(format),
                _text(name),
                bool(assets),
                int(max_assets or 0),
                bool(blocks),
            )
        except DocumentError as error:
            raise _document_error(error, path, format, id)
        return Document(
            str(id)
            if id is not None
            else (path or (str(name) if name else "document")),
            payload.get("format"),
            payload.get("source"),
            payload.get("chars"),
            payload.get("markdown") or "",
            [_asset(entry) for entry in (payload.get("assets") or [])],
            text=payload.get("text"),
            blocks=[Block(entry) for entry in (payload.get("blocks") or [])],
            pages=payload.get("pages"),
        )

    def to_markdown_bytes(data, format=None, name=None):
        """GitHub-Flavored Markdown for document bytes."""
        payload = _call(
            _markdown, _b64(data), _text(format), _text(name), False, 0, False
        )
        return payload.get("markdown") or ""

    def to_markdown(source, format=None, name=None):
        """GitHub-Flavored Markdown for a document: a path, bytes or an open file."""
        data, name, path = _source(source, name)
        try:
            return to_markdown_bytes(data, format=format, name=name)
        except DocumentError as error:
            raise _document_error(error, path, format)

    def read(path, format=None, assets=True, max_assets=0, id=None, blocks=True):
        """`to_document` for a document on disk; its id is the path you gave."""
        return to_document(
            path,
            format=format,
            assets=assets,
            max_assets=max_assets,
            id=id,
            blocks=blocks,
        )

    def detect(source=b"", name=None, format=None):
        """Identify a document without converting it: `{format, source, formats}`.

        `source` is bytes, an open file or a path — a path is read only as far
        as a signature reaches.

        The container's own signature is asked first, because a signature cannot
        lie and an extension routinely does; `format` is None when nothing
        recognised the input.
        """
        data, name, _path = _source(source, name, head=4096)
        return _call(_detect, _b64(data), _text(format), _text(name))

    def format_from_bytes(data):
        """The format a document's own signature reports, or None."""
        return detect(_as_bytes(data))["format"]

    def format_from_extension(extension):
        """The format a file extension claims, or None."""
        return detect(b"", name=str(extension))["format"]

    def format_from_path(path):
        """The format of a file on disk: signature first, extension second."""
        return detect(path)["format"]

    def formats():
        """Every format this converter understands."""
        return tuple(detect()["formats"])

    def cache_info():
        """The host conversion cache: `{entries, chars, hits, misses, limit}`.

        Proof that a second question about a corpus converted nothing: `hits`
        goes up, `misses` does not.
        """
        return _call(_cache, "info")

    def clear_cache():
        """Empty the host conversion cache and return its final counters."""
        _documents.clear()
        return _call(_cache, "clear")

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

    # Matching

    _MAX_FILE_BYTES = 128 * 1024 * 1024

    def _spans(clause, folded, words, starts):
        """Every `(start, end)` this clause matches in one folded text."""
        if clause.kind == "page":
            return []
        if clause.kind == "near":
            return _near_spans(clause, folded, words, starts)
        return [(m.start(), m.end()) for m in clause.pattern.finditer(folded)]

    def _near_spans(clause, folded, words, starts):
        """Windows where every NEAR term sits within `slop` words of the others."""
        hits = []
        for index, pattern in enumerate(clause.pattern):
            for match in pattern.finditer(folded):
                position = _word_index(starts, match.start())
                hits.append((position, index, match.start(), match.end()))
        if len(set(hit[1] for hit in hits)) < len(clause.pattern):
            return []
        hits.sort()
        spans = []
        for left in range(len(hits)):
            seen = {}
            for right in range(left, len(hits)):
                if hits[right][0] - hits[left][0] > clause.slop:
                    break
                seen.setdefault(hits[right][1], hits[right])
                if len(seen) == len(clause.pattern):
                    found = sorted(seen.values(), key=lambda hit: hit[2])
                    spans.append((found[0][2], found[-1][3]))
                    break
        return _dedupe(spans)

    def _dedupe(spans):
        out = []
        for span in sorted(set(spans)):
            if out and span[0] < out[-1][1]:
                continue
            out.append(span)
        return out

    def _word_index(starts, offset):
        low, high = 0, len(starts) - 1
        best = 0
        while low <= high:
            mid = (low + high) // 2
            if starts[mid] <= offset:
                best = mid
                low = mid + 1
            else:
                high = mid - 1
        return best

    _KIND_BOOST = {"heading": 2.0, "table-row": 1.15, "code": 0.8, "note": 0.8}

    def _snippet(text, start, end, block, width, mark):
        """A passage around the match, cut at a sentence or a word, never mid-word."""
        if width <= 0:
            return text[start:end], (mark[0] + text[start:end] + mark[1])
        low = max(block.start if block else 0, start - width // 2)
        high = min(block.end if block else len(text), end + width // 2)
        head = text[low:start]
        tail = text[end:high]
        cut = max(
            head.rfind(". "), head.rfind("! "), head.rfind("? "), head.rfind("\n")
        )
        if cut >= 0:
            low += cut + 1
        elif low > (block.start if block else 0):
            space = head.find(" ")
            low += space + 1 if space >= 0 else 0
        stop = max(tail.find(". "), tail.find("! "), tail.find("? "))
        newline = tail.find("\n")
        if stop >= 0:
            high = end + stop + 1
        elif newline >= 0:
            high = end + newline
        elif high < (block.end if block else len(text)):
            space = tail.rfind(" ")
            high = end + space if space >= 0 else high
        lead = "\u2026" if low > (block.start if block else 0) else ""
        trail = "\u2026" if high < (block.end if block else len(text)) else ""
        body = text[low:start], text[start:end], text[end:high]
        snippet = (lead + "".join(body) + trail).strip()
        highlight = (
            lead + body[0] + mark[0] + body[1] + mark[1] + body[2] + trail
        ).strip()
        return snippet, highlight

    def _citation(document, clause, span, options, score):
        start, end, block = span
        line, line_text, line_start = document.line_at(start)
        context = int(options.get("context") or 0)
        # The nearest NON-BLANK neighbours: the plain text separates blocks with a
        # blank line, so positional context would otherwise hand back `['']`.
        before, after = document.neighbours(line, context)
        snippet, highlight = _snippet(
            document.text,
            start,
            end,
            block,
            int(options.get("snippet", 160) or 0),
            options.get("mark") or ("**", "**"),
        )
        return Citation(
            document_id=document.id,
            format=document.format,
            query=options.get("query"),
            match=document.text[start:end],
            line=line,
            column=start - line_start + 1,
            offset=start,
            end=end,
            text=line_text,
            before=before,
            after=after,
            page=block.page if block else None,
            section=(block.path[-1] if block and block.path else ""),
            path=block.path if block else (),
            block_kind=block.kind if block else None,
            block_index=block.index if block else None,
            cell=block.cell_at(start) if block and block.cells else None,
            snippet=snippet,
            highlight=highlight,
            score=score,
            clause=clause,
        )

    def _in_scope(clause, block, options):
        if block is None:
            return clause.kinds is None and not options.get("sections")
        if clause.kinds and block.kind not in clause.kinds:
            return False
        for needle in options.get("sections") or ():
            crumbs = _fold_query(
                " ".join(block.path), options["ignore_case"], options["fold"]
            )
            if needle not in crumbs:
                return False
        pages = options.get("pages")
        if pages and block.page not in pages:
            return False
        kinds = options.get("kinds")
        if kinds and block.kind not in kinds:
            return False
        return True

    def _measure(document, clauses, options):
        """Every clause's spans in one document, in DOCUMENT character offsets.

        Matching happens one BLOCK at a time, never over the whole document, and
        that is a contract rather than an optimisation: a phrase must not be able
        to straddle the gap between a heading and the paragraph under it, and
        `NEAR` must not pair a word in a table with a word three blocks away. A
        block IS the unit a fact lives on, so it is the unit a match lives in.
        """
        found = dict((clause.key, []) for clause in clauses)
        words = 0
        for block, folded, origin, starts in document.folded_blocks(
            options["ignore_case"], options["fold"]
        ):
            words += len(starts)
            for clause in clauses:
                if not _in_scope(clause, block, options):
                    continue
                for span in _spans(clause, folded, None, starts):
                    stop = origin[min(span[1], len(origin) - 1)]
                    found[clause.key].append(
                        (block.start + origin[span[0]], block.start + stop, block)
                    )
        return found, words

    def _bm25(tf, df, count, length, average):
        if tf <= 0:
            return 0.0
        k1, b = 1.2, 0.75
        idf = math.log(1.0 + (count - df + 0.5) / (df + 0.5))
        norm = tf * (k1 + 1.0) / (tf + k1 * (1.0 - b + b * (length / (average or 1.0))))
        return idf * norm

    # Sources

    _documents = {}
    _DOCUMENT_CACHE = 64

    def _cached_read(path, format):
        """`read` memoized on (path, mtime, size) for THIS sandbox session.

        The host caches the conversion itself; this also skips re-reading and
        re-encoding the bytes, which is what makes a second question about a
        directory of PDFs instant.
        """
        try:
            stat = os.stat(path)
            key = (os.path.abspath(path), stat.st_mtime_ns, stat.st_size, format)
        except OSError:
            key = None
        if key is not None and key in _documents:
            return _documents[key], True
        document = read(path, format=format, assets=False, blocks=True)
        if key is not None:
            if len(_documents) >= _DOCUMENT_CACHE:
                _documents.pop(next(iter(_documents)))
            _documents[key] = document
        return document, False

    def _load_path(path, format):
        return lambda: _cached_read(path, format)

    def _walk(directory, format, hidden):
        """Every file under `directory` whose extension names a known format.

        The walk stays in the sandbox on purpose — the sandbox's filesystem is
        confined to the configured roots, and this is what keeps a corpus scan
        inside them. Hidden directories are skipped unless `hidden=True`.
        """
        for root, directories, names in os.walk(str(directory)):
            directories[:] = sorted(
                name for name in directories if hidden or not name.startswith(".")
            )
            for name in sorted(names):
                if not hidden and name.startswith("."):
                    continue
                if _extension_format(name) is None:
                    continue
                path = os.path.join(root, name)
                yield path, _load_path(path, format), False

    def _sources(sources, format, hidden, name=None):
        """One document, a few, a mapping of ids, or a directory of many.

        Yields `(id, load, is_explicit)`. Explicit means the caller named this
        document, so its failure is raised; a file merely FOUND under a
        directory is reported as skipped instead of ending the search.

        A mapping key doubles as a NAME: `{"ledger.csv": data}` tells the
        converter what those bytes are, exactly the way a file on disk does.
        """
        if isinstance(sources, Document):
            yield sources.id, (lambda document=sources: (document, True)), True
        elif isinstance(sources, (bytes, bytearray, memoryview)):
            data = bytes(sources)
            hint = name if name and _extension_format(name) else None

            def load(data=data, hint=hint, name=name):
                try:
                    return (
                        to_document(data, format=format, name=hint, assets=False),
                        False,
                    )
                except DocumentError as error:
                    raise DocumentError(
                        "%s\nThese bytes carry no signature naming their format: "
                        "pass format='csv', or name them - "
                        "search(query, {'ledger.csv': data})." % error.message,
                        document_id=name or "document",
                        source="bytes",
                        format=format,
                    )

            yield (name or "document", load, True)
        elif isinstance(sources, str) or hasattr(sources, "__fspath__"):
            path = os.fspath(sources) if hasattr(sources, "__fspath__") else sources
            if os.path.isdir(path):
                for found in _walk(path, format, hidden):
                    yield found
            else:
                yield path, _load_path(path, format), True
        elif hasattr(sources, "items"):
            for name, source in sources.items():
                for _, load, _explicit in _sources(
                    source, format, hidden, name=str(name)
                ):
                    yield str(name), load, True
        elif hasattr(sources, "__iter__"):
            for source in sources:
                for found in _sources(source, format, hidden):
                    yield found
        else:
            raise SourceError(
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

    def _suggest(clauses, documents, misses):
        """`{term: [close words]}` for terms nothing matched — answer a typo."""
        if not misses:
            return {}
        vocabulary = set()
        for document in documents.values():
            folded, _ = document.folded(True, True)
            vocabulary.update(
                word for word in _WORD_RE.findall(folded) if len(word) > 2
            )
            if len(vocabulary) > 60000:
                break
        suggestions = {}
        for term in sorted(misses):
            close = difflib.get_close_matches(term, vocabulary, 3, 0.78)
            if close:
                suggestions[term] = close
        return suggestions

    def search(
        query,
        sources,
        regex=False,
        ignore_case=True,
        fold=True,
        stem=True,
        whole_word=True,
        context=0,
        snippet=160,
        mark=("**", "**"),
        limit=0,
        per_document=0,
        format=None,
        kinds=None,
        pages=None,
        order="score",
        hidden=False,
    ):
        """Ask one document, a few, or a whole corpus a question, with citations.

        `sources` is a path, a directory (walked recursively), raw bytes, a
        `Document`, a list of any of those, or a `{id: source}` mapping when you
        want to name the ids yourself.

        `query` is the little query language documented on `anydoc` itself:
        bare terms are OR, `"a phrase"` crosses line wraps and table cells,
        `+must` / `-must_not` / `AND` / `NOT` filter documents, `rev*` is a
        prefix, `NEAR(a b, 8)` is proximity, `/re/` is a regex and
        `heading: table: code: list: note: section: page:` scope a clause.
        Both sides are FOLDED before matching (ligatures, accents, curly
        quotes, hyphen line-breaks, NBSP, case) unless `fold=False`.

        Results are ranked with BM25 over the corpus, best first, and
        `results.explain()` prints the parse and the ranking. `limit` caps the
        citations RETURNED, never the ones counted: `total_matches` and
        `is_truncated` always tell the truth. `per_document` caps each document
        so one file cannot drown the others.
        """
        text, clauses, filters = _parse_query(
            query, regex, ignore_case, fold, stem, whole_word
        )
        options = {
            "query": text,
            "ignore_case": bool(ignore_case),
            "fold": bool(fold),
            "stem": bool(stem),
            "whole_word": bool(whole_word),
            "regex": bool(regex),
            "context": int(context or 0),
            "snippet": int(snippet or 0),
            "mark": tuple(mark) if mark else ("", ""),
            "kinds": tuple(kinds) if kinds else None,
            # A `page:`/`section:` written INSIDE the query narrows exactly like the
            # keyword argument, and the two intersect rather than fight.
            "pages": tuple(
                sorted(set(int(page) for page in (pages or ())) | filters["pages"])
            )
            or None,
            "sections": tuple(filters["sections"]) or None,
            "converted": 0,
            "words": {},
        }
        documents = {}
        skipped = []
        measured = {}
        lengths = {}
        for source_id, load, is_explicit in _sources(sources, format, hidden):
            try:
                document, was_cached = load()
            except (AnydocError, OSError, ValueError) as error:
                if is_explicit:
                    raise
                skipped.append(Skipped(source_id, str(error)))
                continue
            except Exception as error:  # a corpus is never ended by one bad file
                if is_explicit:
                    raise
                skipped.append(
                    Skipped(source_id, "%s: %s" % (type(error).__name__, error))
                )
                continue
            if not was_cached:
                options["converted"] += 1
            document.id = _unique(documents, source_id)
            documents[document.id] = document
            found, words = _measure(document, clauses, options)
            measured[document.id] = found
            lengths[document.id] = words
            options["words"][document.id] = words

        count = len(documents) or 1
        average = (sum(lengths.values()) / count) if lengths else 1.0
        for clause in clauses:
            hit = sum(1 for found in measured.values() if found.get(clause.key))
            clause.idf = math.log(1.0 + (count - hit + 0.5) / (hit + 0.5))

        positives = [clause for clause in clauses if clause.is_positive]
        required = [clause for clause in clauses if clause.occur == "must"]
        excluded = [clause for clause in clauses if clause.occur == "must_not"]
        ranking = []
        per_doc_citations = {}
        total = 0
        for document_id, found in measured.items():
            if any(not found.get(clause.key) for clause in required):
                continue
            if any(found.get(clause.key) for clause in excluded):
                continue
            document = documents[document_id]
            score = 0.0
            citations = []
            # Score the PASSAGE, not only the document — Elastic's unified
            # highlighter picks a fragment by scoring it, and so do we: a block
            # that answers the WHOLE query outranks a heading that answers half
            # of it, however loudly headings are boosted.
            weight = {}
            for clause in positives:
                for span in found.get(clause.key) or ():
                    weight.setdefault(span[2].index, {})[clause.key] = clause.idf + 0.1
            for clause in positives:
                spans = found.get(clause.key) or []
                score += _bm25(
                    len(spans),
                    max(
                        sum(1 for other in measured.values() if other.get(clause.key)),
                        1,
                    ),
                    count,
                    lengths[document_id],
                    average,
                )
                for span in spans:
                    boost = _KIND_BOOST.get(span[2].kind, 1.0)
                    matched = weight.get(span[2].index, {})
                    coverage = len(matched) / max(len(positives), 1)
                    citations.append(
                        _citation(
                            document,
                            clause,
                            span,
                            options,
                            sum(matched.values()) * boost * (0.5 + coverage),
                        )
                    )
            if not citations and not positives:
                continue
            total += len(citations)
            citations.sort(key=lambda citation: (-citation.score, citation.offset))
            if per_document:
                citations = citations[: int(per_document)]
            per_doc_citations[document_id] = citations
            ranking.append((document_id, score))

        ranking.sort(key=lambda entry: (-entry[1], entry[0]))
        flat = []
        for document_id, _ in ranking:
            flat.extend(per_doc_citations.get(document_id, ()))
        if order == "document":
            flat.sort(key=lambda citation: (citation.document_id, citation.offset))
        else:
            flat.sort(
                key=lambda citation: (
                    -citation.score,
                    citation.document_id,
                    citation.offset,
                )
            )
        shown = flat[: int(limit)] if limit else flat
        misses = set()
        for clause in positives:
            if clause.kind in ("term", "phrase") and not any(
                found.get(clause.key) for found in measured.values()
            ):
                misses.update(clause.roots)
        return SearchResults(
            text,
            clauses,
            shown,
            documents,
            skipped,
            total,
            bool(limit) and total > len(shown),
            ranking,
            _suggest(clauses, documents, misses),
            options,
        )

    def explain_query(query, **options):
        """Parse a query and describe it, without reading a single document."""
        text, clauses, filters = _parse_query(
            query,
            options.get("regex", False),
            options.get("ignore_case", True),
            options.get("fold", True),
            options.get("stem", True),
            options.get("whole_word", True),
        )
        lines = ["query: %s" % text] + ["  " + c.describe() for c in clauses]
        for page in sorted(filters["pages"]):
            lines.append("  filter: page %d only" % page)
        for section in filters["sections"]:
            lines.append("  filter: under a heading matching %r" % section)
        return "\n".join(lines)

    def __getattr__(name):
        # Lazy so importing the module costs no host call at all.
        if name == "FORMATS":
            value = formats()
            mod.FORMATS = value
            return value
        raise AttributeError("module 'anydoc' has no attribute %r" % name)

    mod = types.ModuleType("anydoc")
    mod.__doc__ = """Any document as Markdown, and any question about it as citations.

Reading:

    anydoc.to_markdown("q1.pdf")                 -> str
    anydoc.to_markdown(raw, name="q1.pdf")       -> str
    doc = anydoc.read("q1.pdf")                  -> Document
    doc.markdown / doc.text / doc.blocks / doc.pages / doc.assets
    doc.outline()                                -> the headings, indented

Asking:

    hits = anydoc.search("March", "/data/reports")
    for c in hits:
        print(c)          # q1.pdf p.7 line 12 > Revenue: ...March broke...

A document is a path, raw bytes or an open binary file, at every reading door:
to_markdown, to_document, read and detect take any of the three. Bytes carrying
no signature of their own need name="ledger.csv" or format="csv" to be read.

`sources` is a path, a directory (walked), bytes, a Document, a list of any of
those, or a {id: source} mapping to name the ids yourself.

Query language — each line below is a WHOLE query, and `explain_query` parses
one without reading a file:

    march revenue           bare terms, ANY may match (OR), ranked by BM25
    "quarterly revenue"     a phrase — crosses line wraps AND table cells
    revenue +march          AND: the document MUST contain `march`
    revenue -draft          NOT: the document must NOT contain `draft`
    rev*                    prefix
    NEAR(revenue march, 8)  within 8 words of each other
    /reven[us]e?/           a regular expression over folded text
    heading:march           headings only; also table: list: code: note: paragraph:
    revenue section:Revenue under that heading only
    revenue page:3          on that page only

`+`/`-` may be spelled `AND`/`NOT`. A query needs at least one thing to look
FOR: `-draft` or `page:3` alone is refused, and says so.

Both the corpus and the query are FOLDED before matching, so `efficient` finds
the ligature in a PDF, `Zurich` finds `Zürich`, `HAUPTSTRASSE` finds
`Hauptstraße`, `don't` finds a Word curly apostrophe, `quarterly revenue` finds
it wrapped over two lines, `quarterly` finds `quar-\\nterly`, and `payments`
finds `payment`. `fold=False`, `stem=False`, `ignore_case=False` and
`whole_word=False` each turn one of those off.

Every hit is a Citation: `.document_id .page .section .line .column .offset
.snippet .highlight .score .text .match .block_kind .cell .before .after`.
Results are ranked, and `.total_matches` / `.is_truncated` never lie about a
`limit`. `results.explain()` prints the parse and the ranking; `.suggestions`
answers a typo; `.skipped` names files that could not be read.

Conversions are cached on the content hash (`anydoc.cache_info()`), so
`doc.search(...)` and a second question about the same corpus convert nothing.

Errors are typed and catchable: QueryError (points at the character),
DocumentError (carries `.document_id`), SourceError — all AnydocError, and all
also the plain builtin (ValueError / TypeError) you would have caught."""
    mod.__version__ = "vis"
    mod.__all__ = [
        "AnydocError",
        "Asset",
        "Block",
        "Cell",
        "Citation",
        "Document",
        "DocumentError",
        "QueryError",
        "SearchResults",
        "Skipped",
        "SourceError",
        "cache_info",
        "clear_cache",
        "detect",
        "explain_query",
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
    mod.Block = Block
    mod.Cell = Cell
    mod.Citation = Citation
    mod.Document = Document
    mod.DocumentError = DocumentError
    mod.QueryError = QueryError
    mod.SearchResults = SearchResults
    mod.Skipped = Skipped
    mod.SourceError = SourceError
    mod.cache_info = cache_info
    mod.clear_cache = clear_cache
    mod.detect = detect
    mod.explain_query = explain_query
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
