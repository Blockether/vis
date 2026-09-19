"""Shared building blocks for the language surfaces Vis ships.

Vis carries no parser of its own. Before a patch is written, the write gate asks
the language surface registered for that file's language whether the edit still
parses, so every verdict here is a plain `syntax_result` document: the language,
whether the source is clean, and one located finding per fault.

A finding names a 1-based `line`, a 0-based `col` and one of the four kinds the
language-surface contract accepts: `unclosed`, `unexpected`, `missing` or
`parse`. A verdict that breaks the contract is discarded by the gate and leaves
the language unguarded, so these helpers build the shape once.
"""

KINDS = ("unclosed", "unexpected", "missing", "parse")


def finding(kind, line, col, **extra):
    """One located fault. Positions are clamped to the range the contract allows.

    Args:
        kind: `"unclosed"`, `"unexpected"`, `"missing"` or `"parse"`.
        line: 1-based line of the fault.
        col: 0-based column of the fault.
        **extra: Optional `delimiter`, `expected`, `message`, `text`, `end_line`
            or `end_col` detail; a `None` value is dropped.

    Returns:
        The finding dict.

    Raises:
        ValueError: The kind is not one the contract accepts.
    """
    if kind not in KINDS:
        raise ValueError(f"unknown syntax finding kind: {kind!r}")
    row = {"line": max(int(line), 1), "col": max(int(col), 0), "kind": kind}
    row.update({key: value for key, value in extra.items() if value is not None})
    return row


def verdict(language, findings=(), *, validator=None, is_exact=True):
    """The `syntax_result` document the write gate reads.

    Args:
        language: Language the verdict is about.
        findings: Located faults; an empty sequence means the source is clean.
        validator: Name of the parser that decided, for the transcript.
        is_exact: Whether the parser decides the language exactly rather than
            approximating it.

    Returns:
        The verdict dict.
    """
    rows = list(findings)
    result = {
        "language": language,
        "is_clean": not rows,
        "findings": rows,
        "is_exact": bool(is_exact),
    }
    if validator:
        result["validator"] = validator
    return result


def source_of(request):
    """The source text of a `syntax` request, empty when it carries none."""
    if isinstance(request, dict):
        return str(request.get("source") or "")
    return str(request or "")


def language_of(request, default):
    """The language a `syntax` request names, or `default` when it names none."""
    if isinstance(request, dict):
        named = request.get("language")
        if named:
            return str(named)
    return default
