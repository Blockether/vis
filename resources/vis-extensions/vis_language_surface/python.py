"""Exact syntax verdict for Python source, decided by the interpreter's compiler.

`compile` is the same parser that would later run the file, so a verdict here is
never an approximation. The compiler's message also names the delimiter that was
left open or closed too early, which the gate reports instead of a bare count.
"""

import re

from . import finding, language_of, source_of, verdict

_NEVER_CLOSED = re.compile(r"'(.)' was never closed")
_UNMATCHED = re.compile(r"unmatched '(.)'")
_MISMATCHED = re.compile(r"closing parenthesis '(.)' does not match")


def syntax(request):
    """Verdict for one Python source text.

    Args:
        request: The gate's `{"language", "source"}` request.

    Returns:
        A `syntax_result`; a fault carries the compiler's message and, when the
        compiler named one, the delimiter at fault.
    """
    source = source_of(request)
    language = language_of(request, "python")
    try:
        compile(source, "<vis-edit>", "exec", dont_inherit=True)
    except SyntaxError as exc:
        return verdict(language, [_syntax_finding(exc)], validator="compile")
    except ValueError as exc:
        return verdict(
            language, [finding("parse", 1, 0, message=str(exc))], validator="compile"
        )
    return verdict(language, (), validator="compile")


def _syntax_finding(exc):
    """The compiler's SyntaxError as one located finding."""
    message = exc.msg or "syntax error"
    line = exc.lineno or 1
    col = max((exc.offset or 1) - 1, 0)
    end_line = exc.end_lineno if exc.end_lineno and exc.end_lineno >= line else None
    end_col = max(exc.end_offset - 1, 0) if exc.end_offset else None
    detail = {"message": message, "end_line": end_line, "end_col": end_col}
    never_closed = _NEVER_CLOSED.search(message)
    if never_closed:
        return finding("unclosed", line, col, delimiter=never_closed.group(1), **detail)
    unmatched = _UNMATCHED.search(message) or _MISMATCHED.search(message)
    if unmatched:
        return finding("unexpected", line, col, delimiter=unmatched.group(1), **detail)
    return finding("parse", line, col, **detail)
