"""Exact syntax verdicts for the data languages the standard library parses.

JSON and TOML are decided by `json.loads` and `tomllib.loads`: the same parsers
the rest of the toolchain uses, so a verdict here never disagrees with the file
a build actually reads.
"""

import json as _json
import re

from . import finding, language_of, source_of, verdict

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - tomllib is standard from 3.11
    tomllib = None

HAS_TOML = tomllib is not None

_TOML_POSITION = re.compile(r"at line (\d+), column (\d+)")


def json_syntax(request):
    """Verdict for one JSON document.

    Args:
        request: The gate's `{"language", "source"}` request.

    Returns:
        A `syntax_result` with one `parse` finding at the decoder's own position.
    """
    source = source_of(request)
    language = language_of(request, "json")
    try:
        _json.loads(source)
    except _json.JSONDecodeError as exc:
        row = finding("parse", exc.lineno, max(exc.colno - 1, 0), message=exc.msg)
        return verdict(language, [row], validator="json.loads")
    except ValueError as exc:
        return verdict(
            language, [finding("parse", 1, 0, message=str(exc))], validator="json.loads"
        )
    return verdict(language, (), validator="json.loads")


def toml_syntax(request):
    """Verdict for one TOML document, decided by `tomllib`.

    Args:
        request: The gate's `{"language", "source"}` request.

    Returns:
        A `syntax_result` with one `parse` finding when the document is invalid.

    Raises:
        RuntimeError: This interpreter ships no `tomllib`, so nothing judged the
            source; the gate leaves TOML unguarded rather than guessing.
    """
    if tomllib is None:  # pragma: no cover - tomllib is standard from 3.11
        raise RuntimeError("tomllib is unavailable in this interpreter")
    source = source_of(request)
    language = language_of(request, "toml")
    try:
        tomllib.loads(source)
    except tomllib.TOMLDecodeError as exc:
        line, col = _toml_position(exc)
        return verdict(
            language,
            [finding("parse", line, col, message=str(exc))],
            validator="tomllib",
        )
    except ValueError as exc:
        return verdict(
            language, [finding("parse", 1, 0, message=str(exc))], validator="tomllib"
        )
    return verdict(language, (), validator="tomllib")


def _toml_position(exc):
    """1-based line and 0-based column of a TOML error, from the exception or its text."""
    line = getattr(exc, "lineno", None)
    col = getattr(exc, "colno", None)
    if isinstance(line, int) and isinstance(col, int):
        return line, max(col - 1, 0)
    found = _TOML_POSITION.search(str(exc))
    if found:
        return int(found.group(1)), max(int(found.group(2)) - 1, 0)
    return 1, 0
