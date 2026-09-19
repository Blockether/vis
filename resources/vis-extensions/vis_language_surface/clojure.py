"""Delimiter and literal verdict for Clojure source, in pure Python.

The write gate does not need a Clojure reader; what breaks an edit is an
unbalanced delimiter or an unterminated literal. This scanner walks the text
once and honors every lexical rule that decides whether a bracket counts: line
comments, strings, regex literals, character literals and the reader macros that
only prefix a form (`#{`, `#(`, `#?(`, `#_`, `'`, `~`, `@`, `^`).

It is deliberately not exact: it judges delimiters and literals, not whether a
form reads. The surface declares `is_exact_syntax=False` for that reason, and a
file it calls clean can still fail the Clojure reader for another reason.
"""

from . import finding, language_of, source_of, verdict

OPENERS = {"(": ")", "[": "]", "{": "}"}
CLOSERS = {")": "(", "]": "[", "}": "{"}


def syntax(request):
    """Verdict for one Clojure, ClojureScript or EDN source text.

    Args:
        request: The gate's `{"language", "source"}` request.

    Returns:
        A `syntax_result` whose findings name the delimiter at fault and the
        position where it opened.
    """
    source = source_of(request)
    language = language_of(request, "clojure")
    return verdict(
        language, scan(source), validator="clojure-delimiters", is_exact=False
    )


def scan(source):
    """Every delimiter or literal fault in `source`, in document order.

    Args:
        source: Clojure source text.

    Returns:
        A list of findings: `unexpected` for a closer nothing opened, `unclosed`
        for an opener or string still open at the end of the file.
    """
    findings = []
    stack = []
    state = "code"
    string_start = None
    escaped = False
    line = 1
    col = 0
    index = 0
    length = len(source)

    while index < length:
        char = source[index]
        following = source[index + 1] if index + 1 < length else ""

        if state == "code":
            if char == ";" or (char == "#" and following == "!"):
                state = "comment"
            elif char == '"':
                state = "string"
                string_start = (line, col)
            elif char == "\\":
                state = "character"
            elif char in OPENERS:
                stack.append((char, line, col))
            elif char in CLOSERS:
                if stack and stack[-1][0] == CLOSERS[char]:
                    stack.pop()
                else:
                    findings.append(
                        finding(
                            "unexpected",
                            line,
                            col,
                            delimiter=char,
                            message=f"unexpected '{char}'",
                        )
                    )
        elif state == "comment":
            if char == "\n":
                state = "code"
        elif state == "string":
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                state = "code"
                string_start = None
        elif state == "character":
            # The backslash plus one character is the whole literal; a multi-character
            # name (`\newline`, `\u0041`) continues as an ordinary symbol.
            state = "code"

        if char == "\n":
            line += 1
            col = 0
        else:
            col += 1
        index += 1

    if state == "string" and string_start:
        findings.append(
            finding(
                "unclosed",
                string_start[0],
                string_start[1],
                delimiter='"',
                expected='"',
                message="unterminated string",
            )
        )
    elif state == "character":
        findings.append(
            finding(
                "parse",
                line,
                col,
                message="a character literal is missing its character",
            )
        )

    for opener, open_line, open_col in stack:
        findings.append(
            finding(
                "unclosed",
                open_line,
                open_col,
                delimiter=opener,
                expected=OPENERS[opener],
                message=f"unclosed '{opener}', expected '{OPENERS[opener]}'",
            )
        )

    findings.sort(key=lambda row: (row["line"], row["col"]))
    return findings
