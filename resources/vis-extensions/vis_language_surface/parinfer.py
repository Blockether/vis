"""Indent-mode delimiter repair for Clojure source, in pure Python.

A model writing Clojure drops a closing delimiter far more often than it writes
one too many, and the indentation it wrote says where the missing one belongs:
a line that starts at or left of the column an opener sits in cannot still be
inside that form, so the form closes at the end of the previous line of code.
That is parinfer's indent mode, and it is the only evidence a repair has.

This repair only ADDS delimiters. A closer nothing opened, a delimiter of the
wrong kind and an unterminated string are all left alone: they read the same as
a LOST OPENER, and guessing between the two rewrites code instead of closing it.
`repair` answers None for every one of them, and the caller keeps its refusal.
"""

OPENERS = {"(": ")", "[": "]", "{": "}"}
CLOSERS = {")": "(", "]": "[", "}": "{"}


def terminated_lines(source):
    """The lines of `source`, each keeping its own line ending.

    Args:
        source: Any text.

    Returns:
        A list of lines; joining them restores `source` exactly.
    """
    lines = source.splitlines(keepends=True)
    return lines or [""]


def repair(source):
    """Clojure source with the delimiters its indentation says are missing.

    Args:
        source: Clojure source text.

    Returns:
        The repaired text, `source` itself when nothing is open, or None when
        the fault is not an omitted closer.
    """
    if not isinstance(source, str) or not source:
        return None

    lines = terminated_lines(source)
    stack = []
    pending = {}
    code_end = [0] * len(lines)
    last_code = None
    state = "code"
    escaped = False

    for index, line in enumerate(lines):
        if state == "code":
            stripped = line.lstrip(" \t")
            indent = len(line) - len(stripped)
            head = stripped[:1]
            if (
                head not in ("", "\n", "\r", ";", ")", "]", "}")
                and last_code is not None
            ):
                while stack and stack[-1][1] >= indent:
                    pending.setdefault(last_code, []).append(stack.pop()[0])

        for col, char in enumerate(line):
            if state == "code":
                following = line[col + 1] if col + 1 < len(line) else ""
                if char == ";" or (char == "#" and following == "!"):
                    state = "comment"
                    continue
                if not char.isspace():
                    code_end[index] = col + 1
                    last_code = index
                if char == '"':
                    state = "string"
                elif char == "\\":
                    state = "character"
                elif char in OPENERS:
                    stack.append((OPENERS[char], col))
                elif char in CLOSERS:
                    if not stack or stack[-1][0] != char:
                        return None
                    stack.pop()
            elif state == "comment":
                if char == "\n":
                    state = "code"
            elif state == "string":
                code_end[index] = col + 1
                if escaped:
                    escaped = False
                elif char == "\\":
                    escaped = True
                elif char == '"':
                    state = "code"
            elif state == "character":
                code_end[index] = col + 1
                state = "code"

    if state in ("string", "character"):
        return None
    if stack and last_code is None:
        return None
    for closer, _ in reversed(stack):
        pending.setdefault(last_code, []).append(closer)
    if not pending:
        return source

    repaired = list(lines)
    for index, closers in pending.items():
        line = repaired[index]
        at = code_end[index]
        repaired[index] = line[:at] + "".join(closers) + line[at:]
    return "".join(repaired)
