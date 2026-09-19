"""Whether a delimiter repair may be written over the lines an edit wrote.

An editor refusal is information: the caller learns the replacement did not fit
where it aimed it, and re-reads. A repair that runs on the REPLACEMENT ALONE
cannot know that — it balances a fragment against nothing, so a partial line
(`[{:keys [a b]}`, deliberately open because its enclosing form closes it) comes
back "repaired" into a whole form, the splice accepts it, and the file parses
while meaning something else.

So the repair runs on the WHOLE spliced file, where the enclosing forms and the
caller's own indentation decide where a delimiter belongs, and it is accepted
only when it is provably confined to the lines the caller wrote:

  1. the repaired file parses clean;
  2. it keeps the same number of lines and the same final newline;
  3. every line it changes lies inside an edited span;
  4. it only ADDED delimiters the caller omitted — one they WROTE is never
     deleted, moved or retyped — and every other character, whitespace and line
     ending included, is theirs, in order.

Where the text a line REPLACED is known it is the strictest licence of all: on a
line whose code survived the edit, every delimiter it dropped goes back exactly
where that text had it (`reseat`), and a repair that instead invents one there is
refused. When neither witness can say where, the closers the edit never wrote are
appended at the end of the last line it wrote (`closed_at_tail`), and only for a
call that wrote one region.

This module decides; it does not repair. The repair itself is a balancer,
`str -> str | None`, and `vis_language_surface.parinfer` is the one the Clojure
surface hands it.
"""

import re

DELIMITERS = "()[]{}"
OPENERS = "([{"
ALIGN_MAX_CELLS = 1000000


def split_lines(source):
    """The lines of `source`, without endings and without trailing blanks.

    Args:
        source: Any text.

    Returns:
        A list of lines, matching Clojure's `clojure.string/split-lines`.
    """
    parts = re.split(r"\r?\n", source)
    while parts and parts[-1] == "":
        parts.pop()
    return parts


def terminated_lines(source):
    """The lines of `source`, each keeping its own line ending.

    Args:
        source: Any text.

    Returns:
        A list of lines; joining them restores `source` exactly.
    """
    return re.findall(r"[^\n]*\n|[^\n]+", source)


def line_ending(line):
    """The line ending `line` carries, if any.

    Args:
        line: One line, possibly terminated.

    Returns:
        `"\r\n"`, `"\n"` or `""`.
    """
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def skeleton(source):
    """`source` with every delimiter and every space removed.

    Args:
        source: Any text.

    Returns:
        The code a repair may not touch: two texts with the same skeleton differ
        only in delimiters and whitespace.
    """
    return "".join(c for c in source if not c.isspace() and c not in DELIMITERS)


def delimiters(source):
    """Every `()[]{}` character of `source`, in order.

    Args:
        source: Any text.

    Returns:
        A string of delimiters.
    """
    return "".join(c for c in source if c in DELIMITERS)


def undelimited(source):
    """`source` with its delimiters removed and all other characters kept.

    Args:
        source: Any text.

    Returns:
        The whitespace and code a repair may not re-indent.
    """
    return "".join(c for c in source if c not in DELIMITERS)


def surplus(a, b):
    """The characters `a` has more of than `b`.

    Args:
        a: A string of characters.
        b: The string to subtract, as a multiset.

    Returns:
        The extra characters, sorted, as a string.
    """
    counts = {}
    for c in a:
        counts[c] = counts.get(c, 0) + 1
    for c in b:
        if c in counts:
            counts[c] -= 1
    return "".join(c * n for c, n in sorted(counts.items()) if n > 0)


def subsequence(a, b):
    """Whether `a` appears in `b` in order, gaps allowed.

    Args:
        a: The sequence that must be preserved.
        b: The sequence to look in.

    Returns:
        True when every element of `a` is found in `b`, in order.
    """
    it = iter(b)
    return all(any(x == y for y in it) for x in a)


def additions_only(before, after):
    """Whether `after` only ADDS to `before`.

    Args:
        before: The caller's own delimiters.
        after: The repaired delimiters.

    Returns:
        True when nothing was deleted, moved or retyped.
    """
    return subsequence(before, after)


def unterminated_string(source):
    """The line that opens a string `source` never closes.

    Args:
        source: Any text.

    Returns:
        A 1-based line number, or None when every string closes.
    """
    line = 1
    opened = None
    in_string = False
    in_comment = False
    escaped = False
    for char in source:
        if escaped:
            escaped = False
            if char == "\n" and not in_string:
                line += 1
            continue
        if in_string:
            if char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
                opened = None
            elif char == "\n":
                line += 1
            continue
        if in_comment:
            in_comment = char != "\n"
            if char == "\n":
                line += 1
            continue
        if char == "\\":
            escaped = True
        elif char == '"':
            in_string = True
            opened = line
        elif char == ";":
            in_comment = True
        elif char == "\n":
            line += 1
    return opened if in_string else None


def open_string_why(source):
    """Why no delimiter repair can be offered for an unterminated string.

    Args:
        source: The source an edit would have written.

    Returns:
        A refusal sentence, or None when every string closes.
    """
    line = unterminated_string(source)
    if line is None:
        return None
    return (
        f"no delimiter repair is possible: line {line} opens a string that is "
        "never closed, and a repair only puts back `()[]{}`"
    )


def open_stack(source):
    """The closers `source` still owes, innermost last.

    Args:
        source: Any text.

    Returns:
        A list of expected closing characters, or None when a closer matches
        nothing or a string is left open.
    """
    stack = []
    in_string = False
    in_comment = False
    escaped = False
    for char in source:
        if escaped:
            escaped = False
            continue
        if in_string:
            if char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue
        if in_comment:
            in_comment = char != "\n"
            continue
        if char == "\\":
            escaped = True
        elif char == '"':
            in_string = True
        elif char == ";":
            in_comment = True
        elif char == "(":
            stack.append(")")
        elif char == "[":
            stack.append("]")
        elif char == "{":
            stack.append("}")
        elif char in DELIMITERS:
            if not stack or stack[-1] != char:
                return None
            stack.pop()
    return None if in_string else stack


def direction_why(source, candidate, subject):
    """Why a repair that is not add-only is refused.

    Args:
        source: The source an edit would have written.
        candidate: The repair offered for it.
        subject: Whose delimiters the refusal is about.

    Returns:
        A refusal sentence naming deletion or relocation.
    """
    source_delimiters = delimiters(source)
    candidate_delimiters = delimiters(candidate)
    if subsequence(candidate_delimiters, source_delimiters):
        return (
            "the delimiter repair would delete `"
            + surplus(source_delimiters, candidate_delimiters)
            + "` "
            + subject
            + ": it closes more than it opens, or an opener was lost"
        )
    return "the delimiter repair would move or retype a delimiter " + subject


def inside_spans(line, spans):
    """Whether 1-based `line` falls inside one of the caller's spans.

    Args:
        line: A 1-based line number in the new content.
        spans: `[from, to]` pairs, 1-based and inclusive.

    Returns:
        True when the line is one this call wrote.
    """
    return any(int(span[0]) <= line <= int(span[1]) for span in spans or ())


def excerpt(line):
    """One line as the caller should re-read it.

    Args:
        line: The repaired line.

    Returns:
        The line trimmed, and cut when it is too long for a status line.
    """
    trimmed = line.strip()
    return trimmed[:55] + "\u2026" if len(trimmed) > 56 else trimmed


def delimiter_note(line_no, before, after):
    """What the repair did to ONE line, as the caller reads it.

    Args:
        line_no: The 1-based line number.
        before: The line the edit wrote.
        after: The line the repair produced.

    Returns:
        A note such as ``line 3 added `)` → `(defn ok [] (inc 1))` ``.
    """
    added = surplus(delimiters(after), delimiters(before))
    removed = surplus(delimiters(before), delimiters(after))
    note = f"line {line_no}"
    if added:
        note += f" added `{added}`"
    if removed:
        note += f" removed `{removed}`"
    return note + f" \u2192 `{excerpt(after)}`"


def align(a, b):
    """The longest common subsequence of `a` and `b`, as index pairs.

    Args:
        a: A string or a list of comparable elements.
        b: The sequence to align it with.

    Returns:
        A list of `(index in a, index in b)` anchors, or None when either side
        is empty or the table would be too large to fill.
    """
    m = len(a)
    n = len(b)
    if not m or not n or m * n > ALIGN_MAX_CELLS:
        return None
    table = [[0] * (n + 1) for _ in range(m + 1)]
    for i in range(m - 1, -1, -1):
        row = table[i]
        below = table[i + 1]
        for j in range(n - 1, -1, -1):
            row[j] = below[j + 1] + 1 if a[i] == b[j] else max(below[j], row[j + 1])
    anchors = []
    i = 0
    j = 0
    while i < m and j < n:
        if a[i] == b[j]:
            anchors.append((i, j))
            i += 1
            j += 1
        elif table[i + 1][j] >= table[i][j + 1]:
            i += 1
        else:
            j += 1
    return anchors


def lcs_length(a, b):
    """The length of the longest common subsequence of two strings.

    Args:
        a: One string.
        b: The other string.

    Returns:
        The number of shared characters, in order; 0 when the table would be
        too large to fill.
    """
    m = len(a)
    n = len(b)
    if not m or not n or m * n > ALIGN_MAX_CELLS:
        return 0
    row = [0] * (n + 1)
    for i in range(m - 1, -1, -1):
        diagonal = 0
        for j in range(n - 1, -1, -1):
            previous = row[j]
            row[j] = diagonal + 1 if a[i] == b[j] else max(row[j + 1], previous)
            diagonal = previous
    return row[0]


def similar(was, now):
    """Whether two lines are the same code edited, rather than different code.

    Args:
        was: The line the edit replaced.
        now: The line it wrote.

    Returns:
        True when more than half of the longer skeleton is shared.
    """
    a = skeleton(was)
    b = skeleton(now)
    longer = max(len(a), len(b))
    return longer > 0 and 2 * lcs_length(a, b) > longer


def middles(original, source):
    """The lines that differ, with the shared head dropped.

    Args:
        original: The content this edit replaced.
        source: The content it would write.

    Returns:
        `(head, was, now)`: the count of shared leading lines and the two
        middles that differ.
    """
    was = split_lines(original)
    now = split_lines(source)
    head = 0
    while head < len(was) and head < len(now) and was[head] == now[head]:
        head += 1
    tail = 0
    while (
        tail < len(was) - head
        and tail < len(now) - head
        and was[len(was) - 1 - tail] == now[len(now) - 1 - tail]
    ):
        tail += 1
    return head, was[head : len(was) - tail], now[head : len(now) - tail]


def paired_lines(original, source):
    """Which line of the replaced text each written line came from.

    Args:
        original: The content this edit replaced.
        source: The content it would write.

    Returns:
        A list of `{"line", "replaced", "wrote", "same_code"}` rows, one per
        written line whose replaced text is known.
    """
    head, was_mid, now_mid = middles(original, source)
    anchors = align(
        [skeleton(line) for line in was_mid], [skeleton(line) for line in now_mid]
    )
    if anchors is None:
        anchors = []
    indexes = []
    was_at = 0
    now_at = 0
    for was_to, now_to in list(anchors) + [(len(was_mid), len(now_mid))]:
        gap = was_to - was_at
        if gap > 0 and gap == now_to - now_at:
            indexes.extend((was_at + k, now_at + k, False) for k in range(gap))
        if was_to < len(was_mid):
            indexes.append((was_to, now_to, True))
        was_at = was_to + 1
        now_at = now_to + 1
    rows = []
    for was_index, now_index, same_code in indexes:
        replaced = was_mid[was_index]
        wrote = now_mid[now_index]
        if not skeleton(replaced).strip():
            continue
        if not same_code and not similar(replaced, wrote):
            continue
        rows.append(
            {
                "line": head + now_index + 1,
                "replaced": replaced,
                "wrote": wrote,
                "same_code": same_code,
            }
        )
    return rows


def splice(seats, source):
    """`source` with each seated character inserted at its index.

    Args:
        seats: `(index, character)` pairs, in index order.
        source: The line to insert into.

    Returns:
        The line with every seated character in place.
    """
    out = []
    index = 0
    remaining = list(seats)
    while True:
        if remaining and remaining[0][0] == index:
            out.append(remaining.pop(0)[1])
            continue
        if index < len(source):
            out.append(source[index])
            index += 1
            continue
        break
    return "".join(out)


def reseat_line(replaced, wrote):
    """The written line with the delimiters the replaced text had put back.

    Args:
        replaced: The line this edit replaced.
        wrote: The line it wrote instead.

    Returns:
        The reseated line, or None when nothing was dropped from a gap that
        held only delimiters and spaces.
    """
    anchors = align(replaced, wrote)
    if not anchors:
        return None
    seats = []
    was_from = 0
    wrote_from = 0
    for was_at, wrote_at in list(anchors) + [(len(replaced), len(wrote))]:
        gap = replaced[was_from:was_at]
        dropped = [c for c in gap if c in DELIMITERS]
        seat = (
            wrote_from if dropped and all(c in OPENERS for c in dropped) else wrote_at
        )
        if all(c in DELIMITERS or c.isspace() for c in gap):
            seats.extend((seat, c) for c in dropped)
        was_from = was_at + 1
        wrote_from = wrote_at + 1
    return splice(seats, wrote) if seats else None


def reseat(pairs, source):
    """`source` with every paired line's dropped delimiters put back.

    Args:
        pairs: The rows `paired_lines` answered.
        source: The content this edit would write.

    Returns:
        The reseated content, or None when no line could be reseated.
    """
    lines = terminated_lines(source)
    reseated = {}
    for row in pairs or ():
        if row["replaced"] == row["wrote"]:
            continue
        seated = reseat_line(row["replaced"], row["wrote"])
        if seated is not None:
            reseated[row["line"] - 1] = seated
    if not reseated:
        return None
    out = list(lines)
    for index, seated in reseated.items():
        if index < len(out):
            out[index] = seated + line_ending(out[index])
    return "".join(out)


def written_lines(original, source, lines):
    """The 1-based line range this call actually wrote.

    Args:
        original: The content this edit replaced, when there is one.
        source: The content it would write.
        lines: The terminated lines of `source`.

    Returns:
        A `[from, to]` pair.
    """
    if isinstance(original, str):
        head, _was, now_mid = middles(original, source)
        return [head + 1, head + len(now_mid)]
    return [1, len(lines)]


def span_seat(spans, lines, written):
    """The index of the last line this call wrote that can carry a closer.

    Args:
        spans: The caller's `[from, to]` spans.
        lines: The terminated lines of the new content.
        written: The `[from, to]` range the call wrote.

    Returns:
        A 0-based index, or None for a call that wrote more than one region.
    """
    if len(spans or ()) != 1:
        return None
    seat = None
    for index, line in enumerate(lines):
        number = index + 1
        if (
            written[0] <= number <= written[1]
            and inside_spans(number, spans)
            and line.strip()
        ):
            seat = index
    return seat


def append_at(lines, index, text):
    """The document with `text` appended to one line's code.

    Args:
        lines: The terminated lines of the document.
        index: The 0-based line to append to.
        text: The characters to append.

    Returns:
        The whole document, with trailing whitespace and the line ending kept.
    """
    line = lines[index]
    ending = line_ending(line)
    body = line[: len(line) - len(ending)]
    code = body.rstrip()
    out = list(lines)
    out[index] = code + text + body[len(code) :] + ending
    return "".join(out)


def seat_closers(lines, from_line, index, witness):
    """The closers that would close what this call left open, at one line.

    Args:
        lines: The terminated lines of the new content.
        from_line: The 1-based first line this call wrote.
        index: The 0-based line the closers would be appended to.
        witness: Whether the text this edit replaced is known.

    Returns:
        The closing characters, or None when the lines before the call do not
        prefix the lines through it, or nothing closes the file.
    """
    before = open_stack("".join(lines[: from_line - 1]))
    through = open_stack("".join(lines[: index + 1]))
    if before is None or through is None:
        return None
    if not (len(before) < len(through) and before == through[: len(before)]):
        return None

    def closes(count):
        closers = "".join(reversed(through[-count:]))
        remaining = open_stack(append_at(lines, index, closers))
        return closers if remaining == [] else None

    opened = len(through) - len(before)
    if not witness:
        return closes(opened)
    for count in range(1, opened + 1):
        answer = closes(count)
        if answer:
            return answer
    return None


def closed_at_tail(spans, original, source):
    """The new content with the omitted closers at the end of the last line written.

    Args:
        spans: The caller's `[from, to]` spans.
        original: The content this edit replaced, when there is one.
        source: The content it would write.

    Returns:
        The candidate content, or None when there is no single seat for it.
    """
    lines = terminated_lines(source)
    written = written_lines(original, source, lines)
    index = span_seat(spans, lines, written)
    if index is None:
        return None
    missing = seat_closers(lines, written[0], index, isinstance(original, str))
    return append_at(lines, index, missing) if missing else None


def substitution(replaced, wrote):
    """The delimiter this edit typed instead of one the replaced text had.

    Args:
        replaced: The line this edit replaced.
        wrote: The line it wrote.

    Returns:
        `(typed, had)` — the retyped delimiter and the one it stands in for —
        or None when the written line only dropped delimiters.
    """
    a = delimiters(wrote)
    b = delimiters(replaced)
    i = 0
    j = 0
    skipped = None
    while True:
        if i >= len(a):
            return None
        if j >= len(b):
            return (a[i], skipped)
        if a[i] == b[j]:
            i += 1
            j += 1
            skipped = None
        else:
            skipped = skipped or b[j]
            j += 1


def substitution_why(line_no, typed, had, repaired, subject):
    """Why closing a retyped delimiter is refused.

    Args:
        line_no: The 1-based line.
        typed: The delimiter the repair would close.
        had: The delimiter the replaced text had there, if any.
        repaired: The line the repair produced.
        subject: Whose delimiters the refusal is about.

    Returns:
        A refusal sentence.
    """
    where = (
        f", where the text it replaced had `{had}`"
        if had
        else ", one more than the text it replaced has"
    )
    return (
        f"the delimiter repair would close `{typed}` {subject} on line {line_no}{where}"
        ": that delimiter was retyped or added, not omitted, and closing it regroups"
        f" the line into `{excerpt(repaired)}`"
    )


def invention(replaced, repaired):
    """The delimiters a repair added to a line whose code did not change.

    Args:
        replaced: The line this edit replaced.
        repaired: The line the repair produced.

    Returns:
        The invented delimiters, or None when the line only kept what it had.
    """
    had = delimiters(replaced)
    now = delimiters(repaired)
    return None if subsequence(now, had) else surplus(now, had)


def invention_why(line_no, added, replaced):
    """Why a repair on an unchanged line is refused.

    Args:
        line_no: The 1-based line.
        added: The invented delimiters.
        replaced: The line this edit replaced.

    Returns:
        A refusal sentence.
    """
    return (
        f"a delimiter repair exists but it adds `{added}` to line {line_no}, whose code"
        f" this edit did not change — the text it replaced was `{excerpt(replaced)}`"
        " and never had that delimiter, so what this call omitted is on another line"
    )


def changed_lines(before, after):
    """The 1-based lines that differ between two equal-length line lists.

    Args:
        before: The lines the edit wrote.
        after: The lines the repair produced.

    Returns:
        A list of line numbers, or None when the two differ in length.
    """
    if len(before) != len(after):
        return None
    return [i + 1 for i, line in enumerate(before) if line != after[i]]


def verdict(request, candidate):
    """Whether `candidate` may be written in place of the source.

    Args:
        request: The rebalance request, carrying `source`, `spans`, `pairs`,
            `subject` and `parses_clean`.
        candidate: The repair to judge, or None.

    Returns:
        `{"ok": True, "content", "notes"}` for a repair that may be written, or
        `{"ok": False, "why"}` naming what is wrong with it.
    """
    source = request["source"]
    spans = request.get("spans") or ()
    subject = request.get("subject") or "this edit wrote"
    parses_clean = request["parses_clean"]

    if not isinstance(candidate, str) or candidate == source:
        return {
            "ok": False,
            "why": open_string_why(source) or "no delimiter repair was found",
        }
    if not parses_clean(candidate):
        return {
            "ok": False,
            "why": open_string_why(source)
            or "a delimiter repair was found but it still would not parse",
        }
    if skeleton(source) != skeleton(candidate):
        return {
            "ok": False,
            "why": "the delimiter repair would rewrite code, not delimiters",
        }
    if not additions_only(delimiters(source), delimiters(candidate)):
        return {"ok": False, "why": direction_why(source, candidate, subject)}
    if source.endswith("\n") != candidate.endswith("\n"):
        return {
            "ok": False,
            "why": "the delimiter repair would change the file's final newline",
        }

    before = split_lines(source)
    after = split_lines(candidate)
    if len(before) != len(after):
        return {"ok": False, "why": "the delimiter repair would add or drop lines"}

    changed = changed_lines(before, after)
    outside = [line for line in changed if not inside_spans(line, spans)]
    if outside:
        return {
            "ok": False,
            "why": (
                f"a delimiter repair exists but it changes line {outside[0]}, outside the"
                " lines this call edited"
            ),
        }
    if undelimited(source) != undelimited(candidate):
        return {
            "ok": False,
            "why": (
                f"the delimiter repair would change whitespace {subject}: it re-indents or"
                " re-ends lines instead of only putting back the delimiters that were omitted"
            ),
        }

    paired = {
        row["line"]: row for row in request.get("pairs") or () if row["same_code"]
    }
    for line in changed:
        row = paired.get(line)
        if not row:
            continue
        typed = substitution(row["replaced"], row["wrote"])
        if typed:
            return {
                "ok": False,
                "why": substitution_why(
                    line, typed[0], typed[1], after[line - 1], subject
                ),
            }
    for line in changed:
        row = paired.get(line)
        if not row:
            continue
        added = invention(row["replaced"], after[line - 1])
        if added:
            return {"ok": False, "why": invention_why(line, added, row["replaced"])}

    return {
        "ok": True,
        "content": candidate,
        "notes": [
            delimiter_note(line, before[line - 1], after[line - 1]) for line in changed
        ],
    }


def line_offsets(lines):
    """The character offset each line starts at.

    Args:
        lines: The terminated lines of a document.

    Returns:
        A list of offsets, one longer than `lines`.
    """
    offsets = [0]
    for line in lines:
        offsets.append(offsets[-1] + len(line))
    return offsets


def self_contained(source):
    """Whether `source` opens and closes every delimiter it uses.

    Args:
        source: Any text.

    Returns:
        True when nothing is left open and nothing closes too much.
    """
    return open_stack(source) == []


WINDOW_TRIES = 32


def balancer_window(source, lines, offsets, spans):
    """The self-contained window of lines a balancer should be handed.

    Args:
        source: The content this edit would write.
        lines: Its terminated lines.
        offsets: The offset each line starts at.
        spans: The caller's `[from, to]` spans.

    Returns:
        A `(from line index, end line index)` pair, or None when no smaller
        window than the whole file is self-contained on both sides.
    """
    if not spans:
        return None
    count = len(lines)
    first = int(min(span[0] for span in spans)) - 1
    last = int(max(span[1] for span in spans)) - 1

    def own_form(index):
        line = lines[index]
        return bool(line) and not line[0].isspace()

    heads = []
    for index in range(min(first, count - 1), -1, -1):
        if len(heads) >= 2:
            break
        if own_form(index) and self_contained(source[: offsets[index]]):
            heads.append(index)
    ends = []
    for index in range(last + 1, count + 1):
        if len(ends) >= 2:
            break
        if (index == count or own_form(index)) and self_contained(
            source[offsets[index] :]
        ):
            ends.append(index)
    if not heads or not ends:
        return None
    head = heads[-1]
    end = ends[-1]
    return (head, end) if end - head < count else None


def balancer_answer(balancer, source, spans):
    """What the balancer makes of the file, or of the window this call wrote.

    Args:
        balancer: The repair, `str -> str | None`.
        source: The content this edit would write.
        spans: The caller's `[from, to]` spans.

    Returns:
        The repaired content, or None.
    """
    lines = terminated_lines(source)
    offsets = line_offsets(lines)

    def answer(text):
        try:
            return balancer(text)
        except Exception:
            return None

    window = balancer_window(source, lines, offsets, spans)
    if window:
        start = offsets[window[0]]
        stop = offsets[window[1]]
        fixed = answer(source[start:stop])
        return None if fixed is None else source[:start] + fixed + source[stop:]
    return answer(source)


def rebalance(request):
    """Try to make the content an edit would write parse, without reaching past its lines.

    Args:
        request: `balancer`, `parses_clean`, `source`, `original` (the content
            this edit replaced, or None), `spans` and an optional `subject`.

    Returns:
        `{"ok": True, "content", "notes"}` for a repair that may be written,
        `{"ok": False, "why"}` for one that was found and rejected, or None
        when there is no balancer to ask.
    """
    balancer = request.get("balancer")
    if not callable(balancer):
        return None

    source = request["source"]
    original = request.get("original")
    spans = request.get("spans") or ()
    pairs = paired_lines(original, source) if isinstance(original, str) else None
    request = dict(request, pairs=pairs)

    seated = verdict(request, reseat(pairs, source)) if pairs else None
    if seated and seated["ok"]:
        return seated
    asked = verdict(request, balancer_answer(balancer, source, spans))
    if asked["ok"]:
        return asked
    tailed = verdict(request, closed_at_tail(spans, original, source))
    return tailed if tailed["ok"] else asked
