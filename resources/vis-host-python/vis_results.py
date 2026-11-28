"""Model-facing views of Vis results. Mappings and JSON keep every original field.

Only presentation lives here. The runtime still owns result identity, shell handles,
paging and execution. Installation changes the session-local result classes, never
builtins.dict or a process-global runtime class. Unknown operations keep dict repr
at the top level. Use the variable holding a result to inspect keys, or pass it
to dict()/json.dumps().
"""

from collections import Counter
from reprlib import recursive_repr


def _bounded(value, source, limit):
    text = str(value).rstrip("\n")
    if len(text) <= limit:
        return text
    half = limit // 2
    return (
        text[:half]
        + f"\n… {len(text) - 2 * half} chars omitted; full field on this result: {source}\n"
        + text[-half:]
    )


def _duration(ms):
    return f"{ms:g}ms" if ms < 1000 else f"{ms / 1000:.3g}s"


def _details(result):
    return [
        f"{key}: {_bounded(result[key], f'[{key!r}]', 600)}"
        for key in ("error", "hint", "note", "warning")
        if result.get(key)
    ]


def _shell(result):
    parts = [f"shell {result['id']}: {result.get('status') or 'unknown'}"]
    if result.get("exit") is not None:
        parts.append(f"exit={result['exit']}")
    if result.get("duration_ms") is not None:
        parts.append(_duration(result["duration_ms"]))
    if result.get("timed_out"):
        waiting = result.get("stage") == "wait"
        parts.append("wait timed out" if waiting else "timed_out")
    lines = ["; ".join(parts)]
    if result.get("out"):
        lines.append(_bounded(result["out"], "['out']", 4000))
    lines.extend(_details(result))
    if result.get("out_omitted_chars"):
        lines.append(
            f"{result['out_omitted_chars']} log chars omitted by the host; "
            "call .logs(offset=0) on the saved shell handle, "
            "or read ['log_path'] on this result."
        )
    if result.get("is_eof") is False:
        lines.append(
            f"Continue log: call .logs(offset={result.get('next_offset', 0)}) "
            "on the saved shell handle."
        )
    return "\n".join(lines)


def _one_line(value, limit):
    text = " ".join(str(value or "").split())
    return text if len(text) <= limit else text[: limit - 1] + "…"


def _quoted(value, limit):
    """The exact text in quotes, or its start with the full length when it is long."""
    text = str(value or "")
    if len(text) <= limit and "\n" not in text:
        return '"' + text.replace("\\", "\\\\").replace('"', '\\"') + '"'
    return f"{_one_line(text, limit)} ({len(text)} chars)"


def _short_preview(value, limit):
    text = _one_line(value, limit).encode("utf-8", errors="replace")
    if len(text) <= limit:
        return text.decode("utf-8")
    return text[: limit - 3].decode("utf-8", errors="ignore") + "…"


def _child_line(child):
    outcome = child.get("outcome")
    status = child.get("status", "unknown")
    state = f"{status} ({outcome})" if outcome else status
    title = _one_line(child.get("title"), 60)
    result = _one_line(child.get("result_headline"), 120)
    text = (
        f"{child.get('id')} {child.get('role', '')}: {state} {title} {result}".rstrip()
    )
    return f"{text} [{child['session_id']}]"


def _council_row(entry):
    turn = f"t{entry['turn']}, " if entry.get("turn") else ""
    state = f" ({turn}{entry['state']})" if entry.get("state") else ""
    return (
        f"Council #{entry.get('entry_id')} from {entry.get('author_session_id')}{state}: "
        + _quoted(entry.get("content"), 100)
    )


def _routes(steps):
    routes = []
    for step in steps:
        actual = step.get("model")
        selected = step.get("selected_model")
        if not actual:
            continue
        route = (
            f"selected {selected}, actual {actual} (fallback)"
            if selected and selected != actual
            else actual
        )
        if route not in routes:
            routes.append(route)
    return routes


def _turn_detail(turn):
    lines = [f"Turn {turn['position']}: {turn.get('outcome') or turn.get('status')}"]
    if turn.get("user_request"):
        lines.append("Request: " + _one_line(turn["user_request"], 100))
    elif turn.get("request_kind") == "council":
        council = turn.get("council") or {}
        entries = council.get("entries") or ([council] if council else [])
        noun = "entry" if len(entries) == 1 else "entries"
        lines.append(f"Request: Council, {len(entries)} {noun}")
        lines.extend("Request: " + _council_row(entry) for entry in entries[:4])
    if turn.get("answer"):
        lines.append("Answer: " + _one_line(turn["answer"], 480))
        if turn.get("answer_next_offset") is not None:
            lines.append(f"Next answer offset: {turn['answer_next_offset']}")
    steps = turn.get("iterations", [])
    step_lines = [
        f"i{step['position']} {step.get('status')} ({step['block_count']} blocks)"
        + (f": {_one_line(step['error'], 48)}" if step.get("error") else "")
        for step in steps
    ]
    if step_lines:
        lines.append("Steps: " + _one_line(", ".join(step_lines), 150))
    routes = _routes(steps)
    if routes:
        lines.append("Route: " + _one_line("; ".join(routes), 150))
    return lines


def _tokens(tokens):
    return (
        f"input {tokens.get('input', 0)} (cached {tokens.get('cached', 0)}, "
        f"uncached {tokens.get('uncached', 0)}), output {tokens.get('output', 0)}"
    )


def _route(route):
    return f"{route.get('provider')}/{route.get('model')}"


def _usage(usage):
    totals = usage.get("totals") or {}
    lines = [
        f"Tokens: {_tokens(totals.get('tokens') or {})}; "
        f"iterations {totals.get('iterations', 0)}; cost ${totals.get('cost_usd', 0):g}"
    ]
    routing = usage.get("routing") or {}
    actual = ", ".join(_route(row) for row in (routing.get("actual") or [])[:3])
    if actual:
        lines.append(
            f"Route: {actual}; fallbacks {routing.get('fallbacks', 0)}, "
            f"retries {routing.get('retries', 0)}"
        )
    lines.extend(
        f"Fallback: {_route(row['from'])} -> {_route(row['to'])} x{row.get('count', 1)}"
        for row in (routing.get("transitions") or [])[:3]
    )
    if totals.get("tool_errors"):
        lines.append(
            f"Tool errors: {totals['tool_errors']} of {totals.get('tool_calls', 0)} calls"
        )
    turns = usage.get("turns") or []
    if len(turns) > 1:
        lines.extend(
            f"t{row.get('position')}: {_tokens(row.get('tokens') or {})}"
            for row in turns[:6]
        )
    return lines


_EVENT_HIDDEN = ("seq", "type", "ts", "session_id", "turn_id", "started_at", "detached")


def _events(result):
    events = result.get("events") or []
    coverage = result.get("coverage") or {}
    state = "complete" if coverage.get("is_complete") else "incomplete"
    lines = [f"Events: {len(events)}; journal {state}"]
    for event in events[:12]:
        facts = " ".join(
            f"{key}={_one_line(value, 40)}"
            for key, value in event.items()
            if key not in _EVENT_HIDDEN
            and value is not None
            and not isinstance(value, (dict, list))
        )
        lines.append(_one_line(f"{event.get('seq')} {event.get('type')} {facts}", 140))
    if len(events) > 12:
        lines.append(f"… {len(events) - 12} more events: ['events'] on this result.")
    return lines


def _blocks(transcript):
    rows = [
        (turn, step, block)
        for turn in transcript.get("turns") or []
        for step in turn.get("iterations") or []
        for block in step.get("blocks") or []
    ]
    lines = []
    for turn, step, block in rows[:6]:
        number = block.get("position", 0) + 1
        state = "error" if block.get("error") else "ok"
        chars = block.get("stdout_chars", len(block.get("stdout") or ""))
        lines.append(
            f"t{turn.get('position')}/i{step.get('position')}/b{number} "
            f"{block.get('tool_name') or 'block'} {state}; stdout {chars} chars"
        )
        if block.get("error"):
            lines.append("error: " + _one_line(block["error"], 160))
        if block.get("stdout"):
            if len(rows) == 1:
                lines.append(_bounded(block["stdout"], "the block's ['stdout']", 600))
            else:
                lines.append("  " + _one_line(block["stdout"], 80))
        if block.get("stdout_next_offset") is not None:
            lines.append(f"Next stdout offset: {block['stdout_next_offset']}")
    if len(rows) > 6:
        lines.append(
            f"… {len(rows) - 6} more blocks; select one with iteration=K, block=B."
        )
    return lines


def _session(result):
    session = result.get("session") or {}
    ident = result.get("session_id") or session.get("id") or "?"
    view = result.get("view") or "full"
    if not session and not result.get("transcript"):
        return f"read_session {ident}: not found or unavailable" + (
            "\n" + "\n".join(_details(result)) if result.get("error") else ""
        )
    title = _one_line(session.get("title") or "untitled", 82)
    turn = result.get("turn")
    scope = f"turn {turn['position']}" if turn and view == "index" else view
    lines = [f"read_session {ident} ({scope}): {title}"]
    current = result.get("current_turn") or {}
    if current.get("status"):
        lines.append(f"Current turn: {current['status']}")
    if turn:
        lines.extend(_turn_detail(turn))
    for row in result.get("turns", []):
        status = _short_preview(row.get("outcome") or row.get("status"), 18)
        request = _short_preview(
            row.get("request_preview") or row.get("request_kind"), 36
        )
        if row.get("request_kind") == "council" and row.get("request_preview"):
            request = f"council: {request}"
        answer = _short_preview(row.get("answer_preview"), 42)
        lines.append(
            f"t{row['position']} {status}: {request}"
            + (f" | {answer}" if answer else "")
        )
    if not turn:
        lines.extend(_child_line(child) for child in result.get("children", []))
    council = result.get("council") or {}
    if not turn:
        lines.extend(
            _council_row(entry) for entry in (council.get("entries") or [])[:8]
        )
    if result.get("usage"):
        lines.extend(_usage(result["usage"]))
    if "events" in result:
        lines.extend(_events(result))
    if view == "blocks":
        lines.extend(_blocks(result.get("transcript") or {}))
    diagnosis = result.get("diagnosis") or {}
    if (
        view == "diagnosis"
        or diagnosis.get("failure_count")
        or diagnosis.get("hung_turns")
    ):
        lines.append(
            f"Failures: {diagnosis.get('failure_count', 0)}; "
            f"hung turns: {len(diagnosis.get('hung_turns') or [])}"
        )
        for row in (diagnosis.get("failures") or [])[:5]:
            place = f"i{row['iteration']} " if row.get("iteration") else ""
            kind = f"{row['classification']}: " if row.get("classification") else ""
            lines.append(_one_line(f"{place}{kind}{row.get('message')}", 160))
    lines.extend(_details(result))
    if result.get("next_offset") is not None:
        lines.append(f"Next offset: {result['next_offset']}")
    fields = (key for key, value in result.items() if value is not None)
    lines.append("Fields: " + ", ".join(fields))
    return "\n".join(lines)


def _reply_counts(replies):
    counts = Counter(reply["state"] for reply in replies)
    return ", ".join(f"{state}={count}" for state, count in sorted(counts.items()))


def _council_entry(entry, source):
    if "entry_id" in entry:
        heading = f"Entry #{entry['entry_id']}; thread #{entry['thread_id']}"
    else:
        heading = f"Thread #{entry['thread_id']}"
    lines = [heading]
    for key in ("author_session_id", "group_id", "title"):
        if entry.get(key):
            lines.append(f"{key}: {_bounded(entry[key], f'{source}[{key!r}]', 160)}")
    if entry.get("ping"):
        lines.append(
            f"Ping: {len(entry['ping'])} recipients; "
            + _bounded(entry["ping"], f"{source}['ping']", 300)
        )
    if "reply_required" in entry:
        lines.append(f"reply_required={entry['reply_required']}")
    if entry.get("reply_to") is not None:
        lines.append(f"reply_to=#{entry['reply_to']}")
    replies = entry.get("replies") or []
    if replies:
        lines.append("Replies: " + _reply_counts(replies))
        for index, reply in enumerate(replies[:3]):
            recipient = _bounded(
                reply["session_id"], f"{source}['replies'][{index}]['session_id']", 80
            )
            state = f"  {reply['state']} {recipient}"
            if reply.get("reply_entry_id") is not None:
                state += f"; reply_entry_id={reply['reply_entry_id']}"
            lines.append(state)
        if len(replies) > 3:
            lines.append(
                f"… {len(replies) - 3} recipient states omitted; full replies: {source}['replies']."
            )
    if entry.get("content"):
        lines.append(_bounded(entry["content"], f"{source}['content']", 600))
        if (
            entry.get("entry_id") is not None
            and len(str(entry["content"]).rstrip("\n")) > 600
        ):
            lines.append(
                f"To read full content without the original variable: "
                f"entry = await council.get({entry['entry_id']}); "
                "print(entry['content'])."
            )
    return lines


def _council(result):
    op = result["op"]
    if "entries" in result:
        entries = result["entries"]
        lines = [
            f"{op}: {len(entries)} entries; after={result['after']}; has_more={result['has_more']}"
        ]
        required = sum(bool(entry.get("reply_required")) for entry in entries)
        replies = [reply for entry in entries for reply in (entry.get("replies") or [])]
        if required or replies:
            lines.append(
                f"Page reply_required=True: {required}; replies: {_reply_counts(replies) or 'none'}."
            )
        for index, entry in enumerate(entries[:5]):
            lines.extend(_council_entry(entry, f"['entries'][{index}]"))
        if len(entries) > 5:
            key = "thread_id" if op == "council.threads" else "entry_id"
            identities = ", ".join(f"#{entry[key]}" for entry in entries[5:])
            lines.append(
                f"… {len(entries) - 5} more entries ({key}: {identities}); "
                "full messages and reply states: ['entries'] on this result."
            )
        if result["has_more"]:
            lines.append(
                f"Next page: {op}(after={result['after']}) with the same filters."
            )
    elif "entry_id" in result:
        lines = [op] + _council_entry(result, "")
    elif result.get("error"):
        lines = [f"{op}: error"]
    else:
        return dict.__repr__(result)
    lines.extend(_details(result))
    lines.append(
        "Full data: field paths above start from this result; use the variable holding it. "
        "To retrieve an entry, call council.get with its displayed ID."
    )
    return "\n".join(lines)


@recursive_repr(fillvalue="{...}")
def _mapping_repr(result):
    return repr({key: value for key, value in result.items() if value is not None})


def _result_repr(result):
    """Render known model-facing results without changing their underlying data."""
    op = result.get("op")
    try:
        if op == "read_session":
            return _session(result)
        if op == "get_session":
            return _mapping_repr(result)
        if op in ("council.publish", "council.get", "council.read", "council.threads"):
            return _council(result)
    except (AttributeError, KeyError, TypeError, ValueError):
        # Incomplete/unknown shapes must remain inspectable, not fail during print.
        pass
    return dict.__repr__(result)


def _shell_repr(result):
    """Render a shell handle. The runtime decides which results are shell handles."""
    try:
        return _shell(result)
    except (AttributeError, KeyError, TypeError, ValueError):
        return dict.__repr__(result)


def install(namespace):
    """Attach host presentation to this session's runtime-owned result classes."""
    namespace["__VisDict__"].__repr__ = _mapping_repr
    namespace["__VisResult__"].__repr__ = _result_repr
    namespace["__VisShell__"].__repr__ = _shell_repr
