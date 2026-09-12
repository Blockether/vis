"""Model-facing views of Vis results; mappings and JSON retain every original field.

Only presentation lives here. The runtime still owns result identity, shell handles,
paging and execution. Installation changes the session-local result class, never
builtins.dict or a process-global runtime class. Unknown operations keep dict repr.
Use dict(r), r.keys(), or json.dumps(r) for the complete data.
"""

from collections import Counter


def _bounded(value, source, limit):
    text = str(value).rstrip("\n")
    if len(text) <= limit:
        return text
    half = limit // 2
    return (
        text[:half]
        + f"\n… {len(text) - 2 * half} chars omitted; full text: {source}\n"
        + text[-half:]
    )


def _duration(ms):
    return f"{ms:g}ms" if ms < 1000 else f"{ms / 1000:.3g}s"


def _details(result):
    return [
        f"{key}: {_bounded(result[key], f'r[{key!r}]', 600)}"
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
        waiting = result.get("op") == "_shell_wait" or result.get("stage") == "wait"
        parts.append("wait timed out" if waiting else "timed_out")
    lines = ["; ".join(parts)]
    if result.get("out"):
        lines.append(_bounded(result["out"], "r['out']", 4000))
    lines.extend(_details(result))
    if result.get("out_omitted_chars"):
        lines.append(
            f"{result['out_omitted_chars']} log chars omitted by the host; "
            "read r.logs(offset=0), or r['log_path']."
        )
    if result.get("is_eof") is False:
        lines.append(f"Continue log: r.logs(offset={result.get('next_offset', 0)}).")
    return "\n".join(lines)


def _tests(result):
    faults = result.get("failures") or []
    failed = (
        result.get("is_pass") is False
        or result.get("error")
        or result.get("exit") not in (None, 0)
        or result.get("fail", 0)
        or result.get("errored", 0)
        or result.get("repl_wedged")
        or result.get("repl_unusable")
        or faults
    )
    if result.get("timed_out"):
        verdict = "TIMEOUT"
    elif failed:
        verdict = "FAIL"
    elif result.get("total") == 0:
        verdict = "NO TESTS"
    else:
        verdict = "PASS" if result.get("is_pass") is True else "UNKNOWN"
    parts = [f"run_tests: {verdict}"]
    for key, label in (
        ("total", "tests"),
        ("fail", "failures"),
        ("errored", "errors"),
        ("skipped", "skipped"),
    ):
        if result.get(key) is not None:
            parts.append(f"{result[key]} {label}")
    if result.get("selected") is not None and result.get("selected") != result.get(
        "total"
    ):
        parts.append(f"{result['selected']} selected")
    if result.get("ms") is not None:
        parts.append(_duration(result["ms"]))
    lines = ["; ".join(parts)]
    context = [
        str(result[key])
        for key in ("language", "framework", "target")
        if result.get(key)
    ]
    if context:
        lines.append(_bounded(" / ".join(context), "dict(r)", 500))
    for key in ("repl_wedged", "repl_unusable", "recovered"):
        if result.get(key):
            lines.append(f"{key}=True")
    if result.get("exit") not in (None, 0):
        lines.append(f"exit={result['exit']}")
    lines.extend(_details(result))
    for index, fault in enumerate(faults[:5]):
        location = fault.get("file") or fault.get("ns") or ""
        text = f"{fault.get('type', 'fault')} {location} {fault.get('test', '?')}: {fault.get('message', '')}"
        lines.append(_bounded(text, f"r['failures'][{index}]", 600))
    if len(faults) > 5:
        lines.append(f"… {len(faults) - 5} more faults; read r['failures'].")
    output = result.get("output")
    if output:
        if verdict != "PASS":
            lines.append(_bounded(output, "r['output']", 2000))
        else:
            lines.append(f"Runner output: {len(output)} chars in r['output'].")
    return "\n".join(lines)


def _session(result):
    session = result.get("session") or {}
    transcript = result.get("transcript")
    ident = result.get("session_id") or session.get("id") or "?"
    if not session and not transcript:
        return f"read_session {ident}: not found or unavailable\n" + "\n".join(
            _details(result)
        )
    turns = (transcript or {}).get("turns") or []
    failures = result.get("failures") or []
    title = _bounded(session.get("title") or "untitled", "r['session']['title']", 160)
    lines = [
        f"read_session {ident}: {title}",
        f"{len(turns)} transcript turns; {len(failures)} failures",
    ]
    current = result.get("current_turn") or {}
    if current.get("status"):
        lines.append(f"Current turn: {current['status']}")
    if transcript is None:
        lines.append("Transcript unavailable.")
    if result.get("diagnosis"):
        lines.append(
            "Diagnosis: " + _bounded(result["diagnosis"], "r['diagnosis']", 600)
        )
    lines.extend(_details(result))
    lines.append(
        "History: r['transcript']['turns'] → iterations → blocks (code/stdout/error)."
    )
    lines.append(
        "Full data: " + ", ".join(result.keys()) + "; inspect r[key] or dict(r)."
    )
    return "\n".join(lines)


def _reply_counts(replies):
    counts = Counter(reply["state"] for reply in replies)
    return ", ".join(f"{state}={count}" for state, count in sorted(counts.items()))


def _council_entry(entry, source):
    if "entry_id" in entry:
        heading = f"Entry #{entry['entry_id']}; thread #{entry['thread_id']}"
    else:
        heading = f"Thread #{entry['thread_id']}"
    lines = [heading + f"; {entry['kind']}"]
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
            lines.extend(_council_entry(entry, f"r['entries'][{index}]"))
        if len(entries) > 5:
            key = "thread_id" if op == "council.threads" else "entry_id"
            identities = ", ".join(f"#{entry[key]}" for entry in entries[5:])
            lines.append(
                f"… {len(entries) - 5} more entries ({key}: {identities}); "
                "full messages and reply states: r['entries']."
            )
        if result["has_more"]:
            lines.append(
                f"Next page: {op}(after={result['after']}) with the same filters."
            )
    elif "entry_id" in result:
        lines = [op] + _council_entry(result, "r")
    elif result.get("error"):
        lines = [f"{op}: error"]
    else:
        return dict.__repr__(result)
    lines.extend(_details(result))
    lines.append(
        "Full data: inspect r[key] or dict(r); retrieve an entry with await council.get(entry_id)."
    )
    return "\n".join(lines)


def _result_repr(result):
    """Render known model-facing results without changing their underlying data."""
    op = result.get("op")
    try:
        if (
            op in ("shell", "_shell_logs", "_shell_wait", "_shell_type", "_shell_stop")
            and "id" in result
        ):
            return _shell(result)
        if op == "run_tests":
            return _tests(result)
        if op == "read_session":
            return _session(result)
        if op in ("council.publish", "council.get", "council.read", "council.threads"):
            return _council(result)
    except (AttributeError, KeyError, TypeError, ValueError):
        # Incomplete/unknown shapes must remain inspectable, not fail during print.
        pass
    return dict.__repr__(result)


def install(namespace):
    """Attach host presentation to this session's runtime-owned result class."""
    namespace["__VisResult__"].__repr__ = _result_repr
