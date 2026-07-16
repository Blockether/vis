#!/usr/bin/env python3
"""Render a collected Vis JSONL trace as a readable standalone HTML transcript."""
from __future__ import annotations

import argparse
import html
import json
from pathlib import Path
from typing import Any


MAX_BLOCK_CHARS = 24_000


def text(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, str):
        return value
    return json.dumps(value, indent=2, ensure_ascii=False, sort_keys=True)


def clipped(value: Any) -> str:
    value = text(value)
    if len(value) <= MAX_BLOCK_CHARS:
        return value
    return value[:MAX_BLOCK_CHARS] + "\n\n[truncated in HTML transcript]"


def block(kind: str, title: str, body: Any, *, open_: bool = False) -> str:
    body = clipped(body)
    if not body:
        return ""
    open_attr = " open" if open_ else ""
    return (
        f'<details class="{kind}"{open_attr}><summary>{html.escape(title)}</summary>'
        f"<pre>{html.escape(body)}</pre></details>"
    )


def render(trace: Path, title: str) -> str:
    entries: list[str] = []
    reasoning: dict[int, str] = {}
    rendered_reasoning: set[int] = set()
    seen: set[tuple[str, int, int]] = set()
    final = ""

    def emit_reasoning(iteration: int) -> None:
        if iteration not in rendered_reasoning and reasoning.get(iteration):
            entries.append(block("reasoning", f"Iteration {iteration} reasoning", reasoning[iteration]))
            rendered_reasoning.add(iteration)

    for raw_line in trace.open(errors="replace"):
        try:
            frame = json.loads(raw_line)
        except json.JSONDecodeError:
            continue
        if frame.get("event") == "result":
            answer = frame.get("payload", {}).get("answer")
            final = text(answer.get("answer") if isinstance(answer, dict) else answer)
            continue
        payload = frame.get("payload")
        if not isinstance(payload, dict):
            continue
        phase = payload.get("phase")
        iteration = payload.get("iteration")
        if not isinstance(iteration, int):
            continue
        if phase == "reasoning":
            reasoning[iteration] = text(payload.get("thinking"))
        elif phase == "assistant-prose":
            emit_reasoning(iteration)
            key = ("prose", iteration, 0)
            if key not in seen:
                entries.append(block("assistant", f"Vis — iteration {iteration}", payload.get("text"), open_=True))
                seen.add(key)
        elif phase == "form-start":
            position = payload.get("position", 0)
            key = ("call", iteration, position if isinstance(position, int) else 0)
            if key not in seen:
                entries.append(block("tool", f"Tool call — iteration {iteration}", payload.get("code"), open_=True))
                seen.add(key)
        elif phase == "form-result":
            position = payload.get("position", 0)
            key = ("result", iteration, position if isinstance(position, int) else 0)
            if key not in seen:
                entries.append(block("tool-result", f"Tool result — iteration {iteration}", payload.get("result-render") or payload.get("result"), open_=False))
                seen.add(key)

    for iteration in sorted(reasoning):
        emit_reasoning(iteration)
    if final:
        entries.append(block("final", "Final answer", final, open_=True))
    body = "\n".join(entries) or "<p>No readable conversation events were found in this trace.</p>"
    return f"""<!doctype html>
<html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>{html.escape(title)}</title><style>
body{{margin:0;background:#fdf6e3;color:#586e75;font:16px/1.5 system-ui,sans-serif}}main{{max-width:980px;margin:auto;padding:2rem 1rem 5rem}}h1{{color:#073642}}details{{margin:1rem 0;border:1px solid #eee8d5;border-radius:8px;background:#fffdf7}}summary{{padding:.7rem 1rem;cursor:pointer;font-weight:650}}pre{{margin:0;padding:1rem;overflow:auto;white-space:pre-wrap;word-break:break-word;border-top:1px solid #eee8d5;font:13px/1.45 ui-monospace,SFMono-Regular,monospace}}.assistant summary,.final summary{{color:#268bd2}}.tool summary{{color:#859900}}.tool-result summary{{color:#6c71c4}}.reasoning summary{{color:#b58900}}</style></head>
<body><main><h1>{html.escape(title)}</h1><p>Generated from the redacted Vis full-trace JSONL artifact.</p>{body}</main></body></html>"""


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("trace", type=Path)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--title", default="Vis conversation transcript")
    args = parser.parse_args()
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(render(args.trace, args.title))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
