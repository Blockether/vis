#!/usr/bin/env python3
"""Render an HTML index for Senior SWE-Bench result directories."""

from __future__ import annotations

import argparse
import html
import json
from collections import defaultdict
from pathlib import Path


def load(path: Path) -> dict:
    try:
        value = json.loads(path.read_text())
        return value if isinstance(value, dict) else {}
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return {}


def first(*values):
    return next((value for value in values if value is not None and value != ""), None)


def run_record(path: Path) -> dict | None:
    command = load(path / "command.json")
    summary = load(path / "summary.json")
    if not command and not summary:
        return None
    completion = summary.get("completion") or {}
    agent = summary.get("agent") or {}
    trace = summary.get("agent_trace") or {}
    usage = (
        agent.get("usage")
        or trace.get("tokens")
        or (summary.get("vis") or {}).get("tokens")
        or {}
    )
    reward = summary.get("harbor_reward") or {}
    task_ids = command.get("task_ids") or []
    task = first(summary.get("task_id"), task_ids[0] if task_ids else None, path.name)
    score = first(
        completion.get("score"), reward.get("reward"), summary.get("rubric_score")
    )
    passed = first(
        completion.get("passed"), summary.get("verifier_pass"), summary.get("resolved")
    )
    status = first(completion.get("status"), summary.get("failure_class"))
    if not status:
        status = (
            "preflight"
            if (path / "preflight-only.txt").exists()
            else "in progress / incomplete"
        )
    return {
        "path": path,
        "run": path.name,
        "task": str(task),
        "harness": str(
            first(agent.get("harness"), command.get("bench_agent_label"), "unknown")
        ),
        "model": str(first(agent.get("model"), command.get("bench_model"), "unknown")),
        "effort": str(
            first(
                agent.get("reasoning_effort"),
                command.get("bench_agent_reasoning_effort"),
                "—",
            )
        ),
        "status": str(status),
        "passed": passed,
        "score": score,
        "iterations": first(
            trace.get("iterations"), (summary.get("vis") or {}).get("iterations")
        ),
        "tokens": first(usage.get("total_tokens"), usage.get("total")),
        "cost": first(
            trace.get("cost_usd"),
            usage.get("reported_cost_usd"),
            (summary.get("vis") or {}).get("cost_usd"),
        ),
    }


def fmt(value, digits: int = 0) -> str:
    if value is None:
        return "—"
    if isinstance(value, bool):
        return "pass" if value else "fail"
    if isinstance(value, (int, float)):
        return f"{value:,.{digits}f}"
    return str(value)


def link(path: Path, label: str, output: Path) -> str:
    try:
        target = path.relative_to(output.parent)
    except ValueError:
        target = path
    return f'<a href="{html.escape(target.as_posix(), quote=True)}">{html.escape(label)}</a>'


def render(results: Path, output: Path, title: str = "Senior SWE-Bench runs") -> str:
    runs = [
        record
        for path in results.iterdir()
        if path.is_dir() and (record := run_record(path))
    ]
    runs.sort(key=lambda run: run["path"].stat().st_mtime, reverse=True)
    groups: dict[str, list[dict]] = defaultdict(list)
    for run in runs:
        groups[run["task"]].append(run)

    group_rows = []
    for task, similar in sorted(groups.items()):
        scored = [run for run in similar if isinstance(run["score"], (int, float))]
        passed = sum(run["passed"] is True for run in similar)
        harnesses = ", ".join(sorted({run["harness"] for run in similar}))
        group_rows.append(
            "<tr>"
            f"<td>{html.escape(task)}</td><td>{len(similar)}</td><td>{len(scored)}</td>"
            f"<td>{passed}</td><td>{fmt(max((run['score'] for run in scored), default=None), 3)}</td>"
            f"<td>{html.escape(harnesses)}</td></tr>"
        )

    run_rows = []
    for run in runs:
        artifacts = [link(run["path"], "dir", output)]
        for name, label in (
            ("summary.json", "summary"),
            ("vis-transcript.html", "transcript"),
        ):
            artifact = run["path"] / name
            if artifact.exists():
                artifacts.append(link(artifact, label, output))
        passed_class = (
            "pass"
            if run["passed"] is True
            else "fail"
            if run["passed"] is False
            else "unknown"
        )
        run_rows.append(
            "<tr>"
            f"<td>{html.escape(run['task'])}</td><td>{html.escape(run['run'])}</td>"
            f"<td>{html.escape(run['harness'])}</td><td>{html.escape(run['model'])}</td>"
            f"<td>{html.escape(run['effort'])}</td><td>{html.escape(run['status'])}</td>"
            f"<td class='{passed_class}'>{fmt(run['passed'])}</td><td>{fmt(run['score'], 3)}</td>"
            f"<td>{fmt(run['iterations'])}</td><td>{fmt(run['tokens'])}</td><td>{fmt(run['cost'], 3)}</td>"
            f"<td>{' · '.join(artifacts)}</td></tr>"
        )

    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>{html.escape(title)}</title><style>
body{{margin:0;background:#f7f7f8;color:#202124;font:14px/1.45 system-ui,sans-serif}}main{{max-width:1600px;margin:auto;padding:2rem}}h1,h2{{color:#172554}}.meta{{color:#64748b}}.table{{overflow:auto;background:white;border:1px solid #ddd;border-radius:8px;margin:1rem 0 2rem}}table{{border-collapse:collapse;width:100%}}th,td{{padding:.55rem .7rem;text-align:left;border-bottom:1px solid #eee;white-space:nowrap}}th{{position:sticky;top:0;background:#eef2ff}}tr:hover{{background:#fafafa}}a{{color:#1d4ed8}}.pass{{color:#15803d;font-weight:650}}.fail{{color:#b91c1c;font-weight:650}}.unknown{{color:#64748b}}
</style></head><body><main><h1>{html.escape(title)}</h1>
<p class="meta">{len(runs)} runs across {len(groups)} task groups. Comparable rows share the same task ID.</p>
<h2>Comparable task summary</h2><div class="table"><table><thead><tr><th>Task</th><th>Runs</th><th>Scored</th><th>Passed</th><th>Best score</th><th>Harnesses</th></tr></thead><tbody>{"".join(group_rows)}</tbody></table></div>
<h2>All runs</h2><div class="table"><table><thead><tr><th>Task</th><th>Run</th><th>Harness</th><th>Model</th><th>Effort</th><th>Status</th><th>Verifier</th><th>Score</th><th>Iterations</th><th>Tokens</th><th>Cost USD</th><th>Artifacts</th></tr></thead><tbody>{"".join(run_rows)}</tbody></table></div>
</main></body></html>"""


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results", type=Path)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--title", default="Senior SWE-Bench runs")
    args = parser.parse_args()
    args.out.write_text(render(args.results, args.out, args.title))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
