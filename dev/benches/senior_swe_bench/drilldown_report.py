#!/usr/bin/env python3
"""Generate per-task Senior SWE-Bench subset/run drilldown reports."""
from __future__ import annotations

import argparse
import json
import re
from collections import Counter
from pathlib import Path
from typing import Any


HERE = Path(__file__).resolve().parent
RESULTS_DIR = HERE / "results"
REPORTS_DIR = RESULTS_DIR / "reports"
SECRET_KEY_RE = re.compile(r"(^|_)(KEY|TOKEN|SECRET)($|_)", re.IGNORECASE)


def load_json(path: Path) -> Any:
    if not path.exists():
        return None
    return json.loads(path.read_text(errors="replace"))


def as_dict(value: Any) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


def first_present(*values: Any) -> Any:
    for value in values:
        if value is not None:
            return value
    return None


def deep_get(mapping: dict[str, Any], *keys: str) -> Any:
    value: Any = mapping
    for key in keys:
        if not isinstance(value, dict):
            return None
        value = value.get(key)
    return value


def redact(value: Any) -> Any:
    if isinstance(value, dict):
        return {key: "[REDACTED]" if SECRET_KEY_RE.search(str(key)) else redact(item) for key, item in value.items()}
    if isinstance(value, list):
        return [redact(item) for item in value]
    return value


def read_text(path: Path, *, limit: int = 4000) -> str | None:
    if not path.exists():
        return None
    text = path.read_text(errors="replace")
    if len(text) <= limit:
        return text.strip()
    head = text[: limit // 2].rstrip()
    tail = text[-limit // 2 :].lstrip()
    return f"{head}\n… <truncated {len(text) - limit} chars> …\n{tail}".strip()


def diagnostic_excerpt(text: str | None, *, max_lines: int = 24) -> str | None:
    if not text:
        return None
    needles = [
        "Authentication",
        "api_error",
        "tool",
        "RewardFile",
        "Traceback",
        "Error",
        "ERROR",
        "FAILED",
        "failed",
        "Exception",
    ]
    lines = text.splitlines()
    selected: list[str] = []
    for index, line in enumerate(lines):
        if any(needle in line for needle in needles):
            start = max(0, index - 1)
            end = min(len(lines), index + 2)
            selected.extend(lines[start:end])
        if len(selected) >= max_lines:
            break
    if not selected:
        selected = lines[-max_lines:]
    deduped = list(dict.fromkeys(selected))[:max_lines]
    return "\n".join(deduped).strip() or None


def patch_file_stats(patch_text: str | None) -> dict[str, Any]:
    files: set[str] = set()
    added_files = 0
    deleted_files = 0
    if not patch_text:
        return {"files": [], "added": 0, "deleted": 0}
    for line in patch_text.splitlines():
        if line.startswith("diff --git "):
            parts = line.split()
            if len(parts) >= 4:
                files.add(parts[3][2:] if parts[3].startswith("b/") else parts[3])
        elif line.startswith("new file mode"):
            added_files += 1
        elif line.startswith("deleted file mode"):
            deleted_files += 1
    return {"files": sorted(files), "added": added_files, "deleted": deleted_files}


def task_metadata(run_dir: Path, task_id: str | None) -> dict[str, Any]:
    if not task_id:
        return {}
    path = run_dir / "dataset" / "tasks" / task_id / "task.toml"
    if not path.exists():
        return {}
    try:
        try:
            import tomllib
        except ModuleNotFoundError:  # Python < 3.11
            import tomli as tomllib  # type: ignore[no-redef]

        data = tomllib.loads(path.read_text(errors="replace"))
    except Exception:
        return {}

    metadata = as_dict(data.get("metadata"))
    taxonomy = as_dict(metadata.get("taxonomy"))
    origin = as_dict(metadata.get("origin"))
    return {
        "repo": first_present(origin.get("repo"), metadata.get("repo")),
        "segment": metadata.get("segment"),
        "task_type": taxonomy.get("task_type"),
    }


def verifier_dir(run_dir: Path) -> Path:
    return run_dir / "harbor-output" / "trial" / "verifier"


def find_patch_text(run_dir: Path, task_id: str | None) -> str | None:
    candidates = [verifier_dir(run_dir) / "agent.patch"]
    if task_id:
        candidates.append(run_dir / "patches" / f"{task_id}.diff")
    candidates.extend(sorted((run_dir / "patches").glob("*.diff")))
    for path in candidates:
        if path.exists():
            return path.read_text(errors="replace")
    return None


def contains_auth_failure(*values: Any) -> bool:
    text = "\n".join(str(value) for value in values if value is not None)
    return "Authentication Failed" in text or "AuthenticationError" in text or "authentication failed" in text.lower()


def classify(row: dict[str, Any], texts: list[str | None]) -> str:
    ledger = as_dict(row.get("ledger"))
    summary = as_dict(row.get("summary"))
    reward = as_dict(row.get("reward"))
    verifier = as_dict(row.get("verifier"))
    validation_status = as_dict(row.get("validation_agent_status"))
    validation_results = as_dict(row.get("validation_results"))
    judge = as_dict(row.get("judge"))
    harbor_exception = as_dict(summary.get("harbor_exception"))
    preflight = as_dict(row.get("preflight_failure"))
    ledger_failure = str(ledger.get("failure_class") or "")
    ledger_exception_type = str(ledger.get("harbor_exception_type") or "")

    if not summary and row.get("exit_status") not in (None, 0):
        if preflight or row.get("command") == {}:
            return "missing_summary"
    if contains_auth_failure(summary, validation_status, validation_results, judge, *texts):
        return "auth_failure"
    if "RewardFileEmptyError" in {ledger_failure, ledger_exception_type}:
        return "reward_empty"
    if any("Docker daemon" in (text or "") or "Cannot connect to the Docker" in (text or "") for text in texts):
        return "docker_or_platform_failure"
    if preflight:
        return "docker_or_platform_failure"
    if harbor_exception.get("exception_type") == "RewardFileEmptyError":
        return "reward_empty"
    if summary.get("resolved") is True or reward.get("resolved") is True or reward.get("reward") == 1:
        return "scoreable_pass"
    if any(reward.get(key) is not None for key in ("reward", "correctness", "resolved")):
        return "scoreable_fail"
    if validation_results.get("validation_agent_infrastructure_failure") or validation_status.get("returncode") not in (None, 0):
        return "validation_agent_infra_failure"
    if not validation_results and validation_status:
        return "validation_agent_no_results"
    if verifier.get("total") == 0:
        return "verifier_zero_total"
    if verifier and verifier.get("all_pass") is False:
        return "verifier_failed"
    rubric_status = str(judge.get("rubric_status") or "")
    taste_status = str(judge.get("taste_status") or "")
    if "api_error" in rubric_status or "api_error" in taste_status:
        return "judge_api_error"
    if "tool" in str(judge.get("rubric_error") or "").lower() or "tool" in str(judge.get("taste_error") or "").lower():
        return "judge_no_tool_use"
    if not summary:
        return "missing_summary"
    return str(summary.get("failure_class") or ledger_failure or "scoreable_fail")


def task_row(task: dict[str, Any]) -> dict[str, Any]:
    run_dir = Path(str(task.get("run_dir") or ""))
    summary = as_dict(load_json(run_dir / "summary.json"))
    command = as_dict(load_json(run_dir / "command.json"))
    preflight = as_dict(load_json(run_dir / "preflight-failure.json"))
    vdir = verifier_dir(run_dir)
    validation_status = as_dict(load_json(vdir / "validation_agent_status.json"))
    validation_results = as_dict(load_json(vdir / "validation_results.json"))
    judge = as_dict(load_json(vdir / "judge_output.json") or summary.get("judge"))
    verifier = as_dict(load_json(vdir / "verifier_results.json") or summary.get("verifier"))
    reward = as_dict(
        load_json(vdir / "reward_details.json")
        or load_json(vdir / "reward.json")
        or load_json(vdir / "score.json")
        or summary.get("harbor_reward")
    )
    task_id = first_present(summary.get("task_id"), task.get("task_id"), (command.get("task_ids") or [None])[0] if isinstance(command.get("task_ids"), list) else None)
    patch_text = find_patch_text(run_dir, task_id)
    diagnostics = {
        "test_stdout": diagnostic_excerpt(read_text(vdir / "test-stdout.txt", limit=8000)),
        "run_judge_rubric_stderr": diagnostic_excerpt(read_text(vdir / "run_judge_rubric.stderr", limit=4000)),
        "run_judge_taste_stderr": diagnostic_excerpt(read_text(vdir / "run_judge_taste.stderr", limit=4000)),
    }
    row: dict[str, Any] = {
        "task_id": task_id,
        "run_id": first_present(command.get("run_id"), task.get("run_id"), run_dir.name),
        "run_dir": str(run_dir),
        "exit_status": task.get("exit_status"),
        "metadata": task_metadata(run_dir, task_id),
        "reward": {
            "reward": reward.get("reward"),
            "correctness": reward.get("correctness"),
            "resolved": first_present(reward.get("resolved"), summary.get("resolved")),
        },
        "verifier": {
            "passed": verifier.get("passed"),
            "total": verifier.get("total"),
            "all_pass": verifier.get("all_pass"),
        },
        "validation_agent_status": {
            "exit_status": validation_status.get("exit_status"),
            "model": validation_status.get("model"),
            "harness": validation_status.get("harness"),
            "returncode": validation_status.get("returncode"),
        },
        "validation_results": {
            "score": validation_results.get("validation_score"),
            "passed_stories": validation_results.get("passed_stories"),
            "total_stories": validation_results.get("total_stories"),
            "passed_cases": validation_results.get("passed_cases"),
            "total_cases": validation_results.get("total_cases"),
            "infrastructure_failure": validation_results.get("validation_agent_infrastructure_failure"),
        },
        "judge": {
            "rubric_status": judge.get("rubric_status"),
            "rubric_score": first_present(reward.get("rubric_score"), reward.get("score"), summary.get("rubric_score")),
            "taste_status": judge.get("taste_status"),
        },
        "patch": {
            **patch_file_stats(patch_text),
            "lines": summary.get("patch_bloat") or task.get("patch_bloat"),
        },
        "vis": {
            "iterations": deep_get(summary, "vis", "iterations"),
            "elapsed_s": deep_get(summary, "vis", "elapsed_s"),
            "tokens": deep_get(summary, "vis", "tokens"),
            "cost_usd": deep_get(summary, "vis", "cost_usd"),
        },
        "diagnostics": diagnostics,
        "ledger": task,
        "summary": summary,
        "command": redact(command),
        "preflight_failure": preflight,
    }
    row["failure_class"] = classify(row, list(diagnostics.values()) + [read_text(run_dir / "harbor.log", limit=4000)])
    row.pop("ledger", None)
    row.pop("summary", None)
    row.pop("preflight_failure", None)
    return redact(row)


def resolve_subset(identifier: str) -> Path:
    path = Path(identifier)
    if path.exists():
        return path
    candidate = RESULTS_DIR / "subsets" / (identifier if identifier.endswith(".json") else f"{identifier}.json")
    if candidate.exists():
        return candidate
    raise SystemExit(f"subset ledger not found: {identifier}")


def build_report(subset: Path) -> dict[str, Any]:
    ledger = as_dict(load_json(subset))
    tasks = [task_row(task) for task in ledger.get("tasks", []) if isinstance(task, dict)]
    counts = Counter(str(task["failure_class"]) for task in tasks)
    return {
        "subset": {
            "name": ledger.get("subset_name") or subset.stem,
            "path": str(subset),
            "total_tasks": len(tasks),
            "exit_status": ledger.get("exit_status"),
        },
        "failure_counts": dict(sorted(counts.items())),
        "tasks": tasks,
    }


def md_value(value: Any) -> str:
    if value is None or value == {}:
        return ""
    if isinstance(value, float):
        return f"{value:.4g}"
    if isinstance(value, (dict, list)):
        return json.dumps(value, sort_keys=True)
    return str(value).replace("\n", "<br>")


def render_markdown(report: dict[str, Any]) -> str:
    subset = report["subset"]
    lines = [
        f"# Senior SWE-Bench drilldown: {subset['name']}",
        "",
        f"- Subset ledger: `{subset['path']}`",
        f"- Total tasks: {subset['total_tasks']}",
        f"- Exit status: {subset['exit_status']}",
        "",
        "## Failure classes",
        "",
    ]
    for name, count in report["failure_counts"].items():
        lines.append(f"- `{name}`: {count}")
    lines.extend([
        "",
        "## Tasks",
        "",
        "| task | repo | segment | type | exit | class | reward | verifier | validation | rubric | taste | patch | vis |",
        "| --- | --- | --- | --- | ---: | --- | --- | --- | --- | --- | --- | --- | --- |",
    ])
    for task in report["tasks"]:
        meta = task["metadata"]
        verifier = task["verifier"]
        validation = task["validation_results"]
        patch = task["patch"]
        vis = task["vis"]
        lines.append(
            "| "
            + " | ".join(
                md_value(value)
                for value in [
                    task.get("task_id"),
                    meta.get("repo"),
                    meta.get("segment"),
                    meta.get("task_type"),
                    task.get("exit_status"),
                    f"`{task.get('failure_class')}`",
                    task.get("reward"),
                    f"{verifier.get('passed')}/{verifier.get('total')} all={verifier.get('all_pass')}",
                    f"{validation.get('score')} stories={validation.get('passed_stories')}/{validation.get('total_stories')} cases={validation.get('passed_cases')}/{validation.get('total_cases')}",
                    f"{task['judge'].get('rubric_status')} score={task['judge'].get('rubric_score')}",
                    task["judge"].get("taste_status"),
                    f"files={len(patch.get('files') or [])} +{patch.get('added')} -{patch.get('deleted')}",
                    f"iter={vis.get('iterations')} elapsed={vis.get('elapsed_s')} tokens={md_value(vis.get('tokens'))} cost={vis.get('cost_usd')}",
                ]
            )
            + " |"
        )
    lines.extend(["", "## Diagnostics", ""])
    for task in report["tasks"]:
        lines.extend([f"### {task.get('task_id') or task.get('run_id')} — `{task.get('failure_class')}`", ""])
        for name, excerpt in task.get("diagnostics", {}).items():
            if excerpt:
                lines.extend([f"#### {name}", "", "```text", excerpt, "```", ""])
    return "\n".join(lines).rstrip() + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("subset", help="Subset ledger path or run id under results/subsets")
    parser.add_argument("--out-dir", type=Path, help="Directory for default JSON and Markdown report outputs")
    parser.add_argument("--json-out", type=Path)
    parser.add_argument("--md-out", type=Path)
    args = parser.parse_args()

    subset = resolve_subset(args.subset)
    report = build_report(subset)
    stem = subset.stem
    out_dir = args.out_dir or REPORTS_DIR
    json_out = args.json_out or out_dir / f"{stem}-drilldown.json"
    md_out = args.md_out or out_dir / f"{stem}-drilldown.md"
    json_out.parent.mkdir(parents=True, exist_ok=True)
    md_out.parent.mkdir(parents=True, exist_ok=True)
    json_out.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    md_out.write_text(render_markdown(report))
    print(json.dumps({"json": str(json_out), "markdown": str(md_out), "failure_counts": report["failure_counts"]}, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
