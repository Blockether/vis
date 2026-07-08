#!/usr/bin/env python3
"""Render a side-by-side Senior SWE-Bench HTML report."""
from __future__ import annotations

import argparse
import html
import json
import re
from datetime import datetime
from pathlib import Path
from typing import Any


MISSING = "not reported"


def load_json(path: Path) -> Any:
    if not path.exists():
        return None
    return json.loads(path.read_text())


def first_json(run_dir: Path, *names: str) -> Any:
    for name in names:
        for path in sorted(run_dir.glob(f"**/{name}")):
            data = load_json(path)
            if data is not None:
                return data
    return None


def deep_get(data: Any, *keys: str) -> Any:
    cur = data
    for key in keys:
        if not isinstance(cur, dict):
            return None
        cur = cur.get(key)
    return cur


def first_present(*values: Any) -> Any:
    for value in values:
        if value not in (None, ""):
            return value
    return None


def html_escape(value: Any) -> str:
    return html.escape(str(value), quote=True)


def shorten(value: Any, limit: int = 380) -> str:
    text = str(value).strip()
    if len(text) <= limit:
        return text
    return text[: limit - 3].rstrip() + "..."


def parse_dt(value: Any) -> datetime | None:
    if not isinstance(value, str) or not value:
        return None
    try:
        return datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None


def duration_seconds(start: Any, finish: Any) -> float | None:
    started_at = parse_dt(start)
    finished_at = parse_dt(finish)
    if not started_at or not finished_at:
        return None
    return max(0.0, (finished_at - started_at).total_seconds())


def format_duration(seconds: Any) -> str:
    if not isinstance(seconds, (int, float)):
        return MISSING
    total = int(round(seconds))
    if total >= 3600:
        hours, rem = divmod(total, 3600)
        minutes, sec = divmod(rem, 60)
        return f"{hours}h {minutes:02d}m {sec:02d}s"
    if total >= 60:
        minutes, sec = divmod(total, 60)
        return f"{minutes}m {sec:02d}s"
    return f"{total}s"


def format_number(value: Any) -> str:
    if value is None:
        return MISSING
    if isinstance(value, float):
        return f"{value:.3f}".rstrip("0").rstrip(".")
    return str(value)


def format_rate(rate: dict[str, Any] | None) -> str:
    if not isinstance(rate, dict):
        return MISSING
    passed = rate.get("passed")
    total = rate.get("total")
    if not isinstance(passed, int) or not isinstance(total, int) or total <= 0:
        return MISSING
    percent = (passed / total) * 100.0
    return f"{passed} / {total} ({percent:.1f}%)"


def format_token_cost(metrics: dict[str, Any]) -> str:
    tokens = metrics.get("tokens")
    parts: list[str] = []
    if isinstance(tokens, dict) and tokens:
        for key in sorted(tokens):
            parts.append(f"{key}: {format_number(tokens[key])}")
    for key, label in [
        ("n_input_tokens", "input"),
        ("n_cache_tokens", "cache"),
        ("n_output_tokens", "output"),
    ]:
        if metrics.get(key) is not None:
            parts.append(f"{label}: {format_number(metrics[key])}")
    if metrics.get("cost_usd") is not None:
        parts.append(f"cost: ${format_number(metrics['cost_usd'])}")
    return "; ".join(parts) if parts else MISSING


def arg_after(command: Any, flag: str) -> Any:
    if not isinstance(command, list):
        return None
    try:
        index = command.index(flag)
    except ValueError:
        return None
    if index + 1 >= len(command):
        return None
    return command[index + 1]


def job_eval_metrics(job_result: dict[str, Any]) -> list[dict[str, Any]]:
    stats = job_result.get("stats")
    evals = stats.get("evals") if isinstance(stats, dict) else None
    if not isinstance(evals, dict):
        return []
    metrics: list[dict[str, Any]] = []
    for entry in evals.values():
        if not isinstance(entry, dict):
            continue
        for metric in entry.get("metrics", []):
            if isinstance(metric, dict):
                metrics.append(metric)
    return metrics


def task_pass(summary: dict[str, Any], reward: dict[str, Any], job_result: dict[str, Any]) -> bool | None:
    for value in [summary.get("resolved"), summary.get("verifier_pass")]:
        if isinstance(value, bool):
            return value
    for key in ["reward", "correctness", "verifier_score"]:
        value = reward.get(key)
        if isinstance(value, (int, float)):
            return value >= 1
    metrics = job_eval_metrics(job_result)
    if metrics:
        return any(
            isinstance(metric.get(key), (int, float)) and metric[key] >= 1
            for metric in metrics
            for key in ["reward", "correctness", "verifier_score"]
        )
    return None


def task_pass_rate(summary: dict[str, Any], reward: dict[str, Any], job_result: dict[str, Any]) -> dict[str, Any]:
    metrics = job_eval_metrics(job_result)
    if metrics:
        passed = 0
        for metric in metrics:
            if any(
                isinstance(metric.get(key), (int, float)) and metric[key] >= 1
                for key in ["reward", "correctness", "verifier_score"]
            ):
                passed += 1
        total = deep_get(job_result, "n_total_trials") or len(metrics)
        return {"passed": passed, "total": int(total), "source": "Harbor trial metrics"}
    passed_bool = task_pass(summary, reward, job_result)
    if passed_bool is None:
        return {"passed": None, "total": None, "source": "unavailable"}
    return {"passed": 1 if passed_bool else 0, "total": 1, "source": "summary"}


def verifier_rate(summary: dict[str, Any], reward_details: dict[str, Any]) -> dict[str, Any]:
    verifier = summary.get("verifier")
    if not isinstance(verifier, dict):
        verifier = reward_details.get("verifier")
    if not isinstance(verifier, dict):
        return {"passed": None, "total": None, "source": "unavailable"}
    passed = verifier.get("passed")
    total = verifier.get("total")
    return {
        "passed": int(passed) if isinstance(passed, int) else None,
        "total": int(total) if isinstance(total, int) else None,
        "source": "Senior SWE-Bench verifier command",
    }


def parse_pytest_counts(text: str) -> dict[str, Any] | None:
    for line in reversed(text.splitlines()):
        if not any(word in line for word in [" passed", " failed", " error", " errors", " skipped"]):
            continue
        counts: dict[str, int] = {}
        for amount, name in re.findall(r"(\d+)\s+(passed|failed|skipped|errors?|xfailed|xpassed)", line):
            key = "errors" if name in {"error", "errors"} else name
            counts[key] = counts.get(key, 0) + int(amount)
        if counts:
            total = sum(counts.get(key, 0) for key in ["passed", "failed", "errors", "skipped", "xfailed", "xpassed"])
            return {
                "passed": counts.get("passed", 0),
                "total": total,
                "counts": counts,
                "summary_line": line.strip(),
                "source": "pytest runner_shell.log",
            }
    return None


def extract_failure_details(text: str) -> dict[str, Any]:
    if not text:
        return {"pytest": None, "failed_tests": [], "assertions": []}
    failed_tests: list[str] = []
    assertions: list[str] = []
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if " FAILED" in line and "::" in line:
            failed_tests.append(shorten(line, 240))
        elif line.startswith("FAILED ") and "::" in line:
            failed_tests.append(shorten(line, 240))
        elif "AssertionError:" in line:
            assertions.append(shorten(line, 500))
    return {
        "pytest": parse_pytest_counts(text),
        "failed_tests": failed_tests[:5],
        "assertions": assertions[:5],
    }


def patch_stats_from_text(diff: str) -> dict[str, int]:
    added = deleted = files = hunks = 0
    for line in diff.splitlines():
        if line.startswith("diff --git "):
            files += 1
        elif line.startswith("@@"):
            hunks += 1
        elif line.startswith("+") and not line.startswith("+++"):
            added += 1
        elif line.startswith("-") and not line.startswith("---"):
            deleted += 1
    return {"files": files, "hunks": hunks, "added_lines": added, "deleted_lines": deleted, "total_changed_lines": added + deleted}


def collect_timings(trial: dict[str, Any], job_result: dict[str, Any]) -> dict[str, Any]:
    timings = {
        "started_at": first_present(trial.get("started_at"), job_result.get("started_at")),
        "finished_at": first_present(trial.get("finished_at"), job_result.get("finished_at")),
        "total_s": None,
        "phases": {},
    }
    timings["total_s"] = duration_seconds(timings["started_at"], timings["finished_at"])
    phases: dict[str, Any] = {}
    for key, label in [
        ("environment_setup", "environment setup"),
        ("agent_setup", "agent setup"),
        ("agent_execution", "agent execution"),
        ("verifier", "verifier"),
    ]:
        phase = trial.get(key)
        if isinstance(phase, dict):
            phases[label] = duration_seconds(phase.get("started_at"), phase.get("finished_at"))
    timings["phases"] = phases
    return timings


def collect_run(
    run_dir: Path,
    *,
    label: str,
    harness: str,
    provider_override: str | None = None,
    model_override: str | None = None,
    fair_provider: str,
    fair_model: str,
) -> dict[str, Any]:
    summary = load_json(run_dir / "summary.json")
    command = load_json(run_dir / "command.json")
    trial = first_json(run_dir, "result.json")
    job_result = load_json(run_dir / "harbor-output" / "job-result.json")
    job_config = load_json(run_dir / "harbor-output" / "job-config.json")
    trial_config = load_json(run_dir / "harbor-output" / "trial" / "config.json")
    reward_details = first_json(run_dir, "reward_details.json")
    reward_file = first_json(run_dir, "reward.json")
    judge_output = first_json(run_dir, "judge_output.json")
    verifier_results = first_json(run_dir, "verifier_results.json")

    summary = summary if isinstance(summary, dict) else {}
    command = command if isinstance(command, dict) else {}
    trial = trial if isinstance(trial, dict) else {}
    job_result = job_result if isinstance(job_result, dict) else {}
    job_config = job_config if isinstance(job_config, dict) else {}
    trial_config = trial_config if isinstance(trial_config, dict) else {}
    reward_details = reward_details if isinstance(reward_details, dict) else {}
    reward_file = reward_file if isinstance(reward_file, dict) else {}
    judge_output = judge_output if isinstance(judge_output, dict) else {}
    verifier_results = verifier_results if isinstance(verifier_results, dict) else {}

    agent_env = deep_get(trial_config, "agent", "env")
    if not isinstance(agent_env, dict):
        agents = job_config.get("agents")
        agent_env = agents[0].get("env") if isinstance(agents, list) and agents and isinstance(agents[0], dict) else {}
    if not isinstance(agent_env, dict):
        agent_env = {}

    agent_info = trial.get("agent_info") if isinstance(trial.get("agent_info"), dict) else {}
    agent_model_info = agent_info.get("model_info") if isinstance(agent_info.get("model_info"), dict) else {}
    provider = first_present(
        provider_override,
        command.get("vis_provider"),
        agent_env.get("VIS_PROVIDER"),
        agent_model_info.get("provider"),
    )
    if provider is None and harness.lower() == "vis":
        provider = "zai-coding-plan (adapter default)"
    model = first_present(
        model_override,
        command.get("vis_model"),
        agent_env.get("VIS_MODEL"),
        deep_get(trial_config, "agent", "model_name"),
        arg_after(command.get("command"), "--model"),
        agent_model_info.get("name"),
    )

    reward = summary.get("harbor_reward")
    if not isinstance(reward, dict):
        reward = reward_details or reward_file
    if not isinstance(reward, dict):
        reward = {}

    runner_log = run_dir / "harbor-output" / "trial" / "verifier" / "runner_shell.log"
    runner_text = runner_log.read_text(errors="replace") if runner_log.exists() else ""
    failure_details = extract_failure_details(runner_text)

    patch_path = run_dir / "harbor-output" / "trial" / "verifier" / "agent.patch"
    patch_text = patch_path.read_text(errors="replace") if patch_path.exists() else ""
    local_patch_stats = summary.get("patch_bloat")
    if not isinstance(local_patch_stats, dict):
        local_patch_stats = patch_stats_from_text(patch_text)

    stats = job_result.get("stats") if isinstance(job_result.get("stats"), dict) else {}
    vis_stats = summary.get("vis") if isinstance(summary.get("vis"), dict) else {}
    metrics = {
        "iterations": first_present(vis_stats.get("iterations"), deep_get(trial, "agent_result", "metadata", "vis", "iterations")),
        "tool_calls": first_present(vis_stats.get("tool_calls"), deep_get(trial, "agent_result", "metadata", "vis", "tool_calls")),
        "shell_calls": vis_stats.get("shell_calls"),
        "failed_shell_calls": vis_stats.get("failed_shell_calls"),
        "file_reads": vis_stats.get("file_reads"),
        "tokens": first_present(vis_stats.get("tokens"), deep_get(trial, "agent_result", "metadata", "vis", "tokens")),
        "cost_usd": first_present(vis_stats.get("cost_usd"), deep_get(trial, "agent_result", "metadata", "vis", "cost_usd"), stats.get("cost_usd")),
        "n_input_tokens": stats.get("n_input_tokens"),
        "n_cache_tokens": stats.get("n_cache_tokens"),
        "n_output_tokens": stats.get("n_output_tokens"),
    }

    pb = deep_get(reward_details, "taste", "patch_bloat")
    if not isinstance(pb, dict):
        pb = deep_get(judge_output, "patch_bloat")
    if not isinstance(pb, dict):
        pb = {}

    task_id = first_present(summary.get("task_id"), command.get("task_ids", [None])[0] if isinstance(command.get("task_ids"), list) else None)
    pass_rate = task_pass_rate(summary, reward, job_result)
    verifier_command_rate = verifier_rate(summary, reward_details)
    status = summary.get("failure_class")
    if task_pass(summary, reward, job_result) is True:
        status = "passed"
    elif status is None:
        status = "unknown"

    return {
        "label": label,
        "harness": harness,
        "status": status,
        "task_id": task_id,
        "run_id": first_present(command.get("run_id"), run_dir.name),
        "run_dir": str(run_dir.resolve()),
        "pass_rate": pass_rate,
        "verifier_command_rate": verifier_command_rate,
        "pytest_rate": failure_details.get("pytest"),
        "params": {
            "provider": provider,
            "model": model,
            "fair_provider": fair_provider,
            "fair_model": fair_model,
            "agent": first_present(agent_info.get("name"), deep_get(trial_config, "agent", "name"), "unknown"),
            "artifact": command.get("vis_bench_artifact"),
            "config_delivery": command.get("vis_bench_config_delivery"),
            "remote_home": command.get("vis_bench_remote_home"),
            "remote_home_mount": command.get("vis_bench_remote_home_mount") or "none",
            "remote_home_mount_mode": command.get("vis_bench_remote_home_mount_mode"),
            "task_base_image": command.get("task_base_image"),
            "selected_task_image": command.get("selected_task_image"),
            "install_only": command.get("install_only"),
            "n_attempts": job_config.get("n_attempts"),
            "n_concurrent_trials": job_config.get("n_concurrent_trials"),
            "verifier_provider": command.get("vis_bench_verifier_provider"),
            "verifier_env_file": command.get("vis_bench_verifier_env_file"),
            "verifier_openai_base_url": command.get("vis_bench_verifier_openai_base_url"),
            "verifier_judge_model": command.get("vis_bench_verifier_judge_model"),
            "verifier_classifier_model": command.get("vis_bench_verifier_classifier_model"),
            "verifier_tool_choice_compat": command.get("vis_bench_verifier_tool_choice_compat"),
            "verifier_response_format_compat": command.get("vis_bench_verifier_response_format_compat"),
            "verifier_timeout_multiplier": command.get("vis_bench_verifier_timeout_multiplier"),
        },
        "timing": collect_timings(trial, job_result),
        "metrics": metrics,
        "diff_stats": {
            "local_patch": local_patch_stats,
            "agent_vs_oracle": pb,
            "agent_patch_lines": len(patch_text.splitlines()) if patch_text else None,
        },
        "scoring": {
            "reward": reward.get("reward"),
            "correctness": reward.get("correctness"),
            "verifier_score": first_present(reward.get("verifier_score"), deep_get(trial, "verifier_result", "rewards", "verifier_score")),
            "taste_patch_bloat": first_present(reward.get("taste_patch_bloat"), deep_get(trial, "verifier_result", "rewards", "taste_patch_bloat")),
            "rubric_judge_ok": first_present(reward.get("rubric_judge_ok"), deep_get(trial, "verifier_result", "rewards", "rubric_judge_ok")),
            "taste_judge_ok": first_present(reward.get("taste_judge_ok"), deep_get(trial, "verifier_result", "rewards", "taste_judge_ok")),
            "rubric_status": first_present(deep_get(reward_details, "rubric", "status"), judge_output.get("rubric_status")),
            "taste_status": first_present(judge_output.get("taste_status"), deep_get(reward_details, "taste", "status")),
            "verifier_results": verifier_results,
        },
        "failure_details": failure_details,
        "artifacts": {
            "runner_log": str(runner_log.resolve()) if runner_log.exists() else None,
            "patch": str(patch_path.resolve()) if patch_path.exists() else None,
            "trace": deep_get(trial, "agent_result", "metadata", "vis", "trace"),
            "harbor_job_dir": deep_get(first_json(run_dir, "collection.json") or {}, "job_dir"),
        },
        "notes": [],
    }


def pending_harness(label: str, *, task_id: Any, fair_provider: str, fair_model: str) -> dict[str, Any]:
    return {
        "label": label,
        "harness": label,
        "status": "pending-data",
        "task_id": task_id,
        "run_id": None,
        "run_dir": None,
        "pass_rate": {"passed": None, "total": None, "source": "pending"},
        "verifier_command_rate": {"passed": None, "total": None, "source": "pending"},
        "pytest_rate": None,
        "params": {
            "provider": fair_provider,
            "model": fair_model,
            "fair_provider": fair_provider,
            "fair_model": fair_model,
            "agent": "pi.dev",
            "config_delivery": "pending",
        },
        "timing": {"started_at": None, "finished_at": None, "total_s": None, "phases": {}},
        "metrics": {},
        "diff_stats": {"local_patch": {}, "agent_vs_oracle": {}, "agent_patch_lines": None},
        "scoring": {},
        "failure_details": {"pytest": None, "failed_tests": [], "assertions": []},
        "artifacts": {},
        "notes": [
            "No pi.dev result artifact was provided. This slot is seeded with the requested fair comparison params and should be replaced with a pi.dev run over the same task set.",
        ],
    }


def external_harness(path: Path, label: str, *, fair_provider: str, fair_model: str, task_id: Any) -> dict[str, Any]:
    data = load_json(path)
    if not isinstance(data, dict):
        raise SystemExit(f"comparison JSON must be an object: {path}")
    params = data.get("params") if isinstance(data.get("params"), dict) else {}
    result = pending_harness(label, task_id=task_id or data.get("task_id"), fair_provider=fair_provider, fair_model=fair_model)
    result.update({key: value for key, value in data.items() if key in result})
    result["label"] = label
    result["harness"] = data.get("harness") or label
    result["status"] = data.get("status") or "loaded-json"
    result["params"] = {**result["params"], **params}
    result["run_dir"] = str(path.resolve())
    return result


def class_for_status(status: Any) -> str:
    text = str(status)
    if text == "passed":
        return "good"
    if text in {"pending-data", "loaded-json"}:
        return "warn"
    if text in {"unknown", "not reported"}:
        return "neutral"
    return "bad"


def status_pill(status: Any) -> str:
    status_text = html_escape(status or MISSING)
    return f'<span class="pill {class_for_status(status)}">{status_text}</span>'


def kv_table(rows: list[tuple[str, Any]], *, compact: bool = False) -> str:
    cls = "kv compact" if compact else "kv"
    out = [f'<table class="{cls}"><tbody>']
    for key, value in rows:
        rendered = value if isinstance(value, str) and value.startswith("<") else html_escape(value if value not in (None, "") else MISSING)
        out.append(f"<tr><th>{html_escape(key)}</th><td>{rendered}</td></tr>")
    out.append("</tbody></table>")
    return "\n".join(out)


def rate_cell(rate: dict[str, Any] | None) -> str:
    text = format_rate(rate)
    source = rate.get("source") if isinstance(rate, dict) else None
    source_html = f'<div class="hint">{html_escape(source)}</div>' if source else ""
    return f"<span>{html_escape(text)}</span>{source_html}"


def params_summary(harness: dict[str, Any]) -> str:
    params = harness.get("params") if isinstance(harness.get("params"), dict) else {}
    provider = params.get("provider") or MISSING
    model = params.get("model") or MISSING
    fair_provider = params.get("fair_provider")
    fair_model = params.get("fair_model")
    delta = ""
    if fair_provider and fair_model and (provider != fair_provider or model != fair_model):
        delta = f'<div class="hint">fair target: {html_escape(fair_provider)} / {html_escape(fair_model)}</div>'
    return f"{html_escape(provider)} / {html_escape(model)}{delta}"


def diff_summary(harness: dict[str, Any]) -> str:
    diff_stats = harness.get("diff_stats") if isinstance(harness.get("diff_stats"), dict) else {}
    local = diff_stats.get("local_patch") if isinstance(diff_stats.get("local_patch"), dict) else {}
    if not local:
        return MISSING
    files = local.get("files")
    added = local.get("added_lines")
    deleted = local.get("deleted_lines")
    total = local.get("total_changed_lines")
    return f"{format_number(files)} files, +{format_number(added)} / -{format_number(deleted)} ({format_number(total)} changed)"


def scoring_summary(harness: dict[str, Any]) -> str:
    scoring = harness.get("scoring") if isinstance(harness.get("scoring"), dict) else {}
    if not scoring:
        return MISSING
    parts = [
        f"reward {format_number(scoring.get('reward'))}",
        f"correctness {format_number(scoring.get('correctness'))}",
        f"verifier {format_number(scoring.get('verifier_score'))}",
    ]
    rubric = scoring.get("rubric_status")
    taste = scoring.get("taste_status")
    if rubric:
        parts.append(f"rubric {rubric}")
    if taste:
        parts.append(f"taste {taste}")
    return html_escape("; ".join(parts))


def comparison_table(harnesses: list[dict[str, Any]]) -> str:
    rows = [
        ("Status", lambda h: status_pill(h.get("status"))),
        ("Task pass rate", lambda h: rate_cell(h.get("pass_rate"))),
        ("Verifier command pass", lambda h: rate_cell(h.get("verifier_command_rate"))),
        ("Inner pytest pass", lambda h: rate_cell(h.get("pytest_rate"))),
        ("Provider / model", params_summary),
        ("Wall time", lambda h: html_escape(format_duration(deep_get(h, "timing", "total_s")))),
        ("Iterations", lambda h: html_escape(format_number(deep_get(h, "metrics", "iterations")))),
        ("Tokens / cost", lambda h: html_escape(format_token_cost(h.get("metrics") if isinstance(h.get("metrics"), dict) else {}))),
        ("Diff stats", lambda h: html_escape(diff_summary(h))),
        ("Scoring / judging", scoring_summary),
    ]
    header = "".join(f"<th>{html_escape(h['label'])}</th>" for h in harnesses)
    out = [f'<table class="comparison"><thead><tr><th>Metric</th>{header}</tr></thead><tbody>']
    for label, renderer in rows:
        out.append(f"<tr><th>{html_escape(label)}</th>")
        for harness in harnesses:
            out.append(f"<td>{renderer(harness)}</td>")
        out.append("</tr>")
    out.append("</tbody></table>")
    return "\n".join(out)


def harness_card(harness: dict[str, Any]) -> str:
    params = harness.get("params") if isinstance(harness.get("params"), dict) else {}
    timing = harness.get("timing") if isinstance(harness.get("timing"), dict) else {}
    phases = timing.get("phases") if isinstance(timing.get("phases"), dict) else {}
    metrics = harness.get("metrics") if isinstance(harness.get("metrics"), dict) else {}
    diff_stats = harness.get("diff_stats") if isinstance(harness.get("diff_stats"), dict) else {}
    scoring = harness.get("scoring") if isinstance(harness.get("scoring"), dict) else {}
    failure = harness.get("failure_details") if isinstance(harness.get("failure_details"), dict) else {}
    artifacts = harness.get("artifacts") if isinstance(harness.get("artifacts"), dict) else {}
    notes = harness.get("notes") if isinstance(harness.get("notes"), list) else []

    phase_rows = [(name, format_duration(value)) for name, value in phases.items()]
    if not phase_rows:
        phase_rows = [("phases", MISSING)]

    diff_rows = [
        ("local patch", diff_summary(harness)),
        ("agent patch lines", format_number(diff_stats.get("agent_patch_lines"))),
        ("agent/oracle bloat", format_number(deep_get(diff_stats, "agent_vs_oracle", "bloat_ratio"))),
        ("agent sloc/files/hunks", f"{format_number(deep_get(diff_stats, 'agent_vs_oracle', 'agent_sloc'))} / {format_number(deep_get(diff_stats, 'agent_vs_oracle', 'agent_files'))} / {format_number(deep_get(diff_stats, 'agent_vs_oracle', 'agent_hunks'))}"),
        ("oracle sloc/files/hunks", f"{format_number(deep_get(diff_stats, 'agent_vs_oracle', 'oracle_sloc'))} / {format_number(deep_get(diff_stats, 'agent_vs_oracle', 'oracle_files'))} / {format_number(deep_get(diff_stats, 'agent_vs_oracle', 'oracle_hunks'))}"),
    ]

    failure_items = []
    for item in failure.get("failed_tests", []):
        failure_items.append(f"<li>{html_escape(item)}</li>")
    for item in failure.get("assertions", []):
        failure_items.append(f"<li>{html_escape(item)}</li>")
    if not failure_items:
        failure_items.append(f"<li>{MISSING}</li>")

    artifact_items = []
    for key, value in artifacts.items():
        if value:
            artifact_items.append(f"<li><span>{html_escape(key)}</span><code>{html_escape(value)}</code></li>")
    if not artifact_items:
        artifact_items.append(f"<li>{MISSING}</li>")

    note_items = "".join(f"<li>{html_escape(note)}</li>" for note in notes) if notes else f"<li>{MISSING}</li>"

    return f"""
<section class="card">
  <div class="card-head">
    <h2>{html_escape(harness.get("label", "Harness"))}</h2>
    {status_pill(harness.get("status"))}
  </div>
  <h3>Params</h3>
  {kv_table([
      ("task", harness.get("task_id")),
      ("run id", harness.get("run_id")),
      ("provider", params.get("provider")),
      ("model", params.get("model")),
      ("fair target", f"{params.get('fair_provider') or MISSING} / {params.get('fair_model') or MISSING}"),
      ("agent", params.get("agent")),
      ("artifact", params.get("artifact")),
      ("config delivery", params.get("config_delivery")),
      ("remote home", params.get("remote_home")),
      ("remote home mount", params.get("remote_home_mount")),
      ("task image", params.get("selected_task_image") or params.get("task_base_image")),
      ("verifier provider", params.get("verifier_provider")),
      ("verifier env file", params.get("verifier_env_file")),
      ("verifier base URL", params.get("verifier_openai_base_url")),
      ("verifier judge model", params.get("verifier_judge_model")),
      ("verifier classifier", params.get("verifier_classifier_model")),
      ("verifier tool choice", params.get("verifier_tool_choice_compat") or MISSING),
      ("verifier response format", params.get("verifier_response_format_compat") or MISSING),
      ("verifier timeout x", params.get("verifier_timeout_multiplier") or MISSING),
      ("attempts / concurrency", f"{format_number(params.get('n_attempts'))} / {format_number(params.get('n_concurrent_trials'))}"),
  ], compact=True)}
  <h3>Time</h3>
  {kv_table([("started", timing.get("started_at")), ("finished", timing.get("finished_at")), ("total", format_duration(timing.get("total_s")))] + phase_rows, compact=True)}
  <h3>Harness Metrics</h3>
  {kv_table([
      ("task pass rate", rate_cell(harness.get("pass_rate"))),
      ("verifier command", rate_cell(harness.get("verifier_command_rate"))),
      ("inner pytest", rate_cell(harness.get("pytest_rate"))),
      ("iterations", format_number(metrics.get("iterations"))),
      ("tool calls", format_number(metrics.get("tool_calls"))),
      ("shell calls", format_number(metrics.get("shell_calls"))),
      ("file reads", format_number(metrics.get("file_reads"))),
      ("tokens / cost", format_token_cost(metrics)),
  ], compact=True)}
  <h3>Diff Stats</h3>
  {kv_table(diff_rows, compact=True)}
  <h3>Scoring / Judging</h3>
  {kv_table([
      ("reward", scoring.get("reward")),
      ("correctness", scoring.get("correctness")),
      ("verifier score", scoring.get("verifier_score")),
      ("taste patch bloat", scoring.get("taste_patch_bloat")),
      ("rubric judge ok", scoring.get("rubric_judge_ok")),
      ("taste judge ok", scoring.get("taste_judge_ok")),
      ("rubric status", scoring.get("rubric_status")),
      ("taste status", scoring.get("taste_status")),
  ], compact=True)}
  <h3>Failure Details</h3>
  <ul class="detail-list">{''.join(failure_items)}</ul>
  <h3>Artifacts</h3>
  <ul class="artifact-list">{''.join(artifact_items)}</ul>
  <h3>Notes</h3>
  <ul class="detail-list">{note_items}</ul>
</section>
"""


def render_html(harnesses: list[dict[str, Any]], *, title: str, fair_provider: str, fair_model: str) -> str:
    generated_at = datetime.now().astimezone().isoformat(timespec="seconds")
    task_ids = sorted({str(h.get("task_id")) for h in harnesses if h.get("task_id")})
    cards = "\n".join(harness_card(harness) for harness in harnesses)
    raw_json = html_escape(json.dumps({"generated_at": generated_at, "harnesses": harnesses}, indent=2, sort_keys=True))
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{html_escape(title)}</title>
  <style>
    :root {{
      --base03: #002b36;
      --base02: #073642;
      --base01: #586e75;
      --base00: #657b83;
      --base0: #839496;
      --base1: #93a1a1;
      --base2: #eee8d5;
      --base3: #fdf6e3;
      --yellow: #b58900;
      --orange: #cb4b16;
      --red: #dc322f;
      --magenta: #d33682;
      --violet: #6c71c4;
      --blue: #268bd2;
      --cyan: #2aa198;
      --green: #859900;
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      background: var(--base3);
      color: var(--base00);
      font: 14px/1.5 ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    }}
    main {{ width: min(1380px, calc(100vw - 48px)); margin: 0 auto; padding: 28px 0 48px; }}
    header {{ margin-bottom: 24px; border-bottom: 1px solid var(--base2); padding-bottom: 18px; }}
    h1, h2, h3 {{ color: var(--base03); margin: 0; letter-spacing: 0; }}
    h1 {{ font-size: 28px; line-height: 1.15; }}
    h2 {{ font-size: 18px; }}
    h3 {{ font-size: 13px; margin-top: 20px; margin-bottom: 8px; color: var(--base01); text-transform: uppercase; }}
    p {{ margin: 8px 0 0; max-width: 920px; }}
    code, pre {{ font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace; }}
    code {{ color: var(--base02); overflow-wrap: anywhere; }}
    .subtle {{ color: var(--base01); }}
    .summary-strip {{
      display: flex;
      flex-wrap: wrap;
      gap: 10px;
      margin-top: 16px;
    }}
    .chip {{
      border: 1px solid var(--base2);
      background: #fbf1d3;
      color: var(--base01);
      padding: 6px 9px;
      border-radius: 6px;
      font-size: 13px;
    }}
    .comparison {{
      width: 100%;
      border-collapse: collapse;
      background: #fbf1d3;
      border: 1px solid var(--base2);
      margin: 18px 0 24px;
    }}
    .comparison th, .comparison td {{
      border: 1px solid var(--base2);
      padding: 10px 12px;
      text-align: left;
      vertical-align: top;
    }}
    .comparison thead th {{ background: var(--base2); color: var(--base03); }}
    .comparison tbody th {{ width: 190px; color: var(--base01); background: rgba(238, 232, 213, 0.35); }}
    .grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(380px, 1fr)); gap: 18px; }}
    .card {{
      border: 1px solid var(--base2);
      background: #fbf1d3;
      border-radius: 8px;
      padding: 16px;
      min-width: 0;
    }}
    .card-head {{ display: flex; align-items: center; justify-content: space-between; gap: 12px; }}
    .pill {{
      display: inline-flex;
      align-items: center;
      border-radius: 999px;
      border: 1px solid currentColor;
      padding: 3px 8px;
      font-size: 12px;
      font-weight: 700;
      white-space: nowrap;
    }}
    .pill.good {{ color: var(--green); }}
    .pill.bad {{ color: var(--red); }}
    .pill.warn {{ color: var(--yellow); }}
    .pill.neutral {{ color: var(--base01); }}
    .kv {{ width: 100%; border-collapse: collapse; }}
    .kv th, .kv td {{ border-top: 1px solid var(--base2); padding: 7px 0; text-align: left; vertical-align: top; }}
    .kv th {{ width: 180px; padding-right: 12px; color: var(--base01); font-weight: 600; }}
    .kv.compact th {{ width: 160px; }}
    .hint {{ color: var(--base1); font-size: 12px; margin-top: 2px; }}
    .detail-list, .artifact-list {{ margin: 0; padding-left: 18px; }}
    .detail-list li, .artifact-list li {{ margin: 4px 0; }}
    .artifact-list span {{ display: inline-block; min-width: 110px; color: var(--base01); }}
    details {{ margin-top: 22px; }}
    summary {{ cursor: pointer; color: var(--blue); font-weight: 700; }}
    pre {{
      overflow: auto;
      background: var(--base2);
      color: var(--base03);
      padding: 12px;
      border-radius: 6px;
      border: 1px solid var(--base1);
      max-height: 420px;
    }}
    @media (max-width: 760px) {{
      main {{ width: min(100vw - 24px, 1380px); padding-top: 18px; }}
      .comparison {{ display: block; overflow-x: auto; }}
      .grid {{ grid-template-columns: 1fr; }}
      .kv th {{ width: 130px; }}
    }}
  </style>
</head>
<body>
  <main>
    <header>
      <h1>{html_escape(title)}</h1>
      <p class="subtle">Static Senior SWE-Bench comparison report using Solarized Light. The fair comparison target is {html_escape(fair_provider)} / {html_escape(fair_model)}.</p>
      <div class="summary-strip">
        <span class="chip">generated: {html_escape(generated_at)}</span>
        <span class="chip">tasks: {html_escape(', '.join(task_ids) if task_ids else MISSING)}</span>
        <span class="chip">harnesses: {html_escape(', '.join(h['label'] for h in harnesses))}</span>
      </div>
    </header>
    {comparison_table(harnesses)}
    <div class="grid">
      {cards}
    </div>
    <details>
      <summary>Normalized report JSON</summary>
      <pre>{raw_json}</pre>
    </details>
  </main>
</body>
</html>
"""


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--vis-run", type=Path, required=True, help="Vis Senior SWE-Bench run directory.")
    parser.add_argument("--pi-run", type=Path, help="Optional pi.dev run directory with Harbor-like artifacts.")
    parser.add_argument("--pi-json", type=Path, help="Optional normalized pi.dev comparison JSON.")
    parser.add_argument("--out", type=Path, required=True, help="HTML report output path.")
    parser.add_argument("--title", default="Vis vs pi.dev Senior SWE-Bench Report")
    parser.add_argument("--vis-label", default="Vis")
    parser.add_argument("--pi-label", default="pi.dev")
    parser.add_argument("--vis-provider")
    parser.add_argument("--vis-model")
    parser.add_argument("--pi-provider")
    parser.add_argument("--pi-model")
    parser.add_argument("--fair-provider", default="z.ai plan")
    parser.add_argument("--fair-model", default="glm-turbo")
    args = parser.parse_args()

    vis = collect_run(
        args.vis_run,
        label=args.vis_label,
        harness="Vis",
        provider_override=args.vis_provider,
        model_override=args.vis_model,
        fair_provider=args.fair_provider,
        fair_model=args.fair_model,
    )
    if args.pi_run and args.pi_json:
        raise SystemExit("choose only one of --pi-run or --pi-json")
    if args.pi_run:
        pi = collect_run(
            args.pi_run,
            label=args.pi_label,
            harness="pi.dev",
            provider_override=args.pi_provider,
            model_override=args.pi_model,
            fair_provider=args.fair_provider,
            fair_model=args.fair_model,
        )
    elif args.pi_json:
        pi = external_harness(args.pi_json, args.pi_label, fair_provider=args.fair_provider, fair_model=args.fair_model, task_id=vis.get("task_id"))
    else:
        pi = pending_harness(args.pi_label, task_id=vis.get("task_id"), fair_provider=args.fair_provider, fair_model=args.fair_model)

    html_text = render_html([vis, pi], title=args.title, fair_provider=args.fair_provider, fair_model=args.fair_model)
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(html_text)
    print(f"wrote {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
