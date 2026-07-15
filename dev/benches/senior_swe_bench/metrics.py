#!/usr/bin/env python3
"""Emit Senior SWE-Bench run metrics without collapsing to pass/fail."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


TOKEN_KEYS = [
    "input_tokens",
    "cached_input_tokens",
    "uncached_input_tokens",
    "cache_write_tokens",
    "output_tokens",
    "reasoning_tokens",
    "total_tokens",
]


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


def first_harbor_exception(run_dir: Path) -> dict[str, Any] | None:
    for path in sorted(run_dir.glob("harbor-output/**/result.json")):
        data = load_json(path)
        if isinstance(data, dict) and isinstance(data.get("exception_info"), dict):
            return data["exception_info"]
    return None


def harbor_log_failure_class(run_dir: Path) -> str | None:
    log = run_dir / "harbor.log"
    if not log.exists():
        return None
    text = log.read_text(errors="replace")
    docker_markers = [
        "Docker daemon is not running",
        "Cannot connect to the Docker daemon",
        "Is the docker daemon running",
    ]
    if any(marker in text for marker in docker_markers):
        return "docker_daemon_unavailable"
    return None


def preflight_failure(run_dir: Path) -> dict[str, Any] | None:
    data = load_json(run_dir / "preflight-failure.json")
    return data if isinstance(data, dict) else None


def install_only_success(run_dir: Path) -> bool:
    for path in sorted(run_dir.glob("harbor-output/**/result.json")):
        data = load_json(path)
        if not isinstance(data, dict):
            continue
        config = data.get("config")
        if isinstance(config, dict) and config.get("install_only") is True and data.get("exception_info") is None:
            return True
    return False


def patch_bloat(diff: str) -> dict[str, int]:
    added = deleted = files = 0
    for line in diff.splitlines():
        if line.startswith("diff --git "):
            files += 1
        elif line.startswith("+") and not line.startswith("+++"):
            added += 1
        elif line.startswith("-") and not line.startswith("---"):
            deleted += 1
    return {"files": files, "added_lines": added, "deleted_lines": deleted, "total_changed_lines": added + deleted}


def _cost_usd(cost: dict[str, Any]) -> Any:
    for key in ("total-cost", "total_cost", "cost_usd", "usd"):
        if key in cost:
            return cost[key]
    return None


def _merge_trace_metrics(stats: dict[str, Any], source: dict[str, Any], max_iteration: int) -> int:
    if isinstance(source.get("tokens"), dict):
        stats["tokens"] = source["tokens"]
    if isinstance(source.get("cost"), dict):
        cost = _cost_usd(source["cost"])
        if cost is not None:
            stats["cost_usd"] = cost
    if isinstance(source.get("elapsed_s"), (int, float)):
        stats["elapsed_s"] = source["elapsed_s"]
    if isinstance(source.get("duration-ms"), (int, float)):
        stats["elapsed_s"] = source["duration-ms"] / 1000.0
    if isinstance(source.get("duration_ms"), (int, float)):
        stats["elapsed_s"] = source["duration_ms"] / 1000.0
    iteration_count = source.get("iteration-count", source.get("iteration_count"))
    if isinstance(iteration_count, int):
        max_iteration = max(max_iteration, iteration_count)
    return max_iteration


def summarize_trace(trace: Path) -> dict[str, Any]:
    stats: dict[str, Any] = {
        "source": "vis_trace",
        "trace": str(trace),
        "available": trace.exists(),
        "telemetry_complete": False,
        "iterations": 0,
        "tool_calls": 0,
        "shell_calls": 0,
        "failed_shell_calls": 0,
        "file_reads": 0,
        "elapsed_s": None,
        "tokens": {},
        "cost_usd": None,
        "reasoning_reporting": "unreported",
    }
    if not trace.exists():
        return stats
    max_iteration = 0
    tool_ids: set[str] = set()
    file_read_tools = {"cat", "rg", "find_files", "ls", "outline"}
    for line in trace.read_text(errors="replace").splitlines():
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        payload = obj.get("payload") if isinstance(obj.get("payload"), dict) else {}
        phase = obj.get("phase") or obj.get("type") or payload.get("phase")
        if phase in {"turn-complete", "iteration"}:
            stats["iterations"] += 1
        iteration = obj.get("iteration", payload.get("iteration"))
        if isinstance(iteration, int):
            max_iteration = max(max_iteration, iteration)
        tool_event = payload.get("tool-event") if isinstance(payload.get("tool-event"), dict) else {}
        tool = str(
            obj.get("tool")
            or obj.get("name")
            or payload.get("tool-name")
            or payload.get("tool")
            or payload.get("name")
            or tool_event.get("op")
            or ""
        )
        scope = payload.get("scope")
        if phase in {"form-start", "tool-start"} and (scope or tool):
            tool_id = str(scope or f"{phase}:{iteration}:{payload.get('position')}:{tool}")
            if tool_id not in tool_ids:
                tool_ids.add(tool_id)
                stats["tool_calls"] += 1
                if "shell" in tool:
                    stats["shell_calls"] += 1
                    if obj.get("exit") not in (None, 0) or obj.get("error") or payload.get("error"):
                        stats["failed_shell_calls"] += 1
                if tool in file_read_tools:
                    stats["file_reads"] += 1
        max_iteration = _merge_trace_metrics(stats, obj, max_iteration)
        max_iteration = _merge_trace_metrics(stats, payload, max_iteration)
        final_payload = payload.get("final")
        if isinstance(final_payload, dict):
            max_iteration = _merge_trace_metrics(stats, final_payload, max_iteration)
        eval_evidence = payload.get("eval")
        if isinstance(eval_evidence, dict):
            stats["eval"] = eval_evidence
            stats["eval_valid"] = eval_evidence.get("valid?") is True
            effort_evidence = eval_evidence.get("reasoning-effort")
            if isinstance(effort_evidence, dict) and isinstance(effort_evidence.get("requested"), str):
                stats["reasoning_effort"] = effort_evidence["requested"]
                stats["reasoning_effort_explicit"] = True
    if max_iteration:
        stats["iterations"] = max(stats["iterations"], max_iteration)
    stats["telemetry_complete"] = stats["available"] and stats["iterations"] > 0
    stats["reasoning_reporting"] = "reported" if _first_number(stats["tokens"], "reasoning", "reasoning_tokens") is not None else "unreported"
    return stats


def summarize_pi_trace(trace: Path) -> dict[str, Any]:
    """Normalize pi's JSONL event stream without counting duplicate events."""
    stats: dict[str, Any] = {
        "source": "pi_agent_log",
        "trace": str(trace),
        "available": trace.exists(),
        "telemetry_complete": False,
        "iterations": 0,
        "tool_calls": 0,
        "shell_calls": 0,
        "failed_shell_calls": 0,
        "file_reads": 0,
        "thinking_blocks": 0,
        "reasoning_reporting": "unreported",
        "elapsed_s": None,
        "tokens": {},
        "cost_usd": None,
    }
    if not trace.exists():
        return stats

    events = _jsonl(trace)
    tool_ids: set[str] = set()
    tool_names: dict[str, str] = {}
    failed_tool_ids: set[str] = set()
    totals = {"input": 0, "cached": 0, "output": 0, "total": 0}
    cost = 0.0
    cost_reported = False
    file_read_words = ("cat", "sed", "rg", "grep", "find", "ls", "head", "tail", "git show", "git diff")

    for event in events:
        if event.get("type") == "turn_end":
            stats["iterations"] += 1
        if event.get("type") == "tool_execution_end":
            tool_id = event.get("toolCallId")
            result = event.get("result") if isinstance(event.get("result"), dict) else {}
            if result.get("isError") is True and isinstance(tool_id, str):
                failed_tool_ids.add(tool_id)
        if event.get("type") != "message_end":
            continue
        message = event.get("message") if isinstance(event.get("message"), dict) else {}
        if message.get("role") != "assistant":
            continue
        for block in message.get("content", []) if isinstance(message.get("content"), list) else []:
            if not isinstance(block, dict):
                continue
            if block.get("type") == "thinking":
                stats["thinking_blocks"] += 1
            if block.get("type") != "toolCall":
                continue
            tool_id = block.get("id")
            if not isinstance(tool_id, str) or tool_id in tool_ids:
                continue
            tool_ids.add(tool_id)
            name = str(block.get("name") or "")
            tool_names[tool_id] = name
            if name in {"bash", "shell"}:
                stats["shell_calls"] += 1
            arguments = block.get("arguments") if isinstance(block.get("arguments"), dict) else {}
            command = str(arguments.get("command") or "")
            if any(word in command for word in file_read_words):
                stats["file_reads"] += 1
        usage = message.get("usage") if isinstance(message.get("usage"), dict) else {}
        input_value = usage.get("input") if isinstance(usage.get("input"), (int, float)) else 0
        cache_value = usage.get("cacheRead") if isinstance(usage.get("cacheRead"), (int, float)) else 0
        totals["input"] += input_value + cache_value
        totals["cached"] += cache_value
        for key, aliases in {
            "output": ("output",),
            "total": ("totalTokens", "total_tokens"),
        }.items():
            value = next((usage.get(alias) for alias in aliases if isinstance(usage.get(alias), (int, float))), None)
            if value is not None:
                totals[key] += value
        usage_cost = usage.get("cost") if isinstance(usage.get("cost"), dict) else {}
        if isinstance(usage_cost.get("total"), (int, float)):
            cost += usage_cost["total"]
            cost_reported = True

    stats["tool_calls"] = len(tool_ids)
    stats["failed_shell_calls"] = len(failed_tool_ids & {tool_id for tool_id, name in tool_names.items() if name in {"bash", "shell"}})
    stats["tokens"] = totals
    if cost_reported and cost > 0:
        stats["cost_usd"] = cost
    stats["telemetry_complete"] = stats["iterations"] > 0
    return stats


def _agent_trace(run_dir: Path, vis_trace: dict[str, Any]) -> dict[str, Any]:
    if vis_trace.get("available") and vis_trace.get("source") == "vis_trace":
        return vis_trace
    pi_paths = sorted(run_dir.glob("harbor-output/**/agent/pi.txt"))
    return summarize_pi_trace(pi_paths[0]) if pi_paths else vis_trace


def task_metadata(dataset_copy: Path | None, task_id: str) -> dict[str, Any]:
    if not dataset_copy:
        return {}
    toml = dataset_copy / "tasks" / task_id / "task.toml"
    if not toml.exists():
        return {}
    try:
        import tomllib
        data = tomllib.loads(toml.read_text())
    except ModuleNotFoundError:
        from list_tasks import _mini_toml_metadata
        data = _mini_toml_metadata(toml.read_text())
    md = data.get("metadata", {})
    tx = md.get("taxonomy", {})
    return {"segment": md.get("segment"), "task_type": tx.get("task_type"), "tags": md.get("tags", [])}


def verifier_summary(verifier: Any) -> dict[str, Any] | None:
    if not isinstance(verifier, dict):
        return None
    return {
        "passed": verifier.get("passed"),
        "total": verifier.get("total"),
        "all_pass": verifier.get("all_pass"),
        "runner_errors": verifier.get("runner_errors"),
    }


def judge_summary(judge_output: Any) -> dict[str, Any] | None:
    if not isinstance(judge_output, dict):
        return None
    keys = [
        "rubric_status",
        "rubric_error",
        "taste_status",
        "taste_error",
        "patch_bloat",
        "patch_classifications",
    ]
    return {key: judge_output[key] for key in keys if key in judge_output}


def first_present(mapping: dict[str, Any], *keys: str) -> Any:
    for key in keys:
        if key in mapping:
            return mapping[key]
    return None


def _number(value: Any) -> int | float | None:
    return value if isinstance(value, (int, float)) and not isinstance(value, bool) else None


def _first_number(mapping: dict[str, Any], *keys: str) -> int | float | None:
    for key in keys:
        value = _number(mapping.get(key))
        if value is not None:
            return value
    return None


def _usage_event(event: dict[str, Any]) -> dict[str, Any]:
    usage = event.get("usage") if isinstance(event.get("usage"), dict) else event
    prompt_details = (
        usage.get("prompt_tokens_details") if isinstance(usage.get("prompt_tokens_details"), dict) else {}
    )
    completion_details = (
        usage.get("completion_tokens_details")
        if isinstance(usage.get("completion_tokens_details"), dict)
        else {}
    )
    input_tokens = _first_number(usage, "prompt_tokens", "input_tokens", "input")
    cached_tokens = _first_number(
        usage,
        "cached_input_tokens",
        "cache_read_input_tokens",
        "cache_read_tokens",
        "cached_tokens",
    )
    if cached_tokens is None:
        cached_tokens = _first_number(prompt_details, "cached_tokens", "cache_read_tokens")
    cache_write_tokens = _first_number(
        usage,
        "cache_write_tokens",
        "cache_creation_input_tokens",
        "cache_creation_tokens",
    )
    output_tokens = _first_number(usage, "completion_tokens", "output_tokens", "output")
    reasoning_tokens = _first_number(usage, "reasoning_tokens")
    if reasoning_tokens is None:
        reasoning_tokens = _first_number(completion_details, "reasoning_tokens")
    total_tokens = _first_number(usage, "total_tokens", "total")
    if total_tokens is None and input_tokens is not None and output_tokens is not None:
        total_tokens = input_tokens + output_tokens
    uncached_tokens = None
    if input_tokens is not None and cached_tokens is not None:
        uncached_tokens = max(0, input_tokens - cached_tokens)
    cost_usd = _first_number(event, "reported_cost_usd", "cost_usd", "cost")
    available = any(value is not None for value in (input_tokens, output_tokens, total_tokens))
    return {
        "available": available,
        "core_complete": all(value is not None for value in (input_tokens, output_tokens, total_tokens)),
        "source": event.get("source"),
        "purpose": event.get("purpose"),
        "model": event.get("model"),
        "input_tokens": input_tokens,
        "cached_input_tokens": cached_tokens,
        "uncached_input_tokens": uncached_tokens,
        "cache_write_tokens": cache_write_tokens,
        "output_tokens": output_tokens,
        "reasoning_tokens": reasoning_tokens,
        "total_tokens": total_tokens,
        "reported_cost_usd": cost_usd,
    }


def _aggregate_usage_events(events: list[dict[str, Any]], *, scope: str) -> dict[str, Any]:
    normalized = [item for event in events if (item := _usage_event(event))["available"]]
    token_values = {
        key: [item[key] for item in normalized if isinstance(item.get(key), (int, float))]
        for key in TOKEN_KEYS
    }
    costs = [
        item["reported_cost_usd"]
        for item in normalized
        if isinstance(item.get("reported_cost_usd"), (int, float))
    ]
    purposes = sorted({str(item["purpose"]) for item in normalized if item.get("purpose")})
    models = sorted({str(item["model"]) for item in normalized if item.get("model")})
    sources = sorted({str(item["source"]) for item in normalized if item.get("source")})
    return {
        "scope": scope,
        "available": bool(normalized),
        "complete": bool(normalized) and all(item["core_complete"] for item in normalized),
        "calls": len(normalized),
        "sources": sources,
        "purposes": purposes,
        "models": models,
        **{key: sum(values) if values else None for key, values in token_values.items()},
        "token_coverage": {key: len(values) for key, values in token_values.items()},
        "reported_cost_usd": sum(costs) if costs else None,
        "calls_with_reported_cost": len(costs),
    }


def _jsonl(path: Path) -> list[dict[str, Any]]:
    events = []
    if not path.exists():
        return events
    for line in path.read_text(errors="replace").splitlines():
        try:
            item = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(item, dict):
            events.append(item)
    return events


def _judge_usage_events(run_dir: Path) -> list[dict[str, Any]]:
    events: list[dict[str, Any]] = []
    for path in sorted(run_dir.glob("harbor-output/**/llm_usage.jsonl")):
        events.extend(_jsonl(path))
    return events


def _validation_agent_usage_events(run_dir: Path) -> list[dict[str, Any]]:
    events: list[dict[str, Any]] = []
    for path in sorted(run_dir.glob("harbor-output/**/miniswe/*.trajectory.json")):
        data = load_json(path)
        if not isinstance(data, dict):
            continue
        for message in data.get("messages", []):
            extra = message.get("extra") if isinstance(message, dict) and isinstance(message.get("extra"), dict) else {}
            response = extra.get("response") if isinstance(extra.get("response"), dict) else {}
            usage = response.get("usage") if isinstance(response.get("usage"), dict) else None
            if usage:
                events.append(
                    {
                        "source": "validation_agent_trajectory",
                        "purpose": "validation_agent",
                        "model": response.get("model"),
                        "usage": usage,
                        "reported_cost_usd": extra.get("cost"),
                    }
                )
    for path in sorted(run_dir.glob("harbor-output/**/cc_stream/*.jsonl")):
        result_events = [event for event in _jsonl(path) if event.get("type") == "result"]
        if not result_events:
            continue
        result = result_events[-1]
        usage = result.get("usage") if isinstance(result.get("usage"), dict) else None
        if usage:
            events.append(
                {
                    "source": "validation_agent_trajectory",
                    "purpose": "validation_agent",
                    "model": result.get("model"),
                    "usage": usage,
                    "reported_cost_usd": result.get("total_cost_usd", result.get("cost_usd")),
                }
            )
    return events


def verifier_usage(run_dir: Path, command: dict[str, Any], segment: Any) -> dict[str, Any]:
    judge_events = _judge_usage_events(run_dir)
    validation_events = _validation_agent_usage_events(run_dir)
    judges = _aggregate_usage_events(judge_events, scope="verifier_judges")
    validation_agent = _aggregate_usage_events(validation_events, scope="validation_agent")
    aggregate = _aggregate_usage_events(judge_events + validation_events, scope="verifier_total")
    validation_required = str(segment).lower() == "design"
    capture = command.get("vis_bench_verifier_usage_capture") or None
    aggregate.update(
        {
            "capture": capture,
            "capture_instrumented": bool(capture),
            "validation_required": validation_required,
            "complete": bool(capture)
            and judges["complete"]
            and (not validation_required or validation_agent["complete"]),
            "components": {
                "judges": judges,
                "validation_agent": validation_agent,
            },
        }
    )
    return aggregate


def normalized_usage(trace_stats: dict[str, Any], trial_result: dict[str, Any]) -> dict[str, Any]:
    tokens = trace_stats.get("tokens") if isinstance(trace_stats.get("tokens"), dict) else {}
    agent_result = trial_result.get("agent_result") if isinstance(trial_result.get("agent_result"), dict) else {}
    if trace_stats.get("available") and tokens:
        input_tokens = _first_number(tokens, "input", "input_tokens", "prompt")
        cached_tokens = _first_number(tokens, "cached", "cache-read", "cache_read", "cache_tokens")
        cache_write_tokens = _first_number(tokens, "cache-created", "cache_write", "cache_write_tokens")
        output_tokens = _first_number(tokens, "output", "output_tokens", "completion")
        reasoning_tokens = _first_number(tokens, "reasoning", "reasoning_tokens")
        total_tokens = _first_number(tokens, "total", "total_tokens")
        cost_usd = _number(trace_stats.get("cost_usd"))
        source = trace_stats.get("source") or "agent_trace"
    else:
        input_tokens = _number(agent_result.get("n_input_tokens"))
        cached_tokens = _number(agent_result.get("n_cache_tokens"))
        cache_write_tokens = _number(agent_result.get("n_cache_write_tokens"))
        output_tokens = _number(agent_result.get("n_output_tokens"))
        reasoning_tokens = _number(agent_result.get("n_reasoning_tokens"))
        total_tokens = None
        cost_usd = _number(agent_result.get("cost_usd"))
        source = "harbor_agent_result"
    if total_tokens is None and input_tokens is not None and output_tokens is not None:
        total_tokens = input_tokens + output_tokens
    uncached_tokens = None
    if input_tokens is not None and cached_tokens is not None:
        uncached_tokens = max(0, input_tokens - cached_tokens)
    available = input_tokens is not None or output_tokens is not None or total_tokens is not None
    complete = all(value is not None for value in (input_tokens, cached_tokens, output_tokens, total_tokens))
    return {
        "scope": "agent",
        "available": available,
        "complete": complete,
        "source": source if available else None,
        "input_tokens": input_tokens,
        "cached_input_tokens": cached_tokens,
        "uncached_input_tokens": uncached_tokens,
        "cache_write_tokens": cache_write_tokens,
        "output_tokens": output_tokens,
        "reasoning_tokens": reasoning_tokens,
        "total_tokens": total_tokens,
        "reported_cost_usd": cost_usd,
    }


def completion_summary(
    reward: Any,
    harbor_exception: dict[str, Any] | None,
    install_only_ok: bool,
    verifier: Any,
    judge_output: Any,
    validation_status: Any,
    validation_results: Any,
    segment: Any,
) -> dict[str, Any]:
    reward_dict = reward if isinstance(reward, dict) else {}
    verifier_dict = verifier if isinstance(verifier, dict) else {}
    judge_dict = judge_output if isinstance(judge_output, dict) else {}
    validation_status_dict = validation_status if isinstance(validation_status, dict) else {}
    validation_results_dict = validation_results if isinstance(validation_results, dict) else {}
    resolved = reward_dict.get("resolved")
    score = _first_number(reward_dict, "reward", "correctness", "verifier_score")
    scoreable = isinstance(resolved, bool) or score is not None
    passed = resolved if isinstance(resolved, bool) else (score >= 1 if score is not None else None)
    rubric = reward_dict.get("rubric") if isinstance(reward_dict.get("rubric"), dict) else {}
    taste = reward_dict.get("taste") if isinstance(reward_dict.get("taste"), dict) else {}
    rubric_status = rubric.get("status") or judge_dict.get("rubric_status")
    taste_status = taste.get("status") or judge_dict.get("taste_status")
    verifier_complete = isinstance(verifier_dict.get("total"), int) and verifier_dict.get("total", 0) > 0
    judges_complete = rubric_status == "ok" and taste_status == "ok"
    validation_required = str(segment).lower() == "design"
    validation_score = _first_number(validation_results_dict, "validation_score", "score")
    validation_complete = not validation_required or (
        validation_status_dict.get("returncode") == 0
        and not validation_results_dict.get("validation_agent_infrastructure_failure")
        and validation_score is not None
    )
    complete = bool(
        scoreable
        and not harbor_exception
        and verifier_complete
        and judges_complete
        and validation_complete
    )
    if install_only_ok:
        status = "install_only"
    elif harbor_exception:
        status = "unscoreable_error"
    elif complete:
        status = "complete_pass" if passed else "complete_fail"
    elif scoreable and not verifier_complete:
        status = "incomplete_verifier"
    elif scoreable and not judges_complete:
        status = "incomplete_judge"
    elif scoreable and not validation_complete:
        status = "incomplete_validation"
    else:
        status = "unscoreable"
    return {
        "status": status,
        "complete": complete,
        "scoreable": scoreable,
        "passed": passed,
        "score": score,
        "verifier_complete": verifier_complete,
        "rubric_status": rubric_status,
        "taste_status": taste_status,
        "judges_complete": judges_complete,
        "validation_required": validation_required,
        "validation_complete": validation_complete,
        "exception_type": harbor_exception.get("exception_type") if harbor_exception else None,
    }


def agent_summary(
    command: dict[str, Any],
    trial_result: dict[str, Any],
    trace_stats: dict[str, Any],
    preflight: dict[str, Any],
) -> dict[str, Any]:
    agent_info = trial_result.get("agent_info") if isinstance(trial_result.get("agent_info"), dict) else {}
    model_info = agent_info.get("model_info") if isinstance(agent_info.get("model_info"), dict) else {}
    harness = command.get("bench_agent_label") or ("Vis" if command.get("vis_provider") else None)
    raw_model = model_info.get("name") or command.get("bench_model") or command.get("vis_model")
    model_provider = raw_model.split("/", 1)[0] if isinstance(raw_model, str) and "/" in raw_model else None
    model = raw_model.split("/", 1)[1] if isinstance(raw_model, str) and "/" in raw_model else raw_model
    provider = model_info.get("provider") or model_provider
    if not provider and str(harness).lower() == "vis":
        provider = command.get("vis_provider")
    requested_version = command.get("bench_agent_version_requested") or None
    version = agent_info.get("version") or None
    if isinstance(version, str) and version.lower() == "unknown":
        version = None
    artifact = preflight.get("artifact") if str(harness).lower() == "vis" and isinstance(preflight.get("artifact"), dict) else None
    return {
        "name": agent_info.get("name") or command.get("bench_agent_label") or command.get("bench_agent"),
        "harness": harness,
        "version": version,
        "requested_version": requested_version,
        "version_matches_requested": version == requested_version if requested_version else None,
        "provider": provider,
        "model": model,
        "reasoning_effort": trace_stats.get("reasoning_effort")
        or command.get("bench_agent_reasoning_effort"),
        "reasoning_effort_explicit": trace_stats.get("reasoning_effort_explicit") is True
        or command.get("bench_agent_reasoning_effort_explicit") is True,
        "pi_thinking_level": command.get("bench_agent_pi_thinking_level") or None,
        "output_cap": command.get("bench_agent_output_cap"),
        "vis_eval_valid": trace_stats.get("eval_valid")
        if str(harness).lower() == "vis"
        else None,
        "pi_models_sha256": command.get("bench_agent_pi_models_sha256") or None,
        "endpoint": command.get("bench_agent_endpoint") or None,
        "route": command.get("bench_agent_route") or None,
        "artifact": artifact,
        "usage": normalized_usage(trace_stats, trial_result),
    }


def failure_class(
    resolved: Any,
    reward: Any,
    verifier: Any,
    harbor_exception: dict[str, Any] | None,
    preflight_error: dict[str, Any] | None = None,
    log_failure: str | None = None,
    install_only_ok: bool = False,
) -> str | None:
    if resolved:
        return None
    if install_only_ok:
        return None
    if preflight_error and preflight_error.get("failure_class"):
        return str(preflight_error["failure_class"])
    if harbor_exception and harbor_exception.get("exception_type"):
        return str(harbor_exception["exception_type"])
    if isinstance(verifier, dict):
        if verifier.get("runner_errors"):
            return "verifier_runner_failure"
        if verifier.get("total") and verifier.get("all_pass") is False:
            return "verifier_failed"
    if isinstance(reward, dict) and reward.get("reward") == 0:
        return "reward_zero"
    if isinstance(reward, dict) and _first_number(reward, "reward", "correctness", "verifier_score") is not None:
        return None
    if log_failure:
        return log_failure
    return "unknown"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--task-id", required=True)
    ap.add_argument("--run-dir", type=Path, required=True)
    ap.add_argument("--harbor-reward", type=Path)
    ap.add_argument("--patch", type=Path)
    ap.add_argument("--trace", type=Path)
    ap.add_argument("--dataset-copy", type=Path)
    ap.add_argument("--out", type=Path, required=True)
    args = ap.parse_args()

    reward = (
        load_json(args.harbor_reward)
        if args.harbor_reward
        else first_json(args.run_dir, "reward_details.json", "reward.json", "score.json")
    )
    verifier = first_json(args.run_dir, "verifier_results.json")
    judge_output = first_json(args.run_dir, "judge_output.json")
    validation_status = first_json(args.run_dir, "validation_agent_status.json")
    validation_results = first_json(args.run_dir, "validation_results.json")
    command = load_json(args.run_dir / "command.json")
    preflight = load_json(args.run_dir / "preflight.json")
    trial_result = first_json(args.run_dir, "result.json")
    command = command if isinstance(command, dict) else {}
    preflight = preflight if isinstance(preflight, dict) else {}
    trial_result = trial_result if isinstance(trial_result, dict) else {}
    harbor_exception = first_harbor_exception(args.run_dir)
    preflight_error = preflight_failure(args.run_dir)
    log_failure = harbor_log_failure_class(args.run_dir)
    install_only_ok = install_only_success(args.run_dir)
    patch_text = args.patch.read_text(errors="replace") if args.patch and args.patch.exists() else ""
    trace = args.trace or (args.run_dir / "vis-traces" / args.task_id / "vis.trace.jsonl")
    meta = task_metadata(args.dataset_copy, args.task_id)
    vis_trace_stats = summarize_trace(trace)
    trace_stats = _agent_trace(args.run_dir, vis_trace_stats)
    if command.get("bench_agent_reasoning_effort"):
        trace_stats = dict(trace_stats)
        trace_stats.setdefault("reasoning_effort", command["bench_agent_reasoning_effort"])
        trace_stats["reasoning_effort_explicit"] = (
            command.get("bench_agent_reasoning_effort_explicit") is True
        )
    verifier_model_usage = verifier_usage(args.run_dir, command, meta.get("segment"))
    completion = completion_summary(
        reward,
        harbor_exception,
        install_only_ok,
        verifier,
        judge_output,
        validation_status,
        validation_results,
        meta.get("segment"),
    )

    resolved = None
    verifier_pass = None
    validation_agent_pass = None
    rubric_score = None
    if isinstance(reward, dict):
        resolved = reward.get("resolved")
        verifier_pass = first_present(reward, "verifier_pass", "tests_passed")
        validation_agent_pass = first_present(reward, "validation_agent_pass")
        rubric_score = first_present(reward, "rubric_score", "score")
    if isinstance(verifier, dict) and verifier_pass is None and verifier.get("total") is not None:
        verifier_pass = verifier.get("all_pass")

    failure = failure_class(
        resolved,
        reward,
        verifier,
        harbor_exception,
        preflight_error,
        log_failure,
        install_only_ok,
    )
    if completion["status"].startswith("incomplete_"):
        failure = completion["status"]

    summary = {
        "task_id": args.task_id,
        "segment": meta.get("segment", "Investigate-and-Fix"),
        "harbor_reward": reward,
        "harbor_exception": harbor_exception,
        "preflight_failure": preflight_error,
        "resolved": bool(resolved) if resolved is not None else None,
        "tasteful_solve": False,
        "verifier_pass": bool(verifier_pass) if verifier_pass is not None else None,
        "verifier": verifier_summary(verifier),
        "judge": judge_summary(judge_output),
        "install_only_success": install_only_ok,
        "validation_agent_pass": bool(validation_agent_pass) if validation_agent_pass is not None else None,
        "rubric_score": rubric_score,
        "patch_bloat": patch_bloat(patch_text),
        "vis": vis_trace_stats,
        "agent_trace": trace_stats,
        "agent": agent_summary(command, trial_result, trace_stats, preflight),
        "verifier_usage": verifier_model_usage,
        "harbor": {
            "version": command.get("harbor_version"),
            "locked_version": command.get("harbor_version_locked"),
            "version_matches_lock": command.get("harbor_version") == command.get("harbor_version_locked")
            if command.get("harbor_version_locked")
            else None,
        },
        "task_checksum": trial_result.get("task_checksum"),
        "completion": completion,
        "failure_class": failure,
    }
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(summary, indent=2, sort_keys=True))
    print(json.dumps(summary, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
