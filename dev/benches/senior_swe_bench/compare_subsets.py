#!/usr/bin/env python3
"""Compare two Senior SWE-Bench subset ledgers with explicit evidence gates."""
from __future__ import annotations

import argparse
import html
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any
from urllib.parse import urlparse


HERE = Path(__file__).resolve().parent
REPORTS_DIR = HERE / "results" / "reports"
TOKEN_KEYS = [
    "input_tokens",
    "cached_input_tokens",
    "uncached_input_tokens",
    "cache_write_tokens",
    "output_tokens",
    "reasoning_tokens",
    "total_tokens",
]
PI_GLM52_MODELS_SHA256 = "c8663c7d2287c51cea995b3e5c47a2c7f94dce8dc1e629ee5c89ae6682c673a9"


def load_ledger(path: Path) -> dict[str, Any]:
    data = json.loads(path.read_text())
    if not isinstance(data, dict) or not isinstance(data.get("tasks"), list):
        raise SystemExit(f"invalid subset ledger: {path}")
    return data


def _first(items: Any) -> dict[str, Any]:
    return items[0] if isinstance(items, list) and len(items) == 1 and isinstance(items[0], dict) else {}


def _task_map(ledger: dict[str, Any]) -> tuple[dict[str, dict[str, Any]], bool]:
    tasks: dict[str, dict[str, Any]] = {}
    duplicate = False
    for task in ledger.get("tasks", []):
        if not isinstance(task, dict) or not task.get("task_id"):
            continue
        task_id = str(task["task_id"])
        duplicate = duplicate or task_id in tasks
        tasks[task_id] = task
    return tasks, duplicate


def _side(label: str, path: Path, ledger: dict[str, Any]) -> dict[str, Any]:
    provenance = ledger.get("provenance") if isinstance(ledger.get("provenance"), dict) else {}
    agent = _first(provenance.get("agents"))
    completion = ledger.get("completion") if isinstance(ledger.get("completion"), dict) else {}
    agent_usage = (
        ledger.get("agent_usage")
        if isinstance(ledger.get("agent_usage"), dict)
        else ledger.get("usage")
        if isinstance(ledger.get("usage"), dict)
        else {}
    )
    verifier_usage = ledger.get("verifier_usage") if isinstance(ledger.get("verifier_usage"), dict) else {}
    return {
        "label": label,
        "ledger": str(path.resolve()),
        "subset_name": ledger.get("subset_name"),
        "task_count": ledger.get("total_tasks"),
        "agent": agent,
        "completion": completion,
        "usage": agent_usage,
        "agent_usage": agent_usage,
        "verifier_usage": verifier_usage,
        "spend_reporting_complete": ledger.get("spend_reporting_complete") is True,
        "tool_telemetry_complete": ledger.get("tool_telemetry_complete") is True,
        "agent_telemetry": ledger.get("agent_telemetry") if isinstance(ledger.get("agent_telemetry"), dict) else {},
        "secret_redaction_complete": ledger.get("secret_redaction_complete") is True,
        "comparison_ready": ledger.get("comparison_ready") is True,
        "dataset_commits": provenance.get("dataset_commits") or [],
        "verifiers": provenance.get("verifiers") or [],
        "artifacts": provenance.get("artifacts") or [],
        "harbor_versions": provenance.get("harbor_versions") or [],
        "harbor_locked_versions": provenance.get("harbor_locked_versions") or [],
        "provenance_consistent": provenance.get("consistent") is True,
    }


def _gate(passed: bool, detail: str) -> dict[str, Any]:
    return {"passed": bool(passed), "detail": detail}


def _number(value: Any) -> int | float | None:
    return value if isinstance(value, (int, float)) and not isinstance(value, bool) else None


def _numeric_comparison(left: Any, right: Any) -> dict[str, Any]:
    left_number = _number(left)
    right_number = _number(right)
    return {
        "vis": left_number,
        "pi": right_number,
        "delta_pi_minus_vis": right_number - left_number if left_number is not None and right_number is not None else None,
        "ratio_pi_over_vis": right_number / left_number if left_number not in (None, 0) and right_number is not None else None,
    }


def _normalized_url(value: Any) -> str | None:
    return value.rstrip("/") if isinstance(value, str) and value else None


def _known_version(value: Any) -> bool:
    return isinstance(value, str) and bool(value) and value.lower() != "unknown"


def _endpoint_matches_route(endpoint: str | None, route: Any) -> bool:
    if not endpoint or not isinstance(route, str) or not route:
        return False
    if route == "zai-coding-plan":
        parsed = urlparse(endpoint)
        return (
            parsed.scheme == "https"
            and parsed.hostname == "api.z.ai"
            and parsed.path.rstrip("/") in {"/api/anthropic/v1", "/api/coding/paas/v4"}
        )
    return True


def _task_values(tasks: dict[str, dict[str, Any]], *keys: str) -> dict[str, Any]:
    values = {}
    for task_id, task in tasks.items():
        value: Any = task
        for key in keys:
            value = value.get(key) if isinstance(value, dict) else None
        values[task_id] = value
    return values


def build_comparison(
    vis_path: Path,
    pi_path: Path,
    *,
    vis_label: str = "Vis",
    pi_label: str = "pi.dev",
) -> dict[str, Any]:
    vis_ledger = load_ledger(vis_path)
    pi_ledger = load_ledger(pi_path)
    vis_tasks, vis_duplicates = _task_map(vis_ledger)
    pi_tasks, pi_duplicates = _task_map(pi_ledger)
    vis = _side(vis_label, vis_path, vis_ledger)
    pi = _side(pi_label, pi_path, pi_ledger)

    task_set_match = bool(vis_tasks) and set(vis_tasks) == set(pi_tasks)
    dataset_match = (
        len(vis["dataset_commits"]) == 1
        and len(pi["dataset_commits"]) == 1
        and vis["dataset_commits"] == pi["dataset_commits"]
    )
    model_match = bool(vis["agent"].get("model")) and vis["agent"].get("model") == pi["agent"].get("model")
    vis_effort = vis["agent"].get("reasoning_effort")
    pi_effort = pi["agent"].get("reasoning_effort")
    pi_thinking_level = pi["agent"].get("pi_thinking_level")
    expected_pi_thinking_level = {"high": "high", "max": "xhigh"}.get(pi_effort)
    reasoning_control_present = (
        vis["agent"].get("reasoning_effort_explicit") is True
        and pi["agent"].get("reasoning_effort_explicit") is True
        and vis_effort in {"high", "max"}
        and pi_effort in {"high", "max"}
        and isinstance(pi_thinking_level, str)
        and bool(pi_thinking_level)
        and pi["agent"].get("pi_models_sha256") == PI_GLM52_MODELS_SHA256
    )
    reasoning_effort_match = reasoning_control_present and vis_effort == pi_effort
    pi_effort_translation = reasoning_control_present and pi_thinking_level == expected_pi_thinking_level
    vis_output_cap = _number(vis["agent"].get("output_cap"))
    pi_output_cap = _number(pi["agent"].get("output_cap"))
    output_cap_match = vis_output_cap == pi_output_cap == 32_768
    vis_eval_values = _task_values(vis_tasks, "agent", "vis_eval_valid")
    vis_eval_valid = bool(vis_tasks) and all(value is True for value in vis_eval_values.values())
    vis_endpoint = _normalized_url(vis["agent"].get("endpoint"))
    pi_endpoint = _normalized_url(pi["agent"].get("endpoint"))
    vis_route = vis["agent"].get("route")
    pi_route = pi["agent"].get("route")
    provider_route_match = bool(vis_route and vis_route == pi_route) and (
        _endpoint_matches_route(vis_endpoint, vis_route)
        and _endpoint_matches_route(pi_endpoint, pi_route)
        and (vis_route == "zai-coding-plan" or vis_endpoint == pi_endpoint)
    )
    verifier_match = len(vis["verifiers"]) == 1 and vis["verifiers"] == pi["verifiers"]
    task_checksum_match = task_set_match and all(_task_values(vis_tasks, "task_checksum").values()) and (
        _task_values(vis_tasks, "task_checksum") == _task_values(pi_tasks, "task_checksum")
    )
    task_environment_match = task_set_match and all(_task_values(vis_tasks, "task_base_image").values()) and (
        {
            task_id: (task.get("task_base_image"), task.get("selected_task_image"))
            for task_id, task in vis_tasks.items()
        }
        == {
            task_id: (task.get("task_base_image"), task.get("selected_task_image"))
            for task_id, task in pi_tasks.items()
        }
    )
    vis_artifact = _first(vis["artifacts"])
    vis_artifact_identified = bool(
        vis_artifact.get("native_sha256") and vis_artifact.get("vis_revision") and vis_artifact.get("native_elf")
    )
    vis_artifact_clean = vis_artifact_identified and vis_artifact.get("source_dirty") is False
    pi_version_pinned = bool(pi["agent"].get("requested_version")) and (
        pi["agent"].get("version_matches_requested") is True
    )
    vis_version_identified = _known_version(vis["agent"].get("version"))
    harbor_version_match = (
        len(vis["harbor_versions"]) == 1
        and vis["harbor_versions"] == pi["harbor_versions"]
        and vis["harbor_versions"] == vis["harbor_locked_versions"]
        and pi["harbor_versions"] == pi["harbor_locked_versions"]
    )
    gates = {
        "vis_complete": _gate(vis["comparison_ready"], "all Vis tasks completed verifier, judges, required validation, and agent token accounting"),
        "pi_complete": _gate(pi["comparison_ready"], "all pi.dev tasks completed verifier, judges, required validation, and agent token accounting"),
        "token_spend_complete": _gate(
            vis["spend_reporting_complete"] and pi["spend_reporting_complete"],
            "agent spend and verifier overhead are captured for every task on both sides",
        ),
        "tool_telemetry_complete": _gate(
            vis["tool_telemetry_complete"] and pi["tool_telemetry_complete"],
            "both sides retain parseable agent rollout traces with iterations and tool calls",
        ),
        "secret_redaction_complete": _gate(
            vis["secret_redaction_complete"] and pi["secret_redaction_complete"],
            "every result tree was scanned after redaction with credential values loaded and no occurrences remaining",
        ),
        "unique_task_ids": _gate(not vis_duplicates and not pi_duplicates, "neither ledger contains duplicate task ids"),
        "task_set_match": _gate(task_set_match, f"Vis={len(vis_tasks)} tasks, pi.dev={len(pi_tasks)} tasks"),
        "dataset_match": _gate(dataset_match, f"Vis={vis['dataset_commits']}, pi.dev={pi['dataset_commits']}"),
        "task_checksum_match": _gate(task_checksum_match, "every paired Harbor task checksum is present and equal"),
        "task_environment_match": _gate(
            task_environment_match,
            "base and selected task images are equal for every paired task",
        ),
        "model_match": _gate(model_match, f"Vis={vis['agent'].get('model')}, pi.dev={pi['agent'].get('model')}"),
        "reasoning_control_present": _gate(
            reasoning_control_present,
            "both sides explicitly record provider-native high|max; pi.dev also records its translated level and pinned models.json hash",
        ),
        "reasoning_effort_match": _gate(
            reasoning_effort_match,
            f"Vis={vis_effort or 'not reported'}, pi.dev={pi_effort or 'not reported'}",
        ),
        "pi_effort_translation": _gate(
            pi_effort_translation,
            f"provider={pi_effort or 'not reported'}, Pi={pi_thinking_level or 'not reported'}, "
            f"expected={expected_pi_thinking_level or 'not available'}",
        ),
        "output_cap_match": _gate(
            output_cap_match,
            f"Vis={vis_output_cap if vis_output_cap is not None else 'not reported'}, "
            f"pi.dev={pi_output_cap if pi_output_cap is not None else 'not reported'}; required=32768",
        ),
        "vis_eval_valid": _gate(
            vis_eval_valid,
            f"valid Vis reasoning evidence={sum(value is True for value in vis_eval_values.values())}/{len(vis_tasks)} tasks",
        ),
        "provider_route_match": _gate(
            provider_route_match,
            f"Vis={vis['agent'].get('provider')} route={vis_route or 'not reported'} @ {vis_endpoint or 'not reported'}, "
            f"pi.dev={pi['agent'].get('provider')} route={pi_route or 'not reported'} @ {pi_endpoint or 'not reported'}",
        ),
        "vis_version_identified": _gate(vis_version_identified, f"Vis={vis['agent'].get('version')}"),
        "pi_version_pinned": _gate(
            pi_version_pinned,
            f"requested={pi['agent'].get('requested_version')}, actual={pi['agent'].get('version')}",
        ),
        "vis_artifact_identified": _gate(
            vis_artifact_identified,
            f"revision={vis_artifact.get('vis_revision')}, sha256={vis_artifact.get('native_sha256')}",
        ),
        "vis_artifact_clean": _gate(
            vis_artifact_clean,
            f"source_dirty={vis_artifact.get('source_dirty')}, source_status_count={vis_artifact.get('source_status_count')}",
        ),
        "harbor_version_match": _gate(
            harbor_version_match,
            f"Vis={vis['harbor_versions']} locked={vis['harbor_locked_versions']}; "
            f"pi.dev={pi['harbor_versions']} locked={pi['harbor_locked_versions']}",
        ),
        "verifier_match": _gate(verifier_match, "judge, classifier, validation agent, endpoint, and compatibility settings match"),
        "provenance_consistent": _gate(
            vis["provenance_consistent"] and pi["provenance_consistent"],
            "each ledger uses one dataset commit, agent configuration, and verifier configuration",
        ),
    }
    data_complete = all(
        gates[key]["passed"] for key in ("vis_complete", "pi_complete", "unique_task_ids", "task_set_match")
    )
    authoritative = data_complete and all(gate["passed"] for gate in gates.values())

    def compare_usage(scope: str) -> dict[str, Any]:
        vis_usage = vis[scope]
        pi_usage = pi[scope]
        vis_tokens = vis_usage.get("tokens") if isinstance(vis_usage.get("tokens"), dict) else {}
        pi_tokens = pi_usage.get("tokens") if isinstance(pi_usage.get("tokens"), dict) else {}
        compared = {key: _numeric_comparison(vis_tokens.get(key), pi_tokens.get(key)) for key in TOKEN_KEYS}
        compared["reported_cost_usd"] = _numeric_comparison(
            vis_usage.get("reported_cost_usd"), pi_usage.get("reported_cost_usd")
        )
        return compared

    aggregate_usage = compare_usage("agent_usage")
    verifier_usage_comparison = compare_usage("verifier_usage")

    task_rows = []
    for task_id in sorted(set(vis_tasks) | set(pi_tasks)):
        left = vis_tasks.get(task_id, {})
        right = pi_tasks.get(task_id, {})
        left_usage = left.get("usage") if isinstance(left.get("usage"), dict) else {}
        right_usage = right.get("usage") if isinstance(right.get("usage"), dict) else {}
        left_verifier_usage = left.get("verifier_usage") if isinstance(left.get("verifier_usage"), dict) else {}
        right_verifier_usage = right.get("verifier_usage") if isinstance(right.get("verifier_usage"), dict) else {}
        task_rows.append(
            {
                "task_id": task_id,
                "vis": {
                    "completion": left.get("completion") or {},
                    "usage": left_usage,
                    "verifier_usage": left_verifier_usage,
                    "failure_class": left.get("failure_class"),
                    "run_dir": left.get("run_dir"),
                    "exit_status": left.get("exit_status"),
                    "task_checksum": left.get("task_checksum"),
                    "agent_trace": left.get("agent_trace") or {},
                },
                "pi": {
                    "completion": right.get("completion") or {},
                    "usage": right_usage,
                    "verifier_usage": right_verifier_usage,
                    "failure_class": right.get("failure_class"),
                    "run_dir": right.get("run_dir"),
                    "exit_status": right.get("exit_status"),
                    "task_checksum": right.get("task_checksum"),
                    "agent_trace": right.get("agent_trace") or {},
                },
                "usage_delta": {
                    key: _numeric_comparison(left_usage.get(key), right_usage.get(key)) for key in TOKEN_KEYS
                },
                "verifier_usage_delta": {
                    key: _numeric_comparison(left_verifier_usage.get(key), right_verifier_usage.get(key))
                    for key in TOKEN_KEYS
                },
            }
        )

    return {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "state": {
            "authoritative": authoritative,
            "data_complete": data_complete,
            "comparison_ready": vis["comparison_ready"]
            and pi["comparison_ready"]
            and task_set_match
            and vis["tool_telemetry_complete"]
            and pi["tool_telemetry_complete"],
            "spend_reporting_complete": vis["spend_reporting_complete"] and pi["spend_reporting_complete"],
            "tool_telemetry_complete": vis["tool_telemetry_complete"] and pi["tool_telemetry_complete"],
            "secret_redaction_complete": vis["secret_redaction_complete"] and pi["secret_redaction_complete"],
        },
        "gates": gates,
        "sides": {"vis": vis, "pi": pi},
        "aggregate_usage": aggregate_usage,
        "agent_usage_comparison": aggregate_usage,
        "verifier_usage_comparison": verifier_usage_comparison,
        "tasks": task_rows,
    }


def _fmt(value: Any) -> str:
    if value is None:
        return "not reported"
    if isinstance(value, bool):
        return "yes" if value else "no"
    if isinstance(value, float):
        return f"{value:,.3f}".rstrip("0").rstrip(".")
    if isinstance(value, int):
        return f"{value:,}"
    return str(value)


def _coverage(side: dict[str, Any], metric: str, usage_key: str = "agent_usage") -> str:
    usage = side.get(usage_key) if isinstance(side.get(usage_key), dict) else {}
    coverage = usage.get("token_coverage") if isinstance(usage.get("token_coverage"), dict) else {}
    return f"{coverage.get(metric, 0)}/{side.get('task_count') or 0}"


def _cost_coverage(side: dict[str, Any], usage_key: str) -> str:
    usage = side.get(usage_key) if isinstance(side.get(usage_key), dict) else {}
    return f"{usage.get('tasks_with_reported_cost', 0)}/{side.get('task_count') or 0}"


def _artifact_label(side: dict[str, Any]) -> str:
    artifact = _first(side.get("artifacts"))
    if not artifact:
        return "not reported" if str(side.get("agent", {}).get("harness")).lower() == "vis" else "not applicable"
    dirty = (
        "dirty"
        if artifact.get("source_dirty") is True
        else "clean"
        if artifact.get("source_dirty") is False
        else "unknown"
    )
    return f"{artifact.get('vis_revision') or 'unknown'} / {str(artifact.get('native_sha256') or 'unknown')[:12]} / {dirty}"


def render_markdown(report: dict[str, Any]) -> str:
    state = report["state"]
    lines = [
        "# Vis vs pi.dev Senior SWE-Bench comparison",
        "",
        f"- Authoritative: **{_fmt(state['authoritative'])}**",
        f"- Data complete: **{_fmt(state['data_complete'])}**",
        f"- Comparison ready: **{_fmt(state['comparison_ready'])}**",
        f"- Spend reporting complete: **{_fmt(state['spend_reporting_complete'])}**",
        f"- Agent rollout telemetry complete: **{_fmt(state['tool_telemetry_complete'])}**",
        f"- Secret redaction complete: **{_fmt(state['secret_redaction_complete'])}**",
        "",
        "## Provenance",
        "",
        "| side | harness | version | provider | route | endpoint | model | effort | Pi level | output cap | Harbor | artifact |",
        "| --- | --- | --- | --- | --- | --- | --- | --- | --- | ---: | --- | --- |",
    ]
    for key in ("vis", "pi"):
        side = report["sides"][key]
        agent = side["agent"]
        lines.append(
            f"| {side['label']} | {_fmt(agent.get('harness'))} | {_fmt(agent.get('version'))} | "
            f"{_fmt(agent.get('provider'))} | {_fmt(agent.get('route'))} | {_fmt(agent.get('endpoint'))} | {_fmt(agent.get('model'))} | "
            f"{_fmt(agent.get('reasoning_effort'))} | {_fmt(agent.get('pi_thinking_level'))} | "
            f"{_fmt(agent.get('output_cap'))} | {_fmt(', '.join(side.get('harbor_versions') or []))} | "
            f"{_artifact_label(side)} |"
        )
    lines.extend(
        [
        "",
        "## Evidence gates",
        "",
        "| gate | pass | detail |",
        "| --- | --- | --- |",
        ]
    )
    for name, gate in report["gates"].items():
        lines.append(f"| `{name}` | {_fmt(gate['passed'])} | {gate['detail']} |")
    lines.extend(
        [
            "",
            "## Agent rollout telemetry",
            "",
            "| side | trace tasks | iterations | tool calls | shell calls | file reads | sources |",
            "| --- | ---: | ---: | ---: | ---: | ---: | --- |",
        ]
    )
    for key in ("vis", "pi"):
        side = report["sides"][key]
        telemetry = side.get("agent_telemetry") or {}
        lines.append(
            f"| {side['label']} | {_fmt(telemetry.get('tasks_with_trace'))}/{_fmt(side.get('task_count'))} | "
            f"{_fmt(telemetry.get('iterations'))} | {_fmt(telemetry.get('tool_calls'))} | "
            f"{_fmt(telemetry.get('shell_calls'))} | {_fmt(telemetry.get('file_reads'))} | "
            f"{_fmt(', '.join(telemetry.get('sources') or []))} |"
        )
    lines.extend(
        [
            "",
            "## Completion",
            "",
            "| side | expected | present | scoreable | completed | passed | pass rate | agent tokens | verifier tokens | agent cost | verifier cost |",
            "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |",
        ]
    )
    for key in ("vis", "pi"):
        side = report["sides"][key]
        completion = side["completion"]
        agent_usage = side["agent_usage"]
        verifier_usage = side["verifier_usage"]
        lines.append(
            f"| {side['label']} | {_fmt(completion.get('expected_tasks'))} | {_fmt(side.get('task_count'))} | "
            f"{_fmt(completion.get('scoreable_tasks'))} | {_fmt(completion.get('completed_tasks'))} | "
            f"{_fmt(completion.get('passed_tasks'))} | {_fmt(completion.get('pass_rate'))} | "
            f"{_fmt(agent_usage.get('tasks_with_complete_usage'))}/{_fmt(side.get('task_count'))} | "
            f"{_fmt(verifier_usage.get('tasks_with_complete_usage'))}/{_fmt(side.get('task_count'))} | "
            f"{_fmt(agent_usage.get('tasks_with_reported_cost'))}/{_fmt(side.get('task_count'))} | "
            f"{_fmt(verifier_usage.get('tasks_with_reported_cost'))}/{_fmt(side.get('task_count'))} |"
        )

    def append_usage_section(title: str, comparison_key: str, usage_key: str) -> None:
        lines.extend(
            [
                "",
                f"## {title}",
                "",
                "| metric | Vis | coverage | pi.dev | coverage | pi - Vis | pi / Vis |",
                "| --- | ---: | ---: | ---: | ---: | ---: | ---: |",
            ]
        )
        for name, values in report[comparison_key].items():
            vis_coverage = (
                _coverage(report["sides"]["vis"], name, usage_key)
                if name in TOKEN_KEYS
                else _cost_coverage(report["sides"]["vis"], usage_key)
            )
            pi_coverage = (
                _coverage(report["sides"]["pi"], name, usage_key)
                if name in TOKEN_KEYS
                else _cost_coverage(report["sides"]["pi"], usage_key)
            )
            lines.append(
                f"| `{name}` | {_fmt(values['vis'])} | {vis_coverage} | {_fmt(values['pi'])} | {pi_coverage} | "
                f"{_fmt(values['delta_pi_minus_vis'])} | {_fmt(values['ratio_pi_over_vis'])} |"
            )

    append_usage_section("Agent token spend", "agent_usage_comparison", "agent_usage")
    append_usage_section("Verifier token overhead", "verifier_usage_comparison", "verifier_usage")
    lines.extend(["", "Reported USD cost is provider metadata and does not include subscription fees."])
    lines.extend(
        [
            "",
            "## Tasks",
            "",
            "| task | Vis result | pi.dev result | Vis judges | pi.dev judges | Vis agent | pi.dev agent | Vis iterations | pi iterations | Vis tools | pi tools | Vis verifier | pi.dev verifier | pi / Vis agent |",
            "| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |",
        ]
    )
    for task in report["tasks"]:
        token_delta = task["usage_delta"]["total_tokens"]
        lines.append(
            f"| `{task['task_id']}` | {task['vis']['completion'].get('status') or task['vis'].get('failure_class') or 'missing'} | "
            f"{task['pi']['completion'].get('status') or task['pi'].get('failure_class') or 'missing'} | "
            f"{task['vis']['completion'].get('rubric_status') or '-'} / {task['vis']['completion'].get('taste_status') or '-'} | "
            f"{task['pi']['completion'].get('rubric_status') or '-'} / {task['pi']['completion'].get('taste_status') or '-'} | "
            f"{_fmt(task['vis']['usage'].get('total_tokens'))} | {_fmt(task['pi']['usage'].get('total_tokens'))} | "
            f"{_fmt(task['vis']['agent_trace'].get('iterations'))} | {_fmt(task['pi']['agent_trace'].get('iterations'))} | "
            f"{_fmt(task['vis']['agent_trace'].get('tool_calls'))} | {_fmt(task['pi']['agent_trace'].get('tool_calls'))} | "
            f"{_fmt(task['vis']['verifier_usage'].get('total_tokens'))} | "
            f"{_fmt(task['pi']['verifier_usage'].get('total_tokens'))} | "
            f"{_fmt(token_delta['ratio_pi_over_vis'])} |"
        )
        lines.append(f"  - Vis: `{task['vis'].get('run_dir') or 'missing'}`")
        lines.append(f"  - pi.dev: `{task['pi'].get('run_dir') or 'missing'}`")
    return "\n".join(lines).rstrip() + "\n"


def render_html(report: dict[str, Any]) -> str:
    state = report["state"]

    def esc(value: Any) -> str:
        return html.escape(_fmt(value))
    gate_rows = "".join(
        f"<tr><td><code>{html.escape(name)}</code></td><td class={'pass' if gate['passed'] else 'fail'}>{_fmt(gate['passed'])}</td>"
        f"<td>{html.escape(gate['detail'])}</td></tr>"
        for name, gate in report["gates"].items()
    )
    side_rows = []
    completion_rows = []
    telemetry_rows = []
    for key in ("vis", "pi"):
        side = report["sides"][key]
        agent = side["agent"]
        completion = side["completion"]
        agent_usage = side["agent_usage"]
        verifier_usage = side["verifier_usage"]
        telemetry = side.get("agent_telemetry") or {}
        side_rows.append(
            f"<tr><td>{esc(side['label'])}</td><td>{esc(agent.get('harness'))}</td>"
            f"<td>{esc(agent.get('version'))}</td><td>{esc(agent.get('provider'))}</td>"
            f"<td>{esc(agent.get('route'))}</td><td class='path'>{esc(agent.get('endpoint'))}</td><td>{esc(agent.get('model'))}</td>"
            f"<td>{esc(agent.get('reasoning_effort'))}</td><td>{esc(agent.get('pi_thinking_level'))}</td>"
            f"<td>{esc(agent.get('output_cap'))}</td>"
            f"<td>{esc(', '.join(side.get('harbor_versions') or []))}</td>"
            f"<td class='path'>{html.escape(_artifact_label(side))}</td></tr>"
        )
        completion_rows.append(
            f"<tr><td>{esc(side['label'])}</td><td>{esc(completion.get('expected_tasks'))}</td>"
            f"<td>{esc(side.get('task_count'))}</td><td>{esc(completion.get('scoreable_tasks'))}</td>"
            f"<td>{esc(completion.get('completed_tasks'))}</td><td>{esc(completion.get('passed_tasks'))}</td>"
            f"<td>{esc(completion.get('pass_rate'))}</td>"
            f"<td>{esc(agent_usage.get('tasks_with_complete_usage'))}/{esc(side.get('task_count'))}</td>"
            f"<td>{esc(verifier_usage.get('tasks_with_complete_usage'))}/{esc(side.get('task_count'))}</td>"
            f"<td>{esc(agent_usage.get('tasks_with_reported_cost'))}/{esc(side.get('task_count'))}</td>"
            f"<td>{esc(verifier_usage.get('tasks_with_reported_cost'))}/{esc(side.get('task_count'))}</td></tr>"
        )
        telemetry_rows.append(
            f"<tr><td>{esc(side['label'])}</td><td>{esc(telemetry.get('tasks_with_trace'))}/{esc(side.get('task_count'))}</td>"
            f"<td>{esc(telemetry.get('iterations'))}</td><td>{esc(telemetry.get('tool_calls'))}</td>"
            f"<td>{esc(telemetry.get('shell_calls'))}</td><td>{esc(telemetry.get('file_reads'))}</td>"
            f"<td>{esc(', '.join(telemetry.get('sources') or []))}</td></tr>"
        )

    def usage_rows(comparison_key: str, usage_key: str) -> str:
        rows = []
        for name, values in report[comparison_key].items():
            vis_coverage = (
                _coverage(report["sides"]["vis"], name, usage_key)
                if name in TOKEN_KEYS
                else _cost_coverage(report["sides"]["vis"], usage_key)
            )
            pi_coverage = (
                _coverage(report["sides"]["pi"], name, usage_key)
                if name in TOKEN_KEYS
                else _cost_coverage(report["sides"]["pi"], usage_key)
            )
            rows.append(
                f"<tr><td><code>{html.escape(name)}</code></td><td>{esc(values['vis'])}</td>"
                f"<td>{html.escape(vis_coverage)}</td><td>{esc(values['pi'])}</td>"
                f"<td>{html.escape(pi_coverage)}</td><td>{esc(values['delta_pi_minus_vis'])}</td>"
                f"<td>{esc(values['ratio_pi_over_vis'])}</td></tr>"
            )
        return "".join(rows)

    agent_metric_rows = usage_rows("agent_usage_comparison", "agent_usage")
    verifier_metric_rows = usage_rows("verifier_usage_comparison", "verifier_usage")
    task_rows = []
    for task in report["tasks"]:
        vis_completion = task["vis"]["completion"]
        pi_completion = task["pi"]["completion"]
        vis_status = vis_completion.get("status") or task["vis"].get("failure_class") or "missing"
        pi_status = pi_completion.get("status") or task["pi"].get("failure_class") or "missing"
        task_rows.append(
            f"<tr><td><code>{html.escape(task['task_id'])}</code>"
            f"<div class='run-path'>Vis: {esc(task['vis'].get('run_dir'))}<br>pi.dev: {esc(task['pi'].get('run_dir'))}</div></td>"
            f"<td>{esc(vis_status)}</td><td>{esc(pi_status)}</td>"
            f"<td>{esc(vis_completion.get('rubric_status') or '-')} / {esc(vis_completion.get('taste_status') or '-')}</td>"
            f"<td>{esc(pi_completion.get('rubric_status') or '-')} / {esc(pi_completion.get('taste_status') or '-')}</td>"
            f"<td>{esc(task['vis']['usage'].get('total_tokens'))}</td>"
            f"<td>{esc(task['pi']['usage'].get('total_tokens'))}</td>"
            f"<td>{esc(task['vis']['agent_trace'].get('iterations'))}</td>"
            f"<td>{esc(task['pi']['agent_trace'].get('iterations'))}</td>"
            f"<td>{esc(task['vis']['agent_trace'].get('tool_calls'))}</td>"
            f"<td>{esc(task['pi']['agent_trace'].get('tool_calls'))}</td>"
            f"<td>{esc(task['vis']['verifier_usage'].get('total_tokens'))}</td>"
            f"<td>{esc(task['pi']['verifier_usage'].get('total_tokens'))}</td>"
            f"<td>{esc(task['usage_delta']['total_tokens']['ratio_pi_over_vis'])}</td></tr>"
        )
    raw = html.escape(json.dumps(report, indent=2, sort_keys=True))
    status_class = "pass" if state["authoritative"] else "fail"
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Vis vs pi.dev Senior SWE-Bench comparison</title>
<style>
*{{box-sizing:border-box}} body{{margin:0;background:#f7f8fa;color:#20242a;font:14px/1.45 ui-monospace,SFMono-Regular,Menlo,monospace}}
main{{max-width:1240px;margin:auto;padding:28px 20px 48px}} h1{{font-size:24px;margin:0 0 12px;letter-spacing:0}} h2{{font-size:17px;margin:30px 0 10px;letter-spacing:0}}
.state{{border-left:5px solid #c9362b;padding:10px 14px;background:#fff;border-top:1px solid #dfe3e8;border-bottom:1px solid #dfe3e8}} .state.pass{{border-color:#27814a}}
.table-wrap{{width:100%;overflow-x:auto}} table{{border-collapse:collapse;width:100%;min-width:760px;background:#fff}} th,td{{border:1px solid #dfe3e8;padding:8px 10px;text-align:left;vertical-align:top}}
th{{background:#eef1f4;color:#3d4650}} td.num{{text-align:right}} .pass{{color:#217a42;font-weight:700}} .fail{{color:#b72c24;font-weight:700}}
code{{color:#0969a8}} .path,.run-path{{overflow-wrap:anywhere}} .run-path{{margin-top:6px;color:#66717d;font-size:12px;line-height:1.35}}
details{{margin-top:28px}} summary{{cursor:pointer}} pre{{overflow:auto;background:#20242a;color:#f4f6f8;padding:16px}}
@media(max-width:640px){{main{{padding:20px 12px 36px}} h1{{font-size:20px}}}}
</style></head><body><main>
<h1>Vis vs pi.dev Senior SWE-Bench comparison</h1>
<p class="state {status_class}">Authoritative: <strong>{_fmt(state['authoritative'])}</strong> · Data complete: <strong>{_fmt(state['data_complete'])}</strong> · Comparison ready: <strong>{_fmt(state['comparison_ready'])}</strong> · Spend reporting complete: <strong>{_fmt(state['spend_reporting_complete'])}</strong> · Agent rollout telemetry complete: <strong>{_fmt(state['tool_telemetry_complete'])}</strong> · Secret redaction complete: <strong>{_fmt(state['secret_redaction_complete'])}</strong></p>
<h2>Provenance</h2><div class="table-wrap"><table><thead><tr><th>Side</th><th>Harness</th><th>Version</th><th>Provider</th><th>Route</th><th>Endpoint</th><th>Model</th><th>Effort</th><th>Pi level</th><th>Output cap</th><th>Harbor</th><th>Artifact</th></tr></thead><tbody>{''.join(side_rows)}</tbody></table></div>
<h2>Evidence gates</h2><div class="table-wrap"><table><thead><tr><th>Gate</th><th>Pass</th><th>Detail</th></tr></thead><tbody>{gate_rows}</tbody></table></div>
<h2>Agent rollout telemetry</h2><div class="table-wrap"><table><thead><tr><th>Side</th><th>Trace tasks</th><th>Iterations</th><th>Tool calls</th><th>Shell calls</th><th>File reads</th><th>Sources</th></tr></thead><tbody>{''.join(telemetry_rows)}</tbody></table></div>
<h2>Completion</h2><div class="table-wrap"><table><thead><tr><th>Side</th><th>Expected</th><th>Present</th><th>Scoreable</th><th>Completed</th><th>Passed</th><th>Pass rate</th><th>Agent tokens</th><th>Verifier tokens</th><th>Agent cost</th><th>Verifier cost</th></tr></thead><tbody>{''.join(completion_rows)}</tbody></table></div>
<h2>Agent token spend</h2><div class="table-wrap"><table><thead><tr><th>Metric</th><th>Vis</th><th>Coverage</th><th>pi.dev</th><th>Coverage</th><th>pi - Vis</th><th>pi / Vis</th></tr></thead><tbody>{agent_metric_rows}</tbody></table></div>
<h2>Verifier token overhead</h2><div class="table-wrap"><table><thead><tr><th>Metric</th><th>Vis</th><th>Coverage</th><th>pi.dev</th><th>Coverage</th><th>pi - Vis</th><th>pi / Vis</th></tr></thead><tbody>{verifier_metric_rows}</tbody></table></div>
<p>Reported USD cost is provider metadata and does not include subscription fees.</p>
<h2>Tasks</h2><div class="table-wrap"><table><thead><tr><th>Task</th><th>Vis result</th><th>pi.dev result</th><th>Vis judges</th><th>pi.dev judges</th><th>Vis agent</th><th>pi.dev agent</th><th>Vis iterations</th><th>pi iterations</th><th>Vis tools</th><th>pi tools</th><th>Vis verifier</th><th>pi.dev verifier</th><th>pi / Vis agent</th></tr></thead><tbody>{''.join(task_rows)}</tbody></table></div>
<details><summary>Normalized JSON</summary><pre>{raw}</pre></details>
</main></body></html>"""


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("vis_ledger", type=Path)
    parser.add_argument("pi_ledger", type=Path)
    parser.add_argument("--out-dir", type=Path, default=REPORTS_DIR)
    parser.add_argument("--name")
    parser.add_argument("--vis-label", default="Vis")
    parser.add_argument("--pi-label", default="pi.dev")
    args = parser.parse_args()

    report = build_comparison(args.vis_ledger, args.pi_ledger, vis_label=args.vis_label, pi_label=args.pi_label)
    stem = args.name or f"{args.vis_ledger.stem}-vs-{args.pi_ledger.stem}"
    args.out_dir.mkdir(parents=True, exist_ok=True)
    outputs = {
        "json": args.out_dir / f"{stem}.json",
        "markdown": args.out_dir / f"{stem}.md",
        "html": args.out_dir / f"{stem}.html",
    }
    outputs["json"].write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    outputs["markdown"].write_text(render_markdown(report))
    outputs["html"].write_text(render_html(report))
    print(json.dumps({key: str(value) for key, value in outputs.items()} | {"authoritative": report["state"]["authoritative"]}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
