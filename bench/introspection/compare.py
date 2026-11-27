"""Compare two Harbor job directories without printing prompts or tool output."""

import argparse
import ast
import json
import re
from collections import Counter, defaultdict
from pathlib import Path
from statistics import median

EVENT = re.compile(r"(?:\.ndjson\b|['\"]?events['\"]?)")
LOG = re.compile(r"(?:\.log\b|['\"]?logs['\"]?)")


def read(path):
    return json.loads(path.read_text(encoding="utf-8")) if path.is_file() else {}


def documents(tree):
    yield tree
    for child in tree.get("subagent_trajectories", []):
        yield from documents(child)


def activity_rows(rows):
    for row in rows:
        yield row
        yield from activity_rows(row.get("children", []))


TRACKED = {"read_session", "council.read", "council.get", "council.threads"}


def inspection(tree):
    """Prefer host records; count raw file reads as static code-block candidates."""
    counts = Counter()
    seen = set()
    for doc in documents(tree):
        for step in doc.get("steps", []):
            for call in step.get("tool_calls", []):
                name = call["function_name"]
                if name in TRACKED:
                    counts[name] += 1
                    continue
                activity = (call.get("extra") or {}).get("activity")
                if isinstance(activity, dict):
                    counts["activity_blocks"] += 1
                    for row in activity_rows(activity.get("rows") or []):
                        operation = row.get("operation")
                        key = (doc.get("session_id"), row.get("id"))
                        if operation in TRACKED and key not in seen:
                            counts[operation] += 1
                            seen.add(key)
                code = call.get("arguments", {}).get("code")
                if not code:
                    continue
                try:
                    calls = [n for n in ast.walk(ast.parse(code)) if isinstance(n, ast.Call)]
                except SyntaxError:
                    counts['unparsed_blocks'] += 1
                    continue
                if not isinstance(activity, dict):
                    counts["static_operation_blocks"] += 1
                    for n in calls:
                        fn = n.func
                        if isinstance(fn, ast.Name) and fn.id == "read_session":
                            counts["read_session"] += 1
                        elif (isinstance(fn, ast.Attribute) and isinstance(fn.value, ast.Name)
                              and fn.value.id == "council" and fn.attr in {"read", "get", "threads"}):
                            counts[f"council.{fn.attr}"] += 1
                reads = sum(isinstance(n.func, ast.Name) and n.func.id in {"open", "shell", "grep", "ls"}
                            or isinstance(n.func, ast.Attribute) and n.func.attr in {"open", "read_text", "read_bytes"}
                            for n in calls)
                if reads and EVENT.search(code):
                    counts["journal_read_blocks"] += 1
                if reads and LOG.search(code):
                    counts["log_read_blocks"] += 1
    return counts


def estimates(tree):
    yield from tree.get("requests", [])
    for child in tree.get("children", []):
        yield from estimates(child)


def handoff_estimates(tree):
    if (row := tree.get("handoff")) and type(row.get("tokens")) is int:
        yield row
    for child in tree.get("children", []):
        yield from handoff_estimates(child)


def summarize(folder, result):
    agent = result.get("agent_result") or {}
    trajectory = read(folder / "agent/trajectory.json")
    docs = list(documents(trajectory)) if trajectory else []
    metrics = [doc.get("final_metrics") or {} for doc in docs]
    session_prompt = read(folder / "agent/prompt-accounting.json")
    requests = list(estimates(session_prompt))
    parts = Counter()
    for row in requests:
        parts.update(row["parts"])
    reward = ((result.get("verifier_result") or {}).get("rewards") or {}).get("reward")
    if reward is None:
        reward = read(folder / "verifier/reward.json").get("reward")
    counts = inspection(trajectory)
    stdout_chars = [(call.get("extra") or {}).get("stdout_chars") for doc in docs
                    for step in doc.get("steps", []) for call in step.get("tool_calls", [])]
    iteration = [m.get("extra", {}).get("iteration_count") for m in metrics]
    iteration = [n if n is not None else sum(step.get("source") == "agent" and bool(step.get("metrics"))
                                             for step in doc.get("steps", []))
                 for n, doc in zip(iteration, docs, strict=True)]
    data = {"correct": None if reward is None else int(reward == 1),
            "iterations_leader": iteration[0] if iteration else None,
            "iterations_children": sum(iteration[1:]) if iteration else None,
            "children": len(docs) - 1 if docs else None,
            "tool_stdout_chars": sum(n for n in stdout_chars if isinstance(n, int))
                if any(isinstance(n, int) for n in stdout_chars) else None,
            "request_estimates": len(requests) if session_prompt else None,
            "request_estimated_input": sum(r["input"] for r in requests) if requests else None}
    for field in ("read_session", "journal_read_blocks", "log_read_blocks", "council.read", "council.get", "council.threads", "unparsed_blocks", "activity_blocks", "static_operation_blocks"):
        data[field] = counts[field] if docs else None
    for name, atif, harbor in (("input", "total_prompt_tokens", "n_input_tokens"),
                               ("cached_input", "total_cached_tokens", "n_cache_tokens"),
                               ("output", "total_completion_tokens", "n_output_tokens")):
        root = metrics[0].get(atif) if metrics else None
        data[name + "_leader"] = root if root is not None else agent.get(harbor)
        children = [m.get(atif) for m in metrics[1:]]
        data[name + "_children"] = sum(children) if docs and all(x is not None for x in children) else None
        if all(data[name + suffix] is not None for suffix in ("_leader", "_children")):
            data[name + "_team"] = data[name + "_leader"] + data[name + "_children"]
    if data.get("input_team") is not None and data.get("cached_input_team") is not None:
        data["uncached_input_team"] = data["input_team"] - data["cached_input_team"]
    if requests:
        data.update({"estimated:" + label: n for label, n in parts.items()})
        data["initial_request_estimated_input_leader"] = (session_prompt.get("requests") or [{}])[0].get("input")
        data["initial_request_estimated_input_children"] = sum(c["requests"][0]["input"] for c in session_prompt.get("children", []) if c.get("requests"))
    if handoffs := list(handoff_estimates(session_prompt)):
        data["handoff_estimated_input_children"] = sum(row["tokens"] for row in handoffs)
        data["handoff_estimate_count"] = len(handoffs)
    routes = set()
    for doc in docs:
        info = doc.get("agent") or {}
        for step in doc.get("steps", []):
            if step.get("source") == "agent" and step.get("model_name"):
                prov = (step.get("extra") or {}).get("provider")
                routes.add(f"{prov}/{step['model_name']}" if prov else step["model_name"])
        if not routes and info.get("model_name"):
            routes.add(info["model_name"])
    data["routes"] = sorted(routes)
    data["estimate_projections"] = sorted({r["projection"] for r in requests if r.get("projection")})
    return data


def trials(job):
    data = defaultdict(list)
    for path in sorted({*job.glob("*/result.json"), *job.glob("*/*/result.json")}):
        result = read(path)
        task = result.get("task_name", "").rsplit("/", 1)[-1]
        if result.get("trial_name") and task.startswith("introspection-"):
            data[task].append(summarize(path.parent, result))
    return data


def comparison(old, new):
    if not old or not new:
        raise ValueError("Both jobs must have trial results for the introspection suite")
    rows = {}
    for case in sorted(old.keys() | new.keys()):
        a, b = old.get(case, []), new.get(case, [])
        sa, sb = ([r for r in arm if r["correct"] == 1] for arm in (a, b))
        keys = sorted({k for row in a + b for k in row} - {"correct", "routes", "estimate_projections"})
        def values_for(arms, keys=keys):
            for key in keys:
                numbers = []
                for arm in arms:
                    ns = [r.get(key, 0) if key.startswith("estimated:") and r.get("request_estimates") else r.get(key)
                          for r in arm]
                    numbers.append([n for n in ns if n is not None])
                x, y = [median(n) if n else None for n in numbers]
                yield key, {"baseline_median": x, "candidate_median": y,
                            "delta": y - x if x is not None and y is not None else None,
                            "observed_n": [len(ns) for ns in numbers]}
        rows[case] = {"correct": [len(sa), len(sb)], "attempted": [len(a), len(b)],
                      "unscored": [sum(r["correct"] is None for r in arm) for arm in (a, b)],
                      "model_routes": [sorted({route for r in arm for route in r['routes']}) for arm in (a, b)],
                      "estimate_projections": [sorted({p for r in arm for p in r['estimate_projections']})
                                               for arm in (a, b)],
                      "correct_trials_only": dict(values_for((sa, sb))),
                      "all_observed_trials": dict(values_for((a, b)))}
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline", type=Path)
    parser.add_argument("candidate", type=Path)
    args = parser.parse_args()
    print(json.dumps(comparison(trials(args.baseline), trials(args.candidate)), indent=2))


if __name__ == "__main__":
    main()
