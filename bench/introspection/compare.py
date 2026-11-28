"""Compare two or three Harbor job directories without printing prompts or tool output."""

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


TRACKED = {"read_session", "council.ask", "council.read", "council.get", "council.threads"}


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
                recorded = set()
                if isinstance(activity, dict):
                    counts["activity_blocks"] += 1
                    for row in activity_rows(activity.get("rows") or []):
                        operation = row.get("operation")
                        if operation == "read-session":
                            operation = "read_session"
                        key = (doc.get("session_id"), row.get("id"))
                        if operation in TRACKED:
                            recorded.add(operation)
                            if key not in seen:
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
                counted_sites = False
                for n in calls:
                    fn = n.func
                    op = (fn.id if isinstance(fn, ast.Name) else
                          f"{fn.value.id}.{fn.attr}" if isinstance(fn, ast.Attribute)
                          and isinstance(fn.value, ast.Name) else None)
                    if op in TRACKED and op not in recorded:
                        counts[op] += 1
                        counted_sites = True
                if counted_sites:
                    counts["static_operation_blocks"] += 1
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


def handoff_sources(trajectory, host, correct):
    """Classify observed paths to the synthetic value, not hidden model reasoning."""
    value = "VIOLET-9231-ORBIT"
    sources = set()
    for child in trajectory.get("subagent_trajectories", []):
        user = next((s for s in child.get("steps", []) if s.get("source") == "user"), {})
        if value in str(user.get("message", "")):
            sources.add("task-text")
        questions = {q["entry_id"] for q in host.get("questions", [])
                     if q.get("session_id") == child.get("session_id") and q.get("state") == "replied"}
        if any(r.get("reply_to") in questions and value in r.get("content", "")
               for r in host.get("replies", [])):
            sources.add("council.ask")
        if inspection(child)["read_session"]:
            sources.add("child-read")
    if inspection({k: v for k, v in trajectory.items() if k != "subagent_trajectories"})["read_session"]:
        sources.add("parent-read")
    if correct and not (sources - {"parent-read"}):
        sources.add("inherited-prefix-or-unobserved")
    return sorted(sources or {"not-observed"})


def explicit_parent_reads(child, parent):
    """Count saved call sites that name the parent session, not the child itself."""
    from blockether.vis.evals.task_forms import argument, resolve

    if not parent:
        return 0
    forms = [call.get("arguments", {}).get("code", "") for step in child.get("steps", ())
             for call in step.get("tool_calls", ())]
    try:
        tree = ast.parse("\n".join(forms))
    except SyntaxError:
        return 0
    reads = 0
    for call in ast.walk(tree):
        if isinstance(call, ast.Call) and isinstance(call.func, ast.Name) and call.func.id == "read_session":
            target = resolve(tree, argument(call, 0, "target"), call.lineno)
            if isinstance(target, ast.Constant) and target.value in {parent, f"vis_session_id#{parent}"}:
                reads += 1
    return reads


def question_window_requests(trace_path, questions, host):
    """Count distinct leader iterations that start while a child waits for a reply."""
    if not questions:
        return 0
    if not trace_path.is_file() or any(type(q.get("created_at_ms")) is not int for q in questions):
        return None
    reply_times = {r["reply_to"]: r["created_at_ms"] for r in host.get("replies", ())}
    ended = max((e["task"].get("ended_at", 0) for e in host.get("events", ())
                 if e["type"] == "task.ended" and e["task"].get("kind") == "subagent"), default=0)
    windows = [(q["created_at_ms"], reply_times.get(q["entry_id"], ended or float("inf")))
               for q in questions]
    starts = {}
    with trace_path.open(encoding="utf-8") as stream:
        for line in stream:
            row = json.loads(line)
            payload = row.get("payload") or {}
            if (row.get("event") == "trace-chunk" and payload.get("phase") == "provider-call"
                    and type(payload.get("iteration")) is int and type(payload.get("started-at-ms")) is int):
                starts.setdefault(payload["iteration"], payload["started-at-ms"])
    if not starts:
        return None
    return sum(any(first <= start <= last for first, last in windows) for start in starts.values())


def pull_metrics(trajectory, host, correct, trace_path):
    """Report the measured child pull routes; missing logs are not zero counts."""
    from handoff_case import child_questions, has_value

    children = trajectory.get("subagent_trajectories", [])
    child_ids = {child.get("session_id") for child in children} - {None}
    questions = child_questions(host, child_ids)
    reads = sum(explicit_parent_reads(c, trajectory.get("session_id")) for c in children)
    question_ids = {row["entry_id"] for row in questions if row.get("state") == "replied"}
    sources = set()
    if reads:
        sources.add("child-read-parent")
    if any(row.get("reply_to") in question_ids and has_value(row.get("content"))
           for row in host.get("replies", ())):
        sources.add("council.ask" if any(inspection(c)["council.ask"] for c in children)
                    else "council-reply-unattributed")
    if any(has_value(t.get("task")) or has_value(t.get("title"))
           for sid, t in host.get("child_tasks", {}).items() if sid in child_ids):
        sources.add("task-text")
    if correct and not sources:
        sources.add("inherited-prefix-or-unobserved")
    return {"handoff_sources": sorted(sources or {"not-observed"}),
            "handoff_child_parent_reads": reads if children else None,
            "handoff_child_questions": len(questions) if host and children else None,
            "handoff_leader_question_requests": question_window_requests(trace_path, questions, host)
            if host and children else None}


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
    grade = read(folder / "verifier/grade.json")
    guards = grade.get("guards")
    without_cap = (int(grade.get("converged") is True and grade.get("errors") == 0
                       and all(passed for name, passed in guards.items() if name != "total_output"))
                   if guards else None)
    data = {"correct_without_output_cap": without_cap,"correct": None if reward is None else int(reward == 1),
            "provider_calls": grade.get("provider_calls"),
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
    for scope in ("leader", "children", "team"):
        if data.get(f"input_{scope}") is not None and data.get(f"cached_input_{scope}") is not None:
            data[f"uncached_input_{scope}"] = data[f"input_{scope}"] - data[f"cached_input_{scope}"]
    if requests:
        data.update({"estimated:" + label: n for label, n in parts.items()})
        data["initial_request_estimated_input_leader"] = (session_prompt.get("requests") or [{}])[0].get("input")
        data["initial_request_estimated_input_children"] = sum(c["requests"][0]["input"] for c in session_prompt.get("children", []) if c.get("requests"))
    if handoffs := list(handoff_estimates(session_prompt)):
        data["handoff_estimated_input_children"] = sum(row["tokens"] for row in handoffs)
        data["handoff_estimate_count"] = len(handoffs)
    task = result.get("task_name", "").rsplit("/", 1)[-1]
    if task in {"introspection-handoff-recall", "introspection-handoff-pull"}:
        host = read(folder / "agent/task-evidence.json")
        data["handoff_trial"] = result["trial_name"]
        if task == "introspection-handoff-pull":
            data.update(pull_metrics(trajectory, host, data["correct"], folder / "agent/trace.jsonl"))
        else:
            data["handoff_sources"] = handoff_sources(trajectory, host, data["correct"])
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


def comparison(*jobs):
    if len(jobs) not in (2, 3) or not all(jobs):
        raise ValueError("Two or three jobs must have trial results for the introspection suite")
    labels = ("baseline", "candidate", "work")[:len(jobs)]
    rows = {}
    for case in sorted(set().union(*jobs)):
        arms = [job.get(case, []) for job in jobs]
        correct = [[r for r in arm if r["correct"] == 1] for arm in arms]
        without_cap = [[r for r in arm if r["correct_without_output_cap"] == 1] for arm in arms]
        keys = sorted({k for arm in arms for row in arm for k in row}
                      - {"correct", "correct_without_output_cap", "routes", "estimate_projections", "handoff_sources", "handoff_trial"})

        def values_for(groups, keys=keys):
            for key in keys:
                numbers = []
                for arm in groups:
                    values = [r.get(key, 0) if key.startswith("estimated:") and r.get("request_estimates")
                              else r.get(key) for r in arm]
                    numbers.append([value for value in values if value is not None])
                values = [median(ns) if ns else None for ns in numbers]
                a, b = values[:2]
                yield key, {**{f"{label}_median": value for label, value in zip(labels, values, strict=True)},
                            "delta": b - a if a is not None and b is not None else None,
                            "observed_n": [len(ns) for ns in numbers]}

        rows[case] = {"correct": [len(arm) for arm in correct],
                      "attempted": [len(arm) for arm in arms],
                      "pass_rate": [len(wins) / len(arm) if arm else None
                                    for arm, wins in zip(arms, correct, strict=True)],
                      "unscored": [sum(r["correct"] is None for r in arm) for arm in arms],
                      "correct_without_output_cap": [len(arm) for arm in without_cap],
                      "output_cap_observed_n": [sum(r["correct_without_output_cap"] is not None for r in arm)
                                                for arm in arms],
                      "model_routes": [sorted({route for r in arm for route in r["routes"]}) for arm in arms],
                      "estimate_projections": [sorted({p for r in arm for p in r["estimate_projections"]})
                                               for arm in arms],
                      "handoff_sources": [dict(Counter(source for r in arm
                                                       for source in r.get("handoff_sources", ()))) for arm in arms],
                      "correct_trials_only": dict(values_for(correct)),
                      "correct_without_output_cap_trials_only": dict(values_for(without_cap)),
                      "all_observed_trials": dict(values_for(arms))}
        if case in {"introspection-handoff-pull", "introspection-handoff-recall"}:
            fields = ("handoff_trial", "correct", "handoff_sources", "provider_calls",
                      "handoff_child_parent_reads", "handoff_child_questions", "handoff_leader_question_requests",
                      "cached_input_team", "uncached_input_team", "output_team")
            rows[case]["handoff_trials"] = [[{k: r[k] for k in fields if k in r} for r in arm] for arm in arms]
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline", type=Path)
    parser.add_argument("candidate", type=Path)
    parser.add_argument("work", type=Path, nargs="?")
    args = parser.parse_args()
    jobs = [trials(path) for path in (args.baseline, args.candidate, args.work) if path]
    print(json.dumps(comparison(*jobs), indent=2))


if __name__ == "__main__":
    main()
