"""Check every fixture without invoking a model or running Harbor."""

import argparse
import asyncio
import shlex
import sys
import json
import os
import re
import subprocess
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from types import SimpleNamespace

from blockether.vis.evals.agent import source_classpath
from blockether.vis.evals.grade import check_problems, structured_failures, verifier_failures
from blockether.vis.evals.harbor import read_tasks

from bench_agent import HERE, SID, TURN, InspectionAgent, journal, write_fallback
from harbor.models.agent.context import AgentContext
from compare import comparison, trials


def load(path):
    return json.loads(path.read_text(encoding="utf-8"))


def check_dataset(checkout, tmp):
    tasks = [task for task in read_tasks(HERE.parent) if task.name.startswith("introspection-")]
    assert len(tasks) == 10
    cp = source_classpath("clojure", checkout)
    with ThreadPoolExecutor(max_workers=3) as workers:
        list(workers.map(lambda task: check_task(task, cp, tmp), tasks))


def check_task(task, cp, tmp):
    tests = task.path / "tests"
    work = task.path / "environment/workspace"
    checks = load(tests / "checks.json")
    expected = checks["want_answer_json"]
    kind = load(tests / "fixture.json")["kind"]
    assert task.instruction and task.agent_timeout_sec and task.verifier_timeout_sec
    assert work.is_dir() and os.access(tests / "test.sh", os.X_OK)
    assert not check_problems(checks)
    assert not structured_failures(checks, work, json.dumps(expected), [])
    assert structured_failures(checks, work, '{"wrong":true}', [])
    if kind in {"handoff", "resume"}:
        endings = ["done"] * (1 if kind == "handoff" else 2)
        events = [e for i, state in enumerate(endings, 1) for e in (
            {"type": "task.started", "task": {"id": f"t{i}", "kind": "subagent", "state": "running"}},
            {"type": "task.ended", "task": {"id": f"t{i}", "kind": "subagent", "state": state}},
        )]
        evidence = {"events": events, "children": {"a1": SID}}
        verifier = tests / checks["verify"]
        marks = list(expected.values())
        child = SimpleNamespace(agent_steps=[SimpleNamespace(message=mark) for mark in marks])
        trajectory = SimpleNamespace(subagent_trajectories=[child])
        assert not verifier_failures(verifier, SimpleNamespace(host_evidence=evidence, trajectory=trajectory))
        assert verifier_failures(verifier, SimpleNamespace(host_evidence=evidence, trajectory=None))
        assert verifier_failures(verifier, SimpleNamespace(host_evidence={}, trajectory=trajectory))
        if kind == "handoff":
            actual = {"marker": (work / "handoff.txt").read_text().splitlines()[1]}
        else:
            actual = {key: (work / f"{filename}.txt").read_text().strip()
                      for key, filename in [("initial", "initial"), ("revised", "revised")]}
    else:
        actual = None
        if rows := journal(kind):
            path = work / "diagnostics" / f"{SID}.ndjson"
            actual_rows = [json.loads(line) for line in path.read_text().splitlines()]
            assert actual_rows == rows
            scoped = [row for row in rows if row.get("turn_id") == TURN
                      or row.get("task", {}).get("origin", {}).get("turn") == 2]
            if kind == "origin":
                actual = {"request_kind": next(row["request_kind"] for row in scoped if row["type"] == "turn.started")}
            elif kind == "wait":
                progress = [row for row in scoped if row.get("progress") == "provider-wait" and row["iteration"] == 2]
                actual = {key: progress[-1][key] for key in ["connection", "silent_ms"]}
            elif kind == "task":
                start = next(row["task"] for row in scoped if row["type"] == "task.started")
                actual = {"state": next(row["task"]["state"] for row in scoped
                         if row["type"] == "task.ended" and row["task"]["id"] == start["id"])}
            elif kind == "log":
                line = next(line for line in (work / "diagnostics/vis.log").read_text().splitlines()
                            if SID in line and " turn=2 " in line)
                actual = {"reason": re.search(r"reason=(\w+)$", line)[1]}
        root = tmp / kind
        root.mkdir()
        env = dict(os.environ, VIS_BENCH_CHECK_SEED="1", VIS_BENCH_DB=str(root / "vis.mdb"),
                   VIS_HOME=str(root / "home"),
                   VIS_BENCH_ROOT=str(work), VIS_BENCH_KIND=kind, VIS_BENCH_SID=SID)
        if journal(kind):
            write_fallback(work, root / "home", kind)
        seed = ["clojure", "-Scp", cp, "-M", "-e", f'(load-file "{HERE / "seed.clj"}")']
        proc = subprocess.run(seed, cwd=root, env=env, capture_output=True, text=True, timeout=90)
        if proc.returncode:
            raise AssertionError(proc.stderr)
        if proc.stdout.strip():
            projected = json.loads(proc.stdout.strip().splitlines()[-1])
            assert projected == expected, (task.name, projected, expected)
            if actual is None:
                actual = projected
    assert actual == expected, (task.name, actual, expected)
    print(f"PASS {task.name}")


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value), encoding="utf-8")


def check_comparison(tmp):
    for side, inp in [("baseline", 1800), ("candidate", 1400)]:
        folder = tmp / side / "trial"
        write_json(folder / "result.json", {"trial_name": "trial", "task_name": "introspection-test",
                   "verifier_result": {"rewards": {"reward": 1}}})
        def trajectory(tokens, code, children=None):
            return {
                "final_metrics": {"total_prompt_tokens": tokens, "total_cached_tokens": 100,
                                  "total_completion_tokens": 50, "extra": {"iteration_count": 1}},
                "agent": {"model_name": "same-model"},
                "steps": [{"tool_calls": [{"function_name": "py3", "arguments": {"code": code}}]}],
                "subagent_trajectories": children or [],
            }
        code = ('await read_session("id"); Path("diagnostics/id.ndjson").read_text()'
                if side == "baseline" else 'await read_session("id")')
        write_json(folder / "agent/trajectory.json", trajectory(inp, code, [trajectory(250, 'await council.get(1)')]))
        write_json(folder / "agent/prompt-accounting.json", {"requests": [{"projection": "prepared_request",
                   "input": inp, "parts": {"Vis core system prompt": 1250 if side == "baseline" else 1000}}],
                   "children": [{"handoff": {"full": 800, "bounded": 300,
                                              "mode": "full" if side == "baseline" else "bounded"}}]})
    row = comparison(trials(tmp / "baseline"), trials(tmp / "candidate"))["introspection-test"]
    assert row["correct"] == [1, 1]
    delta = row["correct_trials_only"]
    assert delta["input_team"]["delta"] == -400 and delta["input_children"]["baseline_median"] == 250
    assert delta["estimated:Vis core system prompt"]["delta"] == -250
    assert delta["journal_read_blocks"]["delta"] == -1 and delta["council.get"]["candidate_median"] == 1
    assert delta["handoff_estimated_full_input_children"]["baseline_median"] == 800
    assert delta["handoff_estimated_bounded_input_children"]["candidate_median"] == 300
    assert delta["handoff_bounded_count"]["delta"] == 1
    print("PASS comparator: fixture team tokens, prompt accounting and fallback deltas")


def check_runner(checkout, tmp):
    """Exercise the Harbor agent path with a non-model CLI stub."""
    workspace = tmp / "smoke-workspace"
    logs = tmp / "smoke-logs"
    workspace.mkdir()
    logs.mkdir()
    stub = tmp / "stub.py"
    stub.write_text(f"""import json
import os
import sqlite3
import sys
from pathlib import Path
args = sys.argv[1:]
root = Path(os.environ['VIS_DB_PATH'])
with sqlite3.connect((root / 'vis.db').as_uri() + '?mode=ro', uri=True) as db:
    assert db.execute('select count(*) from session_turn_soul').fetchone() == (16,)
assert Path(os.environ['VIS_HOME']).parent == root.parent
assert 'VIS_GATEWAY_URL' not in os.environ
path = Path(args[args.index('--trajectory') + 1])
path.write_text(json.dumps({{'schema_version':'ATIF-v1.8', 'session_id':{SID!r},
    'agent':{{'name':'vis', 'model_name':'smoke'}}, 'steps':[{{'step_id':1, 'source':'user', 'message':'fixture'}}],
    'final_metrics':{{'total_prompt_tokens':9, 'total_cached_tokens':4, 'total_completion_tokens':2}}}}))
""", encoding="utf-8")
    agent = InspectionAgent(logs, model_name="openai/smoke", command=shlex.join([sys.executable, str(stub)]))
    agent.workspace = workspace
    agent.kind = "block"
    agent.fixture_cp = source_classpath("clojure", checkout)

    async def exec_command(command, env):
        trial_runtime.append(Path(env["VIS_DB_PATH"]).parent)
        extra = dict(os.environ, **env, VIS_HARBOR_WORKSPACE=str(workspace), VIS_HARBOR_AGENT_LOGS=str(logs))
        proc = await asyncio.to_thread(subprocess.run, ["bash", "-c", command], env=extra,
                                       cwd=workspace, capture_output=True, text=True, timeout=90)
        assert proc.returncode == 0, proc.stderr
        return SimpleNamespace(return_code=proc.returncode)

    context = AgentContext()
    trial_runtime = []
    asyncio.run(agent.run("fixture", SimpleNamespace(exec=exec_command), context))
    assert (context.n_input_tokens, context.n_cache_tokens, context.n_output_tokens) == (9, 4, 2)
    rows = load(logs / "prompt-accounting.json")["requests"]
    assert rows == [{"projection": "prepared-request", "input": 490,
                    "parts": {"Vis core system prompt": 300, "Built-in tools and rules": 190}}]
    assert len(trial_runtime) == 1 and not trial_runtime[0].exists()
    print("PASS runner: fixture preflight, isolated home, token capture and cleanup (stub)")


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("checkout", type=Path)
    p.add_argument("--tmp-dir", type=Path, required=True)
    args = p.parse_args()
    with tempfile.TemporaryDirectory(dir=args.tmp_dir) as scratch:
        tmp = Path(scratch)
        check_dataset(args.checkout.resolve(), tmp)
        check_comparison(tmp)
        check_runner(args.checkout.resolve(), tmp)
