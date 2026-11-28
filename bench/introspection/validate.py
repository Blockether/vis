"""Check every fixture without invoking a model or running Harbor."""

import argparse
import asyncio
import json
import os
import re
import shlex
import sqlite3
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from types import SimpleNamespace

from bench_agent import HERE, SID, TASKS, TURN, InspectionAgent, journal, write_fallback
from blockether.vis.evals.agent import source_classpath
from blockether.vis.evals.grade import (
    check_problems,
    structured_failures,
    verifier_failures,
)
from blockether.vis.evals.harbor import read_tasks
from blockether.vis.evals.task_evidence import capture
from blockether.vis.evals.task_forms import writes_file
from compare import comparison, handoff_sources, trials
from harbor.models.agent.context import AgentContext


def load(path):
    return json.loads(path.read_text(encoding="utf-8"))


def check_dataset(checkout, tmp):
    tasks = [task for task in read_tasks(TASKS) if task.name.startswith("introspection-")]
    assert len(tasks) == 11
    cp = source_classpath("clojure", checkout)
    with ThreadPoolExecutor(max_workers=1) as workers:
        list(workers.map(lambda task: check_task(task, cp, tmp), tasks))


def check_task(task, cp, tmp):
    tests = task.path / "tests"
    work = task.path / "environment/workspace"
    checks = load(tests / "checks.json")
    expected = checks["want_answer_json"]
    fixture = load(tests / "fixture.json")
    kind = fixture["kind"]
    assert task.instruction and task.agent_timeout_sec and task.verifier_timeout_sec
    assert work.is_dir() and os.access(tests / "test.sh", os.X_OK)
    assert not check_problems(checks)
    assert not structured_failures(checks, work, json.dumps(expected), [])
    assert structured_failures(checks, work, '{"wrong":true}', [])
    if kind not in {"resume", "handoff"}:
        # Allow selected saved facts, plus the entire documented fallback if needed.
        diagnostics = work / "diagnostics"
        fallback = diagnostics / ("vis.log" if kind == "log" else f"{SID}.ndjson")
        targeted_chars = len(json.dumps(expected))
        if kind == "block":
            targeted_chars = max(targeted_chars, len(f"Verification code: {expected['verification_code']}\n"))
        if fallback.is_file():
            targeted_chars += len(fallback.read_text())
        assert checks["max_total_output_chars"] >= 3 * targeted_chars
    if kind == "resume":
        events = [e for i in (1, 2) for e in (
            {"type": "task.started", "task": {"id": f"t{i}", "kind": "subagent", "state": "running"}},
            {"type": "task.ended", "task": {"id": f"t{i}", "kind": "subagent", "state": "done"}},
        )]
        evidence = {"events": events, "children": {"a1": SID}}
        verifier = tests / checks["verify"]
        marks = list(expected.values())
        child = SimpleNamespace(agent_steps=[SimpleNamespace(message=mark) for mark in marks])
        trajectory = SimpleNamespace(subagent_trajectories=[child])
        assert not verifier_failures(verifier, SimpleNamespace(host_evidence=evidence, trajectory=trajectory))
        assert verifier_failures(verifier, SimpleNamespace(host_evidence=evidence, trajectory=None))
        assert verifier_failures(verifier, SimpleNamespace(host_evidence={}, trajectory=trajectory))
        actual = {key: (work / f"{key}.txt").read_text().strip()
                  for key in ("initial", "revised")}
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
        root = tmp / task.name
        root.mkdir()
        env = dict(os.environ, VIS_BENCH_CHECK_SEED="1", VIS_BENCH_DB=str(root / "vis.mdb"),
                   VIS_HOME=str(root / "home"),
                   VIS_BENCH_ROOT=str(work), VIS_BENCH_KIND=kind, VIS_BENCH_SID=SID,
                   VIS_BENCH_ANSWER=fixture.get("answer", ""))
        if journal(kind):
            write_fallback(work, root / "home", kind)
        seed = ["clojure", "-Scp", cp, "-M", "-e", f'(load-file "{HERE / "seed.clj"}")']
        proc = subprocess.run(seed, cwd=root, env=env, capture_output=True, text=True, timeout=90)
        if proc.returncode:
            raise AssertionError(proc.stderr)
        if proc.stdout.strip():
            projected = json.loads(proc.stdout.strip().splitlines()[-1])
            if kind == "handoff":
                assert projected["early_has_value"] and not projected["later_has_value"]
                assert projected["later_tokens"] >= 6000, projected
                # The tail that a child inherits has 2,000 tokens: a long answer stays out of it.
                assert (projected["early_tokens"] > 2500) == (fixture.get("answer") == "long"), projected
                check_handoff(tests, root)
                actual = expected
                print(f'  Handoff: {projected["early_tokens"]} early and {projected["later_tokens"]} later tokens (Svar/gpt-4o)')
            else:
                assert projected == expected, (task.name, projected, expected)
                if actual is None:
                    actual = projected
    assert actual == expected, (task.name, actual, expected)
    print(f"PASS {task.name}")


def check_handoff(tests, root):
    verifier = tests / "verify.py"
    output = root / "transfer.txt"
    value = "VIOLET-9231-ORBIT"
    output.write_text(value)
    child = SimpleNamespace(session_id=SID, tool_calls=[SimpleNamespace(
        error=None, code='Path("transfer.txt").write_text(value)')])
    trajectory = SimpleNamespace(subagent_trajectories=[child])
    host = {"children": {"a1": SID}, "events": [
        {"type": f"task.{event}", "task": {"id": "t1", "kind": "subagent", "state": state}}
        for event, state in (("started", "running"), ("ended", "done"))]}
    host.update(child_tasks={SID: {"task": "Write the approved value.", "title": "Write transfer",
                                  "files": {"write": [str(output)]}}}, leader_messages=[])
    trajectory.steps = []
    child.steps = [SimpleNamespace(source="user", message="Write the approved value.")]
    evidence = SimpleNamespace(trajectory=trajectory, host_evidence=host, workspace=root, forms=[])
    assert not verifier_failures(verifier, evidence)
    code = '''p = Path(session["workspace"]["filesystem_roots"][0]["cwd"]).expanduser()
p.write_text(value)'''
    assert not writes_file(code, "transfer.txt")
    child.tool_calls[0].code = code
    assert not verifier_failures(verifier, evidence)
    del host["child_tasks"][SID]["files"]["write"]
    assert verifier_failures(verifier, evidence) == ["The child must write transfer.txt."]
    host["child_tasks"][SID]["files"]["write"] = [str(output)]
    host["children"]["a2"] = "another-child"
    assert verifier_failures(verifier, evidence)
    del host["children"]["a2"]
    output.write_text("wrong")
    assert verifier_failures(verifier, evidence)
    output.write_text(value)
    evidence.forms = [f'Path("transfer.txt").write_text("{value}")']
    assert verifier_failures(verifier, evidence)
    raw_child = {"session_id": SID, "steps": [{"source": "user", "message": value}]}
    assert handoff_sources({"subagent_trajectories": [raw_child]}, {}, True) == ["task-text"]
    raw_child["steps"][0]["message"] = "Write the value."
    assert handoff_sources({"subagent_trajectories": [raw_child]}, {}, True) == ["inherited-prefix-or-unobserved"]
    host["questions"] = [{"session_id": SID, "entry_id": 1, "state": "replied"}]
    host["replies"] = [{"reply_to": 1, "content": value}]
    assert handoff_sources({"subagent_trajectories": [raw_child]}, host, True) == ["council.ask"]
    if tests.parent.name == "introspection-handoff-pull":
        evidence.forms = []
        check_pull(verifier, evidence, host, child, value)


def check_pull(verifier, evidence, host, child, value):
    host.update(questions=[], replies=[])
    assert not verifier_failures(verifier, evidence)
    task = host["child_tasks"][SID]
    for field in ("task", "title"):
        prior = task[field]
        task[field] = "Violet / 9231 / Orbit"
        assert verifier_failures(verifier, evidence) == ["The task or title must not give the child the transfer value."]
        task[field] = prior
    prior = child.steps[0].message
    child.steps[0].message = value
    assert verifier_failures(verifier, evidence)
    child.steps[0].message = prior
    q = {"session_id": SID, "entry_id": 2, "state": "replied", "created_at_ms": 20}
    reply = {"entry_id": 3, "reply_to": 2, "content": value, "created_at_ms": 30, "title": ""}
    host.update(questions=[q], replies=[reply], leader_messages=[reply])
    assert not verifier_failures(verifier, evidence)
    host["leader_messages"].append({"entry_id": 1, "content": value, "created_at_ms": 19})
    assert verifier_failures(verifier, evidence) == ["The leader must not give the transfer value in a Council note."]
    host["leader_messages"].pop()
    step = SimpleNamespace(source="agent", message=value, finished_at_ms=25,
                           tool_calls=[SimpleNamespace(started_at_ms=15)])
    evidence.trajectory.steps = [step]
    assert verifier_failures(verifier, evidence) == ["The leader must not state the transfer value before the child asks."]
    step.tool_calls[0].started_at_ms = 22
    assert not verifier_failures(verifier, evidence)
    reply["title"] = value
    assert verifier_failures(verifier, evidence) == ["The leader must not give the transfer value in a Council note."]


def check_host_capture(tmp):
    database = tmp / "handoff-evidence.db"
    with sqlite3.connect(database) as db:
        db.executescript("""
            CREATE TABLE session_agent (ordinal, session_id, parent_id, team_id, task, title, files);
            CREATE TABLE council_entry (id, group_id, author_sid, content, title, created_at, reply_required, reply_to);
            CREATE TABLE council_ping (entry_id, recipient_sid, state, reply_entry_id);
            CREATE TABLE extension_aggregate (session_soul_id, extension_id, kind, index_data);
            INSERT INTO session_agent VALUES (1, 'child', 'leader', 'team', 'Write the value.', 'Transfer', '{}');
            INSERT INTO council_entry VALUES (1, 'team', 'leader', 'Delegate', 'Task', 10, 0, NULL);
            INSERT INTO council_entry VALUES (2, 'team', 'child', 'What value?', NULL, 20, 1, NULL);
            INSERT INTO council_entry VALUES (3, 'team', 'leader', 'Reply', NULL, 30, 0, 2);
            INSERT INTO council_entry VALUES (4, 'team', 'another-author', 'Other', NULL, 40, 0, NULL);
            INSERT INTO council_ping VALUES (2, 'leader', 'replied', 3);
        """)
        for table, column in [("session_agent", "parent_id"), ("council_entry", "author_sid"),
                              ("council_ping", "recipient_sid")]:
            db.execute(f"UPDATE {table} SET {column} = ? WHERE {column} = 'leader'", (SID,))
    host = capture(SID, tmp, database, include_handoff=True)
    assert host["questions"][0]["created_at_ms"] == 20 and host["replies"][0]["created_at_ms"] == 30
    assert [m["entry_id"] for m in host["leader_messages"]] == [1, 3]
    assert host["child_tasks"]["child"]["title"] == "Transfer"
    assert "leader_messages" not in capture(SID, tmp, database)
    print("PASS host capture: question times and Council entries from the measured leader")


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value), encoding="utf-8")


def check_comparison(tmp):
    for side, inp in [("baseline", 1800), ("candidate", 1400), ("work", 1200)]:
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
        write_json(folder / "verifier/grade.json", {"provider_calls": inp // 200})
        document = trajectory(inp, code, [trajectory(250, 'await council.get(1)')])
        call = document["steps"][0]["tool_calls"][0]
        call["extra"] = {"stdout_chars": 12, "activity": {"rows": [{"id": "1", "operation": "ls"}]}}
        write_json(folder / "agent/trajectory.json", document)
        write_json(folder / "agent/prompt-accounting.json", {"requests": [{"projection": "prepared_request",
                   "input": inp, "parts": {"Vis core system prompt": 1250 if side == "baseline" else 1000}}],
                    "children": [{"handoff": {"source": "svar-estimate", "projection": "inherited-prefix",
                                               "model": "same-model", "tokens": 800 if side == "baseline" else 300}}]})
    row = comparison(*(trials(tmp / arm) for arm in ("baseline", "candidate", "work")))["introspection-test"]
    assert row["correct"] == [1, 1, 1] and row["pass_rate"] == [1, 1, 1]
    delta = row["correct_trials_only"]
    assert delta["input_team"]["delta"] == -400 and delta["input_children"]["baseline_median"] == 250
    assert delta["estimated:Vis core system prompt"]["delta"] == -250
    assert delta["journal_read_blocks"]["delta"] == -1 and delta["council.get"]["candidate_median"] == 1
    assert delta["handoff_estimated_input_children"]["delta"] == -500
    assert delta["handoff_estimate_count"]["candidate_median"] == 1
    assert delta["provider_calls"] == {"baseline_median": 9, "candidate_median": 7,
                                       "work_median": 6, "delta": -2, "observed_n": [1, 1, 1]}
    assert delta["read_session"]["work_median"] == 1
    assert delta["uncached_input_leader"]["work_median"] == 1100
    assert delta["tool_stdout_chars"]["work_median"] == 12
    capped = tmp / "baseline/trial"
    result = load(capped / "result.json")
    result["verifier_result"]["rewards"]["reward"] = 0
    write_json(capped / "result.json", result)
    write_json(capped / "verifier/grade.json", {"provider_calls": 9, "converged": True, "errors": 0,
                                               "guards": {"structured": True, "total_output": False}})
    row = comparison(trials(tmp / "baseline"), trials(tmp / "candidate"))["introspection-test"]
    assert row["correct"] == [0, 1] and row["correct_without_output_cap"] == [1, 0]
    check_pull_comparison(tmp)
    print("PASS comparator: fixture team tokens, prompt accounting and handoff paths")


def check_pull_comparison(tmp):
    for arm in ("prefix", "read", "ask"):
        folder = tmp / arm / "pull-trial"
        write_json(folder / "result.json", {"trial_name": arm, "task_name": "introspection-handoff-pull",
                   "verifier_result": {"rewards": {"reward": 1}}})
        metrics = {"total_prompt_tokens": 100, "total_cached_tokens": 40, "total_completion_tokens": 5}
        def block(code):
            return {"tool_calls": [{"function_name": "py3", "arguments": {"code": code}}]}
        steps = []
        if arm == "read":
            steps = [block(f'parent = "{SID}"'), block(
                f'await read_session(parent)\nawait read_session(target="vis_session_id#{SID}")\n'
                'await read_session("other")\nawait read_session()')]
        elif arm == "ask":
            steps = [block('await council.ask("What value?")')]
        write_json(folder / "agent/trajectory.json", {"session_id": SID,
                   "final_metrics": metrics, "subagent_trajectories": [
                       {"session_id": "child", "steps": steps, "final_metrics": metrics}]})
        host = {"child_tasks": {"child": {"task": "Write the value.", "title": "Transfer"}}}
        if arm == "ask":
            host["questions"] = [{"session_id": "child", "entry_id": 2, "state": "replied", "created_at_ms": 15},
                                 {"session_id": "other", "entry_id": 4, "state": "pending", "created_at_ms": 0}]
            host["replies"] = [{"reply_to": 2, "content": "VIOLET-9231-ORBIT", "created_at_ms": 30}]
            trace = [{"event": "trace-chunk", "payload": {"phase": "provider-call", "iteration": i,
                       "started-at-ms": t}} for i, t in [(1, 10), (2, 20), (2, 21), (3, 25), (4, 35)]]
            (folder / "agent/trace.jsonl").write_text("".join(json.dumps(row) + "\n" for row in trace))
        write_json(folder / "agent/task-evidence.json", host)
    row = comparison(*(trials(tmp / arm) for arm in ("prefix", "read", "ask")))["introspection-handoff-pull"]
    assert row["handoff_sources"] == [{"inherited-prefix-or-unobserved": 1}, {"child-read-parent": 1}, {"council.ask": 1}]
    paths = [arm[0] for arm in row["handoff_trials"]]
    assert [r["handoff_child_parent_reads"] for r in paths] == [0, 2, 0]
    assert [r["handoff_child_questions"] for r in paths] == [0, 0, 1]
    assert [r["handoff_leader_question_requests"] for r in paths] == [0, 0, 2]
    assert [r["cached_input_team"] for r in paths] == [80, 80, 80]


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
    agent.auth_home = None
    auth = tmp / "auth"
    auth.mkdir()
    sign_in = auth / "smoke-auth.json"
    sign_in.touch()
    agent.kind = "handoff"
    agent.auth_home = auth
    home = tmp / "oauth-runtime/home"
    values = agent.process_env("fixture", home.parent)
    assert values["VIS_HOME"] == str(home)
    assert values["VIS_HARBOR_SESSION_ID"] == SID
    assert (home / sign_in.name).is_symlink() and (home / sign_in.name).resolve() == sign_in.resolve()
    assert "api_key" not in load(home / "config.yml")["providers"][0]
    agent.kind = "block"
    agent.auth_home = None
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
        check_host_capture(tmp)
        check_runner(args.checkout.resolve(), tmp)
