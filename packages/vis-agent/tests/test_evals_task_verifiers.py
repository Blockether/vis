"""Regression checks for evidence that must distinguish real Task behavior."""

import json
import sqlite3
import textwrap
from pathlib import Path

import evals_atif as atif
import pytest
from blockether.vis.evals import grade, task_evidence

E2E = Path(__file__).resolve().parents[3] / "e2e"


def check(tmp_path, name, *blocks, host=None):
    doc = atif.trajectory(*(atif.step(i, block) for i, block in enumerate(blocks, 1)))
    trial = atif.trial(doc, host_evidence=host or {})
    result = grade.grade_trial(
        {"verify": str(E2E / name / "tests/verify.py")}, tmp_path, trial
    )
    return result.result["guards"]["task"]


@pytest.mark.parametrize("case", ["valid", "dead", "marker-outside-handler"])
def test_shield_binding_needs_a_live_timeout_handler(tmp_path, case):
    first = """plain = task_probe.hold("plain")
await task_probe.started("plain")
try:
    await asyncio.wait_for(plain, .2)
except TimeoutError:
    print("plain-timeout")
plain_cancelled = plain.cancelled()
shielded = task_probe.hold("shielded")
await task_probe.started("shielded")
protected = asyncio.shield(shielded)
"""
    shield = """try:
    await asyncio.wait_for(protected, .2)
except TimeoutError:
    print("shield-timeout")
"""
    last = """shielded_running = not shielded.done()
await task_probe.release("shielded")
first = await shielded
print(first)
second = await shielded
print(second)
print(json.dumps(dict(plain_cancelled=plain_cancelled, shielded_running=shielded_running, results=[first, second])))
"""
    if case == "dead":
        shield = "if False:\n" + textwrap.indent(shield, "    ")
    elif case == "marker-outside-handler":
        shield = shield.replace(
            '    print("shield-timeout")', '    pass\nprint("shield-timeout")'
        )
    code = first + shield + last
    stdout = "plain-timeout\nshield-timeout\nreleased\nreleased\n" + json.dumps(
        {
            "plain_cancelled": True,
            "shielded_running": True,
            "results": ["released", "released"],
        }
    )
    assert check(
        tmp_path,
        "task-asyncio-timeouts",
        atif.block("t1/i1/f1", code, stdout=stdout),
    ) is (case in {"valid", "alias"})


def test_eager_case_reports_missing_trajectory(tmp_path):
    verifier = E2E / "task-extension-eager/tests/verify.py"
    evidence = grade.TaskEvidence("", tmp_path, (), (), (), None)
    assert grade.verifier_failures(verifier, evidence) == [
        "no trajectory was recorded for the eager Task trial"
    ]


@pytest.mark.parametrize(
    "case", ["valid", "dead", "early-pending-marker", "helper", "alias"]
)
def test_eager_case_needs_the_live_bounded_wait(tmp_path, case):
    code = 't = task_probe.hold("eager")\nstarted = await task_probe.started("eager")\n'
    code += 'print("eager-started")\n'
    wait = "done, pending = await asyncio.wait({t}, timeout=.2)\n"
    pending = 'print("eager-pending")\n'
    if case == "dead":
        code += "if False:\n    " + wait + pending
    elif case == "early-pending-marker":
        code += pending + wait
    elif case == "helper":
        # A wait in a function that the block never calls is not a wait.
        code += "def unused():\n    return asyncio.wait({t}, timeout=.2)\n"
        code += "done, pending = set(), {t}\n" + pending
    elif case == "alias":
        code += "from asyncio import wait as wait_tasks\n"
        code += wait.replace("asyncio.wait", "wait_tasks") + pending
    else:
        code += wait + pending
    code += """t.cancel()
try:
    await t
except asyncio.CancelledError:
    pass
print(json.dumps(dict(name=t.get_name(), started=started, pending=not done and pending == {t}, done=t.done(), cancelled=t.cancelled())))
"""
    stdout = "eager-started\neager-pending\n" + json.dumps(
        {
            "name": "t1 task_probe.hold",
            "started": True,
            "pending": True,
            "done": True,
            "cancelled": True,
        }
    )
    assert check(
        tmp_path,
        "task-extension-eager",
        atif.block("t1/i1/f1", code, stdout=stdout),
    ) is (case in {"valid", "alias"})


@pytest.mark.parametrize(
    "case", ["overlap", "sequential", "short", "overlap-outside-empty-helper"]
)
def test_watchdog_requires_overlapping_poll_intervals(tmp_path, case):
    short = atif.row("short", "watchdog_probe.poll", duration_ms=1000)
    long = atif.row(
        "long",
        "watchdog_probe.poll_async",
        sequence=2,
        duration_ms=0 if case == "short" else 310_000,
    )
    path = tmp_path / ".vis/calls/watchdog.jsonl"
    path.parent.mkdir(parents=True)
    long_start = 3.0 if case == "sequential" else 1.5
    path.write_text(
        "\n".join(
            json.dumps(
                {
                    "operation": op,
                    "duration": duration,
                    "count": count,
                    "timed_out": False,
                    "started": start,
                    "finished": finish,
                }
            )
            for op, duration, count, start, finish in [
                ("poll", 1, 1, 1.0, 2.0),
                (
                    "poll_async",
                    0 if case == "short" else 310,
                    2,
                    long_start,
                    long_start + 310,
                ),
            ]
        )
    )
    helper = (
        "return await collect()"
        if case in {"sequential", "overlap-outside-empty-helper"}
        else "tasks = (watchdog_probe.poll(1), watchdog_probe.poll_async(300 + 10))\n    return await collect(*tasks)"
    )
    block1 = atif.block(
        "t1/i1/f1",
        "from asyncio import gather as collect\nasync def wait_release_progress():\n    "
        + helper,
    )
    invoke = "call = wait_release_progress\nawait call()"
    if case == "sequential":
        invoke += "\nawait watchdog_probe.poll(1)\nawait watchdog_probe.poll_async(310)"
    if case.startswith("overlap"):
        prefix = (
            "await wait_release_progress()\n" if case.endswith("empty-helper") else ""
        )
        invoke = (
            prefix
            + "short_task = watchdog_probe.poll(1)\nlong_task = watchdog_probe.poll_async(310)\nawait collect(short_task, long_task)"
        )
    block2 = atif.block(
        "t1/i2/f1",
        invoke,
        activity=atif.receipt(short, long),
        stdout='{"counts": [1, 2], "timed_out": [false, false]}\nEXTENSION_WATCHDOG_OK\n',
    )
    assert check(tmp_path, "extension-watchdog", block1, block2) is case.startswith(
        "overlap"
    )


def shell_launch_group(row_id):
    """Model the native Activity group for a launch and its log read."""
    launch = atif.row(
        row_id,
        "shell",
        presenter="shell",
        signal="mutation",
        handle_id="python3",
        resources=[
            {"type": "task", "id": "t1"},
            {"type": "shell-handle", "id": "python3"},
        ],
    )
    logs = atif.row(f"{row_id}-logs", "_shell-logs", sequence=2, handle_id="python3")
    return {**launch, "id": f"group-{row_id}", "children": [launch, logs]}


def shell_events(task_id="t1", *, state="done"):
    row = {"kind": "shell", "name": f"{task_id} shell", "id": task_id}
    return [
        {"type": "task.started", "task": {**row, "state": "running"}},
        {"type": "task.ended", "task": {**row, "state": state}},
    ]


@pytest.mark.parametrize(
    "case",
    [
        "unpack",
        "iterator",
        "wrong-task",
        "dead-input",
        "original-handle",
        "original-await",
        "filter",
        "dict",
        "attach",
        "split",
        "split-unnamed",
    ],
)
def test_fold_matches_recovered_name_and_host_result(tmp_path, case):
    command = "python3 fold_probe.py"
    name = f't1 shell("{command}")'
    start = atif.block(
        "t1/i1/f1",
        f'sh = await shell(command={command!r}, stdin="tty")\nname = sh.get_name()\nprint(name, sh.logs())',
        stdout="ready\n",
        activity=atif.receipt(shell_launch_group("launch")),
    )
    lookup = (
        "tasks = asyncio.all_tasks()\nrecovered = next(t for t in tasks if t.get_name() == name)"
        if case == "iterator"
        else "[recovered] = [t for t in asyncio.all_tasks() if t.get_name() == name]"
    )
    if case == "filter":
        lookup = "recovered = next(filter(lambda t: t.get_name() == name, asyncio.all_tasks()))"
    elif case == "dict":
        lookup = "by_name = {t.get_name(): t for t in asyncio.all_tasks()}\nrecovered = by_name[name]"
    code = (
        'print(fold_session("-t1/i1", name))\n'
        + lookup
        + "\nprint(recovered.get_name())\n"
    )
    rest = """send = recovered.type
send("release")
result = await recovered
print(json.dumps(dict(exit=result.get("exit"), out=result.get("out"))))
"""
    if case == "original-await":
        rest = rest.replace("await recovered", "await sh")
    code += (
        "if False:\n" + textwrap.indent(rest, "    ") if case == "dead-input" else rest
    )
    if case == "original-handle":
        code = 'print(fold_session("-t1/i1", name))\nasyncio.all_tasks()\nprint(sh.get_name())\n'
        code += rest.replace("recovered", "sh")
    recovered = name.replace("t1 ", "t9 ", 1) if case == "wrong-task" else name
    attach = None
    if case == "attach":
        # `shell(op="attach")` returns the same handle; the grader counted it as a launch.
        code = code.replace(
            "send = ", 'await shell(op="attach", id="python3")\nsend = ', 1
        )
        attach = atif.receipt(
            atif.row(
                "attach",
                "shell",
                presenter="shell",
                evidence=[
                    {"kind": "arguments", "text": '[{"id" "python3", "op" "attach"}]'}
                ],
            )
        )
    fold = atif.block(
        "t1/i2/f1",
        code,
        stdout=f'{recovered}\n{{"exit": 0, "out": "fold-result-7351"}}\n',
        activity=attach,
    )
    blocks = [start, fold]
    if case.startswith("split"):
        # A model may print the recovered name, then send input in a later block.
        named = case == "split"
        lookup_code = code[: code.index("send = ")]
        if not named:
            lookup_code = lookup_code.replace("print(recovered.get_name())\n", "")
        blocks = [
            start,
            atif.block(
                "t1/i2/f1", lookup_code, stdout=f"{recovered}\n" if named else ""
            ),
            atif.block(
                "t1/i3/f1", rest, stdout='{"exit": 0, "out": "fold-result-7351"}\n'
            ),
        ]
    events = shell_events(state="failed" if case == "dead-input" else "done")
    output = (
        "ready\nNo release line received.\n"
        if case == "dead-input"
        else "ready\nrelease\nfold-result-7351\n"
    )
    assert check(
        tmp_path,
        "task-fold-recover",
        *blocks,
        host={
            "events": events,
            "shells": [
                {
                    "command": command,
                    "exit": int(case == "dead-input"),
                    "output": output,
                }
            ],
        },
    ) is (case in {"unpack", "iterator", "filter", "dict", "attach", "split"})


def test_host_capture_uses_only_the_trial_session_journal_and_questions(tmp_path):
    sid = "00000000-0000-4000-8000-000000000001"
    db = tmp_path / "trial.db"
    with sqlite3.connect(db) as connection:
        connection.executescript("""
            CREATE TABLE session_agent (ordinal, session_id, parent_id);
            CREATE TABLE extension_aggregate (index_data, extension_id, kind, session_soul_id);
            CREATE TABLE council_entry (id, author_sid, content, reply_required);
            CREATE TABLE council_ping (entry_id, recipient_sid, state);
            INSERT INTO council_entry VALUES (1, 'child', 'question', 1),
                (2, 'child', 'not a question', 0), (3, 'other-child', 'other question', 1);
        """)
        connection.executemany(
            "INSERT INTO council_ping VALUES (?, ?, ?)",
            [
                (1, sid, "delivered"),
                (2, sid, "delivered"),
                (3, "other", "delivered"),
            ],
        )
        connection.execute("INSERT INTO session_agent VALUES (1, 'child', ?)", (sid,))
        log = tmp_path / "logs/2026-01-01/shell" / sid / "one.log"
        log.parent.mkdir(parents=True)
        log.write_text("process output\n")
        data = {"command": "python3 probe.py", "log_path": str(log), "exit": 0}
        connection.execute(
            "INSERT INTO extension_aggregate VALUES (?, 'foundation-core', ':shell-log', ?)",
            (json.dumps(data), sid),
        )
    journal = tmp_path / "gateway/events"
    journal.mkdir(parents=True)
    own = {
        "session_id": sid,
        "type": "task.started",
        "task": {"session_id": sid, "id": "t1"},
    }
    foreign = {**own, "session_id": "other"}
    (journal / f"{sid}.ndjson").write_text(
        json.dumps(own) + "\n" + json.dumps(foreign) + "\n"
    )
    assert task_evidence.capture(sid, tmp_path, db) == {
        "events": [{"type": "task.started", "task": own["task"]}],
        "children": {"a1": "child"},
        "shells": [
            {"command": "python3 probe.py", "exit": 0, "output": "process output\n"}
        ],
        "questions": [
            {
                "session_id": "child",
                "entry_id": 1,
                "content": "question",
                "state": "delivered",
            }
        ],
    }


@pytest.mark.parametrize(
    "case",
    [
        "live",
        "aliased",
        "inline",
        "contains",
        "snapshot",
        "dead",
        "unrelated-wait",
        "logs-only",
        "hard-coded-running",
        "unused-membership",
        "rebound-pending",
        "done-set",
    ],
)
def test_shell_needs_live_control_and_real_terminal_events(tmp_path, case):
    hold, finish = "python3 shell_probe.py hold", "python3 shell_probe.py finish"
    wait = "done, pending = await asyncio.wait([sh], timeout=.2)"
    if case == "aliased":
        wait = "from asyncio import wait as pause\nhandles = {sh}\ndone, pending = await pause(handles, timeout=.2)"
    elif case == "unrelated-wait":
        wait = "done, pending = await other.wait([sh], timeout=.2)"
    output = {
        "aliased": 'sh.get("out")',
        "logs-only": "sh.logs().out",
        "snapshot": "sh.snapshot().out",
    }.get(
        case, "sh.out"
    )
    running = {
        "hard-coded-running": "True",
        "contains": "pending.__contains__(sh)",
        "done-set": "sh in done",
    }.get(case, "sh in pending")
    if case == "rebound-pending":
        wait += "\npending = {sh}"
    control = f"""{wait}
running = {running}
start_output = {output}
sh.cancel()
try:
    await sh
except asyncio.CancelledError:
    pass
cancelled = sh.cancelled()
"""
    first = f"sh = await shell({hold!r})\n" + (
        "if False:\n" + textwrap.indent(control, "    ") if case == "dead" else control
    )
    report_value = {"unused-membership": "True", "inline": "sh in pending"}.get(
        case, "running"
    )
    second = f"""import json
sh2 = await shell(command={finish!r})
r = await sh2
report = {{"running": {report_value}, "start_output": start_output,
          "cancelled": cancelled, "exit": r.exit, "output": r.out}}
print(json.dumps(report))
"""
    events = shell_events(state="cancelled") + shell_events("t2")
    events[1]["task"]["cancel_reason"] = "agent"
    block1 = atif.block(
        "t1/i1/f1", first, activity=atif.receipt(shell_launch_group("hold"))
    )
    block2 = atif.block(
        "t1/i2/f1",
        second,
        activity=atif.receipt(atif.row("finish", "shell")),
        stdout=json.dumps(
            {
                "running": True,
                "start_output": "hold-started",
                "cancelled": True,
                "exit": 0,
                "output": "finish-ok",
            }
        ),
    )
    shells = [
        {"command": hold, "exit": None, "output": "hold-started"},
        {"command": finish, "exit": 0, "output": "finish-ok"},
    ]
    assert check(
        tmp_path,
        "task-shell-lifecycle",
        block1,
        block2,
        host={"events": events, "shells": shells},
    ) is (case in {"live", "aliased", "inline", "contains", "snapshot"})
    assert not check(tmp_path, "task-shell-lifecycle", block1, block2)


def test_fold_then_test_accepts_a_command_wrapper_and_colored_log(tmp_path):
    command = "cd /workspace && python3 -m unittest discover -s tests -t . 2>&1"
    block = atif.block(
        "t1/i2/f1",
        f"sh = await shell(command={command!r})\nr = await sh\nprint(r.out)",
        stdout="test_discount_caps_at_fifty\nFAILED (failures=1)\n",
        activity=atif.receipt(atif.row("test", "shell")),
    )
    assert check(
        tmp_path,
        "fold-then-test",
        block,
        host={
            "events": shell_events(),
            "shells": [
                {
                    "command": command,
                    "exit": 1,
                    "output": "test_discount_caps_at_fifty\n\x1b[31mFAILED\x1b[0m (failures=1)\n",
                }
            ],
        },
    )


@pytest.mark.parametrize("case", ["native", "wrong-server", "wrong-task"])
def test_native_mcp_cancel_receipt_and_later_observation(tmp_path, case):
    task = E2E / "task-mcp-cancel" / "tests"
    scenario = json.loads((task / "checks.json").read_text())
    scenario["verify"] = str(task / scenario["verify"])
    # This is the top-level Activity row of the paid native trial, with a stable ID.
    row = json.loads(
        (
            Path(__file__).parent / "fixtures/evals/native-mcp-hold-activity.json"
        ).read_text()
    )
    if case == "wrong-server":
        row["evidence"][0]["text"] = '["not-harbor-probe" "hold" {}]'
    if case == "wrong-task":
        row["resources"][0]["id"] = "t9"
    call = atif.block(
        "t1/i1/f1",
        "task = asyncio.create_task(harbor_probe.hold())\nawait harbor_probe.started()\ntask.cancel()\ntry:\n    await task\nexcept asyncio.CancelledError:\n    cancelled = task.cancelled()\nsummary = await harbor_probe.summary()",
        activity=atif.receipt(row, atif.row("summary", "call", sequence=2)),
    )
    report = atif.block(
        "t1/i2/f1",
        'print(json.dumps({"cancelled": cancelled, **json.loads(summary["content"][0]["text"])}))',
        stdout='{"cancelled": true, "holds": 1, "cancels": 1}\n',
    )
    doc = atif.trajectory(
        atif.step(1, call), atif.step(2, report), answer="mcp-task-ok"
    )
    task_row = {"id": "t1", "kind": "mcp", "name": "t1 harbor_probe.hold"}
    host = {
        "events": [
            {"type": "task.started", "task": {**task_row, "state": "running"}},
            {
                "type": "task.ended",
                "task": {**task_row, "state": "cancelled", "cancel_reason": "agent"},
            },
        ]
    }
    log = tmp_path / ".vis/calls/mcp.jsonl"
    log.parent.mkdir(parents=True)
    log.write_text(
        '{"event":"hold"}\n{"event":"started"}\n{"event":"cancel","matched":true}\n{"event":"summary","holds":1,"cancels":1}\n'
    )
    result = grade.grade_trial(scenario, tmp_path, atif.trial(doc, host_evidence=host))
    assert result.is_passed is (case == "native"), result.result["err_msgs"]
