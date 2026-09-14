"""Opt-in JVM gateway workload using the real SDK and deterministic provider.

Set VIS_TEST_LOCAL_COMMAND to JVM engine argv and VIS_TEST_GATEWAY_PROFILE=1.
Run with pytest -s to see CPU/RSS/collected-heap samples and allocation totals.
JDK 21+ jcmd, jfr and jshell are required. JVM-wide allocation counter deltas
measure volume; JFR weights are sampling estimates, not exact per-site totals.
VIS_TEST_GATEWAY_PROFILE_TASKS and VIS_TEST_GATEWAY_PROFILE_WARMUP control the
measured and warmup session counts (defaults: 20 and 5). Set
VIS_TEST_GATEWAY_PROFILE_TURNS_PER_SESSION (default: 1) to exercise growing history.
VIS_TEST_GATEWAY_PROFILE_IDLE_SECONDS (default: 5) controls the final idle sample;
use 65 or more to include the default 60-second periodic GC interval.
VIS_TEST_GATEWAY_PROFILE_COLLECT_BEFORE=0 omits the pre-measurement full GC
(default: 1). Use this to check whether the GC barrier distorts CPU measurements;
only the final collected sample then represents explicitly collected heap.
VIS_TEST_GATEWAY_PROFILE_TRANSCRIPT_EVERY_TURN=0 exports the transcript only once
per session (default: 1, after every turn). Both modes verify every tool result;
the final-only mode still includes one transcript export in the measured workload.
JFR retains the measured workload profile, not the subsequent idle window.
Only disposable sessions, files and gateway processes are used. No paid calls.
"""

import json
import os
import re
import shutil
import subprocess
import time

import pytest
from test_engine import sdk_fixture


def _jvm_total_allocated_bytes(pid, jshell):
    # JDK 21+ exposes a JVM-wide counter, including threads that have terminated.
    # Unlike weighted JFR samples, its delta is bounded by our two snapshots.
    source = """
import com.sun.tools.attach.VirtualMachine;
import javax.management.ObjectName;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
{
    var vm = VirtualMachine.attach("GATEWAY_PID");
    String address;
    try {
        address = vm.getAgentProperties().getProperty(
            "com.sun.management.jmxremote.localConnectorAddress");
    } finally {
        vm.detach();
    }
    try (var connector = JMXConnectorFactory.connect(new JMXServiceURL(address))) {
        var connection = connector.getMBeanServerConnection();
        var bean = new ObjectName("java.lang:type=Threading");
        System.out.println("VIS_ALLOCATED_BYTES="
            + connection.getAttribute(bean, "TotalThreadAllocatedBytes"));
    }
}
/exit
""".replace("GATEWAY_PID", str(pid))
    result = subprocess.run(
        [jshell, "--execution", "local", "--feedback", "silent", "--no-startup", "-"],
        input=source,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=True,
        timeout=30,
    )
    match = re.search(r"VIS_ALLOCATED_BYTES=(\d+)", result.stdout)
    assert match, f"JVM allocation counter unavailable: {result.stdout}"
    return int(match[1])


@pytest.mark.parametrize(
    ("output", "expected"),
    [
        ("VIS_ALLOCATED_BYTES=12345678900", 12345678900),
        ("VIS_ALLOCATED_BYTES=-1", None),
        ("Error: unsupported attribute", None),
    ],
)
def test_jvm_allocation_counter_result(monkeypatch, output, expected):
    def run(args, **kwargs):
        assert args[-1] == "-"
        assert 'VirtualMachine.attach("321")' in kwargs["input"]
        assert "TotalThreadAllocatedBytes" in kwargs["input"]
        return subprocess.CompletedProcess(args, 0, stdout=output)

    monkeypatch.setattr(subprocess, "run", run)
    if expected is None:
        with pytest.raises(AssertionError, match="allocation counter unavailable"):
            _jvm_total_allocated_bytes(321, "jshell")
    else:
        assert _jvm_total_allocated_bytes(321, "jshell") == expected


@pytest.mark.parametrize("setting", ["COLLECT_BEFORE", "TRANSCRIPT_EVERY_TURN"])
def test_profile_rejects_invalid_mode(tmp_path, monkeypatch, setting):
    monkeypatch.setattr(shutil, "which", lambda name: name)
    monkeypatch.setenv("VIS_TEST_GATEWAY_PROFILE_COLLECT_BEFORE", "1")
    monkeypatch.setenv("VIS_TEST_GATEWAY_PROFILE_TRANSCRIPT_EVERY_TURN", "1")
    monkeypatch.setenv(f"VIS_TEST_GATEWAY_PROFILE_{setting}", "invalid")
    with pytest.raises(AssertionError, match=f"{setting} must be 0 or 1"):
        test_jvm_gateway_repeated_sessions(tmp_path, monkeypatch)


@pytest.mark.skipif(
    os.environ.get("VIS_TEST_GATEWAY_PROFILE") != "1",
    reason="set VIS_TEST_GATEWAY_PROFILE=1 for JVM resource measurements",
)
def test_jvm_gateway_repeated_sessions(tmp_path, monkeypatch):
    jcmd = shutil.which("jcmd")
    jfr = shutil.which("jfr")
    jshell = shutil.which("jshell")
    if not jcmd or not jfr or not jshell:
        pytest.skip("JDK 21+ jcmd, jfr and jshell are required for JVM profiling")
    tasks = int(os.environ.get("VIS_TEST_GATEWAY_PROFILE_TASKS", "20"))
    warmup_tasks = int(os.environ.get("VIS_TEST_GATEWAY_PROFILE_WARMUP", "5"))
    turns_per_session = int(
        os.environ.get("VIS_TEST_GATEWAY_PROFILE_TURNS_PER_SESSION", "1")
    )
    idle_seconds = int(os.environ.get("VIS_TEST_GATEWAY_PROFILE_IDLE_SECONDS", "5"))
    collect_before_value = os.environ.get(
        "VIS_TEST_GATEWAY_PROFILE_COLLECT_BEFORE", "1"
    )
    assert collect_before_value in {"0", "1"}, "COLLECT_BEFORE must be 0 or 1"
    collect_before = collect_before_value == "1"
    transcript_every_turn_value = os.environ.get(
        "VIS_TEST_GATEWAY_PROFILE_TRANSCRIPT_EVERY_TURN", "1"
    )
    assert transcript_every_turn_value in {"0", "1"}, (
        "TRANSCRIPT_EVERY_TURN must be 0 or 1"
    )
    transcript_every_turn = transcript_every_turn_value == "1"
    assert tasks > 0 and warmup_tasks >= 0 and turns_per_session > 0
    assert idle_seconds > 0
    processes = []
    worker_samples = []
    tool_call_ids = set()
    with sdk_fixture(
        tmp_path,
        monkeypatch,
        "http",
        tool_code=lambda position: (
            f"print(sum(range(1000))); print('execution-{position}')"
        ),
        on_process=processes.append,
    ) as (client, work, requests):
        process = processes[0]

        def command(*args):
            return subprocess.check_output(
                [jcmd, str(process.pid), *args], text=True, timeout=30
            )

        def cpu_seconds(value):
            days, _, clock = value.rpartition("-")
            seconds = 0.0
            for part in clock.split(":"):
                seconds = seconds * 60 + float(part)
            return seconds + (int(days) * 86400 if days else 0)

        def process_sample(*, exclude_children=()):
            rows = subprocess.check_output(
                ["ps", "-axo", "pid=,ppid=,rss=,time="], text=True, timeout=10
            )
            process_info = {}
            for row in rows.splitlines():
                pid, parent, rss, cpu = row.split()
                process_info[int(pid)] = {
                    "parent": int(parent),
                    "rss_bytes": int(rss) * 1024,
                    "cpu_seconds": cpu_seconds(cpu),
                }
            children = set()
            parents = {process.pid}
            while parents:
                parents = {
                    pid
                    for pid, info in process_info.items()
                    if info["parent"] in parents
                } - children
                children.update(parents)
            children.difference_update(exclude_children)
            return {
                "rss_bytes": process_info[process.pid]["rss_bytes"],
                "cpu_seconds": process_info[process.pid]["cpu_seconds"],
                "child_processes": len(children),
                "child_pids": sorted(children),
                "child_rss_bytes": sum(
                    process_info[pid]["rss_bytes"] for pid in children
                ),
                "child_cpu_seconds": sum(
                    process_info[pid]["cpu_seconds"] for pid in children
                ),
            }

        def sample():
            resources = process_sample()
            heap = command("GC.heap_info")
            used = re.search(r"heap.*?used (\d+)K", heap)
            assert used, heap
            return {
                **resources,
                "heap_used_bytes": int(used[1]) * 1024,
                "total_allocated_bytes": _jvm_total_allocated_bytes(
                    process.pid, jshell
                ),
            }

        def task(streamed, *, measure_worker=False):
            session = client.create_session(root=str(work), channel="app")
            try:
                for turn_index in range(turns_per_session):
                    turn = session.send(
                        f"Turn {turn_index}: compute the sum of integers from zero through 999"
                    )
                    if streamed:
                        with session.events(cursor=turn.cursor, reconnects=0) as events:
                            for event in events:
                                assert event.type != "iteration.error", event.data
                                if event.type in {
                                    "turn.completed",
                                    "turn.failed",
                                    "turn.cancelled",
                                }:
                                    assert event.type == "turn.completed", event.data
                                    break
                    assert turn.wait(timeout=60)["status"] == "completed"
                    if transcript_every_turn:
                        assert "499500" in str(session.transcript().content)
                    assert len(requests) == 2
                    tool_result = requests[-1]["messages"][-1]
                    assert tool_result["role"] == "tool"
                    assert "499500" in str(tool_result["content"])
                    call_id = tool_result["tool_call_id"]
                    assert call_id not in tool_call_ids, (
                        "Provider reused a tool call ID"
                    )
                    assert f"execution-{call_id.removeprefix('call_')}" in str(
                        tool_result["content"]
                    )
                    tool_call_ids.add(call_id)
                    requests.clear()  # Do not retain earlier prompts in the provider.
                if not transcript_every_turn:
                    assert "499500" in str(session.transcript().content)
                assert len(session.turns()) == turns_per_session
                if measure_worker:
                    worker_samples.append(
                        process_sample(exclude_children=before["child_pids"])
                    )
            finally:
                session.delete()

        command("ManagementAgent.start_local")
        for index in range(warmup_tasks):
            task(index % 2 == 0)
        recording = tmp_path / "gateway.jfr"
        command(
            "JFR.start",
            "name=sdk-dogfood",
            "settings=profile",
            f"filename={recording}",
            "dumponexit=true",
        )
        if collect_before:
            command("GC.run")
        before = sample()
        started = time.monotonic()
        for index in range(tasks):
            task(index % 2 == 0, measure_worker=True)
        elapsed = time.monotonic() - started
        after = sample()
        assert after["total_allocated_bytes"] > before["total_allocated_bytes"]
        command("JFR.stop", "name=sdk-dogfood")
        command("GC.run")
        collected = sample()
        idle_started = time.monotonic()
        time.sleep(idle_seconds)
        idle = sample()
        idle_elapsed = time.monotonic() - idle_started
        assert process.poll() is None
        # Registration keeps its shared trusted worker; session-owned workers
        # must be gone, without replacing or accumulating shared processes.
        assert after["child_pids"] == idle["child_pids"] == before["child_pids"]
        # A worker may already have exited at an end-of-task point sample.
        # Prove execution from each turn's tool result above, not process liveness.
        # Summarize while pytest still owns the artifact. No stack data is needed
        # for the total; excluding it keeps the SDK-side profiling overhead small.
        allocation_events = json.loads(
            subprocess.check_output(
                [
                    jfr,
                    "print",
                    "--json",
                    "--stack-depth",
                    "0",
                    "--events",
                    "jdk.ObjectAllocationSample",
                    str(recording),
                ],
                text=True,
                timeout=60,
            )
        )["recording"]["events"]
        assert allocation_events, "JFR did not collect allocation samples"
        print(
            "SDK_GATEWAY_PROFILE "
            + json.dumps(
                {
                    "collected_before": collect_before,
                    "transcript_every_turn": transcript_every_turn,
                    "transcript_exports": tasks
                    * (turns_per_session if transcript_every_turn else 1),
                    "tasks": tasks,
                    "warmup_tasks": warmup_tasks,
                    "turns_per_session": turns_per_session,
                    "measured_turns": tasks * turns_per_session,
                    "wall_seconds": elapsed,
                    "jvm_allocated_bytes": (
                        after["total_allocated_bytes"] - before["total_allocated_bytes"]
                    ),
                    "before": before,
                    "after": after,
                    "collected": collected,
                    "idle": idle,
                    "idle_requested_seconds": idle_seconds,
                    "idle_wall_seconds": idle_elapsed,
                    # End-of-task samples may miss workers that already exited;
                    # CPU is a lower bound and RSS is not a peak measurement.
                    "observed_cpu_seconds_lower_bound": (
                        after["cpu_seconds"]
                        - before["cpu_seconds"]
                        + after["child_cpu_seconds"]
                        - before["child_cpu_seconds"]
                        + sum(sample["child_cpu_seconds"] for sample in worker_samples)
                    ),
                    "sampled_session_worker_cpu_seconds": sum(
                        sample["child_cpu_seconds"] for sample in worker_samples
                    ),
                    "shared_worker_cpu_seconds": (
                        after["child_cpu_seconds"] - before["child_cpu_seconds"]
                    ),
                    "session_worker_processes": sorted(
                        {sample["child_processes"] for sample in worker_samples}
                    ),
                    "max_sampled_session_worker_rss_bytes": max(
                        sample["child_rss_bytes"] for sample in worker_samples
                    ),
                    "sampled_allocation_bytes": sum(
                        event["values"]["weight"] for event in allocation_events
                    ),
                    "allocation_samples": len(allocation_events),
                    "recording": str(recording),
                },
                sort_keys=True,
            )
        )
    assert processes[0].poll() is not None
