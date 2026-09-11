"""#202: execute the documented synchronous monitoring recipe with real readers."""

import json
import re
import sys
import time
from collections import Counter
from pathlib import Path
from threading import Barrier, Event, Lock, Thread, current_thread
from types import ModuleType, SimpleNamespace

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts


@pytest.fixture
def recipe(monkeypatch):
    document = Path(__file__).parents[3] / "resources/vis-docs/live-views.md"
    source = document.read_text()
    module = ModuleType("monitor")
    monkeypatch.setitem(sys.modules, "monitor", module)
    code = re.search(r"```python\n# monitor.py\n(.*?)\n```", source, re.S)[1]
    exec(compile(code, str(document), "exec"), module.__dict__)
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    closed = Event()
    original = recorder.live
    host_threads = []

    def live(envelope):
        host_threads.append(current_thread())
        result = original(envelope)
        if json.loads(envelope)["op"] == "close":
            closed.set()
        return result

    monkeypatch.setattr(recorder, "live", live)
    threads = set()
    events = []
    deadlines = []
    counts = Counter()
    lock = Lock()

    def run(read_once, builds=None, **options):
        def read(build, *, deadline, stop):
            with lock:
                threads.add(current_thread())
                counts["active"] += 1
                counts["peak"] = max(counts["peak"], counts["active"])
                counts[build] += 1
                events.append(stop)
                deadlines.append(deadline - time.monotonic())
            try:
                return read_once(build, deadline=deadline, stop=stop)
            finally:
                with lock:
                    counts["active"] -= 1
                    counts["cleaned"] += 1

        try:
            return module.watch_builds(
                builds if builds is not None else [module.Build("staging", "42")],
                read,
                **({"timeout_s": 2.0, "poll_s": 0.001} | options),
            )
        finally:
            assert all(not thread.is_alive() for thread in threads)
            assert all(event.is_set() for event in events)
            assert counts["active"] == 0
            assert counts["cleaned"] == len(events)
            assert counts["peak"] <= 2
            assert all(0 < remaining <= 5.0 for remaining in deadlines)

    return SimpleNamespace(
        module=module,
        recorder=recorder,
        run=run,
        counts=counts,
        closed=closed,
        host_threads=host_threads,
        source=source,
    )


def test_fast_failure_closes_before_slow_reader_cleanup_without_starting_next(recipe):
    slow_started = Event()
    builds = [recipe.module.Build("staging", str(n)) for n in (40, 41, 42)]

    def read(build, *, deadline, stop):
        if build == builds[0]:
            slow_started.set()
            assert stop.wait(1.5)
            # Joining this reader before closing the view would deadlock the test.
            assert recipe.closed.wait(0.3)
            return "succeeded"
        assert build == builds[1]
        assert slow_started.wait(1)
        return "failed"

    result = recipe.run(read, builds)
    assert result["outcome"] == "failed"
    assert result["view"]["reason"] == "failed"
    assert result["observations"] == [
        {
            "environment": "staging",
            "build_id": str(n),
            "state": state,
            "read_error": None,
        }
        for n, state in [(40, "unobserved"), (41, "failed"), (42, "unobserved")]
    ]
    assert recipe.counts[builds[2]] == 0
    assert all(thread is current_thread() for thread in recipe.host_threads)


def test_success_polls_the_entire_fixed_set_with_only_two_readers(recipe):
    barrier = Barrier(2)
    builds = [recipe.module.Build(env, "42") for env in ("staging", "qa", "preview")]

    def read(build, *, deadline, stop):
        if build in builds[:2] and recipe.counts[build] == 1:
            barrier.wait(timeout=1)
        return "succeeded" if recipe.counts[build] == 2 else "running"

    result = recipe.run(read, builds)
    assert result["outcome"] == "completed"
    assert [r["state"] for r in result["observations"]] == ["succeeded"] * 3
    assert all(recipe.counts[b] == 2 for b in builds)
    assert recipe.counts["peak"] == 2


# #203: the catalog stays inert; the registered observation still owns cancellation.
@pytest.mark.parametrize("registered", [False, True])
def test_stop_during_host_wait_joins_readers_and_preserves_note(
    recipe, monkeypatch, registered
):
    if registered:
        symbol = vis.Symbol(
            recipe.module.watch_builds,
            name="watch_builds",
            activity=vis.Activity(label="Watch builds"),
        )
        catalog = vis.Catalog([symbol])
        vis.testing.assert_catalog(catalog, names=["watch_builds"])
        assert "watch_builds" in catalog.help("watch_builds").text
        assert not recipe.counts
        monkeypatch.setattr(vis, "_registration", {"spec": None})
        vis.register(
            vis.Extension(
                name="catalog-monitor",
                description="Monitor catalog test.",
                alias="monitor",
                symbols=[symbol],
            )
        )
        monkeypatch.setattr(
            recipe.module, "watch_builds", vis._registration["spec"]["symbols"][0]["fn"]
        )
    started = Event()

    def read(build, *, deadline, stop):
        started.set()
        assert stop.wait(1.5)
        return "running"

    def interrupt():
        assert started.wait(1)
        recipe.recorder.close(reason="interrupted", note="Stop monitoring")

    operator = Thread(target=interrupt)
    operator.start()
    try:
        result = recipe.run(read)
    finally:
        operator.join(timeout=2)
    assert not operator.is_alive()
    assert result["outcome"] == "interrupted"
    assert result["view"]["note"] == "Stop monitoring"
    assert result["observations"][0]["state"] == "unobserved"


def test_stop_racing_a_patch_is_caught_as_vis_interrupted(recipe, monkeypatch):
    original = recipe.recorder.live
    interrupted = False

    def live(envelope):
        nonlocal interrupted
        if json.loads(envelope)["op"] == "patch" and not interrupted:
            interrupted = True
            recipe.recorder.close(reason="interrupted", note="Stop during update")
        return original(envelope)

    monkeypatch.setattr(recipe.recorder, "live", live)
    result = recipe.run(lambda *args, **kwargs: "running")
    assert interrupted
    assert result["outcome"] == "interrupted"
    assert result["view"]["note"] == "Stop during update"


def test_overall_timeout_signals_inflight_readers_and_keeps_unknown_state(recipe):
    def read(build, *, deadline, stop):
        assert deadline - time.monotonic() <= 0.05
        assert stop.wait(1)
        return "running"

    result = recipe.run(read, timeout_s=0.05)
    assert result["outcome"] == "timeout"
    assert result["view"]["reason"] == "timeout"
    assert result["observations"][0]["state"] == "unobserved"


@pytest.mark.parametrize("invalid", [False, True])
def test_read_error_is_distinct_from_build_failure_and_keeps_last_observation(
    recipe, invalid
):
    def read(build, *, deadline, stop):
        if recipe.counts[build] == 1:
            return "running"
        if invalid:
            return "unknown"
        raise TimeoutError("Do not publish the raw client diagnostic")

    result = recipe.run(read)
    assert result["outcome"] == "observation_error"
    assert result["view"]["reason"] == "failed"
    assert result["observations"][0]["state"] == "running"
    assert result["observations"][0]["read_error"] == (
        "ValueError" if invalid else "TimeoutError"
    )
    assert "raw client diagnostic" not in str(result)


def test_unexpected_producer_failure_still_closes_failed_and_joins_readers(
    recipe, monkeypatch
):
    original = recipe.recorder.live

    def fail_wait(envelope):
        if json.loads(envelope).get("timeout_ms"):
            raise RuntimeError("Producer stopped")
        return original(envelope)

    monkeypatch.setattr(recipe.recorder, "live", fail_wait)
    with pytest.raises(RuntimeError, match="Producer stopped"):
        recipe.run(lambda *args, **kwargs: "running")
    assert recipe.recorder.close()["reason"] == "failed"


@pytest.mark.parametrize(
    "case", ["empty", "duplicate", "blank", "nan", "zero", "negative_poll"]
)
def test_invalid_selection_is_refused_before_opening_or_polling(recipe, case):
    build = recipe.module.Build("staging", "42")
    builds = {
        "empty": [],
        "duplicate": [build, build],
        "blank": [recipe.module.Build("", "42")],
    }.get(case, [build])
    options = {
        "nan": {"timeout_s": float("nan")},
        "zero": {"timeout_s": 0},
        "negative_poll": {"poll_s": -1},
    }.get(case, {})
    with pytest.raises(ValueError):
        recipe.run(
            lambda *args, **kwargs: pytest.fail("Must not read"), builds, **options
        )
    assert not recipe.recorder.said


def test_registration_exposes_only_typed_wrapper_with_activity_states(
    recipe, monkeypatch
):
    client = ModuleType("my_ci")
    client.read_build = lambda *args, **kwargs: "succeeded"
    monkeypatch.setitem(sys.modules, "my_ci", client)
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    entry = ModuleType("ci_monitor")
    monkeypatch.setitem(sys.modules, "ci_monitor", entry)
    source = re.search(
        r"```python\n# \.vis/extensions/ci_monitor.py\n(.*?)\n```", recipe.source, re.S
    )[1]
    exec(compile(source, "ci_monitor.py", "exec"), entry.__dict__)
    tool = vis._registration["spec"]["symbols"][0]
    assert tool["contract"]["name"] == "watch_builds"
    assert _contracts.validate("symbol", "callable", tool["contract"])
    assert tool["activity"]["show_start"] is True
    assert _contracts.validate("activity", "declaration", tool["activity"])
    assert entry.watch([recipe.module.Build("staging", "42")])["outcome"] == "completed"
    for phase, result, error, expected in [
        ("start", None, None, "Watching the selected build set"),
        (
            "success",
            {"outcome": "failed", "observations": [{}]},
            None,
            "failed: 1 builds",
        ),
        (
            "success",
            {"outcome": "completed", "observations": []},
            None,
            "completed: 0 builds",
        ),
        (
            "failure",
            None,
            ValueError("empty selection"),
            "Monitoring could not finish: ValueError",
        ),
    ]:
        presentation = entry.present(
            phase=phase, args=(), kwargs={}, result=result, error=error
        )
        assert presentation.summary == expected
