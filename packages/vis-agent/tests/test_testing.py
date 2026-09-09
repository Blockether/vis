"""Reusable extension live-view test host."""

import json

import blockether.vis.extension as vis
import pytest


def test_live_recorder_isolates_extension_output_from_surface_actions():
    # Generic harness contract: extension envelopes are recorded; simulated human
    # actions alter the same materialized state without pretending the extension said them.
    LiveRecorder = vis.testing.LiveRecorder

    recorder = LiveRecorder(vis._host)
    opened = json.loads(
        recorder.live(
            json.dumps(
                {
                    "op": "open",
                    "view": {
                        "title": "Build",
                        "nodes": [
                            {
                                "id": "jobs",
                                "type": "table",
                                "label": "Jobs",
                                "columns": ["Job"],
                                "is_selectable": True,
                                "selected_ids": ["linux"],
                                "rows": [{"id": "linux", "cells": ["Linux"]}],
                            }
                        ],
                    },
                }
            )
        )
    )

    recorder.select("jobs", ["macos"])
    state = json.loads(
        recorder.host_live(
            json.dumps(
                {
                    "op": "state",
                    "view_id": opened["view_id"],
                }
            )
        )
    )

    assert recorder.node("jobs")["selected_ids"] == ["macos"]
    assert state["view"]["nodes"][0]["selected_ids"] == ["macos"]
    assert len(recorder.said) == 1
    assert recorder.ops()[0]["op"] == "open"


def test_live_recorder_returns_the_terminal_materialized_picture():
    LiveRecorder = vis.testing.LiveRecorder
    assert_tree = vis.testing.assert_tree

    recorder = LiveRecorder(vis._host)
    opened = json.loads(
        recorder.live(
            json.dumps(
                {
                    "op": "open",
                    "view": {
                        "title": "Logs",
                        "nodes": [
                            {"id": "log", "type": "log", "label": "Log", "lines": []}
                        ],
                    },
                }
            )
        )
    )
    recorder.host_live(
        json.dumps(
            {
                "op": "patch",
                "view_id": opened["view_id"],
                "patch": {
                    "ops": [{"op": "append", "node_id": "log", "lines": ["done"]}]
                },
            }
        )
    )
    result = recorder.close(reason="interrupted", summary="Stopped")

    assert result["reason"] == "interrupted"
    assert result["is_completed"] is False
    assert_tree(result["view"]["nodes"][0]["lines"], ["done"], path="view.log.lines")


def test_live_recorder_starts_a_cleared_log_record_over():
    LiveRecorder = vis.testing.LiveRecorder

    recorder = LiveRecorder(vis._host)
    opened = json.loads(
        recorder.live(
            json.dumps(
                {
                    "op": "open",
                    "view": {
                        "title": "Logs",
                        "nodes": [
                            {"id": "log", "type": "log", "label": "Log", "lines": []}
                        ],
                    },
                }
            )
        )
    )

    def patch(*ops):
        recorder.host_live(
            json.dumps(
                {
                    "op": "patch",
                    "view_id": opened["view_id"],
                    "patch": {"ops": list(ops)},
                }
            )
        )

    patch({"op": "append", "node_id": "log", "lines": ["one", "two"]})
    assert recorder.node("log")["total_lines"] == 2

    patch({"op": "clear", "node_id": "log"})

    # `live/apply-clear`: the RECORD starts over with the window, so a pane rewritten in
    # place never claims earlier lines the gateway's record reader cannot serve.
    assert recorder.node("log")["lines"] == []
    assert recorder.node("log")["total_lines"] == 0

    patch({"op": "append", "node_id": "log", "lines": ["again"]})
    assert recorder.node("log")["total_lines"] == 1


def test_live_view_documentation_renders_terminal_jobs(monkeypatch):
    # The example previously omitted conclusion from the query and skipped final rows.
    import re
    from pathlib import Path
    from types import SimpleNamespace

    document = Path(__file__).parents[3] / "resources/vis-docs/live-views.md"
    example = re.search(
        r"```python\n(.*?)\n```", document.read_text(), re.DOTALL
    ).group(1)
    namespace = {}
    exec(compile(example, str(document), "exec"), namespace)
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    requests = []
    polled_at = []
    now = [0.0]
    waits = []
    replies = iter(
        [
            {
                "status": "in_progress",
                "conclusion": "",
                "url": "https://github.com/example/repo/actions/runs/1",
                "jobs": [
                    {
                        "databaseId": 1,
                        "name": "Tests",
                        "status": "in_progress",
                        "conclusion": "",
                    }
                ],
            },
            {
                "status": "completed",
                "conclusion": "success",
                "url": "https://github.com/example/repo/actions/runs/1",
                "jobs": [
                    {
                        "databaseId": 1,
                        "name": "Tests",
                        "status": "completed",
                        "conclusion": "success",
                    }
                ],
            },
        ]
    )

    def shell(request):
        requests.append(request)
        polled_at.append(now[0])
        fields = request["command"].split("--json ")[1].split(",")
        snapshot = next(replies)
        return SimpleNamespace(
            wait=lambda _: {"out": json.dumps({key: snapshot[key] for key in fields})}
        )

    def sleep(view, seconds):
        # A local event wakes early; the next GitHub request still waits five seconds.
        waits.append(seconds)
        now[0] += 0.1 if len(waits) == 1 else seconds
        return len(waits) == 1

    monkeypatch.setattr(vis, "shell", shell)
    monkeypatch.setattr(namespace["time"], "monotonic", lambda: now[0])
    monkeypatch.setattr(vis.LiveView, "sleep", sleep)
    result = namespace["watch_run"](1)
    assert len(requests) == 2
    assert polled_at == [0.0, 5.0]
    assert len(waits) == 2
    assert result["summary"] == "Run result: success"
    assert recorder.node("jobs")["rows"][0]["cells"] == ["Tests", "success"]
    assert recorder.node("progress")["done"] == 1


@pytest.mark.parametrize("counts", [(0, 0), (0, 1, 0, 2)])
def test_live_view_documentation_handles_queued_and_growing_job_lists(
    monkeypatch, counts
):
    # #180: the documented example must accept no jobs, later additions and empty polls.
    import re
    from pathlib import Path

    document = Path(__file__).parents[3] / "resources/vis-docs/live-views.md"
    example = re.search(r"```python\n(.*?)\n```", document.read_text(), re.S)[1]
    namespace = {}
    exec(compile(example, str(document), "exec"), namespace)
    now = [0.0]
    monkeypatch.setattr(namespace["time"], "monotonic", lambda: now[0])

    def sleep(view, seconds):
        now[0] += seconds
        return False

    monkeypatch.setattr(vis.LiveView, "sleep", sleep)
    recorder = vis.testing.LiveRecorder(vis._host)
    monkeypatch.setattr(vis, "_host", recorder)
    snapshots = iter(
        {
            "status": "completed" if index == len(counts) - 1 else "in_progress",
            "conclusion": "success" if index == len(counts) - 1 else "",
            "url": "https://github.com/example/repo/actions/runs/1",
            "jobs": [
                {
                    "databaseId": job,
                    "name": f"Job {job}",
                    "status": "completed",
                    "conclusion": "success",
                }
                for job in range(count)
            ],
        }
        for index, count in enumerate(counts)
    )
    namespace["poll"] = lambda _: next(snapshots)
    result = namespace["watch_run"](1)
    assert result["summary"] == "Run result: success"
    progress = recorder.node("progress")
    if counts[-1]:
        assert progress["done"] == progress["total"] == counts[-1]
    else:
        assert progress.get("total") is None
    assert all(
        operation.get("total") != 0
        for operation in recorder.patched()
        if operation.get("node_id") == "progress"
    )
