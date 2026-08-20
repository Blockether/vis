"""Reusable extension live-view test host."""

import json

import vis


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
                                "is_focusable": True,
                                "focused_ids": ["linux"],
                                "rows": [{"id": "linux", "cells": ["Linux"]}],
                            }
                        ],
                    },
                }
            )
        )
    )

    recorder.focus("jobs", ["macos"])
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

    assert recorder.node("jobs")["focused_ids"] == ["macos"]
    assert state["view"]["nodes"][0]["focused_ids"] == ["macos"]
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
