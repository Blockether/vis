"""The gh view, proven against two real polls of one real GitHub Actions run.

`fixtures/run-mid.json` and `fixtures/run-final.json` are the same repository CI run captured twice
by `gh run view --json`, trimmed to six jobs and otherwise untouched — real ids, real timestamps,
one job that fails and two that are still running when the first poll lands.

`fixtures/ops.json` is what those two polls made this extension SAY to the engine, `view.json` the
picture the human was left with. `test/com/blockether/vis/internal/human_input/gh_live_test.clj`
replays that same `ops.json` through the engine's own live dispatch, so an op this extension emits
that the engine would refuse turns a Clojure test red rather than failing in front of a human.
"""

import json
import pathlib

import gh
import pytest
import vis

FIXTURES = pathlib.Path(__file__).parent / "fixtures"

# The view the goldens were captured under: the run's own workflow, title and event.
TITLE = "CI · run 32146686161"
DESCRIPTION = "docs(release): record the TestFlight build testers now have · push"
# The tail the goldens carry: the real log of the job that failed, cut where it failed.
TAIL_LINES = 6


def job_log():
    return gh.log_window((FIXTURES / "job-log.txt").read_text(), TAIL_LINES)


def fixture(name):
    return json.loads((FIXTURES / name).read_text())


class Recorder:
    """The host with every live envelope kept — what the extension SAID, verbatim.

    It delegates to the real host, so the view is opened, patched and closed by the same code
    that runs in a session; the recording is only a carbon copy of the wire.
    """

    def __init__(self, inner):
        self._inner = inner
        self.said = []

    def live(self, envelope_json):
        self.said.append(json.loads(envelope_json))
        return self._inner.live(envelope_json)

    def __getattr__(self, name):
        return getattr(self._inner, name)

    def ops(self):
        """The envelopes with the run-specific view id dropped: what a golden can hold."""
        return [{k: v for k, v in one.items() if k != "view_id"} for one in self.said]

    def patched(self):
        """Every patch op, in order, flattened out of the envelopes that carried them."""
        return [
            op
            for one in self.said
            if one.get("op") == "patch"
            for op in (one.get("patch") or {}).get("ops") or []
        ]


@pytest.fixture
def recorder(monkeypatch):
    kept = Recorder(vis._host)
    monkeypatch.setattr(vis, "_host", kept)
    monkeypatch.setattr(gh, "FAST_TICK_S", 0.0)
    monkeypatch.setattr(gh, "SLOW_TICK_S", 0.0)
    return kept


@pytest.fixture
def watched(recorder):
    """One whole watch: the mid poll, then the final one, through the real live view."""
    polls = [fixture("run-mid.json"), fixture("run-final.json")]
    verdict = gh.watch(
        TITLE,
        DESCRIPTION,
        lambda: polls.pop(0) if len(polls) > 1 else polls[0],
        log_tail=lambda job_id: job_log(),
    )
    return recorder, verdict


def node(view, node_id):
    return next(one for one in view["nodes"] if one["id"] == node_id)


def test_a_job_state_is_one_tone_everywhere():
    assert gh.tone_of("queued", "") == "running"
    assert gh.tone_of("in_progress", "") == "running"
    assert gh.tone_of("completed", "success") == "ok"
    assert gh.tone_of("completed", "skipped") == "idle"
    assert gh.tone_of("completed", "neutral") == "idle"
    assert gh.tone_of("completed", "failure") == "error"
    assert gh.tone_of("completed", "cancelled") == "error"
    assert gh.tone_of("completed", "timed_out") == "error"


def test_a_poll_reads_as_the_seven_answers():
    shape = gh.run_shape(fixture("run-mid.json"))

    assert shape["is_over"] is False
    assert shape["headline"] == "4 of 6 jobs finished, 1 failed"
    assert (shape["done"], shape["total"]) == (4, 6)
    assert shape["tone"] == "running"
    assert {one["id"]: one["value_text"] for one in shape["score"]} == {
        "passed": "3",
        "failed": "1",
        "skipped": "0",
        "queued": "2",
    }
    # A row is addressed by the job's databaseId, which is why it keeps its slot as it changes.
    assert [one["id"] for one in shape["rows"]] == [
        "95742028721",
        "95742028770",
        "95742028781",
        "95742028809",
        "95742028943",
        "95742029230",
    ]
    assert shape["rows"][1]["cells"] == [
        "tests / vis-agent + vis-contract (PyPI packages)",
        "failure",
        "12s",
    ]
    assert shape["rows"][1]["tone"] == "error"
    # A running job shows the job in focus; the elapsed column waits for its end.
    assert shape["focus"] == "tests / macos-latest"
    assert shape["rows"][0]["cells"][2] == "·"
    assert [one["id"] for one in shape["links"]] == ["run", "95742028770"]


def test_a_finished_run_focuses_the_job_that_failed():
    shape = gh.run_shape(fixture("run-final.json"))

    assert shape["is_over"] is True
    assert shape["tone"] == "error"
    assert shape["focus"] == "tests / vis-agent + vis-contract (PyPI packages)"
    assert shape["focus_id"] == "95742028770"
    assert [one["value_text"] for one in shape["score"]] == ["5", "1", "0", "0"]


def test_the_view_opens_declared_from_the_first_poll(watched):
    recorder, _ = watched
    opened = recorder.said[0]

    assert opened["op"] == "open"
    assert opened["view"]["title"] == TITLE
    assert [one["id"] for one in opened["view"]["nodes"]] == [
        "run",
        "progress",
        "score",
        "jobs",
        "failing",
        "output",
        "links",
    ]
    assert [one["type"] for one in opened["view"]["nodes"]] == [
        "status",
        "progress",
        "stat",
        "table",
        "steps",
        "log",
        "link",
    ]


def test_only_what_moved_since_the_last_poll_crosses_the_wire(watched):
    recorder, _ = watched
    rows = [op for op in recorder.patched() if op.get("node_id") == "jobs"]
    touched = {
        item["id"] for op in rows for item in (op.get("items") or op.get("rows") or [])
    }

    # Three jobs were already green in the first poll and never moved: nothing is said about them.
    assert touched == {"95742028721", "95742028781"}
    assert "95742028809" not in touched
    assert "95742028943" not in touched
    assert "95742029230" not in touched


def test_the_steps_start_over_when_the_job_in_focus_changes(watched):
    recorder, _ = watched
    failing = [op for op in recorder.patched() if op.get("node_id") == "failing"]

    # Focus moves from the running job to the one that failed, so the checklist is emptied
    # before the new job's steps land — a step of the old job must not linger under the new one.
    assert failing[0]["op"] == "clear"
    assert any(op["op"] != "clear" for op in failing[1:])


def test_the_log_window_ends_where_the_job_failed():
    window = gh.log_window((FIXTURES / "job-log.txt").read_text())

    # The last lines of a failing job are the runner cleaning up; the reason it failed is above
    # them, so the window ends at the error and the pane shows what led there.
    assert window[-1].endswith("##[error]Process completed with exit code 1.")
    assert "1 failed, 58 passed" in window[-2]
    assert not any("Terminate orphan process" in line for line in window)
    # gh repeats the job and the step name on every line: noise in a pane three columns wide.
    assert not any("\t" in line for line in window)
    assert all(line.startswith("2026-08-18T") for line in window)


def test_the_log_arrives_only_when_the_run_is_over(watched):
    recorder, verdict = watched
    lines = node(verdict["view"], "output")["lines"]

    assert lines == job_log()
    written = [op for op in recorder.patched() if op.get("node_id") == "output"]
    assert written[0]["lines"] == ["· GitHub serves a job's log when the run finishes"]
    assert any(op["op"] == "clear" for op in written)


def test_the_picture_is_the_one_the_human_watched(watched):
    _, verdict = watched

    assert verdict["is_completed"] is True
    assert verdict["summary"].startswith("6 of 6 jobs finished, 1 failed")
    assert verdict["view"] == fixture("view.json")


def test_the_ops_are_the_ones_the_engine_replays(watched):
    recorder, _ = watched

    assert recorder.ops() == fixture("ops.json")


def test_a_stop_answers_the_picture_the_human_left(recorder):
    class Stopping(Recorder):
        """A human pressing stop between two polls: the view ends under the loop's feet."""

        def live(self, envelope_json):
            envelope = json.loads(envelope_json)
            if envelope.get("op") == "patch" and not getattr(self, "stopped", False):
                self.stopped = True
                self._inner.live(
                    json.dumps(
                        {
                            "op": "close",
                            "view_id": envelope["view_id"],
                            "ending": {"reason": "interrupted"},
                        }
                    )
                )
            return super().live(envelope_json)

    stopping = Stopping(recorder)
    vis._host = stopping
    polls = [fixture("run-mid.json"), fixture("run-final.json")]
    verdict = gh.watch(
        TITLE, DESCRIPTION, lambda: polls.pop(0) if len(polls) > 1 else polls[0]
    )

    # Whatever ended it, the model is answered the same shape — here, the run half-done.
    assert verdict["reason"] == "interrupted"
    assert verdict["is_completed"] is False
    assert node(verdict["view"], "run")["text"] == "4 of 6 jobs finished, 1 failed"
    assert node(verdict["view"], "progress")["done"] == 4


def test_a_watch_has_no_clock_of_its_own(recorder, monkeypatch):
    # Regression: a watch stopped itself after 90 minutes it invented, so a longer CI run was
    # abandoned mid-flight and the model was told "still running" about a run it never saw end.
    hours = iter(range(1, 200))
    seen = []

    def a_slow_clock():
        seen.append(next(hours) * 3600.0)
        return seen[-1]

    monkeypatch.setattr(gh.time, "monotonic", a_slow_clock)
    polls = []

    def poll():
        polls.append(True)
        if len(polls) == 6:
            # The only thing that may end this: the person watching.
            vis._host.live(
                json.dumps(
                    {
                        "op": "close",
                        "view_id": next(
                            one["view_id"]
                            for one in recorder.said
                            if one.get("view_id")
                        ),
                        "ending": {"reason": "interrupted"},
                    }
                )
            )
        return fixture("run-mid.json")

    verdict = gh.watch(TITLE, DESCRIPTION, poll)

    # Hours passed with the run still going and nothing here counted them.
    assert seen[-1] - seen[0] > 3 * 3600
    assert len(polls) >= 6
    assert verdict["reason"] == "interrupted"
    assert verdict["is_completed"] is False


def test_a_missing_gh_refuses_in_one_line_before_a_view_opens(recorder, monkeypatch):
    monkeypatch.setattr(
        gh,
        "_capture",
        lambda command, seconds=120: (1, "You are not logged into any GitHub hosts.\n"),
    )

    with pytest.raises(gh.GhMissing) as refusal:
        gh.gh_watch_run(32146686161)

    assert "\n" not in str(refusal.value)
    assert "gh auth login" in str(refusal.value)
    assert recorder.said == []


def test_a_pull_requests_checks_read_as_the_same_run():
    rows = [
        {"name": "lint", "bucket": "pass", "link": "https://github.com/o/r/runs/1"},
        {"name": "tests", "bucket": "pending", "link": "https://github.com/o/r/runs/2"},
        {"name": "build", "bucket": "fail", "link": "https://github.com/o/r/runs/3"},
    ]

    shape = gh.run_shape(gh.checks_payload(rows, "1421"))

    # One mapping, two commands: a check is a job, and its bucket is a job's state.
    assert [one["tone"] for one in shape["rows"]] == ["ok", "running", "error"]
    assert [one["cells"][0] for one in shape["rows"]] == ["lint", "tests", "build"]
    assert shape["is_over"] is False
    assert shape["headline"] == "2 of 3 jobs finished, 1 failed"

    settled = gh.checks_payload([dict(one, bucket="pass") for one in rows], "1421")
    assert gh.run_shape(settled)["is_over"] is True
    assert gh.run_shape(settled)["tone"] == "ok"
