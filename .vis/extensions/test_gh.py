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


def failing_log(lines=TAIL_LINES):
    return gh.log_window((FIXTURES / "job-log.txt").read_text(), lines)


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
        envelope = json.loads(envelope_json)
        self.said.append(envelope)
        answer_json = self._inner.live(envelope_json)
        if envelope.get("op") == "open":
            self.view_id = str(json.loads(answer_json).get("view_id") or "")
        return answer_json

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
    monkeypatch.setattr(gh, "_wall_time", lambda: gh._timestamp("2026-08-18T14:20:47Z"))
    return kept


@pytest.fixture
def watched(recorder):
    """One whole watch: the mid poll, then the final one, through the real live view."""
    polls = [fixture("run-mid.json"), fixture("run-final.json")]
    asked = []

    def log_of(job_id, lines):
        asked.append((job_id, lines))
        return failing_log()

    verdict = gh.watch(
        TITLE,
        DESCRIPTION,
        lambda: polls.pop(0) if len(polls) > 1 else polls[0],
        log_of,
    )
    recorder.asked = asked
    return recorder, verdict


def node(view, node_id):
    return next(one for one in view["nodes"] if one["id"] == node_id)


def assert_tree(actual, expected, path="view"):
    """Compare a golden tree at the exact leaf that moved."""
    assert (path, type(expected)) == (path, type(actual))
    if isinstance(expected, dict):
        assert (path, sorted(expected)) == (path, sorted(actual))
        for key, value in expected.items():
            assert_tree(actual[key], value, f"{path}.{key}")
    elif isinstance(expected, list):
        assert (path, len(expected)) == (path, len(actual))
        for index, value in enumerate(expected):
            assert_tree(actual[index], value, f"{path}[{index}]")
    else:
        assert (path, expected) == (path, actual)


def test_a_job_state_is_one_tone_everywhere():
    assert gh.tone_of("queued", "") == "running"
    assert gh.tone_of("in_progress", "") == "running"
    assert gh.tone_of("completed", "success") == "ok"
    assert gh.tone_of("completed", "skipped") == "idle"
    assert gh.tone_of("completed", "neutral") == "idle"
    assert gh.tone_of("completed", "failure") == "error"
    assert gh.tone_of("completed", "cancelled") == "error"
    assert gh.tone_of("completed", "timed_out") == "error"


def test_a_poll_reads_as_the_eight_answers():
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
    # Every concurrently running job is focused; the elapsed column waits for its end.
    assert shape["focus_ids"] == ["95742028721", "95742028781"]
    assert shape["focus"] == "tests / macos-latest + tests / ubuntu-latest"
    assert shape["rows"][0]["cells"][2] == "·"
    assert [one["id"] for one in shape["links"]] == ["run", "95742028770"]


def test_parallel_jobs_are_all_focused_and_an_explicit_focus_wins():
    mid = fixture("run-mid.json")
    shape = gh.run_shape(mid)

    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: the table looked
    # selectable but always showed the first running job's details and log.
    assert shape["focus_ids"] == ["95742028721", "95742028781"]
    selected = gh.run_shape(mid, focus_ids=["95742028770"])
    assert selected["focus_ids"] == ["95742028770"]
    assert selected["focus"] == "tests / vis-agent + vis-contract (PyPI packages)"


def test_a_green_run_defaults_to_its_last_job():
    payload = fixture("run-final.json")
    payload["conclusion"] = "success"
    for job in payload["jobs"]:
        job["status"] = "completed"
        job["conclusion"] = "success"

    shape = gh.run_shape(payload)

    assert shape["focus_ids"] == [str(payload["jobs"][-1]["databaseId"])]
    assert shape["focus"] == payload["jobs"][-1]["name"]


def test_a_finished_run_focuses_the_job_that_failed():
    shape = gh.run_shape(fixture("run-final.json"))

    assert shape["is_over"] is True
    assert shape["tone"] == "error"
    assert shape["focus"] == "tests / vis-agent + vis-contract (PyPI packages)"
    assert shape["focus_ids"] == ["95742028770"]
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
        "steps",
        "activity",
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
        "log",
        "link",
    ]
    jobs = node(opened["view"], "jobs")
    assert jobs["is_focusable"] is True
    assert jobs["focused_ids"] == ["95742028721", "95742028781"]


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
    steps = [op for op in recorder.patched() if op.get("node_id") == "steps"]

    # Focus moves from the running job to the one that failed, so the checklist is emptied
    # before the new job's steps land — a step of the old job must not linger under the new one.
    assert steps[0]["op"] == "clear"
    assert any(op["op"] != "clear" for op in steps[1:])


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


def test_running_focus_shows_current_step_before_raw_logs_exist():
    payload = fixture("run-mid.json")
    shape = gh.run_shape(
        payload,
        focus_ids=["95742028721"],
        now=gh._timestamp("2026-08-18T14:20:47Z"),
    )
    later = gh.run_shape(
        payload,
        focus_ids=["95742028721"],
        now=gh._timestamp("2026-08-18T14:20:50Z"),
    )
    lines = gh._focus_log_lines(shape, None, {}, gh.FAILED_TAIL_LINES)

    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: a running job left both
    # activity panes static while GitHub withheld its raw log until the job completed.
    assert shape["rows"][0]["cells"][2] == "10m 34s"
    assert gh.active_lines(shape) == [
        "▶ tests / macos-latest · Run test suite · 10m 00s"
    ]
    assert lines == [
        "── tests / macos-latest · live progress",
        "▶ Run test suite · 10m 00s",
        "· GitHub publishes the raw job log when this job ends",
    ]
    assert later["rows"][0]["cells"][2] == "10m 37s"
    assert gh.active_lines(later) == [
        "▶ tests / macos-latest · Run test suite · 10m 03s"
    ]
    assert gh._focus_signature(shape) != gh._focus_signature(later)


def test_running_focus_waits_while_a_failed_log_stays_visible_in_activity(watched):
    recorder, _ = watched
    output = [op for op in recorder.patched() if op.get("node_id") == "output"]
    activity = [op for op in recorder.patched() if op.get("node_id") == "activity"]

    # A running matrix starts with every running job selected. Before GitHub publishes raw logs,
    # both panes identify the exact work underway and the current step keeps a live timer.
    assert output[0]["lines"] == [
        "── tests / macos-latest · live progress",
        "▶ Run test suite · 10m 00s",
        "· GitHub publishes the raw job log when this job ends",
        "── tests / ubuntu-latest · live progress",
        "▶ Waiting for this job to start",
        "· GitHub publishes the raw job log when this job ends",
    ]
    assert activity[0]["lines"][:2] == [
        "▶ tests / macos-latest · Run test suite · 10m 00s",
        "▶ tests / ubuntu-latest · waiting to start",
    ]
    activity_lines = [line for op in activity for line in op.get("lines", [])]
    # Preserve the original live-watch promise: a job that fails is filed immediately,
    # even while the focused running jobs have no logs of their own yet.
    assert (
        "── tests / vis-agent + vis-contract (PyPI packages) · failed log"
        in activity_lines
    )
    assert any("1 failed, 58 passed" in line for line in activity_lines)
    assert any("macos-latest · success" in line for line in activity_lines)
    assert recorder.asked == [
        ("95742028770", gh.FAILED_TAIL_LINES),
        ("95742028770", gh.LOG_TAIL_LINES),
    ]


def test_the_feed_says_what_moved_between_two_polls():
    lines = gh.feed_lines(
        gh.run_shape(fixture("run-mid.json")), gh.run_shape(fixture("run-final.json"))
    )

    # Only the two jobs that were still running and are now over: the four that did not move say
    # nothing, and no step of the new job in focus dumps a checklist the `steps` node paints.
    assert lines == [
        "✓ tests / macos-latest · success · 28m 33s",
        "✓ tests / ubuntu-latest · success · 12m 54s",
    ]


def test_the_settled_pane_is_one_photograph(watched):
    recorder, verdict = watched
    lines = node(verdict["view"], "output")["lines"]
    written = [op for op in recorder.patched() if op.get("node_id") == "output"]

    # The feed is the story WHILE it runs; what is LEFT is the log of the job that has to be
    # acted on, asked for at the model's own budget. The record still holds every line.
    assert any(op["op"] == "clear" for op in written)
    assert lines[0] == "── tests / vis-agent + vis-contract (PyPI packages) · log"
    assert lines[1:] == failing_log()
    assert recorder.asked[-1] == ("95742028770", gh.LOG_TAIL_LINES)


def test_a_log_is_asked_of_the_job_not_of_the_run(monkeypatch):
    asked = []

    def capture(command, seconds=120):
        asked.append(command)
        return 0, (FIXTURES / "job-log.txt").read_text()

    monkeypatch.setattr(gh, "_capture", capture)
    tail = gh.job_log(gh.repo_of(fixture("run-mid.json")), "95742028770", TAIL_LINES)

    assert asked == ["gh api repos/Blockether/vis/actions/jobs/95742028770/logs"]
    assert tail == failing_log()
    # A run names its own repository, and nothing is asked without one.
    assert gh.repo_of(fixture("run-mid.json")) == "Blockether/vis"
    assert (
        gh.repo_of({"url": "https://github.com/o/r/actions/runs/1"}, "other/name")
        == "other/name"
    )
    assert gh.job_log("", "95742028770") is None
    assert gh.job_log("Blockether/vis", "") is None


def test_a_human_focus_is_read_back_and_kept_across_the_next_poll(recorder):
    polls = [fixture("run-mid.json"), fixture("run-final.json")]
    selected = "95742028809"
    asked = []
    clicked = False

    def press_row():
        nonlocal clicked
        if clicked:
            return
        clicked = True
        view_id = recorder.view_id
        # The surface action reaches the host directly; Recorder only captures what
        # the extension says, so this remains a real external focus change.
        recorder._inner.live(
            json.dumps(
                {
                    "op": "patch",
                    "view_id": view_id,
                    "patch": {
                        "ops": [
                            {"op": "set", "node_id": "jobs", "focused_ids": [selected]}
                        ]
                    },
                }
            )
        )

    def poll():
        # Click WHILE the second provider poll is in flight. Reading focus before
        # that poll used to overwrite this choice with the extension's default.
        if len(polls) == 1:
            press_row()
        return polls.pop(0) if len(polls) > 1 else polls[0]

    def log_of(job_id, lines):
        asked.append((job_id, lines))
        return [f"log for {job_id}"]

    verdict = gh.watch(
        TITLE,
        DESCRIPTION,
        poll,
        log_of,
    )

    focus_ops = [
        op
        for op in recorder.patched()
        if op.get("node_id") == "jobs" and "focused_ids" in op
    ]
    assert focus_ops[-1]["focused_ids"] == [selected]
    assert node(verdict["view"], "output")["lines"] == [
        "── lint / clj-kondo · log",
        f"log for {selected}",
    ]
    assert asked == [
        ("95742028770", gh.FAILED_TAIL_LINES),
        (selected, gh.LOG_TAIL_LINES),
    ]


def test_the_picture_is_the_one_the_human_watched(watched):
    _, verdict = watched

    assert verdict["is_completed"] is True
    assert verdict["summary"].startswith("6 of 6 jobs finished, 1 failed")
    assert_tree(verdict["view"], fixture("view.json"))


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


def test_a_newer_commit_supersedes_the_implicit_run_watch(recorder):
    polls = []
    newer = {
        "databaseId": 32146699999,
        "url": "https://github.com/Blockether/vis/actions/runs/32146699999",
        "displayTitle": "fix(tui): replace the watched commit",
    }

    def poll():
        polls.append(True)
        return fixture("run-mid.json")

    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: when a later push
    # overtook a running CI run, the live view kept polling obsolete work indefinitely.
    verdict = gh.watch(TITLE, DESCRIPTION, poll, superseded_by=lambda: newer)

    assert len(polls) == 1
    assert verdict["is_completed"] is True
    assert verdict["summary"].startswith("Superseded by run 32146699999")
    assert node(verdict["view"], "run")["text"] == "Superseded by newer run 32146699999"
    assert node(verdict["view"], "run")["tone"] == "idle"
    assert node(verdict["view"], "activity")["lines"][0] == (
        "– Stopped: newer run 32146699999 started for this workflow"
    )
    assert node(verdict["view"], "links")["links"][-1]["id"] == "newer-run"


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


def test_a_newer_run_is_matched_to_the_same_workflow_branch_and_event(monkeypatch):
    payload = fixture("run-mid.json")
    payload["databaseId"] = 32146686161
    asked = []

    def capture(command, seconds=120):
        asked.append(command)
        return 0, json.dumps(
            [
                {
                    "databaseId": 32146699999,
                    "url": "https://github.com/Blockether/vis/actions/runs/32146699999",
                    "displayTitle": "newer push",
                }
            ]
        )

    monkeypatch.setattr(gh, "_capture", capture)

    assert gh.newer_run(payload)["databaseId"] == 32146699999
    assert asked == [
        "gh run list --workflow CI --branch main --event push -L 1 "
        "--json databaseId,url,displayTitle"
    ]


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
