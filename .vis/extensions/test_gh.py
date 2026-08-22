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


Recorder = vis.testing.LiveRecorder
assert_tree = vis.testing.assert_tree


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


def test_a_job_state_is_one_tone_everywhere():
    assert gh.tone_of("queued", "") == "running"
    assert gh.tone_of("in_progress", "") == "running"
    assert gh.tone_of("completed", "success") == "ok"
    assert gh.tone_of("completed", "skipped") == "idle"
    assert gh.tone_of("completed", "neutral") == "idle"
    assert gh.tone_of("completed", "failure") == "error"
    assert gh.tone_of("completed", "cancelled") == "error"
    assert gh.tone_of("completed", "timed_out") == "error"
    assert gh.tone_of("completed", "action_required") == "error"
    assert gh.tone_of("completed", "startup_failure") == "error"
    assert gh.tone_of("completed", "stale") == "error"
    assert gh.tone_of("mystery_pending_state", "") == "running"


def test_a_poll_reads_as_the_seven_answers():
    shape = gh.run_shape(fixture("run-mid.json"))

    assert shape["is_over"] is False
    assert shape["headline"] == "4 of 6 jobs finished, 1 failed"
    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: a live run showed
    # progress and elapsed durations but not the calendar date and time it began.
    assert shape["detail"] == (
        "workflow **CI** on `main` · started **18 Aug 2026, 14:10 UTC** · "
        "in focus **2 jobs**"
    )
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
    # Matrix variants share one semantic parent instead of repeating that parent as flat peers.
    assert shape["rows"][0]["branch"] == "tests"
    assert shape["rows"][1]["branch"] == "tests"
    assert "branch" not in next(
        row for row in shape["rows"] if row["cells"][0] == "lint / clj-kondo"
    )
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

    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: GitHub exposed both selected
    # job details and a second run-wide activity feed that repeated the table and steps.
    assert opened["op"] == "open"
    assert opened["view"]["title"] == TITLE
    assert [one["id"] for one in opened["view"]["nodes"]] == [
        "run",
        "progress",
        "score",
        "jobs",
        "steps",
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

    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: a running job left its
    # focused details static while GitHub withheld the raw log until the job completed.
    assert shape["rows"][0]["cells"][2] == "10m 34s"
    assert lines == [
        "── tests / macos-latest · live progress",
        "▶ Run test suite · 10m 00s",
        "· GitHub publishes the raw job log when this job ends",
    ]
    assert later["rows"][0]["cells"][2] == "10m 37s"
    assert gh._focus_signature(shape) != gh._focus_signature(later)


def test_running_focus_waits_while_finished_focus_gets_its_log(watched):
    recorder, _ = watched
    output = [op for op in recorder.patched() if op.get("node_id") == "output"]

    assert output[0]["lines"] == [
        "── tests / macos-latest · live progress",
        "▶ Run test suite · 10m 00s",
        "· GitHub publishes the raw job log when this job ends",
        "── tests / ubuntu-latest · live progress",
        "▶ Waiting for this job to start",
        "· GitHub publishes the raw job log when this job ends",
    ]
    assert recorder.asked[0] == ("95742028770", gh.LOG_TAIL_LINES)
    assert {job_id for job_id, _lines in recorder.asked} == {
        str(job["databaseId"]) for job in fixture("run-final.json")["jobs"]
    }


def test_the_settled_pane_is_one_photograph(watched):
    recorder, result = watched
    lines = node(recorder.picture(), "output")["lines"]
    written = [op for op in recorder.patched() if op.get("node_id") == "output"]

    # The feed is the story WHILE it runs; what is LEFT is the log of the job that has to be
    # acted on. The model receives only the compact semantic ending below.
    assert any(op["op"] == "clear" for op in written)
    assert lines[0] == "── tests / vis-agent + vis-contract (PyPI packages) · log"
    assert lines[1:] == failing_log()
    assert json.loads(result)["run"]["conclusion"] == "failure"
    assert ("95742028770", gh.LOG_TAIL_LINES) in recorder.asked
    snapshots = recorder.said[-1]["ending"]["focus_snapshots"]
    assert [one["focused_ids"] for one in snapshots] == [
        [str(job["databaseId"])] for job in fixture("run-final.json")["jobs"]
    ]
    assert node(snapshots[0]["view"], "jobs")["focused_ids"] == ["95742028721"]
    assert node(snapshots[-1]["view"], "output")["lines"][0].endswith(" · log")


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


def test_an_unpublished_log_is_not_cached_as_if_it_were_final():
    answers = iter([None, ["the log arrived"]])
    cache = {}

    assert gh._job_log_tail("42", 10, lambda _job, _lines: next(answers), cache) is None
    assert gh._job_log_tail("42", 10, lambda _job, _lines: next(answers), cache) == [
        "the log arrived"
    ]


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
        # The generic harness applies this as a surface action, so it changes shared
        # state without pretending the extension emitted the focus patch.
        recorder.focus("jobs", [selected])

    def poll():
        # Click WHILE the second provider poll is in flight. Reading focus before
        # that poll used to overwrite this choice with the extension's default.
        if len(polls) == 1:
            press_row()
        return polls.pop(0) if len(polls) > 1 else polls[0]

    def log_of(job_id, lines):
        asked.append((job_id, lines))
        return [f"log for {job_id}"]

    result = gh.watch(
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
    assert json.loads(result)["run"]["conclusion"] == "failure"
    assert node(recorder.picture(), "output")["lines"] == [
        "── lint / clj-kondo · log",
        f"log for {selected}",
    ]
    assert asked[0] == (selected, gh.LOG_TAIL_LINES)
    assert {job_id for job_id, _lines in asked} == {
        str(job["databaseId"]) for job in fixture("run-final.json")["jobs"]
    }


def test_a_focus_change_refreshes_details_even_while_github_is_unavailable(recorder):
    """A local selection is live state; a failed provider poll must not freeze its details."""
    first = fixture("run-mid.json")
    selected = "95742028770"
    calls = 0

    def poll():
        nonlocal calls
        calls += 1
        if calls == 1:
            return first
        if calls == 2:
            recorder.focus("jobs", [selected])
        raise RuntimeError("GitHub unavailable in regression fixture")

    result = gh.watch(
        TITLE,
        DESCRIPTION,
        poll,
        lambda job_id, _lines: [f"log for {job_id}"],
    )

    assert recorder.node("jobs")["focused_ids"] == [selected]
    assert json.loads(result)["ending"]["reason"] == "poll_failure"
    assert node(recorder.picture(), "steps")["steps"][0]["label"] == "Set up job"
    assert node(recorder.picture(), "output")["lines"] == [
        "── tests / vis-agent + vis-contract (PyPI packages) · log",
        f"log for {selected}",
    ]


def test_the_model_gets_one_deduplicated_diagnostic_string(watched):
    recorder, result = watched
    payload = fixture("run-final.json")

    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: the compact result omitted
    # the job inventory, stable ids, timing, steps, and the failed job's actionable error.
    assert isinstance(result, str)
    report = json.loads(result)
    assert report["run"] == {
        "id": 32146686161,
        "workflow": "CI",
        "branch": "main",
        "status": "completed",
        "conclusion": "failure",
        "url": "https://github.com/Blockether/vis/actions/runs/32146686161",
    }
    assert report["job_fields"] == [
        "id",
        "name",
        "conclusion",
        "started_at",
        "completed_at",
        "steps",
    ]
    assert report["step_fields"] == ["id", "conclusion", "name"]
    assert report["jobs"] == [
        [
            job["databaseId"],
            job["name"],
            job["conclusion"],
            job["startedAt"],
            job["completedAt"],
            [
                [step["number"], step["conclusion"], step["name"]]
                for step in job["steps"]
            ],
        ]
        for job in payload["jobs"]
    ]
    assert list(report["failed_logs"]) == ["95742028770"]
    failed_log = report["failed_logs"]["95742028770"]
    assert sum("1 failed, 58 passed in 0.72s" in line for line in failed_log) == 1
    assert (
        sum(
            "##[error]Process completed with exit code 1." in line
            for line in failed_log
        )
        == 1
    )
    assert not any("Terminate orphan process" in line for line in failed_log)
    assert result == json.dumps(report, ensure_ascii=False, separators=(",", ":"))
    assert_tree(recorder.picture(), fixture("view.json"))


def test_the_ops_are_the_ones_the_engine_replays(watched):
    recorder, _ = watched

    actual = recorder.ops()
    # Archive-only focus pictures do not change the live operation golden.
    actual[-1]["ending"].pop("focus_snapshots")
    assert actual == fixture("ops.json")


def test_a_stop_answers_the_picture_the_human_left(recorder):
    class Stopping(Recorder):
        """A human pressing stop between two polls: the view ends under the loop's feet."""

        def live(self, envelope_json):
            envelope = json.loads(envelope_json)
            if envelope.get("op") == "patch" and not getattr(self, "stopped", False):
                self.stopped = True
                self.close(reason="interrupted")
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
    # overtook a running CI run, the settled record said "finished" while its rows
    # remained queued/in progress forever. Close it as superseded and make every
    # unfinished row explicitly superseded instead of preserving a stale action state.
    result = gh.watch(TITLE, DESCRIPTION, poll, superseded_by=lambda: newer)

    assert len(polls) == 1
    assert json.loads(result)["ending"]["replacement_run_id"] == 32146699999
    picture = recorder.picture()
    assert node(picture, "run")["text"] == "Superseded by newer run 32146699999"
    assert node(picture, "run")["tone"] == "idle"
    rows = node(picture, "jobs")["rows"]
    assert [
        row["cells"][1] for row in rows if row["id"] in {"95742028721", "95742028781"}
    ] == [
        "superseded",
        "superseded",
    ]
    assert all(row["tone"] != "running" for row in rows)
    assert node(picture, "score")["stats"][-1] == {
        "id": "queued",
        "value_text": "0",
        "label": "queued",
        "tone": "idle",
    }
    assert node(picture, "links")["links"][-1]["id"] == "newer-run"


def test_a_transient_github_failure_keeps_the_watch_alive(recorder):
    polls = [
        fixture("run-mid.json"),
        RuntimeError("gh run view failed: HTTP 502"),
        RuntimeError("gh run view failed: rate limit temporarily exceeded"),
        fixture("run-final.json"),
    ]

    def poll():
        answer = polls.pop(0)
        if isinstance(answer, Exception):
            raise answer
        return answer

    # Regression: one temporary CLI, network, or API failure ended the view while the run
    # was still alive. GitHub outages are state too: retain the last picture and retry.
    result = gh.watch(TITLE, DESCRIPTION, poll)

    assert json.loads(result)["run"]["conclusion"] == "failure"
    assert not polls
    picture = recorder.picture()
    assert all(one["id"] != "activity" for one in picture["nodes"])
    assert node(picture, "run")["text"].startswith("6 of 6 jobs finished")


def test_a_permanently_unavailable_run_stops_after_bounded_retries(recorder):
    attempts = []

    def poll():
        attempts.append(True)
        if len(attempts) == 1:
            return fixture("run-mid.json")
        raise RuntimeError("gh run view failed: run was deleted")

    result = gh.watch(TITLE, DESCRIPTION, poll)

    assert len(attempts) == gh.MAX_CONSECUTIVE_POLL_FAILURES + 1
    assert json.loads(result)["ending"]["reason"] == "poll_failure"
    assert node(recorder.picture(), "run")["tone"] == "error"


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
            recorder.close(reason="interrupted")
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


def test_an_explicit_running_run_still_yields_to_its_replacement(monkeypatch):
    # Regression, session 89177777-0681-498d-8b26-b6d59ea67d75: the release watcher
    # kept a cancelled run open for hours because passing its id disabled replacement checks.
    first = fixture("run-mid.json")
    first["databaseId"] = 32146686161
    replacement = {"databaseId": 32146699999}
    received = {}

    monkeypatch.setattr(gh, "require_gh", lambda: None)
    monkeypatch.setattr(gh, "fetch_run", lambda run_id, repo=None: first)
    monkeypatch.setattr(gh, "repo_of", lambda payload, repo=None: "Blockether/vis")
    monkeypatch.setattr(gh, "job_log", lambda owner, job_id, lines: [])
    monkeypatch.setattr(gh, "newer_run", lambda payload, repo=None: replacement)

    def capture_watch(title, description, poll, log_of=None, superseded_by=None):
        received["superseded_by"] = superseded_by
        return "watched"

    monkeypatch.setattr(gh, "watch", capture_watch)

    assert gh.gh_watch_run(32146686161) == "watched"
    assert received["superseded_by"]() == replacement


class _LoginProcess(dict):
    def __init__(self, output):
        super().__init__(status="running", exit=None, out="")
        self.output = output
        self.sent = []
        self.stopped = False

    def logs(self, offset=None, limit=None):
        return {"status": "running", "exit": None, "out": self.output}

    def type(self, text, is_enter=True):
        self.sent.append((text, is_enter))
        return self

    def wait(self, seconds=120):
        return {"status": "exited", "exit": 0, "out": "Authentication complete."}

    def stop(self):
        self.stopped = True
        return {"status": "stopped", "exit": 143, "out": ""}


def test_device_login_keeps_the_oauth_code_inside_human_input(monkeypatch):
    commands = []
    started = []
    asks = []
    status_checks = 0

    process = _LoginProcess(
        "! First copy your one-time code: ABCD-EFGH\n"
        "Press Enter to open https://github.com/login/device in your browser...\n"
    )

    def capture(command, seconds=120):
        nonlocal status_checks
        commands.append(command)
        if command == "gh --version":
            return 0, "gh version 2"
        if command == "gh auth status --hostname github.com":
            status_checks += 1
            if status_checks == 1:
                return 1, "You are not logged into any GitHub hosts."
            return 0, "Logged in to github.com account operator"
        raise AssertionError(command)

    def ask(title, fields, **options):
        asks.append((title, fields, options))
        return {"authorized": True}

    def start(options):
        started.append(options)
        return process

    monkeypatch.setattr(gh, "_capture", capture)
    monkeypatch.setattr(gh.vis, "shell", start)
    monkeypatch.setattr(gh.vis, "ask", ask)

    result = gh.gh_login()

    assert result == "GitHub CLI authenticated with github.com."
    assert commands == [
        "gh auth status --hostname github.com",
        "gh --version",
        "gh auth status --hostname github.com",
    ]
    assert started[0]["op"] == "background"
    assert started[0]["id"].startswith("gh-login-")
    assert started[0]["command"] == (
        "GH_BROWSER=true gh auth login --hostname github.com "
        "--git-protocol https --web --skip-ssh-key"
    )
    assert process.sent == [("", True)]
    assert len(asks) == 1
    title, fields, options = asks[0]
    rendered = json.dumps(fields)
    assert title == "Sign in to GitHub"
    assert "https://github.com/login/device" in rendered
    assert "ABCD-EFGH" in rendered
    assert "**" not in rendered
    assert "[github.com](" not in rendered
    assert options == {
        "description": "Authorize GitHub CLI without sharing credentials with the model.",
        "submit_label": "I authorized GitHub",
        "cancel_label": "Cancel sign-in",
        "timeout_ms": 0,
    }
    assert "ABCD-EFGH" not in result
    assert process.stopped is False


def test_login_returns_without_hitl_when_the_cli_is_already_authenticated(monkeypatch):
    monkeypatch.setattr(gh, "_capture", lambda command, seconds=120: (0, "signed in"))
    monkeypatch.setattr(
        gh.vis,
        "ask",
        lambda *args, **kwargs: (_ for _ in ()).throw(
            AssertionError("unexpected HITL")
        ),
    )

    assert gh.gh_login() == "GitHub CLI is already authenticated with github.com."
    with pytest.raises(ValueError, match="host name"):
        gh.gh_login("github.com; echo unsafe")


def test_cancelling_device_login_stops_the_waiting_process(monkeypatch):
    class Cancelled:
        reason = "cancelled"

        def __bool__(self):
            return False

    process = _LoginProcess("Code WXYZ-1234 https://github.com/login/device")
    monkeypatch.setattr(
        gh,
        "_capture",
        lambda command, seconds=120: (
            (0, "gh version 2") if command == "gh --version" else (1, "signed out")
        ),
    )
    monkeypatch.setattr(gh.vis, "shell", lambda options: process)
    monkeypatch.setattr(gh.vis, "ask", lambda *args, **kwargs: Cancelled())

    with pytest.raises(gh.GhMissing, match="cancelled by the human"):
        gh.gh_login()

    assert process.stopped is True


def test_missing_auth_uses_login_hitl_before_a_view_opens(recorder, monkeypatch):
    called = []

    def login():
        called.append(True)
        raise gh.GhMissing("GitHub authentication was cancelled by the human")

    monkeypatch.setattr(gh, "gh_login", login)

    with pytest.raises(gh.GhMissing) as refusal:
        gh.gh_watch_run(32146686161)

    assert "\n" not in str(refusal.value)
    assert "cancelled by the human" in str(refusal.value)
    assert called == [True]
    assert recorder.said == []


def test_one_public_watcher_routes_pull_request_checks(monkeypatch):
    rows = [
        {"name": "tests", "bucket": "pending", "link": "https://github.com/o/r/runs/2"}
    ]
    received = {}

    monkeypatch.setattr(gh, "require_gh", lambda: None)

    def fetch(pr, repo=None):
        received["fetch"] = (pr, repo)
        return gh.checks_payload(rows, pr)

    def capture(title, description, poll, log_of=None, superseded_by=None):
        received["watch"] = (title, description, poll())
        return "watched"

    monkeypatch.setattr(gh, "fetch_checks", fetch)
    monkeypatch.setattr(gh, "watch", capture)

    assert gh.gh_watch_run(pr=1421, repo="o/r") == "watched"
    assert received["fetch"] == (1421, "o/r")
    assert received["watch"][:2] == ("Checks · 1421", "Checks on 1421")
    assert not hasattr(gh, "gh_watch_checks")
    with pytest.raises(ValueError, match="either run or pr"):
        gh.gh_watch_run(run=7, pr=1421)


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

    empty = gh.checks_payload([], "1421")
    assert gh.run_shape(empty)["is_over"] is True
    assert gh.run_shape(empty)["tone"] == "idle"


def test_malformed_github_json_is_a_retryable_poll_failure(monkeypatch):
    monkeypatch.setattr(gh, "_capture", lambda command, seconds=120: (0, "[not-json"))

    with pytest.raises(RuntimeError, match="invalid JSON"):
        gh.fetch_run(42)
    with pytest.raises(RuntimeError, match="invalid JSON"):
        gh.fetch_checks(42)


def test_a_tap_during_the_nap_is_answered_before_the_next_poll(recorder, monkeypatch):
    """Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: a tap waited out the tick.

    The watcher slept the whole three-second tick in one piece and only then read the
    shared selection, so a click on a job row took a tick (eight seconds on a long run)
    plus a GitHub poll before its steps and log appeared.
    """
    monkeypatch.setattr(gh, "FAST_TICK_S", 3.0)
    polls = [fixture("run-mid.json"), fixture("run-final.json")]
    selected = "95742028809"
    events = []
    now = [0.0]
    slept = []

    def fake_sleep(seconds):
        now[0] += seconds
        slept.append(seconds)
        if len(slept) == 2:
            recorder.focus("jobs", [selected])

    monkeypatch.setattr(vis.time, "monotonic", lambda: now[0])
    monkeypatch.setattr(vis.time, "sleep", fake_sleep)

    def poll():
        events.append("poll")
        return polls.pop(0) if len(polls) > 1 else polls[0]

    def log_of(job_id, lines):
        events.append(f"log {job_id}")
        return [f"log for {job_id}"]

    gh.watch(TITLE, DESCRIPTION, poll, log_of)

    second_poll = [index for index, one in enumerate(events) if one == "poll"][1]
    assert events.index(f"log {selected}") < second_poll
    # The tap was answered a slice after it landed, not a tick.
    assert sum(slept[:2]) <= gh.NAP_SLICE_S * 2
    # GitHub keeps its own cadence: the tap neither polls it nor cuts the tick short.
    assert sum(slept) == pytest.approx(3.0)
