"""gh — watch a GitHub Actions run on a live view, and hand the model the picture at the end.

A CI run is the archetype a live view exists for: it takes fifteen minutes, the extension can see
exactly when it ends, a person wants to WATCH it, and the model needs one paragraph afterwards.
Today that run is observed the expensive way — a log poll per provider round trip — or not at all.

**Everything GitHub is the `gh` CLI through the sandbox shell verb.** No hand-built HTTPS, no token
read, copied or printed: authentication is the operator's own `gh` session, and a missing or
unauthenticated `gh` refuses in ONE line before any view opens — nothing to watch beats an empty
pane.

Seven nodes, one per question a person asks about a run: what is happening (`run`), how far
(`progress`), how many of each (`score`), which jobs (`jobs`), which steps of the job in focus
(`failing`), what it printed (`output`), where to open it (`links`). One mapping serves all of them:
`queued`/`in_progress` is `running`, `success` is `ok`, `skipped` and `neutral` are `idle`, anything
else is `error`. Rows are upserted by the job's `databaseId`, so a job that changes state keeps its
slot and the eye keeps its place, and only what CHANGED since the last poll crosses the wire.

GitHub serves a log per JOB, not per run: `gh run view --job N --log` refuses with "run … is
still in progress" until the whole run is over, while the REST endpoint `actions/jobs/N/logs`
answers the moment THAT job ends. So a job that fails eight minutes into a twenty-minute matrix
shows its log eight minutes in, and until then the pane is a feed of what MOVED — every job that
changed state, every step of the job in focus. The tail is what the model reads; the view's
record keeps every line that was written, and the settled pane is one photograph of the log that
matters rather than the hour of feed that led to it.
"""

import json
import os
import tempfile
import time

import vis

# `gh run view --json <these>` is the whole payload the view is built from: one call per poll.
RUN_FIELDS = "jobs,status,conclusion,workflowName,headBranch,url,displayTitle,number,event,databaseId"

# The tick a person watches things move on, and the one a long run settles into. Three seconds is
# the slowest tick a counter still reads as LIVE on; a poll is one `gh run view --json` call, so
# even an hour of watching stays far inside a person's own rate limit.
FAST_TICK_S = 3.0
SLOW_TICK_S = 8.0
BACKOFF_AFTER_S = 300.0

# The model's copy of a log is a TAIL: the whole log stays in the view's record. The settled tail
# is the engine's own model budget for a log node, so the picture elides nothing; a job that fails
# mid-run says less, because the run is not over and the story is still moving.
LOG_TAIL_LINES = 120
FAILED_TAIL_LINES = 40

_RUNNING_STATES = ("queued", "in_progress", "waiting", "requested", "pending")


class GhMissing(RuntimeError):
    """`gh` is absent or signed out — the one refusal that happens before a view opens."""


# -- the mapping: a `gh run view` payload, read as the seven answers --------------------


def tone_of(status, conclusion):
    """The one tone mapping every node uses: a job's state read as `running`/`ok`/`idle`/`error`."""
    if str(status or "") in _RUNNING_STATES:
        return "running"
    finished = str(conclusion or "")
    if finished == "success":
        return "ok"
    if finished in ("skipped", "neutral"):
        return "idle"
    return "error"


def _elapsed(job):
    """`1m 12s` between a job's start and its end — `·` while it is still running."""
    started, ended = str(job.get("startedAt") or ""), str(job.get("completedAt") or "")
    if not started or not ended or ended.startswith("0001-01-01"):
        return "·"
    try:
        began = time.strptime(started[:19], "%Y-%m-%dT%H:%M:%S")
        over = time.strptime(ended[:19], "%Y-%m-%dT%H:%M:%S")
    except ValueError:
        return "·"
    seconds = max(0, int(time.mktime(over) - time.mktime(began)))
    minutes, seconds = divmod(seconds, 60)
    return f"{minutes}m {seconds:02d}s" if minutes else f"{seconds}s"


def _state_text(job):
    return (
        str(job.get("conclusion") or job.get("status") or "").replace("_", " ") or "·"
    )


def run_shape(payload):
    """Everything the seven nodes show, derived from one `gh run view --json` payload.

    Pure: the same payload always answers the same shape, which is what makes the mapping
    testable without a network and the patches computable by comparing two of them.
    """
    jobs = [j for j in (payload.get("jobs") or []) if isinstance(j, dict)]
    tones = [tone_of(j.get("status"), j.get("conclusion")) for j in jobs]
    finished = [j for j in jobs if str(j.get("status") or "") == "completed"]
    failed = [j for j, tone in zip(jobs, tones, strict=True) if tone == "error"]
    running = [j for j in jobs if str(j.get("status") or "") in _RUNNING_STATES]
    counted = {
        "passed": sum(1 for t in tones if t == "ok"),
        "failed": len(failed),
        "skipped": sum(1 for t in tones if t == "idle"),
        "queued": sum(1 for t in tones if t == "running"),
    }
    headline = f"{len(finished)} of {len(jobs)} jobs finished"
    if failed:
        headline += f", {len(failed)} failed"
    is_over = str(payload.get("status") or "") == "completed"
    focus = (running or failed or jobs or [None])[0]
    in_focus = str(focus.get("name") or "?") if focus else ""
    return {
        "is_over": is_over,
        "run_url": str(payload.get("url") or ""),
        "headline": headline,
        # A node's LABEL is declared once and never patched, so the job whose steps and log are
        # shown is named here, where it can change with the focus.
        "detail": "workflow **{}** on `{}` · in focus **{}**".format(
            payload.get("workflowName") or "?",
            payload.get("headBranch") or "?",
            in_focus,
        ),
        "tone": tone_of(payload.get("status"), payload.get("conclusion")),
        "done": len(finished),
        "total": len(jobs),
        "score": [
            {"id": name, "value_text": str(counted[name]), "label": name, "tone": tone}
            for name, tone in (
                ("passed", "ok"),
                ("failed", "error"),
                ("skipped", "idle"),
                ("queued", "running"),
            )
        ],
        "rows": [
            {
                "id": str(job.get("databaseId") or job.get("name") or index),
                "cells": [str(job.get("name") or "?"), _state_text(job), _elapsed(job)],
                "tone": tone,
            }
            for index, (job, tone) in enumerate(zip(jobs, tones, strict=True))
        ],
        "focus": in_focus,
        "focus_id": str(focus.get("databaseId") or "") if focus else "",
        "steps": [
            {
                "id": str(step.get("number") or step.get("name") or index),
                "label": str(step.get("name") or "?"),
                "tone": tone_of(step.get("status"), step.get("conclusion")),
            }
            for index, step in enumerate(focus.get("steps") or [] if focus else [])
        ],
        "links": [
            {"id": "run", "label": "This run", "target": str(payload.get("url") or "")},
        ]
        + [
            {
                "id": str(job.get("databaseId") or job.get("name")),
                "label": f"Failed · {job.get('name')}",
                "target": str(job.get("url") or payload.get("url") or ""),
            }
            for job in failed
        ],
    }


def declared_nodes(shape):
    """The seven nodes, declared once from the first poll and addressed by id ever after."""
    return [
        vis.status(
            "run", shape["headline"], tone=shape["tone"], detail=shape["detail"]
        ),
        vis.progress("progress", done=shape["done"], total=shape["total"]),
        vis.stat("score", stats=[dict(one) for one in shape["score"]]),
        vis.table(
            "jobs",
            columns=[
                vis.table_column("job", "Job"),
                vis.table_column("state", "Status"),
                vis.table_column("took", "Took"),
            ],
            rows=[
                vis.table_row(row["id"], row["cells"], tone=row["tone"])
                for row in shape["rows"]
            ],
        ),
        vis.steps(
            "failing",
            steps=[dict(one) for one in shape["steps"]],
            label="Steps of the job in focus",
        ),
        vis.output("output", label="Log of the job in focus"),
        vis.link("links", links=[dict(one) for one in shape["links"]]),
    ]


def push_changes(view, before, after):
    """Patch ONLY what moved between two polls — the whole point of upserting by id.

    A run of eighteen jobs settles one row at a time; re-pushing the table every five seconds
    would reshuffle nothing but would cost the wire (and the reader's place) every tick.
    """
    if (
        before.get("headline") != after["headline"]
        or before.get("tone") != after["tone"]
        or before.get("detail") != after["detail"]
    ):
        view["run"].set(after["headline"], tone=after["tone"], detail=after["detail"])
    if before.get("done") != after["done"] or before.get("total") != after["total"]:
        view["progress"].set(done=after["done"], total=after["total"])
    was = {one["id"]: one for one in before.get("score") or []}
    for one in after["score"]:
        if was.get(one["id"], {}).get("value_text") != one["value_text"]:
            view["score"].set(
                one["id"], one["value_text"], label=one["label"], tone=one["tone"]
            )
    rows = {one["id"]: one for one in before.get("rows") or []}
    for row in after["rows"]:
        if rows.get(row["id"]) != row:
            view["jobs"].upsert(row["id"], row["cells"], tone=row["tone"])
    steps = {one["id"]: one for one in before.get("steps") or []}
    if before.get("focus_id") != after["focus_id"]:
        steps = {}
        view["failing"].clear()
    for step in after["steps"]:
        if steps.get(step["id"]) != step:
            view["failing"].set(step["id"], tone=step["tone"], label=step["label"])
    known = {one["id"] for one in before.get("links") or []}
    for one in after["links"]:
        if one["id"] not in known:
            view["links"].add(one["id"], one["label"], one["target"])


# -- gh, and nothing but gh ------------------------------------------------------------


def _shell(command, seconds=120):
    """One command through the sandbox shell verb, waited out, answered as its result map."""
    handle = vis.shell(
        {
            "op": "background",
            "id": f"gh-{int(time.monotonic() * 1000)}",
            "command": command,
        }
    )
    return handle.wait(int(seconds))


def _capture(command, seconds=120):
    """Run `command` with stdout on a file, answering `(exit, text)`.

    A run's JSON and a job's log are both far past what a shell result carries inline, so the
    bytes land on disk and are read back whole rather than through a truncated tail.
    """
    handle, path = tempfile.mkstemp(prefix="vis-gh-", suffix=".out")
    os.close(handle)
    try:
        done = _shell(f"{command} > {path} 2>&1", seconds)
        with open(path, encoding="utf-8", errors="replace") as f:
            return int(done.get("exit") or 0), f.read()
    finally:
        os.unlink(path)


def require_gh():
    """Refuse in one line when `gh` is absent or signed out — before anything is opened."""
    exit_code, text = _capture("gh auth status", 60)
    if exit_code != 0:
        first = next((line.strip() for line in text.splitlines() if line.strip()), "")
        raise GhMissing(
            "gh is not usable here: "
            + (first or "`gh auth status` failed")
            + " — install GitHub CLI and run `gh auth login`, then ask again"
        )


def _repo_flag(repo):
    return f" --repo {repo}" if repo else ""


def fetch_run(run_id, repo=None):
    """One poll: the run as `gh` reports it, or a raised failure carrying gh's own words."""
    exit_code, text = _capture(
        f"gh run view {run_id}{_repo_flag(repo)} --json {RUN_FIELDS}"
    )
    if exit_code != 0:
        raise RuntimeError(f"gh run view {run_id} failed: {text.strip()[:400]}")
    return json.loads(text)


def newest_run(repo=None):
    """The run id of the newest run on the current branch — what `gh run watch` would pick."""
    exit_code, branch = _capture("git branch --show-current", 30)
    on = f" --branch {branch.strip()}" if exit_code == 0 and branch.strip() else ""
    exit_code, text = _capture(
        f"gh run list{_repo_flag(repo)}{on} -L 1 --json databaseId"
    )
    rows = json.loads(text) if exit_code == 0 and text.strip().startswith("[") else []
    if not rows:
        raise RuntimeError(
            f"no GitHub Actions run to watch{on or ''} — push a commit, or pass a run id"
        )
    return rows[0]["databaseId"]


def log_window(text, lines=LOG_TAIL_LINES):
    """The lines worth showing out of a job's whole log.

    `gh` repeats the job and step name on every line — noise in a pane three columns wide — so
    only the timestamped text is kept. And a failing job's LAST lines are the runner cleaning up
    orphan processes, while the reason it failed is above them: when the log carries an
    `##[error]` marker, the window ENDS at the last one, so the tail a person reads is what led
    to the failure rather than what happened after it.
    """
    written = [
        line.split("\t")[-1].rstrip() for line in text.splitlines() if line.strip()
    ]
    marked = [at for at, line in enumerate(written) if "##[error]" in line]
    end = marked[-1] + 1 if marked else len(written)
    return written[max(0, end - int(lines)) : end]


def repo_of(payload, repo=None):
    """`owner/name` for the run — the flag when one was given, else read off the run's own URL.

    The log endpoint is addressed by repository, and the payload already carries the one the run
    belongs to, so a watch stays at one `gh` call per poll.
    """
    if repo:
        return str(repo)
    parts = [one for one in str(payload.get("url") or "").split("/") if one]
    at = parts.index("github.com") if "github.com" in parts else -1
    return "/".join(parts[at + 1 : at + 3]) if 0 <= at < len(parts) - 2 else ""


def job_log(repo, job_id, lines=LOG_TAIL_LINES):
    """The tail of ONE job's log the moment that job is over — nil while it is still writing.

    `gh run view --job N --log` answers "run … is still in progress" until the whole RUN
    completes, which is why a job that failed early used to stay silent for as long as the rest of
    the matrix took. The REST endpoint is per JOB: 404 while it runs, the whole log once it ends.
    """
    if not repo or not job_id:
        return None
    exit_code, text = _capture(f"gh api repos/{repo}/actions/jobs/{job_id}/logs", 180)
    if exit_code != 0:
        return None
    return log_window(text, lines)


# -- what the human watches ------------------------------------------------------------


def _tick(since):
    return FAST_TICK_S if since < BACKOFF_AFTER_S else SLOW_TICK_S


def _summary(shape):
    ending = "finished" if shape["is_over"] else "still running"
    return "{} — {}, {} · {}".format(
        shape["headline"], shape["detail"], ending, shape["run_url"]
    )


# The mark a tone wears in the feed: what a person scans for when a run is long.
_MARKS = {"ok": "✓", "error": "✗", "running": "▶", "idle": "–"}


def feed_lines(before, after):
    """What APPEARED between two polls: every job that changed state, every step that moved.

    A twenty-minute run with one number ticking reads as a hang. These lines are the run's story
    as it happens, and they cost nothing: they are read out of the poll the view is patched from.

    A job seen for the first time is news; a STEP is not, or every focus change would dump the
    whole checklist the `failing` node already paints.
    """
    was = {row["id"]: row for row in before.get("rows") or []}
    lines = []
    for row in after.get("rows") or []:
        older = was.get(row["id"])
        if older is not None and older["cells"][1] == row["cells"][1]:
            continue
        took = row["cells"][2]
        lines.append(
            "{} {} · {}{}".format(
                _MARKS.get(row["tone"], "·"),
                row["cells"][0],
                row["cells"][1],
                f" · {took}" if took and took != "·" else "",
            )
        )
    steps = (
        {one["id"]: one for one in before.get("steps") or []}
        if before.get("focus_id") == after.get("focus_id")
        else {}
    )
    for step in after.get("steps") or []:
        older = steps.get(step["id"])
        if older is None or older["tone"] == step["tone"]:
            continue
        lines.append("  {} {}".format(_MARKS.get(step["tone"], "·"), step["label"]))
    return lines


def _file_logs(view, shape, logged, log_of):
    """Write the log of every job that has FAILED and has not been shown yet.

    Attempted once per job: a log GitHub would not serve is not re-asked every three seconds, and
    the job in focus is fetched again when the run ends anyway.
    """
    for row in shape.get("rows") or []:
        if row["tone"] != "error" or row["id"] in logged:
            continue
        logged.add(row["id"])
        tail = log_of(row["id"], FAILED_TAIL_LINES)
        if tail:
            view["output"].write(f"── {row['cells'][0]} · log", *tail)


def watch(title, description, poll, log_of=None):
    """Open the view on the first poll, patch it until the run ends, answer the picture.

    Whatever ends it, the model gets the same shape: a human's stop answers the state they were
    looking at plus their note, a finished run answers the finished run.

    There is no clock here. The loop ends when GitHub says the run is over, when `gh` stops
    answering, or when the human presses Interrupt — never on a duration this extension
    invented. A watch whose end the extension cannot SEE is meant to hang: the view is on
    screen and the stop is one keystroke away, so the person watching decides, and the engine's
    eval wall (lifted for as long as a view is open) is nobody's deadline.
    """
    shape = run_shape(poll())
    began = time.monotonic()
    logged = set()
    with vis.live(title, declared_nodes(shape), description=description) as view:
        try:
            view["output"].write("· what moves is written here as it happens")
            if log_of:
                _file_logs(view, shape, logged, log_of)
            while not shape["is_over"]:
                if view.is_interrupted:
                    break
                time.sleep(_tick(time.monotonic() - began))
                try:
                    fresh = run_shape(poll())
                except RuntimeError as failure:
                    view["run"].set(str(failure)[:200], tone="error")
                    break
                push_changes(view, shape, fresh)
                moved = feed_lines(shape, fresh)
                if moved:
                    view["output"].write(*moved)
                if log_of:
                    _file_logs(view, fresh, logged, log_of)
                shape = fresh
            if shape["is_over"] and log_of:
                # The settled pane is ONE photograph: the log of the job that must be acted on,
                # not the feed that led there. The record still holds every line of it.
                tail = log_of(shape["focus_id"], LOG_TAIL_LINES)
                if tail:
                    view["output"].clear()
                    view["output"].write(f"── {shape['focus']} · log", *tail)
        except vis.Interrupted:
            # The human stopped watching. The view already holds its verdict, `close` answers
            # it, and `shape` is the last poll that reached them — which is the picture they
            # were looking at when they stopped.
            pass
        return view.close(summary=_summary(shape))


def gh_watch_run(run=None, repo=None):
    """What is this CI run doing, and how did it end? Watch a GitHub Actions run to its verdict.

    Opens a live view a person can watch (and stop) while it polls `gh run view` every three
    seconds, easing to eight past the first five minutes, and answers the picture at the end: the
    jobs, the score, the failing job's steps and the tail of its log. Every job that moves is
    written into the log pane as it moves, and a job that FAILS shows its log the moment it
    fails — the rest of the matrix is not waited for. `run` is a run id or URL; without one, the
    newest run on the current branch. `repo` is `owner/name` for a repo other than the working
    directory's.
    """
    require_gh()
    run_id = run or newest_run(repo)
    first = fetch_run(run_id, repo)
    title = str(first.get("workflowName") or "GitHub Actions")
    described = "{} · {}".format(
        first.get("displayTitle") or run_id, first.get("event") or ""
    )
    owner = repo_of(first, repo)
    return watch(
        f"{title} · run {run_id}",
        described.strip(" ·"),
        lambda: fetch_run(run_id, repo),
        lambda job_id, lines: job_log(owner, job_id, lines),
    )


def checks_payload(rows, pull):
    """`gh pr checks --json` rows read as a run payload, so ONE mapping serves both views."""
    states = {
        "pass": ("completed", "success"),
        "fail": ("completed", "failure"),
        "skipping": ("completed", "skipped"),
        "cancel": ("completed", "cancelled"),
        "pending": ("in_progress", ""),
    }
    jobs = []
    for row in rows or []:
        status, conclusion = states.get(
            str(row.get("bucket") or ""), ("completed", "neutral")
        )
        jobs.append(
            {
                "databaseId": str(row.get("link") or row.get("name") or ""),
                "name": str(row.get("name") or "?"),
                "status": status,
                "conclusion": conclusion,
                "startedAt": row.get("startedAt") or "",
                "completedAt": row.get("completedAt") or "",
                "url": row.get("link") or "",
                "steps": [],
            }
        )
    is_over = all(str(job["status"]) == "completed" for job in jobs) if jobs else False
    failed = [job for job in jobs if job["conclusion"] == "failure"]
    return {
        "jobs": jobs,
        "status": "completed" if is_over else "in_progress",
        "conclusion": ("failure" if failed else "success") if is_over else "",
        "workflowName": "checks",
        "headBranch": str(pull or "pull request"),
        "displayTitle": f"Checks on {pull}" if pull else "Checks",
        "url": str((rows or [{}])[0].get("link") or ""),
        "event": "pull_request",
    }


def fetch_checks(pull, repo=None):
    """One poll of `gh pr checks`, mapped into the payload `run_shape` already understands."""
    named = f" {pull}" if pull else ""
    command = (
        f"gh pr checks{named}{_repo_flag(repo)} "
        "--json name,state,bucket,startedAt,completedAt,link,workflow"
    )
    exit_code, text = _capture(command)
    # `gh pr checks` exits 8 while checks are pending and 1 when one failed: both are ANSWERS,
    # and only a payload that is not JSON is a failure worth stopping for.
    if not text.strip().startswith("["):
        raise RuntimeError(f"gh pr checks failed: {text.strip()[:400]}")
    return checks_payload(json.loads(text), pull)


def gh_watch_checks(pr=None, repo=None):
    """Are this pull request's checks green yet? Watch `gh pr checks` to its verdict.

    The same seven-node view as `gh_watch_run` over a pull request's checks — each check is a
    row, upserted by its link, and the steps node stays empty because a check has none. `pr` is
    a number, branch or URL; without one, the pull request for the current branch.
    """
    require_gh()
    first = fetch_checks(pr, repo)
    return watch(
        f"Checks · {pr}" if pr else "Checks",
        str(first.get("displayTitle") or "checks"),
        lambda: fetch_checks(pr, repo),
    )


PROMPT = """gh_ surface active — watch a GitHub Actions run on a live view the human can see.
  gh_watch_run(run=None, repo=None)     gh_watch_checks(pr=None, repo=None)
Both open a view, poll `gh` until the run ends, and answer the picture (jobs, score, failing
steps, log tail) — use them instead of a shell loop over `gh run view`."""


vis.extension(
    name="gh",
    description="Watch a GitHub Actions run or a pull request's checks on a live view.",
    version="0.1.0",
    kind="integration",
    alias="gh",
    symbols=[
        vis.symbol(gh_watch_run, tag="observation"),
        vis.symbol(gh_watch_checks, tag="observation"),
    ],
    prompt=PROMPT,
)
