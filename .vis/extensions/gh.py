"""gh — watch a GitHub Actions run on a live view, and hand the model the picture at the end.

A CI run is the archetype a live view exists for: it takes fifteen minutes, the extension can see
exactly when it ends, a person wants to WATCH it, and the model needs one paragraph afterwards.
Today that run is observed the expensive way — a log poll per provider round trip — or not at all.

**Everything GitHub is the `gh` CLI through the sandbox shell verb.** No hand-built HTTPS, no token
read, copied or printed: authentication is the operator's own `gh` session, and a missing or
unauthenticated `gh` refuses in ONE line before any view opens — nothing to watch beats an empty
pane.

Eight nodes, one per question a person asks about a run: what is happening (`run`), how far
(`progress`), how many of each (`score`), which jobs (`jobs`), which steps belong to the focused
jobs (`steps`), what moved (`activity`), what the focused jobs printed (`output`), and where to
open it (`links`). Job rows are controls: all concurrently running jobs are focused by default;
with none running, the last failed job (or simply the last job) is focused. A tap replaces that
focus in shared live state, so the extension, terminal, and every Companion agree on the steps and
logs below it. One mapping serves all nodes: `queued`/`in_progress` is `running`, `success` is `ok`,
`skipped` and `neutral` are `idle`, anything else is `error`. Rows are upserted by the job's
`databaseId`, so a job that changes state keeps its slot and the eye keeps its place, and only what
CHANGED since the last poll crosses the wire.

GitHub serves a log per JOB, not per run: the REST endpoint `actions/jobs/N/logs` returns 404
while that job is writing, then answers with the whole log the moment the job ends. A running
selection therefore shows its current step and live elapsed time; that pulse is replaced by the raw
log as soon as GitHub publishes it. Activity starts with the work already underway instead of an
empty promise, then records every later job and step transition.
"""

import calendar
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


# -- the mapping: a `gh run view` payload, read as the eight answers --------------------


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


def _timestamp(value):
    """A GitHub UTC timestamp as epoch seconds, or None for its zero/invalid sentinel."""
    text = str(value or "")
    if not text or text.startswith("0001-01-01"):
        return None
    try:
        return calendar.timegm(time.strptime(text[:19], "%Y-%m-%dT%H:%M:%S"))
    except ValueError:
        return None


def _wall_time():
    """Current epoch seconds behind a deterministic test seam."""
    return time.time()


def _elapsed(item, now=None):
    """Elapsed wall time, live when `now` is supplied and the item has not ended yet."""
    began = _timestamp(item.get("startedAt"))
    ended = _timestamp(item.get("completedAt"))
    if began is None or (ended is None and now is None):
        return "·"
    seconds = max(0, int((ended if ended is not None else now) - began))
    minutes, seconds = divmod(seconds, 60)
    return f"{minutes}m {seconds:02d}s" if minutes else f"{seconds}s"


def _state_text(job):
    return (
        str(job.get("conclusion") or job.get("status") or "").replace("_", " ") or "·"
    )


def _job_id(job, index=0):
    """The stable table address of one job, with the same fallback its row uses."""
    return str(job.get("databaseId") or job.get("name") or index)


def default_focus_ids(jobs):
    """All running jobs, else the last failed job, else the last job."""
    running = [
        _job_id(job, index)
        for index, job in enumerate(jobs)
        if str(job.get("status") or "") in _RUNNING_STATES
    ]
    if running:
        return running
    failed = [
        _job_id(job, index)
        for index, job in enumerate(jobs)
        if tone_of(job.get("status"), job.get("conclusion")) == "error"
    ]
    if failed:
        return [failed[-1]]
    return [_job_id(jobs[-1], len(jobs) - 1)] if jobs else []


def run_shape(payload, focus_ids=None, now=None):
    """Everything the eight nodes show, derived from one `gh run view --json` payload.

    Pure: `focus_ids=None` applies the live default; explicit ids are the human's shared
    selection, pruned to jobs that still exist. The same payload and focus always answer the
    same shape, which makes both the mapping and click behavior testable without a network.
    """
    jobs = [job for job in (payload.get("jobs") or []) if isinstance(job, dict)]
    tones = [tone_of(job.get("status"), job.get("conclusion")) for job in jobs]
    indexed = [(_job_id(job, index), job) for index, job in enumerate(jobs)]
    by_id = dict(indexed)
    requested = (
        default_focus_ids(jobs)
        if focus_ids is None
        else [str(one) for one in focus_ids]
    )
    selected_ids = []
    for job_id in requested:
        if job_id in by_id and job_id not in selected_ids:
            selected_ids.append(job_id)
    if not selected_ids and jobs:
        selected_ids = default_focus_ids(jobs)
    selected = [(job_id, by_id[job_id]) for job_id in selected_ids]
    finished = [job for job in jobs if str(job.get("status") or "") == "completed"]
    failed = [
        job for job, job_tone in zip(jobs, tones, strict=True) if job_tone == "error"
    ]
    counted = {
        "passed": sum(1 for job_tone in tones if job_tone == "ok"),
        "failed": len(failed),
        "skipped": sum(1 for job_tone in tones if job_tone == "idle"),
        "queued": sum(1 for job_tone in tones if job_tone == "running"),
    }
    headline = f"{len(finished)} of {len(jobs)} jobs finished"
    if failed:
        headline += f", {len(failed)} failed"
    is_over = str(payload.get("status") or "") == "completed"
    focus_names = [str(job.get("name") or "?") for _, job in selected]
    focus = focus_names[0] if len(focus_names) == 1 else " + ".join(focus_names)
    focus_detail = focus if len(focus_names) == 1 else f"{len(focus_names)} jobs"
    steps = []
    active_step_ids = []
    for job_id, job in selected:
        for index, step in enumerate(job.get("steps") or []):
            step_status = str(step.get("status") or "")
            step_name = str(step.get("name") or "?")
            step_id = f"{job_id}:{step.get('number') or step_name or index}"
            if step_status == "in_progress":
                active_step_ids.append(step_id)
            step_tone = tone_of(step.get("status"), step.get("conclusion"))
            step_elapsed = _elapsed(step, now)
            if step_tone == "running" and step_elapsed != "·":
                step_name = f"{step_name} · {step_elapsed}"
            steps.append(
                {
                    "id": step_id,
                    "label": (
                        f"{job.get('name') or '?'} · {step_name}"
                        if len(selected) > 1
                        else step_name
                    ),
                    "tone": step_tone,
                }
            )
    return {
        "is_over": is_over,
        "run_url": str(payload.get("url") or ""),
        "headline": headline,
        "detail": "workflow **{}** on `{}` · in focus **{}**".format(
            payload.get("workflowName") or "?",
            payload.get("headBranch") or "?",
            focus_detail,
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
                "id": _job_id(job, index),
                "cells": [
                    str(job.get("name") or "?"),
                    _state_text(job),
                    _elapsed(job, now),
                ],
                "tone": job_tone,
            }
            for index, (job, job_tone) in enumerate(zip(jobs, tones, strict=True))
        ],
        "focus": focus,
        "focus_ids": selected_ids,
        "steps": steps,
        "active_step_ids": active_step_ids,
        "links": [
            {"id": "run", "label": "This run", "target": str(payload.get("url") or "")},
        ]
        + [
            {
                "id": _job_id(job),
                "label": f"Failed · {job.get('name')}",
                "target": str(job.get("url") or payload.get("url") or ""),
            }
            for job in failed
        ],
    }


def declared_nodes(shape):
    """The eight nodes, declared once from the first poll and addressed by id ever after."""
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
            is_focusable=True,
            focused_ids=shape["focus_ids"],
        ),
        vis.steps(
            "steps",
            steps=[dict(one) for one in shape["steps"]],
            label="Steps of focused jobs",
        ),
        vis.output("activity", label="Activity"),
        vis.output("output", label="Logs of focused jobs"),
        vis.link("links", links=[dict(one) for one in shape["links"]]),
    ]


def push_changes(view, before, after):
    """Patch ONLY what moved between two polls — including shared table focus."""
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
    if before.get("focus_ids") != after["focus_ids"]:
        view["jobs"].focus(*after["focus_ids"])
        steps = {}
        view["steps"].clear()
    for step in after["steps"]:
        if steps.get(step["id"]) != step:
            view["steps"].set(step["id"], tone=step["tone"], label=step["label"])
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
    whole checklist the `steps` node already paints.
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
        if before.get("focus_ids") == after.get("focus_ids")
        else {}
    )
    for step in after.get("steps") or []:
        older = steps.get(step["id"])
        if older is None or older["tone"] == step["tone"]:
            continue
        lines.append("  {} {}".format(_MARKS.get(step["tone"], "·"), step["label"]))
    return lines


def active_lines(shape):
    """The focused work already underway when this poll landed."""
    rows = {row["id"]: row for row in shape.get("rows") or []}
    active_ids = set(shape.get("active_step_ids") or [])
    active_by_job = {}
    for step in shape.get("steps") or []:
        if step["id"] in active_ids:
            active_by_job.setdefault(step["id"].split(":", 1)[0], []).append(step)
    lines = []
    for job_id in shape.get("focus_ids") or []:
        row = rows.get(job_id)
        if not row or row["tone"] != "running":
            continue
        job_name = str(row["cells"][0])
        active = active_by_job.get(job_id) or []
        if not active:
            lines.append(f"▶ {job_name} · waiting to start")
            continue
        for step in active:
            label = step["label"].removeprefix(f"{job_name} · ")
            lines.append(f"▶ {job_name} · {label}")
    if lines:
        return lines
    return (
        ["✓ Run finished"]
        if shape.get("is_over")
        else ["· job and step changes appear here as they happen"]
    )


def _focus_signature(shape):
    """The focused rows and the state that decides whether their logs are ready."""
    rows = {row["id"]: row for row in shape.get("rows") or []}
    return tuple(
        (
            job_id,
            rows.get(job_id, {}).get("tone"),
            tuple(rows.get(job_id, {}).get("cells") or []),
        )
        for job_id in shape.get("focus_ids") or []
    )


def _focused_ids_from_state(state):
    """The jobs table selection a surface last wrote into shared live state."""
    pending = list((state or {}).get("nodes") or [])
    while pending:
        node = pending.pop(0)
        if node.get("id") == "jobs" and node.get("type") == "table":
            return [str(one) for one in node.get("focused_ids") or []]
        pending[0:0] = list(node.get("fields") or [])
    return []


def _job_log_tail(job_id, lines, log_of, cache):
    """One job tail, fetched once for this exact window size."""
    if not log_of:
        return None
    key = (job_id, lines)
    if key not in cache:
        cache[key] = log_of(job_id, lines)
    return cache[key]


def _focus_log_lines(shape, log_of, cache, lines):
    """Raw tails for finished focus, or a live step pulse while GitHub still withholds them."""
    rows = {row["id"]: row for row in shape.get("rows") or []}
    shown = []
    for job_id in shape.get("focus_ids") or []:
        row = rows.get(job_id)
        if not row:
            continue
        if row["tone"] == "running":
            shown.append(f"── {row['cells'][0]} · live progress")
            prefix = f"{job_id}:"
            active_ids = set(shape.get("active_step_ids") or [])
            active = [
                step
                for step in shape.get("steps") or []
                if step["id"].startswith(prefix) and step["id"] in active_ids
            ]
            for step in active:
                label = step["label"]
                job_prefix = f"{row['cells'][0]} · "
                shown.append(f"▶ {label.removeprefix(job_prefix)}")
            if not active:
                shown.append("▶ Waiting for this job to start")
            shown.append("· GitHub publishes the raw job log when this job ends")
            continue
        shown.append(f"── {row['cells'][0]} · log")
        tail = _job_log_tail(job_id, lines, log_of, cache)
        if tail:
            shown.extend(tail)
        else:
            shown.append("· No job log is available")
    return shown or ["· No job is focused"]


def _new_failure_log_lines(shape, filed, log_of, cache):
    """Newly failed jobs and their immediate tails, ready for the Activity history."""
    added = []
    for row in shape.get("rows") or []:
        if row["tone"] != "error" or row["id"] in filed:
            continue
        filed.add(row["id"])
        tail = _job_log_tail(row["id"], FAILED_TAIL_LINES, log_of, cache)
        if tail:
            added.extend([f"── {row['cells'][0]} · failed log", *tail])
    return added


def _show_activity(view, current, history, clear=True):
    """Replace Activity's live pulse while preserving its finite transition history."""
    if clear:
        view["activity"].clear()
    view["activity"].write(*current, *history)


def _show_focus_logs(view, shape, log_of, cache, lines, clear=True):
    """Replace the output pane with one combined photograph of focused job logs."""
    if clear:
        view["output"].clear()
    view["output"].write(*_focus_log_lines(shape, log_of, cache, lines))


def watch(title, description, poll, log_of=None):
    """Open a selectable CI view, patch it until the run ends, answer its picture.

    Job focus is shared live state. Each tick reads it before deriving the next shape, so a
    Companion tap changes the steps and logs the extension writes; absent a tap, all parallel
    running jobs follow together, then the last failed (or last) job becomes the default.

    There is no clock here. The loop ends when GitHub says the run is over, when `gh` stops
    answering, or when the human presses Interrupt — never on a duration this extension invented.
    """
    shape = run_shape(poll(), now=_wall_time())
    began = time.monotonic()
    manual_focus = None
    log_cache = {}
    filed_failures = set()
    activity_history = []
    with vis.live(title, declared_nodes(shape), description=description) as view:
        try:
            current_activity = active_lines(shape)
            activity_history.extend(
                _new_failure_log_lines(shape, filed_failures, log_of, log_cache)
            )
            _show_activity(view, current_activity, activity_history, clear=False)
            _show_focus_logs(
                view, shape, log_of, log_cache, FAILED_TAIL_LINES, clear=False
            )
            shown_focus = _focus_signature(shape)
            while not shape["is_over"]:
                if view.is_interrupted:
                    break
                time.sleep(_tick(time.monotonic() - began))
                try:
                    payload = poll()
                except RuntimeError as failure:
                    view["run"].set(str(failure)[:200], tone="error")
                    break
                try:
                    # Read AFTER the network poll: a tap made while `gh` was answering
                    # must win over the extension's next default-focus patch.
                    selected = _focused_ids_from_state(view.state())
                except vis.Interrupted:
                    break
                if selected != shape["focus_ids"]:
                    manual_focus = selected
                fresh = run_shape(payload, focus_ids=manual_focus, now=_wall_time())
                push_changes(view, shape, fresh)
                moved = feed_lines(shape, fresh)
                failures = _new_failure_log_lines(
                    fresh, filed_failures, log_of, log_cache
                )
                activity_history.extend(moved)
                activity_history.extend(failures)
                fresh_activity = active_lines(fresh)
                if fresh_activity != current_activity or moved or failures:
                    _show_activity(view, fresh_activity, activity_history)
                    current_activity = fresh_activity
                fresh_focus = _focus_signature(fresh)
                if fresh_focus != shown_focus and not fresh["is_over"]:
                    _show_focus_logs(view, fresh, log_of, log_cache, FAILED_TAIL_LINES)
                    shown_focus = fresh_focus
                shape = fresh
            if shape["is_over"]:
                _show_focus_logs(view, shape, log_of, log_cache, LOG_TAIL_LINES)
        except vis.Interrupted:
            # The human stopped watching. The view already holds its verdict, `close` answers
            # it, and `shape` is the last poll that reached them.
            pass
        return view.close(summary=_summary(shape))


def gh_watch_run(run=None, repo=None):
    """What is this CI run doing, and how did it end? Watch a GitHub Actions run to its verdict.

    Opens a live view a person can watch (and stop) while it polls `gh run view` every three
    seconds, easing to eight past five minutes. Job rows are controls: all jobs running in
    parallel are focused initially; tap one to replace the steps and output below with that job.
    With none running, the last failed job (or the last job) is focused. While a selected job runs,
    its current step and elapsed time keep moving; GitHub's raw log replaces that pulse the moment
    the job ends. `run` is a run id or URL; without one, the newest run on the current branch.
    `repo` is `owner/name` for a repository other than the working directory's.
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
Both open a view, poll `gh` until the run ends, and answer the picture (jobs, score, focused
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
