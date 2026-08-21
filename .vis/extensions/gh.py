"""gh — watch a GitHub Actions run on a live view, and hand the model the picture at the end.

A CI run is the archetype a live view exists for: it takes fifteen minutes, the extension can see
exactly when it ends, a person wants to WATCH it, and the model needs one paragraph afterwards.

**Everything GitHub is the `gh` CLI through the sandbox shell verb.** No hand-built HTTPS, no token
read, copied or printed: authentication is the operator's own `gh` session, and a missing or
unauthenticated `gh` refuses in ONE line before any view opens — nothing to watch beats an empty
pane.

Seven nodes answer distinct questions about a run: status (`run`), completion (`progress`), outcome
counts (`score`), jobs (`jobs`), the selected job's steps (`steps`) and log (`output`), and where to
open it (`links`). Job rows are controls: all concurrently running jobs are focused by default; with
none running, the last failed job (or simply the last job) is focused. A tap replaces that focus in
shared live state, so the extension, terminal, and every Companion agree on the steps and log below
it. One mapping serves all nodes: every unfinished status is `running`, `success` is `ok`, `skipped`
and `neutral` are `idle`, and every unsuccessful conclusion is `error`. Rows are upserted by the
job's `databaseId`, so a job that changes state keeps its slot and the eye keeps its place, and only
what CHANGED since the last poll crosses the wire.

GitHub serves a log per JOB, not per run: the REST endpoint `actions/jobs/N/logs` returns 404 while
that job is writing, then answers with the whole log the moment the job ends. A running selection
therefore shows its current step and live elapsed time; that pulse is replaced by the raw log as soon
as GitHub publishes it. Temporary CLI, network, malformed JSON, rate-limit, and provider failures
retain the last good picture and retry visibly in the run status; three consecutive failures settle
the view as failed instead of turning an outage or deleted run into an infinite watch. A missing job
log is never cached as final. When an implicitly selected run is overtaken by a newer commit for the
same workflow, branch and event, the obsolete watch closes and links to its replacement instead of
polling work that no longer matters. A check set with no checks settles neutral rather than waiting
forever.
"""

import calendar
import json
import os
import shlex
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
MAX_CONSECUTIVE_POLL_FAILURES = 3

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
    state = str(status or "")
    finished = str(conclusion or "")
    if state in _RUNNING_STATES or (state != "completed" and not finished):
        return "running"
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


_MONTHS = (
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec",
)


def _display_started(value):
    """A compact, timezone-explicit start instant for the run status detail."""
    instant = _timestamp(value)
    if instant is None:
        return None
    utc = time.gmtime(instant)
    return f"{utc.tm_mday} {_MONTHS[utc.tm_mon - 1]} {utc.tm_year}, {utc.tm_hour:02d}:{utc.tm_min:02d} UTC"


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
        "detail": "workflow **{}** on `{}`{} · in focus **{}**".format(
            payload.get("workflowName") or "?",
            payload.get("headBranch") or "?",
            (
                f" · started **{started}**"
                if (started := _display_started(payload.get("startedAt")))
                else ""
            ),
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
            label="Selected job steps",
        ),
        vis.output("output", label="Selected job log"),
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
    try:
        return json.loads(text)
    except json.JSONDecodeError as failure:
        raise RuntimeError(f"gh run view {run_id} returned invalid JSON") from failure


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


def newer_run(payload, repo=None):
    """A later run of this workflow/branch/event, or None while this run is still newest."""
    workflow = str(payload.get("workflowName") or "")
    branch = str(payload.get("headBranch") or "")
    event = str(payload.get("event") or "")
    current = payload.get("databaseId")
    if not workflow or not current:
        return None
    flags = f" --workflow {shlex.quote(workflow)}"
    if branch:
        flags += f" --branch {shlex.quote(branch)}"
    if event:
        flags += f" --event {shlex.quote(event)}"
    exit_code, text = _capture(
        f"gh run list{_repo_flag(repo)}{flags} -L 1 --json databaseId,url,displayTitle"
    )
    if exit_code != 0:
        return None
    try:
        rows = json.loads(text)
    except json.JSONDecodeError:
        return None
    latest = rows[0] if isinstance(rows, list) and rows else None
    if not isinstance(latest, dict):
        return None
    try:
        return latest if int(latest.get("databaseId") or 0) > int(current) else None
    except (TypeError, ValueError):
        return None


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


def superseded_shape(shape):
    """Settle unfinished snapshot rows when this watch yields to a newer run."""
    settled = dict(shape)
    settled["rows"] = [
        {
            **row,
            "cells": [row["cells"][0], "superseded", row["cells"][2]],
            "tone": "idle",
        }
        if row.get("tone") == "running"
        else row
        for row in shape.get("rows") or []
    ]
    settled["steps"] = [
        {**step, "tone": "idle"} if step.get("tone") == "running" else step
        for step in shape.get("steps") or []
    ]
    settled["active_step_ids"] = []
    settled["score"] = [
        {**stat, "value_text": "0", "tone": "idle"}
        if stat.get("id") == "queued"
        else stat
        for stat in shape.get("score") or []
    ]
    return settled


def _summary(shape, superseded=None, failure=None):
    if superseded:
        return "Superseded by run {} — {} · {}".format(
            superseded.get("databaseId") or "?",
            superseded.get("displayTitle") or "newer workflow run",
            superseded.get("url") or shape["run_url"],
        )
    if failure:
        return f"Stopped watching after GitHub failed {MAX_CONSECUTIVE_POLL_FAILURES} consecutive polls — {shape['run_url']}"
    ending = "finished" if shape["is_over"] else "still running"
    return "{} — {}, {} · {}".format(
        shape["headline"], shape["detail"], ending, shape["run_url"]
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
        tail = log_of(job_id, lines)
        if tail is not None:
            cache[key] = tail
        return tail
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


def _show_focus_logs(view, shape, log_of, cache, lines, clear=True):
    """Replace the output pane with one combined photograph of focused job logs."""
    if clear:
        view["output"].clear()
    view["output"].write(*_focus_log_lines(shape, log_of, cache, lines))


def _archive_focus_snapshots(view, payload, log_of, cache):
    """Finished pictures for every job, without rewriting the live surface.

    The artifact owns these after the watcher exits. They are ordinary live-view
    pictures, so the Companion can switch a focusable table locally while no
    extension process remains to answer a click.
    """
    base = view.state()
    jobs = [job for job in (payload.get("jobs") or []) if isinstance(job, dict)]
    snapshots = []
    for index, job in enumerate(jobs):
        job_id = _job_id(job, index)
        shape = run_shape(payload, focus_ids=[job_id], now=_wall_time())
        picture = json.loads(json.dumps(base))
        nodes = {node["id"]: node for node in picture.get("nodes") or []}
        nodes["run"].update(
            text=shape["headline"], tone=shape["tone"], detail=shape["detail"]
        )
        nodes["jobs"]["focused_ids"] = [job_id]
        nodes["steps"]["steps"] = [dict(step) for step in shape["steps"]]
        lines = _focus_log_lines(shape, log_of, cache, LOG_TAIL_LINES)
        nodes["output"]["lines"] = lines
        nodes["output"]["total_lines"] = len(lines)
        snapshots.append({"node_id": "jobs", "focused_ids": [job_id], "view": picture})
    return snapshots


def _sync_surface_focus(view, payload, shape, manual_focus, log_of, cache, shown_focus):
    """Apply a surface selection from shared state without waiting for GitHub to answer."""
    selected = _focused_ids_from_state(view.state())
    if selected == shape["focus_ids"]:
        return shape, manual_focus, shown_focus
    manual_focus = selected
    focused = run_shape(payload, focus_ids=manual_focus, now=_wall_time())
    push_changes(view, shape, focused)
    fresh_focus = _focus_signature(focused)
    if fresh_focus != shown_focus and not focused["is_over"]:
        _show_focus_logs(view, focused, log_of, cache, FAILED_TAIL_LINES)
    return focused, manual_focus, fresh_focus


def watch(title, description, poll, log_of=None, superseded_by=None):
    """Open a selectable CI view, patch it until the run ends, answer its picture.

    Job focus is shared live state. Each tick reads it before deriving the next shape, so a
    Companion tap changes the steps and logs the extension writes; absent a tap, all parallel
    running jobs follow together, then the last failed (or last) job becomes the default.

    The loop ends when GitHub says the run is over, when a later commit starts the same workflow,
    when `gh` stops answering, or when the human presses Interrupt — never on an invented duration.
    """
    payload = poll()
    shape = run_shape(payload, now=_wall_time())
    began = time.monotonic()
    manual_focus = None
    log_cache = {}
    superseded = None
    unavailable_attempts = 0
    terminal_failure = None
    with vis.live(title, declared_nodes(shape), description=description) as view:
        try:
            _show_focus_logs(
                view, shape, log_of, log_cache, FAILED_TAIL_LINES, clear=False
            )
            shown_focus = _focus_signature(shape)
            while not shape["is_over"]:
                if view.is_interrupted:
                    break
                time.sleep(_tick(time.monotonic() - began))
                try:
                    # Selection is local shared state, not provider data. Apply it BEFORE a
                    # network call so a slow or unavailable GitHub cannot freeze the details.
                    shape, manual_focus, shown_focus = _sync_surface_focus(
                        view,
                        payload,
                        shape,
                        manual_focus,
                        log_of,
                        log_cache,
                        shown_focus,
                    )
                except vis.Interrupted:
                    break
                if superseded_by:
                    superseded = superseded_by()
                    if superseded:
                        run_id = str(superseded.get("databaseId") or "?")
                        settled = superseded_shape(shape)
                        push_changes(view, shape, settled)
                        shape = settled
                        view["run"].set(
                            f"Superseded by newer run {run_id}",
                            tone="idle",
                            detail="Stopped watching obsolete work after a newer commit started",
                        )
                        _show_focus_logs(
                            view, shape, log_of, log_cache, FAILED_TAIL_LINES
                        )
                        target = str(superseded.get("url") or "")
                        if target:
                            view["links"].add("newer-run", "Newer run", target)
                        break
                try:
                    payload = poll()
                except RuntimeError as failure:
                    unavailable_attempts += 1
                    message = str(failure).splitlines()[0][:160]
                    if unavailable_attempts >= MAX_CONSECUTIVE_POLL_FAILURES:
                        terminal_failure = message
                        view["run"].set(
                            "Stopped: GitHub remained unavailable",
                            tone="error",
                            detail=message,
                        )
                        break
                    view["run"].set(
                        "GitHub temporarily unavailable; retrying",
                        tone="running",
                        detail=message,
                    )
                    continue
                if unavailable_attempts:
                    unavailable_attempts = 0
                    view["run"].set(
                        shape["headline"], tone=shape["tone"], detail=shape["detail"]
                    )
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
        focus_snapshots = (
            _archive_focus_snapshots(view, payload, log_of, log_cache)
            if shape["is_over"]
            else []
        )
        if terminal_failure:
            return view.close(
                reason="failed",
                summary=_summary(shape, superseded, terminal_failure),
                error=terminal_failure,
                focus_snapshots=focus_snapshots,
            )
        if superseded:
            return view.close(
                reason="superseded",
                summary=_summary(shape, superseded),
                focus_snapshots=focus_snapshots,
            )
        return view.close(summary=_summary(shape), focus_snapshots=focus_snapshots)


def gh_watch_run(run=None, repo=None):
    """What is this CI run doing, and how did it end? Watch a GitHub Actions run to its verdict.

    Opens a live view a person can watch (and stop) while it polls `gh run view` every three
    seconds, easing to eight past five minutes. Job rows are controls: all jobs running in
    parallel are focused initially; tap one to replace the steps and output below with that job.
    With none running, the last failed job (or the last job) is focused. While a selected job runs,
    its current step and elapsed time keep moving; GitHub's raw log replaces that pulse the moment
    the job ends. `run` is a run id or URL; without one, the newest run on the current branch is
    watched until it ends or a newer commit starts the same workflow, branch and event. An explicit
    run is never replaced. `repo` is `owner/name` for a repository other than the working directory's.
    """
    require_gh()
    implicit = run is None
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
        (lambda: newer_run(first, repo)) if implicit else None,
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
    is_over = all(str(job["status"]) == "completed" for job in jobs)
    failed = [job for job in jobs if job["conclusion"] == "failure"]
    return {
        "jobs": jobs,
        "status": "completed" if is_over else "in_progress",
        "conclusion": ("failure" if failed else "success" if jobs else "neutral")
        if is_over
        else "",
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
    try:
        rows = json.loads(text)
    except json.JSONDecodeError as failure:
        raise RuntimeError("gh pr checks returned invalid JSON") from failure
    return checks_payload(rows, pull)


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
