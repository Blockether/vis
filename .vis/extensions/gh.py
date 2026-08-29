"""gh — watch GitHub Actions live, then give the model one deduplicated diagnostic.

A CI run is the archetype a live view exists for: it takes time, a person wants to WATCH it, and
the model needs the final jobs, steps, timing, and failed logs without repeating the artifact tree.

**Everything GitHub is the `gh` CLI through the sandbox shell verb.** No hand-built HTTPS, token
read, copy, or model-visible credential: when `gh` is signed out, GitHub's browser device flow is
mediated by private human input before any view opens.

Six nodes answer distinct questions about a run: what the machine is doing RIGHT NOW (`run`), how far
the steps have got (`progress`), the outcome counts and the wall clock (`score`), the jobs (`jobs`), the
selected job's timeline (`steps`), and where to open it (`links`). A seventh, the log, is not declared: it
is ADDED after `run` the moment a selected job has something to read and dropped again when it has not, so
a phone shows a failure over the fold instead of a permanent line about a log GitHub has not published.
Job rows are controls: all concurrently running jobs are selected by default; with none running, the last
failed job (or simply the last job) is selected. A tap replaces that selection in shared live state, so the
extension, terminal, and every Companion agree on the timeline and log below it. One mapping serves all
nodes: every unfinished status is `running`, `success` is `ok`, `skipped` and `neutral` are `idle`, and
every unsuccessful conclusion is `error`. Rows are upserted by the job's `databaseId`, so a job that
changes state keeps its slot and the eye keeps its place, and only what CHANGED since the last poll
crosses the wire.

GitHub serves a log per JOB, not per run: the REST endpoint `actions/jobs/N/logs` returns 404 while
that job is writing, then answers with the whole log the moment the job ends. The log pane of a
running selection therefore says only that the log is still unpublished — the current step and its
live elapsed time belong to the steps panel, and repeating them in the log would write a fresh copy
of the same placeholder into the view's record on every tick. The raw log replaces that line the
moment GitHub publishes it. Temporary CLI, network, malformed JSON, rate-limit, and provider failures
retain the last good picture and retry visibly in the run status; three consecutive failures settle
the view as failed instead of turning an outage or deleted run into an infinite watch. A missing job
log is never cached as final. When a running watch is overtaken by a newer commit for the same
workflow, branch and event, the obsolete watch closes and links to its replacement instead of
polling work that no longer matters. Pull-request checks use this same view through the one public
watcher; a check set with no checks settles neutral rather than waiting forever.
"""

import calendar
import json
import os
import re
import shlex
import tempfile
import time
from collections import Counter

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

# A tick is GITHUB's cadence, never the human's. The nap between two polls is slept in
# slices shorter than the live-frame batching window: every slice reads shared selection,
# so the derived timeline and cached log join the same frame as the row tap.
NAP_SLICE_S = 0.05

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


def _span(seconds):
    """A duration read at a glance: hours appear only when there are hours."""
    minutes, rest = divmod(max(0, int(seconds)), 60)
    hours, minutes = divmod(minutes, 60)
    if hours:
        return f"{hours}h {minutes:02d}m"
    return f"{minutes}m {rest:02d}s" if minutes else f"{rest}s"


def _elapsed(item, now=None):
    """Elapsed wall time, live when `now` is supplied and the item has not ended yet."""
    began = _timestamp(item.get("startedAt"))
    ended = _timestamp(item.get("completedAt"))
    if began is None or (ended is None and now is None):
        return "·"
    return _span((ended if ended is not None else now) - began)


def _state_text(job):
    return (
        str(job.get("conclusion") or job.get("status") or "").replace("_", " ") or "·"
    )


def _job_id(job, index=0):
    """The stable table address of one job, with the same fallback its row uses."""
    return str(job.get("databaseId") or job.get("name") or index)


def _job_groups(jobs):
    """Repeated GitHub `parent / variant` names become one collapsible table branch."""
    prefixes = []
    for job in jobs:
        name = str(job.get("name") or "")
        prefixes.append(name.rsplit(" / ", 1)[0] if " / " in name else None)
    counts = Counter(prefix for prefix in prefixes if prefix)
    return [prefix if prefix and counts[prefix] > 1 else None for prefix in prefixes]


def default_selected_ids(jobs):
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


def _job_steps(job):
    return [step for step in (job.get("steps") or []) if isinstance(step, dict)]


def _job_display(job, group=None):
    """The name a row shows: a matrix variant drops the parent its group already names."""
    name = str(job.get("name") or "?")
    if group and name.startswith(f"{group} / "):
        return name[len(group) + 3 :] or name
    return name


def _selection_name(job, group=None):
    """The name the STATUS uses — parent and variant, joined the way a person says it."""
    variant = _job_display(job, group)
    return f"{group} · {variant}" if group else variant


def _current_step(job):
    """The step a job is ON: the one running, else the one that failed, else the last finished."""
    steps = _job_steps(job)
    for step in steps:
        if str(step.get("status") or "") == "in_progress":
            return step
    for step in reversed(steps):
        if tone_of(step.get("status"), step.get("conclusion")) == "error":
            return step
    for step in reversed(steps):
        if str(step.get("status") or "") == "completed":
            return step
    return None


def _step_tone(step):
    """A STEP's tone, which is not a job's: a step nobody has reached is not running.

    `tone_of` answers for a JOB, where "queued" means the runner is coming and the row
    should read as alive. A job's own step list is different — it names every step the
    workflow declares, so the twenty steps after the one under way are all "queued" and
    painting them as running turns the timeline into a wall of activity around the one
    line that is actually moving.
    """
    status = str(step.get("status") or "")
    if status == "in_progress":
        return "running"
    if status == "completed":
        return tone_of(status, step.get("conclusion"))
    return "idle"


def _step_counts(jobs):
    """Steps finished over steps known.

    GitHub lists a job's steps only once that job STARTS, so the total grows as the run
    fans out. That is the truth and not a defect: work nobody has scheduled is work
    nobody can measure. What matters is that it MOVES — a matrix of three that takes
    forty minutes moves a job counter zero times and a step counter every minute or two.
    """
    done = 0
    total = 0
    for job in jobs:
        for step in _job_steps(job):
            total += 1
            if str(step.get("status") or "") == "completed":
                done += 1
    if total:
        return done, total
    # A pull request's checks are jobs with no steps at all: then the check IS the unit,
    # and counting steps would leave the bar dead at 0 of 0 for the whole run.
    finished = sum(1 for job in jobs if str(job.get("status") or "") == "completed")
    return finished, len(jobs)


def _run_elapsed(jobs, now=None):
    """How long the RUN has been going: first job to start, last to finish."""
    began = [
        instant
        for instant in (_timestamp(job.get("startedAt")) for job in jobs)
        if instant is not None
    ]
    if not began:
        return "·"
    ends = [_timestamp(job.get("completedAt")) for job in jobs]
    if ends and all(end is not None for end in ends):
        return _span(max(ends) - min(began))
    return "·" if now is None else _span(now - min(began))


def _run_status(payload, jobs, tones, groups, now=None):
    """The status line is the WORK, not the arithmetic.

    Measured on a 97-minute run: `1 of 12 jobs finished` did not change for 23 minutes,
    so the node a person reads first said nothing about what the machine was doing while
    the machine was busy. The step under way is what moves, so the step is what it says —
    and when something has failed, the failure outranks whatever is still running.
    """
    paired = list(zip(jobs, tones, groups, strict=True))
    failed = [(job, group) for job, tone, group in paired if tone == "error"]
    running = [
        (job, group)
        for job, tone, group in paired
        if tone == "running" and str(job.get("status") or "") == "in_progress"
    ]
    if failed:
        job, group = failed[0]
        step = _current_step(job)
        detail = [str(step.get("name") or "?")] if step is not None else []
        took = _elapsed(job, now)
        detail.append(f"failed after {took}" if took != "·" else "failed")
        if len(failed) > 1:
            detail.append(f"{len(failed) - 1} more failed")
        return f"{_selection_name(job, group)} failed", " · ".join(detail)
    if running:
        job, group = running[0]
        step = _current_step(job)
        detail = []
        if step is not None:
            detail.append(str(step.get("name") or "?"))
            moment = _elapsed(step, now)
            if moment != "·":
                detail.append(f"{moment} in this step")
        if len(running) > 1:
            detail.append(f"{len(running) - 1} other jobs running")
        return _selection_name(job, group), " · ".join(detail) or "starting"
    if str(payload.get("status") or "") == "completed":
        passed = sum(1 for tone in tones if tone == "ok")
        ending = str(payload.get("conclusion") or "").replace("_", " ")
        headline = (
            f"All {passed} jobs passed"
            if ending == "success" and passed
            else (ending.capitalize() or "Finished")
        )
        took = _run_elapsed(jobs, now)
        return (
            headline,
            f"{len(jobs)} jobs · {took}" if took != "·" else f"{len(jobs)} jobs",
        )
    waiting = len(jobs) or "no"
    return "Waiting for a runner", f"{waiting} jobs queued"


def _span_seconds(item):
    """How long one item took, in seconds, or zero when GitHub never said."""
    began = _timestamp(item.get("startedAt"))
    ended = _timestamp(item.get("completedAt"))
    if began is None or ended is None:
        return 0
    return max(0, int(ended - began))


def _timeline(selected, groups_by_id, is_over, now=None):
    """The selected job's steps, folded to what is happening while the run is LIVE.

    A job has sixty steps and a person watching wants three: the one that just finished,
    the one under way, and the one coming. The rest are named — `5 earlier steps · 2m07s`
    is one idle row that says the time went somewhere — and when the run is OVER nothing
    is folded at all, because the settled picture is where the whole story is read.
    """
    steps = []
    active_step_ids = []
    for job_id, job in selected:
        name = _job_display(job, groups_by_id.get(job_id))
        rows = []
        for index, step in enumerate(_job_steps(job)):
            step_name = str(step.get("name") or "?")
            step_id = f"{job_id}:{step.get('number') or step_name or index}"
            tone = _step_tone(step)
            if str(step.get("status") or "") == "in_progress":
                active_step_ids.append(step_id)
            moment = _elapsed(step, now)
            if tone == "running" and moment != "·":
                step_name = f"{step_name} · {moment}"
            label = f"{name} · {step_name}" if len(selected) > 1 else step_name
            rows.append(
                {
                    "id": step_id,
                    "label": label,
                    "tone": tone,
                    "_seconds": _span_seconds(step),
                }
            )
        here = next(
            (index for index, row in enumerate(rows) if row["id"] in active_step_ids),
            None,
        )
        if is_over or here is None or here < 2:
            steps.extend(
                {key: value for key, value in row.items() if key != "_seconds"}
                for row in rows
            )
            continue
        folded = rows[: here - 1]
        seconds = sum(row["_seconds"] for row in folded)
        steps.append(
            {
                "id": f"{job_id}:earlier",
                "label": "{} earlier {} · {}".format(
                    len(folded), "step" if len(folded) == 1 else "steps", _span(seconds)
                ),
                "tone": "idle",
            }
        )
        steps.extend(
            {key: value for key, value in row.items() if key != "_seconds"}
            for row in rows[here - 1 :]
        )
    return steps, active_step_ids


def run_shape(payload, selected_ids=None, now=None):
    """Everything the nodes show, derived from one `gh run view --json` payload.

    Pure: `selected_ids=None` applies the live default; explicit ids are the human's shared
    selection, pruned to jobs that still exist. The same payload and selection always answer the
    same shape, which makes both the mapping and click behavior testable without a network.
    """
    jobs = [job for job in (payload.get("jobs") or []) if isinstance(job, dict)]
    tones = [tone_of(job.get("status"), job.get("conclusion")) for job in jobs]
    groups = _job_groups(jobs)
    # A branch NAMES itself and then qualifies itself, the way every label in this
    # panel does: `Build native image · 3 variants`. The count belongs to the
    # branch and not to a row, so it is added here and nowhere near `_job_display`,
    # which still has to strip the bare parent name off each variant.
    group_sizes = Counter(group for group in groups if group)
    indexed = [(_job_id(job, index), job) for index, job in enumerate(jobs)]
    by_id = dict(indexed)
    groups_by_id = {
        job_id: group for (job_id, _), group in zip(indexed, groups, strict=True)
    }
    requested = (
        default_selected_ids(jobs)
        if selected_ids is None
        else [str(one) for one in selected_ids]
    )
    selected_ids = []
    for job_id in requested:
        if job_id in by_id and job_id not in selected_ids:
            selected_ids.append(job_id)
    if not selected_ids and jobs:
        selected_ids = default_selected_ids(jobs)
    selected = [(job_id, by_id[job_id]) for job_id in selected_ids]
    failed = [
        job for job, job_tone in zip(jobs, tones, strict=True) if job_tone == "error"
    ]
    is_over = str(payload.get("status") or "") == "completed"
    headline, detail = _run_status(payload, jobs, tones, groups, now)
    done, total = _step_counts(jobs)
    steps, active_step_ids = _timeline(selected, groups_by_id, is_over, now)
    selection_names = [
        _job_display(job, groups_by_id.get(job_id)) for job_id, job in selected
    ]
    selection_groups = {groups_by_id.get(job_id) for job_id, _ in selected}
    if len(selected) == 1:
        selection = _selection_name(selected[0][1], groups_by_id.get(selected[0][0]))
    elif len(selection_groups) == 1 and None not in selection_groups:
        # Three legs of one matrix are one thing being done three ways: name the parent
        # once and list the variants, the way a person reads the run out loud.
        selection = f"{selection_groups.pop()} · " + " + ".join(selection_names)
    else:
        selection = " + ".join(
            _selection_name(job, groups_by_id.get(job_id)) for job_id, job in selected
        )
    counted = {
        "passed": sum(1 for job_tone in tones if job_tone == "ok"),
        "failed": len(failed),
        "running": sum(
            1 for job in jobs if str(job.get("status") or "") == "in_progress"
        ),
        "queued": sum(
            1
            for job, job_tone in zip(jobs, tones, strict=True)
            if job_tone == "running" and str(job.get("status") or "") != "in_progress"
        ),
    }
    return {
        "is_over": is_over,
        "run_url": str(payload.get("url") or ""),
        "headline": headline,
        "detail": detail,
        "tone": tone_of(payload.get("status"), payload.get("conclusion")),
        "done": done,
        "total": total,
        # Four counters, four fixed ids, because a stat is patched by id: what CHANGES
        # is the label. Before anything breaks the question is "how much is moving";
        # after, it is "how much never ran" — and the clock is a counter too, the one
        # answer to "is this normal" that no other node carries.
        "score": [
            {
                "id": "active",
                "value_text": str(counted["failed"] if is_over else counted["running"]),
                # While it runs, the question is how much is moving; once it is over,
                # movement is not a thing that can happen and only the damage counts.
                "label": "failed" if is_over else "running",
                "tone": (
                    ("error" if counted["failed"] else "idle")
                    if is_over
                    else ("running" if counted["running"] else "idle")
                ),
            },
            {
                "id": "passed",
                "value_text": str(counted["passed"]),
                "label": "passed",
                "tone": "ok",
            },
            {
                "id": "waiting",
                "value_text": str(counted["queued"]),
                "label": "unrun" if is_over else "queued",
                "tone": "idle",
            },
            {
                "id": "elapsed",
                "value_text": _run_elapsed(jobs, now),
                "label": "elapsed",
                "tone": "idle",
            },
        ],
        "rows": [
            {
                "id": _job_id(job, index),
                "cells": [
                    _job_display(job, group),
                    _state_text(job),
                    _elapsed(job, now),
                ],
                "tone": job_tone,
                **(
                    {"branch": f"{group} · {group_sizes[group]} variants"}
                    if group
                    else {}
                ),
            }
            for index, (job, job_tone, group) in enumerate(
                zip(jobs, tones, groups, strict=True)
            )
        ],
        "selection": selection,
        "selected_ids": selected_ids,
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
    """The six nodes, declared once from the first poll and addressed by id ever after.

    The log is NOT among them: it is added after `run` when there is something to read
    (`_show_selection_logs`), so an empty pane never takes the space above the jobs.
    """
    return [
        vis.status(
            "run", shape["headline"], tone=shape["tone"], detail=shape["detail"]
        ),
        vis.progress(
            "progress", done=shape["done"], total=shape["total"], label="Steps finished"
        ),
        vis.stat("score", stats=[dict(one) for one in shape["score"]]),
        vis.table(
            "jobs",
            columns=[
                vis.table_column("job", "Job"),
                vis.table_column("state", "Now"),
                vis.table_column("took", "Took"),
            ],
            rows=[
                vis.table_row(
                    row["id"], row["cells"], tone=row["tone"], branch=row.get("branch")
                )
                for row in shape["rows"]
            ],
            is_selectable=True,
            selected_ids=shape["selected_ids"],
        ),
        vis.steps(
            "steps",
            steps=[dict(one) for one in shape["steps"]],
            label="Timeline",
        ),
        vis.link("links", links=[dict(one) for one in shape["links"]]),
    ]


def push_changes(view, before, after):
    """Patch ONLY what moved between two polls — including shared table selection."""
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
        if was.get(one["id"]) != one:
            view["score"].set(
                one["id"], one["value_text"], label=one["label"], tone=one["tone"]
            )
    rows = {one["id"]: one for one in before.get("rows") or []}
    for row in after["rows"]:
        if rows.get(row["id"]) != row:
            view["jobs"].upsert(
                row["id"], row["cells"], tone=row["tone"], branch=row.get("branch")
            )
    steps = {one["id"]: one for one in before.get("steps") or []}
    if before.get("selected_ids") != after["selected_ids"]:
        view["jobs"].select(*after["selected_ids"])
        steps = {}
        view["steps"].clear()
    # The timeline FOLDS while the run is live, so a step can leave it: the rows that
    # went behind `5 earlier steps` are cleared rather than left behind the fold.
    if steps and {one["id"] for one in after["steps"]} < set(steps):
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


def _github_hostname(value):
    """A shell-safe GitHub host name, without accepting a URL or command syntax."""
    hostname = str(value or "").strip().lower()
    if not re.fullmatch(r"[A-Za-z0-9](?:[A-Za-z0-9.-]*[A-Za-z0-9])?", hostname):
        raise ValueError("hostname must be a host name such as github.com")
    return hostname


def _auth_status(hostname="github.com"):
    """Whether GitHub CLI has a valid account for one host, without exposing its status text."""
    hostname = _github_hostname(hostname)
    exit_code, _ = _capture(
        f"gh auth status --hostname {shlex.quote(hostname)}",
        60,
    )
    return exit_code == 0


def _device_authorization(process, hostname, seconds=60):
    """Read the short-lived OAuth device code and URL from a running `gh auth login`."""
    deadline = time.monotonic() + seconds
    ansi = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")
    while True:
        state = process.logs(-80)
        text = ansi.sub("", str(state.get("out") or ""))
        code = re.search(r"\b[A-Z0-9]{4}-[A-Z0-9]{4}\b", text)
        url = re.search(r"https://[^\s]+/login/device", text)
        if code and url:
            target = url.group(0)
            if not target.lower().startswith(f"https://{hostname}/"):
                raise GhMissing("GitHub CLI returned an unexpected authorization host")
            return code.group(0), target
        if state.get("status") != "running":
            raise GhMissing(
                "GitHub CLI exited before browser authorization could begin"
            )
        if time.monotonic() >= deadline:
            process.stop()
            raise GhMissing("GitHub CLI did not produce a browser authorization code")
        process.wait(1)


def gh_login(hostname="github.com"):
    """Authenticate GitHub CLI through private human input and GitHub's browser device flow.

    If already signed in, returns immediately. Otherwise GitHub creates a short-lived code which is
    shown only in a HITL dialog; the human opens GitHub, authorizes the CLI, and confirms there. The
    model receives only the final status, never the code, token, account status output, or process
    transcript. HTTPS Git credentials are configured as part of login. `hostname` defaults to
    `github.com` and may name a GitHub Enterprise host.
    """
    hostname = _github_hostname(hostname)
    if _auth_status(hostname):
        return f"GitHub CLI is already authenticated with {hostname}."

    installed, _ = _capture("gh --version", 30)
    if installed != 0:
        raise GhMissing("GitHub CLI is not installed; install `gh`, then ask again")

    quoted = shlex.quote(hostname)
    process = vis.shell(
        {
            "op": "background",
            "id": f"gh-login-{int(time.monotonic() * 1000)}",
            "command": (
                "GH_BROWSER=true gh auth login "
                f"--hostname {quoted} --git-protocol https --web --skip-ssh-key"
            ),
        }
    )
    completed = False
    try:
        code, url = _device_authorization(process, hostname)
        # `gh` pauses before launching its browser helper. The helper is deliberately `true`: HITL
        # is the browser boundary, while Enter lets `gh` begin polling GitHub for authorization.
        process.type("")
        answer = vis.ask(
            "Sign in to GitHub",
            [
                vis.heading("Authorize GitHub CLI"),
                vis.paragraph(f"Open this address in a browser: {url}"),
                vis.paragraph(f"Enter this one-time code: {code}"),
                vis.paragraph(
                    "GitHub will show the requested CLI permissions. Approve them there, then "
                    "return here. The code and resulting credential are never sent to the model."
                ),
                vis.checkbox(
                    "authorized",
                    label="I completed authorization in GitHub",
                    is_required=True,
                ),
            ],
            description="Authorize GitHub CLI without sharing credentials with the model.",
            submit_label="I authorized GitHub",
            cancel_label="Cancel sign-in",
            timeout_ms=0,
        )
        if not answer:
            reason = str(getattr(answer, "reason", "cancelled") or "cancelled")
            if reason == "cancelled":
                raise GhMissing("GitHub authentication was cancelled by the human")
            raise GhMissing(f"GitHub authentication could not continue: {reason}")

        finished = process.wait(300)
        if finished.get("status") == "running":
            raise GhMissing(
                "GitHub authentication did not finish after human authorization"
            )
        if finished.get("exit") != 0 or not _auth_status(hostname):
            raise GhMissing(
                "GitHub rejected or could not complete browser authentication"
            )
        completed = True
        return f"GitHub CLI authenticated with {hostname}."
    finally:
        if not completed:
            process.stop()


def require_gh():
    """Ensure the default GitHub CLI account exists, asking the human to sign in when needed."""
    gh_login()


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
    # Nothing is running or queued once the run has been handed over: a counter that
    # still says "running 2" is the same stale state the rows were just cleared of.
    settled["score"] = [
        {**stat, "value_text": "0", "tone": "idle"}
        if stat.get("id") in {"active", "waiting"} and stat.get("label") != "failed"
        else stat
        for stat in shape.get("score") or []
    ]
    return settled


def _model_report(payload, log_of, cache, superseded=None, failure=None):
    """One compact, lossless-enough CI report: schema once, each fact once."""
    jobs = [job for job in (payload.get("jobs") or []) if isinstance(job, dict)]
    report = {
        "run": {
            "id": payload.get("databaseId"),
            "workflow": payload.get("workflowName") or "?",
            "branch": payload.get("headBranch") or "?",
            "status": payload.get("status") or "",
            "conclusion": payload.get("conclusion") or "",
            "url": payload.get("url") or "",
        },
        "job_fields": [
            "id",
            "name",
            "conclusion",
            "started_at",
            "completed_at",
            "steps",
        ],
        "step_fields": ["id", "conclusion", "name"],
        "jobs": [
            [
                job.get("databaseId") or job.get("name") or index,
                job.get("name") or "?",
                job.get("conclusion") or job.get("status") or "",
                job.get("startedAt") or "",
                job.get("completedAt") or "",
                [
                    [
                        step.get("number") or step_index,
                        step.get("conclusion") or step.get("status") or "",
                        step.get("name") or "?",
                    ]
                    for step_index, step in enumerate(job.get("steps") or [])
                    if isinstance(step, dict)
                ],
            ]
            for index, job in enumerate(jobs)
        ],
        "failed_logs": {
            str(job.get("databaseId") or job.get("name") or index): tail
            for index, job in enumerate(jobs)
            if tone_of(job.get("status"), job.get("conclusion")) == "error"
            if (
                tail := _job_log_tail(
                    _job_id(job, index), LOG_TAIL_LINES, log_of, cache
                )
            )
        },
    }
    if superseded:
        report["ending"] = {
            "reason": "superseded",
            "replacement_run_id": superseded.get("databaseId"),
            "replacement_title": superseded.get("displayTitle") or "",
            "replacement_url": superseded.get("url") or "",
        }
    elif failure:
        report["ending"] = {"reason": "poll_failure", "error": failure}
    return json.dumps(report, ensure_ascii=False, separators=(",", ":"))


def _selection_signature(shape):
    """The selected rows and the state that decides whether their logs are ready.

    Deliberately free of anything that TICKS: elapsed time changes every poll, and a
    signature that moved with it rewrote the log pane once per tick.
    """
    rows = {row["id"]: row for row in shape.get("rows") or []}
    return tuple(
        (job_id, rows.get(job_id, {}).get("tone"))
        for job_id in shape.get("selected_ids") or []
    )


def _selected_ids_from_state(state):
    """The jobs table selection a surface last wrote into shared live state."""
    pending = list((state or {}).get("nodes") or [])
    while pending:
        node = pending.pop(0)
        if node.get("id") == "jobs" and node.get("type") == "table":
            return [str(one) for one in node.get("selected_ids") or []]
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


def _selection_log_lines(shape, log_of, cache, lines):
    """The raw tail of every selected job GitHub has actually published.

    A running job HAS no log — GitHub publishes it when the job ends — and a pane that
    says so is one line of nothing sitting above the jobs for the forty minutes somebody
    is watching. So a job with nothing to read contributes nothing, the caller drops the
    node when this comes back empty, and the pane exists only while it holds something.

    One selected job needs no heading: the node's own label already names it. Several do,
    and then each tail is introduced by the job it came from.
    """
    rows = {row["id"]: row for row in shape.get("rows") or []}
    picked = []
    for job_id in shape.get("selected_ids") or []:
        row = rows.get(job_id)
        if not row or row["tone"] == "running":
            continue
        tail = _job_log_tail(job_id, lines, log_of, cache)
        if tail:
            picked.append((row["cells"][0], list(tail)))
    if len(picked) == 1:
        return picked[0][1]
    shown = []
    for name, tail in picked:
        shown.append(f"── {name} · log")
        shown.extend(tail)
    return shown


def _log_label(shape):
    """The log node's name: one word for what it is, then what it is a log OF."""
    rows = {row["id"]: row for row in shape.get("rows") or []}
    selected = [
        rows[job_id] for job_id in shape.get("selected_ids") or [] if job_id in rows
    ]
    if not selected:
        return "Log"
    named = " + ".join(row["cells"][0] for row in selected)
    broke = any(row.get("tone") == "error" for row in selected)
    return f"{'Failure' if broke else 'Log'} · {named}"


def _show_selection_logs(view, shape, log_of, cache, lines):
    """Put the selected job's log directly under the status — when there IS one.

    A log node declared with the rest spends most of a run saying that GitHub has not
    published anything yet: one line of nothing, above everything the view was opened
    for. So the pane is ADDED when there is something to read, dropped when there is
    not, and it sits after `run` — on a phone that is where the eye already is when
    something breaks. Its label names the job it belongs to, so a new job is a new node
    rather than a rename, and the callers only reach here when the selection moved.
    """
    with view.batch():
        written = _selection_log_lines(shape, log_of, cache, lines)
        if "output" in view:
            view.drop("output")
        if not written:
            return
        view.add(vis.output("output", label=_log_label(shape)), after="run")
        view["output"].write(*written)


def _archive_selection_snapshots(view, payload, log_of, cache):
    """Finished pictures for every job, without rewriting the live surface.

    The artifact owns these after the watcher exits. They are ordinary live-view
    pictures, so the Companion can switch a selectable table locally while no
    extension process remains to answer a click.
    """
    base = view.state()
    jobs = [job for job in (payload.get("jobs") or []) if isinstance(job, dict)]
    snapshots = []
    for index, job in enumerate(jobs):
        job_id = _job_id(job, index)
        shape = run_shape(payload, selected_ids=[job_id], now=_wall_time())
        picture = json.loads(json.dumps(base))
        nodes = {node["id"]: node for node in picture.get("nodes") or []}
        nodes["run"].update(
            text=shape["headline"], tone=shape["tone"], detail=shape["detail"]
        )
        nodes["jobs"]["selected_ids"] = [job_id]
        if "steps" in nodes:
            nodes["steps"]["steps"] = [dict(step) for step in shape["steps"]]
        lines = _selection_log_lines(shape, log_of, cache, LOG_TAIL_LINES)
        log = vis.output(
            "output", label=_log_label(shape), lines=lines, total_lines=len(lines)
        )
        if "output" in nodes:
            nodes["output"].update(log)
        else:
            # The live view only carries a log while one is worth reading; a settled
            # picture of ONE job always is, so the snapshot declares it in the same
            # place the live one would have taken, under the status.
            picture["nodes"].insert(1, log)
        snapshots.append({"node_id": "jobs", "selected_ids": [job_id], "view": picture})
    return snapshots


def _refresh_surface(view, before, after, shown_selection, log_of, cache):
    """Publish one internally consistent picture for a provider poll or selection."""
    fresh_selection = _selection_signature(after)
    with view.batch():
        push_changes(view, before, after)
        if fresh_selection != shown_selection and not after["is_over"]:
            _show_selection_logs(view, after, log_of, cache, FAILED_TAIL_LINES)
    return fresh_selection


def _sync_surface_selection(
    view, payload, shape, manual_selection, log_of, cache, shown_selection
):
    """Apply a surface selection from shared state without waiting for GitHub to answer."""
    selected = _selected_ids_from_state(view.state())
    if selected == shape["selected_ids"]:
        return shape, manual_selection, shown_selection
    manual_selection = selected
    selected = run_shape(payload, selected_ids=manual_selection, now=_wall_time())
    fresh_selection = _refresh_surface(
        view, shape, selected, shown_selection, log_of, cache
    )
    return selected, manual_selection, fresh_selection


def _nap(
    view, seconds, payload, shape, manual_selection, log_of, cache, shown_selection
):
    """Wait out one tick, answering a surface tap the moment it lands.

    GitHub keeps its own cadence; a selection is local shared state, so the steps and
    log a person just asked for are pushed inside the nap rather than a whole tick and
    a poll later. The tick is never cut short — a tap costs no GitHub call.
    """
    deadline = time.monotonic() + seconds
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0 or view.is_interrupted:
            return shape, manual_selection, shown_selection
        if view.sleep(remaining, slice_ms=NAP_SLICE_S * 1000):
            shape, manual_selection, shown_selection = _sync_surface_selection(
                view, payload, shape, manual_selection, log_of, cache, shown_selection
            )


def watch(title, description, poll, log_of=None, superseded_by=None):
    """Open a selectable CI view, patch it until the run ends, answer its picture.

    Job selection is shared live state. Each tick reads it before deriving the next shape, so a
    Companion tap changes the steps and logs the extension writes; absent a tap, all parallel
    running jobs follow together, then the last failed (or last) job becomes the default.

    The loop ends when GitHub says the run is over, when a later commit starts the same workflow,
    when `gh` stops answering, or when the human presses Interrupt — never on an invented duration.
    """
    payload = poll()
    shape = run_shape(payload, now=_wall_time())
    began = time.monotonic()
    manual_selection = None
    log_cache = {}
    superseded = None
    unavailable_attempts = 0
    terminal_failure = None
    with vis.live(title, declared_nodes(shape), description=description) as view:
        try:
            _show_selection_logs(view, shape, log_of, log_cache, FAILED_TAIL_LINES)
            shown_selection = _selection_signature(shape)
            while not shape["is_over"]:
                if view.is_interrupted:
                    break
                try:
                    # The nap is where a tap is ANSWERED: shared state is read every
                    # slice, so a click does not wait out the tick.
                    shape, manual_selection, shown_selection = _nap(
                        view,
                        _tick(time.monotonic() - began),
                        payload,
                        shape,
                        manual_selection,
                        log_of,
                        log_cache,
                        shown_selection,
                    )
                    # Selection is local shared state, not provider data. Apply it BEFORE a
                    # network call so a slow or unavailable GitHub cannot freeze the details.
                    shape, manual_selection, shown_selection = _sync_surface_selection(
                        view,
                        payload,
                        shape,
                        manual_selection,
                        log_of,
                        log_cache,
                        shown_selection,
                    )
                except vis.Interrupted:
                    break
                if superseded_by:
                    superseded = superseded_by()
                    if superseded:
                        run_id = str(superseded.get("databaseId") or "?")
                        settled = superseded_shape(shape)
                        with view.batch():
                            push_changes(view, shape, settled)
                            shape = settled
                            view["run"].set(
                                f"Superseded by newer run {run_id}",
                                tone="idle",
                                detail="Stopped watching obsolete work after a newer commit started",
                            )
                            _show_selection_logs(
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
                    # must win over the extension's next default-selection patch.
                    selected = _selected_ids_from_state(view.state())
                except vis.Interrupted:
                    break
                if selected != shape["selected_ids"]:
                    manual_selection = selected
                fresh = run_shape(
                    payload, selected_ids=manual_selection, now=_wall_time()
                )
                shown_selection = _refresh_surface(
                    view, shape, fresh, shown_selection, log_of, log_cache
                )
                shape = fresh
            if shape["is_over"]:
                _show_selection_logs(view, shape, log_of, log_cache, LOG_TAIL_LINES)
        except vis.Interrupted:
            # The human stopped watching. The view already holds its verdict, `close` answers
            # it, and `shape` is the last poll that reached them.
            pass
        selection_snapshots = (
            _archive_selection_snapshots(view, payload, log_of, log_cache)
            if shape["is_over"]
            else []
        )
        if terminal_failure:
            return view.close(
                reason="failed",
                error=terminal_failure,
                selection_snapshots=selection_snapshots,
                model_result=_model_report(
                    payload, log_of, log_cache, superseded, terminal_failure
                ),
            )
        if superseded:
            return view.close(
                reason="superseded",
                selection_snapshots=selection_snapshots,
                model_result=_model_report(payload, log_of, log_cache, superseded),
            )
        return view.close(
            selection_snapshots=selection_snapshots,
            model_result=_model_report(payload, log_of, log_cache),
        )


def gh_watch_run(run=None, repo=None, pr=None):
    """What is this CI activity doing, and how did it end? Watch one run or one PR's checks.

    Opens a live view a person can watch (and stop), easing polls from three seconds to eight after
    five minutes. Job rows are controls: all jobs running in parallel are selected initially; tap one
    to replace the steps and output below with that job, answered within a fifth of a second
    whatever the poll cadence. The returned string is compact JSON: run
    metadata; a schema-once list of every job with id, outcome, start/end times, and nested step
    ids/outcomes/names; and one bounded log tail for each failed job. It never repeats the artifact
    tree. `run` is a run id or URL; without `run` or `pr`, the newest run on the current branch is
    selected. `pr` is a pull-request number, branch, URL, or `"current"`; it watches that PR's
    aggregate checks through the same view. `run` and `pr` are mutually exclusive. Any running run
    yields when a newer run starts the same workflow, branch and event. `repo` is `owner/name` for
    another repository.
    """
    if run is not None and pr is not None:
        raise ValueError("Choose either run or pr, not both")
    require_gh()
    if pr is not None:
        pull = None if pr == "current" else pr
        first = fetch_checks(pull, repo)
        return watch(
            f"Checks · {pull}" if pull else "Checks",
            str(first.get("displayTitle") or "checks"),
            lambda: fetch_checks(pull, repo),
        )
    run_id = run or newest_run(repo)
    first = fetch_run(run_id, repo)
    title = str(first.get("workflowName") or "GitHub Actions")
    # Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: a live run showed
    # progress and elapsed durations but never the calendar date and time it began.
    # It is an identity fact of the run, so it belongs in the head with the branch
    # and the title — the status line above the jobs is for what is happening NOW.
    described = " · ".join(
        part
        for part in (
            str(first.get("headBranch") or ""),
            str(first.get("displayTitle") or run_id),
            str(first.get("event") or ""),
            (lambda began: f"started {began}" if began else "")(
                _display_started(first.get("startedAt"))
            ),
        )
        if part
    )
    owner = repo_of(first, repo)
    return watch(
        f"{title} · run {run_id}",
        described.strip(" ·"),
        lambda: fetch_run(run_id, repo),
        lambda job_id, lines: job_log(owner, job_id, lines),
        lambda: newer_run(first, repo),
    )


def checks_payload(rows, pull):
    """Read `gh pr checks --json` rows as the run payload the shared view understands."""
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


PROMPT = """gh_ surface active — authenticate GitHub and watch Actions on a live view the human can see.
  gh_login(hostname="github.com")
  gh_watch_run(run=None, repo=None, pr=None)
`gh_login` runs GitHub's browser device flow through private human input; no code or credential is
returned to the model. A watcher invokes it automatically when signed out. The watcher follows one
run or one pull request's checks until completion, then answers the diagnostic picture — use it
instead of a shell polling loop."""


vis.extension(
    name="gh",
    description="Authenticate GitHub CLI and watch Actions on a live view.",
    version="0.2.0",
    kind="integration",
    alias="gh",
    symbols=[
        vis.symbol(gh_login, tag="mutation"),
        vis.symbol(gh_watch_run, tag="observation"),
    ],
    prompt=PROMPT,
)
