"""Swarm Forge: a pipeline of role sessions that work on tasks, one draft for each role.

`/swarm start <pack>` creates one session for each role of a pack. Each role session works in its
own `worktree` draft. A task goes through the roles in order. Each role merges the commit of the
previous role, does its own work and commits it. The last role stops at an approval. Approve lands
its draft on the default branch. Reject sends the task back with a comment.

The board is a `panel` view in the session that started the swarm. `vis.state` keeps each swarm, so
`/reload` opens the board again.
Packs are YAML files in `swarm_forge/packs/`.
"""

import json
import re
import threading
import time
from pathlib import Path
from typing import Literal

import blockether.vis.extension as vis
import yaml
from pydantic import BaseModel, ConfigDict, Field, ValidationError, model_validator

PACKS_DIR = Path(__file__).with_name("swarm_forge") / "packs"
TICK_S = 2.0
MAX_BOUNCES = 3
MAX_CARDS = 12
MAX_LOG = 20
RECEIVE = ("task", "batch")
PROPAGATION = ("forward-only", "back-one", "back-all")
DEFAULT = "default"
ACTIVE = ("running", "question", "blocked", "review", "landing")

_LOCK = threading.RLock()
_BOARDS = {}
_WORKERS = {}

PROTOCOL = """You are the {title} in a Swarm Forge pipeline ({pack}). A person follows your work on a board.

Rules:
- Work only in your own draft. If `draft_status()` shows no draft, call `draft_create("{label}")`. Else continue that draft.
- This request authorizes Git commits in your draft. Do not call `draft_approve()` or `draft_discard()` unless a request tells you to.
- Commit all your changes in the draft before you answer.
- If you need a decision, ask the person with a form, or end with the status `question`.
- If you cannot finish, end with the status `blocked`. Give the reason in `summary`.
- End your reply with one JSON object. Put no text after it:
  {{"status": "done", "summary": "<what you did>", "commit": "<full SHA of HEAD in your draft>", "branch": "vis/<your draft>"}}
- For the status `question`, add the field `question` with your question.
"""

CORRECTION = """Your last reply had no valid final answer: {error}

Do not repeat the work. Reply again with only the JSON object that the rules describe."""

REPLY = """The person answered your question:

{text}

Continue the task. End with the JSON object that the rules describe."""

LAND = """The person approved {tasks}. Land your draft now:

1. Call `draft_approve("{message}")`.
2. If it asks for a sync, call `draft_sync()`, resolve each conflict and call `draft_approve` again.
3. Do not call `draft_discard()`.

End with the JSON object that the rules describe. Put the landed commit in `commit`."""


class RoleAnswer(BaseModel):
    """The final answer of one role turn."""

    model_config = ConfigDict(extra="ignore")
    status: Literal["done", "blocked", "question"]
    summary: str = Field(min_length=1, max_length=4000)
    commit: str | None = None
    branch: str | None = None
    question: str | None = None

    @model_validator(mode="after")
    def _complete(self):
        if self.status == "done":
            if not re.fullmatch(r"[0-9a-f]{7,40}", self.commit or ""):
                raise ValueError(
                    "a done answer needs `commit`, the hex SHA of HEAD in the draft"
                )
            if not (self.branch or "").startswith("vis/"):
                raise ValueError("a done answer needs `branch`, the vis/<draft> branch")
        if self.status == "question" and not (self.question or "").strip():
            raise ValueError("a question answer needs the field `question`")
        return self


def reply_text(content):
    """Return the text of a turn's `content`: a string or a list of reply blocks."""
    if isinstance(content, list):
        parts = (
            block.get("markdown") or block.get("text") or ""
            for block in content
            if isinstance(block, dict)
        )
        return "\n\n".join(part for part in parts if part)
    return str(content or "")


def parse_answer(content):
    """Return the `RoleAnswer` at the end of `content`, or raise ValueError."""
    text = reply_text(content).strip()
    decoder = json.JSONDecoder()
    for at in reversed([m.start() for m in re.finditer(r"\{", text)]):
        try:
            data, end = decoder.raw_decode(text, at)
        except ValueError:
            continue
        if not isinstance(data, dict) or text[end:].strip(" \n\t`") != "":
            continue
        try:
            return RoleAnswer.model_validate(data)
        except ValidationError as error:
            first = error.errors()[0]
            raise ValueError(str(first.get("msg") or first)) from None
    raise ValueError("the reply does not end with a JSON object")


# -- packs and state ---------------------------------------------------------------------------


def pack_names():
    """Return the names of the packs in `PACKS_DIR`."""
    return sorted(path.stem for path in PACKS_DIR.glob("*.yml"))


def load_pack(name):
    """Read and check one pack file. Return its plain data."""
    path = PACKS_DIR / f"{name}.yml"
    if not path.is_file():
        raise ValueError(f"unknown pack {name!r}; use one of {', '.join(pack_names())}")
    data = yaml.safe_load(path.read_text())
    roles = data.get("roles") or []
    if not roles:
        raise ValueError(f"pack {name!r} has no roles")
    seen = set()
    for role in roles:
        role_name = str(role.get("name") or "")
        if not re.fullmatch(r"[a-z][a-z0-9-]*", role_name) or role_name in seen:
            raise ValueError(f"pack {name!r}: bad or repeated role name {role_name!r}")
        seen.add(role_name)
        if role.get("receive", "task") not in RECEIVE:
            raise ValueError(f"role {role_name}: receive must be task or batch")
        if role.get("propagation", "forward-only") not in PROPAGATION:
            raise ValueError(
                f"role {role_name}: bad propagation {role.get('propagation')!r}"
            )
        if not str(role.get("prompt") or "").strip():
            raise ValueError(f"role {role_name}: the prompt is empty")
    return {
        "name": str(data.get("name") or name),
        "description": data.get("description"),
        "roles": roles,
    }


def _key(swarm_id):
    return f"swarm:{swarm_id}"


def load(swarm_id):
    """Return the saved swarm of leader session `swarm_id`, or None."""
    return vis.state.get(_key(swarm_id))


def save(swarm):
    """Keep `swarm` in `vis.state` and paint its board."""
    vis.state[_key(swarm["id"])] = swarm
    index = list(vis.state.get("swarms") or [])
    if swarm["id"] not in index:
        vis.state["swarms"] = index + [swarm["id"]]
    paint(swarm)


def forget(swarm_id):
    vis.state[_key(swarm_id)] = None
    vis.state["swarms"] = [
        one for one in vis.state.get("swarms") or [] if one != swarm_id
    ] or None


def note(swarm, line):
    swarm["log"] = (swarm.get("log") or [])[-(MAX_LOG - 1) :] + [line]


def _models():
    """Return `{provider: [[name, label], ...]}` of the available models, in picker order."""
    found = {}
    try:
        for model in vis.sessions.models(is_available=True):
            found.setdefault(model.provider, []).append(
                [model.name, model.label or model.name]
            )
    except Exception:  # noqa: BLE001 - a board without a model list still works
        return {}
    return found


def new_swarm(leader_id, pack, root=None):
    """Create the role sessions of `pack` and return the new swarm state."""
    models = _models()
    roles = []
    for spec in pack["roles"]:
        options = {"draft_backend": "worktree"}
        if root:
            options["root"] = str(root)
        provider, model = spec.get("provider"), spec.get("model")
        session = vis.sessions.create(provider=provider, model=model, **options)
        roles.append(
            {
                "name": spec["name"],
                "title": spec.get("title") or spec["name"].title(),
                "prompt": str(spec["prompt"]).strip(),
                "receive": spec.get("receive", "task"),
                "propagation": spec.get("propagation", "forward-only"),
                "provider": provider,
                "model": model,
                "session_id": session.id,
                "branch": None,
            }
        )
    return {
        "id": leader_id,
        "pack": pack["name"],
        "root": str(root) if root else None,
        "status": "running",
        "roles": roles,
        "tasks": [],
        "jobs": [],
        "next_task": 1,
        "next_job": 1,
        "models": models,
        "is_confirming": False,
        "log": [],
    }


def add_task(swarm, title, text=""):
    """Queue a new task at the first role and return it."""
    task = {
        "id": f"T{swarm['next_task']}",
        "title": str(title).strip()[:200],
        "text": str(text or "").strip(),
        "stage": 0,
        "state": "waiting",
        "commit": None,
        "branch": None,
        "feedback": None,
        "bounces": 0,
        "summaries": [],
    }
    swarm["next_task"] += 1
    swarm["tasks"].append(task)
    note(swarm, f"{task['id']} added: {task['title']}")
    return task


def _task(swarm, task_id):
    return next(task for task in swarm["tasks"] if task["id"] == task_id)


def _job(swarm, job_id):
    return next((job for job in swarm["jobs"] if job["id"] == job_id), None)


def _session(swarm, role_index):
    return vis.sessions.get(swarm["roles"][role_index]["session_id"])


def _read_turn(session, turn_id):
    """Read one turn record. Tests replace this seam."""
    from blockether.vis.engine import Turn

    return Turn(session, turn_id).read()


def _cancel_turn(session, turn_id):
    """Ask the engine to cancel one turn. Tests replace this seam."""
    from blockether.vis.engine import Turn

    return Turn(session, turn_id).cancel()


# -- prompts -----------------------------------------------------------------------------------


def work_prompt(swarm, role_index, tasks):
    role = swarm["roles"][role_index]
    label = f"swarm-{role['name']}"
    parts = [
        PROTOCOL.format(title=role["title"], pack=swarm["pack"], label=label),
        f"## Your role\n\n{role['prompt']}",
    ]
    for task in tasks:
        block = [f"## Task {task['id']}: {task['title']}"]
        if task["text"]:
            block.append(task["text"])
        if task["summaries"]:
            block.append("Work of the previous roles:")
            block += [
                f"- {one['role']}: {one['summary']}" for one in task["summaries"][-6:]
            ]
        if task["commit"] and task["branch"]:
            block.append(
                f"Merge first: run `git merge --no-edit {task['commit']}` in your draft. "
                f"The commit is on the branch `{task['branch']}`. Resolve each conflict."
            )
        if task["feedback"]:
            block.append(f"Feedback to fix:\n{task['feedback']}")
        parts.append("\n\n".join(block))
    if len(tasks) > 1:
        parts.append(
            "Do all these tasks in this one turn. Give one answer for all of them."
        )
    return "\n\n".join(parts)


# -- the pipeline ------------------------------------------------------------------------------


def _send(swarm, job, text):
    turn = _session(swarm, job["role"]).send(text)
    job["turn_id"] = turn.id
    job["state"] = "running"
    job["views"] = []


def start_job(swarm, role_index, tasks, kind="work"):
    job = {
        "id": f"J{swarm['next_job']}",
        "role": role_index,
        "tasks": [task["id"] for task in tasks],
        "kind": kind,
        "state": "running",
        "turn_id": None,
        "is_corrected": False,
        "summary": None,
        "question": None,
        "views": [],
    }
    swarm["next_job"] += 1
    swarm["jobs"].append(job)
    for task in tasks:
        task["state"] = "working" if kind == "work" else "landing"
        task["stage"] = role_index
    if kind == "work":
        text = work_prompt(swarm, role_index, tasks)
    else:
        names = ", ".join(f"{task['id']} ({task['title']})" for task in tasks)
        subject = (
            tasks[0]["title"]
            if len(tasks) == 1
            else f"swarm tasks {', '.join(job['tasks'])}"
        )
        text = LAND.format(
            tasks=names, message=f"feat: {subject}"[:72].replace('"', "'")
        )
    _send(swarm, job, text)
    who = swarm["roles"][role_index]["title"]
    note(
        swarm,
        f"{who} started {', '.join(job['tasks'])}"
        + (" (land)" if kind == "land" else ""),
    )
    return job


def _finish(swarm, job):
    swarm["jobs"] = [one for one in swarm["jobs"] if one["id"] != job["id"]]


def send_back(swarm, job, feedback, propagation):
    """Move the tasks of `job` to an earlier role, as `propagation` says."""
    role = job["role"]
    target = {"forward-only": role, "back-one": max(0, role - 1), "back-all": 0}[
        propagation
    ]
    tasks = [_task(swarm, task_id) for task_id in job["tasks"]]
    if any(task["bounces"] >= MAX_BOUNCES for task in tasks):
        job["state"], job["summary"] = (
            "blocked",
            f"Sent back {MAX_BOUNCES} times. {feedback}",
        )
        for task in tasks:
            task["state"] = "blocked"
        return
    for task in tasks:
        task["bounces"] += 1
        task["stage"], task["state"], task["feedback"] = target, "waiting", feedback
    _finish(swarm, job)
    note(
        swarm,
        f"{', '.join(job['tasks'])} went back to {swarm['roles'][target]['title']}",
    )


def apply_answer(swarm, job, answer):
    role = swarm["roles"][job["role"]]
    tasks = [_task(swarm, task_id) for task_id in job["tasks"]]
    last = job["role"] == len(swarm["roles"]) - 1
    job["summary"] = answer.summary
    if answer.status == "question":
        job["state"], job["question"] = "question", answer.question
        for task in tasks:
            task["state"] = "question"
        return
    if answer.status == "blocked":
        if (
            job["kind"] == "work"
            and job["role"] > 0
            and role["propagation"] != "forward-only"
        ):
            send_back(
                swarm,
                job,
                f"{role['title']} is blocked: {answer.summary}",
                role["propagation"],
            )
            return
        job["state"] = "blocked"
        for task in tasks:
            task["state"] = "blocked"
        return
    role["branch"] = answer.branch
    for task in tasks:
        task["summaries"].append({"role": role["title"], "summary": answer.summary})
        task["commit"], task["branch"], task["feedback"] = (
            answer.commit,
            answer.branch,
            None,
        )
    if job["kind"] == "land":
        for task in tasks:
            task["state"] = "landed"
        _finish(swarm, job)
        note(swarm, f"{', '.join(job['tasks'])} landed at {answer.commit[:7]}")
    elif last:
        job["state"] = "review"
        for task in tasks:
            task["state"] = "review"
        note(swarm, f"{', '.join(job['tasks'])} wait for approval")
    else:
        for task in tasks:
            task["stage"], task["state"] = job["role"] + 1, "waiting"
        _finish(swarm, job)


def check_job(swarm, job):
    """Read the turn of a running job and act on its end."""
    session = _session(swarm, job["role"])
    record = _read_turn(session, job["turn_id"])
    status = record.get("status")
    if status not in ("completed", "failed", "cancelled", "error"):
        try:
            job["views"] = [_input_view(view) for view in session.input_views()]
        except Exception:  # noqa: BLE001 - the next tick reads them again
            pass
        return
    job["views"] = []
    if status != "completed":
        job["state"], job["summary"] = (
            "blocked",
            f"The turn ended with the status {status}.",
        )
        for task_id in job["tasks"]:
            _task(swarm, task_id)["state"] = "blocked"
        return
    try:
        answer = parse_answer(record.get("content"))
    except ValueError as error:
        if job["is_corrected"]:
            job["state"], job["summary"] = "blocked", f"No valid answer: {error}"
            for task_id in job["tasks"]:
                _task(swarm, task_id)["state"] = "blocked"
            return
        job["is_corrected"] = True
        _send(swarm, job, CORRECTION.format(error=error))
        return
    job["is_corrected"] = False
    apply_answer(swarm, job, answer)


def schedule(swarm):
    """Give each free role the tasks that wait for it."""
    for index, role in enumerate(swarm["roles"]):
        if any(
            job["role"] == index and job["state"] in ACTIVE for job in swarm["jobs"]
        ):
            continue
        waiting = [
            t for t in swarm["tasks"] if t["stage"] == index and t["state"] == "waiting"
        ]
        if waiting:
            start_job(
                swarm, index, waiting if role["receive"] == "batch" else waiting[:1]
            )


def tick(swarm_id):
    """Run one step of the pipeline of `swarm_id`. Return False when the swarm stopped."""
    with _LOCK:
        swarm = load(swarm_id)
        if not swarm or swarm["status"] != "running":
            return False
        before = json.dumps(swarm, sort_keys=True)
        for job in list(swarm["jobs"]):
            if job["state"] in ("running", "landing") and job["turn_id"]:
                try:
                    check_job(swarm, job)
                except Exception as error:  # noqa: BLE001 - one failed read must not stop the swarm
                    note(swarm, f"Read failed for {job['id']}: {error}"[:300])
        try:
            schedule(swarm)
        except Exception as error:  # noqa: BLE001
            note(swarm, f"Start failed: {error}"[:300])
        if json.dumps(swarm, sort_keys=True) != before:
            save(swarm)
        return True


def _worker(swarm_id, panel):
    while True:
        time.sleep(TICK_S)
        try:
            if panel.is_interrupted or not tick(swarm_id):
                return
        except Exception as error:  # noqa: BLE001 - the next tick tries again
            vis.log("warn", f"Swarm {swarm_id[:8]} tick failed: {error}")


def start_worker(swarm_id):
    """Start the pipeline thread of `swarm_id` unless one runs for its current board."""
    panel = (_BOARDS.get(swarm_id) or {}).get("panel")
    running = _WORKERS.get(swarm_id)
    if panel is None or (running and running[1] is panel and running[0].is_alive()):
        return
    thread = threading.Thread(
        target=_worker,
        args=(swarm_id, panel),
        name=f"swarm-{swarm_id[:8]}",
        daemon=True,
    )
    _WORKERS[swarm_id] = (thread, panel)
    thread.start()


# -- human actions -----------------------------------------------------------------------------


def approve(swarm, job_id):
    job = _job(swarm, job_id)
    if not job or job["state"] != "review":
        return
    _finish(swarm, job)
    start_job(
        swarm,
        job["role"],
        [_task(swarm, task_id) for task_id in job["tasks"]],
        kind="land",
    )


def reject(swarm, job_id, comment):
    job = _job(swarm, job_id)
    if not job or job["state"] != "review":
        return
    role = swarm["roles"][job["role"]]
    feedback = (
        f"The person rejected the result: {str(comment or '').strip() or 'No comment.'}"
    )
    for task_id in job["tasks"]:
        _task(swarm, task_id)["bounces"] = 0
    send_back(swarm, job, feedback, role["propagation"])


def reply(swarm, job_id, text):
    job = _job(swarm, job_id)
    if job and job["state"] == "question" and str(text or "").strip():
        for task_id in job["tasks"]:
            _task(swarm, task_id)["state"] = "working"
        _send(swarm, job, REPLY.format(text=str(text).strip()))


def retry(swarm, job_id):
    job = _job(swarm, job_id)
    if job and job["state"] == "blocked":
        _finish(swarm, job)
        tasks = [_task(swarm, task_id) for task_id in job["tasks"]]
        for task in tasks:
            task["bounces"] = 0
        start_job(swarm, job["role"], tasks, kind=job["kind"])


def drop(swarm, job_id):
    job = _job(swarm, job_id)
    if job and job["state"] in ("blocked", "question", "review"):
        for task_id in job["tasks"]:
            _task(swarm, task_id)["state"] = "dropped"
        _finish(swarm, job)
        note(swarm, f"{', '.join(job['tasks'])} dropped")


def answer_form(swarm, job_id, index, values):
    job = _job(swarm, job_id)
    if not job or index >= len(job["views"]):
        return
    view = job["views"].pop(index)
    prefix = _prefix(job_id, index)
    clean = {
        key[len(prefix) :]: value
        for key, value in values.items()
        if key.startswith(prefix)
    }
    _session(swarm, job["role"]).answer(view["id"], clean)


def set_role_model(swarm, role_name, provider=None, model=None):
    role = next(one for one in swarm["roles"] if one["name"] == role_name)
    if provider is not None and (provider or DEFAULT) != (role["provider"] or DEFAULT):
        role["provider"] = None if provider in ("", DEFAULT) else provider
        names = (swarm.get("models") or {}).get(role["provider"]) or []
        role["model"] = names[0][0] if names else None
    if model is not None:
        role["model"] = None if model in ("", DEFAULT) else model
    if bool(role["provider"]) == bool(role["model"]):
        vis.sessions.get(role["session_id"]).set_model(role["provider"], role["model"])


def teardown(swarm_id, reason="Teardown"):
    """Stop the swarm: cancel running turns and close its views. Role sessions stay."""
    with _LOCK:
        swarm = load(swarm_id)
        if not swarm:
            return None
        for job in swarm["jobs"]:
            if job["state"] in ("running", "landing") and job["turn_id"]:
                try:
                    _cancel_turn(_session(swarm, job["role"]), job["turn_id"])
                except Exception:  # noqa: BLE001 - a finished turn cannot be cancelled
                    pass
        swarm["status"] = "stopped"
        forget(swarm_id)
        board = _BOARDS.pop(swarm_id, None) or {}
        if board.get("panel"):
            try:
                board["panel"].close(reason="completed", summary=reason)
            except Exception:  # noqa: BLE001
                pass
        _WORKERS.pop(swarm_id, None)
        return swarm


def on_event(swarm_id, event):
    """Handle one human action on the board of `swarm_id`."""
    node = str(event.node_id or "")
    values = dict(event.values or {})
    with _LOCK:
        swarm = load(swarm_id)
        if not swarm or swarm["status"] != "running":
            return
        verb, _, rest = node.partition(":")
        if event.type == "change" and verb in ("provider", "model"):
            if verb == "provider":
                set_role_model(swarm, rest, provider=event.value or "")
            else:
                set_role_model(swarm, rest, model=event.value or "")
        elif node == "add-task" and str(values.get("task-title") or "").strip():
            add_task(swarm, values["task-title"], values.get("task-text"))
            schedule(swarm)
            _clear_form(swarm_id)
        elif verb == "approve":
            approve(swarm, rest)
        elif verb == "reject":
            reject(swarm, rest, values.get(f"comment:{rest}"))
        elif verb == "reply":
            reply(swarm, rest, values.get(f"answer:{rest}"))
        elif verb == "retry":
            retry(swarm, rest)
        elif verb == "drop":
            drop(swarm, rest)
        elif verb == "form":
            job_id, _, index = rest.rpartition(":")
            answer_form(swarm, job_id, int(index), values)
        elif node == "teardown":
            swarm["is_confirming"] = True
        elif node == "teardown-cancel":
            swarm["is_confirming"] = False
        elif node == "teardown-confirm":
            teardown(swarm_id)
            return
        else:
            return
        save(swarm)


def _clear_form(swarm_id):
    panel = (_BOARDS.get(swarm_id) or {}).get("panel")
    if panel is not None:
        panel["task-title"].clear()
        panel["task-text"].clear()


# -- the board ---------------------------------------------------------------------------------

TONES = {
    "waiting": "idle",
    "working": "running",
    "landing": "running",
    "review": "warn",
    "question": "warn",
    "blocked": "error",
    "landed": "ok",
    "dropped": "idle",
}
LABELS = {
    "waiting": "Waiting",
    "working": "Working",
    "landing": "Landing",
    "review": "Needs approval",
    "question": "Has a question",
    "blocked": "Blocked",
    "landed": "Landed",
    "dropped": "Dropped",
}


def counts(swarm):
    tasks = swarm["tasks"]
    attention = [
        job
        for job in swarm["jobs"]
        if job["state"] in ("review", "question", "blocked") or job["views"]
    ]
    return {
        "working": sum(task["state"] in ("working", "landing") for task in tasks),
        "attention": len(attention),
        "landed": sum(task["state"] == "landed" for task in tasks),
        "open": sum(task["state"] not in ("landed", "dropped") for task in tasks),
    }


def summary_line(swarm):
    found = counts(swarm)
    return (
        f"{found['open']} open, {found['working']} working, "
        f"{found['attention']} need you, {found['landed']} landed"
    )


def summary_stats(swarm):
    """Return the counters of the board summary, each with a tone."""
    found = counts(swarm)
    shown = (
        ("open", "Open", "idle"),
        ("working", "Working", "running"),
        ("attention", "Need you", "warn"),
        ("landed", "Landed", "ok"),
    )
    return [
        {
            "id": key,
            "label": label,
            "value_text": str(found[key]),
            "tone": tone if found[key] else "idle",
        }
        for key, label, tone in shown
    ]


def _prefix(job_id, index):
    return f"q-{job_id}-{index}-"


def _input_view(view):
    """Keep one `InputView` as plain data for the board."""
    return {
        "id": view.id,
        "title": view.title,
        "description": view.description,
        "nodes": json.loads(json.dumps([dict(node) for node in view.nodes])),
        "submit_label": view.submit_label,
    }


def _renamed(nodes, prefix):
    out = []
    for node in nodes:
        node = dict(node)
        for field in ("id", "name"):
            if node.get(field):
                node[field] = prefix + str(node[field])
        if isinstance(node.get("children"), list):
            node["children"] = _renamed(node["children"], prefix)
        if node.get("type") == "button":
            continue
        out.append(node)
    return out


def _attention(swarm):
    items = []
    for job in swarm["jobs"]:
        role = swarm["roles"][job["role"]]
        tasks = [_task(swarm, task_id) for task_id in job["tasks"]]
        name = ", ".join(f"{task['id']} {task['title']}" for task in tasks)
        jid = job["id"]
        if job["state"] == "review":
            items.append(
                vis.ui.group(
                    f"att:{jid}",
                    vis.ui.card(
                        f"att-card:{jid}",
                        name,
                        text=job["summary"] or "",
                        tone="warn",
                        meta=(tasks[0]["commit"] or "")[:7] or role["title"],
                    ),
                    vis.ui.row(
                        f"att-actions:{jid}",
                        vis.ui.button(f"approve:{jid}", "Approve"),
                        vis.ui.button(f"drop:{jid}", "Drop"),
                    ),
                    # A submit button collects the inputs of its nearest group.
                    vis.ui.group(
                        f"att-reject:{jid}",
                        vis.ui.multiline(f"comment:{jid}", label="Comment"),
                        vis.ui.button(f"reject:{jid}", "Reject", action="submit"),
                    ),
                )
            )
        elif job["state"] == "question":
            items.append(
                vis.ui.group(
                    f"att:{jid}",
                    vis.ui.card(
                        f"att-card:{jid}",
                        name,
                        text=job["question"] or "",
                        tone="warn",
                        meta=role["title"],
                    ),
                    vis.ui.multiline(f"answer:{jid}", label="Answer", is_required=True),
                    vis.ui.button(f"reply:{jid}", "Send answer", action="submit"),
                    vis.ui.button(f"drop:{jid}", "Drop"),
                )
            )
        elif job["state"] == "blocked":
            items.append(
                vis.ui.group(
                    f"att:{jid}",
                    vis.ui.card(
                        f"att-card:{jid}",
                        name,
                        text=job["summary"] or "",
                        tone="error",
                        meta=role["title"],
                    ),
                    vis.ui.row(
                        f"att-actions:{jid}",
                        vis.ui.button(f"retry:{jid}", "Retry"),
                        vis.ui.button(f"drop:{jid}", "Drop"),
                    ),
                )
            )
        for index, view in enumerate(job["views"]):
            prefix = _prefix(jid, index)
            body = _renamed(view["nodes"], prefix) or [
                vis.ui.paragraph(f"{prefix}empty", "The form has no fields.")
            ]
            items.append(
                vis.ui.group(
                    f"att:{jid}:{index}",
                    vis.ui.card(
                        f"att-card:{jid}:{index}",
                        view["title"],
                        text=view.get("description") or "",
                        tone="warn",
                        meta=name,
                    ),
                    *body,
                    vis.ui.button(
                        f"form:{jid}:{index}",
                        view.get("submit_label") or "Send",
                        action="submit",
                    ),
                )
            )
    if not items:
        items = [vis.ui.paragraph("att-none", "Nothing needs you now.")]
    return vis.ui.group("attention", *items, label="Attention")


def _lanes(swarm):
    lanes = []
    last = len(swarm["roles"]) - 1
    for index, role in enumerate(swarm["roles"]):
        here = [
            task
            for task in swarm["tasks"]
            if task["stage"] == index
            and task["state"] != "dropped"
            and (task["state"] != "landed" or index == last)
        ][-MAX_CARDS:]
        cards = [
            vis.ui.card(
                f"card:{role['name']}:{task['id']}",
                f"{task['id']} {task['title']}",
                text=LABELS[task["state"]],
                tone=TONES[task["state"]],
                **({"meta": task["commit"][:7]} if task["commit"] else {}),
            )
            for task in here
        ] or [vis.ui.paragraph(f"lane-empty:{role['name']}", "No tasks.")]
        lanes.append(vis.ui.group(f"lane:{role['name']}", *cards, label=role["title"]))
    return vis.ui.group("lanes", *lanes, direction="row", label="Lanes")


def _queue(swarm):
    rows = [
        vis.ui.table_row(
            task["id"],
            [f"{task['id']} {task['title']}", LABELS[task["state"]]],
            tone=TONES[task["state"]],
        )
        for task in swarm["tasks"]
    ]
    return vis.ui.table(
        "queue",
        columns=[
            vis.ui.table_column("task", "Task"),
            vis.ui.table_column("state", "State"),
        ],
        rows=rows,
        label="Work queue",
    )


def _roles(swarm):
    models = swarm.get("models") or {}
    rows = []
    for role in swarm["roles"]:
        providers = [vis.ui.option(DEFAULT, "Session default")] + [
            vis.ui.option(name) for name in models
        ]
        names = models.get(role["provider"]) or []
        choices = [vis.ui.option(DEFAULT, "Session default")] + [
            vis.ui.option(n, label) for n, label in names
        ]
        rows.append(
            vis.ui.group(
                f"role:{role['name']}",
                vis.ui.select(
                    f"provider:{role['name']}",
                    providers,
                    label="Provider",
                    value=role["provider"] or DEFAULT,
                ),
                vis.ui.select(
                    f"model:{role['name']}",
                    choices,
                    label="Model",
                    value=role["model"] or DEFAULT,
                ),
                direction="row",
                label=role["title"],
            )
        )
    return vis.ui.group("roles", *rows, label="Roles")


def _new_task():
    return vis.ui.group(
        "new-task",
        vis.ui.plaintext("task-title", label="Title", is_required=True),
        vis.ui.multiline("task-text", label="Details"),
        vis.ui.button("add-task", "Add task", action="submit"),
        label="New task",
    )


def _teardown(swarm):
    if swarm.get("is_confirming"):
        return vis.ui.group(
            "end",
            vis.ui.paragraph(
                "end-text",
                "Teardown cancels running turns and closes the board. "
                "The role sessions and their drafts stay.",
            ),
            vis.ui.row(
                "end-actions",
                vis.ui.button("teardown-confirm", "Confirm teardown"),
                vis.ui.button("teardown-cancel", "Keep swarm"),
            ),
            label="Teardown",
        )
    return vis.ui.group("end", vis.ui.button("teardown", "Teardown"))


SECTIONS = ("summary", "attention", "lanes", "queue", "roles", "end")


def board_sections(swarm):
    return {
        "summary": vis.ui.stat("summary", stats=summary_stats(swarm)),
        "attention": _attention(swarm),
        "lanes": _lanes(swarm),
        "queue": _queue(swarm),
        "roles": _roles(swarm),
        "end": _teardown(swarm),
    }


def board_nodes(swarm):
    sections = board_sections(swarm)
    nodes = [sections[name] for name in SECTIONS[:4]]
    return nodes + [_new_task(), sections["roles"], sections["end"]]


def open_board(swarm):
    """Open or replace the panel board of `swarm`."""
    swarm_id = swarm["id"]
    panel = vis.live(
        "Swarm Forge",
        board_nodes(swarm),
        placement="panel",
        key="swarm-board",
        session_id=swarm_id,
        on_event=lambda event: on_event(swarm_id, event),
    )
    _BOARDS[swarm_id] = {"panel": panel, "painted": board_sections(swarm)}
    return panel


def paint(swarm):
    """Replace the board sections that changed."""
    board = _BOARDS.get(swarm["id"])
    if not board or swarm["status"] != "running":
        return
    fresh = board_sections(swarm)
    old = board["painted"]
    panel = board["panel"]
    order = list(SECTIONS[:4]) + ["new-task", "roles", "end"]
    try:
        with panel.batch():
            if fresh["summary"] != old.get("summary"):
                # The summary is the first node, so it changes in place.
                for item in summary_stats(swarm):
                    panel["summary"].set(
                        item["id"], item["value_text"], item["label"], item["tone"]
                    )
            for name in SECTIONS[1:]:
                if fresh[name] != old.get(name):
                    panel.drop(name)
                    panel.add(fresh[name], after=order[order.index(name) - 1])
        board["painted"] = fresh
    except vis.Interrupted:
        _BOARDS.pop(swarm["id"], None)


# -- commands, tools and restore ---------------------------------------------------------------


def start(leader_id, pack_name, root=None):
    """Start a swarm for leader session `leader_id`. Only a callback can call it."""
    with _LOCK:
        if load(leader_id):
            raise ValueError("this session already runs a swarm; use /swarm stop first")
        swarm = new_swarm(leader_id, load_pack(pack_name), root=root)
        vis.state[_key(leader_id)] = swarm
        vis.state["swarms"] = list(vis.state.get("swarms") or []) + [leader_id]
        try:
            open_board(swarm)
        except Exception:
            forget(leader_id)
            raise
    start_worker(leader_id)
    return swarm


def describe(swarm):
    lines = [f"Swarm Forge · {swarm['pack']} · {summary_line(swarm)}"]
    for role in swarm["roles"]:
        model = (
            f"{role['provider']}/{role['model']}"
            if role["provider"]
            else "session default"
        )
        lines.append(f"- {role['title']}: session {role['session_id']} · {model}")
    for task in swarm["tasks"]:
        stage = swarm["roles"][task["stage"]]["title"]
        lines.append(
            f"- {task['id']} {task['title']}: {LABELS[task['state']]} at {stage}"
        )
    return "\n".join(lines)


USAGE = "/swarm start <pack> [--root <path>] | /swarm task <title> | /swarm status | /swarm stop"


def _slash_swarm(ctx):
    args = list(ctx.get("args") or [])
    leader = ctx.get("session_id")
    verb = args[0] if args else "status"
    if not leader:
        return vis.err("Swarm Forge needs a session")
    try:
        if verb == "start":
            if len(args) < 2:
                return vis.err(f"Name a pack: {', '.join(pack_names())}")
            root = None
            if "--root" in args:
                at = args.index("--root")
                root = args[at + 1] if at + 1 < len(args) else None
                if not root or not Path(root).expanduser().is_dir():
                    return vis.err("--root needs an existing directory")
                root = str(Path(root).expanduser().resolve())
            swarm = start(leader, args[1], root=root)
            return vis.ok(f"Swarm {swarm['pack']} started", describe(swarm))
        swarm = load(leader)
        if not swarm:
            return vis.err("No swarm runs in this session. Use /swarm start <pack>.")
        if verb == "task":
            title = " ".join(args[1:]).strip()
            if not title:
                return vis.err("Give the task title: /swarm task <title>")
            with _LOCK:
                swarm = load(leader)
                task = add_task(swarm, title)
                save(swarm)
            return vis.ok(f"Task {task['id']} added", task["title"])
        if verb == "status":
            if leader not in _BOARDS:
                open_board(swarm)
            start_worker(leader)
            return vis.ok("Swarm status", describe(swarm))
        if verb == "stop":
            teardown(leader, reason="Stopped with /swarm stop")
            return vis.ok("Swarm stopped", "The role sessions and their drafts stay.")
    except ValueError as error:
        return vis.err(str(error))
    return vis.err(USAGE)


def _task_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return vis.ActivityPresentation(
        "Add swarm task", f"{result['id']} · {result['title'][:80]}"
    )


def _status_activity(*, phase, result, **_):
    if phase != "success":
        return None
    return vis.ActivityPresentation(
        "Read swarm status", str(result.get("summary") or "")[:200]
    )


class Swarm:
    """The swarm of the bound session, for the model of that session."""

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Add swarm task", show_start=False, render=_task_activity
        ),
    )
    def add_task(self, title: str, text: str = "") -> dict:
        """Queue a task at the first role of this session's swarm. Return the task."""
        leader = vis.session.id
        with _LOCK:
            swarm = load(leader)
            if not swarm:
                raise RuntimeError(
                    "no swarm runs in this session; the person starts one with /swarm start"
                )
            task = add_task(swarm, title, text)
            save(swarm)
        return {"id": task["id"], "title": task["title"]}

    @vis.method(
        tag="observation",
        activity=vis.Activity(
            label="Read swarm status", show_start=False, render=_status_activity
        ),
    )
    def status(self) -> dict:
        """Return the summary, the roles and the tasks of this session's swarm."""
        swarm = load(vis.session.id)
        if not swarm:
            return {
                "summary": "No swarm runs in this session.",
                "roles": [],
                "tasks": [],
            }
        return {
            "summary": summary_line(swarm),
            "roles": [
                {
                    key: role[key]
                    for key in ("name", "title", "provider", "model", "session_id")
                }
                for role in swarm["roles"]
            ],
            "tasks": [
                {key: task[key] for key in ("id", "title", "stage", "state", "commit")}
                for task in swarm["tasks"]
            ],
        }


swarm = Swarm()


def restore():
    """Open the board of each running swarm again, for example after `/reload`."""
    for swarm_id in list(vis.state.get("swarms") or []):
        try:
            found = load(swarm_id)
            if found and found["status"] == "running":
                open_board(found)
                start_worker(swarm_id)
        except Exception:  # noqa: BLE001 - one broken swarm must not stop the others
            continue


PROMPT = """swarm surface active — the Swarm Forge board of this session:
  swarm.add_task(title, text="")   queue a task at the first role
  swarm.status()                   summary, roles and tasks
The person starts, watches and approves the swarm on its board."""

vis.register_extension(
    vis.Extension(
        name="swarm_forge",
        description="Swarm Forge: role sessions in drafts that pass each task along, with a board.",
        version="0.1.0",
        kind="integration",
        alias="swarm",
        symbols=[vis.Symbol(swarm, name="swarm", tag="mutation")],
        prompt=PROMPT,
        slash_commands=[
            vis.SlashCommand(
                "swarm",
                _slash_swarm,
                doc="Start, show or stop a Swarm Forge pipeline.",
                usage=USAGE,
            )
        ],
    )
)

restore()
