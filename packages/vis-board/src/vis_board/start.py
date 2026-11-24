"""Start the work of an issue that a person moves to Implementing."""

import json
import logging
import re
import subprocess
import urllib.request
from dataclasses import dataclass
from pathlib import Path

import blockether.vis.extension as vis

LOG = logging.getLogger(__name__)

START_COLUMN = "implementing"
DONE_COLUMN = "done"

WORKSPACE = vis.Setting(
    id="board_start_workspace",
    label="Start works in",
    type="enum",
    default="worktree",
    choices=("worktree", "branch", "folder"),
    description="Where a started issue works: a new git worktree beside the project, "
    "a new branch in the project folder, or the project folder as it is.",
)
ASSIGNEE = vis.Setting(
    id="board_start_assignee",
    label="Start gives the issue to",
    type="enum",
    default="agent",
    choices=("agent", "person"),
    description="agent: a Vis session implements the issue and moves it to "
    "Verification. person: the issue keeps its assignee and no session starts.",
)
SYNC = vis.Setting(
    id="board_sync",
    label="Sync started issues with",
    type="enum",
    default="off",
    choices=("off", "github", "linear"),
    description="Start makes an issue in GitHub or Linear, and Done closes it.",
)
LINEAR_TEAM = vis.Setting(
    id="board_linear_team",
    label="Linear team key",
    type="string",
    default="",
    parent="board_sync",
    description="The key of the Linear team of new issues, for example ENG.",
)
LINEAR_KEY = vis.Setting(
    id="board_linear_api_key",
    label="Linear API key",
    type="string",
    default="",
    is_secret=True,
    parent="board_sync",
    description="A personal API key from the Linear security settings.",
)
SETTINGS = (WORKSPACE, ASSIGNEE, SYNC, LINEAR_TEAM, LINEAR_KEY)

AGENT = vis.ui.board.person("vis-agent", "Vis agent", color="#7c3aed")


@dataclass(frozen=True)
class Plan:
    """The Start choices of the settings."""

    workspace: str = "worktree"
    assignee: str = "agent"
    sync: str = "off"
    linear_team: str = ""
    linear_key: str = ""


def plan():
    """Read the Start choices. Call it in an extension callback."""
    return Plan(
        WORKSPACE.value(),
        ASSIGNEE.value(),
        SYNC.value(),
        LINEAR_TEAM.value().strip(),
        LINEAR_KEY.value().strip(),
    )


def slug(text, limit):
    """Lowercase words of `text` joined by hyphens, at most `limit` characters."""
    return (
        re.sub(r"[^a-z0-9]+", "-", str(text).casefold()).strip("-")[:limit].strip("-")
    )


def _run(folder, *argv):
    done = subprocess.run(
        argv,
        cwd=folder,
        capture_output=True,
        encoding="utf-8",
        errors="replace",
        timeout=120,
    )
    if done.returncode:
        message = (done.stderr or done.stdout).strip() or f"exit {done.returncode}"
        raise RuntimeError(f"{' '.join(argv[:3])}: {message}")
    return done.stdout.strip()


def git(folder, *args):
    """Run git in `folder` and return its output. A failure raises with its message."""
    return _run(folder, "git", *args)


def _has_branch(project, branch):
    try:
        git(project, "show-ref", "--verify", "--quiet", f"refs/heads/{branch}")
    except RuntimeError:
        return False
    return True


def project_folder(folder):
    """The main worktree of linked worktree `folder`, else `folder`.

    Each worktree of a project then shares the board of the project.
    """
    folder = Path(folder).resolve()
    if not (folder / ".git").is_file():
        return folder
    try:
        common = Path(
            git(folder, "rev-parse", "--path-format=absolute", "--git-common-dir")
        )
    except (OSError, subprocess.SubprocessError, RuntimeError):
        return folder
    return common.parent if common.name == ".git" else folder


def _name(issue):
    return slug(issue.get("key") or issue["id"], 20)


def workspace(project, issue, mode, record):
    """The folder and branch of the work of `issue`. A second Start reuses them."""
    project = Path(project)
    if mode == "folder":
        return str(project), None
    branch = record.get("branch") or "board/" + "-".join(
        part for part in (_name(issue), slug(issue["title"], 40)) if part
    )
    exists = _has_branch(project, branch)
    if mode == "branch":
        if git(project, "rev-parse", "--abbrev-ref", "HEAD") != branch:
            git(project, "switch", *([branch] if exists else ["-c", branch]))
        return str(project), branch
    folder = Path(
        record.get("workspace") or project.parent / f"{project.name}-{_name(issue)}"
    )
    if not folder.exists():
        place = [str(folder), branch] if exists else ["-b", branch, str(folder)]
        git(project, "worktree", "add", *place)
    elif str(folder) != record.get("workspace"):
        raise ValueError(f"The folder {folder} exists already.")
    return str(folder), branch


def _summary(issue):
    return issue.get("text") or "The issue has no description."


def github_issue(plan, project, issue):
    """Make a GitHub issue in the repository of `project`."""
    out = _run(
        project,
        "gh",
        "issue",
        "create",
        "--title",
        f"{issue.get('key', '')} {issue['title']}".strip(),
        "--body",
        _summary(issue),
    )
    url = out.splitlines()[-1].strip()
    return {"kind": "github", "id": url, "url": url}


def github_close(plan, project, external):
    """Close the GitHub issue of `external`."""
    _run(project, "gh", "issue", "close", external["id"])


LINEAR_URL = "https://api.linear.app/graphql"
TEAM = """query($key: String!) {
  teams(filter: {key: {eq: $key}}) {
    nodes { id states { nodes { id type position } } }
  }
}"""
CREATE = """mutation($input: IssueCreateInput!) {
  issueCreate(input: $input) { issue { id identifier url } }
}"""
UPDATE = """mutation($id: String!, $input: IssueUpdateInput!) {
  issueUpdate(id: $id, input: $input) { success }
}"""


def linear(plan, query, variables):
    """Send one GraphQL request to Linear and return its data."""
    if not (plan.linear_key and plan.linear_team):
        raise ValueError("Linear sync needs the Linear team key and API key settings.")
    request = urllib.request.Request(
        LINEAR_URL,
        data=json.dumps({"query": query, "variables": variables}).encode(),
        headers={"Content-Type": "application/json", "Authorization": plan.linear_key},
    )
    with urllib.request.urlopen(request, timeout=30) as response:
        answer = json.load(response)
    if answer.get("errors"):
        raise RuntimeError(answer["errors"][0].get("message", "Linear refused it."))
    return answer["data"]


def _linear_state(plan, kind):
    teams = linear(plan, TEAM, {"key": plan.linear_team})["teams"]["nodes"]
    if not teams:
        raise ValueError(f"No Linear team has the key {plan.linear_team}.")
    states = [one for one in teams[0]["states"]["nodes"] if one["type"] == kind]
    first = min(states, key=lambda one: one["position"], default=None)
    return teams[0]["id"], first and first["id"]


def linear_issue(plan, project, issue):
    """Make a started Linear issue in the team of the settings."""
    team, state = _linear_state(plan, "started")
    made = {"teamId": team, "title": issue["title"], "description": _summary(issue)}
    if state:
        made["stateId"] = state
    data = linear(plan, CREATE, {"input": made})["issueCreate"]["issue"]
    return {"kind": "linear", "id": data["id"], "url": data["url"]}


def linear_close(plan, project, external):
    """Move the Linear issue of `external` to the first completed state."""
    _, state = _linear_state(plan, "completed")
    if not state:
        raise ValueError("The Linear team has no completed state.")
    linear(plan, UPDATE, {"id": external["id"], "input": {"stateId": state}})


MAKERS = {"github": github_issue, "linear": linear_issue}
CLOSERS = {"github": github_close, "linear": linear_close}


def brief(issue, record):
    """The first request of the agent session of `issue`."""
    key = issue.get("key") or issue["id"]
    place = f"Work in {record['workspace']}"
    if record.get("branch"):
        place += f" on the branch {record['branch']}"
    lines = [f"Implement the board issue {key}: {issue['title']}", "", _summary(issue)]
    lines += ["", place + "."]
    if external := record.get("external"):
        lines.append(f"The issue is also {external['url']}.")
    lines.append(
        "When the work is ready for review, move the issue to Verification with "
        f'`board.move_issue("{key}", "verification", 0)` and summarize the change.'
    )
    return "\n".join(lines)


def _find(data, issue_id):
    return next(one for one in data["issues"] if one["id"] == issue_id)


def _record(store, issue_id, record, *, is_agent=False):
    def apply(data):
        data.setdefault("starts", {})[issue_id] = record
        if is_agent:
            if not any(one["id"] == AGENT["id"] for one in data["people"]):
                data["people"].append(AGENT)
            _find(data, issue_id)["assignee"] = AGENT["id"]

    store.change(apply)


def start(store, issue_id, plan, *, sessions=None, log=LOG.warning):
    """Start the work of issue `issue_id` of `store` as `plan` says.

    The workspace, the external issue and the agent session go to the `starts` of
    the board, so a second Start reuses them. A failed sync is logged and the Start
    goes on. Return the record.
    """
    data = store.read()
    issue = _find(data, issue_id)
    record = dict(data.get("starts", {}).get(issue_id) or {})
    if record.get("mode") != plan.workspace:
        record = {"external": record["external"]} if "external" in record else {}
    folder, branch = workspace(store.workspace, issue, plan.workspace, record)
    record.update(mode=plan.workspace, workspace=folder, branch=branch)
    if plan.sync in MAKERS and (record.get("external") or {}).get("kind") != plan.sync:
        try:
            record["external"] = MAKERS[plan.sync](plan, store.workspace, issue)
        except Exception as error:
            log(f"{issue.get('key', issue_id)}: the {plan.sync} sync failed: {error}")
    is_agent = plan.assignee == "agent"
    if not is_agent:
        _record(store, issue_id, record)
        return record
    sessions = sessions or vis.sessions
    request = brief(issue, record)
    if record.get("session"):
        session = sessions.get(record["session"])
        request = "The issue is back in Implementing. Continue the work.\n\n" + request
    else:
        session = sessions.create(
            root=folder,
            title=f"{issue.get('key', '')} {issue['title']}".strip()[:200],
            idempotency_key=f"vis-board-{issue_id}-{plan.workspace}",
        )
        record["session"] = session.id
    _record(store, issue_id, record, is_agent=True)
    session.send(request)
    return record


def finish(store, issue_id, plan, *, log=LOG.warning):
    """Close the external issue of done issue `issue_id`, once."""
    record = dict(store.read().get("starts", {}).get(issue_id) or {})
    external = record.get("external")
    if not external or record.get("is_closed"):
        return
    try:
        CLOSERS[external["kind"]](plan, store.workspace, external)
    except Exception as error:
        log(f"Closing {external['url']} failed: {error}")
        return
    record["is_closed"] = True
    _record(store, issue_id, record)
