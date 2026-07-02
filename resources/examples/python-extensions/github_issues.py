"""GitHub issues — give the model read access to a repo's issue tracker.

A real-world integration: extension contexts are TRUSTED, so plain
`urllib` network calls work here (unlike the model's sandbox, where
network is off by default). Demonstrates:
  * tools doing real HTTP from the trusted context
  * configuration via a slash command + `vis.state`
    (`/gh-repo owner/repo` — persisted, survives restarts)
  * secrets via environment variables (GITHUB_TOKEN, optional —
    without it public repos still work at a lower rate limit)
"""

import json
import os
import urllib.request

import vis

API = "https://api.github.com"


def _repo():
    repo = os.environ.get("GITHUB_REPO") or vis.state.get("repo")
    if not repo:
        raise ValueError(
            "no repo configured — run /gh-repo owner/repo (or set GITHUB_REPO)"
        )
    return repo


def _get(path):
    req = urllib.request.Request(
        API + path,
        headers={
            "Accept": "application/vnd.github+json",
            "User-Agent": "vis-python-extension",
            **(
                {"Authorization": "Bearer " + os.environ["GITHUB_TOKEN"]}
                if os.environ.get("GITHUB_TOKEN")
                else {}
            ),
        },
    )
    with urllib.request.urlopen(req, timeout=15) as resp:
        return json.loads(resp.read().decode("utf-8"))


def gh_issue(number):
    """await gh_issue(number) -> {"number", "title", "state", "labels", "body", "url"} — fetch one issue."""
    d = _get(f"/repos/{_repo()}/issues/{int(number)}")
    return {
        "number": d["number"],
        "title": d["title"],
        "state": d["state"],
        "labels": [l["name"] for l in d.get("labels", [])],
        "author": d.get("user", {}).get("login"),
        "body": (d.get("body") or "")[:4000],
        "comments": d.get("comments", 0),
        "url": d["html_url"],
    }


def gh_issues(state="open"):
    """await gh_issues(state="open") -> {"issues": [{"number", "title", "labels"}, ...]} — list issues (state: open|closed|all)."""
    ds = _get(f"/repos/{_repo()}/issues?state={state}&per_page=30")
    return {
        "repo": _repo(),
        "issues": [
            {
                "number": d["number"],
                "title": d["title"],
                "labels": [l["name"] for l in d.get("labels", [])],
            }
            for d in ds
            if "pull_request" not in d  # issues only, not PRs
        ],
    }


def _slash_repo(ctx):
    args = ctx["args"]
    if args:
        vis.state["repo"] = args[0]
        return vis.ok(f"GitHub repo set: {args[0]}")
    repo = vis.state.get("repo")
    return vis.ok(f"GitHub repo: {repo}" if repo else "No repo set — /gh-repo owner/repo")


PROMPT = """gh_ surface active — read access to the configured GitHub repo's issues.
  gh_issue(number)   gh_issues(state="open"|"closed"|"all")
Use when the user references an issue by number or asks what's open."""


vis.extension(
    name="github-issues",
    description="Read GitHub issues for the configured repo.",
    version="0.1.0",
    kind="integration",
    alias="gh",
    symbols=[
        vis.symbol(gh_issue, tag="observation"),
        vis.symbol(gh_issues, tag="observation"),
    ],
    prompt=PROMPT,
    slash_commands=[
        vis.slash("gh-repo", _slash_repo, doc="Set or show the GitHub repo.",
                  usage="/gh-repo owner/repo"),
    ],
)
