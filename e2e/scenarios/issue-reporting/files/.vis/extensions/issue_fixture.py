"""Issue #239: local GitHub and Jira stand-ins; never contact either service."""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass
from pathlib import Path

import blockether.vis.extension as sdk


@dataclass(frozen=True, slots=True)
class CreatedIssue:
    number: int
    url: str


def search_activity(*, phase, result, **_):
    if phase == "success":
        return sdk.ActivityPresentation(
            "Search GitHub issues", f"{len(result['matches'])} matches"
        )
    return None


def create_activity(*, phase, result, **_):
    if phase == "success":
        return sdk.ActivityPresentation(
            "Create GitHub issue",
            f"Issue #{result.number}",
            (sdk.ActivityText(result.url),),
        )
    return None


class GitHubIssues:
    def __init__(self):
        self._ledger = Path(__file__).resolve().parents[2] / "issue-events.jsonl"
        self._created = None
        self._searched = False

    def _record(self, event):
        with self._ledger.open("a") as ledger:
            ledger.write(json.dumps(event, sort_keys=True) + "\n")

    @sdk.method(
        activity=sdk.Activity(
            label="Search GitHub issues", show_start=False, render=search_activity
        )
    )
    def issue_find(self, repo: str, query: str) -> dict[str, list[str]]:
        """Search GitHub issues in blockether/vis. Returns a mapping of matches."""
        if repo.lower() != "blockether/vis" or not query.strip():
            raise ValueError("Specify blockether/vis and a nonblank search query")
        self._searched = True
        self._record({"op": "find", "repo": repo.lower()})
        return {"matches": [self._created.url] if self._created else []}

    @sdk.method(
        tag="mutation",
        activity=sdk.Activity(
            label="Create GitHub issue", show_start=False, render=create_activity
        ),
    )
    def issue_create(self, repo: str, title: str, body: str) -> CreatedIssue:
        """Create one GitHub issue after duplicate search. Only the local ledger changes."""
        if repo.lower() != "blockether/vis" or not title.strip() or not body.strip():
            raise ValueError("Specify blockether/vis, a nonblank title and body")
        if not self._searched:
            raise ValueError("Search for duplicates before creating an issue")
        number = 240 if self._created else 239
        self._created = CreatedIssue(
            number, f"https://github.com/Blockether/vis/issues/{number}"
        )
        self._record({"op": "create", **asdict(self._created)})
        return self._created


class JiraIssues:
    @sdk.method(activity=sdk.Activity(label="Search Jira issues", show_start=False))
    def issue_find(self, project: str, query: str) -> dict[str, list[str]]:
        """Search a Jira project, not a GitHub repository. BLOCKETHER is not a project."""
        raise ValueError(
            "No matching Jira project; GitHub repository names are not keys"
        )


sdk.register_extension(
    sdk.Extension(
        name="issue-fixture",
        description="Local issue-reporting fixture with separate GitHub and Jira tools.",
        alias="issue_fixture",
        symbols=[
            sdk.Symbol(GitHubIssues(), name="vis"),
            sdk.Symbol(JiraIssues(), name="jira"),
        ],
    )
)
