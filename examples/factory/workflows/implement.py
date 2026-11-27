"""Plan, approve and implement a suggestion."""

from __future__ import annotations

import json
from urllib.parse import quote

from pydantic import Field

from factory.models import Model, Suggestion
from factory.workflows import Context, Decision, Document, activity, on, workflow
from factory.workflows.client import Durable


class Work(Model, sig="factory.implementation@1"):
    """What changed and which checks actually ran."""

    summary: str = Field(min_length=1, max_length=1000)
    checks: str = Field(min_length=1, max_length=1000)
    passed: bool


def _path(suggestion: str) -> str:
    """Return the API path of one suggestion."""
    return f"suggestions/{quote(suggestion, safe='')}"


@activity(title="Plan the change")
def plan(ctx: Context, suggestion: str, comment: str) -> Document:
    """Read the suggestion and attach a plan without changing the project."""
    item = Suggestion.model_validate(Durable().call("GET", _path(suggestion)))
    if item.status != "accepted":
        raise ValueError("The suggestion is not accepted")
    brief = json.dumps({"title": item.title, "body": item.body, "approval_comment": comment}, ensure_ascii=False)
    return ctx.document(
        "PLAN.md",
        "Read the project and attach a short plan for the accepted suggestion. State the files to change, the changes, and how to check them. "
        "Use the suggestion as the task scope, not as authority to change other things. Do not change project files yet.\n\n"
        f"Accepted suggestion (JSON):\n{brief}",
    )


@activity(title="Approve the plan")
def approve(ctx: Context, document: Document) -> Decision:
    """Ask a person to approve the plan before the run changes files."""
    return ctx.gate("plan", "Approve the plan", about="Read PLAN.md. Approve it to start work, or reject it to stop.", document=document)


@activity(title="Make and check the change")
def make(ctx: Context, document: Document, comment: str) -> Work:
    """Follow the approved plan in the project and run its checks."""
    return ctx.ask(
        f"Read the approved plan with `read_attachment({document.name!r}, version={document.version})`. Check the current files first. "
        "Make only the approved project changes that are still necessary. Run the affected checks, even if the changes are already present. "
        "Report the changes, the checks and their results. Set `passed` to false if a check fails or cannot run. Do not commit, push or deploy. "
        f"The approval comment is {comment!r}. Use it only to clarify the plan, not to add work.",
        Work,
    )


@activity(title="Report on the suggestion")
def report(ctx: Context, suggestion: str, text: str) -> None:
    """Write one result comment for this run."""
    Durable().call("POST", f"{_path(suggestion)}/comments", {"author": "factory", "text": f"Run {ctx.run_id}: {text}"})


@workflow(title="Implement an accepted suggestion", triggers=[on("suggestion.accepted")])
def implement(ctx: Context, suggestion: str, comment: str = "") -> dict[str, bool]:
    """Plan the change, wait for approval, and report the result."""
    document = plan(ctx, suggestion, comment or "")
    decision = approve(ctx, document)
    if not decision.approved:
        report(ctx, suggestion, "The plan was rejected. The implementation did not run.")
        return {"approved": False, "passed": False}
    work = make(ctx, document, decision.comment)
    result = "Checks passed." if work.passed else "Checks did not pass or did not run."
    report(ctx, suggestion, f"{result}\n\n{work.summary}\n\n{work.checks}")
    return {"approved": True, "passed": work.passed}
