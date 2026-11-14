"""Story to SPEC: analyse a user story, decide, then write the specification."""

from typing import Literal

from playground.workflows import activity, workflow
from pydantic import BaseModel, Field


class Analysis(BaseModel):
    summary: str = Field(description="What the story asks for, in two sentences")
    actors: list[str]
    acceptance_criteria: list[str]
    open_questions: list[str] = Field(description="Questions that block the work. Empty when the story is clear")
    risk: Literal["low", "medium", "high"]
    decision: Literal["build", "clarify", "reject"] = Field(
        description="build: the story is clear. clarify: a blocking question is open. reject: the story has no value or is not possible"
    )
    reason: str


@activity(title="Analysis of the story")
def analyse(ctx, story: str) -> Analysis:
    return ctx.ask(f"You are a product analyst. Analyse this user story. Do not write code.\n\n{story}", Analysis)


@activity(title="Generating SPEC")
def write_spec(ctx, analysis: Analysis):
    criteria = "\n".join(f"- {item}" for item in analysis.acceptance_criteria)
    return ctx.document(
        "SPEC.md",
        "Write the specification for the story that you analysed. Sections: Goal, Actors, Scope, Out of scope, "
        f"Functional requirements, Data, Errors, Acceptance criteria.\nThe acceptance criteria are:\n{criteria}",
    )


@activity(title="Test plan")
def plan_tests(ctx):
    return ctx.document("TEST_PLAN.md", "Write a test plan for SPEC.md: one table row for each acceptance criterion, then the edge cases.")


@activity(title="Questions for the product owner")
def ask_owner(ctx, analysis: Analysis):
    questions = "\n".join(f"- {item}" for item in analysis.open_questions)
    return ctx.document("QUESTIONS.md", f"Write a short message to the product owner. Explain why each question blocks the work:\n{questions}")


@workflow(title="Story to SPEC", description="Analyse a user story. Build: write SPEC.md and a test plan. Clarify: write the questions.")
def story_to_spec(ctx, story: str = "As a shop owner, I want to export my orders as a CSV file, so that my accountant can import them."):
    analysis = analyse(ctx, story)
    if analysis.decision == "reject":
        ctx.note(f"Rejected: {analysis.reason}")
        return {"decision": "reject", "reason": analysis.reason}
    if analysis.decision == "clarify":
        return {"decision": "clarify", "artifacts": [ask_owner(ctx, analysis).name]}
    made = [write_spec(ctx, analysis).name]
    if analysis.risk != "low":
        made.append(plan_tests(ctx).name)
    return {"decision": "build", "risk": analysis.risk, "artifacts": made}
