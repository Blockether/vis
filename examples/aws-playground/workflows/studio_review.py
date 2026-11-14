"""Studio review: run the checks of the studio, read each song and write a review."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Finding(BaseModel):
    song: str = Field(description="The id of the song, or the file for a problem of the code")
    severity: Literal["low", "medium", "high"]
    problem: str


class Inspection(BaseModel):
    songs: list[str] = Field(description="The id of each song of the catalog")
    tests_passed: bool
    lint_clean: bool
    findings: list[Finding] = Field(description="Real problems that you saw in the music, in the code or in the tool output")


@activity(title="Inspect the studio")
def inspect(ctx) -> Inspection:
    return ctx.ask(
        "Inspect the project directory. Run `py.run_tests()` and `py.lint_code()`. Read `src/studio/songs.py`. "
        "For each song of `CATALOG`, look for a part that ends in another bar than the other parts, a bass that plays above the melody, "
        "a pitch that its instrument cannot play and a bar without a note. Do not change a file.",
        Inspection,
    )


@activity(title="Review report")
def report(ctx, inspection: Inspection):
    return ctx.document("REVIEW.md", f"Write the review of the studio. Start with a table of the {len(inspection.findings)} findings, sorted by severity. Then write one line for each song.")


@activity(title="Fix plan")
def fix_plan(ctx):
    return ctx.document("FIX_PLAN.md", "Write a fix plan for the findings with high severity: one section for each, with the function to change and the test to add.")


@workflow(title="Studio review", description="Run the tests and the lint of the studio and read each song. Write REVIEW.md. High findings also get a fix plan.")
def studio_review(ctx):
    inspection = inspect(ctx)
    made = [report(ctx, inspection).name]
    high = [finding for finding in inspection.findings if finding.severity == "high"]
    if high or not inspection.tests_passed:
        made.append(fix_plan(ctx).name)
    return {"songs": len(inspection.songs), "findings": len(inspection.findings), "high": len(high), "tests_passed": inspection.tests_passed, "files": made}
