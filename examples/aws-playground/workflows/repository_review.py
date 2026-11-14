"""Repository review: run the checks of the repository and write a review."""

from typing import Literal

from playground.workflows import activity, workflow
from pydantic import BaseModel, Field


class Finding(BaseModel):
    file: str
    severity: Literal["low", "medium", "high"]
    problem: str


class Inspection(BaseModel):
    languages: list[str]
    python_tests_passed: bool
    clojure_lint_clean: bool
    findings: list[Finding] = Field(description="Real problems that you saw in the code or in the tool output")


@activity(title="Inspect the repository")
def inspect(ctx) -> Inspection:
    return ctx.ask(
        "Inspect the project directory. List the files. Run `py.run_tests()` and `py.lint_code()` for the Python code and "
        "`clj.lint_code()` for the Clojure code. Read the source files. Do not change a file.",
        Inspection,
    )


@activity(title="Review report")
def report(ctx, inspection: Inspection):
    return ctx.document("REVIEW.md", f"Write the review of the repository. Start with a table of the {len(inspection.findings)} findings, sorted by severity.")


@activity(title="Fix plan")
def fix_plan(ctx):
    return ctx.document("FIX_PLAN.md", "Write a fix plan for the findings with high severity: one section for each, with the files to change and the test to add.")


@workflow(title="Repository review", description="Run the Python and Clojure checks of the repository. Write REVIEW.md. High findings also get a fix plan.")
def repository_review(ctx):
    inspection = inspect(ctx)
    made = [report(ctx, inspection).name]
    high = [finding for finding in inspection.findings if finding.severity == "high"]
    if high or not inspection.python_tests_passed:
        made.append(fix_plan(ctx).name)
    return {"findings": len(inspection.findings), "high": len(high), "tests_passed": inspection.python_tests_passed, "artifacts": made}
