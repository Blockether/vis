"""Change and publish: make a change in the repository, test it, then push it to GitHub."""

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Change(BaseModel):
    files_changed: list[str]
    tests_passed: bool = Field(description="True only when you ran the tests after the change and each test passed")
    summary: str


class Publication(BaseModel):
    branch: str
    commit: str
    url: str


@activity(title="Implement the change")
def implement(ctx, request: str) -> Change:
    return ctx.ask(
        f"Make this change in the project directory:\n\n{request}\n\nAdd or change a test for it. Then run `py.run_tests()` for Python code or `clj.lint_code()` for Clojure code and read the result.",
        Change,
    )


@activity(title="Publish to GitHub")
def publish(ctx, change: Change) -> Publication:
    return ctx.ask(f"Call `playground.publish(message)`. The message is one commit subject of at most 72 characters for this change: {change.summary} Report its result.", Publication)


@activity(title="Failure report")
def failure(ctx):
    return ctx.document("FAILURE.md", "The tests did not pass, so nothing was published. Write what failed and what you would try next.")


@workflow(title="Change and publish", description="Make a small change in the repository. When the tests pass, push it to GitHub. Else write a failure report.")
def change_and_publish(ctx, request: str = "Add a function `slugify(text)` to the Python package that makes a URL slug, with tests."):
    change = implement(ctx, request)
    if not change.tests_passed:
        return {"published": False, "files": [failure(ctx).name]}
    publication = publish(ctx, change)
    ctx.note(f"Published {publication.commit[:12]} to {publication.branch}")
    return {"published": True, "branch": publication.branch, "url": publication.url, "files": change.files_changed}
