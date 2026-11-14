"""Remix and publish: change a song of the studio, test it, then push it to GitHub."""

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Remix(BaseModel):
    songs_changed: list[str] = Field(description="The id of each song that you changed")
    files_changed: list[str]
    tests_passed: bool = Field(description="True only when you ran the tests after the change and each test passed")
    summary: str


class Publication(BaseModel):
    branch: str
    commit: str
    url: str


@activity(title="Remix the song")
def remix(ctx, request: str) -> Remix:
    return ctx.ask(
        f"The songs of the project are music21 code in `src/studio/songs.py`. Make this change:\n\n{request}\n\n"
        "Add or change a test for it in `tests/test_songs.py`. Then run `py.run_tests()` and read the result.",
        Remix,
    )


@activity(title="Publish to GitHub")
def publish(ctx, change: Remix) -> Publication:
    return ctx.ask(f"Call `playground.publish(message)`. The message is one commit subject of at most 72 characters for this change: {change.summary} Report its result.", Publication)


@activity(title="Failure report")
def failure(ctx):
    return ctx.document("FAILURE.md", "The tests did not pass, so nothing was published. Write what failed and what you would try next.")


@workflow(title="Remix and publish", description="Change a song of the studio. When the tests pass, push it to GitHub. Else write a failure report.")
def remix_and_publish(ctx, request: str = "Give the round `row-row` a third voice: a bassoon (General MIDI program 70) that starts two bars after the flute, one octave lower."):
    change = remix(ctx, request)
    if not change.tests_passed:
        return {"published": False, "files": [failure(ctx).name]}
    publication = publish(ctx, change)
    ctx.note(f"Published {publication.commit[:12]} to {publication.branch}")
    return {"published": True, "songs": change.songs_changed, "branch": publication.branch, "url": publication.url, "files": change.files_changed}
