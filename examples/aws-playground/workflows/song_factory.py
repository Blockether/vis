"""Song factory: wait for a brief, write the song sheet, check it, then record the song as code and ship it to GitHub."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Inspection(BaseModel):
    verdict: Literal["pass", "rework"] = Field(
        description="pass: a musician can record the song from SONG.md alone. rework: SONG.md misses a wish of the brief, states one wrongly, or has a part that the studio cannot play"
    )
    score: int = Field(ge=0, le=100, description="How much of the brief is in SONG.md, as a percentage")
    gaps: list[str] = Field(description="Each wish that SONG.md misses or states wrongly. Empty for pass")


class Shipment(BaseModel):
    song: str = Field(description="The id of the new song in the catalog")
    tests_passed: bool = Field(description="True only when you ran the tests after the change and each test passed")
    branch: str
    commit: str
    url: str


@activity(title="Brief intake")
def intake(ctx):
    return ctx.receive("brief", "Give the brief of the song", accept=".md,.txt")


@activity(title="Write the song sheet")
def arrange(ctx, title: str, brief: str):
    return ctx.document(
        "SONG.md",
        f"You are a music producer. Write the song sheet of the song {title!r} from its brief. Keep each wish of the brief. "
        "Where the brief says nothing, make a choice and mark it as a choice. "
        "Sections: Idea, Key and tempo, Form (each section with its bars), Chords (one roman numeral for each bar), "
        "Tracks (name, General MIDI program number, role, rhythm), Drums (kick, snare and hi-hat pattern), Acceptance criteria. "
        "The studio plays 4/4, at most 4 tracks with pitches and one drum track, and at most 32 bars. Write at most 120 lines.\n\n"
        f"The brief:\n\n{brief}",
    )


@activity(title="Quality gate")
def inspect(ctx) -> Inspection:
    return ctx.ask(
        "You are the second producer. Read your song sheet again with `read_attachment('SONG.md').decode()`. "
        "Compare it with the brief of the first request, wish by wish. Do not change the song sheet.",
        Inspection,
    )


@activity(title="Rework the sheet")
def rework(ctx, inspection: Inspection):
    gaps = "\n".join(f"- {gap}" for gap in inspection.gaps)
    return ctx.document("SONG.md", f"Correct the song sheet. Attach the complete new version, not a list of changes. Close each gap:\n{gaps}")


@activity(title="Record and ship")
def record(ctx, title: str) -> Shipment:
    return ctx.ask(
        "Record the song as code. Read `src/studio/songs.py` in the project directory and the newest song sheet (`read_attachment('SONG.md').decode()`). "
        "Add one function that returns the parts of the song, in the style of `night_drive`: use `part`, `drums`, `line` and `roman.RomanNumeral`. "
        f"Add one `Song` for {title!r} to `CATALOG`, with a new id in lower case with hyphens. Do not change another song. "
        "Run `py.run_tests()` and read the result: the tests check each song of the catalog. Correct your code until each test passes. "
        f"Then call `playground.publish('Add the song {title}')` and report its result.",
        Shipment,
    )


@workflow(title="Song factory", description="Wait for a brief. Write SONG.md from it, check it against the brief, then record the song as music21 code and push it to GitHub.")
def song_factory(ctx, title: str = "Midnight train"):
    brief = intake(ctx)
    sheet = arrange(ctx, title, brief.text)
    inspection = inspect(ctx)
    if inspection.verdict == "rework":
        sheet = rework(ctx, inspection)
    shipment = record(ctx, title)
    ctx.note(f"Shipped the song {shipment.song} as {shipment.commit[:12]} to {shipment.branch}")
    return {"verdict": inspection.verdict, "score": inspection.score, "song": shipment.song, "commit": shipment.commit[:12], "url": shipment.url, "files": [sheet.name]}
