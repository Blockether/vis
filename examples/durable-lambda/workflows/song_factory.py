"""Song factory: write the song sheet from a prompt, wait for its approval, record the song as code, wait for the approval of the song, then ship it to GitHub."""

from pydantic import BaseModel, Field

from durable.workflows import activity, workflow

# The number of decisions that a person can make at each of the two gates.
ROUNDS = 3


class Work(BaseModel):
    song: str = Field(description="The id of the new song in the catalog")
    files_changed: list[str]
    tests_passed: bool = Field(description="True only when you ran the tests after the last change and each test passed")
    summary: str = Field(description="What the song is and what you changed, in at most three sentences")


class Shipment(BaseModel):
    branch: str
    commit: str
    url: str


@activity(title="Write the song sheet")
def specify(ctx, prompt: str):
    return ctx.document(
        "SONG.md",
        "You are a music producer. Write the song sheet of one new song from this prompt. Keep each wish of the prompt. "
        "Where the prompt says nothing, make a choice and mark it as a choice. "
        "Sections: Title, Idea, Key and tempo, Form (each section with its bars), Chords (one roman numeral for each bar), "
        "Tracks (name, General MIDI program number, role, rhythm), Drums (kick, snare and hi-hat pattern), Acceptance criteria. "
        "The studio plays 4/4, at most 4 tracks with pitches and one drum track, and at most 32 bars. Write at most 120 lines. Do not write code.\n\n"
        f"The prompt:\n\n{prompt}",
    )


@activity(title="Approve the song sheet")
def approve_sheet(ctx, sheet, attempt: int):
    return ctx.gate(f"sheet_{attempt}", "Approve the song sheet", about="Read SONG.md. Approve it to start the recording. To get a change, reject it and write the change.", document=sheet)


@activity(title="Rework the song sheet")
def rework(ctx, comment: str):
    change = comment or "Find the weakest part of the song sheet and make it better."
    return ctx.document("SONG.md", f"A person read the song sheet and did not approve it. Attach the complete new version, not a list of changes. Make this change:\n\n{change}")


@activity(title="Record the song")
def record(ctx, comment: str = "") -> Work:
    if comment:
        request = f"A person looked at the song in the code and did not approve it. Make this change in the code of the song:\n\n{comment}\n\n"
    else:
        request = (
            "Record the song as code. Read `src/studio/songs.py` in the project directory and the newest song sheet (`read_attachment('SONG.md').decode()`). "
            "Add one function that returns the parts of the song, in the style of `night_drive`: use `part`, `drums`, `line` and `roman.RomanNumeral`. "
            "Add one `Song` to `CATALOG`, with the title of the song sheet and a new id in lower case with hyphens. Do not change another song. "
        )
    return ctx.ask(
        request + "Run `py.run_tests()` and read the result: the tests check each song of the catalog. Correct your code until each test passes. "
        "Do not call `durable.publish`: a person approves the song first.",
        Work,
    )


@activity(title="Approve the song")
def approve_song(ctx, work: Work, attempt: int):
    tests = "Each test passed." if work.tests_passed else "The tests did NOT pass."
    about = f"{work.summary} {tests} Changed files: {', '.join(work.files_changed)}. The Review app of the session shows each change. Approve the song to ship it to GitHub. To get a change, reject it and write the change."
    return ctx.gate(f"song_{attempt}", "Approve the song", about=about)


@activity(title="Ship to GitHub")
def ship(ctx, work: Work) -> Shipment:
    return ctx.ask(
        f"A person approved the song {work.song!r}. Call `durable.publish(message)`. The message is one commit subject of at most 72 characters for the new song. Report its result.", Shipment
    )


@workflow(
    title="Song factory", description="Write SONG.md from a prompt. A person approves the song sheet. Record the song as music21 code and test it. A person approves the song. Then push it to GitHub."
)
def song_factory(ctx, prompt: str = "A slow night-train song in a minor key: a warm electric piano, a walking bass and brushed drums. About one minute."):
    sheet = specify(ctx, prompt)
    for attempt in range(1, ROUNDS + 1):
        decision = approve_sheet(ctx, sheet, attempt)
        if decision.approved:
            break
        if attempt == ROUNDS:
            return {"shipped": False, "reason": f"A person rejected the song sheet {ROUNDS} times", "files": [sheet.name]}
        sheet = rework(ctx, decision.comment)
    work = record(ctx)
    for attempt in range(1, ROUNDS + 1):
        decision = approve_song(ctx, work, attempt)
        if decision.approved:
            break
        if attempt == ROUNDS:
            return {"shipped": False, "reason": f"A person rejected the song {ROUNDS} times", "song": work.song, "files": [sheet.name]}
        work = record(ctx, decision.comment)
    shipment = ship(ctx, work)
    ctx.note(f"Shipped the song {work.song} as {shipment.commit[:12]} to {shipment.branch}")
    return {"shipped": True, "song": work.song, "tests_passed": work.tests_passed, "branch": shipment.branch, "commit": shipment.commit[:12], "url": shipment.url, "files": [sheet.name]}
