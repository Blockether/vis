"""Brief to arrangement: analyse a music brief, decide, then write the arrangement."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Analysis(BaseModel):
    summary: str = Field(description="What the brief asks for, in two sentences")
    mood: str
    key: str = Field(description="The key that fits the mood, such as `A minor`")
    bpm: int = Field(ge=40, le=220)
    instruments: list[str]
    open_questions: list[str] = Field(description="Questions that block the work. Empty when the brief is clear")
    complexity: Literal["low", "medium", "high"]
    decision: Literal["compose", "clarify", "reject"] = Field(description="compose: the brief is clear. clarify: a blocking question is open. reject: MIDI instruments cannot play the brief")
    reason: str


@activity(title="Analysis of the brief")
def analyse(ctx, brief: str) -> Analysis:
    return ctx.ask(f"You are a music producer. Analyse this brief for a piece of MIDI music. Do not write code.\n\n{brief}", Analysis)


@activity(title="Write the arrangement")
def arrangement(ctx, analysis: Analysis):
    instruments = ", ".join(analysis.instruments)
    return ctx.document(
        "ARRANGEMENT.md",
        f"Write the arrangement for the brief that you analysed, in {analysis.key} at {analysis.bpm} bpm, for: {instruments}. "
        "Sections: Idea, Form (each section with its bars), Chords (one roman numeral for each bar), "
        "Tracks (a table: name, General MIDI program number, register, rhythm), Drums, Dynamics.",
    )


@activity(title="Mix notes")
def mix_notes(ctx):
    return ctx.document("MIX_NOTES.md", "Write mix notes for ARRANGEMENT.md: one table row for each track with its MIDI velocity range and its pan, then the parts that can mask each other.")


@activity(title="Questions for the artist")
def ask_artist(ctx, analysis: Analysis):
    questions = "\n".join(f"- {item}" for item in analysis.open_questions)
    return ctx.document("QUESTIONS.md", f"Write a short message to the artist. Explain why each question blocks the work:\n{questions}")


@workflow(title="Brief to arrangement", description="Analyse a music brief. Compose: write ARRANGEMENT.md and the mix notes. Clarify: write the questions.")
def brief_to_arrangement(ctx, brief: str = "A calm lo-fi piece for a study playlist: a soft electric piano, a round bass and a slow drum machine. About one minute."):
    analysis = analyse(ctx, brief)
    if analysis.decision == "reject":
        ctx.note(f"Rejected: {analysis.reason}")
        return {"decision": "reject", "reason": analysis.reason}
    if analysis.decision == "clarify":
        return {"decision": "clarify", "files": [ask_artist(ctx, analysis).name]}
    made = [arrangement(ctx, analysis).name]
    if analysis.complexity != "low":
        made.append(mix_notes(ctx).name)
    return {"decision": "compose", "key": analysis.key, "bpm": analysis.bpm, "complexity": analysis.complexity, "files": made}
