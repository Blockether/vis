"""Mix triage: classify a problem report about a track, then write what its severity needs."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Triage(BaseModel):
    severity: Literal["blocker", "major", "minor", "cosmetic"] = Field(
        description="blocker: the track cannot go out. major: a main part sounds wrong. minor: a small part sounds wrong. cosmetic: a matter of taste"
    )
    track: str
    release_at_risk: bool
    summary: str
    immediate_actions: list[str]


@activity(title="Classify the report")
def classify(ctx, report: str) -> Triage:
    return ctx.ask(f"You are the mix engineer of a MIDI studio. Classify this problem report. Do not write code.\n\n{report}", Triage)


@activity(title="Message to the artist")
def artist_update(ctx, triage: Triage):
    return ctx.document("ARTIST_UPDATE.md", f"Write the message to the artist about: {triage.summary} Keep it calm, exact and short.")


@activity(title="Revision plan")
def revision(ctx, triage: Triage):
    actions = "\n".join(f"- {item}" for item in triage.immediate_actions)
    return ctx.document(
        "REVISION_PLAN.md",
        f"Write the revision plan for the {triage.severity} problem in the track {triage.track}. "
        f"Sections: What the listener hears, Possible causes (velocity, register, rhythm, instrument), Changes to try, How to check each change.\nActions so far:\n{actions}",
    )


@activity(title="Backlog ticket")
def ticket(ctx, triage: Triage):
    return ctx.document("TICKET.md", f"Write a backlog ticket for: {triage.summary} Sections: Problem, Where to hear it, Expected sound, Workaround.")


@workflow(title="Mix triage", description="Classify a problem report about a track. A blocker or a major problem gets a revision plan and a message to the artist. The others get a ticket.")
def mix_triage(ctx, report: str = "In `night-drive` the bass and the kick drum mask each other, and the arpeggio is louder than the pad. The release is on Friday."):
    triage = classify(ctx, report)
    if triage.severity in ("blocker", "major"):
        made = [revision(ctx, triage).name]
        if triage.release_at_risk:
            made.insert(0, artist_update(ctx, triage).name)
        return {"severity": triage.severity, "hold_release": True, "files": made}
    return {"severity": triage.severity, "hold_release": False, "files": [ticket(ctx, triage).name]}
