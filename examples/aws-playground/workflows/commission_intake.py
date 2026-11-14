"""Commission intake: score a request for music and write the next step."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Commission(BaseModel):
    client: str
    kind: Literal["jingle", "soundtrack", "song", "unknown"]
    need: str = Field(description="The music that the sender wants, in one sentence")
    fit_score: int = Field(ge=0, le=100, description="How well a studio that writes MIDI music as code fits this need")
    next_step: Literal["offer", "call", "decline"] = Field(description="offer: the need, the budget and the date are clear. call: the fit is likely, facts are missing. decline: no fit")
    reason: str


@activity(title="Qualify the request")
def qualify(ctx, message: str) -> Commission:
    return ctx.ask(f"You are the producer of a studio that writes MIDI music as code. Qualify this request. Do not write code.\n\n{message}", Commission)


@activity(title="Offer outline")
def offer(ctx, commission: Commission):
    return ctx.document(
        "OFFER.md", f"Write an offer outline for {commission.client}. Need: {commission.need} Sections: Understanding, Musical idea, Deliveries (MIDI files and stems), Dates, Assumptions."
    )


@activity(title="Call agenda")
def agenda(ctx, commission: Commission):
    return ctx.document("CALL_AGENDA.md", f"Write a 30 minute call agenda for {commission.client}. List the facts that are missing to make an offer.")


@activity(title="Decline message")
def decline(ctx, commission: Commission):
    return ctx.document("DECLINE.md", f"Write a short and kind message that declines the request of {commission.client}. Reason: {commission.reason}")


@workflow(title="Commission intake", description="Score a request for music. Write an offer outline, a call agenda or a decline message.")
def commission_intake(
    ctx,
    message: str = "Hello, we make a podcast about cycling. We need a 20 second intro jingle and a 5 second sting, happy and acoustic. We can pay 400 EUR and need it in two weeks.",
):
    commission = qualify(ctx, message)
    step = {"offer": offer, "call": agenda, "decline": decline}[commission.next_step]
    return {"client": commission.client, "fit_score": commission.fit_score, "next_step": commission.next_step, "files": [step(ctx, commission).name]}
