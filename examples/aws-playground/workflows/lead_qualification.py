"""Lead qualification: score an inbound message and write the next step."""

from typing import Literal

from playground.workflows import activity, workflow
from pydantic import BaseModel, Field


class Lead(BaseModel):
    company: str
    segment: Literal["enterprise", "mid-market", "startup", "unknown"]
    need: str = Field(description="The problem that the sender wants to solve, in one sentence")
    fit_score: int = Field(ge=0, le=100, description="How well a team that builds AI agent software fits this need")
    next_step: Literal["proposal", "discovery_call", "decline"] = Field(
        description="proposal: the need and the budget are clear. discovery_call: the fit is likely, facts are missing. decline: no fit"
    )
    reason: str


@activity(title="Qualify the lead")
def qualify(ctx, message: str) -> Lead:
    return ctx.ask(f"You are a sales engineer at a company that builds AI agent software. Qualify this inbound message. Do not write code.\n\n{message}", Lead)


@activity(title="Proposal outline")
def proposal(ctx, lead: Lead):
    return ctx.document("PROPOSAL.md", f"Write a proposal outline for {lead.company}. Need: {lead.need} Sections: Understanding, Approach, Milestones, Team, Assumptions.")


@activity(title="Discovery call agenda")
def agenda(ctx, lead: Lead):
    return ctx.document("CALL_AGENDA.md", f"Write a 30 minute discovery call agenda for {lead.company}. List the facts that are missing to make a proposal.")


@activity(title="Decline message")
def decline(ctx, lead: Lead):
    return ctx.document("DECLINE.md", f"Write a short and kind message that declines the request of {lead.company}. Reason: {lead.reason}")


@workflow(title="Lead qualification", description="Score an inbound sales message. Write a proposal outline, a call agenda or a decline message.")
def lead_qualification(
    ctx,
    message: str = "Hi, we are a logistics company with 400 dispatchers. We want an agent that reads carrier emails and updates our TMS. Budget is approved for Q3.",
):
    lead = qualify(ctx, message)
    step = {"proposal": proposal, "discovery_call": agenda, "decline": decline}[lead.next_step]
    return {"company": lead.company, "fit_score": lead.fit_score, "next_step": lead.next_step, "artifacts": [step(ctx, lead).name]}
