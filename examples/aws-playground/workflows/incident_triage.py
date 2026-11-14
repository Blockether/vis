"""Incident triage: classify a report, then write what its severity needs."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Triage(BaseModel):
    severity: Literal["sev1", "sev2", "sev3", "sev4"] = Field(description="sev1: all customers down or data loss. sev2: a main function fails for many. sev3: a workaround exists. sev4: cosmetic")
    component: str
    customers_affected: bool
    summary: str
    immediate_actions: list[str]


@activity(title="Classify the incident")
def classify(ctx, report: str) -> Triage:
    return ctx.ask(f"You are the incident commander. Classify this report. Do not write code.\n\n{report}", Triage)


@activity(title="Customer update")
def customer_update(ctx, triage: Triage):
    return ctx.document("CUSTOMER_UPDATE.md", f"Write the status page update for customers about: {triage.summary} Keep it calm, exact and short.")


@activity(title="Postmortem draft")
def postmortem(ctx, triage: Triage):
    actions = "\n".join(f"- {item}" for item in triage.immediate_actions)
    return ctx.document(
        "POSTMORTEM.md",
        f"Write a blameless postmortem draft for the {triage.severity} incident in {triage.component}. "
        f"Sections: Impact, Timeline (with placeholders), Root cause hypotheses, What went well, Actions.\nActions so far:\n{actions}",
    )


@activity(title="Backlog ticket")
def ticket(ctx, triage: Triage):
    return ctx.document("TICKET.md", f"Write a backlog ticket for: {triage.summary} Sections: Problem, Steps to reproduce, Expected, Workaround.")


@workflow(title="Incident triage", description="Classify an incident report. Sev1 and sev2 get a customer update and a postmortem draft. The others get a ticket.")
def incident_triage(ctx, report: str = "Since 09:40 UTC the checkout page returns HTTP 502 for about 30% of the requests in eu-central-1. Card payments fail."):
    triage = classify(ctx, report)
    if triage.severity in ("sev1", "sev2"):
        made = [postmortem(ctx, triage).name]
        if triage.customers_affected:
            made.insert(0, customer_update(ctx, triage).name)
        return {"severity": triage.severity, "page_on_call": True, "files": made}
    return {"severity": triage.severity, "page_on_call": False, "files": [ticket(ctx, triage).name]}
