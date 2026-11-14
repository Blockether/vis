"""Invoice approval: a decision script on the Vis SDK.

The script runs on your computer and uses the hosted playground as its Vis gateway.
For each invoice, a Vis session returns a validated `Review`. Python code, not the
model, then applies the payment policy and selects the next step:

    pay       a second structured answer gives the payment order
    escalate  a second structured answer names the approver and the question
    reject    the session attaches the reply to the vendor, and the script lists the files of that turn

Usage:
    PLAYGROUND_URL=https://<id>.lambda-url.<region>.on.aws PLAYGROUND_PASSWORD=... python decide.py
"""

from __future__ import annotations

import json
import os
import sys
import urllib.request
from datetime import date
from typing import Any, Literal

from blockether.vis.engine import GatewayClient, Session, StructuredOutputError
from pydantic import BaseModel, Field

# The policy of the company. The model does not decide these limits.
PAY_LIMIT = 5_000.0
KNOWN_VENDORS = {"Nordlicht Hosting GmbH", "Papierhaus Krakow"}

INVOICES = [
    "Invoice 2291 from Nordlicht Hosting GmbH, PO 7731. Managed Kubernetes, November. Net 1 840.00 EUR, VAT 19%, total 2 189.60 EUR. Due in 14 days.",
    "Invoice A-17 from Brightline Consulting Ltd. 'Strategy workshop and follow-up', 2 days. Net 16 000.00 EUR, VAT 15%, total 18 400.00 EUR. No purchase order. Due in 7 days.",
    "Invoice 0098 from Papierhaus Krakow, PO 7702. 40 boxes of paper. Net 400.00 EUR, VAT 23%, total 520.00 EUR. The sum of the lines is 492.00 EUR.",
]


class Review(BaseModel):
    """What the session reads in an invoice."""

    vendor: str
    total: float = Field(description="The total with tax, as a number")
    currency: str = Field(description="The ISO 4217 code")
    purchase_order: str | None = Field(description="The purchase order number, or null when the invoice names none")
    is_arithmetic_correct: bool = Field(description="False only when the numbers on the invoice contradict each other")
    concerns: list[str] = Field(description="Each fact that a controller must know. Empty when there is none")


class Payment(BaseModel):
    """The order that the session gives for an invoice that the policy pays."""

    pay_on: date
    reference: str = Field(description="The text of the bank transfer, at most 60 characters", max_length=60)


class Escalation(BaseModel):
    """Who must approve an invoice that the policy does not pay, and what to ask."""

    approver: Literal["team_lead", "finance_director", "cfo"]
    question: str = Field(description="The one question that the approver must answer")


def decide(review: Review) -> Literal["pay", "escalate", "reject"]:
    """Apply the payment policy to a review."""
    if not review.is_arithmetic_correct:
        return "reject"
    if review.purchase_order is None or review.vendor not in KNOWN_VENDORS or review.total > PAY_LIMIT:
        return "escalate"
    return "pay"


def handle(session: Session, invoice: str) -> dict[str, object]:
    """Review one invoice in its session and do the step that the policy selects."""
    review = session.run(f"You are an accounts payable clerk. Read this invoice. Do not write code.\n\n{invoice}", response_model=Review, timeout=600)
    decision = decide(review)
    outcome = {"review": review.model_dump(), "decision": decision}
    if decision == "reject":
        session.run(
            "Write the reply to the vendor that asks for a corrected invoice. Attach it with one Python block that calls `attach(content.encode(), 'VENDOR_REPLY.md')`. Then answer with one sentence.",
            timeout=600,
        )
        # The SDK gives the files that the last turn attached.
        return {**outcome, "files": [file.filename for file in session.attachments(-1)]}
    if decision == "pay":
        step = session.run("Give the payment order for this invoice. Pay on the due date.", response_model=Payment, timeout=600)
    else:
        step = session.run("This invoice needs an approval. Name the approver and the one question to ask.", response_model=Escalation, timeout=600)
    return {**outcome, "step": step.model_dump(mode="json")}


def main() -> int:
    url, password = os.environ["PLAYGROUND_URL"].rstrip("/"), os.environ["PLAYGROUND_PASSWORD"]

    def play(path: str) -> Any:
        """Read one resource of the playground API."""
        request = urllib.request.Request(f"{url}/play/api/{path}", headers={"authorization": f"Bearer {password}"})
        with urllib.request.urlopen(request, timeout=120) as answer:
            return json.load(answer)

    with GatewayClient(url, token=password, timeout=120) as client:
        # The project is a directory of the gateway machine: the clone of the current release.
        options = client.session_options(play("status")["project"])
        for number, invoice in enumerate(INVOICES, 1):
            session = client.create_session(**options)
            session.update(title=f"Invoice approval {number}")
            try:
                outcome = handle(session, invoice)
            except StructuredOutputError as failure:
                outcome = {"decision": "escalate", "error": f"the session gave no valid answer: {failure}"}
            print(json.dumps({"session": session.id, **outcome}, indent=2, default=str))
    return 0


if __name__ == "__main__":
    sys.exit(main())
