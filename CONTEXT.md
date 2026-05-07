# Vis

Vis is an agent runtime that turns conversation goals into tracked commitments, runtime evidence, and auditable decisions.

## Language

**Intent**:
A commitment Vis is tracking for a conversation.
_Avoid_: task, todo, goal

**Deferred Intent**:
An intent that Vis has accepted but parked until a future trigger or external action occurs.
_Avoid_: backlog item, reminder, active intent

**Backlog Item**:
A possible future improvement that Vis may remember but has not accepted as a commitment.
_Avoid_: deferred intent, obligation

**Plan**:
A strategy for resolving one intent.
_Avoid_: prompt, checklist

**Gate**:
A blocking proposition that must be resolved before a plan can complete.
_Avoid_: test, assertion, condition

**Attestation**:
A structured decision over evidence.
_Avoid_: proof, proof blob

**Audit**:
A validation pass that checks whether Vis state transitions are justified by evidence and attestations.
_Avoid_: proof check, report scrape

## Relationships

- An **Intent** has zero or more **Plans**.
- A **Plan** has zero or more **Gates**.
- A **Gate** is resolved by an **Attestation**.
- An **Intent** may be active, deferred, fulfilled, or abandoned.
- A **Deferred Intent** remains a commitment but must not block the current final answer solely because its trigger has not occurred.
- A **Backlog Item** is not an **Intent** until Vis or the user accepts it as a commitment.
- An **Audit** validates **Attestations**, **Gates**, **Plans**, and **Intents** against recorded evidence.

## Example dialogue

> **Dev:** "If the user says, 'When I paste the API key later, wire it into Telegram,' is that a todo?"
> **Domain expert:** "No. That is a **Deferred Intent**: Vis accepted a future commitment, but it waits for the user-provided key before resuming."

> **Dev:** "If the assistant says, 'We could add Slack alerts someday,' is that also a Deferred Intent?"
> **Domain expert:** "No. That is only a **Backlog Item** unless the user or system accepts it as a commitment."

## Flagged ambiguities

- "future thing to do" was ambiguous between **Deferred Intent** and **Backlog Item** — resolved: commitments are **Deferred Intents**; suggestions are **Backlog Items**.
