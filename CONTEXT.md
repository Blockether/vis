# Vis

Vis is an AI coding environment that preserves conversations, turns, iterations, and diagnostic evidence so agents and humans can inspect how work happened.

## Language

**User Request**:
The exact human-authored text submitted to Vis for one turn.
_Avoid_: Query, Goal, Prompt

**Turn**:
One unit of work initiated by a **User Request**, including the agent's iterations and final outcome.
_Avoid_: Query

**Goal**:
An interpreted objective derived from a **User Request**, only present when Vis explicitly derives one.
_Avoid_: User Request, Query

## Relationships

- A **Turn** is initiated by exactly one **User Request**.
- A **Goal** may be derived from a **User Request**, but the raw **User Request** is not itself the **Goal**.

## Example dialogue

> **Dev:** "Should `v/inspect` show the user's raw text as `:goal`?"
> **Domain expert:** "No — that is the **User Request**. A **Goal** would be an interpreted objective, and only exists if Vis derives one explicitly."

## Flagged ambiguities

- "goal" was used to mean the raw human text for a turn — resolved: use **User Request** for raw human-authored text; reserve **Goal** for an explicitly derived objective.
- "query" was used as a legacy implementation term for **Turn** and **User Request** — resolved: avoid it in public/domain surfaces.
