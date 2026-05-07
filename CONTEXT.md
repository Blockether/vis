# Vis

Vis is an agent runtime that turns conversation goals into tracked commitments, runtime evidence, and auditable decisions.

## Language

**Intent**:
A commitment Vis is tracking for a conversation.
_Avoid_: task, todo, goal

**Deferred Intent**:
An intent that Vis has accepted but parked until a future trigger or external action occurs.
_Avoid_: backlog item, reminder, active intent

**Suggested Intent**:
An extension-proposed intent that is not yet accepted as a Vis commitment.
_Avoid_: deferred intent, active intent, backlog item

**Intent Acceptance**:
The user or host-owned system policy decision that turns a suggested intent into a Vis commitment.
_Avoid_: extension self-acceptance, hidden activation

**Extension-Owned Intent**:
An intent proposed or managed by one extension on behalf of a conversation.
_Avoid_: extension sidecar, extension aggregate, hidden task

**Intent Query**:
A durable read of intent state used by extensions to discover work they may affect.
_Avoid_: intent event, notification, prompt scrape

**Active Intent**:
An intent that currently blocks Vis from treating the focused work as complete.
_Avoid_: deferred intent, backlog item

**Running Intent**:
The single intent node Vis is currently executing in a conversation's intent tree.
_Avoid_: parallel intent, focused intent set

**Subintent**:
An intent nested under a parent intent that must be executed before returning to adjacent work at the parent level.
_Avoid_: parallel intent, plan step

**Intent Cursor**:
The current position in the intent tree; exactly one intent node is running at the cursor.
_Avoid_: focused intent set, parallel focus

**Intent Tree**:
A hierarchy of intents where execution descends into subintents before moving to adjacent sibling subintents.
_Avoid_: flat intent list, parallel queue

**Abandonment Gate**:
A required decision point that records why an intent branch is intentionally stopped before fulfillment.
_Avoid_: delete intent, silent skip, cancellation

**Abandonment Scope**:
The boundary of intent-tree work stopped by an abandonment decision.
_Avoid_: implicit cancellation, context switch guess

**Defer Trigger**:
The future condition that can make a deferred intent resumable.
_Avoid_: waiting gate, reminder

**Defer Sibling Policy**:
The rule that says whether sibling subintents may continue while one subintent is deferred.
_Avoid_: implicit dependency, hidden ordering

**Resumable Intent**:
A deferred intent whose trigger has been observed but has not yet become the running intent again.
_Avoid_: automatically resumed intent, hidden context switch

**Resume Decision**:
The user or host-owned system policy decision that moves a resumable intent back to the intent cursor.
_Avoid_: extension self-resume, hidden context switch

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
- An **Intent** may be suggested, deferred, active, fulfilled, or abandoned.
- An **Active Intent** blocks current completion until it is fulfilled, abandoned, or deferred.
- A conversation has exactly one **Intent Cursor** and therefore at most one **Running Intent** at a time.
- A **Subintent** is not parallel work; it becomes the **Running Intent** when the **Intent Cursor** descends into it.
- An **Intent Tree** executes depth-first: finish or defer/abandon the current subintent branch before moving to an adjacent sibling subintent.
- An intent is either decomposed into **Subintents** or resolved by its own **Plan** and **Gates**, not both.
- An **Abandonment Gate** records a user/system decision to stop work and requires evidence for the change of direction.
- An **Abandonment Scope** is explicit: current intent, current branch, or all running work; Vis asks when unclear.
- A **Deferred Intent** remains a commitment but must not block the current final answer solely because its trigger has not occurred.
- A **Deferred Intent** waits on exactly one **Defer Trigger** kind: user input, time, extension signal, or another intent.
- A **Deferred Intent** records a **Defer Sibling Policy**: continue sibling subintents or block the parent until resumed.
- A **Resumable Intent** requires a separate **Resume Decision** before becoming the **Running Intent** again.
- A **Resume Decision** may be made by the user or host-owned system policy, not silently by the owning extension.
- A **Suggested Intent** is stored with other **Intents** and may become a **Deferred Intent** only after **Intent Acceptance**.
- **Intent Acceptance** may be performed by the user or host-owned system policy, not silently by the owning extension.
- An **Extension-Owned Intent** names exactly one owning extension.
- Extensions discover **Suggested Intents** and **Deferred Intents** through **Intent Queries**, not intent transition events.
- A **Backlog Item** is not an **Intent** until Vis or the user accepts it as a commitment.
- An **Audit** validates **Attestations**, **Gates**, **Plans**, and **Intents** against recorded evidence.

## Example dialogue

> **Dev:** "If the user says, 'When I paste the API key later, wire it into Telegram,' is that a todo?"
> **Domain expert:** "No. That is a **Deferred Intent**: Vis accepted a future commitment, but it waits for the user-provided key before resuming."

> **Dev:** "If the assistant says, 'We could add Slack alerts someday,' is that also a Deferred Intent?"
> **Domain expert:** "No. That is only a **Backlog Item** unless the user or system accepts it as a commitment."

> **Dev:** "Can the Telegram extension create 'Configure Telegram API key' by itself?"
> **Domain expert:** "It may create a **Suggested Intent** owned by that extension. It becomes a **Deferred Intent** only through **Intent Acceptance** by the user or host-owned system policy."

> **Dev:** "How does the Telegram extension learn that its Deferred Intent exists?"
> **Domain expert:** "It performs an **Intent Query** for deferred or suggested intents it owns. Intent state in the database is the source of truth."

> **Dev:** "Can Vis run two active intents in the same conversation if they are related?"
> **Domain expert:** "No. Related intents may exist, but only one is the **Running Intent** at a time."

> **Dev:** "If a running intent has subintents, are those parallel active intents?"
> **Domain expert:** "No. The **Intent Cursor** descends into one **Subintent**, completes or parks that branch, then moves to the next sibling **Subintent**."

> **Dev:** "If the user changes their mind midway, do we delete the current branch?"
> **Domain expert:** "No. We resolve an **Abandonment Gate** with evidence and explicit **Abandonment Scope**, then move the **Intent Cursor**."

> **Dev:** "If a subintent is deferred, do sibling subintents keep running?"
> **Domain expert:** "Only if the deferred subintent's **Defer Sibling Policy** says siblings may continue; otherwise the parent remains blocked."

> **Dev:** "If the user finally pastes the Telegram API key, does Vis silently jump back to Telegram setup?"
> **Domain expert:** "No. The intent becomes a **Resumable Intent** first; a separate **Resume Decision** by the user or host-owned system policy moves the **Intent Cursor** back."

## Flagged ambiguities

- "future thing to do" was ambiguous between **Deferred Intent** and **Backlog Item** — resolved: commitments are **Deferred Intents**; suggestions are **Backlog Items**.
- "active later" was ambiguous — resolved: **Deferred Intent** is first-class status, not an **Active Intent** with a waiting gate.
- "extension-created deferred intent" was ambiguous — resolved: extensions may create **Suggested Intents** with an owner; acceptance turns them into **Deferred Intents**.
- "extension hook into intents" was ambiguous — resolved: extensions use **Intent Queries** for durable intent discovery; no first-class intent transition event bus for now.
- "suggestion storage" was ambiguous — resolved: **Suggested Intents** live in the same intent lifecycle as active/deferred/fulfilled/abandoned intents, distinguished by status.
- "who accepts extension suggestions" was ambiguous — resolved: **Intent Acceptance** comes from user or host-owned system policy; extensions may suggest but not silently accept their own suggestions.
- "system policy" was ambiguous — resolved: auto-acceptance policy is host-owned for now; extension-declared auto-acceptance is not trusted without a future permission model.
- "what can deferred intents wait on" was ambiguous — resolved: minimal **Defer Trigger** kinds are user input, time, extension signal, and another intent.
- "running intents" was ambiguous — resolved: a conversation has exactly one **Running Intent** at a time; other intents may be suggested, deferred, fulfilled, abandoned, or related but not running in parallel.
- "subintent" was ambiguous — resolved: subintents form an **Intent Tree** executed depth-first by a single **Intent Cursor**, not same-level parallel work.
- "parent intent gates with subintents" was ambiguous — resolved: an intent is either decomposed into **Subintents** or resolved by its own **Plan** and **Gates**, not both.
- "user changed direction" was ambiguous — resolved: use an **Abandonment Gate** with evidence instead of deleting or silently skipping the current branch.
- "how much to abandon" was ambiguous — resolved: every abandonment has explicit **Abandonment Scope**; Vis asks when the scope is unclear.
- "what cursor does after defer" was ambiguous — resolved: **Deferred Intents** record **Defer Sibling Policy** so the cursor either continues siblings or blocks the parent explicitly.
- "trigger observed" was ambiguous — resolved: observing a trigger makes a **Deferred Intent** a **Resumable Intent**; it does not silently move the **Intent Cursor**.
- "who resumes deferred work" was ambiguous — resolved: **Resume Decisions** come from user or host-owned system policy; extensions may observe/query/nudge but may not move the **Intent Cursor** themselves.
