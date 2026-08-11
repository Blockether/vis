---
name: plan-md
description: >
  The exact five-part shape of PLAN.md, the one plan in flight at the repo root:
  title, catchy phrase, context, phases with Rationale/Data/Acceptance/Unknowns,
  and state. Read before writing, restructuring or accepting a plan.
---

# Planning artifacts: `PLAN.md` has exactly five parts

`PLAN.md` at the repo root is the plan for the work in flight, and there is **one at a time**. It has these five parts in order and nothing else — no status chatter, no changelog, no notes to self.

1. **Title** — `# PLAN — <the change, imperative>`. Names what is being done, not the area it touches.
2. **Catchy phrase** — one line: the thesis someone repeats back in review. *The engine owns the context, not the transcript.*
3. **Context** — the STATE BEFORE (`file:line` for every claim, measurements not impressions), the ROOT PROBLEM (the force under the symptom), WHAT WE SOLVE and what we explicitly do not, and the ALTERNATIVES CONSIDERED each with the reason it lost. An alternative with no recorded reason gets re-proposed at the next review.
4. **Proposal phases** — one `## Phase N — <the verb it performs>` each, ordered so every phase lands on a product that already works. Each carries exactly four things: **Rationale** (what stays broken without it); **Data** (a `clojure.spec.alpha` block ONLY when the phase changes data that CROSSES a boundary — persisted on disk, sent over the wire, or a contract another language mirrors — written in the plan before the code, no prose schemas and no second schema library; a function's internal argument shape, a map that never leaves one namespace, and data the phase merely deletes are NOT data changes, and that phase writes `**Data.** None.` with the one line saying why); **Acceptance criteria** (the files it changes, one line each, plus the test that proves it done); **Unknowns** (as questions — a phase with none says so).
5. **State of the plan** — **ACCEPTED**, **REQUIRES WORK**, or **DONE**, then what is done per phase with its commit, and a TODO list of the rest in order. The only part edited as work lands, and it is edited in the same commit as the work it records.
