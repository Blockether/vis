# Task Gates — proposal (no code yet; poke holes)

## Problem (from observation)
1. Tasks are under-created: a solo agent sees no value in them *to itself*.
2. `done()` fires prematurely / unfinished work falls out of attention.

## Reframe (the load-bearing idea)
Tasks are the **harness's control surface**, not the model's notepad. An LLM is
an attention model: commitments decay out of the effective window over a long
transcript. The task list — **re-rendered every turn AND acted on** — is how the
harness drags unfinished work back into attention and *refuses to let the turn
end on it*. Value is real **today**, zero parallel agents required. Parallelism
later is a bonus consumer, not the justification.

Corollary: "tasks not created" is not a prompt-wording problem. The fix is to
make the harness an active adversary that **cannot be satisfied without them** —
exactly how the existing **title-gate** forces a title.

## Mental model: a self-authoring BEHAVIOR TREE (not a script)
Two framings that triangulate on the same architecture:

**Orchestration-as-code vs orchestration-as-data.** Claude Code's Workflow tool
orchestrates subagents via an imperative SCRIPT (`parallel()`/`pipeline()`/loops are
code; agents are pure functions; deterministic; plan fixed ahead; state ephemeral).
vis is the DUAL: the plan is a TREE in the ctx engine, GROWN at runtime by the model,
gates as invariants, state persistent + addressable. The graph IS the program.
  parallel([a,b]) ≈ siblings w/o depends_on   pipeline(a→b→c) ≈ depends_on chain
Split it cleanly: PLANNING (model = compiler, task→tree IR, adaptive) vs EXECUTION
(runner = interpreter, walks the tree, deterministic+resumable GIVEN the tree).

**It's a Behavior Tree (UE-style) for LLM agents.** Near-exact mapping — use the BT
vocabulary, it names what we hand-wave:
  BT                    vis                          status
  Blackboard            ctx facts / :files           HAVE
  Task (leaf)           leaf work node               HAVE
  Node status           :status                      PARTIAL → 3-valued + propagation
  Sequence composite    :order / :depends_on         make EXPLICIT (:composite)
  Selector composite    —                            MISSING (retry / alternatives!)
  Parallel composite    siblings, no deps            implicit
  Decorator             gates (plan/done)            ad hoc → reusable policy
  Tick                  per-turn re-render           HAVE (expensive → event-driven)
  Status propagation    rollup leaf→root             HAVE → formalize success/failure
  Observer abort        —                            MISSING (reactive invalidation)
  Service               per-turn hooks               PARTIAL

### BT concepts to spec (the gaps)
1. **`:composite` on each internal node** ∈ {`:sequence` `:selector` `:parallel`}:
   - `:sequence` — children in `:order`; first failure fails the node (research→edit→verify).
   - `:selector` — try children until one SUCCEEDS = retry / alternative strategies.
     **The big missing piece:** today everything must succeed; selectors give
     first-class fallback ("try A; if it fails, try B").
   - `:parallel` — independent children; success policy (all / N-of-M).
2. **Three-valued status + propagation:** success | failure | running. Failure is a
   PROPAGATING signal — bubbles to the nearest `:selector` ancestor (next child) or
   fails its `:sequence` parent. This is what the sub_loop callbacks carry.
3. **Decorators = reusable composable policy** (data on a node): `retry(n)`,
   `timeout(ms)`, `guard(fact-cond)`, `loop-until`, `invert`. The gates become decorators.
4. **Observer aborts:** a fact change invalidating a running subtree aborts + replans it.
5. **Services:** background monitors while a branch is active.

### Execution = async sub_loops with engine callbacks
  sub_loop(ctx-slice, prompt, subtree) -> future
A node dispatched to a child loop returns a FUTURE. On completion it fires an
ENGINE-SIDE callback (not a bare return) that (a) folds the child's facts up
(summarized), (b) applies the node's `:composite` propagation rule (success/failure),
(c) re-ticks affected ancestors / unblocks dependents. = BT status propagation, async.
A Clojure runner coordinates (parallel independent siblings, await sequences) — the
Workflow combinators, walking the model-grown tree.

### Where the BT analogy BREAKS (LLM-specific — don't copy blindly)
- **Self-modifying tree:** game BT is static + human-authored; vis's is grown by the
  model at runtime. A *self-authoring* BT — powerful, non-deterministic.
- **Expensive ticks:** BT ticks are per-frame/µs; an LLM tick is a turn ($$/seconds).
  Re-evaluate at node boundaries / on events, NOT continuously. Caps observer-abort reactivity.
- **Stochastic leaves:** BT leaves are deterministic; vis leaves can "succeed" but
  WRONGLY. Success is untrustworthy on self-report → evidence/verify discipline matters
  MORE here (same root as the evidence-not-status gate).
- **Summarizing blackboard:** BT blackboard is tiny typed KV; vis's is huge text that
  must be folded/summarized on rollup (the original concern; absent in game BTs).

## RESOLVED — dispatch model: declarative SPEC + imperative EXECUTION (two layers)
The declarative-vs-imperative fork was a false dichotomy. Resolution:
- **Tasks = the declarative SPECIFICATION.** Planned FIRST (plan-gate), human-eyeballable
  (F2), stable. States WHAT + acceptance (done-means). The thing you VERIFY against.
- **The dispatch loop = the imperative IMPLEMENTATION.** The `sub_loop` coordination that
  REALIZES the spec. Adaptive; the HOW is free.
- **Gates = the conformance checker.** plan-gate: spec exists before execution. done-gate:
  execution satisfied every task WITH EVIDENCE. Verification = conformance of implementation
  to spec.

This is the deepest answer to "tasks under-created": an imperative-only loop has nothing
stable to verify against — the program is its own description. The declarative spec is the
fixed, reviewable contract the execution is held to. No spec ⇒ nothing to verify. Tasks are
the CONTRACT, not the transcript — which is also why a person can eyeball them.

The dispatch mechanism (auto-walk vs explicit `sub_loop()`) drops to an EXECUTION-layer
detail — both fine as long as execution conforms to the task spec. The fork was at the wrong layer.

### Two guards (or the separation collapses)
1. **Specs are OUTCOME-level (verifiable WHATs), NOT procedural (HOWs).** If a task is
   "call X then Y then Z," spec == program → zero verification value. Tasks state CHECKABLE
   outcomes (acceptance = criterion); the imperative loop owns the procedure.
2. **The spec is REVISABLE — explicitly + gated, never by silent drift.** Explore reveals
   reality ⇒ spec changes via a visible, re-approved `update_plan`. A child finding the
   parent's spec wrong = a spec-revision request bubbling up. Always conform to the CURRENT
   spec; spec changes are first-class events.

### Folds the earlier framings together
- compiler/interpreter: model COMPILES task→spec; loop INTERPRETS spec→execution; gates
  TYPE-CHECK execution against spec.
- behavior tree: tree = declarative spec; ticking = imperative execution; evidence-conformance
  tames stochastic leaves (a leaf can't fake Success past the done-gate).

## Primitives we already have (reuse, don't reinvent)
- `candidate` task status → the explore/propose-first, stop-for-approval path.
- `:files` on facts (full path + region) → locator substrate; makes a task
  self-contained / dispatchable and is the future conflict key.
- `:acceptance` + `:verified?` on tasks → seed of verification-as-contract.
- `update_plan` (declarative whole-list) + `plan_step` (single merge).
- GATES infra: the **title-gate** is the pattern we generalize.

## Loop shape (ordering — you can't plan before you've located)
    explore/locate → gather `:files` facts → PLAN-GATE → execute → DONE-GATE → done()
- **explore** produces the facts (what/where/what-to-change) as `:files`-bearing
  facts. Planning is downstream of facts; don't waterfall against assumptions.
- For risky/ambiguous asks the plan is emitted as `candidate` and STOPS for
  approval; for confident multi-step it goes straight to `todo`.

## The three gates (precise)

### G0. Title-gate — EXISTING (reference)
Hard-blocks `done()` until a session title exists. A title is always producible,
so a hard block is safe. Template for the others.

### G1. Plan-gate — ✅ SHIPPED 2026-06-10 (forces task *creation*)
- **Arms when:** a turn's cumulative file-mutations cross a threshold **without an
  approved plan**. Recommended threshold: the *first* file-edit is free; a
  **second distinct file** (or a second turn that continues editing) without a
  plan blocks. Tunable — see Holes.
- **Blocks:** further mutations (edits/patches), not reads.
- **Passes when:** an **approved** plan exists — ≥1 task with `:plan? true`, status
  ∈ {todo, doing, done}, NOT `candidate`. (Candidate = proposal awaiting approval;
  it does NOT satisfy the gate until approved, so the stop-for-approval path is
  enforced for risky work.)
- **Trivial work** (never crosses the threshold) → never armed → no ceremony.

### G2. Done-gate — NEW (forces honest *completion*)
- **Blocks `done()`** while ANY plan step is non-terminal.
- **Terminal closures (per step) — gate passes only on one of:**
  | close          | required to pass                                              |
  |----------------|---------------------------------------------------------------|
  | `done`         | `:verified? true` **AND** `:evidence` non-blank (the *how*)   |
  | `cancelled`    | `:reason` non-blank                                           |
  | `rejected`     | `:reason` non-blank                                           |
  | `deferred` (NEW)| `:reason` non-blank → **recorded as a fact** the user sees    |
- **Same threshold as G1:** if no plan was required (trivial), no done-gate. The
  two gates share a trigger so they don't fight (no plan ⇒ no nag; plan ⇒ enforced).
- **Active re-surface:** every turn the harness *asserts* open steps (imperative,
  high-salience), and on a blocked `done()` returns the list: "N open — verify,
  cancel/defer with a reason, or finish."

## Extended task shape (minimal additions)
Current: `{title, status, acceptance, verified?, order, plan?, facts, depends_on,
source, hook-id, importance}`. Add:
- `:evidence` (string) — the PROOF the acceptance was met (command+result, test
  name, `file:line`). Distinct from `:acceptance` (the criterion). **Gate checks
  evidence, not the boolean** (see Failure mode 1).
- `:kind` ∈ {`:work` (default), `:verify`} — a `:verify` task's `:acceptance`
  MUST be an **executable** criterion (run X → expect Y), not prose. (`:kind` only
  flags the executable-acceptance requirement; it does NOT carry structure.)
- `:reason` (string) — for `cancelled`/`rejected`/`deferred`.
- status gains `:deferred`.
- Plan steps SHOULD link the `:files`-bearing facts they touch via `:facts`
  (today display-only; for plan steps treat as the targeting/dispatch key).

### Hierarchy = `:parent` TREE (the dispatch topology) + `:order` + `:depends_on`
The task tree is NOT display sugar — it is the **recursive subagent dispatch
topology**. A node may be handed to a subagent that decomposes it into children
and dispatches THOSE to sub-subagents, unbounded depth. Completion is a fold from
leaves to root: a node is done ⇔ its subtree is done+verified ⇔ … down to leaves.
That recursion needs a real tree, which a `depends_on` DAG cannot express.

- **`:parent`** (entry-key) → ownership/recursion tree. Single parent, acyclic.
  Drives rollup (node can't close until all children terminal + its own verify
  passes) and nested display. The done-gate keys off the tree.
- **`:order`** (existing) → sequencing among siblings under one parent.
- **`:depends_on`** (existing) → demoted to the ESCAPE for dependencies that cross
  the tree (a node in subtree A must finish before a node in subtree B), plus
  cross-cutting task→fact prereqs. Kept pure for this because composition now lives
  in `:parent`, not overloaded onto `depends_on`.

Rationale for both (reversing an earlier "drop parent" note): `:parent` keeps
`:depends_on` uncontaminated. Ownership in the tree, cross-cutting order in the DAG.

### Consequences of the recursive tree (design must account for)
1. **Self-contained nodes.** A subagent gets only its node, so the node must carry
   its goal + `:files` facts + acceptance. Parent gathers facts (explore phase) and
   EMBEDS them in the child before dispatch. A node without `:files` is undispatchable.
2. **Fractal gates.** Plan-gate + done-gate apply at EVERY node, not just root. A
   decomposing subagent faces the same plan-gate (children before edits) and
   done-gate (subtree rolls up with evidence before reporting done to its parent).
3. **Verification at internal nodes too.** Leaves verify atomic work; a parent
   verifies INTEGRATION (children done AND together meet the parent's acceptance).
4. **Facts fold up.** A child's discovered facts/evidence propagate into the
   parent's context on rollup (attention refresh, recursively).
5. **Conflict at every fork.** Sibling subtrees whose `:files` regions overlap must
   serialize. `:files`↔task link is the conflict key — at every parallel branch.
6. **Hierarchical node addresses.** Nodes want path keys (`root/auth/middleware/
   verify`) mirroring scope paths, so a node is addressable + its subtree recallable.

## The node contract (KEYSTONE — gates + dispatch hang off this)
A node = a task entry that COULD be handed to a subagent. Treat it as a function
`node(down-contract) -> up-contract`, so it executes blind and the parent can
verify + fold the result.

### Down-contract (what the parent supplies = dispatch input)
| field        | role                                                                 |
|--------------|----------------------------------------------------------------------|
| `:key`       | local key (the `:session/tasks` map key). ADDRESS = parent-chain path (`root/auth/middleware`) — derived, not stored. |
| `:parent`    | tree edge (nil at root).                                             |
| `:kind`      | `:work` \| `:verify`.                                               |
| `:title`     | short label.                                                        |
| `:objective` | (opt) richer "what done looks like"; recommended for internal nodes a subagent will decompose. |
| `:acceptance`| the criterion. For `:verify`, MUST be executable (run X → expect Y).|
| `:facts`     | **DISPATCH MANIFEST** (was display-only): fact keys the dispatcher RESOLVES + materializes into the child's starting context. MUST include locator (`:files`) facts — a node without them is undispatchable. Facts stay canonical (referenced, not copied); `recall()` still windows them. |
| `:order`     | sibling sequence under the parent.                                  |
| `:depends_on`| cross-tree prereqs only (task→task\|fact).                          |

### Up-contract (what the node returns on close = rollup output)
| field            | role                                                              |
|------------------|-------------------------------------------------------------------|
| `:status`        | done \| deferred \| cancelled \| rejected.                        |
| `:evidence`      | proof acceptance met (cmd+result / test / `file:line`). REQUIRED to close `:done` — done-gate checks THIS, not a boolean. |
| `:reason`        | REQUIRED for deferred/cancelled/rejected (recorded as a fact).    |
| produced facts   | facts the subagent discovered/created (esp. new/changed `:files` regions) fold into the PARENT's context on rollup (attention refresh). Engine auto-captures from the child's fact writes — no manual list. |
| realized subtree | children the subagent created (its decomposition) become visible to the parent, so it sees the actual tree it got back. |

### Rollup rule
A node closes `:done` ⇔ every child is terminal AND `:verify` children are
`done`+evidence AND the node's own verify passes. Leaves (no children) close on
their own work+verify. Fold is leaf → root; the done-gate enforces it at EVERY node.

## sub_loop execution model (GraalVM-MEASURED 2026-06-10, on Oracle GraalVM 25.0.1)
The parent governs MANY concurrent child Contexts. Each `sub_loop` FORKS a real
GraalPy Context (true Python isolation), and the parent coordinates the fleet.

**The one Truffle hazard + its fix (both reproduced on the prod runtime):**
- A STANDALONE `Context.build()` called WHILE another eval runs on a (virtual)
  thread → **froze the whole JVM** (Truffle safepoint deadlock — this is the
  hazard the current `create-python-context` docstring warns about).
- The SAME create-during-eval through a **single shared `org.graalvm.polyglot.Engine`**
  (built ONCE at session start) → **OK, no deadlock.** A shared Engine moves engine
  init off the hot path; concurrent Context creation becomes safe.

**Measured (shared Engine):** child Context ~38 ms warm (vs 60 ms standalone — shared
code cache), 4–6 children eval CONCURRENTLY with no hang. `bind-ctx!` of the
subslice into a child is 0.6 ms.

**Decision (per user): FORK the Context.**
- **ONE shared `Engine` per session**, pre-built at session start. The main Context
  AND every child `sub_loop` Context are `.engine(shared)`-built from it. This is
  THE change that makes concurrent forks safe.
- **A sub_loop forks a child Context** (~38 ms) — true Python-global isolation, no
  snapshot/restore needed.
- **PARALLELISM is first-class**: the parent dispatches N dispatchable children and
  awaits their rollups (futures), bounded by a concurrency cap. Each child Context
  evals on its own thread.
- **Child ctx = subslice + recall-back** (chosen): the child's `context` is bound
  (via `bind-ctx!`) to `node-subslice` (re-rooted node + its subtree + linked
  facts); it runs on its OWN isolated engine ctx-atom; `recall(...)` reaches the
  PARENT's facts/archive READ-ONLY for anything not embedded.
- **Fold-up**: on child close, `node-rollup-report` merges status + evidence +
  produced facts into the parent node; the realized child subtree becomes visible.
- **Conflict serialization**: children whose `:files` regions overlap can't run in
  parallel — the `:files`↔node link is the conflict key the scheduler honors.

Build slices: (A) **shared `Engine`** wired into `create-python-context` + a
`fork-context!` that builds a child Context on it [GraalVM-verified: no deadlock];
(B) `node-subslice` (pure) + bind it as the child's `context`; (C) the `sub-loop!`
runner (isolated child ctx-atom + recall-back + concurrency cap + fold-up); (D) the
`sub_loop([keys])` model verb (dispatch a set of children, await rollups) + prompt.

## G1 threshold — when the plan-gate arms (per node, fractal)
Decides enforcement vs nagging. **Structural / observed — never self-declared
scope** (that hands the gate's key back to the biased party). First atomic action
is always free.

**ARM** (block further mutation) on ANY of:
- the node mutates a **2nd distinct file**, OR
- mutation **continues into a 2nd turn** for this node (outgrew one shot), OR
- explore surfaced **≥2 independent `:files` regions before the first edit**
  (multi-file scope known up front → plan before editing at all).

**PASS** (either):
- the node has an **approved child plan** (≥1 child, not all `:candidate`) — it
  decomposed; OR
- the node is `:atomic? true` + has an `:acceptance` — an explicit, **RECORDED**
  opt-out asserting "one coherent change, verified as a unit." Auditable (visible to
  user + parent on rollup); abuse leaves a trail; parent/root can override.

**NEVER arms for:** a single-file, single-turn change with a stated acceptance (the
common small edit) — zero ceremony.

**Risk interaction:** risky/ambiguous node → its decomposition must be `:candidate`
and STOP for approval before the subagent proceeds (candidate children don't satisfy
PASS until approved).

**Why an escape** (unlike the title-gate's hard block): "must decompose" isn't always
right — a 2-file change can be a legit leaf, and file-count is a poor proxy for "needs
decomposition." The `:atomic?` opt-out absorbs that — auditable, not brittle. **Needs
A/B tuning** (you already A/B-measure verb usage): watch the `:atomic?`-opt-out rate;
if it's ~always, the threshold is too low (nagging) or the opt-out too cheap (gaming).

## Failure modes (codified — these WILL bite if naive)
1. **Silencing loop (already happened):** `extension.clj` TTL-prune comment —
   *"model kept re-emitting `(task-set! … :done)` to silence stale CTX chrome."*
   If the done-gate checks *status*, the model marks everything `done` to escape
   → fake completions, worse than no tasks. **Mitigation: gate on `:evidence`, not
   status.** A boolean is a self-asserted lie; an evidence string is checkable and
   makes lying cost ≈ doing.
2. **Hard-block deadlock:** task completion isn't always producible (unlike a
   title). A pure hard block ⇒ infinite loop. **Mitigation: the `deferred`+reason
   escape is cheap AND honest** — passes the gate, recorded as a fact. We block
   *silent* abandonment, not *acknowledged* abandonment.
3. **Nagging on trivial work:** **Mitigation: G1 and G2 share the threshold.**
4. **Prose acceptance is unenforceable:** **Mitigation: `:verify` subtasks carry
   executable criteria;** a `:work` step's prose acceptance is fine only if it has
   a `:verify` child that is checkable.

## What's genuinely new vs reuse
- Reuse: candidate flow, `:files` facts, acceptance/verified, GATES infra,
  update_plan declarative.
- New: G1 plan-gate, G2 done-gate, `:evidence`/`:parent`/`:kind`/`:reason` fields,
  `:deferred` status, active re-surface on blocked `done()`.

## Persistence / storage (CONVERGED — conservative)
Current (SQLite, Flyway V1): `session_soul → session_state → session_turn_soul →
session_turn_state` (ctx Nippy blob: tasks/facts/trailer/archived) `→ session_turn_iteration`
(`code` + `forms` BLOB + result + error). Facade + pluggable adapters (SQLite now, PG planned);
backend-neutral search DSL (FTS5 → tsquery).

Direction — DON'T disturb what works; promote durable memory to rows:
- **`forms` BLOB: UNTOUCHED.** Canonical raw form-result store on the iteration row. It works;
  leave it. (recall-recent reads the in-mem trailer; `introspect-*` reads this blob.)
- **`iteration`: stays `code` + `result` + `error`.**
- **Promote `task` / `fact` / `archive` to TABLES** (key-columns + payload), so the durable
  cross-turn memory is queryable, FTS-indexed, tree-spanning (owner = node-address). Same
  structure-vs-content split as `task_node` (the `task` table IS the `task_node` tree).
- **`archive` row = snippet + pointer (DECIDED):**
  - folded ENTITIES (facts/tasks) held INLINE.
  - form-based recoverables held as a **scope POINTER** (`tN/iM/fK`) into the untouched forms
    blob — NO duplication.
  - each row carries a **search SNIPPET** (FTS-indexed) + the pointer. Search hits the snippet;
    `recall` resolves the pointer → forms blob for full content. One search index (archive's).
- **recall** = trailer (live, in-mem) → else archive (resolve: inline entity OR pointer→forms).
- ctx blob shrinks: tasks/facts/archive move to tables; trailer becomes a projection over
  recent iterations + active facts.

Why conservative wins: the fold ALREADY leaves pointers to the forms blob (raw stays in DB),
so copying results into archive would DUPLICATE for zero gain. Pointing formalizes the existing
pattern into a queryable table. Lowest risk, saves space, forms machinery untouched.

Still open: per-form vs per-iteration recovery granularity; Flyway V2 migration of existing
blob tasks/facts/archived → rows (backfill vs lazy); retention/GC of closed-subtree raw forms;
resume (durable node status + idempotent re-dispatch).

## HOLES CHECK — full sweep (did we think of everything? no — here's the list)
Legend: ✅ decided · 🟡 partial/leaning · ❌ open / not-yet-considered.

### 1. Visibility in F2 + TUI (human-facing)
- 🟡 **F2 must render a TREE, not a flat list** — nested nodes, per-node status
  (running/done/failed/deferred), `:composite` type, decorators, evidence. F2 becomes
  a behavior-tree debugger / CI-pipeline view. **1b SHIPPED the data + pure render**
  (`task-tree-lines`: indented + rolled-up outcome glyph; F2 chunk now carries the nested
  `nest-tasks` view). REMAINING: the live F2 panel must consume `:depth`/`:outcome` and add
  drill-down + collapse triangles (live keypress pending — consumer is outside this src tree).
- ❌ **Drill-down:** select a node → see ITS sub-loop transcript/forms/facts (the node's
  own ctx). F2 = tree navigator, not one flat panel.
- ❌ **Live parallel streaming:** N sub-RLMs run at once; the TUI must multiplex their
  progress without blocking. Today the channel streams ONE loop. **Concurrency in TUI
  render is a real unsolved piece** (lanterna single paint thread + N event sources).
- 🟡 **Model-facing vs human-facing split:** the model's working render should show only
  ITS subtree slice (bounded, summarized); F2 (human) shows the WHOLE tree (inspectable).
  Two different projections of the same data. **1b LANDED two projections off ONE walk**
  (`nest-tasks` for the model dict, `task-tree-lines` for the human panel) — the SLICING
  (show only the model's own subtree) is the remaining piece.
- ❌ **Failure surfacing:** a failed leaf deep in a parallel subtree must surface to the
  footer/F2 immediately (not buried). Needs a tree-level "attention" signal.

### 2. What a sub-RLM contributes back to context, and how
- ❌ **THE fork: shared ctx vs isolated-ctx-merged-on-rollup.**
  - shared (all children write the one engine): simplest, but parallel children RACE on
    facts + key collisions.
  - isolated subtree ctx + fold-up on rollup (LEAN): each sub-RLM gets its own ctx slice,
    writes freely, and only a DIGEST folds into the parent's live context; full child ctx
    persists (recall reaches in). Clean isolation, bounded parent context, but needs a
    merge/namespacing strategy.
- 🟡 **What it can contribute:** evidence, facts it discovered (esp. new/changed `:files`
  regions), its realized subtree, status. Engine auto-captures from the child's fact writes.
- ❌ **Upward disagreement:** a child may discover the PARENT's plan is wrong (a
  `fact_contradicts` of a parent assumption). How does it signal "replan, parent"? →
  failure-with-reason that triggers the parent's `:selector`/replan. Not specced.
- ❌ **Namespacing:** child fact/task keys vs parent keys — collision rule across subtrees.

### 3. Who drives task progression (state transitions)
- 🟡 **Mixed authority (LEAN, needs ratifying):**
  - **runner** sets `:running`/dispatched when it ticks a node.
  - **leaf sub-RLM** self-asserts terminal status (done+evidence / deferred+reason) —
    GATED on evidence (can't lie cheaply).
  - **engine** DERIVES internal-node status from children rollup (a parent can't self-claim
    done while children aren't) — `:composite` rule decides success/failure.
- ❌ **Approval authority:** who approves a `:candidate` plan — human only, or may a PARENT
  agent approve a child's candidate decomposition? (human-in-loop vs autonomous fan-out.)
- ❌ **Who ticks / the scheduler:** event-driven (node completes → tick parent)? a Clojure
  event loop? concurrency cap + rate/cost limits? Not specced.

### 4. Persistence & resumption
- ✅ **Tree persists** as the existing keyed `:session/tasks` map + `:parent` edges (already
  blob-persisted per turn). CONFIRMED 2026-06-10 by a real-DB round-trip test
  (`task-tree-survives-db-round-trip-test`): `:parent`/`:composite`/`:decorators`/`:evidence`
  + fact `:depends_on`/`:contradicts` all survive `db-load-latest-ctx`, and `derived-outcome`
  rolls up correctly on the RESTORED data. NO schema change — the V1 ctx blob already carries it.
- ❌ **Per-node execution state:** each sub-loop is effectively a sub-session with its own
  turns/iterations/forms. Schema today is session→turn→iteration→forms (flat). Needs to
  become **hierarchical: session → node-tree → per-node loop(turns/iters/forms)**. Real
  schema evolution.
- ❌ **Resumption of a partial tree:** on crash, running futures are lost. Resume = durable
  per-node status + **idempotent re-dispatch** of pending/running nodes. Needs node-level
  checkpointing.
- 🟡 **Full vs digest:** full child ctx persists in DB (cheap storage, audit/recall); only
  the digest lives in parent live context (cheap attention). Leaning; recall is the bridge.

### 5. Cross-cutting holes we hadn't named
- ❌ **Declarative vs imperative dispatch (FUNDAMENTAL FORK):** does the model GROW the tree
  and the runner AUTO-dispatches dispatchable nodes (orchestration-as-data, fits the thesis)
  — or does the model CALL `sub_loop(node)` imperatively and await (model orchestrates)?
  Everything downstream (verbs, scheduler, gates) depends on this.
- ❌ **`update_plan` is declarative-REPLACE-whole — but in a tree, a child rewriting "the
  plan" would clobber siblings.** Need **subtree-scoped** plan ops (a node may only rewrite
  its own children). Big — `update_plan`'s core semantics vs the tree.
- ❌ **Budget / runaway protection:** per-subtree token/cost budget (children draw from
  parent's allocation; exhaustion → fail/defer); max depth; max node count; max iters/node.
  An agent-tree can explode $$$ without this. Mirror the Workflow `budget`.
- ❌ **Cancellation cascade:** user cancels → abort ALL running sub-loops in the subtree.
- ❌ **Verify execution model:** is a `:verify` node an LLM judge, or a DETERMINISTIC command
  (`:exec "clojure -M -e …"`) the engine runs? Probably both; not specced. Deterministic
  verify is far stronger (see "stochastic leaves").
- ❌ **Composite/decorator authoring verbs:** how does the model EXPRESS `:composite`,
  decorators, `:atomic?` in `update_plan`/`plan_step`? New verb surface.
- 🟡 **Interaction with existing summarize/trailer fold:** per-node trailers + a tree-level
  fold policy; the fold-up (§2) IS the summarization the original question was about.

### Decided-vs-open ledger (coverage at a glance)
- ✅ DECIDED: harness-as-control-surface; two gates (plan/done); evidence-not-status;
  deferred escape; `:parent` tree over `depends_on`; node contract (down/up); G1 threshold
  w/ `:atomic?` escape; BT framing; planning↔execution (compiler↔interpreter) split;
  **dispatch = declarative-spec(tasks) + imperative-execution(loop), gates = conformance**
  (specs are outcome-level not procedural; spec revisable only via gated re-plan).
- ✅ SHIPPED + LIVE-VERIFIED (real `bin/vis` agent, 2026-06-10): status algebra; conformance
  passes; title-gate removed; **forcing done-gate** — the live check CAUGHT A REAL BUG the
  unit tests missed: the gate ran in run-iteration POST-eval, AFTER `answer-fn`→`apply-done!`
  already finalized during eval, so `done()` was never actually blocked. Moved the gate INTO
  `answer-fn` before `apply-done!`; re-verified live: `done()` REFUSED with open plan steps
  ("Cannot finalize — 2 open plan step(s): design, implement…") and the `cancelled(+reason)`
  escape lets the agent proceed.
- ✅ SHIPPED + LIVE-VERIFIED (real `bin/vis` agent, 2026-06-10): **evidence-not-status** —
  the done-gate now also blocks a `:done` plan step that has an `:acceptance` but blank
  `:evidence` (closes the silencing-loop hole); `plan_step`/`update_plan` accept `:evidence`
  + `:reason`; prompt shows the field + rule. Live proof of intent: the agent hit the refusal,
  **actually ran a verification** (`clj_eval … :compiled-ok`), attached it as `:evidence`, then
  finalized — the gate forced REAL verification instead of self-assertion.
- ✅ SHIPPED + LIVE-VERIFIED (real `bin/vis` agent, 2026-06-10): **no-silent-abandonment** —
  the done-gate also blocks a non-success terminal step (`:cancelled`/`:deferred`/`:rejected`/
  `:failed`) with blank `:reason`. Live proof: agent cancelled a step without a reason, `done()`
  REFUSED, agent added the reason, `done()` accepted. **The forcing done-gate is now complete on
  all three escapes: open / fake-done(no-evidence) / silent-cancel(no-reason).**
- ✅ SHIPPED + LIVE-VERIFIED (real `bin/vis` agent, 2026-06-10): **`:parent`/`:composite`
  tree-creation surface** — `update_plan`/`plan_step` accept `"parent"` (+ explicit `"key"` so
  refs don't drift off the title slug) and `"composite"` (`sequence`/`selector`/`parallel`);
  `pass-task-parent` flags dangling + cyclic edges. Live proof: agent built `auth(selector)` with
  `middleware`/`decorator` both `parent:auth`, confirmed in `session_tasks`. **Unblocks the tree
  tier** (subtree `update_plan`, node contract, parent-rollup, F2 tree render). Note: live check
  surfaced the title-slug-key gotcha → added explicit `"key"` (headless-verified).
- ✅ SHIPPED (spec + declaration; execution = runtime tier): **decorators + spec hardening** —
  `::entry-key` now accepts STRING keys (plan-step keys are strings; the old `keyword?` never
  matched real `:parent` refs); full `::decorator` spec (`retry`/`timeout`/`guard`/`loop-until`/
  `invert`) + `:session.task/decorators`; `:session.task/attempts` = the **INFORMED-retry failure
  log** (each retry carries WHY the prior attempt failed, so it's not blind repetition — blind
  repeat trips the loop-checkpoint). `update_plan`/`plan_step` accept `"decorators"` (string→keyword
  normalized). 77 spec cases green. Decorator EXECUTION (retry/timeout actually firing) = runtime tier.
- ✅ SHIPPED + RENDER-VERIFIED (real production path, 2026-06-10): **1b nested tree render +
  rolled-up status** — one PURE DFS walk (`task-tree-walk`, cycle-guarded by visited-set +
  depth-cap-32, orphans-in-a-cycle kept flat so nothing drops) feeds TWO projections: `nest-tasks`
  (model dict — ordered `array-map` in parent→child DFS order, each node annotated with `:depth`
  + rolled-up `:outcome` via `derived-outcome`) wired into `session-view`, and `task-tree-lines`
  (human/F2 — 2-space indent per depth + outcome glyph). Proven through the REAL render path
  (`render-ctx`→`session-view`→`nest-tasks`→GraalPy `__vis_pp__`): a `selector` parent with a
  failed `oauth` + done `apikey` child renders `outcome:"success"` (fallback worked) with
  `depth` 0/1/1 in DFS order. **Glyph audit (user-flagged):** dropped `▸`/`▶` from status — those
  are reserved for tree DISCLOSURE/collapse + form-title markers (main.clj, welcome); outcome now
  `✓`/`✗` (matches trace) + `◐`/`○`/`·` circles (progress/pending/neutral), all NARROW cells.
  F2 chunk (loop.clj) now ships the nested view too. Live `bin/vis` drive ran **exit-0
  end-to-end** (render path exercised every iteration, zero breakage); 189 render/engine cases +
  6 new tree-render cases green. **Live F2 keypress = PENDING** (interactive TUI; the F2 consumer
  renders the on-chunk `:tasks` payload outside this src tree).
- ✅ SHIPPED (Decision 2, 2026-06-10): **FORCING plan-gate (G1)** — the FIRST file mutated in a
  turn is free; the 2nd DISTINCT file write/patch is REFUSED until an approved plan exists
  (`approved-plan?` = ≥1 non-`candidate` `:plan?` step), with a per-write `atomic=True` escape
  (user-chosen ergonomics) RECORDED as an `atomic_override_*` audit fact. PURE policy
  (`plan-gate-block` — first-free, 2nd-distinct-file arms, same-file re-edit free, single multi-file
  patch caught) injected into foundation editing as a `:mutation-gate` callback so that layer stays
  engine-agnostic (dependency inversion); composed AFTER path-protection (protected paths never reach
  the gate). `:files-mutated` set tracked per turn on turn-state (reset each turn). Prompt teaches
  the rule + escape. 7 policy + 6 before-fn-composition cases green; full suite 2043 cases, **0 new
  failures** (the 11 reds are pre-existing Telegram-extension harness issues, baseline-confirmed via
  stash). Threshold refinements (2nd-turn continuation, ≥2 regions pre-edit) = future.
- ✅ SHIPPED (2026-06-10): **subtree-scoped `update_plan` — UNIFIED, no new verb.**
  `update_plan(steps)` replaces the whole plan; an optional 2nd positional scope key
  `update_plan(steps, "parent_key")` rebuilds ONLY that node's subtree (its transitive
  descendants — grandchildren too, no orphans), leaving the parent, sibling subtrees, and root
  steps untouched. New steps default `:parent parent_key`; a step may name a deeper `:parent`.
  Blank scope → whole-plan replace. Unknown parent → soft-warn no-op. Both paths share
  `build-plan-entry` (whole-plan refactored onto it — provably identical, all prior update_plan
  tests still green). **Per user (UNIFICATION / "fuck legacy"): rejected a separate
  `update_subplan` verb — it's the SAME verb, scoped.** 5 engine cases green; verified through the
  REAL model-facing binding (`build-engine-bindings` `update_plan([...]) ` then `update_plan([...],
  "auth")` → auth+ui survive, oauth/apikey gone, saml re-parented). Live `bin/vis` exit-0 (agent
  looped on the multi-step prompt, no clean dict-paste — binding proof is authoritative). Internal
  one-`:doing` invariant stays global (V1).
- ✅ SHIPPED (2026-06-10): **fact surface unified to ONE verb — `fact_set`.** Per user ("make the
  facts a single function / remove the old stuff"): DELETED the legacy `fact_depends` /
  `fact_contradicts` / `fact_contradicts_remove` model verbs AND their engine mutators
  (`apply-fact-contradicts!` / `apply-fact-contradicts-remove!` / `apply-depends!` + dispatch cases).
  Relations are now DECLARATIVE FIELDS on `fact_set(k, {depends_on:[…], contradicts:[…]})` —
  present key replaces the edge set, contradicts reconciles the symmetric back-links on both facts,
  re-send-without retracts, self-refs dropped, missing targets warn. Removed from bindings +
  core-mutation-heads + engine-form-heads + ctx_spec; prompt teaches relations-as-fields; all
  legacy-verb tests migrated to `fact_set` fields. 340 cases green. Mirrors the plan side: ONE task
  verb (`update_plan`) + ONE fact verb (`fact_set`).
- ✅ SHIPPED (2026-06-10): **tree persistence AUDITED — no schema change (V1 blob suffices).** Added
  `task-tree-survives-db-round-trip-test` to the canonical real-DB persistence integration suite: a
  selector parent + two children (one with a `retry` decorator, one `done`+`evidence`) and two facts
  wired by `depends_on`/`contradicts` are driven through `build-engine-bindings` against a real
  in-memory SQLite, persisted, and reloaded via `db-load-latest-ctx` — the ENTIRE tree + decorators +
  fact relations survive, and `derived-outcome` rolls up to `:success` on the RESTORED data. Confirms
  the persistence tier needs ZERO new tables; the per-turn Nippy ctx blob already carries the topology.
- ✅ SHIPPED (2026-06-10): **node contract (DOWN packet + UP rollup) — pure data foundation.**
  `node-dispatch-packet` (DOWN): the self-contained spec a subagent needs to work a node without
  the parent's context — goal/acceptance/kind/composite/decorators + the embedded `:files` it
  targets (gathered from linked facts via `node-files`) + its child keys; `:dispatchable?` enforces
  the proposal's "a node without :files is undispatchable" (a parent is dispatchable via children).
  `node-rollup-report` (UP): rolled-up `:outcome` (`derived-outcome`) + own evidence/reason + the
  facts produced across the whole subtree (folded + de-duped) — what lets a parent verify
  INTEGRATION from children's evidence. 6 cases green. The recursive DISPATCHER that consumes these
  (sync/async sub-loops, shared vs isolated child ctx, scheduler, concurrency caps) is the OPEN
  runtime tier — needs a design decision before building.
- ✅ SHIPPED (sub_loop slice A, 2026-06-10): **shared GraalVM `Engine` + `fork-context!` — the
  substrate that makes the parent-governs-many-Contexts model SAFE.** Decided WITH the user (forking,
  not rebind). Reproduced the prod hazard on GraalVM 25.0.1 (standalone `Context.build()` during a
  live virtual-thread eval FREEZES the JVM at a Truffle safepoint) and the FIX: one process-wide
  `shared-engine` (defonce, forced at session start); `build-agent-context` builds the main sandbox
  AND every child on it; `fork-context!` forks a deny-by-default child Context. GraalVM-verified:
  fork-during-parent-eval returns cleanly (was a hard freeze standalone), child has ISOLATED Python
  globals, N children eval concurrently (~38ms warm child). Full suite 2054 cases, 0 new failures.
  NEXT sub_loop slices: (B) `node-subslice` (pure) bound as child `context`; (C) `sub-loop!` runner
  (isolated child ctx-atom + recall-back + concurrency cap + fold-up via `node-rollup-report`);
  (D) `sub_loop([keys])` model verb + prompt.
- 🟡 LEANING: isolated-ctx + fold-up; mixed progression authority; model-slice vs human-F2;
  full-persist + digest-in-context.
- ❌ OPEN (must decide):
  status algebra + composites (3-valued); budget/recursion caps; persistence MIGRATION (V2
  backfill) + resume + recovery granularity; TUI parallel streaming + tree render; approval
  authority; verify execution model;
  cancellation; child→parent disagreement.

## Holes to poke (original short list)
- **Threshold for G1/G2.** "2nd file edit" is one rule; alternatives: line-count,
  cross-turn continuation, or the model self-declaring scope (gameable). Which?
- **Evidence verification depth.** Harness can check `:evidence` is non-blank and
  plausibly references a command/test/loc — but it can't *run* it. Do we want a
  `:verify` subtask whose acceptance the harness (or a sub-agent) actually executes?
- **Approval granularity.** candidate→approved is per-plan today; do we need
  per-step approval ("do 1 and 3")? (Memory says partial-approve exists — confirm.)
- **Deferred vs cancelled semantics** — is `deferred` resumable next turn (re-armed)
  or terminal-for-the-turn? Leaning: resumable, re-surfaced until truly closed.
- **Conflict detection** (parallel future): `:files` region overlap between two
  tasks ⇒ must serialize. Out of scope now, but the `:files` link should be shaped
  so it's addable later without a migration.
