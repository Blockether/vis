# TASKS — VIS backlog (captured 2026-06-06)

Source: brainstorm dump (PL). Each item below is restated with **context**,
**rationale (why it matters)**, **where in the code it lives**, and a
**proposed approach**. Nothing here is decided — these are scoped starting
points, not commitments.

Task IDs are stable (`VIS-1` … `VIS-8`, with `VIS-8a/b/c` sub-tasks and
`VIS-EVAL`). Reference them in commits/PRs. Every task carries an explicit
**Acceptance criteria** block — the task is done only when every box is
checkable and verified (not self-asserted).

Terminology used below:
- **CTX** — the session memory the engine renders as bare EDN under a `;; ctx`
  marker. It is **TEXT in the prompt, not a binding**. Defined/rendered in
  `internal/ctx_loop.clj` + `internal/prompt.clj`; engine state in
  `internal/ctx_engine.clj` (one atom per session, `:ctx-atom` on env).
- **SVAR** — the streaming LLM router lib (`com.blockether.svar.*`) wired in
  `internal/loop.clj`. Handles provider routing, fences, retries.
- **Trailer** — recent form-result pins shown in CTX (`:session/trailer`).
- **Foundation tools** — bare Python symbols `cat` / `ls` / `rg` / `patch` /
  `write` / `git/*`, defined under `internal/foundation/` and
  `extensions/common/vis-foundation-*`.

---

## VIS-1 — Expose CTX as a Python variable (read-only) — investigate

**Status:** open · **Depends on:** VIS-EVAL (to measure) · **Relates to:** VIS-8c

> "Przeanalizować czy czasem nie dodawać CTX po prostu jeszcze w SCI jako
> zmienna … ALE żeby broń boże nie zmieniał tego CTX z palucha."

### Context
Today CTX is **text only**. The prompt is explicit about it
(`internal/prompt.clj:124`):

```
CTX — session memory, bare EDN under a `;; ctx` marker. TEXT, not a
binding. `ctx` is unbound; reading it errors. Re-rendered each iter
with this turn's pins visible.
```

The live engine state already exists as data — `:ctx-atom` on the env map
(`internal/ctx_loop.clj:11`, `make-ctx-atom`). The model just has no symbol
to reach it. The rendered, digest-enriched ctx (with `:session/env`,
`:session/utilization`, `:session/workspace`, etc.) is built in
`ctx_loop.clj` around `render` (`:276`–`:331`) and handed to the renderer.

### Rationale
The model frequently wants to *read* derived session state programmatically
instead of eyeballing the EDN — utilization %, workspace root, current scope.
The dump itself proves it (see item 8): the user keeps writing forms like
`(let [env (:session/env ctx) …] …)` that **fail with `Unable to resolve
symbol: ctx`**. A read-only binding would make that natural and let the model
branch on its own context budget (e.g. "if `:pct-of-limit` > 70, summarize").

### The hard constraint
The CTX must **not** be hand-mutable. If we bind a plain value, the model could
`(def ctx (assoc ctx …))` or otherwise desync its view from engine truth. The
engine is the single writer (mutators in `ctx_loop.clj` `build-engine-bindings`
swap! the atom and return `:vis/silent`). A writable `ctx` symbol would break
that invariant.

### Proposed approach (to evaluate, not final)
- Bind `ctx` (or a less collision-prone name, e.g. `*ctx*` / `(session)`) as
  a **read-only snapshot fn**, not a var: `(session)` → the rendered ctx map
  as of the current iter; recomputed each iter from the atom, never settable.
  A fn sidesteps "model rebinds the symbol" because shadowing a fn locally
  doesn't corrupt engine state — the next iter re-resolves the engine fn.
- Alternatively expose **narrow accessors only** instead of the whole map:
  `(util)` → `:session/utilization`, `(workspace)` → `:session/workspace`.
  Smaller blast radius, no temptation to treat it as writable state.
- Whichever shape: keep the value **immutable** (the snapshot is a plain map;
  mutators stay the only write path) and update `internal/prompt.clj:124` so
  the "TEXT, not a binding" wording matches reality.
- Decide collision policy: a bare `ctx` symbol is attractive but the model
  already *tries* it (item 8), so binding exactly that name has the best
  ergonomics — at the cost of forever reserving it.

### Open questions
- One snapshot fn vs. several narrow accessors?
- Does exposing the full map leak internal keys we'd rather not contract on?
- Caching: recompute per-iter (cheap, the renderer already builds it) vs. lazy.

### Acceptance criteria
- [ ] A read-only handle exists in the GraalPy sandbox (`(session)` / accessor fn /
      bound `ctx`) returning the current rendered ctx (or chosen subset).
- [ ] The handle is **immutable**: no model form can mutate engine state through
      it; a test proves a `(def ctx …)` / local shadow does **not** desync the
      next iter's engine truth.
- [ ] The mutator path (`build-engine-bindings` → swap! atom → `:vis/silent`)
      remains the **only** writer; verified by test.
- [ ] The handle's value matches `:session/utilization` / `:session/env` shown
      in the rendered EDN for the same iter (no drift).
- [ ] `internal/prompt.clj:124` wording updated to match the new reality
      (no longer claims `ctx` is unbound, if we bind it).
- [ ] EVAL (VIS-EVAL) shows the model successfully branching on its own budget
      at least once where it previously errored (ties to VIS-8c).

---

## VIS-2 — Run VIS on Codex (and other backends), A/B vs Opus

**Status:** open · **Depends on:** VIS-EVAL · **May spawn:** SVAR fix sub-tasks

> "Przetestować na Codexie jak działa VIS, bo u mnie na OPUS to jest po prostu
> jak PI. Nie widzę żadnej różnicy poza tym że trochę lepiej robi Clojure.
> Jeżeli się loopuje to znów jakaś poprawka do SVAR'a musi pójść."

### Context
VIS routes through SVAR (`internal/loop.clj`), which is provider-agnostic.
Providers live under `extensions/providers/` — `provider-anthropic`,
`provider-openai-codex`, `provider-standard` (openai/ollama/lmstudio),
`provider-github-copilot`, `provider-zai`. The Codex provider is
`extensions/providers/vis-provider-openai-codex/`.

### Rationale
The perceived value of the engine (gates, CTX, summarize/recall) should hold
across models. The user reports that on Opus the engine feels neutral ("like
π") — only marginal Clojure-quality gains, no behavioral lift from the
scaffolding. We need a **second strong backend (Codex)** to tell whether that's
a model artifact or a prompt/engine-design artifact. If the engine only helps
weaker models, that's a finding; if it helps none, the gates need rework.

### The loop concern
"If it loops → another SVAR fix." Loop/retry handling is already nontrivial in
`internal/loop.clj`: TTFT + idle watchdogs (`:57`–`:70`), retryable error set
(`:976`–`:982`), no-code retry logic driven by svar fence observations
(`:1186`–`:1197`, `:1423`+). Codex's streaming/fence shape may differ enough to
trip these. Expect to harden the no-code-retry and fence-normalization paths
per provider.

### Proposed approach
- Stand up a Codex run through the existing bench harness (see **EVAL** below;
  `dev/benches/4clojure/`, `swe-bench/`, `filewrite/`) so the comparison is
  measured, not vibes.
- Capture: loop/stall incidence, no-code-retry count, fence-detection misses,
  tool-call correctness, end-to-end task success. Same task set, Opus vs Codex.
- Any loop reproduced → isolate whether it's SVAR fence/normalization
  (`loop.clj` fence-observation block) or a prompt-gate issue, and fix at the
  lowest layer that reproduces it.

### Deliverable
A short A/B writeup (mirror the `INVESTIGATION.md` style referenced in
`ctx_engine_underuse` memory) with per-metric deltas.

### Acceptance criteria
- [ ] The same fixed task subset runs end-to-end on **both** Opus and Codex
      through the bench harness (VIS-EVAL), no manual babysitting.
- [ ] A writeup reports per-metric deltas: task success rate, iteration count,
      tool-call mix, loop/stall incidence, no-code-retry count, fence-detection
      misses, token/utilization.
- [ ] Every loop/stall reproduced on Codex is root-caused to a named layer
      (SVAR fence/normalization in `loop.clj` **or** a prompt gate) and either
      fixed or filed as a follow-up sub-task with a repro.
- [ ] Conclusion explicitly answers: does the engine scaffolding lift Codex,
      Opus, both, or neither — with the data to back it.

---

## VIS-3 — Surface known-language dependency paths in CTX up front

**Status:** open · **Relates to:** VIS-4, VIS-6 · **Verify via:** VIS-EVAL

> "Dla znanych języków powinniśmy mówić jakie są ścieżki ich depsów w
> kontekście od razu żeby nie trzeba było robić LSów i mnóstwa RG."

### Context
`internal/env_digest.clj` already guesses the primary language cheaply:
`primary-language-guess` (`:109`) peeks at top-level build files —
`clojure-lang-files #{"deps.edn" "project.clj" "shadow-cljs.edn"}` (`:106`) —
and `project-digest` (`:129`) stamps `:primary-language` onto
`:session/env :project`. But it stops at *what language*; it never says
*where that language's dependencies live on disk*.

So today the model discovers dep locations by hand: `ls ~/.m2`, `rg` across
`node_modules`, hunting the gitlibs cache, etc.

### Rationale
For known ecosystems the dep root is deterministic and boring:
- Clojure/deps.edn → `~/.m2/repository` + `~/.gitlibs/libs`
- Node → `./node_modules` (+ pnpm store)
- Python → the active venv `site-packages`
- Rust → `~/.cargo/registry/src`
- Go → `$GOMODCACHE` / `~/go/pkg/mod`

Pre-computing these and putting them in `:session/env` (or a dedicated
`:session/deps` key) saves a whole class of exploratory `ls`/`rg` iterations —
exactly the "locate-waste" the engine is supposed to cut (cf. memory
`ctx_engine_underuse`, which already drove locate-waste 13→0 via `:files`
facts).

### Proposed approach
- Extend `env_digest.clj`: when `primary-language` is known, resolve that
  ecosystem's canonical dependency roots and add them to the digest (e.g.
  `:project/dependency-roots [paths…]`). Keep it **cheap** — the file already
  warns that heavy scans are out of scope (`env_digest.clj:16`); resolve paths
  from env vars / well-known locations, do **not** walk them.
- Only emit roots that actually exist (stat the dir; skip otherwise).
- Document the new key in the ENTITY SHAPES section of `internal/prompt.clj`
  (the `:session/env` / `:session/workspace` block, `~:179`).
- Multi-language repos: emit roots for each detected ecosystem, not just the
  single `primary-language` guess.

### Watch-outs
- Don't bloat CTX — paths only, no listings.
- Respect monorepos / non-default stores (pnpm, custom `GOMODCACHE`,
  `CARGO_HOME`, poetry/uv venvs).

### Acceptance criteria
- [ ] `env_digest.clj` emits a new key (e.g. `:project/dependency-roots`) on the
      session env when `primary-language` is known.
- [ ] Only **existing** dep roots are emitted (dir stat'd; missing ones skipped).
- [ ] Resolution is **cheap** — paths from env vars / well-known locations,
      **no directory walks** (honoring the `env_digest.clj:16` no-heavy-scan rule);
      a test/bench confirms no added latency from a tree scan.
- [ ] Multi-ecosystem repos emit roots for each detected language, not just the
      single primary guess.
- [ ] The new key is documented in the ENTITY SHAPES block of `prompt.clj`.
- [ ] EVAL shows a measurable drop in exploratory `ls`/`rg`-for-deps iterations
      vs. baseline on at least one dep-reading task.

---

## VIS-4 — Fix the prompt so `rg` searches from root first

**Status:** open · **Relates to:** VIS-3 · **Verify via:** VIS-EVAL

> "Poprawić trzeba prompta, żeby RG leciało bardziej od roota a dopiero potem
> inne ścieżki."

### Context
`rg` is the foundation search tool. Its argument discipline is enforced in
`internal/foundation/introspection.clj` (`:236`–`:265`, `:561`): it takes
**one spec map** with literal vectors (`{:any […]}` / `{:all […]}`), not
regex strings or positional args. The system prompt teaches `rg` mostly by
example — `internal/prompt.clj:156` and `:164`:

```
(let [h (rg {…}) s (cat (:path (first h)))] s)        ; locate AND read
(let [hit (rg {:any ["defn add"] :path "calc.clj"})  ; … narrowed to a file
```

Both examples pass a **narrow `:path`** immediately. There is no guidance that
says "search broad from the workspace root first, narrow only after."

### Rationale
The model mirrors the prompt's examples and jumps straight to narrow,
guessed paths — which miss, then it widens, costing iterations. A root-first
default ("`rg` with no `:path` searches the whole workspace; only add `:path`
to narrow a confirmed hit") matches how a human greps and reduces miss-retry
loops. This dovetails with item 3 (known dep roots) — root-first for *project*
code, dep-roots for *library* code.

### Proposed approach
- Edit `internal/prompt.clj`: add an explicit ordering rule near the BATCH/
  search examples — search from workspace root first (omit `:path`), then
  narrow to a `:path` only once a hit is confirmed. Reframe the `:164` example
  to show the broad call first, then the narrowed follow-up.
- Verify the `rg` tool actually defaults to the **workspace root**
  (`*workspace-root*`, `internal/workspace.clj:37`) when `:path` is omitted —
  if it defaults to JVM cwd instead, fix the tool, not just the prompt.
- Confirm via the SEARCH line at `prompt.clj:265` ("use rg for files") stays
  consistent with the new ordering.

### Acceptance criteria
- [ ] `prompt.clj` contains an explicit ordering rule: search from workspace
      root first (omit `:path`), narrow to `:path` only after a confirmed hit.
- [ ] The `:164`-style example is reframed to show the broad call **first**,
      then the narrowed follow-up.
- [ ] Verified that `rg` with no `:path` defaults to `*workspace-root*`
      (`workspace.clj:37`), **not** JVM cwd — tool fixed if it doesn't.
- [ ] EVAL shows a measurable drop in `rg` miss→re-search cycles vs. baseline.

---

## VIS-5 — Subagents: recursive VIS with fork + shared/append-back context (DESIGN DOC)

**Status:** open (design only) · **Touches invariants:** one-atom-per-session,
1:1 session↔workspace · **Relates to:** VIS-6

> "Pomyśleć jak mają działać subagenci (rekursywny VIS z forkiem i
> współdziałanym kontekstem i dopiskami potem do kontekstu). Tutaj bym
> poprosił jakiegoś design doca 😄"

### Context
There's an `internal/agents.clj`, but today it only does **project-guidance
discovery** (reads `AGENTS.md`/`CLAUDE.md` into the system prompt) — it is NOT
a subagent runtime. There is currently no mechanism to spawn a child VIS that
runs its own loop and folds results back. The engine is built around **one
`:ctx-atom` per session** (`ctx_loop.clj:11`), and the workspace model is
**1:1 session↔workspace** (memory `workspace_architecture`), so subagents
touch two load-bearing invariants at once.

### Rationale
Recursive delegation (a parent VIS forking child VIS instances for
independent sub-tasks, then merging their findings) is the standard lever for
parallel exploration and context-budget relief — let a child burn its own
window on a sub-investigation and return only the distilled facts. But "fork +
shared context + append-back" is underspecified and high-risk, hence the user
explicitly asks for a **design doc first, not code**.

### Questions the design doc must answer
1. **Fork semantics** — does a child get a *copy* of the parent ctx
   (`empty-ctx` + seeded facts) or a *reference*? Given one-atom-per-session,
   a child almost certainly needs its **own** `:ctx-atom`.
2. **Workspace** — 1:1 session↔workspace is locked. Does a child share the
   parent's workspace root, get a git-worktree fork, or run read-only? (See
   `internal/workspace.clj` clone/trunk machinery, `:102`+.)
3. **Append-back contract** — what exactly merges into the parent on child
   completion? Proposal: children return **facts only** (the durable
   `:content` + `:files` shape), never raw trailer; parent decides what to
   `fact-set!`. This keeps the parent's CTX curated.
4. **Shared vs isolated memory** — "współdzielony kontekst" needs a precise
   meaning: read-through to parent facts at spawn time, but isolated writes?
5. **Concurrency / cancellation** — children must honor
   `internal/cancellation.clj` and not orphan streams.
6. **Provider/cost** — each child is its own SVAR session; budget accounting?
7. **Persistence** — children in the session DB
   (`reference_session_db_introspection` memory) as sub-sessions or inline?

### Deliverable
`docs/design/subagents.md` covering the seven questions above with a
recommended shape and an explicit list of invariants it would change. **No
implementation until the doc is reviewed.**

### Acceptance criteria (design phase only)
- [ ] `docs/design/subagents.md` exists and answers all 7 questions (fork
      semantics, workspace, append-back contract, shared-vs-isolated memory,
      concurrency/cancellation, provider/cost, persistence).
- [ ] The doc lists, explicitly, every locked invariant it would change
      (one-atom-per-session, 1:1 session↔workspace) and the migration risk.
- [ ] The append-back contract is concrete (proposal: children return **facts
      only**, parent decides what to `fact-set!`).
- [ ] The doc has a recommended shape + rejected alternatives, ready for review.
- [ ] **No implementation code lands** under this task.

---

## VIS-EVAL — Consolidate the eval harness into a first-class gate

**Status:** open · **Blocks measurement of:** VIS-1, VIS-2, VIS-3, VIS-4, VIS-8c

> "Aaa, no i EVAL"

### Context
Eval scaffolding already exists under `dev/benches/`:
- `4clojure/` — problem set + `vis_agent.py`, `eval_one.clj`, `metrics.py`,
  `instances.json`, an `autoresearch_runner.py`, and dated `results/`.
- `swe-bench/`, `filewrite/`, `ctx_metrics.clj`, `run.sh`, `test.sh`,
  plus W6 scripts (`w6_forced.sh`, `w6_hard.sh`, `w6_multiturn.sh`).

So eval isn't greenfield — it needs to become a **first-class, repeatable
gate**, not an ad-hoc collection.

### Rationale
Every other item here is a hypothesis ("CTX-as-var helps", "Codex differs",
"root-first rg cuts iterations", "dep-roots cut locate-waste"). Without a
standing eval we're back to vibes. The `ctx_engine_underuse` memory already
shows the payoff of measured A/B (task-set! 0→8, batching 0→12, locate-waste
13→0) — that rigor should be the default for all of items 1–4 and 8.

### Proposed approach
- Consolidate the bench scripts into one documented entrypoint with a fixed
  task subset and stable metrics (iterations, tool-call mix, success rate,
  loop incidence, token/utilization).
- Make it the measurement vehicle for: item 2 (Opus vs Codex), item 3
  (dep-roots → locate-waste delta), item 4 (root-first rg → miss-retry delta),
  item 1 (ctx-var → does the model branch on its own budget?).
- Emit per-run `INVESTIGATION.md`-style writeups (consistent with existing
  `ctx_metrics.clj` output) so deltas are auditable.

### Acceptance criteria
- [ ] One documented entrypoint runs a fixed task subset reproducibly (same
      input → comparable output), replacing the scattered ad-hoc scripts.
- [ ] Stable metrics emitted per run: iterations, tool-call mix, success rate,
      loop incidence, token/utilization.
- [ ] The harness supports an A/B mode (two configs/models, same task set) used
      by VIS-2.
- [ ] Per-run writeup is auto-generated in the `INVESTIGATION.md`/`ctx_metrics`
      style so deltas are auditable.
- [ ] README documents how to run it and read the output.

---

## VIS-6 — Folder scope: let the user pick which folders VIS works on

**Status:** open (decision needed) · **Relates to:** VIS-3, VIS-5 ·
**Touches invariant:** 1:1 session↔workspace

> "Rozkminić kwestię folderów — czy aktualnie to że VIS mówi 'oj ja nie będę
> sobie działać na innych folderach' jest okay, może trzeba dodać jakąś
> możliwość wybierania folderów."

### Context
VIS is scoped to a single workspace root via `*workspace-root*`
(`internal/workspace.clj:37`), rebound per-turn by the channel; `cwd` falls
back to `user.dir` (`:62`). The model is told it works inside the user's HOST
project (`internal/prompt.clj` IDENTITY block) and the sandbox forbids shell
escape (`:311`). The workspace model is **locked 1:1 session↔workspace**
(memory `workspace_architecture`). Net effect: VIS politely refuses to operate
outside its one root.

### Rationale
The single-root guarantee is a real safety property (no accidental writes
across the filesystem) and underpins the persistence/trunk model. But it's also
a usability wall: legitimate multi-folder work (a repo + its sibling library, a
monorepo subtree, reference reading in a dep checkout) is impossible. The
question is **whether** to relax it and **how** without losing the safety/1:1
invariant.

### Options to weigh (decision needed, ties to item 5)
- **A — keep single root, read-only widen:** allow `cat`/`ls`/`rg` outside
  root (read), keep `patch`/`write` strictly inside. Cheapest; covers the
  "read a dep" case (overlaps item 3).
- **B — explicit folder allowlist:** user adds extra roots to the session;
  tools accept any allowlisted root, refuse others. Needs a picker/command and
  per-tool root validation.
- **C — multi-workspace sessions:** breaks the locked 1:1 model — heavy,
  probably out of scope, defer to the subagent design (item 5) where a child
  could own a different root.

### Proposed approach
- Start with **Option A** (read-only widen) as the low-risk win; it directly
  serves item 3. Gate writes to `*workspace-root*` in the tool layer.
- If real multi-root *write* demand exists, design **Option B** alongside the
  subagent doc (item 5) so the two scoping models stay consistent.
- Whatever ships: the refusal message should be **honest about the boundary
  and how to extend it**, not a flat "I can't."

### Acceptance criteria
- [ ] A decision is recorded (Option A / B / C) with rationale.
- [ ] If Option A ships: `cat`/`ls`/`rg` may read outside `*workspace-root*`,
      while `patch`/`write` are **gated to the root** — proven by a test that a
      write outside root is refused.
- [ ] The single-root **write** safety property is preserved (no accidental
      cross-filesystem writes) regardless of chosen option.
- [ ] The refusal/boundary message tells the user how to extend scope, not just
      that it can't.
- [ ] Any multi-root *write* design (Option B/C) is deferred to / aligned with
      the VIS-5 subagent doc, not shipped ad-hoc.

---

## VIS-7 — System / Codex output should render as *internal*, not a pink bubble

**Status:** open · **Relates to:** VIS-8a (system ≠ answer)

> "No to co napisałeś z Codexem że może fajnie żeby tam to leciało jako
> internal a nie różowy bubble."

### Context
The TUI renders turns as bubbles (`extensions/channels/vis-channel-tui/.../
chat.clj`). User messages are `:role :user`, assistant answers
`:role :assistant` (`assistant-message`, `chat.clj:406`+). Engine-internal
probes and `:vis/silent` mutators are already special-cased in the trace
rendering (`chat.clj:51`–`:56`, the `:code`/silent handling) and in
`internal/render.clj` / `progress.clj` (the `:vis/silent` plumbing). The "pink
bubble" is the styled assistant/answer bubble; "internal" is the muted
trace/system styling.

### Rationale
Some content the model emits — Codex-style reasoning, system bookkeeping,
intermediate engine chatter — is **not a user-facing answer** and shouldn't
wear the prominent answer styling. Misclassifying it as a pink answer bubble
makes the transcript noisy and implies "this is the deliverable" when it isn't.
Routing it to internal/trace styling keeps the answer bubble meaning
"the actual reply."

### Proposed approach
- Pin down the exact content class meant here (Codex reasoning deltas? a
  specific provider message role? `:vis/silent` system forms that currently
  still bubble?). The reasoning-delta path is in `internal/loop.clj` (`:2245`,
  "Reasoning DELTA contract").
- Route that class to the **internal/trace** render path
  (`chat.clj` trace-bubble helpers, `:51`–`:117`) instead of
  `assistant-message`, so it shows muted, not as a pink answer bubble.
- Cross-check with item 8: `:vis/silent` system forms must not leak into the
  trailer/answer surface either — same underlying "system ≠ answer" principle.

### Acceptance criteria
- [ ] The exact content class is pinned down (Codex reasoning deltas / specific
      role / still-bubbling `:vis/silent` forms) and named in the task.
- [ ] That class renders via the **internal/trace** path (muted), not as a pink
      `assistant-message` answer bubble — confirmed visually and by a render test.
- [ ] Genuine user-facing answers still render as the answer bubble (no
      regression — the answer surface keeps its meaning).
- [ ] Restored transcripts render the reclassified content the same way as live
      (parity with `chat.clj` restored-bubble path).

---

## VIS-8 — `:vis/silent` coverage, `recall` correctness, and the `ctx` symbol error

**Status:** open · Three intertwined sub-tasks (VIS-8a / VIS-8b / VIS-8c)

> ":vis/silent … systemowe rzeczy nie dają głupotek do trailera. … recalla
> trzeba poprawić — nie jestem pewien czy potrafi robić recalla na from/do …
> recall coś mi śmierdzi. … w każdej sesji mam: (let [env (:session/env ctx)
> …]) Unable to resolve symbol: ctx"

This item is three intertwined bugs. Track them together; they share the
"system vs. user-visible" and "CTX surface" theme.

### VIS-8a — `:vis/silent` should fully suppress system noise in the trailer

**Context.** `:vis/silent` is the engine mutator sentinel: `task-set!`,
`fact-set!`, `summarize`, etc. return it (`ctx_loop.clj:150`, `:158`;
`ctx_engine.clj:651`–`:657`) so the form result isn't echoed. The render layer
honors it in several places (`render.clj:1350`, `:1449`; `progress.clj:198`–
`:264`; `loop.clj:2639`–`:2719`; tui `state.clj:391`). A toggle exists to
*show* silent forms in traces (`toggles.clj:392`).

**The concern.** "Not sure all system forms respect it." The handling is spread
across render/progress/loop/state — easy for some system path to slip a result
into `:session/trailer`. Note `progress.clj:222` and `:264` document that a
`:vis/silent` *result alone* no longer elides a code-bearing form — so the
suppression rule is subtle and worth auditing end-to-end.

**Approach.** Audit every producer that can write a trailer pin and confirm
system/`:vis/silent` forms are excluded by default (toggle off). Add a test
that asserts no `:vis/silent` system mutator leaves a trailer entry under the
default toggle. Files: `progress.clj`, `loop.clj` (trailer-pin build ~`:2574`–
`:2719`), `ctx_engine.clj` (`:651`).

**Acceptance criteria.**
- [ ] An audit lists every producer that can write a `:session/trailer` pin.
- [ ] Under the default toggle (show-silent OFF), no `:vis/silent` system
      mutator (`task-set!`/`fact-set!`/`summarize`/…) leaves a trailer entry —
      asserted by a test.
- [ ] The show-silent toggle (`toggles.clj:392`) still surfaces them when ON
      (no regression to the debug view).
- [ ] The subtle "code-bearing form is NOT elided by a silent result alone"
      rule (`progress.clj:222/264`) is preserved and covered by a test.

### VIS-8b — `recall`: verify from/to windowing actually works

**Context.** `recall` is the single recovery verb, dispatching on arg shape
(`ctx_loop.clj:368`–`:529`):
- RESTORE — `(recall {:ids […] :why …})` / `(recall {:scopes […] :why …})`,
  `:why` REQUIRED (`:425`–`:469`).
- WINDOW — `(recall "tN/iM/fK")` / `(recall :K)`, with `{:offset N}` to scroll;
  returns `:vis/window [from to]`, `:vis/next` literal next call
  (`prompt.clj:259`–`:264`; engine `recall-window`, `ctx_loop.clj:529`).
- SEARCH — `(recall {:match … :scope-after …})` plain-text history search
  (`:477`–`:523`).

**The concern.** "Not sure it can recall on from/to; recall smells off." The
windowing contract is `:offset` = CHAR position into `pr-str`, ~8000/window,
with `:vis/next` as the scroll cursor (`prompt.clj:260`–`:264`). Suspected
weak spots: the `from`/`to` (`:vis/window`) math, whether `:vis/next`
terminates correctly at end-of-value (it should be absent at end), and whether
`{:offset to}` round-trips exactly. The search path also explicitly excludes
summarized iters (`:519`-ish) and forbids raw FTS syntax (`:523`).

**Approach.** Write focused tests against `recall-window` for: offset round-
trip (`:vis/next` → feed back → no gaps/overlap), end-of-value termination
(`:vis/next` absent), tiny values (no window), and `:offset` past end.
Then verify RESTORE actually re-pins/re-lives entities and SEARCH ranking is
sane. Files: `ctx_loop.clj:420`–`:529`, engine `recall-window` / `recall-entity`
in `ctx_engine.clj`.

**Acceptance criteria.**
- [ ] WINDOW: `:offset` round-trips exactly — feeding `:vis/next` back produces
      no gaps and no overlap across the full value (test).
- [ ] WINDOW: `:vis/next` is **absent** at end-of-value; present mid-value (test).
- [ ] WINDOW: small values (< one window) return the whole value with no
      `:vis/next`; `:offset` past end is handled gracefully, not an error (test).
- [ ] RESTORE: `(recall {:ids …})` / `(recall {:scopes …})` actually re-live /
      re-pin entities, each stamped `:recalled {:scope :why}`; `:why` enforced.
- [ ] SEARCH: `(recall {:match …})` returns sanely-ranked hits, excludes
      summarized iters, and rejects raw FTS syntax with a clear hint.
- [ ] Any defect found is fixed (not just documented).

### VIS-8c — `ctx` symbol: the recurring `Unable to resolve symbol: ctx`

**Context.** This is the concrete failure that motivates item 1. The model
keeps writing forms that assume a `ctx` binding:

```clojure
(let [env (:session/env ctx)
      utilization (:session/utilization env)
      limit (or (:model-input-limit utilization)
                (get-in ctx [:session/env :session/utilization :model-input-limit]))]
  (if limit {:answer "Vis d…"} …))
;; => Unable to resolve symbol: ctx
```

It fails because, per `prompt.clj:124`, `ctx` is deliberately unbound (TEXT,
not a binding). The model reads the EDN, sees `:session/env`/
`:session/utilization`, and reasonably *assumes* a `ctx` value it can address.

**The tension.** Two consistent resolutions:
1. **Bind it** (item 1) — give a read-only `ctx`/`(session)` snapshot so these
   forms just work. Best ergonomics; the model clearly wants it.
2. **Teach harder** — strengthen `prompt.clj:124` so the model stops emitting
   `ctx`-addressing forms and instead reads the EDN it's shown.

Note the example also mislocates the limit: it's
`:session/env :session/utilization :model-input-limit` in one branch but the
top-level shape in the prompt is `:session/utilization` directly under ctx
(`prompt.clj:171`). So part of 8c is **shape confusion**, not just binding.

**Approach.** Resolve jointly with item 1. If we bind a read-only snapshot,
this error disappears and 8c becomes "document the accessor + fix the shape
example." If we don't, tighten the prompt wording and add a clear "to read
your own budget, look at `:session/utilization` in the ctx EDN — there is no
`ctx` value to dereference." Either way, **make the model's natural instinct
either work or stop happening** — the current middle ground (it tries, it
fails, every session) is the worst case.

**Acceptance criteria.**
- [ ] A decision is recorded: bind a read-only `ctx`/snapshot (→ VIS-1) **or**
      tighten the prompt to stop the model emitting `ctx`-addressing forms.
- [ ] The shape confusion is fixed: the prompt's utilization path is internally
      consistent (`:session/utilization` location agrees everywhere;
      `prompt.clj:171` vs the example in the dump).
- [ ] EVAL over a representative session shows the `Unable to resolve symbol:
      ctx` error no longer occurs (either it resolves, or the model stops
      emitting it).

---

## Session log — 2026-06-07

Record of what shipped this session (for continuity; details in commits):

- **SCI→Python migration finished** — dual path removed, GraalPy canonical,
  native-Python tools, `:op` result keys (dropped `:vis.op`), two-context hang
  fix. (`0d6d2ad7`, `8b98ca0f`)
- **All agent-facing prompts wired to native Python** — aliased tools fold to
  flat snake (`git/status`→`git_status`, `exists?`→`is_exists`); option keys
  snake/`is_` so Python dicts reach the tools; git/search/bridge/clj + foundation
  prompts/errors/docstrings converted. (`eeff02ef`, `28cfcf92`, `b88a5271`)
- **`ls()` 0-arg** lists cwd (was ArityException → groped). (`8b98ca0f`)
- **Emoji/astral done-loop fixed** — GraalPy `ast.get_source_segment` truncated
  per-form source on non-BMP chars (emoji in `done("""…""")`), causing a
  spurious "unterminated string" SyntaxError → the answer never finalized → the
  model looped re-emitting `done()`. Replaced with a pure-Python codepoint slice;
  regression-tested. (`92cc9eb6`) — root-caused from session `f41ca531`.
- **Tool result shapes** — observation tools reshaped to state-the-shared-thing-once
  (`ls` dir-groups, `git_status` code-groups, `git_blame` commit-legend, `git_log`
  slim-commit); ~42% token savings measured on representative live data via
  `dev/benches/shape_metrics.clj`. (`826ca564`, `c366541b`, `26845874`, `7f82dfad`)

---

## Suggested sequencing

1. **VIS-EVAL** — harness consolidation; unblocks measuring everything else.
2. **VIS-8c + VIS-1** together — decide the `ctx` binding question; it's the
   most-hit daily failure and gates the prompt wording.
3. **VIS-4** (root-first rg) + **VIS-3** (dep-roots) — cheap, measurable
   locate-waste wins; verify via VIS-EVAL.
4. **VIS-8a / VIS-8b** — silent-coverage audit + recall windowing tests.
5. **VIS-7** — internal-vs-bubble rendering cleanup.
6. **VIS-2** — Opus vs Codex A/B once the above stabilize the engine.
7. **VIS-6 + VIS-5** — folder scope and subagents; design docs first,
   they share the workspace-scoping model and are the heaviest changes.
