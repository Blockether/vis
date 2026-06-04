# ctx-engine investigation — session `b117af1a`

**Date:** 2026-06-04
**Source session:** `b117af1a-9cd8-4374-aeca-052b286b0a11` — *"Git Commit and Push Commands"*
(TUI close-button work; the commits `80b77ca` / `5757dda3` / `1a56fbd3` …)
**Method:** Restarted the dev nREPL (killed stale 1.8-day-old JVM pid 71887 → fresh pid
on :7888), loaded the session straight from `~/.vis/vis.mdb/vis.db` via
`persistance` facade, and inspected the engine's *ctx state + per-iteration
thinking/code* — NOT the polished `:answer-markdown`.

**Shape:** 4 turns, 41 iterations total.

| turn | user request | iters | status |
|---|---|---|---|
| 1 | suppress ✕ when only one session | 16 | done |
| 2 | add a `│` before the ✕ | 9 | done |
| 3 | make it read like `│ ✕ ` (`| x `) | 10 | done |
| 4 | add all, commit, push | 6 | done |

---

## The four issues (all confirmed with hard evidence)

### 1. No record of *which files we edited and where* — STRUCTURAL

- `:session/workspace` carries **only VCS coords** (`:workspace/root`, `:vcs/kind`,
  `:vcs/head`, `:vcs/dirty?` …). There is **no touched-files list**.
- `:session/tasks` = `{}`, and the only `:session/facts` are engine-auto
  (`:done-auto` / `:done-summarize`). They mention **bare filenames**
  (`components.clj`, `header.clj`) — **never the real path**
  `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/components.clj`.
- Edit/read tools DO return `{:path …}` (`v/patch`, `v/cat`, `clj/edit`), and the
  loop already captures per-form `:result` envelopes per scope — **but that trace is
  ephemeral**: it gets summarized down to bare filenames at turn close.

**Verdict:** the path data exists at runtime; nothing persists it into durable,
rendered state. Cheap to harvest, currently dropped.

### 2. Model stored ZERO facts of its own — PROMPT

- Across all 41 iters: `fact-set!` = **0**, `ctx-add!` = **0**.
- 100% of facts present are engine-generated. The model never promoted a single
  discovery (a file path, a key symbol, a decision) to durable memory.
- The system prompt *documents* `fact-set!` and calls facts "immortal knowledge"
  / "Engine fns ARE your System 2" — but gives **no concrete trigger** for WHEN to
  write one.

### 3. Tasks were NEVER created — PROMPT

- `:session/tasks` = `{}`. `task-set!` = **0** across all 41 iters.
- The engine's entire planning/commitment system went **completely unused**. The
  model operated as a pure tool-runner.
- Same root cause as #2: the prompt exhorts abstractly but never says *"a multi-step
  turn opens with a task."*

### 4. One form per iteration — never batched — PROMPT (engine already supports N)

- forms-per-iter distribution = **`{1 41}`** — every single iteration emitted
  exactly one top-level form.
- The engine **fully supports N forms per fence** AND nested tool calls inside a
  `let` (`iteration/entry-ops`, `iteration/form->ops`: "ops from every form flatten
  into one ordered op list"). The capability is there and unused.
- The prompt's heavy **"Each iteration → exactly ONE ```clojure``` fence"** framing
  reads as "do one thing"; nothing encourages composing independent/deterministic
  steps into one fence.
- The existing nudge `iteration/block-batch-hints` only fires for **≥5 of the same
  op inside ONE block** (a `doseq` fan-out). It is **structurally blind** to the
  dominant waste here: one form per iter spread across many iters.

---

## Measured cost (the symptom)

Both follow-up turns *opened* by re-discovering a file they had edited the
**previous** turn:

| turn | iters | locate-iters before first edit | opening forms |
|---|---|---|---|
| 1 | 16 | 6 | `v/rg` ×3 — legit first discovery |
| 2 | 9 | **3** | `v/rg`, `v/rg`, `v/cat` → re-locating `components.clj` |
| 3 | 10 | **3** | `v/rg`, `v/cat`, `v/rg` → re-locating `components.clj` |
| 4 | 6 | 1 | `apropos "git"` |

- **6 of 41 iters (turns 2+3) were pure re-location of an already-edited file.**
- ~32% of all iters went to locating before the first edit.
- Cost scales linearly with every follow-up turn (no memory of file locations →
  re-grep every time).

`recall` (the recovery verb: window / restore / search) exists but is **reactive** —
the model must think to call it. It was called **once** in the whole session.

---

## Root-cause split

- **#1** is structural — the engine needs a durable, auto-maintained file-edit ledger.
- **#2 / #3 / #4** are mostly the **prompt** — the machinery exists; the prompt fails
  to make the model *use* it (no concrete task/fact triggers, no batching guidance),
  and the batch-hint can't see the real pattern.

---

## Candidate strategy (to discuss one-by-one)

> We will walk these in order; this is the menu, not a commitment.

1. **Engine: durable file-edit ledger.**
   Harvest `:path` from patch/edit (and optionally read) results into
   `:session/workspace :files` (or a new `:session/files`) — `path → {:last-touched
   scope :op :one-line}`. Auto-rendered every turn (the `:session/workspace` slot is
   already rendered and currently empty). Survives summarization (structured, not
   prose). Needs: render slot, GC/TTL policy, size cap. Belt-and-suspenders — works
   even if the model never `fact-set!`s a path. Kills #1 + the re-location waste.

2. **Prompt: concrete task triggers (#3).**
   Replace abstract exhortation with WHEN rules, e.g. *"A turn with ≥2 dependent
   steps opens with `(task-set! …)`; flip `:doing`/`:done` as you go."*

3. **Prompt: concrete fact triggers (#2).**
   e.g. *"Located a file/symbol you'll reference again → `fact-set!` its path. A
   decision that outlives the turn → `fact-set!` it."*

4. **Prompt: batching guidance (#4).**
   State the fence holds N forms; show the locate→read `let` pattern
   (`(let [src (v/cat (:path (first (v/rg …))))] src)`); tell it to read multiple
   files in one fence.

5. *(Optional)* **Cross-iter batch hint.**
   Extend `block-batch-hints` to detect one-form-per-iter streaks across iterations,
   not just intra-block fan-out.

6. *(Optional)* **Proactive recall** at turn entry.
   When a new request references entities/files in archived facts or the file
   ledger, auto-surface them (so recall isn't purely reactive).

---

## Validation plan (when we implement)

- **Replay** the same 4 user turns through the patched engine/prompt on the live
  nREPL; measure against this baseline:
  - tasks created: **0** → expect > 0 on multi-step turns
  - facts stored by model: **0** → expect file paths / decisions captured
  - forms-per-iter: **`{1 41}`** → expect batched iters
  - locate-before-edit iters: **6 wasted** → expect ~0 on follow-up turns
- **Unit tests** for the engine ledger: harvest path → `:session/workspace :files`,
  render, GC/TTL, size cap.

---

## Reference — key files

- `src/com/blockether/vis/internal/ctx_engine.clj` — tasks/facts/recall/summarize
  (`auto-fact-for-turn-answer` ~1203, `recall-window`/`recall-entity` ~1297-1351).
- `src/com/blockether/vis/internal/ctx_renderer.clj` — `:session/workspace` render
  (~365).
- `src/com/blockether/vis/internal/iteration.clj` — N-form execution
  (`form->ops`/`entry-ops` ~139-155), `block-batch-hints` (~350).
- `src/com/blockether/vis/internal/ctx_loop.clj` — per-form `:result` envelopes,
  recall bindings (~260-395).
- Edit tools: `extensions/common/vis-foundation-core/src/.../editing/core.clj`
  (`patch-arg-paths` ~164-171 — already extracts `:path`s).
- System prompt: rendered ~205 lines; documents `task-set!`/`fact-set!`/`recall`
  but no batching/trigger guidance.
