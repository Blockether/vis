# ctx-engine investigation & redesign — session `b117af1a`

**Date:** 2026-06-04
**Source session:** `b117af1a-9cd8-4374-aeca-052b286b0a11` — *"Git Commit and Push Commands"*
(TUI close-button work → commits `80b77ca` / `5757dda3` / `1a56fbd3` …)
**Method:** Restarted dev nREPL (killed stale 1.8-day JVM → fresh on :7888), loaded the
session from `~/.vis/vis.mdb/vis.db` via the `persistance` facade, inspected the
engine's *ctx state + per-iteration thinking/code* — NOT the polished answers.
**Shape:** 4 turns, 41 iterations.

> **Headline (user's framing):** *the prompt is broken and most of this is prompt
> work.* The engine has a rich tasks/facts/recall/summarize System-2; the prompt
> fails to make the model use it, fails to force thinking, and is too long. The
> file-ledger idea is **rejected** (see W2). Everything below is to be decided
> empirically on the benchmarks (W6), then walked one-by-one.

---

## Baseline evidence (the diagnosis — all confirmed)

| Signal | Measured | Meaning |
|---|---|---|
| `task-set!` calls | **0 / 41 iters** | model never plans; task system unused |
| `fact-set!` calls | **0 / 41 iters** | model never stores durable knowledge |
| `:session/tasks` | **`{}`** | no commitments, no acceptance, no verification surfaced |
| facts present | 0 model `fact-set!`; only `:done-auto` (engine auto-logs each turn's answer) + `:done-summarize` (model-folded those answer-facts) | the model never stored a NEW discovery |
| forms-per-iter | **`{1 41}`** | one tool per iter, never batched (engine supports N) |
| `recall` calls | **1 / 41** | recovery verb essentially unused |
| locate-before-edit iters | **6 of 41** (turns 2+3) | re-discovering an already-edited file every follow-up turn |

**Stored memory is path-blind:** facts/trailer hold bare names (`components.clj`),
never `extensions/channels/vis-channel-tui/src/.../components.clj`. So turns 2 & 3
each *opened* with `v/rg`+`v/cat` to re-find the file they'd edited the prior turn.

**Engine capabilities that already exist but go unused/under-leveraged:**
edit/read tools return `{:path …}`; the loop captures per-form `:result` envelopes;
`iteration/entry-ops` runs N forms + nested tool calls per fence; `recall`
(window/restore/search); `summarize` (N→1, archives originals, recoverable).

### Frozen baseline scorecard (reproduce with the W6 probe)

Tool: `dev/benches/ctx_metrics.clj` (ns `benches.ctx-metrics`). In the dev nREPL:
```clojure
(require 'benches.ctx-metrics :reload)
(def db (benches.ctx-metrics/open-db "/Users/fierycod/.vis/vis.mdb"))
(benches.ctx-metrics/report db #uuid "b117af1a-9cd8-4374-aeca-052b286b0a11")
```

```
turns=4 iters=41  prompt=11031 chars  tokens in=888770 out=18861
task-set!=0  fact-set!=0  ctx-add!=0  recall=1
forms-per-iter={1 41}  multi-form-iters=0
locate-before-edit waste=13 iters
final engine state: tasks=0 facts=3 (model-authored facts=0)

turn iters task fact recall multi locate  in-tok  out-tok
  1    16    0    0    1     0     6     483773  10763
  2     9    0    0    0     0     3     164486   2406
  3    10    0    0    0     0     3     156673   4985
  4     6    0    0    0     0     1      83838    707
```

**New finding — token blowup.** 888K input tokens for a 4-turn one-file TUI tweak
(turn 1 alone = 483K). Each iter re-carries full-file observations; nothing is
distilled to ranges. This is the cost W1 (actionable summarization → drop big
observations) and W5 (auto-summarization safety) must bring down. It is now a
first-class signal in the scorecard (`in-tokens-total`).

These eight numbers are the **A/B targets**: every W1–W5 change is judged by moving
them in the right direction (task/fact > 0, forms-per-iter shifts off `{1 N}`,
locate-waste → ~0 on follow-ups, in-tokens down) without regressing bench pass-rate.

---

## Corrected direction — workstreams (walk one-by-one)

### W1 — Summarization must be ACTIONABLE, not prose
**Problem.** Summaries (trailer pins, and `:summarize` in `done`) say *"something was
changed there"* / *"explored auth"*. They lose the one thing that matters: **where**
and **what's interesting**. We read big files, do research, decide what matters —
then throw that judgement away at summarization.

**Direction.** Every summary about a read/changed file should carry:
- **full path** of the interesting file,
- the **interesting line ranges**,
- **what is there** + why it matters / what changed vs not.

So we can **drop the big full-file observations from the prompt** and keep only the
surgical ranges. The model should never re-read a whole file — it keeps
"`path` lines a–b: `<what>`". The judgement made during research is *preserved into*
the summary instead of discarded.

**Decided.** Source = **model-produced** (model knows what's *interesting*). Couples
W1 to W4 (prompt must force it). Renders as plain EDN on the summary stub (pins are
already EDN; no renderer change).

**Status: contract IMPLEMENTED + tested (2026-06-04).** Behavioral validation (does
the model actually emit `:files`, does locate-waste/tokens drop) is pending a W6
bench run.
- `ctx_spec.clj` — `::trailer-summary` gains optional `:files`. Final shape,
  aligned to vis's **native hashline editing**:
  ```clojure
  :files [{:path "full/path.clj"
           :regions [{:src       "<verbatim text>"  ; MEMORY + :search fallback (required)
                      :note      "what/why"
                      :from-hash "a1b2"             ; native hashline edit address
                      :to-hash   "c3d4"}]}]         ; range end (defaults to from-hash)
  ```
  Design decisions (each from review):
  - **Content, not labels.** Regions carry the **verbatim `:src`** so they're
    readable/editable from memory — a summary without the text forces a re-read,
    and a bare hash is opaque. `:src` is the required field.
  - **Hashline anchors, not line numbers.** vis edits by content hash:
    `v/cat` shows `<ln> <hash>│ text`, `v/patch {:from-hash :to-hash :replace}`
    resolves those per-line hashes against LIVE content. Line numbers drift AND
    aren't the edit address (gutter uses hashes), so the region carries
    `:from-hash`/`:to-hash` (copied from the cat gutter) → re-patch from memory,
    no re-cat. `:lines` dropped.
  - **No content hash.** A drifted region just fails to patch — stale `:src`
    misses on `:search`; a stale `:from-hash` yields `:hash-not-found` ("re-read
    with v/cat"). Self-validating; a file hash would be redundant + unsupplyable
    by the model. *(Considered and dropped.)*
- `ctx_engine.clj` `apply-trailer-summarize` — carries `:files` onto the stub
  (`cond-> … (seq files) (assoc :files …)`), back-compatible.
- `ctx_renderer.clj` — no change; summary pin `(zp pin)` auto-renders `:files`.
- `prompt.clj` — terse MUST-add-`:files` clause in the `:summarize` block.
- `ctx_engine_test.clj` — `trailer-summarize-files-test` (6 cases). Full ns 114/114;
  done-test 18/18. (Fixed a generative-test regression my first spec draft caused:
  `s/and vector? (s/cat …)` → `s/tuple`.)

**W1b — `v/cat` read-by-hash (the round-trip).** vis already edits by content hash
(`v/patch :from-hash`/`:to-hash`) but could only *read* by line number — so a kept
region's stored hashes had no read path back. Closed that:
- `editing/patch.clj` — factored `resolve-hash-range` (1-based inclusive line span
  from a hash pair, with not-found/ambiguous/inverted errors); `resolve-hash-edit`
  now reuses it, so READ and WRITE address lines by content identically.
- `editing/core.clj` — `read-file-by-hash` (slurp → `resolve-hash-range` → window)
  + `cat-tool` arities `(v/cat path :hash H)` and `(v/cat path :hash H1 H2)`
  (positional, per the project args rule) + `hash-read-error-message` pointing back
  to a fresh read. Docstring updated.
- `core_test.clj` — `vis-cat-hash-read-test`: single line, range, **drift survival**
  (prepend shifts line numbers; the hash still finds the line), missing-hash throw,
  bad-mode throw. Verified live in the dev nREPL against a temp file.
- `prompt.clj` — `:files` clause notes refresh via `(v/cat path :hash from to)`.
Now hashline is symmetric: read-by-hash + edit-by-hash, both drift-proof.

**W7 — foundation-core promoted to a CORE module (DONE; de-alias W7c pending).**
Relocated the whole `vis-foundation-core` extension → `src/com/blockether/vis/
internal/foundation/**` (ns `…ext.foundation-core.*` → `…internal.foundation.*`);
tests → `test/…/internal/foundation/**`. Folded deps (babashka.fs, java-diff-utils)
into root `deps.edn`; removed the `:local/root` + 2 test-path lines; deleted the
extension dir + its `META-INF/vis-extension/vis.edn` manifest. Internal loads it as a
BUILT-IN: `extension/discover-extensions!` → `load-builtin-extensions!`
(`builtin-extension-nses '[…internal.foundation.core]`) BEFORE the third-party
manifest scan, so its top-level `register-extension!` fires from core — the
"internal registers tools/renderers" capability — while the public extension API
stays intact. Verified: clj-kondo **0 errors**; live nREPL — foundation loads, all 16
extensions register incl. `foundation-core`, `v/cat :hash`/`resolve-hash-range`
resolve; the one move-caused test (`foundation/core_test` "ships a manifest")
rewritten → "registers as a BUILT-IN" (6/6). Other ~31 suite failures are
PRE-EXISTING (render-contract migration in `*/render_test`, TUI fixtures, slash
content drift) — only 1 structural fingerprint in the full run, now fixed.
**W7c — de-alias built-ins to BARE symbols (DONE).** Foundation is no longer aliased
`v/`; its tools bind BARE into the sandbox ns next to the engine verbs:
`(cat …)`, `(patch …)`, `(rg …)`, `(ls …)`.
- `:ext/sci` gains `:ext.sci/builtin? true` (replaces `:ext.sci/alias 'v`);
  `ext-builtin?` accessor; spec predicate `ns-alias-required-when-symbols?` accepts
  built-ins; `wrap-extension-thunked` (env via thunk) + `builtin-sandbox-bindings`
  merged into `env-bindings` (loop.clj) so symbols intern bare at sci-context
  creation, like `done`/`task-set!`.
- **op-tag classification works automatically**: `tool-call-name` returns `(str sym)`
  with no alias → canonical op-keywords are bare (`:cat`, `:patch`), so
  `op-keyword->tag` + the head-tag-resolver classify bare `(patch …)` as `:mutation`.
  Verified live: `op-tag :patch → :mutation`, `:cat → :observation`.
- **render-fns preserved** (the explicit requirement): bare symbols route through the
  same `invoke-symbol-wrapper → write-sink-entries! → render-fn` pipeline; `cat`/`ls`/
  `rg`/`patch` all carry `render-fn? true`.
- **Reword:** 650 `v/<sym>` → bare across 50 files (prompt, docstrings, error
  messages, op-labels, tests). op-friendly-labels keyed bare (`:cat` → READ);
  3 fallout tests updated (CAT→READ label now correct for the bare op).
- **Prompt fold:** built-in extension prompts render header-less + first
  (`extension-prompt-fragment` / `extensions-prompt-block`), so foundation's prompt
  reads as CORE, not a `;; -- EXTENSION --` fragment.
- *Deferred:* a few cosmetic `v/` doc-text mentions to non-tool symbols
  (`v/cwd`, `v/grep`, `:v/tool`, `(v/extensions)`); proper prompt OPTIMIZATION → W4
  (measured on benches, per the "judge on benches" principle).

**Open / follow-ups.**
- **Graduation to facts (W2).** `:files` records live on the trailer stub, which
  can itself be re-summarized away — W2 decides whether/when they become durable
  facts.
- **Bench-validation prerequisites.** Behavioral proof needs (a) the prompt to
  actually force `:files` (W4), and (b) a MULTI-TURN scenario — the existing
  single-shot benches (`filewrite`/`4clojure`) don't reproduce the cross-turn
  re-location that W1 targets. See W6 note.

---

### W2 — Facts vs Tasks DECISION MATRIX (replaces the file-ledger idea)
**Rejected:** a workspace/file ledger like git. *"In git we know how to do it, but
what if we have a draft — then it's not easy; the idea is completely broken."*
File-location knowledge belongs in **facts**, not a separate ledger.

**Problem.** No rules for what is a fact vs not, what is a task vs not. The model
guesses → writes neither.

**Direction.** An explicit decision matrix the prompt enforces:
- **Fact** = durable knowledge that outlives the turn (a file path + interesting
  range, a decision, an invariant, a discovered API shape). Immortal until superseded.
- **Not a fact** = transient probe output, a full file dump, one-off scratch.
- **Task** = a commitment with acceptance criteria (see W3).
- **Not a task** = a single trivial action.

W1's actionable summaries are what *become* facts (path + range + what).

**Open questions.** Crisp, short heuristics that fit a tight prompt. Should the
engine *nudge* ("you patched a file but stored no fact about it")?

**Status: DONE (graduation) — model-driven.**
- Facts now carry the SAME structured `:files` regions as trailer summaries:
  `ctx_spec.clj` `::fact` gains optional `:session.fact/files`
  (reuses `::trailer-summary-file`: path + verbatim `:src` + `:from-hash`/`:to-hash`
  + `:lines`). So file knowledge graduates from a transient trailer stub to an
  immortal fact, directly re-patchable by hash — never re-`cat`.
- `apply-fact-set!` already merges the upsert map, so `:files` rides through with NO
  engine change; `ctx_renderer/project-fact` now surfaces `:files`.
- Decision matrix lives in the prompt GATES (PLAN=task, REMEMBER=fact) + the
  `fact-set!` doc shows `:files`. Graduation is **model-driven** (the GATES tell the
  model to `fact-set!` a file fact) — not auto, keeping the engine simple and
  consistent with W1's model-produced choice.
- Tests: `fact-files-test` (carries-through, born/id stamp, spec-valid, back-compat)
  + the ctx generative test passes with the new fact `:files` generator. ctx-engine
  ns 118/118.
- *Deferred:* an engine nudge ("patched a file, stored no fact") → could be a
  `:session/warnings` entry; left for later (advisory, low-risk).

---

### W3 — Task system: enforce + lifecycle + verification/acceptance + USER visibility
**Problem.** The system: doesn't create tasks, doesn't enforce tasks, doesn't show
the user which tasks were done, says nothing about **verification** or **acceptance
criteria**. *"This is all wrong and has to be fixed."*

**Direction.**
- **Enforce** task creation for multi-step turns (prompt gate, maybe engine warning).
- **Acceptance criteria** + **verification** as first-class fields on a task — a task
  isn't `:done` until its verification passed.
- **Surface to the user**: which tasks exist, their status (todo/doing/done), and
  the verification result — in the channel UI, not buried in ctx.

**Open questions.** Task schema additions (`:acceptance`, `:verify`, `:verified?`).
How does `done` self-assertion interact with verification (engine doesn't verify
claims today)? What does the user-facing task panel look like (TUI render)?

---

### W4 — Prompt: shorter AND forces thinking
**Problem.** ~205-line system prompt; documents verbs but exhorts abstractly
("Engine fns ARE your System 2") with **no concrete triggers**, and it **does not
force the model to think/plan** before acting. Too long, low leverage.

**Direction.** Cut length; replace philosophy with **concrete decision gates**:
when to open a task, when to write a fact, when to batch forms, when to summarize.
Make planning/thinking a required step, not a suggestion.

**Open questions.** What's the minimal prompt that still produces tasks/facts/
batching? Measure prompt-size vs behavior on the benches (W6).

---

### W5 — Auto-summarization safety (don't kill important stuff)
**Problem.** Auto-summarization (engine folds oldest trailer under size pressure)
can **compress away something important**. It's a blunt instrument.

**Direction.** Make auto-summarization importance-aware / reversible-by-default:
never silently drop pinned/fact-backed content; prefer recoverable stubs; maybe
protect ranges the model marked important (ties to W1). Investigate concrete
scenarios where it currently loses signal.

**Open questions.** What heuristic marks "important"? Is recall enough of a safety
net, or do we need pin-protection? Reproduce a loss case from real sessions.

---

### W6 — Benchmark-driven evaluation (the decision loop for W1–W5)
**This is how we decide what's good/bad — not by intuition.** Existing harnesses in
`dev/benches/`:

| Bench | What it tests | Scorer | Runner |
|---|---|---|---|
| `filewrite` | multi-file edits (our exact scenario) | `verify` form per problem → `:pass` | `dev/benches/filewrite/` |
| `4clojure` | write one solving form | tests → `pass_pct`, traces per task | `dev/benches/4clojure/run_subset.sh` |
| `swe-bench` | SWE-bench Lite | `resolved_pct` | `dev/benches/swe-bench/run_subset.sh` |

Each problem has objective verification (e.g. filewrite `fw-001`: `verify` asserts
`(= 5 (core/add 2 3))` → `:pass`), produces `predictions.jsonl` + `summary.json` +
per-task traces. `vis_agent.py` drives Vis; results live under `results/<ITER_TAG>/`.

**Plan.** For each candidate prompt/summarization/task change:
1. Run the relevant bench subset (start `filewrite`, it mirrors the real failure).
2. Capture pass-rate **and** the engine signals from this doc's baseline table
   (tasks created, facts stored, forms-per-iter, locate-before-edit iters,
   prompt-size, tokens/turn).
3. A/B old vs new; keep changes that improve pass-rate and/or the efficiency
   signals without regressions. A "judge" can grade trace quality where pass/fail
   is too coarse.

**Open questions.** Do current benches emit the engine signals, or do we add a
metrics pass that reads the trace ctx (like this investigation did)? Which subset is
fastest to iterate on?

---

## How we proceed
Walk W1 → W6 one at a time. For each: agree the design, implement behind the bench
harness (W6), measure against the baseline table, keep or revert. W6 gets stood up
early enough to measure W1/W3/W4 (the highest-leverage prompt changes).

---

## Reference — key files
- `src/com/blockether/vis/internal/ctx_engine.clj` — tasks/facts/recall/summarize
  (`auto-fact-for-turn-answer` ~1203; `apply-summarize` ~1142; `recall-*` ~1297-1351;
  `pick-oldest-batch-for-summarization` ~947; `gc-pass` ~698).
- `src/com/blockether/vis/internal/ctx_renderer.clj` — ctx render incl.
  `:session/workspace` (~365); where W1 ranges + W3 task panel would render.
- `src/com/blockether/vis/internal/iteration.clj` — N-form exec (`form->ops`/
  `entry-ops` ~139-155); `block-batch-hints` (~350, intra-block only).
- `src/com/blockether/vis/internal/ctx_loop.clj` — per-form `:result` envelopes;
  recall bindings (~260-395).
- System prompt — rendered ~205 lines (W4 target). Dump:
  `(:llm-system-prompt <iter>)`.
- Benches: `dev/benches/{filewrite,4clojure,swe-bench}/` (W6).
- Edit tools: `extensions/common/vis-foundation-core/src/.../editing/core.clj`
  (`patch-arg-paths` ~164 already extracts `:path`s — feeds W1).
