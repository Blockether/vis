# SPEC — maki-style execution-engine redesign

Status: DRAFT / planning. Owner: Karol. Source of decision: vis-vs-pi (GLM-5-Turbo)
eval + maki (https://maki.sh) comparison (June 2026).

## 0. Why

The eval showed vis matches pi on correctness but is heavier and more fragile:
per-form `r[...]` auto-capture bloats the wire, the system prompt is ~7k tokens,
and `done()` + the runtime `done-gate` is the thing that broke (tag detection).
maki — same species (Python code-execution sandbox, intermediate data stays out
of context) — is leaner because it (a) terminates turns natively (no `done`
verb), (b) surfaces only what the model `print`s, (c) ships a lean prompt.

This redesign adopts maki's model **on top of vis's fenced-Python substrate**.
We keep what's ours (fenced python, `summarize`/compaction, the live session
dict, multi-channel) and drop the heavy/fragile parts.

## 1. The new model (target state)

1. **No `done()`. Termination by fence type — `python` XOR `answer`.**
   A reply is EITHER a ```python block (act → results come back next reply, turn
   continues) OR an ```answer block (final markdown answer → turn ends). Never
   both. The `answer` block content IS the answer.

2. **`print(...)` is the ONLY way to put data in context (maki-style).**
   No `emit`, no `r[...]`. A bare tool call's return value is NOT surfaced; the
   model `print`s what it needs to see/keep. Plain variables are scratch.

3. **Context is PER ITERATION, not per form.**
   One reply = one iteration block on the wire. Every `print()` in that reply
   appends to that single block (stdout-style), in order. No `r["tN/iN/fN"]`
   addressing.

4. **`summarize` / `drop` operate per ITERATION.**
   Targets are iteration ids `tN/iN` (collapse a whole iteration's printed
   output to a gist), not form scopes. Encouraged to run early/often.

5. **Locals persist across iterations (REPL globals), scratch-by-default.**
   The model keeps state in variables; surfaces conclusions with `print`.
   (Cross-PROCESS resume: see Decision D3.)

6. **Lean maki-style system prompt** replacing the ~7k CORE prompt.

7. **`ctx` is renamed** (Decision D4) — the read-only live session dict stays
   (static embed + deltas), only the identifier changes.

8. **Async Python**: `await` / `asyncio.gather` supported; tools are awaitable so
   one reply can fan out reads/subagents/builds concurrently.

### Wire shape (before → after)

Before (per form):
```
# t2/i1
r["t2/i1/f1"] = {... big cat ...}
r["t2/i1/f2"] = {... rg hits ...}
```
After (per iteration, stdout-style):
```
[assistant] ```python
c = cat("http.py")                # scratch, NOT surfaced
print("timeout at", c["anchors"]) # surfaced
```
[results t2/i1]
timeout at 52:0e3
```
`summarize(["t2/i1"], "gist")` collapses the whole `[results t2/i1]` block.

## 2. Decisions to lock first (Phase 0)

- **D1 — answer strictness.** Strict (require explicit ```answer fence) vs
  lenient (a reply with no ```python and non-empty prose = the answer, à la
  maki). Recommendation: **lenient-with-preference** — explicit ```answer wins;
  no-python + prose ⇒ answer; empty reply ⇒ reject.
- **D2 — both fences present.** If a reply has BOTH ```python and ```answer:
  run python, **ignore/relocate** the answer, bounce the model (the answer's
  evidence wasn't observed). This is the syntactic replacement for the done-gate.
- **D3 — resume durability.** Without `r[...]` pickle, in-process globals die on
  resume. Recommendation: **maki-style** — resume replays the conversation
  (printed context + answers), not live values; drop the pickle/rebind machinery.
  (Alt: pickle the whole sandbox globals on suspend — more work, keep only if a
  real workflow needs live values across process restart.)
- **D4 — `ctx` new name.** Options: `session` / `s` / `sess` / `world`.
  Recommendation: **`session`** (descriptive), optional short alias `s`.
- **D5 — `print` semantics.** Capture as TEXT (maki) vs keep structured. Recommend
  **text** (it's stdout); the model formats structure itself before printing
  (this is the whole point — minimize). Large prints are clipped on the wire
  (keep the existing clip view, now per-iteration).
- **D6 — `set_session_title`.** The other engine "mutation head". Keep as a
  fire-and-forget tool or drop (host already auto-titles). Recommend **drop**.

## 3. Affected code

### Remove
- `done` verb + termination plumbing:
  - `loop.clj` `done-gate-error` (L1144), `final-answer-gate-error` (L1197) and
    its call sites in `run-iteration`.
  - `ctx_engine.clj` `core-mutation-heads` (L270) — `done`/`set_session_title`
    classification; the `:mutation`/`:observation` tag machinery used only by the
    gate (`classify-form-tag` L287, the `:tag` in `block->envelope` L383) can be
    retired or reduced (still used for channel badges — verify before deleting).
  - `loop.clj` `block-ops` (L1102) — only fed the done-gate; remove.
- `r[...]` result memory:
  - `env_python.clj` r-prefetch / rebind / pickle (`r-keys-in-block` ~L443–474,
    rebind hop ~L317–364), `SYSTEM_VAR_NAMES` r handling.
  - `ctx_renderer.clj` `render-form-value` (L192) and per-form value wire lines.
  - `loop.clj` "Frozen result messages" trailer (L1809+) is re-based on
    iterations (see Change) — the per-form `:result-pickle`/`:bound-name` carry
    (`ctx_engine.clj` `block->envelope` L334) is removed.
- `prompt.clj` — the "There is no stdout / don't use print()" line and the "write
  bare value-returning forms, not data = cat(...)" guidance (inverted).

### Change
- **Fence reader** (`loop.clj` reply parse + `svar` ask-code path): classify the
  reply as `:python` | `:answer` (D1/D2). `:python` → eval forms (continue);
  `:answer` → finalize (render markdown → IR, end turn).
- **`print` capture** (`env_python.clj`): bind a sandbox `print`/stdout sink that
  appends to a per-iteration buffer; the buffer is the iteration's wire block.
  (Today `print` is banned — unban + capture.)
- **Per-iteration context** (`ctx_engine.clj` `blocks->forms`/`block->envelope`,
  `loop.clj` trailer): collapse the form granularity to one block per iteration
  carrying `{:src :stdout}` instead of N `{:scope :result}` forms.
- **`summarize`/`drop` per iteration** (`loop.clj` `apply-summaries` L1848):
  retarget from `tN/iN/fN` scopes to `tN/iN` iterations.
- **ctx rename** (`ctx_renderer.clj`, `env_python.clj`, prompt): identifier only.

### Add
- **Async runtime** (`env_python.clj`): run each ```python block inside a GraalPy
  asyncio event loop; expose `gather`; wrap tool symbols as awaitables (sync impl
  on a thread pool). Concurrency care: per-iteration stdout ordering, channel
  render-sink ordering, op/permission accounting under parallelism.
- **Lean prompt** (`prompt.clj`): port maki's lean style (get the actual text via
  `maki` binary `strings`/docs as a reference; keep vis-specific: fences, print,
  summarize, the renamed session dict, tool discovery via `apropos`/`doc`).

## 4. Task breakdown (phased)

**Phase 0 — Decisions** (D1–D6 above). Blocks everything. ~0.5d.

**Phase 1 — Fence reader + remove `done()`** (independent, high value, low risk).
- 1a. Parser: classify reply `:python | :answer`; first-reply = no answer.
- 1b. Loop: `:answer` finalizes (reuse the markdown→IR path done() used);
  remove `done` symbol, `done-gate-error`, `final-answer-gate-error`, `block-ops`.
- 1c. D2 rule: reply with both fences → run python, bounce.
- 1d. Tests: answer-only finalizes; python+answer bounces; python-only continues;
  first-reply answer rejected; lenient prose (D1).

**Phase 2 — `print` → per-iteration context; remove `r[...]`** (the big one).
- 2a. Unban + capture `print` into a per-iteration stdout buffer.
- 2b. Re-base the trailer/wire on iteration blocks (`{:src :stdout}`), drop
  per-form `r[...]` lines + pickle/rebind.
- 2c. Locals persist across iterations in-process (verify GraalPy globals carry).
- 2d. Decouple: bare tool calls still emit channel cards (human) but DON'T
  surface to model context unless printed.
- 2e. Tests: print appends per-iteration; multiple prints → one block; bare call
  not surfaced; locals persist across replies; clip view per-iteration.

**Phase 3 — `summarize`/`drop` per iteration** (depends on Phase 2).
- Retarget verbs to `tN/iN`; wire collapse `# -- tN/iN -- summarized: …`; tests.

**Phase 4 — Lean maki-style prompt** (depends on 1–3; documents the new model).
- Port lean prompt; encode fences/print/summarize/renamed-session/async; cut the
  redundancy; capability note for weak models. Eval-validate (GLM-5-Turbo).

**Phase 5 — `ctx` rename** (small; can land anytime after Phase 0/D4).

**Phase 6 — Async + `gather`** (separable, biggest engine change).
- 6a. asyncio loop per block; `await` support.
- 6b. tool symbols → awaitable wrappers (threadpool); `gather` helper.
- 6c. Ordering/accounting under concurrency; tests (parallel reads/subagents).

Suggested order: 0 → 1 → 2 → 3 → 4 → (5 anytime) → 6.

## 5. Risks & mitigations
- **Weak-model regression** (the verbose prompt took GLM-5-Turbo 40%→65%):
  validate each of Phase 1/2/4 with a focused eval before the next. Keep a
  capability note in the lean prompt.
- **Resume semantics change** (D3): document that scratch is in-process; resume
  replays context. Confirm no workflow depends on cross-process live values.
- **Async concurrency hazards**: interleaved stdout / sink ordering / double tool
  accounting. Land Phase 6 last, behind tests; keep sync path as default until
  green.
- **Big-bang risk**: Phases 1 and 2 are independently shippable + testable; do
  not merge them. Each behind the existing test suite + a 26-task eval.

## 6. Definition of done
- No `done`/`r[...]` anywhere; replies finalize via `answer` fence.
- `print` is the only context surface; context is per-iteration; `summarize`
  works per-iteration.
- Lean prompt; `ctx` renamed.
- `await`/`gather` work; a reply can run N tools concurrently.
- 26-task eval: pass rate ≥ baseline (65%), tokens/task down, no regressions.
