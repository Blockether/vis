# Autoresearch ideas backlog

Promising directions that aren't the next experiment.

## Session 2026-05-14 wrap-up

Baseline 39 → 6 iter_score (-85%); 6/6 pass on rotated workload; block_errors
9 → 0; cost $0.030 → $0.017 (-43%); z_patch_share 0% → ~67% (honest count).

What worked (cumulative):
  1. Preflight final-answer gate threads prior-iterations through — stopped
     the spurious 'no evidence for this turn' rejections that wasted ~6 iters
     per task pre-fix.
  2. Evidence-prior? requires a real non-error non-answer block (not just
     `(seq prior-iters)`); closes the loophole where preflight-rejected iters
     falsely counted as evidence.
  3. answer-with-extension preflight no longer rejects whole iteration; drops
     only the turn-answer form so the extension call still runs. Stops the
     model's 'keeps re-emitting same bundle because patch never lands' loop.
  4. CORE prompt layering refactor: no extension names in core, extensions
     own their surfaces. -27% chars on CORE_SYSTEM_PROMPT.
  5. z/ ext prompt rewritten as STRUCTURAL EDITING manifesto + SCI reader-
     macro gotcha + (z/source) escape hatch with concrete example.
  6. Foundation v/ editing prompt -10% with explicit 'trust patch result'
     anti-verify hint.
  7. 4Clojure user prompt uses z/patch + (z/source) shape with conj-list
     example (was a v/patch shape that triggered quote-strip).
  8. Runner cache fingerprint uses relative path (multiple core.clj files
     no longer collide on basename).
  9. Metric parser counts patch calls from form-result channel entries
     (catches calls inside `(do ...)` wrappers; tool-event chunk missed them).

## Tighten the prompt

- Drop the long "## Restrictions" / "## Restricted symbols" / "## Extra
  requires" sections from the prompt template; restate them tighter
  inline. Goal: cut `mean_prompt_chars` from ~1600 → ~900.
- Tell the model explicitly: "iteration 1 = patch; iteration 2 = answer".
  The Vis gate already enforces this; making it explicit may collapse
  3-iter trivial answers into 2-iter answers.

## Vis-side gate (PARTIALLY DONE 2026-05-14)

- DONE: `answer-with-extension-preflight-mismatch` no longer rejects the
  whole iteration. The extension calls run; only the turn-answer form is
  dropped with a clear message. This alone got iter_score 7 → 6 because
  the model stopped looping while its z/patch never landed.
- DONE: `final-answer-structural-criteria-errors` evidence-prior? now
  requires a prior iter with a real non-error non-answer block; preflight
  gate threads previous-iterations correctly.

Remaining (DEFERRED — benchmark-overfit risk):

- For programmatic answer-only workloads (4Clojure), the probe iter is
  still wasted overhead. An allow-list shape `(do <ext-call> (turn-answer!
  ...))` could collapse 2-iter solutions into 1. But the answer is
  composed BEFORE the model sees the tool result — only safe when the
  IR is self-contained (no derived values). Hard to detect in general;
  loosening universally is risky for real workloads.
- Possible refinement: detect when the `turn-answer!` IR contains no free
  symbols (only literals + collection literals), then allow the bundle.
  Walk the form, reject if any symbol references something from
  `:bindings` or the current iter's defs.

## Full RLM exploration ("pełny RLM")

Today Vis carries two parallel state buffers per turn:
- `<journal>` — append-only event log per iteration (tool calls,
  errors, set-conversation-title!, render outputs)
- `<bindings>` — current symbol table from `(def …)` calls

These get re-serialized into the prompt every iteration. The user
hypothesis: this split is "rozjebany" — broken. A "full RLM"
(Recursive/Reasoning Loop Machine) unifies them into one structured
reasoning state that is the actual program-under-construction. The
agent edits its own structured plan; iterations are plan moves; the
trace is the program.

Concrete first step that doesn't require boiling the ocean:

1. Add a `(v/state)` query that lets the model see the current
   `<journal>` + `<bindings>` as a single structured map.
2. Add a `(v/plan! [...])` form that lets the model rewrite a
   structured plan element in place — this becomes the "RLM tape".
3. Measure: does access to a coherent state representation reduce
   iteration count on 4Clojure / SWE-bench Lite?

This is a multi-week change. Defer until prompt-and-gate wins are
exhausted.

## Neurosymbolic angle

For Clojure problems specifically, a small symbolic preprocessor could:
- Parse the test form, identify the blank position, infer the expected
  arity of the answer (single form vs source fragment).
- Hand the model a typed slot: "fill the blank with a single Clojure
  form that returns a value of type X".
- That cuts ambiguity → fewer iterations on "is the answer one form
  or multiple?" confusion.

Risk: pre-solving the structure feels like cheating on the benchmark.
Acceptable only if the symbolic step is independent of the *content*
of any specific 4Clojure problem.

## Measurement-side

- `--full-trace-json-stream` is now the source of truth; the `--json`
  envelope path can be removed once we trust the stream parser.
- Add `mean_input_tokens` and `mean_output_tokens` to metrics so we
  can see which axis context inflation hits.
- Save the model's per-iteration thinking traces for offline analysis.
- FIX: `z_patch_calls` / `v_patch_calls` only count tool-events that fire
  for BARE top-level tool forms. When the model wraps a tool call in
  `(do ...)` (e.g. `(do (set-conversation-title!) (z/patch [...]))`),
  the tool runs but no tool-event chunk fires — so the metric shows 0.
  Walk the parsed code-entries themselves to count instead of relying
  on trace tool-events.
