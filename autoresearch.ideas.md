# Autoresearch ideas backlog

Promising directions that aren't the next experiment.

## Tighten the prompt

- Drop the long "## Restrictions" / "## Restricted symbols" / "## Extra
  requires" sections from the prompt template; restate them tighter
  inline. Goal: cut `mean_prompt_chars` from ~1600 → ~900.
- Tell the model explicitly: "iteration 1 = patch; iteration 2 = answer".
  The Vis gate already enforces this; making it explicit may collapse
  3-iter trivial answers into 2-iter answers.

## Vis-side gate

- `final-answer-structural-criteria-errors` in `loop.clj` forces a probe
  iteration before any `turn-answer!`. For programmatic answer-only
  workloads (4Clojure), the probe is wasted overhead. Add an allow-list
  shape: `(do <patch-tool-call> (turn-answer! …))` should satisfy the
  gate in one iteration because the patch *is* the evidence.
- Investigate whether `(v/patch [...])` in the same form as
  `(turn-answer! …)` could be promoted to "evidence + answer in one
  iteration" without breaking the broader `answer-with-extension-preflight-mismatch`
  contract.

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
