# Vis — ideas backlog

Bullet-list of promising but not-yet-actioned ideas. Append-only. Lines
with `[DONE]` prefix once landed.

## ctx visibility / iteration discipline

- Context-budget pressure signal: when rendered `;; ctx` exceeds N tokens
  (e.g. 60% of model window), emit `;; ⚠ ctx-pressure: ~X tokens (Y% of
  cap) — consider :trailer-summarize` as a top-of-ctx banner alongside
  the existing ctx-summary line. Re-use `internal/tokens.clj` estimator.

- Anti-pattern soft warning at render time: when an iter's trailer pin
  has ≥3 similar tool defs (same fn head, e.g. `exa/web-search` or
  `v/rg`) without a covering `spec-set!`, anchor a `;; ⚠ tool-rush:
  declare spec for repeated probes` warning on that pin. Engine already
  has the soft-warning mechanism (`derive-warnings`) — reuse it.

- Validator coverage warning: when a spec has reqs without
  `:validator-fn`, emit `;; ⚠ spec :K has N unverifiable req(s)` so
  the model is nudged to attach validators (without forcing it on
  trivial reqs).

- Hard enforce P1 fix-before-P3 work: today `:blocking? true` is only a
  banner. Stronger gate: engine refuses `(done …)` while P1 blocking
  actions exist, with a structured error pointing at the offending
  spec/task. Make it opt-in per session via a config flag first.

## prompt experiments

- A/B benchmark V1 vs V2 prompt on the 4clojure / swe-bench-lite
  harness in `dev/benches/`. Metrics: iterations per task, tool calls
  per turn, success rate, prompt cache hit ratio. Use autoresearch
  loop.

- Try a V3 with explicit phase headers in user message (not just
  CORE_SYSTEM_PROMPT) — e.g. `;; phase :formulation` injected on
  iter 1 of each turn so the model is reminded which Generate-Test-
  Critique slot it is in.

## refinement / backtracking

- LOGIC-LM++ style backtracking: when a `:validator-fn` flips a task
  from `:done` back to `:todo`, record the failed-proof scope as
  archived so the model sees `was-proof :tN/iM/fK rejected by :req-id
  reason …` next turn. Today reverts emit a warning but the original
  proof attempt is gone.

- Contrastive trailer signal (ENVISIONS-style): per-iter render of
  `:successes` + `:failures` summary so the model has explicit positive/
  negative examples to learn from in the same turn. Today success/error
  exists per form but not summarised at iter level.
