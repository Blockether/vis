# Iteration Flow

What happens when the user sends a message, end to end.

## Sequence

```
user message
  → conversation/core.clj :: send!         acquire per-conv lock; build history
  → query/core.clj       :: query!         validate; store query; enter loop
      loop:
        1. Build Context
        2. Ask LLM
        3. Execute Code
        4. Persist + Decide
             — has :code, no :final  → back to step 1
             — :final present        → return answer + metadata
             — error                 → feed error as user msg, back to step 1
```

**Step details:**

1. **Build Context** — iter header, `<prior_thinking>`, `<journal>`, `<var_index>` (compact pseudo-source index of defs/defns), nudges (built-in + extension)
2. **Ask LLM** — svar structured JSON output: code blocks + optional
   `:final`. The call site is `(llm/ask! (:router environment) …)`,
   i.e. the env's snapshot router — NOT the global
   `query-core/router-atom`. See
   [Router Lifecycle](state.md#router-lifecycle) for why this matters
   when provider/model is switched mid-session.
3. **Execute Code** — lint, SCI eval with timeout, capture stdout/stderr/result per block
4. **Persist + Decide** — `store-iteration!`, attach extension metadata, route to next step

## System Prompt Assembly

`loop-core/assemble-system-prompt` is the **single source of truth** for
the system message content. Both iteration loop paths and the TUI
`[?]` inspector call it. It composes:

1. **Core instructions** (`CORE_SYSTEM_PROMPT`) — iteration steps
   (READ/COMPUTE/PERSIST/FINALIZE), Mustache docs, grounding rule,
   query primacy, perf hints, tool discipline, CLJ rules, output voice
2. **Date + environment block** — CWD, home, user, platform, shell
3. **Extension prompts** — each active extension’s canonical
   symbol-derived prompt block, prefixed with `[namespace: alias → ns]`,
   plus any optional extra `:ext/prompt` tail

The iteration spec schema (svar’s `spec->prompt`) is appended separately
by svar as a final user message — it is NOT part of the system message.

## Error Recovery

When an iteration throws:

1. If the error is **infrastructure** (router down, DB closed, JVM): re-throw — the turn aborts.
2. Otherwise: normalize the error and append it as the next user message; continue the loop.
3. If consecutive errors reach **5**, attempt a **strategy restart** (fresh prompt assembly, reset call counts). Up to **3** restarts.
4. After 3 failed restarts, the budget is considered exhausted and the turn ends with `:status :error`.

## Budget Extension

The default budget is **4 iterations** — deliberately tight so the LLM
must plan. When more work is genuinely needed, the LLM calls
`(request-more-iterations n)` from `:code` to extend on demand.
There is **no cap** on how high the budget can grow.

This is especially important when a budget `[system_nudge]` fires.
The intended behavior is: read the nudge, decide whether more work is
actually needed, and if yes call `(request-more-iterations n)`
immediately instead of limping into a bad finalize.

## Prior Thinking

Only the **most recent** iteration's `:thinking` is shipped in
`<prior_thinking>`. Older reasonings are accessible on demand via
`(var-history '*reasoning*)` from `:code`. This is deliberate —
eager auto-context burns tokens on summaries nobody asked for.

More generally, `<var_index>` is only the latest namespace snapshot.
When a symbol shows `:v N`, the full persisted version timeline is
available via `(var-history 'sym)`. This includes SYSTEM vars like
`*query*`, `*reasoning*`, and `*answer*`.

Cross-query handover at iteration 0 ships the last 2 reasonings +
final answer from the previous turn. This is a separate mechanism.
