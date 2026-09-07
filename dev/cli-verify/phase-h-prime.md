# Historical Phase H' CLI verification

> **Obsolete since 2026-06-09.** The `consult-*` subsystem was removed. These
> commands no longer run and are retained as a record of the original test.
> Current Vis uses Python actions rather than the Clojure forms below.

This test used real model calls and required credentials for at least two
providers. Credentials are not included in this directory.

## Setup

    export ANTHROPIC_API_KEY=...        # for anthropic-coding-plan
    export OPENAI_API_KEY=...           # for openai-codex (optional 2nd provider)
    export EXA_API_KEY=...              # OPTIONAL — higher MCP rate limits

## Probe — anthropic

    ./bin/vis-agent --provider anthropic-coding-plan --model claude-sonnet-4-6 \
      'Use consult-request! :reflexion :deep with focus ["verify 91%
       HumanEval claim"] and a question about the Reflexion paper. In
       the next turn await-consult! :reflexion, check :confidence, and
       consult-promote! the result to fact :reflexion-paper. Then done
       with a summary.'

## Probe — openai-codex (second provider for cross-validation)

    ./bin/vis-agent --provider openai-codex --model gpt-5.5 \
      'Same prompt as above.'

## Expected

  Iter 1
    (consult-request! :reflexion :deep
      {:focus ["verify 91% HumanEval claim"]
       :question "..."})
    → :vis/silent
    (done {:answer "..."}) ATTEMPTED → REFUSED (R4 gate) with
    `;; ⚠ done-blocked-by-pending-consults` warning.

  Iter 2+
    :session/consult-results carries :reflexion entry
    (def r (await-consult! :reflexion))     → entry map pinned in trailer
    Check :confidence on r — expect :high or :medium for this prompt
    (consult-promote! :reflexion :reflexion-paper)  → removes the trailer entry

  Final :session/facts :reflexion-paper carries
    :content    "..."
    :citations  [{:type :paper :url "https://arxiv.org/abs/2303.11366" :title "Reflexion"}]
    :focus      ["verify 91% HumanEval claim"]
    :source     :consult

  No `consult-fast` / `consult-balanced` / `consult-deep` ever appears
  in any iteration source.

## Checks (manual inspection)

  GLOBAL G1   `clojure -M:test` exits 0
  GLOBAL G2   `rg "(consult-fast|consult-balanced|consult-deep)\\(" src extensions`
              returns only comments, not callable functions
  GLOBAL G3   thread isolation — check the consult log line confirms
              the side-thread runner; primary's ctx-atom never mutates
              while the future is pending
  GLOBAL G4   done refusal — observable in the warning trail before
              the model awaits
  GLOBAL G5   same-iter await refused — log shows :consult-not-resolved-yet
  GLOBAL G6   trailer scrub — `(introspect-iter "tN/iM")` for the await
              iter shows the await form gone after the next-iter promote
  GLOBAL G7   token cap retry-to-fit — log inspection only; the
              entry's :retries == 0 for an uncompressed result, == 1 if compression
              was needed
  GLOBAL G8   prompt cache check — diff `llm_cached_tokens` on iter 2
              vs baseline; expect within 30%
  GLOBAL G9   token budget — render the same 6-turn session pre/post
              Phase H'; check size is neutral or smaller
  GLOBAL G10  4Clojure regression — `./dev/benches/4clojure/run_subset.sh`
              should not regress > 2pp pass rate

## Notes

  - Inspect the renderer's `;; consult-results (N entries)` header to see
    whether results are included in the next model request.
  - `(introspect-changes "tN")` shows the consult-driven facts
    materialised between turns once promoted.
