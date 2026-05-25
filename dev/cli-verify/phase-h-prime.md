# Phase H' CLI Verify Recipe

End-to-end real-model smoke test. Requires API keys for at least
two providers; this directory ships none. The fence below is the
canonical probe per PLAN.md gate 11.

## Setup

    export ANTHROPIC_API_KEY=...        # for anthropic-coding-plan
    export OPENAI_API_KEY=...           # for openai-codex (optional 2nd provider)
    export EXA_API_KEY=...              # OPTIONAL — higher MCP rate limits

## Probe — anthropic

    ./bin/vis --provider anthropic-coding-plan --model claude-sonnet-4-6 \
      'Use consult-request! :reflexion :deep with focus ["verify 91%
       HumanEval claim"] and a question about the Reflexion paper. In
       the next turn await-consult! :reflexion, check :confidence, and
       consult-promote! the result to fact :reflexion-paper. Then done
       with a summary.'

## Probe — openai-codex (second provider for cross-validation)

    ./bin/vis --provider openai-codex --model gpt-5.5 \
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
    (consult-promote! :reflexion :reflexion-paper)  → scrubs trailer pin

  Final :session/facts :reflexion-paper carries
    :content    "..."
    :citations  [{:type :paper :url "https://arxiv.org/abs/2303.11366" :title "Reflexion"}]
    :focus      ["verify 91% HumanEval claim"]
    :source     :consult

  No `consult-fast` / `consult-balanced` / `consult-deep` ever appears
  in any iteration source.

## Gates (manual inspection)

  GLOBAL G1   `clojure -M:test` exits 0
  GLOBAL G2   `rg "(consult-fast|consult-balanced|consult-deep)\\(" src extensions`
              returns only comment mentions (no resolvable surface)
  GLOBAL G3   thread isolation — check the consult log line confirms
              the side-thread runner; primary's ctx-atom never mutates
              while the future is pending
  GLOBAL G4   done refusal — observable in the warning trail before
              the model awaits
  GLOBAL G5   same-iter await refused — log shows :consult-not-resolved-yet
  GLOBAL G6   trailer scrub — `(introspect-iter "tN/iM")` for the await
              iter shows the await form gone after the next-iter promote
  GLOBAL G7   token cap retry-to-fit — log inspection only; the
              entry's :retries == 0 on happy paths, == 1 if compression
              was needed
  GLOBAL G8   prompt cache check — diff `llm_cached_tokens` on iter 2
              vs baseline; expect within 30%
  GLOBAL G9   token budget — render the same 6-turn session pre/post
              Phase H'; check size is neutral or smaller
  GLOBAL G10  4Clojure regression — `./dev/benches/4clojure/run_subset.sh`
              should not regress > 2pp pass rate

## Notes

  - The renderer compact preview header (`;; consult-results (N
    entries)`) is the fastest way to spot whether the model is
    integrating results into its next-action plan.
  - `(introspect-changes "tN")` shows the consult-driven facts
    materialised between turns once promoted.
