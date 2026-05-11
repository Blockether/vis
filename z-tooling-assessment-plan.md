# z-tooling-assessment-plan

## Goal
Complete the z/ tooling assessment task reliably in one persisted `vis run` without getting stuck in a long first provider call or leaving a stale `:running` turn behind.

## What went wrong
- The benchmark target was framed as an iteration-loop problem, but the observed failure happened before iteration 1 was persisted.
- The first `zai-coding/glm-5.1` request was dispatched, then the outer harness timeout killed the run before the provider returned a response or error.
- Because the process died mid-turn, the persisted CLI conversation was captured as `running` with `0` iterations, which made the transcript look like a loop/guard failure when it was really an in-flight provider timeout.
- The autoresearch harness therefore optimized the wrong layer.

## Constraints to keep
- Keep the real task semantics: inspect z/ implementation and tests, summarize strengths/gaps, give a score out of 10.
- Keep `zai-coding` + `glm-5.1` only if the provider timeout is made shorter than the outer benchmark timeout.
- Preserve forensic artifacts when a run dies, but do not classify a killed in-flight request as a true `0-iteration` completion.

## Plan
1. **Fix timeout ordering**
   - Set the inner provider/run timeout lower than the outer harness timeout.
   - Leave enough wall-clock headroom for transcript export and metric emission.

2. **Fix failure classification**
   - Detect and label: `provider-request-started-but-no-iteration-persisted`.
   - Do not treat that state as ordinary `iter_count=0`.

3. **Fix persisted turn cleanup for CLI runs**
   - Sweep or finalize orphaned `:running` CLI turns after timeout/interruption.
   - Make the persisted state reflect `:interrupted` or equivalent, not stale `:running`.

4. **Add stronger instrumentation**
   - Capture timestamps for:
     - request dispatched
     - first provider bytes/response
     - first iteration persisted
   - Emit explicit benchmark metrics for these boundaries.

5. **Only then tune prompt/loop behavior**
   - Re-run the real z/ assessment task.
   - If provider latency is no longer dominant, then investigate guard churn, context pressure, prompt pruning, or answer-preflight loops.

## Success criteria
- Persisted CLI task finishes with a real answer, not a stale `running` turn.
- Timeouts produce a correctly classified interruption state.
- Benchmark output distinguishes provider stall vs. loop churn.
- Only after that, iteration count becomes a meaningful optimization target.

## Recommended next patch order
1. timeout alignment
2. orphaned CLI turn finalization
3. better run classification metrics
4. rebaseline the workload
