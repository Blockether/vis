# Iteration Flow

What happens when the user sends a message, end to end.

## Sequence

```mermaid
flowchart TD
    User(["User Message"]) --> Send

    subgraph Conversation["conversation/core.clj"]
        Send["send!<br/>acquire lock, build history"]
    end

    Send --> QueryFn

    subgraph Query["query/core.clj"]
        QueryFn["query!<br/>validate, store query, enter loop"]
    end

    QueryFn --> BuildCtx

    subgraph Loop["iteration-loop"]
        BuildCtx["1. Build Context<br/>iter header, prior_thinking<br/>journal, var_index, nudges"]
        AskLLM["2. Ask LLM<br/>svar ask - structured JSON<br/>code blocks + optional final"]
        Execute["3. Execute Code<br/>lint · SCI eval with timeout<br/>capture stdout/stderr/result"]
        Persist["4. Persist + Decide<br/>store-iteration!, extension metadata"]

        BuildCtx --> AskLLM --> Execute --> Persist
        Persist -->|":code, no :final"| BuildCtx
    end

    Persist -->|":final"| Answer(["Answer + metadata<br/>trace, tokens, cost"])
    Persist -->|"errors"| FeedBack["Feed error back to LLM"] --> BuildCtx
```

## Error Recovery

```mermaid
flowchart LR
    Error["Iteration throws"] --> Classify{Infrastructure?}
    Classify -->|Yes| Abort["Re-throw<br/>turn aborts"]
    Classify -->|No| Normalize["Normalize error<br/>compact description"]
    Normalize --> FeedBack["Append as user message"]
    FeedBack --> NextIter["Next iteration<br/>LLM adjusts strategy"]
    NextIter --> Budget{"consecutive<br/>errors >= 5?"}
    Budget -->|No| Continue["Continue loop"]
    Budget -->|Yes| Restart{"restarts < 3?"}
    Restart -->|Yes| Reset["Strategy restart<br/>anti-knowledge prompt"]
    Restart -->|No| GiveUp["Error budget exhausted<br/>fallback answer"]
```

## Budget Extension

The LLM can call `(request-more-iterations n)` from `:code` to extend
its iteration budget at runtime. Each request is capped at
`MAX_EXTENSION_PER_REQUEST` (50), and the total can never exceed
`MAX_ITERATION_CAP` (500).

## Prior Thinking

Only the **most recent** iteration's `:thinking` is shipped in
`<prior_thinking>`. Older reasonings are accessible on demand via
`(var-history '*reasoning*)` from `:code`. This is deliberate —
eager auto-context burns tokens on summaries nobody asked for.

Cross-query handover at iteration 0 ships the last 2 reasonings +
final answer from the previous turn. This is a separate mechanism.
