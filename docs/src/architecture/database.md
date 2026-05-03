# Database schema

Vis stores conversation identity separately from branch state and turn runs:

```text
conversation_soul
  -> conversation_state
    -> conversation_turn_soul
      -> conversation_turn_state
        -> iteration
```

Conversation-scoped intent state hangs from `conversation_soul`; per-run focus hangs from `conversation_turn_state`.

```text
conversation_soul
  -> conversation_intent
    -> conversation_intent_ref
    -> conversation_intent_relation
    -> conversation_intent_plan
      -> conversation_intent_gate
        -> conversation_intent_gate_ref
conversation_turn_state
  -> conversation_intent_focus
```

## Intent tables

`conversation_intent` records the top-level objective Vis is pursuing in one conversation. Status is `active`, `fulfilled`, or `abandoned`. Fulfilled intents require a fulfillment summary, resolved timestamp, and fulfillment evidence refs. Abandoned intents require an abandonment reason, resolved timestamp, and abandonment evidence refs.

`conversation_intent_ref` stores canonical evidence/context refs for fulfillment, abandonment, or context.

`conversation_intent_relation` records intent-to-intent structure (`subintent`, `related`, `supports`, `blocks`). Triggers reject relations across different `conversation_soul` rows.

## Plan and gate tables

`conversation_intent_plan` records one strategy for an intent. A partial unique index allows at most one active plan per intent. Issuing a new plan supersedes the previous active plan transactionally.

`conversation_intent_gate` records blocking checks for one plan. Status is `open`, `proven`, or `blocked`. Required open gates block. Required blocked gates require re-plan or intent abandonment before final answer.

`conversation_intent_gate_ref` stores canonical refs for gate evidence. Triggers prevent updating or deleting refs after a gate is terminal (`proven` or `blocked`).

## Focus sidecar

`conversation_intent_focus` connects a turn-state run to the intents it is currently pursuing. A trigger ensures the focused intent belongs to the same `conversation_soul` as the turn-state.

## Iteration blocks

`iteration.blocks` is a Nippy blob of block maps. New persisted blocks include canonical provenance and rendering metadata:

```clojure
{:idx 0
 :code "(+ 1 2)"
 :result 3
 :provenance {:ref "turn/3f2a91c0/iteration/1/block/1"
              :op :sci/eval
              :status :done}
 :rendering-kind :vis/sci}
```

Provenance maps do not store Markdown or presentation keys. Channels derive display from `:rendering-kind`, structured result/error/stdout/stderr data, and shared provenance formatters.
