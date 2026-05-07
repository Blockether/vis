# Database schema

Vis stores conversation identity separately from branch state and turn runs:

```text
conversation_soul
  -> conversation_state
    -> conversation_turn_soul
      -> conversation_turn_state
        -> iteration
```

Conversation-scoped intent state hangs from `conversation_soul`; per-run focus hangs from `conversation_turn_state`. Extension-owned sidecar state lives in `extension_aggregate` and points at any relevant scope row.

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

extension_aggregate
  -> conversation_soul? / conversation_state? / conversation_turn_state? / iteration?
```

## Extension aggregate table

`extension_aggregate` stores durable extension-owned sidecars: caches, background status, external ids, notification dedupe, checkpoint refs, and per-iteration/per-block metadata. It is not the installed-extension registry and not the proof ledger.

`extension_id` is runtime-filled from the active extension callback. Normal extension helpers (`ext-create!`, `ext-put!`, `ext-get`, `ext-list`, `ext-delete!`, `ext-swap!`) always scope reads and writes to that extension id and reject caller-supplied extension ids. Public admin facade functions (`db-list-extension-aggregates`, `db-get-extension-aggregate`) may inspect rows across extensions; public cross-extension writes are not exposed.

`scope_key` is the normalized singleton key used by `ext-put!` upserts, for example `global`, `conversation-soul:<id>`, `conversation-state:<id>`, `turn-state:<id>`, `iteration:<id>`, `block:<iteration-id>:<index>`, or `block-id:<id>`. `iteration_block_id` is intentionally soft until `iteration_block` becomes a first-class table; current block-scoped rows use `iteration_id` plus `iteration_block_index`.

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
