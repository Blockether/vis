# vis-foundation extension

The `v/` foundation extension exposes model-facing helpers for inspection, reporting, Markdown composition, filesystem/tool operations, provenance, and conversation intents.

## Intent API

```clojure
(v/issue-intent! {:title "..." :rationale "..."})
(v/focus-intent! intent-id {:rationale "..."})
(v/relate-intents! {:from-intent-id ... :to-intent-id ... :relation :subintent})
(v/issue-plan! {:intent-id ... :summary "..." :steps [...]})
(v/issue-gate! {:plan-id ... :question "..." :required? true})
(v/prove-gate! gate-id {:summary "..." :refs ["turn/3f2a91c0/iteration/5/block/2"]})
(v/block-gate! gate-id {:reason "..." :refs ["turn/3f2a91c0/iteration/5/block/2/error"]})
(v/fulfill-intent! intent-id {:summary "..." :refs ["turn/3f2a91c0/iteration/5/block/2"]})
(v/abandon-intent! intent-id {:reason "..." :refs ["turn/3f2a91c0/iteration/5/block/2"]})
(v/intents)
```

`v/intents` is the single read/check/report surface for conversation intent state.

## Provenance API

`v/provenance-timeline`, `v/provenance-stats`, `v/provenance-guards`, and `v/provenance-report` expose observed evidence. Writer APIs accept canonical refs only; compact labels such as `i4.2`, `i4.2/tool`, `E1`, and `G1` are rejected.

## Rendering

Execution blocks include `:rendering-kind` values such as `:vis/sci`, `:vis/silent`, `:vis/system`, `:vis/tool`, `:vis/answer`, `:vis/error`, and `:vis/diagnostic`. `:vis/system` blocks render as collapsed `SYSTEM` entries with generated details. Raw system errors are preserved for audit/debug views but hidden from normal chat.
