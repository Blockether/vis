# RLM memory

Vis treats each iteration as executable memory. The model emits Clojure forms, Vis evaluates them, and the next iteration observes the persisted journal and var index.

## Journal evidence

Journal entries are backed by `iteration.blocks`. New writes carry canonical provenance and rendering metadata so hidden, silent, system, tool, answer, error, and diagnostic projections remain citeable.

Canonical refs look like:

```text
turn/3f2a91c0/iteration/4/block/2
turn/3f2a91c0/iteration/4/block/2/tool/bash
turn/3f2a91c0/iteration/6/block/1/error
```

`i4.2` may appear only as a display label. It is not accepted by proof/fulfillment writers.

## Evidence roles

Vis separates evidence production from resolution. Full rules live in [Evidence, Diagnostics, and Resolution](evidence.md).

| Role | Examples | Contract |
|---|---|---|
| Evidence producer | eval result, tool result, `provider-limits` report, runtime snapshot | Emits observed facts as journal blocks with canonical refs. |
| Diagnostic enricher | parse diagnosis, doctor warning, error classifier | Adds explanation or classification to failure evidence. |
| Resolution state | intent, plan, gate, proof slot | Stores what must be proven and which refs proved or impeded it. |

A proof slot is a required evidence address in the resolution graph. It is not a separate proof object and it is not true until filled with an observed ref.

## Intent memory

Conversation-scoped intents are the durable memory of what Vis is trying to satisfy. `(v/intents)` reports focused intents, old unfocused active intents, plans, gates, checks, violations, a Markdown summary, and the aggregate `:success?` boolean.

The model should prove gates and fulfill/abandon focused intents using observed canonical refs before answering.

Deferred work is lifecycle-based. A future/deferred child ref with `:status :running` proves only that work was started. It cannot prove a gate. The model must cite a completed `:done` event, or cite terminal failure/timeout/cancellation as blocker evidence.

## Restore behavior

Runtime-only values may be stored internally as `{:vis/ref :expr}`. Chat, transcript, report, and provenance renderers must treat that as an internal pointer and show a reconstructed value or diagnostic fallback, never the raw sentinel.
