# vis-foundation extension

The `v/` foundation extension exposes model-facing helpers for inspection, reporting, Markdown composition, filesystem/tool operations, provenance, and conversation intents.

## Intent API

`v/` exposes the resolution API used by the model. Evidence comes from journal refs; resolution state consumes those refs.

```clojure
(def intent (v/issue-intent! {:title "..." :rationale "..."}))
(v/focus-intent! (:id intent) {:rationale "..."})
(v/relate-intents! {:from-intent-id ... :to-intent-id ... :relation :subintent})

(def verification-slot (v/proof-slot intent :verification))
(def plan
  (v/issue-plan! {:intent-id (:id intent)
                  :summary "Inspect, act on evidence, verify."
                  :plan (v/plan intent {:requires [verification-slot]})}))
(def gate
  (v/issue-gate! {:plan-id (:id plan)
                  :proposition "Verification passes."
                  :expected-proof {:slots {verification-slot {:required? true}}}}))

(v/offer-proof! {:gate-id (:id gate)
                 :slots {verification-slot {:ref "turn/3f2a91c0/iteration/5/block/2"}}})
(v/prove-gate! gate {:summary "Verification passed."
                     :refs ["turn/3f2a91c0/iteration/5/block/2"]
                     :slots {verification-slot {:ref "turn/3f2a91c0/iteration/5/block/2"}}})
(v/impede-gate! gate {:reason "Verification timed out."
                      :refs ["turn/3f2a91c0/iteration/5/block/2/error"]})
(v/fulfill-intent! (:id intent) {:summary "Done."
                                 :refs ["turn/3f2a91c0/iteration/5/block/2"]})
(v/abandon-intent! (:id intent) {:reason "Cannot continue."
                                 :refs ["turn/3f2a91c0/iteration/5/block/2/error"]})
(v/intents)
```

`v/intents` is the single read/check/report surface for conversation intent state. `v/block-gate!` exists only as a legacy alias for `v/impede-gate!`.

## Provenance API

`v/provenance-timeline`, `v/provenance-stats`, `v/provenance-guards`, `v/latest-provenance-refs`, and `v/provenance-report` expose observed evidence. Writer APIs accept canonical refs only; compact labels such as `i4.2`, `i4.2/tool`, `E1`, and `G1` are rejected.

Use provenance APIs before proof or fulfillment:

```clojure
{:refs (v/latest-provenance-refs)
 :guards (v/provenance-guards)
 :intents (v/intents)}
```

For deferred work, cite a terminal observed ref. A running future/deferred child ref proves only start, not completion.

## Rendering

Execution blocks include `:rendering-kind` values such as `:vis/sci`, `:vis/silent`, `:vis/system`, `:vis/tool`, `:vis/answer`, `:vis/error`, and `:vis/diagnostic`. `:vis/system` blocks render as collapsed `SYSTEM` entries with generated details. Raw system errors are preserved for audit/debug views but hidden from normal chat.
