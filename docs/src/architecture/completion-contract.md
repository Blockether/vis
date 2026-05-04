# Conversation Intents, Plans, and Gates

Vis gates final answers through conversation-scoped intents. An intent records what Vis believes the user wants, a plan records the current strategy for satisfying that intent, and gates record concrete blocking checks for the active plan.

```text
ConversationIntent
  -> ConversationPlan
    -> BlockingGate
      -> Provenance Reference+
```

There is no separate proof object. Proof or blocker data lives directly on the gate together with canonical provenance references. A proof slot is only a named expectation on a gate or plan edge; it is not evidence until it is filled with an observed canonical ref.

## Evidence taxonomy

These names describe roles, not extra architecture layers. See [Evidence, Diagnostics, and Resolution](evidence.md) for the full glossary.

| Role | Examples | Meaning |
|---|---|---|
| Evidence producers | eval blocks, tool results, `provider-limits` snapshots, runtime snapshots | Runtime facts persisted as journal blocks with canonical provenance refs. |
| Diagnostic enrichers | `parse-diagnose`, error classifiers, doctor checks | Explanations attached to evidence. They help decide what happened but do not prove gates by themselves unless their own diagnostic block is cited. |
| Resolution state | intents, plans, gates, proof slots | Consumes observed provenance refs to decide whether work is done or impeded. It does not create facts from model claims. |

Use **evidence** for observed runtime facts, **diagnostics** for explanations about evidence, and **resolution** for intent/plan/gate state. Avoid calling this a separate "proof layer"; proof is gate state plus cited refs.

## Scope and focus

- Intents belong to `conversation_soul`.
- Focus belongs to `conversation_turn_state`.
- Old unfocused active intents are reported, but they do not block every later answer.
- The focused intent set decides whether the current answer is allowed.
- Unrelated work should use a fork or first fulfill/abandon the current focused intent.

`v/focus-intent!` is guarded: it may focus the same/related/subintent work, but it rejects an unrelated switch while the current focus has unresolved blocking work.

## Public API

The preferred gate failure verb is **impede**. `v/block-gate!` remains as a legacy alias only.

```clojure
(def intent (v/issue-intent! {:title "..." :rationale "..."}))
(v/focus-intent! (:id intent) {:rationale "..."})
(v/relate-intents! {:from-intent-id ... :to-intent-id ... :relation :subintent})

(def slot (v/proof-slot intent :verification))
(def plan-graph (v/plan intent {:requires [slot] :steps [{:id :verify}]}))
(def plan (v/issue-plan! {:intent-id (:id intent) :summary "..." :plan plan-graph}))
(def gate (v/issue-gate! {:plan-id (:id plan) :proposition "Verification passes." :expected-proof {:slots {slot {:required? true}}}}))
(v/offer-proof! {:gate-id (:id gate) :slots {slot {:ref "turn/3f2a91c0/iteration/5/block/2"}}})
(v/prove-gate! gate {:summary "..." :refs ["turn/3f2a91c0/iteration/5/block/2"] :slots {slot {:ref "turn/3f2a91c0/iteration/5/block/2"}}})
(v/impede-gate! gate {:reason "..." :refs ["turn/3f2a91c0/iteration/5/block/2/error"]})
(v/fulfill-intent! (:id intent) {:summary "..." :refs ["turn/3f2a91c0/iteration/5/block/2"]})
(v/abandon-intent! (:id intent) {:reason "..." :refs ["turn/3f2a91c0/iteration/5/block/2"]})
(v/intents)
```

`v/intents` is the single read/check/report surface. It returns focus, old unfocused active intents, aggregate intent/plan/gate state, checks, violations, and a Markdown report.

## Final-answer readiness

A final answer is rejected when any focused active intent has:

- no active plan;
- more than one active plan;
- an active plan with no gates;
- a required open gate;
- a required blocked gate without re-plan or abandonment;
- unresolved active status after gates are proven.

A final answer is accepted only after every focused intent is fulfilled or abandoned with observed provenance references.

## Canonical provenance references

Writer APIs accept canonical refs only:

```text
turn/<turn8>/iteration/<iteration-number>/block/<block-number>
turn/<turn8>/iteration/<iteration-number>/block/<block-number>/tool/<tool-id>
turn/<turn8>/iteration/<iteration-number>/block/<block-number>/error
conversation/<conversation8>/turn/<turn8>/iteration/<iteration-number>/block/<block-number>
```

Compact display labels such as `i4.2`, `i4.2/tool`, `E1`, or `G1` are not accepted writer references and are not stored.

## Lifecycle provenance

Provenance is lifecycle-based. A block ref proves only the block observation itself. Deferred/future work gets child lifecycle events so the system can distinguish `:running` from terminal `:done`, `:error`, `:timeout`, `:cancelled`, or `:interrupted`.

Example: a block that starts a future can persist a running child event:

```clojure
{:provenance {:ref "turn/3f2a91c0/iteration/4/block/2/tool/future"
              :parent-ref "turn/3f2a91c0/iteration/4/block/2"
              :op :future/deferred
              :status :running}
 :rendering-kind :vis/tool}
```

Gate proof cannot cite a running lifecycle event. Proof requires a completed successful event (`:status :done`). Blocker evidence may cite terminal failures/timeouts/cancellations, but not still-running work.

## Rendering metadata

Every persisted execution block carries:

```clojure
{:idx 0
 :code "..."
 :result ...
 :error nil
 :provenance {:ref "turn/3f2a91c0/iteration/4/block/2"
              :op :sci/eval
              :status :done}
 :rendering-kind :vis/sci}
```

Rendering kinds are namespaced keywords: `:vis/sci`, `:vis/silent`, `:vis/system`, `:vis/tool`, `:vis/answer`, `:vis/error`, and `:vis/diagnostic`.

Provenance is machine data. It does not store Markdown, labels, short labels, result data, stdout, stderr, errors, or rendering hints. Channels derive presentation from structured data and shared formatters.

`{:vis/ref :expr}` is an internal persistence sentinel. Resumed chat, transcript, report, and provenance views must render a reconstructed or diagnostic projection, not the raw sentinel.
