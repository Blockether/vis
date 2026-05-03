# Conversation Intents, Plans, and Gates

Vis gates final answers through conversation-scoped intents. An intent records what Vis believes the user wants, a plan records the current strategy for satisfying that intent, and gates record concrete blocking checks for the active plan.

```text
ConversationIntent
  -> ConversationPlan
    -> BlockingGate
      -> Provenance Reference+
```

There is no separate proof object. Proof or blocker data lives directly on the gate together with canonical provenance references.

## Scope and focus

- Intents belong to `conversation_soul`.
- Focus belongs to `conversation_turn_state`.
- Old unfocused active intents are reported, but they do not block every later answer.
- The focused intent set decides whether the current answer is allowed.
- Unrelated work should use a fork or first fulfill/abandon the current focused intent.

`v/focus-intent!` is guarded: it may focus the same/related/subintent work, but it rejects an unrelated switch while the current focus has unresolved blocking work.

## Public API

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
