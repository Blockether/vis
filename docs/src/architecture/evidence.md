# Evidence, Diagnostics, and Resolution

Vis keeps three ideas separate:

1. **Evidence** — observed runtime facts.
2. **Diagnostics** — explanations or classifications attached to evidence.
3. **Resolution** — intent, plan, and gate state that decides whether the user request is done or impeded.

This is a taxonomy for how existing parts cooperate. It is not a new architecture layer and it is not a separate proof system.

## Short version

| Role | Examples | Stored where | Can prove a gate? |
|---|---|---|---|
| Evidence producer | eval block, tool result, `provider-limits` report, runtime snapshot | `iteration.blocks` with `:provenance` | Yes, when the cited ref is observed and terminal-success. |
| Diagnostic enricher | `parse-diagnose`, doctor check, error classifier | diagnostic/error blocks or structured fields on evidence | Only if its own observed diagnostic ref is cited and the gate expects that diagnostic fact. |
| Resolution state | intent, plan, gate, proof slot | `conversation_intent*` tables | No by itself. It consumes evidence refs. |

## Terms

### Evidence

Evidence is a fact emitted by the runtime and persisted into the journal. Every evidence item has a canonical provenance ref.

Examples:

```text
turn/3f2a91c0/iteration/4/block/2
turn/3f2a91c0/iteration/4/block/2/tool/bash
turn/3f2a91c0/iteration/6/block/1/error
```

Evidence may be success, failure, timeout, cancellation, stdout, stderr, parsed data, or a runtime snapshot. Errors are evidence too.

### Diagnostic

A diagnostic explains evidence. It can say why parsing failed, which provider limit applies, why a doctor check warns, or which error class matched.

Diagnostics do not magically resolve work. They help the model or runtime decide what to do next. If a diagnostic itself matters for proof or impediment, cite the diagnostic block's canonical ref.

### Resolution

Resolution is the durable work graph:

```text
Intent -> Plan -> Gate -> Provenance Ref+
                 -> Proof Slot+
```

The resolution graph asks:

- What is the user objective?
- What plan is active?
- What gates block final answer?
- Which observed refs prove or impede those gates?
- Can the intent be fulfilled or abandoned?

There is no separate proof object. Proof is gate state plus cited refs.

## Proof slots

A proof slot is a named expectation. It is shaped like:

```clojure
[intent-id :slot-name]
```

Use `v/proof-slot` to build it:

```clojure
(def verification-slot (v/proof-slot intent :verification))
```

A proof slot is not evidence. It becomes useful only when filled with observed evidence:

```clojure
(v/prove-gate! gate
  {:summary "Targeted verification passed."
   :refs ["turn/3f2a91c0/iteration/5/block/2"]
   :slots {verification-slot
           {:ref "turn/3f2a91c0/iteration/5/block/2"}}})
```

## Lifecycle rule

Refs have lifecycle status.

- Successful proof needs an observed terminal success ref, usually `:status :done`.
- Impediment evidence needs an observed terminal failure, timeout, cancellation, or interruption ref.
- A running future/deferred ref proves only that work started.

Wrong:

```clojure
;; Running child ref only proves the future started.
(v/prove-gate! gate {:summary "Done" :refs ["turn/3f2a91c0/iteration/4/block/2/tool/future"]})
```

Right:

```clojure
;; Await first; cite the observed await result when terminal.
(def awaited (v/await-proof! f {:timeout-ms 30000}))
awaited
```

Then, in the next observed iteration, cite the await block ref.

## Naming rules

Use these words consistently:

| Use | Avoid | Why |
|---|---|---|
| Evidence producer | proof producer | Producers emit facts; gates decide proof. |
| Diagnostic enricher | diagnostic proof layer | Diagnostics explain; they do not own completion. |
| Resolution state | proof layer | Intents/plans/gates consume refs; proof is not separate state. |
| Impeded gate | blocked gate in new docs | `v/impede-gate!` is the preferred API. `v/block-gate!` is legacy alias. |
| Proposition | question for gates | Runtime gate API uses `:proposition`. |

## End-to-end flow

1. Model creates an intent and active plan.
2. Model creates required gates with expected proof slots.
3. Runtime probes produce journal evidence.
4. Diagnostic enrichers may attach explanations to failures.
5. Model observes the journal and cites canonical refs.
6. Gates become proven or impeded.
7. Intent becomes fulfilled or abandoned.
8. Final answer is accepted only after focused intents are resolved.

## API example

```clojure
(def intent
  (v/issue-intent! {:title TURN_USER_REQUEST
                    :rationale "User asked for this objective."}))

(def verification-slot (v/proof-slot intent :verification))

(def plan
  (v/issue-plan! {:intent-id (:id intent)
                  :summary "Inspect, act on evidence, verify."
                  :plan (v/plan intent {:requires [verification-slot]
                                        :steps [{:id :inspect}
                                                {:id :verify}]})}))

(def gate
  (v/issue-gate! {:plan-id (:id plan)
                  :proposition "Verification passes."
                  :expected-proof {:slots {verification-slot {:required? true}}
                                   :guard [:exists [:slot verification-slot :ref]]}}))
```

After running a check and observing its journal ref:

```clojure
(v/prove-gate! gate
  {:summary "Verification passed."
   :refs ["turn/3f2a91c0/iteration/5/block/2"]
   :slots {verification-slot
           {:ref "turn/3f2a91c0/iteration/5/block/2"}}})

(v/fulfill-intent! (:id intent)
  {:summary "User objective satisfied."
   :refs ["turn/3f2a91c0/iteration/5/block/2"]})
```

If verification cannot complete:

```clojure
(v/impede-gate! gate
  {:reason "Verification timed out."
   :refs ["turn/3f2a91c0/iteration/5/block/2/error"]})
```

Then re-plan, retry, or abandon the intent with impediment refs.

## Related docs

- [Iteration Flow](iteration-flow.md) — how journal blocks and lifecycle refs are emitted.
- [RLM Memory](rlm-memory.md) — how journal evidence and intent memory work together.
- [Conversation Intents, Plans, and Gates](completion-contract.md) — final-answer guard and intent API.
- [State Ownership](state.md) — which tables own durable, branch-local, and run-local state.
