# Iteration flow

Each Vis turn runs as one or more model iterations. The model emits Clojure forms, Vis evaluates them in order, persists observations into `iteration.blocks`, and either continues or accepts one terminal `(answer ...)`.

## Provenance

Every persisted block has a canonical provenance reference and a rendering kind. Examples:

```text
turn/3f2a91c0/iteration/1/block/1
turn/3f2a91c0/iteration/4/block/2/tool/bash
turn/3f2a91c0/iteration/6/block/1/error
```

The block map stores machine data:

```clojure
{:provenance {:ref "turn/3f2a91c0/iteration/4/block/2"
              :op :sci/eval
              :status :done}
 :rendering-kind :vis/sci}
```

Markdown is not stored in provenance.

## Final-answer guard

Before Vis accepts `(answer ...)`, it reads `(v/intents)` / `db-intents` for the current turn-state focus. The answer is rejected unless every focused intent is fulfilled or abandoned.

The guard rejects:

- no focused intent;
- focused active intent without an active plan;
- active plan without gates;
- required open gate;
- required blocked gate without re-plan or abandonment;
- proven gates without intent fulfillment.

Unfocused old active intents are reported by `v/intents` but do not block the current answer.

## Rendering kinds

Channels render blocks by `:rendering-kind`:

- `:vis/sci` — normal evaluated form/result;
- `:vis/silent` — executed and citeable, hidden/collapsed in chat;
- `:vis/system` — Vis-owned intent/gate/focus/control call, collapsed as `SYSTEM`;
- `:vis/tool` — tool child projection;
- `:vis/answer` — final answer form;
- `:vis/error` and `:vis/diagnostic` — diagnostic/audit surfaces.

Raw errors from `:vis/system` blocks are preserved for audit/debug views, but normal chat rendering suppresses them unless the user opens diagnostics.
