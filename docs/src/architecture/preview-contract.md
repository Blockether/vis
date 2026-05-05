# Preview contract

Vis separates **acquisition** from **preview projection** from **presentation**.

- Acquisition functions read, search, run, or fetch data.
- `v/preview` chooses the payload projection that enters the journal and human surfaces.
- Tools/results own presentation metadata.
- Provenance and lifecycle metadata stay unchanged.

The journal is an attention window, not storage. Runtime values and persisted evidence are memory; previews are projections over that memory.

## Core rule

```clojure
(def data (v/cat "src/foo.clj"))
(v/preview data {:result [[:lines {:from 40 :to 120}]]})
```

Read once. Bind the full value. Preview only the payload slice needed for the next model step or human display.

## One public operation

There is one model-facing operation:

```clojure
(v/preview value)
(v/preview value preview-eql)
```

`eql` is the projection directly. There is no option map wrapper.

No:

- `:select`
- `:presentation-kind`
- `:show`
- surface opts
- title/columns/language knobs
- hidden mode

`v/silent!` is removed. Shape/default display is:

```clojure
(v/preview value)
```

## Preview always enters the journal

Every `v/preview` call is journal-visible evidence.

If one top-level form calls `v/preview` multiple times, every preview projection from that form is carried to the journal. The renderer must not keep only the final preview.

Example:

```clojure
(let [f (v/cat "src/foo.clj")
      hits (v/rg ["foo"] "src")]
  [(v/preview f {:result [[:lines {:from 0 :to 80}]]})
   (v/preview hits {:result [[:hits {:from 0 :to 10} [:path :line :text]]]})])
```

Both previews land in the journal.

## Preview always shows shape

Every preview shown in the journal includes shape metadata.

Minimum journal payload:

```clojure
{:source-shape {...}
 :projection-shape {...}}
```

If an preview EQL projection also produces a body, the journal shows both:

1. shape: type, keys, counts, truncation metadata;
2. selected projection: lines/items/text rendered from the same data the user can inspect.

## Same data, different presentation

User and model must see the same selected data.

- Journal and TUI receive the same preview projection.
- Renderers may format that same data differently: compact journal text, foldable TUI panel, transcript row, final-answer block.
- Presentation can differ; data cannot.
- No journal-only or TUI-only data branches.

## Presentation metadata is source-owned

`v/preview` does not accept presentation hints.

Tools and SCI results report presentation metadata because they know their domain. Preview preserves that metadata on the selected projection.

Examples:

```clojure
{:result {:path "src/foo.clj"
          :lines [...]}
 :presentation {:kind :source
                :path "src/foo.clj"
                :line-key :lines}}
```

```clojure
{:result {:hits [{:path "src/foo.clj" :line 42 :text "..."}]}
 :presentation {:kind :search-hits
                :row-keys [:path :line :text]}}
```

Presentation source order:

1. presentation metadata already in the tool/SCI result;
2. source tool provenance, e.g. `v/cat` path + line structure;
3. projection shape, e.g. maps with `:path`, `:line`, `:text` imply search hits;
4. fallback data rendering.

Recursive presentation rule: when selected nested values carry presentation metadata, preview preserves that metadata on the corresponding nested projection.

## Provenance boundary

`v/preview` operates on payload fields, not proof metadata.

A Vis block/tool/eval value may contain:

```clojure
{:result ...
 :stdout ...
 :stderr ...
 :error ...
 :provenance ...
 :rendering-kind ...
 :execution-time-ms ...}
```

Preview EQL may select payload fields such as `:result`, `:stdout`, `:stderr`, `:error`, and ordinary nested values.

Preview must not mutate, drop, fabricate, or reinterpret:

- canonical refs;
- provenance;
- lifecycle status;
- execution timing;
- proof/intent/gate metadata.

Those remain attached around the preview and remain the source of proofability. If proof needs provenance, use provenance/intents APIs; do not pull provenance through `v/preview`.

## Acquisition contract

Acquisition functions return data. They do not decide attention.

Examples:

```clojure
(v/cat path)
(v/rg ["literal"] root)
(v/ls root)
(v/bash command)
(exa/search ...)
```

Rules:

1. No hidden journal-oriented character caps in acquisition functions.
2. If acquisition cannot return complete data, it must say so in data: `:complete? false`, `:truncated-by`, `:next-offset`, `:limit`, or equivalent explicit metadata.
3. Safety caps are allowed only at true boundary risks: process output explosion, remote API limits, timeout, memory pressure, or provider-side truncation. These caps are not presentation policy and must be observable.
4. Default renderers may be compact, but compact rendering must not imply the underlying acquisition result was compact.
5. `v/preview` is the normal way to reduce journal/TUI/final-answer noise.

So `v/rg` should not have a journal character preview baked into the search result. It may have explicit search scope/result controls, and it must expose whether more hits exist. The model then does:

```clojure
(def hits (v/rg ["render"] "src"))
(v/preview hits {:result [[:hits {:from 0 :to 20} [:path :line :text]]]})
```

Same for Exa: fetch result data first; preview it separately. If Exa or the client truncates because of remote/output constraints, expose truncation metadata and let `v/preview` choose the visible projection.

## preview EQL projection contract

`v/preview` takes zero or one preview EQL projection.

Plain keywords always mean map keys. This is why range controls live in a **field parameter map**, not in the selector vector itself: payload keys named `:from` or `:to` remain selectable as ordinary keys.

Supported forms:

| Form | Meaning |
|---|---|
| omitted / nil | Shape of the whole payload, plus renderer-inferred tiny head when safe. |
| `:key` | Keep map key `:key`. |
| `[:a :b]` | Pull keys `:a` and `:b` from a map. Keys named `:from` and `:to` are just keys here. |
| `[:*]` | Pull all map keys at this level. |
| `[{:k eql}]` | Pull key `:k` and apply nested projection `eql`. |
| `{:k eql}` | Same nested pull form, useful at map boundaries. |
| `[[:k {:from n :to m}]]` | Pull key `:k`, then take zero-based `[n,m)` range from its sequential/text value. |
| `[[:k {:from n :to m} eql]]` | Pull key `:k`, take zero-based `[n,m)` range, then apply `eql` to each selected item. |

Projection is recursive. This supports nested result trees and recursive presentation metadata.

Examples:

```clojure
;; Pull a plain result submap.
(v/preview x {:result [:path :total-lines]})

;; Pull keys literally named :from and :to.
(v/preview x {:result [:from :to]})

;; Pull all fields under :result.
(v/preview x {:result [:*]})

;; Pull source lines by data index.
(v/preview x {:result [[:lines {:from 40 :to 120}]]})

;; Pull first search hits and only selected fields from each hit.
(v/preview x {:result [[:hits {:from 0 :to 12} [:path :line :text]]]})

;; Pull stdout and error payload.
(v/preview x [:stdout :error])
```

Add new EQL forms only after a real repeated need appears.

## Renderer inference

Renderers render the same selected data according to preserved presentation metadata.

Surface behavior is renderer-owned:

- Journal: compact attention for next model iteration; always includes shape; includes every preview emitted by a block.
- TUI: readable, foldable, copyable display inferred from projection and presentation metadata.
- Transcript: forensic record with preview metadata and refs. Full raw evidence is already stored elsewhere when available; no opt needed.
- Final answer: user-facing rendering of only what supports the answer.

If something should not clutter the TUI, the TUI decides fold/elision from size, shape, provenance, and presentation metadata. The model does not pass a visibility flag.

## Preview result shape

A preview result carries projection, preserved presentation metadata, shape, and original proof metadata around it:

```clojure
{:preview-eql {:result [[:lines {:from 40 :to 120}]]}
 :presentation <preserved-or-derived>
 :source-shape {...}
 :projection-shape {...}
 :truncated? true
 :truncated-by :limit
 :result ...
 :provenance <unchanged>}
```

The preview projection is evidence-bearing because it is produced by a top-level form and gets a canonical ref. The full underlying value is evidence too when it was emitted or bound earlier. Provenance on both remains canonical and unchanged.

## Renderer duties

Renderers must not dump arbitrary full values by accident.

For any value:

1. If the value is a `v/preview` result, render shape plus selected projection.
2. If a block contains multiple preview results, render all of them into the journal.
3. If the value is a raw acquisition result, render compact metadata and tell the model to call `v/preview` for curated display.
4. If the value is huge and not a preview, show shape, counts, truncation metadata, and provenance, not full body.
5. Never hide truncation. Show `:truncated-by` or equivalent.

## Examples

### Full file once, source slice visible

```clojure
(def core-file (v/cat "extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj"))
(v/preview core-file {:result [[:lines {:from 728 :to 790}]]})
```

### Search once, top hits visible

```clojure
(def hits (v/rg ["render-silent"] "src"))
(v/preview hits {:result [[:hits {:from 0 :to 12} [:path :line :text]]]})
```

### Exa fetch once, top results visible

```clojure
(def papers (exa/search {:query "recursive language models journal memory"}))
(v/preview papers {:result [[:results {:from 0 :to 8} [:title :url :published-date]]]})
```

### Aggregate state, shape only

```clojure
(def investigation {:files [core-file]
                    :hits hits
                    :notes notes})
(v/preview investigation)
```

## Prompt wording

The system prompt should teach:

> Read/search/fetch once and bind the full result with `def`. Use `v/preview` with EQL to choose the result/stdout/stderr/error projection that appears in `<journal>` and human surfaces. Every preview includes shape in the journal. Every preview emitted by a block lands in the journal. `[:*]` pulls all map keys at one level. Plain keywords always mean payload keys; range uses field parameter maps like `[[:hits {:from 0 :to 12} [:path :line :text]]]` so payload keys named `:from` and `:to` remain selectable. User and model see the same selected data; presentation may differ by renderer. Tools/results own presentation metadata; preview preserves it. Provenance and lifecycle metadata stay unchanged and remain the proof substrate. Acquisition functions return data; preview controls attention. Avoid duplicate reads just to change display. Do not use `v/silent!`; use `(v/preview value)` for shape/default preview or `(v/preview value preview-eql)` for a selected projection.

## Non-goals

- `v/preview` is not a summarizer that invents facts.
- `v/preview` does not replace evidence refs, intents, gates, or proof slots.
- `v/preview` does not authorize hiding failures or truncation.
- `v/preview` does not mutate provenance or lifecycle state.
- Acquisition functions may still expose explicit paging/scope controls for performance and safety; those controls are not journal presentation policy.
