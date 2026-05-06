# Preview contract

Vis uses three separate layers:

- **Acquire** — tools read/search/fetch/run and return data.
- **Preview** — `v/preview` selects the data slice that should be visible and records that projection for the journal.
- **Present** — rendering-kind functions display that same selected data for journal, TUI, transcript, or answer.

Journal is attention, not storage. Full runtime values and persisted evidence are memory.

## Public API

One public operation:

```clojure
(v/preview value)
(v/preview value preview-eql)
```

Rules:

- `preview-eql` is the projection directly.
- No option wrapper.
- No `:select`.
- No `:presentation-kind`.
- No `:show`.
- No surface opts.
- No title / columns / language knobs.
- No hidden mode.
- `v/silent!` is removed.

Full-payload preview:

```clojure
(v/preview value)
```

With no `preview-eql`, preview selects the whole payload. Renderers still apply safety bounds.

## Read once, preview many

```clojure
(def file (v/cat "src/foo.clj"))
(v/preview file {:result [[:lines {:from 40 :to 120}]]})
```

- `v/cat` reads the whole file.
- `v/preview` selects lines for journal/TUI.
- `v/preview` is an observation call, not a value you need to `def`.
- It writes the selected projection into `<journal>` and TUI/human surfaces.
- Use `v/preview` instead of echo-only binding patterns when the goal is only to show data in the journal.
- Keep `def` for durable source/acquisition values you need later.
- Call preview separately as a standalone observation.
- Correct: `(v/preview focus {:result [[:lines {:from 100 :to 180}]]})`.
- Do not call `v/cat` again just to display a different slice.

Search example:

```clojure
(def hits (v/rg {:all ["render"] :paths ["src"]}))
(v/preview hits {:result [[:hits {:from 0 :to 12} [:path :line :text]]]})
```

Many previews over one value are fine:

```clojure
(def file (v/cat "src/foo.clj"))
(v/preview file {:result [[:lines {:from 0 :to 80}]]})
(v/preview file {:result [[:lines {:from 200 :to 260}]]})
```

Preview strategies:

- **Show a slice** as journal-visible evidence:

  ```clojure
  (v/preview focus {:result [[:lines {:from 100 :to 180}]]})
  ```

- **Show many slices** from one full value:

  ```clojure
  (do
    (v/preview file {:result [[:lines {:from 0 :to 80}]]})
    (v/preview file {:result [[:lines {:from 200 :to 260}]]})
    :done)
  ```

- **Show a whole payload** when no slicing is needed:

  ```clojure
  (v/preview value)
  ```

- **Compute later and display now** by binding the durable value, then previewing separately:

  ```clojure
  (def x expensive-or-useful-value)
  (v/preview x)
  ```

## Every preview lands in journal

Every `v/preview` call is journal-visible evidence.

If one top-level block emits many previews, all of them enter the journal.

```clojure
(let [f    (v/cat "src/foo.clj")
      hits (v/rg {:all ["foo"] :paths ["src"]})]
  [(v/preview f {:result [[:lines {:from 0 :to 80}]]})
   (v/preview hits {:result [[:hits {:from 0 :to 10} [:path :line :text]]]})])
```

Journal must contain both previews. Renderer must not keep only the final value.

Implementation schema:

- executed block carries `:previews` — vector of every preview emitted by that block;
- each preview carries `:preview-eql` — the projection used;
- each preview also carries `:preview {:rendering-kind ...}` when known;
- provenance/lifecycle stays separate and unchanged.

## Same data, different presentation

User and model see the same selected data.

- Journal and TUI receive the same preview projection.
- Renderers may format differently.
- Presentation can differ.
- Data cannot differ.
- No journal-only data branch.
- No TUI-only data branch.

## Rendering kind belongs to tools/results

`v/preview` does not accept presentation hints.

Tools and SCI results own rendering-kind metadata because they know their domain.
Extensions can register rendering-kind functions that turn selected data into Markdown/text.

Example source result:

```clojure
{:result {:path "src/foo.clj"
          :lines [...]}
 :presentation {:kind :source
                :path "src/foo.clj"
                :line-key :lines}}
```

Example search result:

```clojure
{:result {:hits [{:path "src/foo.clj"
                  :line 42
                  :text "..."}]}
 :presentation {:kind :search-hits
                :row-keys [:path :line :text]}}
```

Preview preserves rendering-kind metadata on the selected projection.

Rendering source order:

1. `:presentation` already on the result;
2. tool provenance, e.g. `v/cat` path + line structure;
3. projection shape, e.g. maps with `:path`, `:line`, `:text`;
4. fallback data rendering.

Nested selected values may carry nested rendering metadata. Preserve it recursively.

## Built-in rendering strategies

Foundation registers initial rendering-kind functions:

- `:source` — numbered source/text lines.
- `:search-hits` — path/line/text hit rows.
- `:table` — sequence-of-maps or map key/value table.
- `:tree` — directory/tree-shaped data.
- `:diagnostic` — exit/stdout/stderr/error report.
- `:text` — plain text block.
- `:markdown` — already-rendered Markdown.
- `:diff` — diff fenced block.
- `:data` — EDN fallback.

Other extensions may add more rendering-kind functions through `:ext/rendering-kinds`.

## Provenance boundary

`v/preview` selects payload fields only:

- `:result`
- `:stdout`
- `:stderr`
- `:error`
- ordinary nested values

`v/preview` must not mutate, drop, fabricate, or reinterpret:

- canonical refs;
- provenance;
- lifecycle status;
- execution timing;
- proof/intent/gate metadata.

Proof uses provenance/intents APIs, not preview pulls.

## Acquisition contract

Acquisition functions return data. They do not decide attention.

Examples:

```clojure
(v/cat path)
(v/rg {:all ["literal"] :paths [root]})
(v/ls root)
(v/bash command)
(exa/search ...)
```

Rules:

- `v/cat` is cat: reads the whole file. No max chars, max width, max lines, offset, or pagination opts.
- No hidden journal-oriented character caps in acquisition functions.
- If acquisition cannot return complete data, it must say so with explicit metadata such as `:complete? false`, `:truncated-by`, or remote/provider truncation fields.
- Safety caps are allowed only at real boundary risks: process output explosion, remote API limit, timeout, memory pressure, provider-side truncation.
- Safety caps are not presentation policy and must be observable.
- Default raw-tool renderers may be compact, but compact rendering must not imply the underlying acquisition result was compact.
- `v/preview` is the normal way to reduce journal/TUI/final-answer noise.

## Preview EQL

`v/preview` takes zero or one `preview-eql` projection.

Plain keywords always mean payload keys. Therefore keys named `:from` and `:to` are safe.

Supported forms:

| Form | Meaning |
|---|---|
| omitted / nil | Whole payload, rendered with renderer safety bounds. |
| `:key` | Pull map key `:key`. |
| `[:a :b]` | Pull keys `:a` and `:b`. |
| `[:*]` | Pull all map keys at this level. |
| `[{:k preview-eql}]` | Pull key `:k`, then apply nested projection. |
| `{:k preview-eql}` | Same nested pull form, useful at map boundaries. |
| `[[:k {:from n :to m}]]` | Pull key `:k`, then take zero-based `[n,m)` range. |
| `[[:k {:from n :to m} preview-eql]]` | Range key `:k`, then project each selected item. |

Examples:

```clojure
;; Pull result metadata.
(v/preview x {:result [:path :total-lines]})

;; Pull literal keys named :from and :to.
(v/preview x {:result [:from :to]})

;; Pull all keys under :result.
(v/preview x {:result [:*]})

;; Pull source lines by zero-based data index.
(v/preview x {:result [[:lines {:from 40 :to 120}]]})

;; Pull first search hits, selecting fields from each hit.
(v/preview x {:result [[:hits {:from 0 :to 12} [:path :line :text]]]})

;; Pull stdout and error payload.
(v/preview x [:stdout :error])
```

Add new EQL forms only after repeated real need.

## Renderer duties

Renderers must not dump arbitrary full values by accident.

For any value:

- If value is a preview result, render the selected projection.
- If a block has many previews, render all of them into journal.
- Use registered rendering-kind functions when available.
- If value is raw acquisition result, render compact metadata and nudge model to use `v/preview`.
- If value is huge and not preview, show counts/truncation/provenance, not full body.
- Never hide truncation.

Surface behavior:

- Journal: compact attention; every preview from block.
- TUI: readable, foldable, copyable display inferred from projection and rendering-kind metadata.
- Transcript: forensic record with preview metadata and refs.
- Final answer: user-facing rendering of selected data only when useful.

## Prompt wording

Teach model:

> Read/search/fetch once and bind the full result with `def` when you need it later. Use `v/preview` as a standalone observation to choose the result/stdout/stderr/error projection that appears in `<journal>` and human surfaces; do not echo vars just to inspect them. Correct strategies: `(def file (v/cat "src/foo.clj"))` then `(v/preview file {:result [[:lines {:from 40 :to 120}]]})`, `(do (v/preview a) (v/preview b) :done)`, `(v/preview value)`. `(v/preview value)` without EQL previews the whole payload. Every preview emitted by a block lands in the journal. `[:*]` pulls all map keys at one level. Plain keywords always mean payload keys; ranges use field parameter maps like `[[:hits {:from 0 :to 12} [:path :line :text]]]` so payload keys named `:from` and `:to` remain selectable. User and model see the same selected data; rendering may differ by surface renderer. Tools/results own rendering-kind metadata; preview preserves it. Extensions own rendering-kind functions that turn selected values into Markdown/text. Provenance and lifecycle metadata stay unchanged and remain the proof substrate. `v/cat` reads whole files; use preview for display ranges. Do not use `v/silent!`.

## Non-goals

- No invented summaries.
- No proof replacement.
- No provenance mutation.
- No hidden truncation.
- No display limits in `v/cat`.
