# TUI block/result UX revamp

## Current badge pipeline

Tool badges are emergent, not explicit:

1. Extension symbols declare:
   - `:ext.symbol/symbol`
   - `:ext.symbol/tag` — `:observation` / `:mutation`
   - `:ext.symbol/render-fn` — returns channel IR for the tool result

2. Runtime wrapper writes sink entries in `extension/write-sink-entries!`:
   ```clojure
   {:position n
    :form "(git/status)"
    :symbol 'status
    :tag :observation
    :success? true
    :result <render-fn IR>
    :error nil}
   ```

3. TUI restore path (`channel_tui/chat.clj`):
   - `form-result-kind` → `:tool` when `:channel` exists
   - `form-result-detail` reads `:symbol` / `:tag` from first channel entry
   - `form-result-render` combines sink IR via `combine-render-values`

4. TUI renderer (`channel_tui/render.clj`) collapses tool result by reading the
   **first rendered IR row** as badge text:
   - `maybe-collapse-raw-text-block`
   - `maybe-collapse-block`
   - `collapsible-tool-summary-entry`

Result: renderer infers summary from rendered body. Tool owns the knowledge,
UI guesses from IR. Live and resume paths reimplement the same projection
twice.

## Target UX

One visual block per emitted code fence / code-entry. Inside, ordered
operation rows from the channel sink.

```text
BLOCK - t7/i3 | 2 observations · 1 mutation |                         ✓ 812ms

(let [a (v/cat "a")
      b (v/cat "b")]
  (v/patch ...))

▶ CAT     a.txt                                              120 lines
▶ CAT     b.txt                                               88 lines
▶ PATCH   3 files                                                +24 -8
```

Error:

```text
BLOCK - t7/i3 | 1 observation · 1 mutation |                          ✗ 812ms

(v/patch ...)
 ^--- hunk mismatch

PATCH failed: hunk mismatch
```

Cancellation:

```text
BLOCK - t7/i3 |                                                      ⊘ 5s

(some-long-thing)
```

Timeout:

```text
BLOCK - t7/i3 |                                                     ⏱ 30s
```

Running:

```text
BLOCK - t7/i3 | 1 observation                                       ↻ 5s
```

Rules:

- Code shown once per block.
- Op rows under code, ordered by sink `:position`.
- `▶` / `▼` is the only disclosure affordance; no `[details]` text.
- Header: `BLOCK - <scope> | <op counts> | <status+duration>`.
- Scope is block-level (`t7/i3`), never `/fN`.
- Plain-value form results are **not** rendered (no row). Errors on
  value-only forms render inline with caret + message.
- Multi-fence merge → one block. Header may annotate `merged N fences`.

## Phase 0 — foundation (do this first)

The recurring live-vs-resume split is the root cause of every UX regression
in this area. Fix the data model and naming before touching the renderer.

- [ ] Naming:
  - `display-block` = one TUI card per emitted code fence / merged code-entry
  - `ops` = ordered tool sink events under a `display-block`
  - `proof envelope` / `form` = model-facing per-top-level-form record
    (engine `:forms` BLOB)
- [ ] One canonical `iteration-entry` shape used by both live progress and
      resume:
  ```clojure
  {:position n
   :scope "tN/iM"
   :thinking string-or-nil
   :code "<full fence body, merged sources>"
   :ops [<sink-entry-derived op>]
   :forms [<proof envelopes, model-facing>]
   :status :ok|:error|:running|:cancelled|:timeout
   :duration-ms long
   :error error-map-or-nil}
  ```
  - `:ops` carries display state; `:forms` stays proof-granular.
  - One builder fn `iteration-entry->display-block` consumed by renderer.
- [ ] Parity invariant test (regression gate):
  ```clojure
  (= (iteration-entry-from-progress chunks)
     (iteration-entry-from-restored row))
  ```
  Drive both paths from the same chunk fixture; assert structural equality.
  Failing this test blocks any UX change.
- [ ] Status enum standardised across engine, progress, restore:
      `:ok :error :running :cancelled :timeout`
- [ ] Summary IR contract (see Phase 1).
- [ ] Edge case list captured here:
  - plain-value forms
  - error inside value-only form
  - multi-fence merged code
  - cancellation mid-block
  - timeout mid-block
  - nested tool calls (`def` / `let` / `doseq`)
  - high-fan-out tool loops (>5 same op)
  - mutation success vs observation success (visual parity, no severity bias)

## Phase 1 — explicit display metadata

Tool result metadata gains an explicit `:display` map. Renderer never invents
summary again — it reads metadata, falls back to first-IR-line only when
absent.

- [ ] Metadata spec:
  ```clojure
  {:display {:label "STATUS"
             :summary <ir-or-map-or-string>}}
  ```
- [ ] `:summary` shape:
  - string → lifted via `markdown->ir`, left-aligned, ellipsis on the right
  - `[:ir ...]` → painted as inline flow, left-to-right
  - map `{:left :center :right}` where each value is string or IR:
    - 2-zone layout when `:center` omitted: `left ... right`
    - 3-zone layout otherwise: `left ... center ... right`
    - padding via `display-width`, ellipsis on the side that overflows
  - render contract: one visual row at op-row width
- [ ] Optional metadata fields:
  - `:collapse-default?` boolean (default `true`)
  - `:label-style` `:default | :muted | :strong` (renderer hint)
- [ ] Helpers in `extension.clj`:
  - `display-label`
  - `display-summary`
  - `with-display`
- [ ] Sink-entry spec extension:
  - add `:op` keyword (canonical, e.g. `:git/status`)
  - add `:display` map (copy of `[:metadata :display]`)
  - add `:started-at-ms` / `:finished-at-ms` (set by `write-sink-entries!`
    using the wrapper's timestamps)
- [ ] Backward compat:
  - persisted rows missing `:display` keep working via fallback path
  - first-IR-line still used as last-resort summary
  - no `:tag` on observed tool remains an error (no change)

## Phase 2 — tool summaries

Add `:display` metadata to every observed tool. Two passes: foundation-core
first, then git, then per-language tools.

- [ ] foundation-core editing:
  - `v/ls` — `{:label "LS" :summary {:left path :right "N entries"}}`
  - `v/cat` — `{:label "CAT" :summary {:left path :right "N lines"}}`
  - `v/rg`  — `{:label "RG" :summary {:left pattern :right "N hits"}}`
  - `v/patch` — `{:label "PATCH" :summary {:left "N files" :right "+X -Y"}}`
  - file write/edit helpers analogous
- [ ] foundation-git:
  - `git/status` — `{:label "STATUS" :summary {:left branch :center sha :right "N entries"}}`
                   or `{:right "clean"}`
  - `git/add` — `{:label "ADD" :summary {:left "all" | "N paths"}}`
  - `git/commit!` — `{:label "COMMIT" :summary {:left short-sha :right subject}}`
  - `git/push!` — `{:label "PUSH" :summary {:left ref :right state}}`
  - `git/diff` / `git/log` / `git/show` / merge ops
- [ ] foundation-search, voice, bridge: same pattern.
- [ ] Batch-friendly redesign (see Phase 4 doseq policy):
  - prefer vec-of-inputs entry points (`(v/cat ["a" "b" "c"])`) wherever
    sensible
  - summary collapses to `{:left "N files" :right total-lines}`
- [ ] Fallback tests: tool without `:display` still renders with derived
      label + summary.

## Phase 3 — unified iteration-entry pipeline

Single source of truth for live and resume.

- [ ] Move the canonical `iteration-entry` shape into a shared ns
      (`com.blockether.vis.internal.iteration` or similar).
- [ ] Rewrite `progress/update-entry` to populate the shared shape:
  - `:ops` populated from `:form-result` chunks via sink entries
  - `:forms` retained for proof envelopes
- [ ] Rewrite `chat/it->iteration-entry` (resume) to populate the same
      shape from persisted rows:
  - read `:forms` BLOB
  - reconstruct `:ops` from each form's `:channel` slice
  - retain `:forms` for the model-facing surface used elsewhere
- [ ] Resume keeps **one** `display-block` per persisted iteration, not one
      per form (current quick fix is correct; bake it into the shared
      builder).
- [ ] Merged multi-fence: one `display-block`. Engine's `multi-fence-merged?`
      flag exposed in the iteration-entry as `:merged-fences int`. Header may
      annotate.
- [ ] Cancellation / timeout statuses propagate into the iteration-entry,
      not just the turn-level.
- [ ] Op aggregation policy lives in this builder (see Phase 4).

## Phase 4 — high-fan-out policy

100 same-op rows in a single block is bad agent behaviour AND bad UX.
Pressure model toward batch, render gracefully when it slips.

- [ ] Engine soft warning: when a single `display-block` accumulates
      `> threshold` ops with the same `:op`, append a recap line:
  ```
  RECAP HINT  call (v/cat ["a" "b" "c"]) once instead of N times
  ```
  Recap visible to model; surfaces in the trace.
- [ ] Threshold: `5` for non-batch tools (configurable per-tool via
      `:ext.symbol/batch-hint`).
- [ ] Renderer aggregation for any block where `count(ops same :op) > 10`:
  - one synthetic row `▶ CAT × 100 (a, b, c, …)`
  - expanded shows full list with per-op summaries
- [ ] Header counts always reflect REAL counts:
      `100 observations · 1 mutation` regardless of aggregation.
- [ ] No hard engine cap on op count. Refusal not necessary; warning + UI
      aggregation enough.

## Phase 5 — renderer

One renderer fn consumes `display-block`.

- [ ] Header row:
  ```
  BLOCK - <scope> | <counts> | <status+duration>
  ```
  - counts:
    - full ≥ 50 cols: `2 observations · 1 mutation`
    - compact ≥ 36 cols: `2 obs · 1 mut`
    - narrow: `O2 M1`
  - status glyphs:
    - `✓ <dur>` ok
    - `✗ <dur>` error
    - `↻ <dur>` running
    - `⊘ <dur>` cancelled
    - `⏱ <dur>` timeout
- [ ] Code body once, full source, no per-form chrome.
- [ ] Op rows:
  ```
  ▶ <LABEL>   <summary>
  ```
  - label width = max label in block, padded
  - summary uses zone-aware layout from `:display/summary`
  - expand shows full `:result-render`
  - disclosure node id: `<block-scope>:op<position>`
- [ ] Plain-value form: no op row, no body. Hide entirely.
- [ ] Value-only form with error: inline caret + error message under code,
      no fake badge.
- [ ] Long error: collapsible body, summary on header row, full trace under
      disclosure.
- [ ] Strip block-level footer scope stamps (now in header).

## Phase 6 — tests & fixtures

- [ ] **Parity invariant** (Phase 0 gate) — live chunks vs persisted rows
      produce equal `iteration-entry`.
- [ ] Fixture: session `37e61e52-2fdc-4925-b84b-b53fa9619588`, turn 6.
  - one block for `git/status`, `git/add`, `git/commit!`, `git/push!`
  - header: `1 observation · 3 mutations`, `✓ 3.3s`
  - four op rows
- [ ] Fixture: nested `(let [a (v/cat) b (v/cat)] (v/patch))`
  - one block
  - three op rows
  - counts `2 observations · 1 mutation`
- [ ] Fixture: `(def x (v/cat "x"))`
  - one block
  - one op row labelled `CAT`
  - code row shows `def` line
- [ ] Fixture: plain value `(+ 1 2)`
  - one block
  - zero op rows
  - no result body
- [ ] Fixture: value-only form error `(/ 1 0)`
  - one block
  - zero op rows
  - inline caret + message under code
- [ ] Fixture: doseq fan-out `(doseq [p ["a" "b" "c" ... × 30]] (v/cat p))`
  - one block
  - synthetic grouped row `▶ CAT × 30 (a, b, c, …)`
  - header counts `30 observations`
  - recap hint surfaces to model
- [ ] Fixture: cancellation mid-block
  - block status `:cancelled`
  - ops show whichever completed
- [ ] Fixture: timeout
  - block status `:timeout`
  - last op row may be `↻` if it never resolved (then forced to `⏱`)
- [ ] Fixture: merged multi-fence
  - one block, header annotates `merged 2 fences`
  - all ops visible

## Phase 7 — cleanup

- [ ] Rename TUI vars away from overloaded `forms` where they mean blocks.
- [ ] Drop “summary from first IR paragraph” code path once Phase 2 covers
      all in-tree tools (keep helper for unknown 3rd-party tools).
- [ ] Telegram + other channels: reuse `:display {:label :summary}` so each
      channel does NOT reimplement summary inference.
- [ ] Comments updated:
  - proof envelopes vs display blocks
  - `:forms` (persisted) vs `:ops` (display)
- [ ] Lint warning when an observed tool registers `:render-fn` without
      `:display` metadata (advisory, not blocking).
