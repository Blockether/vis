# TUI block/result UX revamp

## Core decision

`:render-fn` MUST return a map:

```clojure
{:summary <summary>     ;; required — single visual row, the badge
 :display <ir>}          ;; required — full expanded body
```

`<summary>` is either:

```clojure
<ir>                          ;; one [:p ...] paragraph, left-to-right flow
                              ;; first [:strong ...] is the label
| {:left   <ir-or-string>     ;; zone layout
   :center <ir-or-string>     ;; optional
   :right  <ir-or-string>}    ;; optional, defaults right-anchored
```

`<ir>` is canonical channel IR (`[:ir ...]`).

No legacy. No fallback to "first paragraph of raw IR". `:render-fn` returning
raw IR is invalid and refused at extension register time.

Same shape applies to `:render-error-fn`.

## Current pipeline (for reference)

1. Extension symbols declare `:ext.symbol/symbol`, `:ext.symbol/tag`
   (`:observation` / `:mutation`), `:ext.symbol/render-fn` returning IR.

2. Runtime wrapper writes sink entries (`extension/write-sink-entries!`):
   ```clojure
   {:position n :form "(git/status)"
    :symbol 'status :tag :observation
    :success? true
    :result <render-fn IR>
    :error nil}
   ```

3. TUI restore reads sink entries (`channel_tui/chat.clj`).

4. TUI renderer (`channel_tui/render.clj`) collapses tool result by
   reading first IR paragraph as badge.

This emergent badge derivation is removed. Renderer reads `:summary` directly.

## Target UX

One block per emitted code fence / merged code-entry. Ordered op rows under
code, sourced from the channel sink.

```text
BLOCK - t7/i3 | 2 observations · 1 mutation |                         ✓ 812ms

(let [a (v/cat "a")
      b (v/cat "b")]
  (v/patch ...))

▶ CAT     a.txt                                              120 lines
▶ CAT     b.txt                                               88 lines
▶ PATCH   3 files                                                +24 -8
```

Statuses:
- `✓ <dur>` ok
- `✗ <dur>` error
- `↻ <dur>` running
- `⊘ <dur>` cancelled
- `⏱ <dur>` timeout

Rules:
- Code shown once per block.
- `▶` / `▼` only disclosure affordance. No `[details]` text.
- Header: `BLOCK - <scope> | <op counts> | <status+duration>`.
- Scope is block-level (`t7/i3`), never `/fN`.
- Plain-value forms hidden (no op row, no body). Errors on value-only forms
  render inline with caret + message.
- Multi-fence merge → one block; header may annotate `merged N fences`.

## Phase 0 — foundation

Live-vs-resume split is the root cause of every regression. Fix the data
model and naming before touching the renderer.

- [ ] Naming:
  - `display-block` = one TUI card per code fence / merged code-entry
  - `ops` = ordered tool sink events under a `display-block`
  - `proof envelope` / `form` = model-facing per-top-level-form record
    (engine `:forms` BLOB)
- [ ] Canonical `iteration-entry` shape used by live AND resume:
  ```clojure
  {:position n
   :scope "tN/iM"
   :thinking string-or-nil
   :code "<full fence body, merged sources>"
   :ops [<sink-derived op>]
   :forms [<proof envelopes>]
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
  Same chunk fixture, two paths, equal output. Failing blocks any UX change.
- [ ] Status enum standardised across engine, progress, restore.
- [ ] Edge case list:
  - plain-value forms
  - error inside value-only form
  - merged multi-fence
  - cancellation mid-block
  - timeout mid-block
  - nested tool calls (`def` / `let` / `doseq`)
  - high-fan-out tool loops
  - mutation success vs observation success (visual parity, no severity bias)

## Phase 1 — `:render-fn` contract

Hard cut. New shape only.

- [ ] Spec:
  ```clojure
  (s/def :render.zone/left   render-value-or-string?)
  (s/def :render.zone/center render-value-or-string?)
  (s/def :render.zone/right  render-value-or-string?)
  (s/def :render/zones
    (s/keys :req-un [:render.zone/left]
            :opt-un [:render.zone/center :render.zone/right]))

  (s/def :render/summary
    (s/or :ir    render-value?
          :zones :render/zones))

  (s/def :render/display render-value?)

  (s/def ::render-fn-result
    (s/keys :req-un [:render/summary :render/display]))
  ```
- [ ] `:render-fn` and `:render-error-fn` MUST return `::render-fn-result`.
- [ ] `register-extension!` validates each registered observed-tool symbol
      against the new contract at register time (smoke: call `:render-fn`
      with a sentinel payload OR rely on per-call validation at sink write).
- [ ] Sink-entry spec extension:
  - replace `:result` IR with full `{:summary :display}` value
  - add `:op` keyword (`:git/status`)
  - add `:started-at-ms` / `:finished-at-ms`
- [ ] Renderer reads `:summary` and `:display` directly. Heuristic
      "first paragraph = summary" deleted.
- [ ] Convention (not spec): first `[:strong ...]` in `<summary>` IR is the
      label. No `:label` field. If you want explicit label, lift it into the
      summary IR.

## Phase 2 — tool migration (hard cut)

Every observed tool with `:render-fn` rewritten.

- [ ] foundation-core editing:
  - `v/ls`, `v/cat`, `v/rg`, `v/patch`, file write/edit helpers
- [ ] foundation-git:
  - `git/status`, `git/add`, `git/commit!`, `git/push!`,
    `git/diff`, `git/log`, `git/show`, `git/blame`, merge ops, fetch
- [ ] foundation-search, foundation-voice, foundation-bridge
- [ ] language-clojure tools
- [ ] Each rewrite:
  - summary as zones `{:left ... :right ...}` where natural
  - summary as plain IR otherwise
  - display = canonical full IR (code blocks, tables, badges, etc.)
- [ ] Each tool gets a unit test asserting `::render-fn-result`
      conformance.
- [ ] `:render-error-fn` rewritten to same shape.

## Phase 3 — unified iteration-entry pipeline

Single source of truth for live and resume.

- [ ] Move `iteration-entry` shape into shared ns
      (`com.blockether.vis.internal.iteration` or similar).
- [ ] Rewrite `progress/update-entry` to populate the shared shape, with
      `:ops` from `:form-result` chunks via sink entries.
- [ ] Rewrite `chat/it->iteration-entry` (resume) to populate the same
      shape from persisted rows.
- [ ] One `display-block` per persisted iteration, not per form.
- [ ] Merged multi-fence: one `display-block` with `:merged-fences int`.
- [ ] Cancellation / timeout propagate into iteration-entry, not just
      turn-level.

## Phase 4 — high-fan-out policy

100 same-op rows in one block is bad agent behaviour AND bad UX.

- [ ] Engine soft warning: when a single `display-block` accumulates
      `> threshold` ops with the same `:op`, append a recap line:
  ```
  RECAP HINT  call (v/cat ["a" "b" "c"]) once instead of N times
  ```
- [ ] Default threshold `5`. Per-tool override via `:ext.symbol/batch-hint`.
- [ ] Renderer aggregation when `count(ops same :op) > 10`:
  - synthetic row `▶ CAT × 100 (a, b, c, …)`
  - expanded shows full list with per-op summaries
- [ ] Header counts always real:
      `100 observations · 1 mutation` regardless of aggregation.

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
  - status glyphs as above
- [ ] Code body once, full source, no per-form chrome.
- [ ] Op row:
  ```
  ▶ <LABEL>   <summary-row>
  ```
  - label derived from first `[:strong ...]` in summary IR (or zone left
    if zones)
  - label padded to max in block
  - summary row painted from `:summary`:
    - zones map → 2-zone (`left ... right`) or 3-zone (`left ... center
      ... right`) padding
    - IR → left-aligned, ellipsis right
  - expand reveals `:display`
  - disclosure node id: `<block-scope>:op<position>`
- [ ] Plain-value form: no op row, no body.
- [ ] Value-only form with error: inline caret + message under code, no
      fake badge.
- [ ] Long error: header carries status, full trace under disclosure.
- [ ] Strip block-level footer scope stamps (now in header).

## Phase 6 — tests & fixtures

- [ ] **Parity invariant** (Phase 0 gate).
- [ ] Per-tool `:render-fn` conformance unit tests (Phase 2).
- [ ] Fixture: session `37e61e52-2fdc-4925-b84b-b53fa9619588`, turn 6.
  - one block for `git/status`, `git/add`, `git/commit!`, `git/push!`
  - header: `1 observation · 3 mutations`, `✓ 3.3s`
  - four op rows
- [ ] Fixture: nested `(let [a (v/cat) b (v/cat)] (v/patch))` → one block,
      three op rows, counts `2 observations · 1 mutation`.
- [ ] Fixture: `(def x (v/cat "x"))` → one block, one op row.
- [ ] Fixture: plain value `(+ 1 2)` → one block, zero op rows.
- [ ] Fixture: value-only form error `(/ 1 0)` → inline caret.
- [ ] Fixture: doseq fan-out → grouped row + header counts + recap hint.
- [ ] Fixture: cancellation mid-block → status `:cancelled`.
- [ ] Fixture: timeout → status `:timeout`.
- [ ] Fixture: merged multi-fence → header annotates `merged N fences`.

## Phase 7 — cleanup

- [ ] Rename TUI vars away from overloaded `forms` where they mean blocks.
- [ ] Drop every heuristic that derived summary from rendered IR.
- [ ] Telegram + other channels reuse `:summary` and `:display` directly.
- [ ] Comments updated:
  - proof envelopes vs display blocks
  - `:forms` (persisted) vs `:ops` (display)
- [ ] `register-extension!` rejects observed tools whose `:render-fn` does
      not match `::render-fn-result`. Hard error, no warning.
