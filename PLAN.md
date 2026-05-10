# PLAN — `extension.clj` spec cleanup + iteration-block classification

Status: design-locked, pre-implementation.
Scope: spec-shape changes + one rename pass + one new namespace.
**No new behaviour, no DB schema migration.** One 6-line compat shim
in persistence covers existing rows for one release.

---

## 1. Rationale — why this PR exists

The iteration-block surface in `extension.clj` accreted four problems
in lock-step. Each is small in isolation; together they make the
spec hard to read and the renderer hard to extend:

### 1.1 Dead spec lines pretending to be load-bearing

- `(s/def ::rendered string?)` at line 54 has **zero readers** in
  `src/`, `test/`, or any extension. It's a leftover from an early
  rendering experiment that never landed. New readers find it,
  assume it matters, and waste cycles tracing it.
- `(s/def ::kind …)` is **defined twice** (lines 69 and 690), both
  for "extension kind" with the same `non-blank-string?` predicate.
  `s/def` registers by qualified keyword, so the second def silently
  overrides the first — equivalent to `(reset! global ...)` in the
  middle of a 1900-line file. A future tweak to one would have no
  effect on the other half of the codebase.

### 1.2 Field names that don't say their type

`:op` and `:op-class` both live on the same `info` map, and the
distinction matters:

- `:op` carries the operation **symbol** — `'v/cat`, `'z/patch`,
  `'vis/answer`. Open set; one-of-many namespaces.
- `:op-class` carries the operation **tag** — closed `#{:op/read
  :op/write :op/exec …}` enum.

Today both look like "op something" so readers conflate them.
Worse, when an SCI eval has no symbol (raw user code), `(:op info)`
is `nil` and `(:op-class info)` may be `:op/sci`, leading callers to
write `(or (:op info) (:op-class info))` patches that flatten the
distinction. **Renaming pins the type** at the field name:

- `:op` → `:op-symbol` (it's a symbol)
- `:op-class` → `:op-tag` (it's a closed-enum tag)

Field names with type hints are how Rich Hickey designs spec
keywords; this PR catches up.

### 1.3 Six iteration-block roles where four would do

The current iteration-block role taxonomy carries six values:

```
#{:vis/answer :vis/tool :vis/system :vis/diagnostic :vis/error :vis/sci}
```

Each value drives a separate renderer arm. Three of them are
artifacts, not real distinctions:

- **`:vis/diagnostic` is a `:vis/system`** at the rendering level.
  A "diagnostic" message ("the model emitted X but expected Y") and
  a "system" message ("preflight rejected this code") flow through
  the same channel display path with the same intent: nudge the
  model. The split was UX accidental.
- **`:vis/error` is orthogonal to role**. Any block of any kind can
  fail. `:success? false` already lives on tool-result envelopes
  and is the canonical error marker. The renderer should dispatch
  on `:success?`, not on a parallel `:vis/error` role that races
  the per-tool-result success flag.
- **`:vis/sci` is a `:vis/tool`** in everything but ceremony. Every
  SCI eval IS a tool call from the iteration loop's POV; whether
  the LLM emitted `(v/cat path)` or `(some-fn 1 2)`, the loop
  evaluates code in the sandbox and gets a result. The distinction
  is recoverable from `:op-symbol` (nil for raw user code,
  qualified for known tools) without a separate role.

Collapsing these three gives **four real roles**:

```
#{:answer :nudge :tool :thinking}
```

Plus one **new** role for parity:

- **`:thinking`** lifts model reasoning into the same iteration-
  block stream as everything else. Today reasoning is rendered via
  a separate timeline path; channels (TUI, Telegram) duplicate
  dispatch logic. Adding `:thinking` to the iteration-block role
  enum lets the renderer switch on one closed set across thinking
  + tool + answer + nudge.

The role keyword also drops the `:vis/` namespace prefix because
these are **iteration-block roles**, not extension-namespaced
concepts. `:vis/answer` was misleading (the answer isn't owned by
`vis`; it's the iteration-block's role).

### 1.4 `:rendering-kind` is mis-named

The field is called `:rendering-kind`, suggesting it's about how to
render. It's not — it carries **what kind of operation the block
represents**. Renderers happen to dispatch on it, but rendering is
a downstream effect, not the meaning.

Renaming to `:op-system-kind` makes the meaning honest. The field
distinguishes operation-system classifications:

- `:answer` — the assistant's final answer to the user.
- `:nudge` — system-emitted preflight rejections / convergence
  reminders / diagnostics.
- `:tool` — any SCI evaluation, with `:op-symbol` distinguishing
  which tool (or `nil` for raw user code).
- `:thinking` — model reasoning blocks.

The rename is hard (no `:rendering-kind` reads survive in source);
existing DB blobs carry the old keyword, so a single read-side
`case` shim in `persistance_sqlite/core.clj` translates them on
load. Removed in the next release after migration window.

---

## 2. Spec — the new shape

### 2.1 `src/com/blockether/vis/internal/extension.clj`

The single source of truth for tool-result-level fields. Final
specs after this PR:

```clojure
;; ── operation classification ──────────────────────────────────────
(s/def ::op-symbol
  ;; The fully-qualified symbol the iteration block evaluated.
  ;; Open set (any tool extension can register); nil for raw user
  ;; code with no recognised top-level call. Replaces the prior
  ;; ::op spec.
  (s/nilable symbol?))

(s/def ::op-tag
  ;; The closed-enum tag classifying the operation's effect.
  ;; Renamed from ::op-class. Set membership unchanged from today.
  #{:op/read :op/write :op/exec :op/search
    :op/answer :op/nudge :op/inspect :op/network})

;; ── content shape ─────────────────────────────────────────────────
(s/def ::content-kind
  ;; Closed enum describing the SHAPE of the block's value, separate
  ;; from its operation class. Lets renderers pick a layout
  ;; (text vs structured-data vs binary) without re-inspecting the
  ;; value.
  #{:text :data :binary :empty})

;; ── metadata ──────────────────────────────────────────────────────
(s/def ::metadata
  ;; Free-form auxiliary data extensions can attach to a tool result
  ;; (timing, locations, retry hints, etc.). Map-shape is mandatory
  ;; so accumulators can merge cleanly.
  (s/map-of keyword? any?))

;; ── info envelope ─────────────────────────────────────────────────
(s/def ::info
  ;; Tool-result envelope. Adds ::op-symbol (renamed) and the new
  ;; ::metadata slot. Keeps existing ::op-tag, ::success?, ::result,
  ;; ::error, ::stdout, ::stderr fields.
  (s/keys :opt-un [::op-symbol ::op-tag ::content-kind ::metadata
                   ::success? ::result ::error ::stdout ::stderr]))
```

**Removed from `extension.clj`:**

- `(s/def ::rendered string?)` (line 54) — dead.
- `(s/def ::kind …)` at line 690 — duplicate of line 69. Keep line
  69's definition (top of the file, where extension-kind specs
  live); delete line 690.
- `(s/def ::op …)` (line 56) — renamed to `::op-symbol`. No
  fallback registration.
- `(s/def ::op-class …)` (line current) — renamed to `::op-tag`.
- `:rendering-kind` field is **not in `extension.clj`** and never
  was; it lives on iteration-block rows, owned by the new
  namespace below.

### 2.2 `src/com/blockether/vis/internal/iter_block.clj` *(NEW)*

Iteration-block-level fields get their own namespace because they
are not tool-result concerns; they are loop-emission classifications
that span across tool results, answers, nudges, and thinking blocks.

```clojure
(ns com.blockether.vis.internal.iter-block
  "Spec for iteration-block-level classification fields. Iteration
   blocks are the units of an iteration trace: each one is the
   result of one assistant turn through the SCI loop.

   These fields live at the iteration-block top level, NOT inside
   tool-result `:info`. Tool-result fields live in `extension.clj`."
  (:require [clojure.spec.alpha :as s]))

(s/def ::op-system-kind
  ;; Closed enum; collapsed from the prior 6-value
  ;; #{:vis/answer :vis/tool :vis/system :vis/diagnostic
  ;;   :vis/error :vis/sci}. See PLAN.md §1.3 for the rationale of
  ;; each merge.
  #{:answer :nudge :tool :thinking})

(s/def ::iter-block
  ;; Minimum-required iteration-block shape. Other fields
  ;; (timestamps, ids, raw value, etc.) live in their respective
  ;; ns and are merged at read/write boundaries.
  (s/keys :req-un [::op-system-kind]))
```

### 2.3 Read-side compat shim — `persistance_sqlite/core.clj`

Existing local DB rows carry the legacy `:rendering-kind` field
with the old 6-value enum. Translate on read:

```clojure
(defn- read-op-system-kind
  "Translate persisted iteration-block classification into the new
   closed set. New rows write :op-system-kind directly; legacy rows
   carry :rendering-kind with the prior 6-value enum.

   Removed in the next release after this one ships."
  [exec]
  (or (:op-system-kind exec)
      (case (:rendering-kind exec)
        :vis/answer     :answer
        :vis/tool       :tool
        :vis/sci        :tool             ; collapsed
        :vis/system     :nudge
        :vis/diagnostic :nudge            ; collapsed
        :vis/error      :tool             ; collapsed; :success? false carries the error fact
        nil)))
```

Six lines of `case`. Every iteration-block read goes through this.
**Write path emits only the new shape.** Mixed-shape DBs work for
the duration of one release, then the shim is deleted along with
the legacy keyword space.

---

## 3. Change set — file by file

### 3.1 `src/com/blockether/vis/internal/extension.clj`

| line     | change                                                   | why                                                                                          |
|----------|----------------------------------------------------------|----------------------------------------------------------------------------------------------|
| 54       | DELETE `(s/def ::rendered string?)`                      | dead — zero readers                                                                           |
| 56       | RENAME `(s/def ::op …)` → `(s/def ::op-symbol …)`        | type-in-name; distinguish from `::op-tag`                                                     |
| 69       | KEEP `(s/def ::kind …)`                                  | canonical location at top of file alongside other extension-kind specs                        |
| 85       | UPDATE `(s/def ::info …)`                                | swap `::op` → `::op-symbol`, swap `::op-class` → `::op-tag`, add `::metadata` to `:opt-un`    |
| 690      | DELETE duplicate `(s/def ::kind …)`                      | shadows line 69; future tweaks would silently miss callers                                    |
| (current)| RENAME `(s/def ::op-class …)` → `(s/def ::op-tag …)`     | same rationale as `::op` → `::op-symbol`                                                      |
| new      | ADD `(s/def ::op-tag #{…})`                              | promotes the bare `op-classes` set to a real spec                                             |
| new      | ADD `(s/def ::content-kind #{…})`                        | layout shape, decoupled from operation class                                                  |
| new      | ADD `(s/def ::metadata (s/map-of keyword? any?))`        | extension-attached aux data slot                                                              |

### 3.2 `src/com/blockether/vis/internal/iter_block.clj` *(NEW FILE)*

Created from scratch. Contents per §2.2. ~25 LOC including
docstrings.

### 3.3 Caller updates — same commit, no fallback

Every read of the renamed fields migrates atomically:

| reader pattern                     | becomes                                        |
|------------------------------------|------------------------------------------------|
| `(:op info)`                       | `(:op-symbol info)`                            |
| `(:op-class info)`                 | `(:op-tag info)`                               |
| `(:rendering-kind exec)`           | `(:op-system-kind exec)` (via read-helper §2.3)|
| `:vis/answer` (role keyword)       | `:answer`                                      |
| `:vis/tool`                        | `:tool`                                        |
| `:vis/system`                      | `:nudge`                                       |
| `:vis/diagnostic`                  | `:nudge`                                       |
| `:vis/error`                       | DELETED — readers now check `(false? (:success? info))` |
| `:vis/sci`                         | `:tool`                                        |

Estimated callsites (per `rg`):

- `(:op info)` reads — TBD by survey at implementation start.
- `(:op-class info)` reads — TBD.
- `:rendering-kind` reads — only in `persistance_sqlite` and
  channel renderers (TUI, Telegram bot).
- Role keyword reads — channel renderers + iteration-loop emission
  sites.

All reads land in **one commit** alongside the spec rename. Any
`rg` mismatch after the commit is a bug, not a back-compat ladder.

### 3.4 `persistance_sqlite/core.clj`

Add `read-op-system-kind` per §2.3. Wire into the existing
iteration-block read path (replace direct `:rendering-kind` access
with a call to the helper). Write path swaps `:rendering-kind` →
`:op-system-kind`; new rows never carry the legacy key.

---

## 4. What changes for callers

### 4.1 Channel renderers (TUI, Telegram bot, CLI run)

**Before:**

```clojure
(case (:rendering-kind exec)
  :vis/answer     (render-answer …)
  :vis/tool       (render-tool …)
  :vis/sci        (render-sci-or-tool …)   ; near-duplicate of :vis/tool arm
  :vis/system     (render-system …)
  :vis/diagnostic (render-diagnostic …)    ; near-duplicate of :vis/system arm
  :vis/error      (render-error …)         ; orthogonal — also fired alongside other arms
  nil             (render-fallback …))
```

**After:**

```clojure
(case (:op-system-kind exec)
  :answer   (render-answer …)
  :tool     (render-tool exec)             ; :op-symbol distinguishes which tool
  :nudge    (render-nudge …)               ; absorbs prior :system + :diagnostic arms
  :thinking (render-thinking …))           ; new — replaces separate timeline path
;; error rendering happens INSIDE each arm via (false? (:success? info))
```

**Win:** four arms instead of seven; one closed set; error rendering
becomes a status check inside each arm rather than a parallel role.

### 4.2 Tool-result info readers

`(:op info)` callsites become `(:op-symbol info)`. `(:op-class
info)` callsites become `(:op-tag info)`. **No fallback;**
mechanical rename.

### 4.3 Persistence

Cross-version `~/.vis/vis.mdb` files keep working: legacy rows
read through the `case` shim, new rows write the new shape. **No
migration script. No DDL.** Mixed-shape DBs are valid for the
duration of one release.

### 4.4 LLM-emitted code

`(answer …)` API unchanged. The role-keyword changes are at the
loop-emission boundary; the LLM never sees them.

### 4.5 Tests

Existing tests assert exact role keywords (`:vis/answer` etc.)
and exact field names (`:op`, `:rendering-kind`). All such asserts
update in the same commit. Per AGENTS.md S3, every touched ns
keeps its matching test ns; no new test ns required (the renames
ride existing coverage).

---

## 5. Risks & mitigations

| Risk                                                                             | Mitigation                                                                                                                           |
|----------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| Caller missed in the rename pass — `(:op info)` survives, returns `nil`          | clj-kondo + tests catch most; final `rg ":op\\b\\|:op-class\\|:rendering-kind\\|:vis/answer\\|:vis/tool\\|:vis/system\\|:vis/diagnostic\\|:vis/error\\|:vis/sci"` survey before merge                                                                                              |
| Compat shim missed — legacy DB row reads as `nil` op-system-kind                 | Renderer's default arm logs a `tel/log!` warn so observed-in-the-wild misses surface; shim updated                                   |
| Third-party extension reads `(:op info)` directly                                | None today; surface this in CHANGELOG when published                                                                                 |
| `:thinking` role added but no emission site wired in this PR                     | Phase B (separate PR) wires reasoning emission into iteration-block stream; this PR ships the spec slot only                          |
| `:vis/error` removal hides errors in the renderer                                | Each renderer arm now reads `(false? (:success? info))` and surfaces the error consistently — covered by per-arm tests                |
| 6-line case shim becomes permanent because we forget to delete                   | Removal is a TODO comment + a calendar reminder; spec is "next release"                                                              |

---

## 6. Out of scope

- DB schema changes. Zero DDL touched.
- New rendering behaviour. The four renderer arms after collapse
  do exactly what the seven did before, just dispatched cleaner.
- `op-classes` set membership change. Eight values stay; only the
  field name is `op-tag` now.
- `:thinking` emission wiring. Spec slot lands here; the loop-side
  emission of reasoning into iteration-block rows is a separate
  PR.
- Cross-channel renderer-fn changes. Telegram / TUI / CLI
  chokepoints stay as wired in the previous PR.
- LLM prompt changes. Nothing the model sees changes.
- `extension.clj` line-numbering reorg. Edits stay in place; no
  cosmetic shuffle.

---

## 7. Implementation order

Per AGENTS.md S3 + S4 (test-per-ns; nREPL-first verification):

1. **Add `iter_block.clj`** with `::op-system-kind` and `::iter-block`
   specs. No callers yet; loads clean.
2. **Edit `extension.clj`** per §3.1: delete `::rendered`, dedupe
   `::kind`, rename `::op` → `::op-symbol`, rename `::op-class` →
   `::op-tag`, add `::content-kind` + `::metadata`, widen `::info`.
3. **Survey + migrate callers** in one commit:
   - `rg ":op\\b" src/ extensions/ test/` → migrate to `:op-symbol`.
   - `rg ":op-class" src/ extensions/ test/` → migrate to `:op-tag`.
   - `rg ":rendering-kind" src/ extensions/ test/` → migrate to
     `:op-system-kind` reads via the shim helper.
   - `rg ":vis/(answer|tool|system|diagnostic|error|sci)" src/ extensions/ test/`
     → migrate per §3.3 table.
4. **Add `read-op-system-kind` shim** in `persistance_sqlite/core.clj`.
   Wire into the iteration-block read path. Update the write path to
   emit `:op-system-kind`.
5. **`./verify.sh`** full pass.
6. **Add CHANGELOG note** about the shim's removal in the next
   release.
