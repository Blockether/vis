# PLAN — Tool-Result `:metadata` + Iteration-Block Classification (HARD REWRITE)

**No backward compat in code.** One read-side compat shim in the SQLite persistence layer for ONE release (then deleted), because Nippy-frozen blobs in users' local `~/.vis/vis.mdb` carry the legacy field. Everything else: hard delete.

Reproductions in `test/com/blockether/vis/internal/loop_test.clj`.

---

## Headline change — `:metadata` is a NEW thing

The biggest concept introduced by this plan is the **`:metadata` map under `:info`** on every tool-result. It does not exist today. It carries the op-tag, content-kind, path, sha, size, truncation flag, preview, and (SCI-stamped) pointer/iter-bound. **Every tool author stamps it. Every consumer reads from it.**

Without `:metadata`, the bindings index can't tell which file a binding holds, the journal can't summarise without peeking into raw payloads, and the model can't see "I've already read this file." With `:metadata`, all three problems collapse to "read the metadata."

This is the *new thing*. The renames (`:op` → `:op-symbol`, `:rendering-kind` → `:op-system-kind`, `:op-class` → `:op-tag`) are cosmetic surface around it.

---

## Locked field names

### On every tool-result (in `:info`)

| field | type | what |
|---|---|---|
| `:op-symbol` | keyword | which symbol was called (`:v/cat`, `:v/patch`, …) — RENAMED from `:op` |
| `:metadata` | map (NEW) | content-shape + effect classification (see below) |

### Inside `:info :metadata` (NEW)

| field | type | required | what |
|---|---|---|---|
| `:op-tag` | enum | ✓ | `:op/read :op/search :op/edit :op/create :op/delete :op/move :op/shell :op/meta` |
| `:content-kind` | enum | ✓ | `:file :tree :search-hits :patch-result :exec-output :json :other` |
| `:path` | string | when filesystem-bound | absolute or repo-relative path |
| `:sha` | hex64 | when content is bytes-stable | sha-256 of canonical content |
| `:size` | map | optional | `{:lines N}` / `{:bytes N}` / `{:hits N}` / `{:files N}` |
| `:truncated?` | boolean | optional | true when payload was capped at the tool boundary |
| `:preview` | string ≤ 400 chars | optional | head/tail snippet, tool-author owned |
| `:pointer` | string | SCI-stamped | `def`-bound symbol name (e.g. `"f17"`) |
| `:iter-bound` | nat-int | SCI-stamped | iteration the binding was created in |
| `:extras` | map | optional | tool-specific overflow |

### On every iteration block (top-level, separate from tool-result)

| field | type | what |
|---|---|---|
| `:op-system-kind` | enum (NEW) | `:answer :nudge :tool :thinking` (4 values total — see hard removals below) |

---

## Locked enums

```clojure
;; In src/com/blockether/vis/internal/extension.clj
(s/def ::op-tag
  #{:op/read :op/search :op/edit :op/create
    :op/delete :op/move :op/shell :op/meta})

(s/def ::content-kind
  #{:file :tree :search-hits :patch-result
    :exec-output :json :other})

;; In src/com/blockether/vis/internal/iter_block.clj  (NEW file)
(s/def ::op-system-kind
  #{:answer :nudge :tool :thinking})
```

---

## HARD REMOVALS

### From `extension.clj`

1. **`(s/def ::rendered string?)`** at line 54 — dead, no readers anywhere. **DELETE.**
2. **Duplicate `(s/def ::kind …)`** at lines 69 AND 690 (same `non-blank-string?`, both for extension-kind). **DELETE one**, keep the other in its file location.
3. **Field name `:op`** → renamed to `:op-symbol`. Spec `::op` → `::op-symbol`. No fallback. Anything reading `(:op info)` becomes `(:op-symbol info)` in the same commit.

### From the iteration-block role taxonomy

Today's 6 values collapse to 4. Hard removals:

| current | action | rationale |
|---|---|---|
| `:vis/answer` | → `:answer` | keep, rename only |
| `:vis/tool` | → `:tool` | keep, rename only |
| `:vis/system` | → `:nudge` | system-emitted preflight rejections, convergence reminders, etc. are nudges. Renamed for accuracy |
| `:vis/diagnostic` | **MERGED INTO `:nudge`** | diagnostics are a subtype of system nudges; one rendering path is enough |
| `:vis/error` | **DELETED** | errors are orthogonal — `:success? false` already lives on tool-results. Any block of any kind can fail. The renderer dispatches on `:success?`, not on a separate `:error` kind |
| `:vis/sci` | **MERGED INTO `:tool`** | every SCI eval IS effectively a tool call; `:op-symbol` distinguishes which one (or none, for raw user code). One rendering path for both |
| **NEW** | `:thinking` | model reasoning blocks lifted into the iteration-block stream so the renderer dispatches uniformly across thinking + tool + answer + nudge |

Final closed set: **`#{:answer :nudge :tool :thinking}`** (4 values).

### From the field name `:rendering-kind`

Hard rename to `:op-system-kind` everywhere. No fallback in source code. **One read-side compat shim in `persistance_sqlite/core.clj`** for one release, because existing local DB blobs carry `:rendering-kind <legacy-value>`:

```clojure
(defn- read-op-system-kind [exec]
  ;; New rows: :op-system-kind. Legacy rows: :rendering-kind.
  ;; Translate legacy values into the new closed set.
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

Six lines of fallback. Deleted in the next release after this one ships.

### From the `op-classes` set

No removals. The 8 `:op/X` values stay. Only the field name changes (`:op-class` → `:op-tag`).

---

## File homes

### `src/com/blockether/vis/internal/extension.clj`

| change | line | what |
|---|---|---|
| RENAME `(s/def ::op …)` → `(s/def ::op-symbol …)` | 56 | |
| ADD `(s/def ::op-tag #{…})` | new | the closed enum, formerly the bare `op-classes` set |
| ADD `(s/def ::content-kind #{…})` | new | content shape enum |
| ADD `(s/def ::metadata …)` | new | the metadata map spec |
| UPDATE `(s/def ::info …)` | 85 | swap `::op` → `::op-symbol`; add `::metadata` to `:opt-un` |
| DELETE `(s/def ::rendered string?)` | 54 | dead |
| DEDUPE `(s/def ::kind …)` | 69 vs 690 | keep one |

### `src/com/blockether/vis/internal/iter_block.clj` *(NEW)*

```clojure
(ns com.blockether.vis.internal.iter-block
  "Spec for iteration-block-level classification fields. Iteration
   blocks are the units of an iteration trace: each one is the result
   of one assistant turn through the SCI loop.

   These fields live at the iteration-block top level, NOT inside
   tool-result `:info`. Tool-result fields live in `extension.clj`."
  (:require [clojure.spec.alpha :as s]))

(s/def ::op-system-kind
  #{:answer :nudge :tool :thinking})

(s/def ::iter-block
  (s/keys :req-un [::op-system-kind]))
```

40 lines including a smoke test in `iter_block_test.clj`.

### `src/com/blockether/vis/internal/loop.clj`

| line | change |
|---|---|
| 1506-1518 | `eval-rendering-kind` → rename `eval-op-system-kind`; collapse 6 values to 4 per the merge table |
| 1972, 1997, 2017, 2091 | 4 writer sites: `:rendering-kind` → `:op-system-kind` |

### `src/com/blockether/vis/internal/presentation.clj`

Lines 122, 128, 137 — `:rendering-kind` → `:op-system-kind` in `select-keys`.

### `src/com/blockether/vis/internal/env.clj`

| line | change |
|---|---|
| 736-740 | bindings render: read from `:info :metadata` (use `:op-tag`, `:content-kind`, `:path`, `:sha`, `:size`, `:pointer`) |
| (new) | SCI eval hook: when `(def f17 (some-tool …))` lands a tool-result, stamp `[:info :metadata :pointer]` and `[:info :metadata :iter-bound]` on the value |

### `src/com/blockether/vis/internal/progress.clj`

Line 109 — read `:hit-count` / `:truncated-by` from `[:info :metadata]` instead of result top-level.

### `extensions/persistance/vis-persistance-sqlite/src/.../core.clj`

| line | change |
|---|---|
| 968-988 | `normalize-rendering-kind` → `normalize-op-system-kind`; ADD `read-op-system-kind` tolerant-read shim (legacy compat for one release) |
| (new) | write `:op-system-kind` only; never write the old key |

### `extensions/common/vis-foundation/src/.../transcript.clj`

Line 382 — `:rendering-kind` → `:op-system-kind`.

### `extensions/common/vis-foundation/src/.../editing/core.clj`

Producer side: every tool stamps `:metadata` into `:info`. Concrete sites for `v/cat`, `v/rg`, `v/ls`, `v/glob`, `v/exists?`, `v/patch`, `v/write`, `v/append`, `v/bash`, `v/create-dirs`, `v/delete`, `v/move`, `v/copy`. Same commit pulls `:total-lines` / `:truncated-by` / `:offset` / `:hit-count` OUT of `:result` — `:result` becomes pure payload (e.g. just `:lines` for `v/cat`, just `:hits` for `v/rg`).

### `extensions/languages/clojure/src/.../core.clj`

Producer side for `z/locators`, `z/symbols`, `z/locator-for-symbol`, `z/patch`, `z/patch-check`, `z/repair-range`, `z/repair-locator`, `z/repair-file`. Same shape as foundation tools.

### Tests

| file | what |
|---|---|
| `test/com/blockether/vis/internal/extension_test.clj` | spec roundtrip for new `::metadata`, `::op-tag`, `::content-kind`, renamed `::op-symbol` |
| `test/com/blockether/vis/internal/iter_block_test.clj` *(NEW)* | spec smoke for `::op-system-kind` |
| `test/com/blockether/vis/internal/loop_test.clj` | flip existing repros (`REPRO-tool-result-metadata-shape-target`, `REPRO-bindings-tool-result-collapse-misses-file-pointer`) to assert the new shape |
| `test/com/blockether/vis/internal/prompt_test.clj` | 1 hit, mechanical |
| `test/com/blockether/vis/internal/progress_test.clj` | 1 hit, mechanical |
| `extensions/persistance/.../persistance_sqlite_test.clj` | NEW: legacy-blob tolerant-read regression |
| `docs/src/extensions/spec.md` | doc update |

---

## Step list (ordered, file-bounded)

| # | scope | files | est lines |
|---|---|---|---|
| 1 | **Spec only** — new `::metadata`, `::op-tag`, `::content-kind`, `::op-symbol`; new `iter_block.clj` with `::op-system-kind`; delete dead specs; spec tests | `extension.clj`, `extension_test.clj`, NEW `iter_block.clj` + test | ~120 |
| 2 | **Producer cleanup** — every tool stamps `:metadata`; pull fields OUT of `:result` | `editing/core.clj`, `lang-clojure/.../core.clj` (~12 sites) | ~150 |
| 3 | **Reader cleanup** — every site reads from `:metadata`; v/cat + v/rg docstrings rewritten | `env.clj`, `progress.clj`, `prompt.clj`, journal renderers | ~30 |
| 4 | **SCI runtime stamping** — `:pointer` + `:iter-bound` post-eval hook | `env.clj` SCI hook | ~40 |
| 5 | **Iteration-block rename** — `:rendering-kind` → `:op-system-kind`; collapse 6 values to 4; persistence tolerant-read shim | `loop.clj`, `presentation.clj`, `transcript.clj`, `persistance_sqlite/core.clj`, 3 tests | ~80 |

**Dependencies:** Step 2 + Step 3 must land in the same commit (producer change makes readers obsolete). Steps 1, 4, 5 are independent.

**Recommended landing order:** 1 → 5 → 4 → (2+3 together).

Step 1 first because it's pure spec and unblocks the rest.
Step 5 second because it's the highest-risk (touches persistence) and we want it baked in CI first.
Step 4 third because SCI stamping is independent and small.
Steps 2+3 together last because they're the largest mechanical change and depend on Step 1's spec being live.

---

## Reproductions to flip (already landed in `loop_test.clj`)

| repro | today | post-impl |
|---|---|---|
| `REPRO-tool-result-metadata-shape-target` "TODAY: v/cat result has no :info :metadata field" | `(nil? metadata)` | DELETE |
| `REPRO-tool-result-metadata-shape-target` "PROPOSED" | static literal | call actual builder; assert |
| `REPRO-bindings-tool-result-collapse-misses-file-pointer` | `(expect true)` | assert env.clj produces full metadata view |
| `REPRO-replay-reasoning-chars-provider-incoherence` (4 cases) | unchanged (Z.ai truncation OUT of scope) |
| `REPRO-zai-preserved-thinking-budget-too-generous` (2 cases) | unchanged |
| `REPRO-vcat-journal-redundant-reads` (2 cases) | unchanged (per-turn dedup OUT of scope) |
| `REPRO-zai-has-no-thinking-budget-knob` (2 cases) | unchanged |
| `REPRO-zai-head-tail-truncation-policy` (4 cases) | unchanged |

NEW repro to add in Step 5: `REPRO-op-system-kind-rename-target` — pin the legacy→new value mapping in the persistence shim.

---

## What is OUT of scope, explicitly

- Per-turn `files-read-atom` + journal dedup of repeat reads.
- Z.ai head/tail preserved-thinking truncation.
- `:render/as` field — fenced renderer covers it.
- `:op/proof` — no real distinction from `:op/meta` worth a split.
- Server-side conversation compaction.
- Any new SQLite columns.
- Any change to `op-classes` set membership (only the field name `:op-class` → `:op-tag` changes).

---

## Decisions locked

| # | decision | choice |
|---|---|---|
| D1 | preserve-thinking truncation | OUT |
| D2 | per-turn files-read-atom dedup | OUT |
| D3 | `:render/as` rendering hint | OUT (fenced renderer covers it) |
| D4 | `:op/proof` split | OUT (no real distinction from `:op/meta`) |
| D5 | `:op-tag` field required in `:metadata` | YES (producer stamps from `op-class-of`) |
| D6 | dead `(s/def ::rendered)` | DELETE |
| D7 | duplicate `::kind` | DEDUPE — pick one location |
| D8 | block role field name | `:op-system-kind` (renamed from `:rendering-kind`) |
| D9 | block role value set | `#{:answer :nudge :tool :thinking}` (4 values; collapsed from 6) |
| D10 | block role legacy values | `:vis/diagnostic` → `:nudge`; `:vis/sci` → `:tool`; `:vis/error` → DELETED (use `:success?`); `:vis/system` → `:nudge` |
| D11 | tool-result symbol field name | `:op-symbol` (renamed from `:op`) |
| D12 | tool-result effect class field name | `:op-tag` (renamed from `:op-class`) |
| D13 | tool-result content kind field name | `:content-kind` spec, `:kind` map key |
| D14 | persistence backward compat | tolerant-read shim for ONE release in `persistance_sqlite/core.clj`, then delete |
| D15 | iteration-block spec home | NEW file `src/com/blockether/vis/internal/iter_block.clj` |

---

## Ready to start

Step 1 unblocks everything else. ~120 lines, isolated to spec + new file. Safe first commit.
