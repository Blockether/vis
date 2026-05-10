# PLAN — `extension.clj` cleanup + iteration-block classification

Status: design-locked, pre-implementation.
Scope: hard removals + renames + one new namespace. **No new behaviour.**
Net change: a smaller, cleaner spec surface; one `case` shim in
persistence for one release.

---

## 1. HARD REMOVALS

### 1.1 From `extension.clj`

1. **`(s/def ::rendered string?)`** at line 54 — dead, no readers
   anywhere. **DELETE.**
2. **Duplicate `(s/def ::kind …)`** at lines 69 AND 690 (both
   `non-blank-string?`, both for extension-kind). **DELETE one**,
   keep the other in its file location.
3. **Field name `:op`** → renamed to `:op-symbol`. Spec `::op` →
   `::op-symbol`. **No fallback.** Anything reading `(:op info)`
   becomes `(:op-symbol info)` in the same commit.

### 1.2 From the iteration-block role taxonomy

Today's 6 values collapse to 4. Hard removals:

| current          | action                               | rationale                                                                                                                  |
|------------------|--------------------------------------|----------------------------------------------------------------------------------------------------------------------------|
| `:vis/answer`    | → `:answer`                          | keep, rename only                                                                                                          |
| `:vis/tool`      | → `:tool`                            | keep, rename only                                                                                                          |
| `:vis/system`    | → `:nudge`                           | system-emitted preflight rejections, convergence reminders, etc. are nudges. Renamed for accuracy                          |
| `:vis/diagnostic`| **MERGED INTO `:nudge`**             | diagnostics are a subtype of system nudges; one rendering path is enough                                                   |
| `:vis/error`     | **DELETED**                          | errors are orthogonal — `:success? false` already lives on tool-results. Any block of any kind can fail. The renderer dispatches on `:success?`, not on a separate `:error` kind |
| `:vis/sci`       | **MERGED INTO `:tool`**              | every SCI eval IS effectively a tool call; `:op-symbol` distinguishes which one (or none, for raw user code). One rendering path for both |
| **NEW**          | `:thinking`                          | model reasoning blocks lifted into the iteration-block stream so the renderer dispatches uniformly across thinking + tool + answer + nudge |

Final closed set: **`#{:answer :nudge :tool :thinking}`** (4 values).

### 1.3 From the field name `:rendering-kind`

Hard rename to `:op-system-kind` everywhere. **No fallback in source
code.** One read-side compat shim in `persistance_sqlite/core.clj` for
one release, because existing local DB blobs carry `:rendering-kind
<legacy-value>`:

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

**Six lines of fallback. Deleted in the next release after this one
ships.**

### 1.4 From the `op-classes` set

**No removals.** The 8 `:op/X` values stay. Only the field name
changes (`:op-class` → `:op-tag`).

---

## 2. File homes

### 2.1 `src/com/blockether/vis/internal/extension.clj`

| change                                                | line     | what                                                              |
|-------------------------------------------------------|----------|-------------------------------------------------------------------|
| RENAME `(s/def ::op …)` → `(s/def ::op-symbol …)`     | 56       |                                                                   |
| ADD `(s/def ::op-tag #{…})`                           | new      | the closed enum, formerly the bare `op-classes` set               |
| ADD `(s/def ::content-kind #{…})`                     | new      | content shape enum                                                |
| ADD `(s/def ::metadata …)`                            | new      | the metadata map spec                                             |
| UPDATE `(s/def ::info …)`                             | 85       | swap `::op` → `::op-symbol`; add `::metadata` to `:opt-un`        |
| DELETE `(s/def ::rendered string?)`                   | 54       | dead                                                              |
| DEDUPE `(s/def ::kind …)`                             | 69 vs 690| keep one                                                          |

### 2.2 `src/com/blockether/vis/internal/iter_block.clj` *(NEW)*

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

---

## 3. Out of scope

- New rendering behaviour. The renderer's existing `:success?`
  dispatch + `:op-system-kind` switch already covers every legacy
  `:vis/*` case after the collapses; no UI work in this PR.
- `op-classes` semantic changes. Set membership stays at 8.
- DB schema migration. The compat shim covers existing rows
  in-place; new rows write the new shape.
- Cross-channel renderer changes. Telegram / TUI / CLI render the
  collapsed taxonomy through the existing chokepoints unchanged.

---

## 4. Open items

None at plan time. Implementation order:

1. Add `iter_block.clj` (no callers yet).
2. `extension.clj` edits in §2.1 (delete dead spec, dedupe, rename
   `::op` → `::op-symbol`, add `::op-tag` / `::content-kind` /
   `::metadata`, widen `::info`).
3. Update every callsite reading `(:op info)` → `(:op-symbol info)`
   and `:rendering-kind` → `:op-system-kind` in the same commit.
4. Add the 6-line compat shim in `persistance_sqlite/core.clj`.
5. `./verify.sh` full pass.
