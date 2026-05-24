# Trailer / def / context analysis

## Problem

Vis currently mixes three separate roles in one prompt-facing trailer:

1. Evidence for model: what was checked and what happened.
2. Result snapshot: full tool results (`v/rg`, `v/cat`, `git/status`, etc.).
3. Restore state: values captured by `(def ...)` for later history/session restore.

This makes small later turns expensive. Conversation `c8dc39b1-faf0-451e-85ad-8cd054067229` showed this clearly: short messages like `siemanko` carried ~58k input tokens because old tool results stayed in `:session/trailer`.

## Observed case

Session: `c8dc39b1-faf0-451e-85ad-8cd054067229`

Huge prompt source was not user text. It was rendered `;; ctx`, mostly `:session/trailer`.

Before turn 5:

- `;; ctx`: ~103k chars
- `:session/trailer`: ~101.8k chars
- biggest pins:
  - `t1/i2`: ~26.7k chars, broad `(v/rg ...)`, `167` files
  - `t1/i3`: ~68.3k chars, `(v/cat ".../render_ir.clj")`, file size `42036`

Turn 3/4 `siema` had `iteration_count = 0` and burned no provider tokens. The large token row was turn 2 `git status` (`109720` input across 2 iters), caused by old trailer baggage.

## Why `rg` did not disappear

Current stale-prune only drops old observation-only trailer pins after later mutation.

`t1/i2` was not observation-only:

```clojure
(task-set! :vis.foundation/session-title {:status :done :proof "t1/i1/f1"})
(def ir-hits (v/rg {:any ["render-ir" "ir-render" ":ir " "defmethod render"]
                    :paths ["."]
                    :files-only? true}))
ir-hits
```

`def` is tagged `:mutation`, so pin becomes mixed/mutation and is kept. Also result appeared twice:

- result of `(def ir-hits ...)`
- result of `ir-hits`

## Why `v/cat` exploded

`block->envelope` intentionally derefs def results:

```clojure
(def ir-file (v/cat ".../render_ir.clj"))
```

Instead of rendering SCI var `#'sandbox/ir-file`, trailer renders bound value. This was done so model sees actual data immediately and does not waste another iter writing `ir-file`.

That helps interaction, but when bound value is a full file, trailer receives full file data. Then every later prompt carries it until model explicitly emits `:trailer-drop` or `:trailer-summarize`.

## Decision point

Do not teach model to avoid `def` globally. `def` is useful:

- names values for downstream forms
- supports multi-step reasoning
- feeds durable definition history
- enables restore without rerunning user-written exploration manually

Problem is not `def` itself. Problem is prompt trailer treating def value as full prompt payload.

## Proposed architecture

Split responsibilities:

### 1. DB = full truth

Persistence stores full data:

- `definition_state.value`: full snapshot for `def` values
- `session_turn_iteration.forms`: full per-form envelopes
- raw prompt/response/code columns for forensics

Restore should use DB snapshots for impure/tool values. Do not replay side-effectful expressions.

Safe-ish to replay:

- `defn`
- maybe `defmacro`
- small literal `def`
- pure deterministic transforms

Unsafe to replay:

- `v/cat` (file may change)
- `v/rg` (repo may change)
- `git/status` (workspace mutates)
- `git/commit!`, `git/push!` (side effects; never replay)
- network/provider calls

For `(def x (v/cat ...))`, restore from stored value, not from expression.

### 2. Trailer = prompt index, not storage

Prompt-facing trailer should render compact evidence and references, not full payloads.

Small results can remain inline. Large/tool results should become summaries/refs.

Example for `v/cat`:

```clojure
{:scope "t1/i3/f1"
 :tag :mutation
 :src "(def ir-file (v/cat \".../render_ir.clj\"))"
 :result-ref {:scope "t1/i3/f1"
              :kind :tool-result
              :op :v/cat
              :path "extensions/.../render_ir.clj"
              :size 42036
              :line-count 900
              :preview "...first N chars..."
              :full "use introspect-form"}}
```

Example for `v/rg`:

```clojure
{:scope "t1/i2/f2"
 :tag :mutation
 :src "(def ir-hits (v/rg ...))"
 :result-ref {:scope "t1/i2/f2"
              :kind :tool-result
              :op :v/rg
              :file-count 167
              :query ["render-ir" "ir-render" ":ir " "defmethod render"]
              :sample ["file1" "file2" "file3"]
              :truncated? true
              :full "use introspect-form"}}
```

### 3. Introspection retrieves full data

Model can request full stored result lazily:

- `(introspect-form "t1/i3/f1")`
- `(introspect-iter "t1/i3")`
- `(introspect-var 'ir-file)` / equivalent future helper

Important: introspection should read DB/full stored envelopes, not prompt-rendered summaries.

### 4. Proofs/validators must not depend on prompt rendering

`derive-progression`, hook-task validation, and proof scope lookup should use full form-results from stored ctx/DB, not rendered trailer text.

Prompt trailer may be lossy. Engine truth must remain lossless.

## Policy sketch

When building prompt trailer:

1. Keep `:src`, `:scope`, `:tag`, `:error` inline.
2. Inline `:result` only if small and safe.
3. If result exceeds char/token threshold, replace with `:result-ref` summary.
4. If result is known tool result, use op-aware summary (`:vis.op`).
5. Deduplicate same value shown by `(def x ...)` and immediate `x` observation in same iter.
6. Observation-only old pins may still be pruned, but mixed pins with giant results must also be compacted.

Suggested default thresholds:

- inline scalar: always
- inline map/vector: if rendered result <= ~2k chars
- per iter trailer budget: ~8k chars
- full ctx trailer budget: ~20k chars before forced compaction warning/task

## Safer model guidance

Model should still bind useful intermediate values with `def`, but should not rely on full values always appearing in prompt. If summary says `:full "use introspect-form"`, model should call introspection only when details are needed.

Avoid patterns that duplicate large data:

```clojure
(def x (v/rg ...))
x
```

The renderer should dedupe this automatically, but model guidance can still say: bind once, inspect shape/count/slices, not full value.

Prefer:

```clojure
(def hits (v/rg {...}))
(select-keys hits [:file-count :truncated?])
```

For files:

```clojure
(def f (v/read "path" {:offset 1 :limit 80}))
```

or targeted reads over full `v/cat` when possible.

## Main conclusion

Trailer should not be storage. Trailer should be a compact prompt index.

Storage belongs in SQLite. Restore belongs to DB snapshots. Full forensic access belongs to introspection. Prompt only needs enough evidence and handles to ask for more.
