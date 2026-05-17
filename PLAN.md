# PLAN — Full stdout/stderr Drop from Model

Status: in-progress. No backward compat. No legacy. No migrations.

## Why

SCI sandbox currently captures `*out*`/`*err*` per form, stores them in
`conversation_turn_iteration.stdout`/`stderr`, renders them in transcript +
TUI, and lets the model see them. Two problems:

1. stdout is an ambient I/O channel — invisible state, hard to test.
2. Model uses `println` as a "log" instead of returning data, which the
   transcript already renders.

Decision: rip the entire stdout/stderr channel out of model-facing surfaces.
Returned data is the only signal. Subprocess streams (shell, doctor probes,
HTTP) remain as **named result fields** on the tool's own return map — that
is data, not the killed iteration-level `:stdout`/`:stderr` channel.

## Non-goals

- No migration code. Old DBs must be wiped (`rm -f ~/.vis/vis.db*`).
- No legacy mention in docstrings, comments, or prompt copy.
- No `#p` replacement. Macro is deleted entirely.
- CLI adapter prints (`main.clj` `stdout!`, `commandline.clj`, doctor
  `original-stdout`) are **out of scope**. That is the OS terminal, not
  the model.

## Gate definition

Each phase ends with a gate. A gate is a REPL probe or `verify.sh` invocation
that must return the documented success shape before the next phase starts.
A failing gate blocks. Do not soften gates.

REPL gate command shape:
```bash
clj-nrepl-eval -p 7888 "<expr>"
```
nREPL port discovered via `clj-nrepl-eval --discover-ports` (currently 7888).

---

## Phase 0 — Baseline

Pre-flight sanity before any edits.

### Actions
- Confirm clean tree (already committed as `62ebc256 WIP: pre-stdout-ban snapshot`).
- Confirm nREPL alive on 7888.

### Gate 0
```bash
clj-nrepl-eval -p 7888 "(+ 1 2)"
# expect: => 3
```

---

## Phase 1 — Schema (single source of truth)

### File
- `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`

### Actions
- Drop column `stdout TEXT,` (line ~256).
- Drop column `stderr TEXT,` (line ~257).
- Strip header-comment lines mentioning stdout/stderr (lines 12, 36).
- Tighten inline comment above `code TEXT NOT NULL,` — remove the "text
  side effects stay queryable as TEXT" sentence.

### Gate 1
```bash
grep -n "stdout\|stderr" \
  extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql
# expect: no matches
```

---

## Phase 2 — Persistence layer

### Files
- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj`
- `extensions/persistance/vis-persistance-sqlite/test/com/blockether/vis/ext/persistance_sqlite/core_test.clj`

### Actions
- `insert-iteration!` (~1019): remove `stdout stderr` from destructure
  and the two `cond-> assoc` branches.
- Strip outdated comment (~1089) that names them as flat callee fields.
- `row->iteration` (~1302-1303): drop both `(not (str/blank? ...))`
  branches.
- Test: drop `:stdout :stderr` from `select-keys` (~720).
- Test: **delete** entire `it "errors carry the message + stdout + stderr in the BLOB"`
  block (~780-796).

### Gate 2
```bash
# Wipe stale DB so the new schema is the only schema.
rm -f ~/.vis/vis.db ~/.vis/vis.db-shm ~/.vis/vis.db-wal ~/.vis/vis.mdb
```
```bash
grep -n "stdout\|stderr" \
  extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj
# expect: no matches
```
```bash
clj-nrepl-eval -p 7888 \
  "(require '[com.blockether.vis.ext.persistance_sqlite.core :as p] :reload) :ok"
# expect: => :ok (no compile errors)
```

---

## Phase 3 — SCI sandbox bindings (kill the producer surface)

### File
- `src/com/blockether/vis/internal/env.clj`

### Actions
- Delete `sandbox-println` defn (~153-162).
- Delete `'println sandbox-println` from base-bindings (~210).
- Remove from sandbox bindings any I/O side-effect symbols:
  `println`, `print`, `pr`, `prn`, `printf`, `pprint`, `tap>`,
  `flush`, `newline`, `*out*`, `*err*`, `*flush-on-newline*`.
- Delete `#p` reader macro entry (~299). No replacement.
- Strip outdated comment block mentioning `:stdout`/`:stderr` (~466).

### Gate 3
```bash
clj-nrepl-eval -p 7888 \
  "(require '[com.blockether.vis.internal.env :as env] :reload) :ok"
# expect: => :ok
```
```bash
clj-nrepl-eval -p 7888 \
  "(require '[com.blockether.vis.internal.env :as env] :reload)
   (require '[sci.core :as sci])
   (let [ctx (env/create-sci-context nil)]
     (try (sci/eval-string* ctx \"(println :hi)\")
          {:ok :unexpected}
          (catch Throwable t {:err (ex-message t)})))"
# expect: :err string containing "Unable to resolve" or "println"
```

---

## Phase 4 — Eval pipeline (`run-sci-code` / `run-iteration`)

### File
- `src/com/blockether/vis/internal/loop.clj`

### Actions in `run-sci-code` (~547-779)
- Delete `stdout-writer` / `stderr-writer` / `err-pw` StringWriter trio.
- Remove `pre-out`/`pre-err` offset captures and substring slicing.
- Strip `:stdout`/`:stderr` assoc from `eval-one-form` success + catch
  branches.
- Remove the `(sci/binding [sci/out ... sci/err ...])` wrap from the
  exec-future. (SCI's defaults are fine — model has no `println`
  binding anyway.)
- Strip `:stdout`/`:stderr` from the outcome assoc after eval.
- Strip `:stdout ""`/`:stderr ""` from every synthetic fallback map
  (timeout, exception, parse-failure, future-cancel).
- Drop the `.close` writer calls.
- **Compiler-warnings handling**: SCI normally surfaces analyzer errors
  via thrown exceptions, not `*err*` writes. Confirm via REPL probe
  (gate). If anything still slips into `*err*`, fold it into
  `:error.message` — never expose it on a separate channel.

### Actions in `run-iteration` (~882-…)
- Drop `:stdout (:stdout result*)` and `:stderr (:stderr result*)`
  from both `:phase :form-result` `on-chunk` payloads (the live emit
  and the post-validation re-emit).
- Drop `:stdout (:stdout result)` / `:stderr (:stderr result)` from
  the `blocks` cond-> map.
- Drop `:stdout "" :stderr ""` from the validation-error synthetic
  block.
- Drop `:stdout ""`/`:stderr ""` from `raw-result` synthetic maps in
  the preflight/parse-error branches (~1764).
- Remove `s/def ::stdout` `s/def ::stderr` (~1310-1311).
- Drop `::stdout` `::stderr` from `s/keys :req-un` (~1328).

### Gate 4
```bash
grep -n "stdout\|stderr" src/com/blockether/vis/internal/loop.clj
# expect: no matches
```
```bash
clj-nrepl-eval -p 7888 \
  "(require '[com.blockether.vis.internal.loop :as l] :reload) :ok"
# expect: => :ok
```
```bash
clj-nrepl-eval -p 7888 \
  "(require '[com.blockether.vis.internal.loop :as l] :reload)
   (->> (clojure.repl/source-fn 'com.blockether.vis.internal.loop/run-sci-code)
        (re-find #\"stdout|stderr\"))"
# expect: => nil
```

---

## Phase 5 — Transcript + introspection (model-facing rendering)

### Files
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/transcript.clj`
- `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/transcript_test.clj`
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/introspection.clj`

### Actions — transcript
- Strip `:stdout :stderr` from key picks (~60).
- Drop both `contains?` branches that re-emit them on the iteration
  shape (~117-118).
- Drop the six `:stdout-chars`/`:stderr-chars`/`:stdout-preview`/
  `:stderr-preview`/`:stdout-truncated?`/`:stderr-truncated?` branches
  (~269-274).
- Drop `:stdout-preview` / `:stderr-preview` shaping (~398-399).
- Drop `stdout`/`stderr` destructure params and the two `_stdout:_`/
  `_stderr:_` fenced-block render branches (~568-594).
- Rewrite docstrings (~9, ~568) to remove all stdout/stderr mention.

### Actions — transcript test
- Strip stdout/stderr fixture keys + asserts (~15 sites).
- **Delete** `it "stdout / stderr captured under fenced text blocks"` (~464).

### Actions — introspection
- Drop `:stdout (:stdout iteration)` from `vis/introspect` row shape
  (line 112).

### Gate 5
```bash
grep -rn "stdout\|stderr" \
  extensions/common/vis-foundation/src \
  extensions/common/vis-foundation/test
# expect: no matches
```
```bash
clj-nrepl-eval -p 7888 \
  "(require '[com.blockether.vis.ext.foundation.transcript :as tr] :reload) :ok"
# expect: => :ok
```

---

## Phase 6 — TUI primitives + theme

### Files
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/primitives.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/theme.clj`

### Actions
- primitives: delete `MARKER_STDOUT`, `MARKER_STDERR`, `MARKER_STDOUT_SEP`,
  `MARKER_STDOUT_PAD` defs (~760-769).
- theme: delete `stdout-bg`, `stdout-fg`, `stdout-label-fg`,
  `stdout-sep-fg` defs (~103-106).
- theme: delete the four matching entries from the theme map (~212-215).
- theme: audit defaults block for any leftover keys.

### Gate 6
```bash
grep -n "stdout\|stderr\|STDOUT\|STDERR" \
  extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/primitives.clj \
  extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/theme.clj
# expect: no matches
```

---

## Phase 7 — TUI render + chat

### Files
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj`

### Actions — render
- Delete marker private defs `stdout-marker`, `stderr-marker`,
  `stdout-sep-marker`, `stdout-pad-marker` (~950-960).
- Drop them from the styled-marker set (~991) and marker-zones vec
  (~1008).
- Delete the four cond branches in the line painter (Stdout text /
  stderr / sep / pad) (~1885-1907).
- Op-fingerprint: drop `:stdouts`/`:stderrs` from destructure +
  drop the two `mapv text-fingerprint` calls (~2452-2477).
- Strip stdout/stderr mentions from every docstring in this file
  (~100, 984, 1526, 1557, 1662, 1670, 1758, 2471, 2626, 2684, 2858).

### Actions — chat
- Drop `(contains? it :stdout)`/`:stderr` propagation (~198-199).
- Drop `:stdout`/`:stderr` payload assoc branches (~277-280).
- Delete `stdout-strs` / `stderr-strs` lets and `:stdouts` / `:stderrs`
  keys (~314-333).

### Gate 7
```bash
grep -n "stdout\|stderr" \
  extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj \
  extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj
# expect: no matches
```

---

## Phase 8 — TUI tests

### Files
- `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/render_test.clj` (30 hits)
- `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/virtual_test.clj` (4)
- `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/screen_test.clj` (2)
- `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/chat_test.clj` (1)
- `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/input_test.clj` (1)

### Actions
- Strip stdout/stderr fixture keys and asserts.
- Delete dedicated stdout/stderr render `it` blocks.

### Gate 8
```bash
grep -rn "stdout\|stderr" extensions/channels/vis-channel-tui/test
# expect: no matches
```

---

## Phase 9 — CLI run-trace renderer

### File
- `src/com/blockether/vis/internal/main.clj`

### Actions
- Delete `pretty-block "stdout"` line (~862).
- Delete `pretty-block "stderr"` line (~863).
- Leave `stdout!` / `write-stdout!` helpers and every other CLI print
  untouched — they write to the user's terminal, not the model.

### Gate 9
```bash
grep -n "pretty-block \"stdout\"\|pretty-block \"stderr\"" \
  src/com/blockether/vis/internal/main.clj
# expect: no matches
```
```bash
grep -n ":stdout (:stdout chunk)\|:stderr (:stderr chunk)" \
  src/com/blockether/vis/internal/main.clj
# expect: no matches
```

---

## Phase 10 — Prompt contract

### File
- `src/com/blockether/vis/internal/prompt.clj`

### Actions
- Add to banned-heads list: `println`, `print`, `prn`, `pr`, `printf`,
  `pprint`, `tap>`, `flush`, `newline`.
- Add one line in the answer-shape section: "Return data. No I/O side
  effects. No stdout."
- Strip any pre-existing mention of stdout/stderr from the engine
  contract (engine should never have named them, but verify).

### Gate 10
```bash
grep -n "stdout\|stderr" src/com/blockether/vis/internal/prompt.clj
# expect: no matches
```

---

## Phase 11 — Extension audit (one-hit files)

### Files (1 hit each unless noted)
- `extensions/providers/vis-provider-github-copilot/.../provider_github_copilot.clj`
- `extensions/providers/vis-provider-zai/.../provider_zai.clj`
- `extensions/channels/vis-channel-telegram/.../channel_telegram/bot.clj` (2)
- `extensions/common/vis-voice/.../voice/core.clj`
- `extensions/common/vis-foundation/.../foundation/introspection.clj`
  (already handled in Phase 5 — verify)
- `extensions/common/vis-foundation/test/.../editing/core_test.clj`

### Actions
For each hit decide:
- **Subprocess result field** (`{:out … :err …}` on a tool's own return
  map describing a real OS process): keep, rename to `:out`/`:err` if it
  currently leaks the killed `:stdout`/`:stderr` keys. This is data, not
  the model channel.
- **Iteration-block rendering or propagation**: rip.
- **CLI adapter print to original-stdout**: keep.

### Gate 11
```bash
grep -rn "\\bstdout\\b\\|\\bstderr\\b" \
  src/ extensions/ \
  --include='*.clj' --include='*.cljc' --include='*.cljs' \
| grep -v "original-stdout\|stdout!\|write-stdout!\|System/out\|System/err\|/log\|sandbox-println"
# expect: only intentional subprocess `:out`/`:err` data fields (audit
# the remaining output by hand and confirm each one is data, not channel)
```

---

## Phase 12 — Full verify

### Actions
- `./verify.sh` (full, not `--quick`).
- TUI smoke: `clj-nrepl-eval -p 7888 "(dev/tui!)"` — drive one prompt
  through, confirm no stdout/stderr lanes render.

### Gate 12
- `./verify.sh` exits 0.
- TUI session shows code + result blocks only. No "stdout" / "stderr"
  labels anywhere.
- One model iteration completes (model writes `(done "...")`),
  iteration row in DB has no stdout/stderr columns referenced.

---

## Rollback policy

Each phase commits on success. If a gate fails:

1. Diagnose the specific failure.
2. Fix forward when fix is localised.
3. `git reset --hard HEAD~1` to drop the phase only when the surface
   diverged from the plan. Never carry partial state across phases.

No compatibility shims. No "legacy" fallbacks. No commented-out blocks
left as "for reference".

## DB wipe instruction (release note copy)

```
BREAKING: stdout/stderr removed from iteration schema.
Wipe local Vis state before first run:

  rm -f ~/.vis/vis.db ~/.vis/vis.db-shm ~/.vis/vis.db-wal ~/.vis/vis.mdb
```
