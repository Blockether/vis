# Tasks — post per-block-eval pivot

State of the world after commits `8199c28d` + `f8e6797b` + `5480d7d0`:

- svar v0.5.3 (local) is the source of truth. `ask-code!` is strict on
  `:lang`, untagged Markdown code blocks are dropped, `:result` is gone,
  `:blocks` is the contract.
- vis no longer parses, lints, or repairs Clojure source before SCI eval.
  One svar Markdown code block = one code-entry = one `sci/eval-string+`
  call. The splitter / parinfer / quote-rebalance / answer-escape-rescue
  / parse-diagnose stack is deleted (~1300 lines).
- `:llm-executable-code` (concatenated source string) is dead everywhere
  — column, persistance, foundation extensions. `:llm-executable-blocks`
  (JSON vec of `{:lang :source}`) is the single source of truth.

What remains is below.

────────────────────────────────────────────────────────────────────────────────

## P0 — engine correctness

### P0.1 — Mandatory op-tag registry, fail-closed on unknown

**Problem.** `extension/op-tag` falls back to `:op.tag/observation` for
ops that never registered. Unknown ops silently become observations,
which under the upcoming P0.2 gate would mean unknown ops can answer
in the same iteration they fire. That's the wrong direction — unknown
should mean *invalid*, not *safe*.

**Fix.**
- Drop the `:op.tag/observation` default in `extension/op-tag`. Return
  `nil` (or throw `:ext.op/unregistered`) instead.
- Every `extension/register-op!` call site must declare `:tag`. Search
  the tree for `register-op!` invocations missing a tag; refuse to
  load extensions that omit it.
- Audit the existing `#{:op.tag/observation :op.tag/action}` set:
  rename `:op.tag/action` → `:op.tag/mutation` (matches the model's
  mental contract; "action" is overloaded with channel actions).
  Keep `:op.tag/action` as a deprecated alias for one release.
- Add a `clj-kondo` hook (or a doctor check) that scans every
  extension's manifest and fails the build when an op lacks `:tag`.

**Acceptance.** Loading an extension with an untagged op throws at
extension-install time, not at the first call site. Calling an
unregistered op from SCI surfaces `:ext.op/unregistered` as the
block's `:error.message` — model self-corrects.

────────────────────────────────────────────────────────────────────────────────

### P0.2 — ANY extension call blocks same-iteration answer

**Problem.** Today's final-answer gate (`answer-with-mutation-preflight`)
only refuses when *mutating* tool calls coexist with `(turn-answer! …)`.
Read-only tools (`v/cat`, `v/rg`, `z/locators`, web-search, …) are
allowed to fire in the same iteration as the answer. That breaks the
OODA separation: the model can see-and-summarize in one round, before
the harness has finalized the journal. Hard to audit, easy to game.

**Fix.** Stricter rule:

> `(turn-answer! …)` is accepted iff this iteration contains zero
> extension-tool calls of *any* tag. Mutation OR observation both block.
> Evidence must come from *prior* iterations' `<journal>`, never the
> same iteration.

**Detection — deterministic, NO regex.**

```clojure
(defn extension-call-form? [form]
  (boolean
    (some (fn [node]
            (and (seq? node)
              (symbol? (first node))
              (extension/registered-symbol? (first node))))
      (tree-seq coll? seq form))))

(defn iteration-has-extension-call? [code-entries]
  (boolean (some #(extension-call-form? (parsed-entry-form %))
             code-entries)))
```

**Gate composition.** Replace `answer-with-mutation-preflight-mismatch`
with:

```clojure
(defn final-answer-errors [ctx]
  (cond-> []
    (seq (:latest-iteration-errors ctx))
    (conj "latest iteration had errors — resolve before turn-answer!")

    (:extension-called-this-iteration? ctx)
    (conj "latest iteration called extension tool(s); answer in a later clean iteration")

    (not (:has-prior-evidence? ctx))
    (conj "<journal> contains no prior evidence for this turn yet — probe first")))
```

**Acceptance.**
- `(v/cat "README.md") (turn-answer! …)` in the same iteration → refused
  with the "wait for journal" message. Next iteration, `(turn-answer! …)`
  alone → accepted (prior evidence now in `<journal>`).
- `(turn-answer! …)` alone with no prior `<journal>` evidence → refused
  with the "no prior evidence" message (current behavior, retained).
- Mixed shape `(do (v/cat "README.md") (turn-answer! …))` inside a single
  SCI form → refused, NOT partially executed. The deterministic walker
  catches the embedded extension call.
- The block-level mutation/observation distinction stays (P0.1) for UI
  badges, audit logs, sandbox policies, future destructive-tool prompts
  — just not for the answer gate.

**Migration risk.** This is a behavior change for read-only QA turns
(`vis run "what's in README?"`). Today that's one iteration; under the
strict rule it becomes two. Document the tradeoff in the system prompt
so the model expects it.

────────────────────────────────────────────────────────────────────────────────

## P1 — rendering / channel surface

### P1.1 — Per-form silent rendering inside a mixed block

**Problem.** With per-block eval, one Markdown code block can contain
multiple top-level forms — including the answer-bearing call alongside
genuinely useful `(def …)` lines. The current streaming `:form-result`
chunk uses `form-contains-turn-answer-call?` to stamp `:silent? true`
on the WHOLE block. That hides the useful prelude too.

Confirmed by reproduction with prompt asking for all three forms in one
block:

```clojure
(def helper-x 1)                                          ;; useful, want to show
(set-conversation-title! "Mixed forms probe")             ;; hide; show as banner
(turn-answer! [:ir [:p "Mixed-block probe complete"]])    ;; hide; render IR below
```

A whole-block `:silent?` flag erases `(def helper-x 1)` from the user's
view.

**Fix — pure render-side, no engine change.** Parse the block source
with edamame at display time, render each top-level form independently:

- regular forms → show as code
- `(turn-answer! …)` → omit (answer prose renders below)
- `(set-conversation-title! …)` → render as `─ title set: "…" ─` banner
- `needs-input` calls → omit (input prompt renders separately)

This belongs in the **renderer**, not the engine. A new helper
`com.blockether.vis.internal.render.code-block-display` takes a block's
`:code` + `:result` and produces a render-ready vector of
`[{:kind :form :code "..."} {:kind :title :value "..."} {:kind :answer-ref} ...]`
segments. Channels (TUI / Telegram / transcript) consume that.

Also: the persisted block in `iteration.code_blocks` should NOT be the
place where this decision lives. Persist the full source verbatim;
make the renderer the decider.

**Acceptance.**
- Mixed-block reproduction: TUI shows `(def helper-x 1)` as a code line,
  the title set banner, and the answer IR — NO raw `(turn-answer! …)`
  call visible.
- Resume the same conversation from DB → identical rendering. (Currently
  resume shows the full source because the `:silent?` flag was a
  stream-only field.)
- Pure-answer block (just `(turn-answer! …)`) → renders only the answer
  IR with no surrounding code box.

### P1.2 — Stop writing `:comment` to persisted blocks

**Problem.** `:comment` was populated by the pre-Phase-B splitter as
the verbatim prose slice between top-level forms. Per-block eval doesn't
write it. The renderer branches in `prompt.clj` + `transcript.clj` are
already nil-safe, so the field is silently dead for new conversations.

**Fix.** Drop the `(cond-> … form-comment (assoc :comment …))` branch
from the block-construction in `run-iteration`. Keep the renderer's
read-side nil-safe branches as graceful degradation for old DB rows.

**Acceptance.** New iteration rows have no `:comment` key in the
Nippy blob. Old conversations open and render unchanged.

────────────────────────────────────────────────────────────────────────────────

## P2 — housekeeping

### P2.1 — Publish svar v0.5.3, flip vis to the published coord

Local svar at `/Users/fierycod/svar` has the BREAKING changes for
`ask-code!` (strict `:lang`, blocks-only return, drop `:result`,
Markdown-code-blocks prompt). Once it ships:

- `cd ../svar && bb release` (or whatever the publish command is) →
  Clojars `com.blockether/svar 0.5.3`.
- Flip `deps.edn` in vis: `{:mvn/version "0.5.3"}` instead of
  `{:local/root "/Users/fierycod/svar"}`.
- Drop the explanatory comment block above the dep line.
- Verify: `clojure -P` resolves cleanly, `clojure -M:test` 979/979,
  `bin/vis run "witam"` answers.

### P2.2 — Stale doc sweep

Files still referencing pre-pivot concepts (search and refresh prose):

- `docs/src/` mdBook — any chapter explaining how vis evaluates code.
  The "agent loop" / "iteration" / "form" wording is likely out of date.
- `ANALYSIS.md` §4.x — answer-after-error / answer-with-mutation. The
  rules have changed (P0.2 replaces the mutation-only gate).
- `PLAN.md` §2.6/§7.3.7 — the "per-form bounds" / `:start-row` /
  `:start-col` shape no longer exists; preflight builds entries from
  block sources, not parsed-form bounds.

────────────────────────────────────────────────────────────────────────────────

## P3 — future / nice-to-have

### P3.1 — Mandatory `:summary` metadata on op-registry entries

Once P0.1 forces every op to declare a tag, also force a one-line
`:summary` ("Read a file" / "Modify a file"). Powers:

- `vis doctor` extension listings
- channel badges / colors
- mutation confirmation prompts ("about to call v/patch: Modify a file
  — proceed?")
- future destructive-tool sandbox policies

### P3.2 — `<journal>` evidence introspection helper

Today the model has to infer "do I have prior evidence for the answer?"
from the `<journal>` block in its context. A SCI symbol like
`(v/journal-empty?)` / `(v/journal-keys)` would let the model
deterministically check before composing an answer, instead of
optimistically emitting `(turn-answer! …)` and getting refused.

### P3.3 — Faster live verification loop

The full test suite takes ~20-50s depending on multiprocess SQLite
contention. Most iterations only touch `loop.clj` + a couple of
extensions. A `make test-fast` target that skips the multiprocess
suite and the `--full` graal check would cut the inner loop from ~30s
to ~5s. Use `^:slow` or `^:integration` metadata + `lazytest` filter.

────────────────────────────────────────────────────────────────────────────────

## Open questions

1. **P0.2 migration.** Should we ship the strict gate behind a config
   flag (default ON) for one release to ease external integrations, or
   flip immediately? I lean immediate — the gate is a correctness win
   and external integrations would have to fix the same way anyway.

2. **P1.1 title banner.** When `(set-conversation-title! "X")` runs
   mid-iteration, should the renderer show "title set: X" inline in
   the trace, or only update the chrome (sidebar title)? Inline + chrome
   is more discoverable but doubles the visual weight.

3. **Same-form embedded extension call** —
   `(do (v/cat "x") (turn-answer! …))`. P0.2's deterministic walker
   catches this. The current SCI eval would partially execute (`v/cat`
   succeeds, side-effects logged) then refuse the answer. Acceptable, or
   should the walker reject the form *before* eval so the partial
   side-effect doesn't happen? I lean pre-eval rejection — same model
   experience as the cross-form case.
