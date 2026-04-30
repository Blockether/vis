# ACTIONS — Fixing what `REPRODUCTION.md` exposed

Conversation: `eeaf9651-06c7-4dda-9e97-877fcef06337`
Source doc: `REPRODUCTION.md` (incomplete — see Action 0)
Owner: TBD

This file is a **work list, not a narrative**. Every action has: what,
why, where, how, verify. Order = priority. P0 = the thing that bit us
this conversation; P3 = nice-to-have.

---

## P0 — Iteration loop must detect "code block is English prose"

### What
When an iteration emits N consecutive `:unresolved-symbol` errors whose
symbol names are dictionary words / pronouns / articles, surface ONE
hint: *"this code block is prose; move it to `(answer …)` or rewrite as
Clojure"*. Suppress the per-token error spam.

### Why
Iteration `7` of turn `7a4fa411` shipped 28 unresolved-symbol errors for
`Let me dig deeper what …`. The current loop relays each token as an
individual symbol error. The model has no signal that the block was
*entirely* prose; it patches one symbol at a time and re-emits the same
sentence. Three of the doc-writing turns (#39, #40, #41 in the failure
log) failed with the same family — `Invalid symbol: with:` /
`fixes:` / `failures:` — words ending in `:`.

### Where
- `src/com/blockether/vis/internal/parse_diagnose.clj` — add
  `detect-prose-block` alongside `try-quote-rebalance`.
- `src/com/blockether/vis/internal/loop.clj` — call the detector in the
  same path that fires `bare-string-code-block?` (line 384) /
  `comment-only-block?`, before per-form SCI eval.

### How
1. Heuristic: count *top-level* unresolved-symbol errors in a single
   block; if `>= 3` AND `>= 60 %` of those symbols match
   `#"^[A-Za-z][A-Za-z']*[.,!?:;]?$"` (lowercase or capitalised English
   words, optionally trailing punctuation), classify as prose.
2. Replace per-symbol errors with one block-level error:
   `"Code block parsed as English prose, not Clojure (N tokens: 'Let' 'me' 'dig' …). Move text to (answer …) or rewrite as Clojure."`
3. Surface the original code unchanged so the model can see what it
   sent.

### Verify
- `test/com/blockether/vis/internal/parse_diagnose_test.clj` — pin a new
  reproducer with the literal block from turn `7a4fa411` iteration 7.
- Assertion: `detect-prose-block` returns truthy; the loop emits a
  single `:prose-in-code` error, not 28 unresolved-symbol errors.
- Run `./verify.sh` — green.

---

## P0 — Strip `REPRODUCTION.md` of misdiagnoses, regenerate from DB

### What
Re-emit `REPRODUCTION.md` from a real introspection query. The current
file has:

- **Turn-level summary slots empty** (`Status:` `Attempts:` `Cost:`
  `Failures:` `Errors:` blank, `Iterations: 0` everywhere). DB ground
  truth: 16 / 8 / 3 / 2 / 2 / 1 / 1 / 1 iterations across 8 turns,
  statuses `done` / `running` / `interrupted`.
- **Failure 5 ("v/rg Arity Mismatch — Single File vs Directory") is
  wrong.** `grep-files` in
  `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj:288`
  walks `(.isFile f) [f]`; passing a single file works. Row #38's
  arity-1 exception came from a malformed call (the prose-in-code
  bug from P0 again — call truncated mid-arg).
- **Cost field permanently blank** — schema has no per-iteration cost
  column.

### Why
The file is the canonical "we know our agent fails like X" artefact. It
ships wrong root-cause analysis (Failure 5) and a half-blank rollup. If
we keep it as-is, future agents will copy the wrong patterns into their
own retros.

### Where
- `REPRODUCTION.md` — delete or overwrite.
- New: `src/com/blockether/vis/internal/diagnose/reproduction.clj` (or
  extend `parse_diagnose.clj` if we keep the package thin) — a callable
  generator: `(reproduction/render conv-soul-id) -> markdown-string`.

### How
Single SQL pass per conversation:

```clojure
(jdbc/execute! ds
  (sql/format
    {:select [:qs.id :qs.query :qs.created_at
              [[:count :qst.id] :attempts]
              [[:max :qst.status] :last_status]
              [[:max :qst.prior_outcome] :last_outcome]]
     :from [[:query_soul :qs]]
     :join [[:query_state :qst] [:= :qst.query_soul_id :qs.id]]
     :where [:= :qs.conversation_state_id conv-state-id]
     :group-by [:qs.id]
     :order-by [[:qs.created_at :asc]]})
  jdbc-opts)
```

Then per `query_state`, pull `iteration` rows + decode `iteration.blocks`
(Nippy) for the per-block error log. Drop **Cost** until P3 lands.

### Verify
- `test/.../diagnose/reproduction_test.clj` — render a fixture
  conversation, snapshot match.
- Manual: regen `REPRODUCTION.md` for `eeaf9651-…`, every per-turn slot
  populated, "Failure 5" replaced with the actual root cause (prose-in-
  code truncated the call site).

---

## P1 — `vis diagnose <conv-id>` CLI subcommand

### What
Wrap the P0 generator behind a CLI verb so we don't rely on an agent
session to emit diagnostics. `bin/vis diagnose eeaf9651-… > out.md`.

### Why
Agent-authored diagnostics fail under the same pathologies they're
trying to document (this file is the proof). A pure CLI command is
deterministic, auditable, and runnable from CI.

### Where
- `src/com/blockether/vis/internal/main.clj` — register subcommand.
- `src/com/blockether/vis/internal/commandline.clj` — argument spec.
- Generator from P0.

### Verify
- `./verify.sh` smoke step: extend `bin/vis` smoke to also accept
  `--help` listing `diagnose`.
- Unit: golden-file test against a seeded SQLite fixture in
  `extensions/persistance/vis-persistance-sqlite/test/`.

---

## P1 — Post-edit parse verifier on every `v/edit` / `v/zedit`

### What
Every successful `v/edit` / `v/zedit` re-parses the affected file with
edamame; if parse fails, the tool returns
`{:edited? true :parsed? false :parse-error "…line:col"}` instead of
silently succeeding. A second auto-repair attempt may try the parinfer-
style quote rebalance from `parse_diagnose.clj`.

### Why
Failure 3 in `REPRODUCTION.md` (chained edits → unmatched delimiter) is
real and recurring: an `edit-1` ends with `]`, `edit-2` matches the
already-edited line and creates `]]`. The agent only finds out three
iterations later when the next `z/zedit` blows up.

### Where
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj`
  — `edit-file` (line 382). After write, `(edamame/parse-string-all (slurp …))`.
- Same file — `write-file` (line 340) gets the same treatment for `.clj`
  / `.cljc` / `.cljs` paths.
- Returned map gains `:parsed? boolean` and `:parse-error` fields. Old
  callers stay compatible (extra keys ignored).

### Verify
- New cases in
  `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/editing/core_test.clj`:
  - edit that breaks parsing → `:parsed? false`, error message includes
    line/col.
  - edit on `.md` / `.txt` → `:parsed?` absent (we only check Clojure
    files).

---

## P1 — Per-iteration cost column, or drop the field

### What
Pick one:

- **(a)** Add `iteration.llm_cost_usd REAL` (and `llm_input_tokens`,
  `llm_output_tokens`, `llm_cached_tokens`) to a new `V2__cost.sql`
  migration. Populate from provider response in the iteration loop.
- **(b)** Officially drop `Cost:` from any future reproduction
  generator. Note in `docs/src/` that cost lives in `log` events only.

### Why
The empty `Cost:` line in `REPRODUCTION.md` is structural, not a bug
in the agent. Schema inspection
(`extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`)
shows zero token / cost columns on `iteration`.

### Where
- Schema: `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V2__cost.sql` (option a).
- Iteration loop: `src/com/blockether/vis/internal/loop.clj` —
  whichever call wraps `svar/ask-code!`. svar's response should already
  carry usage; surface it.
- Doc: `docs/src/architecture/persistence.md` (or nearest equivalent).

### Verify
- Option (a): a new test in
  `extensions/persistance/vis-persistance-sqlite/test/.../core_test.clj`
  inserts an iteration with cost, reads it back, expects round-trip.
- `./verify.sh` green; `--graal` ratchet not regressed.

---

## P2 — System-prompt nudge: "discover before assuming paths"

### What
One paragraph in the agent's system prompt + a tiny `(v/project-layout)`
helper that returns
`{:src-roots [...] :extension-roots [...] :test-roots [...]}` from the
deps.edn aliases.

### Why
Failure 1 in `REPRODUCTION.md` (turn `7a4fa411`, iters 2 + 4 + 4) — the
agent guessed `src/com/blockether/vis/tui` because of naming convention.
The actual TUI lives under `extensions/channels/vis-channel-tui/src/…`.
The polylith-style layout is non-obvious from the project name alone.

### Where
- System prompt assembly: `src/com/blockether/vis/internal/prompt.clj`.
- Helper: `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/introspection.clj` (already exists,
  extend).

### Verify
- Snapshot test on the rendered system prompt: contains the layout
  preamble.
- Smoke: a new SCI test calls `(v/project-layout)` and asserts the four
  expected roots are present.

---

## P2 — `v/edit` "incomplete prune" guard rail

### What
After `v/edit` deletes a region, expose
`(v/orphans <removed-symbol-or-substring> <root>)` returning every
remaining hit grouped by file. Agent prompt example:

```clojure
(v/edit "footer.clj" "old-segment-code" "")
(v/orphans "ctx-left" "extensions/channels/vis-channel-tui/src")
;; => {:hits [{:path "…/footer.clj" :line 65 :text "(defn- last-assistant-tokens"} …]}
```

### Why
Failure 4 in `REPRODUCTION.md` — feature ripped from one call site,
4 helper fns + 1 `:require` left behind. `v/rg` already does this; the
problem is the agent doesn't run it. Make it ergonomic and document it.

### Where
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj`
  — thin wrapper over `grep-files` with sensible defaults (project root,
  group by file).
- Mention in agent skill / system prompt as the canonical post-prune
  step.

### Verify
- Editing test pins the workflow.

---

## P3 — Skill: "code blocks contain only Clojure"

### What
A new skill under `.pi/skills/clojure-code-blocks/SKILL.md` that the
agent loads when about to emit a code block. Examples + counter-
examples. Triggered by mentions of "write Clojure", "edit code",
"refactor".

### Why
Belt + braces: even with the P0 detector, prevention beats repair.
Skill files are the project-local mechanism for steering this.

### Where
- New: `.pi/skills/clojure-code-blocks/SKILL.md`.

### Verify
- N/A (skill content is prose). Reviewed by hand.

---

## Non-actions (deliberately not fixing)

- **`v/rg` accepting a single file path** — already supported (see
  `editing/core.clj:288`). Failure 5 in `REPRODUCTION.md` is a
  misdiagnosis; address by regenerating the doc (P0), not by changing
  `v/rg`.
- **`Ctrl+Y` rebinding** — out of scope; project rule per `AGENTS.md`
  forbids touching it anyway.

---

## Sequence of work

1. P0 prose-in-code detector + test (closes the recurring loop bug).
2. P0 reproduction generator + regenerate `REPRODUCTION.md`.
3. P1 CLI subcommand (so the generator is reachable without an agent).
4. P1 post-edit parse verifier.
5. P1 cost column decision + migration (or doc note).
6. P2 system-prompt + orphan helper.
7. P3 skill.

Each step ships behind a green `./verify.sh`. Each step adds the
matching test file (HARD RULE in `AGENTS.md`).
