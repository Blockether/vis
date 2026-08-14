# PLAN — Make `patch` take one file's whole batch of anchored edits and write once

*One read, one write: a file's edits arrive together or not at all.*

## Context

### State before

Measured against `fbca3dd3a` (v0.1.37).

- **`patch` is one span per call.** `patch-tool`
  (`src/com/blockether/vis/internal/foundation/editing/core.clj:4972`) has four arities: 3-arity is one
  line, 4-arity a span, and both funnel into `patch-one` (`core.clj:4816`), which resolves ONE anchor
  span, splices ONE replacement, runs ONE parse gate and performs ONE `write-safe` write.
- **The caller carries the ordering rule.** `patch-tool`'s docstring (`core.clj:4981-4983`), the system
  prompt (`src/com/blockether/vis/internal/prompt.clj:249-262`) and
  `resources/vis-docs/token-optimization.md:89` all say "several patches in one block go bottom-up,
  highest line first" — the arithmetic the caller must do because each write shifts the anchors of the
  next one.
- **N edits on one file cost N of everything.** N `slurp`s, N tree-sitter re-parses
  (`patch-parse-gate`, `core.clj:4749`), N writes through the dirty-guard, N answers to read — and N
  windows in which another writer can interleave.
- **A mid-batch refusal leaves the file half-edited.** `patch-one` is atomic for its own span
  (`core.clj:4817-4820`); nothing is atomic for the FILE. A stale anchor on the third of five calls
  leaves two edits written and no record that three were intended. `struct_patch` states the same
  weakness in the opposite direction: "A batch of `edits` applies in order and is never rolled back"
  (`core.clj:5451-5452`).
- **The answer is a limited echo.** `patch-one:4901-4921` renders a window of
  `patch-diff-context-lines` = 3 (`core.clj:3622`) lines around the FIRST edit only — up to 7 rows of
  text the caller mostly already has — and `patch-symbol`'s `:result` promises exactly that
  (`core.clj:5017-5019`). With several edits in one call that window cannot describe what happened; the
  one part of it a next edit actually spends is the fresh `line:hash` anchor.
- **The batch argument shape already exists and is proven.** `normalize-edits-arg` (`core.clj:819`)
  turns a stringified batch, a `{"edits": [...]}` kwargs map, ONE bare edit map, or a vector holding
  stringified entries into a real vector of edit maps; `struct_patch`'s path guards already read it
  (`core.clj:843-852`).
- **The anchor primitive is already batch-ready.** `hashline/resolve-anchor-edit-span`
  (`src/com/blockether/vis/internal/foundation/editing/hashline.clj:471`) answers a CHAR SPAN
  `{:start :end :replacement :from-line :to-line}` "WITHOUT building new content", with deletion and
  CRLF semantics settled.
- **The HEAD/TAIL cap exists and is the right bound for a long report.** `head-tail-cap`
  (`core.clj:3657`) keeps a head AND a tail window instead of a head-cut, and already bounds rendered
  diffs to `patch-diff-max-render-lines` = 240 (`core.clj:3624`, used at `core.clj:3751`).
- **Another layer mirrors the positional shape.** `clj-patch-no-fail-around`
  (`extensions/languages/vis-language-clojure/src/com/blockether/vis/ext/language_clojure/core.clj:964-1013`)
  reads the replacement as "the LAST argument" — `(when (>= idx 2) (nth argv idx))` — to parinfer-repair
  it and retry once. A 2-argument `patch` makes that guard fall through silently, so Clojure delimiter
  repair dies without a failing test unless it changes in the same phase.

### Root problem

The unit of the tool is the SPAN; the unit of a correct edit is the FILE. Every cost above is that
mismatch: an ordering rule pushed onto the caller, a partial write when one anchor of many is stale, a
parse gate per span instead of per file, and an answer that can only describe the span it happened to
touch.

### What we solve

One call carries every edit for one file. All anchors resolve against ONE read, so the order the caller
writes them in is irrelevant and overlapping spans are refused instead of silently mis-applied. One
parse gate, one write, one answer. A refusal writes NOTHING and names the offending edit. The answer
drops the window and states, per edit, the anchors that are live AFTER the write.

### What we explicitly do not solve

Cross-file batches — `patch` stays one file per call, and a block calls it once per file. `struct_patch`
(name/`at`-addressed, supported grammars only) is untouched. The anchor format, the drift tolerance and
the read side (`cat`, `grep`) are untouched. No compatibility path for the positional arities is kept:
the old shape is refused with the new one, never silently accepted.

### Alternatives considered

1. **Keep the positional arities, add a batch arity.** Lost: two shapes for one verb is precisely the
   legacy this removes, and the shape a model reaches for first would still be the one that writes N
   times.
2. **Keep the window, render one per edit.** Lost: cost is O(edits x 7) lines of text the caller already
   holds; for three edits the echo is longer than the region it describes. The fresh anchor is the only
   part a next call spends.
3. **Point multi-edit work at `struct_patch`.** Lost: it is name-addressed, needs a grammar, has no
   answer for prose or config, and is explicitly not rolled back.
4. **Sort the spans for the caller and apply bottom-up, refusing nothing.** Lost: overlapping spans have
   no defined result, and quietly picking one is a wrong-line write — the refusal IS the feature.
5. **Answer with the unified diff instead of the window.** Lost: an anchored edit applies the exact text
   the caller supplied, so the diff is echo-bloat — the model wire already strips it for `struct_patch`
   (`src/com/blockether/vis/internal/loop.clj:3140-3145`). The human card keeps rendering it from
   `:metadata`.

---

## Phase 1 — Take the whole file's batch: `patch(path, edits)`, resolved on one read, written once

**Rationale.** Without it, multi-edit work stays N writes with a partial-write failure mode, N parse
gates, and an ordering rule the caller must compute. This phase is the whole point: the verb becomes
`patch(path, edits)` and nothing else.

The shape, in full:

```python
patch("src/app.clj", [
    {"from": "41:9c2", "replace": "  (defn start [] :ok)"},   # one line
    {"from": "88:0af", "to": "90:7ab", "replace": "…"},        # a span
    {"from": "120:31d", "to": "120:31d", "replace": ""},       # from == to, delete
])
```

`to` is optional and defaults to `from`; `from` and `to` may name the same anchor. `replace` is
required — `""` deletes, and an ABSENT `replace` is refused, never treated as `""`. Edits may be given
in any order: every span is resolved against the single `slurp`, so no anchor from the caller's read is
ever stale mid-batch.

**Data.** None. `edits` is an argument shape consumed inside
`foundation.editing.core`: it is not persisted, it is not a gateway wire payload (the model's call is
Python source inside `python_execution`), and no other language mirrors it. The metadata that DOES
cross to a channel is specified in Phase 3.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `patch-one` (`:4816`) becomes
  `patch-file!`: normalize with `normalize-edits-arg` (`:819`), resolve every entry with
  `hashline/resolve-anchor-edit-span` against one `slurp`, splice the spans in DESCENDING `:start`
  order, then one `patch-parse-gate` and one `write-safe`.
- Same file — a new `patch-edits-shape!` replaces `patch-call-shape!` (`:4934`): refuse an empty batch,
  an entry that is not a map, a missing `from`, a missing `replace` (the `(str nil)` silent-deletion
  regression stays covered), and an entry key that is not `from`/`to`/`replace` (a `replacement` typo is
  named, not guessed).
- Same file — a new `:edits-overlap` refusal: two spans that share a character are refused with both
  edit indices and their line ranges, nothing written.
- Same file — every anchor refusal from `anchor-refusal!` (`:4667`) gains the batch coordinate
  (`edit 2 of 5`), because atomicity means one bad anchor refuses the whole call.
- Same file — `patch-tool` (`:4972`) keeps ONE arity, `[path edits]`; `patch-symbol`'s `:call`
  (`:5028`) becomes `{:pos ["path" "edits"]}`; the 1- and 2-argument legacy arities and the
  `:replacement-is-anchor` guard (`:4959-4970`) are deleted — with a named `replace` key there is no
  positional ambiguity left for an anchor-looking string to be mistaken for.
- Same file — `positional-only!` (`:4416`) keeps guarding `path` and stops guarding the edits slot (a
  map there is ONE edit); its `patch` message names the batch shape.
- Same file — the namespace docstring (`:20`) states the batch verb.
- Test that proves it done: `patch-batch-test` in
  `test/com/blockether/vis/internal/foundation/editing/core_test.clj` — three edits in one call land
  together; the same three given in ascending order produce a byte-identical file; a batch whose second
  anchor is stale writes NOTHING and the message names edit 2; two overlapping spans are refused;
  `from` == `to` and `replace: ""` behave as one-line replace and delete.

**Unknowns.**
- Should a batch in which every edit is a no-op (replacement equals current text) be a refusal rather
  than a `changed: false` success, given a no-op batch is usually a stale plan?
- Is 200 edits per call worth a hard ceiling, or does the overlap gate plus the answer cap make a limit
  arbitrary?

---

## Phase 2 — Answer with fresh anchors per edit, not with a window

**Rationale.** Without it the verb still pays for an echo that cannot describe a batch: one window
around one edit, mute about the rest. The caller needs exactly one thing back — where each edit now
lives — and this is the phase the user asked for when they said the limited information has to go.

The answer becomes a status line plus one row per edit, computed AFTER the splice:

```
patched src/app.clj  3 edits  126 → 131 lines (+5)  parse: clean
  1  41..41   → 2 lines   41:9c2 .. 42:7ab
  2  88..90   → 1 line    89:0af
  3  120..120 → deleted
```

**Data.** None — the result is the plain string the tool already answers with; only its content changes.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — delete the window computation
  (`:4901-4913`) and its append (`:4921`); `patch-diff-context-lines` stays in use for the METADATA diff
  only.
- Same file — `patch-status-line` (`:4792`) becomes file-level: path, edit count, line count before →
  after with the delta, then the parse and gutter clauses it already earns.
- Same file — a new `patch-edit-rows` renders one row per edit in the caller's own order, each carrying
  the post-write anchors (`from` .. `to`, or one anchor when the edit is one line, or `deleted`, or
  `unchanged` for a no-op), bounded by `head-tail-cap` (`:3657`) at a new
  `patch-edit-rows-max`, so a 300-edit batch reports a head, a tail and how many rows were omitted.
- Same file — `patch-symbol`'s `:result` (`:5017-5019`) states the new answer; `doc("patch")` mirrors it
  with no second edit.
- Test that proves it done: `patch-spends-the-anchor-test`
  (`test/.../editing/core_test.clj:1370`) is rewritten — it currently asserts the window — to assert
  that the answer carries NO `line:hash│` window, and that every anchor it reports resolves on the very
  next `patch` with no `cat` in between.

**Unknowns.**
- Does the row need the replacement's first line as a label, or is the anchor pair enough for a human
  reading the transcript?

---

## Phase 3 — Keep the other layers proven: Clojure repair, the human card, and every contract that quotes `patch`

**Rationale.** Without it, Clojure delimiter repair silently stops firing (`(>= idx 2)` never holds for
a 2-argument call), the TUI card shows one file row with nothing under it, and the prompt keeps teaching
a call shape the engine now refuses.

**Data.** The per-edit rows on the tool's `:metadata` DO cross a boundary — the engine hands metadata to
channel extensions and the companion renders the same card — so the shape is fixed here before the code:

```clojure
(s/def :vis.patch.region/from_anchor (s/and string? #(re-matches #"\d+:[0-9a-f]{3}" %)))
(s/def :vis.patch.region/to_anchor   :vis.patch.region/from_anchor)
(s/def :vis.patch.region/note        (s/nilable string?))
(s/def :vis.patch/region  (s/keys :req-un [:vis.patch.region/from_anchor]
                                  :opt-un [:vis.patch.region/to_anchor :vis.patch.region/note]))
(s/def :vis.patch/regions (s/coll-of :vis.patch/region :kind vector? :min-count 1))
```

The key names stay the `from_anchor`/`to_anchor` the TUI already reads
(`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/components.clj:1451-1471`).

**Acceptance criteria.**
- `extensions/languages/vis-language-clojure/src/com/blockether/vis/ext/language_clojure/core.clj:964-1013`
  — `clj-patch-no-fail-around` maps `repair/fix-delimiters` over each entry's `replace` in the 2-argument
  call, retries ONCE when any repair changed something, keeps the `(delimiters repaired)` suffix on the
  first line of the answer, and surfaces the original error otherwise; the repair stays syntax-only.
- `extensions/languages/vis-language-clojure/test/com/blockether/vis/ext/language_clojure/core_test.clj:611`
  — the boundary test moves to the batch shape and proves an unbalanced `replace` inside a MULTI-edit
  batch is repaired and the whole batch still lands atomically.
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `patch`'s `:metadata` gains
  `:regions`, one entry per edit, so the card lists the edits instead of one bare path row.
- `src/com/blockether/vis/internal/prompt.clj:249-262` and
  `test/com/blockether/vis/internal/prompt_test.clj:674-675` — the §3/§4 lines teach
  `patch(path, edits)` and drop the bottom-up rule for one file (it survives only for several files in
  one block).
- `resources/vis-docs/token-optimization.md:80,89` — the worked example and the paragraph use the batch.
- `dev/benches/w6_forced.sh:19` and `resources/vis-python/async_runtime.py:2490` — the forced-tool
  description and the comment quote the new shape.
- Test that proves it done: `run_tests` over `src/com/blockether/vis/internal/foundation/editing/core.clj`,
  `src/com/blockether/vis/internal/prompt.clj` and the language-clojure pack is green, and the
  private-deployment-hygiene and prompt tests still pass unchanged.

**Unknowns.**
- Does the companion's tool card need its own change to show the region rows, or does it already render
  whatever `:regions` the TUI does?

---

## State of the plan

**DONE** — all three phases landed; the smallest relevant `run_tests` namespaces are green.

Decisions taken here so they are not re-litigated in review:

1. **One call shape, no legacy.** `patch(path, edits)` and nothing else; a 3- or 4-argument call is
   refused with the batch shape in the message. `normalize-edits-arg` still accepts a single bare edit
   map or a stringified batch — that is coercion of the SAME argument, not a second shape.
2. **Atomic per FILE.** Every anchor resolves against one read; any refusal writes nothing; overlapping
   spans are refused rather than ordered.
3. **The answer is anchors, not text.** No window, no diff — a status line and one row per edit, capped
   by `head-tail-cap`. The human card keeps the full diff from `:metadata`.

Landed, in order:

1. **Phase 1** — `patch-file!`, `patch-edits-shape!`, the `:edits-overlap` refusal, `edit N of M` on every
   anchor refusal, one `[path edits]` arity and `:call {:pos ["path" "edits"]}`.
2. **Phase 2** — file-level status line, one anchor row per edit bounded by `head-tail-cap` at
   `patch-edit-rows-max` (60), the window deleted, `:result` rewritten.
3. **Phase 3** — `clj-patch-no-fail-around` repairs `edits[*].replace` and retries the WHOLE batch, prompt
   §3/§4 (the prompt still fits its 5900-character budget), `token-optimization.md`, the stale-anchor
   advice in `introspection.clj`, the bench and the two comments that quoted the old shape.

One deviation from the accepted plan: **`:metadata :regions` was NOT added.** The plan assumed the TUI
already read it; `grep` found its only reader is the FACTS panel
(`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/components.clj:1471`) and the
companion has none, so the key would have been a contract with no consumer. `:metadata` carries
`:edit-count` instead, beside the diff the human card already renders.

**Supersedes** *"Let a session speak to the other sessions in its tree"*, which was **ACCEPTED** and never
started; it is preserved verbatim in git at `fbca3dd3a` and can be restored unchanged now that this one is
**DONE**.
