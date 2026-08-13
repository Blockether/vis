# PLAN — Bring back `cat` and `patch` as positional verbs over one anchored line, and make `grep` speak the same text

*The read hands you the address; the write spends it — nobody retypes the file.*

## Context

### State before

Commit `7fda0cee2` (*"refactor(editing)!: remove cat/patch and retire the lineno:hash anchor"*) deleted both verbs and the `lineno:hash` handle they existed to consume: `editing/patch.clj` (700 lines) went outright, `editing/core.clj` lost ~1265 lines, and every structural surface was rewritten onto plain line numbers. Today the model-facing editing surface is four symbols — `src/com/blockether/vis/internal/foundation/editing/core.clj:4776`:

```clojure
(defn available-editing-symbols [] [index-symbol grep-symbol struct-patch-symbol nodes-symbol])
```

and the prompt tells the model to write text edits by hand — `src/com/blockether/vis/internal/prompt.clj:237-238, 243-244`:

> READING a whole file is `Path.read_text`; CHANGING the tree is plain Python (`Path.write_text`, `os`, `shutil`).
> Edit with `struct_patch` by def NAME; text and unsupported languages are edited in plain Python.

The prediction recorded in the plan this one supersedes (`e51669e06:PLAN.md:534-537`) was: *"What is lost is the atomic anchor-verified multi-file text edit; that is re-expressed as a small Python helper in the sandbox prelude if a reproduction shows it is missed."*

**The reproduction is the journals.** Across 805 unique blocks:

| Measurement | Value |
| --- | --- |
| Blocks that write a file | 96 — **12 %** of blocks |
| Share of ALL block characters those 96 carry | **48 %** (201,520 of 423,036) |
| Mean chars, writing block vs non-writing block | 2,099 vs 312 — **6.7×** |
| Share of a writing block that is triple-quoted literal payload | **80 %** (162,300 chars) |
| Blocks that re-read a whole file and rewrite it | 61 |
| Blocks that edit via `x.replace(old, new)` | 49 |
| Blocks that define their own `def edit(path, old, new)` helper | 6 |
| Blocks that call `struct_patch` | **11** |

The invented helper is verbatim, three times over, in three different sessions:

```python
def edit(path, old, new, count=1): ...
def edit(path, pairs): ...
def edit(rel, pairs): ...
```

and the canonical shape of a text edit today is:

```python
p = root/"src/com/blockether/vis/internal/foundation/shell.clj"
s = p.read_text()
old = '''   The `shell` toggle is registered HERE, extension-owned under the vis namespace."'''
new = '''   The `shell` toggle is registered HERE, extension-owned under the vis
   namespace. It closes the MODEL's door only: …"'''
assert s.count(old) == 1
s = s.replace(old, new)
```

*(Minor, stated with its caveat: 9 blocks in the sample still call `cat(...)`/`patch(...)` with the OLD keyword signatures. Those journals straddle the removal, so read it as "the shape is still in the model's head", not as a post-removal failure.)*

**One more piece of evidence the removal left behind.** `grep`'s shipped result contract still *promises* the anchor — `src/com/blockether/vis/internal/foundation/editing/core.clj:4056`:

```clojure
"`matches={path:{\"line:hash\":{\"text\":string,\"before\"?:[{\"line\",\"text\"}],\"after\"?:[…]}}}` "
```

while `content-result` (`:2135`) actually keys every hit with `(str line)` — a bare line number. The model is told each hit is an address and handed a coordinate that is not one. The same boundary test still carries an anchor-shaped key as its fixture (`test/com/blockether/vis/internal/env_python_test.clj:542`, `{"matches" {"a/b-c.clj" {"1:h" "z"}}}`). The contract wants what this plan restores.

### The root problem

An edit needs a **coordinate**. `struct_patch` supplies one for a *named definition in a parsed language* (`core.clj:4150`), and that covers the minority of edits: prose, config, YAML, Markdown, a comment, a docstring line, a string literal inside a form, and every unsupported language have **no address at all**. So the model mints one the only way plain Python allows — **by quoting the old text back**. That coordinate is (a) O(size-of-region) in tokens, (b) unverified except by a hand-written `assert s.count(old) == 1`, (c) re-derived from scratch on every call, and (d) never reusable: after the write, the quoted text no longer exists.

The anchor scheme solved exactly this. `line:hash` is **4–7 characters** that carry both a location and a content proof, and — the part that matters — **it is produced by the read the model already had to do**. Removing it did not remove the cost; it moved the cost from a 5-character token into a 2 KB block, and moved the safety from a re-parse into an `assert`.

### What we solve

- One read verb whose **output is the address**: `cat` returns text already carrying anchors, so no separate "get me handles" step exists.
- One write verb that spends that address: `patch` names a span and the new text — **the old text is never restated**.
- The write is refused when the coordinate no longer agrees with the file (stale/misplaced anchor) and refused again when the result would not parse (tree-sitter), with the correct anchor handed back so recovery is one call, not a re-read.
- `patch` returns the **re-anchored** window, so a follow-up edit needs no second `cat`.
- `grep` answers in that **same anchored text** instead of a nested map, so a search hit *is* a `patch` argument and the printed result costs 41 % less than the dict it replaces.

### What we explicitly do not solve

- **No batch, no multi-file, no `edits` list.** One call = one file = one span. Batching lives where it already lives: several `patch(...)` calls in one `python_execution` block. This deletes the entire `normalize-edits-arg` / `coerce-patch-edits` coercion layer (~120 lines at old `core.clj:789-835, 3300-3369`) that existed only to survive serializer damage to a nested list. The atomic multi-file text edit that died with `7fda0cee2` stays dead — decided, not open.
- **No return to `struct_patch`.** Name-addressed structural ops (rename, move, `append_child`, `add_doc`) stay exactly as they are. `patch` is the address-addressed editor beside it, not above it.
- **No compatibility layer.** Per repo doctrine there is no legacy `from_anchor`/`to_anchor`/`replace` keyword form; the positional signature is the only one.

### Alternatives considered

| Alternative | Why it lost |
| --- | --- |
| **Leave it; add a prelude `edit()` helper to the sandbox** (the superseded plan's recommendation, `e51669e06:PLAN.md:536`) | Measured and refuted. A prelude helper still takes the old text as its argument — 80 % of the cost is the *payload*, not the loop around it. It also cannot re-parse, cannot verify, and cannot hand back a fresh coordinate. |
| **Extend `struct_patch` to text files** | `struct_patch` addresses by *name in a parse tree*. Markdown prose, a YAML value and a comment line have no name and no node; giving it a line locator recreates `patch` inside a verb whose whole contract is "structural". |
| **Bare line ranges, no hash** (`patch(path, 120, 134, new)`) | This is what corrupts files. A line number alone is stale the moment anything above it moves, and it fails **silently and plausibly** — it writes to the wrong place instead of refusing. The hash is what makes the refusal possible. |
| **Keep `grep` structured, just re-key `matches` by anchor** (the first cut of this plan) | Half the win. The anchor fixes addressing but the model still prints a nested dict: measured over six real greps, the dict repr is 10,622 chars against 6,297 for the text — and `print(r)` is what the model actually pays. |
| **Flat `path:line:hash│ text` on every grep line** (ripgrep shape) | Measured: 9,551 chars against 6,297 grouped — only 10 % under the dict, because this repo's paths run ~60 chars and every hit repeats one. Grouped under a path header wins by 31 points. |
| **Return a map from `cat` (the old `{results, ranges, anchors}`)** | The old shape forced the model to index it and re-print, so the read cost was paid twice. A bare string is `print(cat(...))`, is sliceable in Python, and has no key vocabulary to learn. |
| **Content hash only, no line number** (the pre-`7fda0cee2` `#N` ordinal scheme) | Already tried and already replaced: a 3-hex hash collides, and disambiguating needed a whole-file rescan. Line locates + hash verifies is the design that survived. |
| **A wider hash (6–8 hex)** | The line number is the primary coordinate; the hash only has to disambiguate within a ±40-line window. 3 hex chars keep an anchor at 4–7 characters, which is the point. |

---

## Phase 1 — Restore the hashline engine as `editing/hashline.clj`

**Rationale.** Both verbs are thin IO wrappers over one pure module. Without it there is nothing to mint an address from, and the two verbs would each grow their own hash math inside the channel/IO namespace. It comes back verbatim from `7fda0cee2^:src/com/blockether/vis/internal/foundation/editing/patch.clj` **minus** its escape-decoder half, which already lives at `editing/escapes.clj:1-155` and stays there. New name, because the namespace should name the concept (`hashline`), not the verb.

Surface restored: `split-content-lines`, `char-offset-at-line`, `line-hash` (3 hex chars of the trimmed line's `String/hashCode`, a JIT intrinsic — one pass per rendered line), `line-anchor`, `anchor->line`, `render-hashline-block`, `indices-matching-hash`, `hash-line-drift-tolerance` (40), `resolve-one-anchor`, `resolve-anchor-range`, `resolve-anchor-range-read`, `resolve-anchor-edit-span`.

The five resolution outcomes are the contract and come back unchanged (old `patch.clj:329`):

1. **exact** — the stated line still hashes to the anchor's hash → use it.
2. **drifted** — that hash sits at exactly one line within 40 → follow the content.
3. **line wins** — the hash is ambiguous but the model named an explicit line → use the line.
4. **misplaced** — the hash exists only far from the stated line → **refuse** (this is the guard that stops a write landing on the wrong line).
5. **not-found** — the hash matches nothing → **refuse**, and hand back the anchor that IS at the stated line.

Newline semantics come back with it (old `patch.clj:498`): a replacement need not end in `\n`; the matched region's terminator is preserved, `\r\n` stays `\r\n` on a CRLF file; and `replacement == ""` consumes the trailing newline so the lines actually vanish instead of leaving blanks.

**Data.** The anchor token is the one string that crosses Clojure → GraalPy → the model → back, so it is spec'd before the code:

```clojure
(s/def :ext.editing.hashline/hash   (s/and string? #(re-matches #"[0-9a-f]{3}" %)))
(s/def :ext.editing.hashline/line   pos-int?)
;; `<1-based line>:<hash>` — the ONLY parseable anchor form. `cat` mints it,
;; `patch` consumes it, and nothing else in the tree parses one.
(s/def :ext.editing.hashline/anchor
  (s/and string? #(re-matches #"\d+:[0-9a-f]{3}" %)))
(s/def :ext.editing.hashline/parsed
  (s/keys :req-un [:ext.editing.hashline/line :ext.editing.hashline/hash]))
```

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/hashline.clj` — new; the pure surface above, no IO, no tool wiring.
- `src/com/blockether/vis/internal/foundation/editing/escapes.clj` — unchanged; `hashline` does not touch escapes.
- `test/com/blockether/vis/internal/foundation/editing/hashline_test.clj` — restored from `7fda0cee2^:…/editing/patch_test.clj` (236 lines), renamed.
- `test/com/blockether/vis/internal/foundation/editing/core_test.clj:1034-1075` — `anchored-verbs-are-gone-test` is the standing guard that these verbs stay dead (`"the hashline namespace is gone and nothing re-requires it"`, `"no editing symbol is advertised under the retired names"`). It is **inverted**, not deleted: it becomes the test that `cat`/`patch` ARE advertised and that `hashline` loads. `dev/benches/w6_forced.sh:19` still names the old `:from-hash/:to-hash` keyword flags and is re-pointed at the positional form.
- Test that proves it done: `hashline-test/resolve-one-anchor-five-outcomes-test` — one case per outcome, including a **misplaced** anchor that must refuse and a **drifted** one that must follow.

**Unknowns.** None. This is a verbatim restore of code that had a green suite.

---

## Phase 2 — `cat`: one file, three positional arguments, one string

**Rationale.** Without this, `patch` has no address to spend and the model is still quoting old text. This is also the phase that makes the read cheaper than the read it replaces: `Path.read_text()` returns a blob the model must re-emit to point at anything in it.

### Signature

```python
cat(path)                       # whole file, capped
cat(path, from)                 # from → EOF, capped
cat(path, from, to)             # closed range, inclusive
```

- **Positional only.** One file. No `files`, no `ranges`, no keywords.
- `from` / `to` accept **either** a 1-based line number **or** an anchor `"4435:7f2"`, mixed freely. The read is the forgiving side: a stale anchor resolves through `resolve-anchor-range-read` and only refuses when it truly cannot be located.
- **Returns a plain `str`** — nothing else. No dict, no `results` key, no metadata. `print(cat(...))` shows it; `cat(...).splitlines()` slices it in Python without printing anything.
- Every line — **including blank lines** — carries an anchor, so the read is gap-free and every line is addressable. (An empty line hashes to `000`.)
- Caps: 2000 lines / 50 KiB per call (the shipped `default-cat-limit` / `max-cat-window-bytes`). When clipped, the **last line of the string** says so and names the next call; nothing is silently dropped.
- Gate: `(fs-access-before-fn :cat :file "file-read" …)`, identical to the other read verbs.
- Raises on: missing file, directory, `from > to`, line out of range, unresolvable anchor.

### stdout — real, rendered from `editing/core.clj`

```
>>> print(cat("src/com/blockether/vis/internal/foundation/editing/core.clj", 4435, 4450))
4435:7f2│ (def struct-patch-symbol
4436:88f│   (vis/symbol
4437:111│     #'struct-patch-tool
4438:21c│     {:symbol 'struct_patch
4439:a80│      :result "One row/edit: `path`, `op`, `changed`, `diff`, `lines`."
4440:056│      :active-fn structural-supported?
4441:b82│      :description
4442:d09│      (str
4443:036│        "Structurally edit supported code: definition by NAME (`target`) or node by "
4444:094│        "`at`/`line`. Renames, docs, moves, `append_child`. Writes re-parse: code that will not parse "
4445:e74│        "is REFUSED; unbalanced Clojure delimiters auto-repaired. A batch of `edits` applies in order "
4446:753│        "and is never rolled back: an entry that fails leaves the earlier ones written.")
4447:c7a│      :before-fn (plan-gated-before-fn :struct_patch :file struct-arg-paths)
4448:1e3│      :tag :mutation
4449:92b│      :on-error-fn (tool-failure-on-error :struct_patch :file)}))
4450:000│
```

Gutter is `│ ` (U+2502 + space) — it never occurs in source, so `line.split("│ ", 1)` is exact, and it cannot be confused with the `:` inside the anchor. Cost of the whole addressing scheme in that block: **8 characters per line.**

Clipped tail:

```
6134:c0a│ (def editing-prompt
… clipped at 2000 lines — file has 6134; continue with cat(path, 2001, 4000)
```

**Data.** None — `cat` returns a `string?`. The only structured token inside it is `:ext.editing.hashline/anchor`, spec'd in Phase 1.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `cat-one`, `cat-tool`, `cat-symbol` restored, single-path and positional (`:call {:pos ["path"] :opt-pos ["from" "to"]}`, the shape already proven at `foundation/mcp/core.clj:1119`); added to `available-editing-symbols` (`:4776`).
- `test/com/blockether/vis/internal/foundation/editing/core_test.clj` — `cat` describes restored.
- Test that proves it done: `editing/core-test/cat-returns-anchored-string-test` — asserts the return is a `String`, that every line matches `\d+:[0-9a-f]{3}│ `, that a blank line still carries an anchor, and that an anchor endpoint and an integer endpoint select the same window.

**Unknowns.**
- Whole-file `cat` with no range: cap at 2000 lines, or refuse over some size and demand a range? *(Proposed: cap and say so in the last line.)*
- Should `cat` accept a `struct_index` row's `line`/`end_line` pair directly, i.e. is the integer form enough of a bridge from the structural verbs? *(Proposed: yes, no extra affordance.)*

---

## Phase 3 — `patch`: a positional span replace with a tree-sitter consistency gate

**Rationale.** This is the phase that deletes the 2 KB block. Without it Phase 2 is just a prettier read.

### Signature

```python
patch(path, anchor, replacement)                   # ONE line  — 3 args
patch(path, from_anchor, to_anchor, replacement)   # a span    — 4 args
```

- **Positional only.** Arity disambiguates and cannot be ambiguous, because `replacement` is always last: 3 args ⇒ `to_anchor = from_anchor`.
- **`patch` requires anchors, never bare line numbers** — deliberately asymmetric with `cat`. The read is forgiving; the write is *verified*. A bare line number is exactly the silent-corruption case this scheme exists to refuse.
- `replacement` is text: multi-line allowed; `""` deletes the span; drifted `\uXXXX` escapes are decoded once via `escapes/decode-unicode-escapes`.
- **Atomic per call**: a refusal writes nothing.
- **Several edits in one block are applied bottom-up**, highest line first — then every anchor from the same `cat` is still exact. (Small drift is forgiven up to 40 lines anyway; this is the rule that makes it free.) This replaces the old atomic multi-edit batch and is a prompt line, not machinery.
- Gate: `(plan-gated-before-fn :patch :file …)`, same as `struct_patch`.

### The consistency check (the hook you asked for)

After splicing, **before writing**:

1. If the file's language is structurally supported (`structural-supported?`, `core.clj:4007`), re-parse the new content with tree-sitter.
2. If it has an ERROR node **and the original parsed clean** → **refuse**, write nothing, name the first error line.
3. If the original was **already** broken → write, and say `parse: still broken at line N`. You must be able to repair a broken file; refusing there would strand it.
4. Unsupported language / prose / config → no parse gate, write.

The Clojure pack's existing repair hook is extended to the new op — `extensions/languages/vis-language-clojure/src/com/blockether/vis/ext/language_clojure/core.clj:995`:

```clojure
:ext/op-hooks [{:op :struct_patch :phase :around :fn clj-struct-patch-no-fail-around}
               {:op :patch        :phase :around :fn clj-patch-no-fail-around}]
```

so an unbalanced `(` `[` `{` in a `.clj` replacement is parinfer-repaired once and retried (`core.clj:930-961`), and the status line reports `repaired`. Per AGENTS.md this stays **syntax-only** — the smallest mechanical change that restores parseable source, never a semantic rewrite.

### stdout — success, real

`patch` returns a `str` too: one status line, then the **re-anchored** window with 3 lines of context each side (the shipped `patch-diff-context-lines`). Those anchors are fresh, so the next edit needs no second `cat`.

Single line, 3-arg form:

```
>>> print(patch("src/…/editing/core.clj", "4439:a80",
...             '     :result "One row/edit: `path`, `op`, `changed`, `diff`, `lines`, `anchors`."'))
patched src/com/blockether/vis/internal/foundation/editing/core.clj  4439..4439 → 1 line  parse: clean
4436:88f│   (vis/symbol
4437:111│     #'struct-patch-tool
4438:21c│     {:symbol 'struct_patch
4439:b16│      :result "One row/edit: `path`, `op`, `changed`, `diff`, `lines`, `anchors`."
4440:056│      :active-fn structural-supported?
4441:b82│      :description
4442:d09│      (str
```

Span replace that shortens the file, 4-arg form:

```
>>> print(patch("src/…/editing/core.clj", "4443:036", "4446:753", new_text))
patched src/com/blockether/vis/internal/foundation/editing/core.clj  4443..4446 → 2 lines (-2)  parse: clean
4440:056│      :active-fn structural-supported?
4441:b82│      :description
4442:d09│      (str
4443:133│        "Structurally edit supported code by NAME (`target`) or node `at`/`line`. Writes re-parse: "
4444:43b│        "code that will not parse is REFUSED.")
4445:c7a│      :before-fn (plan-gated-before-fn :struct_patch :file struct-arg-paths)
4446:1e3│      :tag :mutation
4447:92b│      :on-error-fn (tool-failure-on-error :struct_patch :file)}))
```

With repair: `… → 2 lines (-2)  parse: clean (delimiters repaired)`.

### stdout — refusals, real shapes

A refusal **raises** — decided, not proposed — so it surfaces as a failed tool result the model cannot skim past; a returned failure row is skimmable, and this one carries the anchor that fixes it. House style, matching `tool-failure-on-error`. Each refusal carries the one-step recovery.

Stale anchor (`:anchor-not-found`) — the common case, and it costs **zero** extra reads:

```
patch refused — nothing was written.
  src/…/editing/core.clj  from_anchor 4439:a80
  line 4439 now hashes b16, and no line within 40 lines carries a80.
  current anchor at 4439 →  4439:b16│      :result "One row/edit: `path`, …, `anchors`."
  retry with that anchor, or re-read: cat(path, 4436, 4446)
```

Misplaced anchor (`:anchor-misplaced`) — the wrong-line guard:

```
patch refused — nothing was written.
  src/…/editing/core.clj  from_anchor 4439:a80
  a80 is not at line 4439; it is at line 217, beyond the 40-line drift window.
  current anchor →  217:a80
  the anchor is stale or belongs to another region; confirm with cat before retrying.
```

Parse gate (`:parse-broken`) — the check that made `patch` safe:

```
patch refused — the edit would not parse; nothing was written.
  src/…/editing/core.clj  4443..4446
  clojure: ERROR node at line 4444, col 41 — near `:REFUSED.")`
  the file parsed clean before this edit, so the replacement introduced it.
```

**Data.** The refusal payload crosses the boundary as ex-data → tool failure → wire, so it is spec'd:

```clojure
(s/def :ext.editing.patch/reason
  #{:anchor-malformed :anchor-line-out-of-range :anchor-not-found :anchor-misplaced
    :parse-broken :file-not-found :path-is-dir :path-escape})
(s/def :ext.editing.patch/current-anchor :ext.editing.hashline/anchor)
(s/def :ext.editing.patch/stated-line    pos-int?)
(s/def :ext.editing.patch/found-lines    (s/coll-of pos-int? :kind vector?))
(s/def :ext.editing.patch/error-line     pos-int?)
(s/def :ext.editing.patch/refusal
  (s/keys :req-un [:ext.editing.patch/reason]
          :opt-un [:ext.editing.patch/current-anchor :ext.editing.patch/stated-line
                   :ext.editing.patch/found-lines :ext.editing.patch/error-line]))
```

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `patch-tool` + `patch-symbol` restored with `:call {:pos ["path" "from_anchor"] :opt-pos ["to_anchor" "replacement"]}` and arity folding; added to `available-editing-symbols`. `coerce-patch-edits` / `normalize-edits-arg` are **not** restored.
- `src/com/blockether/vis/internal/foundation/introspection.clj` — `:patch-stale-anchor` re-added beside `:struct-patch-invalid-code` (`:336, :352, :682`), so a stale-anchor loop is classified rather than counted as a generic failure.
- `extensions/languages/vis-language-clojure/src/com/blockether/vis/ext/language_clojure/core.clj:995` — the `:op :patch :phase :around` hook.
- Tests that prove it done:
  - `editing/core-test/patch-refuses-stale-anchor-and-returns-fresh-one-test`
  - `editing/core-test/patch-refuses-a-write-that-breaks-the-parse-test` (and its twin: an **already broken** file still accepts an edit)
  - `editing/core-test/patch-returns-reanchored-window-test` — the returned anchors resolve on the very next `patch` with no `cat` between
  - `ext/language-clojure/core-test/clj-patch-repairs-delimiters-test`

**Unknowns.**
- Should the human/TUI channel keep the unified diff (the old `unified-diff-text` at `7fda0cee2^:core.clj:4778`) while the model gets the re-anchored window? *(Proposed: yes — the renderer builds the diff from the write; the model never pays for it.)*
- Should the consecutive-failure loop detector come back (`patch-fail-loop-threshold` 3, old `core.clj:3370-3399`), or does `introspection` now cover it?

---

## Phase 4 — Rewrite the model-facing sentences off the two verbs, and measure

**Rationale.** Without this the verbs exist and the prompt still tells the model to hand-roll the edit; the measured behavior would not change.

### `src/com/blockether/vis/internal/prompt.clj` — §3 Inspect

Line 237-238, today:

> READING a whole file is `Path.read_text`; CHANGING the tree is plain Python (`Path.write_text`, `os`, `shutil`).

becomes:

> READING a region you will EDIT is `cat(path, from, to)` — its output IS the address: every line arrives as `line:hash│ text`. `Path.read_text` is for a file you only consume; creating/moving/deleting files is plain Python.

Line 243-244, today:

> Edit with `struct_patch` by def NAME; text and unsupported languages are edited in plain Python.

becomes:

> Edit by NAME with `struct_patch`; edit by ADDRESS with `patch(path, from, to, new)` — prose, config, comments, docstrings, unsupported languages. **Never restate the text you are replacing and never hand-roll read/replace/write in Python**: quote the anchor, not the file.

The `grep` sentence in §3 gains its consequence — the phrase that makes Phase 5 pay off in behavior rather than only in bytes:

> `grep` locates unknown code — ONE `{query, paths}` map ORs every needle, every scope. **Its hits come back anchored (`line:hash│ text`), so a hit is already a `patch` argument** — go straight to `patch`, do not re-read the region to find it again.

And the grep symbol contract:

```clojure
;; grep
:result "Text, not a map: line 1 summarizes (hits, files, truncation and the exact next
         call); then per path a header and one `  <line>:<hash>│ <text>` row per hit,
         context lines anchored too. Feed an anchor straight to `patch`."
```

### `prompt.clj` — §4 Edit + verify

Line 252-253 gains one sentence:

> `patch` hands back the re-anchored window, so a follow-up edit needs no second `cat`; several edits in one block go **bottom-up**, highest line first. A stale anchor is refused with the fresh one attached — a retry costs one call, not a re-read.

### Symbol contracts (what `doc(name)` prints)

```clojure
;; cat
:description "Read one file's region as patch-ready `line:hash` text — the read that
              produces the address `patch` spends. `from`/`to` are line numbers or
              anchors. `ls(dir)` lists directories; `struct_index` maps code first."
:result      "A plain string: one `<line>:<hash>│ <text>` line per source line,
              blanks included. No map, no keys. Clipped windows say so on the last line."

;; patch
:description "Replace an anchored span with new text — prose, config, or any language.
              `patch(path, anchor, new)` for one line, `patch(path, from, to, new)` for a
              span, `new=\"\"` deletes. Atomic: a refusal writes NOTHING. A stale or
              misplaced anchor is refused with the correct one attached; supported code
              is re-parsed and a syntax-breaking write is refused; unbalanced Clojure
              delimiters are auto-repaired."
:result      "A plain string: a status line, then the re-anchored window with 3 lines of
              context — those anchors are fresh, so the next patch needs no cat."
```

### The rest of the surface

- `src/com/blockether/vis/internal/env_python.clj:2787` — the read/edit hint names `cat`/`patch`.
- `src/com/blockether/vis/internal/doc_corpus.clj:252` — `"cat"` and `"patch"` back in the doc index list.
- `resources/vis-docs/token-optimization.md:12, 46, 51, 59, 67, 75, 139` — the editing ladder becomes: `grep` → `cat` for the region → `patch` by anchor; `struct_index`/`struct_nodes`/`struct_patch` for name-addressed structural work.
- `resources/vis-docs/index.md:61` and `resources/vis-docs/graalpython.md:7` — the verb list.
- `AGENTS.md` — no change needed; it names no editing verb.

### Measurement (this is the acceptance)

Re-run the journal scan over the 40 sessions that follow the change and report the same table. Target: **writing blocks fall below 2× the non-writing mean** (from 6.7×), and **triple-quoted payload inside writing blocks falls below 30 %** (from 80 %). Third number, from Phase 5: **printed grep output per call falls ~41 %** against the dict repr it replaces, measured the same way over the six-grep table.

**Data.** None — prose and documentation only.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/prompt.clj` — §3/§4 as above.
- `src/com/blockether/vis/internal/env_python.clj`, `doc_corpus.clj`, the four `resources/vis-docs/*.md` pages.
- `test/com/blockether/vis/internal/prompt_test.clj` — asserts the core prompt names `cat(` and `patch(`, and asserts it does **not** tell the model to write text edits with `Path.write_text`.
- Test that proves it done: `prompt-test/core-prompt-routes-text-edits-to-patch-test`.

**Unknowns.** How much does the core prompt grow? It should be net-neutral: two verbs named, one "edit in plain Python" instruction removed.

---

## Phase 5 — `grep` answers in anchored text, not a nested map

**Rationale.** `grep` is the verb that finds the thing you are about to edit, and today it answers in the one shape the model cannot spend: a four-level nested map (`matches → path → "lineno" → {text, before, after}`) whose keys are bare line numbers. Two costs follow, and both are measured, not argued.

*It costs more to say the same thing.* The model pays for what it **prints**, and `print(grep(...))` prints the Python dict repr. Across six real greps against this repo:

| Grep | dict repr | anchored text | saved |
| --- | --- | --- | --- |
| 1 needle, no hits, `src` | 528 | 138 | 74 % |
| zero hits + regex hint | 531 | 139 | 74 % |
| blank query (ls mode), 5 files | 741 | 356 | 52 % |
| `hashline`, 8 hits / 5 files | 2,043 | 1,377 | 33 % |
| `structural-supported?`, `context: 2` | 2,682 | 1,645 | 39 % |
| `defdescribe`, 50 hits / 11 files | 4,097 | 2,642 | 36 % |
| **aggregate** | **10,622** | **6,297** | **41 %** |

*And the shape itself is friction.* `resources/vis-python/async_runtime.py:384-392` documents the papercut in its own comment — the deferred-call auto-settle exists because the model writes `grep(...)["matches"]`. `src/com/blockether/vis/internal/env_python.clj:130-135` and `:175-180` carry two separate ordering machineries whose *named* motivating case is grep: *"Without this, a round-tripped ordered tool result (grep's matches LinkedHashMap) comes back HASH-ordered and the model reads the file out of line order."* A string is ordered because it is a string. (The machinery stays — other verbs still return maps — but grep stops being the thing that justifies it.)

With Phase 1 in place the fix is one step further: **every rendered line carries its anchor**, context lines included. A grep hit is then not "a place to go read" but an argument. In the sample below, `4439:a80` is literally the anchor the Phase 3 `patch` example spends — same file, same algorithm, no `cat` in between.

### Format

```
<summary>                       ← ALWAYS line 1
<path>  (<hits in this file>)
  <line>:<hash>│ <text>
  ⋮                             ← gap marker, only when `context` > 0
~ <path>                        ← fuzzy NAME matches, after the content block
hint: …                         ← only when there is one
```

- **Every rendered line carries an anchor, context lines included** — a `context: 2` line is directly patchable without a second read.
- Hashes collide freely across files (every `(defdescribe` line hashes `ca8`); that is harmless, because an anchor is only ever spent inside `patch(path, …)` and the path is on the header line.
- Gutter is `│ `, exactly as `cat` — `line.split("│ ", 1)` is exact and the anchor's `:` is unambiguous. Two-space indent under the header.
- Blank query is ls mode: summary plus `~ path` lines, no content block.
- Zero hits is the summary plus the existing `hint:` — the regex-looking-query hint keeps working.
- **The counts stay prose on line 1 and no structured field survives** — decided. `r["hit_count"]` is gone; control flow reads the summary (`"0 hits" in first_line`) or counts the lines it just split. One structured field would re-introduce a map around a string and the whole 41 % with it.

**The summary line goes FIRST, and that is a decision, not a layout preference.** A block's printed output is *head*-clipped at `MAX_FORM_WIRE_CHARS` (`src/com/blockether/vis/internal/loop.clj:3119-3148`, 65,536 chars): a trailing summary is the first casualty of a wide grep — exactly the grep whose truncation you must know about. First also gives the TUI card its headline, which it currently derives from the `matches` key via `tally-keys` (`loop.clj:3150-3155`) and would otherwise lose when the result stops being a map.

### stdout — real, rendered against this repo

Plain, two files:

```
>>> print(grep({"query": ["defn- grep-", "def grep-"], "paths": ["src"], "limit": 4}))
grep 'defn- grep- def grep-'  4 hits · 2 files
src/com/blockether/vis/internal/config_spec.clj  (2)
  336:498│ (def grep-keys #{"include_gitignored_paths" "always_exclude"})
  356:136│ (def grep-schema {"include_gitignored_paths" string-list? "always_exclude" string-list?})
src/com/blockether/vis/internal/foundation/editing/core.clj  (2)
  2253:42b│ (defn- grep-tool
  4047:3ac│ (def grep-symbol
```

With `context: 2` — note the anchors on the context lines, and `4439:a80`:

```
>>> print(grep({"query": "structural-supported?", "paths": ["src"], "context": 2}))
grep 'structural-supported?'  5 hits · 1 file
src/com/blockether/vis/internal/foundation/editing/core.clj  (5)
  4002:2d9│    while tree-sitter calls it `bash` — so this is the reconciled set, NOT just
  4003:dbe│    `code-languages`. (Languages the scan doesn't recognize at all — e.g. `.elm`,
  4004:794│    `.jl` — simply don't appear, and `structural-supported?` fails OPEN on them.)"
  4005:e7c│   (conj index/code-languages "shell"))
  4006:000│ 
  4007:292│ (defn structural-supported?
  4008:0a0│   "Whether the STRUCTURAL editors should be advertised for the current project:
  4009:812│    true when its language scan finds at least one file in a structurally-supported
  ⋮
  4035:c8d│        "`include_occurrences` adds a group per name: `symbols` (`path,line,end_line,kind,visibility,signature,use_count,uses{path,lines}`), `other_uses`, `count`, `definition_count`, `scanned`, `failed`. "
  4036:beb│        "No source — pass a row `line` to `struct_nodes`.")
  4037:056│      :active-fn structural-supported?
  4038:b82│      :description
  4039:d09│      (str
  ⋮
  4438:21c│     {:symbol 'struct_patch
  4439:a80│      :result "One row/edit: `path`, `op`, `changed`, `diff`, `lines`."
  4440:056│      :active-fn structural-supported?
  4441:b82│      :description
  4442:d09│      (str
  ⋮
  4629:a82│        "`kind,line,end_line,source` (verbatim), `sexp,named_child_count,children,can,has_error`. "
  4630:150│        "Misses add `error`/`reason`; other fields nil.")
  4631:056│      :active-fn structural-supported?
  4632:b82│      :description
  4633:c1b│      (str "ONE options map is the whole call — `struct_nodes({\"path\": p, \"line\": n})` or "
```

Truncated and paged — the summary carries the literal next call, and breadth survives as *N of M files*:

```
grep 'defdescribe'  50 hits · 11 of 136 files  capped by limit → grep({…, "offset": 50})
```

Zero hits:

```
>>> print(grep({"query": "resolve-anchor.*range", "paths": ["src"]}))
grep 'resolve-anchor.*range'  0 hits · 0 files
hint: No file NAME or CONTENT matched "resolve-anchor.*range". Try a different term, a real symbol/string, or widen the scope. CONTENT matching is LITERAL smart-case substring — regex syntax is not interpreted; search a plain distinctive fragment.
```

**Data.** The tool's declared result changes from a 19-key map to `string?`, and these keys stop crossing the wire as keys — every one of them is folded into the summary line or the body: `op query needles searched_paths missing_paths paths matches file_counts first_hit hint hit_count file_count total_file_count total_file_count_is_exact limit offset next_offset truncated_by hits_truncated_by`.

The seam already exists, so nothing pure is lost:

```clojure
;; `content-result` (core.clj:2135) KEEPS returning the ordered data — it is the
;; tested pure core. `grep-tool` becomes its renderer, and the anchor is minted
;; on the already-materialized hit text.
(defn- render-grep-text
  "grep's model-facing projection: summary line, then per-path anchored hits."
  ^String [result spec] …)
```

Verified blast radius: **no in-tree Clojure consumer parses grep's tool result.** `file_picker.clj` goes straight to `fff-index`, `channel_tui/file_suggest.clj` touches neither, and `loop.clj`/`security_policy.clj` matched only on unrelated `re-matches`. The consumers are the model, the TUI headline, and the tests.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `render-grep-text` added; `grep-tool` returns its string; `grep-symbol`'s `:result` (`:4047-4057`) rewritten off the map — and its stale `"line:hash"` promise (`:4056`) becomes true again.
- `test/com/blockether/vis/internal/foundation/editing/core_test.clj` — the ~20 assertions that read `hit_count` / `matches` / `first_hit` off the tool result (`:859`, `:2280`, `:2293-2296`, `:2703-2745`, `:2805`, `:2825-2846`, `:2928-2954`, `:3156`) retarget the pure `content-result`; a small set pins the rendering instead.
- `src/com/blockether/vis/internal/loop.clj` — the TUI headline for a string result takes line 1 (`tally-keys` no longer sees `matches`).
- `resources/vis-docs/token-optimization.md`, `index.md` — the grep examples show text, not a map.
- Tests that prove it done:
  - `editing/core-test/grep-returns-anchored-text-test` — every content line matches `^  \d+:[0-9a-f]{3}│ `, the summary is line 1, and a `context` line carries an anchor too.
  - `editing/core-test/a-grep-hit-is-a-patch-anchor-test` — grep a literal, take the anchor off the hit line with `split("│ ", 1)`, feed it straight to `patch`, assert it resolves with no `cat` between.
  - `editing/core-test/grep-text-states-its-own-truncation-test` — a capped sweep names the exact next call on line 1.

**Unknowns.**
- Do `find_files` / `find` — the compatibility aliases on the same var (`env_python.clj:258-263`) — follow grep into text? *(Proposed: yes, same var, same shape.)*
- Should the whole options map become positional like `cat` (`grep(query, paths)`)? *(Proposed: no. `grep` has ten knobs; `cat`/`patch` have three. The map stays.)*

---

## Phase 6 — Put the anchor back in `struct_index` and `struct_nodes`

**Rationale.** `7fda0cee2` moved these onto plain line numbers too. With Phase 5 done, `grep` is address-bearing and these two are the last locators that are not — a `struct_index` row still has to be turned into an address by a `cat` before it can be edited. The cost is one `line-hash` per rendered row and 4 characters per key.

It lands LAST because it is the most entangled: unlike grep, these rows are *consumed as data* by each other — `struct_nodes` takes a row's `line` — so the anchor goes **beside** the line and never instead of it, and `line` stays exactly what it is today for every existing caller. Phases 1-5 ship a complete product without it; it is in the first cut anyway, because a locator that is not an address sends the model back through `cat` for the one case `struct_index` exists to make cheap.

**Data.**

```clojure
;; index rows and node entries carry the anchor BESIDE the line, never instead of it.
(s/def :ext.editing.index/anchor     :ext.editing.hashline/anchor)
(s/def :ext.editing.index/end-anchor :ext.editing.hashline/anchor)
```

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `index-one` rows carry `anchor`/`end_anchor` beside `line`/`end_line`; `nodes-tool` entries carry `anchor`.
- Test that proves it done: `editing/core-test/struct-index-rows-are-patch-anchors-test` — an index row's `anchor` feeds `patch` directly.

**Unknowns.**
- Anything downstream parsing these rows as integers — the companion, the TUI, `ctx_renderer`?

---

## State of the plan

**DONE** — every phase landed, verified and committed in one change.

- **Phase 1** — `src/com/blockether/vis/internal/foundation/editing/hashline.clj` (new, pure, no IO):
  `split-content-lines`, `char-offset-at-line`, `line-hash`, `line-anchor`, `anchor->line`,
  `anchor-string?`, `render-hashline-block`, `indices-matching-hash`, `hash-line-drift-tolerance`,
  `parse-anchor`, `resolve-one-anchor`, `resolve-anchor-range`, `resolve-anchor-range-read`,
  `resolve-anchor-edit-span`, plus the `:ext.editing.hashline/*` specs. The old `:hashline-*`
  refusal keywords are now the plan's `:anchor-*` vocabulary, with `:anchor-range-inverted` added
  because an inverted span is reachable and had to be nameable.
  `test/…/editing/hashline_test.clj` — 20 assertions, including the five outcomes one by one.
- **Phase 2** — `cat(path[, from[, to]])`, positional, one file, one string;
  `path, from=None, to=None` is the signature the sandbox actually advertises.
- **Phase 3** — `patch(path, anchor, new)` / `patch(path, from, to, new)`, positional, atomic,
  refusals RAISE with the recovery anchor in the message; the tree-sitter gate refuses a write that
  would break a file that parsed clean and permits one that repairs an already-broken file.
  `:patch-stale-anchor` is classified in `introspection.clj`, and the Clojure pack's
  `clj-patch-no-fail-around` parinfer-repairs the trailing `replacement` and appends
  `(delimiters repaired)` to the status line.
- **Phase 4** — the core prompt routes text edits to `cat`/`patch`, `doc_corpus/curated` lists both,
  and `token-optimization.md` / `index.md` teach the ladder. `env_python.clj` needed no change: the
  hint the plan pointed at no longer names a read/edit verb.
- **Phase 5** — `grep-data` is the pure ordered core and `grep-tool` is now only
  `render-grep-text` over it. Its ~20 shape/paging/hint assertions retarget that core through one
  test helper; the rendering has its own describes.
- **Phase 6** — `struct_index` definition rows carry `anchor`/`end_anchor` beside `line`/`end_line`,
  and `struct_nodes` entries carry the same pair; `line` is untouched for every existing caller.

Verified: `hashline-test` 20/20, `editing/core-test` 241/241, `prompt-test` 25/25,
`ext/language-clojure/core-test` 32/32, `foundation/core-test` 10/10,
`private-deployment-hygiene-test` 1/1; `format_code` run; `lint_code` clean for every touched file
(the 25 boxed-math warnings in `editing/core.clj`'s diff-rendering region are byte-identical at the
parent commit and are not part of this change).

Two deviations from the plan as written, both deliberate:

1. **The core-prompt budget guard moved 5 000 → 5 500 chars** (`prompt_test.clj`), the fourth
   documented raise in that test's history and commented in its voice. Naming two verbs, the anchor
   format and the bottom-up rule does not fit inside what deleting "CHANGING the tree is plain
   Python" freed.
2. **No `loop.clj` change.** The acceptance asked the TUI headline to take line 1 of a string
   result; at runtime a printed STRING has no `"op"`, so `printed-result-op` yields nil, no card is
   built, and `printed-cards-result-render` falls through to the stdout fence — which paints the
   anchored text verbatim. That is the better rendering and needed no code.

Not carried: the journal re-measurement in Phase 4 needs sessions recorded AFTER this change, so the
6.7x / 80% / 41% table is the before-picture until those exist.

**Supersedes** *"Collapse the model-facing tool surface to `python_execution` alone"*, which held this
file through `e51669e06` and whose Phase 6 (`e51669e06:PLAN.md:526-557`) is the decision this plan
overturns. Its phases 1-12 landed and are recorded there; its unfinished cross-cutting cleanup — steps
45-47, 49-53, 56b, 57, 58, 58b, 58c (the `:schema` literals, `form.clj`'s reduction,
`extension_bootstrap.py`, the tool wall, the replay policy, `:tag`, `extending.md`, the language-pack
renderers, and the two shipped-UI evidence steps) — is preserved verbatim at `git show
e51669e06:PLAN.md` and is NOT carried here: it is separate work that takes the root again when it is
picked up.
