# TODO — introspection surface (from the 2026-08-11 → 08-13 session audit)

Five open items, ranked by measured cost. Evidence is from `read_session` over 7 sessions /
61 turns / 2 592 iterations, plus `usage.failures` (115 rows) and live re-checks.

Decide one at a time. Each item stays here until it is either **DONE** (shipped + tested) or
**DROPPED** (with the reason). Nothing here is committed; this file is a working note.

---

## 1. `apropos` must never answer `{}` for a non-empty query

**Status:** open — decision needed
**Cost observed:** 2 dead-ends in-window, still reproducible today.

Terms are ANDed with no fallback, so the query shape a model naturally types dead-ends:

```
apropos("patch edit file text anchor")                      ->  3 hits
apropos("patch from_anchor to_anchor replace edits schema") ->  0 hits   <- still empty
apropos("patch")                                            -> 19 hits
```

In `d4664dc1` t5–t6 the agent burned ~10 iterations here and only got a real answer by
**deliberately breaking a call to read the refusal**.

**Two root causes, not one** (`src/com/blockether/vis/internal/doc_corpus.clj`):

1. `search` line 236 — `(when (every? pos? scores) ...)`. One missed term discards the entry
   BEFORE it is scored, so the exact-name hit (100) never gets to win.
2. `term-score` line 208 — `str/includes?` on the name pays `name-part-hit` **50** for ANY
   substring, so a 1-2 letter noise word is worth half an exact name match:
   `term-score("companion-ui", "a") = 50`, `("companion-ui", "i") = 50`, `("doc", "do") = 50`
   (verified in the REPL against the real private fn).

Cause 1 makes the empty answers; cause 2 makes the NON-empty answers wrong — which is worse,
because nothing tells the caller it missed:

```
apropos("patch")                                            -> 19 hits, patch first        OK
apropos("patch edit file text anchor")                      ->  3 hits, patch first        OK
apropos("patch from_anchor to_anchor replace edits schema")  ->  0 hits                    cause 1
   (patch, replace, edits all hit `patch`; from_anchor/to_anchor/schema do not)
apropos("run tests for one clojure var")                    ->  9 hits, run_tests ABSENT   cause 1
   (killed by `clojure` + `var`; survivors are the long skills)
apropos("how do I replace lines in a file")                 ->  5 hits, patch ABSENT       cause 1+2
   (killed by `how` + `do`; the survivors won on `i`/`a`/`in` scoring 50 each)
```

**Cause 3 — the corpus itself is polluted** (`env_python.clj:1035`). `sandbox-corpus` builds one
document per LIVE PYTHON GLOBAL: `(distinct (concat (names-fn) (sort (keys docs))))`. Its own
docstring says the intent is "a bound function ... a callable the model can type", but the code
takes every non-underscore global. In THIS session 30 of the 194 documents are my own loop
variables and locals — `x`, `q`, `n`, `i`, `k`, `day_set`, `qs`, `dep_lines` — each an empty
5-byte page whose NAME is an exact match worth 100. `doc("x")` -> `"# x\n\n"`, while
`doc("killers")` -> `"# killers  ·  callable"`. That is where the noise-word wins come from.

**Measured on the real 194-document corpus** (prototype over `doc()` text, 8 queries with a known
right answer; rank of that answer, `-` = absent):

```
query                                             now   some+coverage   BM25F
patch                                              #1        #1          #1
patch edit file text anchor                        #1        #1          #1
patch from_anchor to_anchor replace edits schema    -        #2          #1
run tests for one clojure var                       -       #14          #1
how do I replace lines in a file                    -       #23          #1
fold old steps to save context                      -        #5          #1
search other conversations by title                 -       #14          #1
read a pdf into markdown                            -        #2          #1
```

`some` alone (option A/B) is NOT the fix: it stops the empties and still buries the right answer at
#14 / #23, because nothing normalizes for length — the 73 KB skills outscore a 350 B tool page by
containing every word. The single decisive knob is full length normalization on the body field
(`b = 1.0`): with it all 8 land at #1, without it two sit at #3.

**Library survey — nothing to add, and two things already here.**
- Lucene: correct and enormous. SPI codecs + reflection + resource loading are hostile to the
  native image; megabytes of jar for 194 documents of 372 KB. No.
- `com.blockether/fff` 0.12.4 — already pinned (`deps.edn:186`), already used by the picker and
  `grep`. But it indexes a FILESYSTEM: `fff/create` takes a `:base-path`, `fff/search` is fuzzy
  PATH matching and `fff/grep` is content search over files. Tool documents live in Clojure
  metadata and in live Python globals, not in files. Wrong shape; no.
- The algorithm is already in the tree twice: `resources/vis-shims/anydoc.py:1368` `_bm25`
  (k1 1.2, b 0.75) and a Levenshtein at `config_spec.clj:570`. ~60 lines in `doc_corpus.clj`,
  no dependency, native-image-safe.

**Decision: rewrite `search` as BM25F. No new dependency. — DONE, shipped.**
Landed in `doc_corpus.clj` (`tokens`/`edit-distance`/`index`/`bm25f`/`search`, AND removed) and
`env_python.clj` (`callable?` filter — cause 3). Damerau-OSA instead of plain Levenshtein so a
transposition costs 1. Tests: `doc_corpus_test.clj` search-test + every-document-answers-its-own-name
(194/194 self-retrieval at #1); `env_python_test.clj` partial-coverage/typo/empty + loop-variable guard.
`run_tests` 14/14 and 91/91, `lint_code` clean, `format_code` run.

1. Tokenize both sides on non-alphanumerics + camelCase, so `from_anchor` -> `from` + `anchor`
   and identifier queries stop missing.
2. Three fields with weights name 8 / gist 3 / body 1, `k1 = 1.2`, `b = 0.75` for name and gist,
   **`b = 1.0` for the body**.
3. IDF replaces the stoplist: `how`, `do`, `in`, `a` appear nearly everywhere, so their IDF is
   ~0 and they stop steering the result. No hand-maintained English word list.
4. OR by construction — `every?` disappears; a document scores by whatever it matched, so
   `apropos` cannot answer `{}` for a query with any known term.
5. Exact-name tier kept explicitly: whole normalized query = document name adds a flat 100, which
   holds `apropos("patch")` -> `patch` at #1. Measured over all 194 names as single-word queries:
   191/194 stay #1 (the 3 misses are junk-vs-junk collisions from cause 3: `a_`/`a`, `b_`/`b`,
   `n`/`N`), against 193/194 today. A per-term name boost was tried and REJECTED — it re-broke
   two scenario queries.
6. Typo fallback ONLY for a term no document contains (`df = 0`, length >= 4): Levenshtein <= 1
   (<= 2 over 5 chars) against document-name tokens, capped at 2 substitutions.
   `apropos("aprpos")` -> `apropos` #1. A query of entirely unknown words still returns nothing:
   `apropos("kubernetes helm chart rollout")` -> `{}`, which is the honest answer.

**Sub-question resolved: silent.** BM25 has no "degraded mode" to signal — every result is a
ranked partial match, so there is no second shape to distinguish. `{name: gist}` stays.

**Cost:** 372 KB / 194 documents indexes in 83 ms and answers in 7 ms per query in *pure Python*;
the JVM will be well under that. Index memoized on the corpus, which is rebuilt per call.

**Ships with:** cause 3 fixed in the same commit (register only callables + `__vis_docs__` keys,
never every global), and a test pinning the 8 queries above plus the two empty-answer guards.

---

## 2. `doc(name)` must state required arguments, not only the result shape

**Status:** open — decision needed
**Cost observed:** ~10 iterations in `d4664dc1`; one-off repaired since.

08-13 `doc("patch")` was 510 B and described the RESULT only, never the required edit keys.
Today it is 790 B and shows the call shape — but that was a hand repair of one page, not a rule.

**Options**
- A. Corpus invariant + test: every doc page for a callable names its required arguments;
  a test walks the registry and fails on a page that does not.
- B. Generate the argument block from the verb's own schema so it cannot drift.
- C. Leave as-is (documented convention only).

**Decision:** _pending_

---

## 3. Arm the context budget instead of only publishing it

**Status:** open — decision needed
**Cost observed:** the single highest-leverage number in the audit.

`fold_session` returns a turn to a ~57 k floor every time it fires (8 folds in one 744-iteration
turn, −92 k to −303 k tokens each). But **15 of 26 turns ≥20 iterations folded zero times** and
each ended at its own peak (160 k, 152 k, 150 k, 123 k). `session["utilization"]` was read
**0 times** in 2 592 iterations — folding is driven by feel, not by the number the host publishes
for exactly this.

**Options**
- A. Fire a fold nudge off `session["utilization"]` when pressure crosses a threshold.
- B. Inject the turn's own live number into long turns (the agent never reads it unprompted).
- C. Both: publish the number in-band AND arm the nudge.

**Decision:** _pending_

---

## 4. Surface `defs()` where the amnesia actually happens

**Status:** open — decision needed
**Cost observed:** 162 `def` statements, 99 distinct names, **87 redefinitions (54 %)**,
only **1** byte-identical — the other 86 drifted. `defs()` was called **0** times.

Redefinition is turn-aligned: `V` was redefined at the first iteration of t18, t20, t21, t22,
t23, t24. The agent does not believe its `def`s survive the turn boundary. `show` had 10
different notions of how to render a `grep` result inside one session, so no anchor was stable.

**Options**
- A. Surface the live `defs()` list at the first iteration of a turn (in-band, not prompt-only).
- B. Warn on redefinition of a name that already exists with a different body.
- C. Prompt-only wording change.

**Decision:** _pending_

---

## 5. Promote the four re-derived helpers to `.vis/extensions/*.py`

**Status:** open — decision needed
**Cost observed:** 28 combined re-derivations of four missing capabilities.

| helper | defs | distinct bodies | turns | what it really is |
|---|---|---|---|---|
| `gall` | 11 | 9 | 9 | drain `grep`'s `next_offset` pagination |
| `show` | 10 | 10 | 8 | flatten `grep`'s nested `matches` map |
| `V(path,a,b)` | 9 | 7 | 8 | line-numbered read — a reimplementation of `cat` |
| `rt(path,old,new,n)` | 6 | 6 | 3 | assert-count replace — a reimplementation of `patch` |
| `run` / `git` | 3 / 4 | 3 / 4 | 2 / 4 | shell run-to-completion; git with lock retry |

`V` and `rt` are not extensions — they are evidence that `cat` and `patch` were not discovered
(see items 1 and 2). `gall`, `show`, `run`, `git` are genuine missing capabilities.

**Options**
- A. Write all four as one extension file.
- B. Write only `gall` + `show` (pure `grep` ergonomics) and fix `grep` to not need them.
- C. Fix `grep` to return drained, rendered results and write none.

**Decision:** _pending_

---

## Not actions (recorded so they are not re-litigated)

- **Refusal text is the best documentation surface we have.** 89 % of 115 failures recovered on
  the very next step; longest failure run = 3. Keep writing refusals that name the fix.
- **`ntr` was correctly deleted.** Its only load-bearing use was recovering a live shell handle
  across iterations; with one tool on the wire it has zero rows by construction.
- **Result-shape guessing (16 KeyErrors)** is already mitigated by key-naming refusals.


---

## Item 7 — BM25F ranker: remaining shortcomings (measured t9, REPL corpus 27 docs)

| # | Shortcoming | Evidence | Cost |
|---|---|---|---|
| 1 | **`entries` dominates everything.** The corpus is rebuilt from the source Vars on every call; only the DOCUMENTS can be cached, never the Vars. | `dc/entries` 12.1 ms vs `rank` 0.01–0.03 ms | 400–1000x the search |
| 2 | **Typo rescue is the hot path's worst case.** An unknown term scans the length-bucketed vocabulary; cost is linear in unknown terms. | `zzqqxk` 1.41 ms, four-nonsense-word query **5.23 ms** vs 0.02 ms normal | 100–250x |
| 3 | **Rescue answers noise.** Terms in no document silently resolve to a near word, so an off-corpus query returns confident junk instead of nothing. | `wyszukiwanie dokumentow` -> `companion-ui`, `gateway`, `extending` | wrong answers |
| 4 | **No prefix matching.** Interactive/partial handles do not resolve; edit distance to a longer name exceeds budget. | `apro` -> `configuration`, `apropos` absent | discovery dead end |
| 5 | **No stemming.** Singular and plural are different terms. | `run tests` vs `run test` share no document in the top 3 | rank instability |
| 6 | **Non-ASCII is dropped.** `word-run` is `[A-Za-z0-9]+`, so an accented or non-Latin query tokenizes to nothing and falls entirely into rescue. | item 3's query | silent |
| 7 | **No top-k.** `rank` scores, keeps and sorts EVERY positive document; a blank query materializes the whole corpus. | `a the file` -> 27/27 | O(n log n) always |
| 8 | **`exact-name-bonus` is whole-query only.** `patch anchors` loses the +100 that makes `patch` win. | code: `(get (:handles ix) (str/join "_" raw))` | ranking cliff |
| 9 | **Cache is capacity-8 clear-all, key `[count hash]`.** No LRU; one extra corpus shape evicts every warm index. Fingerprint cannot be computed without building the documents first (see 1). | `bm25.clj:229` | rebuild storms |
| 10 | **Weights are compile-time constants.** `field-weight`/`field-b`/`k1` are private `def`s, so a second consumer cannot retune without editing the engine. | `bm25.clj:42-68` | blocks reuse |

**Ordering:** 1 is the only one that is measurably costing anything today; 2+3 are the only ones that can produce a WRONG answer. 4-10 are correctness-neutral polish.

**Decision:** _pending_
