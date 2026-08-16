# TODO — introspection surface (from the 2026-08-11 → 08-13 session audit)

Five open items, ranked by measured cost. Evidence is from `read_session` over 7 sessions /
61 turns / 2 592 iterations, plus `usage.failures` (115 rows) and live re-checks.

**State after t21: every item is DONE or CLOSED.** 1 · 7 · 8 shipped the ranker and the row;
2 shipped the call line, the keys line and the result contract; 3 · 4 · 5 closed on re-measured
evidence — the behaviour they were written about is gone.

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

**Status:** DONE — shipped t15 `4eb49268f`, t18 `b2a63f670`, t19 `ebf0441cd`, t21 (this turn)
**Cost observed:** ~10 iterations in `d4664dc1`; one-off repaired since.

08-13 `doc("patch")` was 510 B and described the RESULT only, never the required edit keys.
Today it is 790 B and shows the call shape — but that was a hand repair of one page, not a rule.

**Options**
- A. Corpus invariant + test: every doc page for a callable names its required arguments;
  a test walks the registry and fails on a page that does not.
- B. Generate the argument block from the verb's own schema so it cannot drift.
- C. Leave as-is (documented convention only).

**Live re-measure (t14, this sandbox, 35 tool callables):**
- **17 of 35 pages never show a single call** — `run_tests`, `lint_code`, `format_code`, `repl*`,
  `search*`, `struct_index`, `struct_patch`, `find`, `find_files`, `download_*`. `run_tests`' page
  never names `language`, which the harness prompt has to teach instead.
- **14 of 35 answer `(*a, **k)` to `inspect.signature`** — `grep`, `doc`, `apropos`, `fold_session`,
  `lint_code`, `struct_nodes`, `struct_patch`, `monorepo`, `languages`, `repositories`, `find*`.
- **4 are blank on BOTH doors**: `find`, `find_files`, `lint_code`, `struct_patch`.
- The data for B already exists and nothing renders it: `vis/symbol` carries `:call`
  (`{:pos ["path" "edits"]}`, `{:lead-opt "language" :rest :always}`) and a corpus entry already
  has a `:call` slot (`doc_corpus.clj:16,498`) that the page never prints.

**Decision: B — the page RENDERS the call from the entry, so it cannot drift. DONE.**

- `doc(name)` opens with `name(args)` (from `:call`, else the real arglists) and prints
  `Keys: a (REQUIRED) · b (note)` from a new `:params` vocabulary — every engine-bound tool, every
  extension tool (t15). Required keys were PROBED, never read off a docstring.
- Both lines are STRUCTURE, rendered beside the document: writing them INTO the text hijacked the
  first-line BM25F field and dropped `patch` from rank 1 to 19 for its own defining ask (t16).
- The core prompt now spells the call shape of `grep`, `struct_index`, `struct_nodes` beside `cat`
  and `patch` — 8 shape errors in 326 code-verb calls, 3 hard refusals (t18).
- `extending.md` shows the rendered page and the `apropos` row it produces, with the rules that
  earned them (t19).

**t21 — the mirror half: the RESULT that names no key.** 22 of the 23 bound tool pages already named
their own; `run_tests`, the tool every verification goes through, answered *"execution metadata,
counts/details, output, timeout, and REPL-recovery diagnostics"* for a 30-key map, so a caller had to
print the map to find `is_pass`. It now names the verdict, the six counts, the fault rows
(`test,type,message` plus `ns`/`file`), `output` and the REPL-recovery flags — page 640 B → 1 045 B.
`extension_test` fails any contract that names fewer than two of its keys unless it answers plain
text; measured with that rule, `run_tests` was the ONE violator (0 keys named).

---

## 3. Arm the context budget instead of only publishing it

**Status:** closed by the engine — verify, do not rebuild
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

**Runtime check (t14):** both halves already ship. `ctx_engine/over-budget-hint` (there since
`e08f42912`, 2026-07-24) escalates at 75% / 90% of `auto_compress_above` and names `fold_session`,
the settled boundary and the evidence to keep; `session_utilization` carries the live in-band line
(`context 5% · saved …`) every iteration. The audit's 0 reads were a BEHAVIOUR gap, not a missing
surface — nothing left to build here.

**Decision:** C, already shipped — item closed.

---

## 4. Surface `defs()` where the amnesia actually happens

**Status:** CLOSED on fresh evidence (t21) — the measured cost is gone, no code needed
**Cost observed:** 162 `def` statements, 99 distinct names, **87 redefinitions (54 %)**,
only **1** byte-identical — the other 86 drifted. `defs()` was called **0** times.

Redefinition is turn-aligned: `V` was redefined at the first iteration of t18, t20, t21, t22,
t23, t24. The agent does not believe its `def`s survive the turn boundary. `show` had 10
different notions of how to render a `grep` result inside one session, so no anchor was stable.

**Options**
- A. Surface the live `defs()` list at the first iteration of a turn (in-band, not prompt-only).
- B. Warn on redefinition of a name that already exists with a different body.
- C. Prompt-only wording change.

**Re-measured t21 — 60 recent gateway journals, 895 unique sandbox blocks:**

| | audit (08-11 → 08-13) | t21 |
|---|---|---|
| `def` statements | 162 | 59 |
| redefinitions | **87 (54 %)** | **8 (14 %)** |
| byte-identical re-derivations | 86 of 87 drifted | **0** — every redefinition today refines |
| `defs()` calls | **0** | **22** |

**Decision: none of A/B/C — closed by what already shipped.** The prompt rule (`defs()` lists them,
`defs(name)` reads one back, a `def` outlives the block, the turn and a gateway restart) plus t20's
docstring surface (`851ac835f`: one docstring line is the `defs()` gist, the whole of it the
`doc(name)` page, and what `apropos` finds it by) moved the behaviour. B is refuted by the data: a
redefinition warning would fire on 8 refinements and 0 re-derivations — pure noise.

---

## 5. Promote the four re-derived helpers to `.vis/extensions/*.py`

**Status:** CLOSED on fresh evidence (t21) — three of the four helpers no longer exist
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

**Re-measured t21 — the same 60 journals, deduped:**

| helper | audit | t21 | why |
|---|---|---|---|
| `V(path,a,b)` | 9 defs | **0** | `cat` is discovered — `show me lines 10 to 40 of a file` answers it first |
| `rt(path,old,new)` | 6 defs | **0** | `patch` is discovered — `replace lines in a file` answers it first |
| `gall` | 11 defs | **0** | `grep` drains its own pagination and answers anchored TEXT |
| `show` | 10 defs | 4 defs / 3 sessions | flattens a `{"matches": …}` map `grep` NO LONGER RETURNS |
| `run` / `git` | 3 / 4 | 5 one-line shell wrappers | all re-derive `print((await shell(cmd)).wait(secs)["stdout"])`, which `doc("shell")` prints verbatim |

**Decision: C — fix the tool and its page, write no extension.** Nothing goes into
`.vis/extensions/*.py`: the four capabilities are either obsolete or already one documented line, and
an extension would freeze a wrapper around a contract that keeps improving. What remains is discovery,
measured at HEAD: `run a shell command and get its output` / `wait for a command to finish` /
`read the last lines of a running command` all answer `shell` first; `stop a process I started`
answers `repl_stop` first and `shell` fifth — a near-tie, logged, not fixed.

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

**Decision: fix all ten (t10). DONE — shipped, tested, lint clean.**

| # | How it was fixed | Where |
|---|---|---|
| 1 | `register-source!` takes a **stamp** — a cheap 0-arity freshness token — and `entries` memoizes the whole corpus under the stamp vector, answering the IDENTICAL vector while nothing changed. Stamps: `docs/generation` (new), `discovery/generation` (new), MCP visible-servers + cached tool counts (no RPC). `docs/collect` itself is memoized on a two-stage stat pass (manifest marks, then page marks), which retired `site-cache` + `*live-reload?*`. | `doc_corpus.clj`, `docs.clj`, `discovery.clj`, `foundation/mcp/core.clj` |
| 2 | Rescue searches only the term's own **first-letter bucket**, pre-filters on length before the DP, bails on a row that already exceeds budget, and stops at the first distance-1 hit. | `bm25.clj` `nearest` |
| 3 | Rescue answers only a **real** correction: budget 1 under 7 chars / 2 above, same-length preferred. A query no document covers answers `[]`. | `bm25.clj` `resolve-term` |
| 4 | A term nothing carries is **completed as a prefix** (>= 3 chars, shortest completion) before it is spell-corrected. | `bm25.clj` `complete-prefix` |
| 5 | `terms` = tokens + a plural fold (Porter 1a subset), applied to the index AND the query; the handle stays unstemmed. | `bm25.clj` `stem` |
| 6 | Tokenization is `Character/isLetterOrDigit` + camelCase, no regex — every script tokenizes. | `bm25.clj` `tokens` |
| 7 | `:limit` on `rank`/`search`, served by a bounded `PriorityQueue`. `apropos` caps a described ask at **25** (the empty listing stays whole): a six-word ask used to answer ~80 of 146 documents. | `bm25.clj` `top-k`, `env_python.clj` |
| 8 | The whole-query bonus became a **handle-subset** bonus: any document whose handle tokens are all in the resolved query scores `bonus * (handle-tokens / query-terms)`. `patch anchors` -> `patch` #1. | `bm25.clj` `add-handle-bonus!` |
| 9 | True **LRU**: an `AtomicLong` clock per entry, evicting only the least-recently-used one at capacity. Options are part of the key. | `bm25.clj` `evict-lru!` |
| 10 | `default-opts` is public — `:k1`, `:field-weights`, `:field-b`, `:handle-bonus` — merged into every index and part of the cache key; `doc-corpus/search` passes an opts map through. | `bm25.clj`, `doc_corpus.clj` |

**Measured, same 146-document sandbox corpus / 27-document JVM corpus:**

| | before | after |
|---|---|---|
| `dc/entries` (per `apropos`/`doc`) | 12.9 ms | **1.9 ms** |
| `docs/collect` (also per `/docs` request) | 8.38 ms | **0.37 ms** |
| index build (only on a real change) | 17.1 ms | **9.1 ms** |
| rank, warm | 0.032 ms | 0.030 ms |
| typo `pathc` | 0.164 ms | **0.023 ms** |
| four-nonsense-word query | 4.90 ms | **0.159 ms** |
| rows for a six-word ask | ~80 | **25** |

**Verified:** `bm25_test` + `doc_corpus_test` + `docs_test` + `env_python_test` + `discovery_test` + `mcp/core_test` = **195 pass**, `lint_code` clean, `format_code` run. A real bug was found by the new tests and fixed: `evict-lru!` called `val` on a Clojure vector, so ANY cache eviction threw.


---

## Item 8 — the DATA `apropos` eats and answers (measured t11, fresh JVM over the real 146-doc dump)

Engine is fine; the corpus rows and the payload are the weak part.

**Input** — 146 documents from the sandbox source: 101 with text (383 KB), **45 name-only**.
Median 449 B, but the 8 largest (skills + pages) are 250 KB = 65 % of the bytes.
Document as the ranker sees it: `name` (w 8) / `gist` = first non-blank line (w 3) / `body` (w 1).

**Output** — `{name: gist}`, rank order, cap 25. Listing 101 rows = **27 675 B**; a described ask = **4.2–9.3 KB**;
the row actually wanted is **2.4–23 %** of it.

**Retrieval quality (good, keep):** self-rank by own first line 91/101 at #1, 100/101 top-3, 101/101 top-10.
Right answer #1 in 8 of 9 realistic asks; the miss is `take a screenshot of a website` → `impeccable` #1, `spel` #3.

**Defects, all in the data:**

1. **Gist is unbounded.** median 190 B, p90 519 B, max **1857 B** (`anydoc`'s row IS its whole document);
   22 of 101 rows > 400 B. Cap at 140 chars: listing 27.7 → **12.7 KB**, per-query 7.5 → **3.1 KB**.
   *Decision: _pending_*
2. **13 pages ship a breadcrumb as their gist** (`Drafts · Using Vis`) while line 3 holds the real description
   (`Isolated workspaces for speculative changes: create, apply, park, resume…`). Measured: readability win,
   ranking-neutral, and on one query slightly WORSE (`reporting-bugs` overtakes `patch` — its description carries
   "file"). So take it for the text, not for the ranking. *Decision: _pending_*
3. **25 rows where 1–5 matter.** Cutting at 30 % of the top score keeps 1/3/5/5/13 rows for 5 of 6 asks
   (168 B–4.5 KB) but fails on the all-common-words ask (26 rows, flat decay). Hard cap 8 → ~1 KB, and the answer
   was in the top 4 in every ask measured. *Decision: _pending_*
4. **Name-only rows survive the cause-3 fix.** A `def` in a block is callable, so the agent's own helpers
   (`all_blocks`, `killers`, `rank_p`, …) stay documents with an EMPTY gist — ~30 of them here, and `defs()`
   already lists them. *Decision: _pending_*

**Live warning:** the running gateway predates `84b53c5d8` — its `apropos` still ANDs (`patch from_anchor …` → `{}`),
does not spell-correct (`pathc` → `{}`), carries 198 junk globals, and its own `doc('apropos')` still says
"terms are ANDed". On-disk source and tests are the new engine; a gateway restart picks it up.

---

## Item 8 — DECIDED and SHIPPED (t12): a hit is a ROW, not a gist

`apropos(query)` answers `{name: {kind, gist, at, hit}}`. One decision covers all four defects: the row
describes the MATCH, and the body is never in it because `doc(name)` answers one whole.

| field | what it is | why |
|---|---|---|
| `kind` | closed vocabulary `tool` · `shim` · `page` · `skill` · `mcp` · `local` | what the document IS decides what to DO with it — call it or read it. Defect 4. |
| `gist` | up to 3 bounded parts joined by ` … `: the document's OPENING (its first line, plus the line under it when the first is a breadcrumb), the MATCHED region below what the opening showed, and a fragment from DEEPER down when a strong term recurs 400+ chars later | Defects 1 + 2 at once: bounded, and a breadcrumb page finally says what it is |
| `at` | 1-based line the matched region starts on, 0 when the opening held the match | a 70 KB skill is read from where it answers |
| `hit` | up to 3 resolved terms, a rewrite rendered `pathc→patch` | the ranker completes prefixes and corrects typos; silent rewriting was a documented iteration sink |

**Decision 1 (unbounded gist): SHIPPED.** `doc-corpus/preview` — opening 110 chars, match window 90 (lead-in 35),
tail 60 (lead-in 12). Rows are 110–300 chars whatever the document weighs.

**Decision 2 (breadcrumb pages): SHIPPED, in the PREVIEW, not the data.** An opening under 45 chars takes the next
non-blank line with it (`Drafts · Using Vis — Isolated workspaces for speculative changes.`). Page text is
untouched, so ranking is unchanged — which t11 measured as the safer half of the trade.

**Decision 3 (25 rows): SHIPPED as 10, not 8.** Every measured ask answered inside the first four rows; 10 leaves
slack for a query the corpus covers broadly. `apropos('')` is a LISTING and stays whole and uncapped.

**Decision 4 (name-only rows): SHIPPED as `kind: "local"` + not searchable.** An undocumented callable has no text
to answer with, and its handle (`vars`, `where`, `hits`) is a common English word that won rows off real contracts
— `vars` ranked #2 for "run tests for one clojure var". A described ask now searches only documented entries; the
listing still shows every callable, labelled `local`, so nothing the model can type is invisible.

**Measured (146-doc dump, fresh JVM):**

| ask | before `{name: gist}` cap 25 | after `{name: row}` cap 10 |
|---|---|---|
| how do I replace lines in a file | 7 905 B | **2 730 B** |
| run tests for one clojure var | 7 147 B | **3 301 B** |
| read a pdf into markdown | 9 465 B | **2 431 B** |
| fold old steps to save context | 9 405 B | **2 846 B** |
| search other conversations by title | 4 833 B | **2 334 B** |
| patch from_anchor to_anchor replace edits schema | 8 837 B | **2 940 B** |
| `apropos('')` listing, 146 rows | 28 512 B | **18 667 B** |

~3× smaller *and* it carries the excerpt, the line and the matched terms. Search + preview for 10 rows: 0.92 ms
warm (rank itself is 0.03 ms; the rest is lower-casing bodies for the windows).

**Shipped in:** `src/com/blockether/vis/internal/doc_corpus.clj` (`preview`, `kinds`), `bm25.clj` (`rank` answers
its resolved terms as metadata), `env_python.clj` (row assembly, `apropos-limit` 25 → 10, `local` filter,
`__vis_kinds__` seeding), `foundation/mcp/core.clj` (`:kind "mcp"`), `resources/vis-docs/token-optimization.md`.
Tests: `bm25_test` (resolved-term metadata), `doc_corpus_test` (preview: 3 parts, `at`, breadcrumb, bounds,
correction), `env_python_test` (row shape end-to-end, `local` excluded from a described ask, listed in the listing).
