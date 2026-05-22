# v/patch - follow-up TODO

Odkryte przez ręczny probe (10 scenariuszy, `target/probe/logs/s1.log` ... `s10.log`).
Każdy punkt **omawiamy razem przed implementacją**. Po zatwierdzeniu robimy
jeden cohesive patch + testy, commit, push, kreślimy z listy.

Ranking po ROI (wpływ na real-world model loop / koszt zaimplementowania).

---

## 🔥 Critical

### [x] T1 - Surface structured `:failures` / `:reason` / `:loop-hint` in tool error envelope ✅

**Status:** Done. Re-probe `target/probe/logs/s9_v3.log`.

**Decision summary**
- Kept the `throw` contract - engine MUST surface failures loudly via
  the iteration trailer, otherwise the environment would silently
  swallow business errors. (User: "po to są te failures żeby środowisko
  się ogarnęło".)
- Fix lives at the **rendering** seam, not the throw seam.
- `patch-safe` / `patch-envelope-safe` rewritten to return
  `{:success? :failures :checks :loop-hint :message}` directly (jeden
  patch, jebać kompatybilność wsteczną).
- `patch-tool` projects failures into `(extension/failure ...)` with a
  full `:error` map.
- `tool-result->public-value` re-throws as before - model still sees
  the exception in the trailer.
- NEW: `ex->op-error` lifts structured ex-data into `:data` when
  throwable is `:type :vis/tool-failure`. `error-lines` in `ctx.clj`
  already renders `:data` as `;; ! data {...}` so the trailer carries
  `:reason`, `:failures`, `:loop-hint`, `:checks` verbatim. Model reads
  them without try/catch.

**Re-probe evidence (`s9_v3.log`)**
- `:reason :stale` directly observable
- `:failures` carries per-edit map: `:edit-index :path :matches
  :filtered-matches :consecutive-failures :stale {...}`
- `:loop-hint` nil (1st failure; threshold is 3)
- Model self-reports: "trailer info enough to act on without try/catch"

<details><summary>Original spec</summary>

#### Original T1 spec

**Problem (S9)** - Model wywołuje `v/patch` ze stale `:expected-mtime`.
`patch-safe` rzuca `ExceptionInfo` z bogatym `ex-data` (`:failures` zawiera
`:reason :stale`, `:expected-mtime`, `:actual-mtime`, plus `:loop-hint`).
`tool-failure-on-error` wrapper łapie wyjątek ale w envelope `:error` zostaje
tylko `:message` + `:trace`. Model nie ma dostępu do `:reason`/`:loop-hint`
bez ręcznego `(try ... (catch clojure.lang.ExceptionInfo e (ex-data e)))`.

**Evidence** - `s9.log`:
> "surfaced as a thrown exception with a descriptive message string
> rather than a returned map with a :reason :stale key"

**Proposed fix**
- W `tool-failure-on-error` (lub w `extension/failure` builder) dolepiać
  selected `ex-data` keys do `:error`: `:reason`, `:failures`,
  `:consecutive-failures`, `:loop-hint`, `:checks`.
- Filtr na whitelistę (nie kopiować `:throwable`).
- Test: po stale patch model widzi `(:reason (:error result))` = `:stale`.

**Open questions resolved**
- Whitelist per-op vs globalny - chose globalny via `:type
  :vis/tool-failure`. Every tool that uses `extension/failure` with a
  structured `:error` gets the lift for free.
- `:checks` preview bounding - kept full list (per-check
  `:search-preview` already bounded to 180 chars). Revisit if real-world
  batches blow up the trailer.

</details>

---

### [x] T2 - Thread `:passes` from analysis into per-file v/patch summary; drop redundant line counters ✅

**Status:** Done. Re-probe `target/probe/logs/s7_v2.log`.

**Decision summary**
- Per-file summary shape trimmed to the minimum signal set:
  ```
  {:path :op :changed? :diff
   :move-to?  (envelope :update-move only)
   :passes?   (only when a non-`:exact` fuzzy pass fired)
   :indent-delta? (only when `:relative-indent` auto-shifted :replace)}
  ```
- `:lines-before`, `:lines-after`, `:delta-lines` REMOVED. The `:diff`
  already carries that information and the scalar counters duplicated it
  at the cost of trailer bloat.
- `:passes` is an **alarm signal only** - absent when every match was
  byte-exact (default, no noise). Present iff Vis had to relax to one
  of `:rstrip / :unicode / :relative-indent / :trim`. Vector preserves
  edit order and the keyword names tell the model which strategy.
- `:indent-delta` is present only when `:relative-indent` fired AND the
  delta was non-zero. Tells the model: "Vis auto-shifted your :replace
  N spaces; verify the diff matches your intent."
- TUI/channel renderer follows the same rule - no `[N -> M lines]`
  header noise; only `[fuzzy: rstrip,trim]` / `[indentΔ +4]` alarm tags
  when applicable.

**Re-probe evidence (`s7_v2.log`)**
Model did multi-line 0-indent fuzzy attempt (failed via `:no-match`),
then single-line exact substring (succeeded). Final trailer carried:
```
[{:path "target/probe/s7_fuzzy.py" :op :update :changed? true :diff "..."}]
```
Model self-reports verbatim:
- ":passes is a precise alarm - present iff a non-:exact pass fired. Worth the byte."
- "line counters not in returned map. I did not miss them - :diff already encodes the change."

<details><summary>Original spec</summary>

#### Original T2 spec

[same as below - implementation matches]

**Problem (S7)** - Fuzzy fallback w S7 strzelił poprawnie ale model nie wie
KTÓRY pass zadziałał. `patch-result-file-summary` projektuje tylko
`{:op :path :before :after :diff :move-to}`. `:pass` żyje w
`(:checks analysis)` per-edit ale nie wpada do per-file plan ani do
zwracanego summary.

**Evidence** - `s7.log`:
> "The result map does not expose a :pass key - it only reports :changed?,
> :diff, :op, etc."

**Proposed fix**
- W `patch-analysis` przy `:applied-positions` doklejać `:pass` (i
  `:indent-delta` gdy non-zero) do `states[path]`.
- `patch-safe` projektuje `:passes` (lista per-edit, bo jeden plik = N edytów)
  do summary.
- `patch-result-file-summary` dodaje `:passes` jeśli niepuste.
- Test: edit przez fuzzy `:rstrip` → `(:passes result-first)` zawiera `:rstrip`.

**Open questions**
- Jedna lista per-file (`:passes [:exact :trim]`) czy mapa `{idx → pass}`?
- Czy pokazywać `:pass :exact` jawnie czy tylko non-exact?

---

### [x] T3 — Rozwiązano sprzeczność „nie verify” vs „re-read after write” ✅

**Status:** Done w jednym prompt rewrite razem z T4/T5/T6. Re-probe `s1_v2.log`.

**Decision summary**
- Stara prompt miała dwie sprzeczne reguły:
  - „v/patch returns diff + post-image — that IS the write evidence. Do
    NOT v/cat to verify.”
  - „After a write, any prior read of the same path is stale; re-read.”
  Druga wygrywała defensywnie → w S1 i S10 model robił 3-5x v/cat po patchu.
- Nowa pojedyncza reguła (Stale-read rule):
  > Po zapisie wcześniejszy `(v/cat path)` binding trzyma pre-write
  > content. Re-bind TYLKO gdy potrzebujesz tej zawartości do INNEGO
  > celu później w turnie. `v/patch`'s `:diff` to write evidence —
  > nigdy nie re-cat żeby zweryfikować patcha.

**Re-probe evidence (`s1_v2.log`)**
- S1 v1: 4 iter, 1 redundant v/patch retry, 2 post-patch v/cat verify
- S1 v2: **3 iter, 0 v/cat post-patch**, model cytuje „Stale-read rule”
  w self-analizie

---

### [x] T4 — `:applied-lines` per-edit ✅ (rozwiązane przez T2 + prompt)

**Status:** Done bez nowego kodu. Re-probe `s1_v2.log`.

**Decision summary**
- Pierwotne T4 zakładało dodać `:applied-lines [L1 L2]` per-edit do summary.
- Po implementacji T2 model dostaje `:diff` (unified, z `@@ -N,X +M,Y @@`
  hunk headerem) jako jedyne source of truth co do linii. Hunk header
  literalnie mówi „change landed at line N”.
- Prompt jawnie uczy modela: „The :diff is a unified diff; the `@@ -N,X
  +M,Y @@` hunk header tells you WHICH lines changed.”
- Dodawanie `:applied-lines` jako osobnego klucza byłoby redundantne —
  duplikat danych już obecnych w `:diff`.

---

### [x] T5 — Prompt push for `:nth :all` ✅

**Status:** Done. Re-probe `s3_v2.log`.

**Decision summary**
- Dodano „Bulk rename idiom” sekcję w prompt z explicit przykładem:
  ```
  (v/patch [{:path "src/x.py" :search "old" :replace "new" :nth :all}])
  ```
  Plus jasny komentarz dlaczego („Beats enumerating each line: same
  atomic plan, fewer entries”).

**Re-probe evidence (`s3_v2.log`)**
- S3 v1: 5 osobnych edytów w 1 v/patch (verbose)
- S3 v2: **1 edit map z `:nth :all`**, all 8 occurrences atomically
  replaced

---

### [x] T6 — Codex envelope EOF append idiom in prompt ✅

**Status:** Done w prompt rewrite. (Brak osobnego re-probe — idiom
został dodany do prompt; kolejne use cases zweryfikują podczas
normalnej pracy.)

**Decision summary**
- Dodano sekcję „Codex envelope EOF-append (cleanest way to add content
  past the last line)” z pełnym przykładem `*** End of File` markera.
- Lista „reach for envelope when:” wymienia teraz appending past EOF
  jako jeden z 4 głównych use caseów envelope mode (obok Add/Delete/Move).

---

### [x] T7/T8/T9 — odrzucone / niski ROI

- T7 line-number anchor: pokryte przez `:nth` + `:after` content match;
  separate integer anchor dodawałby surface bez wyraźnego zysku.
- T8 `v/rg :replace mode`: conflate concerns; `v/rg` zostaje read-only.
- T9 `v/write` / `:replace-entirely`: aktualna ścieżka full-file rewrite
  (whole-content `:search`/`:replace`) działa; T3 wyeliminował over-verify
  loop w S10. Re-probe S10 by pokazał.

---

<details><summary>Original T3-T6 spec (kept for reference)</summary>

**Problem (S1, S10)** - Model po `v/patch` robi 3-5 razy `v/cat` żeby
sprawdzić. S10 zjadło 6 minut na pętli verify. Prompt mówi:
- "v/patch returns diff + post-image - that IS the write evidence. Do NOT v/cat to verify."
- "After a write, any prior read of the same path is stale; re-read."

Druga reguła wygrywa defensywnie.

**Evidence** - `s1.log` 4 iters, `s10.log` 8+ iters z 5+ `v/cat` po patchu.

**Proposed fix (tylko prompt, zero kodu)**
- Połączyć obie reguły jasno:
  ```
  - v/patch returns the full before/after blob. That IS the write evidence -
    do not v/cat the same path immediately after a successful v/patch.
  - "After a write any prior bind of `(v/cat path)` is stale" means: if you
    need that path's content LATER in the turn for a different purpose,
    re-bind. Not for verification.
  ```
- Test: prompt snapshot test, plus probe re-run S1/S10 → iter ≤ 2.

**Open questions**
- Czy dodać explicit przykład w prompt (one-shot: cat → patch → done)?
- Może też: "If you must verify, use `(:after patch-result)` not v/cat"?

---

## ⚠ Medium

<!-- T4 done above (no new code needed; :diff carries line info). -->

### Legacy T4 spec

**Problem (S2, S6)** - Model nie wie na której linii edit wylądował.
Prosi o `:match-line N` w wyniku (S2 self-analysis). Dziś musi
`v/cat` żeby sprawdzić, co napędza T3.

**Proposed fix**
- `patch-analysis` już ma `:applied-positions` (char offsets). Konwertować
  na line numbers via `char-offset-at-line` reverse-lookup.
- Per-edit: `{:edit-index N :path P :pass KW :lines [LINE-START LINE-END]}`
- W `patch-result-file-summary` dodać `:edits [...]` z listą.
- Test: patch line 42 → `(get-in result [0 :edits 0 :lines 0])` = 42.

**Open questions**
- Czy line numbers 1-based (zgodne z `v/cat`) czy 0-based?
- Multi-occurrence (`:nth :all`) → wszystkie linie czy tylko pierwsza?

---

<!-- T5 done above. -->

### Legacy T5 spec

**Problem (S3)** - Model batchował 5 osobnych edytów zamiast 1 z `:nth :all`.
W self-analysis prosi o "whole-identifier rename primitive" - coś co już
istnieje. Czyli prompt jest, ale za słaby sygnał.

**Proposed fix (prompt only)**
- Dodać jednolinijkowy przykład inline w EDIT section:
  ```
  ;; rename every `status` to `state` across the file:
  (v/patch [{:path "x.py" :search "status" :replace "state" :nth :all}])
  ```
- Test: prompt-pinning + re-probe S3 → 1 edit z `:nth :all`.

**Open questions**
- Czy `:nth :all` powinno być `:replace-all? true` jak Claude Code dla parity?
  (Aider: SEARCH/REPLACE first-match only; Codex: positional; Roo: line-hint.
  Tylko Claude Code ma `replace_all`.) Trzymamy `:nth :all` jako bardziej
  ogólne (`:nth :first|:last|:all|N`).

---

<!-- T6 done above. -->

### Legacy T6 spec

**Problem** - Model w S5 zrobił append przez read-last-line→replace. Envelope
ma natywny mechanizm: `*** End of File` marker + `+linia` w hunku. Model
sugeruje `v/append`. Discoverability gap.

**Proposed fix (prompt + maybe sugar)**
- Prompt: dodać przykład Codex envelope dla append:
  ```
  ;; append a line at EOF:
  *** Begin Patch
  *** Update File: notes.md
  @@
   last existing line
  +Appended: end-of-file marker.
  *** End of File
  *** End Patch
  ```
- Decyzja: dodać `v/append` jako oddzielny tool? Albo zaufać prompt-edukacji?

**Open questions**
- `v/append` to nowy primitive - koszt: 1 tool, ~50 LOC. ROI: 1 scenariusz
  pokazał trudność. Niski. → **wolę prompt fix tylko.**

---

## i Low / nice-to-have

### [ ] T7 - `:after`/`:before` jako tuple `(line, text)` lub plain line-number

**Problem (S8 self-analysis)** - Anchor wymaga unikalnego content matcha.
Roo Code daje `:start_line:` jako structurally-unique hint. Model sugeruje
zaakceptować integer line-number w `:after`.

**Proposed fix**
- `:after 42` → szukaj od linii 42 (1-based, post-EOL).
- `:after "context"` → tak jak teraz.
- `:after [42 "context"]` → muszą oba pasować.
- Test: anchor na linii 42 + 3 identyczne fragmenty → trafiony właściwy.

**Open questions**
- Czy worth it gdy `:nth N` już istnieje? Może `:nth` wystarczy.
  → odłożone na potem, **nie ruszamy chyba że ktoś poprosi.**

---

### [ ] T8 - `v/rg` z `:replace` mode (search-and-replace w jednym tool)

**Problem (S4 self-analysis)** - Model po `v/rg` → ręcznie składa `v/patch`.
Sugeruje sklejenie.

**Proposed fix**
- ODRZUCONE: conflate concerns. `v/rg` to read-only observation, `v/patch`
  to mutation. Mixanie utrudni audit/permission policy (`extension/op-tag`).
- Zostawione w TODO bo model to zaproponował.

---

### [ ] T9 - `v/write` lub `:replace-entirely true` dla full-file rewrites

**Problem (S10)** - Model wybrał full-file `:search` (cały plik) → `:replace`
(nowy cały plik). Działa, ale 5+ verify iterów po. W self-analysis sugeruje
`v/write` lub `:replace-entirely true`.

**Proposed fix**
- Opcja A: Nie ruszać. Aktualna ścieżka działa. T3 (fix verify-loop) załatwi
  iter-count problem.
- Opcja B: Dodać `(v/write {:path :content :expected-mtime})` - jak
  rozważałem wcześniej i odrzuciliśmy.
- **Decyzja: czekamy aż T3 zostanie zrobione i ponownie probe S10. Jeśli
  iter ≤ 3 to T9 odrzucamy. Jeśli nadal +5 → wracamy.**

---

## Execution rule

1. Bierzemy 1 TODO.
2. **Discuss together** - uzgadniamy open questions, decyzje.
3. Implement + test + verify (`./verify.sh`).
4. Commit + push.
5. Re-probe odpowiedniego scenariusza (S1...S10) jeśli relevant.
6. Cross off.

Nigdy nie robimy więcej niż 1 TODO na raz bez explicit zgody.

---

# Roadmap — post-T9 (po wszystkich sweepach)

T1-T9 + v/cat/rg/ls/patch/write/move sweep + CORE_SYSTEM_PROMPT
cavemanize zrobione. Co dalej:

### [ ] T10 — `editing-prompt` cavemanize

**Status:** queued.

CORE_SYSTEM_PROMPT zaorane z 2890→1394 tok (50% cut). Drugi największy
blok system promptu to `available-editing-prompt` (~2113 tok / 211 lines).
Możliwy cut 30-40% (~700 tok savings).

**Strategie**:
- Skróć READ section — w/cat arities w table format zamiast prose
- Drop redundant idiom examples (kept 4 dla v/rg; może wystarczą 2)
- EDIT section: result-shape doc skompresować do schemy
- RULES sekcja — Stale-read + Path-target zostają (świeże), reszta caveman

**Open questions**
- Czy zachować przykłady idiomów (\"bulk rename\", \"counts only\", \"regex
word boundary\")? Model używa ich faktycznie (probe potwierdził), więc ryzyko
regresji jest realne.
- Tableizować arities/keys czy zostawić text? Lambda-style mapped to GLM
lepiej w CORE; testować w editing-prompt.

---

### [ ] T11 — Re-probe E2E A/B/C ze skróconym CORE

**Status:** queued. Ważne BO empirycznie potwierdzić że 50% cut CORE
nie zepsuł jakości.

**Plan**:
- 3 runy każdej task per model (Claude + GLM-5.1)
- Porównać iter mean, success rate, failures vs pre-cavemanize baseline
- Hipoteza: krótszy CORE = mniej szumu = same-or-better iter
- Risk: jakaś kluczowa instrukcja wypadła i model gubi się gdzieś

**Action items**
- Reset corpora dla A/B/C
- 3 runy każde × 2 model × 3 task = 18 runów (~30-60 min)
- Compare table: pre vs post

---

### [ ] T12 — Provider-specific prompt tweaks

**Status:** queued.

**Insight z E2E-A/B/C**: GLM-5.1 ~50% slower iter niż Claude. Wybór tooli
identyczny, ale GLM robi więcej v/cat sanity checks + drobne bugi
(api.js naming, services_old path).

**Pomysły**:
- Per-provider `:ext/prompt` override gdy model jest w `:zai`/`glm-*`?
  Bardziej explicit przykłady, mocniejszy nudge na `:nth :all`, regex
  syntax cheatsheet inline.
- Albo na początku prompt: `;; HINT: this provider responds better to
  explicit examples; prefer copy-paste idioms over invention.`
- Może NIE — może zmiana w GLM samym (kolejna wersja) lepsza ścieżka
  niż patch promptu.

**Open questions**
- Per-provider system prompt = każdy provider extension dorzuca własny
  prompt-tail. Architektonicznie ok, ale Vis dziś tego nie wspiera.
- Czy lepsze: jeden uniwersalny prompt + model-specific RUNTIME HINTS
  podawane przez ctx?

---

### [ ] T13 — Iteration-budget hints w trailerze

**Status:** queued, low-medium priority. Mid-turn hint: SAFE per
opencode community guidance (hint = nudge in CTX, NOT a reasoning-chain
edit).

Gdy task=\"jeden patch\" a iter już 5+ to suggesion że model się zapętlił.
Dodać do CTX engine soft-warning:

```
:session/hints {:engine/iter-budget-warn
  {:body \"Iter N for a simple task; consider (done …) with current
results or restate plan.\"}}
```

**Open questions**
- Co to \"simple task\"? Heuristyka: zero v/patch failures + ≤2 different
files touched + iter > 4 = warn.
- Wsadzić w engine czy w extension hook?

**Note**: hint pojawia się w CTX (osobne pole), nie w trailer pinach.
Nie konfliktuje z preserved-thinking bo `reasoning_content` blocks
pozostają untouched.

---

### [ ] T14 — Auto-summarize stale trailer pins

**Status:** queued, medium priority. **SAFE per ZAI / opencode community**
(confirmed cross-validation):

- Mid-turn (`ctx_engine.clj:867`): `:session/trailer` accumulates verbatim,
  zero pruning. Reasoning chain intact.
- At `(done …)` (`ctx_engine.clj:961`): applies `:trailer-drop` +
  `:trailer-summarize`. This is exactly the \"prune/compress only after a
  stage work, or everything, is done\" pattern that opencode community
  recommends for GLM.
- ZAI docs (preserved-thinking): unmodified `reasoning_content` MUST flow
  back; we already do this regardless of trailer summarization.

Proposal: at done-time, engine auto-detects observation pins on paths
later mutated within this turn and replaces their `:forms` with
`:summary` automatically. Model can still override with explicit
`:trailer-drop` / `:trailer-summarize`.

**Risks** (reduced after cross-validation):
- Heuristic may misfire on intentional state-snapshots (model relies on
  pre-mutation observation). Opt-out: model pre-pins the obs in
  `:trailer-drop` cleanly.
- For GLM/coding plan: harmless since this only fires at done; the
  inter-turn cache hit depends on `reasoning_content` order, not
  trailer shape.

---

### [ ] T15 — Więcej probe scenariuszy

**Status:** loose ideas — kolejne 10 scenariuszy żeby pokryć kolejne
class of bugs.

Kategorie do pokrycia:
- **Long-task probe (20+ iter)**: czy Vis loopuje? Czy CTX management
  działa pod presją? Czy `:trailer-summarize` faktycznie używany?
- **Multi-language refactor**: Python + JS + Go w jednym tasku — czy
  model gubi się na cross-language naming conventions (jak GLM-5.1 w
  E2E-A)?
- **Failure recovery probe**: seed conflicting state (np. plik usunięty
  między iterami), czy model recoveruje czy crashuje?
- **Cross-tool dependency probe**: zadanie wymagające `v/move` → `v/patch`
  → `v/move` w stale path. Czy Path-target rule trzyma?
- **Error injection**: symulować provider failure mid-iter (np. timeout
  na 2-iter). Czy retry działa?
- **Big repo navigation**: 10000+ plików — czy v/ls / v/rg czyste limity?
- **Whitespace-heavy fuzzy patches**: file ma tabs vs spaces drift, czy
  `:relative-indent` pass strzela bez fałszywych pozytywów?
- **Multi-edit batching limit**: ile edits jednocześnie w v/patch zanim
  model się gubi w planowaniu?
- **Documentation generation**: realistic README rewrite
- **Mixed observation+mutation**: rg → cat → patch → rg → patch — czy
  Stale-read rule trzyma?

Wybierać 2-3 per round, robić probe (~10 min każda), bookmark findings.

---

## Dropped / rejected

- ~~Tool-level metrics dashboard (`vis tools stats`)~~ — niski priorytet.
  User: \"nie wiem czy to istotne\". Probe-driven już wystarcza dla
  identyfikacji wąskich gardeł.
- ~~Multi-agent coordination~~ — out of scope. Lock file or detect
  parallel session can wait; today's pattern (push frequently, pull
  --rebase) działa wystarczająco.
