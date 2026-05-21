# v/patch — follow-up TODO

Odkryte przez ręczny probe (10 scenariuszy, `target/probe/logs/s1.log` … `s10.log`).
Każdy punkt **omawiamy razem przed implementacją**. Po zatwierdzeniu robimy
jeden cohesive patch + testy, commit, push, kreślimy z listy.

Ranking po ROI (wpływ na real-world model loop / koszt zaimplementowania).

---

## 🔥 Critical

### [x] T1 — Surface structured `:failures` / `:reason` / `:loop-hint` in tool error envelope ✅

**Status:** Done. Re-probe `target/probe/logs/s9_v3.log`.

**Decision summary**
- Kept the `throw` contract — engine MUST surface failures loudly via
  the iteration trailer, otherwise the environment would silently
  swallow business errors. (User: "po to są te failures żeby środowisko
  się ogarnęło".)
- Fix lives at the **rendering** seam, not the throw seam.
- `patch-safe` / `patch-envelope-safe` rewritten to return
  `{:success? :failures :checks :loop-hint :message}` directly (jeden
  patch, jebać kompatybilność wsteczną).
- `patch-tool` projects failures into `(extension/failure ...)` with a
  full `:error` map.
- `tool-result->public-value` re-throws as before — model still sees
  the exception in the trailer.
- NEW: `ex->op-error` lifts structured ex-data into `:data` when
  throwable is `:type :vis/tool-failure`. `error-lines` in `ctx.clj`
  already renders `:data` as `;; ! data {…}` so the trailer carries
  `:reason`, `:failures`, `:loop-hint`, `:checks` verbatim. Model reads
  them without try/catch.

**Re-probe evidence (`s9_v3.log`)**
- `:reason :stale` directly observable
- `:failures` carries per-edit map: `:edit-index :path :matches
  :filtered-matches :consecutive-failures :stale {…}`
- `:loop-hint` nil (1st failure; threshold is 3)
- Model self-reports: "trailer info enough to act on without try/catch"

<details><summary>Original spec</summary>

#### Original T1 spec

**Problem (S9)** — Model wywołuje `v/patch` ze stale `:expected-mtime`.
`patch-safe` rzuca `ExceptionInfo` z bogatym `ex-data` (`:failures` zawiera
`:reason :stale`, `:expected-mtime`, `:actual-mtime`, plus `:loop-hint`).
`tool-failure-on-error` wrapper łapie wyjątek ale w envelope `:error` zostaje
tylko `:message` + `:trace`. Model nie ma dostępu do `:reason`/`:loop-hint`
bez ręcznego `(try ... (catch clojure.lang.ExceptionInfo e (ex-data e)))`.

**Evidence** — `s9.log`:
> "surfaced as a thrown exception with a descriptive message string
> rather than a returned map with a :reason :stale key"

**Proposed fix**
- W `tool-failure-on-error` (lub w `extension/failure` builder) dolepiać
  selected `ex-data` keys do `:error`: `:reason`, `:failures`,
  `:consecutive-failures`, `:loop-hint`, `:checks`.
- Filtr na whitelistę (nie kopiować `:throwable`).
- Test: po stale patch model widzi `(:reason (:error result))` = `:stale`.

**Open questions resolved**
- Whitelist per-op vs globalny — chose globalny via `:type
  :vis/tool-failure`. Every tool that uses `extension/failure` with a
  structured `:error` gets the lift for free.
- `:checks` preview bounding — kept full list (per-check
  `:search-preview` already bounded to 180 chars). Revisit if real-world
  batches blow up the trailer.

</details>

---

### [ ] T2 — Thread `:matched-pass` from analysis checks into per-file v/patch result

**Problem (S7)** — Fuzzy fallback w S7 strzelił poprawnie ale model nie wie
KTÓRY pass zadziałał. `patch-result-file-summary` projektuje tylko
`{:op :path :before :after :diff :move-to}`. `:pass` żyje w
`(:checks analysis)` per-edit ale nie wpada do per-file plan ani do
zwracanego summary.

**Evidence** — `s7.log`:
> "The result map does not expose a :pass key — it only reports :changed?,
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

### [ ] T3 — Rozwiązać sprzeczność w prompt: „nie verify" vs „re-read after write"

**Problem (S1, S10)** — Model po `v/patch` robi 3-5 razy `v/cat` żeby
sprawdzić. S10 zjadło 6 minut na pętli verify. Prompt mówi:
- "v/patch returns diff + post-image — that IS the write evidence. Do NOT v/cat to verify."
- "After a write, any prior read of the same path is stale; re-read."

Druga reguła wygrywa defensywnie.

**Evidence** — `s1.log` 4 iters, `s10.log` 8+ iters z 5+ `v/cat` po patchu.

**Proposed fix (tylko prompt, zero kodu)**
- Połączyć obie reguły jasno:
  ```
  - v/patch returns the full before/after blob. That IS the write evidence —
    do not v/cat the same path immediately after a successful v/patch.
  - "After a write any prior bind of `(v/cat path)` is stale" means: if you
    need that path's content LATER in the turn for a different purpose,
    re-bind. Not for verification.
  ```
- Test: prompt snapshot test, plus probe re-run S1/S10 → iter ≤ 2.

**Open questions**
- Czy dodać explicit przykład w prompt (one-shot: cat → patch → done)?
- Może też: „If you must verify, use `(:after patch-result)` not v/cat"?

---

## ⚠ Medium

### [ ] T4 — v/patch result: `:applied-lines` per-edit (eliminuje pokusę v/cat-verify)

**Problem (S2, S6)** — Model nie wie na której linii edit wylądował.
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

### [ ] T5 — Prompt: wzmocnić discoverability `:nth :all` dla bulk rename

**Problem (S3)** — Model batchował 5 osobnych edytów zamiast 1 z `:nth :all`.
W self-analysis prosi o „whole-identifier rename primitive" — coś co już
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

### [ ] T6 — Codex envelope EOF append nie odkryty (S5)

**Problem** — Model w S5 zrobił append przez read-last-line→replace. Envelope
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
- `v/append` to nowy primitive — koszt: 1 tool, ~50 LOC. ROI: 1 scenariusz
  pokazał trudność. Niski. → **wolę prompt fix tylko.**

---

## ℹ Low / nice-to-have

### [ ] T7 — `:after`/`:before` jako tuple `(line, text)` lub plain line-number

**Problem (S8 self-analysis)** — Anchor wymaga unikalnego content matcha.
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

### [ ] T8 — `v/rg` z `:replace` mode (search-and-replace w jednym tool)

**Problem (S4 self-analysis)** — Model po `v/rg` → ręcznie składa `v/patch`.
Sugeruje sklejenie.

**Proposed fix**
- ODRZUCONE: conflate concerns. `v/rg` to read-only observation, `v/patch`
  to mutation. Mixanie utrudni audit/permission policy (`extension/op-tag`).
- Zostawione w TODO bo model to zaproponował.

---

### [ ] T9 — `v/write` lub `:replace-entirely true` dla full-file rewrites

**Problem (S10)** — Model wybrał full-file `:search` (cały plik) → `:replace`
(nowy cały plik). Działa, ale 5+ verify iterów po. W self-analysis sugeruje
`v/write` lub `:replace-entirely true`.

**Proposed fix**
- Opcja A: Nie ruszać. Aktualna ścieżka działa. T3 (fix verify-loop) załatwi
  iter-count problem.
- Opcja B: Dodać `(v/write {:path :content :expected-mtime})` — jak
  rozważałem wcześniej i odrzuciliśmy.
- **Decyzja: czekamy aż T3 zostanie zrobione i ponownie probe S10. Jeśli
  iter ≤ 3 to T9 odrzucamy. Jeśli nadal +5 → wracamy.**

---

## Execution rule

1. Bierzemy 1 TODO.
2. **Discuss together** — uzgadniamy open questions, decyzje.
3. Implement + test + verify (`./verify.sh`).
4. Commit + push.
5. Re-probe odpowiedniego scenariusza (S1…S10) jeśli relevant.
6. Cross off.

Nigdy nie robimy więcej niż 1 TODO na raz bez explicit zgody.
