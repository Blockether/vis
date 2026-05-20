# CTX design — example conversations

Goal: stress-test the proposed CTX shape with realistic flows.
Read these, mark what feels invented, what feels essential, what's missing.

Shape used in these examples (subject to debate):

```clojure
{:session/id          "01HXYZ"
 :session/turn        N
 :session/counters    {…}                  ; gen-id seeds

 ;; engine-managed
 :session/recent      [{:turn :subject :status}]
 :session/archived    {:turn-ids […]}
 :session/env         {:defs :open-files :git-branch}
 :session/file-truth  {path [turn iter]}

 ;; model-managed (via (mem! …))
 :session/preferences {…}
 :session/rules       {id {…}}
 :session/decisions   {id {…}}
 :session/specs       {id {…}}
 :session/tasks       {id {…}}
 :session/work        {:phase :focus :blockers}}
```

Conventions in these examples:
- `USER >` — user message
- `MODEL >` — what model emits (one SCI form per line, or block)
- `=> …` — engine eval result
- `CTX AFTER TURN N:` — full CTX blob state after `(done …)`
- Lazy fetches via `(v/turn-message N)`, `(v/iterations N)`, `(v/turn-answer N)` shown as needed

---

## Example 1 — pure conversation, recall-after-5-turns

User chatting, no code. Tests whether `:session/preferences` carries trivially-stated facts.

### Turn 1

```
USER > Lubię żółty.
```

```
MODEL >
(mem! [:session/preferences :color] "yellow")
=> :ok
(done [:p [:t "Zapamiętane — kolor żółty."]]
      {:subject "noted color preference"})
=> :done
```

### Turn 2

```
USER > Ile jest 17 * 23?
```

```
MODEL >
(* 17 23)
=> 391
(done [:p [:t "391"]]
      {:subject "arithmetic"})
=> :done
```

### Turn 3

```
USER > Mam psa.
```

```
MODEL >
(mem! [:session/preferences :has-pet] :dog)
=> :ok
(done [:p [:t "Ok, pies zapamiętany."]]
      {:subject "noted pet"})
=> :done
```

### Turn 4

```
USER > Powiedz coś po polsku.
```

```
MODEL >
(done [:p [:t "Cześć! Co słychać?"]]
      {:subject "polish greeting"})
=> :done
```

### Turn 5

```
USER > Co lubię?
```

Model sees in `;; ctx`:
```clojure
:session/preferences {:color "yellow" :has-pet :dog}
```

```
MODEL >
(done [:p [:t "Lubisz żółty kolor i masz psa."]]
      {:subject "recalled preferences"})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/id        "01HXYZ"
 :session/turn      5
 :session/counters  {}

 :session/recent
   [{:turn 1 :subject "noted color preference" :status :done}
    {:turn 2 :subject "arithmetic"             :status :done}
    {:turn 3 :subject "noted pet"              :status :done}
    {:turn 4 :subject "polish greeting"        :status :done}
    {:turn 5 :subject "recalled preferences"   :status :done}]

 :session/env         {:defs {} :open-files #{} :git-branch "main"}
 :session/file-truth  {}

 :session/preferences {:color "yellow" :has-pet :dog}}
```

**Observations to discuss:**
- `:session/recent` of 5 entries with only `:subject` strings is cheap, but is it useful? Model already sees `:session/preferences`, doesn't need to know turn-by-turn subjects to answer "co lubię".
- Maybe `:session/recent` only needed when conversation refers to *prior turn structure* ("zrób to co przed chwilą"). Otherwise it's dead weight.
- Question: should `:session/recent` exist at all, or just `:session/archived` after K?

---

## Example 2 — read-patch-verify (file-truth + invalidation)

Coding flow. Stress-tests the patch-invalidates-read story.

### Turn 1

```
USER > Pokaż mi src/auth.clj i powiedz co tam jest.
```

```
MODEL >
(v/cat "src/auth.clj")          ;; iter [1 1]
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(done [:p [:t "Plik definiuje check/1: literal string compare na secret."]]
      {:subject "inspected auth"})
=> :done
```

### CTX AFTER TURN 1

```clojure
{:session/id        "01HXYZ"
 :session/turn      1
 :session/recent    [{:turn 1 :subject "inspected auth" :status :done}]
 :session/env       {:defs {} :open-files #{} :git-branch "main"}
 :session/file-truth {}                         ;; read-only, file not written
 :session/preferences {} :session/rules {} :session/decisions {}
 :session/specs {} :session/tasks {} :session/work {}}
```

**Note:** `v/cat` is a READ — does it land in `:session/file-truth`? Proposal: no. `file-truth` tracks writes only.

### Turn 2

```
USER > Zamień to literal compare na bcrypt verify.
```

Model sees no `:session/file-truth` for `src/auth.clj` — so the truth is "whatever filesystem has now". Model needs to re-read OR rely on Turn 1's iter result (which is in current-turn chat? NO — Turn 1 closed. Iter result lives in SQLite, reachable via `(v/iterations 1)`).

Question: should Turn 1's `v/cat` result be visible without an explicit fetch in Turn 2?

Two camps:
- **Yes**: model needs working memory. Auto-include last N iter results across turns (engine-decided). But then bloat.
- **No**: model must explicitly `(v/iterations 1)` or re-`(v/cat …)`. Cleaner but verbose.

Sketch with **No** policy:

```
MODEL >
(v/cat "src/auth.clj")          ;; iter [2 1] — re-read; FS truth
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(v/patch [{:path "src/auth.clj"
           :search "(defn check [tok] (= tok \"secret\"))"
           :replace "(defn check [tok] (bcrypt/check tok stored-hash))"}])
;; iter [2 2]
=> {:applied 1 :path "src/auth.clj"}

;; Engine notes: [2 1] result on src/auth.clj is now superseded by [2 2].
;; (Marked in chat trailer of THIS turn only; cross-turn doesn't carry it.)

(done [:p [:t "Zmieniono check na bcrypt verify."]]
      {:subject "auth → bcrypt"})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/recent [{:turn 1 :subject "inspected auth" :status :done}
                  {:turn 2 :subject "auth → bcrypt"  :status :done}]
 :session/env {:defs {} :open-files #{} :git-branch "main"}   ;; reset per turn
 :session/file-truth {"src/auth.clj" [2 2]}                   ;; latest write
 ...}
```

### Turn 3

```
USER > Hmm, czy w tym pliku jest jeszcze coś poza check?
```

Model sees `:session/file-truth {"src/auth.clj" [2 2]}`. The "truth" of `src/auth.clj` is whatever iter [2 2] left it as. Model has options:

```
MODEL >
;; option A: re-read FS (cheap, definitive)
(v/cat "src/auth.clj")
=> "(ns auth)\n(defn check [tok] (bcrypt/check tok stored-hash))\n"

(done [:p [:t "Tylko check/1. Nic więcej."]]
      {:subject "auth file inventory"})

;; option B: fetch the iter row directly
(v/iterations 2)
=> [{:position 1 :code "(v/cat …)" :result "(ns auth)\n…secret…"}
    {:position 2 :code "(v/patch …)" :result "{:applied 1…}"}]
;; — useless for current state; just shows history

;; option C: deterministic via :session/file-truth pointer
;; (no operator exists for "give me file content as of iter [t i]")
```

**Question to settle:** do we add an operator like `(file-as-of [turn iter])` that reconstructs the file at a given write? Or always re-read FS via `v/cat`? Latter is dead simple and matches reality (FS is the truth; CTX just points at last writer).

**Proposal:** kill `:session/file-truth` entirely. It's a derived view that the model can recompute by `v/cat` (cheap) or by scanning `(v/iterations N)`. Saves a subtree.

---

## Example 3 — spec → tasks → execute → decision

Multi-turn coding job. Tests `:session/specs` / `:session/tasks` / `:session/decisions` / `:session/work`.

### Turn 1

```
USER > Zaplanuj refactor logging do strukturalnych eventów (EDN, nie text).
```

```
MODEL >
(let [sid (gen-id :spec)]
  (mem! [:session/specs sid]
        {:title "logging → structured EDN events"
         :acceptance ["no str/format in src/"
                      "all logs go through emit-event"
                      "log levels preserved"]
         :status :draft
         :added-turn 1}))
=> :ok

(let [t1 (gen-id :task)
      t2 (gen-id :task)
      t3 (gen-id :task)]
  (mem! [:session/tasks t1]
        {:title "introduce emit-event fn" :spec 1 :status :todo
         :deps [] :added-turn 1})
  (mem! [:session/tasks t2]
        {:title "convert str/format call-sites" :spec 1 :status :todo
         :deps [t1] :added-turn 1})
  (mem! [:session/tasks t3]
        {:title "wire levels through emit-event" :spec 1 :status :todo
         :deps [t1] :added-turn 1}))
=> :ok

(mem! [:session/work] {:phase :planning :focus [:session/specs 1] :blockers []})
=> :ok

(done [:p [:t "Spec #1 + 3 tasks. Zaczynam od emit-event."]]
      {:subject "planned logging refactor"})
=> :done
```

### CTX AFTER TURN 1

```clojure
{:session/turn 1
 :session/counters {:spec 1 :task 3}
 :session/recent [{:turn 1 :subject "planned logging refactor" :status :done}]
 :session/specs {1 {:title "logging → structured EDN events"
                    :acceptance [...] :status :draft :added-turn 1}}
 :session/tasks {1 {:title "introduce emit-event fn" :spec 1 :status :todo
                    :deps [] :added-turn 1}
                 2 {:title "convert str/format call-sites" :spec 1
                    :status :todo :deps [1] :added-turn 1}
                 3 {:title "wire levels through emit-event" :spec 1
                    :status :todo :deps [1] :added-turn 1}}
 :session/work {:phase :planning :focus [:session/specs 1] :blockers []}
 :session/env  {:defs {} :open-files #{} :git-branch "main"}}
```

### Turn 2

```
USER > Działaj.
```

Model reads `:session/work :focus → spec 1`; `:session/tasks` shows t1 has no deps. Picks t1.

```
MODEL >
(mem-fn [:session/tasks 1 :status] (constantly :doing))
=> :ok
(mem! [:session/work :phase] :step-1-of-3)
=> :ok

(v/cat "src/logging.clj")          ;; iter [2 1]
=> "(ns logging)\n(defn log [level msg] (println (str/format \"%s: %s\" level msg)))\n"

(v/patch [{:path "src/logging.clj"
           :search "(defn log [level msg]\n  (println (str/format \"%s: %s\" level msg)))"
           :replace "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))\n\n(defn log [level msg]\n  (emit-event {:level level :msg msg}))"}])
;; iter [2 2]
=> {:applied 1}

(mem-fn [:session/tasks 1 :status] (constantly :done))
=> :ok
(mem-fn [:session/tasks 1 :done-turn] (constantly 2))
=> :ok

(done [:p [:t "Task #1 done. emit-event wprowadzony, log/2 deleguje."]]
      {:subject "added emit-event"})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/counters {:spec 1 :task 3}
 :session/recent [{:turn 1 :subject "planned logging refactor" :status :done}
                  {:turn 2 :subject "added emit-event"         :status :done}]
 :session/specs {1 {:title "…" :status :draft :added-turn 1}}  ;; still :draft
 :session/tasks {1 {:title "…" :status :done :done-turn 2 :spec 1 :added-turn 1 :deps []}
                 2 {…} 3 {…}}
 :session/work  {:phase :step-1-of-3 :focus [:session/specs 1] :blockers []}
 :session/env   {:defs {} :open-files #{} :git-branch "main"}}
```

**Note:** model forgot to flip spec status to `:doing`. Engine doesn't auto-promote. Question: do we want auto-promotion (any spec with a `:doing` task → `:doing`)? Or is that hidden magic? Lean explicit, no magic.

### Turn 3

```
USER > Wait, maybe emit-event powinno mieć timestamp dodawany przez engine, nie przez caller?
```

This is a design question, not work. Model records a decision.

```
MODEL >
(let [d (gen-id :decision)]
  (mem! [:session/decisions d]
        {:body "emit-event auto-stamps :ts (UTC ms) if absent; callers pass only domain fields"
         :added-turn 3
         :tags #{:logging :api}}))
=> :ok

(let [t (gen-id :task)]
  (mem! [:session/tasks t]
        {:title "emit-event auto-stamp :ts" :spec 1 :status :todo
         :deps [1] :added-turn 3}))
=> :ok

(done [:p [:t "Decyzja zapisana, task #4 dodany."]]
      {:subject "decided :ts auto-stamp"})
=> :done
```

### CTX AFTER TURN 3

```clojure
{:session/turn 3
 :session/counters {:spec 1 :task 4 :decision 1}
 :session/decisions {1 {:body "emit-event auto-stamps :ts (UTC ms) if absent; callers pass only domain fields"
                        :added-turn 3 :tags #{:logging :api}}}
 :session/tasks (… 4 added …)
 …}
```

---

## Example 4 — cross-session project rule

Session A teaches a project rule. Session B (next day, same repo) picks it up.

### Session A, Turn 7

```
USER > Pamiętaj, w tym projekcie testy muszą uderzać w prawdziwy SQLite, nigdy mockować.
```

```
MODEL >
(let [r (gen-id :rule)]
  (mem! [:session/rules r]
        {:body "tests must hit real SQLite — no mocks"
         :scope :project                    ;; ← triggers mirror to project_rule
         :added-turn 7}))
=> :ok

(done [:p [:t "Reguła #1 zapisana jako project-scoped."]]
      {:subject "added project rule: real-DB testing"})
=> :done
```

### Engine side-effect

On `(mem! …)` landing a `:scope :project`, engine writes:

```sql
INSERT INTO project_rule (repo_id, body, added_at)
VALUES ('repo:vis', 'tests must hit real SQLite — no mocks', ...)
```

(Or whatever the persistance API ends up being.)

### Session B start — next day

Engine loads `:session/rules` blob = `{}` (new session, no prior rules). Then merges from `project_rule` for same repo:

```clojure
:session/rules
  {1 {:body "tests must hit real SQLite — no mocks"
      :scope :project
      :added-turn nil                       ;; not from this session
      :source     :project-mirror}}         ;; engine-set
```

### Session B, Turn 1

```
USER > Dodaj test dla emit-event.
```

Model sees `:session/rules 1 :body` → knows to write a real-DB test, not a mock.

```
MODEL >
(v/cat "test/logging_test.clj")
=> ;; existing tests
…
(v/patch [{…test using real next.jdbc + tmp sqlite file…}])
…
(done [:p [:t "Test dodany, używa tmp SQLite jak reguła projektu wymaga."]]
      {:subject "added emit-event test (real DB)"})
=> :done
```

---

## Cross-cutting questions raised by these examples

1. **`:session/recent` — value or noise?**
   Example 1 shows 5 entries with only `:subject`. Was any of them ever read by the model? Probably not — model used `:session/preferences` directly. Maybe `:session/recent` is only useful when user says "zrób to co przed chwilą" or "wycofaj poprzednią zmianę" — and even then, `(v/turn-toc)` / `(v/iterations N)` cover it.

   **Proposal:** drop `:session/recent`. Keep `:session/archived {:turn-ids […]}` or even drop that too — `session_turn` table already enumerates them.

2. **`:session/file-truth` — derived or invented?**
   Example 2 showed that `v/cat` to FS is cheap and definitive. `file-truth` is a denormalisation of `session_turn_iteration` rows. Cost > benefit?

   **Proposal:** drop. Re-read FS via `v/cat` whenever uncertain. Engine renders within-turn iter trailers with "superseded by [t i]" markers; cross-turn nothing.

3. **Spec status promotion — auto or manual?**
   Example 3 Turn 2: model finished a task but didn't bump spec status. Auto-promote (any `:doing` task → spec `:doing`) is a small rule but hidden magic. Lean manual; prompt nudge.

4. **`:session/work` necessity.**
   Examples 1 & 2 don't use it. Example 3 uses it lightly. Maybe `:session/work` is redundant — model can write `{:focus 1}` directly into the first task it's working on (`:status :doing` already signals that). Sticky `:phase` could just be the `:subject` of last turn.

   **Proposal:** drop `:session/work`. Model uses `:status :doing` on tasks as the focus signal. Phase = derived from "which step of which spec is being worked".

5. **Counters' visibility.**
   `:session/counters` is engine-managed and uninteresting to the model. Render-time hide?

6. **Decision tagging.**
   `:tags` on decisions is freeform but only model-set. Useful for search-back? Probably yes ("show me ctx decisions").

---

## What's left after dropping invented stuff

If we drop `:session/recent`, `:session/file-truth`, `:session/work`, and hide `:session/counters` on the wire:

```clojure
;; storage:
{:session/id          "01HXYZ"
 :session/turn        7
 :session/counters    {…}                ; engine; hidden from wire

 :session/env         {:defs        {…}  ; live SCI introspection
                       :open-files  #{}  ; touched THIS turn
                       :git-branch  "…"}

 :session/preferences {…}                ; model-written kv
 :session/rules       {id {…}}           ; model-written; :scope :session|:project
 :session/decisions   {id {…}}           ; model-written; append-only
 :session/specs       {id {…}}           ; model-written
 :session/tasks       {id {…}}}          ; model-written
```

Five model-managed subtrees, one engine-managed (env). That's the minimum-viable shape.

Question to commit on before any code:
- Is this the minimum? Anything still invented?
- Is anything missing that would force the model back into ad-hoc top-level keys?
