# CTX design — example conversations

Goal: stress-test the proposed CTX shape with realistic flows.
Read these, mark what feels invented, what feels essential, what's missing.

**Markdown-native answer note:** Vis is markdown-native today. Model finishes
each turn with `(done {:answer "..."})` where `:answer` is a markdown string.
No IR (`[:p [:t "..."]]` is gone). No `:subject` arg on `done`. Anything we
want as a "subject" for the recent-turn breadcrumb must be either (a) written
explicitly by the model via `(mem! ...)` or (b) derived deterministically by
the engine (e.g. first line of `:answer`).

Shape used in these examples (subject to debate):

```clojure
{:session/id          "01HXYZ"
 :session/turn        N

 ;; engine-managed (rebuilt every iter; never model-writable)
 :session/env
   {:workspace {:id          "ws/01HXYZ"
                :root        "/Users/fierycod/.vis/workspaces/repo:vis/ws-01HXYZ"
                :branch      "feat/ctx-redesign"   ; workspace branch
                :trunk       "main"                ; merge target
                :head        "abc1234"             ; current sha
                :dirty?      true
                :diff-stat   "+47 -12 across 3 files"}  ; cheap; full diff via v/workspace.diff
    :symbols    {build-ws {:sig "[id]" :doc "…" :defined-turn 7}
                 emit-event {:sig "[ev]" :defined-turn 2}}     ; user-defined SCI symbols this turn
                                                                ; engine-bare (v/cat, mem!, gen-id, ctx) NOT listed
    :open-files #{…}}                              ; touched THIS turn only

 ;; model-managed (via (mem! …))
 :session/preferences {…}
 :session/rules       {id {…}}
 :session/decisions   {id {…}}
 :session/specs       {id {…}}
 :session/tasks       {id {…}}}
```

**No `:session/counters`.** IDs derived at write time:
`(gen-id :tasks)` ≡ `(inc (apply max 0 (keys (:session/tasks ctx))))`.
No extra state, no race in single-threaded SCI eval, no bullshit.

Conventions in these examples:
- `USER >` — user message
- `MODEL >` — what model emits in a single ```clojure``` code block per iter
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

```clojure
MODEL >
(mem! [:session/preferences :color] "yellow")
=> :ok

(done {:answer "Zapamiętane — kolor żółty."})
=> :done
```

### Turn 2

```
USER > Ile jest 17 * 23?
```

```clojure
MODEL >
(* 17 23)
=> 391

(done {:answer "391"})
=> :done
```

### Turn 3

```
USER > Mam psa.
```

```clojure
MODEL >
(mem! [:session/preferences :has-pet] :dog)
=> :ok

(done {:answer "Ok, pies zapamiętany."})
=> :done
```

### Turn 4

```
USER > Powiedz coś po polsku.
```

```clojure
MODEL >
(done {:answer "Cześć! Co słychać?"})
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

```clojure
MODEL >
(done {:answer "Lubisz żółty kolor i masz psa."})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/id        "01HXYZ"
 :session/turn      5
 :session/counters  {}

 :session/recent
   [{:turn 1 :subject "Zapamiętane — kolor żółty."   :status :done}
    {:turn 2 :subject "391"                          :status :done}
    {:turn 3 :subject "Ok, pies zapamiętany."        :status :done}
    {:turn 4 :subject "Cześć! Co słychać?"           :status :done}
    {:turn 5 :subject "Lubisz żółty kolor i masz psa." :status :done}]

 :session/env
   {:workspace {:id "ws/01HXYZ" :root "/…/ws-01HXYZ"
                :branch "main" :trunk "main" :head "abc1234"
                :dirty? false :diff-stat "clean"}
    :symbols       {}
    :open-files #{}}

 :session/preferences {:color "yellow" :has-pet :dog}}
```

(Subject above is the first line of `:answer`. If we'd rather have model-supplied
subjects, model would emit `(mem! [:session/recent <turn> :subject] "…")` before
`(done …)` — extra call, more friction. Lean engine-derived from `:answer`.)

**Observations to discuss:**
- `:session/recent` of 5 entries with first-line-of-answer subjects: is it useful? Model already sees `:session/preferences`, doesn't need turn-by-turn subjects to answer "co lubię".
- Maybe `:session/recent` only needed when conversation refers to *prior turn structure* ("zrób to co przed chwilą"). Otherwise dead weight.
- Question: drop `:session/recent` entirely?

---

## Example 2 — read-patch-verify (file-truth + invalidation)

Coding flow. Stress-tests the patch-invalidates-read story.

### Turn 1

```
USER > Pokaż mi src/auth.clj i powiedz co tam jest.
```

```clojure
MODEL >
(v/cat "src/auth.clj")          ;; iter [1 1]
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(done {:answer "Plik definiuje `check/1`: literal string compare na `\"secret\"`."})
=> :done
```

### CTX AFTER TURN 1

```clojure
{:session/id        "01HXYZ"
 :session/turn      1
 :session/recent    [{:turn 1
                      :subject "Plik definiuje `check/1`: literal string compare na `\"secret\"`."
                      :status :done}]
 :session/env       {:workspace {:branch "main" :dirty? false :head "abc1234" …}
                     :symbols {} :open-files #{}}
 :session/file-truth {}                         ;; read-only, file not written
 :session/preferences {} :session/rules {} :session/decisions {}
 :session/specs {} :session/tasks {} :session/work {}}
```

**Note:** `v/cat` is a READ — does it land in `:session/file-truth`? Proposal: no. `file-truth` tracks writes only.

### Turn 2

```
USER > Zamień to literal compare na bcrypt verify.
```

Model sees no `:session/file-truth` for `src/auth.clj` — so the truth is "whatever filesystem has now". Turn 1's `v/cat` result lives in SQLite (reachable via `(v/iterations 1)`) but isn't auto-included in this turn's context.

Question: should Turn 1's `v/cat` result be visible without an explicit fetch in Turn 2?

Two camps:
- **Yes**: model needs working memory. Auto-include last N iter results across turns (engine-decided). But then bloat.
- **No**: model must explicitly `(v/iterations 1)` or re-`(v/cat …)`. Cleaner but verbose.

Sketch with **No** policy:

```clojure
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

(done {:answer "Zmieniono `check` na bcrypt verify."})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/recent [{:turn 1 :subject "Plik definiuje …"        :status :done}
                  {:turn 2 :subject "Zmieniono `check` na bcrypt verify." :status :done}]
 :session/env {:workspace {:branch "main" :dirty? true :diff-stat "+1 -1 in src/auth.clj" …}
               :symbols {} :open-files #{"src/auth.clj"}}        ;; this turn
 :session/file-truth {"src/auth.clj" [2 2]}                   ;; latest write
 ...}
```

### Turn 3

```
USER > Hmm, czy w tym pliku jest jeszcze coś poza check?
```

Model sees `:session/file-truth {"src/auth.clj" [2 2]}`. The "truth" of `src/auth.clj` is whatever iter [2 2] left it as. Model has options:

```clojure
MODEL >
;; option A: re-read FS (cheap, definitive)
(v/cat "src/auth.clj")
=> "(ns auth)\n(defn check [tok] (bcrypt/check tok stored-hash))\n"

(done {:answer "Tylko `check/1`. Nic więcej w pliku."})

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

```clojure
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

(done {:answer "Spec #1 zapisany + 3 taski. Zaczynam od `emit-event`."})
=> :done
```

### CTX AFTER TURN 1

```clojure
{:session/turn 1
 :session/recent [{:turn 1 :subject "Spec #1 zapisany + 3 taski. Zaczynam od `emit-event`." :status :done}]
 :session/specs {1 {:title "logging → structured EDN events"
                    :acceptance [...] :status :draft :added-turn 1}}
 :session/tasks {1 {:title "introduce emit-event fn" :spec 1 :status :todo
                    :deps [] :added-turn 1}
                 2 {:title "convert str/format call-sites" :spec 1
                    :status :todo :deps [1] :added-turn 1}
                 3 {:title "wire levels through emit-event" :spec 1
                    :status :todo :deps [1] :added-turn 1}}
 :session/work {:phase :planning :focus [:session/specs 1] :blockers []}
 :session/env  {:workspace {:branch "main" :dirty? false :head "abc1234" …}
                :symbols {} :open-files #{}}}
```

### Turn 2

```
USER > Działaj.
```

Model reads `:session/work :focus → spec 1`; `:session/tasks` shows t1 has no deps. Picks t1.

```clojure
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

(done {:answer "Task #1 done. `emit-event` wprowadzony, `log/2` deleguje."})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/recent [{:turn 1 :subject "Spec #1 zapisany + 3 taski. Zaczynam od `emit-event`." :status :done}
                  {:turn 2 :subject "Task #1 done. `emit-event` wprowadzony, `log/2` deleguje." :status :done}]
 :session/specs {1 {:title "…" :status :draft :added-turn 1}}  ;; still :draft
 :session/tasks {1 {:title "…" :status :done :done-turn 2 :spec 1 :added-turn 1 :deps []}
                 2 {…} 3 {…}}
 :session/work  {:phase :step-1-of-3 :focus [:session/specs 1] :blockers []}
 :session/env   {:workspace {:branch "feat/logging" :dirty? true
                             :diff-stat "+5 -2 in src/logging.clj" :head "def5678" …}
                 :symbols {} :open-files #{"src/logging.clj"}}}
```

**Note:** model forgot to flip spec status to `:doing`. Engine doesn't auto-promote. Question: do we want auto-promotion (any spec with a `:doing` task → `:doing`)? Or is that hidden magic? Lean explicit, no magic.

### Turn 3

```
USER > Wait, maybe emit-event powinno mieć timestamp dodawany przez engine, nie przez caller?
```

This is a design question, not work. Model records a decision.

```clojure
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

(done {:answer "Decyzja zapisana, task #4 dodany."})
=> :done
```

### CTX AFTER TURN 3

```clojure
{:session/turn 3
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

```clojure
MODEL >
(let [r (gen-id :rule)]
  (mem! [:session/rules r]
        {:body "tests must hit real SQLite — no mocks"
         :scope :project                    ;; ← triggers mirror to project_rule
         :added-turn 7}))
=> :ok

(done {:answer "Reguła #1 zapisana jako project-scoped."})
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

```clojure
MODEL >
(v/cat "test/logging_test.clj")
=> ;; existing tests
…
(v/patch [{…test using real next.jdbc + tmp sqlite file…}])
…
(done {:answer "Test dodany, używa tmp SQLite jak reguła projektu wymaga."})
=> :done
```

---

## Cross-cutting questions raised by these examples

1. **`:session/recent` — value or noise?**
   Example 1 shows 5 entries where each `:subject` is just the first-line of `:answer`. The model never read them — used `:session/preferences` directly. Maybe `:session/recent` is only useful when user says "zrób to co przed chwilą" or "wycofaj poprzednią zmianę" — and even then, `(v/turn-toc)` / `(v/iterations N)` / `(v/turn-answer N)` cover it.

   **Proposal:** drop `:session/recent`. `session_turn` table already has the full history; `(v/turn-toc)` lists them on demand.

2. **Subject — derived or model-emitted?**
   With markdown-native `(done {:answer "…"})`, model has no `:subject` arg. Two options:
   - **Engine-derives:** first line of `:answer`. Free, deterministic, but only as good as the model's first sentence.
   - **Model writes:** `(mem! [:session/recent <turn> :subject] "…")` before `(done …)`. More friction.
   - **Drop subjects entirely.** Lazy fetch `(v/turn-answer N)` shows the full markdown when actually needed.
   Lean drop entirely if we also drop `:session/recent`.

3. **`:session/file-truth` — derived or invented?**
   Example 2 showed that `v/cat` to FS is cheap and definitive. `file-truth` is a denormalisation of `session_turn_iteration` rows. Cost > benefit?

   **Proposal:** drop. Re-read FS via `v/cat` whenever uncertain. Engine renders within-turn iter trailers with "superseded by [t i]" markers; cross-turn nothing.

4. **Spec status promotion — auto or manual?**
   Example 3 Turn 2: model finished a task but didn't bump spec status. Auto-promote (any `:doing` task → spec `:doing`) is a small rule but hidden magic. Lean manual; prompt nudge.

5. **`:session/work` necessity.**
   Examples 1 & 2 don't use it. Example 3 uses it lightly. Maybe `:session/work` is redundant — model can write `:status :doing` on tasks as the focus signal. Sticky `:phase` could just be the trailing `:status` of the in-progress task.

   **Proposal:** drop `:session/work`. Model uses `:status :doing` on tasks as the focus signal.

6. **Counters — killed.**
   IDs derive at write time from `(inc (apply max 0 (keys table)))`. No `:session/counters` blob. Saves a subtree.

7. **Decision tagging.**
   `:tags` on decisions is freeform but only model-set. Useful for search-back? Probably yes ("show me ctx decisions").

8. **Workspace in env.**
   Engine-derived via `workspace/for-session` + `workspace/status`. Surfaces `:branch`, `:trunk`, `:head`, `:dirty?`, `:diff-stat`. Full diff stays lazy via `v/workspace.diff`. Model uses this to know "am I on trunk" / "is there pending work" / "do I need to apply or discard before X".

---

## What's left after dropping invented stuff

If we drop `:session/recent`, `:session/archived`, `:session/file-truth`, `:session/work`, `:session/counters`:

```clojure
;; storage:
{:session/id          "01HXYZ"
 :session/turn        7

 :session/env         {:workspace  {:id :root :branch :trunk :head :dirty? :diff-stat}
                       :symbols       {…}    ; live SCI introspection
                       :open-files #{}}   ; touched THIS turn

 :session/preferences {…}                ; model-written kv
 :session/rules       {id {…}}           ; model-written; :scope :session|:project
 :session/decisions   {id {…}}           ; model-written; append-only
 :session/specs       {id {…}}           ; model-written
 :session/tasks       {id {…}}}          ; model-written
```

Five model-managed subtrees, one engine-managed (env w/ workspace sub-view). That's the minimum-viable shape.

Cross-turn history lives entirely in SQLite (`session_turn`, `session_turn_iteration`) — reachable via foundation calls (`v/turn-message`, `v/turn-answer`, `v/iterations`, `v/turn-toc`), never preloaded into the prompt.

Question to commit on before any code:
- Is this the minimum? Anything still invented?
- Is anything missing that would force the model back into ad-hoc top-level keys?
