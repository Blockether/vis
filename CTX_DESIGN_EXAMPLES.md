# CTX design — example conversations

Goal: stress-test the proposed CTX shape with realistic flows.
Read these, mark what feels invented, what feels essential, what's missing.

## Conventions

- `USER >` — user message
- `MODEL >` — what model emits in a single ```clojure``` code block per iter
- `=> …` — engine eval result
- `CTX AFTER TURN N:` — full CTX blob rendered the way model sees it in the next user message
- Lazy fetches via `(v/turn-message N)`, `(v/iterations N)`, `(v/turn-answer N)`, `(v/turn-toc from to)`

## Operator API — final

```clojure
;; ─── MEMOS (mid-turn; model writes; stored in session_state.ctx blob) ───
(mem-set!    path value)         ; assoc-in semantics
(mem-update! path f & args)      ; update-in semantics
(mem-remove! path)               ; dissoc-in semantics

;; ─── SYMBOLS (native SCI + existing Vis persistence machinery) ───
(defn foo [x] …)                 ; create / overwrite — auto-persists in definition_state
(def  foo nil)                   ; drop — engine treats latest expression IS NULL as forgotten

;; ─── FINAL ───
(done {:answer "markdown string"})
```

**No `ctx` symbol. No `(ctx)` fn. No reads in eval.**

CTX is always rendered into the user message of the next turn with inline
`;;` comments explaining each subtree. Model reads from the rendered text,
writes back via the four verbs above.

## Entry keys

All entries in `:session/{rules, decisions, specs, tasks}` are keyed by
**model-chosen keywords**. Cross-table refs are bare keywords.

```clojure
(mem-set! [:session/tasks :wire-symbol-render]
          {:title "Render :session.env/symbols from definition_state"
           :spec   :ctx-redesign
           :status :todo
           :deps   [:dissoc-counters]})
```

Engine stamps `:added-turn (:session/turn ctx)` automatically on first
`mem-set!` to a new key. No counters subtree. No mint operator.

## Storage shape

```clojure
{:session/id     "01HXYZ"
 :session/turn   N

 ;; engine-rendered views (NOT stored as EDN — derived per dump)
 ;;
 ;; :session.env/symbols   — derived from definition_soul + latest definition_state
 ;;                          (rows whose latest expression IS NOT NULL only)
 ;; :session.env/workspace — derived from workspace/for-session + workspace/status
 :session.env/symbols    {sym {:arglists :doc :turn}}
 :session.env/workspace  {:branch :trunk :head :dirty? :diff-stat}

 ;; model-managed memos (stored as EDN inside session_state.ctx)
 :session/rules      {keyword {:body :scope :added-turn}}
 :session/decisions  {keyword {:body :tags :added-turn}}
 :session/specs      {keyword {:title :acceptance :status :added-turn}}
 :session/tasks      {keyword {:title :spec :status :deps :added-turn :done-turn}}}
```

Six substantive subtrees. Two engine-derived (`:session.env/symbols`,
`:session.env/workspace`); four model-managed (`:session/rules`,
`:decisions`, `:specs`, `:tasks`).

## Rendered CTX block (what model actually sees)

```clojure
;; ════════════════════════════════════════════════════════════════════
;; CTX — session state. Renders fresh into every user message.
;; ════════════════════════════════════════════════════════════════════

{:session/id   "01HXYZ"
 :session/turn 7

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session.env/workspace — current workspace + diff against trunk.
 ;;   Derived from workspace state; full diff lives in (v/workspace.diff).
 ;; ──────────────────────────────────────────────────────────────────
 :session.env/workspace
   {:branch "feat/ctx-redesign" :trunk "main"
    :head "abc1234" :dirty? true
    :diff-stat "+47 -12 across 3 files"}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session.env/symbols — SCI user-defined symbols that are live right now.
 ;;   :turn = the turn in which the symbol was last (re)defined.
 ;;   Symbols (defined as) nil are dropped from this view automatically.
 ;;
 ;;   To call    : (build-ws "x")
 ;;                — invoke as a normal function; it is already live.
 ;;
 ;;   To create  : (defn build-ws [id] …)
 ;;                — engine persists it; survives turn boundary.
 ;;
 ;;   To drop    : (def build-ws nil)
 ;;                — engine drops on next restore; gone from this view.
 ;;
 ;;   To inspect : (meta #'build-ws) for live arglists/doc.
 ;; ──────────────────────────────────────────────────────────────────
 :session.env/symbols
   {build-ws   {:arglists ([id])                              :doc "Workspace builder" :turn 4}
    emit-event {:arglists ([{:keys [level msg] :as ev}])      :doc "Event emitter"     :turn 5}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/rules — durable facts about the user and how to respond.
 ;;   Captures user-stated preferences, behavioral directives, and project
 ;;   conventions. Read these before every answer.
 ;;   Shape   : {:body string :scope #{:session :project}}
 ;;
 ;;   To add  : (mem-set!    [:session/rules :caveman-pl]
 ;;                          {:body "respond in PL caveman style"
 ;;                           :scope :session})
 ;;   To drop : (mem-remove! [:session/rules :caveman-pl])
 ;;
 ;;   :scope :project mirrors to the project_rule table and loads into
 ;;   every new session in this repo.
 ;; ──────────────────────────────────────────────────────────────────
 :session/rules
   {:caveman-pl     {:body "respond in PL caveman style" :scope :session :added-turn 1}
    :likes-yellow   {:body "user likes yellow color"     :scope :session :added-turn 1}
    :real-db-tests  {:body "tests must hit real SQLite — no mocks" :scope :project :added-turn 7}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/decisions — append-only "why we did X". Read-back for design
 ;;   rationale questions; never updated; never auto-pruned.
 ;;   Shape   : {:body string :tags #{keyword}}
 ;;
 ;;   To add  : (mem-set! [:session/decisions :no-llm-compaction]
 ;;                       {:body "Never LLM-compact ctx; deterministic rules only"
 ;;                        :tags #{:ctx :design}})
 ;; ──────────────────────────────────────────────────────────────────
 :session/decisions
   {:no-llm-compaction
      {:body "Never LLM-compact ctx; deterministic rules only"
       :tags #{:ctx :design} :added-turn 3}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/specs — formal requirements with acceptance criteria.
 ;;   Shape   : {:title string :acceptance [string] :status keyword}
 ;;   :status ∈ #{:draft :doing :done :cancelled}
 ;;
 ;;   To add     : (mem-set! [:session/specs :ctx-redesign] {…})
 ;;   To advance : (mem-set! [:session/specs :ctx-redesign :status] :doing)
 ;; ──────────────────────────────────────────────────────────────────
 :session/specs
   {:ctx-redesign
      {:title "new CTX model"
       :acceptance ["no LLM compaction"
                    "no counters / no scratchpad concept"
                    "symbols persist via existing definition_state machinery"]
       :status :doing :added-turn 4}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/tasks — work items. Reference specs and other tasks by keyword.
 ;;   Shape   : {:title string :spec keyword :status keyword :deps [keyword]}
 ;;   :status ∈ #{:todo :doing :done :blocked :cancelled}
 ;;
 ;;   To add     : (mem-set! [:session/tasks :wire-symbol-render]
 ;;                          {:title "…" :spec :ctx-redesign
 ;;                           :status :todo :deps [:dissoc-counters]})
 ;;   To advance : (mem-set! [:session/tasks :wire-symbol-render :status] :doing)
 ;; ──────────────────────────────────────────────────────────────────
 :session/tasks
   {:dissoc-counters
      {:title "remove :session/counters and gen-id"
       :spec :ctx-redesign :status :done :deps []
       :added-turn 4 :done-turn 5}
    :wire-symbol-render
      {:title "render :session.env/symbols from definition_state"
       :spec :ctx-redesign :status :doing :deps [:dissoc-counters]
       :added-turn 4}}}
```

---

## Example 1 — pure conversation, recall-after-5-turns

User chatting, no code. Tests whether `:session/rules` carries trivial
user facts as well as behavioral directives.

### Turn 1

```
USER > Lubię żółty.
```

```clojure
MODEL >
(mem-set! [:session/rules :likes-yellow]
          {:body "user likes yellow color" :scope :session})
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
(mem-set! [:session/rules :has-pet-dog]
          {:body "user has a dog" :scope :session})
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

Model reads from the rendered CTX block:

```clojure
:session/rules
  {:likes-yellow {:body "user likes yellow color" :scope :session :added-turn 1}
   :has-pet-dog  {:body "user has a dog"          :scope :session :added-turn 3}}
```

```clojure
MODEL >
(done {:answer "Lubisz żółty kolor i masz psa."})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/id   "01HXYZ"
 :session/turn 5
 :session.env/workspace {:branch "main" :trunk "main" :head "abc1234"
                         :dirty? false :diff-stat "clean"}
 :session.env/symbols   {}                       ; no defs in this session
 :session/rules
   {:likes-yellow {:body "user likes yellow color" :scope :session :added-turn 1}
    :has-pet-dog  {:body "user has a dog"          :scope :session :added-turn 3}}
 :session/decisions {} :session/specs {} :session/tasks {}}
```

**Observations:**
- Conversation works without any per-turn breadcrumb. `:session/rules`
  carries the facts; `(v/turn-message N)` / `(v/turn-answer N)` give
  per-turn detail on demand.
- One bucket (`:session/rules`) holds both trivial facts and behavioral
  rules. Coherence is enforced by single-bucket-per-concept design.

---

## Example 2 — read-patch-verify (workspace state surfaces in env)

Coding flow. Tests that `:session.env/workspace` reflects edits and the
model orients off it.

### Turn 1

```
USER > Pokaż mi src/auth.clj i powiedz co tam jest.
```

```clojure
MODEL >
(v/cat "src/auth.clj")          ;; iter [1 1]
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(done {:answer "Plik definiuje `check/1`: literal compare na `\"secret\"`."})
=> :done
```

### CTX AFTER TURN 1

```clojure
{:session/id   "01HXYZ"
 :session/turn 1
 :session.env/workspace {:branch "main" :trunk "main" :head "abc1234"
                         :dirty? false :diff-stat "clean"}
 :session.env/symbols   {}
 :session/rules {} :session/decisions {} :session/specs {} :session/tasks {}}
```

### Turn 2

```
USER > Zamień to literal compare na bcrypt verify.
```

Within this turn the model re-reads (cheap, definitive) then patches.
The iter trailer of this turn marks the just-completed `v/cat` result
as superseded once the `v/patch` lands.

```clojure
MODEL >
(v/cat "src/auth.clj")          ;; iter [2 1] — fresh read
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(v/patch [{:path "src/auth.clj"
           :search "(defn check [tok] (= tok \"secret\"))"
           :replace "(defn check [tok] (bcrypt/check tok stored-hash))"}])
;; iter [2 2]
=> {:applied 1 :path "src/auth.clj"}
;; (within-turn trailer notes iter [2 1] v/cat src/auth.clj :superseded-by [2 2])

(done {:answer "Zamieniono `check` na bcrypt verify."})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/id   "01HXYZ"
 :session/turn 2
 :session.env/workspace {:branch "feat/auth-bcrypt" :trunk "main"
                         :head "def5678" :dirty? true
                         :diff-stat "+1 -1 in src/auth.clj"}
 :session.env/symbols   {}
 :session/rules {} :session/decisions {} :session/specs {} :session/tasks {}}
```

**Observations:**
- No `:session/file-truth` cache. Workspace diff-stat is the cross-turn
  signal that "something is pending".
- Cross-turn there is no preserved file-state cache; FS is the truth,
  re-read via `v/cat` on demand.

---

## Example 3 — spec + tasks + decision + symbols lifecycle

Multi-turn coding job. Tests `:session/specs`, `:session/tasks`,
`:session/decisions`, and native symbol persistence (no scratchpad).

### Turn 1

```
USER > Zaplanuj refactor logging do strukturalnych eventów (EDN, nie text).
```

```clojure
MODEL >
(mem-set! [:session/specs :logging-edn]
          {:title "logging → structured EDN events"
           :acceptance ["no str/format in src/"
                        "all logs go through emit-event"
                        "log levels preserved"]
           :status :draft})
=> :ok

(mem-set! [:session/tasks :introduce-emit-event]
          {:title "introduce emit-event fn"
           :spec :logging-edn :status :todo :deps []})
=> :ok

(mem-set! [:session/tasks :convert-call-sites]
          {:title "convert str/format call-sites"
           :spec :logging-edn :status :todo :deps [:introduce-emit-event]})
=> :ok

(mem-set! [:session/tasks :wire-levels]
          {:title "wire levels through emit-event"
           :spec :logging-edn :status :todo :deps [:introduce-emit-event]})
=> :ok

(done {:answer "Spec `:logging-edn` zapisany. Trzy taski, zaczynam od `:introduce-emit-event`."})
=> :done
```

### Turn 2

```
USER > Działaj.
```

Model reads `:session/tasks` in CTX, sees `:introduce-emit-event` has no
deps, picks it. Defines `emit-event` normally — Vis's existing
`definition_state` machinery persists it for free.

```clojure
MODEL >
(mem-set! [:session/tasks :introduce-emit-event :status] :doing)
=> :ok

(v/cat "src/logging.clj")
=> "(ns logging)\n(defn log [level msg] (println (str/format \"%s: %s\" level msg)))\n"

(defn emit-event [{:keys [level msg] :as ev}]
  (println (pr-str ev)))
=> #'vis.user/emit-event

(v/patch [{:path "src/logging.clj"
           :search "(defn log [level msg]\n  (println (str/format \"%s: %s\" level msg)))"
           :replace "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))\n\n(defn log [level msg]\n  (emit-event {:level level :msg msg}))"}])
=> {:applied 1}

(mem-set! [:session/tasks :introduce-emit-event :status] :done)
=> :ok

(done {:answer "Task `:introduce-emit-event` done. `emit-event` w pliku, i w runtime persistowane przez Vis."})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session.env/workspace {:branch "feat/logging" :trunk "main" :head "ghi9012"
                         :dirty? true :diff-stat "+5 -2 in src/logging.clj"}
 :session.env/symbols
   {emit-event {:arglists ([{:keys [level msg] :as ev}]) :doc nil :turn 2}}
 :session/specs
   {:logging-edn {:title "…" :status :draft :added-turn 1 …}}
 :session/tasks
   {:introduce-emit-event {:title "…" :spec :logging-edn :status :done
                           :added-turn 1 :done-turn 2 :deps []}
    :convert-call-sites   {:title "…" :spec :logging-edn :status :todo
                           :deps [:introduce-emit-event] :added-turn 1}
    :wire-levels          {:title "…" :spec :logging-edn :status :todo
                           :deps [:introduce-emit-event] :added-turn 1}}
 :session/decisions {} :session/rules {}}
```

### Turn 3 — symbol survives turn boundary; redefine + decision

Engine restored `emit-event` via existing `restore-sandbox!`. It's live.

```
USER > Wait, emit-event powinno mieć timestamp dodawany przez engine, nie przez caller.
```

```clojure
MODEL >
(mem-set! [:session/decisions :emit-event-auto-ts]
          {:body "emit-event auto-stamps :ts (UTC ms) if absent; callers pass only domain fields"
           :tags #{:logging :api}})
=> :ok

(defn emit-event [{:keys [level msg] :as ev}]
  (println (pr-str (assoc ev :ts (or (:ts ev) (System/currentTimeMillis))))))
=> #'vis.user/emit-event
;; latest definition_state row updates; :turn for this sym becomes 3

(mem-set! [:session/tasks :auto-stamp-ts]
          {:title "emit-event auto-stamp :ts"
           :spec :logging-edn :status :todo
           :deps [:introduce-emit-event]})
=> :ok

(done {:answer "Decyzja `:emit-event-auto-ts` zapisana. `emit-event` zaktualizowane, task `:auto-stamp-ts` dodany."})
=> :done
```

### CTX AFTER TURN 3

```clojure
{:session/turn 3
 :session.env/workspace {…}
 :session.env/symbols
   {emit-event {:arglists ([{:keys [level msg] :as ev}]) :doc nil :turn 3}}  ; :turn bumped on redef
 :session/decisions
   {:emit-event-auto-ts {:body "emit-event auto-stamps :ts (UTC ms) …"
                         :tags #{:logging :api} :added-turn 3}}
 :session/tasks
   {…
    :auto-stamp-ts {:status :todo :deps [:introduce-emit-event] :added-turn 3 …}}
 …}
```

### Turn 4 — call the symbol restored from prior turn

```
USER > Pokaż wynik wywołania emit-event z {:level :info :msg "ok"}.
```

```clojure
MODEL >
(with-out-str (emit-event {:level :info :msg "ok"}))
=> "{:level :info, :msg \"ok\", :ts 1758284401234}\n"

(done {:answer "```\n{:level :info, :msg \"ok\", :ts 1758284401234}\n```"})
=> :done
```

### Turn 5 — drop the symbol via `(def x nil)`

```
USER > OK, juz nie potrzebuję emit-event.
```

```clojure
MODEL >
(def emit-event nil)
=> #'vis.user/emit-event
;; latest definition_state row gets expression IS NULL → engine drops on next restore

(done {:answer "Usunięte. `emit-event` od następnej tury nie będzie materializowane."})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/turn 5
 :session.env/workspace {…}
 :session.env/symbols   {}                          ; emit-event omitted (latest expression IS NULL)
 …}
```

Turn 6 starts: engine's `restore-sandbox!` skips the nil-bound row.
`emit-event` is unavailable unless redefined.

---

## Example 4 — cross-session project rule

Session A teaches a project rule. Session B (next day, same repo) picks it up.

### Session A, Turn 7

```
USER > Pamiętaj, w tym projekcie testy muszą uderzać w prawdziwy SQLite, nigdy mockować.
```

```clojure
MODEL >
(mem-set! [:session/rules :real-db-tests]
          {:body "tests must hit real SQLite — no mocks"
           :scope :project})         ;; ← triggers mirror to project_rule table
=> :ok

(done {:answer "Reguła `:real-db-tests` zapisana jako project-scoped."})
=> :done
```

### Engine side-effect

```sql
INSERT INTO project_rule (repo_id, rule_key, body, added_at)
VALUES ('repo:vis', 'real-db-tests', 'tests must hit real SQLite — no mocks', …)
```

### Session B start (next day, same repo)

Engine loads `:session/rules` from new `session_state.ctx` = `{}`,
then merges project_rule rows for this repo:

```clojure
:session/rules
  {:real-db-tests {:body "tests must hit real SQLite — no mocks"
                   :scope :project
                   :source :project-mirror}}    ; engine-set; not from this session
```

### Session B, Turn 1

```
USER > Dodaj test dla emit-event.
```

Model reads `:session/rules :real-db-tests :body` from rendered CTX.

```clojure
MODEL >
(v/cat "test/logging_test.clj")
=> …existing tests…

(v/patch [{…test using real next.jdbc + tmp sqlite file…}])
=> {:applied 1}

(done {:answer "Test dodany. Używa tmp SQLite jak wymaga reguła `:real-db-tests`."})
=> :done
```

---

## Cross-cutting questions still open

1. **`:turn` semantics on `:session.env/symbols`.**
   - `:turn` = first-def (creation, immutable across redefs), or
   - `:turn` = last-redef (mutable; refreshed every time the form changes).
   Lean **last-redef** — more actionable ("is this fresh?"). First-def is
   reachable via DB query on demand.

2. **Workspace branch spawn — engine or model?**
   `:session.env/workspace :branch` reads `"main"` when the session has no
   workspace branch yet. Should the engine auto-spawn before any `v/patch`,
   or rely on existing Vis flow? Today the existing flow handles this; no
   change proposed.

3. **Spec status auto-promotion.**
   Example 3 didn't bump `:logging-edn :status` from `:draft` to `:doing`
   when first task started. Lean **manual** — explicit and inspectable.

4. **Overwrite warning on existing key.**
   `(mem-set! [:session/tasks :X] …)` when `:X` already exists with a
   different shape — silent replace, or `;; ⚠ overwriting [:session/tasks :X]
   (prior :status was :doing)`? Lean **warn**, never refuse.

5. **Cross-turn symbol use of `(meta #'sym)`.**
   After restore, does `(meta #'build-ws)` return the full `:arglists`/`:doc`
   the model expects? Depends on whether Vis's `restore-sandbox!` re-evals
   the source form (preserves meta) or only restores the value
   (loses meta). To probe live before locking the prompt copy.

---

## Final minimum-viable shape

```clojure
{:session/id     "01HXYZ"
 :session/turn   N

 :session.env/symbols    {sym {:arglists :doc :turn}}
 :session.env/workspace  {:branch :trunk :head :dirty? :diff-stat}

 :session/rules     {keyword {:body :scope :added-turn}}
 :session/decisions {keyword {:body :tags :added-turn}}
 :session/specs     {keyword {:title :acceptance :status :added-turn}}
 :session/tasks     {keyword {:title :spec :status :deps :added-turn :done-turn}}}
```

Six substantive subtrees. Two engine-rendered, four model-managed via
`mem-set!` / `mem-update!` / `mem-remove!`. Symbols managed natively via
`defn` / `(def x nil)` against existing Vis persistence.

Cross-turn history lives entirely in SQLite (`session_turn`,
`session_turn_iteration`); reach it via foundation calls
(`v/turn-message`, `v/turn-answer`, `v/iterations`, `v/turn-toc`).
Never preloaded into the prompt.
