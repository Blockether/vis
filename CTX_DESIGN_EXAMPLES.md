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
tool -> [] 
[user-message, tool, assistant; tool, tool tool tool, assistant] => duzy...

(def a "..czesc pliku a[100-300]")
(def b "..czesc pliku a[300-500])
(reduce [a, b])
kod początkowy -> TRANSFORMACJE -> kod v2 -> .....

1. Kontekst nie puchnie...
2. Dalej masz dostep do poprzednich informacji...
3. Nie puchnie ENV (SCI).. 


Three mid-turn verbs + finalizer. Pure semantics. No magic.

```clojure
;; ─── WRITE (mid-turn, memos only) ───
(mem-set!    path value)         ; assoc-in
(mem-update! path f & args)      ; update-in
(mem-remove! path)               ; dissoc-in

;; ─── FINAL (end of turn) ───
(done {:answer  "markdown string"
       :promote '[sym ...]       ; optional — add these SCI symbols to scratchpad
       :forget  '[sym ...]})     ; optional — drop these from scratchpad
```

Scratchpad mutations happen only at `done` time. Forces a single conscious
"what survives this turn" decision at the boundary, rather than mid-turn churn.
If turn fails (`:error` / `:cancelled`) before `done` — nothing promoted,
nothing forgotten. Atomic.

**No `ctx` symbol. No `(ctx)` fn. No reads in eval.**
CTX is always rendered into the user message of the next turn with inline
`;;` comments explaining each subtree. Model reads from the rendered text,
writes back via the four verbs.

## Entry keys

All entries in `:session/{tasks,specs,decisions,rules,preferences}` are keyed
by **model-chosen keywords**, not int ids. Refs across tables are bare keywords.

```clojure
(mem-set! [:session/tasks :wire-scratchpad-reeval]
          {:title "Wire :session/scratchpad re-eval at turn start"
           :spec   :ctx-redesign
           :status :todo
           :deps   [:dissoc-counters]})
```

Engine stamps `:added-turn` automatically on first `mem-set!` to a new key.
No counters subtree. No mint operator.

## Storage shape

```clojure
{:session/id          "01HXYZ"
 :session/turn        N

 ;; engine-managed (rebuilt every iter; model never writes here)
 :session/env
   {:workspace  {:id "ws/01HXYZ"
                 :root "/Users/fierycod/.vis/workspaces/repo:vis/ws-01HXYZ"
                 :branch "feat/ctx-redesign"
                 :trunk  "main"
                 :head   "abc1234"
                 :dirty? true
                 :diff-stat "+47 -12 across 3 files"}
    :symbols    {build-ws {:arglists ([id]) :doc "…"
                           :source :scratchpad}              ; from scratchpad re-eval
                 quick-helper {:arglists ([x])
                               :source :this-turn}}          ; defined in this turn, dies on (done …)
    :open-files #{"src/foo.clj"}}

 ;; model-managed (writes via mem-set! / mem-update! / mem-remove!)
 :session/preferences {…}
 :session/rules       {keyword {…}}
 :session/decisions   {keyword {…}}
 :session/specs       {keyword {…}}
 :session/tasks       {keyword {…}}

 ;; model-managed (scratchpad — uses dedicated verbs)
 :session/scratchpad  {sym '(defn sym […] …)}}
```

That's it. One engine subtree (`:env`), five model subtrees, one scratchpad.

## Rendered CTX block (what model actually sees)

This is the rendered shape pasted under `;; ctx` in the next user message.
Inline `;;` comments are stable per subtree. Positive-only framing.

```clojure
;; ════════════════════════════════════════════════════════════════════
;; CTX — session state. Renders fresh into every user message.
;; ════════════════════════════════════════════════════════════════════

{:session/id   "01HXYZ"
 :session/turn 7

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/env — live runtime view. Engine-managed; rebuilt each iter.
 ;;   :workspace  — current workspace branch + diff against trunk.
 ;;                 Full diff lives in (v/workspace.diff); this is summary only.
 ;;   :symbols    — SCI user-defined symbols visible right now.
 ;;                 :source :scratchpad — durable, survives turns.
 ;;                 :source :this-turn  — ephemeral, dies on (done …).
 ;;   :open-files — files this turn has read or written.
 ;; ──────────────────────────────────────────────────────────────────
 :session/env
   {:workspace  {:branch "feat/ctx-redesign" :trunk "main"
                 :head "abc1234" :dirty? true
                 :diff-stat "+47 -12 across 3 files"}
    :symbols    {build-ws {:arglists ([id]) :doc "…" :source :scratchpad}}
    :open-files #{"src/foo.clj"}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/preferences — user-stated facts about themselves or how they
 ;; want responses shaped. Keys are keywords. Values are anything EDN.
 ;;   To set    : (mem-set!    [:session/preferences :color] "yellow")
 ;;   To drop   : (mem-remove! [:session/preferences :color])
 ;;   Scope     : session lifetime; for cross-session use :session/rules.
 ;; ──────────────────────────────────────────────────────────────────
 :session/preferences {:color "yellow" :style :caveman-pl}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/rules — durable "always do X / always Y" rules.
 ;;   Shape    : {:body string :scope #{:session :project}}
 ;;   To add   : (mem-set!    [:session/rules :real-db-tests]
 ;;                           {:body "tests hit real SQLite"
 ;;                            :scope :project})
 ;;   :scope :project mirrors to project_rule table and loads into every
 ;;   new session in this repo.
 ;;   To drop  : (mem-remove! [:session/rules :real-db-tests])
 ;; ──────────────────────────────────────────────────────────────────
 :session/rules
   {:caveman-pl {:body "respond in PL caveman style"
                 :scope :session :added-turn 1}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/decisions — append-only "why we did X". Read-back for
 ;; design rationale questions; never updated; never auto-pruned.
 ;;   Shape   : {:body string :tags #{keyword}}
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
 ;;   To add  : (mem-set! [:session/specs :ctx-redesign] {…})
 ;;   To advance status:
 ;;             (mem-set! [:session/specs :ctx-redesign :status] :doing)
 ;; ──────────────────────────────────────────────────────────────────
 :session/specs
   {:ctx-redesign
      {:title "new CTX model"
       :acceptance ["no LLM compaction"
                    "no counters"
                    "patches supersede prior reads in same turn"]
       :status :doing :added-turn 4}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/tasks — work items. Reference specs and other tasks by keyword.
 ;;   Shape   : {:title string :spec keyword :status keyword :deps [keyword]}
 ;;   :status ∈ #{:todo :doing :done :blocked :cancelled}
 ;;   To add  : (mem-set! [:session/tasks :wire-reeval]
 ;;                       {:title "…" :spec :ctx-redesign
 ;;                        :status :todo :deps [:dissoc-counters]})
 ;;   To advance:
 ;;             (mem-set! [:session/tasks :wire-reeval :status] :doing)
 ;; ──────────────────────────────────────────────────────────────────
 :session/tasks
   {:dissoc-counters
      {:title "remove :session/counters and gen-id"
       :spec :ctx-redesign :status :done :deps []
       :added-turn 4 :done-turn 5}
    :wire-reeval
      {:title "wire scratchpad re-eval at turn start"
       :spec :ctx-redesign :status :doing :deps [:dissoc-counters]
       :added-turn 4}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/scratchpad — durable callables. Engine re-evals each form
 ;; silently at turn start, before iter 1. Forms become live SCI symbols.
 ;;
 ;;   To call    : (build-ws "x")
 ;;                — invoke as a normal function; it is already live.
 ;;
 ;;   To promote : define normally this turn, then list in done :promote.
 ;;                  (defn build-ws [id] (workspace/spawn-branch! id))
 ;;                  …
 ;;                  (done {:answer "…" :promote '[build-ws]})
 ;;
 ;;   To drop    : list in done :forget.
 ;;                  (done {:answer "…" :forget '[build-ws]})
 ;;                — next turn the symbol stops being materialized.
 ;;
 ;;   To inspect : read this map for the source form, or use
 ;;                (meta #'build-ws) for live arglists/doc.
 ;; ──────────────────────────────────────────────────────────────────
 :session/scratchpad
   {build-ws '(defn build-ws [id] (workspace/spawn-branch! id))}}
```

---

## Example 1 — pure conversation, recall-after-5-turns

User chatting, no code. Tests whether `:session/preferences` carries trivially-stated facts across turns.

### Turn 1

```
USER > Lubię żółty.
```

```clojure
MODEL >
(mem-set! [:session/preferences :color] "yellow")
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
(mem-set! [:session/preferences :has-pet] :dog)
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
:session/preferences {:color "yellow" :has-pet :dog}
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
 :session/env  {:workspace {:branch "main" :dirty? false …}
                :symbols {} :open-files #{}}
 :session/preferences {:color "yellow" :has-pet :dog}
 :session/rules {} :session/decisions {} :session/specs {}
 :session/tasks {} :session/scratchpad {}}
```

**Observations to discuss:**
- Conversation works without any per-turn breadcrumb. `:session/preferences` carries the facts; `(v/turn-message N)` / `(v/turn-answer N)` give per-turn detail on demand.
- The model never needed to "recall a turn"; it read durable state.

---

## Example 2 — read-patch-verify (within-turn supersede)

Coding flow. Tests that within-turn patches mark prior reads as superseded in the iter trailer.

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
 :session/env  {:workspace {:branch "main" :dirty? false :head "abc1234" …}
                :symbols {} :open-files #{}}             ; per-turn open-files cleared
 :session/preferences {} :session/rules {} :session/decisions {}
 :session/specs {} :session/tasks {} :session/scratchpad {}}
```

### Turn 2

```
USER > Zamień to literal compare na bcrypt verify.
```

Model has no working memory of Turn 1's `v/cat` result. To act safely, re-read.
Within this turn, `v/patch` against `src/auth.clj` marks the iter-1 `v/cat`
result of this turn as superseded in the trailer.

```clojure
MODEL >
(v/cat "src/auth.clj")          ;; iter [2 1] — fresh read
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(v/patch [{:path "src/auth.clj"
           :search "(defn check [tok] (= tok \"secret\"))"
           :replace "(defn check [tok] (bcrypt/check tok stored-hash))"}])
;; iter [2 2]
=> {:applied 1 :path "src/auth.clj"}
;; (trailer marks iter [2 1] :v/cat src/auth.clj :superseded-by [2 2])

(done {:answer "Zamieniono `check` na bcrypt verify."})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/env  {:workspace {:branch "main" :dirty? true
                            :diff-stat "+1 -1 in src/auth.clj" …}
                :symbols {} :open-files #{"src/auth.clj"}}     ; touched this turn
 …}
```

**Observations:**
- No `:session/file-truth` in storage. Supersede marker lives only in the
  within-turn trailer rendering. Cross-turn there is no "file state cache";
  the FS is the truth (re-read via `v/cat`).

---

## Example 3 — spec + tasks + decision + scratchpad lifecycle

Multi-turn coding job. Tests `:session/specs`, `:session/tasks`,
`:session/decisions`, `:session/scratchpad`.

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
deps, picks it. Defines `emit-event` then promotes to scratchpad so the
follow-up tasks can call it without redefinition.

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

(done {:answer  "Task `:introduce-emit-event` done. `emit-event` w pliku + w scratchpadzie."
       :promote '[emit-event]})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/env
   {:workspace {:branch "feat/logging" :dirty? true
                :diff-stat "+5 -2 in src/logging.clj" …}
    :symbols    {emit-event {:arglists ([{:keys [level msg] :as ev}])
                             :source :this-turn}}    ; live this turn, dies on (done)
    :open-files #{"src/logging.clj"}}
 :session/specs
   {:logging-edn {:title "…" :status :draft :added-turn 1 …}}
 :session/tasks
   {:introduce-emit-event {:title "…" :spec :logging-edn :status :done
                           :added-turn 1 :done-turn 2 :deps []}
    :convert-call-sites   {:title "…" :spec :logging-edn :status :todo
                           :deps [:introduce-emit-event] :added-turn 1}
    :wire-levels          {:title "…" :spec :logging-edn :status :todo
                           :deps [:introduce-emit-event] :added-turn 1}}
 :session/scratchpad
   {emit-event '(defn emit-event [{:keys [level msg] :as ev}]
                  (println (pr-str ev)))}}
```

### Turn 3 — scratchpad re-eval

Engine reads `:session/scratchpad` at turn start, evals `emit-event` source
silently. `:session/env :symbols` now shows `:source :scratchpad`.

```
USER > Wait, emit-event powinno mieć timestamp dodawany przez engine, nie przez caller.
```

Model records a decision, edits the scratchpad form, then re-promotes via
redefining + re-adding.

```clojure
MODEL >
(mem-set! [:session/decisions :emit-event-auto-ts]
          {:body "emit-event auto-stamps :ts (UTC ms) if absent; callers pass only domain fields"
           :tags #{:logging :api}})
=> :ok

(defn emit-event [{:keys [level msg] :as ev}]
  (println (pr-str (assoc ev :ts (or (:ts ev) (System/currentTimeMillis))))))
=> #'vis.user/emit-event

(mem-set! [:session/tasks :auto-stamp-ts]
          {:title "emit-event auto-stamp :ts"
           :spec :logging-edn :status :todo
           :deps [:introduce-emit-event]})
=> :ok

(done {:answer  "Decyzja `:emit-event-auto-ts` zapisana. Scratchpadowa wersja `emit-event` zaktualizowana, dodany task `:auto-stamp-ts`."
       :promote '[emit-event]})       ;; overwrites prior scratchpad form with the redefined source
=> :done
```

### CTX AFTER TURN 3

```clojure
{:session/turn 3
 :session/env
   {:workspace {…}
    :symbols   {emit-event {:arglists ([{:keys [level msg] :as ev}])
                            :source :this-turn}}        ; redefined this turn; promoted to scratchpad
    :open-files #{}}
 :session/decisions
   {:emit-event-auto-ts {:body "emit-event auto-stamps :ts (UTC ms) …"
                         :tags #{:logging :api} :added-turn 3}}
 :session/tasks
   {:introduce-emit-event {…done…}
    :convert-call-sites   {…todo…}
    :wire-levels          {…todo…}
    :auto-stamp-ts        {:status :todo :deps [:introduce-emit-event]
                           :added-turn 3 …}}
 :session/scratchpad
   {emit-event '(defn emit-event [{:keys [level msg] :as ev}]
                  (println (pr-str (assoc ev :ts (or (:ts ev) (System/currentTimeMillis))))))}}
```

### Turn 4 — scratchpad re-eval (proves cross-turn callable)

```
USER > Pokaż wynik wywołania emit-event z {:level :info :msg "ok"}.
```

Engine has already re-evaled scratchpad at turn start. `emit-event` is live.

```clojure
MODEL >
(with-out-str (emit-event {:level :info :msg "ok"}))
=> "{:level :info, :msg \"ok\", :ts 1758284401234}\n"

(done {:answer "```\n{:level :info, :msg \"ok\", :ts 1758284401234}\n```"})
=> :done
```

`:session/env :symbols` shows `emit-event :source :scratchpad` this turn —
no redefinition needed; engine bootstrapped it.

### Turn 5 — drop from scratchpad

```
USER > OK, juz nie potrzebuję emit-event w scratchpadzie.
```

```clojure
MODEL >
(done {:answer "Usunięte. `emit-event` od następnej tury nie będzie materializowane."
       :forget '[emit-event]})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/turn 5
 :session/env  {:workspace {…} :symbols {} :open-files #{}}
 :session/scratchpad {}                      ; empty
 …}
```

Turn 6 starts: engine re-evals nothing, `emit-event` is gone unless model
redefines locally.

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
           :scope :project})       ;; ← triggers mirror to project_rule table
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

Engine loads `:session/rules` blob from new `session_state.ctx` = `{}`.
Then merges from `project_rule` for this repo:

```clojure
:session/rules
  {:real-db-tests {:body "tests must hit real SQLite — no mocks"
                   :scope :project
                   :source :project-mirror}}   ;; engine-set; tells model "not from this session"
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

1. **Subject on turns — keep or drop?**
   Examples above never needed a turn breadcrumb. `(v/turn-message N)` /
   `(v/turn-answer N)` cover "what happened in turn N" lazily. Proposal:
   drop subjects entirely. No `:session/recent`, no `:session/archived`.

2. **Workspace branch on session start.**
   Engine renders `:workspace :branch "main"` when the session has no
   workspace branch yet. Should the model proactively spawn a workspace
   branch before any `v/patch`? Or rely on the existing Vis flow?

3. **Spec status promotion — automatic or manual?**
   Example 3 didn't bump `:logging-edn :status` from `:draft` to `:doing`
   when first task started. Lean **manual** — explicit and inspectable.

4. **Rules and preferences distinction.**
   "I prefer caveman PL" — `:session/preferences` or `:session/rules`?
   Proposal:
   - `:session/preferences` — facts about the user / their setup
     (`:color :has-pet :timezone :editor`).
   - `:session/rules`        — imperatives that bind future model behavior
     (`:caveman-pl :real-db-tests :no-mocks`).
   Both keyword-keyed. Rules can be `:scope :project`; preferences cannot
   (preferences are per-user-session by definition).

5. **Scratchpad scope.**
   `:session/scratchpad` is session-only. Cross-session callables would
   need a project-scoped scratchpad. Deferred to v2.

6. **Overwrite warning on existing key.**
   `(mem-set! [:session/tasks :X] …)` when `:X` already exists — silent
   replace, or `;; ⚠ overwriting :session/tasks :X (prior :status was :doing)`?
   Lean **warn**, never refuse.

---

## Final minimum-viable shape

```clojure
{:session/id          "01HXYZ"
 :session/turn        N

 :session/env         {:workspace {…} :symbols {…} :open-files #{…}}

 :session/preferences {keyword value}
 :session/rules       {keyword {:body :scope :added-turn}}
 :session/decisions   {keyword {:body :tags :added-turn}}
 :session/specs       {keyword {:title :acceptance :status :added-turn}}
 :session/tasks       {keyword {:title :spec :status :deps :added-turn :done-turn}}
 :session/scratchpad  {sym '(defn sym […] …)}}
```

Six subtrees. One engine-managed (`:env`), four model-managed via the
generic `mem-set!` / `mem-update!` / `mem-remove!`, one (`:scratchpad`)
mutated only at turn boundary via `(done {:promote … :forget …})`.

Cross-turn history lives entirely in SQLite (`session_turn`,
`session_turn_iteration`); reach it via foundation calls
(`v/turn-message`, `v/turn-answer`, `v/iterations`, `v/turn-toc`).
Never preloaded into the prompt.
