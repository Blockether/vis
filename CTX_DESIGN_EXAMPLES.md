# CTX design — example conversations

Goal: stress-test the proposed CTX shape with realistic flows.
Read these, mark what feels invented, what feels essential, what's missing.

## Rationale

Two anchors fix this design.

**1. Vis's op-tag taxonomy is the canonical mutation/observation source.**
Every op registered through `vis/register-op!` carries a `:tag` of either
`:op.tag/observation` or `:op.tag/mutation`. The taxonomy is registered
once in extension source and consumed by every channel renderer
(`channel_tui/render.clj` maps the tag to colour). The trailer entries in
this design reuse those exact keyword values: zero translation layer,
zero risk of drift. Forms that are not registered ops — `defn`, `def`,
the `mem-*` verbs, plain expressions — are classified by engine-side
pattern at trailer-promotion time and resolve to the same two values.

**2. The model's SCI surface is the registered `:v/` and extension table,
nothing more.** All cross-turn introspection rides on
`(v/session-state)` and `(v/session-report)` — the only registered
introspection ops. There is no per-turn fetch surface (no `v/turn-*`, no
`v/iterations`); model filters the structured map returned by
`v/session-state` instead. Workspace mutations (branch spawn, apply,
discard) live as internal Clojure fns in `src/.../internal/workspace.clj`
and are reached through engine flows, not SCI calls; the model sees
workspace as a read-only block in CTX (`:session/workspace`) plus the
`git/` extension's observation triplet (`git/diff`, `git/status`,
`git/log`). File mutations go through the registered editing surface
(`v/patch`, `v/move`, `v/delete`, `v/copy`, `v/create-dirs`,
`v/delete-if-exists`).

Together these mean: the trailer is **a structured replay of forms that
were already inside Vis's existing classification machinery**, with the
same vocabulary the rest of the system uses. No new tag domain, no
shadow op table.

## Conventions

- `USER >` — user message
- `MODEL >` — what model emits in a single ```clojure``` code block per iter
- `=> …` — engine eval result
- `CTX AFTER TURN N:` — rendered CTX block the model sees in the next user message
- Lazy history fetches: `(v/session-state)` / `(v/session-report)` — the ONLY registered
  introspection ops; per-turn ops (`v/turn-*`, `v/iterations`) do NOT exist.

## Operator API — final

```clojure
;; ─── MEMOS (mid-turn; model writes; stored in session_state.ctx) ───
(mem-set!    path value)         ; assoc-in
(mem-update! path f & args)      ; update-in
(mem-remove! path)               ; dissoc-in

;; ─── SYMBOLS (native SCI + Vis's existing definition_state machinery) ───
(defn foo [x] …)                 ; create / overwrite — auto-persists
(def  foo nil)                   ; drop — engine treats latest expression IS NULL as forgotten

;; ─── FINAL + TRAILER ───
(done {:answer       "markdown string"
       :trailer      [{:i N :note "..."} …]      ; pin iters of current turn (omit :t = current)
       :trailer-drop [[t i] …]})                  ; drop by composite coord
```

**No `ctx` symbol. No `(ctx)` fn. No reads in eval.**

CTX is always rendered into the user message of the next turn as a bare
EDN literal under a `;; ctx` marker, **NEVER** as `(def ctx {…})`. Two
reasons:

1. **No SCI binding** — `ctx` is not a var; model attempting `(get-in ctx …)`
   errors with `Unable to resolve symbol: ctx`. Footgun structurally impossible.
2. **Storage / render separation** — the rendered text is a read-only snapshot;
   the persistent state lives in `session_state.ctx` (SQLite TEXT blob).
   Mutations go through `mem-*` verbs and `done` keys, never through SCI bindings.

## Entry keys

Model-managed memos (`:rules :decisions :specs :tasks`) are keyed by
**model-chosen keywords**. Cross-table refs are bare keywords.

```clojure
(mem-set! [:session/tasks :wire-trailer-render]
          {:title  "Render :session/trailer with per-form envelopes"
           :spec   :ctx-redesign
           :status :todo
           :deps   [:dissoc-counters]})
```

Engine stamps `:added-turn (:session/turn ctx)` on first `mem-set!` to a
new key. No counters subtree. No mint operator.

## Storage shape

```clojure
{:session/id     "01HXYZ"
 :session/turn   N

 ;; engine-rendered views (NOT stored as EDN — derived per dump)
 :session/workspace  {:branch :trunk :head :dirty? :stats}
 :session/symbols    {sym {:arglists :doc :defined}}

 ;; model-managed memos (stored as EDN inside session_state.ctx)
 :session/rules      {keyword {:body :scope :added-turn}}
 :session/decisions  {keyword {:body :tags :added-turn}}
 :session/specs      {keyword {:title :acceptance :status :added-turn}}
 :session/tasks      {keyword {:title :spec :status :deps :added-turn :done-turn}}

 ;; model-managed only via (done {:trailer … :trailer-drop …})
 :session/trailer    [{:scope :note :forms [{:scope :tag :src :result :error}]}]}
```

Seven substantive subtrees. Two engine-rendered, four memos via `mem-*`,
one trailer via `done`.

## Tag classification (cross-validated against source)

For REGISTERED ops, engine reads `:tag` from `extension/op-tag` table.
For non-registered forms, engine derives by head-symbol pattern. Both
resolve to canonical `:op.tag/observation` or `:op.tag/mutation`.

### Registered ops (verified)

From `extensions/common/vis-foundation-core/src/.../core.clj` and `.../editing/core.clj`:

| op | tag |
|---|---|
| `:v/cat` `:v/ls` `:v/rg` `:v/exists?` `:v/snapshot` | `:op.tag/observation` |
| `:v/repositories` `:v/git` `:v/languages` `:v/monorepo` `:v/main-agent-instructions` | `:op.tag/observation` |
| `:v/session-state` `:v/session-report` | `:op.tag/observation` |
| `:v/engine-symbol-documentation` `:v/engine-symbol-source-code` `:v/engine-symbol-metadata` `:v/engine-symbol-apropos` | `:op.tag/observation` |
| `:v/patch` `:v/create-dirs` `:v/copy` `:v/move` `:v/delete` `:v/delete-if-exists` | `:op.tag/mutation` |
| `:v/refresh!` `:v/reload-extensions!` | `:op.tag/mutation` |

From `extensions/common/vis-foundation-git/src/.../core.clj`:

| op | tag |
|---|---|
| `:git/diff` `:git/status` `:git/log` | `:op.tag/observation` |

From `extensions/common/vis-foundation-exa/src/.../core.clj`: all `:op.tag/observation`.

### Non-registered forms (engine derives by head symbol)

| head pattern | tag |
|---|---|
| `(defn …)` `(def …)` | `:op.tag/mutation` |
| `(mem-set! …)` `(mem-update! …)` `(mem-remove! …)` | `:op.tag/mutation` |
| arithmetic, str ops, `get-in`, `filter`, plain expressions | `:op.tag/observation` |
| `(done …)` | excluded from trailer (it IS the finalizer) |

### Render-shortening question (open)

Storage / canonical = namespaced (`:op.tag/observation`). For CTX render
in the user message, do we shorten to bare `:observation` to save tokens?
Lean **shorten in render**, keep canonical in storage. Examples below
render canonical to match codebase exactly.

### Compactness rules per form

- Drop `:result` when value is the trivial default (`:ok` for `:mem`, `#'sym` for `:def`/`:defn`).
- Drop `:result` when nil but `:error` is present (error implies no result).
- Drop `:error` when nil.

## Rendered CTX block (what model actually sees)

Bare EDN literal under `;; ctx` marker. **No `(def ctx …)`.** Plain text in the user message, not code.

```clojure
;; ════════════════════════════════════════════════════════════════════
;; ctx — session state snapshot. Read this. Write back via mem-* verbs.
;; ════════════════════════════════════════════════════════════════════

{:session/id   "01HXYZ"
 :session/turn 7

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/workspace — current workspace + diff against trunk.
 ;;   :stats is per-file added/removed. Full diff via (git/diff).
 ;; ──────────────────────────────────────────────────────────────────
 :session/workspace
   {:branch "feat/ctx-redesign" :trunk "main" :head "abc1234" :dirty? true
    :stats  {"src/auth.clj"     {:added 5  :removed 2}
             "src/logging.clj"  {:added 47 :removed 0}
             "test/x.clj"       {:added 0  :removed 12}}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/symbols — SCI user-defined symbols that are live right now.
 ;;   :born <scope-string> = the form coord (t<N>/i<N>/f<N>) where the current definition was set.
 ;;   Symbols (defined as) nil are dropped from this view automatically.
 ;;
 ;;   To call    : (build-ws "x")
 ;;                — invoke as a normal function; it is already live.
 ;;   To create  : (defn build-ws [id] …)   — engine persists it; survives turn boundary.
 ;;   To drop    : (def build-ws nil)       — engine drops on next restore.
 ;;   To inspect : (meta #'build-ws)        — live arglists/doc.
 ;; ──────────────────────────────────────────────────────────────────
 :session/symbols
   {auth-check {:arglists ([tok]) :doc "literal-compare check; deprecated" :born "t5/i1/f1"}
    emit-event {:arglists ([{:keys [level msg] :as ev}]) :born "t4/i1/f1"}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/rules — durable facts about the user and how to respond.
 ;;   Captures user-stated preferences, behavioral directives, project
 ;;   conventions. Read these before every answer.
 ;;   Shape   : {:body string :scope #{:session :project}}
 ;;
 ;;   To add  : (mem-set!    [:session/rules :caveman-pl]
 ;;                          {:body "respond in PL caveman" :scope :session})
 ;;   To drop : (mem-remove! [:session/rules :caveman-pl])
 ;;
 ;;   :scope :project mirrors to the project_rule table; loads into every
 ;;   new session in this repo.
 ;; ──────────────────────────────────────────────────────────────────
 :session/rules
   {:caveman-pl    {:body "respond in PL caveman style" :scope :session :added-turn 1}
    :likes-yellow  {:body "user likes yellow color"     :scope :session :added-turn 1}
    :real-db-tests {:body "tests must hit real SQLite — no mocks" :scope :project :added-turn 7}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/decisions — append-only "why we did X". Never updated;
 ;;   never auto-pruned. Shape : {:body string :tags #{keyword}}
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
 ;; :session/specs — formal requirements + acceptance criteria.
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
                    "symbols persist via existing definition_state machinery"
                    "trailer carries per-form envelopes with :tag classification"]
       :status :doing :added-turn 4}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/tasks — work items. Reference specs / other tasks by keyword.
 ;;   Shape   : {:title string :spec keyword :status keyword :deps [keyword]}
 ;;   :status ∈ #{:todo :doing :done :blocked :cancelled}
 ;;
 ;;   To add     : (mem-set! [:session/tasks :wire-trailer-render]
 ;;                          {:title "…" :spec :ctx-redesign
 ;;                           :status :todo :deps [:dissoc-counters]})
 ;;   To advance : (mem-set! [:session/tasks :wire-trailer-render :status] :doing)
 ;; ──────────────────────────────────────────────────────────────────
 :session/tasks
   {:dissoc-counters       {:title "remove :session/counters" :spec :ctx-redesign
                            :status :done :deps [] :added-turn 4 :done-turn 5}
    :wire-trailer-render   {:title "render :session/trailer with per-form envelopes"
                            :spec :ctx-redesign :status :doing
                            :deps [:dissoc-counters] :added-turn 4}}

 ;; ──────────────────────────────────────────────────────────────────
 ;; :session/trailer — pinned iter envelopes from prior turns.
 ;;   Sorted by [t i] ascending. Each entry captures one iter:
 ;;     {:scope "t<N>/i<N>" :note "…" :forms [{:scope "t<N>/i<N>/f<N>" :tag :src :result :error}]}
 ;;   :tag ∈ #{:observation :mutation}.
 ;;   Per-form keys :result/:error are dropped when default.
 ;;   :done forms are excluded.
 ;;
 ;;   To pin   : (done {:answer "..." :trailer [{:i M :note "..."} …]})
 ;;              — engine fills :forms from this turn's iter rows; stamps :t.
 ;;              — pin a prior turn's iter by providing explicit :t.
 ;;   To drop  : (done {:answer "..." :trailer-drop [[t i] …]})
 ;;
 ;;   Full session detail via (v/session-state).
 ;; ──────────────────────────────────────────────────────────────────
 :session/trailer
   [{:scope "t3/i2" :note "auth literal compare on \"secret\""
     :forms [{:scope "t3/i2/f1" :tag :op.tag/observation :src "(v/cat \"src/auth.clj\")"
              :result "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"}
             {:scope "t3/i2/f2" :tag :op.tag/mutation    :src "(defn auth-check [tok] (= tok \"secret\"))"}
             {:scope "t3/i2/f3" :tag :op.tag/mutation    :src "(mem-set! [:session/tasks :wire :status] :doing)"}]}
    {:scope "t3/i4" :note "patched check/1 → bcrypt verify"
     :forms [{:scope "t3/i4/f1" :tag :op.tag/mutation :src "(v/patch [{:path \"src/auth.clj\" :search \"…\" :replace \"…\"}])"
              :result {:applied 1 :path "src/auth.clj"}}
             {:scope "t3/i4/f2" :tag :op.tag/mutation :src "(mem-set! [:session/tasks :wire :status] :done)"}]}
    {:scope "t5/i1" :note "git status post-patch"
     :forms [{:scope "t5/i1/f1" :tag :op.tag/observation :src "(git/status)"
              :result {:branch "feat/auth-bcrypt" :ahead 1 :modified ["src/auth.clj"]}}]}]}
```

---

## Example 1 — pure conversation, recall-after-5-turns

User chatting, no code. Tests `:session/rules` as the single bucket for user facts + behavior.

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

Model reads the `;; ctx` block, sees:
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
 :session/workspace {:branch "main" :trunk "main" :head "abc1234" :dirty? false :stats {}}
 :session/symbols   {}
 :session/rules
   {:likes-yellow {:body "user likes yellow color" :scope :session :added-turn 1}
    :has-pet-dog  {:body "user has a dog"          :scope :session :added-turn 3}}
 :session/decisions {} :session/specs {} :session/tasks {}
 :session/trailer   []}
```

**Observations:**
- One bucket (`:session/rules`) holds both trivial facts and behavioral rules.
- No `:session/trailer` entries — conversation needed no pinned iter.
- `(v/session-state)` returns structured session data; model filters by turn.

---

## Example 2 — read-patch-verify (workspace stats reflect edits)

### Turn 1
```
USER > Pokaż mi src/auth.clj i powiedz co tam jest.
```
```clojure
MODEL >
(v/cat "src/auth.clj")          ;; scope t1/i1
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(done {:answer "Plik definiuje `check/1`: literal compare na `\"secret\"`."})
=> :done
```

### CTX AFTER TURN 1
```clojure
{:session/turn 1
 :session/workspace {:branch "main" :trunk "main" :head "abc1234" :dirty? false :stats {}}
 :session/symbols {} :session/rules {} :session/decisions {} :session/specs {}
 :session/tasks {} :session/trailer []}
```

### Turn 2
```
USER > Zamień to literal compare na bcrypt verify.
```

Within turn: re-read (cheap), patch. Engine renders supersede marker on
the iter-1 `v/cat` in this turn's trailer (within-turn chat ping-pong).
Pin the patch iter for cross-turn recall.

```clojure
MODEL >
(v/cat "src/auth.clj")          ;; scope t2/i1
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(v/patch [{:path "src/auth.clj"
           :search "(defn check [tok] (= tok \"secret\"))"
           :replace "(defn check [tok] (bcrypt/check tok stored-hash))"}])
;; scope t2/i2
=> {:applied 1 :path "src/auth.clj"}

(done {:answer  "Zamieniono `check` na bcrypt verify."
       :trailer [{:i 2 :note "patched check/1 → bcrypt verify"}]})
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/workspace
   {:branch "feat/auth-bcrypt" :trunk "main" :head "def5678" :dirty? true
    :stats {"src/auth.clj" {:added 1 :removed 1}}}
 :session/symbols   {}
 :session/trailer
   [{:scope "t2/i2" :note "patched check/1 → bcrypt verify"
     :forms [{:scope "t2/i2/f1" :tag :op.tag/mutation
              :src "(v/patch [{:path \"src/auth.clj\" :search \"(defn check [tok] (= tok \\\"secret\\\"))\"
                              :replace \"(defn check [tok] (bcrypt/check tok stored-hash))\"}])"
              :result {:applied 1 :path "src/auth.clj"}}]}]
 :session/rules {} :session/decisions {} :session/specs {} :session/tasks {}}
```

---

## Example 3 — spec + tasks + decision + symbols lifecycle + trailer

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
(mem-set! [:session/tasks :introduce-emit-event]
          {:title "introduce emit-event fn"
           :spec :logging-edn :status :todo :deps []})
(mem-set! [:session/tasks :convert-call-sites]
          {:title "convert str/format call-sites"
           :spec :logging-edn :status :todo :deps [:introduce-emit-event]})
(mem-set! [:session/tasks :wire-levels]
          {:title "wire levels through emit-event"
           :spec :logging-edn :status :todo :deps [:introduce-emit-event]})

(done {:answer "Spec `:logging-edn` zapisany. Trzy taski, zaczynam od `:introduce-emit-event`."})
=> :done
```

### Turn 2
```
USER > Działaj.
```
```clojure
MODEL >
(mem-set! [:session/tasks :introduce-emit-event :status] :doing)

(v/cat "src/logging.clj")          ;; scope t2/i2
=> "(ns logging)\n(defn log [level msg] (println (str/format \"%s: %s\" level msg)))\n"

(defn emit-event [{:keys [level msg] :as ev}]   ;; scope t2/i3
  (println (pr-str ev)))
=> #'vis.user/emit-event

(v/patch [{:path "src/logging.clj"
           :search "(defn log [level msg]\n  (println (str/format \"%s: %s\" level msg)))"
           :replace "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))\n\n(defn log [level msg]\n  (emit-event {:level level :msg msg}))"}])
=> {:applied 1}

(mem-set! [:session/tasks :introduce-emit-event :status] :done)

(done {:answer  "Task `:introduce-emit-event` done. `emit-event` w pliku, persisted by Vis."
       :trailer [{:i 3 :note "defined emit-event"}
                 {:i 4 :note "patched src/logging.clj to use emit-event"}]})
=> :done
```

### CTX AFTER TURN 2 (excerpt)

```clojure
{:session/turn 2
 :session/workspace
   {:branch "feat/logging" :trunk "main" :head "ghi9012" :dirty? true
    :stats {"src/logging.clj" {:added 5 :removed 2}}}
 :session/symbols
   {emit-event {:arglists ([{:keys [level msg] :as ev}]) :born "t2/i3/f1"}}
 :session/tasks
   {:introduce-emit-event {:status :done :done-turn 2 :added-turn 1 :spec :logging-edn :deps []}
    :convert-call-sites   {:status :todo :deps [:introduce-emit-event] :added-turn 1 …}
    :wire-levels          {:status :todo :deps [:introduce-emit-event] :added-turn 1 …}}
 :session/trailer
   [{:scope "t2/i3" :note "defined emit-event"
     :forms [{:scope "t2/i3/f1" :tag :op.tag/mutation
              :src "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))"}]}
    {:scope "t2/i4" :note "patched src/logging.clj to use emit-event"
     :forms [{:scope "t2/i4/f1" :tag :op.tag/mutation
              :src "(v/patch [{:path \"src/logging.clj\" :search \"…\" :replace \"…\"}])"
              :result {:applied 1}}]}]}
```

### Turn 3 — redefine + decision

```
USER > Wait, emit-event powinno mieć timestamp dodawany przez engine, nie przez caller.
```
```clojure
MODEL >
(mem-set! [:session/decisions :emit-event-auto-ts]
          {:body "emit-event auto-stamps :ts (UTC ms) if absent"
           :tags #{:logging :api}})

(defn emit-event [{:keys [level msg] :as ev}]   ;; scope t3/i2
  (println (pr-str (assoc ev :ts (or (:ts ev) (System/currentTimeMillis))))))
=> #'vis.user/emit-event

(mem-set! [:session/tasks :auto-stamp-ts]
          {:title "emit-event auto-stamp :ts" :spec :logging-edn
           :status :todo :deps [:introduce-emit-event]})

(done {:answer  "Decyzja `:emit-event-auto-ts` zapisana. `emit-event` zaktualizowane, task `:auto-stamp-ts` dodany."
       :trailer [{:i 2 :note "added auto-ts stamping to emit-event"}]})
=> :done
```

### CTX AFTER TURN 3 (symbols + trailer excerpt)
```clojure
:session/symbols
  {emit-event {:arglists ([{:keys [level msg] :as ev}]) :born "t3/i2/f1"}}   ;; born refreshed on redef

:session/trailer
  [{:scope "t2/i3" :note "defined emit-event" :forms […]}
   {:scope "t2/i4" :note "patched src/logging.clj to use emit-event" :forms […]}
   {:scope "t3/i2" :note "added auto-ts stamping to emit-event"
    :forms [{:scope "t3/i2/f1" :tag :op.tag/mutation
             :src "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str (assoc ev :ts (or (:ts ev) (System/currentTimeMillis))))))"}]}]
```

### Turn 4 — call symbol restored across turn boundary
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

### Turn 5 — drop symbol + clean older trailer

```
USER > OK, już nie potrzebuję emit-event. Wyczyść też ślady promocji z trailera.
```
```clojure
MODEL >
(def emit-event nil)               ;; scope t5/i1

(done {:answer       "Usunięte. `emit-event` od następnej tury nieaktywne. Wyczyszczone wpisy z trailera."
       :trailer-drop ["t2/i3" "t2/i4" "t3/i2"]})
=> :done
```

### CTX AFTER TURN 5
```clojure
:session/symbols {}                 ;; emit-event omitted (latest expression IS NULL)
:session/trailer []                 ;; dropped
```

---

## Example 4 — cross-session project rule

### Session A, Turn 7
```
USER > Pamiętaj, w tym projekcie testy muszą uderzać w prawdziwy SQLite, nigdy mockować.
```
```clojure
MODEL >
(mem-set! [:session/rules :real-db-tests]
          {:body "tests must hit real SQLite — no mocks" :scope :project})

(done {:answer "Reguła `:real-db-tests` zapisana jako project-scoped."})
=> :done
```

### Engine side-effect
```sql
INSERT INTO project_rule (repo_id, rule_key, body, added_at)
VALUES ('repo:vis', 'real-db-tests', 'tests must hit real SQLite — no mocks', …)
```

### Session B start (next day, same repo)
```clojure
:session/rules
  {:real-db-tests {:body "tests must hit real SQLite — no mocks"
                   :scope :project
                   :source :project-mirror}}      ;; engine-set; not from this session
```

### Session B, Turn 1
```
USER > Dodaj test dla emit-event.
```
```clojure
MODEL >
(v/cat "test/logging_test.clj")
…
(v/patch [{…real-DB test…}])

(done {:answer "Test dodany. Używa tmp SQLite jak wymaga reguła `:real-db-tests`."})
=> :done
```

---

## Cross-cutting questions still open

1. **Render-shortening of tag value.** Storage / canonical =
   `:op.tag/observation`. In CTX user-message render: shorten to bare
   `:observation` for tokens vs keep canonical for consistency.
   Lean **shorten in render**. Examples above render canonical to
   match codebase exactly.

2. **`:born <scope>` semantics on `:session/symbols`.** Currently = LAST
   redef coord. Earlier open question (first-def vs last-redef) resolved
   in favour of last-redef. Confirm.

3. **Workspace branch spawn — engine or model?** `:branch "main"` on a
   fresh session: should engine auto-spawn before any `v/patch`, or rely
   on existing Vis flow? Today's flow handles this; no change proposed.

4. **Spec status auto-promotion.** Example 3 didn't bump `:logging-edn`
   from `:draft` to `:doing`. Lean **manual**.

5. **Overwrite warning on existing memo key.** Silent replace vs
   `;; ⚠ overwriting [:session/tasks :X] (prior :status was :doing)`.
   Lean **warn**, never refuse.

6. **Big result handling in trailer entries.** A 47kB file body in
   `:forms[0] :result` is real. Three policies discussed: inline verbatim,
   truncate at render, inline + soft warn. Lean **inline + soft warn**
   (`;; ⚠ trailer entry t3/i2/f1 result is 47kB; consider :note summary`).
   Model can `:trailer-drop` if size matters.

7. **Auto-prune of `:session/trailer`.** Grows unbounded otherwise.
   Options: model-only drop, engine FIFO at cap, engine age-out. Lean
   **model-only** with count surfaced in render comment
   (`;; :session/trailer (12 entries)`).

8. **Promote post-error iters.** Yes — test failures are exactly what
   model wants to remember. Engine fills `:error` map, `:result` is
   dropped (nil + error present → drop result).

9. **Pin from earlier turn.** `(done {:trailer [{:scope "t3/i2" :note "…"}]})`
   — explicit `:t` overrides the default-to-current rule.

10. **`(meta #'sym)` survives `restore-sandbox!`?** Open. To probe live
    before locking the prompt copy that recommends introspection.

---

## Final minimum-viable shape

```clojure
{:session/id   "01HXYZ"
 :session/turn N

 :session/workspace  {:branch :trunk :head :dirty? :stats}
 :session/symbols    {sym {:arglists :doc :defined}}

 :session/rules      {keyword {:body :scope :added-turn}}
 :session/decisions  {keyword {:body :tags :added-turn}}
 :session/specs      {keyword {:title :acceptance :status :added-turn}}
 :session/tasks      {keyword {:title :spec :status :deps :added-turn :done-turn}}

 :session/trailer    [{:scope :note :forms [{:scope :tag :src :result :error}]}]}
```

Seven substantive subtrees. Two engine-rendered, four memos (mem-*), one
trailer (done). Symbols managed natively via `defn` / `(def x nil)` against
existing Vis persistence.

Cross-turn raw history stays in SQLite (`session_turn`,
`session_turn_iteration`); reach via `(v/session-state)` /
`(v/session-report)` (the only registered introspection ops). Never
preloaded into the prompt.
