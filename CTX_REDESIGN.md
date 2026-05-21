# CTX redesign

Status: design — not yet implemented in Vis engine.
Replaces the original `CTX_SPEC.md` (obsolete). Folds every locked decision
from the design discussion. Examples included for stress-testing.

---

## Rationale

Three anchors fix this design.

**1. Vis's op-tag taxonomy is canonical.**
Every op registered through `vis/register-op!` carries a `:tag` of either
`:observation` or `:mutation` (bare keywords; the `:op.tag/` namespace was
killed in commit `4481acec`). The trailer entries in this design reuse
those exact keywords. Forms that are not registered ops — `defn`, `def`,
the memo verbs, engine introspection — are classified by engine-side
pattern at trailer auto-pin time and resolve to the same two values.

**2. Two surfaces for model interaction.**
- **SCI symbols registered by extensions** (with a `:tag`): `v/cat`, `v/patch`,
  `git/diff`, etc.
- **Engine primitives** (bare symbols, hidden, peer with `done`):
  memo mutators (`fact-set!` / `fact-remove!` / `task-set!` / `task-remove!`),
  introspection (`introspect-iter` / `introspect-form` / `introspect-turn`
  / `introspect-iter-heads` / `introspect-turn-list` / `introspect-symbol-doc`
  / `introspect-symbol-source` / `introspect-symbol-meta` / `introspect-symbol-apropos`),
  control (`done` / `set-session-title!` / `satisfy-hint!`).

Workspace mutations (`spawn-branch!`, `apply-to-trunk!`, `discard!`) are
internal Clojure functions in `src/.../internal/workspace.clj`; not exposed
under `:v/`. Model sees workspace state read-only through `:session/workspace`
in CTX plus `git/diff` / `git/status` / `git/log` from the `git/` extension.

**3. CTX is the working memory; no ping-pong.**
Within a turn there is NO assistant/tool message ping-pong. Each iter the
engine sends a single user message containing the current `;; ctx` block.
`:session/trailer` updates per-iter as forms execute, so iter N+1's user
message renders with iter N's pin already visible. Cross-turn carry is
just the same `:session/trailer` after `(done …)` applied its
`:trailer-drop` / `:trailer-summarize` keys.

---

## Vocabulary (locked)

Canonical units of the eval loop:

- **turn** — one user message → … → `(done …)` cycle.
- **iter** — one provider round-trip inside a turn. Emits exactly one ` ```clojure ` fence with N forms.
- **form** — one top-level parenthesized expression in that fence. Unit of evaluation.
- **fence** — the markdown ` ```clojure ` delimiter. Exactly one per iter. "Block" was legacy name for the fence and only the fence.

**Scope** is the canonical coordinate of a form: `t<turn>/i<iter>/f<form>`, e.g. `t3/i2/f1`. Every model-created entity carries `:born <scope-string>`. Engine ships a comparator that parses scope segments for sort.

---

## Wire shape

Per iter (every iter — system stable; user rebuilt each call):

```
[{:role "system" :content STABLE_PROMPT}
 {:role "user"   :content "<USER-MSG>\n\n;; ctx\n<ctx-blob-rendered-here>"}]
```

Model responds with one ` ```clojure ` fence containing forms. Engine:
1. Parses fence into ordered forms.
2. Evaluates each form sequentially with normal SCI binding semantics
   (later forms can reference earlier bindings within the iter).
3. Captures `{:source :result :error}` per form.
4. Auto-pins this iter into `:session/trailer` if non-empty after excluding `(done …)`.
5. Re-renders user message with updated CTX.
6. Repeats until model emits `(done …)`.

No `:role "assistant"` and no `:role "tool"` messages persist between iters.
Each provider call is a one-shot ask with fresh CTX. System prompt is stable
(cacheable); user message changes per iter.

---

## Operator API

```clojure
;; ─── MEMO MUTATION ───
(fact-set!     :K {:content string :tags? :connections? :scope?})
(fact-remove!  :K)
(task-set!     :K {:title string :connections? :status :evidence? :blocked-on?})
(task-remove!  :K)

;; ─── SYMBOLS (native SCI; engine persists via existing restore-sandbox!) ───
(defn foo [x] …)                 ; create / overwrite; survives turn boundary
(def  foo nil)                   ; drop; engine forgets on next restore

;; ─── ENGINE INTROSPECTION (bare; hidden-sym set) ───
;; Session structure:
(introspect-iter        "tN/iN")           ; → {:scope :forms [{:scope :tag :src :result :error}]}
(introspect-form        "tN/iN/fK")        ; → {:scope :tag :src :result :error}
(introspect-turn        "tN")              ; → {:scope :user-msg :answer :iter-scopes […]}
(introspect-iter-heads  "tN")              ; → [{:scope :head :tag} …]
(introspect-turn-list)                     ; → [{:scope :user-msg-head :status} …]
;; SCI symbols:
(introspect-symbol-doc      'sym)
(introspect-symbol-source   'sym)
(introspect-symbol-meta     'sym)
(introspect-symbol-apropos  "pattern")

;; ─── CONTROL ───
(done                {:answer            "markdown"
                      :trailer-drop      ["tN/iN" …]
                      :trailer-summarize [{:scope "tN/iN" :summary "…"} …]})
(set-session-title!  "title")
(satisfy-hint!       :hint/id [<scope> …] "optional prose")    ; evidence required
```

**No `ctx` SCI binding.** CTX is text rendered into the user message; reading from a `ctx` symbol errors.

---

## Storage shape

```clojure
{:session/id     "01HXYZ"
 :session/turn   N

 ;; ── ENGINE-RENDERED VIEWS (derived per dump; not stored as EDN) ──
 :session/workspace
   {:branch :trunk :head :dirty?
    :stats  {"path" {:added :removed}}}              ; per-file diff vs trunk

 :session/symbols
   {<sym> {:arglists ([params] …)                    ; nil for non-callable
           :doc      string                          ; from var meta
           :born     "tN/iN/fK"}}                    ; engine-side index, not var meta

 :session/hints
   {<hint-id> {:body string :importance :satisfy-with}}   ; transient, engine-rendered

 ;; ── MODEL-MANAGED MEMOS (stored as EDN inside session_state.ctx) ──

 :session/facts
   {<kw> {:content     string                        ; required
          :tags        #{keyword}                    ; optional, default empty
          :connections [<kw-ref>]                    ; optional, refs to facts or tasks
          :born        "tN/iN/fK"                    ; REQUIRED, engine-stamped on creation
          :scope       :session | :project           ; optional, default :session
          :source      :project-mirror}}             ; engine-set when loaded cross-session

 :session/tasks
   {<kw> {:title       string                        ; required
          :connections [<kw-ref>]                    ; optional, refs to facts (incl. spec-tagged) or other tasks
          :status      :todo | :doing | :done | :blocked | :cancelled
          :evidence    [<scope-string>]              ; required on :done
          :journal     [{:status :scope}]            ; engine-appended on every :status change
          :born        "tN/iN/fK"
          :blocked-on  string}}                      ; required on :blocked

 ;; ── TRAILER (mutated by engine each iter; drop/summarize at done) ──
 :session/trailer
   ;; Two shapes coexist, both keyed by :scope. Sorted by scope.
   [{:scope "tN/iN"                                   ; verbatim pin (auto-pinned per iter)
     :forms [{:scope "tN/iN/fK"
              :tag   :observation | :mutation
              :src   string
              :result any                             ; dropped if default
              :error  {:message :data}}]}             ; dropped if nil

    {:scope            "tN/iN"                        ; summary entry (model-replaced)
     :summary          string
     :summarized-born  "tN/iN/fK"}]}                  ; scope where summary was created
```

Five substantive subtrees:
- engine: `:session/workspace`, `:session/symbols`, `:session/hints`
- model memos: `:session/facts`, `:session/tasks`
- engine+model: `:session/trailer` (engine auto-pins per iter; model prunes via `done`)

---

## Engine behaviors

### Per-iter (within turn)

```
1. Build user message: <USER-MSG-VERBATIM> + "\n\n;; ctx\n" + <render(ctx)>.
2. Send [system, user] to provider.
3. Parse the response's ```clojure``` fence into ordered forms.
4. Eval each form sequentially; capture {:source :result :error}.
5. Excluding `(done …)`, if the form set is non-empty:
     Append {:scope "t<cur>/i<N>" :forms [...]} to :session/trailer.
6. Re-render user message with updated trailer for next iter.
```

### At `(done …)`

```
1. Apply :trailer-drop      — remove entries by exact :scope match (idempotent).
2. Apply :trailer-summarize — find scope, replace its :forms with :summary.
3. Sort :session/trailer by scope.
4. Persist whole CTX blob to session_state.ctx.
```

Conflict (`same scope in both drop and summarize`) → engine errors.

### Memo mutation

- `fact-set!` new key → stamp `:born <current-form-scope>`; existing → merge partials.
- `task-set!` new key → stamp `:born`; existing → merge.
- `task-set!` with `:status` change → append `{:status :scope}` to `:journal`.
- `*-remove!` non-existent key → silent no-op.

### Soft validations (engine warns; never refuses)

| call | required field | hint |
|---|---|---|
| `fact-set!` | `:content` | string non-empty |
| `task-set!` with `:status :done` | `:evidence` | non-empty vec of scopes |
| `task-set!` with `:status :blocked` | `:blocked-on` | non-empty string |
| `satisfy-hint!` | evidence vec | non-empty vec of scopes |

Warnings appear as `;; ⚠ …` annotations in the next CTX render.

### Auto-stamped fields

| trigger | field |
|---|---|
| any `*-set!` on a new key | `:born` |
| `task-set!` that changes `:status` | append to `:journal` |
| `fact-set!` with `:tags #{:spec}` and `:status :done`/`:cancelled` | `:done-born` (if model includes `:status`) |

---

## Tag classification (cross-validated against source)

### Registered ops (verified)

From `extensions/common/vis-foundation-core/src/.../core.clj` and `.../editing/core.clj`:

| op | tag |
|---|---|
| `:v/cat` `:v/ls` `:v/rg` `:v/exists?` `:v/snapshot` | `:observation` |
| `:v/repositories` `:v/git` `:v/languages` `:v/monorepo` `:v/main-agent-instructions` | `:observation` |
| `:v/patch` `:v/create-dirs` `:v/copy` `:v/move` `:v/delete` `:v/delete-if-exists` | `:mutation` |
| `:v/refresh!` `:v/reload-extensions!` | `:mutation` |

From `vis-foundation-git`: `:git/diff` `:git/status` `:git/log` → `:observation`.
From `vis-foundation-exa`: all `:observation`.

### Deprecated registered ops (move to engine primitives)

- `:v/session-state` `:v/session-report` → replaced by `introspect-*` ops.
- `:v/engine-symbol-documentation` `:v/engine-symbol-source-code` `:v/engine-symbol-metadata` `:v/engine-symbol-apropos` → replaced by `introspect-symbol-*` ops.

Engine primitives don't go through `register-op!`. Bound in the SCI hidden-sym set; no `:tag` registration; engine derives `:observation` at trailer auto-pin time.

### Non-registered form classification

| head pattern | tag |
|---|---|
| `(defn …)` `(def …)` | `:mutation` |
| `(fact-set! …)` `(fact-remove! …)` `(task-set! …)` `(task-remove! …)` | `:mutation` |
| `(introspect-* …)` | `:observation` |
| arithmetic, str ops, `get-in`, `filter`, plain expressions | `:observation` |
| `(done …)` `(set-session-title! …)` `(satisfy-hint! …)` | excluded from trailer |

### Compactness rules per form (in trailer)

- Drop `:result` when value is the trivial default (`:ok` for mem ops, `#'sym` for `def`/`defn`, `:done`).
- Drop `:result` when nil and `:error` is present.
- Drop `:error` when nil.

---

## Rendered CTX block (what model sees in user message)

Bare EDN literal under `;; ctx` marker. **No `(def ctx …)`.** Plain text, not code. Engine emits sparse `;;` provenance hints next to specific entries (project-mirror source, summarized-in marker, big result warning). No schema docs in render — those live in system prompt.

```clojure
;; ctx
{:session/id   "01HXYZ"
 :session/turn 7

 :session/workspace
   {:branch "feat/ctx-redesign" :trunk "main" :head "abc1234" :dirty? true
    :stats  {"src/auth.clj"    {:added 5  :removed 2}
             "src/logging.clj" {:added 47 :removed 0}}}

 :session/symbols
   {auth-check {:arglists ([tok]) :doc "literal-compare check" :born "t5/i1/f1"}
    emit-event {:arglists ([{:keys [level msg]}]) :born "t4/i1/f1"}}

 :session/hints {}

 :session/facts
   {:auth-literal-compare
      {:content "src/auth.clj uses (= tok \"secret\") for check/1"
       :born "t3/i2/f1"}
    :no-bcrypt-dep
      {:content "deps.edn does not include bcrypt yet"
       :born "t3/i3/f1"}
    :caveman-pl
      {:content "respond in PL caveman style"
       :tags #{:behavior}
       :scope :session :born "t1/i1/f1"}
    :real-db-tests
      {:content "tests must hit real SQLite — no mocks"
       :tags #{:rule} :scope :project :source :project-mirror :born "tA/iX/fY"}    ;; loaded from project_fact
    :no-llm-compaction
      {:content "Never LLM-compact ctx; deterministic rules only"
       :tags #{:decision} :born "t3/i2/f1"}
    :auth-bcrypt
      {:content "switch auth/check to bcrypt"
       :tags #{:spec}
       :connections [:auth-literal-compare :no-bcrypt-dep
                     :ac-bcrypt-check :ac-stored-hash :ac-wrong-pw-test]
       :born "t5/i1/f1"}
    :ac-bcrypt-check
      {:content "check/1 calls bcrypt/check"
       :tags #{:acceptance} :born "t5/i1/f2"}
    :ac-stored-hash
      {:content "stored-hash plumbed through" :tags #{:acceptance} :born "t5/i1/f3"}
    :ac-wrong-pw-test
      {:content "tests cover wrong-password path" :tags #{:acceptance} :born "t5/i1/f4"}}

 :session/tasks
   {:add-bcrypt-dep
      {:title "add bcrypt to deps.edn"
       :connections [:auth-bcrypt]                                       ; ref to spec fact
       :status :done
       :evidence ["t5/i2/f1"]
       :journal [{:status :doing :scope "t5/i1/f5"}
                 {:status :done  :scope "t5/i2/f2"}]
       :born "t5/i1/f5"}
    :replace-check
      {:title "replace literal compare with bcrypt/check"
       :connections [:auth-bcrypt :add-bcrypt-dep]                       ; spec + prereq task
       :status :doing
       :journal [{:status :doing :scope "t6/i1/f1"}]
       :born "t5/i1/f6"}}

 :session/trailer
   [{:scope "t3/i2"
     :forms [{:scope "t3/i2/f1" :tag :observation
              :src "(v/cat \"src/auth.clj\")"
              :result "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"}]}
    {:scope "t5/i2"
     :forms [{:scope "t5/i2/f1" :tag :mutation
              :src "(v/patch [{:path \"deps.edn\" :search \"…\" :replace \"…\"}])"
              :result {:applied 1 :path "deps.edn"}}]}
    {:scope "t6/i1" :summary "verified bcrypt available; running tests"
     :summarized-born "t6/i2/f1"}]}                                       ;; summarized in t6
```

---

## Example 1 — conversation, recall-after-5-turns

User chatting, no code. Tests `:session/facts` as the single bucket for everything durable.

### Turn 1
```
USER > Lubię żółty.
```
```clojure
MODEL >
(fact-set! :likes-yellow {:content "user likes yellow color" :scope :session})
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
(fact-set! :has-pet-dog {:content "user has a dog" :scope :session})
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
This iter has only `(done …)`. After exclusion the iter's :forms vec is empty; engine SKIPS auto-pinning.

### Turn 5
```
USER > Co lubię?
```

Model reads from rendered CTX:
```clojure
:session/facts
  {:likes-yellow {:content "user likes yellow color" :scope :session :born "t1/i1/f1"}
   :has-pet-dog  {:content "user has a dog"          :scope :session :born "t3/i1/f1"}}
```
```clojure
MODEL >
(done {:answer "Lubisz żółty kolor i masz psa."
       :trailer-summarize [{:scope "t2/i1" :summary "computed 17*23 = 391"}
                           {:scope "t1/i1" :summary "noted :likes-yellow"}
                           {:scope "t3/i1" :summary "noted :has-pet-dog"}]})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/id   "01HXYZ"
 :session/turn 5
 :session/workspace {:branch "main" :dirty? false :stats {}}
 :session/symbols   {}
 :session/hints     {}
 :session/facts
   {:likes-yellow {:content "user likes yellow color" :scope :session :born "t1/i1/f1"}
    :has-pet-dog  {:content "user has a dog"          :scope :session :born "t3/i1/f1"}}
 :session/tasks {}
 :session/trailer
   [{:scope "t1/i1" :summary "noted :likes-yellow"      :summarized-born "t5/i1/f1"}
    {:scope "t2/i1" :summary "computed 17*23 = 391"     :summarized-born "t5/i1/f1"}
    {:scope "t3/i1" :summary "noted :has-pet-dog"       :summarized-born "t5/i1/f1"}]}
```

---

## Example 2 — read-patch-verify (workspace + stale-read drop)

### Turn 1
```
USER > Pokaż mi src/auth.clj i powiedz co tam jest.
```
```clojure
MODEL >
(v/cat "src/auth.clj")          ;; t1/i1/f1
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(fact-set! :auth-literal-compare
           {:content "src/auth.clj uses (= tok \"secret\") for check/1"})

(done {:answer "Plik definiuje `check/1`: literal compare na `\"secret\"`."})
=> :done
```

### Turn 2
```
USER > Zamień to literal compare na bcrypt verify.
```

Model has the fact `:auth-literal-compare` in CTX — knows current shape without re-reading. Patches directly. Drops the now-stale `v/cat` pin from prior turn.

```clojure
MODEL >
(v/patch [{:path "src/auth.clj"
           :search "(defn check [tok] (= tok \"secret\"))"
           :replace "(defn check [tok] (bcrypt/check tok stored-hash))"}])
=> {:applied 1 :path "src/auth.clj"}

(fact-remove! :auth-literal-compare)   ; superseded by the patch

(done {:answer       "Zamieniono `check` na bcrypt verify."
       :trailer-drop ["t1/i1"]})        ; the v/cat result is now stale
=> :done
```

### CTX AFTER TURN 2 (excerpt)

```clojure
:session/workspace
  {:branch "feat/auth-bcrypt" :dirty? true
   :stats {"src/auth.clj" {:added 1 :removed 1}}}
:session/facts {}                                     ; stale fact removed
:session/trailer
  [{:scope "t2/i1"
    :forms [{:scope "t2/i1/f1" :tag :mutation
             :src "(v/patch [{:path \"src/auth.clj\" :search \"…\" :replace \"…\"}])"
             :result {:applied 1 :path "src/auth.clj"}}]}]
```

---

## Example 3 — facts graph + tasks with evidence + journal

Tests fact-as-spec, `:connections` graph, task lifecycle.

### Turn 1 — observe + crystallize facts
```
USER > Refactor src/logging.clj to emit structured EDN events instead of formatted text.
```
```clojure
MODEL >
(v/cat "src/logging.clj")
=> "(ns logging)\n(defn log [level msg] (println (str/format \"%s: %s\" level msg)))\n"

(fact-set! :logging-current
           {:content "src/logging.clj: log/2 uses str/format to write text to stdout"})
(fact-set! :no-event-fn
           {:content "No emit-event or structured-logging fn exists yet"})

(done {:answer "Przeczytałem `src/logging.clj`. Dwa fakty zapisane."})
=> :done
```

### Turn 2 — formalize spec + acceptance facts, link via :connections, create tasks

```
USER > Działaj — spec + plan.
```
```clojure
MODEL >
;; spec is just a fact tagged :spec with :connections to its supporting facts + acceptance criteria
(fact-set! :ac-emit-event-shape
           {:content "emit-event takes a map and prints (pr-str …)"
            :tags #{:acceptance}})
(fact-set! :ac-log-delegates
           {:content "log/2 delegates to emit-event"
            :tags #{:acceptance}})
(fact-set! :ac-no-strformat
           {:content "no str/format remains in src/logging.clj"
            :tags #{:acceptance}})

(fact-set! :logging-edn
           {:content     "src/logging.clj emits structured EDN events"
            :tags        #{:spec}
            :connections [:logging-current :no-event-fn
                          :ac-emit-event-shape :ac-log-delegates :ac-no-strformat]})

(task-set! :introduce-emit-event
           {:title       "introduce emit-event fn"
            :connections [:logging-edn]
            :status      :todo})

(task-set! :rewire-log
           {:title       "rewire log/2 through emit-event"
            :connections [:logging-edn :introduce-emit-event]
            :status      :todo})

(done {:answer "Spec `:logging-edn` zapisany (5 :connections). 2 taski."})
=> :done
```

### Turn 3 — work the first task

```
USER > Zaczynaj.
```
```clojure
MODEL >
(task-set! :introduce-emit-event {:status :doing})
;; engine auto-appends {:status :doing :scope "t3/i1/f1"} to :journal

(defn emit-event [{:keys [level msg] :as ev}]
  (println (pr-str ev)))
=> #'vis.user/emit-event

(v/patch [{:path "src/logging.clj"
           :search "(defn log [level msg]\n  (println (str/format \"%s: %s\" level msg)))"
           :replace "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))\n\n(defn log [level msg]\n  (emit-event {:level level :msg msg}))"}])
=> {:applied 1}

(task-set! :introduce-emit-event
           {:status :done :evidence ["t3/i1/f2" "t3/i1/f3"]})    ; defn + v/patch

(done {:answer "Task `:introduce-emit-event` done. Evidence pinned."
       :trailer-drop ["t1/i1"]})         ; the original v/cat now stale
=> :done
```

### CTX AFTER TURN 3 (excerpt)

```clojure
:session/symbols
  {emit-event {:arglists ([{:keys [level msg] :as ev}]) :born "t3/i1/f2"}}

:session/tasks
  {:introduce-emit-event
     {:title       "introduce emit-event fn"
      :connections [:logging-edn]
      :status      :done
      :evidence    ["t3/i1/f2" "t3/i1/f3"]
      :journal     [{:status :doing :scope "t3/i1/f1"}
                    {:status :done  :scope "t3/i1/f4"}]
      :born        "t2/i1/f5"}
   :rewire-log {…}}              ; unchanged; still :todo
```

---

## Example 4 — cross-session project fact

### Session A, Turn 7
```
USER > Pamiętaj, w tym projekcie testy muszą uderzać w prawdziwy SQLite, nigdy mockować.
```
```clojure
MODEL >
(fact-set! :real-db-tests
           {:content "tests must hit real SQLite — no mocks"
            :tags    #{:rule}
            :scope   :project})            ;; mirrors to project_fact

(done {:answer "Reguła `:real-db-tests` zapisana jako project-scoped."})
=> :done
```

### Engine side-effect
```sql
INSERT INTO project_fact (repo_id, fact_key, content, tags, scope, added_at)
VALUES ('repo:vis', 'real-db-tests',
        'tests must hit real SQLite — no mocks',
        '#{:rule}', 'project', …)
```

### Session B start (next day, same repo)
```clojure
:session/facts
  {:real-db-tests {:content "tests must hit real SQLite — no mocks"
                   :tags #{:rule}
                   :scope :project
                   :source :project-mirror      ;; engine-set
                   :born "tA/iX/fY"}}            ;; original session scope
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

(done {:answer "Test dodany. Używa tmp SQLite jak wymaga fact `:real-db-tests`."})
=> :done
```

---

## System prompt — required additions

CTX schema documentation lives **only in the system prompt**, never in the rendered ctx block. Replace the existing CORE_SYSTEM_PROMPT section to reflect this design — the prompt currently describes an older shape.

### Replacement block (post-implementation)

```text
CTX is your session state. Engine renders it as a bare EDN literal under
a `;; ctx` marker in every user message. There is no `ctx` SCI binding;
read from the rendered text, write back via the operators below. Each
iter the engine re-renders CTX with this turn's pins already visible.

Subtrees:
  :session/workspace  engine-managed; current branch + per-file diff stats
  :session/symbols    engine-managed; live SCI symbols {:arglists :doc :born}
  :session/hints      engine-managed; pending one-shot instructions
  :session/facts      everything durable. Single bucket. Tags discriminate
                      kinds: :decision, :rule, :behavior, :spec, :acceptance,
                      :observation, anything else.
                      shape: {<kw> {:content :tags? :connections? :born :scope? :source?}}
                      :scope optional (default :session); :project mirrors cross-session
                      :connections is a vec of refs to other facts or tasks
  :session/tasks      work items
                      shape: {<kw> {:title :connections? :status :evidence?
                                    :journal :born :blocked-on?}}
                      :status ∈ #{:todo :doing :done :blocked :cancelled}
                      :evidence REQUIRED on :done; :blocked-on REQUIRED on :blocked
                      :journal engine-appended on every :status change
                      :connections is a vec of refs to facts or other tasks;
                      typically references a spec-tagged fact
  :session/trailer    pinned iter envelopes; engine auto-pins per iter;
                      drop/summarize via done keys

Mutation operators (memos):
  (fact-set!    :K {:content :tags? :connections? :scope?})
  (fact-remove! :K)
  (task-set!    :K {:title :connections? :status :evidence? :blocked-on?})
  (task-remove! :K)
  Engine stamps :born on first set. Auto-appends to :journal on task :status change.

Symbol lifecycle (native SCI; engine persists across turns):
  (defn foo [x] …)        ; create / overwrite
  (def  foo nil)          ; drop

Final form per iter:
  (done {:answer            "markdown string"
         :trailer-drop      ["t<N>/i<N>" …]
         :trailer-summarize [{:scope "t<N>/i<N>" :summary "…"} …]})

Engine introspection (bare primitives; introspect- prefix):
  Session structure:
    (introspect-iter "tN/iN"), (introspect-form "tN/iN/fK"),
    (introspect-turn "tN"), (introspect-iter-heads "tN"),
    (introspect-turn-list)
  SCI symbols:
    (introspect-symbol-doc 'sym), (introspect-symbol-source 'sym),
    (introspect-symbol-meta 'sym), (introspect-symbol-apropos "pattern")

Scope coordinates:
  Format    "t<N>/i<N>/f<N>"     e.g. "t3/i2/f1"
  Iter      "t<N>/i<N>"          e.g. "t3/i2"
  Turn      "t<N>"               e.g. "t3"
  Every model-created entry carries :born <scope>.

Schema is soft. Engine warns on missing required fields, never refuses.
```

### Inline render hints (engine emits)

Deterministic `;;` annotations next to entries:

| condition | hint |
|---|---|
| `:source :project-mirror` on a fact | `;; loaded from project_fact` |
| trailer entry is summary shape | `;; summarized in t<N>` |
| `:result` value in pinned form exceeds threshold (e.g. 4kB) | `;; ⚠ result is <size>; consider summarizing` |
| `:session/trailer` count exceeds threshold | `;; :session/trailer (32 entries; 5 summarized)` |
| missing required field on memo | `;; ⚠ task :K missing :evidence (status :done)` |

---

## Cross-cutting questions

| # | item | status |
|---|---|---|
| 1 | Tag value format | RESOLVED — bare `:observation` / `:mutation` |
| 2 | `:born <scope>` semantics | last-redef coord; engine maintains separate index, merged at render |
| 3 | Workspace branch spawn | rely on existing Vis flow |
| 4 | Spec status auto-promotion | n/a — specs are facts; tasks track status |
| 5 | Overwrite warning on upsert | warn, never refuse |
| 6 | Big-result handling | inline + soft warn |
| 7 | Trailer auto-prune | model-only via drop / summarize |
| 8 | Stale-read heuristic | model self-applies via convention |
| 9 | `(meta #'sym)` survives restore | RESOLVED — yes for any `def`/`defn` |
| 10 | Engine introspection name collision | RESOLVED — `introspect-` prefix |
| 11 | Wire shape: ping-pong vs CTX-only | RESOLVED — CTX-only; no chat history between iters |

---

## Implementation surface (high-level)

### Engine changes

1. **CTX storage** — `session_state.ctx TEXT` blob.
2. **Memo verbs** — `fact-set!`, `fact-remove!`, `task-set!`, `task-remove!` bound in SCI hidden-sym set. Engine validates shape, stamps `:born`, auto-journals on task `:status` change, soft-warns on missing required fields.
3. **Introspection** — `introspect-iter` / `introspect-form` / `introspect-turn` / `introspect-iter-heads` / `introspect-turn-list` SELECT against `session_turn` / `session_turn_iteration`. `introspect-symbol-*` query SCI introspection.
4. **`(done …)`** — handle `:trailer-drop` and `:trailer-summarize` keys.
5. **Trailer comparator** — parse `t<N>/i<N>` segments for sort.
6. **Per-iter loop** — each iter rebuilds user message with fresh CTX render. No assistant/tool messages persist between iters.
7. **CTX render** — bare EDN under `;; ctx` marker + deterministic `;;` provenance hints.
8. **`:session/workspace :stats`** — derive from `git diff --numstat` against trunk.
9. **`:session/symbols`** — filter `definition_state.expression IS NOT NULL`; merge `(meta #'sym)` with engine-side `:born` index.

### Foundation extension changes

1. **Remove from `register-op!`**:
   - `:v/session-state` `:v/session-report` (replaced by engine `introspect-*`).
   - `:v/engine-symbol-documentation` `:v/engine-symbol-source-code` `:v/engine-symbol-metadata` `:v/engine-symbol-apropos` (replaced by engine `introspect-symbol-*`).
2. **No additions** — introspection ops are engine primitives.

### Persistance changes

1. **`project_fact` table** — `(repo_id, fact_key, content, tags, scope, added_at)`. Mirrored on `fact-set!` with `:scope :project`.
2. **Session resume** — merge `project_fact` rows into `:session/facts` on session start.

### Prompt changes (deferred until engine ships)

Replace the current `Read \`ctx\` first. Engine context keys:` section with the schema docs from §"System prompt — required additions".

### Code rename (legacy → form)

Done in commit `776ca1ce`. Scope URL format reshape (`turn/<prefix>/iteration/N/block/N` → `t<N>/i<N>/f<N>`) deferred until engine impl.

---

## Final minimum-viable shape

```clojure
{:session/id     "01HXYZ"
 :session/turn   N

 :session/workspace  {:branch :trunk :head :dirty? :stats}
 :session/symbols    {sym {:arglists :doc :born}}
 :session/hints      {hint-id {:body :importance :satisfy-with}}

 :session/facts
   {keyword {:content     string
             :tags        #{keyword}
             :connections [<kw-ref>]
             :born        scope
             :scope       :session | :project
             :source      :project-mirror}}

 :session/tasks
   {keyword {:title       string
             :connections [<kw-ref>]
             :status      keyword
             :evidence    [scope]
             :journal     [{:status :scope}]
             :born        scope
             :blocked-on  string}}

 :session/trailer
   [{:scope :forms [{:scope :tag :src :result :error}]}    ; verbatim pin
    {:scope :summary :summarized-born}]}                    ; summary entry
```

Five substantive subtrees:
- 3 engine-rendered (workspace, symbols, hints)
- 2 model-managed memos (facts, tasks)
- 1 engine+model-managed trailer

Two memo verb pairs (`fact-*`, `task-*`) + `satisfy-hint!` + `done` + native `defn`/`(def x nil)`.

Cross-turn raw history reachable via engine primitives (`introspect-*`). Never preloaded.
