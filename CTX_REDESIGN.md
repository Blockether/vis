# CTX redesign

Status: design — not yet implemented in Vis engine. The data shape lives
in `src/com/blockether/vis/internal/ctx_spec.clj` as the canonical
`clojure.spec.alpha` definitions. This doc carries the RATIONALE: why
each subtree exists, how the engine behaves, examples, open questions.
Shape disagreements between this doc and `ctx_spec.clj` are bugs;
ctx_spec.clj wins.

---

## Rationale

Three anchors fix this design.

**1. Vis's op-tag taxonomy is canonical.**
Every op registered through `vis/register-op!` carries a `:tag` of either
`:observation` or `:mutation` (bare keywords). The trailer entries
reuse those exact keywords. Forms that are not registered ops —
`defn`, `def`, the memo verbs, engine introspection — are classified by
engine-side pattern at trailer auto-pin time and resolve to the same
two values.

**2. Two surfaces for model interaction.**
- **SCI symbols registered by extensions** (with a `:tag`): `v/cat`, `v/patch`,
  `git/diff`, etc.
- **Engine primitives** (bare symbols, hidden, peer with `done`):
  memo mutators (`spec-set!` / `spec-remove!` / `task-set!` / `task-remove!`
  / `fact-set!` / `fact-remove!`),
  introspection (`introspect-iter` / `introspect-form` / `introspect-turn`
  / `introspect-iter-heads` / `introspect-turn-list` / `introspect-symbol-doc`
  / `introspect-symbol-source` / `introspect-symbol-meta` / `introspect-symbol-apropos`),
  control (`done` / `set-session-title!` / `satisfy-hint!`).

**3. CTX is the working memory; no within-turn ping-pong.**
Each iter the engine sends a single user message containing the current
`;; ctx` block. `:session/trailer` updates per-iter as forms execute,
so iter N+1's user message renders with iter N's pin already visible.
No assistant/tool messages persist between iters.

---

## Vocabulary (locked)

- **turn** — one user message → … → `(done …)` cycle.
- **iter** — one provider round-trip inside a turn. Emits exactly one ` ```clojure ` fence with N forms.
- **form** — one top-level parenthesized expression in that fence. Unit of evaluation.
- **fence** — the markdown ` ```clojure ` delimiter. Exactly one per iter.

**Scope coordinates** (regex-validated; no leading zero):
- form: `t<N>/i<N>/f<N>` e.g. `t3/i2/f1`
- iter: `t<N>/i<N>` e.g. `t3/i2`
- turn: `t<N>` e.g. `t3`

Every model-created entry carries `:born <scope-string>`.

---

## Wire shape

Per iter:

```
[{:role "system" :content STABLE_PROMPT}
 {:role "user"   :content "<USER-MSG>\n\n;; ctx\n<ctx-blob-rendered-here>"}]
```

Model responds with one ` ```clojure ` fence containing N forms. Engine:
1. Parses fence into ordered forms.
2. Evaluates each form sequentially.
3. Captures `{:source :result :error}` per form.
4. Auto-pins this iter into `:session/trailer` if non-empty after excluding `(done …)`.
5. Re-renders user message with updated CTX for next iter.

No assistant/tool messages between iters. Single user message per round.

---

## Storage shape

Three model-managed memo subtrees + engine views + trailer:

```clojure
{:session/id      "01HXYZ"
 :session/turn    N

 ;; ── ENGINE-RENDERED VIEWS ──
 :session/workspace  {:git/branch :git/trunk :git/head :git/dirty? :git/stats}
 :session/symbols    {sym {:arglists :doc :born}}
 :session/hints      {hint-id {:body :importance :satisfy-with}}

 ;; ── MODEL-MANAGED MEMOS ──
 :session/specs
   {<kw> {:title       string                        ; required
          :acceptance  [string]                      ; required, non-empty vec
          :facts       [<kw-ref>]                    ; refs to grounding facts
          :tasks       [<kw-ref>]                    ; refs to tasks serving this spec
          :status      :draft | :doing | :done | :cancelled
          :born        "tN/iN/fK"                    ; required, engine-stamped
          :done-born   "tN/iN/fK"}}                  ; engine-stamps on terminal status

 :session/tasks
   {<kw> {:title       string                        ; required
          :spec        <kw-ref>                      ; required, ONE spec served
          :depends-on  [<kw-ref>]                    ; optional, DAG of other tasks
          :facts       [<kw-ref>]                    ; OWNED facts; cascade-removed on task-remove!
          :status      :todo | :doing | :done | :cancelled
          :evidence    [<scope-string>]              ; required on :done
          :journal     [{:status :scope}]            ; engine-appended on every :status change
          :born        "tN/iN/fK"}}                  ; required, engine-stamped

 :session/facts
   {<kw> {:content     string                        ; required
          :tags        #{keyword}                    ; optional, default empty
          :connections [<kw-ref>]                    ; optional, refs to facts/tasks/specs
          :born        "tN/iN/fK"}}                  ; required, engine-stamped

 ;; ── TRAILER ──
 :session/trailer
   ;; Two shapes. Sorted by scope (pins) / scope-start (summaries).
   [;; Verbatim pin (engine auto-pinned per iter)
    {:scope "tN/iN"
     :forms [{:scope "tN/iN/fK"
              :tag   :observation | :mutation
              :src   string
              :result any                            ; dropped if default
              :error  {:message :data}}]}            ; dropped if nil

    ;; Summary entry (model-replaced via :trailer-summarize)
    {:scope-start "tA/iX"                            ; iter scope, inclusive
     :scope-end   "tB/iY"                            ; iter scope, inclusive
     :summary     string
     :born        "tN/iN/fK"}]}                       ; engine-stamps; scope where summarize was emitted
```

Six substantive subtrees.

---

## Operator API

```clojure
;; ─── MEMO MUTATION ───
(spec-set!     :K {:title :acceptance :facts? :tasks? :status})
(spec-remove!  :K)

(task-set!     :K {:title :spec :depends-on? :facts? :status :evidence?})
(task-remove!  :K)               ; CASCADE: removes facts in :facts

(fact-set!     :K {:content :tags? :connections?})
(fact-remove!  :K)

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
                      :trailer-drop      ["tN/iN" "tA/iX->tB/iY" …]
                      :trailer-summarize [{:scope-start "tA/iX" :scope-end "tB/iY" :summary "…"} …]})
(set-session-title!  "title")
(satisfy-hint!       :hint/id [<scope> …] "optional prose")    ; evidence required
```

**No `ctx` SCI binding.** CTX is text rendered into the user message; reading from a `ctx` symbol errors.

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
1. Apply :trailer-drop:
     For each scope-key:
       - "tN/iN"                → remove the pin with matching :scope
       - "tA/iX->tB/iY"         → remove the summary with matching :scope-start + :scope-end
     Idempotent (silent if absent).
   ↓
2. Apply :trailer-summarize:
     For each summary entry:
       a. Validate :scope-start ≤ :scope-end per comparator.
       b. Find all trailer entries fully contained in [:scope-start, :scope-end]:
          - pin scope in [start, end]                → absorbed
          - existing summary entirely in [start, end] → absorbed
          - existing summary partially overlaps       → ERROR
            (`;; ⚠ partial overlap with existing summary tA/iX→tB/iY; drop or extend`)
       c. Remove absorbed entries; insert new summary;
          engine stamps :born to current form scope.
   ↓
3. Sort trailer by composite key (pins by :scope, summaries by :scope-start).
   ↓
4. Persist whole CTX blob to session_state.ctx.
```

Conflict (same scope in both `:trailer-drop` and `:trailer-summarize`) → engine errors.

### Memo mutation

- `*-set!` new key → stamp `:born <current-form-scope>`; existing → merge partials.
- `task-set!` with `:status` change → append `{:status :scope}` to `:journal`.
- `spec-set!` with `:status :done | :cancelled` → stamp `:done-born`.
- `task-remove!` cascades: for each fact-ref in the task's `:facts`, remove that fact from `:session/facts` (exclusive ownership).
- `*-remove!` non-existent key → silent no-op.

### Soft validations (engine warns; never refuses)

| call | required field | hint |
|---|---|---|
| `spec-set!` | `:acceptance` | non-empty vec of strings |
| `task-set!` | `:spec` | refs an existing key in `:session/specs` |
| `task-set!` with `:status :done` | `:evidence` | non-empty vec of scopes |
| `fact-set!` | `:content` | non-empty string |
| `satisfy-hint!` | evidence vec | non-empty vec of scopes |
| any ref (`:facts`, `:tasks`, `:spec`, `:depends-on`, `:connections`) | target key must exist | soft warn on dangling |
| task's `:facts` | not in another task's `:facts` | soft warn on duplicate ownership |

Warnings appear as `;; ⚠ …` annotations in the next CTX render.

### Auto-stamped fields

| trigger | field |
|---|---|
| any `*-set!` on a new key | `:born` |
| `task-set!` that changes `:status` | append to `:journal` |
| `spec-set!` `:status :done`/`:cancelled` | `:done-born` |
| `done {:trailer-summarize …}` creates summary entry | `:born` on the summary |

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

### Non-registered form classification

| head pattern | tag |
|---|---|
| `(defn …)` `(def …)` | `:mutation` |
| `(spec-set! …)` `(spec-remove! …)` `(task-set! …)` `(task-remove! …)` `(fact-set! …)` `(fact-remove! …)` | `:mutation` |
| `(introspect-* …)` | `:observation` |
| arithmetic, str ops, `get-in`, `filter`, plain expressions | `:observation` |
| `(done …)` `(set-session-title! …)` `(satisfy-hint! …)` | excluded from trailer |

### Compactness rules per form (in trailer)

- Drop `:result` when value is the trivial default (`:ok`, `#'sym`, `:done`).
- Drop `:result` when nil and `:error` is present.
- Drop `:error` when nil.

---

## Rendered CTX block (what model sees in user message)

Bare EDN literal under `;; ctx` marker. **No `(def ctx …)`.** Engine emits sparse `;;` provenance hints next to specific entries.

```clojure
;; ctx
{:session/id   "01HXYZ"
 :session/turn 7

 :session/workspace
   {:git/branch "feat/ctx-redesign" :git/trunk "main" :git/head "abc1234" :git/dirty? true
    :git/stats  {"src/auth.clj"    {:added 5 :removed 2}
                 "src/logging.clj" {:added 47 :removed 0}}}

 :session/symbols
   {auth-check {:arglists ([tok]) :doc "literal-compare check" :born "t5/i1/f1"}
    emit-event {:arglists ([{:keys [level msg]}]) :born "t4/i1/f1"}}

 :session/hints {}

 :session/specs
   {:auth-bcrypt
     {:title       "switch auth/check to bcrypt"
      :acceptance  ["check/1 calls bcrypt/check"
                    "stored-hash plumbed"
                    "tests cover wrong-password path"]
      :facts       [:auth-literal-compare :no-bcrypt-dep]
      :tasks       [:add-bcrypt-dep :replace-check]
      :status      :doing
      :born        "t5/i1/f1"}}

 :session/tasks
   {:add-bcrypt-dep
     {:title       "add bcrypt to deps.edn"
      :spec        :auth-bcrypt
      :depends-on  []
      :facts       []
      :status      :done
      :evidence    ["t5/i2/f1"]
      :journal     [{:status :doing :scope "t5/i1/f2"}
                    {:status :done  :scope "t5/i2/f2"}]
      :born        "t5/i1/f2"}
    :replace-check
     {:title       "replace literal compare with bcrypt/check"
      :spec        :auth-bcrypt
      :depends-on  [:add-bcrypt-dep]
      :facts       []
      :status      :doing
      :journal     [{:status :doing :scope "t6/i1/f1"}]
      :born        "t5/i1/f3"}}

 :session/facts
   {:auth-literal-compare
     {:content "src/auth.clj uses (= tok \"secret\") for check/1"
      :born "t3/i2/f1"}
    :no-bcrypt-dep
     {:content "deps.edn does not include bcrypt yet"
      :born "t3/i3/f1"}}

 :session/trailer
   [{:scope "t3/i2"
     :forms [{:scope "t3/i2/f1" :tag :observation
              :src "(v/cat \"src/auth.clj\")"
              :result "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"}]}
    {:scope "t5/i2"
     :forms [{:scope "t5/i2/f1" :tag :mutation
              :src "(v/patch [{:path \"deps.edn\" :search \"…\" :replace \"…\"}])"
              :result {:applied 1 :path "deps.edn"}}]}
    {:scope-start "t6/i1" :scope-end "t6/i2"
     :summary     "verified bcrypt available; running tests; all green"
     :born        "t6/i3/f1"}]}
```

---

## Example 1 — conversation, recall-after-5-turns

### Turn 1
```
USER > Lubię żółty.
```
```clojure
MODEL >
(fact-set! :likes-yellow {:content "user likes yellow color"})
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
(fact-set! :has-pet-dog {:content "user has a dog"})
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
Iter has only `(done …)`. After exclusion :forms empty → engine SKIPS auto-pin.

### Turn 5
```
USER > Co lubię?
```

Model reads from rendered CTX:
```clojure
:session/facts
  {:likes-yellow {:content "user likes yellow color" :born "t1/i1/f1"}
   :has-pet-dog  {:content "user has a dog"          :born "t3/i1/f1"}}
```
```clojure
MODEL >
(done {:answer "Lubisz żółty kolor i masz psa."
       :trailer-summarize [{:scope-start "t1/i1" :scope-end "t3/i1"
                            :summary "noted user prefs + arithmetic"}]})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/id   "01HXYZ"
 :session/turn 5
 :session/workspace {:git/branch "main" :git/trunk "main" :git/head "abc1234" :git/dirty? false :git/stats {}}
 :session/symbols   {}
 :session/hints     {}
 :session/specs     {}
 :session/tasks     {}
 :session/facts
   {:likes-yellow {:content "user likes yellow color" :born "t1/i1/f1"}
    :has-pet-dog  {:content "user has a dog"          :born "t3/i1/f1"}}
 :session/trailer
   [{:scope-start "t1/i1" :scope-end "t3/i1"
     :summary "noted user prefs + arithmetic"
     :born "t5/i1/f1"}]}
```

Three iters (t1, t2, t3) folded into one summary. t4 wasn't pinned (only `done`). t5 is the current turn.

---

## Example 2 — read-patch-verify

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
  {:git/branch "feat/auth-bcrypt" :git/trunk "main" :git/head "def5678" :git/dirty? true
   :git/stats {"src/auth.clj" {:added 1 :removed 1}}}
:session/facts {}                                     ; stale fact removed
:session/trailer
  [{:scope "t2/i1"
    :forms [{:scope "t2/i1/f1" :tag :mutation
             :src "(v/patch [{…}])"
             :result {:applied 1 :path "src/auth.clj"}}]}]
```

---

## Example 3 — facts → spec → tasks with evidence + journal

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

### Turn 2 — formalize spec + tasks
```
USER > Działaj — spec + plan.
```
```clojure
MODEL >
(spec-set! :logging-edn
           {:title       "src/logging.clj emits structured EDN events"
            :acceptance  ["emit-event takes a map and prints (pr-str …)"
                          "log/2 delegates to emit-event"
                          "no str/format remains in src/logging.clj"]
            :facts       [:logging-current :no-event-fn]
            :tasks       []
            :status      :draft})

(task-set! :introduce-emit-event
           {:title       "introduce emit-event fn"
            :spec        :logging-edn
            :depends-on  []
            :facts       []
            :status      :todo})

(task-set! :rewire-log
           {:title       "rewire log/2 through emit-event"
            :spec        :logging-edn
            :depends-on  [:introduce-emit-event]
            :facts       []
            :status      :todo})

;; Update spec's :tasks ref to include the just-created tasks
(spec-set! :logging-edn {:tasks [:introduce-emit-event :rewire-log]})

(done {:answer "Spec `:logging-edn` + 2 taski."})
=> :done
```

### Turn 3 — work the first task
```
USER > Zaczynaj.
```
```clojure
MODEL >
(task-set! :introduce-emit-event {:status :doing})
;; engine appends {:status :doing :scope "t3/i1/f1"} to :journal

(defn emit-event [{:keys [level msg] :as ev}]
  (println (pr-str ev)))
=> #'vis.user/emit-event

(v/patch [{:path "src/logging.clj"
           :search "(defn log [level msg]\n  (println (str/format \"%s: %s\" level msg)))"
           :replace "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))\n\n(defn log [level msg]\n  (emit-event {:level level :msg msg}))"}])
=> {:applied 1}

(task-set! :introduce-emit-event
           {:status :done :evidence ["t3/i1/f2" "t3/i1/f3"]})

(done {:answer       "Task `:introduce-emit-event` done. Evidence pinned."
       :trailer-drop ["t1/i1"]})            ; the original v/cat now stale
=> :done
```

### CTX AFTER TURN 3 (excerpt)

```clojure
:session/symbols
  {emit-event {:arglists ([{:keys [level msg] :as ev}]) :born "t3/i1/f2"}}

:session/tasks
  {:introduce-emit-event
     {:title       "introduce emit-event fn"
      :spec        :logging-edn
      :depends-on  []
      :facts       []
      :status      :done
      :evidence    ["t3/i1/f2" "t3/i1/f3"]
      :journal     [{:status :doing :scope "t3/i1/f1"}
                    {:status :done  :scope "t3/i1/f4"}]
      :born        "t2/i1/f3"}
   :rewire-log {…}}              ; unchanged; still :todo
```

---

## Example 4 — long session, range summary

After many turns of exploratory work, model folds a chunk into a summary:

```clojure
(done {:answer "Wszystko działa, testy zielone."
       :trailer-drop      []
       :trailer-summarize
         [{:scope-start "t10/i1" :scope-end "t14/i6"
           :summary "explored 3 alternative auth schemes; rejected JWT (overhead) and OAuth (out of scope); committed to bcrypt + session-cookie; tests green at t14/i6"}]})
```

Engine: removes pins/summaries inside [t10/i1, t14/i6]; inserts one summary with `:born "t15/i1/f1"`. Net trailer shrinks from ~30 entries to 1 entry for that range.

---

## System prompt — required additions

Schema docs live ONLY in the system prompt. Replace the current `CORE_SYSTEM_PROMPT` section (post-implementation) with content matching `ctx_spec.clj`. Today's prompt has a vocabulary block; the next replacement adds the full schema section per this design.

### Inline render hints (engine emits)

Deterministic `;;` annotations next to entries:

| condition | hint |
|---|---|
| trailer entry is summary shape | `;; summarized in t<N>` (from `:born`) |
| `:result` value in pinned form exceeds threshold (4kB) | `;; ⚠ result is <size>; consider summarizing` |
| `:session/trailer` count exceeds threshold | `;; :session/trailer (32 entries; 5 summaries)` |
| missing required field on memo | `;; ⚠ task :K missing :evidence (status :done)` |
| dangling cross-subtree ref | `;; ⚠ task :K :spec refs nonexistent spec :foo` |
| duplicate fact ownership | `;; ⚠ fact :K in two tasks' :facts vecs (exclusive ownership)` |

---

## Cross-cutting questions

| # | item | status |
|---|---|---|
| 1 | Tag value format | RESOLVED — bare `:observation` / `:mutation` |
| 2 | `:born` semantics | last-touch coord; engine-stamped; consistent across subtrees |
| 3 | Workspace branch spawn | rely on existing Vis flow |
| 4 | Spec status auto-promotion | manual via `spec-set!`; engine stamps `:done-born` on terminal |
| 5 | Overwrite warning on upsert | warn, never refuse |
| 6 | Big-result handling | inline + soft warn |
| 7 | Trailer auto-prune | model-only via drop / summarize |
| 8 | Stale-read heuristic | model self-applies |
| 9 | `(meta #'sym)` survives restore | RESOLVED — yes for any `def`/`defn` |
| 10 | Engine introspection name collision | RESOLVED — `introspect-` prefix |
| 11 | Wire shape: ping-pong vs CTX-only | RESOLVED — CTX-only |
| 12 | Summary range vs per-iter | RESOLVED — range with `:scope-start :scope-end` |

---

## Implementation surface (high-level)

### Engine changes (probed against live source)

#### Schema migrations (edit V1 inline per AGENTS.md)

1. **`session_state.ctx TEXT NOT NULL DEFAULT '{}'`** — currently MISSING. New column for the CTX blob.
2. **`session_turn_iteration.form_results TEXT`** — currently MISSING. JSON vec of per-form envelopes `[{:source :result :error} …]`. Required because the runtime captures per-form results that today have nowhere to persist. Without this, `(introspect-form "tN/iN/fK")` cannot return `:result` / `:error`.

#### Memo verbs

3. **`spec-set!` / `spec-remove!` / `task-set!` / `task-remove!` / `fact-set!` / `fact-remove!`** bound in SCI hidden-sym set. Validate shape via `ctx_spec.clj` specs (`s/explain-data`), stamp `:born`, auto-journal on task `:status` change, auto-stamp `:done-born` on spec terminal, cascade-remove on `task-remove!`, soft-warn on missing required fields and dangling refs.

#### Introspection

4. **`introspect-iter` / `introspect-form` / `introspect-turn` / `introspect-iter-heads` / `introspect-turn-list`** SELECT against `session_turn_soul` (position + user_request) + `session_turn_state` (status + answer_markdown) + `session_turn_iteration` (position + code + form_results JSON). Composite-key lookup is by (turn position, iter position, form position).
5. **`introspect-symbol-doc` / `introspect-symbol-source` / `introspect-symbol-meta` / `introspect-symbol-apropos`** query SCI introspection via `(meta #'sym)` and `(ns-publics 'sandbox)`. Probed in Q9 — meta survives `restore-sandbox!` for any def/defn.

#### Trailer

6. **`(done …)`** handles `:trailer-drop` (`"tN/iN"` for pin, `"tA/iX->tB/iY"` for summary) and `:trailer-summarize` (vec of `{:scope-start :scope-end :summary}`) with range semantics + partial-overlap validation.
7. **Trailer comparator** parses `t<N>/i<N>` segments for sort. Composite key handles pin (`:scope`) vs summary (`:scope-start`).

#### Wire loop

8. **Per-iter loop** rebuilds user message with fresh CTX render. No assistant/tool messages persist between iters.
9. **CTX render** emits bare EDN under `;; ctx` marker + deterministic `;;` provenance hints.

#### Workspace

10. **`:session/workspace`** rendered from `(workspace/for-session db-info session-id)` + `(workspace/status db-info ws-id)` + `(workspace/trunk-info)`. The Vis-native `:git/*` keys flow through directly to CTX — namespacing is preserved as a VCS discriminator. Future Mercurial/jj/etc support adds `:hg/*` / `:jj/*` alongside without colliding.

    Engine picks the relevant keys:
    - `:git/branch`  from `(ws/status …)`
    - `:git/trunk`   from `(ws/trunk-info) :branch`
    - `:git/head`    from `(ws/status …)`
    - `:git/dirty?`  from `(ws/status …)`
    - `:git/stats`   derived via `git diff --numstat <trunk> HEAD` (per-file added/removed)

#### Symbols

11. **`:session/symbols`** filters `definition_state.expression IS NOT NULL`; merges `(meta #'sym)` with an engine-maintained `{sym → :born-scope}` index per session. `:arglists` and `:doc` from var meta; `:born` from the index.

### Foundation extension changes

1. **Remove from `register-op!`**:
   - `:v/session-state` `:v/session-report` (replaced by engine `introspect-*`).
   - `:v/engine-symbol-documentation` `:v/engine-symbol-source-code` `:v/engine-symbol-metadata` `:v/engine-symbol-apropos` (replaced by engine `introspect-symbol-*`).
2. **No additions** — introspection ops are engine primitives.

### Prompt changes (deferred until engine ships)

Replace the `Subtrees:` block and `Memory:` operators in `CORE_SYSTEM_PROMPT` with the spec-centric three-subtree layout. Update `Trailer` section with range summary semantics.

### Code rename (legacy → form)

Done in commit `776ca1ce`. Scope URL format reshape (`turn/<prefix>/iteration/N/block/N` → `t<N>/i<N>/f<N>`) deferred until engine impl.

---

## Final minimum-viable shape

```clojure
{:session/id     "01HXYZ"
 :session/turn   N

 :session/workspace  {:git/branch :git/trunk :git/head :git/dirty? :git/stats}
 :session/symbols    {sym {:arglists :doc :born}}
 :session/hints      {hint-id {:body :importance :satisfy-with}}

 :session/specs
   {keyword {:title :acceptance :facts :tasks :status :born :done-born?}}

 :session/tasks
   {keyword {:title :spec :depends-on :facts :status :evidence :journal :born}}

 :session/facts
   {keyword {:content :tags? :connections? :born}}

 :session/trailer
   [{:scope :forms [{:scope :tag :src :result :error}]}                  ; verbatim pin
    {:scope-start :scope-end :summary :born}]}                            ; range summary
```

Three model-managed memo subtrees + 3 engine views + 1 trailer = 7 substantive subtrees. Six memo verbs + trailer/done/control + symbols (native SCI) + introspection (9 ops).

Cross-turn raw history reachable via `introspect-*`. Never preloaded.
