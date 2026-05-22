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
  memo mutators (`spec-set!` / `task-set!` / `fact-set!`; upsert-only, no remove),
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
 :session/scope      {:turn N :iter M :next-form K}    ; current cursor; engine-stamped each iter
 :session/workspace  {:git/branch :git/trunk :git/head :git/dirty? :git/stats}
 :session/symbols    {sym {:arglists? :doc? :born}}   ; :doc omitted when no docstring; never :doc nil
 :session/hints      {hint-id {:body :importance :satisfy-with}}

 ;; ── MODEL-MANAGED MEMOS ──
 :session/specs
   {<kw> {:title        string                       ; required
          :requirements [{:id keyword
                          :title string
                          :facts [<kw-ref>]           ; optional refs to :session/facts
                          :validator-fn string}]      ; optional SCI fn source (bounded sandbox)
          :status       :draft | :doing | :done | :cancelled
          :born         "tN/iN/fK"                  ; required, engine-stamped
          :done-born    "tN/iN/fK"}}                ; engine-stamps on terminal status

 :session/tasks
   {<kw> {:title       string                        ; required
          :specs       {<spec-kw> [{:requirement keyword
                                    :proof "tN/iN/fK"}]} ; proofs grouped by spec
          :depends-on  [<kw-ref>]                    ; optional, prerequisite tasks
          :status      :todo | :doing | :done | :cancelled
          :born        "tN/iN/fK"}}                ; required, engine-stamped; status history lives in trailer

 :session/facts
   {<kw> {:content     string                        ; required
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
;; ─── MEMO MUTATION (upsert-only — no remove) ───

;; Top-level entity (partial merge; :requirements NOT here)
(spec-set! :K {:title :status})
(task-set! :K {:title :depends-on :status})
(fact-set! :K {:content :status?})           ; :status ∈ #{:active :superseded}; default :active

;; Per-requirement (on :session/specs/:K/:requirements)
(req-add!    :spec-K {:id :title :facts? :validator-fn?})    ; collision warn on existing :id
(req-update! :spec-K :req-id {:title? :facts? :validator-fn?})   ; :id immutable
(req-remove! :spec-K :req-id)                ; cascade-warns orphaned task proofs

;; Per-proof (on :session/tasks/:K/:specs/:spec-K)
(proof-add!    :task-K :spec-K {:requirement :proof})
(proof-remove! :task-K :spec-K :req-id)

;; Abandon = flip :status to :cancelled (task / spec) or :superseded (fact).
;; Engine stamps :done-born on terminal flip and archives from live CTX after TTL.

;; Sugar: spec-set! :K {:requirements [...]} is legal — engine diffs old vs new
;; and emits cascade warnings for any removed req that had proofs, same as
;; an explicit (req-remove! …) sequence. Granular ops just spare the model
;; from re-emitting the full vec on minor edits.

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
4. Nippy-encode CTX, write to session_turn_state.ctx in the same
     transaction that flips the turn-state status to 'done'.
```

Conflict (same scope in both `:trailer-drop` and `:trailer-summarize`) → engine errors.

### Invariants (engine derives every render + every mutation)

Pure-fn invariants computed from `(build-indexes ctx)`. Every violation
emits one `;; ⚠ …` warning anchored at the offending entry on the next
render. Only two are hard (write refused): malformed scope, depends-on cycle.

| # | invariant | trigger | severity |
|---|---|---|---|
| 1 | requirement `:id` unique inside spec | `req-add!`, `spec-set!` :requirements | soft (collision warn, no write) |
| 2 | requirement `:facts` refs exist in `:session/facts` | `req-add!`, `req-update!` | soft |
| 3 | requirement `:validator-fn` parses in SCI | `req-add!`, `req-update!` | soft |
| 4 | task `:specs` keys exist in `:session/specs` | `task-set!`, `proof-add!` | soft |
| 5 | task `:depends-on` keys exist in `:session/tasks` | `task-set!` | soft |
| 6 | task `:depends-on` graph is acyclic | `task-set!` `:depends-on` | **HARD reject** |
| 7 | task proof `:requirement` exists on referenced spec | `proof-add!`, `task-set!` :specs, `req-remove!` cascade | soft |
| 8 | task proof `:proof` scope-form classifies `:ok` | every render | soft (per `:future-*`/`:unknown`/`:errored` class) |
| 9 | proof scope `:proof` validator-fn returns truthy | every render | soft |
| 10 | spec `:status :done` ⇒ every requirement has ≥1 valid proof | `spec-set!` :status :done, every render | soft |
| 11 | task `:status :done` ⇒ every `:depends-on` target is `:done`/`:cancelled` | `task-set!` :status :done | soft |
| 12 | terminal-status entry has `:done-born` | engine-stamped | n/a (engine never fails to stamp) |
| 13 | trailer summary ranges do not partially overlap | `(done …)` `:trailer-summarize` | **HARD reject** |
| 14 | every `::scope-form` string matches regex | every write | **HARD reject** (malformed) |
| 15 | fact `:active` unreferenced by any active requirement for > 12 turns | every render | soft (suggest supersede) |

Engine NEVER refuses writes outside the three hard rules. Soft warnings are
the entire enforcement channel: model reads `;; ⚠` annotations and decides.

### Versioning + GC

CTX snapshots live INLINE on the existing soul/state chain. **NO new
history table.** Two columns added to existing tables (V1 inline edits;
Nippy BLOB, same convention as `session_turn_iteration.result`):

```
session_turn_state         + ctx    BLOB    -- Nippy CTX as of end of this turn version
session_turn_iteration     + forms  BLOB    -- Nippy vec of per-form envelopes
                                            --   {:scope :tag :src :result :error}
                                            -- Replaces the legacy single-form result/error
                                            -- columns which have been dropped.
```

Live CTX = `ctx` on the latest `session_turn_state` for the latest
`session_turn_soul` (join by max(position) for the soul, max(version) for
the state).

History = `SELECT ctx FROM session_turn_state JOIN session_turn_soul
WHERE session_state_id = ? ORDER BY position`. Per-turn snapshots come for
free from the soul/state chain.

Forks copy the parent's latest ctx into the new state's turn-1 row,
so branched timelines keep their pre-fork history intact via
`parent_state_id`.

Memo deletion is forbidden. `*-remove!` does not exist. To drop a thing the
model flips its `:status`:

- task / spec → `:cancelled`
- fact         → `:superseded`

Engine stamps `:done-born` on the terminal-status flip (mirrors the spec
behaviour for `:done`). At turn boundary, engine GCs from live CTX:

| entity | terminal status | TTL (turns past `:done-born.turn`) |
|---|---|---|
| task | `:done`        | 6 |
| task | `:cancelled`   | 10 |
| spec | `:done`        | 6 |
| spec | `:cancelled`   | 10 |
| fact | `:superseded`  | 6 |
| fact | `:active`      | never (model owns lifecycle) |

Archived entries leave `:session/specs` / `:session/tasks` / `:session/facts`
in live CTX but persist in every snapshot they were present in. Model reaches
them through:

```clojure
(introspect-spec     :K)             ; latest turn the entry existed
(introspect-task     :K)
(introspect-fact     :K)
(introspect-archived :tasks)         ; vec of archived task summaries
(introspect-archived :specs)
(introspect-archived :facts)
(introspect-ctx-at   "tN")           ; full CTX snapshot at end of turn N
```

Timeline replay is therefore deterministic: pick a turn, fetch its snapshot,
you have the exact CTX the model saw closing that turn.

### Scope cursor + scope-form classification

`:session/scope` is engine-rendered every iter, before the user message goes
out. Shape: `{:turn N :iter M :next-form K}`. `:turn` matches `:session/turn`;
`:iter` is the current iter inside the turn; `:next-form` is the 1-based index
the model's first form in this iter's fence will receive. Subsequent forms in
the same fence increment from there.

Every `::scope-form` value the model passes (most importantly `:proof` on a
task, but also any manually authored `:born` or trailer scope) is classified
relative to the cursor by a pure fn:

| class | condition | reaction |
|---|---|---|
| `:malformed` | fails `^t[1-9]\d*/i[1-9]\d*/f[1-9]\d*$` | HARD reject — value not written; warn anchored to the call |
| `:future-turn` | `scope.turn > cursor.turn` | soft warn `;; ⚠ proof tX/iY/fZ is from a future turn` |
| `:future-iter` | `scope.turn = cursor.turn` and `scope.iter > cursor.iter` | soft warn `;; ⚠ proof tX/iY/fZ is from a future iter` |
| `:future-form` | same iter as cursor and `scope.form ≥ cursor.next-form` | soft warn — model referenced a form not yet executed in this fence |
| `:unknown` | format ok and not future, but engine never executed this scope | soft warn `;; ⚠ proof tX/iY/fZ refers to no executed form` |
| `:errored` | scope refers to a form whose eval threw (no `:result`) | soft warn `;; ⚠ proof tX/iY/fZ references an errored form` |
| `:ok` | scope is past-or-current and points at a form with `:result` | silent |

The only hard reject is `:malformed`. Everything else writes through to CTX
and surfaces as a `;; ⚠` next to the offending entry on the next render. The
engine never refuses content based on cross-tree consistency — that is what
render warnings are for.

### Memo mutation

- `*-set!` new key → stamp `:born <current-form-scope>`; existing → merge partials.
- `spec-set!` with `:status :done | :cancelled` → stamp `:done-born`.

### Soft validations (engine warns; never refuses)

| call | required field | hint |
|---|---|---|
| `spec-set!` | `:requirements` | non-empty vec of requirements |
| `task-set!` | `:specs` | map from spec key to proof entries |
| `task-set!` with `:status :done` | `:specs` proofs | requirement proofs grouped by spec |
| `fact-set!` | `:content` | non-empty string |
| `satisfy-hint!` | proof scopes vec | non-empty vec of scopes |
| any ref (`:requirements[].facts`, `:specs` keys, `:depends-on`, proof `:requirement`) | target key/id must exist | soft warn on dangling |
| any `::scope-form` arg (`:proof`, manual `:born`, trailer scopes) | classify vs `:session/scope`: `:ok` `:unknown` `:errored` `:future-form` `:future-iter` `:future-turn`; only `:ok` is silent |

Warnings appear as `;; ⚠ …` annotations in the next CTX render.

Spec schemas are open: they define what must/should be present. They do not reject extra keys. Retirement of old keys is prompt/design guidance plus soft warnings, not `s/and #(not …)` shape logic.

### Auto-stamped fields

| trigger | field |
|---|---|
| any `*-set!` on a new key | `:born` |
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
| `(spec-set! …)` `(task-set! …)` `(fact-set! …)` | `:mutation` |
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
 :session/scope {:turn 7 :iter 3 :next-form 1}

 :session/workspace
   {:git/branch "feat/ctx-redesign" :git/trunk "main" :git/head "abc1234" :git/dirty? true
    :git/stats  {"src/auth.clj"    {:added 5 :removed 2}
                 "src/logging.clj" {:added 47 :removed 0}}}

 :session/symbols
   {auth-check {:arglists ([tok]) :doc "literal-compare check" :born "t5/i1/f1"}
    emit-event {:arglists ([{:keys [level msg]}]) :born "t4/i1/f1"}} ; no :doc key — never :doc nil

 :session/hints {}

 :session/specs
   {:auth-bcrypt
     {:title        "switch auth/check to bcrypt"
      :requirements [{:id :bcrypt-check
                      :title "check/1 calls bcrypt/check"
                      :facts [:auth-literal-compare :no-bcrypt-dep]
                      :validator-fn "(fn [{:keys [result]}] (:ok? result))"}
                     {:id :wrong-password
                      :title "wrong password path is covered"}]
      :status       :doing
      :born         "t5/i1/f1"}}

 :session/tasks
   {:add-bcrypt-dep
     {:title       "add bcrypt to deps.edn"
      :specs       {:auth-bcrypt [{:requirement :bcrypt-check
                                   :proof "t5/i2/f1"}]}
      :depends-on  []
      :status      :done
      :born        "t5/i1/f2"}
    :replace-check
     {:title       "replace literal compare with bcrypt/check"
      :specs       {:auth-bcrypt []}
      :depends-on  [:add-bcrypt-dep]
      :status      :doing
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

(fact-set! :auth-literal-compare {:status :superseded})  ; superseded by the patch; engine stamps :done-born + GCs after TTL

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
           {:title        "src/logging.clj emits structured EDN events"
            :requirements [{:id :emit-event-shape
                            :title "emit-event takes a map and prints (pr-str …)"
                            :facts [:logging-current :no-event-fn]
                            :validator-fn "(fn [{:keys [result]}] (map? result))"}
                           {:id :log-delegates
                            :title "log/2 delegates to emit-event"}
                           {:id :no-format
                            :title "no str/format remains in src/logging.clj"}]
            :status       :draft})

(task-set! :introduce-emit-event
           {:title       "introduce emit-event fn"
            :specs       {:logging-edn []}
            :depends-on  []
            :status      :todo})

(task-set! :rewire-log
           {:title       "rewire log/2 through emit-event"
            :specs       {:logging-edn []}
            :depends-on  [:introduce-emit-event]
            :status      :todo})

;; Proof slots already name task ownership; no duplicate task list needed.

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
;; status transition is captured in :session/trailer mutation pin for this iter

(defn emit-event [{:keys [level msg] :as ev}]
  (println (pr-str ev)))
=> #'vis.user/emit-event

(v/patch [{:path "src/logging.clj"
           :search "(defn log [level msg]\n  (println (str/format \"%s: %s\" level msg)))"
           :replace "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))\n\n(defn log [level msg]\n  (emit-event {:level level :msg msg}))"}])
=> {:applied 1}

(task-set! :introduce-emit-event
           {:status :done
            :specs {:logging-edn [{:requirement :emit-event-shape
                                   :proof "t3/i1/f3"}]}})

(done {:answer       "Task `:introduce-emit-event` done. Requirement proof filled on task."
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
      :specs       {:logging-edn [{:requirement :emit-event-shape
                                   :proof "t3/i1/f3"}]}
      :depends-on  []
      :status      :done
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
| missing required field on memo | `;; ⚠ spec :K missing :requirements` |
| done spec with unsatisfied requirement | `;; ⚠ spec :K done but req :R has no valid task proof` |
| dangling cross-subtree ref | `;; ⚠ task :K :specs refs nonexistent spec :foo` |
| proof scope classified non-`:ok` | `;; ⚠ task :K proof tN/iM/fK is :future-iter relative to :session/scope` |

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

1. **`session_turn_state.ctx BLOB`** — currently MISSING. Per-turn CTX snapshot, Nippy-encoded (same convention as `definition_state.value`). Live CTX = `ctx` on the latest turn-state for the latest turn-soul. History walks the soul chain.
2. **`session_turn_iteration.forms BLOB`** — currently MISSING. Nippy vec of per-form envelopes `[{:scope :tag :src :result :error} …]`. The model emits N forms per fence; each form gets its own scope + result + error. **The legacy `result BLOB` / `error BLOB` columns on this table have been dropped** — `forms` is the only place per-form payload lives. `(introspect-form "tN/iN/fK")` decodes the matching entry.

#### Memo verbs

3. **`spec-set!` / `task-set!` / `fact-set!`** bound in SCI hidden-sym set (upsert-only; no remove verbs). Validate shape via `ctx_spec.clj` specs (`s/explain-data`), stamp `:born`, auto-stamp `:done-born` on terminal `:status` flip, soft-warn on missing required fields, dangling refs, future-scope proofs.

#### Introspection

4. **`introspect-iter` / `introspect-form` / `introspect-turn` / `introspect-iter-heads` / `introspect-turn-list`** SELECT against `session_turn_soul` (position + user_request) + `session_turn_state` (status + answer_markdown) + `session_turn_iteration` (position + code + forms Nippy BLOB). Composite-key lookup is by (turn position, iter position, form position). `forms` decodes to the per-form envelope vec; `introspect-form` indexes into it by form position.
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

### Prompt changes

`CORE_SYSTEM_PROMPT` now uses the requirements/proofs layout: specs have `:requirements [{:id :title :facts? :validator-fn?}]`; tasks have `:specs {spec-id [{:requirement :proof}]}` plus `:depends-on`. Proofs live on tasks. Specs contain requirements only. Facts link through requirement `:facts` only. `:session/scope {:turn :iter :next-form}` is added so the model can pick proof scopes deterministically. Task `:journal` is dropped — status history is reconstructable from `:session/trailer` mutation pins. `:doc` on symbols is omitted (never `:doc nil`).

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

 :session/scope     {:turn :iter :next-form}

 :session/specs
   {keyword {:title :requirements :status :born :done-born?}}

 :session/tasks
   {keyword {:title :specs :depends-on :status :born}}

 :session/facts
   {keyword {:content :born}}

 :session/trailer
   [{:scope :forms [{:scope :tag :src :result :error}]}                  ; verbatim pin
    {:scope-start :scope-end :summary :born}]}                            ; range summary
```

Three model-managed memo subtrees + 3 engine views + 1 trailer = 7 substantive subtrees. Six memo verbs + trailer/done/control + symbols (native SCI) + introspection (9 ops).

Cross-turn raw history reachable via `introspect-*`. Never preloaded.

---

## Lifecycle expectations — what model sees + what engine enforces

This section locks in the BEHAVIOR the new engine must exhibit so we can
test against it deterministically. Anything below contradicts the schema
or invariants tables above → schema/invariants win.

### Trailer between iters (within one turn)

```
T<N>.i1 fence executes
  → eval-loop captures blocks: [{:code :result :error} …]
  → ctx-engine/blocks->forms → envelope vec:
      [{:scope "tN/i1/f1" :tag :mutation     :src "…" :result …}
       {:scope "tN/i1/f2" :tag :observation  :src "…" :result …}
       …]
  → ctx-engine/advance-iter:
      - excludes any form whose :src begins with "(done"
      - if remaining vec non-empty:
          appends {:scope "tN/i1" :forms <remaining>} to :session/trailer
      - bumps :session/scope :iter +1, :next-form := 1

T<N>.i2 starts:
  → user message rendered with the pin from i1 visible inside
    :session/trailer. Model can:
      - point a task :proof at any scope inside (proof-add!).
      - call (introspect-form "tN/i1/fK") to refetch full envelope
        (engine resolves through forms BLOB on session_turn_iteration).
      - decide it's stale and emit (done {:trailer-drop ["tN/i1"]}).
```

**Invariants the engine guarantees**:
- A pin's `:scope` is iter-scope (`tN/iM`). A pin's inner forms have
  form-scope (`tN/iM/fK`). Engine NEVER mixes those.
- Pins arrive in **chronological order** (sorted by iter scope) at
  every render. The done-time sort comparator is stable.
- A pin captures the iter's `:forms` exactly as the eval loop saw
  them. Mutator forms (`(spec-set! …)`, etc.) and observation forms
  (`(v/cat …)`, etc.) coexist in one pin under one iter scope.
- A pin's `:src` is the source string of one top-level form, not the
  whole fence body. Whole fence stays on `session_turn_iteration.code`.

### Trailer between turns

CTX (including `:session/trailer`) is Nippy-snapshotted to
`session_turn_state.ctx` at `(done …)` time. The next turn loads
that snapshot via `db-load-latest-ctx` and resumes with the same
trailer.

So pins from `t5/i1` … `t5/i3` are still visible in `t6.i1`'s render
until the model drops/summarizes them. Engine does NOT auto-prune
pins; only `(done {:trailer-drop […] :trailer-summarize […]})` and
`gc-pass` at turn boundary touch the trailer.

### Summarization — model-owned, engine-validated

The model owns trailer compaction. Engine refuses bad shapes only:
hard rejects on partial-overlap summary ranges, inverted ranges, and
bad scope strings; otherwise everything passes.

`(done {:trailer-summarize [{:scope-start "tA/iX" :scope-end "tB/iY"
                              :summary "free-form string"}]})`

For each summary directive:
1. **Validate** `:scope-start` ≤ `:scope-end` (iter-compare). Inverted
   range → soft warn, directive skipped.
2. **Validate** no existing summary partially overlaps. Partial overlap
   = ranges share points but neither is contained in the other. Partial
   overlap → HARD reject, directive skipped, warning emitted.
3. **Absorb** every trailer entry whose iter-range is fully contained
   in `[scope-start, scope-end]` — both pins AND existing summaries
   that are wholly inside. They are dropped from the trailer.
4. **Insert** a new entry `{:scope-start :scope-end :summary :born}`
   where `:born` is stamped by the engine to the form scope of the
   `(done …)` call.
5. **Sort** trailer by composite key after all directives applied.

Summarization is **lossy on purpose**. Once a range is summarized, the
per-form envelopes inside that range are gone from the live trailer.
The HISTORICAL session_turn_state snapshots still carry them (one
snapshot per turn), so `introspect-iter` / `introspect-form` can still
recover them from the past turn's `session_turn_iteration.forms` BLOB.

Rule of thumb model is expected to follow:
- Drop verbatim pins of observations that have been since mutated
  (e.g. read a file, then patched it → original read is stale).
- Summarize long exploratory ranges when their outcome converged
  ("explored 3 schemes, picked bcrypt; verified t14/i6 green").
- Never summarize across `(done …)` boundaries — summaries are
  per-turn semantics; cross-turn aggregation is the soul/state chain's
  job.

### Validation tiers — engine-enforced

| tier | what | when | severity |
|---|---|---|---|
| T0 | shape (clojure.spec) | every `*-set!`/`req-*`/`proof-*` | soft warn (open schema) |
| T1 | ref existence (facts, specs, tasks, requirements) | every render | soft warn |
| T2 | validator-fn SCI eval against proof scope :result | every render | soft warn (5 reason codes) |
| T3 | lifecycle (spec :done ⇒ all reqs proven; task :done ⇒ deps terminal) | every render | soft warn |
| T4 | scope class (cursor + form-results) | every render | soft warn (6 future-* classes) |
| T5 | id collision (`req-add!` on existing id) | mutation time | soft reject (no write) |
| T6 | depends-on cycle (DFS over dep-graph) | task-set! :depends-on | **HARD reject** |
| T7 | trailer partial-overlap | done summarize | **HARD reject** |

Soft = engine writes but emits `;; ⚠` annotation in next render.
Hard = engine refuses the write; ctx-atom unchanged; warning still
surfaced.

### Tasks — what the model is expected to do

The CTX engine is **descriptive, not directive**. It records the
model's plan + execution. Engine never demands tasks — it only
provides:
- a shape (`:session/tasks`) for the model to drop work items into
- a `:session/next-actions` derived ranking that suggests what's
  unblocked / what needs proof / what's stale

The model uses tasks for:
- planning a multi-iter goal: spec → 3 requirements → 3 tasks
- tracking progress: `(task-set! :K {:status :doing})` per iter
- proving completion: `(proof-add! :K :spec-K {:requirement :R :proof "tN/iM/fK"})`
- abandoning work: `(task-set! :K {:status :cancelled})`

Tasks WITHOUT proofs ARE legal — they exist as planning markers.
Engine only complains when:
- a task `:status :done` has no proofs for the requirements it claims
- a task's `:depends-on` graph is broken (dangling or cyclic)

The model is NEVER forced to use tasks. Single-form Q&A turns work
without any spec/task — the CTX subtrees are simply empty.

### Form-results — propagation to next iter

```
iter K executes:
  blocks (eval result) → form-results envelope
  → session_turn_iteration.forms BLOB (Nippy) — persisted
  → ctx-engine/advance-iter — also pinned into :session/trailer

iter K+1 render:
  ctx-loop/trailer->form-results — flatten all trailer pins'
    :forms into a {scope → envelope} map.
  → pass to derive-warnings (T2 + T4) and derive-progression so:
      - validator-fn eval can read `:result` per proof scope
      - classify-scope can distinguish :ok / :unknown / :errored
      - progression counts proofs whose scope appears in form-results
```

A task proof whose scope is NOT in the current trailer's form-results
classifies `:unknown` and warns. This drives the "proofs must reference
live executed forms" invariant without engine policing.

### What model can NEVER do

These are NOT enforced by spec but the engine MUST reject / warn:
- Mutate via `(swap! some-atom …)` to bypass mutators — engine doesn't
  control this; we trust the sandbox to expose only the engine bindings
  for CTX mutations.
- Reference scopes from the FUTURE (`scope.turn > cursor.turn`, etc.).
  Engine soft-warns; the write proceeds.
- Carry `:done-born` manually on a `(spec-set! …)`. Engine ignores
  caller-supplied `:done-born` and stamps its own when status flips
  terminal.
- Lie about `:born`. Engine stamps `:born` on entity creation; the
  open-schema allows the model to override but T0 soft-warns.

### Persistence guarantees

| thing | persisted where | recovered on restart |
|---|---|---|
| live CTX (specs/tasks/facts/trailer/scope) | `session_turn_state.ctx` BLOB | yes, via `db-load-latest-ctx` |
| per-turn history (every snapshot ever) | same column, every soul/state row | yes, via `db-load-ctx-history` |
| per-iter form envelopes | `session_turn_iteration.forms` BLOB | yes, indexed by scope |
| SCI sandbox vars (`(def …)` / `(defn …)`) | `definition_state.value` BLOB | yes, via `restore-sandbox!` |
| trailer (within latest CTX) | same blob as live CTX | yes |
| validator-fn compiled fn | NOT persisted — recompiled from `:validator-fn` source on demand | recompiled (cached) |
| warnings | NOT persisted — derived fresh every render | derived |
| progression | NOT persisted — derived fresh | derived |
| next-actions | NOT persisted — derived fresh | derived |

The single source of truth is `session_turn_state.ctx`. Everything
else is either historical (other turn-state rows), derived (warnings /
progression / next-actions), or unrelated (sandbox vars in
`definition_state`).

### Out of scope for this engine

These are intentionally NOT part of the CTX engine:
- multi-agent coordination
- cross-session sharing (specs from session A flowing to session B)
- automatic task generation / planning from user request
- automatic summarization (model owns it; engine doesn't decide
  what to summarize)
- proof scope synthesis (model must pick form scopes; engine
  classifies what it gets)

These remain channel / extension responsibilities or future work.

---

## Hints — current shape + proposed validator-fn extension

### Current behaviour (and a known bug)

`:session/hints` is an engine-rendered map of pending one-shot instructions
the model must satisfy. Today each hint carries:

```clojure
:session/hints
{:vis.foundation/session-title
   {:body "session has no title — set one with (set-session-title! \"…\")"
    :importance :high
    :satisfy-with "(satisfy-hint! :vis.foundation/session-title)"}}
```

Model is supposed to:
1. **Perform** the requested action (e.g. `(set-session-title! "Auth bcrypt refactor")`).
2. **Acknowledge** via `(satisfy-hint! :vis.foundation/session-title [<scope> …])`.

The engine then drops the hint id from `:session/hints` on the next render.

**The bug Karol called out**: title hints sometimes fail to satisfy.
Diagnosed cause:

- The `:satisfy-with` field today is a STRING that LOOKS like a canonical
  form but isn't enforced. Some hooks emit a 0-arity satisfy template
  (`"(satisfy-hint! :hint/id)"`) while others expect proof scopes. The
  model sometimes calls `(satisfy-hint! :session-title)` (zero arity)
  when the registered hook requires `[<scope>]`, and the engine
  cheerfully removes the hint id even though no proof was attached.
- There is no validator. The engine never checks that the model actually
  did the requested action. If the model calls `(satisfy-hint! …)` without
  ever calling `(set-session-title! …)`, the hint silently disappears.

Net effect: hints behave like soft polite reminders, not invariants. Title
goes missing for whole sessions and the model never re-prompts itself.

### Proposed extension: hints get the same validator-fn machinery as requirements

Lift the requirement-validator pattern up to hints:

```clojure
:session/hints
{:vis.foundation/session-title
   {:body          "session has no title — set one with (set-session-title! \"…\")"
    :importance    :high
    :satisfy-with  "(satisfy-hint! :vis.foundation/session-title [<scope of (set-session-title! …)>])"
    :validator-fn  "(fn [{:keys [src]}] (clojure.string/includes? src \"set-session-title!\"))"}}
```

Schema addition (in `ctx-spec.clj`):

```clojure
(s/def :session.hint/validator-fn string?)

(s/def ::hint
  (s/keys :req-un [:session.hint/body]
    :opt-un [:session.hint/importance
             :session.hint/satisfy-with
             :session.hint/validator-fn]))
```

### Satisfaction flow with validator

```
Model emits two forms in one iter:
  (set-session-title! "Auth bcrypt refactor")    ;; tN/i1/f3
  (satisfy-hint! :vis.foundation/session-title
                 ["tN/i1/f3"])                   ;; tN/i1/f4

Engine handles (satisfy-hint! id scopes):
  1. Look up hint. If absent → silent no-op (model satisfied a hint
     that was already gone — fine).
  2. If hint has no :validator-fn → drop the hint id (legacy behaviour).
  3. If hint has :validator-fn:
       - For each scope in `scopes`, look up form-results map.
       - Call run-validator-fn against the form envelope.
       - If ALL scopes pass → drop the hint id from :session/hints.
       - If ANY scope fails (falsy / threw / timeout / compile-error)
         OR no scopes provided when validator-fn is present →
           keep the hint id alive, emit:
           `;; ⚠ satisfy-hint! :id failed validator: <reason>`
```

The validator receives the canonical form-result envelope
`{:scope :tag :src :result :error}`. Most hint validators will check
`:src` (was the right call made?) or `:result` (did the call return
the expected sentinel like `:vis/silent`?).

### Hint authoring contract (foundation extensions + future hooks)

Hint hooks defined under foundation core (e.g. `title-hint`,
`context-pressure-hint`) MUST now:

1. Provide a `:satisfy-with` string that names the **exact form shape**
   the model should emit, including required scope args.
2. Provide a `:validator-fn` source string that confirms the satisfying
   form actually fired. Examples:
     ```clojure
     ;; title hint validator: any form whose src contains set-session-title!
     "(fn [{:keys [src]}]
        (and (string? src)
             (clojure.string/includes? src \"set-session-title!\")))"

     ;; context-pressure validator: model called a summarizer
     "(fn [{:keys [src]}]
        (and (string? src)
             (re-find #\":trailer-summarize\" src)))"
     ```
3. Optional: bump `:importance` to `:critical` when the hint MUST be
   satisfied this turn (engine then surfaces `;; ⚠ critical hint not
   satisfied` if it persists past `(done …)`).

Hint authoring becomes a small contract: emit body, satisfy-with,
validator-fn. The engine handles the rest.

### Soft rules around hints

| condition | hint emitted by engine |
|---|---|
| hint with `:validator-fn` but model satisfies it with 0 scopes | `;; ⚠ satisfy-hint! :id needs proof scope(s); none provided` |
| hint with `:importance :critical` still in :session/hints at done | `;; ⚠ critical hint :id unsatisfied at done; turn continues anyway` |
| hint validator-fn errors out (compile / runtime) | `;; ⚠ hint :id validator failed: <reason>` |
| `(satisfy-hint! :id …)` for an id that does not exist in :session/hints | silent no-op (idempotent) |

These align with the soft-warning convention everywhere else: engine
never refuses, but pollutes the next render with `;; ⚠` until the
model fixes it.

### Why this fixes the title bug

After this change:
- Foundation `title-hint` registers `:validator-fn` checking
  `set-session-title!` was actually called.
- Model calling `(satisfy-hint! :session-title)` without first calling
  `(set-session-title! "…")` will FAIL the validator (no scope, no
  source) and the hint persists. Next iter's `;; ⚠` makes it
  impossible to ignore.
- Model calling `(set-session-title! "x")` then
  `(satisfy-hint! :session-title ["tN/iM/fK"])` validates cleanly and
  the hint disappears.

Same machinery, same SCI eval cache, same 50ms timeout. Hints become
first-class engine objects with the same enforcement surface as
requirements.

### Implementation order (when we get to it)

1. Add `:session.hint/validator-fn` to ctx-spec.
2. Extend the `satisfy-hint!` SCI binding in ctx-loop to:
   - Accept the proof scope vec as a second arg.
   - Look up the hint's `:validator-fn` from `(get-in ctx [:session/hints id])`.
   - Run each scope's envelope through `run-validator-fn`.
   - Drop the hint id only when all scopes pass.
3. Migrate the two existing foundation hooks (title, context-pressure)
   to ship `:validator-fn` and the right `:satisfy-with` shape.
4. Add tests under `ctx_engine_done_test.clj`: satisfy with no scopes
   when validator expected → keep, warn; satisfy with passing scope →
   drop; satisfy with failing scope → keep, warn.

Out of scope for this commit. Tracked for D10.
