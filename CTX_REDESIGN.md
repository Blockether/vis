# CTX redesign

Status: design — not yet implemented in Vis engine.
Replaces the original `CTX_SPEC.md` (now obsolete). Folds every locked
decision from the design discussion. Examples included for stress-testing.

---

## Rationale

Two anchors fix this design.

**1. Vis's op-tag taxonomy is the canonical mutation/observation source.**
Every op registered through `vis/register-op!` carries a `:tag` of either
`:observation` or `:mutation`. The taxonomy is consumed by
every channel renderer (`channel_tui/render.clj` colours by tag). The
trailer entries in this design reuse those exact keyword values: zero
translation layer, zero risk of drift. Forms that are not registered ops
— `defn`, `def`, the `mem-*` verbs, engine introspection ops — are
classified by engine-side pattern at trailer auto-pin time and resolve
to the same two values.

**2. Two surfaces for model interaction:**
- **SCI symbols** (extensions register them with a `:tag`): `v/cat`, `v/patch`, `git/diff`, …
- **Engine primitives** (bare symbols, hidden, peer with `done`): per-subtree mutators (`rule-set!`, `rule-remove!`, `decision!`, `fact-set!`, `fact-remove!`, `spec-set!`, `spec-remove!`, `task-set!`, `task-remove!`), and introspection (`iter`, `form`, `turn`, `iter-heads`, `turn-list`, `symbol-doc`, `symbol-source`, `symbol-meta`, `symbol-apropos`).

Workspace mutations (`spawn-branch!`, `apply-to-trunk!`, `discard!`) are
internal Clojure functions in `src/.../internal/workspace.clj`; they are
NOT exposed under `:v/`. Model sees workspace state read-only through
`:session/workspace` in CTX, plus `git/diff` / `git/status` / `git/log`
from the `git/` extension.

The trailer is a structured replay of forms that were already inside
Vis's existing classification machinery, with the same vocabulary the
rest of the system uses. No new tag domain, no shadow op table.

---

## Vocabulary (locked)

Canonical units of the eval loop:

- **turn** — one user message → … → `(done …)` cycle.
- **iter** — one provider round-trip inside a turn. Emits exactly one ` ```clojure ` fence.
- **form** — one top-level parenthesized expression in that fence. Unit of evaluation. An iter has N forms.
- **fence** — the markdown ` ```clojure ` delimiter. Exactly one per iter. "Block" was the legacy name for the fence and only the fence; never call individual forms blocks.

**Scope** is the canonical coordinate of a form: `t<turn>/i<iter>/f<form>`, e.g. `t3/i2/f1`. Every model-created entity that needs provenance carries `:born <scope-string>`. Engine ships a comparator that parses scope segments for sort.

Legacy code still says `block-source`, `block-results`, `:block-count`, `/block/` URL segments — scheduled for rename to `form-*` and `/f/N`. New code uses `form-*`.

---

## Operator API

```clojure
;; ─── MEMOS (mid-turn; model writes; stored in session_state.ctx) ───
;;
;; Per-subtree functions. Engine validates shape, stamps :born,
;; auto-journals on status transitions, soft-warns on missing required
;; fields. No generic path-keyed mutator.

;; Rules — durable behavior + facts about user (upsert; merges partials)
(rule-set!     :K {:body string :scope #{:session :project}})
(rule-remove!  :K)

;; Decisions — APPEND-ONLY (no update, no remove)
(decision!     :K {:body string :tags #{keyword}})

;; Facts — observations
(fact-set!     :K {:body string})
(fact-remove!  :K)

;; Specs — requirements built FROM facts
(spec-set!     :K {:title string :acceptance [string] :facts [<kw-ref>]
                   :status #{:draft :doing :done :cancelled}})
(spec-remove!  :K)

;; Tasks — work items; engine auto-journals on :status change
(task-set!     :K {:title string :spec <kw-ref> :depends-on [<kw-ref>]
                   :status #{:todo :doing :done :blocked :cancelled}
                   :evidence [<scope>] :blocked-on string?})
(task-remove!  :K)

;; Engine behaviors:
;;   *-set!         : new key → stamp :born; existing → merge partials
;;   task-set!      : :status change → auto-append {:status :scope} to :journal
;;                    :status :done without :evidence → soft warn
;;                    :status :blocked without :blocked-on → soft warn
;;   spec-set!      : :status :done | :cancelled → auto-stamp :done-born
;;   decision!      : second call with same key → warn (append-only)
;;   *-remove!      : non-existent key → silent no-op

;; ─── SYMBOLS (native SCI; Vis's existing definition_state machinery persists) ───
(defn foo [x] …)                 ; create / overwrite — auto-persists
(def  foo nil)                   ; drop — engine treats latest expression IS NULL as forgotten

;; ─── INTROSPECTION (engine primitives — bare symbols, hidden-sym set) ───
;; Session structure
(iter       "tN/iN")             ; → {:scope :forms [{:scope :tag :src :result :error}]}
(form       "tN/iN/fK")          ; → {:scope :tag :src :result :error}
(turn       "tN")                ; → {:scope :user-msg :answer :iter-scopes [...]}
(iter-heads "tN")                ; → [{:scope :head :tag} …]
(turn-list)                      ; → [{:scope :user-msg-head :status} …]
;; SCI symbols (moved from foundation; no longer registered as v/ ops)
(symbol-doc     'sym)            ; → string or nil
(symbol-source  'sym)            ; → string (full def source)
(symbol-meta    'sym)            ; → map (full (meta #'sym))
(symbol-apropos "pattern")       ; → vec of matching syms

;; ─── FINAL ───
(done {:answer            "markdown string"
       :trailer-drop      ["t<N>/i<N>" …]                     ; iter scopes only
       :trailer-summarize [{:scope "t<N>/i<N>" :summary "…"} …]})
```

**No `ctx` symbol. No `(ctx)` fn. No reads in eval.**

CTX is always rendered into the user message of the next turn as a bare
EDN literal under a `;; ctx` marker, **NEVER** as `(def ctx {…})`. Two reasons:

1. **No SCI binding** — `ctx` is not a var; `(get-in ctx …)` errors with `Unable to resolve symbol: ctx`. Footgun structurally impossible.
2. **Storage / render separation** — rendered text is a read-only snapshot; persistent state lives in `session_state.ctx` (SQLite TEXT blob). Mutations go through `mem-*` and `done` keys.

---

## System prompt — required additions

CTX schema documentation lives **only in the system prompt**, never in the
rendered ctx block. The prompt is read once per provider call (cached);
rendering schema docs inline in every turn's ctx would duplicate and waste
tokens.

Today's `src/com/blockether/vis/internal/prompt.clj` has an OLD section
referencing `(:session ctx)` / `(:llm-provider ctx)` / `(:project ctx)` /
`(:extensions ctx)` / `(:defs ctx)`. Those keys are the current live engine
shape. When this redesign ships, replace that section with the block below.

Until ship: design doc only; don't edit prompt.clj to lie about a shape
the engine doesn't yet emit.

### Replacement block (post-implementation)

```text
CTX is your session state. Engine renders it as a bare EDN literal under
a `;; ctx` marker in every user message. There is no `ctx` SCI binding;
read from the rendered text, write back via the operators below.

Subtrees:
  :session/workspace  engine-managed; current branch + per-file diff stats
  :session/symbols    engine-managed; live SCI symbols {:arglists :doc :born}
  :session/rules      durable behavior + facts about the user (model-managed)
                      shape: {<kw> {:body string :scope #{:session :project} :born scope}}
                      :scope :project mirrors to project_rule (cross-session)
  :session/decisions  append-only "why we did X" audit (model-managed)
                      shape: {<kw> {:body string :tags #{kw} :born scope}}
  :session/facts      observations the model wants to remember (model-managed)
                      shape: {<kw> {:body string :born scope?}}
                      :born OPTIONAL (engine-derived facts may omit)
  :session/specs      requirements built FROM facts (model-managed)
                      shape: {<kw> {:title :acceptance [string]
                                    :facts [<kw-ref>]
                                    :status keyword :born scope :done-born scope?}}
                      :status ∈ #{:draft :doing :done :cancelled}
  :session/tasks      work items toward specs (model-managed)
                      shape: {<kw> {:title :spec <kw-ref>
                                    :depends-on [<kw-ref>]
                                    :status keyword
                                    :evidence [<scope-string>]
                                    :journal [{:status :scope}]
                                    :born scope :blocked-on string?}}
                      :status ∈ #{:todo :doing :done :blocked :cancelled}
                      :spec REQUIRED; :evidence REQUIRED when :status :done
                      :journal engine-appended on every :status change
  :session/trailer    pinned iter envelopes from prior turns
                      auto-pinned by engine each turn; you drop/summarize via done

Per-subtree mutation functions (engine validates shape; stamps :born;
auto-journals on status changes; soft-warns on missing required fields):

  Rules         (rule-set! :K {:body :scope})
                (rule-remove! :K)
  Decisions     (decision! :K {:body :tags})              ; append-only
  Facts         (fact-set! :K {:body})
                (fact-remove! :K)
  Specs         (spec-set! :K {:title :acceptance :facts :status})
                (spec-remove! :K)
  Tasks         (task-set! :K {:title :spec :depends-on :status :evidence :blocked-on})
                (task-remove! :K)

  All *-set! calls merge partial maps into existing entries.
  task-set! with :status change auto-appends to :journal.
  spec-set! with :status :done | :cancelled auto-stamps :done-born.

Symbol lifecycle (native SCI; persisted by engine):
  (defn foo [x] …)          ; create / overwrite; survives turns
  (def  foo nil)            ; drop; engine treats expression IS NULL as forgotten

Final form per iter:
  (done {:answer            "markdown string"
         :trailer-drop      ["t<N>/i<N>" …]                       ; iter scopes
         :trailer-summarize [{:scope "t<N>/i<N>" :summary "…"} …]})
  Engine auto-pins each current-turn iter; :trailer-drop prunes,
  :trailer-summarize replaces a verbatim pin with prose. Both apply to
  any entry (current or prior). Conflict (same scope in both) errors.

Engine introspection (bare primitives — not registered as v/ ops):
  Session structure:
    (iter "tN/iN")           one iter, full forms vec
    (form "tN/iN/fK")        single form envelope
    (turn "tN")              turn TOC: user-msg + answer + iter-scopes
    (iter-heads "tN")        iter list with first-form head per iter
    (turn-list)              all turns, with head + status

  SCI symbols:
    (symbol-doc 'sym)        docstring or nil
    (symbol-source 'sym)     full def source as string
    (symbol-meta 'sym)       full var metadata
    (symbol-apropos "pattern")  matching symbol list

Scope coordinates:
  Format    "t<N>/i<N>/f<N>"     e.g. "t3/i2/f1"
  Iter      "t<N>/i<N>"          e.g. "t3/i2"
  Turn      "t<N>"               e.g. "t3"
  Every model-created entity that needs provenance carries :born <scope>.
  Engine sorts trailer by scope via a comparator that parses segments.

Schema is soft. Engine warns on unknown subtree keys but never refuses.
```

### Inline render hints (not in prompt — engine emits per dump)

The render adds a few deterministic `;;` annotations next to data when
state warrants. These are not schema docs; they're contextual flags.

Engine hint rules:

| condition | hint |
|---|---|
| entry has `:source :project-mirror` | `;; loaded from project_rule` |
| trailer entry is summary shape | `;; summarized in t<N>` |
| `:result` value in a pinned form exceeds threshold (e.g. 4kB) | `;; ⚠ result is <size>; consider summarizing or dropping` |
| `:session/trailer` count exceeds threshold (e.g. 30) | `;; :session/trailer (32 entries; 5 summarized)` |
| `:session/symbols` symbol's `:born` points into a turn now summarized | `;; born iter summarized; (form …) for original source` |

Nothing else. Engine never explains schema in render.

---

## Trailer mechanics (auto-pin + drop + summarize)

At each `(done …)`, engine runs in this order:

```
1. Auto-pin every current-turn iter whose :forms (excluding `(done …)`) is non-empty.
   Each becomes {:scope "t<cur>/i<N>" :forms [...]} in trailer.
   ↓
2. Apply :trailer-drop      — remove entries by exact :scope match. Idempotent if absent.
   ↓
3. Apply :trailer-summarize — find entry with matching :scope, REPLACE its value with
                              {:scope :summary}. Errors if scope not present.
   ↓
4. Sort trailer by :scope.
   ↓
5. Persist.
```

**Conflict rule**: same scope in both `:trailer-drop` and `:trailer-summarize` → engine errors (`;; ⚠ scope t6/i2 in both drop and summarize — pick one`).

**Apply to ANY entry**: drop and summarize work on current-turn entries AND historical ones. Model can retroactively summarize a prior turn's iter.

**Folding many iters into one summary**: drop iters i2-i5, summarize i1 with note covering all five. Schema stays flat (per-iter entries); summary text encodes breadth.

**Range summaries are NOT supported** (no `:scope-start` / `:scope-end`). Per-iter granularity throughout. Simpler schema, model writes breadth in `:summary` string.

---

## Entry keys for memo subtrees

Memos (`:rules :decisions :specs :tasks`) keyed by model-chosen keywords. Cross-table refs are bare keywords.

```clojure
(task-set! :wire-trailer-render
           {:title      "Render :session/trailer with per-form envelopes"
            :spec       :ctx-redesign
            :status     :todo
            :depends-on [:dissoc-counters]})
```

Engine stamps `:born <current-form-scope>` on first `task-set!` to a new
key. Engine appends to `:journal` on every `:status` change. No counters
subtree. No mint operator. No path-keyed mutator (the legacy `mem-*`
verbs are gone).

---

## Storage shape

The ctx blob is a flat map with `:session/*` keys. Pure EDN; no comments
in storage. Five model-managed subtrees, two engine-rendered views, one
trailer (mutated via `done`).

```clojure
{:session/id     "01HXYZ"
 :session/turn   7

 ;; ── ENGINE-RENDERED VIEWS (derived per dump) ──
 :session/workspace
   {:branch   "feat/X"           ; current workspace branch
    :trunk    "main"             ; upstream merge target
    :head     "abc1234"          ; current commit sha (short)
    :dirty?   true               ; (boolean (seq stats))
    :stats    {"path/to/file" {:added int :removed int}}}

 :session/symbols
   {<sym> {:arglists ([params] …)   ; nil for non-callable defs
           :doc       string         ; may be nil
           :born      "tN/iN/fK"}}   ; scope of LAST def (last-redef semantics)

 ;; ── MODEL-MANAGED MEMOS ──

 :session/rules        ; durable behavior + facts about the user
   {<kw> {:body   string
          :scope  :session | :project        ; :project mirrors to project_rule
          :born   "tN/iN/fK"
          :source :project-mirror}}          ; engine-set when loaded across sessions

 :session/decisions    ; append-only audit ("why we did X")
   {<kw> {:body  string
          :tags  #{kw}
          :born  "tN/iN/fK"}}

 :session/facts        ; observations the model wants to remember
   {<kw> {:body  string
          :born  "tN/iN/fK"}}                ; :born OPTIONAL — engine-derived facts may omit

 :session/specs        ; formal requirements; specs are BASED ON facts
   {<kw> {:title      string
          :acceptance [string]
          :facts      [<kw-ref>]             ; refs into :session/facts
          :status     :draft | :doing | :done | :cancelled
          :born       "tN/iN/fK"
          :done-born  "tN/iN/fK"}}           ; scope where :status became :done/:cancelled

 :session/tasks        ; work items; ALWAYS point to a spec; require evidence on done
   {<kw> {:title       string
          :spec        <kw-ref>              ; REQUIRED — task must point to a spec
          :depends-on  [<kw-ref>]            ; renamed from :deps
          :status      :todo | :doing | :done | :blocked | :cancelled
          :evidence    [<scope-string>]      ; REQUIRED on :status :done — scopes proving completion
          :journal     [{:status :doing :scope "tN/iN/fK"}
                        {:status :done  :scope "tM/iM/fK"}]   ; engine-appended on every :status change
          :born        "tN/iN/fK"
          :blocked-on  string}}              ; free text when :status :blocked

 ;; ── TRAILER (mutated via done keys) ──
 :session/trailer
   ;; Two shapes coexist, both keyed by :scope. Sorted by scope.
   [{:scope "tN/iN"                          ; verbatim pin (auto-pinned at done)
     :forms [{:scope "tN/iN/fK"
              :tag   :observation | :mutation
              :src   string
              :result any                    ; dropped if default
              :error  {:message :data}}]}    ; dropped if nil

    {:scope            "tN/iN"               ; summary entry (model-replaced)
     :summary          string
     :summarized-born  "tN/iN/fK"}]}         ; scope of the done that summarized
```

## Engine-enforced fields

Engine warns (does NOT refuse) on:

| call | required field | rule |
|---|---|---|
| `task-set!` | `:spec` | task must reference a spec |
| `task-set!` with `:status :done` | `:evidence` | required vec of scopes proving completion |
| `task-set!` with `:status :blocked` | `:blocked-on` | required free-text reason |
| `spec-set!` | `:facts` | vec may be empty but field should exist |

Soft schema — engine warns via `;; ⚠ task :wire-render missing :spec` in next render, never refuses the write.

## Engine-auto-stamped fields

| call pattern | field | shape |
|---|---|---|
| any `*-set!` on a NEW key | `:born` | engine stamps `<current-form-scope>` |
| `task-set!` that changes `:status` | `:journal` | engine appends `{:status <new> :scope <current-form-scope>}` |
| `spec-set!` with `:status :done` or `:cancelled` | `:done-born` | engine stamps `<current-form-scope>` |

Model never writes `:born`, `:journal`, or `:done-born` directly; they grow from the call patterns above.


---

## Tag classification (cross-validated against source)

### Registered ops (verified)

From `extensions/common/vis-foundation/src/.../core.clj` and `.../editing/core.clj`:

| op | tag |
|---|---|
| `:v/cat` `:v/ls` `:v/rg` `:v/exists?` `:v/snapshot` | `:observation` |
| `:v/repositories` `:v/git` `:v/languages` `:v/monorepo` `:v/main-agent-instructions` | `:observation` |
| `:v/patch` `:v/create-dirs` `:v/copy` `:v/move` `:v/delete` `:v/delete-if-exists` | `:mutation` |
| `:v/refresh!` `:v/reload-extensions!` | `:mutation` |

From `extensions/common/vis-foundation-git/src/.../core.clj`:

| op | tag |
|---|---|
| `:git/diff` `:git/status` `:git/log` | `:observation` |

From `extensions/common/vis-exa/src/.../core.clj`: all `:observation`.

### Deprecated registered ops (slated for removal from foundation)

Migrate to engine primitives:

- `:v/session-state` `:v/session-report` → replaced by `iter` / `form` / `turn` / `iter-heads` / `turn-list`.
- `:v/engine-symbol-documentation` `:v/engine-symbol-source-code` `:v/engine-symbol-metadata` `:v/engine-symbol-apropos` → replaced by `symbol-doc` / `symbol-source` / `symbol-meta` / `symbol-apropos` (engine-bare, no `v/` prefix).

Engine primitives don't go through `register-op!`. They're bound in the SCI hidden-sym set alongside `done` / `mem-*` / `set-session-title!` / `satisfy-hint!`. They have no `:tag` registration; engine derives `:observation` at trailer auto-pin time when their head is the called symbol.

### Engine primitive forms (not in register-op!; engine derives tag at trailer auto-pin)

| head pattern | tag |
|---|---|
| `(defn …)` `(def …)` | `:mutation` |
| `(rule-set! …)` `(rule-remove! …)` `(decision! …)` `(fact-set! …)` `(fact-remove! …)` | `:mutation` |
| `(spec-set! …)` `(spec-remove! …)` `(task-set! …)` `(task-remove! …)` | `:mutation` |
| `(iter …)` `(form …)` `(turn …)` `(iter-heads …)` `(turn-list)` | `:observation` |
| `(symbol-doc …)` `(symbol-source …)` `(symbol-meta …)` `(symbol-apropos …)` | `:observation` |
| arithmetic, string ops, `get-in`, `filter`, plain expressions | `:observation` |
| `(done …)` `(set-session-title! …)` `(satisfy-hint! …)` | excluded from trailer |

### Tag values are bare keywords

Canonical = bare `:observation` / `:mutation`. The legacy `:op.tag/`
namespace is killed. Storage, render, and `register-op!` validation
all use the same two keywords. No render-vs-storage divergence.

### Compactness rules per form

- Drop `:result` when value is the trivial default (`:ok` for `:mem`, `#'sym` for `:def`/`:defn`).
- Drop `:result` when nil and `:error` is present.
- Drop `:error` when nil.

---

## Rendered CTX block (what model sees)

Schema docs live in the system prompt — explained ONCE per session. The
rendered ctx is just the pure EDN data plus occasional inline `;;`
provenance hints (cross-session sources, summarization markers, size
warnings). Storage in `session_state.ctx TEXT` is the same EDN minus
the hints.

Hints emitted by the engine, deterministically:

| condition | hint |
|---|---|
| `:source :project-mirror` on an entry | `;; loaded from project_rule` |
| trailer entry is summary shape | `;; summarized in t<N>` (engine knows from a small `:summarized-in` engine-set key, or by entry creation provenance) |
| `:result` value in a pinned form exceeds threshold | `;; ⚠ result is <size>; consider summarizing` |

Nothing else. No per-subtree schema explanation in the render.

```clojure
;; ctx
{:session/id   "01HXYZ"
 :session/turn 7

 :session/workspace
   {:branch "feat/ctx-redesign" :trunk "main" :head "abc1234" :dirty? true
    :stats  {"src/auth.clj"    {:added 5  :removed 2}
             "src/logging.clj" {:added 47 :removed 0}
             "test/x.clj"      {:added 0  :removed 12}}}

 :session/symbols
   {auth-check {:arglists ([tok]) :doc "literal-compare check; deprecated" :born "t5/i1/f1"}
    emit-event {:arglists ([{:keys [level msg] :as ev}]) :born "t4/i1/f1"}}

 :session/rules
   {:caveman-pl    {:body "respond in PL caveman style" :scope :session :born "t1/i1/f1"}
    :real-db-tests {:body "tests must hit real SQLite — no mocks" :scope :project
                    :source :project-mirror}}                                       ;; loaded from project_rule

 :session/decisions
   {:no-llm-compaction {:body "Never LLM-compact ctx; deterministic rules only"
                        :tags #{:ctx :design} :born "t3/i2/f1"}}

 :session/facts
   {:auth-literal-compare {:body "src/auth.clj uses `(= tok \"secret\")` for check/1"
                           :born "t3/i2/f1"}
    :no-bcrypt-dep        {:body "deps.edn does not include bcrypt yet"
                           :born "t3/i3/f1"}}

 :session/specs
   {:auth-bcrypt {:title      "switch auth/check to bcrypt"
                  :acceptance ["check/1 calls bcrypt/check"
                               "stored-hash plumbed"
                               "tests cover wrong-password path"]
                  :facts      [:auth-literal-compare :no-bcrypt-dep]
                  :status     :doing
                  :born       "t5/i1/f1"}}

 :session/tasks
   {:add-bcrypt-dep {:title      "add bcrypt to deps.edn"
                     :spec       :auth-bcrypt
                     :depends-on []
                     :status     :done
                     :evidence   ["t5/i2/f1"]                                   ;; the v/patch
                     :journal    [{:status :doing :scope "t5/i1/f2"}
                                  {:status :done  :scope "t5/i2/f2"}]
                     :born       "t5/i1/f2"}
    :replace-check  {:title      "replace literal compare with bcrypt/check"
                     :spec       :auth-bcrypt
                     :depends-on [:add-bcrypt-dep]
                     :status     :doing
                     :journal    [{:status :doing :scope "t6/i1/f1"}]
                     :born       "t5/i1/f3"}}

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
     :summarized-born "t6/i2/f1"}]}                                                ;; summarized in t6
```

## Example 1 — pure conversation, recall-after-5-turns

User chatting, no code. Tests `:session/rules` as the single bucket for user facts + behavior.

### Turn 1
```
USER > Lubię żółty.
```
```clojure
MODEL >
(rule-set! :likes-yellow {:body "user likes yellow color" :scope :session})
=> :ok

(done {:answer "Zapamiętane — kolor żółty."})
=> :done
```
Engine auto-pin: iter contains only `rule-set!` + `done`. After excluding `done`, `:forms` = `[rule-set!]`. Pinned.

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
Auto-pin: `(* 17 23)` form. Pinned.

### Turn 3
```
USER > Mam psa.
```
```clojure
MODEL >
(rule-set! :has-pet-dog {:body "user has a dog" :scope :session})
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
Auto-pin: only `(done …)`. After exclusion, `:forms` empty. Engine SKIPS auto-pin for this iter.

### Turn 5
```
USER > Co lubię?
```
Model reads `:session/rules` from CTX. No introspection needed; trivial recall.
```clojure
MODEL >
(done {:answer "Lubisz żółty kolor i masz psa."
       ;; the math + pref-set iters are clutter; summarize them
       :trailer-summarize [{:scope "t2/i1" :summary "computed 17*23 = 391"}
                           {:scope "t1/i1" :summary "set :likes-yellow"}
                           {:scope "t3/i1" :summary "set :has-pet-dog"}]})
=> :done
```

### CTX AFTER TURN 5

```clojure
{:session/id   "01HXYZ"
 :session/turn 5
 :session/workspace {:branch "main" :trunk "main" :head "abc1234" :dirty? false :stats {}}
 :session/symbols   {}
 :session/rules
   {:likes-yellow {:body "user likes yellow color" :scope :session :born "t1/i1/f1"}
    :has-pet-dog  {:body "user has a dog"          :scope :session :born "t3/i1/f1"}}
 :session/decisions {} :session/specs {} :session/tasks {}
 :session/trailer
   [{:scope "t1/i1" :summary "set :likes-yellow"}
    {:scope "t2/i1" :summary "computed 17*23 = 391"}
    {:scope "t3/i1" :summary "set :has-pet-dog"}]}
```

**Observations:**
- One bucket (`:session/rules`) holds user facts + behavior rules.
- Trailer holds three summaries — minimal token cost for the historical iters.
- `(turn "t4")` returns the conversational answer on demand without bloating trailer.

---

## Example 2 — read-patch-verify (workspace stats + stale-read drop)

### Turn 1
```
USER > Pokaż mi src/auth.clj i powiedz co tam jest.
```
```clojure
MODEL >
(v/cat "src/auth.clj")          ;; scope t1/i1/f1
=> "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"

(done {:answer "Plik definiuje `check/1`: literal compare na `\"secret\"`."})
=> :done
```

### CTX AFTER TURN 1
```clojure
{:session/turn 1
 :session/workspace {:branch "main" :trunk "main" :head "abc1234" :dirty? false :stats {}}
 :session/trailer
   [{:scope "t1/i1"
     :forms [{:scope "t1/i1/f1" :tag :observation
              :src "(v/cat \"src/auth.clj\")"
              :result "(ns auth)\n(defn check [tok] (= tok \"secret\"))\n"}]}]
 …}
```

### Turn 2
```
USER > Zamień to literal compare na bcrypt verify.
```

Model has `t1/i1` in trailer (verbatim) → already knows current file content. No re-cat needed. Patches directly. Then drops the now-stale read.

```clojure
MODEL >
(v/patch [{:path "src/auth.clj"
           :search "(defn check [tok] (= tok \"secret\"))"
           :replace "(defn check [tok] (bcrypt/check tok stored-hash))"}])
=> {:applied 1 :path "src/auth.clj"}

(done {:answer       "Zamieniono `check` na bcrypt verify."
       :trailer-drop ["t1/i1"]})                   ; the v/cat result is now stale
=> :done
```

### CTX AFTER TURN 2

```clojure
{:session/turn 2
 :session/workspace
   {:branch "feat/auth-bcrypt" :trunk "main" :head "def5678" :dirty? true
    :stats {"src/auth.clj" {:added 1 :removed 1}}}
 :session/trailer
   [{:scope "t2/i1"
     :forms [{:scope "t2/i1/f1" :tag :mutation
              :src "(v/patch [{:path \"src/auth.clj\" :search \"…\" :replace \"…\"}])"
              :result {:applied 1 :path "src/auth.clj"}}]}]}
```

**Observations:**
- `t1/i1` (the cat) gone from trailer — explicit drop. No engine magic.
- `t2/i1` auto-pinned.
- Cross-turn no stale data lingering.

---

## Example 3 — facts → specs → tasks with evidence + journal

Multi-turn coding job. Demonstrates the new schema end-to-end.

### Turn 1 — observe and crystallize facts

```
USER > Refactor src/logging.clj to emit structured EDN events instead of formatted text.
```
```clojure
MODEL >
(v/cat "src/logging.clj")          ;; scope t1/i1/f1
=> "(ns logging)\n(defn log [level msg] (println (str/format \"%s: %s\" level msg)))\n"

(fact-set! :logging-current-shape
           {:body "src/logging.clj: log/2 uses str/format to write text to stdout"})
=> :ok

(fact-set! :no-event-fn
           {:body "No emit-event or structured-logging fn exists yet"})
=> :ok

(done {:answer "Przeczytałem `src/logging.clj`, zapisałem dwa fakty. Następna tura: spec."})
=> :done
```

### CTX AFTER TURN 1 (excerpt)

```clojure
:session/facts
  {:logging-current-shape {:body "src/logging.clj: log/2 uses str/format …" :born "t1/i2/f1"}
   :no-event-fn           {:body "No emit-event or structured-logging fn exists yet" :born "t1/i3/f1"}}
:session/trailer
  [{:scope "t1/i1" :forms [{:scope "t1/i1/f1" :tag :observation :src "(v/cat …)" :result "…"}]}]
```

### Turn 2 — formalize spec citing facts; create tasks

```
USER > Działaj — spec + plan.
```
```clojure
MODEL >
(spec-set! :logging-edn
           {:title      "src/logging.clj emits structured EDN events"
            :acceptance ["emit-event takes a map and prints (pr-str …)"
                         "log/2 delegates to emit-event"
                         "no str/format remains in src/logging.clj"]
            :facts      [:logging-current-shape :no-event-fn]
            :status     :draft})
=> :ok

(task-set! :introduce-emit-event
           {:title      "introduce emit-event fn"
            :spec       :logging-edn
            :depends-on []
            :status     :todo})
;; engine warns: ;; ⚠ task :introduce-emit-event lacks :evidence (only required on :done)
=> :ok

(task-set! :rewire-log
           {:title      "rewire log/2 through emit-event"
            :spec       :logging-edn
            :depends-on [:introduce-emit-event]
            :status     :todo})
=> :ok

(done {:answer "Spec `:logging-edn` zapisany (2 facty). 2 taski."})
=> :done
```

### CTX AFTER TURN 2 (excerpt)

```clojure
:session/specs
  {:logging-edn {:title      "src/logging.clj emits structured EDN events"
                 :acceptance […]
                 :facts      [:logging-current-shape :no-event-fn]
                 :status     :draft :born "t2/i1/f1"}}
:session/tasks
  {:introduce-emit-event {:title "introduce emit-event fn" :spec :logging-edn
                          :depends-on [] :status :todo :born "t2/i1/f2"}
   :rewire-log           {:title "rewire log/2 through emit-event" :spec :logging-edn
                          :depends-on [:introduce-emit-event] :status :todo :born "t2/i1/f3"}}
```

### Turn 3 — work the first task; evidence + journal

```
USER > Zaczynaj.
```
```clojure
MODEL >
(task-set! :introduce-emit-event {:status :doing})
;; engine appends to :journal: {:status :doing :scope "t3/i1/f1"}
=> :ok

(defn emit-event [{:keys [level msg] :as ev}]
  (println (pr-str ev)))
=> #'vis.user/emit-event

(v/patch [{:path "src/logging.clj"
           :search "(defn log [level msg]\n  (println (str/format \"%s: %s\" level msg)))"
           :replace "(defn emit-event [{:keys [level msg] :as ev}]\n  (println (pr-str ev)))\n\n(defn log [level msg]\n  (emit-event {:level level :msg msg}))"}])
=> {:applied 1}

(task-set! :introduce-emit-event {:status :done :evidence ["t3/i2/f1" "t3/i3/f1"]})
;; engine appends to :journal: {:status :done :scope "t3/i4/f1"}
;; t3/i2/f1 = defn emit-event; t3/i3/f1 = v/patch (engine stamped both atomically above)
=> :ok

(done {:answer       "Task `:introduce-emit-event` done. Evidence pinned."
       :trailer-drop ["t3/i2"]})            ;; the bare defn iter — already mutating; engine has source via :defined
=> :done
```

### CTX AFTER TURN 3 (excerpt)

```clojure
:session/tasks
  {:introduce-emit-event
     {:title      "introduce emit-event fn"
      :spec       :logging-edn
      :depends-on []
      :status     :done
      :evidence   ["t3/i2/f1" "t3/i3/f1"]
      :journal    [{:status :doing :scope "t3/i1/f1"}
                   {:status :done  :scope "t3/i4/f1"}]
      :born       "t2/i1/f2"}
   :rewire-log {…}}                        ; unchanged; still :todo
```

### Turn 4 — facts go stale after mutation; drop the stale fact

```
USER > Kontynuuj.
```
```clojure
MODEL >
;; the fact :logging-current-shape now describes pre-patch state — stale.
(fact-remove! :logging-current-shape)
=> :ok

;; we can promote :no-event-fn → :event-fn-exists, but simpler: drop and re-fact
(fact-remove! :no-event-fn)
(fact-set! :emit-event-shape
           {:body "emit-event takes {:level :msg :ts?} and prints (pr-str)"})
=> :ok

(task-set! :rewire-log {:status :doing})
=> :ok

;; …work happens, second task done…
(task-set! :rewire-log {:status :done :evidence ["t3/i3/f1"]})    ;; same v/patch covered both
=> :ok

(spec-set! :logging-edn {:status :done})
;; engine stamps :done-born "t4/i5/f1"
=> :ok

(done {:answer "Spec `:logging-edn` zamknięty. Wszystkie taski done z evidence."})
=> :done
```

### CTX AFTER TURN 4 (excerpt)

```clojure
:session/facts {:emit-event-shape {:body "emit-event takes {:level :msg :ts?} and prints (pr-str)" :born "t4/i3/f1"}}

:session/specs
  {:logging-edn {:title "…" :facts [:logging-current-shape :no-event-fn]    ;; refs point to dropped facts — soft
                 :acceptance […]
                 :status :done :born "t2/i1/f1" :done-born "t4/i5/f1"}}

:session/tasks
  {:introduce-emit-event {…done with evidence + journal…}
   :rewire-log
     {:title "rewire log/2 through emit-event" :spec :logging-edn
      :depends-on [:introduce-emit-event] :status :done
      :evidence   ["t3/i3/f1"]
      :journal    [{:status :doing :scope "t4/i4/f1"}
                   {:status :done  :scope "t4/i5/f1"}]
      :born "t2/i1/f3"}}
```

**Note on stale spec refs**: spec `:logging-edn :facts` still references
`:logging-current-shape` and `:no-event-fn` which are now dropped from
`:session/facts`. Engine could warn (`;; ⚠ spec :logging-edn references
dropped fact :logging-current-shape`) but does not refuse. Model can
update spec's `:facts` field if needed.


## Example 4 — cross-session project rule

### Session A, Turn 7
```
USER > Pamiętaj, w tym projekcie testy muszą uderzać w prawdziwy SQLite, nigdy mockować.
```
```clojure
MODEL >
(rule-set! :real-db-tests
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

(done {:answer "Test dodany. Używa tmp SQLite jak wymaga reguła `:real-db-tests`."
       :trailer-drop ["t1/i1"]})                  ;; stale cat after patch
=> :done
```

---

## Cross-cutting questions still open

1. ~~**Render-shortening of tag value.**~~ **RESOLVED.** Canonical is
   bare `:observation` / `:mutation`. The `:op.tag/` namespace is killed.
   Storage, render, and `register-op!` validation all use the same
   two keywords.

2. **`:born <scope>` semantics on `:session/symbols`.** Currently = LAST
   redef coord. Earlier open question (first-def vs last-redef) resolved
   in favour of last-redef. Confirm.

3. **Workspace branch spawn — engine or model?** `:branch "main"` on a
   fresh session: should engine auto-spawn before any `v/patch`, or rely
   on existing Vis flow? Today's flow handles this; no change proposed.

4. **Spec status auto-promotion.** Example 3 didn't bump `:logging-edn`
   from `:draft` to `:doing`. Lean **manual** — no hidden magic.

5. **Overwrite warning on existing memo key.** Silent replace vs
   `;; ⚠ overwriting [:session/tasks :X] (prior :status was :doing)`.
   Lean **warn**, never refuse.

6. **Big result handling in trailer entries.** A 47kB file body in
   `:forms[0] :result` is real. Lean **inline + soft warn**
   (`;; ⚠ trailer entry t3/i2/f1 result is 47kB; consider summarizing
   or dropping`).

7. **Auto-prune of `:session/trailer`.** Model-explicit summarization /
   drop is the only bound. Engine never auto-prunes. Render comment
   surfaces count (`;; :session/trailer (12 entries; 3 summarized)`).

8. **Stale-read heuristic.** Project rule `:trailer-stale-reads` nudges
   model to drop or summarize observation pins whose target was later
   mutated. Engine never enforces.

9. **`(meta #'sym)` survives `restore-sandbox!`?** Probe needed before
   locking prompt copy that recommends introspection.

10. **Naming collision** for engine introspection ops (`iter`, `form`,
    `turn`). SCI shadowing is benign (local bindings work), but model
    documentation should call out the engine-bare nature. Lean **bare
    names** + explicit hidden-sym registration.

---

## Implementation surface (high-level)

### Engine changes

1. **CTX storage** — `session_state.ctx TEXT` (already exists or add).
2. **Per-subtree functions** — `rule-set!` / `rule-remove!` / `decision!` / `fact-set!` / `fact-remove!` / `spec-set!` / `spec-remove!` / `task-set!` / `task-remove!`. Bind in SCI hidden-sym set; engine validates shape, stamps `:born`, auto-journals on task `:status` changes, soft-warns on missing required fields. Write through to `session_state.ctx` blob.
3. **`iter` / `form` / `turn` / `iter-heads` / `turn-list`** — bind in SCI hidden-sym set; SELECT against `session_turn` / `session_turn_iteration`.
4. **`(done {…})`** — handle `:trailer-drop` and `:trailer-summarize` keys; engine auto-pin loop.
5. **Trailer comparator** — parse `t<N>/i<N>` segments for sort.
6. **CTX render** — pretty-print blob as bare EDN literal under `;; ctx` marker. NO inline schema-explanation comments (those live ONLY in the system prompt). Add deterministic provenance hints only: `;; loaded from project_rule` for `:source :project-mirror`, `;; summarized in t<N>` for summary entries, `;; ⚠ result is <size>` for big results, `;; ⚠ task :K missing :spec` for soft schema violations.
7. **Symbol persistence** — already exists (`restore-sandbox!` + `definition_state.expression IS NULL` semantics).

### Foundation extension changes

1. **Remove from `register-op!` doseq**:
   - `:v/session-state` `:v/session-report` (replaced by engine `iter` / `form` / `turn` / `iter-heads` / `turn-list`).
   - `:v/engine-symbol-documentation` `:v/engine-symbol-source-code` `:v/engine-symbol-metadata` `:v/engine-symbol-apropos` (replaced by engine `symbol-doc` / `symbol-source` / `symbol-meta` / `symbol-apropos`).
2. **No additions** — introspection ops are engine primitives, not extension ops.

### Workspace integration

1. **`:session/workspace`** — engine computes from `workspace/for-session` + `workspace/status` per render; `:stats` from `git diff --numstat` against trunk.

### Persistance changes

1. **`project_rule`** table — `(repo_id, rule_key, body, scope, added_at)`. Mirrored on `rule-set!` with `:scope :project`.
2. **Session resume** — merge `project_rule` rows into `:session/rules` on session start.

### Prompt changes

**Already done in `src/.../internal/prompt.clj`:**
- Session vocabulary (turn / iter / form / scope) — added.
- Fence vs block — fence is the markdown delimiter; form is the unit.

**Deferred until engine ships the new CTX:**
- Replace the `Read \`ctx\` first. Engine context keys:` block with the schema documentation in this file's §"System prompt — required additions".
- Drop the `Use \`def\` for working memory.` and `No separate memory API.` lines (no longer true — per-subtree functions ARE the memory API).

### Code rename (legacy → form)

| was | new |
|---|---|
| `:block-source` | `:form-source` |
| `block-results` | `form-results` |
| `code-blocks` | `form-sources` |
| `block-segments` | `form-segments` |
| `block-silents` | `form-silents` |
| `:block-count` | `:form-count` |
| `:all-block-count` | `:all-form-count` |
| `:dropped-block-count` | `:dropped-form-count` |
| `:block-index` (scope) | `:form-index` |
| `:iteration-block-index` | `:iteration-form-index` |
| `/block/N` in scope URL | `/f/N` |
| scope format `turn/<ulid>/iteration/N/block/N` | `t<N>/i<N>/f<N>` |

---

## Final minimum-viable shape

```clojure
{:session/id     "01HXYZ"
 :session/turn   N

 :session/workspace  {:branch :trunk :head :dirty? :stats}
 :session/symbols    {sym {:arglists :doc :born}}

 :session/rules      {keyword {:body :scope :born}}
 :session/decisions  {keyword {:body :tags :born}}
 :session/facts      {keyword {:body :born?}}
 :session/specs      {keyword {:title :acceptance :facts :status :born :done-born?}}
 :session/tasks      {keyword {:title :spec :depends-on :status
                               :evidence :journal :born :blocked-on?}}

 :session/trailer    [{:scope :forms [{:scope :tag :src :result :error}]}   ; verbatim pin
                      {:scope :summary}]}                                    ; summary entry
```

Eight substantive subtrees. Two engine-rendered; five memos via
per-subtree functions (`rule-*`, `decision!`, `fact-*`, `spec-*`, `task-*`);
one trailer via `done`. Symbols managed natively via `defn` / `(def x nil)`
against existing Vis persistence.

Cross-turn raw history reachable via engine primitives (`iter`, `form`,
`turn`, `iter-heads`, `turn-list`); never preloaded.
